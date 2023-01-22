module HsNixPkgs.BuildSupport.SetupHooks.PatchShebang (patchShebang) where

import Data.Functor
import HsNixPkgs.ExtendDrv
import HsNixPkgs.Shell
import HsNixPkgs.Shell.Util

patchShebang :: MonadDeriv m => ScriptTerm m -> Script m (ScriptTerm m -> Script m ())
patchShebang p =
  funcDef
    "patchShebang"
    ( do
        dir <- localVar ("dir" @= var "1")
        f <- localVar ("f" @= "")
        oldPath <- localVar ("oldPath" @= "")
        newPath <- localVar ("newPath" @= "")
        arg0 <- localVar ("arg0" @= "")
        args <- localVar ("args" @= "")
        oldInterpreterLine <- localVar ("oldInterpreterLine" @= "")
        newInterpreterLine <- localVar ("newInterpreterLine" @= "")
        findCmd (quote (var dir)) ["-type", "f", "-term", "-0100"]
          -|- whileCmd
            (readR [f])
            ( do
                andOrCmd (aoTerm (isScript (varQ f)) `orC` aoTerm continueCmd)
                readR [oldInterpreterLine] <| varQ f
                readR [oldPath, arg0, args] <<| quote (varSuffix f "2")

                ifCmd
                  (test (varQ oldPath `TStrEqual` "*/bin/env"))
                  ( do
                      ifTrue
                        ( test
                            ( (varQ arg0 `TStrEqual` "-*")
                                `TOr` (varQ args `TStrEqual` "*=*")
                            )
                        )
                        ( echoCmd [var f, ": unsupported interpreter directive \"", var oldInterpreterLine]
                            >> exitCmd ["1"]
                        )
                      setVar
                        ( newPath
                            @= outputQ
                              ( runCmd ["PATH" @= p] "command" ["-v", varQ arg0]
                                  `orCC` runCmd_ "true" []
                              )
                        )
                  )
                  ( do
                      ifTrue
                        (test (TZeroStr (varQ oldPath)))
                        (setVar (oldPath @= "/bin/sh"))
                      setVar
                        ( newPath
                            @= outputQ
                              ( runCmd
                                  ["PATH" @= p]
                                  "command"
                                  [ "-v",
                                    outputQ (runCmd_ "basename" [varQ oldPath])
                                  ]
                                  `orCC` runCmd_ "true" []
                              )
                        )
                      setVar (args @= quoteTerms [var arg0, " ", var args])
                  )
                setVar (newInterpreterLine @= quoteTerms [var newPath, " ", var args])
                setVar
                  ( newInterpreterLine
                      @= varTrimSuffix
                        newInterpreterLine
                        ShortestM
                        (varTrimPrefix newInterpreterLine LongestM "*[![:space:]]")
                  )
                let store = Var "NIX_STORE"
                ifTrue
                  ( test
                      ( TNonZeroStr (varQ oldPath)
                          `TAnd` ( quote (varSubstr oldPath "0" (varLength store))
                                     `TStrNotEqual` varQ store
                                 )
                          `TAnd` TNonZeroStr (varQ newPath)
                          `TAnd` (varQ newPath `TStrNotEqual` varQ oldPath)
                      )
                  )
                  ( do
                      echoCmd
                        [ var f,
                          ": interpreter directive changed from \"",
                          var oldInterpreterLine,
                          "\" to \"",
                          var newInterpreterLine,
                          "\""
                        ]
                      time <- localVar ("timestamp" @= output (runCmd_ "stat" ["--printf", "%y", varQ f]))
                      runCmd_
                        "sed"
                        [ "-i",
                          "-e",
                          quoteTerms
                            [ "1 s|.*|#\\!",
                              varReplace newInterpreterLine AllM "\\" "\\\\",
                              "|"
                            ],
                          varQ f
                        ]
                      runCmd_ "touch" ["--date", varQ time, varQ f]
                  )
            )
    )
    <&> \patchF d -> runCmd_ patchF [d]