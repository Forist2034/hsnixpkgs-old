module HsNixPkgs.Shell.Util
  ( echoCmd,
    rmCmd,
    findCmd,
    findCmdExec,
    grepCmd,
    grepQ,
    readCmd,
    readR,
    forOutputLine,
    forFind,
    continueCmd,
    exitCmd,
    isELF,
    isScript,
  )
where

import HsNixPkgs.Shell

echoCmd :: Shell t m => [Term t m] -> m ()
echoCmd t = runCmd_ "echo" [quoteTerms t]

rmCmd :: Shell t m => [Term t m] -> m ()
rmCmd = runCmd_ "rm"

findCmdExec :: (Shell t m) => Term t m -> [Term t m] -> (Term t m -> [[Term t m]]) -> m ()
findCmdExec p opt exe =
  runCmd_ "find" (p : (opt ++ convE (exe (quote "{}"))))
  where
    convE [] = []
    convE [x] = str "-exec" : x
    convE (x : xs@(_ : _)) = str "-exec" : x ++ (quote ";" : convE xs)

findCmd :: (Shell t m) => Term t m -> [Term t m] -> m ()
findCmd p opt = findCmdExec p opt (const [])

grepCmd :: Shell t m => [Term t m] -> m ()
grepCmd = runCmd_ "grep"

grepQ :: Shell t m => [Term t m] -> m ()
grepQ t = grepCmd ("-q" : t)

readCmd :: Shell t m => [Term t m] -> [Var t] -> m ()
readCmd opt t = runCmd_ "read" (opt ++ fmap (str . varName) t)

readR :: Shell t m => [Var t] -> m ()
readR = readCmd ["-r"]

forOutputLine :: Shell t m => m () -> (Var t -> m ()) -> m ()
forOutputLine m c = do
  iter <- localVar ("_forIter" @= "")
  m -|- whileCmd (readR [iter]) (c iter)

forFind ::
  Shell t m =>
  Term t m ->
  [Term t m] ->
  (Var t -> m ()) ->
  m ()
forFind p fa = forOutputLine (findCmd p fa)

continueCmd :: Shell t m => m ()
continueCmd = runCmd_ "continue" []

exitCmd :: Shell t m => [Term t m] -> m ()
exitCmd = runCmd_ "exit"

isELF :: Shell t m => Term t m -> m ()
isELF t = runCmd_ "file" [quote t] -|- grepQ ["ELF"]

isScript :: Shell t m => Term t m -> m ()
isScript t = runCmd_ "file" [quote t] -|- grepQ ["shell script"]