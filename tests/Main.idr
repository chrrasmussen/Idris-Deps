module Main

import System

-- Copied from: https://github.com/edwinb/Blodwen/blob/master/tests/Main.idr

%default covering

tests : List String
tests =
  ["test001"]

chdir : String -> IO Bool
chdir dir = do
  ok <- foreign FFI_C "chdir" (String -> IO Int) dir
  pure (ok == 0)

fail : String -> IO ()
fail err = do
  putStrLn err
  exitWith (ExitFailure 1)

runTest : String -> String -> String -> IO Bool
runTest dir prog test = do
  chdir (dir ++ "/" ++ test)
  putStr $ dir ++ "/" ++ test ++ ": "
  system $ "sh ./run " ++ prog ++ " > output"
  Right out <- readFile "output"
    | Left err => do
      print err
      pure False
  Right exp <- readFile "expected"
    | Left err => do
      print err
      pure False
  if (out == exp)
    then putStrLn "success"
    else putStrLn "FAILURE"
  chdir "../.."
  pure (out == exp)

main : IO ()
main = do
  [_, deps] <- getArgs
    | _ => do putStrLn "Usage: runtests [depsPath]"
  results <- traverse (runTest "deps" deps) tests
  if any not results
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess
