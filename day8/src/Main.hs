module Main where

import Data.Array (Array, bounds, listArray, (!), (//))
import Data.HashSet (Set, empty, insert, member)

data Instruction = Instruction String Int

instance Show Instruction where
  show (Instruction command arg) = command ++ " " ++ show arg

data ExitCode = Running | EOF | EncounteredTwice Int deriving (Eq, Show)

data Machine = Machine {acc :: Int, ran :: Set Int, pc :: Int, state :: ExitCode, txt :: Array Int Instruction} deriving (Show)

parse :: [String] -> Instruction
parse s = Instruction (head s) (toInt (s !! 1))
  where
    toInt ('+' : xs) = toInt xs
    toInt s = read s :: Int

initMachine :: [Instruction] -> Machine
initMachine txt =
  Machine
    { acc = 0,
      ran = empty,
      pc = 0,
      state = Running,
      txt = listArray (0, length txt - 1) txt
    }

execStep :: Machine -> Machine
execStep m | pc m > snd (bounds $ txt m) = m {state = EOF} -- Halt if pc is out of bounds
execStep m | pc m `member` ran m = m {state = EncounteredTwice (pc m)} -- Halt if instruction was already executed
execStep m = case txt m ! pc m of
  Instruction "acc" x -> m {acc = acc m + x, ran = insert (pc m) (ran m), pc = pc m + 1}
  Instruction "jmp" x -> m {ran = insert (pc m) (ran m), pc = pc m + x}
  Instruction "nop" _ -> m {ran = insert (pc m) (ran m), pc = pc m + 1}

run :: Machine -> Machine
run m | state m /= Running = m
run m = run $ execStep m

cmd :: Instruction -> String
cmd (Instruction c _) = c

runAndFix :: Machine -> Machine
runAndFix m | state m /= Running = m
runAndFix m
  | cmd (txt m ! pc m) == "nop" || cmd (txt m ! pc m) == "jmp" =
    if state (runAndFix next) == EOF
      then runAndFix next
      else run $ execStep $ switchOp m
  where
    next = execStep m
    switchOp m = case txt m ! pc m of
      Instruction "nop" arg -> m {txt = txt m // [(pc m, Instruction "jmp" arg)]}
      Instruction "jmp" arg -> m {txt = txt m // [(pc m, Instruction "nop" arg)]}
runAndFix m = runAndFix $ execStep m

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ acc $ run $ initMachine $ map (parse . words) $ lines contents
  print $ acc $ runAndFix $ initMachine $ map (parse . words) $ lines contents
