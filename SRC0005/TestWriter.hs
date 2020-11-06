module TestWriter where
 
import Control.Monad.Writer
 
-- Define your writer type. String forms a monoid (neutral = [], concat = ++), therefore can
-- be an argument of Writer
type W a = Writer String a

a = [1..10]
 
-- simple putStrLn equivalent
wPutStrLn :: String -> W ()
wPutStrLn s = tell s >> tell "\n"
 
-- Fibonacci function with intermediate val labeling.
lblFib :: Int -> W Int
lblFib x = fibaux x 0 1
  where
    fibaux 0 a _ = return a
    fibaux x a b = wPutStrLn (show a) >> fibaux (x-1) b (a+b)
   
-- And here you run and collect the outputs.
-- Whenever you just need outputs, and they're not too big, this is a nice choice!
--
-- Another very cool thing is that you can actually use the list monoid and
-- pass outputs that are not strings, to later perform some postprocessing on them.
testAndFormat :: IO Int
testAndFormat
  = do
    let (r, str) = runWriter (lblFib 10)
    putStr str
    return r

