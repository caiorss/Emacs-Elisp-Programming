
import qualified System.Directory as D

main :: IO ()
main = do 
  putStrLn "Hello World Haskell!"
  D.getDirectoryContents "/" >>= mapM_ putStrLn
