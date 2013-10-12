main :: IO ()
main = do putStrLn "Hello!"
	  putStrLn "First commit!"
	  putStrLn "What to come?!"
          morePrinting

morePrinting :: IO ()
morePrinting = do putStrLn "Some testing"
  	    	  putStrLn "Will this work?"
