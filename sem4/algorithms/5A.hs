main :: IO ()
main = do
    input1 <- getLine
    input2 <- getLine
 
    let number1 = read input1 :: Integer
    let number2 = read input2 :: Integer
    let result = number1 * number2
 
    print result