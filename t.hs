

sevensOnly1 :: [Int]  
sevensOnly1 = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x

sevensOnly2 :: [Int]  
sevensOnly2 = do  
    x <- [1..50]  
    --guard ('7' `elem` show x)  
    return x    
