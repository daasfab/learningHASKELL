type Transaction = (Char, Int, Int, String, Int) 

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11), 
            ('S', 100,  210, "IWRD", 12)
            ]


-- Converts a single transaction into a specific format -> "transaction_to_string ('B', 100, 1104, "VTI", 1)" would return "Bought 100 units of VTI for 1104 pounds each on day 1"
transaction_to_string :: Transaction -> String          -- fun. 1
transaction_to_string (action, quantity, cost, stock, day) 
            | action == 'B' = "Bought " ++ show quantity ++ " units of " ++ stock ++ " for " ++ show cost ++ " pounds each on day " ++ show day --show converts the pattern matched elements of the tuple into string
            | action == 'S' = "Sold " ++ show quantity ++ " units of " ++ stock ++ " for " ++ show cost ++ " pounds each on day " ++ show day


-- Takes a list of transactions, converting each transaction into a str
trade_report_list :: [Transaction] -> [String]          -- fun. 2
trade_report_list providedlog = map transaction_to_string providedlog 


--Takes a stock and a transaction log, returning True if the transaction trades that stock, and False otherwise
stock_test :: String -> Transaction -> Bool             -- fun. 3
stock_test string (action, quantity, cost, stock, day)
    | string == stock = True 
    | otherwise = False 
    
stock_test :: String -> Transaction -> Bool             -- fun. 3 (More Efficient Code)
stock_test stock (_,_,_, s, _) = s == stock


-- Takes a stock and a transaction log, returning all trades in the transaction log that trade the given stock
get_trades :: String -> [Transaction] -> [Transaction]  -- fun. 4
get_trades stockName purchases = filter (\x -> stock_test stockName x == True) purchases 

get_trades :: String -> [Transaction] -> [Transaction]  -- fun. 4 (More Efficient Code)
get_trades stockName purchases = filter (stock_test stockName) purchases 


-- Takes a stock and transaction log, returning a string containing human-readable version of the log with "/n" for next line
trade_report :: String -> [Transaction] -> String       -- fun. 5
trade_report stockName purchases = unlines ( map transaction_to_string (get_trades stockName purchases))


-- Takes a transaction and current amount of money that you have, returning the amount of money that you have after the transaction
update_money :: Transaction -> Int -> Int               -- fun. 6
update_money (action, quantity, cost, stock, day) userMoney 
    | action == 'B' = userMoney - (cost * quantity)
    | action == 'S' = userMoney + (cost * quantity)


-- Takes a transaction log and name of a stock, returning total amount of profit or loss made for that stock
profit :: [Transaction] -> String -> Int                -- fun. 7
profit purchases stockName = foldr update_money 0 (get_trades stockName purchases) 


-- Takes a list of stocks and a transaction log, returning human-readable str containing profit and loss report
profit_report :: [String] -> [Transaction] -> String    -- fun. 8
profit_report list purchases = unlines (map (\ stockName -> stockName ++ ": " ++ show (profit purchases stockName)) list ) --"list" is the list of stocks, using the map function to repeatedly perform the same action joining the stockName to the string of what "profit" function outputs on every element of the list. 


-- Takes a transaction log and price database, returning a profit and loss report in the same format as in fun. 8
test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"

type Prices = [(String, [Int])]

test_prices :: Prices 
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]

complex_profit_report :: String -> Prices -> String -- fun. 9  (uses profit_report and 5 helper functions to output the desired result
complex_profit_report userInput pricesList = profit_report (stockList (abc userInput)) (map (\ x -> (transactionMaker pricesList x) ) (abc userInput)  ) 

-- Helper Function 1: (puts the userInput string into a list of lists, every element in which is a string. This way I'll be able to work with individual elements of the list)
abc userInput =  (map words . lines $ userInput) 

-- Helper Function 2: (outputs the cost of specific stock, on the specific day. This is needed to use the previous "profit_report" function, as otherwise the list of transactions passed into it (and "profit","update_money") will not be valid as it isn't yet in the form of ((action, quantity, cost, stock, day)))
cost_OnDay stockName day list = snd (head (filter (\ cost -> stockName == fst cost ) list)) !! (day - 1) -- takes the 2nd element of the tuple where the 1st one is stockName, just to find the price of that stock on day n, using the index of numbers in the list (-1 because the indexing starts with 0)

-- Helper Function 3: (puts all the data into the form of the tuple that will be accepted by profit_report and other functions (action, quantity, cost, stock, day))
transactionMaker pricesList singleTranaction =  --takes given price list for all days, and the userInput (transaction string)
    let
        action = head ( head singleTranaction) -- gives me either char 'S' or char 'B'
        quantity = read (head (drop 1 singleTranaction)) :: Int -- converts string quantity to type Int
        cost = cost_OnDay stockName day pricesList
        stockName =  head (drop 2 singleTranaction) -- unword takes out the stockName of type string, outside of the list brackets, so that its just a string
        day = read ( head (drop 3 singleTranaction) ) :: Int
    in 
        (action, quantity, cost, stockName, day)
        
-- Helper Function 4: (gets a list of stock names, so that I'll be able to pass it into profit_report as the first attribute
stockList listOfLists = deleteRepeats ( map (\ fstList -> head (drop 2 fstList)) listOfLists )

-- Helper Function 5: (removes repeating stocks from stockList) 
deleteRepeats [] = []
deleteRepeats (x:xs) = x : deleteRepeats (filter (/=x) xs)



