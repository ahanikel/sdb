import Store
import Test.HUnit

main :: IO ()
main = do
  testStoreTarFiles

testStoreTarFiles = do
  let aTars           = "not_a_tarfile.txt" : tars 'a' [1..100]

  putStrLn ""
  putStrLn "Testing the tar filter #1"
  assertEqual ("length of " ++ show (tarFilter aTars))
              (length (tarFilter aTars)) 100

  let cTars           = tars 'c' [53, 77, 85]
      allTars         = aTars ++ cTars
      allTarsFiltered = tarFilter allTars
      allTarsYoungest = youngestOnly allTarsFiltered

  putStrLn "Testing the tar filter #2"
  assertEqual ("length of " ++ show allTarsFiltered)
              (length allTarsFiltered) 103

  putStrLn "Testing the youngest generation filter"
  assertEqual ("length of " ++ show allTarsYoungest)
              (length allTarsYoungest) 100

  putStrLn "Checking that the newest generations are in the list"
  assertBool  "Checking correct generations #1 "
              (all (`elem` (map tName allTarsYoungest))         [ "data00053c.tar"
                                                                , "data00077c.tar"
                                                                , "data00085c.tar"
                                                                , "data00086a.tar"
                                                                ]
              )

  putStrLn "Checking that older generations are not in the list"
  assertBool  "Checking correct generations #2 "
              (all (not . (`elem` (map tName allTarsYoungest))) [ "data00053a.tar"
                                                                , "data00077a.tar"
                                                                , "data00085a.tar"
                                                                ]
              )
  where
    tars c = map (("data" ++) . reverse . ("rat." ++)) . nums
      where
        nums = map (c :) . map (take 5 . reverse) . map (("00000" ++) . show)

