-- Prática 02 de Haskell
-- Nome: Thiago Rodrigues Silva

temFebre :: Float -> Bool
temFebre f = f > 37.8

comFebre :: [Float] -> [Float]
comFebre listF = filter temFebre listF

comFebre' :: [Float] -> [Float]
comFebre' listF = filter (\f -> f > 37.8) listF

itemize :: [String] -> [String]
itemize listStr = map (\s -> "<li>"++s++"</ li>") listStr

bigCircles :: Float -> [Int] -> [Float]
bigCircles f listInt = map fromIntegral (filter (\x -> (fromIntegral x) ^ 2 * pi > f) listInt)

pessoaComFebre :: (String, Float) -> Bool
pessoaComFebre pessoa = (snd pessoa) > 37.8

quarentena :: [(String,Float)] -> [(String,Float)]
quarentena pessoas = filter pessoaComFebre pessoas

idadesEm :: [Int] -> Int -> [Int]
idadesEm listAnos ano = map (\x -> ano - x) listAnos 

changeNames :: [String] -> [String]
changeNames nameList = map 
  (\name -> case () of 
  _ | (take 1 name) == "A" -> "Super "++name 
    | otherwise -> name) nameList


onlyShorts :: [String] -> [String]
onlyShorts strList = filter (\name -> length name < 5) strList