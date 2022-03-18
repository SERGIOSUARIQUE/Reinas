import Data.List (delete)
type Tablero = [Int]

reinas :: Int -> [Tablero]
reinas n = map fst $ loop [([], [1..n])] 0
  where
    loop :: [(Tablero, [Int])] -> Int -> [(Tablero, [Int])]
    loop tableros contador
      | contador == n = tableros
      | otherwise = loop (concatMap expandir tableros) (contador+1)

    expandir :: (Tablero, [Int]) -> [(Tablero, [Int])]
    expandir (tablero, candidatos) =
      [(x : tablero, delete x candidatos) | x <- candidatos, safe x tablero]

    safe x tablero = and [ x /= c + n && x /= c - n  | (n,c) <- zip [1..] tablero]
    
main = do
    print "Ingrese el tamanio del tablero: "
    tablero_size <- getLine
    let n = (read tablero_size:: Int)
    print (reinas n)