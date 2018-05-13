import Data.Array
import Data.List

pindices :: (Enum t, Num t, Ord t) => t -> t -> t -> [(t, t)]
pindices degree1 degree2 current
  | current > prod_degree = []
  | otherwise = [(i, current - i) | i <- [(max 0 (current - degree2)) .. (min current degree1)]]
  where prod_degree = degree1 + degree2

enumerate xs = zip [0 ..] xs

outer xs ys = [((i, j), ei * ej) | (i, ei) <- enumerate xs, (j, ej) <- enumerate ys ]

poly_product xs ys =
  map coeff_sum [0 .. (degree1 + degree2)]
  where
    prod_matrix = array ((0, 0), (degree1, degree2)) $ outer xs ys
    coeff_sum current = sum . map (\i -> prod_matrix!i) $ pindices degree1 degree2 current
    degree1 = (length xs) - 1 
    degree2 = (length ys) - 1

show_monomial (i, x)
  | i == 0 = show x
  | i == 1 = (show x) ++ "x"
  | otherwise = (show x) ++ "x^" ++ (show i)

show_poly xs =
  intercalate " + " $ reverse $ map show_monomial $ enumerate xs

poly_sum xs ys
  | length xs < length ys = poly_sum ys xs
  | otherwise = (map (\(x, y) -> x + y) $ zip xs ys) ++ (drop (length ys) xs)
