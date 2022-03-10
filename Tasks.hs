{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
 
{-
   PP Project 2021
 
   This is where you will write the implementation for the given tasks.
   You can add other modules aswell.
-}
 
module Tasks where
 
import Dataset
import Text.Printf
import Data.List
 
type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

 
{-
   TASK SET 1
-}
 
-- Task 1
compute_exam_grades :: Table -> Table
compute_exam_grades [] = []
compute_exam_grades (x:xs)
   |(head x) == "Nume" = (build_first x):(compute_exam_grades xs)
   |otherwise = (build_line x):(compute_exam_grades xs)
 
--izoleaza notele de celelalte linii
ret_grades_str :: Row -> Row
ret_grades_str row = tail(init row)
 
--inlocuieste stringul null cu '0'
replace_null :: Row -> Row
replace_null row = map op (ret_grades_str row)
   where op "" = "0"
         op x = x
 
--transforma notele din String in float
ret_grades_float :: Row -> [Float]
ret_grades_float row = map (read :: String -> Float) (replace_null row)
 
--calculeaza nota dupa formula
calculate_grade :: Row -> Float
calculate_grade row = (sum(ret_grades_float row) / 4) + (read (last row) :: Float)
 
--construieste o linie din tabelul rezultat
build_line :: Row -> Row
build_line [] = []
build_line row = (row !! 0):[printf "%.2f" (calculate_grade row)]
 
--construieste prima linie
build_first :: Row -> Row
build_first row = (row !! 0):["Punctaj Exam"]
 
 
 
-- Task 2
 
--a
 
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num [] = 0
get_passed_students_num (x:xs)
   |(head x) == "Nume" = get_passed_students_num(compute_exam_grades xs)
   |otherwise = check_passed x + get_passed_students_num(compute_exam_grades xs)
 
--verifica daca un student a trecut
check_passed :: Row -> Int
check_passed row
   |(last row) < "2.5" = 0
   |otherwise = 1
 
--b
 
--Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = (fromIntegral
   (get_passed_students_num table) :: Float) / (get_students_number table)
 
--returneaza numarul total de studenti
get_students_number :: Table -> Float
get_students_number table = (fromIntegral (length table) :: Float) - one
 
one = 1;
 
--c
 
-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table = (get_grade_sum table) / (get_students_number table)
 
--returneaza nota fiecarui student in float
get_grade_row :: Row -> Float
get_grade_row row = read (last row) :: Float
 
--calculeaza suma tuturor notelor, pentru a face media
get_grade_sum :: Table -> Float
get_grade_sum [] = 0.0
get_grade_sum (x:xs)
   |(head x) == "Nume" = get_grade_sum (compute_exam_grades xs)
   |otherwise = (get_grade_row x) + get_grade_sum (compute_exam_grades xs)
 
--d
 
-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num [] = 0
get_passed_hw_num (x:xs)
   |(head x) == "Nume" = (get_passed_hw_num xs)
   |otherwise = (check_hw_passed x) + (get_passed_hw_num xs)
 
--izoleaza coloanele cu notele din teme
get_hw_grade_row :: Row -> Row
get_hw_grade_row row = drop 2 (take 5 row)
 
--inlocuieste "" cu "0"
replace_null_hw :: Row -> Row
replace_null_hw row = map op (get_hw_grade_row row)
   where op "" = "0"
         op x = x
 
--returneaza notele in float
ret_hw_float :: Row -> [Float]
ret_hw_float row = map (read :: String -> Float) (replace_null_hw row)
 
--verifica daca un student are nota necesara pe teme
check_hw_passed :: Row -> Int
check_hw_passed row
   |sum(ret_hw_float row) >= 1.5 = 1
   |otherwise = 0
 
 
 
-- Task 3
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = [(build_first_line table)] ++ [(change_float table)]
 
--construieste antetul tabelei
build_first_line :: Table -> Row
build_first_line table = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]
 
--returneaza tabela de note in float
get_grades_table :: Table -> [[Float]]
get_grades_table table = map ret_grades_float (tail table)
 
--calculeaza suma pt fiecare intrebare
get_sum_questions :: Table -> [Float]
get_sum_questions table = map sum (transpose (get_grades_table table))
 
--face media pentru fiecare intrebare
make_avg :: Table -> [Float]
make_avg table = map (/(get_students_number table)) (get_sum_questions table)
 
--transforma lista de medii din [Float] in [String]
change_float :: Table -> Row
change_float table = map (printf "%.2f") (make_avg table)
 
 
 
-- Task 4
get_exam_summary :: Table -> Table
get_exam_summary table = [(build_first_line4 table)] ++ [second_line_res table]
    ++ [third_line_res table] ++ [forth_line_res table] ++ [fifth_line_res table]
    ++ [sixth_line_res table] ++ [seventh_line_res table]
 
--construieste prima linie
build_first_line4 :: Table -> Row
build_first_line4 table = ["Q","0","1","2"]
 
--numara cate note de 0 sunt date la o intrebare
count_zeroes_line :: [Float] -> Value
count_zeroes_line row = show (length(filter ( == 0.0) row))
 
--numara cate note de 1 sunt date la o intrebare
count_ones_line :: [Float] -> Value
count_ones_line row = show (length(filter ( == 1.0) row))
 
--numara cate note de 2 sunt date la o intrebare
count_twos_line :: [Float] -> Value
count_twos_line row = show (length(filter ( == 2.0) row))
 
--returneaza tabela de note pt fiecare intrebare
get_grades_per_qs :: Table -> [[Float]]
get_grades_per_qs table = transpose (get_grades_table table)
 
--a doua linie din tabela rezultat
second_line_res :: Table -> Row
second_line_res table = ["Q1"] ++ [count_zeroes_line
   ((get_grades_per_qs table) !! 0)] ++ [count_ones_line ((get_grades_per_qs table) !! 0)]
   ++ [count_twos_line ((get_grades_per_qs table) !! 0)]
 
--a treia linie din tabela rezultat
third_line_res :: Table -> Row
third_line_res table = ["Q2"] ++ [count_zeroes_line
   ((get_grades_per_qs table) !! 1)] ++ [count_ones_line ((get_grades_per_qs table) !! 1)]
   ++ [count_twos_line ((get_grades_per_qs table) !! 1)]
 
--a patra linie din tabela rezultat
forth_line_res :: Table -> Row
forth_line_res table = ["Q3"] ++ [count_zeroes_line
   ((get_grades_per_qs table) !! 2)] ++ [count_ones_line ((get_grades_per_qs table) !! 2)]
   ++ [count_twos_line ((get_grades_per_qs table) !! 2)]
 
--a cincea linie din tabela rezultat
fifth_line_res :: Table -> Row
fifth_line_res table = ["Q4"] ++ [count_zeroes_line
   ((get_grades_per_qs table) !! 3)] ++ [count_ones_line ((get_grades_per_qs table) !! 3)]
   ++ [count_twos_line ((get_grades_per_qs table) !! 3)]
 
--a sasea linie din tabelul rezultat
sixth_line_res :: Table -> Row
sixth_line_res table = ["Q5"] ++ [count_zeroes_line
   ((get_grades_per_qs table) !! 4)] ++ [count_ones_line ((get_grades_per_qs table) !! 4)]
   ++ [count_twos_line ((get_grades_per_qs table) !! 4)]
 
--a saptea linie din tabelul rezultat
seventh_line_res :: Table -> Row
seventh_line_res table = ["Q6"] ++ [count_zeroes_line
   ((get_grades_per_qs table) !! 5)] ++ [count_ones_line ((get_grades_per_qs table) !! 5)]
   ++ [count_twos_line ((get_grades_per_qs table) !! 5)]
 
-- Task 5

get_ranking table = [(head (compute_exam_grades table))]  ++
   (switch_columns (sort (switch_columns (tail (compute_exam_grades table)))))
 
--interschimba termenii de pe o linie intre ei
switch_terms :: Row -> Row
switch_terms (x:y) = y ++ [x]
 
--aplica interschimbarea pe tot tabelul
switch_columns :: Table -> Table
switch_columns table = map switch_terms table
 
 
 
-- Task 6
get_exam_diff_table :: Table -> Table
get_exam_diff_table table = (build_first_line6 (head table)) : (switch_back_all6 table)
 
--construieste prima linie dim tabelul rezultat
build_first_line6 :: Row -> Row
build_first_line6 row =
   ["Nume", "Punctaj interviu" , "Punctaj scris" , "Diferenta"]
 
--calculeaza nota pe interviu
get_oral_grade :: Row -> Float
get_oral_grade row = (sum(ret_grades_float row) / 4)
 
--construieste o linie din tabelul rezultat
build_line6 :: Row -> Row
build_line6 row
   |(get_oral_grade row) > (read (last row) :: Float) = [head row] ++
       [(printf "%.2f" (get_oral_grade row))] ++ [(printf "%.2f" ( read (last row) :: Float))] ++
       [printf "%.2f" ((get_oral_grade row) - (read (last row) :: Float))]
   |otherwise = [head row] ++ [(printf "%.2f" (get_oral_grade row))] ++
       [(printf "%.2f" ( read (last row) :: Float))] ++
       [printf "%.2f" ((read (last row) :: Float) - (get_oral_grade row))]
 
--construieste tabelul rezultat
build_table6 :: Table -> Table
build_table6 table = map build_line6 (tail table)
 
--muta ultima intrare dintr o linie pe prima pozitie
switch_line6 :: Row -> Row
switch_line6 row = (last row) : (init row)
 
--aplica mutarea de mai sus pe tot tabelul rezultat
switch_all6 :: Table -> Table
switch_all6 table = map switch_line6 (build_table6 table)
 
--sorteaza tabela rezultat
sort6 :: Table -> Table
sort6 table = sort (switch_all6 table)
 
--readuce la forma initiala o linie tabela rezultat
switch_back_line6 :: Row -> Row
switch_back_line6 row = (tail row) ++ [(head row)]
 
--aplica mutarea de mai sus pe toata tabela
switch_back_all6 :: Table -> Table
switch_back_all6 table = map switch_back_line6 (sort6 table)
 
 
{-
   TASK SET 2
-}
 
 
--Prerequisite
 
-- READ
 
read_csv :: CSV -> Table
read_csv table = map (split_commas comma) (split_lines backslash_n table)
 
backslash_n = '\n'
comma = ','
 
split_lines :: Char -> CSV -> Row
split_lines _ [] = [""]
split_lines delimiter (c:cs)
   |c == delimiter = "" : (split_lines delimiter cs)
   |otherwise = (c : (head (split_lines delimiter cs))) : tail (split_lines delimiter cs)
 
string_to_row :: Value -> Row
string_to_row v = [v]
 
split_commas :: Char -> Value -> Row
split_commas _ [] = [""]
split_commas delimiter (h:t)
   |h == delimiter = "" : (split_commas delimiter t)
   |otherwise = (h : (head (split_commas delimiter t))) : tail (split_commas delimiter t)
 
form_table_aux :: Value -> Table
form_table_aux table = map (split_commas comma) (split_lines backslash_n table)
 
-- WRITE
 
write_csv :: Table -> CSV
write_csv table = delete_last (write_csv_aux table)
 
write_csv_aux :: Table -> CSV
write_csv_aux [] = ""
write_csv_aux (h:t) = (row_to_string h) ++ (write_csv_aux t)
 
row_to_string :: Row -> String
row_to_string row = (intercalate "," row) ++ "\n"
 
delete_last :: CSV -> CSV
delete_last str = reverse(drop 1 (reverse str))
 
-- Task 1
 
as_list :: String -> Table -> [String]
as_list str table = concat (tail (map (take 1) (show_col_aux table str)))
 
get_number :: Row -> String -> Int -> Int
get_number [] _ _ = 100
get_number (x:xs) str n
   |x == str = n
   |otherwise = get_number xs str (n + 1)
 
show_col_aux :: Table -> String -> Table
show_col_aux table str = map (drop ((get_number (head table) str 1 ) - 1) ) table
 
 
-- Task 2 
 
tsort :: String -> Table -> Table
tsort str table = (head table) : (sortBy (sort2_2 (get_number (head table) str 1)) (tail table))

sort2_2 :: Int -> Row -> Row -> Ordering
sort2_2 index r1 r2 
   |r1 !! (index-1) > r2 !! (index-1) = GT
   |r1 !! (index-1) == r2 !! (index-1) && (head r1) > (head r2) = GT
   |otherwise = LT

-- Task 3
 
vmap :: (Value -> Value) -> Table -> Table
vmap func = map (map func)
 
-- Task 4
 
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap func h table = h : (map func (tail table))
 
get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row) : [(get_grades_hw_row row)]
 
get_grades_hw_row :: Row -> Value
get_grades_hw_row row = (printf "%.2f") (sum (map (read :: String -> Float) (null_zero (drop 2 row))))
 
null_zero :: Row -> Row
null_zero = map op
   where op "" = "0"
         op x = x
 
-- Task 5
 
vunion :: Table -> Table -> Table
vunion t1 t2
   |(test_vunion (head t1) (head t2) True) == True = t1 ++ (tail t2)
   |otherwise = t1
 
 
test_vunion :: Row -> Row -> Bool -> Bool
test_vunion [] [] _ = True
test_vunion [] _ _ = False
test_vunion _ [] _ = False
test_vunion (x1:xs1) (x2:xs2) bool
   |x1 == x2 = test_vunion xs1 xs2 bool
   |otherwise = False
 
 
-- Task 6
 
hunion :: Table -> Table -> Table
hunion t1 t2 = hunion_aux t1 t2 t1 t2
 
hunion_aux :: Table -> Table -> Table -> Table -> Table
hunion_aux [] [] _ _ = []
hunion_aux [] (x2:xs2) t1 t2 = ((generate_empty (head t1)) ++ x2) : (hunion_aux [] xs2 t1 t2)
hunion_aux (x1:xs1) [] t1 t2 = (x1 ++ (generate_empty (head t2))) : (hunion_aux xs1 [] t1 t2)
hunion_aux (x1:xs1) (x2:xs2) t1 t2 = (x1 ++ x2) : (hunion_aux xs1 xs2 t1 t2)
 
generate_empty :: Row -> Row
generate_empty [] = []
generate_empty (x:xs) = "" : generate_empty(xs)
 
 
-- Task 7
 
tjoin :: String -> Table -> Table -> Table
tjoin str t1 t2 = tjoin_aux t1 t2

tjoin_aux :: Table -> Table -> Table
tjoin_aux [] _ = []
tjoin_aux _ [] = []
tjoin_aux t1@(x1:xs1) t2@(x2:xs2)
   |(head x1) == (head x2) = [x1 ++ (tail x2)] ++ (tjoin_aux xs1 xs2)
   |otherwise = [x1 ++ (tail (generate_empty x2))] ++ (tjoin_aux xs1 t2)

 
-- Task 8
 
cartesian_aux :: (Row -> Row -> Row) -> Table -> Table -> Table
cartesian_aux _ [] _ = []
cartesian_aux _ _ [] = []
cartesian_aux fct t1 t2 = (map (fct (head t1)) t2) ++ (cartesian_aux fct (tail t1) t2)
 
cartesian ::(Row -> Row -> Row) -> Row -> Table -> Table -> Table
cartesian fct c t1 t2 = [c] ++ (cartesian_aux fct (tail t1) (tail t2))
 
-- Task 9
 
projection :: [String] -> Table -> Table
projection list t1 = projection_aux list t1 t1
 
projection_aux :: [String] -> Table -> Table -> Table
projection_aux _ [] _ = []
projection_aux l (x:xs) t1 = (projection_one l t1 x) : (projection_aux l xs t1)   
 
projection_one_aux :: String -> Table -> Row -> Row
projection_one_aux str t1 r = take 1 (drop ((get_number (head t1) str 1) - 1) r)
 
projection_one :: [String] -> Table -> Row -> Row
projection_one [] _ _ = []
projection_one (x:xs) t1 r = (projection_one_aux x t1 r) ++ (projection_one xs t1 r)
 
{-
   TASK SET 3
-}
 
data Query =
   FromCSV CSV
   | ToCSV Query
   | AsList String Query
   | Sort String Query
   | ValueMap (Value -> Value) Query
   | RowMap (Row -> Row) [String] Query
   | VUnion Query Query
   | HUnion Query Query
   | TableJoin String Query Query
   | Cartesian (Row -> Row -> Row) [String] Query Query
   | Projection [String] Query
   | forall a. FEval a => Filter (FilterCondition a) Query
   | Graph EdgeOp Query

type EdgeOp = Row -> Row -> Maybe Value
data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
   show (CSV csv) = show csv
   show (Table table) = write_csv table
   show (List list) = show list
 
class Eval a where
   eval :: a -> QResult

qToTable :: Query -> Table
qToTable q = read_csv (show (eval q))

-- Task 1

instance Eval Query where
   eval (FromCSV csv) = Table (read_csv csv)
   eval (ToCSV q) = CSV (show (eval q))
   eval (AsList str q) = List (as_list str (qToTable q))
   eval (Sort str q) = Table (tsort str (qToTable q))
   eval (ValueMap f q) = Table (vmap f (qToTable q))
   eval (RowMap f col q) = Table (rmap f col (qToTable q))
   eval (VUnion q1 q2) = Table (vunion (qToTable q1) (qToTable q2))
   eval (HUnion q1 q2) = Table (hunion (qToTable q1) (qToTable q2))
   eval (TableJoin str q1 q2) = Table (tjoin str (qToTable q1) (qToTable q2))
   eval (Cartesian f list q1 q2) = Table (cartesian f list (qToTable q1) (qToTable q2))
   eval (Projection l q) = Table (projection l (qToTable q))
   eval (Filter condition q) = Table ((head (qToTable q)) : (filter (feval (head (qToTable q)) condition) (tail (qToTable q))))

-- 2

data FilterCondition a =
   Eq String a |
   Lt String a |
   Gt String a |
   In String [a] |
   FNot (FilterCondition a) |
   FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
   feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
   feval list (Eq str ref) = \row -> ((get_element_float list str row) == ref)
   feval list (Lt str ref) = \row -> ((get_element_float list str row) < ref)
   feval list (Gt str ref) = \row -> ((get_element_float list str row) > ref)
   feval list (In str ref) = \row -> (get_number_float ref (read ((null_zero row) !! (get_number list str 0)) :: Float) 0 < 100)
   feval list (FNot cond) = \row -> (not((feval list cond row)))
   feval list (FieldEq str ref) = \row -> ((get_element_float list str row) == (get_element_float list ref row))
   
instance FEval String where
   feval list (Eq str ref) = \row -> ((get_element_string list str row) == ref)
   feval list (Lt str ref) = \row -> ((get_element_string list str row) < ref)
   feval list (Gt str ref) = \row -> ((get_element_string list str row) > ref)
   feval list (In str ref) = \row -> (get_number ref (row !! (get_number list str 0)) 0 < 100)
   feval list (FNot cond) = \row -> (not((feval list cond row)))
   feval list (FieldEq str ref) = \row -> ((get_element_string list str row) == (get_element_string list ref row))
    
similarities_query :: Query
similarities_query = undefined

get_element_float :: [String] -> String -> Row -> Float
get_element_float list str row = read (row !! (get_number list str 0)) :: Float

get_element_string :: [String] -> String -> Row -> String
get_element_string list str row = row !! (get_number list str 0)

get_number_float :: [Float] -> Float -> Int -> Int
get_number_float [] _ _ = 100
get_number_float (x:xs) str n
   |x == str = n
   |otherwise = get_number_float xs str (n + 1)


{-
   TASK SET 4
-}

-- Task 1
correct_table :: [Char] -> [Char] -> [Char] -> String
correct_table = undefined

-- Task 2
grades :: [Char] -> [Char] -> [Char] -> [Char] -> String
grades = undefined
