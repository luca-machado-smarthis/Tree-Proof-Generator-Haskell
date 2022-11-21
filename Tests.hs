module Tests where

import Tableu

ex1 = "(p_(q^r))>((p_q)^(p_r))"
ex2 = "a>(a>(b>a))"
ex3 = "b>(a^(b_a))"

choiceSelection :: String -> String
choiceSelection "1" = ex1
choiceSelection "2" = ex2
choiceSelection "3" = ex3

testing = do
    putStrLn ("1: " ++ ex1)
    putStrLn ("2: " ++ ex2)
    putStrLn ("3: " ++ ex3)
    putStrLn "Escolha um dos casos de teste acima digitando o numero correspondente\n"
    input <- getLine
    let n = input
    let initial_formula = checkForInitialNegation (choiceSelection n)
    let op_idx = findOP (fst initial_formula)
    let parsed = formulaParser  (breakdownFormula (fst initial_formula) op_idx) False (not (snd initial_formula))
    let initial_node = No [parsed] True Empty Empty
    let noo = buildTree initial_node
    putStr (drawTree noo 0)
    print (showValidation (validation (getRamosForvalidation noo [])))