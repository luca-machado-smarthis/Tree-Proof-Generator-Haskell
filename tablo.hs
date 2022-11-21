import Data.Char
import Data.List

---                    --- Essa seção se concentram as declarações especificas de 
--- Estrutura de dados --- "data's" utilizados no programa bem como instancias de 
---                    --- algumas classes e funções auxiliares para tais instancias

-- Pode assumir o valor de 'Skip', que é meramente auxiliar para indicar que não
-- haverá propagação de Uma função na hora da ramificação. Quando a função é 
-- atomica, atomic será == True e rside será == "". o campo valid se refere se a
-- formula estiver negada ou não
data Formula = Skip | Formula
  { lside :: String,
    rside :: String,
    op :: Char,
    atomic :: Bool,
    valid :: Bool
  } deriving(Eq)

-- Assume 'Empty' quando o nó for vazio
data No
  = Empty
  | No
      { formulas :: [Formula],
        folha :: Bool,
        lno :: No,
        rno :: No
      }
  deriving (Show, Eq)

showBool :: Bool -> String
showBool False = "F:"
showBool True = "V:"

-- showAtomic :: Bool -> String
-- showAtomic False = ""
-- showAtomic True = "@"

instance Show Formula where
    show (Formula a b c d e) 
                    | not d = showBool e ++ show ( a ++ [c] ++ b ++ ['/'])
                    | otherwise = showBool e ++ show a

---                      --- Nessa seção estão todas as formulas usadas para tratar strings 
--- Tratamento de String --- de tal modo que as transformarem em formulas (a estrutura) além 
---                      --- do tratamento necessário de remoção de parenteses e negações

-- Todos os connectivos lógicos usados no trabalho 
-- _:or ^:and >:imply
connectives = ['_', '^', '>']

-- Todos os connectivos lógicos mais o simbolo de negação 
-- ~:negação
symbols = '~' : connectives

--- Achar o connector ---    Funções usadas para achar o conector, que é tanto 
                          -- essencial para saber a operação quanto para divir 
                          -- a formula em 2

-- Função principal para achar os conectores, caso a formula seja atomica
-- (Ou seja, sem connectores) devolve (-1) que será usado por demais formulas
findOP :: String -> Int
findOP str
  | isStringAtomic str = -1
  | onlyOneConnective str = findFirstConnective str
  | otherwise = findCentralOP str 0 False

--Acha o conctor quando a formula só tem 1 conector. Não depende da contagem de parenteses
findFirstConnective :: String -> Int
findFirstConnective str = head [y | (x, y) <- zip str [0 ..], x `elem` connectives]

-- Como o nome sugere, acha o conector central contando o número de parenteses
-- quando ele acha o primeiro '(' a contagem começa com 1, aumentando a cada '('
-- e diminuindo a cada ')', quando volta a ser zero, é porque ele saiu do primeiro bloco
-- e encontrou o conector cental
findCentralOP :: String -> Int -> Bool -> Int
findCentralOP ('~' : xs) _ False = 1 + findCentralOP xs 0 False
findCentralOP (x : xs) _ False
  | isAlpha (head (x : xs)) || (x == '~') = findFirstConnective (x : xs)
  | otherwise = 1 + findCentralOP xs 1 True
findCentralOP (x : xs) 0 True = 0
findCentralOP (x : xs) num True
  | x == '(' = 1 + findCentralOP xs (num + 1) True
  | x == ')' = 1 + findCentralOP xs (num - 1) True
  | otherwise = 1 + findCentralOP xs num True
findCentralOP [] _ _ = 0

-- checa a ausencia de parentesis, o que indica que é uma formula atomica ou 2
-- formulas atomicas. Apesar do nome, também pega formulas atomicas que (nao tem conectivo)
-- mas essas já tem sua propria função para serem identificadas com maior precisão
onlyOneConnective :: String -> Bool
onlyOneConnective str
  | parenthesisCount str > 0 = False
  | otherwise = True

parenthesisCount :: String -> Int -- \x -> x =='(' --
parenthesisCount str = length $ filter (== '(') str

-- verifica pela presença de connectivos na String da formula
-- se ela é atômica
isStringAtomic :: String -> Bool
isStringAtomic str
  | any (`elem` connectives) str = False
  | otherwise = True

removeParenthesis :: String -> String
removeParenthesis str -- init (tail str)
  | head str == '(' && last str == ')' = init $ tail str
  | otherwise = str

-- separa uma fórmula em duas , devolvendo o lado esquerdo e o direito, bem como o conector
-- Na função "findOP" pode retornar o idx como -1, indicando que a formula é atômica
breakdownFormula :: String -> Int -> (String, Char, String)
breakdownFormula str (-1) = (str, '@', "")
breakdownFormula str idx = (take idx str, str !! idx, drop (idx + 1) str)

--Também remove parenteses--
checkForNegation :: String -> (String, Bool)
checkForNegation str
  | head str /= '~' = (removeParenthesis str, True)
  | otherwise = (drop 2 . init $ str, False)

--Usada apenas na entrada inicial por não remover os parenteses junto
checkForInitialNegation:: String -> (String, Bool)
checkForInitialNegation str
  | head str /= '~' = (str, True)
  | otherwise = (drop 2 . init $ str, False)

--usa da tupla gerado por "breakdownFormula" para construir uma formula
formulaParser :: (String, Char, String) -> Bool -> Bool -> Formula
formulaParser (str1, conec, str2) isatomic valid | conec == '@' = Formula str1 str2 conec True valid
                            | otherwise = Formula str1 str2 conec isatomic valid

---                           --- Seção onde estão as função relacionadas a criação
--- Arvore e resolução tableu --- da arvore e resolução do tableu 
---                           --- 

 
buildTree :: No -> No
buildTree no | no == Empty = Empty
             |otherwise = No (formulas (rPF no)) (folha (rPF no)) (buildTree (lno (rPF no))) (buildTree (rno (rPF no))) 

-- rpf = ramification plus fill 
-- aplica ramificação após a função de fill 
rPF :: No -> No
rPF no = ramificationNo (No (fillNo (formulas no) 0) (folha no) (lno no) (rno no)) 0

--resolve as fórmulas de ramificação no nó atual ramificação no nó atual
ramificationNo :: No -> Int -> No
ramificationNo no idx 
                    | idx >= length(formulas no) = no
                    | otherwise = ramificationNo (percorre no (resolveOtherbranch ((formulas no) !! idx))) (idx+1)

-- resolve as fórmulas que não ramificam
fillNo :: [Formula] -> Int -> [Formula]
fillNo formulas idx
  | idx >= length formulas = formulas
  | otherwise = fillNo (formulas ++ resolveSameBranch (formulas !! idx)) (idx+1)

-- Percorre a arvore aplicando as formulas recebidas por 'resolvesamebranch'
-- até chegar nas folhas e criar novos nos a partir delas. se não chegar nas folhas
-- continua percorrendo até achar os nos folha
percorre :: No -> [Formula] -> No
percorre no forms
    | head forms == Skip = no
    | folha no = No (formulas no) False (No [forms !! 0] True Empty Empty) (No [forms !! 1] True Empty Empty)
    | otherwise = No (formulas no) (folha no) (percorre (lno no) forms) (percorre (rno no) forms)

-- resolve os predicados que ramificam (por isso "other branch")
-- caso não se aplique (sendo uma formula atomica por exemplo)
-- ele retorna "Skip" para que a função "percorre" saiba que essa 
-- formula tem que ser ignorada
resolveOtherbranch :: Formula -> [Formula]
resolveOtherbranch form
  | op form == '>' && valid form =  resolver (lside form) False : [resolver (rside form) True] 
  | op form == '^' && not (valid form) = resolver (lside form) False : [resolver (rside form) False] 
  | op form == '_' && valid form = resolver (lside form) True : [resolver (rside form) True] 
  | otherwise = [Skip]

-- Resolve os predicados que ficam na mesma branch
-- caso não se aplique (sendo uma formula atomica por exemplo)
-- ele retorna uma lista vazia indicando que nada será acrescentado
-- a lista de formulas do branch atual
resolveSameBranch :: Formula -> [Formula]
resolveSameBranch form
  | op form == '>' && not (valid form) =  resolver (lside form) True : [resolver (rside form) False] 
  | op form == '^' && valid form = resolver (lside form) True : [resolver (rside form) True] 
  | op form == '_' && not (valid form) = resolver (lside form) False : [resolver (rside form) False] 
  | otherwise = []

-- Função usada tanto em resolveSameBranch quanto resolveOtherBranch
-- Resolve de fato as formulas indicadas pelo predicado, aplicando
-- "breakdownFormula" em cada lado da formula
resolver :: String -> Bool -> Formula
resolver str is
            | is = formulaParser (breakdownFormula (fst  (checkForNegation str)) (findOP (fst  (checkForNegation str)))) False (snd (checkForNegation str))
            | otherwise = formulaParser (breakdownFormula (fst  (checkForNegation str)) (findOP (fst  (checkForNegation str)))) False (not (snd (checkForNegation str)))

--Desenha a arvore para que ela possa ser printada e facilmente vizualizada
drawTree :: No -> Int -> String
drawTree no depth | no /= Empty = take (5*depth) (repeat '-') ++ show (formulas no) ++ "\n" ++ drawTree (lno no) (depth+1) ++ "\n" ++ drawTree (rno no) (depth+1)
                   | otherwise = take (5*depth) (repeat '-') ++ show "_vazio_"

---           --- Seção onde as função são responsáveis pela validação da arvore
--- Validação --- e , se necessário, geração do contra modelo
---           --- 

-- Verifica se uma formula é atomica, (pela formula, não pela
-- String nela contida)

isFormulaAtomic :: Formula -> Bool
isFormulaAtomic form | atomic form = True
                     | otherwise = False

filterAtomic :: [Formula] -> [Formula]
filterAtomic forms = (filter isFormulaAtomic forms)

-- Não esquecer de chamar com list vazia
getRamosForvalidation :: No -> [Formula] -> [[Formula]]
getRamosForvalidation no forms | folha no = [forms ++ (filterAtomic (formulas no))]
                    | otherwise = (getRamosForvalidation (lno no) (forms++(filterAtomic (formulas no)))) ++ (getRamosForvalidation (rno no) (forms++(filterAtomic (formulas no))))

checkForContradiction :: [Formula] -> Bool
checkForContradiction (x:xs) | length [y | y<-xs, lside x == lside y, valid x /= valid y] > 0 = True
                          | otherwise = checkForContradiction xs
checkForContradiction [] = False

validation :: [[Formula]] -> [Bool]
validation x = map checkForContradiction x

showValidation :: [Bool] -> String
showValidation list | any (\x -> x == False) list = "Formula invalida, pelo menos 1 dos ramos nao demonstrou contradição"
                    | otherwise = "Formula Valida"
--- EXEMPLOS ---

ex1 = "(p_(q^r))>((p_q)^(p_r))"
ex2 = "a>(a>(b>a))"
ex3 = "b>(a^(b_a))"

-- NÃO FINAL --
main = do
    let test = checkForInitialNegation ex2
    let op_idx = findOP (fst test)
    let initial = formulaParser  (breakdownFormula (fst test) op_idx) False (not (snd test))
    let initial_node = No [initial] True Empty Empty
    let noo = buildTree initial_node
    putStr (drawTree noo 0)
    print (showValidation (validation (getRamosForvalidation noo [])))