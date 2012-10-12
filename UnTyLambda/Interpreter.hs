{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать им енованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

--- ...

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого 
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = case (fst p) of
	True -> no (n - 1) (snd p)
	False -> (snd p)
	where p = saitr t

saitr :: Term -> (Bool, Term)
saitr (Lam x y) = (fst k, Lam x (snd k))
	where k = saitr y
saitr (App (Lam x y) z) = case (fst t) of
	True -> (True, App (Lam x y) (snd t))
	False -> (True, betaR x y z)
	where t = saitr z
saitr (App x y) = case (fst t) of
	True -> (True, App (snd t) y)
	False -> (fst k, App x (snd k)) 
	where 
	  t = saitr x
	  k = saitr y
saitr t = (False, t)

                  

-- Нормализация нормальным порядком
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = case (fst p) of
	True -> no (n - 1) (snd p)
	False -> (snd p)
	where p = noitr t

noitr :: Term -> (Bool, Term)
noitr (Var x) = (False, (Var x))
noitr (Lam x y) = (fst k, Lam x (snd k))
	where k = noitr y
noitr (App (Lam x y) z) = (True, betaR x y z)
noitr (App x y) = case (fst t) of
	True -> (True, App (snd t) y)
	False -> (fst k, App x (snd k)) 
	where 
	  t = noitr x
	  k = noitr y

betaR :: Variable -> Term -> Term -> Term
betaR var w t = case w of
	Var x -> case (x == var) of
		  True -> t
		  False -> w
	Lam x y -> case elem x (free t) of
		     True -> Lam nn (betaR var (subst y x (Var nn)) t)
			where nn = newname (free t ++ free y) x
		     False -> Lam x (betaR var y t) 
	App x y -> App (betaR var x t) (betaR var y t) 


-- Редукция в слабую головную нормальную форму
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = case (fst p) of
	True -> wh (n - 1) (snd p)
	False -> (snd p)
	where p = whitr t

whitr :: Term -> (Bool, Term)
whitr (App (Lam x y) z) = (True, betaR x y z)
whitr (App x y) = case (fst t) of
	True -> (True, App (snd t) y)
	False -> (fst k, App x (snd k)) 
	where
	  t = whitr x
	  k = whitr y 
whitr x = (False, x)
                   
-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).