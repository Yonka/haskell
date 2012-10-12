{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- � ������ ������� �ॡ���� ॠ�������� �������� ���
-- ��⨯���஢����� �ﬡ��
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- �����-� �������. �������, �� � �⮬ ������� ����
-- �ᯮ�짮���� ������ Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- ��।������ ���⠩�� ��� ��⨯���஢����� �ﬡ��
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- ����� ��� �� ��� �ᬮ�७��

-- �᫨ ����� �㤥� �ᯮ�짮���� �� ��������� �।�⠢�����, �
-- � ��� �訫 ������� ��� ������
-- (���� ������, ��� �� �⮣� ࠧ���� ����� ᮢᥬ �모����,
-- �᫨ �����)

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
-- �� �᪫�祭��� ⮣�, �� �ॡ���� ॠ�������� ᫥���騥
-- ���⥣�� ��ଠ����樨 (��� �� �ਭ����� ���ᨬ��쭮�
-- �᫮ 蠣�� �������� � ����⢥ ��ࢮ�� 
-- ��ࠬ��� (n); �᫨ �� n 蠣�� ��ଠ�������� �� 㤠����,
-- � ᫥��� ����� error, ���� ��� �������):

wh, no, wa, sa :: Integer -> Term -> Term

-- ������ �������⨢�� ���浪��
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

                  

-- ��ଠ������ ��ଠ��� ���浪��
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


-- ������ � ᫠��� �������� ��ଠ���� ���
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
                   
-- (*) (�� ��易⥫쭮) ������ "᫠��" �������⨢�� ���浪��.
-- �⫨砥��� �� ���筮�� �������⨢���� ⥬, �� �� ����� ������
-- �ﬡ� � �ࠢ� ��� �������権, ����� �� ��������.
wa = undefined

-- ����砭��: c������ ࠡ��� ��襣� �������� ᯥ樠�쭮 �� �業�������,
-- ��⮬� ����� �ᯮ�짮���� ᢮� �������� (� �筮���� �� ����-������ᨨ)
-- ⨯ ��� �।�⠢����� �ମ� � �८�ࠧ��뢠�� Term � ���� � ���⭮.

-- ����᫥��� ��� ��� ���浪�� (� ���浪� �⫨筮� ��
-- ��।������, ��)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- ����� �᪮����஢���, ��
    , ("sa", sa) ]

------------------------------------------------------------
-- �������� ��, �᫨ �룫廊� ������⭮
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- � ����� ��������� ����
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- ������� ⥮���᪨� ����砭��, �᫨ ��� ��� �������
--
-- ������ ᯥ樠�쭮 �⬥���, �� ��᪮��� � ���� ���
-- १���� ���᫥��� ������, � ��������� Haskell ��
-- ����� �� ᥬ��⨪� �������㥬��� ���᫥���.
--
-- �⮡� �� �ᮡ���� ����ભ��� � ���� ��� � ����ᠫ
-- seq � ����᭮� ���� (��� �����⭮ �� ⠬ ��祣� ��
-- ��࠭����, �� ᠬ��-� ����).