{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- Можно писать лямбды в своё удовольствие.
{- HLINT ignore "Redundant lambda" -}

-- | Домашка 4. Введение в Haskell: уровень 2.
--
-- Условие каждой задачи — комментарий перед её кодом; решение пишется на месте заглушки @todo@,
-- а в задаче 2.6 — на месте заглушки 'Todo' в типе.
module Level2 where

import Level1 (pair, fstChurch, sndChurch)
import MetaUtils (todo)
import TypeCheck (Todo)


-- 2.1. Варианты из λ-исчисления
--
-- Оттранслируйте в Haskell варианты в стиле чистого λ-исчисления (по Чёрчу): термы inl,
-- inr и either. Имя either в Haskell занято стандартной функцией, ваша называется eitherChurch.

inl x = \f _ -> f x
inr x = \_ g -> g x
eitherChurch f g v = v f g


-- 2.2. Предыдущее число
--
-- Оттранслируйте в Haskell λ-терм, используя пары из задачи 1.1:
-- pred := λn s z. snd (n (λp. pair (s (fst p)) (fst p)) (pair z z))
-- В Haskell это predChurch, а fst и snd в нём — ваши fstChurch и sndChurch.
-- Ознакомьтесь с тем, как этот код тестируется в test/SpecLevel2.hs.

predChurch n s z = sndChurch $ n (\p -> pair (s (fstChurch p)) (fstChurch p)) (pair z z)

-- 2.3. Простые числа
--
-- Реализуйте проверку числа на простоту.

isPrime :: Integer -> Bool
isPrime n = go 2 n
  where
    go iter n'
      | n' <= 1             = False
      | iter * iter > n'    = True
      | n' `mod` iter == 0  = False
      | otherwise           = go (iter + 1) n'


-- 2.4. Множество как функция
--
-- Известно, что множество задаётся своей характеристической функцией:
-- то есть функцией, которая возвращает False на значении, если такого элемента
-- в множестве нет, и True — если есть.
-- Реализуйте функции для работы с множеством строк, заданным такой функцией:
--    emptySet — задаёт пустое множество: emptySet "something" == False
--    (+++) — оператор добавления элемента в множество: (set +++ "elem") "elem" == True
--      Он должен быть левоассоциативным с 5-м приоритетом.
--    (///) — оператор удаления элемента из множества.
--      Он должен быть левоассоциативным с 5-м приоритетом.
-- Пример множества {"a", "b", "c"}: emptySet +++ "a" +++ "b" +++ "c".

emptySet :: String -> Bool
emptySet _ = False

-- TODO объявление приоритета и ассоциативности
infixl 5 +++
(+++) :: (String -> Bool) -> String -> (String -> Bool)
f +++ p = go
  where
    go s
      | s == p        = True
      | otherwise     = f s

-- TODO объявление приоритета и ассоциативности
infixl 5 ///
(///) :: (String -> Bool) -> String -> (String -> Bool)
f /// p = go
  where
    go s
      | s == p      = False
      | otherwise   = f s


-- 2.5. Функция по типу
--
-- Реализуйте функцию, применяющую функцию к первой компоненте пары.
-- Двигайтесь от сигнатуры, как на паре: поставьте на место результата дыру `_` и читайте,
-- какой тип компилятор ждёт на её месте и что лежит в контексте (Relevant bindings).

first :: (a -> a') -> (a, b) -> (a', b)
first f p = (f $ fst p, snd p)


-- 2.6. Предскажите тип: посложнее
--
-- Правила те же, что в задаче 1.6: замените Todo наиболее общим типом выражения
-- из комментария, тело оставьте заглушкой.

-- flip :: (a -> b -> c) -> b -> a -> c
-- flip const :: b -> a -> a
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry (flip const) :: (b, a) -> a
-- uncurry (flip const)
typeOfUncurryFlipConst :: (b, a) -> a
typeOfUncurryFlipConst = undefined

-- id :: a -> a
-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry id :: a -> b -> (a, b)
-- curry id
typeOfCurryId :: a -> b -> (a, b)
typeOfCurryId = undefined

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip (.) :: (a -> b) -> (b -> c) -> (a -> c)
-- flip (.)
typeOfFlipCompose :: (a -> b) -> (b -> c) -> (a -> c)
typeOfFlipCompose = undefined
