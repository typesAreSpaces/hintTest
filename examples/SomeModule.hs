module SomeModule(g, h) where

f = head

g = f [f]

h = f

i x 
  | x < 10 = True
  | otherwise = False
