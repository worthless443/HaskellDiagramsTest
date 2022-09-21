{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
t ::  (Floating a, Ord a) => Double -> Colour a
t i = case i of 
	0 -> red 
	1 -> green 
	2 -> blue 
	otherwise -> yellow

myCircle :: Double -> Double -> [Diagram B]
myCircle 10 b =  [circle 10 # fc (t b)]
myCircle a b = circle a # fc (t b) : myCircle (a+1) b


allCircles :: Diagram B -> Double -> [Diagram B]
allCircles a 3 = myCircle 4 3
allCircles a b = hcat $ myCircle 4 b : [allCircles (hcat $ myCircle 4 b) (b+1)]

testCricle :: Diagram B
testCricle  = hcat $ allCircles ( hcat $ myCircle 4 0) 0
--testCricle  = hcat $ map (hcat . myCircle)  [1..4]


main = mainWith   testCricle
