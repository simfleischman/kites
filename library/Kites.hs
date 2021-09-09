module Kites where

import Data.Functor.Const (Const(..))
import Data.Prodder
import Data.Summer
import Data.Void (Void)

data LetterA = LetterA
data LetterB = LetterB
data LetterC = LetterC
data LetterD = LetterD
data LetterE = LetterE

x1 :: Sum '[LetterA, LetterB, LetterC, LetterD, LetterE]
x1 = Inj LetterA

x2 :: Sum '[LetterA, LetterE, LetterB, LetterC, LetterD]
x2 = weaken x1 -- preferred "reorder x1" would be nice to only allow reordering so I know I didn't accidentally weaken

x3 :: Sum '[Sum '[LetterA, LetterE], Sum '[LetterB, LetterC, LetterD]]
x3 = error "assocOneLevel x2" -- I would be ok if this could be done one "level" at a time, sort of like "unconcat" or something

v1 :: Sum '[LetterA, Void, LetterB, Void]
v1 = Inj LetterA

v2 :: Sum '[LetterA, LetterB]
v2 = error "dropVoids v1" -- Voids are impossible cases, so it would be nice to remove them in intermediate computations

v3 :: Sum '[LetterA, Void, LetterB, Void]
v3 = error "addVoids v2" -- Should be able to add voids at any point (useful if we want to use a prism to isolate that branch)

data Apostrophe = Apostrophe
type Letter = Sum '[LetterA, LetterB, LetterC, LetterD, LetterE]
type IsContracted = Const Bool "Contracted"
type IsPossessive = Const Bool "Possessive"

u1 :: Prod '[ [Letter], IsContracted ]
u1 = undefined

u1' :: Prod '[ IsContracted, [Letter] ]
u1' = strengthen u1 -- could we just have "reorder u1" so I don't accidentally 'strenghten' the type? (which would lose information)

u2 :: Prod '[ (), [Letter], (), IsContracted ]
u2 = error "addUnits u1" -- we could add units any old place (useful for applying a lens to that part)

u3 :: Prod '[ [Letter], IsContracted ]
u3 = error "removeUnits u2" -- we could remove units to simplify the type

myData ::
  Prod '[
    [ Sum '[Letter, Apostrophe] ] -- representing a word as a list of letters or an apostrophe
  ]
myData = undefined -- some data 

-- we have some function that processes a list of letters or apostrophe and extracts what that apostrophe means (contraction and/or possession)
processApostrophe
  :: Sum '[Letter, Apostrophe]
  -> Prod '[ Sum '[Letter], IsContracted, IsPossessive ]
processApostrophe = undefined -- some function

myData2 ::
  Prod
    '[ [ Sum '[Letter] ] -- representing a word as a list of letters
    ,  IsContracted
    ,  IsPossessive
    ]
myData2 =
  error
    "embed processApostrophe myData2" -- can we automatically embed a function like this?

-- it seems we could transform this:
--    processApostrophe
--      :: Sum '[Letter, Apostrophe]
--      -> Prod '[ Sum '[Letter], IsContracted, IsPossessive ]
-- 
-- into:
--    processApostrophe'
--      :: Prod '[ Sum '[Letter, Apostrophe], (),           ()           ]
--      -> Prod '[ Sum '[Letter],             IsContracted, IsPossessive ]
-- 
-- then turn this:
--    myData ::
--      Prod '[
--        [ Sum '[Letter, Apostrophe] ]
--      ]
--
-- into:
--    myData ::
--      Prod '[
--        [ Sum '[Letter, Apostrophe], (), () ]
--      ]
--
-- then apply processApostrophe' to get our final result
