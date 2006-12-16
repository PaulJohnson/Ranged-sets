module Data.Ranged.RangedSet ( 
   -- ** Ranged Set Type
   RSet,
   rSetRanges,
   -- ** Ranged Set construction functions and their Preconditions
   makeRangedSet,
   unsafeRangedSet,
   validRangeList,
   normaliseRangeList,
   rSingleton,
   -- ** Predicates
   rSetIsEmpty,
   (-?-),  rSetHas, 
   (-<=-), rSetIsSubset,
   (-<-),  rSetIsSubsetStrict,
   -- ** Set Operations
   (-\/-), rSetUnion, 
   (-/\-), rSetIntersection, 
   (-!-),  rSetDifference,
   rSetNegation,
   -- ** Useful Sets
   rSetEmpty,
   rSetFull,
   rSetUnfold
   
   -- ** QuickCheck Properties
   
   -- *** Construction
   -- $ConstructionProperties
   
   -- *** Basic Operations
   -- $BasicOperationProperties
   
   -- *** Some Identities and Inequalities
   -- $SomeIdentitiesAndInequalities
) where

import Data.Ranged.Boundaries
import Data.Ranged.Ranges
import Data.Monoid

import Data.List
import Test.QuickCheck

infixl 7 -/\-
infixl 6 -\/-, -!-
infixl 5 -<=-, -<-, -?-

-- | An RSet (for Ranged Set) is a list of ranges.  The ranges must be sorted
-- and not overlap.

newtype DiscreteOrdered v => RSet v = RSet {rSetRanges :: [Range v]}
   deriving (Eq, Show)

instance DiscreteOrdered a => Monoid (RSet a) where
    mappend = rSetUnion
    mempty = rSetEmpty

-- | Determine if the ranges in the list are both in order and non-overlapping.
-- If so then they are suitable input for the unsafeRangedSet function.
validRangeList :: DiscreteOrdered v => [Range v] -> Bool

validRangeList [] = True
validRangeList [Range lower upper] = lower <= upper
validRangeList ranges = and $ zipWith okAdjacent ranges (tail ranges)
   where
      okAdjacent (Range lower1 upper1) (Range lower2 upper2) =
         lower1 <= upper1 && upper1 <= lower2 && lower2 <= upper2


-- | Rearrange and merge the ranges in the list so that they are in order and
-- non-overlapping.
normaliseRangeList :: DiscreteOrdered v => [Range v] -> [Range v]
   
normaliseRangeList ranges = 
   normalise $ sort $ filter (not . rangeIsEmpty) ranges
      

-- Private routine: normalise a range list that is known to be already sorted.
-- This precondition is not checked.
normalise :: DiscreteOrdered v => [Range v] -> [Range v]
normalise (r1:r2:rs) =
         if overlap r1 r2  
               then normalise $
                       Range (rangeLower r1) 
                             (max (rangeUpper r1) (rangeUpper r2))
                       : rs
               else r1 : (normalise $ r2 : rs)
   where
      overlap (Range _ upper1) (Range lower2 _) = upper1 >= lower2
      
normalise rs = rs


-- | Create a new Ranged Set from a list of ranges.  The list may contain
-- ranges that overlap or are not in ascending order.
makeRangedSet :: DiscreteOrdered v => [Range v] -> RSet v
makeRangedSet = RSet . normaliseRangeList


-- | Create a new Ranged Set from a list of ranges. @validRangeList ranges@ 
-- must return @True@.  This precondition is not checked.
unsafeRangedSet :: DiscreteOrdered v => [Range v] -> RSet v
unsafeRangedSet = RSet

-- | Create a Ranged Set from a single element.
rSingleton :: DiscreteOrdered v => v -> RSet v
rSingleton v = unsafeRangedSet [singletonRange v]

-- | True if the set has no members.
rSetIsEmpty :: DiscreteOrdered v => RSet v -> Bool
rSetIsEmpty = null . rSetRanges


-- | True if the negation of the set has no members.
rSetIsFull :: DiscreteOrdered v => RSet v -> Bool
rSetIsFull = rSetIsEmpty . rSetNegation


-- | True if the value is within the ranged set.  Infix precedence is left 5.
rSetHas, (-?-) :: DiscreteOrdered v => RSet v -> v -> Bool
rSetHas (RSet ls) value = rSetHas1 ls
   where
      rSetHas1 [] = False
      rSetHas1 (r:rs)
         | value />/ rangeLower r = rangeHas r value || rSetHas1 rs
         | otherwise              = False

(-?-) = rSetHas

-- | True if the first argument is a subset of the second argument, or is 
-- equal. 
-- 
-- Infix precedence is left 5.
rSetIsSubset, (-<=-) :: DiscreteOrdered v => RSet v -> RSet v -> Bool
rSetIsSubset rs1 rs2 = rSetIsEmpty (rs1 -!- rs2)
(-<=-) = rSetIsSubset


-- | True if the first argument is a strict subset of the second argument.
-- 
-- Infix precedence is left 5.
rSetIsSubsetStrict, (-<-) :: DiscreteOrdered v => RSet v -> RSet v -> Bool
rSetIsSubsetStrict rs1 rs2 = 
   rSetIsEmpty (rs1 -!- rs2) 
   && not (rSetIsEmpty (rs2 -!- rs1))
   
(-<-) = rSetIsSubsetStrict

-- | Set union for ranged sets.  Infix precedence is left 6.
rSetUnion, (-\/-) :: DiscreteOrdered v => RSet v -> RSet v -> RSet v
-- Implementation note: rSetUnion merges the two lists into a single
-- sorted list and then calls normalise to combine overlapping ranges.
rSetUnion (RSet ls1) (RSet ls2) = RSet $ normalise $ merge ls1 ls2
   where
      merge ls1 [] = ls1
      merge [] ls2 = ls2
      merge ls1@(h1:t1) ls2@(h2:t2) =
         if h1 <  h2
            then h1 : merge t1 ls2
            else h2 : merge ls1 t2

(-\/-) = rSetUnion

-- | Set intersection for ranged sets.  Infix precedence is left 7.
rSetIntersection, (-/\-) :: DiscreteOrdered v => RSet v -> RSet v -> RSet v
rSetIntersection (RSet ls1) (RSet ls2) =  
   RSet $ filter (not . rangeIsEmpty) $ merge ls1 ls2
   where
      merge ls1@(h1:t1) ls2@(h2:t2) =
         rangeIntersection h1 h2 
         : if rangeUpper h1 < rangeUpper h2
               then merge t1 ls2
               else merge ls1 t2
      merge _ _ = []

(-/\-) = rSetIntersection


-- | Set difference.  Infix precedence is left 6.
rSetDifference, (-!-) :: DiscreteOrdered v => RSet v -> RSet v -> RSet v
rSetDifference rs1 rs2 = rs1 -/\- (rSetNegation rs2)
(-!-) = rSetDifference


-- | Set negation.
rSetNegation :: DiscreteOrdered a => RSet a -> RSet a
rSetNegation set = RSet $ ranges $ setBounds1
   where
      ranges (b1:b2:bs) = Range b1 b2 : ranges bs
      ranges [BoundaryAboveAll] = []
      ranges [b] = [Range b BoundaryAboveAll]
      ranges _ = []
      setBounds1 = case setBounds of
         (BoundaryBelowAll : bs)  -> bs
         _                        -> BoundaryBelowAll : setBounds
      setBounds = bounds $ rSetRanges set 
      bounds (r:rs) = rangeLower r : rangeUpper r : bounds rs
      bounds _ = []


-- | The empty set.
rSetEmpty :: DiscreteOrdered a => RSet a
rSetEmpty = RSet []

-- | The set that contains everything.
rSetFull :: DiscreteOrdered a => RSet a
rSetFull = RSet [Range BoundaryBelowAll BoundaryAboveAll]


-- | Construct a range set.
rSetUnfold :: DiscreteOrdered a => 
   Boundary a
      -- ^ A first lower boundary.
   -> (Boundary a -> Boundary a) 
      -- ^ A function from a lower boundary to an upper boundary, which must
      -- return a result greater than the argument (not checked).
   -> (Boundary a -> Maybe (Boundary a))
      -- ^ A function from a lower boundary to @Maybe@ the successor lower 
      -- boundary, which must return a result greater than the argument 
      -- (not checked).
   -> RSet a
rSetUnfold bound upperFunc succFunc = RSet $ normalise $ ranges bound
   where
      ranges b = 
         Range b (upperFunc bound)
         : case succFunc b of
            Just b2 -> ranges b2
            Nothing -> []
   
   
-- QuickCheck Generators

instance (Arbitrary v, DiscreteOrdered v, Show v) => 
      Arbitrary (RSet v) 
   where
   arbitrary = frequency [
      (1, return rSetEmpty),
      (1, return rSetFull),
      (18, do
         ls <- arbitrary
         return $ makeRangedSet $ rangeList $ sort ls
      )]
      where
         -- Arbitrary lists of ranges don't give many interesting sets after
         -- normalisation.  So instead generate a sorted list of boundaries
         -- and pair them off.  Odd boundaries are dropped.
         rangeList (b1:b2:bs) = Range b1 b2 : rangeList bs
         rangeList _ = []
      
   coarbitrary (RSet ls) = variant 0 . coarbitrary ls

-- ==================================================================  
-- QuickCheck Properties
-- ==================================================================

-- Note for maintenance: Haddock does not include QuickCheck properties,
-- so they have to be copied into documentation blocks manually.  This
-- process must be repeated for new or modified properties.


---------------------------------------------------------------------
-- Construction properties
---------------------------------------------------------------------

{- $ConstructionProperties

A normalised range list is valid for unsafeRangedSet

> prop_validNormalised ls = validRangeList $ normaliseRangeList ls
>    where types = ls :: [Range Double]

Iff a value is in a range list then it is in a ranged set
constructed from that list.

> prop_has ls v = (ls `rangeListHas` v) == rangedSet ls -?- v

-}

-- A normalised range list is valid for unsafeRangedSet
prop_validNormalised ls = validRangeList $ normaliseRangeList ls
   where types = ls :: [Range Integer]

-- Iff a value is in a range list then it is in a ranged set
-- constructed from that list.
prop_has ls v = (ls `rangeListHas` v) == makeRangedSet ls -?- v
   where types = v :: Integer

---------------------------------------------------------------------
-- Basic operation properties
---------------------------------------------------------------------

{- $BasicOperationProperties
Iff a value is in either of two ranged sets then it is in the union of
those two sets.

> prop_union rs1 rs2 v =
>    (rs1 -?- v || rs2 -?- v) == ((rs1 -\/- rs2) -?- v)

Iff a value is in both of two ranged sets then it is in the intersection
of those two sets.

> prop_intersection rs1 rs2 v =
>    (rs1 -?- v && rs2 -?- v) == ((rs1 -/\- rs2) -?- v)

      
Iff a value is in ranged set 1 and not in ranged set 2 then it is in the
difference of the two.

> prop_difference rs1 rs2 v = 
>    (rs1 -?- v && not (rs2 -?- v)) == ((rs1 -!- rs2) -?- v)


Iff a value is not in a ranged set then it is in its negation.      

> prop_negation rs v = rs -?- v == not (rSetNegation rs -?- v)


A set that contains a value is not empty

> prop_not_empty rs v = (rs -?- v) ==> not (rSetIsEmpty rs)

-}     
 
-- Iff a value is in either of two ranged sets then it is in the union of
-- those two sets.
prop_union rs1 rs2 v = (rs1 -?- v || rs2 -?- v) == ((rs1 -\/- rs2) -?- v)
   where types = v :: Integer

-- Iff a value is in both of two ranged sets then it is in the intersection
-- of those two sets.
prop_intersection rs1 rs2 v = 
   (rs1 -?- v && rs2 -?- v) == ((rs1 `rSetIntersection` rs2) -?- v)
   where types = v :: Integer

      
-- Iff a value is in ranged set 1 and not in ranged set 2 then it is in the
-- difference of the two.
prop_difference rs1 rs2 v = 
   (rs1 -?- v && not (rs2 -?- v)) == ((rs1 -!- rs2) -?- v)
   where types = v :: Integer


-- Iff a value is not in a ranged set then it is in its negation.      
prop_negation rs v = rs -?- v == not (rSetNegation rs -?- v)
   where types = v :: Integer


-- A set that contains a value is not empty
prop_not_empty rs v = (rs -?- v) ==> not (rSetIsEmpty rs)
   where types = v :: Integer
   

---------------------------------------------------------------------
-- Some identities and inequalities of sets
---------------------------------------------------------------------

{- $SomeIdentitiesAndInequalities

The empty set has no members.

> prop_empty v = not (rSetEmpty -?- v)


The full set has every member.

> prop_full v = rSetFull -?- v


The intersection of a set with its negation is empty.

> prop_empty_intersection rs =
>    rSetIsEmpty (rs -/\- rSetNegation rs) 
   
   
The union of a set with its negation is full.

> prop_full_union rs v =
>    rSetIsFull (rs -\/- rSetNegation rs)


The union of two sets is the non-strict superset of both.

> prop_union_superset rs1 rs2 =
>    rs1 -<=- u && rs2 -<=- u 
>    where
>       u = rs1 -\/- rs2
      
The intersection of two sets is the non-strict subset of both.

> prop_intersection_subset rs1 rs2 =
>    i -<=- rs1 && i -<=- rs2
>    where
>       i = rs1 -/\- rs2

The difference of two sets intersected with the subtractand is empty.

> prop_diff_intersect rs1 rs2 =
>    rSetIsEmpty ((rs1 -!- rs2) -/\- rs2)

A set is the non-strict subset of itself.

> prop_subset rs = rs -<=- rs

   
A set is not the strict subset of itself.

> prop_strict_subset rs = not (rs -<- rs)
   

If rs1 - rs2 is not empty then the union of rs1 and rs2 will be a strict 
superset of rs2.

> prop_union_strict_superset rs1 rs2 =
>    (not $ rSetIsEmpty (rs1 -!- rs2))
>    ==> (rs2 -<- (rs1 -\/- rs2))

Intersection commutes

> prop_intersection_commutes rs1 rs2 =
>    (rs1 -/\- rs2) == (rs2 -/\- rs1)
   
Union commutes

> prop_union_commutes rs1 rs2 =
>    (rs1 -\/- rs2) == (rs2 -\/- rs1)
   
Intersection associates

> prop_intersection_associates rs1 rs2 rs3 =
>    ((rs1 -/\- rs2) -/\- rs3) == (rs1 -/\- (rs2 -/\- rs3))
  
Union associates

> prop_union_associates rs1 rs2 rs3 =
>    ((rs1 -\/- rs2) -\/- rs3) == (rs1 -\/- (rs2 -\/- rs3))

De Morgan's Law for Intersection

> prop_de_morgan_intersection rs1 rs2 =
>    rSetNegation (rs1 -/\- rs2) == (rSetNegation rs1 -\/- rSetNegation rs2)

De Morgan's Law for Union

> prop_de_morgan_union rs1 rs2 =
>    rSetNegation (rs1 -\/- rs2) == (rSetNegation rs1 -/\- rSetNegation rs2)

-}

-- The empty set has no members.
prop_empty v = not (rSetEmpty -?- v)
   where types = v :: Integer


-- The full set has every member.
prop_full v = rSetFull -?- v
   where types = v :: Integer


-- The intersection of a set with its negation is empty.
prop_empty_intersection rs =
   rSetIsEmpty (rs -/\- rSetNegation rs) 
   where types = rs :: RSet Integer
   
   
-- The union of a set with its negation is full.
prop_full_union rs =
   rSetIsFull (rs -\/- rSetNegation rs)
   where types = rs :: RSet Integer


-- The union of two sets is the non-strict superset of both.
prop_union_superset rs1 rs2 =
   rs1 -<=- u && rs2 -<=- u 
   where
      u :: RSet Integer
      u = rs1 -\/- rs2
      
-- The intersection of two sets is the non-strict subset of both.
prop_intersection_subset rs1 rs2 =
   i -<=- rs1 && i -<=- rs2
   where
      i :: RSet Integer
      i = rs1 -/\- rs2

-- The difference of two sets intersected with the subtractand is empty.
prop_diff_intersect rs1 rs2 =
   rSetIsEmpty ((rs1 -!- rs2) -/\- rs2)
   where types = rs1 :: RSet Integer
   
   
-- A set is the non-strict subset of itself.
prop_subset rs =
   rs -<=- rs
   where types = rs :: RSet Integer
   
-- A set is not the strict subset of itself.
prop_strict_subset rs =
   not (rs -<- rs)
   where types = rs :: RSet Integer
   

-- If rs1 - rs2 is not empty then the union of rs1 and rs2 will be a strict 
-- superset of rs2.
prop_union_strict_superset rs1 rs2 =
   (not $ rSetIsEmpty (rs1 -!- rs2))
   ==> (rs2 -<- (rs1 -\/- rs2))
   where types = rs1 :: RSet Integer

-- Intersection commutes
prop_intersection_commutes :: RSet Integer -> RSet Integer -> Bool
prop_intersection_commutes rs1 rs2 =
   (rs1 -/\- rs2) == (rs2 -/\- rs1)
   where types = rs1 :: RSet Integer
   
-- Union commutes
prop_union_commutes rs1 rs2 =
   (rs1 -\/- rs2) == (rs2 -\/- rs1)
   where types = rs1 :: RSet Integer
   
-- Intersection associates
prop_intersection_associates rs1 rs2 rs3 =
   ((rs1 -/\- rs2) -/\- rs3) == (rs1 -/\- (rs2 -/\- rs3))
   where types = rs1 :: RSet Integer
   
-- Union associates
prop_union_associates rs1 rs2 rs3 =
   ((rs1 -\/- rs2) -\/- rs3) == (rs1 -\/- (rs2 -\/- rs3))
   where types = rs1 :: RSet Integer
   
-- De Morgan's Law for Intersection
prop_de_morgan_intersection rs1 rs2 =
   rSetNegation (rs1 -/\- rs2) == (rSetNegation rs1 -\/- rSetNegation rs2)
   where types = rs1 :: RSet Integer

-- De Morgan's Law for Union
prop_de_morgan_union rs1 rs2 =
   rSetNegation (rs1 -\/- rs2) == (rSetNegation rs1 -/\- rSetNegation rs2)
   where types = rs1 :: RSet Integer
