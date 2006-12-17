-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ranged.Ranges
-- Copyright   :  (c) Paul Johnson 2006
-- License     :  BSD-style
-- Maintainer  :  paul@cogito.org.uk
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------


-- | A range has an upper and lower boundary.
module Data.Ranged.Ranges (
   -- ** Construction
   Range (..),
   emptyRange,
   fullRange,
   -- ** Predicates
   rangeIsEmpty,
   rangeOverlap,
   rangeEncloses,
   rangeSingletonValue,
   -- ** Membership
   rangeHas,
   rangeListHas,
   -- ** Set Operations
   singletonRange,
   rangeIntersection,
   rangeUnion,
   rangeDifference
   -- ** QuickCheck properties
   -- $properties
) where

import Data.Ranged.Boundaries
import Data.Maybe
import Test.QuickCheck

-- | A Range has upper and lower boundaries.
data Ord v => Range v = Range {rangeLower, rangeUpper :: Boundary v}

instance (DiscreteOrdered a) => Eq (Range a) where
   r1 == r2   = (rangeIsEmpty r1 && rangeIsEmpty r2) || 
                (rangeLower r1 == rangeLower r2 && 
                 rangeUpper r1 == rangeUpper r2)


instance (DiscreteOrdered a) => Ord (Range a) where
   compare r1 r2
      | r1 == r2       = EQ
      | rangeIsEmpty r1  = LT
      | rangeIsEmpty r2  = GT
      | otherwise      = compare (rangeLower r1, rangeUpper r1)
                                 (rangeLower r2, rangeUpper r2)
                                 
instance (Show a, DiscreteOrdered a) => Show (Range a) where
   show r
      | rangeIsEmpty r     = "Empty"
      | otherwise          = 
         case rangeSingletonValue r of
            Just v  -> "x == " ++ show v
            Nothing -> lowerBound ++ "x" ++ upperBound
      where
         lowerBound = case rangeLower r of
            BoundaryBelowAll -> ""
            BoundaryBelow v  -> show v ++ " <= "
            BoundaryAbove v  -> show v ++ " < "
            BoundaryAboveAll -> error "show Range: lower bound is BoundaryAboveAll"
         upperBound = case rangeUpper r of
            BoundaryBelowAll -> error "show Range: upper bound is BoundaryBelowAll"
            BoundaryBelow v  -> " < " ++ show v
            BoundaryAbove v  -> " <= " ++ show v
            BoundaryAboveAll -> ""


-- | True if the value is within the range.
rangeHas :: Ord v => Range v -> v -> Bool

rangeHas (Range b1 b2) v =
   (v />/ b1) && not (v />/ b2)


-- | True if the value is within one of the ranges.
rangeListHas :: Ord v =>
   [Range v] -> v -> Bool
rangeListHas ls v = or $ map (\r -> rangeHas r v) ls

-- | The empty range
emptyRange :: DiscreteOrdered v => Range v
emptyRange = Range BoundaryAboveAll BoundaryBelowAll

-- | The full range.  All values are within it.
fullRange :: DiscreteOrdered v => Range v
fullRange = Range BoundaryBelowAll BoundaryAboveAll

-- | A range containing a single value
singletonRange :: DiscreteOrdered v => v -> Range v
singletonRange v = Range (BoundaryBelow v) (BoundaryAbove v)

-- | If the range is a singleton, returns @Just@ the value.  Otherwise returns
-- @Nothing@.
rangeSingletonValue :: DiscreteOrdered v => Range v -> Maybe v
rangeSingletonValue (Range (BoundaryBelow v1) (BoundaryAbove v2))
   | v1 == v2    = Just v1
   | otherwise   = Nothing
rangeSingletonValue _ = Nothing

-- | A range is empty unless its upper boundary is greater than its lower
-- boundary.
rangeIsEmpty :: DiscreteOrdered v => Range v -> Bool
rangeIsEmpty (Range lower upper) = upper <= lower


-- | Two ranges overlap if their intersection is non-empty.
rangeOverlap :: DiscreteOrdered v => Range v -> Range v -> Bool
rangeOverlap r1 r2 = 
   not (rangeIsEmpty r1)
   && not (rangeIsEmpty r2)
   && not (rangeUpper r1 <= rangeLower r2 || rangeUpper r2 <= rangeLower r1)
 
  
-- | The first range encloses the second if every value in the second range is 
-- also within the first range.  If the second range is empty then this is
-- always true.
rangeEncloses :: DiscreteOrdered v => Range v -> Range v -> Bool
rangeEncloses r1 r2 =
   (rangeLower r1 <= rangeLower r2 && rangeUpper r2 <= rangeUpper r1) 
   || rangeIsEmpty r2


-- | Intersection of two ranges, if any.
rangeIntersection :: DiscreteOrdered v => Range v -> Range v -> Range v
   
rangeIntersection (Range lower1 upper1) (Range lower2 upper2) =
   Range (max lower1 lower2) (min upper1 upper2)
     
     
-- | Union of two ranges.  Returns one or two results.
--
-- If there are two results then they are guaranteed to have a non-empty
-- gap in between, but may not be in ascending order.
rangeUnion :: DiscreteOrdered v => Range v -> Range v -> [Range v]
   
rangeUnion r1@(Range lower1 upper1) r2@(Range lower2 upper2) =
   if touching
      then [Range lower upper]
      else [r1, r2]
   where
      touching = (max lower1 lower2) <= (min upper1 upper2)
      lower = min lower1 lower2
      upper = max upper1 upper2


-- | @range1@ minus @range2@.  Returns zero, one or two results.  Multiple 
-- results are guaranteed to have non-empty gaps in between, but may not be in 
-- ascending order.
rangeDifference :: DiscreteOrdered v => Range v -> Range v -> [Range v]
   
rangeDifference r1@(Range lower1 upper1) r2@(Range lower2 upper2) =
   -- There are six possibilities
   --    1: r2 completely less than r1
   --    2: r2 overlaps bottom of r1
   --    3: r2 encloses r1
   --    4: r1 encloses r2
   --    5: r2 overlaps top of r1
   --    6: r2 completely greater than r1
   if intersects
      then -- Cases 2,3,4,5
         filter (not . rangeIsEmpty) [Range lower1 lower2, Range upper2 upper1]
      else -- Cases 1, 6
         [r1]
   where
      intersects = (max lower1 lower2) < (min upper1 upper2)


-- QuickCheck generators

instance (Arbitrary v,  DiscreteOrdered v, Show v) => 
   Arbitrary (Range v) where
   
   arbitrary = frequency [
      (17, do  -- Ordinary range
         b1 <- arbitrary
         b2 <- arbitrary
         if b1 < b2 
            then return $ Range b1 b2
            else return $ Range b2 b1
      ),
      (1, do  -- Singleton range
         v <- arbitrary
         return $ singletonRange v
      ),
      (1, return emptyRange),
      (1, return fullRange)
      ]
      
   coarbitrary (Range lower upper) =
      variant 0 . coarbitrary lower . coarbitrary upper
      

         
-- QuickCheck Properties

{- $properties
Range union

> prop_union r1 r2 n =
>    (r1 `rangeHas` n || r2 `rangeHas` n) 
>    == (r1 `rangeUnion` r2) `rangeListHas` n

Range intersection

> prop_intersection r1 r2 n =
>    (r1 `rangeHas` n && r2 `rangeHas` n)
>    == (r1 `rangeIntersection` r2) `rangeHas` n

Range difference

> prop_difference r1 r2 n =
>    (r1 `rangeHas` n && not (r2 `rangeHas` n))
>    == (r1 `rangeDifference` r2) `rangeListHas` n

Singleton range

> prop_singletonHas v =
>    singletonRange v `rangeHas` v

> prop_singletonConverse v =
>    rangeSingletonValue (singletonRange v) == Just v

-}

-- For Integers (sparse type)

-- Range union
prop_union_int r1 r2 n = 
   (r1 `rangeHas` n || r2 `rangeHas` n) 
   == (r1 `rangeUnion` r2) `rangeListHas` n
   where t :: Integer ; t = n


-- Range intersection
prop_intersection_int r1 r2 n =
   (r1 `rangeHas` n && r2 `rangeHas` n)
   == (r1 `rangeIntersection` r2) `rangeHas` n
   where t :: Integer ; t = n

-- Range difference
prop_difference_int r1 r2 n =
   (r1 `rangeHas` n && not (r2 `rangeHas` n))
   == (r1 `rangeDifference` r2) `rangeListHas` n
   where t :: Integer ; t = n

-- Range Singleton Has
prop_singletonHas_int v =
   singletonRange v `rangeHas` v
   where t :: Integer ; t = v

-- Range Singleton inverse
prop_singletonConverse_int v =
   rangeSingletonValue (singletonRange v) == Just v
   where t :: Integer ; t = v

-- For Reals (dense type)

-- Range Union
prop_union_real r1 r2 n = 
   (r1 `rangeHas` n || r2 `rangeHas` n) 
   == (r1 `rangeUnion` r2) `rangeListHas` n
   where t :: Double ; t = n

-- Range intersection
prop_intersection_real r1 r2 n =
   (r1 `rangeHas` n && r2 `rangeHas` n)
   == (r1 `rangeIntersection` r2) `rangeHas` n
   where t :: Double ; t = n

-- Range difference
prop_difference_real r1 r2 n =
   (r1 `rangeHas` n && not (r2 `rangeHas` n))
   == (r1 `rangeDifference` r2) `rangeListHas` n
   where t :: Double ; t = n

-- Range Singleton Has
prop_singletonHas_real v =
   singletonRange v `rangeHas` v
   where t :: Double ; t = v

-- Range Singleton inverse
prop_singletonConverse_real v =
   rangeSingletonValue (singletonRange v) == Just v
   where t :: Double ; t = v
