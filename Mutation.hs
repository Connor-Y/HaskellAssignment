{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer, inList,
	getInt, getBool
    )
    where

import AList (AList, lookupA, insertA, updateA)	
-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value
-- Same as [(Integer, Value)]

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

inList :: Eq a => AList a b -> a -> Bool
inList alist key = 	if (null alist)
						then False
						else 
							if (fst (head alist)) == key
								then True
								else inList (tail alist) key	
						
getInt :: Value -> Integer
getInt (IntVal x) = x
getInt _ = error "Invalid Type"

getBool :: Value -> Bool
getBool (BoolVal x) = x
getBool _ = error "Invalid Type"

bp1 :: Pointer Bool
bp1 = (P 3)

bp2 :: Pointer Bool
bp2 = (P 4)

testMem = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]
testBool = [(3, BoolVal True), (4, BoolVal False)]
-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)
	
instance Mutable Integer where
	get mem (P x) =  if (inList mem x)
						then getInt (lookupA mem x)
						else error "Invalid Memory Address"

	set mem (P x) newVal = if (inList mem x)
						then updateA mem (x, (IntVal newVal))
						else error "Invalid Memory Address"
	-- TODO: make sure valid input ?
	def mem key val = if (inList mem key)
						then error "Memory Already in Use"
						else ((P key), (insertA mem (key, (IntVal val))))

instance Mutable Bool where
	get mem (P x) =  if (inList mem x)
						then getBool (lookupA mem x)
						else error "Invalid Memory Address"

	set mem (P x) newVal = if (inList mem x)
						then updateA mem (x, (BoolVal newVal))
						else error "Invalid Memory Address"
	
	def mem key val = if (inList mem key)
						then error "Memory Already in Use"
						else ((P key), (insertA mem (key, (BoolVal val))))
						
