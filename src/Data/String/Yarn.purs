module Data.String.Yarn
  ( class IsString
  , fromString
  , TagString
  , runTag
  , tag
  , fromChars
  , toChars
  , cons, (:)
  , snoc
  , range, (..)
  , head, last
  , tail, init
  , index, (!!)
  , lines, unlines
  , words, unwords
  , substitute, substituteMany
  , capitalize, capWords
  , rightpad, rightpadBy
  , leftpad, leftpadBy
  , reverse
  , replicate
  , charMap, charTraverse, charFold
  , rot13
  ) where

import Prelude
import Data.Array as Array
import Data.Char (toCharCode, fromCharCode, toUpper)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String (Pattern(Pattern), Replacement, charAt, contains, fromCharArray, joinWith, length, null, replace, singleton, split, take, toCharArray, uncons)
import Data.Traversable (class Foldable, foldMap, traverse, foldl)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

class IsString a where
  fromString :: String -> a

instance isStringString :: IsString String where
  fromString = id

instance isStringArrayChar :: IsString (Array Char) where
  fromString = toCharArray

-- | A generic `String` tagged by a row of data types
newtype TagString (a :: # Type) = Tag String

derive instance eqTagString :: Eq (TagString a)

derive instance ordTagString :: Ord (TagString a)

derive instance genericTagString :: Generic (TagString a) _

instance showTagString :: Show (TagString a) where
  show (Tag s) = "Tag " <> show s

instance semigroupTagString :: Semigroup (TagString a) where
  append (Tag s) (Tag p) = Tag (s <> p)

instance monoidTagString :: Monoid (TagString a) where
  mempty = Tag ""

-- | Turn a `TagString` into a `String`
runTag :: forall a. TagString a -> String
runTag (Tag s) = s

-- | Turn a `String` into a `TagString`
tag :: forall a. String -> TagString a
tag = Tag

-- | Turn a `Foldable` container of `Char`s to a `String`
fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap singleton

-- | Turn a `String` into an `Unfoldable` container of `Char`s. For example:
-- |
-- | ~~~ purescript
-- | toChars "Foo" == ['F','o','o'] :: Array Char
-- | ~~~
toChars :: forall f. Unfoldable f => String -> f Char
toChars = unfoldr step
  where step str = Tuple <$> head str <*> tail str

-- | Attach a `Char` to the front of a `String`
cons :: Char -> String -> String
cons = append <<< singleton

infixr 5 cons as :

-- | Attach a `Char` to the end of a `String`
snoc :: String -> Char -> String
snoc s c = s <> singleton c

-- | Create a `String` containing a range of `Char`s, inclusive
range :: Int -> Int -> String
range mn mx = fromCharArray $ map fromCharCode $ Array.range mn mx

infix 8 range as ..

-- | Safely get the first `Char` in a `String`
head :: String -> Maybe Char
head = charAt 0

-- | Safely get all but the first `Char` in a `String`
tail :: String -> Maybe String
tail s = case uncons s of
  Just r -> Just r.tail
  _ -> Nothing

-- | Safely get the last `Char` in a `String
last :: String -> Maybe Char
last s | null s = Nothing
last s = charAt (length s - 1) s

-- | Safely get all but the last `Char` in a `String`
init :: String -> Maybe String
init s | null s = Nothing
init s = Just (take (length s - 1) s)

-- | Safely get the `Char` at a given index of a `String`
index :: String -> Int -> Maybe Char
index = flip charAt

infixl 8 index as !!

-- | Split a `String` by its newlines
lines :: String -> Array String
lines = split (Pattern "\n")

-- | Join an `Array` of `String`s with newlines
unlines :: Array String -> String
unlines = joinWith "\n"

-- | Split a `String` by its spaces
words :: String -> Array String
words = split (Pattern " ")

-- | Join an `Array` of `String`s with spaces
unwords :: Array String -> String
unwords = joinWith " "

-- | Check if a `Char` is in a `String`
elem :: Char -> String -> Boolean
elem = contains <<< Pattern <<< singleton

until :: forall a. (a -> a -> Boolean) -> (a -> a) -> a -> a
until p f x | p x (f x) = x
until p f x = until p f (f x)

-- | Like `replace` but acts globally
substitute :: Pattern -> Replacement -> String -> String
substitute old new = until eq $ replace old new

-- | Replace many substitutions given some association list
substituteMany :: forall f. Foldable f => f (Tuple Pattern Replacement) -> String -> String
substituteMany = flip (foldl f)
  where
    f str (Tuple old new) = substitute old new str

-- | Capitalize the first `Char` in a `String`
capitalize :: String -> String
capitalize str = case uncons str of
  Just o -> cons (toUpper o.head) o.tail
  _ -> str

-- | Capitalize the first `Char` in each word of a given `String`
capWords :: String -> String
capWords = unwords <<< map capitalize <<< words

-- | Append a space to the right of a `String`
rightpad :: String -> String
rightpad = (_ <> " ")

-- | Append a given number of spaces to the right of a `String`
rightpadBy :: Int -> String -> String
rightpadBy n = (_ <> replicate n ' ')

-- | Append a space to the left of a `String`
leftpad :: String -> String
leftpad = append " "

-- | Append a given number of spaces to the left of a `String`
leftpadBy :: Int -> String -> String
leftpadBy n = append (replicate n ' ')

-- | Reverse a `String`, may give funky results with unicode
reverse :: String -> String
reverse = fromCharArray <<< Array.reverse <<< toCharArray

-- | Replicate a `Char` a given number of times
replicate :: Int -> Char -> String
replicate = replicate' ""
  where
    replicate' acc n c | n <= 0 = acc
                       | otherwise = replicate' (cons c acc) (n - 1) c

-- | Transform a function on `Char`s to a function on `String`s
charMap :: (Char -> Char) -> String -> String
charMap f = fromCharArray <<< map f <<< toCharArray

-- | Fold over a `String` with a function that takes an accumulator `String` and next `Char` as input
charFold :: (String -> Char -> String) -> String -> String -> String
charFold f z str = case uncons str of
  Just o -> charFold f (f z o.head) o.tail
  _ -> z

-- | Transform a Kleisli arrow on `Char`s to one on `String`s
charTraverse :: forall m. Applicative m => (Char -> m Char) -> String -> m String
charTraverse f str = fromCharArray <$> traverse f (toCharArray str)

-- | Cresbezf n ebg13 fhofgvghgvba ba n `Fgevat`
rot13 :: String -> String
rot13 = charMap rotate
  where
    rotate :: Char -> Char
    rotate c
      | toCharCode c <= 90 && toCharCode c >= 65 = fromCharCode $ 65 + ((toCharCode c - 52) `mod` 26)
      | toCharCode c <= 122 && toCharCode c >= 97 = fromCharCode $ 97 + ((toCharCode c - 84) `mod` 26)
      | otherwise = c
