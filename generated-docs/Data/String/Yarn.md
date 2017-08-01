## Module Data.String.Yarn

#### `IsString`

``` purescript
class IsString a  where
  fromString :: String -> a
```

##### Instances
``` purescript
IsString String
IsString (Array Char)
```

#### `TagString`

``` purescript
newtype TagString (a :: # Type)
```

A generic `String` tagged by a row of data types

##### Instances
``` purescript
Eq (TagString a)
Ord (TagString a)
Generic (TagString a) _
Show (TagString a)
Semigroup (TagString a)
Monoid (TagString a)
```

#### `runTag`

``` purescript
runTag :: forall a. TagString a -> String
```

Turn a `TagString` into a `String`

#### `tag`

``` purescript
tag :: forall a. String -> TagString a
```

Turn a `String` into a `TagString`

#### `fromChars`

``` purescript
fromChars :: forall f. Foldable f => f Char -> String
```

Turn a `Foldable` container of `Char`s to a `String`

#### `toChars`

``` purescript
toChars :: forall f. Unfoldable f => String -> f Char
```

Turn a `String` into an `Unfoldable` container of `Char`s. For example:

~~~ purescript
toChars "Foo" == ['F','o','o'] :: Array Char
~~~

#### `cons`

``` purescript
cons :: Char -> String -> String
```

Attach a `Char` to the front of a `String`

#### `(:)`

``` purescript
infixr 5 cons as :
```

#### `snoc`

``` purescript
snoc :: String -> Char -> String
```

Attach a `Char` to the end of a `String`

#### `range`

``` purescript
range :: Int -> Int -> String
```

Create a `String` containing a range of `Char`s, inclusive

#### `(..)`

``` purescript
infix 8 range as ..
```

#### `head`

``` purescript
head :: String -> Maybe Char
```

Safely get the first `Char` in a `String`

#### `tail`

``` purescript
tail :: String -> Maybe String
```

Safely get all but the first `Char` in a `String`

#### `last`

``` purescript
last :: String -> Maybe Char
```

Safely get the last `Char` in a `String

#### `init`

``` purescript
init :: String -> Maybe String
```

Safely get all but the last `Char` in a `String`

#### `index`

``` purescript
index :: String -> Int -> Maybe Char
```

Safely get the `Char` at a given index of a `String`

#### `(!!)`

``` purescript
infixl 8 index as !!
```

#### `lines`

``` purescript
lines :: String -> Array String
```

Split a `String` by its newlines

#### `unlines`

``` purescript
unlines :: Array String -> String
```

Join an `Array` of `String`s with newlines

#### `words`

``` purescript
words :: String -> Array String
```

Split a `String` by its spaces

#### `unwords`

``` purescript
unwords :: Array String -> String
```

Join an `Array` of `String`s with spaces

#### `substitute`

``` purescript
substitute :: String -> String -> String -> String
```

Like `replace` but acts globally

#### `substituteMany`

``` purescript
substituteMany :: forall f. Foldable f => f (Tuple String String) -> String -> String
```

Replace many substitutions given some association list

#### `capitalize`

``` purescript
capitalize :: String -> String
```

Capitalize the first `Char` in a `String`

#### `capWords`

``` purescript
capWords :: String -> String
```

Capitalize the first `Char` in each word of a given `String`

#### `rightpad`

``` purescript
rightpad :: String -> String
```

Append a space to the right of a `String`

#### `rightpadBy`

``` purescript
rightpadBy :: Int -> String -> String
```

Append a given number of spaces to the right of a `String`

#### `leftpad`

``` purescript
leftpad :: String -> String
```

Append a space to the left of a `String`

#### `leftpadBy`

``` purescript
leftpadBy :: Int -> String -> String
```

Append a given number of spaces to the left of a `String`

#### `reverse`

``` purescript
reverse :: String -> String
```

Reverse a `String`, may give funky results with unicode

#### `replicate`

``` purescript
replicate :: Int -> Char -> String
```

Replicate a `Char` a given number of times

#### `charMap`

``` purescript
charMap :: (Char -> Char) -> String -> String
```

Transform a function on `Char`s to a function on `String`s

#### `charFold`

``` purescript
charFold :: (String -> Char -> String) -> String -> String -> String
```

Fold over a `String` with a function that takes an accumulator `String` and next `Char` as input

#### `charTraverse`

``` purescript
charTraverse :: forall m. Applicative m => (Char -> m Char) -> String -> m String
```

Transform a Kleisli arrow on `Char`s to one on `String`s

#### `rot13`

``` purescript
rot13 :: String -> String
```

Cresbezf n ebg13 fhofgvghgvba ba n `Fgevat`


