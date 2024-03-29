{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A ReadP for ByteString
module ReadP.ByteString
  ( -- * The 'ReadP' type
    ReadP,

    -- * Primitive operations
    get,
    look,
    (+++),
    (<++),
    gather,

    -- * Other operations
    pfail,
    eof,
    satisfy,
    char,
    string,
    munch,
    munch1,
    skipSpaces,
    choice,
    count,
    between,
    option,
    optional,
    many,
    many1,
    skipMany,
    skipMany1,
    sepBy,
    sepBy1,
    endBy,
    endBy1,
    chainr,
    chainl,
    chainl1,
    chainr1,
    manyTill,
    rest,

    -- * Running a parser
    ReadS,
    readP_to_S,
    readS_to_P,

    -- * Properties
    -- $properties
  )
where

import Control.Monad ((<=<))
import Control.Monad.Fail
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List.NonEmpty (singleton)
import Data.String (IsString (..))
import GHC.Base hiding (many)
import GHC.List qualified as L
import GHC.Unicode (isSpace)

infixr 5 +++, <++

infixr 5 :<

pattern (:<) :: Char -> ByteString -> ByteString
pattern b :< bs <- (B.uncons -> Just (b, bs))

pattern Empty :: ByteString
pattern Empty <- (B.uncons -> Nothing)

------------------------------------------------------------------------
-- ReadS

-- | A parser for a type @a@, represented as a function that takes a
-- 'String' and returns a list of possible parses as @(a,'String')@ pairs.
--
-- Note that this kind of backtracking parser is very inefficient;
-- reading a large structure may be quite slow (cf 'ReadP').
type ReadS a = ByteString -> [(a, ByteString)]

-- ---------------------------------------------------------------------------
-- The P type
-- is representation type -- should be kept abstract

data P a
  = Get (Char -> P a)
  | Look (ByteString -> P a)
  | Fail
  | Result a (P a)
  | Final (NonEmpty (a, ByteString))
  deriving (-- | @since 4.8.0.0
           Functor)

-- Monad, MonadPlus

-- | @since 4.5.0.0
instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

-- | @since 2.01
instance MonadPlus P

-- | @since 2.01
instance Monad P where
  (Get f) >>= k = Get (\c -> f c >>= k)
  (Look f) >>= k = Look (\s -> f s >>= k)
  Fail >>= _ = Fail
  (Result x p) >>= k = k x <|> (p >>= k)
  (Final (r :| rs)) >>= k = final [ys' | (x, s) <- (r : rs), ys' <- run (k x) s]

-- | @since 4.9.0.0
instance MonadFail P where
  fail _ = Fail

-- | @since 4.5.0.0
instance Alternative P where
  empty = Fail

  -- most common case: two gets are combined
  Get f1 <|> Get f2 = Get (\c -> f1 c <|> f2 c)
  -- results are delivered as soon as possible
  Result x p <|> q = Result x (p <|> q)
  p <|> Result x q = Result x (p <|> q)
  -- fail disappears
  Fail <|> p = p
  p <|> Fail = p
  -- two finals are combined
  -- final + look becomes one look and one final (=optimization)
  -- final + sthg else becomes one look and one final
  Final r <|> Final t = Final (r <> t)
  Final (r :| rs) <|> Look f = Look (\s -> Final (r :| (rs ++ run (f s) s)))
  Final (r :| rs) <|> p = Look (\s -> Final (r :| (rs ++ run p s)))
  Look f <|> Final r =
    Look
      ( \s ->
          Final
            ( case run (f s) s of
                [] -> r
                (x : xs) -> (x :| xs) <> r
            )
      )
  p <|> Final r =
    Look
      ( \s ->
          Final
            ( case run p s of
                [] -> r
                (x : xs) -> (x :| xs) <> r
            )
      )
  -- two looks are combined (=optimization)
  -- look + sthg else floats upwards
  Look f <|> Look g = Look (\s -> f s <|> g s)
  Look f <|> p = Look (\s -> f s <|> p)
  p <|> Look f = Look (\s -> p <|> f s)

-- ---------------------------------------------------------------------------
-- The ReadP type

newtype ReadP a = R (forall b. (a -> P b) -> P b)

instance IsString (ReadP ByteString) where
  fromString = string . B.pack

-- | @since 2.01
instance Functor ReadP where
  fmap h (R f) = R (\k -> f (k . h))

-- | @since 4.6.0.0
instance Applicative ReadP where
  pure x = R (\k -> k x)
  (<*>) = ap

-- liftA2 = liftM2

-- | @since 2.01
instance Monad ReadP where
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))

-- | @since 4.9.0.0
instance MonadFail ReadP where
  fail _ = R (\_ -> Fail)

-- | @since 4.6.0.0
instance Alternative ReadP where
  empty = pfail
  (<|>) = (+++)

-- | @since 2.01
instance MonadPlus ReadP

-- ---------------------------------------------------------------------------
-- Operations over P

final :: [(a, ByteString)] -> P a
final [] = Fail
final (r : rs) = Final (r :| rs)

run :: P a -> ReadS a
run (Get f) (c :< s) = run (f c) s
run (Look f) s = run (f s) s
run (Result x p) s = (x, s) : run p s
run (Final (r :| rs)) _ = (r : rs)
run _ _ = []

-- -- ---------------------------------------------------------------------------
-- -- Operations over ReadP

get :: ReadP Char
-- ^ Consumes and returns the next character.
--   Fails if there is no input left.
get = R Get

look :: ReadP ByteString
-- ^ Look-ahead: returns the part of the input that is left, without
--   consuming it.
look = R Look

pfail :: ReadP a
-- ^ Always fails.
pfail = R (\_ -> Fail)

(+++) :: ReadP a -> ReadP a -> ReadP a
-- ^ Symmetric choice.
R f1 +++ R f2 = R (\k -> f1 k <|> f2 k)

(<++) :: ReadP a -> ReadP a -> ReadP a
-- ^ Local, exclusive, left-biased choice: If left parser
--   locally produces any result at all, then right parser is
--   not used.
R f0 <++ q =
  do
    s <- look
    probe (f0 return) s 0#
  where
    probe (Get f) (c :< s) n = probe (f c) s (n +# 1#)
    probe (Look f) s n = probe (f s) s n
    probe p@(Result _ _) _ n = discard n >> R (p >>=)
    probe (Final r) _ _ = R (Final r >>=)
    probe _ _ _ = q

    discard 0# = return ()
    discard n = get >> discard (n -# 1#)

gather :: ReadP a -> ReadP (ByteString, a)
-- ^ Transforms a parser into one that does the same, but
--   in addition returns the exact characters read.
--   IMPORTANT NOTE: 'gather' gives a runtime error if its first argument
--   is built using any occurrences of readS_to_P.
gather (R m) =
  R (\k -> gath id (m (\a -> return (\s -> k (s, a)))))
  where
    gath :: (ByteString -> ByteString) -> P (ByteString -> P b) -> P b
    gath l (Get f) = Get (\c -> gath (l . (B.cons c)) (f c))
    gath _ Fail = Fail
    gath l (Look f) = Look (\s -> gath l (f s))
    gath l (Result k p) = k (l mempty) <|> gath l p
    gath _ (Final _) = errorWithoutStackTrace "do not use readS_to_P in gather!"

-- -- ---------------------------------------------------------------------------
-- -- Derived operations

satisfy :: (Char -> Bool) -> ReadP Char
-- ^ Consumes and returns the next character, if it satisfies the
--   specified predicate.
satisfy p = do c <- get; if p c then return c else pfail

char :: Char -> ReadP Char
-- ^ Parses and returns the specified character.
char c = satisfy (c ==)

eof :: ReadP ()
-- ^ Succeeds iff we are at the end of input
eof = do
  s <- look
  if B.null s
    then return ()
    else pfail

string :: ByteString -> ReadP ByteString
-- ^ Parses and returns the specified string.
string this = do s <- look; scan this s
  where
    scan Empty _ = return this
    scan (x :< xs) (y :< ys) | x == y = do _ <- get; scan xs ys
    scan _ _ = pfail

munch :: (Char -> Bool) -> ReadP ByteString
-- ^ Parses the first zero or more characters satisfying the predicate.
--   Always succeeds, exactly once having consumed all the characters
--   Hence NOT the same as (many (satisfy p))
munch p =
  do
    s <- look
    scan s
  where
    scan (c :< cs) | p c = do _ <- get; s <- scan cs; return (B.cons c s)
    scan _ = return mempty

munch1 :: (Char -> Bool) -> ReadP ByteString
-- ^ Parses the first one or more characters satisfying the predicate.
--   Fails if none, else succeeds exactly once having consumed all the characters
--   Hence NOT the same as (many1 (satisfy p))
munch1 p =
  do
    c <- get
    if p c
      then do s <- munch p; return (B.cons c s)
      else pfail

choice :: [ReadP a] -> ReadP a
-- ^ Combines all parsers in the specified list.
choice [] = pfail
choice [p] = p
choice (p : ps) = p +++ choice ps

skipSpaces :: ReadP ()
-- ^ Skips all whitespace.
skipSpaces =
  do
    s <- look
    skip s
  where
    skip (c :< s) | isSpace c = do _ <- get; skip s
    skip _ = return ()

count :: Int -> ReadP a -> ReadP [a]
-- ^ @count n p@ parses @n@ occurrences of @p@ in sequence. A list of
--   results is returned.
count n p = sequence (L.replicate n p)

between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
-- ^ @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

option :: a -> ReadP a -> ReadP a
-- ^ @option x p@ will either parse @p@ or return @x@ without consuming
--   any input.
option x p = p +++ return x

optional :: ReadP a -> ReadP ()
-- ^ @optional p@ optionally parses @p@ and always returns @()@.
optional p = (p >> return ()) +++ return ()

many :: ReadP a -> ReadP [a]
-- ^ Parses zero or more occurrences of the given parser.
many p = return [] +++ many1 p

many1 :: ReadP a -> ReadP [a]
-- ^ Parses one or more occurrences of the given parser.
many1 p = liftM2 (:) p (many p)

skipMany :: ReadP a -> ReadP ()
-- ^ Like 'many', but discards the result.
skipMany p = many p >> return ()

skipMany1 :: ReadP a -> ReadP ()
-- ^ Like 'many1', but discards the result.
skipMany1 p = p >> skipMany p

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

endBy :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @endBy p sep@ parses zero or more occurrences of @p@, separated and ended
--   by @sep@.
endBy p sep = many (do x <- p; _ <- sep; return x)

endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
-- ^ @endBy p sep@ parses one or more occurrences of @p@, separated and ended
--   by @sep@.
endBy1 p sep = many1 (do x <- p; _ <- sep; return x)

chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
-- ^ @chainr p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /right/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainr p op x = chainr1 p op +++ return x

chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
-- ^ @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl p op x = chainl1 p op +++ return x

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
-- ^ Like 'chainr', but parses one or more occurrences of @p@.
chainr1 p op = scan
  where
    scan = p >>= rest'
    rest' x =
      do
        f <- op
        y <- scan
        return (f x y)
        +++ return x

chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
-- ^ Like 'chainl', but parses one or more occurrences of @p@.
chainl1 p op = p >>= rest'
  where
    rest' x =
      do
        f <- op
        y <- p
        rest' (f x y)
        +++ return x

manyTill :: ReadP a -> ReadP end -> ReadP [a]
-- ^ @manyTill p end@ parses zero or more occurrences of @p@, until @end@
--   succeeds. Returns a list of values returned by @p@.
manyTill p end = scan
  where
    scan = (end >> return []) <++ (liftM2 (:) p scan)

rest :: ReadP ByteString
rest = R (end <=< Look)
  where
    end = Final . singleton . (,mempty)

-- -- ---------------------------------------------------------------------------
-- -- Converting between ReadP and Read

readP_to_S :: ReadP a -> ReadS a
-- ^ Converts a parser into a Haskell ReadS-style function.
--   This is the main way in which you can \"run\" a 'ReadP' parser:
--   the expanded type is
-- @ readP_to_S :: ReadP a -> String -> [(a,String)] @
readP_to_S (R f) = run (f return)

readS_to_P :: ReadS a -> ReadP a
-- ^ Converts a Haskell ReadS-style function into a parser.
--   Warning: This introduces local backtracking in the resulting
--   parser, and therefore a possible inefficiency.
readS_to_P r =
  R (\k -> Look (\s -> final [bs'' | (a, s') <- r s, bs'' <- run (k a) s']))

-- -- ---------------------------------------------------------------------------
-- -- QuickCheck properties that hold for the combinators

-- {- $properties
-- The following are QuickCheck specifications of what the combinators do.
-- These can be seen as formal specifications of the behavior of the
-- combinators.

-- For some values, we only care about the lists contents, not their order,

-- > (=~) :: Ord a => [a] -> [a] -> Bool
-- > xs =~ ys = sort xs == sort ys

-- Here follow the properties:

-- >>> readP_to_S get []
-- []

-- prop> \c str -> readP_to_S get (c:str) == [(c, str)]

-- prop> \str -> readP_to_S look str == [(str, str)]

-- prop> \str -> readP_to_S pfail str == []

-- prop> \x str -> readP_to_S (return x) s == [(x,s)]

-- > prop_Bind p k s =
-- >    readP_to_S (p >>= k) s =~
-- >      [ ys''
-- >      | (x,s') <- readP_to_S p s
-- >      , ys''   <- readP_to_S (k (x::Int)) s'
-- >      ]

-- > prop_Plus p q s =
-- >   readP_to_S (p +++ q) s =~
-- >     (readP_to_S p s ++ readP_to_S q s)

-- > prop_LeftPlus p q s =
-- >   readP_to_S (p <++ q) s =~
-- >     (readP_to_S p s +<+ readP_to_S q s)
-- >  where
-- >   [] +<+ ys = ys
-- >   xs +<+ _  = xs

-- > prop_Gather s =
-- >   forAll readPWithoutReadS $ \p ->
-- >     readP_to_S (gather p) s =~
-- >       [ ((pre,x::Int),s')
-- >       | (x,s') <- readP_to_S p s
-- >       , let pre = take (length s - length s') s
-- >       ]

-- prop> \this str -> readP_to_S (string this) (this ++ str) == [(this,str)]

-- > prop_String_Maybe this s =
-- >   readP_to_S (string this) s =~
-- >     [(this, drop (length this) s) | this `isPrefixOf` s]

-- > prop_Munch p s =
-- >   readP_to_S (munch p) s =~
-- >     [(takeWhile p s, dropWhile p s)]

-- > prop_Munch1 p s =
-- >   readP_to_S (munch1 p) s =~
-- >     [(res,s') | let (res,s') = (takeWhile p s, dropWhile p s), not (null res)]

-- > prop_Choice ps s =
-- >   readP_to_S (choice ps) s =~
-- >     readP_to_S (foldr (+++) pfail ps) s

-- > prop_ReadS r s =
-- >   readP_to_S (readS_to_P r) s =~ r s
-- -}
