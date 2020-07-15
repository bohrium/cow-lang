{-  author: samtenka
 -  change: 2020-07-14
 -  create: 2020-07-10
 -  descrp: 
 -  to use: 
 -
 -  Helped by Eli Bendersky's post on the Applicative typeclass:
 -      "Deciphering Haskell's applicative and monadic parsers"
 -}

import System.IO
import Data.Char
import Data.Maybe
import Data.List

-- ============================================================================
-- ===  0. PARSER MONAD  ======================================================
-- ============================================================================

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  0.0. Parser Type  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newtype Parser a = PP (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (PP p) inp = p inp

failure :: Parser a
failure = PP (\inp -> Nothing)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  0.1. Instantiate Composition Operators  ~~~~~~~~~~~~~~~~~~~~~~~

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = PP (\inp -> case parse p inp of
                                Nothing -> Nothing
                                Just (v, out) -> Just (f v, out)) 

instance Applicative Parser where
    -- pure  :: a -> Parser a
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pure a = PP (\inp -> Just (a, inp))
    fp <*> ap = PP (\inp -> case parse fp inp of
                                 Nothing -> Nothing
                                 Just (f, out) -> parse (fmap f ap) out)    

instance Monad Parser where
    -- return :: a -> Parser a
    -- (>>=)  :: Parser a -> (a -> Parser b) -> Parser b
    return v = pure v
    PP p >>= f = PP (\inp -> case parse (PP p) inp of
                                  Nothing -> Nothing
                                  Just (v,out) -> parse (f v) out)

-- ============================================================================
-- ===  1. BASIC PARSERS  =====================================================
-- ============================================================================

item  :: Parser Char
(+++) :: Parser a -> Parser a -> Parser a
sat   :: (Char -> Bool) -> Parser Char
sats  :: (Char -> Bool) -> Parser String
space :: Parser ()
strg  :: String -> Parser String
spop  :: String -> Parser String
wrap  :: Parser x -> Parser a -> Parser y -> Parser a

item = PP (\inp -> case inp of
                        [] -> Nothing
                        (x:xs) -> Just (x, xs))

p +++ q = PP (\inp -> case parse p inp of
                           Nothing -> parse q inp
                           Just (v, out) -> Just (v, out))

sat p = do x <- item
           if p x then (return x) else failure

sats p = do c <- sat p
            (do cc <- sats p
                return (c:cc)) +++ return [c]

wrap open p close = do open ; x <- p ; close
                       return x

space = (do sat isSpace
            space) +++ (return ()) 

strg s = case s of
              [] -> return "" 
              (c:cs) -> do cc <- sat ((==) c)
                           ss <- strg cs
                           return (cc:ss) 

spop a = wrap space (strg a) space

chain :: [Parser a] -> Parser [a]
chain ps = case ps of
                []     -> return [] 
                (p:pp) -> do x <- p
                             xx <- chain pp
                             return (x:xx) 

-- ============================================================================
-- ===  2. COWLANG GRAMMAR  ===================================================
-- ============================================================================

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  2.0. ParseTree Type  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data ETree = E String [ETree]

size :: ETree -> Int
size et = 1 + (case et of E _ ets -> sum (map size ets))

showtree :: String -> ETree -> String
showtree sp et = case et of
                      E hd []     -> sp ++ hd 
                      E hd (x:xs) -> if 15 < size et then (
                                         sp ++ hd ++ "[\n" ++
                                         intercalate ",\n" (map (showtree (sp ++ "  ")) (x:xs)) ++
                                         "\n" ++ sp ++ "]"
                                     ) else (
                                         sp ++ hd ++ "[" ++
                                         intercalate "," (map (showtree "") (x:xs)) ++
                                         "]"
                                     )

star :: String -> Parser a -> Parser ETree -> Parser ETree
plus :: String -> Parser a -> Parser ETree -> Parser ETree

star tag delim p = (plus tag delim p) +++ (return (E tag []))
plus tag delim p = do x <- p
                      (do delim
                          E _ xx <- star tag delim p 
                          return (E tag (x:xx))) +++ (return (E tag [x]))

promote :: Parser String -> Parser ETree 
promote p = do s <- p 
               return (E s [])

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  2.1. Toplevel Grammar  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tlvl :: Parser ETree
func :: Parser ETree
strc :: Parser ETree

tlvl = star "tlvl" (return []) (func +++ strc)

func = do spop "fn"
          nm <- iden
          args <- wrap (spop "(") jdgs (spop ")")
          body <- wrap (spop "{") blck (spop "}")
          return (E "fn" [nm, args, body]) 

strc = do spop "tp"
          nm <- iden
          jj <- wrap (spop "(") jdgs (spop ")")
          return (E "tp" [nm, jj]) 

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  2.2. Statement Grammar  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

blck :: Parser ETree
stmt :: Parser ETree
gcmd :: Parser ETree 

ctrl :: Parser ETree
decl :: Parser ETree
assi :: Parser ETree

blck = star "blck" (spop ";") stmt

gcmd = do cond <- expr  
          spop "->"
          body <- stmt
          return (E "gcmd" [cond, body]) 

stmt = (do x <- (wrap (spop "{") blck (spop "}")) 
           return x) +++ ctrl +++ decl +++ assi

ctrl = do flvr <- ((spop "do") +++ (spop "if"))
          E _ b <- wrap (spop "{") (star "gcmds" space gcmd) (spop "}")
          return (E flvr b)

decl = do spop "var" 
          j <- judg
          return (E "decl" [j])

assi = do nm <- iden 
          spop "="
          x <- expr
          return (E "assign" [nm, x])

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  2.3. Type Grammar  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

judg :: Parser ETree
jdgs :: Parser ETree

ptyp :: Parser ETree

jdgs = star "jdgs" (spop ",") judg 

judg = do nm <- iden
          spop ":"
          tp <- ptyp
          return (E "judg" [nm, tp])

ptyp = do c <- (sat isUpper)
          cc <- (sats isLower)
          return (E (c:cc) [])

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~~~~~~~  2.4. Expression Grammar  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

expr :: Parser ETree
term :: Parser ETree
fact :: Parser ETree
numb :: Parser ETree
iden :: Parser ETree

expr = do a <- term  
          (do op <- ((spop "+") +++ (spop "-"))
              b <- expr
              return (E op [a, b])) +++ return a

term = do a <- fact 
          (do op <- ((spop "*") +++ (spop "/"))
              b <- term
              return (E op [a, b])) +++ return a

fact = (do x <- (wrap (spop "(") expr (spop ")")) 
           return x) +++ numb +++ iden

numb  = promote (sats isDigit)

iden  = promote (sats isLower)

-- ============================================================================
-- ===  3. TRANSLATE  =========================================================
-- ============================================================================

--translate :: ETree -> String
--
--trans_gcmd out gc = case gc of 
--                         E _ [cond, body] -> "if ( " ++ translate cond ++ " ) {" ++
--                                             translate body ++
--                                             "; }"
--
--translate et = case et of
--                    E "assi" [nm,vl] -> nm ++ "=" ++ translate vl   
--                    E "do" gs -> "do {" ++
--                                 concat (map (\ca -> "if ( "++(translate))translate gs) ++ 
--                                 "} while ( false );"

-- ============================================================================
-- ===  4. MAIN LOOP  =========================================================
-- ============================================================================

moo s = case parse tlvl s of
             Nothing -> "Failed to Parse" 
             Just (a, out) -> showtree "" a

main = do putStr "hello!\n\n"
          contents <- readFile "moo.txt" 
          putStr (moo contents)
          putStr "\n\nbye!\n"
