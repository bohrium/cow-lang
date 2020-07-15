{-  author: samtenka
 -  change: 2020-07-14
 -  create: 2020-07-10
 -  descrp: combinator-based parsing for the COW language 
 -  thanks: I found Eli Bendersky's post on the Applicative typeclass very
 -          useful: "Deciphering Haskell's applicative and monadic parsers".
 -  to use: `make parse` 
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
showtree sp et =
    let
        base sep space kids = intercalate sep (map (showtree space) kids)
    in case et of
        E hd []   -> sp ++ hd 
        E hd kids -> sp ++ hd ++ "[" ++ (
                         if 15 < size et
                         then "\n" ++ (base ",\n" (sp++" ") kids) ++ "\n" ++ sp
                         else base "," "" kids
                     ) ++ "]" 

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

blck = star "block" (spop ";") stmt

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
          return (E "declare" [j])

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
          return (E "judge" [nm, tp])

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

trans_expr et = "EXPR" 

trans_gcmd last gc =
    case gc of E _ [cond, body] -> "if ( " ++ trans_expr cond ++ " ) {\n" ++
                                   trans_stmt body ++ last ++
                                   "}\n"
trans_do et = 
    case et of E "do" gcmds -> "do {\n" ++
                               concat (map (trans_gcmd "continue;\n") gcmds) ++ 
                               "} while ( false );\n"
trans_if et = 
    case et of E "if" gcmds -> "do {\n" ++
                               concat (map (trans_gcmd "break;\n") gcmds) ++ 
                               "abort();\n} while ( false );\n"

trans_declare et = 
    case et of E "declare" [E nm _] -> "var " ++ nm ++ ";\n"

trans_assign et = 
    case et of E "assign" [E nm _, vl] -> nm ++ "=" ++ trans_expr vl ++ ";\n"

trans_block et =
    case et of E "block" kids -> "{\n" ++ concat (map trans_stmt kids) ++ "}\n" 

trans_stmt et =  
    case et of E "assign" _ -> trans_assign et
               E "do" _ -> trans_do et 
               E "if" _ -> trans_if et 
               E "block" _ -> trans_block et 
               E "declare" _ -> trans_declare et 

trans_func et =
    case et of E "fn" [E nm _, args, body] -> "func " ++ nm ++ "() " ++ trans_block body

trans_tlvl et =
    case et of E "tlvl" kids -> concat (map trans_func kids)  
        
-- ============================================================================
-- ===  4. MAIN LOOP  =========================================================
-- ============================================================================

indent text = indent_inner (lines text) 0
indent_inner lns i = case lns of
                          [] -> ""
                          (l:ls) -> 
                              let count s c = length (filter (==c) s) 
                                  nb_open = count l '{'
                                  nb_close = count l '}'
                              in concat (replicate (i-nb_close) "    ") ++ l ++
                                 "\n" ++ indent_inner ls (i-nb_close+nb_open)

moo s = case parse tlvl s of
             Nothing -> "Failed to Parse" 
             Just (a, out) -> showtree "" a

goo s = case parse tlvl s of
             Nothing -> "Failed to Parse" 
             Just (a, out) -> indent (trans_tlvl a)

main = do putStr "hello!\n\n"
          contents <- readFile "moo.txt" 
          putStr (goo contents)
          putStr "\n\nbye!\n"
