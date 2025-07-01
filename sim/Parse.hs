module Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import HSelect.Types as T
import System.Random

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pos :: Parser T.Pos 
pos = do
    void $ char '<'
    xcoord <- integer
    void $ lexeme (char ',')
    ycoord <- integer
    void $ char '>'
    return (xcoord, ycoord )

integer :: Parser Int
integer = do
    i <- lexeme (some (digitChar))
    return (read i :: Int)

world :: Parser (Int, Int, Int)
world = do
    void $ string "world"
    void $ lexeme (char '(')
    cols <- integer
    void $ lexeme (char ',')
    rows <- integer
    void $ lexeme (char ',')
    seed <- integer
    void $ lexeme (char ')')
    void $ lexeme (char '.')
    return (cols, rows, seed)

spider :: Parser Spider
spider = do
    void $ lexeme (string "spider")
    void $ lexeme (char '(')
    posn <- pos 
    void $ lexeme (char ',')
    energy <- integer
    void $ lexeme (char ')')
    void $ lexeme (char '.')
    return (Spider posn energy)

plant :: Parser Plant
plant = do
    void $ lexeme (string "plant")
    void $ lexeme (char '(')
    posn <- pos
    void $ lexeme (char ',')
    energy <- integer
    void $ lexeme (char ')')
    void $ lexeme (char '.')
    return (Plant posn energy)

genezerod :: Parser Gene
genezerod = do
    op <- lexeme (string "nop" <|> string "endif")
    if op == "nop"
    then
        return NOP
    else
        return EndIf

geneoned :: Parser Gene
geneoned = do
    op <- lexeme (string "up" <|> string "down" <|> string "left" <|> string "right" <|> string "neg" <|> string "get_nearest_spider" <|> string "get_nearest_plant" <|> string "reproduce" )
    void $ lexeme (char '(')
    i <- integer
    void $ lexeme (char ')')
    return (retoned op i)

genetwod :: Parser Gene
genetwod = do
    op <- lexeme (string "place" <|> string "get_mag" <|> string "if_lt" <|> string "if_gt" <|> string "get_x" <|> string "get_y")
    void $ lexeme (char '(')
    i1 <- integer
    void $ lexeme (char ',')
    i2 <- integer
    void $ lexeme (char ')')
    return (rettwod op i1 i2)

gene :: Parser Gene
gene = do
    g <- lexeme ( genetwod <|> geneoned <|> genezerod )
    return g

retoned :: String -> Int -> Gene
retoned "up" i = Up i
retoned "down" i = Down i
retoned "left" i = T.Left i
retoned "right" i = T.Right i
retoned "neg" i = Neg i
retoned "get_nearest_spider" i = GetNearestSpider i
retoned "get_nearest_plant" i = GetNearestPlant i
retoned "reproduce" i = Reproduce i

rettwod :: String -> Int -> Int -> Gene
rettwod "place" i1 i2 = Place i1 i2
rettwod "get_mag" i1 i2 = GetMag i1 i2
rettwod "if_lt" i1 i2 = IfLt i1 i2
rettwod "if_gt" i1 i2 = IfGt i1 i2
rettwod "get_x" i1 i2 = GetX i1 i2
rettwod "get_y" i1 i2 = GetY i1 i2

geneComma :: Parser Gene
geneComma = do
    g <- lexeme gene
    void $ lexeme (char ',' <|> char ']')
    return g

genes :: Parser [Gene]
genes = do
    void $ lexeme (char '[')
    geneList <- lexeme $ (many geneComma)
    return (geneList)

posComma :: Parser T.Pos
posComma = do
    p <- pos
    void $ lexeme (char ',' <|> char ']')
    return p

posns :: Parser [T.Pos]
posns = do
    void $ lexeme (char '[')
    poss <- lexeme (many posComma)
    return (poss)

double :: Parser Double
double = do
    first <- lexeme (many digitChar)
    void $ char '.'
    second <- (many digitChar)
    return (read (first ++ "." ++ second) :: Double)

scratchComma :: Parser Double
scratchComma = do
    d <- double
    void $ lexeme (char ',' <|> char ']')
    return d

scratches :: Parser [Double]
scratches = do
    void $ lexeme (char '[')
    first <- lexeme (many scratchComma)
    return (first )

bug :: Parser Bug 
bug = do
    void $ lexeme (string "bug")
    void $ lexeme (char '(')
    p <- pos
    void $ lexeme (char ',')
    i <- integer
    void $ lexeme (char ',')
    g <- genes
    void $ lexeme (char ',')
    ps <- posns
    void $ lexeme (char ',')
    ss <- scratches
    void $ lexeme (char ')')
    void $ lexeme (char '.')
    return (Bug { bugPosn = p, bugEnergy = i, bugGenes = g, bugCurrentGene = 0, bugScratchPosns = ps, bugScratchDoubles = ss })

file :: Parser World
file = do
    (cols, rows, seed) <- world
    let rands = randoms (mkStdGen seed) :: [Int]
    spiders <- lexeme (many spider)
    plants <- lexeme (many plant)
    bugs <- lexeme (many bug)
    return (World cols rows rands spiders plants bugs [])
