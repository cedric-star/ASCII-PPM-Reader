module PPMReader (
    PPM(..),
    parsePPM,
    isValid,
    p3top2,
    p2top1,
    invert,
    addEdge
) where

import Text.Read

data PPM = PPM {
    pType::Int,
    dimension::(Int,Int),
    res::Int, --8Bit, also 255 erwartet (als maximalwert)
    payload::[Int]
}

instance Show PPM where
    show (PPM t d r p) = 
        "P"++(show t)++"\n"++
        (show $ fst d)++" "++
        (show $ snd d)++"\n"++
        (show r)++"\n"++
        (showIntAr p)

-- aufrufende (wrapper) Funktion zum Parsen des Inhalts
parsePPM::String->Maybe PPM
parsePPM content = myParser cleared
    where 
        myLines = lines content
        cleared = delComments myLines

-- schaut ob die Pixel (payload) mit angegebener Dimension (width, height) übereinstimmen
isValid::Maybe PPM->Maybe PPM
isValid Nothing = Nothing
isValid (Just ppm) = if expected == actual
                     then Just ppm
                     else Nothing
    where
        pixMult = (\x -> case x of
            1 -> 1
            2-> 1
            3-> 3) (pType ppm)
        expected = ((fst $ dimension ppm)*(snd $ dimension ppm))*pixMult
        actual = length (payload ppm)

p3top2::PPM-> Maybe PPM
p3top2 ppm = if (pType ppm) /= 3 then Nothing
             else Just (PPM {
                pType = 2,
                dimension = (dimension ppm),
                res = (res ppm),
                payload = calcGrayPix (payload ppm)
             })

p2top1::PPM-> Maybe PPM
p2top1 ppm = if (pType ppm) /= 2 then Nothing
           else Just (PPM {
                pType = 1,
                dimension = (dimension ppm),
                res = (res ppm),
                payload = calcBinPix (payload ppm)
             })

-- nur für P3 und P2 
invert::PPM->PPM
invert ppm = PPM {
    pType = (pType ppm),
    dimension = (dimension ppm),
    res = (res ppm),
    payload = (calcInvPix (pType ppm) (payload ppm) (res ppm))
}

-- baut ppm mit neuen width height und payload
addEdge::Int->(Int,Int,Int)->PPM->PPM
addEdge size color ppm = PPM {
    pType = (pType ppm),
    dimension = (newWidth, newHeight),
    res = (res ppm),
    payload = newPayload
} 
    where
        newWidth = (2*size) + (fst (dimension ppm))
        newHeight = (2*size) + (snd (dimension ppm))
        newPayload = addTopBots (3*newWidth) ((fst (dimension ppm))*3) size color (payload ppm)

-- Hilfsfunktionen ------------------------------------------------------

addTopBots::Int->Int->Int->(Int,Int,Int)->[Int]->[Int]
addTopBots width oldWidth size color pload = botOrTopRows ++ rest ++ botOrTopRows
    where 
        colList = triplToList color 
        botOrTopRows = take (width*size) (cycle colList)
        rest = addLR chonk (sublistFrom pload oldWidth)
        chonk = take (size*3) (cycle colList) --chonk = one part of row that come before or after every old pixel line

addLR::[Int]->[[Int]]->[Int]
addLR _ [] = []
addLR chonk (x:xs) = chonk++x++chonk++addLR chonk xs

sublistFrom :: [a] -> Int -> [[a]]
sublistFrom [] _ = []
sublistFrom xs n
    | n <= 0    = error "Index muss > 0 sein"
    | otherwise = take n xs : sublistFrom (drop n xs) n


-- invertiert jedes rot grün und blau, ohne rücksicht auf pixel
calcInvPix::Int->[Int]->Int->[Int]
calcInvPix _ [] _ = []
calcInvPix (1) (bin:rest) rs = [converted] ++ calcInvPix 1 rest rs
    where
        converted = (\x -> if x==1 then 0 
                        else 1) bin
calcInvPix (2) lst rs = calcInvPix 3 lst rs
calcInvPix (3) (rgb:rest) rs = (rs - rgb) : calcInvPix 3 rest rs

-- wandelt graustufen in schwarz/weiß um
calcBinPix::[Int]->[Int]
calcBinPix [] = []
calcBinPix (g:gs) = bin : calcBinPix gs
    where bin = if g > 128 then 0 else 1

-- wandelt alle pixel mit rgb in pixel mit einem grauton um
calcGrayPix::[Int]->[Int]
calcGrayPix [] = []
calcGrayPix (r:g:b:rest) = round gray : calcGrayPix rest
    where gray = (fromIntegral (r+g+b))/3 :: Float

-- zeigt den payload ohne die klammern kommas und so an für dateiausgabe
showIntAr::[Int]->String
showIntAr [] = ""
showIntAr (x:xs) = show x ++ "\n" ++ showIntAr xs

-- verpackt bereinigte Zeilen als PPM
myParser::[String]->Maybe PPM
myParser (t:d:r:p) = Just (PPM {pType=pType, dimension=dimension, res=res, payload=payload})
    where
        pType = read (tail t)
        dimension = (read (head dims), read (dims !! 1))
        dims = words d
        res = read r
        payload = map read p
myParser _ = Nothing

-- löscht alle Kommentarzeilen
delComments::[String]->[String]
delComments lst = filter (\l -> not (null l) && head l /= '#') lst

trdT::(a, b, c)->c
trdT (_, _, drei) = drei

sndT::(a,b,c)->b
sndT (_, zwei, _) = zwei

fstT::(a,b,c)->a
fstT (eins, _, _) = eins

triplToList::(a,a,a)->[a]
triplToList (a, b, c) = [a, b, c]