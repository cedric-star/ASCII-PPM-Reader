module PPMReader (
    PPM(..),
    parsePPM,
    isValid,
    p3top2,
    p2top1,
    invert
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

-- wen readFile aufgerufen wird, kommt der string daraus hierrein
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
        expected = ((fst $ dimension ppm)*(snd $ dimension ppm))*3
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
    pType = 3,
    dimension = (dimension ppm),
    res = (res ppm),
    payload = (calcInvPix (payload ppm) (res ppm))
}

addEdge::Int->(Int,Int,Int)->PPM->PPM
addEdge size color ppm = PPM {
    pType = (pType ppm),
    dimension = (newWidth, newHeight),
    res = (res ppm),
    payload = -- hier fehlt code
} 
    where
        newWidth = (2*size) + (fst (dimension ppm))
        newHeight = (2*size) + (snd (dimension ppm))


-- meinen kleinen Helferlein bohaaaa o_o -------------------------------

--invertiert jedes rot grün und gelb, ohne rücksicht auf pixel
calcInvPix::[Int]->Int->[Int]
calcInvPix [] _ = []
calcInvPix (rgb:rest) rs = [(rs-rgb)]++(calcInvPix rest rs)

-- wandelt graustufen in schwarz/weiß um
calcBinPix::[Int]->[Int]
calcBinPix [] = []
calcBinPix (g:gs) = [bin] ++ (calcBinPix gs)
    where bin = (\gr ->if gr > 128 then 0
                else 1) g

-- wandelt alle pixel mit rgb in pixel mit einem grauton um
calcGrayPix::[Int]->[Int]
calcGrayPix [] = []
calcGrayPix (r:g:b:rest) = [(round gray)] ++ (calcGrayPix rest)
    where gray = (fromIntegral (r+g+b))/3 ::Float

-- zeigt den payload ohne die klammern kommas und so an für dateiausgabe
showIntAr::[Int]->String
showIntAr [] = []
showIntAr (x:xs) = (show x) ++ "\n" ++ (showIntAr xs)

-- verpackt bereinigte Zeilen als PPM
myParser::[String]->Maybe PPM
myParser (t:d:r:p) = Just (PPM {pType=pType, dimension=dimension, res=res, payload=payload})
    where
        pType = read (tail t)
        dimension = (read (head dims), read (head $ tail dims))
        dims = words d
        res = read r
        payload = map read p
myParser _ = Nothing

-- löscht alle Kommentarzeilen
delComments::[String]->[String]
delComments lst = filter (\l -> head l /= '#') lst

trd::(a, b, c)->c
trd (_, _, drei) = drei