import PPMReader(
    PPM(..),
    parsePPM,
    isValid,
    p3top2,
    p2top1,
    invert,
    addEdge)


--MagicNumber 	Dateityp 	        Kodierung
--P1 	        Portable Bitmap 	ASCII
--P2 	        Portable Graymap 	ASCII
--P3 	        Portable Pixmap 	ASCII
-- -> P4-P6 sind analog, aber mit binärer codierung, werden hier NICHT behandelt, nur ASCII

main::IO ()
main = do
    let myPPM = PPM {pType=3, dimension=(600, 400), res = 255, payload=[1..6]}
    putStrLn $ show myPPM
    putStrLn "------------------"

    myData <- readFile "xrlab.ppm"
    let ppmMaybe = parsePPM myData
    case isValid ppmMaybe of
        Nothing   -> putStrLn "PPM P3 is not valid!"
        Just ppm  -> do
            putStrLn "PPM is valid!"
            writeFile "invertedXrlab.ppm" (show $ invert ppm)
            let grayPPM = p3top2 ppm
            logOrWrite "P3 not to P2 changable" "grayXrlab.ppm" grayPPM
            let bwPPM = grayPPM >>= p2top1
            logOrWrite "P2 not to P1 changable" "blackwhiteXrlab.ppm" bwPPM

    putStrLn "------------------"
    putStrLn "------------------"
    putStrLn "------------------"
    putStrLn "Adding border"





-- hallo hier unten sind nur Hilfsfunktionen...


-- schreibt 
logOrWrite::String->FilePath->Maybe PPM->IO()
logOrWrite failMsg path maybePPM =
    case maybePPM of
        Nothing -> putStrLn failMsg
        Just p  -> writeFile path (show p)

                    
