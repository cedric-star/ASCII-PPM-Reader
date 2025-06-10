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

    myData <- readFile "xrlab.ppm"
    let ppmMaybe = parsePPM myData
    case isValid ppmMaybe of
        Nothing   -> putStrLn "PPM P3 is not valid!"
        Just ppm  -> do
            putStrLn "PPM P3 is valid!"
            writeFile "./outputFiles/invertedXrlab.ppm" (show $ invert ppm)
            let grayPPM = p3top2 ppm
            logOrWrite "P3 not to P2 changable" "grayXrlab.ppm" grayPPM
            let bwPPM = grayPPM >>= p2top1
            logOrWrite "P2 not to P1 changable" "blackwhiteXrlab.ppm" bwPPM

    myDataP2 <- readFile "./outputFiles/grayXrlab.ppm"
    case isValid (parsePPM myDataP2) of
        Nothing -> putStrLn "pp2 nicht gültig"
        Just ppm -> putStrLn "pp2 ist gültig"

    myDataP1 <- readFile "./outputFiles/blackwhiteXrlab.ppm"
    case isValid (parsePPM myDataP1) of
        Nothing -> putStrLn "pp1 nicht gültig"
        Just ppm -> do
            putStrLn "pp1 ist gültig"
            writeFile "./outputFiles/invp1test.ppm" (show (invert ppm))

    putStrLn "------------------"
    putStrLn "------------------"
    putStrLn "------------------"
    putStrLn "Adding border"

    myEdge <- readFile "test.ppm"
    let mayTest = parsePPM myEdge
    case isValid mayTest of
        Nothing -> putStrLn "test not readanle"
        Just eppm -> do
            let edgyMedgy = addEdge 2 (200, 0, 0) eppm
            logOrWrite "ppm not edgeable" "testBorder.ppm" (Just edgyMedgy)

    putStrLn "------------------"





-- hallo hier unten sind nur Hilfsfunktionen...


-- schreibt 
logOrWrite::String->FilePath->Maybe PPM->IO()
logOrWrite failMsg path maybePPM =
    case maybePPM of
        Nothing -> putStrLn failMsg
        Just p  -> writeFile ("./outputFiles/" ++ path) (show p)

                    
