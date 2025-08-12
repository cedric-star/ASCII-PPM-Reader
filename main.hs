import PPMReader(
    PPM(..),
    parsePPM,
    isValid,
    p3top2,
    p2top1,
    invert,
    addEdge)

import System.Directory

main :: IO ()
main = do
    createDirectory "out"
    putStrLn "(0)   convert p3 to p2"
    putStrLn "(1)   convert p2 to p1"
    putStrLn "(2)   invert ppm for all three types"
    putStrLn "(3)   add Edge to ppm (only p3)"
    putStrLn "------------------------------------"
    putStrLn ""
    putStrLn "put in .ppm file:"
    filePath <- getLine
    putStrLn "choose action (0-3):"
    action <- getLine
    content <- readFile filePath
    let ppmMaybe = parsePPM content
    case isValid ppmMaybe of
        Nothing -> putStrLn "PPM file is not valid!"
        Just ppm -> case action of
            "0" -> convertP3toP2 ppm
            "1" -> convertP2toP1 ppm
            "2" -> invertPPM ppm
            "3" -> addEdgePPM ppm
            _   -> putStrLn "Invalid action!"

-- (0) P3 zu P2
convertP3toP2 :: PPM -> IO ()
convertP3toP2 ppm =
    case p3top2 ppm of
        Nothing -> putStrLn "Not a P3 file or conversion failed."
        Just gray -> do
            writeFile "./out/gray.ppm" (show gray)
            putStrLn "Converted to P2 and saved as ./out/gray.ppm"

-- (1) P2 zu P1
convertP2toP1 :: PPM -> IO ()
convertP2toP1 ppm =
    case p2top1 ppm of
        Nothing -> putStrLn "Not a P2 file or conversion failed."
        Just bw -> do
            writeFile "./out/bw.ppm" (show bw)
            putStrLn "Converted to P1 and saved as ./out/bw.ppm"

-- (2) invertieren
invertPPM :: PPM -> IO ()
invertPPM ppm = do
    writeFile "./out/invert.ppm" (show $ invert ppm)
    putStrLn "Inverted and saved as ./out/invert.ppm"

-- (3) addEdge
addEdgePPM :: PPM -> IO ()
addEdgePPM ppm = do
    putStrLn "Edge size (Int):"
    sizeStr <- getLine
    let size = read sizeStr :: Int
    putStrLn "Edge color as R G B (z.B. 255 0 0):"
    colorStr <- getLine
    let [r,g,b] = map read (words colorStr)
    let edged = addEdge size (r,g,b) ppm
    writeFile "./out/edged.ppm" (show edged)
    putStrLn "Edge added and saved as ./out/edged.ppm"