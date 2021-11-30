module Art where

import ShapeGraphics
import Codec.Picture

art :: Picture
art = tree 12 (Point 470 800) 0.58 (Vector (-110) 0) green

tree :: Int -> Point -> Float -> Vector  -> Colour -> Picture
tree depth treeBase factor treeDirection startColour
  = [Path (tree' depth (vectorLine treeBase treeDirection)) startColour Solid]
  where

    tree' :: Int -> Line -> [Point]
    tree' 0 line = []  
    tree' n line 
      = if (n `mod` 7 /= 0)
        then
        [p1, p4] ++ tree' (n-1) (Line p4 p5) ++
                    tree' (n-1) (Line p5 p3) ++
        [p3,p2] 
        else
        [p1, p4] ++ tree' (n-1) (Line p5 p4) ++
                    tree' (n-1) (Line p4 p5) ++
                    tree' (n-1) (Line p5 p3) ++
                    
        [p3,p2] 
      where 

        flipLine line1 = Line (endLine line1) (startLine line1)
        [p1,p2,p3,p4,_]   = polygon 4 line
        p5           = endLine $ rotateLine (factor * pi ) $ 
                              flipLine $ 
                                  scaleLine (0.5 ) (Line p3 p4)



spiral :: Float -> Float -> Int -> Line -> [Point]
spiral angle scaleFactor n line
  = spiralR' n line
  where
    spiralR' n line
      | n <= 0    = []
      | otherwise = p1 : spiralR' (n-1) newLine
      where
        p1 = startLine line
        p2 = endLine line
        newLine = connectLine line 
                    (scaleLine scaleFactor (rotateLine angle line))


startLineFrom :: Point -> Line -> Line
startLineFrom point line
  = Line (Point x0 y0) (Point (x0 + xE - xS) (y0 + yE - yS))
  where
    x0 = xPoint point
    y0 = yPoint point

    xS = xPoint (startLine (line))
    yS = yPoint (startLine (line))
    xE = xPoint (endLine (line))
    yE = yPoint (endLine (line))


connectLine :: Line -> Line -> Line
connectLine l1 l2 
  = startLineFrom (endLine (l1)) l2


polygon :: Int -> Line -> [Point]
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
  where 
    rotationAngle = (2 * pi) / (fromIntegral n)



rotateLine :: Float -> Line -> Line
rotateLine alpha line
  = Line (Point x1 y1) (Point ((cos   alpha) * nx - (sin  alpha) * ny + x1) 
     ((sin  alpha) * nx +(cos  alpha) * ny + y1))
  where
    (nx, ny) = (x2 - x1, y2 - y1)
    x1 = xPoint (startLine (line))
    y1 = yPoint (startLine (line))
    x2 = xPoint (endLine (line))
    y2 = yPoint (endLine (line))

scaleLine :: Float -> Line -> Line
scaleLine f line
  = Line (Point x1 y1) (Point (x1 + (x2 -x1) * f)  (y1 + (y2 - y1) * f))
  where
    x1 = xPoint (startLine (line))
    y1 = yPoint (startLine (line))
    x2 = xPoint (endLine (line))
    y2 = yPoint (endLine (line))

movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector dx dy)
  = Point (x + dx) (y + dy)



vectorLine :: Point -> Vector -> Line
vectorLine base vector = Line base $ movePoint base vector





writeToFile
  = writePng "art.png" (drawPicture 1 art)
