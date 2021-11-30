module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics


-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path (map tmp houseCOs) green Solid
    door :: PictureObject
    door  = Path (map tmp doorCOs) red Solid
-- these are the coordinates - convert them to a list of Point

tmp :: (Float, Float) -> Point
tmp (a,b) = Point a b


houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200), (615, 325), (615, 250), (650, 250), (650, 363),
         (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(550, 750), (550, 550), (650, 550), (650, 750)]

windowCOs :: [(Float, Float)]
windowCOs = [(350, 650), (350, 550), (450, 550), (450, 650), (350, 650)]

cyan :: Colour
cyan = Colour 96 192 255 255

window :: PictureObject
window = Path (map tmp windowCOs) cyan Solid

chimneyHouse :: Picture
chimneyHouse =  window :housePic



-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

movePoints ::  Vector -> Point -> Point
movePoints (Vector xv yv) (Point x y)
  = Point (x + xv) (y + yv)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) = Path ( map (movePoints vec) points) colour lineStyle
movePictureObject vec (Circle centerPO radiusPO colour lineStyle fillStylePO) = Circle (movePoint centerPO vec) radiusPO colour lineStyle fillStylePO
movePictureObject vec (Ellipse centerPO widthPO heightPO rotationPO colour lineStyle fillStylePO) = Ellipse (movePoint centerPO vec) widthPO heightPO rotationPO colour lineStyle fillStylePO
movePictureObject vec (Polygon points colour lineStyle fillStylePO) = Polygon ( map (movePoints vec) points) colour lineStyle fillStylePO




-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = map (circlePic col) [1*(400/n), 2*(400/n) .. n * (400/n)]

circlePic :: Colour -> Float -> PictureObject
circlePic colour float = Circle (Point 400 400) float colour Solid SolidFill


-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)
