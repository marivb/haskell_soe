module Main where

import Picture

xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5, 2.5), (-3.0, 0), (-1.7, -1.0),
                     (-1.1, 0.2), (-1.5, 2.0)])

reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
pic1 = Region Blue reg1

reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in (Scale (2, 2) circle) `Union` (Translate (1, 0) square)
          `Union` (Translate (-1, 0) square)
pic2 = Region Yellow (Translate (0, -1) reg2)

pic3 = pic2 `Over` pic1

-- main = draw "Chapter 10 Example" pic3

p1, p2, p3, p4 :: Picture
p1 = Region Red r1
p2 = Region Blue r2
p3 = Region Green r3
p4 = Region Yellow r4

pic :: Picture
pic = foldl Over EmptyPicture [p1, p2, p3, p4]
main = draw2 "Picture Clicks" pic
