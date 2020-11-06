{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry
import Graphics.Gloss.Data.Vector

import Model

-- | Drawing

draw :: Float -> Float -> World -> IO Picture
draw horizontalResolution verticalResolution world@(World{..})
  = 
  case state of
    0 -> return render'
    1 -> return render
  where 
    render   = pictures $ ammo'++bgps'++is'++es'++ps'++
                (score':hScore:lives':bs' ++ ship)
    render'  = pictures $ render:menuBg:menuTitle:menuPlay:menuDiff:menuHS:hs
    -- Colors
    white     = makeColor 1.0      0.9922  0.9765  1.0
    white'    = makeColor 1.0      0.9922  0.9765  0.15
    black     = makeColor 0.047 0.043 0.055 1.0
    blackP    = makeColor 0.047 0.043 0.055
    yellowP   = makeColor 1.0   0.773 0.447 
    whiteP    = makeColor 1.0      0.9922  0.9765 
    cyanP     = makeColor 0.357   0.910     0.992 
    crimsonP  = makeColor 0.863   0.071       0.325 

    -- Menu
    sw        = horizontalResolution/2
    sh        = verticalResolution/2
    menuT     = "LAMBDA WARS"
    menuH     = "HIGH SCORES"
    menuD     = "Difficulty <" ++ show menuState ++ ">"
    menuP     = "Press <space> to play!"
    menuBg    = Color (blackP 0.8) $ rect (sw*4) (sh*4)
    menuTitle = Translate (-sw/3 - sw/25) (sh/1.5) $ scale 0.4 0.4 $ Color (crimsonP 1) $ text menuT
    menuDiff  = Translate (-sw/3 + sw/4- sw/24) (-verticalResolution/2 + 100) $ scale 0.15 0.15 $ Color white $ text menuD
    menuPlay  = Translate (-sw/3 + sw/11- sw/20) (-verticalResolution/2 + 50) $ scale 0.2 0.2 $ Color white $ text menuP
    menuHS    = Translate (-sw/3 + sw/5- sw/24) (sh/3) $ scale 0.2 0.2 $ Color white $ text menuH
    hs        = map (\(x,y) -> Translate (-sw/3 + sw/5 + 10- sw/20) (sh/3 - 10 + x * (-20)) $ scale  0.1 0.1 $ Color (mixColors (x/10) (1-(x/10)) (yellowP 1) (crimsonP 1)) $ text $ show y) $ zip [1..] highScoreList
    -- Shapes
    -- Player
    shipWidth = 10
    shipPath  = [(0,shipWidth), (7, 0),  (0,-shipWidth), (30,0), (0,shipWidth)]
    ship'     = Color white $ Rotate (radToDeg (-angle)) $ Line shipPath
    shipInside= Color black $ Rotate (radToDeg (-angle)) $ Translate 3 0 $ Polygon shipPath
    ship      = [shipInside, ship']
    

    --enemyP    = Line $ map (mulSV 8) 

    -- Enemies
    rect      = \x y -> Polygon $ rectanglePath x y
    rectW     = \x y -> rectangleWire x y
    asteroidPaths = [(0, 1), ((-1), 0.2), ((-0.4), (-1)), (0.4,(-1)), (0.6, (-0.3)), (1, (-0.15)), (1, 0.35), (0, 1)] :
                    [((-0.5), 1), ((-1), 0.2), ((-0.9), (-0.9)), (1.2, (-1)), (1, 0.2), (0.15, 0), (0.2, 1), ((-0.5), 1)] : 
                    [((-1), 0.1), ((-1), (-0.8)), (0, (-1)), (0.8, (-0.7)), (1, (-0.2)), ((-0.2), 1), ((-1), 0.1)] : 
                    [((-1), (-0.2)), ((-1), (-1)), (0.25, (-0.6)), (0.9, 0.1), ((-0.15), 0.8), ((-0.45), 0.7), ((-0.45), (-0.05)), ((-1), (-0.2)) ]: []

    --draw functions for objects
    -- Text
    {-
    mapLoc'  = Translate (385) 230 $ scale 0.1 0.1 
                (white $ text $ concatMap (show.mapLoc) backgrounds)
    localLoc'= Translate (385) 210 $ scale 0.1 0.1 
                (white $ text $ concatMap (show.localLoc) backgrounds)
    -}
    score'   = Translate (-385) 230 $ scale 0.1 0.1 
                (Color white $ text (show score))
    hScore   = Translate (-385) 210 $ scale 0.1 0.1 
                (Color white $ text (show $ head highScoreList))
    lives'   = pictures $ take (lives - 1) $ iterate (Translate (-28) 0) $ translate x y $ scale 0.5 0.5 $ Rotate (-90) $ pictures [Color white $ Polygon shipPath, Color black $ Polygon [(0,shipWidth), (7, 0),  (0,-shipWidth)]]
              where
                x = horizontalResolution/2 - 80
                y = verticalResolution/2 - 80
      

    --HuD
    ammo' = map (\p -> Color (pColor p) $ Translate (x p) (y p) $ circleSolid 1) 
                ammo
              where
                x = fst . pos . pObj  
                y = snd . pos . pObj 
    
    -- Projectiles / Bullets
    bs'      = map (\b -> 
              let 
                bLoc = pos (bObj b)
                bVel = vel (bObj b)
                bLoc' = bLoc + (mulSV (10.0 / (magV bVel)) bVel) 
              in 
                Color (bColor b) $ Pictures [(Line [bLoc, bLoc']), 
                Translate (fst bLoc') (snd bLoc') $ circleSolid 2] ) bullets
    bColor b = case weapon of
               0 -> yellowP lt 
               1 -> whiteP  lt 
               2 -> cyanP   lt 
               3 -> crimsonP lt
               where
                lt = (min (bLifeTime b) 1.0)
    -- Enemies
    es'      = map (\e -> Color white $ Translate (x e) (y e) $ Rotate (a e) $ ePath e) enemies
              where
                x = fst . pos . eObj
                y = snd . pos . eObj
                a = (\x -> fst x - snd x) . pos . eObj
                ePath e = Line $ map (mulSV 8) $ asteroidPaths !! mod (eShape e) (length asteroidPaths)
    -- Particles
    ps'      = map (\p -> Color (pColor p) $ Translate (x p) (y p) $ pForm p) 
                (particles)
              where
                x = fst . pos . pObj  
                y = snd . pos . pObj 
    pColor p = case pType p of
               0 -> mixColors (lt) (1-lt) (yellowP (min lt 1.0)) (crimsonP (min lt 1.0))
               1 -> mixColors (lt) (1-lt) (whiteP lt) (yellowP lt)
               2 -> mixColors (1-((5-lt)/15.0)) (lt) (yellowP ((5-lt)/15.0)) (crimsonP ((5-lt)/15.0)) 
               3 -> mixColors (lt/10) (1-lt) (cyanP $lt/2) (yellowP $lt/2)    
               where
                lt = pLifeTime p
    pForm  p = case pType p of
               0 -> rect  10 10
               1 -> rect  (lt' * 20) (lt' * 20)
               2 -> rectW 10 10
               3 -> circleSolid (lt' * 5) --(lt' * 5)
               where
                lt' = (pLifeTime p)

    -- Background
    bgps'    = map (\p -> Color (pColor p) $ Translate (x p) (y p) $ rect (rad p) (rad p)) 
                (bgParticles)
             where
              x = fst . pos . pObj
              y = snd . pos . pObj
              rad = (*2) . ((-) 5) . pLifeTime
    
    -- Items
    is'      = map (\i -> Color (iColor i) $ Translate (x i) (y i) $ thickCircle (radius i) 2) 
                (upgrade:bonus)
             where
              x = fst . pos . iObj
              y = snd . pos . iObj
    iColor i = case iType i of
               0 -> yellowP lt
               1 -> whiteP  lt
               2 -> cyanP   lt
               3 -> crimsonP lt
               where
                lt = (min (iLifeTime i) 1.0) 
    radius i = if pickedUp i then 10 + (0.25 - lt) * 100 else 10
              where
                lt = (min (iLifeTime i) 1.0)
