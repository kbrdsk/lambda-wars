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
    render   = pictures $ bgps'++is'++es'++ps'++
                (mapLoc':localLoc':score':hScore:bs' ++ [ship])
    render'  = pictures $ render:menuBg:menuTitle:menuHS:hs
    -- Colors
    white     = Color $ makeColor 1.0      0.9922  0.9765  1.0
    white'    = Color $ makeColor 1.0      0.9922  0.9765  0.15
    black     = Color $ makeColor 0.0863 0.0863 0.1137 1.0
    blackP    = Color . makeColor 0.0863 0.0863 0.1137 
    yellowP   = Color . makeColor 0.9098   0.8510  0.4196 
    whiteP    = Color . makeColor 1.0      0.9922  0.9765 
    cyanP     = Color . makeColor 0.5412   1.0     0.8040 
    purpleP   = Color . makeColor 0.5412   0       0.8040 

    -- Menu
    menuBg    = blackP 0.5 $ rect horizontalResolution verticalResolution
    menuTitle = Translate (-200) (200) $ scale 0.4 0.4 $ white $ text "LAMBDA WARS"
    menuHS    = Translate (-200) (50) $ scale 0.2 0.2 $ white $ text "HIGH SCORES"
    hs        = map (\(x,y) -> Translate (-160) (x * (-20)) $ scale  0.1 0.1 $ white $ text $ show y) $ zip [1..] highScoreList
    -- Shapes
    -- Player
    shipWidth = 10
    shipPath  = [(0,shipWidth), (7, 0),  (0,-shipWidth), (30,0), (0,shipWidth)] 
    ship      = white $ Rotate (radToDeg (-angle)) $ Line shipPath
    ship'     = black $ Rotate (radToDeg (-angle)) $ Translate 2 0 $ Polygon $ map (mulSV 0.7) shipPath
    -- Enemies
    rect      = \x y -> Polygon $ rectanglePath x y
    rectW     = \x y -> rectangleWire x y
    asteroidPaths = [((-0.5), 1), ((-1), 0.2), ((-0.9), (-0.9)), (1.2, (-1)), (1, 0.2), (0.15, 0), (0.2, 1), ((-0.5), 1)]
    
    asteroidPath = [(cos x * 1, sin y * 1) | x <- circular, y <- circular, r <- take 6 $ map (\(x,y) -> ((+) 0.3 $ abs $ x/400,(+) 0.3 $ abs $ y/400)) randomList]
                  where
                    circular = [0, pi/3 .. pi*2]
    enemyP    = Line $ map (mulSV 17) $ asteroidPath
    
    --draw functions for objects
    -- Text
    mapLoc'  = Translate (385) 230 $ scale 0.1 0.1 
                (white $ text $ concatMap (show.mapLoc) backgrounds)
    localLoc'= Translate (385) 210 $ scale 0.1 0.1 
                (white $ text $ concatMap (show.localLoc) backgrounds)
    score'   = Translate (-385) 230 $ scale 0.1 0.1 
                (white $ text (show score))
    hScore   = Translate (-385) 210 $ scale 0.1 0.1 
                (white $ text (show $ head highScoreList))
    
    -- Projectiles / Bullets
    bs'      = map (\b -> 
              let 
                bLoc = pos (bObj b)
                bVel = vel (bObj b)
                bLoc' = bLoc + (mulSV (5.0 / (magV bVel)) bVel) 
              in 
                (bColor b) $ Pictures [(Line [bLoc, bLoc']), 
                Translate (fst bLoc') (snd bLoc') $ circleSolid 2] ) bullets
    bColor b = case weapon of
               0 -> yellowP lt 
               1 -> whiteP  lt 
               2 -> cyanP   lt 
               3 -> purpleP lt
               where
                lt = (min (bLifeTime b) 1.0)
    -- Enemies
    es'      = map (\e -> white $ Translate (x e) (y e) $ Rotate (a e) $ enemyP) enemies
              where
                x = fst . pos . eObj
                y = snd . pos . eObj
                a = (\x -> fst x - snd x) . pos . eObj
    -- Particles
    ps'      = map (\p -> pColor p $ Translate (x p) (y p) $ pForm p) 
                (particles)
              where
                x = fst . pos . pObj  
                y = snd . pos . pObj 
    pColor p = case pType p of
               0 -> purpleP $ min lt 1.0
               1 -> whiteP  lt
               2 -> yellowP  ((5-lt)/15.0)
               3 -> yellowP lt
               where
                lt = pLifeTime p
    pForm  p = case pType p of
               0 -> rect  10 10
               1 -> rect  10 10
               2 -> rectW 10 10
               3 -> rectW lt' lt'
               where
                lt' = (pLifeTime p) * 10

    -- Background
    bgps'    = map (\p -> pColor p $ Translate (x p) (y p) $ rect (rad p) (rad p)) 
                (bgParticles)
             where
              x = fst . pos . pObj
              y = snd . pos . pObj
              rad = (*2) . ((-) 5) . pLifeTime
    
    -- Items
    is'      = map (\i -> (iColor i) $ Translate (x i) (y i) $ thickCircle (radius i) 2) 
                (upgrade:bonus)
             where
              x = fst . pos . iObj
              y = snd . pos . iObj
    iColor i = case iType i of
               0 -> yellowP lt
               1 -> whiteP  lt
               2 -> cyanP   lt
               3 -> purpleP lt
               where
                lt = (min (iLifeTime i) 1.0) 
    radius i = if pickedUp i then 10 + (0.25 - lt) * 100 else 10
              where
                lt = (min (iLifeTime i) 1.0)