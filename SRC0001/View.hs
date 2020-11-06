{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry
import Graphics.Gloss.Data.Vector

import Model

-- | Drawing

getQuad :: Vector -> Float -> Path
getQuad (x,y) w = [(x,y), (x+w,y), (x+w,y+w), (x, y+w)] 

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = render
    where 
    	render   = pictures $ mapLoc':localLoc':score':ship:bgps'++is'++es'++ps'++bs'

        --colors/shapes
    	white    = Color (makeColor 1.0 1.0 1.0 1.0)
        white'   = Color (makeColor 1.0 1.0 1.0 0.15)
        red      = Color (makeColor 1.0 0   0   1.0)
        blue     = Color (makeColor 0   0   1.0 1.0)
        blue'    = Color (makeColor 0   0   1.0 0.15)
        green    = Color (makeColor 0   1.0 0   1.0)
        shipWidth= 10
        shipPath = [(0,shipWidth), (0,-shipWidth), (30,0), (0,shipWidth)] 
        ship     = white $ Rotate (radToDeg (-angle)) $ Line shipPath
    	line     = white $ (Line [(0,0), mulSV 20 (cos (angle), sin (angle))])
        line' d  = white $ (Line [(0,0), d])

        --draw functions for objects
        mapLoc'  = Translate (385) 230 $ scale 0.1 0.1 (white $ text $ concatMap (show.mapLoc) backgrounds)
        localLoc'= Translate (385) 210 $ scale 0.1 0.1 (white $ text $ concatMap (show.localLoc) backgrounds)
        score'   = Translate (-385) 230 $ scale 0.1 0.1 (white $ text (show score))
        bs'      = map (\b -> let bLoc = pos (bObj b); bVel = vel (bObj b); bLoc' = bLoc + (mulSV (5.0 / (magV bVel)) bVel) in (bColor b) $ Pictures [(Line [bLoc, bLoc + (mulSV (5.0 / (magV bVel)) bVel)]), Translate (fst bLoc') (snd bLoc') $ circleSolid 2] ) bullets
        bColor b = case weapon of
                   0 -> Color $ makeColor 0.9098    0.8510 0.4196 (min (bLifeTime b) 1.0) -- Particle color
                   1 -> Color $ makeColor 1.0       0.9922 0.9765 (min (bLifeTime b) 1.0) -- white
                   2 -> Color $ makeColor 0.5412    1.0    0.8040 (min (bLifeTime b) 1.0) -- cyan
    	es'      = map (\e -> white $ Translate (fst $ pos (eObj e)) (snd $ pos (eObj e)) $ rectangleWire 10 10) enemies
        ps'      = map (\p -> pColor p $ Translate (fst $ pos (pObj p)) (snd $ pos (pObj p)) $ pForm p) (particles)
        pColor p = case pType p of
                   0 -> Color $ makeColor 1.0 0 (min (pLifeTime p) 1.0) (min (pLifeTime p) 1.0)
                   1 -> Color $ makeColor 1.0 1.0 1.0 (pLifeTime p)
                   2 -> Color $ makeColor 1.0 1.0 1.0 0.1
                   3 -> Color $ makeColor 0.9098 0.8510 0.4196 (pLifeTime p)
        pForm  p = case pType p of
                   0 -> Polygon $ rectanglePath 10 10
                   1 -> Polygon $ rectanglePath 10 10
                   2 -> rectangleWire 10 10
                   3 -> rectangleWire ((pLifeTime p) * 10) ((pLifeTime p) * 10)

        bgps'    = map (\p -> pColor p $ Translate (fst $ pos (pObj p)) (snd $ pos (pObj p)) $ polygon $ rectanglePath 3 3) (bgParticles)

        is'      = map (\i -> (iColor i) $ Translate (fst $ pos (iObj i)) (snd $ pos (iObj i)) $ thickCircle 10 2) (upgrade:bonus)
        iColor i = case iType i of
                   0 -> Color $ makeColor 0.9098    0.8510 0.4196 (min (iLifeTime i) 1.0) -- Particle color
                   1 -> Color $ makeColor 1.0       0.9922 0.9765 (min (iLifeTime i) 1.0) -- white
                   2 -> Color $ makeColor 0.5412    1.0    0.8040 (min (iLifeTime i) 1.0) -- cyan

    {-	transPos 	= Translate (fst $ pos player) (snd $ pos player)
    	transPos'	= Translate ((-) 0 $ fst $ pos player) ((-) 0 $ snd $ pos player)
    	ppos 		= mulSV (0 - 0.5) $ pos player
    	white 		= Color (makeColor 1.0 1.0 1.0 1.0)
    	white' 		= Color (makeColor 1.0 1.0 1.0 0.5)
    	white''		= Color (makeColor 1.0 1.0 1.0 0.25)
    	dirLine 	= white $ (Line [dir player, (0,0)])
    	ellipse 	= white $ Circle 20
    	field 		= map (\(x,y) -> white' $ Translate x y $ transPos' $ Polygon $ getQuad (x,y) 10) bg0
    	field' 		= map (\(x,y) -> white'' $ Translate x y $ Translate (fst ppos) (snd ppos) $ Polygon $ getQuad (x,y) 5) bg0
    	render 		= pictures $ field ++ field' ++ [dirLine, ellipse]

-}
