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
    	render = pictures $ line:es'++ps'
    	white = Color (makeColor 1.0 1.0 1.0 1.0)
    	line  = white $ (Line [(0,0), mulSV 20 (cos (angle player), sin (angle player))])
    	es'   = map (\e -> white $ Translate (fst $ pos (eObj e)) (snd $ pos (eObj e)) $ Circle 10) enemies
        ps'   = concatMap ((map (\p -> white $ Translate (fst $ pos (pObj p)) (snd $ pos (pObj p)) $ Circle 5)).particles) pSystems

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