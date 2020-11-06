{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry
import Graphics.Gloss.Data.Vector

import System.Random

import Model

-- | Time handling

addV :: Vector -> Vector -> Vector
addV (u1,u2) (v1,v2) = (u1+v1, u2+v2)

timeHandler :: Float -> World -> World
timeHandler time world@(World{..}) = world{player=p, enemies = es, items = is, pSystems = map ParticleSystem (filter (not.null) ps)}
            where
                -- constants
                theta = pi/50
                decay = 0.98

                -- variables
                ang = angle player + case rotateAction of
                    RotateLeft  -> theta
                    RotateRight -> (-theta)
                    NoRotation  -> 0
                acc = case movementAction of 
                    Thrust      -> (0, 1.3) 
                    NoMovement  -> (0,0)

                -- player 
                vel'   = mulSV (speed player) (cos(ang), sin(ang))  
                vel''  = mulSV decay $ addV vel' $ rotateV (ang) acc   
                p       = Player (score player) ang (magV vel'')
                
                -- global movement
                pMove o = o {pos = addV (pos o) (mulSV (-1) vel'') }
                
                -- updaters
                es      = map (eGT) (enemies)
                eM      = \o -> o {pos = mulSV (0.995) (pos o)}
                eGT     = \e -> e{eObj = (pMove.eM) (eObj e)}
                dying   = \p -> p{pLifeTime = (pLifeTime p) - (time)}
                up      = \o -> o{pos = addV (pos o) (vel o), vel = mulSV decay (vel o)} 
                ps      = map ((map ((\p -> p{pObj = (pMove . up) (pObj p)}).dying)) . (filter (\p -> (pLifeTime p) > 0)) .particles) (pSystems)
                is      = map (\i -> i{iObj = pMove (iObj i)}) (items)
 {- world{player=p}
                                    where 
                                        ang     = case rotateAction of
                                            RotateLeft  -> (pi/50)
                                            RotateRight -> 0 - (pi/50)
                                            NoRotation  -> 0 
                                        acc    = case movementAction of
                                            Thrust      -> (0, 0.3) 
                                            NoMovement  -> (0,0)
                                        part    = case shootAction of
                                            Shoot      -> map (flip rotateV direc) [0,pi/16 .. 2 * pi] ++ particles player
                                            DontShoot  -> particles player
                                        part'   = take 32 $ map (mulSV 1.1) part
                                        velo    = mulSV 0.98 $ addV (vel player) $ rotateV (angle player + ang) acc
                                        direc   = rotateV (ang) $ dir player
                                        newPos  = addV (pos player) velo
                                        tr      | (length $ trail player) < 100 = pos player : trail player
                                                | otherwise                  = pos player : (init $ trail player)
                                        p       = Player (newPos) (direc) (velo) (angle player + ang) tr part'
}
                                    
-}                              
                                            


{-case rotateAction of
                                        RotateLeft -> world{position=(fst position - 5.0, snd position)}
                                        RotateRight -> world{position=(fst position + 5.0, snd position)}
                                        NoRotation -> world
-}