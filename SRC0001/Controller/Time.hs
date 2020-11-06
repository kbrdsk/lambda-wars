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
timeHandler time world@(World{..}) = world{enemies = es, bullets = bs, bonus = bonus', upgrade = upgrade', particles = ps', bgParticles = pMap, score = newScore, multiplier = newMult, velocity = vel', angle = ang, cooldown = cd, weapon = weap', backgrounds = bgs, randomList = rans}
            where
                -- constants
                theta  = pi/40
                decay  = 0.98
                player = Object (0,0) velocity

                -- variables
                ang   = if crash then 0 else angle + case rotateAction of
                      RotateLeft  -> theta
                      RotateRight -> (-theta)
                      NoRotation  -> 0
                acc   = case movementAction of 
                      Thrust      -> (0.15, 0) 
                      NoMovement  -> (0,0)

                -- randoms 
                (ran1:ran2:rans) = randomList

                -- player   
                vel'   = if crash then (0,0) else mulSV decay $ addV velocity $ rotateV (ang) acc   
                cd     = mod (cooldown + 1) weapCd


                --background movement
                bgs    = map moveBG backgrounds
                moveBG m = m {localLoc = lLoc, mapLoc = mLoc}
                         where
                         lLoc   = (lX', lY')
                         (lX,lY) = addV (localLoc m) (mulSV (plFac m) vel')
                         lX'   | abs lX > 512 = 0
                                | otherwise    = lX
                         lY'   | abs lY > 288 = 0
                                | otherwise    = lY
                         mLoc   = let (x,y) = (mapLoc m)in (x+shiftX, y+shiftY)
                         shiftX | lX >   512  =   1
                                | lX < (-512) = (-1)
                                | otherwise   =   0
                         shiftY | lY >   288  =   1
                                | lY < (-288) = (-1)
                                | otherwise   =   0
                plFac m = case mapType m of
                          0 -> 1
                          1 -> 0.75
                          2 -> 0.8
                          3 -> 0.5
                          4 -> 0.25

                --background creation
                pMap           = concatMap getParticles backgrounds
                getParticles m =    let (x,y) = mapLoc m
                                        (x''',y''') = mulSV (-1) (localLoc m)
                                        translateSecP  (w, h) = \p -> p {pObj = (pObj p){pos = addV (pos (pObj p)) (x'''+ fromIntegral (w-x)*512,y'''+ fromIntegral (h-y)*288)}}
                                     in concatMap (\(x'',y'') -> map (translateSecP (x'', y'')) (localPMap (x'', y''))) [(fromIntegral (x+x') :: Int, fromIntegral (y+y') :: Int)|x' <- [(-1),0,1], y' <- [(-1),0,1]]
                                     
                localPMap  (x',y)  = [Particle (Object ((f x' x),(g y x)) (0, 0)) 20 2 | x <- [0, 5..30]]
                                     where f a b = fromIntegral $ rem (a*(-3)^b + b) 512
                                           g a b = fromIntegral $ rem (a*(-3)^b + b) 288

                -- weapon
                weapCd   = case weapon of
                           0 -> 5
                           1 -> 2
                           2 -> 10
                weapSize = case weapon of
                           0 -> 2
                           1 -> 1
                           2 -> 25
                weap'    = if crash then 0 else if got then iType upgrade else weapon
                
                -- global movement
                gMove   = \o -> o {pos = addV (pos o) (mulSV (-1) vel') }
                up      = \o -> o {pos = addV (pos o) (vel o), vel = mulSV decay (vel o)} 
                
                -- enemy updater
                es      = if crash then [] else newE ++ map eGT es'
                newE    = if length es' < 10 then [Enemy (Object (addV ran1 (rotateV (argV ran1) (100,0))) (0,0)) 1 0] else []
                (hitEs, es')  = partition (\e -> any (col (eObj e) (25+weapSize) . bObj) bullets) enemies
                -- movement
                eM   o  = o {pos = addV (mulSV 0.997 (pos o)) (mulSV
 (-0.2/magV (pos o)) (pos o))}
                eMove o = o {pos = addV (pos o) (mulSV (-1) vel') }
                eGT  e  = e{eObj = (eMove.eM) (eObj e)}




                -- bullet updater
                bs         = if crash then [] else newB ++ bsMod bullets
                bsMod bs   = filter (\b -> (bLifeTime b) > 0) $ map ((\b -> b{bObj = up (bObj b)}).dyingB) bs
                dyingB  b  = b{bLifeTime = bLifeTime b - time}
                newB       = case (shootAction, cooldown) of
                               (Shoot, 0) -> bullet
                               (_, _) -> []
                bullet     = case weapon of
                                0 -> [bCreator 15 angle 0.5]
                                1 -> [bCreator 15 angle 0.5]
                                2 -> [bCreator 15 (angle + theta) 0.5, bCreator 30 angle 0.5, bCreator 30 (angle - theta) 0.5]
                bCreator sp a = Bullet (Object (0,0) (mulSV sp (cos a, sin a)))
               
                -- particle update
                ps      = if crash then deadPs else newPs ++ map psMod (filter pCheck particles)
                psMod   = (\p -> p{pObj = (gMove . up) (pObj p)}).dyingP
                pCheck p = pLifeTime p > 0 {-&& magV (pos (pObj p)) > 25-}
                newPs   = concatMap (explosion 1 . pos . eObj) hitEs
                deadPs  = explosion 0 (0,0)
                dyingP p = p{pLifeTime = pLifeTime p - time}
                explosion y v = [Particle (Object v (10.0 * cos x, 10.0 * sin x)) 0.5 y| x <- [0, (pi/8)..(2*pi)]]
                
                -- trails
                trail   = Particle (Object (0,0) $ mulSV (-1) vel') 0.5 3
                ps' = case movementAction of
                        Thrust      -> trail:ps
                        NoMovement  -> ps

                -- score updater
                newScore = if crash then 0 else score + multiplier * sum (map eScore hitEs)
                newMult | crash = 10
                        | any (col (Object (0,0) (0,0)) 27 . iObj) (filter (\i -> iType i == 0)bonus) = multiplier + 20 
                        | otherwise = multiplier

                -- item updater
                -- bonus multiplier
                bonus'  = if crash then [] else newI ++ (filter iCheck  (map ((\i -> i{iObj = gMove (iObj i)}).dyingI) bonus))
                dyingI  i = i {iLifeTime = iLifeTime i - time}
                iCheck  i = iLifeTime i > 0 && not (col (iObj i) 25 player)
                newI    = [Item (Object ran2 (0,0)) 10 0 | length bonus < 3]
                -- weapon upgrade
                got      = col player 30 (iObj upgrade)
                upgrade' = upG'{iObj = gMove (iObj upG')}
                upG'     = if got then Item (Object ran2 (0,0)) 20 (mod (iType upgrade) 2 + 1) else upgrade

 
                --collision
                col o1 rad o2 = magV (pos o1 - pos o2) < rad 
                crash         = any (col player 25 . eObj) enemies

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
