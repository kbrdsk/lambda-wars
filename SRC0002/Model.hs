{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import System.IO
import Graphics.Gloss.Data.Vector

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        rotateAction'    :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        enemies          :: [Enemy],
        bullets          :: [Bullet],
        particles        :: [Particle],
        bgParticles      :: [Particle],
        bonus            :: [Item],
        upgrade          :: Item,
        score            :: Int,
        highScore        :: Int,
        multiplier       :: Int,
        velocity         :: Vector,
        angle            :: Float,
        cooldown         :: Int,
        weapon           :: Int,
        pUpDuration      :: Int,
        backgrounds      :: [Map],
        randomList       :: [(Float, Float)],
        highScoreList    :: [Int],
        state            :: Int,
        menuState        :: Int
            } 

-- DATA TYPES

data Object = Object {
  pos :: Vector,
  vel :: Vector
}

data Enemy = Enemy {
  eObj    :: Object,
  eScore  :: Int,
  eType   :: Int
}

data Particle = Particle {
  pObj      :: Object,
  pLifeTime :: Float,
  pType     :: Int
}

data Bullet = Bullet {
  bObj       :: Object,
  bLifeTime  :: Float
}

data Item = Item {
  iObj       :: Object,
  iLifeTime  :: Float,
  iType      :: Int,
  pickedUp   :: Bool
}

data Map = Map {
  mapLoc           :: (Int, Int),
  localLoc         :: (Float, Float),
  mapType          :: Int
}

--data Weapon deriving enum bounded

data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

zv = (0,0)
e1 = Enemy (Object (100,0) zv) 1 0
ps = [Particle (Object (100,100) (5.0 * (cos x), 5.0 * (sin x))) 10| x <- [0..31]]
i1 = Item (Object (-100,0) zv) 10 1 False
m1 = Map (10,10) (0,0) 0
m2 = Map (-5,6) (0,0) 1
m3 = Map (-8,1) (0,0) 2
m4 = Map (2,9) (0,0) 3
m5 = Map (-7,-7) (0,0) 4

initial :: Int -> [Vector] -> [Int]  -> World
initial seed r hsl = World (mkStdGen seed) NoRotation NoRotation NoMovement DontShoot [e1] [] [] [] [] i1 0 0 10 0 0 0 0 0 [m1, m2, m3, m4, m5] r hsl 0 0
                
{-where
                  enem = map (\(x,y) -> Enemy (Object (x,y) zv) 0 0) r-}
