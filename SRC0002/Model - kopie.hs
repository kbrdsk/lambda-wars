{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Vector

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        -- player           :: Player,
        enemies          :: [Enemy],
        bullets          :: [Bullet],
        particles        :: [Particle],
        bgParticles      :: [Particle],
        --pSystems         :: [ParticleSystem],
        bonus            :: [Item],
        upgrade          :: Item,
        score            :: Int,
        multiplier       :: Int,
        velocity         :: Vector,
        angle            :: Float,
        cooldown         :: Int,
        weapon           :: Int,
        backgrounds      :: [Map],
        randomList       :: [(Float, Float)]
            } 
{-
data ParticleSystem

-- CLASS DEFINITIONS

class Obj a where
  position :: a -> Vector
  velocity :: a -> Vector
-}



-- DATA TYPES

data Object = Object {
  pos :: Vector,
  vel :: Vector
}
{-
data Player = Player {
  score :: Int,
  angle :: Float,
  speed :: Float
}
-}
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

{-data ParticleSystem = ParticleSystem {
   pType'     :: Int,
   particles :: [Particle]
}-}

data Bullet = Bullet {
  bObj       :: Object,
  bLifeTime  :: Float
}

data Item = Item {
  iObj       :: Object,
  iLifeTime  :: Float,
  iType      :: Int
}

data Map = Map {
  mapLoc           :: (Int, Int),
  localLoc         :: (Float, Float),
  mapType          :: Int
}

data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

zv = (0,0)
e1 = Enemy (Object (100,0) zv) 1 0
ps = [Particle (Object (100,100) (5.0 * (cos x), 5.0 * (sin x))) 10| x <- [0..31]]
i1 = Item (Object (-100,0) zv) 10 1
m1 = Map (10,10) (0,0) 0
m2 = Map (-5,6) (0,0) 1
m3 = Map (-8,1) (0,0) 2
m4 = Map (2,9) (0,0) 3
m5 = Map (-7,-7) (0,0) 4

initial :: Int -> [Vector] -> World
initial seed r = World (mkStdGen seed) NoRotation NoMovement DontShoot [e1] [] [] [] [] i1 0 10 0 0 0 0 [m1, m2, m3, m4, m5] r
                

{-where
                  enem = map (\(x,y) -> Enemy (Object (x,y) zv) 0 0) r-}
