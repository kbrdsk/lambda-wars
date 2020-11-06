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
        player           :: Player,
        enemies          :: [Enemy],
        bullets          :: [Bullet],
        pSystems         :: [ParticleSystem],
        items            :: [Item]
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

data Player = Player {
  score :: Int,
  angle :: Float,
  speed :: Float
}

data Enemy = Enemy {
  eObj    :: Object,
  eScore  :: Int,
  eType   :: Int
}

data Particle = Particle {
  pObj    :: Object,
  pLifeTime :: Float
}

data ParticleSystem = ParticleSystem {
  particles :: [Particle]
}

data Bullet = Bullet {
  bObj    :: Object,
  bLifeTime  :: Float
}

data Item = Item {
  iObj    :: Object,
  iLifeTime  :: Float
}

data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

zv = (0,0)
e1 = Enemy (Object zv zv) 0 0
ps = [Particle (Object zv (5.0 * (cos x), 5.0 * (sin x))) 1| x <- [0..100]]

initial :: Int -> [Vector] -> World
initial seed r = World (mkStdGen seed) NoRotation NoMovement DontShoot (Player  0 0 0) enem [] [ParticleSystem ps] []
                where
                  enem = map (\(x,y) -> Enemy (Object (x,y) zv) 0 0) r