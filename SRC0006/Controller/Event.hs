module Controller.Event (
    eventHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Model

-- | Event handling

eventHandler :: Event -> World -> IO World
eventHandler (EventKey (SpecialKey KeyLeft ) Down _ _) world
    = case rotateAction' world of
        NoRotation -> return world { rotateAction = RotateLeft, rotateAction' = RotateLeft} 
        _          -> return world { rotateAction = RotateLeft }
eventHandler (EventKey (SpecialKey KeyLeft ) Up   _ _) world
    = case rotateAction' world of
        RotateLeft -> return world { rotateAction = NoRotation, rotateAction' = NoRotation} 
        _            -> return world { rotateAction = rotateAction' world }
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) world
    = case rotateAction' world of
        NoRotation -> return world { rotateAction = RotateRight, rotateAction' = RotateRight} 
        _          -> return world { rotateAction = RotateRight }
eventHandler (EventKey (SpecialKey KeyRight) Up   _ _) world
    = case rotateAction' world of
        RotateRight -> return world { rotateAction = NoRotation, rotateAction' = NoRotation} 
        _             -> return world { rotateAction = rotateAction' world}
eventHandler (EventKey (SpecialKey KeyUp   ) Down _ _) world
    = return world { movementAction = Thrust }
eventHandler (EventKey (SpecialKey KeyUp   ) Up   _ _) world
    = return world { movementAction = NoMovement }
eventHandler (EventKey (SpecialKey KeySpace) Down _ _) world
    = return world { shootAction = Shoot }
eventHandler (EventKey (SpecialKey KeySpace) Up   _ _) world
    = return world { shootAction = DontShoot }
eventHandler _ world
    = return world
