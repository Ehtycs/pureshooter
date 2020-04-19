
module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array (filter, (:))
import Data.Foldable (sequence_)
import Data.Time.Duration (Milliseconds)
import Data.Time (Time, diff)

import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Now (nowTime)
import Effect.Ref as Ref

import Math as Math

-- HTML and document functions, this is another level than the DOM
import Web.HTML (window)
import Web.HTML.Window (Window, requestAnimationFrame)

import Global.Unsafe (unsafeStringify)

import Graphics.Canvas as Canvas

import Partial.Unsafe (unsafePartial)

import Controls as Controls


type GameState = {player::Player, bullets::Array Bullet}

type Player = {x::Number, y::Number, dx::Number, dy::Number}
type Bullet = {x::Number, y::Number,
               dx::Number, dy::Number,
               life::Int, velocity::Number}

gameState :: Ref.Ref GameState
gameState = unsafePerformEffect $ do
  let player = {x:0.0, y:0.0, dx:0.0, dy: 0.0}
      bullets = [] :: Array Bullet
  Ref.new {player, bullets}

direction
  :: Number -> Number
  -> Number -> Number
  -> (Tuple Number Number)
direction x0 y0 x1 y1 =
  Tuple ((x1-x0)/magn) ((y1-y0)/magn)
  where
    magn = Math.sqrt $ (Math.pow (x1-x0) 2.0) + (Math.pow (y1-y0) 2.0)
      

firing
  :: Controls.ControlState
  -> Player
  -> Array Bullet
  -> Array Bullet
firing k p bs
  | k.mouse1 =
    let (Tuple dx dy) = direction p.x p.y k.mouseX k.mouseY
    in {x: p.x, y: p.y, dx: dx, dy: dy, life: 20, velocity: 50.0} : bs

  | otherwise = bs

updateState :: Controls.ControlState -> GameState -> GameState
updateState k g =
  let nbullets = firing k g.player g.bullets
  in
   {player: evolvePlayer k g.player,
    bullets: map evolveBullet $ filter removeBullet nbullets}

removeBullet :: Bullet -> Boolean
removeBullet b = b.life > 0

evolvePlayer :: Controls.ControlState -> Player -> Player
evolvePlayer k p = 
  let dxr = if k.right then 10.0 else 0.0 
      dxl = if k.left then 10.0 else 0.0
      dyu = if k.up then 10.0 else 0.0
      dyd = if k.down then 10.0 else 0.0
  in
   {x: p.x + dxr - dxl,
    y: p.y - dyu + dyd,
    dx: p.dx,
    dy: p.dy}

evolveBullet :: Bullet -> Bullet
evolveBullet b = b {x=b.x+b.velocity*b.dx,
                    y=b.y+b.velocity*b.dy,
                    velocity=b.velocity*0.9,
                    life=b.life-1}

renderBullet :: Canvas.Context2D -> Bullet -> Effect Unit
renderBullet ctx b = do
  log $ "Bullet!"
  Canvas.strokeRect ctx {height:5.0,
                         width:5.0,
                         x: b.x,
                         y: b.y}

renderGame
  :: RenderingCtx
  -> Controls.ControlState
  -> GameState
  -> Effect Unit
renderGame r c s = unsafePartial $ do
  let p = s.player
  Canvas.strokeRect r.context {height:10.0,
                               width:10.0,
                               y: p.y,
                               x: p.x}

  sequence_ $ map (renderBullet r.context) s.bullets
  
  -- Draw crosshair
  Canvas.strokeRect r.context {height:5.0,
                               width:5.0,
                               y: c.mouseY,
                               x: c.mouseX}

type RenderingCtx = {element::Canvas.CanvasElement,
                     context::Canvas.Context2D}

getRenderingCtx :: Effect RenderingCtx
getRenderingCtx = unsafePartial $ do
  (Just cel) <- Canvas.getCanvasElementById "canvas"
  ctx <- Canvas.getContext2D cel
  pure {element: cel, context: ctx}

clearContext :: RenderingCtx -> Effect Unit
clearContext rend = do
  dim <- Canvas.getCanvasDimensions rend.element
  Canvas.clearRect rend.context {x:0.0, y:0.0,
                                 height: dim.height,
                                 width: dim.width}

gameLoop :: Window -> Time -> Effect Unit
gameLoop win t = do
  nowt <- nowTime 
  let dt = diff nowt t :: Milliseconds
  --log $ "Tick" <> show dt
  rend <- getRenderingCtx
  controls <- Controls.getControlState
  gstate <- Ref.read gameState
  --log $ unsafeStringify controls
  let newgstate = updateState controls gstate
  --log $ unsafeStringify newgstate
  clearContext rend
  renderGame rend controls newgstate
  Ref.write newgstate gameState
  _ <- requestAnimationFrame (gameLoop win nowt) win
  pure unit

main :: Effect Unit
main = unsafePartial $ do
  Controls.initialize
  win <- window
  nowt <- nowTime
  _  <- requestAnimationFrame (gameLoop win nowt) win
  pure unit
  
