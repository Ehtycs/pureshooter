module Controls
       ( ControlState
       , getControlState
       , initialize ) where
 
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)

import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

-- Event data 
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Event as WebEvent

-- HTML and document functions, this is another level than the DOM
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument as HTMLDocument 
import Web.HTML.HTMLElement as WebHtmlElement

import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mousemove, mouseup, mousedown)
import Web.UIEvent.KeyboardEvent.EventTypes (keyup, keydown)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

import Global.Unsafe (unsafeStringify)

-- DOM functions
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)

import Effect.Ref as Ref

import Partial.Unsafe (unsafePartial)

getMouseCoordinates
  :: WebHtmlElement.HTMLElement
  -> MouseEvent.MouseEvent
  -> Effect (Tuple Number Number)
getMouseCoordinates canvas clickEvt = do
  boundingRect <- WebHtmlElement.getBoundingClientRect canvas
  let top = boundingRect.top
      left = boundingRect.left
      mouseX = toNumber $ MouseEvent.clientX clickEvt
      mouseY = toNumber $ MouseEvent.clientY clickEvt
  pure $ Tuple (mouseX-left) (mouseY-top)

myMoveEventListener
  :: WebHtmlElement.HTMLElement
  -> WebEvent.Event
  -> Effect Unit
myMoveEventListener canv evt = unsafePartial $ do
  let (Just event) = MouseEvent.fromEvent evt
  (Tuple x y) <- getMouseCoordinates canv event
  Ref.write {x, y} mousePosition

myMouseDownEventListener :: WebEvent.Event -> Effect Unit
myMouseDownEventListener evt = unsafePartial $ do
  let (Just mevent) = MouseEvent.fromEvent evt
      key = MouseEvent.button mevent
  updateMouseKeyState key true

myMouseUpEventListener :: WebEvent.Event -> Effect Unit
myMouseUpEventListener evt = unsafePartial $ do
  let (Just mevent) = MouseEvent.fromEvent evt
      key = MouseEvent.button mevent
  updateMouseKeyState key false

updateMouseKeyState :: Int -> Boolean -> Effect Unit
updateMouseKeyState key state = do
  Ref.write {mouse1: state} mouseKeyState

myKeyDownEventListener :: WebEvent.Event -> Effect Unit
myKeyDownEventListener evt = unsafePartial $ do
  let (Just keyEvent) = KeyboardEvent.fromEvent evt
      key = KeyboardEvent.key keyEvent
  updateKeyState key true

myKeyUpEventListener :: WebEvent.Event -> Effect Unit
myKeyUpEventListener evt = unsafePartial $ do
  let (Just keyEvent) = KeyboardEvent.fromEvent evt
      key = KeyboardEvent.key keyEvent
  updateKeyState key false

printKeyState :: Effect Unit
printKeyState = do
  s <- Ref.read keyState
  log $ unsafeStringify s 

type KeyState =  {up::Boolean, down::Boolean,
                 left::Boolean, right::Boolean}
                 
type MousePosition = {x::Number, y::Number}

type MouseKeys = {mouse1::Boolean}


keyState :: Ref.Ref KeyState
keyState =
  unsafePerformEffect $ Ref.new {up:false,
                                 down:false,
                                 left:false,
                                 right:false}

mousePosition :: Ref.Ref MousePosition
mousePosition = unsafePerformEffect $ Ref.new {x:0.0, y:0.0}

mouseKeyState :: Ref.Ref MouseKeys
mouseKeyState = unsafePerformEffect $ Ref.new {mouse1: false}

type ControlState = {mouseX::Number,
                     mouseY::Number,
                     mouse1::Boolean,
                     up::Boolean,
                     down::Boolean,
                     left::Boolean,
                     right::Boolean}

getControlState :: Effect ControlState
getControlState = do
  
  keys <- Ref.read keyState
  mpos <- Ref.read mousePosition
  mkeys <- Ref.read mouseKeyState
  
  pure $ {mouseX: mpos.x,
          mouseY: mpos.y,
          mouse1: mkeys.mouse1,
          up: keys.up,
          down: keys.down,
          left: keys.left,
          right: keys.right}

updateKeyState :: String -> Boolean -> Effect Unit
updateKeyState key state = do
  kstate <- Ref.read keyState
  case key of
    "w" -> Ref.write (kstate {up=state}) keyState
    "s" -> Ref.write (kstate {down=state}) keyState
    "a" -> Ref.write (kstate {left=state}) keyState
    "d" -> Ref.write (kstate {right=state}) keyState
    _   -> pure unit


initialize :: Effect Unit
initialize = unsafePartial $ do
  win <- window
  doc <- document win
  let doc' = toNonElementParentNode $ HTMLDocument.toDocument doc
  canv <- getElementById "canvas" doc'
  let (Just canv') = canv 
  let (Just canvElement) = WebHtmlElement.fromElement canv'

  -- Mouse move events - mouse position update
  evtlst <- eventListener (myMoveEventListener canvElement)
  addEventListener mousemove evtlst false (toEventTarget $ canv')

  -- Mouse click up events 
  mUpLst <- eventListener myMouseUpEventListener
  addEventListener mouseup mUpLst false (toEventTarget $ canv')

  -- Mouse click down events
  mDownLst <- eventListener myMouseDownEventListener
  addEventListener mousedown mDownLst false (toEventTarget $ canv')

  -- keyboard key up events
  keyUpLst <- eventListener myKeyUpEventListener
  addEventListener keyup keyUpLst false (HTMLDocument.toEventTarget doc)

  -- keyboard key down events
  keyDownLst <- eventListener myKeyDownEventListener
  addEventListener keydown keyDownLst false (HTMLDocument.toEventTarget doc)


