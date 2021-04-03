module Cube where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Array (mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (cos, sin)
import Svg.Attributes as SA
import Svg.Elements as SE

-- Core Types
type Distance = Number

type Angle = Number

type Point2D =
  { x :: Distance
  , y :: Distance
  }

type Point3D =
  { x :: Distance
  , y :: Distance
  , z :: Distance
  }

type Edge = Tuple Int Int

type Shape =
  { vertices :: Array Point3D
  , edges :: Array Edge
  }

type Angle3D =
  { xa :: Angle
  , ya :: Angle
  , za :: Angle
  }

type AngVelocity3D = Angle3D -- velocity = angle/sec

type RotatingShape =
  { shape :: Shape
  , angVel :: AngVelocity3D
  , forward :: Boolean
  }

data Axis = X | Y | Z

-- Model / State
type State = RotatingShape

-- Values

viewBoxSize :: Number
viewBoxSize = 600.0

viewCenter :: Point2D
viewCenter =
  { x: viewBoxSize / 2.0
  , y: viewBoxSize / 2.0
  }

frameRate :: Number
frameRate = 200.0

oneDegInRad :: Angle
oneDegInRad = 0.01745329255

tenDegInRad :: Angle
tenDegInRad = oneDegInRad * 10.0

accelerateBy :: Number
accelerateBy = oneDegInRad * 50.0

dampenPercent :: Number
dampenPercent = 1.0 - (0.9 / frameRate) -- 10% per second

initCube :: State
initCube =
  { shape:
      { vertices:
          [ { x:  100.0, y:  100.0, z:  100.0 }
          , { x: -100.0, y:  100.0, z:  100.0 }
          , { x:  100.0, y: -100.0, z:  100.0 }
          , { x: -100.0, y: -100.0, z:  100.0 }
          , { x:  100.0, y:  100.0, z: -100.0 }
          , { x: -100.0, y:  100.0, z: -100.0 }
          , { x:  100.0, y: -100.0, z: -100.0 }
          , { x: -100.0, y: -100.0, z: -100.0 }
          ]
      , edges:
          [ Tuple 0 1
          , Tuple 0 2
          , Tuple 0 4
          , Tuple 1 5
          , Tuple 1 3
          , Tuple 2 3
          , Tuple 2 6
          , Tuple 4 5
          , Tuple 4 6
          , Tuple 3 7
          , Tuple 6 7
          , Tuple 5 7
          ]
      }
  , angVel:
      { xa: tenDegInRad
      , ya: tenDegInRad
      , za: tenDegInRad
      }
  , forward: true
  }

-- Events
data Query a
  = Tick a
  | IncAngVelocity Axis a
  | ReverseCube Axis a
  | IncVelocity Axis a
  | DecVelocity Axis a
  | ADDCUBE Axis a
  

-------------------- UPDATE / REDUCERS --------------------

cubes :: forall eff. H.Component HH.HTML Query Unit Unit (Aff (console :: CONSOLE | eff))
cubes =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = initCube
  
    render :: State -> H.ComponentHTML Query
    render = renderView

    eval :: Query ~> H.ComponentDSL State Query Unit (Aff (console :: CONSOLE | eff))
    eval = case _ of
      Tick next -> do
        cube <- H.get
        let angVel = cube.angVel
            {vertices, edges} = cube.shape
            newShape =
              { edges: edges
              , vertices: rotateShape vertices (anglePerFrame angVel)
              }
            newCube = cube
              { angVel = dampenAngVelocity angVel
              , shape = newShape
              }
        H.put newCube
        H.liftEff $ log "tick"
        pure next
      
      IncAngVelocity axis next -> do
        cube <- H.get
        let {xa, ya, za} = cube.angVel
        H.modify
          (\c ->
            case axis of
              X -> c { angVel { xa = xa + accelerateBy } }
              Y -> c { angVel { ya = ya + accelerateBy } }
              Z -> c { angVel { za = za + accelerateBy } }
          )
        pure next

      ReverseCube axis next -> do
        cube <- H.get
        let {xa, ya, za} = cube.angVel
        H.modify
          (\c ->
            case axis of
              X -> c { angVel { xa = xa + accelerateBy } }
              Y -> c { angVel { ya = ya + accelerateBy } }
              Z -> c { angVel { za = za + accelerateBy } }
          )
        pure next
      
      IncVelocity axis next -> do
        cube <- H.get
        let {xa, ya, za} = cube.angVel
        H.modify
          (\c ->
            case axis of
              X -> c { angVel { xa = xa + xa + xa + accelerateBy } }
              Y -> c { angVel { ya = ya + ya + ya + accelerateBy } }
              Z -> c { angVel { za = za + za + za + accelerateBy } }
          )
        pure next
      
      DecVelocity axis next -> do
        cube <- H.get
        let {xa, ya, za} = cube.angVel
        H.modify
          (\c ->
            case axis of
              X -> c { angVel { xa = xa - accelerateBy } }
              Y -> c { angVel { ya = ya - accelerateBy } }
              Z -> c { angVel { za = za - accelerateBy } }
          )
        pure next

      ADDCUBE axis next -> do
        cube <- H.get
        let {xa, ya, za} = cube.angVel
        H.modify
          (\c ->
            case axis of
              X -> c { angVel { xa = xa - accelerateBy } }
              Y -> c { angVel { ya = ya - accelerateBy } }
              Z -> c { angVel { za = za - accelerateBy } }
          )
        pure next  
                
rotateShape :: Array Point3D -> AngVelocity3D -> Array Point3D
rotateShape vertices ang =
  map (rotate ang) vertices

rotate :: AngVelocity3D -> Point3D -> Point3D
rotate { xa, ya, za } = rotateX xa >>> rotateY ya >>> rotateZ za
  where
    rotateX ang {x,y,z} = let Tuple ny nz = rotateInPlane y z ang in { x, y:ny, z:nz }
    rotateY ang {x,y,z} = let Tuple nx nz = rotateInPlane x z ang in { x:nx, y, z:nz }
    rotateZ ang {x,y,z} = let Tuple nx ny = rotateInPlane x y ang in { x:nx, y:ny, z }

    rotateInPlane :: Number -> Number -> Number -> Tuple Number Number
    rotateInPlane axis1 axis2 ang =
      Tuple (axis1 * cos(ang) - axis2 * sin(ang)) (axis2 * cos(ang) + axis1 * sin(ang))

anglePerFrame :: AngVelocity3D -> Angle3D
anglePerFrame {xa, ya, za} =
  { xa: xa / frameRate
  , ya: ya / frameRate
  , za: za / frameRate
  }

dampenAngVelocity :: AngVelocity3D -> AngVelocity3D
dampenAngVelocity {xa, ya, za} =
    { xa: dampen xa
    , ya: dampen ya
    , za: dampen za
    }
  where
    dampen :: Number -> Number
    dampen ang = ang * dampenPercent -- Basics.max 0 (ang-drpf)

-------------------- VIEW --------------------
renderView :: State -> H.ComponentHTML Query
renderView state = let
    {vertices, edges} = state.shape
    vert2Ds = map project vertices
  in
    HH.div [] $
      [ renderButton "rotX++" (IncAngVelocity X)
      , renderButton "rotY++" (IncAngVelocity Y)
      , renderButton "rotZ++" (IncAngVelocity Z)
      , renderButton "reverse" (ReverseCube Z)
      , renderButton "vel++" (IncVelocity Z)
      , renderButton "vel--" (DecVelocity Z)
      , renderButton  "add" (ADDCUBE X) 
      , renderButton "remove" (IncAngVelocity Z)
      ]
      <>
      [ SE.svg
        [ SA.viewBox 0.0 0.0 viewBoxSize viewBoxSize ]
        [ SE.g []
          (drawCube edges vert2Ds)
        ]
      ]
  where
    renderButton label query =
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ query)
        ]
        [ HH.text label ]

    -- parallel projection
    project :: Point3D -> Point2D
    project p =
      { x: p.x + viewCenter.x
      , y: p.y + viewCenter.y
      }

    drawCube :: Array Edge -> Array Point2D -> Array (H.ComponentHTML Query)
    drawCube edges vert2Ds =
      drawEdges edges vert2Ds <> drawVertices vert2Ds

    drawEdges :: Array Edge -> Array Point2D -> Array (H.ComponentHTML Query)
    drawEdges edges verts = let
        connectedVerts = map (\(Tuple v1 v2) -> Tuple (verts !! v1) (verts !! v2)) edges
      in
        map (\(Tuple v1 v2) -> drawLine (getPoint v1) (getPoint v2)) connectedVerts

    getPoint :: Maybe Point2D -> Point2D
    getPoint maybePoint = let
       default = { x: 100.0, y: 100.0 }
      in
        fromMaybe default maybePoint

    drawLine :: Point2D -> Point2D -> H.ComponentHTML Query
    drawLine a b =
      SE.path
        [ SA.d
          [ SA.Abs (SA.M a.x a.y)
          , SA.Abs (SA.L b.x b.y)
          ]
        , SA.stroke $ Just (SA.RGB 50 50 50)
        ]

    drawVertices :: Array Point2D -> Array (H.ComponentHTML Query)
    drawVertices vert2Ds =
      mapWithIndex drawVertex vert2Ds

    drawVertex :: Int -> Point2D -> H.ComponentHTML Query
    drawVertex idx {x, y} = SE.g []
      [ SE.text
          [ SA.x $ x + 5.0
          , SA.y $ y - 5.0
          , SA.fill $ Just (SA.RGB 150 150 150)
          ]
          [ HH.text $ show idx ]
      , SE.circle
          [ SA.r 3.0
          , SA.cx x
          , SA.cy y
          , SA.fill $ Just (SA.RGB 100 100 100)
          ]
      ]