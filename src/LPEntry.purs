module LPEntry
  ( State
  , Query(..)
  , lpEntry
  ) where

import Prelude
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Number as Number
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Matrix (makeMatrix, prettyPrint)

type State =
  { costs :: Array String
  , coefficients :: Array (Array String)
  , bounds :: Array String
  }

updateEntry :: Int -> Int -> String -> Array (Array String) -> Array (Array String)
updateEntry i j newVal =
  modifyAt_ i (updateAt_ j newVal)

modifyAt_ :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyAt_ idx f arr = fromMaybe arr (Array.modifyAt idx f arr)

updateAt_ :: forall a. Int -> a -> Array a -> Array a
updateAt_ idx x arr = fromMaybe arr (Array.updateAt idx x arr)

getEntry1 :: Array String -> Int -> String
getEntry1 arr i = fromMaybe "0" (Array.index arr i)

getEntry2 :: Array (Array String) -> Int -> Int -> String
getEntry2 arr i j = fromMaybe "0" (Array.index arr i >>= flip Array.index j)

data Query a
  = UpdateCoefficient Int Int String a
  | UpdateCost Int String a
  | UpdateBound Int String a
  | AddConstraint a
  | RemoveConstraint a
  | AddVariable a
  | RemoveVariable a

lpEntry :: forall m. H.Component HH.HTML Query Unit Void m
lpEntry =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: State
initialState =
  { costs: ["1", "3", "2"]
  , coefficients: [ ["1", "3", "0"], ["1", "2", "1"] ]
  , bounds: ["2", "4"]
  }

minDimension :: Int
minDimension = 1

maxDimension :: Int
maxDimension = 4

numConstraints :: State -> Int
numConstraints = Array.length <<< _.bounds

numVariables :: State -> Int
numVariables = Array.length <<< _.costs

mayAddConstraint :: State -> Boolean
mayAddConstraint s = numConstraints s < maxDimension

mayRemoveConstraint :: State -> Boolean
mayRemoveConstraint s = numConstraints s > minDimension

mayAddVariable :: State -> Boolean
mayAddVariable s = numVariables s < maxDimension

mayRemoveVariable :: State -> Boolean
mayRemoveVariable s = numVariables s > minDimension

parseEntries :: Array (Array String) -> Maybe (Array (Array Number))
parseEntries = traverse (traverse Number.fromString)

render :: State -> H.ComponentHTML Query
render state =
  HH.div_ [ mainDiv, buttons, debug ]
  where
  mainDiv = HH.div [ HP.class_ (H.ClassName "lp-input") ]
                   [ objFun, subjectTo, constraints ]

  objFun = rowDiv ([ fixedText' "maximise f =" ] <> costs)

  costs =
    coefficientsRow (getEntry1 state.costs) UpdateCost

  numberInput val change =
    HH.input
      [ HP.class_ (H.ClassName "number")
      , HP.value val
      , HE.onValueChange (HE.input change)
      ]

  subjectTo = rowDiv [ fixedText' "subject to:" ]

  constraints =
    let
      makeRow i =
        rowDiv ( coefficientsRow (getEntry2 state.coefficients i) (UpdateCoefficient i) <>
                 [ fixedText' "≤"
                 , HH.div_ [numberInput (getEntry1 state.bounds i) (UpdateBound i)]
                 ]
               )
    in
      HH.div_ (map makeRow (Array.range 0 (numConstraints state - 1)))

  rowDiv = HH.div [ HP.class_ (H.ClassName "lp-input--row") ]

  coefficientsRow currentValue onChange =
    let
      go i = HH.div_
               [ numberInput (currentValue i) (onChange i)
               , HH.span [ HP.class_ (H.ClassName "fixed-text") ]
                   [ HH.text "x" , HH.sub_ [ HH.text (show (i + 1)) ] ]
               ]
    in
     intersperse (fixedText' "+") $
        map go (Array.range 0 (numVariables state - 1))

  fixedText = HH.div [ HP.class_ (H.ClassName "fixed-text")]

  fixedText' msg = fixedText [ HH.text msg ]

  buttons = HH.div_
    let
      button label action enabled =
        HH.button
          [ HP.title label
          , HP.enabled enabled
          , HE.onClick (HE.input_ action)
          ]
          [ HH.text label ]
    in
      [ HH.div_ [ button "Add constraint" AddConstraint (mayAddConstraint state)
                , button "Remove constraint" RemoveConstraint (mayRemoveConstraint state)
                ]
      , HH.div_ [ button "Add variable" AddVariable (mayAddVariable state)
                , button "Remove variable" RemoveVariable (mayRemoveVariable state)
                ]
      ]

  debug = HH.div_ [ HH.pre_ [ HH.text debugMsg ] ]
  debugMsg = dimensionInfo <> "\n\n" <> matrixInfo <>
             "\n\ncosts = " <> show state.costs <>
             "\n\nbounds = " <> show state.bounds
    
  dimensionInfo =
    "Problem has " <> show (numVariables state) <> " variables and "
    <> show (numConstraints state) <> " constraints."

  matrixInfo =
    fromMaybe "(no matrix)" $
      parseEntries state.coefficients >>= makeMatrix prettyPrint

intersperse :: forall a. a -> Array a -> Array a
intersperse x = intercalate [x] <<< map pure

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  UpdateCoefficient i j val next ->
    modify next (over coeffs (updateEntry i j val))
  UpdateCost i val next ->
    modify next (over costs (updateAt_ i val))
  UpdateBound i val next ->
    modify next (over bounds (updateAt_ i val))
  AddConstraint next ->
    modifyGuarding mayAddConstraint next $
      \s -> s { coefficients = Array.snoc s.coefficients (Array.replicate (numVariables s) "0")
              , bounds = Array.snoc s.bounds "0"
              }
  RemoveConstraint next ->
    modifyGuarding mayRemoveConstraint next $
      \s -> s { coefficients = dropEnd 1 s.coefficients
              , bounds = dropEnd 1 s.bounds
              }
  AddVariable next ->
    modifyGuarding mayAddVariable next $
      \s -> s { coefficients = map (flip Array.snoc "0") s.coefficients
              , costs = Array.snoc s.costs "0"
              }
  RemoveVariable next ->
    modifyGuarding mayRemoveVariable next
      \s -> s { coefficients = map (dropEnd 1) s.coefficients
              , costs = dropEnd 1 s.costs
              }
  where
  modifyGuarding check next f = do
    state <- H.get
    when (check state) do
      let nextState = f state
      H.put nextState
    pure next

  modify = modifyGuarding (const true)

  coeffs = prop (SProxy :: SProxy "coefficients")
  costs = prop (SProxy :: SProxy "costs")
  bounds = prop (SProxy :: SProxy "bounds")

dropEnd :: forall a. Int -> Array a -> Array a
dropEnd i = Array.reverse <<< Array.drop i <<< Array.reverse
