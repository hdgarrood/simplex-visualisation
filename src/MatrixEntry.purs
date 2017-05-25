module MatrixEntry
  ( State
  , Query(..)
  , matrixEntry
  ) where

import Prelude
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Number as Number
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Matrix (makeMatrix, prettyPrint)

type State = Array (Array String)

updateEntry :: Int -> Int -> String -> Array (Array String) -> Array (Array String)
updateEntry i j newVal =
  modifyAt_ j (updateAt_ i newVal)
  where
  modifyAt_ idx f arr = fromMaybe arr (Array.modifyAt idx f arr)
  updateAt_ idx x arr = fromMaybe arr (Array.updateAt idx x arr)

data Query a
  = Update Int Int String a
  | AddRow a
  | RemoveRow a
  -- | AddColumn a

matrixEntry :: forall m. H.Component HH.HTML Query Unit Void m
matrixEntry =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: State
initialState =
  [ ["1", "2"], ["0", "3"], ["1", "1"] ]

minDimension :: Int
minDimension = 1

maxDimension :: Int
maxDimension = 6

numRows :: State -> Int
numRows = fromMaybe 0 <<< map Array.length <<< flip Array.index 0

numCols :: State -> Int
numCols = Array.length

mayAddRow :: State -> Boolean
mayAddRow s = numRows s < maxDimension

mayRemoveRow :: State -> Boolean
mayRemoveRow s = numRows s > minDimension

mayAddColumn :: State -> Boolean
mayAddColumn s = numCols s < maxDimension

mayRemoveColumn :: State -> Boolean
mayRemoveColumn s = numCols s > minDimension

parseEntries :: Array (Array String) -> Maybe (Array (Array Number))
parseEntries = traverse (traverse Number.fromString)

render :: State -> H.ComponentHTML Query
render state =
  HH.div []
    [ HH.div [ HP.class_ (H.ClassName "matrix") ]
        [ HH.div [ HP.class_ (H.ClassName "matrix-entries") ]
                 (map renderRow (Array.range 0 (numRows state - 1)))
        , HH.div [ HP.class_ (H.ClassName "modify-cols") ]
                 []
        ]
    , HH.div [ HP.class_ (H.ClassName "modify-rows") ]
             [ addRowButton, removeRowButton ] 
    ]

  where
  renderRow i =
    HH.div [ HP.class_ (H.ClassName "matrix-entry-row") ]
           (map (renderInput i) (Array.range 0 (numCols state - 1)))

  renderInput i j =
    HH.input
      [ HP.class_ (H.ClassName "matrix-entry")
      , HP.value (fromMaybe "" (Array.index state j >>= flip Array.index i))
      , HE.onValueChange (HE.input (Update i j))
      ]

  modifyDimsButton isAllowed query buttonText =
    HH.button
      [ HP.enabled (isAllowed state)
      , HE.onClick (HE.input_ query)
      ]
      [ HH.text buttonText ]

  addRowButton = modifyDimsButton mayAddRow AddRow "▼"
  removeRowButton = modifyDimsButton mayRemoveRow RemoveRow "▲"

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  Update i j val next ->
    modify (updateEntry i j val) next
  AddRow next ->
    modify (\s -> if mayAddRow s
              then map (flip Array.snoc "0") s
              else s) next
  RemoveRow next ->
    modify (\s -> if mayRemoveRow s
              then map (Array.reverse >>> Array.drop 1 >>> Array.reverse) s
              else s) next

  where
  modify f next = do
    state <- H.get
    let nextState = f state
    H.put nextState
    pure next
