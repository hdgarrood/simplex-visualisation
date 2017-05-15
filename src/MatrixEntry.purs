module MatrixEntry
  ( State
  , Query(..)
  , matrixEntry
  ) where

import Prelude
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Int as Int
import Data.Number as Number
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Matrix (makeMatrix, prettyPrint, gaussEliminate)

type State =
  { entries :: Array (Array String)
  , rows :: Int
  , cols :: Int
  }

updateEntry :: Int -> Int -> String -> Array (Array String) -> Array (Array String)
updateEntry i j newVal =
  modifyAt_ j (updateAt_ i newVal)
  where
  modifyAt_ idx f arr = fromMaybe arr (Array.modifyAt idx f arr)
  updateAt_ idx x arr = fromMaybe arr (Array.updateAt idx x arr)

data Query a
  = Update Int Int String a
  -- | AddRow a
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
  { entries: [ ["1", "2"], ["0", "3"], ["1", "1"] ]
  , rows: 2
  , cols: 3
  }

parseEntries :: Array (Array String) -> Maybe (Array (Array Number))
parseEntries = traverse (traverse Number.fromString)

render :: State -> H.ComponentHTML Query
render state =
  HH.div []
    [ HH.div [] (map renderRow (Array.range 0 (state.rows - 1)))
    , HH.pre_ [ HH.text (debugMsg state) ]
    ]

  where
  renderRow i =
    HH.div [] (map (renderInput i) (Array.range 0 (state.cols - 1)))

  renderInput i j =
    HH.input
      [ HP.value (fromMaybe "" (Array.index state.entries j >>= flip Array.index i))
      , HE.onValueChange (HE.input (Update i j))
      ]

  debugMsg state =
    fromMaybe "(no matrix)" $
      parseEntries state.entries >>= makeMatrix \m ->
        prettyPrint m <> "\n\nRow echelon form:\n\n" <> prettyPrint (gaussEliminate m)


eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  Update i j val next -> do
    state <- H.get
    let nextState = state { entries = updateEntry i j val state.entries }
    H.put nextState
    pure next
