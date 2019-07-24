module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Data.Array (length, modifyAt, snoc, zip, (..))
import Data.Array (deleteAt)
import Data.DateTime.Instant (instant)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe, fromMaybe')
import Data.Number (fromString)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafeCrashWith)


type Name = String
type Money = Number

data Transaction = Transaction
    { source :: Name
    , targets :: S.Set Name
    , amount :: String
    , note :: String
    }

data TableRowEvent
    = SetSource Name
    | ToggleTarget Name
    | SetAmount String
    | SetNote String
    | DeleteRow

data TableEvent
    = RowEvent Int TableRowEvent
    | AddRowEvent

derive instance genericTableRowEvent :: Generic TableRowEvent _
instance showTableRowEvent :: Show TableRowEvent where
  show = genericShow

derive instance genericTableEvent :: Generic TableEvent _
instance showTableEvent :: Show TableEvent where
  show = genericShow



hello :: ∀ a. Widget HTML a
hello = do
  void $ D.button [P.onClick] [D.text "Say Hello"]
  D.text "Hello Sailor!"


table :: Array Name -> Array Transaction -> Widget HTML TableEvent
table names transactions = do
    D.table []
        $ [ tableHeader  names ]
        <> map (\(Tuple i x) -> RowEvent i <$> tableRow names x) (zip (0 .. length transactions) transactions)
        <> [ D.tr [] [ D.td [] [ AddRowEvent <$ D.button [P.onClick] [ D.text "+" ] ] ] ]


tableHeader :: ∀ a. Array Name -> Widget HTML a
tableHeader names = firstRow <|> secondRow where
    firstRow = D.tr []
        [ D.th [] []
        , D.th [ P.colSpan (length names) ] [ D.text "Who paid" ]
        , D.th [] [ D.text "how much" ]
        , D.th [ P.colSpan (length names) ] [ D.text "for whom" ]
        , D.th [] [ D.text "Note" ]
        ]
    secondRow = D.tr []
        $ [ D.th [] [] ]
        <> map (\n -> D.td [] [D.text n]) names
        <> [ D.th [] [] ]
        <> map (\n -> D.td [] [D.text n]) names
        <> [ D.td [] [] ]


tableRow :: Array Name -> Transaction -> Widget HTML TableRowEvent
tableRow names (Transaction transaction) = D.tr []
    $ [ D.td [] [ DeleteRow <$ D.button [P.onClick] [ D.text "−" ] ] ]
    <> map showRadio names
    <> [ D.td [] [ D.input [ P._type "text", P.value transaction.amount, (SetAmount <<< unsafeTargetValue) <$> P.onChange ] ] ]
    <> map showCheckbox names
    <> [ D.td [] [ D.input [ P._type "text", P.value transaction.note, (SetNote <<< unsafeTargetValue) <$> P.onChange ] ] ]
     where
    showRadio name =
        SetSource name <$ D.td []
            [ D.input [ P._type "radio", P.onChange, P.checked (name == transaction.source) ] ]
    showCheckbox name =
        ToggleTarget name <$ D.td []
            [ D.input [ P._type "checkbox", P.onChange, P.checked (S.member name transaction.targets)  ] ]


initialState :: Array Transaction
initialState = [ defaultTransaction ]

defaultTransaction :: Transaction
defaultTransaction = Transaction
    { source: "Pavel"
    , targets: S.fromFoldable ["Káťa", "Péťa"]
    , amount: "200.0"
    , note: "Testing transaction"
    }


main :: Effect Unit
main = runWidgetInDom "root" $ go initialState where
    go :: ∀ a. Array Transaction -> Widget HTML a
    go state = do
        event <- table ["Pavel", "Péťa", "Káťa"] state
        liftEffect $ log (show event)
        case event of
            RowEvent i (SetSource name)    -> go $ unsafeModifyAt i (\(Transaction t) -> Transaction (t { source=name })) state
            RowEvent i (ToggleTarget name) -> go $ unsafeModifyAt i (\(Transaction t) -> Transaction (t { targets=toggleSetItem name t.targets })) state
            RowEvent i (SetAmount s)       -> go $ unsafeModifyAt i (\(Transaction t) -> Transaction (t { amount=s })) state
            RowEvent i (SetNote s)         -> go $ unsafeModifyAt i (\(Transaction t) -> Transaction (t { note=s })) state
            RowEvent i DeleteRow           -> go $ unsafeDeleteAt i state
            AddRowEvent -> go (snoc state defaultTransaction)

    toggleSetItem :: ∀ a. Ord a => a -> S.Set a -> S.Set a
    toggleSetItem i s = if S.member i s then S.delete i s else S.insert i s

    -- | `modifyAt` variant that crashes if the index is out-of-bounds
    unsafeModifyAt :: ∀ a. Int -> (a -> a) -> Array a -> Array a
    unsafeModifyAt i f a = fromMaybe' (\_ -> unsafeCrashWith "Out of bounds access.") $ modifyAt i f a

    -- | `deleteAt` variant that crashes if the index is out-of-bounds
    unsafeDeleteAt :: ∀ a. Int -> Array a -> Array a
    unsafeDeleteAt i a = fromMaybe' (\_ -> unsafeCrashWith "Out of bounds delete.") $ deleteAt i a
