module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Data.Array (deleteAt, elem, length, modifyAt, snoc, zip, (..))
import Data.Foldable (sum)
import Data.Formatter.Number (Formatter(..), format)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe')
import Data.Number (fromString)
import Data.Set as S
import Data.Set.NonEmpty as NS
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafeCrashWith)


type Name = String
type Money = Number

type EditedTransType =         { source :: Name
        , targets :: S.Set Name
        , amount :: String
        , note :: String
        }

data Transaction
    = EditedTrans EditedTransType
    | FinalTrans
        { source :: Name
        , targets :: NS.NonEmptySet Name
        , amount :: Number
        , note :: String
    }

data TableRowEvent
    = SetSource Name
    | ToggleTarget Name
    | SetAmount String
    | SetNote String
    | DeleteRow
    | FinalizeRow
    | EditRow

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
table names transactions = D.div_ [P.className "container"] $
    D.table [ P.className "table table-striped" ]
        [ D.thead' [tableHeader names]
        , D.tbody' $
            map (\(Tuple i x) -> RowEvent i <$> tableRow names x) (zip (0 .. length transactions) transactions)
        , D.thead' [ D.tr [] [ D.td []
            [ AddRowEvent <$ D.button [ P.className "btn btn-primary", P.onClick ] [ D.text "+" ] ]
            ] ]
        ]


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
        <> map (\n -> D.th [P.style {transform: "rotate(-45.0deg)"}] [D.text n]) names
        <> [ D.th [] [] ]
        <> map (\n -> D.th [P.style {transform: "rotate(-45.0deg)"}] [D.text n]) names
        <> [ D.td [] [] ]


tableRow :: Array Name -> Transaction -> Widget HTML TableRowEvent
tableRow names (EditedTrans transaction) = D.tr []
    $ [ D.td [] [
        D.div [P.className "btn-group", P.role "group"]
            [ FinalizeRow <$ D.button [P.className "btn btn-primary", P.onClick] [ D.text "✔"]
            ,   DeleteRow <$ D.button [P.className "btn btn-outline-secondary", P.onClick] [ D.text "✗" ]
            ]
        ]
    ]
    <> map showRadio names
    <> [ D.td' [ D.input [ P.className "form-control", P._type "text", P.value transaction.amount, (SetAmount <<< unsafeTargetValue) <$> P.onChange ] ] ]
    <> map showCheckbox names
    <> [ D.td' [ D.input [ P.className "form-control", P._type "text", P.value transaction.note, (SetNote <<< unsafeTargetValue) <$> P.onChange ] ] ]
    where
    showRadio name =
        D.td [ SetSource name <$ P.onClick ]
            [ D.input [ P._type "radio", SetSource name <$ P.onChange, P.checked (name == transaction.source) ] ]
    showCheckbox name =
        D.td [ ToggleTarget name <$ P.onClick ]
            [ D.input [ P._type "checkbox", ToggleTarget name <$ P.onChange, P.checked (S.member name transaction.targets)  ] ]
tableRow names (FinalTrans transaction) = D.tr []
    $ [ D.td [] [
        D.div [P.className "btn-group", P.role "group"]
            [ EditRow <$ D.button [P.className "btn btn-primary", P.onClick] [ D.text "✎"]
            ]
        ]
    ]
    <> map showRadio names
    <> [ D.td [] [ D.text $ formatCurrency transaction.amount ] ]
    <> map showCheckbox names
    <> [ D.td [] [ D.text transaction.note ] ]
    where
    showRadio name =
        D.td []
            [ D.input [ P._type "radio", P.disabled true, P.checked (name == transaction.source) ] ]
    showCheckbox name =
        D.td []
            [ D.input [ P._type "checkbox", P.disabled true, P.checked (NS.member name transaction.targets)  ] ]


results :: ∀ a. Array Name -> Array Transaction -> Widget HTML a
results names transactions = D.div [ P.className "container" ] [
    D.table [ P.className "table" ] (
        [ D.tr []
            [ D.th [] [ D.text "Name" ]
            , D.th [] [ D.text "Paid" ]
            , D.th [] [ D.text "Received" ]
            , D.th [] [ D.text "Must get from others" ]
            ]
        ] <> map resultRow names
    )
    ] where
    resultRow name = D.tr []
        [ D.td [] [ D.text name ]
        , D.td [] [ D.text <<< formatCurrency $ paid ]
        , D.td [] [ D.text <<< formatCurrency $ received ]
        , D.td [] [ D.text <<< formatCurrency $ paid - received ]
        ] where
        paid     = sum <<< (flip map transactions) $ \t -> case t of
            EditedTrans _ -> 0.0
            FinalTrans ft -> if ft.source == name then ft.amount else 0.0
        received = sum <<< (flip map transactions) $ \t -> case t of
            EditedTrans _ -> 0.0
            FinalTrans ft -> if elem name ft.targets then ft.amount / toNumber (NS.size ft.targets) else 0.0


initialState :: Array Transaction
initialState = [ defaultTransaction ]

defaultTransaction :: Transaction
defaultTransaction = EditedTrans
    { source: "Evča"
    , targets: S.empty
    , amount: "200.00"
    , note: "Testing transaction"
    }


formatCurrency :: Number -> String
formatCurrency = format (Formatter { comma: false, before: 0, after: 2, abbreviations: false, sign: false })


main :: Effect Unit
main = runWidgetInDom "root" $ go initialState where
    go :: ∀ a. Array Transaction -> Widget HTML a
    go state = do
        let names = ["Evča", "Pavel", "Jíťa", "Péťa", "Stano"]
        event <- table names state <|> results names state

        liftEffect $ log (show event)

        case event of
            RowEvent i (SetSource name)    -> go $ unsafeModifyAt i (unsafeMutate (\et -> et { source=name })) state
            RowEvent i (ToggleTarget name) -> go $ unsafeModifyAt i (unsafeMutate (\et -> et { targets=toggleSetItem name et.targets })) state
            RowEvent i (SetAmount s)       -> go $ unsafeModifyAt i (unsafeMutate (\et -> et { amount=s })) state
            RowEvent i (SetNote s)         -> go $ unsafeModifyAt i (unsafeMutate (\et -> et { note=s })) state
            RowEvent i DeleteRow           -> go $ unsafeDeleteAt i state
            RowEvent i FinalizeRow         -> go $ unsafeModifyAt i unsafeFinalize state
            RowEvent i EditRow             -> go $ unsafeModifyAt i unsafeEdit state
            AddRowEvent -> go (snoc state defaultTransaction)

    toggleSetItem :: ∀ a. Ord a => a -> S.Set a -> S.Set a
    toggleSetItem i s = if S.member i s then S.delete i s else S.insert i s

    -- | `modifyAt` variant that crashes if the index is out-of-bounds
    unsafeModifyAt :: ∀ a. Int -> (a -> a) -> Array a -> Array a
    unsafeModifyAt i f a = fromMaybe' (\_ -> unsafeCrashWith "Out of bounds access.") $ modifyAt i f a

    -- | `deleteAt` variant that crashes if the index is out-of-bounds
    unsafeDeleteAt :: ∀ a. Int -> Array a -> Array a
    unsafeDeleteAt i a = fromMaybe' (\_ -> unsafeCrashWith "Out of bounds delete.") $ deleteAt i a

    unsafeMutate :: (EditedTransType -> EditedTransType) -> Transaction -> Transaction
    unsafeMutate f (EditedTrans t) = EditedTrans (f t)
    unsafeMutate _ (FinalTrans _) = unsafeCrashWith "Edited non-editable transaction."

    unsafeFinalize :: Transaction -> Transaction
    unsafeFinalize (EditedTrans t) = FinalTrans
        { source: t.source
        , targets: fromMaybe' (\_ -> unsafeCrashWith "Tried to finalize without targets.") $ NS.fromSet t.targets
        , amount: fromMaybe' (\_ -> unsafeCrashWith "Tried to finalize with non-numeric amount.") $ fromString t.amount
        , note: t.note
        }
    unsafeFinalize (FinalTrans _) = unsafeCrashWith "Finalized already-finalized transaction."

    unsafeEdit :: Transaction -> Transaction
    unsafeEdit (FinalTrans t) = EditedTrans
        { source: t.source
        , targets: NS.toSet t.targets
        , amount: formatCurrency t.amount
        , note: t.note
        }
    unsafeEdit (EditedTrans t) = unsafeCrashWith "Finalized already-finalized transaction."
