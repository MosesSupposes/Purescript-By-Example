module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Except (runExcept)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Foreign (ForeignError, readString, unsafeToForeign)
import Foreign.Index (index)
import Data.Maybe (fromJust, fromMaybe)
import Data.List.NonEmpty (NonEmptyList)
import Web.HTML (window)
import Web.HTML.Window (document)
import Partial.Unsafe (unsafePartial)

import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events

newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { person: examplePerson
  , errors: []
  }

valueOf :: Event -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- index (unsafeToForeign e) "target"
  value <- index target "value"
  readString value

updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Person)
  -> Event
  -> Effect Unit
updateAppState ctx update e =
  for_ (valueOf e) \s -> do
    let newPerson = update s

    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> writeState ctx (AppState { person: newPerson, errors: errors })
      Right _ -> writeState ctx (AppState { person: newPerson, errors: [] })

addressBook :: forall props. React.Component props
addressBook = createClass $ spec initialState \ctx -> do
  AppState { person: Person person@{ homeAddress: Address address }, errors } <- readState ctx

  let renderValidationError err = R.li { children : [ R.text err ] }

      renderValidationErrors [] = []
      renderValidationErrors xs =
        [ D.div [ P.className "alert alert-danger" ]
                [ D.ul' (map renderValidationError xs) ]
        ]

      formField name hint value update =
        D.div [ P.className "form-group" ]
              [ D.label [ P.className "col-sm-2 control-label" ]
                        [ D.text name ]
              , D.div [ P.className "col-sm-3" ]
                      [ D.input [ P._type "text"
                                , P.className "form-control"
                                , P.placeholder hint
                                , P.value value
                                , P.onChange (updateAppState ctx update)
                                ] []
                      ]
              ]

      renderPhoneNumber (PhoneNumber phone) index =
        formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
          Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }

      updateFirstName s = Person $ person { firstName = s }
      updateLastName  s = Person $ person { lastName  = s }

      updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
      updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
      updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

      updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Basic Information" ]

                           , formField "First Name" "First Name" person.firstName updateFirstName
                           , formField "Last Name"  "Last Name"  person.lastName  updateLastName

                           , D.h3' [ D.text "Address" ]

                           , formField "Street" "Street" address.street updateStreet
                           , formField "City"   "City"   address.city   updateCity
                           , formField "State"  "State"  address.state  updateState

                           , D.h3' [ D.text "Contact Information" ]
                           ]
                           <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                  ]
          ]

main :: Effect Unit
main = void do
  log "Rendering address book component"
  --let component = D.div [] [ createFactory addressBook unit ]
  --doc <- window >>= document
  --ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  --render component (unsafePartial fromJust ctr)
