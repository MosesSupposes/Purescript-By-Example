module Test.MySolutions where

import Prelude
import Data.AddressBook
import Data.List (filter, head, (:), List(..), nubByEq)
import Data.Maybe (Maybe(..))

type FirstName
  = String

type LastName
  = String

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter (_.address.street >>> eq street)

isInBook :: FirstName -> LastName -> AddressBook -> Boolean
isInBook fName lName book = case findEntry fName lName book of
  Just _ -> true
  Nothing -> false

isInBook' :: FirstName -> LastName -> AddressBook -> Boolean
isInBook' fName lName book = wasFound book
  where
  wasFound :: AddressBook -> Boolean
  wasFound remainingContacts = case remainingContacts of
    Nil -> false
    contact : contacts ->
      if contact.firstName == fName && contact.lastName == lName then
        true
      else
        wasFound contacts

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubByEq (\book1 book2 -> book1.firstName == book2.firstName && book1.lastName == book2.lastName) book
