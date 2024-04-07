{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Person (Person(..), everyoneHasSlackId, identifier) where

import Data.Time.Calendar
import Data.Maybe (isJust)

data Person = Person
            { slackId :: !(Maybe String)
            , email :: !(Maybe String)
            , firstName :: !String
            , lastName :: !String
            , dateOfBirth :: !(Maybe Day)
            } deriving (Show)

everyoneHasSlackId :: [Person] -> Bool
everyoneHasSlackId = all (isJust . (.slackId))

identifier :: Person -> String
identifier Person{slackId=Just slackId} = "<@" ++ slackId ++ ">"
identifier Person{slackId=Nothing, firstName=fn, lastName=ln} = fn ++ " " ++ ln
