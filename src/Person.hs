{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Person (Person(..), everyoneHasSlackId) where

import Data.Time.Calendar
import Data.Maybe (isJust)

data Person = Person
            { slackId :: !(Maybe String)
            , email :: !String
            , birthday :: !Day
            } deriving (Show)

everyoneHasSlackId :: [Person] -> Bool
everyoneHasSlackId = all (isJust . (.slackId))
