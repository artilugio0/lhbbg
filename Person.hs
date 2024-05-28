module Person where

data Person
    = Person
    { name :: String
    , age :: Int
    }

data Pet
    = Pet
    { petAge :: Int
    }

getName :: Person -> String
getName p = case p of
    (Person n a) -> n

getAge :: Person -> Int
getAge (Person n a) = a
