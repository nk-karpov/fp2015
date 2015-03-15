data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName (Person firstName lastName age) = Person (cut firstName) (lastName) (age)
cut :: String -> String
cut ([]) = []
cut (x:[]) = [x]
cut (x:_) =  [x] ++ "."
