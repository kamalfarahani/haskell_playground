data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood

changeMood Blah = Woot

chnageMood _ = Blah
