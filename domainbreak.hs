import Data.Text

import qualified Data.Text.IO as T
import Data.Map as Map 
import Control.Applicative 

data LoginError = InvalidEmail 
    deriving Show


getDomain :: Text -> Either LoginError Text 
getDomain email = 
    case splitOn "@" email of 
        [name, domain] -> Right domain
        _              -> Left InvalidEmail 


