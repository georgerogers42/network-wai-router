module Network.Wai.Router where
import Control.Monad
import Network.Wai
import Control.Applicative

type Router = Request -> IO (Maybe Response)

runRouter :: Application -> Router -> Application
runRouter d r req = do
    maybe <$> (d req) <*> pure id <*> r req
    

route :: [Router] -> Router
route rs req = do
    msum <$> mapM ($req) rs

respond :: Response -> IO (Maybe Response)
respond = return . return

pass :: IO (Maybe Response)
pass = return Nothing
