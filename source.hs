{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Data.Aeson
import qualified Data.Dependent.Map as DMap
import           Data.Map
import           Data.Text
import           Data.Text.Internal (Text)
import           Reflex
import           Reflex.Dom

newtype GameId = GameId Int
  deriving (Eq, Show)

data Game = Game {
   gameId            :: GameId
  ,gameName          :: String
  ,gameLargeImageUrl :: String
  ,gameThumbnailUrl  :: String
  ,gameMinPlayers    :: Int
  ,gameMaxPlayers    :: Int
  ,gamePlayingTime   :: Int
  ,gameYear          :: Int
  ,gameAvgRating     :: Double
  ,gameRank          :: Int
} deriving (Eq, Show)


instance FromJSON Game where
  parseJSON (Object v) = Game <$>
    fmap GameId (v .: "gameId") <*>
    v .: "name" <*>
    v .: "image" <*>
    v .: "thumbnail" <*>
    v .: "minPlayers" <*>
    v .: "maxPlayers" <*>
    v .: "playingTime" <*>
    v .: "yearPublished" <*>
    v .: "averageRating" <*>
    v .: "rank"

div' :: MonadWidget t m => m a -> m a
div' = el "div"

img :: MonadWidget t m => String -> m ()
img url = elAttr "img" ("src" =: pack url) (return ())

gameWidget :: MonadWidget t m => Game -> m (Event t Game)
gameWidget game@Game {
  gameName = name
  , gameThumbnailUrl = thumbnailUrl
  , gameMinPlayers = minPlayers
  , gameMaxPlayers = maxPlayers
  , gameYear = year
  , gamePlayingTime = playingTime
  , gameRank = rank
  , gameAvgRating = avgRating } = do
  (gameDiv, _) <- elAttr' "div" ("class" =: "game") $ do
    elClass "div" "thumb" $
      img thumbnailUrl
    elClass "div" "description" $ do
      div' $ text $ pack (name ++ " " ++ "(" ++ show year ++ ")")
      div' $ text $ pack (show minPlayers ++ "-" ++ show maxPlayers ++ " players")
      div' $ text $ pack (show playingTime ++ " minutes")
      div' $ text $ pack ("BGG Rank: " ++ show rank)
      div' $ text $ pack ("Avg Rating: " ++ show avgRating)
  let clickEvent = domEvent Click gameDiv
  return $ fmap (const game) clickEvent

gamesWidget :: MonadWidget t m => Text -> [Game] -> m (Event t Game)
gamesWidget className games =
  elClass className "div" $ do
    es <- mapM gameWidget games
    return $ leftmost es

turnEventIntoWidgets :: MonadWidget t m => Text -> Event t [Game] -> m () -- (Event t Game)
turnEventIntoWidgets className evt = do
  events <- fmap (gamesWidget className) evt
  return ()
  -- do

  -- dynList <- holdDyn ([] :: [Game]) evt
  -- -- mapDyn (gamesWidget className) dynList
  -- -- Dynamic t [Game] -> m (Event t Game)

  -- return ()

buttonWithAttr :: MonadWidget t m => Map Text Text -> m a -> m (Event t ())
buttonWithAttr attrs child = do
  (e, _) <- elAttr' "button" attrs child
  return $ domEvent Click e

defaultGame = Game {
   gameId = GameId 432
  ,gameName = "6 Nimmt!"
  ,gameLargeImageUrl = "http://cf.geekdo-images.com/images/pic2602138.jpg"
  ,gameThumbnailUrl = "http://cf.geekdo-images.com/images/pic2602138_t.jpg"
  ,gameMinPlayers = 2
  ,gameMaxPlayers = 10
  ,gamePlayingTime = 45
  ,gameYear = 1994
  ,gameAvgRating = 6.89239
  ,gameRank = 510
  }

searchBarAndButton :: MonadWidget t m => m (Event t Text) --(Event t ())
searchBarAndButton =
  do
    (searchBar, buttonClick) <- el "div" $
        elClass "div" "row" $ do
          searchBar <- elClass "div" "col-sm-10" $
            textInput $ def & textInputConfig_attributes .~ constDyn (fromList [("class", "form-control"), ("placeholder", "Please enter a BoardGameGeek user")])
          buttonClick <- elClass "div" "col-sm-2" $
            buttonWithAttr (fromList [("class", "btn btn-primary"),("type", "button")]) $ text "Search"
          return (searchBar, buttonClick)
    let submitEvent = keypress Enter searchBar
    return $ tagPromptlyDyn (_textInput_value searchBar) $ leftmost [buttonClick, submitEvent]

collectionUrlBase :: Text
collectionUrlBase = "https://bgg-json.azurewebsites.net/collection/"

getCollection :: MonadWidget t m => Event t Text -> m (Event t [Game])
getCollection e =
  let modifiedEvent = fmap (Data.Text.append collectionUrlBase) e
      resultFunction (Just games) = games
      resultFunction Nothing      = []
  in do
       eventResult <- getAndDecode modifiedEvent
       return $ fmap resultFunction eventResult

main = mainWidget $ elClass "div" "container" $ do
    el "h1" $ text "Board Game Picker"
    el "div" $ do
        el "div" $
            elClass "div" "panel-body" $ text ""
        elClass "div" "row" $
            elClass "div" "col-sm-6" $ do
                el "h3" $ text "Search"
                evt <- searchBarAndButton
                onResp <- getCollection evt
                turnEventIntoWidgets "test" onResp
                dynList <- holdDyn "" (fmap (pack . show) onResp)
                dynText dynList
                return ()
