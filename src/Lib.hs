module Lib
    ( mainView
    ) where
import Reflex.Dom
import Safe (readMay)
import qualified Data.Map as Map
import Data.Text (unpack)
import Data.Monoid ((<>))

buildRequest :: Integer -> XhrRequest
buildRequest cmc = xhrRequest "GET"
  ("http://pure-dusk-35542.herokuapp.com/" ++ show cmc) def

loadingWidget :: (MonadWidget t m) => a -> m ()
loadingWidget _ = text "Loading"

imageWidget :: (MonadWidget t m) => Maybe String -> m ()
imageWidget (Just img) = emptyElWith "img" $ def
                  & elConfig_attributes .~ ("src" =: img)
imageWidget Nothing = text "Unknown error"

resultWidget :: (MonadWidget t m) => XhrResponse -> m ()
resultWidget res
  | _xhrResponse_status res==404 = text "No card with this cmc found"
  | _xhrResponse_status res==200 = imageWidget $ unpack <$>
                                   _xhrResponse_responseText res
  | otherwise = text "Unknown error"

searchButton :: (MonadWidget t m) =>
  Dynamic t (Maybe Integer) -> m (Event t Integer)
searchButton cmc = do
  disabledAttr <- mapDyn disabled cmc
  (button', _) <- elDynAttr' "button" disabledAttr $ text "Search"
  let clickEvent = domEvent Click button'
  return $ push filterNumber $ tagDyn cmc clickEvent
    where disabled (Just x)
            | x >= 0 = Map.empty
          disabled _  = "disabled" =: ""
          filterNumber (Just x)
            | x >= 0 = return $ Just x
          filterNumber _ = return Nothing

cmcWidget :: (MonadWidget t m) => m (Dynamic t (Maybe Integer))
cmcWidget = do
  t <- textInput $ def & textInputConfig_inputType .~ "range"
                       & textInputConfig_initialValue .~ "0"
                       & textInputConfig_attributes .~ constDyn ("min" =: "0"
                                                                 <> "max" =: "16")
  el "span" $ dynText $ _textInput_value t
  mapDyn readMay $ _textInput_value t

mainView :: IO ()
mainView = mainWidget $ el "div" $ do
  cmc <- cmcWidget
  cmcRequested <- searchButton cmc
  let request = buildRequest <$> cmcRequested
  response <- performRequestAsync request
  let lWidget = loadingWidget <$> cmcRequested
  let iWidget = resultWidget <$> response
  let widgetToInsert = leftmost [iWidget, lWidget]
  _ <- el "div" $
    dyn =<< holdDyn (text "Insert a number and click Search") widgetToInsert
  el "footer" $ do
    el "p" $ text "The program fetches data that is Copyright © Wizards of the Coast - All Rights Reserved"
    el "p" $ text "The program is not affiliated with Wizards of the Coast in any way."
