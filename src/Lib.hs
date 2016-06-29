module Lib
    ( mainView
    ) where
import Reflex.Dom
import Safe (readMay)
import qualified Data.Map as Map

searchButton :: (MonadWidget t m) =>
  Dynamic t (Maybe Integer) -> m (Event t Integer)
searchButton cmc = do
  disabledAttr <- mapDyn disabled cmc
  (button', _) <- elDynAttr' "button" disabledAttr $ text "Search"
  let clickEvent = domEvent Click button'
  return $ push filterNumber $ tagDyn cmc clickEvent
    where disabled (Just x)
            | x >= 0 = Map.empty
          disabled _  = Map.fromList [("disabled", "")]
          filterNumber (Just x)
            | x >= 0 = return $ Just x
          filterNumber _ = return Nothing

cmcWidget :: (MonadWidget t m) => m (Dynamic t (Maybe Integer))
cmcWidget = do
  t <- textInput $ def & textInputConfig_inputType .~ "number"
  mapDyn readMay $ _textInput_value t

mainView :: IO ()
mainView = mainWidget $ el "div" $ do
  cmc <- cmcWidget
  _ <- searchButton cmc
  return ()
