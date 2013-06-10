{-# LANGUAGE OverloadedStrings #-}

module Views where

import qualified Data.Text as T
import Data.Text (Text)
import Happstack.Lite (Response, toResponse)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (customAttribute, dataAttribute, textTag, toValue)
import Text.Blaze.Internal (Attributable, customParent)
import Data.Monoid ((<>))
import Control.Monad
import Data.Maybe (fromJust)

type Template = Html
type TemplateMap = [(Text,Template)]

templates :: TemplateMap
templates = 
  [ ("nameLabel", 
     H.p $ do
       "Logged in as: "
       H.span != "text: name" $ ""
    ) 
  , ("nameList",
     H.ul != "foreach: nameList" $ 
       H.li != "text: $data" $ ""
    )
  ]

homePage :: Response
homePage = pageTmpl "WebSockets!" ["lobby"] ["nameLabel", "nameList"] $ do
  H.h1 "Lobby"
  putTemplate "name: 'nameLabel'"
  H.h2 "Connected clients"
  putTemplate "name: 'nameList'"

pageTmpl :: Text -> [Text] -> [Text] -> Html -> Response
pageTmpl title jsFiles tmplNames inner = toResponse $
  H.html $ do
    H.head $ do
      H.title $ toHtml title
      jsLink "jquery-2.0.1.min"
      jsLink "knockout-2.2.1"
      mapM_ jsLink jsFiles
    H.body $ do 
      forM_ tmplNames $ \nm ->
        H.script ! A.id (toValue nm) ! A.type_ "text/html" $ 
        fromJust $ lookup nm templates
      inner
    
putTemplate :: Text -> Html
putTemplate templateDescr =
  H.div != ("template: {" <> toValue templateDescr <> "}") $ ""

jsLink :: Text -> Html
jsLink filename = 
  H.script ! A.type_ "text/javascript" 
    ! A.src (toValue $ "/static/js/" <> filename <> ".js") $ ""

cAttr :: H.Tag -> H.AttributeValue -> H.Attribute
cAttr = customAttribute
ngAttr :: Text -> H.AttributeValue -> H.Attribute
ngAttr at = cAttr (textTag $ "ng-" <> at)
customTag :: Text -> H.Markup
customTag name = customParent (textTag name) ""
(!=) :: Attributable h => h -> H.AttributeValue -> h
tag != attr = tag ! dataAttribute "bind" attr