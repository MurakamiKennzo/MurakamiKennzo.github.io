{-# LANGUAGE OverloadedStrings #-}

module Html
  (
    allHtml
  ) where

import Prelude hiding ( div
                      , head
                      , id )
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding ( title )
import Text.Blaze.Html.Renderer.Text
import Control.Monad
import Data.Binary.Builder
import Data.Text.Lazy ( Text )

allHtml :: Text
allHtml = renderHtml $ do
  docType
  html $ do
    head $ do
      meta ! charset "UTF-8"
      meta ! name "viewport"
           ! content "width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=0"
      meta ! name "description"
           ! content "murakami's personal website"
      meta ! name "keywords"
           ! content "murakami, Murakami Kennzo, JavaScript, Haskell"
      meta ! name "author"
           ! content "murakami"
      title $ "murakami -- JavaScript & Haskell Programmer"
      link ! rel "stylesheet"
           ! href "./index.css"
    body $ do
      let navs = [ Brief 
                 , Technology 
                 , Life
                 , Work
                 , Contact ]
      websiteHeader
      websiteNavbar navs
      websiteMain navs
      websiteFooter

websiteHeader :: Html
websiteHeader = header ! class_ "header"
  $ do
    div ! id "name"
      $ do
        h1 ! class_ "name-container"
          $ do
            a ! class_ "short-name" 
              ! href "https://github.com/MurakamiKennzo"
              $ "murakami"
        h2 ! class_ "profession" 
            $ "JavaScript & Haskell Programmer"

websiteNavbar :: Navs -> Html
websiteNavbar navs = nav ! class_ "navbar"
                         $ forM_ navs $ do
                           tag <- \nav -> a ! class_ "navbar-item" ! href (stringValue $ '#': show nav)
                           children <- toHtml . show
                           return $ tag children

websiteMain :: Navs -> Html
websiteMain navs = main ! class_ "main" $ do
  article ! class_ "article" $ mempty
  forM_ navs websiteSection

websiteFooter :: Html
websiteFooter = footer ! class_ "footer" $
  p "Create By murakami @Next Innovation^2020"

websiteSection :: Nav -> Html
websiteSection n = section ! id (stringValue $ show n)
                           ! class_ "section"
  $ do
    h3 ! class_ "section-title" $ (toHtml . show $ n)
    websiteSection' n

websiteSection' :: Nav -> Html
websiteSection' Brief = do
  p $ "hello! My name is Murakami Kennzo, and I'm from China. As you see, "
      <> "the name is my japanese name and I love somehow japanese culture. "
  p $ "I'm 26 years old. And as your surprise, I'm not have a girl friend now. If you're a girl and interested with me, "
      <> "you can contact me at the Contact Chapter."
  p $ "Here's my liked girl type, It's japanese star arimura kasumi."
  img ! alt "arimura kasumi"
      ! src "https://i.niupic.com/images/2020/10/22/8U4e.jpg"
  p $ "Feel Happy to talk with me, neither the life or the technology."

websiteSection' Technology = do
  p $ "As you already known, I'm a JavaScript and Haskell programmer. I'm addicted to Functional Programming."
  p $ "Let's look at some code about get all prime numbers Here. And First of all, The Javascript code here: "
  img ! alt "javascript-primes"
      ! src "https://i.niupic.com/images/2020/10/22/8U4b.png"
  p $ "Here's the Haskell code: "
  img ! alt "haskell-primes"
      ! src "https://i.niupic.com/images/2020/10/22/8U4c.png"
  p $ "Oh, by the way, I'm also can talk some japanese and get a N2 certificate. If you'd like japanese, can chat me with it."

websiteSection' Life = do
  p $ "About my life, I'm a rather private person. Of course, I'll do my best to change it. "
  p $ "I don't think introvert is bad, but I feel some problems with it all around life and work such as girl friend hah."
  p $ "As a programmer, I also like play games such as LoL and Dota2 etc. "
  p $ "Currently my life is some monotonous, If you'd have a good idea, share with me please!"

websiteSection' Work = do
  p $ "Now my work is a front-end developer, work in Hupu@Shanghai. "
  p $ "Here is one my company photo: "
  img ! alt "my company"
      ! src "https://i.niupic.com/images/2020/10/22/8U4d.jpg"
  p $ "I'd Happy to code and I will see some good future."

websiteSection' Contact = do
  p $ "Here's some of my contact information: "
  ul $ do
    li $ a ! href "https://github.com/MurakamiKennzo" $ "GitHub"
    li $ a ! href "tel:18191263056" $ "Phone"
    li $ a ! href "sms:18191263056" $ "Message"
    li $ a ! href "mailto:w18191263056@yahoo.co.jp" $ "Email"

type Navs = [Nav]

data Nav = Brief
         | Technology
         | Life
         | Work
         | Contact deriving (Show)
