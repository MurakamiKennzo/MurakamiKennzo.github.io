{-# LANGUAGE OverloadedStrings #-}

module Html
  (
    allHtml
  ) where

import Prelude hiding ( div
                      , head
                      , id )
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8
import Control.Monad
import Data.Binary.Builder
import Style

allHtml :: Builder
allHtml = renderHtmlBuilder $ do
  docType
  html $ do
    head $ do
      meta ! charset "UTF-8"
      meta ! name "viewport"
           ! content "width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=0"
      H.style ! rel "text/stylesheet"
              $ do
                toHtml allStyle
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
  p $ "Here is one of My recent photo: "
  img ! alt "my photo"
      ! src "https://img.iplaysoft.com/wp-content/uploads/2019/free-images/free_stock_photo.jpg!0x0.webp"
  p $ "I'm graduated from Chang'an University. Here's one of my school photo: "
  img ! alt "my school"
      ! src "https://img.iplaysoft.com/wp-content/uploads/2019/free-images/free_stock_photo.jpg!0x0.webp"
  p $ "Feel Happy to talk with me, neither the life or the technology."

websiteSection' Technology = do
  p $ "As you already known, I'm a JavaScript and Haskell programmer. I'm addicted to Functional Programming."
  p $ "Let's look at some code about get all prime numbers Here. And First of all, The Javascript code here: "
  img ! alt "javascript-primes"
      ! src "https://img.iplaysoft.com/wp-content/uploads/2019/free-images/free_stock_photo.jpg!0x0.webp"
  p $ "Here's the Haskell code: "
  img ! alt "haskell-primes"
      ! src "https://img.iplaysoft.com/wp-content/uploads/2019/free-images/free_stock_photo.jpg!0x0.webp"
  p $ "Oh, by the way, I'm also can talk some japanese and get a N2 certificate. If you'd like japanese, can chat me with it."

websiteSection' Life = do
  p $ "About my life, I'm a rather private person. Of course, I'll do my best to change it. "
  p $ "I don't think introvert is bad, but I feel some problems with it all around life and work such as girl friend hah."
  p $ "As a programmer, I also like play games such as LoL and Dota2 etc. "
  p $ "Currently my life is some monotonous, If you'd have a good idea, share with me please!"

websiteSection' Work = do
  p $ "Now my work is a front-end developer, work in Hupu@Shanghai. "
  p $ "Here is one my company photo: "
  img ! alt ""
      ! src "https://img.iplaysoft.com/wp-content/uploads/2019/free-images/free_stock_photo.jpg!0x0.webp"
  p $ "I'd Happy to code and I will see some good future."

websiteSection' Contact = do
  p $ "Here's some of my contact information: "
  ul $ do
    li $ a ! href "https://github.com/MurakamiKennzo" $ "GitHub"
    li $ a ! href "tel:18191263056" $ "Phone"
    li $ a ! href "sms:18191263056?body=Hello, murakami!" $ "Message"
    li $ a ! href "mailto:w18191263056@yahoo.co.jp" $ "Email"

type Navs = [Nav]

data Nav = Brief
         | Technology
         | Life
         | Work
         | Contact

instance Show Nav where
  show Brief = "Brief"
  show Technology = "Technology"
  show Life = "Life"
  show Work = "Work"
  show Contact = "Contact"
