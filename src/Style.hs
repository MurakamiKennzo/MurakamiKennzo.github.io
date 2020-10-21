{-# LANGUAGE OverloadedStrings #-}

module Style
  (
    allStyle
  ) where

import Prelude hiding ( rem )
import Clay
import Data.Text.Lazy ( Text )
import Data.List.NonEmpty ( fromList )

allStyle :: Text
allStyle = render $ do
  rootStyle
  headerStyle
  navbarStyle
  websiteMainStyle
  footerStyle

rootStyle :: Css
rootStyle = do
  html ? do
    fontSize $ vw 13.33
    lineHeight (unitless 1)
  body ? do
    padding nil nil nil nil
    margin nil nil nil nil
    star ? boxSizing borderBox
    a ? do
      textDecoration none
      color $ rgb 56 135 190
    (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ? do
      margin nil nil nil nil

headerStyle :: Css
headerStyle = do
  ".header" ? do
    position fixed
    top nil
    left nil
    right nil
    height (rem 2.4)
    color $ rgb 34 34 34
    backgroundImage $ linearGradient (straight sideTop) [ (rgb 175 80 0, pct 0)
                                                        , (rgb 255 180 70, pct 100) ]
    "#name" ? do
      margin nil auto nil auto
      width (rem 6.5)
      height (pct 100)
      overflow hidden
      backgroundImage $ radialGradient sideCenter (ellipse closestSide) [ (rgba 255 255 0 0.58, pct 0)
                                                                        , (rgba 255 255 0 0.09, pct 50)
                                                                        , (rgba 255 255 0 0, pct 75) ]
      ".name-container" ? do
        fontSize (rem 0.9)
        color $ rgba 255 255 255 0.78
        textShadow (rem 0) (rem 0) (rem 0.2) (rgba 175 80 0 0.78)
        fontWeight normal
        letterSpacing (rem 0.15)
        textAlign center
        margin (rem 0.32) nil nil nil
        lineHeight (unitless 1.48)

        ".short-name" ? do
          color $ rgba 255 255 255 0.78
      ".profession" ? do
        fontSize (rem 0.35)
        color (rgba 0 0 0 0.46)
        textAlign center
        marginTop (rem 0.04)

navbarStyle :: Css
navbarStyle = do
  ".navbar" ? do
    position fixed
    top (rem 2.4)
    left nil
    right nil
    textAlign center
    backgroundColor $ rgba 255 255 255 0.97
    boxShadow $ fromList [rgba 0 0 0 0.07 `bsColor` shadowWithBlur 0 0 (rem 1.2)]
    width (pct 100)
    lineHeight (unitless 1)
    ".navbar-item" ? do
      display inlineBlock
      fontSize (rem 0.38)
      color $ rgb 56 135 190
      marginRight (rem 0.16)
      verticalAlign middle
      position relative
      top $ rem (-0.08)
      lastOfType &
        marginRight nil

websiteMainStyle :: Css
websiteMainStyle = (".main" ? do
  ".article" ? do
    height (rem 3.38)
    width (rem 7.5)) <> websiteSectionStyle


websiteSectionStyle :: Css
websiteSectionStyle = do
  ".section" ? do
    marginTop (rem (-2.98))
    padding (rem 3.38) (rem 0.2) nil (rem 0.2)
    fontSize (rem 0.32)
    ".section-title" ? do
      color $ rgb 0 55 110
      fontWeight bold
      fontSize (rem 0.42)
      marginBottom (rem 0.22)
    p <? do
      margin (rem 0.16) 0 (rem 0.16) 0
      lineHeight $ unitless 1.48
    img <? do
      display block
      width (pct 100)
      height auto
    ul <? do
      paddingLeft (rem 0.6)
      li <? do
        margin (rem 0.16) 0 (rem 0.16) 0

footerStyle :: Css
footerStyle = ".footer" ? do
  color $ rgba 0 0 0 0.58
  fontSize (rem 0.28)
  textAlign center
  borderTop solid (rem 0.01) (rgba 0 0 0 0.07)
  p <? margin (rem 0.36) 0 (rem 0.36) 0
