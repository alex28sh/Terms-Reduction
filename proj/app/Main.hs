
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Reductions
import Lambda
import Data.Maybe
import Data.GI.Base
import Control.Exception
import System.Posix.Unistd
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects as Objects
import Text.Read

reduceIfOkSteps :: Maybe (String, Lambda.Lambda String) -> Strategy  -> String
reduceIfOkSteps (Just ("", lambda)) strategy = checkIfTerminated list_terms 
  where list_terms = (map show (reduce_list strategy lambda 100 []))
        checkIfTerminated list_terms | (length list_terms) == 100 = (unlines list_terms) ++ "Didn't terminate\n"
                                     | otherwise = (unlines list_terms) 
reduceIfOkSteps _ _ =  ("Could not parse lambda")

reduceIfOk :: Maybe (String, Lambda.Lambda String) -> (Maybe Strategy) -> String
reduceIfOk lambda Nothing = "Incorrect strategy"
reduceIfOk lambda (Just strategy) = reduceIfOkSteps lambda strategy

getTypeBools :: [Bool] -> Maybe Strategy
getTypeBools bools | (bools !! 0) = Just NormalOrder
getTypeBools bools | (bools !! 1) = Just ApplicativeOrder
getTypeBools bools | (bools !! 2) = Just CallByValue
getTypeBools bools | (bools !! 3) = Just CallByName
getTypeBools _ = Nothing

getType :: [Gtk.RadioButton] -> IO (Maybe Strategy)
getType buttons  = 
  do 
    bool1 <- Gtk.toggleButtonGetActive (buttons !! 0)
    bool2 <- Gtk.toggleButtonGetActive (buttons !! 1)
    bool3 <- Gtk.toggleButtonGetActive (buttons !! 2)
    bool4 <- Gtk.toggleButtonGetActive (buttons !! 3)
    let res = getTypeBools [bool1, bool2, bool3, bool4]
    return res

buttonClickHandlerUntilConvergence :: Objects.Entry -> [Gtk.RadioButton] -> Gtk.Label -> IO ()
buttonClickHandlerUntilConvergence input buttons output =
  do
    str <- Gtk.entryGetText input
    type_ <- getType buttons 
    let x = Text.unpack str
    let lambda = (runParser exprParser x)
    let response = reduceIfOk lambda type_
    Gtk.labelSetText output (Text.pack response)

main :: IO ()
main = do
  Gtk.init Nothing

  scrolledWin <- new Gtk.ScrolledWindow []

  win <- new Gtk.Window [ #type := Gtk.WindowTypeToplevel
                        , #iconName := "applications-haskell"
                        , #defaultWidth := 1024
                        , #defaultHeight := 768
                        , #child := scrolledWin ]
  on win #destroy Gtk.mainQuit

  examples <- new Gtk.Label [#label := ""]
  Gtk.labelSetText examples (Text.pack ("Examples of reduction (copyable):\nTerm: (λx.λy.x) (λx.x)     Type of reduction: NormalOrder     Reduction: (λx.λy.x) (λx.x) -> (λy.λx.x)\n" ++ 
                                                      "Term: (λy.λx.x) ((λx.x x) (λx.x x))     Type of reduction: ApplicativeOrder     Reduction: (λy.λx.x) ((λx.x x) (λx.x x)) -> (λy.λx.x) ((λx.x x) (λx.x x))  -> ... -> Didn't terminate \n" ++
                                                      "Term: (λy.λx.x     Type of reduction: ApplicativeOrder     Reduction: Could not parse lambda \n"
                            )
                            )
  Gtk.labelSetSelectable examples True
  
  term_message <- new Gtk.Label [#label := ""]
  Gtk.labelSetText term_message "Print term"
  Gtk.labelSetSelectable term_message True

  msg <- new Objects.Entry []

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add scrolledWin box

  #packStart box examples True False 0
  #packStart box term_message True False 0
  #packStart box msg True False 10

  but <- new Gtk.RadioButton [ #label := "Normal order"]
  but1 <- new Gtk.RadioButton [ #label := "Applicative order"]
  but2 <- new Gtk.RadioButton [ #label := "CallByValue"]
  but3 <- new Gtk.RadioButton [ #label := "CallByName"]

  gr <- Gtk.radioButtonSetGroup but [but1, but2, but3]

  #packStart box but False False 10
  #packStart box but1 False False 10
  #packStart box but2 False False 10
  #packStart box but3 False False 10

  text <- new Gtk.Label [#label := ""]
  #packStart box text True False 10

  btnUntilConvergence <- new Gtk.Button [ #label := "Show steps until convergence" ]
  
  #packStart box btnUntilConvergence False False 10

  on btnUntilConvergence #clicked (buttonClickHandlerUntilConvergence msg [but, but1, but2, but3] text)

  #showAll win

  Gtk.main


