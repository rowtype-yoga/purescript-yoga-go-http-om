module Test.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Go.OS (ExitCode(..))
import Go.OS as OS
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, evalSpecT)
import Test.Spec.Summary (successful)
import Test.Yoga.Go.HTTP.OmSpec as OmSpec

main :: Effect Unit
main = launchAff_ do
  results <- un Identity (evalSpecT (defaultConfig { exit = false }) [ consoleReporter ] OmSpec.spec)
  unless (successful results) (liftEffect (OS.exit (ExitCode 1)))
