module Main where

import RIO

import Core
import qualified Docker

import qualified RIO.NonEmpty.Partial as NonEmpty.Partial

-- helper funcs

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
  = Step
    { name = StepName name
    , image = Docker.Image image
    , commands = NonEmpty.Partial.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = NonEmpty.Partial.fromList steps }

-- test values

testPipeline :: Pipeline
testPipeline = makePipeline
  [ makeStep "first step" "ubuntu" ["date"]
  , makeStep "second step" "ubuntu" ["uname -r"]
  ]

testBuild :: Build
testBuild = Build
  { pipeline = testPipeline
  , state = BuildReady
  , completedSteps = mempty
  }

main :: IO ()
main = pure ()
