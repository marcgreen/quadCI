module Main where

import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial

import qualified System.Process.Typed as Process

import Core
import qualified Docker
import qualified Runner

import Test.Hspec

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

-- test cases

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- runner.prepareBuild $ makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded,StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ makePipeline
          [ makeStep "Should fail" "ubuntu" ["exit 1"]
          ]
  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
  build <- runner.prepareBuild $ makePipeline
           [ makeStep "Create file" "ubuntu" ["echo hello > test"]
           , makeStep "Read file" "ubuntu" ["cat test"]
           ]
  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded,StepSucceeded]

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
    it "should share workspace between steps" do
      testSharedWorkspace docker runner

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"
  Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"
