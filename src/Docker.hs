module Docker where

import RIO
import qualified Socket

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

import qualified Network.HTTP.Simple as HTTP

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId id) = id

data CreateContainerOptions
  = CreateContainerOptions
    { image :: Image
    , script :: Text
    , volume :: Volume
    }

newtype Image = Image Text
  deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

type RequestBuilder = Text -> HTTP.Request

newtype Volume = Volume Text
  deriving (Eq, Show)

volumeToText :: Volume -> Text
volumeToText (Volume v) = v

parseResponse
  :: HTTP.Response ByteString
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    Left e -> throwString e
    Right status -> pure status

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  let image = imageToText options.image
  let bind = volumeToText options.volume <> ":/app"

  let body = Aeson.object
               [ ("Image", Aeson.toJSON image)
               , ("Tty", Aeson.toJSON True)
               , ("Labels", Aeson.object [("quad", "")])
               , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
               , ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh")
               , ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script])
               , ("WorkingDir", "/app")
               , ("HostConfig", Aeson.object [ ("Binds", Aeson.toJSON
                                                 [bind]) ])
               ]

  let req = makeReq "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req

  let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId


  parseResponse res parser
  
  -- traceShowIO res

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq container = do
  let path = "/containers/" <> containerIdToText container <> "/start"
  let req = makeReq path
          & HTTP.setRequestMethod "POST"
          
  void $ HTTP.httpBS req

data Service
  = Service
    { createContainer :: CreateContainerOptions -> IO ContainerId
    , startContainer :: ContainerId -> IO ()
    , containerStatus :: ContainerId -> IO ContainerStatus
    , createVolume :: IO Volume
    }

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other

  let req = makeReq
          $ "/containers/" <> containerIdToText container <> "/json"
          
  res <- HTTP.httpBS req
  parseResponse res parser

createService :: IO Service
createService = do
  manager <- Socket.newManager "/var/run/docker.sock"

  -- Make Request
  let makeReq :: RequestBuilder
      makeReq path =
        HTTP.defaultRequest
        & HTTP.setRequestPath (encodeUtf8 $ "/v1.40" <> path)
        & HTTP.setRequestManager manager

  pure Service
    { createContainer = createContainer_ makeReq
    , startContainer = startContainer_ makeReq
    , containerStatus = containerStatus_ makeReq
    , createVolume = createVolume_ makeReq
    }

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)


createVolume_ :: RequestBuilder -> IO Volume
createVolume_ makeReq = do
  let body =
        Aeson.object
          [ ("Labels", Aeson.object [("quad", "")])
          ]
  let req =
        makeReq "/volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  let parser = Aeson.withObject "create-volume" $ \o -> do
        name <- o .: "Name"
        pure $ Volume name
  res <- HTTP.httpBS req
  parseResponse res parser
