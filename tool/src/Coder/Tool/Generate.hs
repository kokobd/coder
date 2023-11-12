module Coder.Tool.Generate
  ( generate,
  )
where

import Coder.Tool.Generate.Docker qualified as Docker
import Control.Exception (throwIO)
import Relude

generate :: IO ()
generate = do
  dockerfiles <- Docker.readFromYamlDir "dockerfiles"
  dockerfiles' <- either throwIO pure $ Docker.renderDockerfiles dockerfiles
  Docker.writeRenderedDockerfiles "dockerfiles" dockerfiles'
