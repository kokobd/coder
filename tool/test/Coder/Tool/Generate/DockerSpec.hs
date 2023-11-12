module Coder.Tool.Generate.DockerSpec where

import Coder.Tool.Generate.Docker
import Data.Map.Strict qualified as Map
import Relude
import Test.Tasty
import Test.Tasty.HUnit

test_renderDockerFiles :: [TestTree]
test_renderDockerFiles =
  [ testCase "Empty input produces empty output" $ assertEqual "Expected empty map" (Right Map.empty) (renderDockerfiles Map.empty),
    testCase "Single Dockerfile with no dependencies"
      $ let singleDockerfile = Map.singleton (ImageAlias "base") (Dockerfile Nothing "FROM ubuntu:latest")
            expectedResult = Map.singleton (ImageAlias "base") "FROM ubuntu:latest"
         in assertEqual "Expected single Dockerfile rendered" (Right expectedResult) (renderDockerfiles singleDockerfile),
    testCase "Invalid dependency reference"
      $ let dockerfileWithInvalidDep = Map.singleton (ImageAlias "app") (Dockerfile (Just (ImageAlias "nonexistent")) "FROM app-base")
         in assertBool "Expected InvalidReferenceError" (isLeft $ renderDockerfiles dockerfileWithInvalidDep),
    testCase "Multiple with deps"
      $ let baseDockerfile = Dockerfile Nothing "FROM ubuntu:latest"
            appDockerfile = Dockerfile (Just (ImageAlias "base")) "RUN install-app"
            webDockerfile = Dockerfile (Just (ImageAlias "app")) "RUN install-webserver"

            dockerfilesMap =
              Map.fromList
                [ (ImageAlias "base", baseDockerfile),
                  (ImageAlias "app", appDockerfile),
                  (ImageAlias "web", webDockerfile)
                ]

            expectedBaseContent = "FROM ubuntu:latest"
            expectedAppContent = expectedBaseContent <> "\n" <> "RUN install-app"
            expectedWebContent = expectedAppContent <> "\n" <> "RUN install-webserver"

            expectedResult =
              Map.fromList
                [ (ImageAlias "base", expectedBaseContent),
                  (ImageAlias "app", expectedAppContent),
                  (ImageAlias "web", expectedWebContent)
                ]
         in assertEqual "Expected correctly resolved dependencies" (Right expectedResult) (renderDockerfiles dockerfilesMap)
  ]
