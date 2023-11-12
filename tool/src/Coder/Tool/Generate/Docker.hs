{-# LANGUAGE QuasiQuotes #-}

module Coder.Tool.Generate.Docker
  ( readFromYamlDir,
    ImageAlias (..),
    Dockerfile,
    Dockerfile' (..),
    renderDockerfiles,
    writeRenderedDockerfiles,
    generateGitHubWorkflow,
  )
where

import Control.Exception (throwIO)
import Control.Lens (view, (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import Data.Traversable (for)
import Data.Yaml qualified as Yaml
import Relude
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (<.>), (</>))

newtype ImageAlias = ImageAlias Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, FromJSON, ToJSON)

type Dockerfile = Dockerfile' Maybe

deriving stock instance Show Dockerfile

deriving stock instance Eq Dockerfile

deriving anyclass instance FromJSON Dockerfile

deriving anyclass instance ToJSON Dockerfile

data Dockerfile' f = Dockerfile
  { dependsOn :: f ImageAlias,
    content :: Text
  }
  deriving stock (Generic)

newtype YamlDockerfileDecodeFailure = YamlDockerfileDecodeFailure {errMsg :: String}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

readFromYamlDir :: FilePath -> IO (Map ImageAlias Dockerfile)
readFromYamlDir yamlDir = do
  yamlFiles <- filter ((== ".yml") . takeExtension) <$> listDirectory yamlDir
  dockerfilesEither <- fmap sequenceA . for yamlFiles $ \yamlFile ->
    fmap (takeBaseName yamlFile,) <$> Yaml.decodeFileEither @Dockerfile (yamlDir </> yamlFile)
  dockerfiles <- either (throwIO . YamlDockerfileDecodeFailure . show) pure dockerfilesEither
  pure . fromList . fmap (first (ImageAlias . toText)) $ dockerfiles

renderDockerfiles :: Map ImageAlias Dockerfile -> Either InvalidReferenceError (Map ImageAlias ByteString)
renderDockerfiles aliasToFile
  | null aliasToFile = Right mempty
  | otherwise =
      let noDeps :: Map ImageAlias Dockerfile = Map.filter (isNothing . view #dependsOn) aliasToFile
          renderedNoDeps :: Map ImageAlias ByteString = fmap (\Dockerfile {content} -> encodeUtf8 content) noDeps
          nextAliasToFile :: Map ImageAlias Dockerfile
          nextAliasToFile =
            fmap
              ( \Dockerfile {dependsOn, content} ->
                  case noDeps Map.!? runIdentity dependsOn of
                    Nothing -> Dockerfile {dependsOn = Just (runIdentity dependsOn), content}
                    Just Dockerfile {content = depContent} ->
                      Dockerfile
                        { dependsOn = Nothing,
                          content = depContent <> "\n" <> content
                        }
              )
              . Map.mapMaybe
                ( \Dockerfile {dependsOn, content} ->
                    fmap (\dependsOn' -> Dockerfile {dependsOn = Identity dependsOn', content}) dependsOn
                )
              $ aliasToFile
       in if null noDeps
            then Left . InvalidReferenceError . fromList . mapMaybe (view #dependsOn) . Map.elems $ aliasToFile
            else Map.union renderedNoDeps <$> renderDockerfiles nextAliasToFile

newtype InvalidReferenceError = InvalidReferenceError
  { refs :: Set ImageAlias
  }
  deriving stock (Show, Eq, Generic)

instance Exception InvalidReferenceError where
  displayException (InvalidReferenceError refs) =
    "Invalid reference(s): " <> show refs

writeRenderedDockerfiles :: FilePath -> Map ImageAlias ByteString -> IO ()
writeRenderedDockerfiles destDir aliasToContent =
  void . flip Map.traverseWithKey aliasToContent $ \(ImageAlias alias) content ->
    writeFileBS (destDir </> "Dockerfile" <.> toString alias)
      $ "# This file was generated, please modify the yml files instead"
      <> "\n"
      <> content

generateGitHubWorkflow :: [ImageAlias] -> Text
generateGitHubWorkflow aliases =
  [__i|
    name: Build docker images and push to Docker Hub

    on:
      push:
        branches:
          - "main"
        paths:
          - "dockerfiles/**"
          - ".github/workflows/image.yml"
    
    jobs:
  |]
    <> "\n"
    <> foldMap
      ( \alias ->
          let t = alias ^. #_ImageAlias
           in addIndentation
                [__i|
                  #{t}:
                    runs-on: ubuntu-latest
                    steps:
                      - uses: actions/checkout@v2
                      - name: Set up Docker Buildx
                        uses: docker/setup-buildx-action@v3
                      - name: Login to Docker Hub
                        uses: docker/login-action@v3
                        with:
                          username: ${{ vars.DOCKERHUB_USERNAME }}
                          password: ${{ secrets.DOCKERHUB_TOKEN }}
                      - name: Build and push coder-#{t}
                        uses: docker/build-push-action@v5
                        with:
                          context: ./dockerfiles
                          file: dockerfiles/Dockerfile.#{t}
                          push: true
                          tags: zelinf/coder-#{t}:latest
                |]
      )
      aliases

addIndentation :: Text -> Text
addIndentation = unlines . fmap ("  " <>) . lines
