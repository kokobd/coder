module Coder.Tool
  ( main,
  )
where

import Coder.Tool.Generate (generate)
import Options.Applicative qualified as Opts
import Relude

main :: IO ()
main = do
  command <- readCommand
  case command of
    Generate -> generate
    InitWorkspace -> pure ()
  pure ()

data Command = Generate | InitWorkspace

readCommand :: IO Command
readCommand =
  Opts.execParser
    $ Opts.info
      (optsParser <**> Opts.helper)
      ( Opts.fullDesc
          <> Opts.progDesc "Print a greeting for TARGET"
          <> Opts.header "hello - a test for optparse-applicative"
      )

optsParser :: Opts.Parser Command
optsParser =
  Opts.subparser
    ( Opts.command "generate" (Opts.info (pure Generate) (Opts.progDesc "General files for the project"))
        <> Opts.command "init-workspace" (Opts.info (pure InitWorkspace) (Opts.progDesc "Initialize a workspace"))
    )
