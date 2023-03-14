module Main (main) where

-- command line parsing
import Options.Applicative

import GenExamples


argPsize :: Parser Int
argPsize = option auto
           ( long "psize"
           <> short 'p'
           <> value 8
           <> showDefault
           <> metavar "PS"
           <> help "Generate protocols of size PS" )

argTsize :: Parser Int
argTsize = option auto
           ( long "tsize"
           <> short 't'
           <> value 32
           <> showDefault
           <> metavar "TS"
           <> help "Generate types of size TS" )

argToolbox :: Parser Bool
argToolbox = switch
          ( long "toolbox"
         <> short 'b'
         <> help "Use predefined toolbox" )


argProtocols :: Parser [String]
argProtocols = many (argument str (metavar "PROTO..."))

cmdLineArgs :: Parser GenConfig
cmdLineArgs = GenConfig
              <$> argPsize
              <*> argTsize
              <*> argToolbox
              <*> argProtocols

opts :: ParserInfo GenConfig
opts = info (cmdLineArgs <**> helper)
      ( fullDesc
     <> progDesc "Run the protocol generator"
     <> header "proto-gen" )

main :: IO ()
main = do
  config <- execParser opts
  example config

