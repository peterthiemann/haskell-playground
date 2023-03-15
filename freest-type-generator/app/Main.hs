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

argPseed :: Parser Int
argPseed = option auto
           ( long "pseed"
           <> short 'P'
           <> value (0-1)
           -- <> showDefault
           <> metavar "PSEED"
           <> help "Use PSEED to generate protocols" )

argTseed :: Parser Int
argTseed = option auto
           ( long "tseed"
           <> short 'T'
           <> value (0-1)
           -- <> showDefault
           <> metavar "TSEED"
           <> help "Use TSEED to generate protocols" )

argToolbox :: Parser Bool
argToolbox = switch
          ( long "toolbox"
         <> short 'b'
         <> help "Use predefined toolbox" )

argRepeatable :: Parser Bool
argRepeatable = switch
              ( long "repeatable"
              <> short 'r'
              <> help "Repeatable run with PSEED and TSEED")

argProtocols :: Parser [String]
argProtocols = many (argument str (metavar "PROTO..."))

cmdLineArgs :: Parser GenConfig
cmdLineArgs = GenConfig
              <$> argPsize
              <*> argTsize
              <*> argPseed
              <*> argTseed
              <*> argToolbox
              <*> argRepeatable
              <*> argProtocols

opts :: ParserInfo GenConfig
opts = info (cmdLineArgs <**> helper)
      ( fullDesc
     <> progDesc "Run the protocol generator"
     <> header "proto-gen" )

main :: IO ()
main = do
  config <- execParser opts
  runGenerator config
