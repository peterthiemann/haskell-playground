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

argPseed :: Parser (Maybe Int)
argPseed = optional . option auto $
           ( long "pseed"
           <> short 'P'
           <> metavar "PSEED"
           <> help "Use PSEED to generate protocols" )

argTseed :: Parser (Maybe Int)
argTseed = optional . option auto $
           ( long "tseed"
           <> short 'T'
           <> metavar "TSEED"
           <> help "Use TSEED to generate protocols" )

argCount :: Parser Int
argCount = option auto
           ( long "count"
           <> short 'N'
           <> metavar "N"
           <> value 1
           <> showDefault
           <> help "Generate N pairs of types to benchmark" )

argNegative :: Parser Bool
argNegative = switch
              ( long "negative"
              <> short 'n'
              <> help "Generate negative test cases" )

argOutput :: Parser (Maybe FilePath)
argOutput = optional . strOption $
            ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Write source to FILE.algst and FILE.fst" )

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
              <*> argPseed
              <*> argTseed
              <*> argCount
              <*> argNegative
              <*> argToolbox
              <*> argOutput
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
