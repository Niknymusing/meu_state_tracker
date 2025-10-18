{-# LANGUAGE OverloadedStrings #-}

import MEU.Performance.WSStateTrackerBenchmark (runWSStateTrackerBenchmark)

main :: IO ()
main = do
  putStrLn "Running WS State Tracker Benchmark..."
  putStrLn ""

  _ <- runWSStateTrackerBenchmark

  putStrLn ""
  putStrLn "Benchmark completed successfully!"