-- Copyright (c) Tom Ellis
--
-- Contains code Copyright (c) Microsoft Corporation MIT License

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

module Benchmark where

import qualified Data.Foldable
import Data.List (intercalate)
import qualified System.Clock as Clock
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory, emptyTempFile)
import qualified System.Mem

import qualified Tree

data BenchmarkParams = BenchmarkParams
  { runsToMinimizeOver :: Int
  , minimumMeasurableTime_secs :: Double
  , maximumTime_micro :: Double
  }

data Algorithms a = Algorithms
  { aList            :: a
  , aStreaming       :: a
  , aStreamingBetter :: a
  , aIO              :: a
  , aStreamly        :: a
  }
  deriving (Functor, Foldable, Traversable)

algorithms_ :: Algorithms (String, Tree.Tree -> IO (), String)
algorithms_ = Algorithms
  { aStreaming       = ("Streaming", Tree.printTreeStreaming, prettyBad)
  , aList            = ("List", Tree.printTreeList, veryBad)
  , aStreamingBetter = ("Streaming better", Tree.printTreeBetterStreaming, good)
  , aIO              = ("IO", Tree.printTreeIO, baseline)
  , aStreamly        = ("Streamly", Tree.printTreeStreamly, purple)
  }
  where
      veryBad   = "red"
      prettyBad = "orange"
      good      = "web-green"
      baseline  = "web-blue"
      purple    = "purple"

fast :: BenchmarkParams
fast = BenchmarkParams
  { runsToMinimizeOver = 5
  , minimumMeasurableTime_secs = 0.01
  , maximumTime_micro = 1000 * 1000
  }

full :: BenchmarkParams
full = BenchmarkParams
  { runsToMinimizeOver = 10
  , minimumMeasurableTime_secs = 0.1
  , maximumTime_micro = 1000 * 1000
  }

-- | This is the entry point to the module.  When run it will
-- benchmark the algorithms on a random set of expressions.  The data
-- from the run will be written out to a directory whose name is
-- displayed at the end of the run.
benchmark :: BenchmarkParams -> IO ()
benchmark bps = do
  benchmarksDir <- createTempDirectory "." "benchmarks"
  results_genNames <- benchmarkResults 80 bps
  flip mapM_ results_genNames $ \results' -> do
    datasets <- flip mapM results' $ \((algorithmName, algorithmColor), results) -> do
        let textOutput = flip concatMap results $ \(size, time) ->
              show size ++ " " ++  show time ++ "\n"

        filename <- emptyTempFile benchmarksDir (algorithmName ++ ".dat")

        writeFile filename textOutput

        return PlotDataset
          { pdFile  = filename
          , pdTitle = algorithmName
          , pdColor = algorithmColor
          , pdStyle = "7"
          , pdSize  = "0.25"
          }

    makeGnuplot benchmarksDir datasets

benchmarkResults :: Int
                 -> BenchmarkParams
                 -> IO [[((String, String), [(Int, Double)])]]
benchmarkResults maxSize bps = do
  let algorithms = Data.Foldable.toList algorithms_
  results <- benchmarkNoPlot bps algorithms maxSize
  pure [results]

benchmarkNoPlot :: BenchmarkParams
                -> [(String, Tree.Tree -> IO (), string)]
                -> Int
                -> IO [((String, string), [(Int, Double)])]
benchmarkNoPlot bps algorithms logMaxSize = do
  flip mapM (enumFrom1 algorithms) $ \(i, algorithm_) -> do
    let (algorithmName, algorithm, algorithmExtra) = algorithm_
    results <- loop (iteratorOfList [60..logMaxSize]) [] $ \logSize rest -> do

      let treeSize = round ((1.1 :: Double) ** fromIntegral logSize)

      -- We force the expression after generating it.  The Expr type
      -- is strict, that is forcing it forces everything it contains,
      -- therefore no time is wasted forcing it in the hashing
      -- algorithm itself.  On the other hand adding this bang pattern
      -- made absolutely no difference to the benchmarks.  Presumably
      -- the expression is generated already in forced state.  But
      -- it's nice to keep this here for clarity.

      putStrLn ("Algorithm "
                 ++ show i ++ "/" ++ show (length algorithms)
                 ++ " (" ++ algorithmName ++ ")")
      putStrLn ("Generating expression ...")
      let !tree = Tree.leftSkewed treeSize
      putStrLn ("done.  Size was " ++ show treeSize)

      let minimumMeasureableTime_micro = minimumMeasurableTime_secs bps * 1000 * 1000

      (repeats, firstStats) <- benchmarkUntil minimumMeasureableTime_micro
                                              1
                                              algorithm
                                              tree

      r <- benchmarkMore firstStats
                         (runsToMinimizeOver bps - 1)
                         repeats
                         algorithm
                         tree

      let (n, mean_micro, tmin_micro, _, stddev_micro) = stats r
          showFloat = printf "%.0f" :: Double -> String

      putStrLn ("Count: "    ++ show n)
      putStrLn ("Mean: "     ++ showFloat mean_micro ++ "us")
      putStrLn ("Min: "      ++ showFloat tmin_micro ++ "us")
      putStrLn ("Std dev: "  ++ showFloat stddev_micro ++ "us")

      let done = tmin_micro > maximumTime_micro bps
          tmin_secs = tmin_micro / (1000 * 1000)
          rest' = (treeSize, tmin_secs):rest

      pure $ (if done then Right else Left) rest'

    pure ((algorithmName, algorithmExtra), results)

makeGnuplot :: FilePath -> [PlotDataset] -> IO ()
makeGnuplot benchmarksDir results = do
  gnuplotFilename <- emptyTempFile benchmarksDir "benchmarks.gnuplot"
  gnuplotPdfFilename <- emptyTempFile benchmarksDir "benchmarks-pdf.gnuplot"

  let gnuplotFileContent = gnuplotFile results
      (outputPdf, gnuplotPdfFileContent) = gnuplotFilePdf benchmarksDir results

  writeFile gnuplotFilename gnuplotFileContent
  writeFile gnuplotPdfFilename gnuplotPdfFileContent

  putStrLn ("I put stuff in " ++ benchmarksDir ++ ".")
  putStrLn "If you have an X server and you want a live graph view run:"
  putStrLn ("gnuplot --persist " ++ gnuplotFilename)
  putStrLn "If you want to generate a PDF run:"
  putStrLn ("gnuplot " ++ gnuplotPdfFilename)
  putStrLn ("You will find the output PDF in " ++ outputPdf)

type AggregateStatistics = (Int, Double, Double, Double)

stats :: AggregateStatistics -> (Int, Double, Double, Double, Double)
stats (n, tsum, tsquaredsum, tmin) = (n, mean, tmin, variance, stddev)
  where n' = fromIntegral n
        mean     = tsum / n'
        variance = tsquaredsum / n' - mean * mean
        stddev   = sqrt variance

-- This is probably the entry point you want to use to benchmark an
-- algorithm on a list of expressions each read from a FilePath.
--
-- Runs algorithm on expression and produces aggregate timing
-- statistics.
--
-- benchmarkOne will seq the result of `algorithm expression`.  It is
-- the caller's responsibility to ensure that this causes *all*
-- desired work to be performed.  If you're not sure on this point
-- please ask the author.
benchmarkOne :: Int
             -> Integer
             -> (e -> IO r)
             -> e
             -> IO AggregateStatistics
benchmarkOne = benchmarkMore (0, 0, 0, infinity)
  where infinity = 1e60

benchmarkMore :: AggregateStatistics
              -> Int
              -> Integer
              -> (e -> IO r)
              -> e
              -> IO AggregateStatistics
benchmarkMore already samplesPerExpression iterationsPerSample algorithm expression =
  times samplesPerExpression already $ \(n, !t, !tsquared, !minSoFar) -> do
        System.Mem.performMajorGC
        start <- Clock.getTime Clock.Monotonic
        times iterationsPerSample () $ \() -> algorithm expression >> pure ()
        stop <- Clock.getTime Clock.Monotonic

        let elapsed_micro = iterationsElapsed_micro / fromIntegral iterationsPerSample
              where iterationsElapsed = Clock.diffTimeSpec stop start
                    iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed
                    iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3

        return (n + 1,
                t + elapsed_micro,
                tsquared + elapsed_micro * elapsed_micro,
                min minSoFar elapsed_micro)

benchmarkUntil :: Double
               -> Integer
               -> (e -> IO r)
               -> e
               -> IO (Integer, AggregateStatistics)
benchmarkUntil minimumMeasurableTime_micro repeats f x = do
  System.Mem.performMajorGC
  start <- Clock.getTime Clock.Monotonic
  times repeats () $ \() -> f x >> pure ()
  stop <- Clock.getTime Clock.Monotonic

  let iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3
        where iterationsElapsed = Clock.diffTimeSpec stop start
              iterationsElapsed_nano = Clock.toNanoSecs iterationsElapsed

      elapsed_micro = iterationsElapsed_micro / fromIntegral repeats

  if iterationsElapsed_micro < minimumMeasurableTime_micro
  then benchmarkUntil minimumMeasurableTime_micro (2 * repeats) f x
  else pure (repeats,
             (1, elapsed_micro, elapsed_micro * elapsed_micro, elapsed_micro))

gnuplotFilePdf :: String
               -> [PlotDataset]
               -> (String, String)
gnuplotFilePdf benchmarksDir results = (outputPdf, unlines [
    "set terminal pdf font \"Helvetica,13\""
  , "set output \"" ++ outputPdf ++ "\""
  , gnuplotFile results
  ])
  where outputPdf = benchmarksDir ++ "/benchmark.pdf"

gnuplotFile :: [PlotDataset] -> String
gnuplotFile results =
  unlines [ "set xlabel \"Number of nodes in tree\""
          , "set ylabel \"Time taken to print tree (s)"
          , "set format y '%.0se%S'"
          , "set format x '%.0se%S'"
--          , "set size 1,1"
          , "set logscale xy 10"
          , "set key left top"
          , "plot " ++ intercalate ", " (fmap plotDataset results)
                    ++ ", "
                    ++ intercalate ", "
                    [ "[x=1e3:] (x/1e3) * 2e-3 title \"x\" at begin lt rgb \"gray\""
                    , "[x=1e3:] (x/1e3)**2 * 10e-3 title \"x^2\" at begin lt rgb \"gray\"" ]
          ]

data PlotDataset = PlotDataset
  { pdFile  :: String
  , pdTitle :: String
  , pdColor :: String
  , pdStyle :: String
  , pdSize  :: String
  }

plotDataset :: PlotDataset -> String
plotDataset pd = intercalate " " [ quote (pdFile pd)
                                 , "title " ++ quote (pdTitle pd)
                                 , "lt rgb " ++ quote (pdColor pd)
                                 , "pt " ++ pdStyle pd
                                 , "ps " ++ pdSize pd ]
  where quote s = "\"" ++ s ++ "\""

times :: (Ord a, Num a, Monad m) => a -> s -> (s -> m s) -> m s
times n s f = times_f 0 s
  where times_f m s_ =
          if m >= n
          then return s_
          else do
            s' <- f s_
            times_f (m + 1) s'

enumFrom1 :: [a] -> [(Int, a)]
enumFrom1 = zip [1..]

loop :: Monad m => Iterator m b -> a -> (b -> a -> m (Either a a)) -> m a
loop iterator a f = do
  mNext <- stepIterator iterator
  case mNext of
    Nothing -> pure a
    Just (b, iteratorNext) -> do
      ea <- f b a
      case ea of
        Left aNext -> loop iteratorNext aNext f
        Right aDone -> pure aDone

newtype Iterator m a = Iterator (m (Maybe (a, Iterator m a)))
  deriving Functor

stepIterator :: Iterator m a -> m (Maybe (a, (Iterator m a)))
stepIterator (Iterator a) = a

iteratorOfList :: Applicative m => [a] -> Iterator m a
iteratorOfList = \case
  []   -> Iterator (pure Nothing)
  x:xs -> Iterator (pure (Just (x, iteratorOfList xs)))
