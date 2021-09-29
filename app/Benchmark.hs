-- Copyright (c) Tom Ellis
--
-- Contains code Copyright (c) Microsoft Corporation MIT License

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}

module Benchmark where

import Streaming (lift)
import qualified Data.Foldable
import Data.List (intercalate,uncons)
import qualified System.Clock as Clock
import Text.Printf (printf)
import System.IO.Temp (createTempDirectory, emptyTempFile)
import qualified System.Mem
import qualified Streaming.Prelude as S
import qualified Streaming.Internal as SI

import qualified Tree

data BenchmarkParams = BenchmarkParams
  { runsToMinimizeOver :: Int
  , minimumMeasurableTime_secs :: Double
  , maximumTime_micro :: Double
  }

data Algorithms a = Algorithms
  { aPipes           :: a
  , aConduit         :: a
  , aList            :: a
  , aStreaming       :: a
  , aStreamly        :: a
  , aStreamingBetter :: a
  , aIO              :: a
  }
  deriving (Functor, Foldable, Traversable)

algorithms_ :: Algorithms (String, Tree.Tree -> IO (), String)
algorithms_ = Algorithms
  { aStreaming       = ("Streaming", Tree.printTreeStreaming, prettyBad)
  , aList            = ("List", Tree.printTreeList, veryBad)
  , aStreamingBetter = ("Streaming better", Tree.printTreeBetterStreaming, good)
  , aIO              = ("IO", Tree.printTreeIO, baseline)
  , aStreamly        = ("Streamly", Tree.printTreeStreamly, purple)
  , aPipes           = ("Pipes", Tree.printTreePipes, cyan)
  , aConduit         = ("Conduit", Tree.printTreePipes, magenta)
  }
  where
      veryBad   = "red"
      prettyBad = "orange"
      good      = "web-green"
      baseline  = "web-blue"
      purple    = "purple"
      cyan      = "cyan"
      magenta   = "magenta"

fast :: BenchmarkParams
fast = BenchmarkParams
  { runsToMinimizeOver = 3
  , minimumMeasurableTime_secs = 0.001
  , maximumTime_micro = 5 * 1000
  }

full :: BenchmarkParams
full = BenchmarkParams
  { runsToMinimizeOver = 5
  , minimumMeasurableTime_secs = 0.01
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
        filename <- emptyTempFile benchmarksDir (algorithmName ++ ".dat")
        S.writeFile filename $
          flip S.map results $ \(size, time) -> show size ++ " " ++  show time ++ "\n"

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
                 -> IO [[((String, String), SI.Stream (S.Of (Int, Double)) IO ())]]
benchmarkResults maxSize bps = do
  let algorithms = Data.Foldable.toList algorithms_
  results <- S.toList_ $ benchmarkNoPlot bps algorithms maxSize

  pure [results]

benchmarkNoPlot :: BenchmarkParams
                -> [(String, Tree.Tree -> IO (), string)]
                -> Int
                -> S.Stream (S.Of ((String, string), S.Stream (S.Of (Int, Double)) IO ())) IO ()
benchmarkNoPlot bps algorithms logMaxSize = do
  S.for (S.each (enumFrom1 algorithms)) $ \(i, algorithm_) -> do
    let (algorithmName, algorithm, algorithmExtra) = algorithm_
    results <- pure $ S.map fst $ span1 snd $ S.for (iteratorOfList [60..logMaxSize]) $ \logSize -> do

      let treeSize = round ((1.1 :: Double) ** fromIntegral logSize)

      lift $ putStrLn ("Algorithm "
                 ++ show i ++ "/" ++ show (length algorithms)
                 ++ " (" ++ algorithmName ++ ")")
      -- We force the tree after generating it.  leftSkewed returns a
      -- fully-evaluated tree, that is forcing it forces everything it
      -- contains, therefore no time is wasted forcing it in the
      -- algorithm itself.
      let !tree = Tree.leftSkewed treeSize

      r <- lift $ benchmark' bps algorithm tree

      let (n, mean_micro, tmin_micro, _, stddev_micro) = stats r
          showFloat = printf "%.0f" :: Double -> String

      lift $ putStrLn ("Count: "    ++ show n)
      lift $ putStrLn ("Mean: "     ++ showFloat mean_micro ++ "us")
      lift $ putStrLn ("Min: "      ++ showFloat tmin_micro ++ "us")
      lift $ putStrLn ("Std dev: "  ++ showFloat stddev_micro ++ "us")

      let done = tmin_micro > maximumTime_micro bps
          tmin_secs = tmin_micro / (1000 * 1000)

      S.yield ((treeSize, tmin_secs), not done)

    S.yield ((algorithmName, algorithmExtra), results)

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

time_nano :: Int -> (a -> IO r) -> a -> IO Integer
time_nano n f x = do
  System.Mem.performMajorGC
  start <- Clock.getTime Clock.Monotonic
  times n () $ \() -> f x >> pure ()
  stop <- Clock.getTime Clock.Monotonic
  pure (Clock.toNanoSecs (Clock.diffTimeSpec stop start))

benchmark' :: BenchmarkParams -> (e -> IO r) -> e -> IO AggregateStatistics
benchmark' bps f x = do
  let minimumMeasurableTime_micro = minimumMeasurableTime_secs bps * 1000 * 1000
  (repeats, firstStats) <- benchmarkUntil minimumMeasurableTime_micro
                                          1
                                          f
                                          x

  benchmarkMore firstStats
                (runsToMinimizeOver bps - 1)
                repeats
                f
                x

benchmarkMore :: AggregateStatistics
              -> Int
              -> Int
              -> (e -> IO r)
              -> e
              -> IO AggregateStatistics
benchmarkMore already samplesPerExpression iterationsPerSample algorithm expression =
  times samplesPerExpression already $ \(n, !t, !tsquared, !minSoFar) -> do
        iterationsElapsed_nano <- time_nano iterationsPerSample algorithm expression 

        let elapsed_micro = iterationsElapsed_micro / fromIntegral iterationsPerSample
              where iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3

        return (n + 1,
                t + elapsed_micro,
                tsquared + elapsed_micro * elapsed_micro,
                min minSoFar elapsed_micro)

benchmarkUntil :: Double
               -> Int
               -> (e -> IO r)
               -> e
               -> IO (Int, AggregateStatistics)
benchmarkUntil minimumMeasurableTime_micro repeats f x = do
  iterationsElapsed_nano <- time_nano repeats f x

  let iterationsElapsed_micro = fromIntegral iterationsElapsed_nano / 1e3
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

type Iterator m a = S.Stream (S.Of a) m ()

stepIterator :: Monad m => Iterator m a -> m (Maybe (a, (Iterator m a)))
stepIterator = S.uncons

iteratorOfList :: Monad m => [a] -> Iterator m a
iteratorOfList = S.each

span :: Functor m
     => (a -> Either b c)
     -> SI.Stream (S.Of a) m r
     -> SI.Stream (S.Of b) m
                  (Either (c, SI.Stream (S.Of a) m r) r)
span thePred = loop' where
  loop' str = case str of
    SI.Return r         -> SI.Return (Right r)
    SI.Effect m          -> SI.Effect $ fmap loop' m
    SI.Step (a S.:> rest) -> case thePred a of
      Left b  -> SI.Step (b S.:> loop' rest)
      Right c -> SI.Return (Left (c, rest))

span1 :: Monad m
      => (a -> Bool)
      -> SI.Stream (S.Of a) m ()
      -> SI.Stream (S.Of a) m ()
span1 thePred str = do
  e <- Benchmark.span (\a -> if thePred a then Left a else Right a) str
  case e of
    Right r -> pure r
    Left (a, _) -> S.yield a
