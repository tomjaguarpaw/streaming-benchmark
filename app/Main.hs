{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Benchmark

main :: IO ()
main = Benchmark.benchmark Benchmark.full
