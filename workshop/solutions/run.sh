#!/bin/sh

# Build and run selected solutions for monad transformer workshop exercises.

echo "building"
stack build

echo "running calc-discount-solution"
stack exec calc-discount-solution

echo "running basic-evaluator"
stack exec basic-evaluator

echo "running monadic-evaluator"
stack exec monadic-evaluator

echo "running graceful-evaluator"
stack exec graceful-evaluator

echo "running environment-dependent-evaluator"
stack exec environment-dependent-evaluator

echo "running profiling-evaluator"
stack exec profiling-evaluator

echo "running logging-evaluator"
stack exec logging-evaluator

echo "running lifting-logging-evaluator"
stack exec lifting-logging-evaluator

echo "running io-evaluator"
stack exec io-evaluator


