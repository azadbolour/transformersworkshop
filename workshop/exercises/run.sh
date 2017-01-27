#!/bin/sh

# Build and run the skeletal starting points for selected workshop exercises.

echo "building"
stack build

echo "running calc-discount"
stack exec calc-discount

echo "running exercises-basic-evaluator"
stack exec exercises-basic-evaluator
