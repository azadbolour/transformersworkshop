#!/bin/sh

# Build and run sample code for the blender abstraction motivating monad transformers.

echo "building"
stack build

echo "running monad-blender-sample"
stack exec monad-blender-sample

echo "running reader-blender"
stack exec reader-blender

