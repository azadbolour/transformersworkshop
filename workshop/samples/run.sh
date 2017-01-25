#!/bin/sh

# Build and run sample code for transformer workshop.

echo "building"
stack build

echo "running stacking-samples"
stack exec stacking-samples

echo "running special-functions-samples"
stack exec special-functions-samples

echo "running flipped-reader-sample"
stack exec flipped-reader-sample <<EOF
input
EOF

