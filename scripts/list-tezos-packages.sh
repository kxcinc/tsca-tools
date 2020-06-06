#! /bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")/vendors/tezos"

opams=$(find "$src_dir/vendors" "$src_dir/src" -name \*.opam -print)

packages=
for opam in $opams; do
    dir=$(dirname $opam)
    file=$(basename $opam)
    package=${file%.opam}
    packages="$packages $package"
done

echo "$packages"

