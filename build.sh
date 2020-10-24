cd "$(dirname "$0")"

mkdir build 2>/dev/null

set -e
ocamlc -c src/main.ml -o build/main.cmo
ocamlc unix.cma build/main.cmo -o build/main

if [ "$1" = 'run' ]; then
    build/main
fi
