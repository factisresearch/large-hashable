#!/bin/bash

cd "$(dirname "$0")"

if type stack >/dev/null 2>&1
then
    echo "Compiling ..."
    stack build || exit 1
    echo "Done compiling"
    exe=$(stack exec -- which large-hashable-benchmark)
else
    exe=./dist/build/large-hashable-benchmark/large-hashable-benchmark
    echo "Not compiling, using executable $exe"
fi

function run()
{
    echo
    echo "Running benchmark $1"
    $exe "$1" +RTS -s 2>&1 | egrep 'total memory in use|Total   time|Productivity|bytes allocated in the heap'
    if ! test ${PIPESTATUS[0]} -eq 0; then
        echo "Benchmark $1 failed!"
        exit 1
    fi
}

run dry
run safecopy
run cryptohash
run large-hashable-serial
run large-hashable
