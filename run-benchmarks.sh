#!/bin/bash

cd "$(dirname "$0")"

if type stack >/dev/null 2>&1
then
	stack build || exit 1
	exe=$(stack exec -- which large-hashable-benchmark)
else
	exe=./dist/build/large-hashable-benchmark/large-hashable-benchmark
fi

echo "Dry"
$exe dry +RTS -s || exit 1

echo "safecopy"
$exe safecopy +RTS -s || exit 1

echo "Cryptohash"
$exe cryptohash +RTS -s || exit 1

echo "LargeHashable"
$exe large-hashable +RTS -s || exit 1

echo "LargeHashable with Serial"
$exe serial +RTS -s || exit 1
