#!/bin/bash

stack build || exit 1

exe=$(stack exec -- which large-hashable-benchmark)

echo "Dry"
$exe dry +RTS -s || exit 1

echo "safecopy"
$exe safecopy +RTS -s || exit 1

echo "Cryptohash"
$exe cryptohash +RTS -s || exit 1

echo "LargeHashable"
$exe large-hashable +RTS -s || exit 1
