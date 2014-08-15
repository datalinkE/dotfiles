#!/bin/bash

directory=$PWD
while [ !  $directory == "/" ]; do
    echo "Looking for Makefile in $directory"
    if [ -e $directory"/Makefile" ]; then
        echo "Ok, continue build here..."
        make -C $directory -j
        exit 0
    fi
    directory="$(dirname "$directory")"
done
echo "Could not find Makefile"
exit 1
