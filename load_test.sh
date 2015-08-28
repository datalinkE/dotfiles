#!/bin/sh
for tt in {1..10000}; do echo $tt; done | parallel -j5 --gnu ./single.sh {}
