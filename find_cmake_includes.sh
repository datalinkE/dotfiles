#!/bin/bash

for ccc in $(find . -name CMakeLists.txt); do grep -nr include_directories $ccc | grep -o -P "\".*?\"" | sed s#\${CMAKE_CURRENT_SOURCE_DIR}#$(dirname $ccc)#; done | while read tt; do bash -c "realpath $tt"; done | sort | uniq

#for ccc in $(find . -name CMakeLists.txt); do echo $ccc; grep -nr include_directories $ccc | grep -o -P "\".*?\"" | sed s#\${CMAKE_CURRENT_SOURCE_DIR}#$(dirname $ccc)# | while read tt; do bash -c "realpath $tt" | sort | uniq; done; done
