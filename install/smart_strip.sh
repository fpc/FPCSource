#!/usr/bin/env bash
# Wrapper for the strip command
#
# The redhat rpm scripts try to strip files, that can't be stripped.
# This wrapper simply skips some files.

set -e
#set -x

Params=$@
# The last parameter is the file
for p in $Params; do
  File=$p
done
echo $File | grep -q '\bpalmos\b' || strip $Params

# end.

