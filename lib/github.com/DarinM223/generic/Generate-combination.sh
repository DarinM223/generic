#!/bin/sh

# Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

source=$(realpath $1)
target=$(realpath $2)

MLTON_LIB="$(cd $(dirname $0)/../../../.. && pwd)"

echo "(* WARNING: This file was generated by running:
 *
 *> $(basename $0) $1 $2
 *)" > "$target"

cd $(dirname $source)

grep -e '.*/with/.*\.sml' "$source"         \
 | sed -e 's#\$(MLTON_LIB)#'$MLTON_LIB'#g'  \
 | xargs cat                                \
 >> "$target"