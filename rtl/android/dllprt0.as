#
#   This file is part of the Free Pascal run time library.
#   Copyright (c) 2018 by Yuriy Sydorov and other
#   members of the Free Pascal development team.
#
#   See the file COPYING.FPC, included in this distribution,
#   for details about the copyright.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY;without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#**********************************************************************}
#
# Shared library startup code for Free Pascal. Android target.
#

/* --------------------------------------------------------- */
  .section .init_array, "aw"

.ifdef CPU64
  .quad FPC_LIB_START_ANDROID
.else
  .long FPC_LIB_START_ANDROID
.endif
