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
# Program startup code for Free Pascal. Android target.
#

/* --------------------------------------------------------- */
.data
/* Define a symbol for the first piece of initialized data.  */
  .globl __data_start
__data_start:
  .long 0
  .weak data_start
  data_start = __data_start

/* --------------------------------------------------------- */
  .section .init_array, "aw"

.ifdef CPU64
  .quad FPC_PROG_START_ANDROID
.else
  .long FPC_PROG_START_ANDROID
.endif
