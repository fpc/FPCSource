{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_UNIT_HAS_STRLEN}
function strlen(p : pchar) : longint;assembler;
{$i strlen.inc}

{
  $Log$
  Revision 1.1  2003-04-30 16:36:39  florian
    + support for generic pchar routines added
    + some basic rtl stuff for x86-64 added
}