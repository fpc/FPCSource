{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$include xbios.inc}
{$include gemdos.inc}

procedure Error2InOutRes(errno: longint);
begin
  if (errno <= -2) and (errno >= -11) then
    InOutRes:=150-errno
  else
    begin
      case errno of
        -32 : InOutRes:=1;
        -33 : InOutRes:=2;
        -34 : InOutRes:=3;
        -35 : InOutRes:=4;
        -36 : InOutRes:=5;
        -37 : InOutRes:=8;
        -39 : InOutRes:=8;
        -40 : InOutRes:=9;
        -46 : InOutRes:=15;
        -67..-64 : InOutRes:=153;
        -15 : InOutRes:=151;
        -13 : InOutRes:=150;
      else
        InOutRes := word(errno);
      end;
    end;
end;
