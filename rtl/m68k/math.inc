{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifdef FPC_INCLUDE_SOFTWARE_LONGWORD_TO_DOUBLE}
function fpc_longword_to_double(i: longword): double; compilerproc;
var
  l : longint;
begin
{$ifopt R+}
{$define HAS_OPTR}
{$endif}
{$r-}
  l:=longint(i);
{$ifdef HAS_OPTR}
{$r+}
{$endif}
  if l>=0 then
    fpc_longword_to_double:=double(l)
  else
    fpc_longword_to_double:=-double(abs(l));
end;
{$endif FPC_INCLUDE_SOFTWARE_LONGWORD_TO_DOUBLE}
