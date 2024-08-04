{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_UNIT_HAS_STRPCOPY}
  function strpcopy(d : PAnsiChar;const s : shortstring) : PAnsiChar;
  var
    n: SizeInt;
  Begin
    n:=length(s);
    Move(s[1], d^, n);
    d[n]:=#0;
    StrPCopy:=D;
  end;
{$endif FPC_UNIT_HAS_STRPCOPY}

