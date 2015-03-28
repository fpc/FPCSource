{
    Copyright (c) 2013 by Jonas Maebe

    This unit implements some LLVM symbol helper routines.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

{$i fpcdefs.inc}

unit llvmsym;

interface

  uses
    globtype,
    symbase,symtype,symsym;

  function llvmparaname(sym: tparavarsym; paralocnr: longint): TSymStr;


implementation

  uses
      cutils,
      symconst;

  function llvmparaname(sym: tparavarsym; paralocnr: longint): TSymStr;
    begin
      result:='p.'+sym.realname;
      { use the same convention as llvm-gcc and clang: if an aggregate parameter
        is split into multiple locations, suffix each part with '.coerce#' }
      if assigned(sym.paraloc[calleeside].location^.next) then
        result:=result+'.coerce'+tostr(paralocnr);
    end;



end.

