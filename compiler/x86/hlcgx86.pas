{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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

unit hlcgx86;

interface

{$i fpcdefs.inc}

  uses
    aasmdata,
    symtype,symdef,
    parabase,
    hlcgobj, hlcg2ll;

  type

    { thlcgx86 }

    thlcgx86 = class(thlcg2ll)
     protected
      procedure gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara); override;
    end;

implementation

  uses
    cgbase,
    cpubase,aasmcpu;

{ thlcgx86 }

  procedure thlcgx86.gen_load_uninitialized_function_result(list: TAsmList; pd: tprocdef; resdef: tdef; const resloc: tcgpara);
    begin
      { the caller will pop a value from the fpu stack }
      if assigned(resloc.location) and
         (resloc.location^.loc=LOC_FPUREGISTER) then
        list.concat(taicpu.op_none(A_FLDZ));
    end;

end.
