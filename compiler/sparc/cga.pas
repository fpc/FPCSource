{******************************************************************************
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

 *****************************************************************************}
unit cga;
{$INCLUDE fpcdefs.inc}
interface
uses
  cpuinfo,cpubase,cginfo,
  symconst,symtype,symdef,aasmbase,aasmtai,aasmcpu;
function def_opsize(p1:tdef):topsize;
function def_getreg(p1:tdef):tregister;
procedure emitjmp(c:tasmcond;var l:tasmlabel);
implementation
uses
  cutils,
  systems,globals,verbose,
  cgbase,cgobj,tgobj,rgobj,rgcpu;
{*****************************************************************************
                                Helpers
*****************************************************************************}
function def_opsize(p1:tdef):topsize;
  begin
    case p1.size of
      1:def_opsize:=S_B;
      2:def_opsize:=S_W;
      4:def_opsize:=S_L;
      8:def_opsize:=S_L;
      else
        InternalError(130820001);
    end;
  end;
function def_getreg(p1:tdef):tregister;
  begin
    with rg do
      def_getreg:=makeregsize(getregisterint(exprasmlist),int_cgsize(p1.size));
  end;
{*****************************************************************************
                              Emit Assembler
*****************************************************************************}
procedure emitjmp(c:tasmcond;var l:tasmlabel);
  var
    ai:taicpu;
  begin
    if c=C_None
    then
      ai:= Taicpu.Op_sym(A_JMPL,S_NO,l)
    else
      begin
        ai:=Taicpu.Op_sym(A_JMPL,S_NO,l);
        ai.SetCondition(c);
      end;
    ai.is_jmp:=true;
    exprasmList.concat(ai);
  end;
end.
{
  $Log$
  Revision 1.6  2002-12-25 20:59:49  mazen
  - many emitXXX removed from cga.pas in order to remove that file.

  Revision 1.5  2002/11/10 19:07:46  mazen
  * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

  Revision 1.4  2002/11/06 11:31:24  mazen
  * op_reg_reg_reg don't need any more a TOpSize parameter

  Revision 1.3  2002/10/22 13:43:01  mazen
  - cga.pas redueced to an empty unit

  Revision 1.2  2002/10/13 19:47:34  mazen
  - logs removed

  Revision 1.1  2002/08/22 08:30:50  mazen
  first insertion 2002\08\22
}
