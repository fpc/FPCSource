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
{$define TESTGETTEMP to store const that are written into temps for later release PM }
function def_opsize(p1:tdef):topsize;
function def_getreg(p1:tdef):tregister;
procedure emitjmp(c:tasmcond;var l:tasmlabel);
procedure emit_none(i:tasmop;s:topsize);
procedure emit_const(i:tasmop;s:topsize;c:longint);
procedure emit_reg(i:tasmop;s:topsize;reg:tregister);
procedure emit_ref(i:tasmop;s:topsize;const ref:treference);
procedure emit_const_reg(i:tasmop;s:topsize;c:longint;reg:tregister);
procedure emit_const_ref(i:tasmop;s:topsize;c:longint;const ref:treference);
procedure emit_ref_reg(i:tasmop;s:topsize;const ref:treference;reg:tregister);
procedure emit_reg_ref(i:tasmop;s:topsize;reg:tregister;const ref:treference);
procedure emit_reg_reg(i:tasmop;s:topsize;reg1,reg2:tregister);
procedure emit_const_reg_reg(i:tasmop;s:topsize;c:longint;reg1,reg2:tregister);
procedure emit_reg_reg_reg(i:tasmop;s:topsize;reg1,reg2,reg3:tregister);
procedure emit_sym(i:tasmop;s:topsize;op:tasmsymbol);
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
procedure emit_none(i:tasmop;s:topsize);
  begin
    exprasmList.concat(Taicpu.Op_none(i,s));
  end;
procedure emit_reg(i:tasmop;s:topsize;reg:tregister);
  begin
    exprasmList.concat(Taicpu.Op_reg(i,s,reg));
  end;
procedure emit_ref(i:tasmop;s:topsize;const ref:treference);
  begin
    exprasmList.concat(Taicpu.Op_ref(i,s,ref));
  end;
procedure emit_const(i:tasmop;s:topsize;c:longint);
  begin
    exprasmList.concat(Taicpu.Op_const(i,s,aword(c)));
  end;
procedure emit_const_reg(i:tasmop;s:topsize;c:longint;reg:tregister);
  begin
    exprasmList.concat(Taicpu.Op_const_reg(i,s,aword(c),reg));
  end;
procedure emit_const_ref(i:tasmop;s:topsize;c:longint;const ref:treference);
  begin
    exprasmList.concat(Taicpu.Op_const_ref(i,s,aword(c),ref));
  end;
procedure emit_ref_reg(i:tasmop;s:topsize;const ref:treference;reg:tregister);
  begin
    exprasmList.concat(Taicpu.Op_ref_reg(i,s,ref,reg));
  end;
procedure emit_reg_ref(i:tasmop;s:topsize;reg:tregister;const ref:treference);
  begin
    exprasmList.concat(Taicpu.Op_reg_ref(i,s,reg,ref));
  end;
procedure emit_reg_reg(i:tasmop;s:topsize;reg1,reg2:tregister);
  begin
    if reg1<>reg2
    then
      exprasmList.concat(Taicpu.Op_reg_reg(i,s,reg1,reg2));
  end;
procedure emit_const_reg_reg(i:tasmop;s:topsize;c:longint;reg1,reg2:tregister);
  begin
    exprasmList.concat(Taicpu.Op_reg_const_reg(i,s,reg1,c,reg2));
  end;
procedure emit_reg_reg_reg(i:tasmop;s:topsize;reg1,reg2,reg3:tregister);
  begin
    exprasmList.concat(Taicpu.Op_reg_reg_reg(i,s,reg1,reg2,reg3));
  end;
procedure emit_sym(i:tasmop;s:topsize;op:tasmsymbol);
  begin
    exprasmList.concat(Taicpu.Op_sym(i,s,op));
  end;
end.
{
  $Log$
  Revision 1.2  2002-10-13 19:47:34  mazen
  - logs removed

  Revision 1.1  2002/08/22 08:30:50  mazen
  first insertion 2002\08\22
}
