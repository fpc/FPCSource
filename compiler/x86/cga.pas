{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Helper routines for the i386 code generator

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

unit cga;

{$i fpcdefs.inc}

interface

    uses
       globtype,
       cpuinfo,cpubase,cgbase,cgutils,
       symconst,symtype,symdef,aasmbase,aasmtai,aasmcpu;

    procedure emit_none(i : tasmop;s : topsize);

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
    procedure emit_ref(i : tasmop;s : topsize;const ref : treference);

    procedure emit_const_reg(i : tasmop;s : topsize;c : aint;reg : tregister);
    procedure emit_const_ref(i : tasmop;s : topsize;c : aint;const ref : treference);
    procedure emit_ref_reg(i : tasmop;s : topsize;const ref : treference;reg : tregister);
    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;const ref : treference);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);


    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);


implementation

    uses
       cutils,
       systems,verbose,
       cgobj;


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emit_none(i : tasmop;s : topsize);
      begin
         exprasmList.concat(Taicpu.Op_none(i,s));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_reg(i,s,reg));
      end;

    procedure emit_ref(i : tasmop;s : topsize;const ref : treference);
      begin
         exprasmList.concat(Taicpu.Op_ref(i,s,ref));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : aint;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_const_reg(i,s,c,reg));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : aint;const ref : treference);
      begin
         exprasmList.concat(Taicpu.Op_const_ref(i,s,c,ref));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;const ref : treference;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_ref_reg(i,s,ref,reg));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;const ref : treference);
      begin
         exprasmList.concat(Taicpu.Op_reg_ref(i,s,reg,ref));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    var instr:Taicpu;

    begin
      if not ((reg1=reg2) and (i=A_MOV)) then
        begin
          instr:=Taicpu.op_reg_reg(i,s,reg1,reg2);
          exprasmlist.concat(instr);
          if i=A_MOV then
            cg.add_move_instruction(instr);
        end;
    end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         exprasmList.concat(Taicpu.Op_const_reg_reg(i,s,c,reg1,reg2));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         exprasmList.concat(Taicpu.Op_reg_reg_reg(i,s,reg1,reg2,reg3));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);
      begin
        exprasmList.concat(Taicpu.Op_sym(i,s,op));
      end;

end.
{
  $Log$
  Revision 1.10  2004-10-31 21:45:04  peter
    * generic tlocation
    * move tlocation to cgutils

  Revision 1.9  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.8  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.7.2.3  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.7.2.2  2004/04/29 23:30:28  peter
    * fix i386 compiler

  Revision 1.7.2.1  2004/04/27 18:18:26  peter
    * aword -> aint

}
