{
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
       symconst,symtype,symdef,aasmbase,aasmtai,aasmdata,aasmcpu;

    procedure emit_none(i : tasmop;s : topsize);

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
    procedure emit_ref(i : tasmop;s : topsize;ref : treference);

    procedure emit_const_reg(i : tasmop;s : topsize;c : aint;reg : tregister);
    procedure emit_const_ref(i : tasmop;s : topsize;c : aint;ref : treference);
    procedure emit_ref_reg(i : tasmop;s : topsize;ref : treference;reg : tregister);
    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : treference);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
    procedure emit_ref_reg_reg(i : tasmop;s : topsize;ref : treference;reg1,reg2 : tregister);


    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);


implementation

    uses
       cutils,
       systems,verbose,
       cgobj,cgx86;


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emit_none(i : tasmop;s : topsize);
      begin
         current_asmdata.CurrAsmList.concat(Taicpu.Op_none(i,s));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         current_asmdata.CurrAsmList.concat(Taicpu.Op_reg(i,s,reg));
      end;

    procedure emit_ref(i : tasmop;s : topsize;ref : treference);
      begin
        tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,ref);
        current_asmdata.CurrAsmList.concat(Taicpu.Op_ref(i,s,ref));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : aint;reg : tregister);
      begin
         current_asmdata.CurrAsmList.concat(Taicpu.Op_const_reg(i,s,c,reg));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : aint;ref : treference);
      begin
        tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,ref);
        current_asmdata.CurrAsmList.concat(Taicpu.Op_const_ref(i,s,c,ref));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;ref : treference;reg : tregister);
      begin
        tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,ref);
        current_asmdata.CurrAsmList.concat(Taicpu.Op_ref_reg(i,s,ref,reg));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : treference);
      begin
        tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,ref);
        current_asmdata.CurrAsmList.concat(Taicpu.Op_reg_ref(i,s,reg,ref));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    var instr:Taicpu;

    begin
      if not ((reg1=reg2) and (i=A_MOV)) then
        begin
          instr:=Taicpu.op_reg_reg(i,s,reg1,reg2);
          current_asmdata.CurrAsmList.concat(instr);
          if i=A_MOV then
            cg.add_move_instruction(instr);
        end;
    end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         current_asmdata.CurrAsmList.concat(Taicpu.Op_const_reg_reg(i,s,c,reg1,reg2));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         current_asmdata.CurrAsmList.concat(Taicpu.Op_reg_reg_reg(i,s,reg1,reg2,reg3));
      end;

    procedure emit_ref_reg_reg(i : tasmop;s : topsize;ref : treference;reg1,reg2 : tregister);
      begin
        tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,ref);
        current_asmdata.CurrAsmList.concat(Taicpu.Op_ref_reg_reg(i,s,ref,reg1,reg2));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);
      begin
        current_asmdata.CurrAsmList.concat(Taicpu.Op_sym(i,s,op));
      end;

end.
