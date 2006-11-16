{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate PowerPC assembler for type converting nodes

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
unit ngppccnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncnv,ncgcnv;

    type
       tgenppctypeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { function first_int_to_real: tnode; override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
         { procedure second_int_to_real;override; }
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
          procedure second_int_to_bool;override;
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override; }
         { procedure second_char_to_char;override; }
       end;


implementation

   uses
      verbose,globtype,globals,systems,
      symconst,symdef,aasmbase,aasmtai,aasmdata,
      defutil,
      cgbase,cgutils,pass_1,pass_2,
      ncgutil,procinfo,
      cpubase,aasmcpu,
      rgobj,tgobj,cgobj;


    procedure tgenppctypeconvnode.second_int_to_bool;
      var
        hreg1,
        hreg2    : tregister;
{$ifndef cpu64bit}
        href     : treference;
{$endif not cpu64bit}
        resflags : tresflags;
        opsize   : tcgsize;
        hlabel, oldTrueLabel, oldFalseLabel : tasmlabel;
      begin
         oldTrueLabel:=current_procinfo.CurrTrueLabel;
         oldFalseLabel:=current_procinfo.CurrFalseLabel;
         current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
         current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
         secondpass(left);
         if codegenerror then
          exit;

         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if (nf_explicit in flags) and
            (left.resultdef.size=resultdef.size) and
            (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
           begin
              current_procinfo.CurrTrueLabel:=oldTrueLabel;
              current_procinfo.CurrFalseLabel:=oldFalseLabel;
              location_copy(location,left.location);
              exit;
           end;

         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
         opsize := def_cgsize(left.resultdef);
         case left.location.loc of
            LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER :
              begin
                if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  begin
                    hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
{$ifndef cpu64bit}
                    if left.location.size in [OS_64,OS_S64] then
                      begin
                        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,left.location.reference,hreg1);
                        hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                        href:=left.location.reference;
                        inc(href.offset,4);
                        cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,href,hreg2);
                        cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,hreg1,hreg2,hreg1);
                      end
                    else
{$endif not cpu64bit}
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.reference,hreg1);
                  end
                else
                  begin
{$ifndef cpu64bit}
                     if left.location.size in [OS_64,OS_S64] then
                       begin
                          hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                          cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_OR,OS_32,left.location.register64.reghi,left.location.register64.reglo,hreg1);
                       end
                     else
{$endif not cpu64bit}
                       hreg1 := left.location.register;
                  end;
                hreg2 := cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBIC,hreg2,hreg1,1));
                current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBFE,hreg1,hreg2,hreg1));
              end;
            LOC_FLAGS :
              begin
                hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                resflags:=left.location.resflags;
                cg.g_flags2reg(current_asmdata.CurrAsmList,location.size,resflags,hreg1);
              end;
            LOC_JUMP :
              begin
                hreg1:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                current_asmdata.getjumplabel(hlabel);
                cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,1,hreg1);
                cg.a_jmp_always(current_asmdata.CurrAsmList,hlabel);
                cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
                cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_INT,0,hreg1);
                cg.a_label(current_asmdata.CurrAsmList,hlabel);
              end;
            else
              internalerror(10062);
         end;
         location.register := hreg1;
         current_procinfo.CurrTrueLabel:=oldTrueLabel;
         current_procinfo.CurrFalseLabel:=oldFalseLabel;
      end;

end.