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
      node,ncnv,ncgcnv,compilerbase;

    type
       tgenppctypeconvnode = class(tcgtypeconvnode)
         protected
         { procedure second_int_to_int(ctx:tpassgeneratecodecontext);override; }
         { procedure second_string_to_string(ctx:tpassgeneratecodecontext);override; }
         { procedure second_cstring_to_pchar(ctx:tpassgeneratecodecontext);override; }
         { procedure second_string_to_chararray(ctx:tpassgeneratecodecontext);override; }
         { procedure second_array_to_pointer(ctx:tpassgeneratecodecontext);override; }
         { function first_int_to_real: tnode; override; }
         { procedure second_pointer_to_array(ctx:tpassgeneratecodecontext);override; }
         { procedure second_chararray_to_string(ctx:tpassgeneratecodecontext);override; }
         { procedure second_char_to_string(ctx:tpassgeneratecodecontext);override; }
         { procedure second_int_to_real(ctx:tpassgeneratecodecontext);override; }
         { procedure second_real_to_real(ctx:tpassgeneratecodecontext);override; }
         { procedure second_cord_to_pointer(ctx:tpassgeneratecodecontext);override; }
         { procedure second_proc_to_procvar(ctx:tpassgeneratecodecontext);override; }
         { procedure second_bool_to_int(ctx:tpassgeneratecodecontext);override; }
          procedure second_int_to_bool(ctx:tpassgeneratecodecontext);override;
         { procedure second_load_smallset(ctx:tpassgeneratecodecontext);override;  }
         { procedure second_ansistring_to_pchar(ctx:tpassgeneratecodecontext);override; }
         { procedure second_pchar_to_string(ctx:tpassgeneratecodecontext);override; }
         { procedure second_class_to_intf(ctx:tpassgeneratecodecontext);override; }
         { procedure second_char_to_char(ctx:tpassgeneratecodecontext);override; }
       end;


implementation

   uses
      verbose,globtype,globals,systemstypes,systems,
      symconst,symdef,aasmbase,aasmtai,aasmdata,
      defutil,cutils,
      cgbase,cgutils,pass_1,pass_2,pass_2_context,
      ncgutil,procinfo,
      cpubase,aasmcpu,
      rgobj,tgobj,cgobj,nodehelper,compiler;


    procedure tgenppctypeconvnode.second_int_to_bool(ctx:tpassgeneratecodecontext);
      var
        hreg1,
        hreg2    : tregister;
{$ifndef cpu64bitalu}
        href     : treference;
{$endif not cpu64bitalu}
        resflags : tresflags;
        opsize   : tcgsize;
        hlabel   : tasmlabel;
        newsize  : tcgsize;
      begin
         secondpass(left,ctx);
         if compiler.verbose.codegenerror then
          exit;

         { Explicit typecasts from any ordinal type to a boolean type }
         { must not change the ordinal value                          }
         if (nf_explicit in flags) and
            not(left.location.loc in [LOC_FLAGS,LOC_JUMP]) then
           begin
              location_copy(location,left.location);
              newsize:=def_cgsize(resultdef);
              { change of size? change sign only if location is LOC_(C)REGISTER? Then we have to sign/zero-extend }
              if (tcgsize2size[newsize]>tcgsize2size[left.location.size]) or
                 ((newsize<>left.location.size) and (location.loc in [LOC_REGISTER,LOC_CREGISTER])) then
                ctx.hlcg.location_force_reg(ctx.CurrAsmList,location,left.resultdef,resultdef,true)
              else
                begin
                  location.size:=newsize;
                  if (compiler.target.info.endian = ENDIAN_BIG) and
                     (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                    begin
                      inc(location.reference.offset,TCGSize2Size[left.location.size]-TCGSize2Size[location.size]);
                      location.reference.alignment:=newalignment(location.reference.alignment,TCGSize2Size[left.location.size]-TCGSize2Size[location.size]);
                    end;
                end;
              exit;
           end;

         location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
         opsize := def_cgsize(left.resultdef);
{$ifndef cpu64bitalu}
         if (opsize in [OS_64,OS_S64]) then
           opsize:=OS_32;
{$endif not cpu64bitalu}

        if (left.location.loc in [LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF]) then
          ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,true);

         case left.location.loc of
            LOC_CREFERENCE,LOC_REFERENCE,LOC_REGISTER,LOC_CREGISTER :
              begin
                if left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  begin
                    hreg1:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
{$ifndef cpu64bitalu}
                    if left.location.size in [OS_64,OS_S64] then
                      begin
                        ctx.cg.a_load_ref_reg(ctx.CurrAsmList,OS_INT,OS_INT,left.location.reference,hreg1);
                        hreg2:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
                        href:=left.location.reference;
                        inc(href.offset,4);
                        ctx.cg.a_load_ref_reg(ctx.CurrAsmList,OS_INT,OS_INT,href,hreg2);
                        ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,OS_32,hreg1,hreg2,hreg1);
                      end
                    else
{$endif not cpu64bitalu}
                      ctx.cg.a_load_ref_reg(ctx.CurrAsmList,opsize,opsize,left.location.reference,hreg1);
                  end
                else
                  begin
                    hreg1:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
{$ifndef cpu64bitalu}
                     if left.location.size in [OS_64,OS_S64] then
                       begin
                          hreg1:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
                          ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,OS_32,left.location.register64.reghi,left.location.register64.reglo,hreg1);
                       end
                     else
{$endif not cpu64bitalu}
                       ctx.cg.a_load_reg_reg(ctx.CurrAsmList,opsize,opsize,left.location.register,hreg1);
                  end;
                hreg2 := ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);

                if not(is_cbool(resultdef)) then
                  begin
                    { hreg2:=hreg1-1; carry:=hreg1=0 }
                    ctx.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBIC,hreg2,hreg1,1));
                    { hreg1:=hreg1-hreg2+carry (= hreg1-(hreg1-1)-carry) }
                    ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBFE,hreg1,hreg2,hreg1));
                  end
                else
                  begin
                    { carry:=hreg1<>0 }
                    ctx.CurrAsmList.concat(taicpu.op_reg_reg_const(A_SUBFIC,hreg2,hreg1,0));
                    { hreg1:=hreg1-hreg1-carry }
                    ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(A_SUBFE,hreg1,hreg1,hreg1));
                  end;
              end;
            LOC_FLAGS :
              begin
                hreg1:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
                resflags:=left.location.resflags;
                ctx.cg.g_flags2reg(ctx.CurrAsmList,location.size,resflags,hreg1);
                if (is_cbool(resultdef)) then
                  ctx.cg.a_op_reg_reg(ctx.CurrAsmList,OP_NEG,location.size,hreg1,hreg1);
              end;
            LOC_JUMP :
              begin
                hreg1:=ctx.cg.getintregister(ctx.CurrAsmList,OS_INT);
                current_asmdata.getjumplabel(hlabel);
                ctx.cg.a_label(ctx.CurrAsmList,left.location.truelabel);
                if not(is_cbool(resultdef)) then
                  ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,1,hreg1)
                else
                  ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,-1,hreg1);
                ctx.cg.a_jmp_always(ctx.CurrAsmList,hlabel);
                ctx.cg.a_label(ctx.CurrAsmList,left.location.falselabel);
                ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,0,hreg1);
                ctx.cg.a_label(ctx.CurrAsmList,hlabel);
              end;
            else
              internalerror(10062);
         end;
{$ifndef cpu64bitalu}
         if (location.size in [OS_64,OS_S64]) then
           begin
             location.register64.reglo:=hreg1;
             location.register64.reghi:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
             if (is_cbool(resultdef)) then
               { reglo is either 0 or -1 -> reghi has to become the same }
               ctx.cg.a_load_reg_reg(ctx.CurrAsmList,OS_32,OS_32,location.register64.reglo,location.register64.reghi)
             else
               { unsigned }
               ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,location.register64.reghi);
           end
         else
{$endif cpu64bitalu}
           location.register:=hreg1;
      end;

end.
