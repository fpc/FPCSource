{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86-64 assembler for type converting nodes

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
unit nx64cnv;

{$i fpcdefs.inc}

interface

    uses
      node,defutil,pass_1,
      nx86cnv;

    type
       tx8664typeconvnode = class(tx86typeconvnode)
         protected
         function first_nothing : tnode;override;
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
         { function first_int_to_real: tnode; override; }
         function first_int_to_real : tnode;override;
         procedure second_int_to_real(ctx:tpassgeneratecodecontext);override;
         { procedure second_real_to_real(ctx:tpassgeneratecodecontext);override; }
         { procedure second_cord_to_pointer(ctx:tpassgeneratecodecontext);override; }
         { procedure second_proc_to_procvar(ctx:tpassgeneratecodecontext);override; }
         { procedure second_bool_to_int(ctx:tpassgeneratecodecontext);override; }
         { procedure second_int_to_bool(ctx:tpassgeneratecodecontext);override; }
         { procedure second_load_smallset(ctx:tpassgeneratecodecontext);override;  }
         { procedure second_ansistring_to_pchar(ctx:tpassgeneratecodecontext);override; }
         { procedure second_pchar_to_string(ctx:tpassgeneratecodecontext);override; }
         { procedure second_class_to_intf(ctx:tpassgeneratecodecontext);override;  }
         { procedure second_char_to_char(ctx:tpassgeneratecodecontext);override; }
       end;


implementation

    uses
      verbose,globals,globtype,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      symconst,symdef,
      cgbase,cga,
      ncnv,
      cpubase,cpuinfo,
      cgutils,cgobj,nodehelper,cgx86,
      pass_2_context,
      compiler;


    function tx8664typeconvnode.first_nothing : tnode;
      begin
        result:=nil;
        { If typecasting between a Single or Double and a record of equal size,
          we can use MOVD and MOVQ }
        if (resultdef.typ in [recorddef,orddef]) and
          use_vectorfpu(left.resultdef) and
          (resultdef.size=left.resultdef.size) then
          expectloc:=LOC_MMREGISTER
        else if (left.resultdef.typ in [recorddef,orddef]) and
          use_vectorfpu(resultdef) and
          (resultdef.size=left.resultdef.size) then
          expectloc:=LOC_REGISTER
        else
          result:=inherited first_nothing;
      end;


    function tx8664typeconvnode.first_int_to_real : tnode;
      begin
        result:=nil;
        if use_vectorfpu(resultdef) and
           (torddef(left.resultdef).ordtype=u32bit) and
           not(FPUX86_HAS_AVX512F in fpu_capabilities[compiler.globals.current_settings.fputype]) then
          begin
            inserttypeconv(left,compiler.deftypes.s64inttype, compiler);
            firstpass(left);
          end
        else
          result:=inherited first_int_to_real;
       if use_vectorfpu(resultdef) then
         expectloc:=LOC_MMREGISTER;
      end;


    procedure tx8664typeconvnode.second_int_to_real(ctx:tpassgeneratecodecontext);
      var
         href : treference;
         l1,l2 : tasmlabel;
         op : tasmop;
      begin
        if use_vectorfpu(resultdef) and not(FPUX86_HAS_AVX512F in fpu_capabilities[compiler.globals.current_settings.fputype]) then
          begin
            if is_double(resultdef) then
              op:=A_CVTSI2SD
            else if is_single(resultdef) then
              op:=A_CVTSI2SS
            else
              internalerror(200506061);

            location_reset(location,LOC_MMREGISTER,def_cgsize(resultdef));
            location.register:=ctx.cg.getmmregister(ctx.CurrAsmList,location.size);

            case torddef(left.resultdef).ordtype of
              u64bit:
                begin
                   { unsigned 64 bit ints are harder to handle:
                     we load bits 0..62 and then check bit 63:
                     if it is 1 then we add $80000000 000000000
                     as double                                  }
                   ctx.CurrAsmList.AsmData.getdatalabel(l1);
                   ctx.CurrAsmList.AsmData.getjumplabel(l2);

                   { Get sign bit }
                   if not(left.location.loc in [LOC_REGISTER,LOC_REFERENCE]) then
                     ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
                   case left.location.loc of
                     LOC_REGISTER :
                       begin
                         ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                         emit_const_reg(ctx,A_BT,S_Q,63,left.location.register);
                         ctx.CurrAsmList.concat(taicpu.op_reg_reg(op,S_Q,left.location.register,location.register));
                       end;
                     LOC_REFERENCE :
                       begin
                         href:=left.location.reference;
                         tcgx86(ctx.cg).make_simple_ref(ctx.CurrAsmList,href);
                         inc(href.offset,4);
                         ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                         emit_const_ref(ctx,A_BT,S_L,31,href);
                         dec(href.offset,4);
                         ctx.CurrAsmList.concat(taicpu.op_ref_reg(op,S_Q,href,location.register));
                       end;
                     else
                       internalerror(200710181);
                   end;

                   ctx.cg.a_jmp_flags(ctx.CurrAsmList,F_NC,l2);
                   ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_DEFAULTFLAGS);
                   new_section(ctx.CurrAsmList.AsmData.asmlists[al_typedconsts],sec_rodata_norel,l1.name,compiler.globals.const_align(sizeof(pint)));
                   ctx.CurrAsmList.AsmData.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                   reference_reset_symbol(href,l1,0,4,[]);
                   { simplify for PIC }
                   tcgx86(ctx.cg).make_simple_ref(ctx.CurrAsmList,href);

                   { I got these constant from a test program (FK) }
                   if is_double(resultdef) then
                     begin
                       { double (2^64) }
                       ctx.CurrAsmList.AsmData.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(0));
                       ctx.CurrAsmList.AsmData.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($43f00000));
                       tcgx86(ctx.cg).make_simple_ref(ctx.CurrAsmList,href);
                       ctx.CurrAsmList.concat(taicpu.op_ref_reg(A_ADDSD,S_NO,href,location.register));
                     end
                   else if is_single(resultdef) then
                     begin
                       { single(2^64) }
                       ctx.CurrAsmList.AsmData.asmlists[al_typedconsts].concat(Tai_const.Create_32bit($5f800000));
                       ctx.CurrAsmList.concat(taicpu.op_ref_reg(A_ADDSS,S_NO,href,location.register));
                     end
                   else
                     internalerror(200506071);
                   ctx.cg.a_label(ctx.CurrAsmList,l2);
                end
              else
                inherited;
            end;
          end
        else
          inherited;
      end;


begin
   ctypeconvnode:=tx8664typeconvnode;
end.
