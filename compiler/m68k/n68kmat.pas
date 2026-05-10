{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate 680x0 assembler for math nodes

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
unit n68kmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,cpubase,cgbase,
      compilerbase;

    type


      tm68knotnode = class(tcgnotnode)
         procedure second_boolean(ctx:tpassgeneratecodecontext);override;
      end;

      tm68kmoddivnode = class(tcgmoddivnode)
      public
        function first_moddivint: tnode;override;
        procedure emit_div_reg_reg_reg(signed: boolean;denum,num,res : tregister;ctx:tpassgeneratecodecontext);override;
        procedure emit_mod_reg_reg_reg(signed: boolean;denum,num,res : tregister;ctx:tpassgeneratecodecontext);override;
      end;

      tm68kunaryminusnode = class(tcgunaryminusnode)
        procedure second_float(ctx:tpassgeneratecodecontext);override;
      end;

      tm68kshlshrnode = class(tshlshrnode)
         procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,symtable,aasmbase,aasmtai,aasmdata,aasmcpu,
      pass_1,pass_2,pass_2_context,procinfo,
      ncon,
      cpuinfo,paramgr,defutil,parabase,
      tgobj,ncgutil,cgobj,nodehelper,cgutils,rgobj,rgcpu,cgcpu,cg64f32,
      compiler;




{*****************************************************************************
                               TM68KNOTNODE
*****************************************************************************}

    procedure tm68knotnode.second_boolean(ctx:tpassgeneratecodecontext);
      var
        hreg: tregister;
        opsize : tcgsize;
      begin
        secondpass(left,ctx);
        if not handle_locjump then
          begin
            opsize:=def_cgsize(resultdef);

            if ((left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and needs_unaligned(left.location.reference.alignment,opsize)) then
              ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,resultdef,true);

            case left.location.loc of
              LOC_FLAGS :
                begin
                  location_copy(location,left.location);
                  inverse_flags(location.resflags);
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE:
                begin
                  tcg68k(ctx.cg).fixref(ctx.CurrAsmList,left.location.reference,false);
                  if is_64bit(resultdef) then
                   begin
                     hreg:=ctx.cg.GetIntRegister(ctx.CurrAsmList,OS_32);
                     ctx.cg.a_load_ref_reg(ctx.CurrAsmList,OS_32,OS_32,left.location.reference,hreg);
                     inc(left.location.reference.offset,4);
                     ctx.cg.a_op_ref_reg(ctx.CurrAsmList,OP_OR,OS_32,left.location.reference,hreg);
                   end
                 else
                   ctx.CurrAsmList.concat(taicpu.op_ref(A_TST,tcgsize2opsize[opsize],left.location.reference));
                   location_reset(location,LOC_FLAGS,OS_NO);
                   location.resflags:=F_E;
                end;
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_SUBSETREG,
              LOC_CSUBSETREG,
              LOC_SUBSETREF,
              LOC_CSUBSETREF:
                begin
                  if is_64bit(resultdef) then
                    begin
                      ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,resultdef,false);
                      ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_OR,S_L,left.location.register64.reghi,left.location.register64.reglo));
                    end
                  else
                    begin
                      ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,resultdef,true);
                      if (not (CPUM68K_HAS_TSTAREG in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype])) and isaddressregister(left.location.register) then
                        begin
                          hreg:=ctx.cg.getintregister(ctx.CurrAsmList,opsize);
                          ctx.cg.a_load_reg_reg(ctx.CurrAsmList,OS_ADDR,opsize,left.location.register,hreg);
                        end
                      else
                        hreg:=left.location.register;
                      ctx.CurrAsmList.concat(taicpu.op_reg(A_TST,tcgsize2opsize[opsize],hreg));
                    end;
                  location_reset(location,LOC_FLAGS,OS_NO);
                  location.resflags:=F_E;
                end;
            else
              internalerror(200203223);
            end;
          end;
      end;


{*****************************************************************************
                               TM68KMODDIVNODE
*****************************************************************************}

  function tm68kmoddivnode.first_moddivint: tnode;
    begin
      if CPUM68K_HAS_32BITDIV in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype] then
        result:=nil
      else
        result:=inherited first_moddivint;
    end;


  procedure tm68kmoddivnode.emit_div_reg_reg_reg(signed: boolean;denum,num,res : tregister;ctx:tpassgeneratecodecontext);
   const
     divudivs: array[boolean] of tasmop = (A_DIVU,A_DIVS);
   begin
     if CPUM68K_HAS_32BITDIV in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype] then
       begin
         ctx.cg.a_load_reg_reg(ctx.CurrAsmList,OS_INT,OS_INT,num,res);
         ctx.CurrAsmList.concat(taicpu.op_reg_reg(divudivs[signed],S_L,denum,res));
       end
     else
       InternalError(2014062801);
   end;


  procedure tm68kmoddivnode.emit_mod_reg_reg_reg(signed: boolean;denum,num,res : tregister;ctx:tpassgeneratecodecontext);
    const
      remop: array[boolean,boolean] of tasmop = ((A_DIVUL,A_DIVSL),(A_REMU,A_REMS));
    var
      tmpreg : tregister;
    begin
      if CPUM68K_HAS_32BITDIV in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype] then
        begin
          ctx.CurrAsmList.concat(taicpu.op_reg_reg_reg(
            remop[CPUM68K_HAS_REMSREMU in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype],signed],S_L,denum,res,num));
        end
      else
        InternalError(2014062802);
    end;


{*****************************************************************************
                          TM68KUNARYMINUSNODE
*****************************************************************************}

    procedure tm68kunaryminusnode.second_float(ctx:tpassgeneratecodecontext);
      var
        href: treference;
      begin
        secondpass(left,ctx);
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        //ctx.CurrAsmList.concat(tai_comment.create(strpnew('unaryminus second_float called!')));

        case left.location.loc of
          LOC_REFERENCE,
          LOC_CREFERENCE :
            begin
              location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);
              href:=left.location.reference;
              tcg68k(ctx.cg).fixref(ctx.CurrAsmList,href,compiler.globals.current_settings.fputype = fpu_coldfire);
              ctx.CurrAsmList.concat(taicpu.op_ref_reg(A_FNEG,tcgsize2opsize[left.location.size],href,location.register));
            end;
          LOC_FPUREGISTER:
            begin
              location.register:=left.location.register;
              ctx.CurrAsmList.concat(taicpu.op_reg(A_FNEG,fpuregopsize,location.register));
            end;
          LOC_CFPUREGISTER:
            begin
               location.register:=ctx.cg.getfpuregister(ctx.CurrAsmList,location.size);
               ctx.CurrAsmList.concat(taicpu.op_reg_reg(A_FNEG,fpuregopsize,left.location.register,location.register));
            end;
          else
            internalerror(2003060202);
        end;
      end;


{*****************************************************************************
                             TM68KSHLRSHRNODE
*****************************************************************************}

    function tm68kShlShrNode.first_shlshr64bitint:TNode;
      begin
        if is_64bit(left.resultdef) and not (right.nodetype=ordconstn) then
          { for 64bit shifts with anything but constants we use rtl helpers }
          result:=inherited
        else
          { 2nd pass is our friend }
          result := nil;
      end;

    procedure tm68kshlshrnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        hregister, hreg64hi, hreg64lo : tregister;
        op : topcg;
        shiftval: aint;
      begin
        secondpass(left,ctx);
        secondpass(right,ctx);
        if is_64bit(left.resultdef) then
          begin
            location_reset(location,LOC_REGISTER,OS_64);

            { load left operator in a register }
            ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,compiler.deftypes.u64inttype,false);
            hreg64hi:=left.location.register64.reghi;
            hreg64lo:=left.location.register64.reglo;

            shiftval := tordconstnode(right).value.svalue;
            shiftval := shiftval and 63;
            if shiftval > 31 then
              begin
                if nodetype = shln then
                  begin
                    ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,hreg64hi);
                    if (shiftval and 31) <> 0 then
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHL,OS_32,shiftval and 31,hreg64lo,hreg64lo);
                  end
                else
                  begin
                    ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_32,0,hreg64lo);
                    if (shiftval and 31) <> 0 then
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHR,OS_32,shiftval and 31,hreg64hi,hreg64hi);
                  end;
                location.register64.reglo:=hreg64hi;
                location.register64.reghi:=hreg64lo;
              end
            else
              if (shiftval = 1) and (CPUM68K_HAS_ROLROR in compiler.target.cpu_capabilities[compiler.globals.current_settings.cputype]) then
                begin
                  if nodetype = shln then
                    begin
                      ctx.CurrAsmList.concat(taicpu.op_const_reg(A_LSL,S_L,1,hreg64lo));
                      ctx.CurrAsmList.concat(taicpu.op_const_reg(A_ROXL,S_L,1,hreg64hi));
                    end
                  else
                    begin
                      ctx.CurrAsmList.concat(taicpu.op_const_reg(A_LSR,S_L,1,hreg64hi));
                      ctx.CurrAsmList.concat(taicpu.op_const_reg(A_ROXR,S_L,1,hreg64lo));
                    end;
                  location.register64.reghi:=hreg64hi;
                  location.register64.reglo:=hreg64lo;
                end
              else
                begin
                  hregister:=ctx.cg.getintregister(ctx.CurrAsmList,OS_32);
                  if nodetype = shln then
                    begin
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHR,OS_32,32-shiftval,hreg64lo,hregister);
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHL,OS_32,shiftval,hreg64hi,hreg64hi);
                      ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,OS_32,hregister,hreg64hi,hreg64hi);
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHL,OS_32,shiftval,hreg64lo,hreg64lo);
                    end
                  else
                    begin
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHL,OS_32,32-shiftval,hreg64hi,hregister);
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHR,OS_32,shiftval,hreg64lo,hreg64lo);
                      ctx.cg.a_op_reg_reg_reg(ctx.CurrAsmList,OP_OR,OS_32,hregister,hreg64lo,hreg64lo);
                      ctx.cg.a_op_const_reg_reg(ctx.CurrAsmList,OP_SHR,OS_32,shiftval,hreg64hi,hreg64hi);
                    end;
                  location.register64.reghi:=hreg64hi;
                  location.register64.reglo:=hreg64lo;
                end;
          end
        else
          begin
            { load left operators in a register }
            ctx.hlcg.location_force_reg(ctx.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
            location_copy(location,left.location);

            { determine operator }
            if nodetype=shln then
              op:=OP_SHL
            else
              op:=OP_SHR;
            { shifting by a constant directly coded: }
            if (right.nodetype=ordconstn) then
              begin
                if tordconstnode(right).value.svalue and 31<>0 then
                  ctx.cg.a_op_const_reg(ctx.CurrAsmList,op,OS_32,tordconstnode(right).value.svalue and 31,location.register)
              end
            else
              begin
                { load shift count in a register if necessary }
                ctx.hlcg.location_force_reg(ctx.CurrAsmList,right.location,right.resultdef,right.resultdef,true);
                ctx.cg.a_op_reg_reg(ctx.CurrAsmList,op,OS_32,right.location.register,location.register);
              end;
          end;
      end;



begin
   cnotnode:=tm68knotnode;
   cmoddivnode:=tm68kmoddivnode;
   cunaryminusnode:=tm68kunaryminusnode;
   cshlshrnode:=tm68kshlshrnode;
end.
