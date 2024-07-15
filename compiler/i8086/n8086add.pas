{
    Copyright (c) 2000-2002 by Florian Klaempfl

    Code generation for add nodes on the i8086

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
unit n8086add;

{$i fpcdefs.inc}

interface

    uses
       node,nadd,cpubase,nx86add;

    type

       { ti8086addnode }

       ti8086addnode = class(tx86addnode)
         function simplify(forinline: boolean) : tnode;override;
         function use_generic_mul32to64: boolean; override;
         function first_addpointer: tnode; override;
         function first_addhugepointer: tnode;
         function first_cmppointer: tnode; override;
         function first_cmphugepointer: tnode;
         function first_cmpfarpointer: tnode;
         procedure second_addordinal; override;
         procedure second_add64bit;override;
         procedure second_addfarpointer;
         procedure second_cmp64bit;override;
         procedure second_cmp32bit;
         procedure second_cmpfarpointer;
         procedure second_cmpordinal;override;
         procedure second_mul(unsigned: boolean);
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,constexp,pass_1,
      symconst,symdef,symtype,symcpu,paramgr,defutil,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      nutils,cgbase,procinfo,
      ncal,ncon,nset,cgutils,tgobj,
      cga,ncgutil,cgobj,cg64f32,cgx86,
      hlcgobj;

{*****************************************************************************
                                simplify
*****************************************************************************}

    function ti8086addnode.simplify(forinline: boolean): tnode;
      var
        t    : tnode;
        lt,rt: tnodetype;
        rd,ld: tdef;
        rv,lv,v: tconstexprint;
      begin
        { load easier access variables }
        rd:=right.resultdef;
        ld:=left.resultdef;
        rt:=right.nodetype;
        lt:=left.nodetype;

        if (
            (lt = pointerconstn) and is_farpointer(ld) and
            is_constintnode(right) and
            (nodetype in [addn,subn])
           ) or
           (
            (rt = pointerconstn) and is_farpointer(rd) and
            is_constintnode(left) and
            (nodetype=addn)
           ) or
           (
            (lt in [pointerconstn,niln]) and is_farpointer(ld) and
            (rt in [pointerconstn,niln]) and is_farpointer(rd) and
            (nodetype in [ltn,lten,gtn,gten,equaln,unequaln])
           ) then
          begin
            t:=nil;

            { load values }
            lv:=get_int_value(left);
            rv:=get_int_value(right);

            case nodetype of
              addn:
                begin
                  v:=lv+rv;
                  if lt=pointerconstn then
                    t := cpointerconstnode.create((qword(lv) and $FFFF0000) or word(qword(v)),resultdef)
                  else if rt=pointerconstn then
                    t := cpointerconstnode.create((qword(rv) and $FFFF0000) or word(qword(v)),resultdef)
                  else
                    internalerror(2014040604);
                end;
              subn:
                begin
                  v:=lv-rv;
                  if (lt=pointerconstn) then
                    { pointer-pointer results in an integer }
                    if (rt=pointerconstn) then
                      begin
                        if not(anf_has_pointerdiv in addnodeflags) then
                          internalerror(2008030102);
                        { todo: implement pointer-pointer as well }
                        internalerror(2014040607);
                        //t := cpointerconstnode.create(qword(v),resultdef);
                      end
                    else
                      t := cpointerconstnode.create((qword(lv) and $FFFF0000) or word(qword(v)),resultdef)
                  else
                    internalerror(2014040606);
                end;
              ltn:
                t:=cordconstnode.create(ord(word(qword(lv))<word(qword(rv))),pasbool1type,true);
              lten:
                t:=cordconstnode.create(ord(word(qword(lv))<=word(qword(rv))),pasbool1type,true);
              gtn:
                t:=cordconstnode.create(ord(word(qword(lv))>word(qword(rv))),pasbool1type,true);
              gten:
                t:=cordconstnode.create(ord(word(qword(lv))>=word(qword(rv))),pasbool1type,true);
              equaln:
                t:=cordconstnode.create(ord(lv=rv),pasbool1type,true);
              unequaln:
                t:=cordconstnode.create(ord(lv<>rv),pasbool1type,true);
              else
                internalerror(2014040605);
            end;
            result:=t;
            exit;
          end
        else
          Result:=inherited simplify(forinline);
      end;

{*****************************************************************************
                                use_generic_mul32to64
*****************************************************************************}

    function ti8086addnode.use_generic_mul32to64: boolean;
    begin
      result := True;
    end;

    { handles all multiplications }
    procedure ti8086addnode.second_addordinal;
    var
      unsigned: boolean;
    begin
      unsigned:=not(is_signed(left.resultdef)) or
                not(is_signed(right.resultdef));
      if nodetype=muln then
        second_mul(unsigned)
      else if is_farpointer(left.resultdef) xor is_farpointer(right.resultdef) then
        second_addfarpointer
      else
        inherited second_addordinal;
    end;

{*****************************************************************************
                                Add64bit
*****************************************************************************}

    procedure ti8086addnode.second_add64bit;
      var
        op         : TOpCG;
        op1,op2    : TAsmOp;
        hregister,
        hregister2 : tregister;
        hl4        : tasmlabel;
        mboverflow,
        unsigned:boolean;
        r:Tregister;
      begin
        pass_left_right;

        op1:=A_NONE;
        op2:=A_NONE;
        mboverflow:=false;
        unsigned:=((left.resultdef.typ=orddef) and
                   (torddef(left.resultdef).ordtype=u64bit)) or
                  ((right.resultdef.typ=orddef) and
                   (torddef(right.resultdef).ordtype=u64bit));
        case nodetype of
          addn :
            begin
              op:=OP_ADD;
              mboverflow:=true;
            end;
          subn :
            begin
              op:=OP_SUB;
              op1:=A_SUB;
              op2:=A_SBB;
              mboverflow:=true;
            end;
          xorn:
            op:=OP_XOR;
          orn:
            op:=OP_OR;
          andn:
            op:=OP_AND;
          else
            begin
              { everything should be handled in pass_1 (JM) }
              internalerror(2001090503);
            end;
        end;

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
              hregister2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
              cg64.a_load64_loc_reg(current_asmdata.CurrAsmList,left.location,joinreg64(hregister,hregister2));
              location_reset(left.location,LOC_REGISTER,left.location.size);
              left.location.register64.reglo:=hregister;
              left.location.register64.reghi:=hregister2;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end;
         end;

        if mboverflow and needoverflowcheck then
          cg.a_reg_alloc(current_asmdata.CurrAsmList, NR_DEFAULTFLAGS);

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           { when swapped another result register }
           if (nodetype=subn) and (nf_swapped in flags) then
            begin
              cg64.a_op64_reg_reg(current_asmdata.CurrAsmList,op,location.size,
                left.location.register64,
                right.location.register64);
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end
           else
            begin
              cg64.a_op64_reg_reg(current_asmdata.CurrAsmList,op,location.size,
                right.location.register64,
                left.location.register64);
            end;
         end
        else
         begin
           { right.location<>LOC_REGISTER }
           if (nodetype=subn) and (nf_swapped in flags) then
            begin
              r:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
              cg64.a_load64low_loc_reg(current_asmdata.CurrAsmList,right.location,r);
              emit_reg_reg(op1,S_W,left.location.register64.reglo,r);
              emit_reg_reg(op2,S_W,cg.GetNextReg(left.location.register64.reglo),cg.GetNextReg(r));
              emit_reg_reg(A_MOV,S_W,r,left.location.register64.reglo);
              emit_reg_reg(A_MOV,S_W,cg.GetNextReg(r),cg.GetNextReg(left.location.register64.reglo));
              cg64.a_load64high_loc_reg(current_asmdata.CurrAsmList,right.location,r);
              { the carry flag is still ok }
              emit_reg_reg(op2,S_W,left.location.register64.reghi,r);
              emit_reg_reg(op2,S_W,cg.GetNextReg(left.location.register64.reghi),cg.GetNextReg(r));
              emit_reg_reg(A_MOV,S_W,r,left.location.register64.reghi);
              emit_reg_reg(A_MOV,S_W,cg.GetNextReg(r),cg.GetNextReg(left.location.register64.reghi));
            end
           else
            begin
              cg64.a_op64_loc_reg(current_asmdata.CurrAsmList,op,location.size,right.location,
                left.location.register64);
            end;
          location_freetemp(current_asmdata.CurrAsmList,right.location);
         end;

        { only in case of overflow operations }
        { produce overflow code }
        { we must put it here directly, because sign of operation }
        { is in unsigned VAR!!                              }
        if mboverflow then
         begin
           if needoverflowcheck then
            begin
              current_asmdata.getjumplabel(hl4);
              if unsigned then
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_AE,hl4)
              else
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NO,hl4);
              cg.a_reg_dealloc(current_asmdata.CurrAsmList, NR_DEFAULTFLAGS);
              cg.a_call_name(current_asmdata.CurrAsmList,'FPC_OVERFLOW',false);
              cg.a_label(current_asmdata.CurrAsmList,hl4);
            end;
         end;

        location_copy(location,left.location);
      end;


    function ti8086addnode.first_addpointer: tnode;
      begin
        if is_hugepointer(left.resultdef) or is_hugepointer(right.resultdef) then
          result:=first_addhugepointer
        else
          result:=inherited;
      end;


    function ti8086addnode.first_addhugepointer: tnode;
      var
        procname:string;
      begin
        result:=nil;

        if (nodetype=subn) and is_hugepointer(left.resultdef) and is_hugepointer(right.resultdef) then
          procname:='fpc_hugeptr_sub_hugeptr'
        else
          begin
            case nodetype of
              addn:
                procname:='fpc_hugeptr_add_longint';
              subn:
                procname:='fpc_hugeptr_sub_longint';
              else
                internalerror(2014070301);
            end;

            if cs_hugeptr_arithmetic_normalization in current_settings.localswitches then
              procname:=procname+'_normalized';
          end;

        if is_hugepointer(left.resultdef) then
          result := ccallnode.createintern(procname,
            ccallparanode.create(right,
            ccallparanode.create(left,nil)))
        else
          result := ccallnode.createintern(procname,
            ccallparanode.create(left,
            ccallparanode.create(right,nil)));
        left := nil;
        right := nil;
        firstpass(result);
      end;


    function ti8086addnode.first_cmppointer: tnode;
      begin
        if is_hugepointer(left.resultdef) or is_hugepointer(right.resultdef) then
          result:=first_cmphugepointer
        else if is_farpointer(left.resultdef) or is_farpointer(right.resultdef) then
          result:=first_cmpfarpointer
        else
          result:=inherited;
      end;


    function ti8086addnode.first_cmphugepointer: tnode;
      var
        procname:string;
      begin
        result:=nil;

        if not (cs_hugeptr_comparison_normalization in current_settings.localswitches) then
          begin
            expectloc:=LOC_JUMP;
            exit;
          end;

        case nodetype of
          equaln:
            procname:='fpc_hugeptr_cmp_normalized_e';
          unequaln:
            procname:='fpc_hugeptr_cmp_normalized_ne';
          ltn:
            procname:='fpc_hugeptr_cmp_normalized_b';
          lten:
            procname:='fpc_hugeptr_cmp_normalized_be';
          gtn:
            procname:='fpc_hugeptr_cmp_normalized_a';
          gten:
            procname:='fpc_hugeptr_cmp_normalized_ae';
          else
            internalerror(2014070401);
        end;

        result := ccallnode.createintern(procname,
          ccallparanode.create(right,
          ccallparanode.create(left,nil)));
        left := nil;
        right := nil;
        firstpass(result);
      end;


    function ti8086addnode.first_cmpfarpointer: tnode;
      begin
        { = and <> are handled as a 32-bit comparison }
        if nodetype in [equaln,unequaln] then
          begin
            result:=nil;
            expectloc:=LOC_JUMP;
          end
        else
          begin
            result:=nil;
            expectloc:=LOC_FLAGS;
          end;
      end;


    procedure ti8086addnode.second_addfarpointer;
      var
        tmpreg : tregister;
        pointernode: tnode;
      begin
        pass_left_right;
        force_reg_left_right(true,true);
        set_result_location_reg;

        if (left.resultdef.typ=pointerdef) and (right.resultdef.typ<>pointerdef) then
          pointernode:=left
        else if (left.resultdef.typ<>pointerdef) and (right.resultdef.typ=pointerdef) then
          pointernode:=right
        else
          internalerror(2014040608);

        if not (nodetype in [addn,subn]) then
          internalerror(2014040602);

        if nodetype=addn then
          begin
            if (right.location.loc<>LOC_CONSTANT) then
              begin
                cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_16,
                   left.location.register,right.location.register,location.register);
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,
                   cg.GetNextReg(pointernode.location.register),cg.GetNextReg(location.register));
              end
            else
              begin
                if pointernode=left then
                  begin
                    { farptr_reg + int_const }
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_16,
                       right.location.value,left.location.register,location.register);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,
                       cg.GetNextReg(left.location.register),cg.GetNextReg(location.register));
                  end
                else
                  begin
                    { int_reg + farptr_const }
                    tmpreg:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
                    hlcg.a_load_const_reg(current_asmdata.CurrAsmList,resultdef,
                      right.location.value,tmpreg);
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_16,
                      left.location.register,tmpreg,location.register);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,
                       cg.GetNextReg(tmpreg),cg.GetNextReg(location.register));
                  end;
              end;
          end
        else  { subtract is a special case since its not commutative }
          begin
            if (nf_swapped in flags) then
              swapleftright;
            { left can only be a pointer in this case, since (int-pointer) is not supported }
            if pointernode<>left then
              internalerror(2014040603);
            if left.location.loc<>LOC_CONSTANT then
              begin
                if right.location.loc<>LOC_CONSTANT then
                  begin
                    cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_16,
                        right.location.register,left.location.register,location.register);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,
                       cg.GetNextReg(pointernode.location.register),cg.GetNextReg(location.register));
                  end
                else
                  begin
                    { farptr_reg - int_const }
                    cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_16,
                       right.location.value,left.location.register,location.register);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,
                       cg.GetNextReg(left.location.register),cg.GetNextReg(location.register));
                  end;
              end
            else
              begin
                { farptr_const - int_reg }
                tmpreg:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
                hlcg.a_load_const_reg(current_asmdata.CurrAsmList,resultdef,
                  left.location.value,tmpreg);
                cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_16,
                  right.location.register,tmpreg,location.register);
                cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_16,OS_16,
                   cg.GetNextReg(tmpreg),cg.GetNextReg(location.register));
              end;
          end;
      end;


    procedure ti8086addnode.second_cmp64bit;
      var
        truelabel,
        falselabel : tasmlabel;
        hregister,
        hregister2 : tregister;
        href       : treference;
        unsigned   : boolean;

      procedure firstjmp64bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swapped);
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.falselabel);
                   toggleflag(nf_swapped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
              unequaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
              else
                internalerror(2019051024);
           end;
        end;

      procedure middlejmp64bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   { the comparisaion of the low word have to be }
                   {  always unsigned!                           }
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swapped);
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.falselabel);
                   toggleflag(nf_swapped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
              unequaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
              else
                internalerror(2019051023);
           end;
        end;

      procedure lastjmp64bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low word have to be }
                   {  always unsigned!                           }
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.truelabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                end;
              equaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
                end;
              unequaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                end;
              else
                internalerror(2019051022);
           end;
        end;

      begin
        truelabel:=nil;
        falselabel:=nil;
        pass_left_right;

        unsigned:=((left.resultdef.typ=orddef) and
                   (torddef(left.resultdef).ordtype=u64bit)) or
                  ((right.resultdef.typ=orddef) and
                   (torddef(right.resultdef).ordtype=u64bit));

        { we have LOC_JUMP as result }
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              { we can reuse a CREGISTER for comparison }
              if (left.location.loc<>LOC_CREGISTER) then
               begin
                 hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                 hregister2:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                 cg64.a_load64_loc_reg(current_asmdata.CurrAsmList,left.location,joinreg64(hregister,hregister2));
                 location_freetemp(current_asmdata.CurrAsmList,left.location);
                 location_reset(left.location,LOC_REGISTER,left.location.size);
                 left.location.register64.reglo:=hregister;
                 left.location.register64.reghi:=hregister2;
               end;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end;
         end;

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           emit_reg_reg(A_CMP,S_W,cg.GetNextReg(right.location.register64.reghi),cg.GetNextReg(left.location.register64.reghi));
           firstjmp64bitcmp;
           emit_reg_reg(A_CMP,S_W,right.location.register64.reghi,left.location.register64.reghi);
           middlejmp64bitcmp;
           emit_reg_reg(A_CMP,S_W,cg.GetNextReg(right.location.register64.reglo),cg.GetNextReg(left.location.register64.reglo));
           middlejmp64bitcmp;
           emit_reg_reg(A_CMP,S_W,right.location.register64.reglo,left.location.register64.reglo);
           lastjmp64bitcmp;
         end
        else
         begin
           case right.location.loc of
             LOC_CREGISTER :
               begin
                 emit_reg_reg(A_CMP,S_W,cg.GetNextReg(right.location.register64.reghi),cg.GetNextReg(left.location.register64.reghi));
                 firstjmp64bitcmp;
                 emit_reg_reg(A_CMP,S_W,right.location.register64.reghi,left.location.register64.reghi);
                 middlejmp64bitcmp;
                 emit_reg_reg(A_CMP,S_W,cg.GetNextReg(right.location.register64.reglo),cg.GetNextReg(left.location.register64.reglo));
                 middlejmp64bitcmp;
                 emit_reg_reg(A_CMP,S_W,right.location.register64.reglo,left.location.register64.reglo);
                 lastjmp64bitcmp;
               end;
             LOC_CREFERENCE,
             LOC_REFERENCE :
               begin
                 tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,right.location.reference);
                 href:=right.location.reference;
                 inc(href.offset,6);
                 emit_ref_reg(A_CMP,S_W,href,cg.GetNextReg(left.location.register64.reghi));
                 firstjmp64bitcmp;
                 dec(href.offset,2);
                 emit_ref_reg(A_CMP,S_W,href,left.location.register64.reghi);
                 middlejmp64bitcmp;
                 dec(href.offset,2);
                 emit_ref_reg(A_CMP,S_W,href,cg.GetNextReg(left.location.register64.reglo));
                 middlejmp64bitcmp;
                 emit_ref_reg(A_CMP,S_W,right.location.reference,left.location.register64.reglo);
                 lastjmp64bitcmp;
                 cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                 location_freetemp(current_asmdata.CurrAsmList,right.location);
               end;
             LOC_CONSTANT :
               begin
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_W,aint((right.location.value64 shr 48) and $FFFF),cg.GetNextReg(left.location.register64.reghi)));
                 firstjmp64bitcmp;
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_W,aint((right.location.value64 shr 32) and $FFFF),left.location.register64.reghi));
                 middlejmp64bitcmp;
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_W,aint((right.location.value64 shr 16) and $FFFF),cg.GetNextReg(left.location.register64.reglo)));
                 middlejmp64bitcmp;
                 current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_W,aint(right.location.value64 and $FFFF),left.location.register64.reglo));
                 lastjmp64bitcmp;
               end;
             else
               internalerror(200203282);
           end;
         end;
      end;

    procedure ti8086addnode.second_cmp32bit;
      var
        truelabel,
        falselabel: tasmlabel;
        hregister : tregister;
        href      : treference;
        unsigned  : boolean;

      procedure firstjmp32bitcmp;

        var
           oldnodetype : tnodetype;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.truelabel);
                   { cheat a little bit for the negative test }
                   toggleflag(nf_swapped);
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.falselabel);
                   toggleflag(nf_swapped);
                end;
              lten,gten:
                begin
                   oldnodetype:=nodetype;
                   if nodetype=lten then
                     nodetype:=ltn
                   else
                     nodetype:=gtn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.truelabel);
                   { cheat for the negative test }
                   if nodetype=ltn then
                     nodetype:=gtn
                   else
                     nodetype:=ltn;
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(unsigned),location.falselabel);
                   nodetype:=oldnodetype;
                end;
              equaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
              unequaln:
                cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
              else
                internalerror(2019051021);
           end;
        end;

      procedure secondjmp32bitcmp;

        begin
           { the jump the sequence is a little bit hairy }
           case nodetype of
              ltn,gtn,lten,gten:
                begin
                   { the comparisaion of the low dword have to be }
                   {  always unsigned!                            }
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,getresflags(true),location.truelabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                end;
              equaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.falselabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.truelabel);
                end;
              unequaln:
                begin
                   cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,location.truelabel);
                   cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                end;
              else
                internalerror(2019051020);
           end;
        end;

      begin
        truelabel:=nil;
        falselabel:=nil;
        pass_left_right;

        unsigned:=((left.resultdef.typ=orddef) and
                   (torddef(left.resultdef).ordtype=u32bit)) or
                  ((right.resultdef.typ=orddef) and
                   (torddef(right.resultdef).ordtype=u32bit)) or
                  is_hugepointer(left.resultdef);

        { we have LOC_JUMP as result }
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        { left and right no register?  }
        { then one must be demanded    }
        if (left.location.loc<>LOC_REGISTER) then
         begin
           if (right.location.loc<>LOC_REGISTER) then
            begin
              { we can reuse a CREGISTER for comparison }
              if (left.location.loc<>LOC_CREGISTER) then
               begin
                 hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                 cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_32,left.location,hregister);
                 location_freetemp(current_asmdata.CurrAsmList,left.location);
                 location_reset(left.location,LOC_REGISTER,left.location.size);
                 left.location.register:=hregister;
               end;
            end
           else
            begin
              location_swap(left.location,right.location);
              toggleflag(nf_swapped);
            end;
         end;

        { at this point, left.location.loc should be LOC_REGISTER }
        if right.location.loc=LOC_REGISTER then
         begin
           emit_reg_reg(A_CMP,S_W,cg.GetNextReg(right.location.register),cg.GetNextReg(left.location.register));
           firstjmp32bitcmp;
           emit_reg_reg(A_CMP,S_W,right.location.register,left.location.register);
           secondjmp32bitcmp;
         end
        else
         begin
           case right.location.loc of
             LOC_CREGISTER :
               begin
                 emit_reg_reg(A_CMP,S_W,cg.GetNextReg(right.location.register),cg.GetNextReg(left.location.register));
                 firstjmp32bitcmp;
                 emit_reg_reg(A_CMP,S_W,right.location.register,left.location.register);
                 secondjmp32bitcmp;
               end;
             LOC_CREFERENCE,
             LOC_REFERENCE :
               begin
                 tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,right.location.reference);
                 href:=right.location.reference;
                 inc(href.offset,2);
                 emit_ref_reg(A_CMP,S_W,href,cg.GetNextReg(left.location.register));
                 firstjmp32bitcmp;
                 dec(href.offset,2);
                 emit_ref_reg(A_CMP,S_W,href,left.location.register);
                 secondjmp32bitcmp;
                 cg.a_jmp_always(current_asmdata.CurrAsmList,location.falselabel);
                 location_freetemp(current_asmdata.CurrAsmList,right.location);
               end;
             LOC_CONSTANT :
               begin
                 if (right.location.value=0) and (nodetype in [equaln,unequaln]) and (left.location.loc=LOC_REGISTER) then
                   begin
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_OR,S_W,cg.GetNextReg(left.location.register),left.location.register));
                     secondjmp32bitcmp;
                   end
                 else
                   begin
                     current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_W,aint((right.location.value shr 16) and $FFFF),cg.GetNextReg(left.location.register)));
                     firstjmp32bitcmp;
                     current_asmdata.CurrAsmList.concat(taicpu.op_const_reg(A_CMP,S_W,aint(right.location.value and $FFFF),left.location.register));
                     secondjmp32bitcmp;
                   end;
               end;
             else
               internalerror(2002032801);
           end;
         end;
      end;


    procedure ti8086addnode.second_cmpfarpointer;
      begin
        { handle = and <> as a 32-bit comparison }
        if nodetype in [equaln,unequaln] then
          begin
            second_cmp32bit;
            exit;
          end;

        pass_left_right;

        { <, >, <= and >= compare the 16-bit offset only }
        if (right.location.loc=LOC_CONSTANT) and
           (left.location.loc in [LOC_REFERENCE, LOC_CREFERENCE])
        then
          begin
            emit_const_ref(A_CMP, S_W, word(right.location.value), left.location.reference);
            location_freetemp(current_asmdata.CurrAsmList,left.location);
          end
        else
          begin
            { left location is not a register? }
            if left.location.loc<>LOC_REGISTER then
             begin
               { if right is register then we can swap the locations }
               if right.location.loc=LOC_REGISTER then
                begin
                  location_swap(left.location,right.location);
                  toggleflag(nf_swapped);
                end
               else
                begin
                  { maybe we can reuse a constant register when the
                    operation is a comparison that doesn't change the
                    value of the register }
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,u16inttype,true);
                end;
              end;

            emit_generic_code(A_CMP,OS_16,true,false,false);
            location_freetemp(current_asmdata.CurrAsmList,right.location);
            location_freetemp(current_asmdata.CurrAsmList,left.location);
          end;
        location_reset(location,LOC_FLAGS,OS_NO);
        location.resflags:=getresflags(true);
      end;


    procedure ti8086addnode.second_cmpordinal;
      begin
        if is_farpointer(left.resultdef) then
          second_cmpfarpointer
        else if is_32bit(left.resultdef) or is_hugepointer(left.resultdef) or is_farprocvar(left.resultdef) then
          second_cmp32bit
        else
          inherited second_cmpordinal;
      end;


{*****************************************************************************
                                x86 MUL
*****************************************************************************}

    procedure ti8086addnode.second_mul(unsigned: boolean);

    var reg:Tregister;
        ref:Treference;
        use_ref:boolean;
        hl4 : tasmlabel;
        overflowcheck: boolean;

    const
      asmops: array[boolean] of tasmop = (A_IMUL, A_MUL);

    begin
      reg:=NR_NO;
      reference_reset(ref,sizeof(pint),[]);

      pass_left_right;

      overflowcheck:=needoverflowcheck;

      { MUL is faster than IMUL on the 8086 & 8088 (and equal in speed on 286+),
        but it's only safe to use in place of IMUL when overflow checking is off
        and we're doing a 16-bit>16-bit multiplication }
      if not overflowcheck and
        (not is_32bitint(resultdef)) then
        unsigned:=true;

      {The location.register will be filled in later (JM)}
      location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
      { Mul supports registers and references, so if not register/reference,
        load the location into a register. }
      use_ref:=false;
      if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
        reg:=left.location.register
      else if left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
        begin
          tcgx86(cg).make_simple_ref(current_asmdata.CurrAsmList,left.location.reference);
          ref:=left.location.reference;
          use_ref:=true;
        end
      else
        begin
          {LOC_CONSTANT for example.}
          reg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
          hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,left.resultdef,osuinttype,left.location,reg);
        end;
      {Allocate AX.}
      cg.getcpuregister(current_asmdata.CurrAsmList,NR_AX);
      {Load the right value.}
      hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,osuinttype,right.location,NR_AX);
      {Also allocate DX, since it is also modified by a mul (JM).}
      cg.getcpuregister(current_asmdata.CurrAsmList,NR_DX);

      if overflowcheck and
        { 16->32 bit cannot overflow }
        (not is_32bitint(resultdef)) then
        cg.a_reg_alloc(current_asmdata.CurrAsmList, NR_DEFAULTFLAGS);

      if use_ref then
        emit_ref(asmops[unsigned],S_W,ref)
      else
        emit_reg(asmops[unsigned],S_W,reg);
      if overflowcheck and
        { 16->32 bit cannot overflow }
        (not is_32bitint(resultdef)) then
        begin
          current_asmdata.getjumplabel(hl4);
          cg.a_jmp_flags(current_asmdata.CurrAsmList,F_AE,hl4);
          cg.a_reg_dealloc(current_asmdata.CurrAsmList, NR_DEFAULTFLAGS);
          cg.a_call_name(current_asmdata.CurrAsmList,'FPC_OVERFLOW',false);
          cg.a_label(current_asmdata.CurrAsmList,hl4);
        end;
      {Free AX,DX}
      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_DX);
      if is_32bitint(resultdef) then
      begin
        {Allocate an imaginary 32-bit register, which consists of a pair of
         16-bit registers and store DX:AX into it}
        location.register := cg.getintregister(current_asmdata.CurrAsmList,OS_32);
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_DX,cg.GetNextReg(location.register));
        cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_AX);
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_AX,location.register);
      end
      else
      begin
        {Allocate a new register and store the result in AX in it.}
        location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
        cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_AX);
        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_AX,location.register);
      end;
      location_freetemp(current_asmdata.CurrAsmList,left.location);
      location_freetemp(current_asmdata.CurrAsmList,right.location);
    end;


begin
   caddnode:=ti8086addnode;
end.
