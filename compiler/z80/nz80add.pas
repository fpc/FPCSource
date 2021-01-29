{
    Copyright (c) 2008 by Florian Klaempfl

    Code generation for add nodes on the AVR

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
unit nz80add;

{$i fpcdefs.inc}

interface

    uses
       node,ncgadd, symtype,cpubase;

    type

       { TZ80AddNode }

       TZ80AddNode = class(tcgaddnode)
       private
         function GetResFlags(unsigned:Boolean;anodetype:tnodetype):TResFlags;
       protected
         function use_mul_helper: boolean;override;
         function first_cmppointer: tnode;override;
         function pass_1 : tnode;override;
         procedure second_cmpordinal;override;
         procedure second_cmpsmallset;override;
         procedure second_cmp64bit;override;
         procedure second_cmp16_32_64bit;
         procedure second_cmp;
       end;

  implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,paramgr,
      aasmbase,aasmtai,aasmdata,aasmcpu,defutil,htypechk,
      cgbase,cgutils,cgcpu,
      cpuinfo,pass_1,pass_2,procinfo,
      cpupara,
      ncon,nset,nadd,
      ncgutil,tgobj,rgobj,rgcpu,cgobj,cg64f32,
      hlcgobj;

{*****************************************************************************
                               TZ80AddNode
*****************************************************************************}


    function TZ80AddNode.GetResFlags(unsigned: Boolean; anodetype: tnodetype): TResFlags;
      begin
        case anodetype of
          equaln:
            GetResFlags:=F_E;
          unequaln:
            GetResFlags:=F_NE;
          else
            if not(unsigned) then
              begin
                { signed }
                if nf_swapped in flags then
                  case anodetype of
                    ltn:
                      GetResFlags:=F_NotPossible;
                    lten:
                      GetResFlags:=F_P;
                    gtn:
                      GetResFlags:=F_M;
                    gten:
                      GetResFlags:=F_NotPossible;
                    else
                      internalerror(2014082020);
                  end
                else
                  case anodetype of
                    ltn:
                      GetResFlags:=F_M;
                    lten:
                      GetResFlags:=F_NotPossible;
                    gtn:
                      GetResFlags:=F_NotPossible;
                    gten:
                      GetResFlags:=F_P;
                    else
                      internalerror(2014082021);
                  end;
              end
            else
              begin
                { unsigned }
                if nf_swapped in Flags then
                  case anodetype of
                    ltn:
                      GetResFlags:=F_NotPossible;
                    lten:
                      GetResFlags:=F_NC;
                    gtn:
                      GetResFlags:=F_C;
                    gten:
                      GetResFlags:=F_NotPossible;
                    else
                      internalerror(2014082022);
                  end
                else
                  case anodetype of
                    ltn:
                      GetResFlags:=F_C;
                    lten:
                      GetResFlags:=F_NotPossible;
                    gtn:
                      GetResFlags:=F_NotPossible;
                    gten:
                      GetResFlags:=F_NC;
                    else
                      internalerror(2014082023);
                  end;
              end;
        end;
      end;


    function TZ80AddNode.use_mul_helper: boolean;
      begin
        result:=(nodetype=muln);
      end;


    function TZ80AddNode.first_cmppointer: tnode;
      begin
        result:=nil;
        expectloc:=LOC_JUMP;
      end;


    procedure TZ80AddNode.second_cmpsmallset;

      procedure gencmp(tmpreg1,tmpreg2 : tregister);
        var
          i : byte;
        begin
          //current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CP,tmpreg1,tmpreg2));
          //for i:=2 to tcgsize2size[left.location.size] do
          //  begin
          //    tmpreg1:=GetNextReg(tmpreg1);
          //    tmpreg2:=GetNextReg(tmpreg2);
          //    current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_CPC,tmpreg1,tmpreg2));
          //  end;
        end;

      var
        tmpreg : tregister;
      begin
        //pass_left_right;
        //location_reset(location,LOC_FLAGS,OS_NO);
        //force_reg_left_right(false,false);
        //
        //case nodetype of
        //  equaln:
        //    begin
        //      gencmp(left.location.register,right.location.register);
        //      location.resflags:=F_EQ;
        //    end;
        //  unequaln:
        //    begin
        //      gencmp(left.location.register,right.location.register);
        //      location.resflags:=F_NE;
        //    end;
        //  lten,
        //  gten:
        //    begin
        //      if (not(nf_swapped in flags) and
        //          (nodetype = lten)) or
        //         ((nf_swapped in flags) and
        //          (nodetype = gten)) then
        //        swapleftright;
        //      tmpreg:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
        //      cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_AND,location.size,
        //        left.location.register,right.location.register,tmpreg);
        //      gencmp(tmpreg,right.location.register);
        //      location.resflags:=F_EQ;
        //    end;
        //  else
        //    internalerror(2004012401);
        //end;
      end;


    procedure TZ80AddNode.second_cmp;
      var
        unsigned : boolean;
        tmpreg1,tmpreg2 : tregister;
        i : longint;
        opdef: tdef;
        opsize: TCgSize;
        l: TAsmLabel;
      begin
        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));
        opdef:=left.resultdef;
        opsize:=def_cgsize(opdef);

        pass_left_right;

        if (opsize=OS_8) or ((opsize=OS_S8) and (NodeType in [equaln,unequaln])) then
          begin
            if getresflags(unsigned,NodeType)=F_NotPossible then
              swapleftright;

            if left.location.loc<>LOC_REGISTER then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              begin
                if is_ref_in_opertypes(right.location.reference,[OT_REF_IX_d,OT_REF_IY_d,OT_REF_HL]) then
                  begin
                    cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                    cg.a_load_loc_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),left.location,NR_A);
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_ref(A_CP,NR_A,right.location.reference));
                    cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                  end
                else
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
              end;
            case right.location.loc of
              LOC_CONSTANT:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  cg.a_load_loc_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),left.location,NR_A);
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_CP,NR_A,right.location.value));
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                end;
              LOC_REGISTER,LOC_CREGISTER:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  cg.a_load_loc_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),left.location,NR_A);
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(A_CP,NR_A,right.location.register));
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                end;
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  { Already handled before the case statement. Nothing to do here. }
                end;
              else
                internalerror(2020040402);
            end;

            location_reset(location,LOC_FLAGS,OS_NO);
            location.resflags:=getresflags(unsigned,NodeType);
          end
        else if opsize=OS_S8 then
          begin
            if getresflags(unsigned,NodeType)=F_NotPossible then
              swapleftright;

            if left.location.loc<>LOC_REGISTER then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              begin
                if is_ref_in_opertypes(right.location.reference,[OT_REF_IX_d,OT_REF_IY_d,OT_REF_HL]) then
                  begin
                    cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                    cg.a_load_loc_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),left.location,NR_A);
                    current_asmdata.CurrAsmList.Concat(taicpu.op_reg_ref(A_SUB,NR_A,right.location.reference));
                  end
                else
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
              end;
            case right.location.loc of
              LOC_CONSTANT:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  cg.a_load_loc_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),left.location,NR_A);
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_SUB,NR_A,right.location.value));
                end;
              LOC_REGISTER,LOC_CREGISTER:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  cg.a_load_loc_reg(current_asmdata.CurrAsmList,def_cgsize(left.resultdef),left.location,NR_A);
                  current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(A_SUB,NR_A,right.location.register));
                end;
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  { Already handled before the case statement. Nothing to do here. }
                end;
              else
                internalerror(2020040403);
            end;
            current_asmdata.getjumplabel(l);
            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_PO,l);
            current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_XOR,NR_A,$80));
            cg.a_label(current_asmdata.CurrAsmList,l);
            cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);

            location_reset(location,LOC_FLAGS,OS_NO);
            location.resflags:=getresflags(unsigned,NodeType);
          end
        else
          internalerror(2020040401);
      end;


    procedure TZ80AddNode.second_cmp64bit;
      begin
        second_cmp16_32_64bit;
      end;


    procedure TZ80AddNode.second_cmp16_32_64bit;
      var
        truelabel,
        falselabel: tasmlabel;
        unsigned  : boolean;
        i, size: Integer;
        tmpref: treference;
        op: TAsmOp;
        actualnodetype: tnodetype;
      begin
        truelabel:=nil;
        falselabel:=nil;
        pass_left_right;

        unsigned:=not(is_signed(left.resultdef)) or
                  not(is_signed(right.resultdef));

        { we have LOC_JUMP as result }
        current_asmdata.getjumplabel(truelabel);
        current_asmdata.getjumplabel(falselabel);
        location_reset_jump(location,truelabel,falselabel);

        size:=tcgsize2size[def_cgsize(left.resultdef)];

        if NodeType in [equaln,unequaln] then
          begin
            if left.location.loc<>LOC_REGISTER then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              begin
                if is_ref_in_opertypes(right.location.reference,[OT_REF_IX_d,OT_REF_IY_d,OT_REF_HL]) then
                  begin
                    cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                    tmpref:=right.location.reference;
                    for i:=0 to size-1 do
                      begin
                        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,tcgz80(cg).GetOffsetReg64(left.location.register,left.location.registerhi,i),NR_A);
                        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_ref(A_CP,NR_A,tmpref));
                        case NodeType of
                          equaln:
                            if i<>(size-1) then
                              cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,falselabel)
                            else
                              cg.a_jmp_flags(current_asmdata.CurrAsmList,F_E,truelabel);
                          unequaln:
                            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,truelabel);
                          else
                            internalerror(2020042102);
                        end;
                        if i<>(size-1) then
                          tcgz80(cg).adjust_normalized_ref(current_asmdata.CurrAsmList,tmpref,1);
                      end;
                    cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                    cg.a_jmp_always(current_asmdata.CurrAsmList,falselabel);
                  end
                else
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
              end;
            case right.location.loc of
              LOC_CONSTANT:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  for i:=0 to size-1 do
                    begin
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,tcgz80(cg).GetOffsetReg64(left.location.register,left.location.registerhi,i),NR_A);
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(A_CP,NR_A,byte(right.location.value shr (i*8))));
                      case NodeType of
                        equaln:
                          if i<>(size-1) then
                            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,falselabel)
                          else
                            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_E,truelabel);
                        unequaln:
                          cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,truelabel);
                        else
                          internalerror(2020042104);
                      end;
                    end;
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                  cg.a_jmp_always(current_asmdata.CurrAsmList,falselabel);
                end;
              LOC_REGISTER,LOC_CREGISTER:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  for i:=0 to size-1 do
                    begin
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,tcgz80(cg).GetOffsetReg64(left.location.register,left.location.registerhi,i),NR_A);
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(A_CP,NR_A,tcgz80(cg).GetOffsetReg64(right.location.register,right.location.registerhi,i)));
                      case NodeType of
                        equaln:
                          if i<>(size-1) then
                            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,falselabel)
                          else
                            cg.a_jmp_flags(current_asmdata.CurrAsmList,F_E,truelabel);
                        unequaln:
                          cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NE,truelabel);
                        else
                          internalerror(2020042105);
                      end;
                    end;
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                  cg.a_jmp_always(current_asmdata.CurrAsmList,falselabel);
                end;
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  { Already handled before the case statement. Nothing to do here. }
                end;
              else
                internalerror(2020042103);
            end;
          end
        else
          begin
            if nf_swapped in Flags then
              begin
                case NodeType of
                  ltn:
                    actualnodetype:=gtn;
                  lten:
                    actualnodetype:=gten;
                  gtn:
                    actualnodetype:=ltn;
                  gten:
                    actualnodetype:=lten;
                  else
                    internalerror(2020042701);
                end;
              end
            else
              actualnodetype:=NodeType;

            if left.location.loc<>LOC_REGISTER then
              hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              begin
                if is_ref_in_opertypes(right.location.reference,[OT_REF_IX_d,OT_REF_IY_d,OT_REF_HL]) then
                  begin
                    cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                    tmpref:=right.location.reference;
                    tcgz80(cg).adjust_normalized_ref(current_asmdata.CurrAsmList,tmpref,size-1);
                    for i:=size-1 downto 0 do
                      begin
                        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,tcgz80(cg).GetOffsetReg64(left.location.register,left.location.registerhi,i),NR_A);
                        if (i=(size-1)) and (not unsigned) then
                          op:=A_SUB
                        else
                          op:=A_CP;
                        current_asmdata.CurrAsmList.Concat(taicpu.op_reg_ref(op,NR_A,tmpref));
                        if (i=(size-1)) and (not unsigned) then
                          case actualnodetype of
                            ltn,
                            lten:
                              tcgz80(cg).a_jmp_signed_cmp_3way(current_asmdata.CurrAsmList,truelabel,nil,falselabel);
                            gtn,
                            gten:
                              tcgz80(cg).a_jmp_signed_cmp_3way(current_asmdata.CurrAsmList,falselabel,nil,truelabel);
                            else
                              internalerror(2020042202);
                          end
                        else if i<>0 then
                          case actualnodetype of
                            ltn,
                            lten:
                              tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,nil,falselabel);
                            gtn,
                            gten:
                              tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,nil,truelabel);
                            else
                              internalerror(2020042207);
                          end
                        else
                          case actualnodetype of
                            ltn:
                              tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,falselabel,falselabel);
                            lten:
                              tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,truelabel,falselabel);
                            gtn:
                              tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,falselabel,truelabel);
                            gten:
                              tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,truelabel,truelabel);
                            else
                              internalerror(2020042203);
                          end;
                        if i<>0 then
                          tcgz80(cg).adjust_normalized_ref(current_asmdata.CurrAsmList,tmpref,-1);
                      end;
                    cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                  end
                else
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,right.resultdef,false);
              end;
            case right.location.loc of
              LOC_CONSTANT:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  for i:=size-1 downto 0 do
                    begin
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,tcgz80(cg).GetOffsetReg64(left.location.register,left.location.registerhi,i),NR_A);
                      if (i=(size-1)) and (not unsigned) then
                        op:=A_SUB
                      else
                        op:=A_CP;
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_const(op,NR_A,byte(right.location.value shr (i*8))));
                      if (i=(size-1)) and (not unsigned) then
                        case actualnodetype of
                          ltn,
                          lten:
                            tcgz80(cg).a_jmp_signed_cmp_3way(current_asmdata.CurrAsmList,truelabel,nil,falselabel);
                          gtn,
                          gten:
                            tcgz80(cg).a_jmp_signed_cmp_3way(current_asmdata.CurrAsmList,falselabel,nil,truelabel);
                          else
                            internalerror(2020042210);
                        end
                      else if i<>0 then
                        case actualnodetype of
                          ltn,
                          lten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,nil,falselabel);
                          gtn,
                          gten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,nil,truelabel);
                          else
                            internalerror(2020042211);
                        end
                      else
                        case actualnodetype of
                          ltn:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,falselabel,falselabel);
                          lten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,truelabel,falselabel);
                          gtn:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,falselabel,truelabel);
                          gten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,truelabel,truelabel);
                          else
                            internalerror(2020042215);
                        end;
                    end;
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                end;
              LOC_REGISTER,LOC_CREGISTER:
                begin
                  cg.getcpuregister(current_asmdata.CurrAsmList,NR_A);
                  for i:=size-1 downto 0 do
                    begin
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_8,OS_8,tcgz80(cg).GetOffsetReg64(left.location.register,left.location.registerhi,i),NR_A);
                      if (i=(size-1)) and (not unsigned) then
                        op:=A_SUB
                      else
                        op:=A_CP;
                      current_asmdata.CurrAsmList.Concat(taicpu.op_reg_reg(op,NR_A,tcgz80(cg).GetOffsetReg64(right.location.register,right.location.registerhi,i)));
                      if (i=(size-1)) and (not unsigned) then
                        case actualnodetype of
                          ltn,
                          lten:
                            tcgz80(cg).a_jmp_signed_cmp_3way(current_asmdata.CurrAsmList,truelabel,nil,falselabel);
                          gtn,
                          gten:
                            tcgz80(cg).a_jmp_signed_cmp_3way(current_asmdata.CurrAsmList,falselabel,nil,truelabel);
                          else
                            internalerror(2020042212);
                        end
                      else if i<>0 then
                        case actualnodetype of
                          ltn,
                          lten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,nil,falselabel);
                          gtn,
                          gten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,nil,truelabel);
                          else
                            internalerror(2020042213);
                        end
                      else
                        case actualnodetype of
                          ltn:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,falselabel,falselabel);
                          lten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,truelabel,truelabel,falselabel);
                          gtn:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,falselabel,truelabel);
                          gten:
                            tcgz80(cg).a_jmp_unsigned_cmp_3way(current_asmdata.CurrAsmList,falselabel,truelabel,truelabel);
                          else
                            internalerror(2020042216);
                        end;
                    end;
                  cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_A);
                end;
              LOC_REFERENCE,LOC_CREFERENCE:
                begin
                  { Already handled before the case statement. Nothing to do here. }
                end;
              else
                internalerror(2020042106);
            end;
          end;
      end;


    function TZ80AddNode.pass_1 : tnode;
      begin
        result:=inherited pass_1;
{$ifdef dummy}
        if not(assigned(result)) then
          begin
            unsigned:=not(is_signed(left.resultdef)) or
              not(is_signed(right.resultdef));

            if is_64bit(left.resultdef) and
              ((nodetype in [equaln,unequaln]) or
               (unsigned and (nodetype in [ltn,lten,gtn,gten]))
              ) then
              expectloc:=LOC_FLAGS;
          end;
        { handling boolean expressions }
        if not(assigned(result)) and
           (
             not(is_boolean(left.resultdef)) or
             not(is_boolean(right.resultdef)) or
             is_dynamic_array(left.resultdef)
           ) then
          expectloc:=LOC_FLAGS;
{$endif dummy}
      end;


    procedure TZ80AddNode.second_cmpordinal;
      begin
        if left.resultdef.size>=2 then
          second_cmp16_32_64bit
        else
          second_cmp;
      end;

begin
  caddnode:=TZ80AddNode;
end.
