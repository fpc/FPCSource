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
         function  GetResFlags(unsigned:Boolean;anodetype:tnodetype):TResFlags;
       protected
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
        ai: taicpu;
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
                if (right.location.reference.base=NR_IX) and (right.location.reference.index=NR_NO) then
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
                if (right.location.reference.base=NR_IX) and (right.location.reference.index=NR_NO) then
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
                internalerror(2020040402);
            end;
            current_asmdata.getjumplabel(l);
            ai:=taicpu.op_cond_sym(A_JP,C_PO,l);
            ai.is_jmp:=true;
            current_asmdata.CurrAsmList.concat(ai);
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

        // todo: implement the rest
        internalerror(2020041601);
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
        if is_32bit(left.resultdef) or is_16bit(left.resultdef) then
          second_cmp16_32_64bit
        else
          second_cmp;
      end;

begin
  caddnode:=TZ80AddNode;
end.
