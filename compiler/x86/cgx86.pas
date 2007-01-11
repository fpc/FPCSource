{
    Copyright (c) 1998-2005 by Florian Klaempfl

    This unit implements the common parts of the code generator for the i386 and the x86-64.

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
{ This unit implements the common parts of the code generator for the i386 and the x86-64.
}
unit cgx86;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       cgbase,cgutils,cgobj,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       cpubase,cpuinfo,rgobj,rgx86,rgcpu,
       symconst,symtype;

    type
      tcgx86 = class(tcg)
        rgfpu   : Trgx86fpu;
        procedure done_register_allocators;override;

        function getfpuregister(list:TAsmList;size:Tcgsize):Tregister;override;
        function getmmxregister(list:TAsmList):Tregister;
        function getmmregister(list:TAsmList;size:Tcgsize):Tregister;override;

        procedure getcpuregister(list:TAsmList;r:Tregister);override;
        procedure ungetcpuregister(list:TAsmList;r:Tregister);override;
        procedure alloccpuregisters(list:TAsmList;rt:Tregistertype;const r:Tcpuregisterset);override;
        procedure dealloccpuregisters(list:TAsmList;rt:Tregistertype;const r:Tcpuregisterset);override;
        function  uses_registers(rt:Tregistertype):boolean;override;
        procedure add_reg_instruction(instr:Tai;r:tregister);override;
        procedure dec_fpu_stack;
        procedure inc_fpu_stack;

        procedure a_call_name(list : TAsmList;const s : string);override;
        procedure a_call_reg(list : TAsmList;reg : tregister);override;
        procedure a_call_ref(list : TAsmList;ref : treference);override;
        procedure a_call_name_static(list : TAsmList;const s : string);override;

        procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; reg: TRegister); override;
        procedure a_op_const_ref(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; const ref: TReference); override;
        procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister); override;
        procedure a_op_ref_reg(list : TAsmList; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister); override;
        procedure a_op_reg_ref(list : TAsmList; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference); override;

        { move instructions }
        procedure a_load_const_reg(list : TAsmList; tosize: tcgsize; a : aint;reg : tregister);override;
        procedure a_load_const_ref(list : TAsmList; tosize: tcgsize; a : aint;const ref : treference);override;
        procedure a_load_reg_ref(list : TAsmList;fromsize,tosize: tcgsize; reg : tregister;const ref : treference);override;
        procedure a_load_ref_reg(list : TAsmList;fromsize,tosize: tcgsize;const ref : treference;reg : tregister);override;
        procedure a_load_reg_reg(list : TAsmList;fromsize,tosize: tcgsize;reg1,reg2 : tregister);override;
        procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;

        { fpu move instructions }
        procedure a_loadfpu_reg_reg(list: TAsmList; size: tcgsize; reg1, reg2: tregister); override;
        procedure a_loadfpu_ref_reg(list: TAsmList; size: tcgsize; const ref: treference; reg: tregister); override;
        procedure a_loadfpu_reg_ref(list: TAsmList; size: tcgsize; reg: tregister; const ref: treference); override;

        { vector register move instructions }
        procedure a_loadmm_reg_reg(list: TAsmList; fromsize, tosize : tcgsize;reg1, reg2: tregister;shuffle : pmmshuffle); override;
        procedure a_loadmm_ref_reg(list: TAsmList; fromsize, tosize : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle); override;
        procedure a_loadmm_reg_ref(list: TAsmList; fromsize, tosize : tcgsize;reg: tregister; const ref: treference;shuffle : pmmshuffle); override;
        procedure a_opmm_ref_reg(list: TAsmList; Op: TOpCG; size : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle); override;
        procedure a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size : tcgsize;src,dst: tregister;shuffle : pmmshuffle);override;

        {  comparison operations }
        procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
          l : tasmlabel);override;
        procedure a_cmp_const_ref_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;const ref : treference;
          l : tasmlabel);override;
        procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;
        procedure a_cmp_ref_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister; l : tasmlabel); override;
        procedure a_cmp_reg_ref_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg : tregister; const ref: treference; l : tasmlabel); override;

        procedure a_jmp_name(list : TAsmList;const s : string);override;
        procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
        procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;

        procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags; reg: TRegister); override;
        procedure g_flags2ref(list: TAsmList; size: TCgSize; const f: tresflags; const ref: TReference); override;

        procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);override;

        { entry/exit code helpers }
        procedure g_releasevaluepara_openarray(list : TAsmList;const l:tlocation);override;
        procedure g_profilecode(list : TAsmList);override;
        procedure g_stackpointer_alloc(list : TAsmList;localsize : longint);override;
        procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;

        procedure g_overflowcheck(list: TAsmList; const l:tlocation;def:tdef);override;

        procedure make_simple_ref(list:TAsmList;var ref: treference);
      protected
        procedure a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
        procedure check_register_size(size:tcgsize;reg:tregister);

        procedure opmm_loc_reg(list: TAsmList; Op: TOpCG; size : tcgsize;loc : tlocation;dst: tregister; shuffle : pmmshuffle);

        function get_darwin_call_stub(const s: string): tasmsymbol;
      private
        procedure sizes2load(s1,s2 : tcgsize;var op: tasmop; var s3: topsize);

        procedure floatload(list: TAsmList; t : tcgsize;const ref : treference);
        procedure floatstore(list: TAsmList; t : tcgsize;const ref : treference);
        procedure floatloadops(t : tcgsize;var op : tasmop;var s : topsize);
        procedure floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);

      end;

   const
{$ifdef x86_64}
      TCGSize2OpSize: Array[tcgsize] of topsize =
        (S_NO,S_B,S_W,S_L,S_Q,S_T,S_B,S_W,S_L,S_Q,S_Q,
         S_FS,S_FL,S_FX,S_IQ,S_FXX,
         S_NO,S_NO,S_NO,S_MD,S_T,
         S_NO,S_NO,S_NO,S_NO,S_T);
{$else x86_64}
      TCGSize2OpSize: Array[tcgsize] of topsize =
        (S_NO,S_B,S_W,S_L,S_L,S_T,S_B,S_W,S_L,S_L,S_L,
         S_FS,S_FL,S_FX,S_IQ,S_FXX,
         S_NO,S_NO,S_NO,S_MD,S_T,
         S_NO,S_NO,S_NO,S_NO,S_T);
{$endif x86_64}

{$ifndef NOTARGETWIN}
      winstackpagesize = 4096;
{$endif NOTARGETWIN}


  implementation

    uses
       globals,verbose,systems,cutils,
       symdef,defutil,paramgr,procinfo,
       fmodule;

    const
      TOpCG2AsmOp: Array[topcg] of TAsmOp = (A_NONE,A_MOV,A_ADD,A_AND,A_DIV,
                            A_IDIV,A_IMUL,A_MUL,A_NEG,A_NOT,A_OR,
                            A_SAR,A_SHL,A_SHR,A_SUB,A_XOR);

      TOpCmp2AsmCond: Array[topcmp] of TAsmCond = (C_NONE,
          C_E,C_G,C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A);

    procedure Tcgx86.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        rg[R_MMREGISTER].free;
        rg[R_MMXREGISTER].free;
        rgfpu.free;
        inherited done_register_allocators;
      end;


    function Tcgx86.getfpuregister(list:TAsmList;size:Tcgsize):Tregister;
      begin
        result:=rgfpu.getregisterfpu(list);
      end;


    function Tcgx86.getmmxregister(list:TAsmList):Tregister;
      begin
        if not assigned(rg[R_MMXREGISTER]) then
          internalerror(2003121214);
        result:=rg[R_MMXREGISTER].getregister(list,R_SUBNONE);
      end;


    function Tcgx86.getmmregister(list:TAsmList;size:Tcgsize):Tregister;
      begin
        if not assigned(rg[R_MMREGISTER]) then
          internalerror(2003121234);
        case size of
          OS_F64:
            result:=rg[R_MMREGISTER].getregister(list,R_SUBMMD);
          OS_F32:
            result:=rg[R_MMREGISTER].getregister(list,R_SUBMMS);
          OS_M128:
            result:=rg[R_MMREGISTER].getregister(list,R_SUBMMWHOLE);
          else
            internalerror(200506041);
        end;
      end;


    procedure Tcgx86.getcpuregister(list:TAsmList;r:Tregister);
      begin
        if getregtype(r)=R_FPUREGISTER then
          internalerror(2003121210)
        else
          inherited getcpuregister(list,r);
      end;


    procedure tcgx86.ungetcpuregister(list:TAsmList;r:Tregister);
      begin
        if getregtype(r)=R_FPUREGISTER then
          rgfpu.ungetregisterfpu(list,r)
        else
          inherited ungetcpuregister(list,r);
      end;


    procedure Tcgx86.alloccpuregisters(list:TAsmList;rt:Tregistertype;const r:Tcpuregisterset);
      begin
        if rt<>R_FPUREGISTER then
          inherited alloccpuregisters(list,rt,r);
      end;


    procedure Tcgx86.dealloccpuregisters(list:TAsmList;rt:Tregistertype;const r:Tcpuregisterset);
      begin
        if rt<>R_FPUREGISTER then
          inherited dealloccpuregisters(list,rt,r);
      end;


    function  Tcgx86.uses_registers(rt:Tregistertype):boolean;
      begin
        if rt=R_FPUREGISTER then
          result:=false
        else
          result:=inherited uses_registers(rt);
      end;


    procedure tcgx86.add_reg_instruction(instr:Tai;r:tregister);
      begin
        if getregtype(r)<>R_FPUREGISTER then
          inherited add_reg_instruction(instr,r);
      end;


    procedure tcgx86.dec_fpu_stack;
      begin
        if rgfpu.fpuvaroffset<=0 then
          internalerror(200604201);
        dec(rgfpu.fpuvaroffset);
      end;


    procedure tcgx86.inc_fpu_stack;
      begin
        inc(rgfpu.fpuvaroffset);
      end;


{****************************************************************************
                       This is private property, keep out! :)
****************************************************************************}

    procedure tcgx86.sizes2load(s1,s2 : tcgsize; var op: tasmop; var s3: topsize);

       begin
         { ensure to have always valid sizes }
         if s1=OS_NO then
           s1:=s2;
         if s2=OS_NO then
           s2:=s1;
         case s2 of
           OS_8,OS_S8 :
             if S1 in [OS_8,OS_S8] then
               s3 := S_B
             else
               internalerror(200109221);
           OS_16,OS_S16:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BW;
               OS_16,OS_S16:
                 s3 := S_W;
               else
                 internalerror(200109222);
             end;
           OS_32,OS_S32:
             case s1 of
               OS_8,OS_S8:
                 s3 := S_BL;
               OS_16,OS_S16:
                 s3 := S_WL;
               OS_32,OS_S32:
                 s3 := S_L;
               else
                 internalerror(200109223);
             end;
{$ifdef x86_64}
           OS_64,OS_S64:
             case s1 of
               OS_8:
                 s3 := S_BL;
               OS_S8:
                 s3 := S_BQ;
               OS_16:
                 s3 := S_WL;
               OS_S16:
                 s3 := S_WQ;
               OS_32:
                 s3 := S_L;
               OS_S32:
                 s3 := S_LQ;
               OS_64,OS_S64:
                 s3 := S_Q;
               else
                 internalerror(200304302);
             end;
{$endif x86_64}
           else
             internalerror(200109227);
         end;
         if s3 in [S_B,S_W,S_L,S_Q] then
           op := A_MOV
         else if s1 in [OS_8,OS_16,OS_32,OS_64] then
           op := A_MOVZX
         else
{$ifdef x86_64}
           if s3 in [S_LQ] then
             op := A_MOVSXD
         else
{$endif x86_64}
           op := A_MOVSX;
       end;


    procedure tcgx86.make_simple_ref(list:TAsmList;var ref: treference);
      var
        hreg : tregister;
        href : treference;
      begin
{$ifdef x86_64}
        { Only 32bit is allowed }
        if ((ref.offset<low(longint)) or (ref.offset>high(longint))) then
          begin
            { Load constant value to register }
            hreg:=GetAddressRegister(list);
            list.concat(taicpu.op_const_reg(A_MOV,S_Q,ref.offset,hreg));
            ref.offset:=0;
            {if assigned(ref.symbol) then
              begin
                list.concat(taicpu.op_sym_ofs_reg(A_ADD,S_Q,ref.symbol,0,hreg));
                ref.symbol:=nil;
              end;}
            { Add register to reference }
            if ref.index=NR_NO then
              ref.index:=hreg
            else
              begin
                if ref.scalefactor<>0 then
                  begin
                    list.concat(taicpu.op_reg_reg(A_ADD,S_Q,ref.base,hreg));
                    ref.base:=hreg;
                  end
                else
                  begin
                    list.concat(taicpu.op_reg_reg(A_ADD,S_Q,ref.index,hreg));
                    ref.index:=hreg;
                  end;
               end;
          end;
        if (cs_create_pic in current_settings.moduleswitches) and
         assigned(ref.symbol) and not((ref.symbol.bind=AB_LOCAL) and (ref.symbol.typ in [AT_LABEL,AT_FUNCTION])) then
          begin
            reference_reset_symbol(href,ref.symbol,0);
            hreg:=getaddressregister(list);
            href.refaddr:=addr_pic;
            href.base:=NR_RIP;
            list.concat(taicpu.op_ref_reg(A_MOV,S_Q,href,hreg));

            ref.symbol:=nil;

            if ref.base=NR_NO then
              ref.base:=hreg
            else if ref.index=NR_NO then
              begin
                ref.index:=hreg;
                ref.scalefactor:=1;
              end
            else
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,S_Q,ref.base,hreg));
                ref.base:=hreg;
              end;
          end;
{$else x86_64}
        if (cs_create_pic in current_settings.moduleswitches) and
          assigned(ref.symbol) and not((ref.symbol.bind=AB_LOCAL) and (ref.symbol.typ in [AT_LABEL,AT_FUNCTION])) then
          begin
            reference_reset_symbol(href,ref.symbol,0);
            hreg:=getaddressregister(list);
            href.refaddr:=addr_pic;
            href.base:=current_procinfo.got;
            include(current_procinfo.flags,pi_needs_got);
            list.concat(taicpu.op_ref_reg(A_MOV,S_L,href,hreg));

            ref.symbol:=nil;

            if ref.base=NR_NO then
              ref.base:=hreg
            else if ref.index=NR_NO then
              begin
                ref.index:=hreg;
                ref.scalefactor:=1;
              end
            else
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,S_L,ref.base,hreg));
                ref.base:=hreg;
              end;
          end;
{$endif x86_64}
      end;


    procedure tcgx86.floatloadops(t : tcgsize;var op : tasmop;var s : topsize);
      begin
         case t of
            OS_F32 :
              begin
                 op:=A_FLD;
                 s:=S_FS;
              end;
            OS_F64 :
              begin
                 op:=A_FLD;
                 s:=S_FL;
              end;
            OS_F80 :
              begin
                 op:=A_FLD;
                 s:=S_FX;
              end;
            OS_C64 :
              begin
                 op:=A_FILD;
                 s:=S_IQ;
              end;
            else
              internalerror(200204041);
         end;
      end;


    procedure tcgx86.floatload(list: TAsmList; t : tcgsize;const ref : treference);

      var
         op : tasmop;
         s : topsize;
         tmpref : treference;
      begin
         tmpref:=ref;
         make_simple_ref(list,tmpref);
         floatloadops(t,op,s);
         list.concat(Taicpu.Op_ref(op,s,tmpref));
         inc_fpu_stack;
      end;


    procedure tcgx86.floatstoreops(t : tcgsize;var op : tasmop;var s : topsize);

      begin
         case t of
            OS_F32 :
              begin
                 op:=A_FSTP;
                 s:=S_FS;
              end;
            OS_F64 :
              begin
                 op:=A_FSTP;
                 s:=S_FL;
              end;
            OS_F80 :
              begin
                  op:=A_FSTP;
                  s:=S_FX;
               end;
            OS_C64 :
               begin
                  op:=A_FISTP;
                  s:=S_IQ;
               end;
            else
               internalerror(200204042);
         end;
      end;


    procedure tcgx86.floatstore(list: TAsmList; t : tcgsize;const ref : treference);

      var
         op : tasmop;
         s : topsize;
         tmpref : treference;
      begin
         tmpref:=ref;
         make_simple_ref(list,tmpref);
         floatstoreops(t,op,s);
         list.concat(Taicpu.Op_ref(op,s,tmpref));
         { storing non extended floats can cause a floating point overflow }
         if (t<>OS_F80) and
            (cs_fpu_fwait in current_settings.localswitches) then
           list.concat(Taicpu.Op_none(A_FWAIT,S_NO));
         dec_fpu_stack;
      end;


    procedure tcgx86.check_register_size(size:tcgsize;reg:tregister);
      begin
        if TCGSize2OpSize[size]<>TCGSize2OpSize[reg_cgsize(reg)] then
          internalerror(200306031);
      end;


{****************************************************************************
                              Assembler code
****************************************************************************}

    procedure tcgx86.a_jmp_name(list : TAsmList;const s : string);
      begin
        list.concat(taicpu.op_sym(A_JMP,S_NO,current_asmdata.RefAsmSymbol(s)));
      end;


    procedure tcgx86.a_jmp_always(list : TAsmList;l: tasmlabel);
      begin
        a_jmp_cond(list, OC_NONE, l);
      end;


    function tcgx86.get_darwin_call_stub(const s: string): tasmsymbol;
      var
        stubname: string;
      begin
        stubname := 'L'+s+'$stub';
        result := current_asmdata.getasmsymbol(stubname);
        if assigned(result) then
          exit;

        if current_asmdata.asmlists[al_imports]=nil then
          current_asmdata.asmlists[al_imports]:=TAsmList.create;

        current_asmdata.asmlists[al_imports].concat(Tai_section.create(sec_stub,'',0));
        result := current_asmdata.RefAsmSymbol(stubname);
        current_asmdata.asmlists[al_imports].concat(Tai_symbol.Create(result,0));
        current_asmdata.asmlists[al_imports].concat(tai_directive.create(asd_indirect_symbol,s));
        current_asmdata.asmlists[al_imports].concat(taicpu.op_none(A_HLT));
        current_asmdata.asmlists[al_imports].concat(taicpu.op_none(A_HLT));
        current_asmdata.asmlists[al_imports].concat(taicpu.op_none(A_HLT));
        current_asmdata.asmlists[al_imports].concat(taicpu.op_none(A_HLT));
        current_asmdata.asmlists[al_imports].concat(taicpu.op_none(A_HLT));
      end;


    procedure tcgx86.a_call_name(list : TAsmList;const s : string);
      var
        sym : tasmsymbol;
        r : treference;
      begin

        if (target_info.system <> system_i386_darwin) then
          begin
            sym:=current_asmdata.RefAsmSymbol(s);
            reference_reset_symbol(r,sym,0);
            if cs_create_pic in current_settings.moduleswitches then
              begin
{$ifdef i386}
                include(current_procinfo.flags,pi_needs_got);
{$endif i386}
                r.refaddr:=addr_pic
              end
            else
              r.refaddr:=addr_full;
          end
        else
          begin
            reference_reset_symbol(r,get_darwin_call_stub(s),0);
            r.refaddr:=addr_full;
          end;
        list.concat(taicpu.op_ref(A_CALL,S_NO,r));
      end;


    procedure tcgx86.a_call_name_static(list : TAsmList;const s : string);
      var
        sym : tasmsymbol;
        r : treference;
      begin
        sym:=current_asmdata.RefAsmSymbol(s);
        reference_reset_symbol(r,sym,0);
        r.refaddr:=addr_full;
        list.concat(taicpu.op_ref(A_CALL,S_NO,r));
      end;


    procedure tcgx86.a_call_reg(list : TAsmList;reg : tregister);
      begin
        list.concat(taicpu.op_reg(A_CALL,S_NO,reg));
      end;


    procedure tcgx86.a_call_ref(list : TAsmList;ref : treference);
      begin
        list.concat(taicpu.op_ref(A_CALL,S_NO,ref));
      end;


{********************** load instructions ********************}

    procedure tcgx86.a_load_const_reg(list : TAsmList; tosize: TCGSize; a : aint; reg : TRegister);

      begin
        check_register_size(tosize,reg);
        { the optimizer will change it to "xor reg,reg" when loading zero, }
        { no need to do it here too (JM)                                   }
        list.concat(taicpu.op_const_reg(A_MOV,TCGSize2OpSize[tosize],a,reg))
      end;


    procedure tcgx86.a_load_const_ref(list : TAsmList; tosize: tcgsize; a : aint;const ref : treference);
      var
        tmpref : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
{$ifdef x86_64}
        { x86_64 only supports signed 32 bits constants directly }
        if (tosize in [OS_S64,OS_64]) and
           ((a<low(longint)) or (a>high(longint))) then
          begin
            a_load_const_ref(list,OS_32,longint(a and $ffffffff),tmpref);
            inc(tmpref.offset,4);
            a_load_const_ref(list,OS_32,longint(a shr 32),tmpref);
          end
        else
{$endif x86_64}
          list.concat(taicpu.op_const_ref(A_MOV,TCGSize2OpSize[tosize],a,tmpref));
      end;


    procedure tcgx86.a_load_reg_ref(list : TAsmList; fromsize,tosize: TCGSize; reg : tregister;const ref : treference);
      var
        op: tasmop;
        s: topsize;
        tmpsize : tcgsize;
        tmpreg  : tregister;
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
        check_register_size(fromsize,reg);
        sizes2load(fromsize,tosize,op,s);
        case s of
{$ifdef x86_64}
          S_BQ,S_WQ,S_LQ,
{$endif x86_64}
          S_BW,S_BL,S_WL :
            begin
              tmpreg:=getintregister(list,tosize);
{$ifdef x86_64}
              { zero extensions to 64 bit on the x86_64 are simply done by writting to the lower 32 bit
                which clears the upper 64 bit too, so it could be that s is S_L while the reg is
                64 bit (FK) }
              if s in [S_BL,S_WL,S_L] then
                begin
                  tmpreg:=makeregsize(list,tmpreg,OS_32);
                  tmpsize:=OS_32;
                end
              else
{$endif x86_64}
                tmpsize:=tosize;
              list.concat(taicpu.op_reg_reg(op,s,reg,tmpreg));
              a_load_reg_ref(list,tmpsize,tosize,tmpreg,tmpref);
            end;
        else
          list.concat(taicpu.op_reg_ref(op,s,reg,tmpref));
        end;
      end;


    procedure tcgx86.a_load_ref_reg(list : TAsmList;fromsize,tosize : tcgsize;const ref: treference;reg : tregister);
      var
        op: tasmop;
        s: topsize;
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
        check_register_size(tosize,reg);
        sizes2load(fromsize,tosize,op,s);
 {$ifdef x86_64}
        { zero extensions to 64 bit on the x86_64 are simply done by writting to the lower 32 bit
          which clears the upper 64 bit too, so it could be that s is S_L while the reg is
          64 bit (FK) }
        if s in [S_BL,S_WL,S_L] then
          reg:=makeregsize(list,reg,OS_32);
{$endif x86_64}
        list.concat(taicpu.op_ref_reg(op,s,tmpref,reg));
      end;


    procedure tcgx86.a_load_reg_reg(list : TAsmList;fromsize,tosize : tcgsize;reg1,reg2 : tregister);
      var
        op: tasmop;
        s: topsize;
        instr:Taicpu;
      begin
        check_register_size(fromsize,reg1);
        check_register_size(tosize,reg2);
        if tcgsize2size[fromsize]>tcgsize2size[tosize] then
          begin
            reg1:=makeregsize(list,reg1,tosize);
            s:=tcgsize2opsize[tosize];
            op:=A_MOV;
          end
        else
          sizes2load(fromsize,tosize,op,s);
{$ifdef x86_64}
        { zero extensions to 64 bit on the x86_64 are simply done by writting to the lower 32 bit
          which clears the upper 64 bit too, so it could be that s is S_L while the reg is
          64 bit (FK)
        }
        if s in [S_BL,S_WL,S_L] then
          reg2:=makeregsize(list,reg2,OS_32);
{$endif x86_64}
        if (reg1<>reg2) then
          begin
            instr:=taicpu.op_reg_reg(op,s,reg1,reg2);
            { Notify the register allocator that we have written a move instruction so
              it can try to eliminate it. }
            if (reg1<>current_procinfo.framepointer) and (reg1<>NR_STACK_POINTER_REG) then
              add_move_instruction(instr);
            list.concat(instr);
          end;
{$ifdef x86_64}
        { avoid merging of registers and killing the zero extensions (FK) }
        if (tosize in [OS_64,OS_S64]) and (s=S_L) then
          list.concat(taicpu.op_const_reg(A_AND,S_L,$ffffffff,reg2));
{$endif x86_64}
      end;


    procedure tcgx86.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
      var
        tmpref  : treference;
      begin
        with ref do
          begin
            if (base=NR_NO) and (index=NR_NO) then
              begin
                if assigned(ref.symbol) then
                  begin
                    if (cs_create_pic in current_settings.moduleswitches) then
                      begin
{$ifdef x86_64}
                        reference_reset_symbol(tmpref,ref.symbol,0);
                        tmpref.refaddr:=addr_pic;
                        tmpref.base:=NR_RIP;
                        list.concat(taicpu.op_ref_reg(A_MOV,S_Q,tmpref,r));
{$else x86_64}
                        reference_reset_symbol(tmpref,ref.symbol,0);
                        tmpref.refaddr:=addr_pic;
                        tmpref.base:=current_procinfo.got;
                        include(current_procinfo.flags,pi_needs_got);
                        list.concat(taicpu.op_ref_reg(A_MOV,S_L,tmpref,r));
{$endif x86_64}
                        if offset<>0 then
                          a_op_const_reg(list,OP_ADD,OS_ADDR,offset,r);
                      end
                    else
                      begin
                        tmpref:=ref;
                        tmpref.refaddr:=ADDR_FULL;
                        list.concat(Taicpu.op_ref_reg(A_MOV,tcgsize2opsize[OS_ADDR],tmpref,r));
                      end
                  end
                else
                  a_load_const_reg(list,OS_ADDR,offset,r)
              end
            else if (base=NR_NO) and (index<>NR_NO) and
                    (offset=0) and (scalefactor=0) and (symbol=nil) then
              a_load_reg_reg(list,OS_ADDR,OS_ADDR,index,r)
            else if (base<>NR_NO) and (index=NR_NO) and
                    (offset=0) and (symbol=nil) then
                a_load_reg_reg(list,OS_ADDR,OS_ADDR,base,r)
            else
              begin
                tmpref:=ref;
                make_simple_ref(list,tmpref);
                list.concat(Taicpu.op_ref_reg(A_LEA,tcgsize2opsize[OS_ADDR],tmpref,r));
              end;
            if segment<>NR_NO then
              begin
                if (tf_section_threadvars in target_info.flags) then
                  begin
                    { Convert thread local address to a process global addres
                      as we cannot handle far pointers.}
                    case target_info.system of
                      system_i386_linux:
                        if segment=NR_GS then
                          begin
                            reference_reset_symbol(tmpref,current_asmdata.RefAsmSymbol('___fpc_threadvar_offset'),0);
                            tmpref.segment:=NR_GS;
                            list.concat(Taicpu.op_ref_reg(A_ADD,tcgsize2opsize[OS_ADDR],tmpref,r));
                          end
                        else
                          cgmessage(cg_e_cant_use_far_pointer_there);
                      system_i386_win32:
                        if segment=NR_FS then
                          begin
                            allocallcpuregisters(list);
                            a_call_name(list,'GetTls');
                            deallocallcpuregisters(list);
                            list.concat(Taicpu.op_reg_reg(A_ADD,tcgsize2opsize[OS_ADDR],NR_EAX,r));
                          end
                        else
                          cgmessage(cg_e_cant_use_far_pointer_there);

                      else
                        cgmessage(cg_e_cant_use_far_pointer_there);
                    end;
                  end
                else
                  cgmessage(cg_e_cant_use_far_pointer_there);
              end;
          end;
      end;


    { all fpu load routines expect that R_ST[0-7] means an fpu regvar and }
    { R_ST means "the current value at the top of the fpu stack" (JM)     }
    procedure tcgx86.a_loadfpu_reg_reg(list: TAsmList; size: tcgsize; reg1, reg2: tregister);

       begin
         if (reg1<>NR_ST) then
           begin
             list.concat(taicpu.op_reg(A_FLD,S_NO,rgfpu.correct_fpuregister(reg1,rgfpu.fpuvaroffset)));
             inc_fpu_stack;
           end;
         if (reg2<>NR_ST) then
           begin
             list.concat(taicpu.op_reg(A_FSTP,S_NO,rgfpu.correct_fpuregister(reg2,rgfpu.fpuvaroffset)));
             dec_fpu_stack;
           end;
       end;


    procedure tcgx86.a_loadfpu_ref_reg(list: TAsmList; size: tcgsize; const ref: treference; reg: tregister);
       begin
         floatload(list,size,ref);
         if (reg<>NR_ST) then
           a_loadfpu_reg_reg(list,size,NR_ST,reg);
       end;


    procedure tcgx86.a_loadfpu_reg_ref(list: TAsmList; size: tcgsize; reg: tregister; const ref: treference);
       begin
         if reg<>NR_ST then
           a_loadfpu_reg_reg(list,size,reg,NR_ST);
         floatstore(list,size,ref);
       end;


    function get_scalar_mm_op(fromsize,tosize : tcgsize) : tasmop;
      const
        convertop : array[OS_F32..OS_F128,OS_F32..OS_F128] of tasmop = (
          (A_MOVSS,A_CVTSS2SD,A_NONE,A_NONE,A_NONE),
          (A_CVTSD2SS,A_MOVSD,A_NONE,A_NONE,A_NONE),
          (A_NONE,A_NONE,A_NONE,A_NONE,A_NONE),
          (A_NONE,A_NONE,A_NONE,A_MOVQ,A_NONE),
          (A_NONE,A_NONE,A_NONE,A_NONE,A_NONE));
      begin
        result:=convertop[fromsize,tosize];
        if result=A_NONE then
          internalerror(200312205);
      end;


    procedure tcgx86.a_loadmm_reg_reg(list: TAsmList; fromsize, tosize : tcgsize;reg1, reg2: tregister;shuffle : pmmshuffle);
      var
        instr : taicpu;
      begin
        if shuffle=nil then
          begin
            if fromsize=tosize then
              { needs correct size in case of spilling }
              case fromsize of
                OS_F32:
                  instr:=taicpu.op_reg_reg(A_MOVAPS,S_NO,reg1,reg2);
                OS_F64:
                  instr:=taicpu.op_reg_reg(A_MOVAPD,S_NO,reg1,reg2);
                else
                  internalerror(2006091201);
              end
            else
              internalerror(200312202);
          end
        else if shufflescalar(shuffle) then
          instr:=taicpu.op_reg_reg(get_scalar_mm_op(fromsize,tosize),S_NO,reg1,reg2)
        else
          internalerror(200312201);
        case get_scalar_mm_op(fromsize,tosize) of
          A_MOVSS,
          A_MOVSD,
          A_MOVQ:
            add_move_instruction(instr);
        end;
        list.concat(instr);
      end;


    procedure tcgx86.a_loadmm_ref_reg(list: TAsmList; fromsize, tosize : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle);
       var
         tmpref  : treference;
       begin
         tmpref:=ref;
         make_simple_ref(list,tmpref);
         if shuffle=nil then
           list.concat(taicpu.op_ref_reg(A_MOVQ,S_NO,tmpref,reg))
         else if shufflescalar(shuffle) then
           list.concat(taicpu.op_ref_reg(get_scalar_mm_op(fromsize,tosize),S_NO,tmpref,reg))
         else
           internalerror(200312252);
       end;


    procedure tcgx86.a_loadmm_reg_ref(list: TAsmList; fromsize, tosize : tcgsize;reg: tregister; const ref: treference;shuffle : pmmshuffle);
       var
         hreg : tregister;
         tmpref  : treference;
       begin
         tmpref:=ref;
         make_simple_ref(list,tmpref);
         if shuffle=nil then
           list.concat(taicpu.op_reg_ref(A_MOVQ,S_NO,reg,tmpref))
         else if shufflescalar(shuffle) then
           begin
             if tosize<>fromsize then
               begin
                 hreg:=getmmregister(list,tosize);
                 list.concat(taicpu.op_reg_reg(get_scalar_mm_op(fromsize,tosize),S_NO,reg,hreg));
                 list.concat(taicpu.op_reg_ref(get_scalar_mm_op(tosize,tosize),S_NO,hreg,tmpref));
               end
             else
               list.concat(taicpu.op_reg_ref(get_scalar_mm_op(fromsize,tosize),S_NO,reg,tmpref));
           end
         else
           internalerror(200312252);
       end;


    procedure tcgx86.a_opmm_ref_reg(list: TAsmList; Op: TOpCG; size : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle);
      var
        l : tlocation;
      begin
        l.loc:=LOC_REFERENCE;
        l.reference:=ref;
        l.size:=size;
        opmm_loc_reg(list,op,size,l,reg,shuffle);
      end;


    procedure tcgx86.a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size : tcgsize;src,dst: tregister;shuffle : pmmshuffle);
     var
       l : tlocation;
     begin
       l.loc:=LOC_MMREGISTER;
       l.register:=src;
       l.size:=size;
       opmm_loc_reg(list,op,size,l,dst,shuffle);
     end;


    procedure tcgx86.opmm_loc_reg(list: TAsmList; Op: TOpCG; size : tcgsize;loc : tlocation;dst: tregister; shuffle : pmmshuffle);
      const
        opmm2asmop : array[0..1,OS_F32..OS_F64,topcg] of tasmop = (
          ( { scalar }
            ( { OS_F32 }
              A_NOP,A_NOP,A_ADDSS,A_NOP,A_DIVSS,A_NOP,A_NOP,A_MULSS,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_SUBSS,A_NOP
            ),
            ( { OS_F64 }
              A_NOP,A_NOP,A_ADDSD,A_NOP,A_DIVSD,A_NOP,A_NOP,A_MULSD,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_SUBSD,A_NOP
            )
          ),
          ( { vectorized/packed }
            { because the logical packed single instructions have shorter op codes, we use always
              these
            }
            ( { OS_F32 }
              A_NOP,A_NOP,A_ADDPS,A_NOP,A_DIVPS,A_NOP,A_NOP,A_MULPS,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_SUBPS,A_XORPS
            ),
            ( { OS_F64 }
              A_NOP,A_NOP,A_ADDPD,A_NOP,A_DIVPD,A_NOP,A_NOP,A_MULPD,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_NOP,A_SUBPD,A_XORPD
            )
          )
        );

      var
        resultreg : tregister;
        asmop : tasmop;
      begin
        { this is an internally used procedure so the parameters have
          some constrains
        }
        if loc.size<>size then
          internalerror(200312213);
        resultreg:=dst;
        { deshuffle }
        //!!!
        if (shuffle<>nil) and not(shufflescalar(shuffle)) then
          begin
          end
        else if (shuffle=nil) then
          asmop:=opmm2asmop[1,size,op]
        else if shufflescalar(shuffle) then
          begin
            asmop:=opmm2asmop[0,size,op];
            { no scalar operation available? }
            if asmop=A_NOP then
              begin
                { do vectorized and shuffle finally }
                //!!!
              end;
          end
        else
          internalerror(200312211);
        if asmop=A_NOP then
          internalerror(200312216);
        case loc.loc of
          LOC_CREFERENCE,LOC_REFERENCE:
            begin
              make_simple_ref(current_asmdata.CurrAsmList,loc.reference);
              list.concat(taicpu.op_ref_reg(asmop,S_NO,loc.reference,resultreg));
            end;
          LOC_CMMREGISTER,LOC_MMREGISTER:
            list.concat(taicpu.op_reg_reg(asmop,S_NO,loc.register,resultreg));
          else
            internalerror(200312214);
        end;
        { shuffle }
        if resultreg<>dst then
          begin
            internalerror(200312212);
          end;
      end;


    procedure tcgx86.a_op_const_reg(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; reg: TRegister);

      var
        opcode : tasmop;
        power  : longint;
{$ifdef x86_64}
        tmpreg : tregister;
{$endif x86_64}
      begin
        optimize_op_const(op, a);
{$ifdef x86_64}
        { x86_64 only supports signed 32 bits constants directly }
        if not(op in [OP_NONE,OP_MOVE]) and
           (size in [OS_S64,OS_64]) and
            ((a<low(longint)) or (a>high(longint))) then
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_op_reg_reg(list,op,size,tmpreg,reg);
            exit;
          end;
{$endif x86_64}
        check_register_size(size,reg);
        case op of
          OP_NONE :
            begin
              { Opcode is optimized away }
            end;
          OP_MOVE :
            begin
              { Optimized, replaced with a simple load }
              a_load_const_reg(list,size,a,reg);
            end;
          OP_DIV, OP_IDIV:
            begin
              if ispowerof2(int64(a),power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_reg(opcode,TCgSize2OpSize[size],power,reg));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109224);
            end;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in current_settings.localswitches) and
                 ispowerof2(int64(a),power) then
                begin
                  list.concat(taicpu.op_const_reg(A_SHL,TCgSize2OpSize[size],power,reg));
                  exit;
                end;
              if op = OP_IMUL then
                list.concat(taicpu.op_const_reg(A_IMUL,TCgSize2OpSize[size],a,reg))
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109225);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if not(cs_check_overflow in current_settings.localswitches) and
               (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_reg(A_INC,TCgSize2OpSize[size],reg))
              else
                list.concat(taicpu.op_reg(A_DEC,TCgSize2OpSize[size],reg))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                list.concat(taicpu.op_const_reg(A_MOV,TCgSize2OpSize[size],0,reg))
            else if (aword(a) = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_reg(A_MOV,TCgSize2OpSize[size],aint(high(aword)),reg));
                       OP_XOR:
                         list.concat(taicpu.op_reg(A_NOT,TCgSize2OpSize[size],reg));
                     end
                   end
            else
              list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],TCgSize2OpSize[size],a,reg));
          OP_SHL,OP_SHR,OP_SAR:
            begin
{$ifdef x86_64}
              if (a and 63) <> 0 Then
                list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 63,reg));
              if (a shr 6) <> 0 Then
                internalerror(200609073);
{$else x86_64}
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,reg));
              if (a shr 5) <> 0 Then
                internalerror(200609071);
{$endif x86_64}
            end
          else internalerror(200609072);
        end;
      end;


    procedure tcgx86.a_op_const_ref(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; const ref: TReference);
      var
        opcode: tasmop;
        power: longint;
{$ifdef x86_64}
        tmpreg : tregister;
{$endif x86_64}
        tmpref  : treference;
      begin
        optimize_op_const(op, a);
        tmpref:=ref;
        make_simple_ref(list,tmpref);
{$ifdef x86_64}
        { x86_64 only supports signed 32 bits constants directly }
        if not(op in [OP_NONE,OP_MOVE]) and
           (size in [OS_S64,OS_64]) and
            ((a<low(longint)) or (a>high(longint))) then
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_op_reg_ref(list,op,size,tmpreg,tmpref);
            exit;
          end;
{$endif x86_64}
        Case Op of
          OP_NONE :
            begin
              { Opcode is optimized away }
            end;
          OP_MOVE :
            begin
              { Optimized, replaced with a simple load }
              a_load_const_ref(list,size,a,ref);
            end;
          OP_DIV, OP_IDIV:
            Begin
              if ispowerof2(int64(a),power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_ref(opcode,
                    TCgSize2OpSize[size],power,tmpref));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109231);
            End;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in current_settings.localswitches) and
                 ispowerof2(int64(a),power) then
                begin
                  list.concat(taicpu.op_const_ref(A_SHL,TCgSize2OpSize[size],
                    power,tmpref));
                  exit;
                end;
              { can't multiply a memory location directly with a constant }
              if op = OP_IMUL then
                inherited a_op_const_ref(list,op,size,a,tmpref)
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109232);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if not(cs_check_overflow in current_settings.localswitches) and
               (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_ref(A_INC,TCgSize2OpSize[size],tmpref))
              else
                list.concat(taicpu.op_ref(A_DEC,TCgSize2OpSize[size],tmpref))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                a_load_const_ref(list,size,0,tmpref)
            else if (aword(a) = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_ref(A_MOV,TCgSize2OpSize[size],aint(high(aword)),tmpref));
                       OP_XOR:
                         list.concat(taicpu.op_ref(A_NOT,TCgSize2OpSize[size],tmpref));
                     end
                   end
            else
              list.concat(taicpu.op_const_ref(TOpCG2AsmOp[op],
                TCgSize2OpSize[size],a,tmpref));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 then
                list.concat(taicpu.op_const_ref(
                  TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,tmpref));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;
      end;


    procedure tcgx86.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; src, dst: TRegister);
      var
        dstsize: topsize;
        instr:Taicpu;
      begin
        check_register_size(size,src);
        check_register_size(size,dst);
        dstsize := tcgsize2opsize[size];
        case op of
          OP_NEG,OP_NOT:
            begin
              if src<>dst then
                a_load_reg_reg(list,size,size,src,dst);
              list.concat(taicpu.op_reg(TOpCG2AsmOp[op],dstsize,dst));
            end;
          OP_MUL,OP_DIV,OP_IDIV:
            { special stuff, needs separate handling inside code }
            { generator                                          }
            internalerror(200109233);
          OP_SHR,OP_SHL,OP_SAR:
            begin
              { Use ecx to load the value, that allows beter coalescing }
              getcpuregister(list,NR_ECX);
              a_load_reg_reg(list,size,OS_32,src,NR_ECX);
              list.concat(taicpu.op_reg_reg(Topcg2asmop[op],tcgsize2opsize[size],NR_CL,dst));
              ungetcpuregister(list,NR_ECX);
            end;
          else
            begin
              if reg2opsize(src) <> dstsize then
                internalerror(200109226);
              instr:=taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,src,dst);
              list.concat(instr);
            end;
        end;
      end;


    procedure tcgx86.a_op_ref_reg(list : TAsmList; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);
      var
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
        check_register_size(size,reg);
        case op of
          OP_NEG,OP_NOT,OP_IMUL:
            begin
              inherited a_op_ref_reg(list,op,size,tmpref,reg);
            end;
          OP_MUL,OP_DIV,OP_IDIV:
            { special stuff, needs separate handling inside code }
            { generator                                          }
            internalerror(200109239);
          else
            begin
              reg := makeregsize(list,reg,size);
              list.concat(taicpu.op_ref_reg(TOpCG2AsmOp[op],tcgsize2opsize[size],tmpref,reg));
            end;
        end;
      end;


    procedure tcgx86.a_op_reg_ref(list : TAsmList; Op: TOpCG; size: TCGSize;reg: TRegister; const ref: TReference);
      var
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
        check_register_size(size,reg);
        case op of
          OP_NEG,OP_NOT:
            begin
              if reg<>NR_NO then
                internalerror(200109237);
              list.concat(taicpu.op_ref(TOpCG2AsmOp[op],tcgsize2opsize[size],tmpref));
            end;
          OP_IMUL:
            begin
              { this one needs a load/imul/store, which is the default }
              inherited a_op_ref_reg(list,op,size,tmpref,reg);
            end;
          OP_MUL,OP_DIV,OP_IDIV:
            { special stuff, needs separate handling inside code }
            { generator                                          }
            internalerror(200109238);
          else
            begin
              list.concat(taicpu.op_reg_ref(TOpCG2AsmOp[op],tcgsize2opsize[size],reg,tmpref));
            end;
        end;
      end;


{*************** compare instructructions ****************}

    procedure tcgx86.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
      l : tasmlabel);

{$ifdef x86_64}
      var
        tmpreg : tregister;
{$endif x86_64}
      begin
{$ifdef x86_64}
        { x86_64 only supports signed 32 bits constants directly }
        if (size in [OS_S64,OS_64]) and
            ((a<low(longint)) or (a>high(longint))) then
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
            exit;
          end;
{$endif x86_64}
        if (a = 0) then
          list.concat(taicpu.op_reg_reg(A_TEST,tcgsize2opsize[size],reg,reg))
        else
          list.concat(taicpu.op_const_reg(A_CMP,tcgsize2opsize[size],a,reg));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_const_ref_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;const ref : treference;
      l : tasmlabel);

      var
{$ifdef x86_64}
        tmpreg : tregister;
{$endif x86_64}
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
{$ifdef x86_64}
        { x86_64 only supports signed 32 bits constants directly }
        if (size in [OS_S64,OS_64]) and
           ((a<low(longint)) or (a>high(longint))) then
          begin
            tmpreg:=getintregister(list,size);
            a_load_const_reg(list,size,a,tmpreg);
            a_cmp_reg_ref_label(list,size,cmp_op,tmpreg,tmpref,l);
            exit;
          end;
{$endif x86_64}
        list.concat(taicpu.op_const_ref(A_CMP,TCgSize2OpSize[size],a,tmpref));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;
      reg1,reg2 : tregister;l : tasmlabel);

      begin
        check_register_size(size,reg1);
        check_register_size(size,reg2);
        list.concat(taicpu.op_reg_reg(A_CMP,TCgSize2OpSize[size],reg1,reg2));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_ref_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;const ref: treference; reg : tregister;l : tasmlabel);
      var
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
        check_register_size(size,reg);
        list.concat(taicpu.op_ref_reg(A_CMP,TCgSize2OpSize[size],tmpref,reg));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_cmp_reg_ref_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg : tregister;const ref: treference; l : tasmlabel);
      var
        tmpref  : treference;
      begin
        tmpref:=ref;
        make_simple_ref(list,tmpref);
        check_register_size(size,reg);
        list.concat(taicpu.op_reg_ref(A_CMP,TCgSize2OpSize[size],reg,tmpref));
        a_jmp_cond(list,cmp_op,l);
      end;


    procedure tcgx86.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
      var
        ai : taicpu;
      begin
        if cond=OC_None then
          ai := Taicpu.Op_sym(A_JMP,S_NO,l)
        else
          begin
            ai:=Taicpu.Op_sym(A_Jcc,S_NO,l);
            ai.SetCondition(TOpCmp2AsmCond[cond]);
          end;
        ai.is_jmp:=true;
        list.concat(ai);
      end;


     procedure tcgx86.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
       var
         ai : taicpu;
       begin
         ai := Taicpu.op_sym(A_Jcc,S_NO,l);
         ai.SetCondition(flags_to_cond(f));
         ai.is_jmp := true;
         list.concat(ai);
       end;


    procedure tcgx86.g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags; reg: TRegister);
      var
        ai : taicpu;
        hreg : tregister;
      begin
        hreg:=makeregsize(list,reg,OS_8);
        ai:=Taicpu.op_reg(A_SETcc,S_B,hreg);
        ai.setcondition(flags_to_cond(f));
        list.concat(ai);
        if (reg<>hreg) then
          a_load_reg_reg(list,OS_8,size,hreg,reg);
      end;


     procedure tcgx86.g_flags2ref(list: TAsmList; size: TCgSize; const f: tresflags; const ref: TReference);
       var
         ai : taicpu;
         tmpref  : treference;
       begin
          tmpref:=ref;
          make_simple_ref(list,tmpref);
          if not(size in [OS_8,OS_S8]) then
            a_load_const_ref(list,size,0,tmpref);
          ai:=Taicpu.op_ref(A_SETcc,S_B,tmpref);
          ai.setcondition(flags_to_cond(f));
          list.concat(ai);
       end;


{ ************* concatcopy ************ }

    procedure Tcgx86.g_concatcopy(list:TAsmList;const source,dest:Treference;len:aint);

    const
{$ifdef cpu64bit}
        REGCX=NR_RCX;
        REGSI=NR_RSI;
        REGDI=NR_RDI;
{$else cpu64bit}
        REGCX=NR_ECX;
        REGSI=NR_ESI;
        REGDI=NR_EDI;
{$endif cpu64bit}

    type  copymode=(copy_move,copy_mmx,copy_string);

    var srcref,dstref:Treference;
        r,r0,r1,r2,r3:Tregister;
        helpsize:aint;
        copysize:byte;
        cgsize:Tcgsize;
        cm:copymode;

    begin
      cm:=copy_move;
      helpsize:=12;
      if cs_opt_size in current_settings.optimizerswitches then
        helpsize:=8;
      if (cs_mmx in current_settings.localswitches) and
         not(pi_uses_fpu in current_procinfo.flags) and
         ((len=8) or (len=16) or (len=24) or (len=32)) then
        cm:=copy_mmx;
      if (len>helpsize) then
        cm:=copy_string;
      if (cs_opt_size in current_settings.optimizerswitches) and
         not((len<=16) and (cm=copy_mmx)) then
        cm:=copy_string;
      case cm of
        copy_move:
          begin
            dstref:=dest;
            srcref:=source;
            copysize:=sizeof(aint);
            cgsize:=int_cgsize(copysize);
            while len<>0 do
              begin
                if len<2 then
                  begin
                    copysize:=1;
                    cgsize:=OS_8;
                  end
                else if len<4 then
                  begin
                    copysize:=2;
                    cgsize:=OS_16;
                  end
                else if len<8 then
                  begin
                    copysize:=4;
                    cgsize:=OS_32;
                  end;
                dec(len,copysize);
                r:=getintregister(list,cgsize);
                a_load_ref_reg(list,cgsize,cgsize,srcref,r);
                a_load_reg_ref(list,cgsize,cgsize,r,dstref);
                inc(srcref.offset,copysize);
                inc(dstref.offset,copysize);
              end;
          end;
        copy_mmx:
          begin
            dstref:=dest;
            srcref:=source;
            r0:=getmmxregister(list);
            a_loadmm_ref_reg(list,OS_M64,OS_M64,srcref,r0,nil);
            if len>=16 then
              begin
                inc(srcref.offset,8);
                r1:=getmmxregister(list);
                a_loadmm_ref_reg(list,OS_M64,OS_M64,srcref,r1,nil);
              end;
            if len>=24 then
              begin
                inc(srcref.offset,8);
                r2:=getmmxregister(list);
                a_loadmm_ref_reg(list,OS_M64,OS_M64,srcref,r2,nil);
              end;
            if len>=32 then
              begin
                inc(srcref.offset,8);
                r3:=getmmxregister(list);
                a_loadmm_ref_reg(list,OS_M64,OS_M64,srcref,r3,nil);
              end;
            a_loadmm_reg_ref(list,OS_M64,OS_M64,r0,dstref,nil);
            if len>=16 then
              begin
                inc(dstref.offset,8);
                a_loadmm_reg_ref(list,OS_M64,OS_M64,r1,dstref,nil);
              end;
            if len>=24 then
              begin
                inc(dstref.offset,8);
                a_loadmm_reg_ref(list,OS_M64,OS_M64,r2,dstref,nil);
              end;
            if len>=32 then
              begin
                inc(dstref.offset,8);
                a_loadmm_reg_ref(list,OS_M64,OS_M64,r3,dstref,nil);
              end;
          end
        else {copy_string, should be a good fallback in case of unhandled}
          begin
            getcpuregister(list,REGDI);
            a_loadaddr_ref_reg(list,dest,REGDI);
            getcpuregister(list,REGSI);
            a_loadaddr_ref_reg(list,source,REGSI);

            getcpuregister(list,REGCX);
{$ifdef i386}
            list.concat(Taicpu.op_none(A_CLD,S_NO));
{$endif i386}
            if cs_opt_size in current_settings.optimizerswitches  then
              begin
                a_load_const_reg(list,OS_INT,len,REGCX);
                list.concat(Taicpu.op_none(A_REP,S_NO));
                list.concat(Taicpu.op_none(A_MOVSB,S_NO));
              end
            else
              begin
                helpsize:=len div sizeof(aint);
                len:=len mod sizeof(aint);
                if helpsize>1 then
                  begin
                    a_load_const_reg(list,OS_INT,helpsize,REGCX);
                    list.concat(Taicpu.op_none(A_REP,S_NO));
                  end;
                if helpsize>0 then
                  begin
{$ifdef cpu64bit}
                    if sizeof(aint)=8 then
                      list.concat(Taicpu.op_none(A_MOVSQ,S_NO))
                    else
{$endif cpu64bit}
                      list.concat(Taicpu.op_none(A_MOVSD,S_NO));
                  end;
                if len>=4 then
                  begin
                    dec(len,4);
                    list.concat(Taicpu.op_none(A_MOVSD,S_NO));
                  end;
                if len>=2 then
                  begin
                    dec(len,2);
                    list.concat(Taicpu.op_none(A_MOVSW,S_NO));
                  end;
                if len=1 then
                  list.concat(Taicpu.op_none(A_MOVSB,S_NO));
              end;
            ungetcpuregister(list,REGCX);
            ungetcpuregister(list,REGSI);
            ungetcpuregister(list,REGDI);
          end;
        end;
    end;


{****************************************************************************
                              Entry/Exit Code Helpers
****************************************************************************}

    procedure tcgx86.g_releasevaluepara_openarray(list : TAsmList;const l:tlocation);
      begin
        if (use_fixed_stack) then
          begin
            inherited g_releasevaluepara_openarray(list,l);
            exit;
          end;
        { Nothing to release }
      end;


    procedure tcgx86.g_profilecode(list : TAsmList);

      var
        pl           : tasmlabel;
        mcountprefix : String[4];

      begin
        case target_info.system of
        {$ifndef NOTARGETWIN}
           system_i386_win32,
        {$endif}
           system_i386_freebsd,
           system_i386_netbsd,
//         system_i386_openbsd,
           system_i386_wdosx :
             begin
                Case target_info.system Of
                 system_i386_freebsd : mcountprefix:='.';
                 system_i386_netbsd : mcountprefix:='__';
//               system_i386_openbsd : mcountprefix:='.';
                else
                 mcountPrefix:='';
                end;
                current_asmdata.getaddrlabel(pl);
                new_section(list,sec_data,lower(current_procinfo.procdef.mangledname),sizeof(aint));
                list.concat(Tai_label.Create(pl));
                list.concat(Tai_const.Create_32bit(0));
                new_section(list,sec_code,lower(current_procinfo.procdef.mangledname),0);
                list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EDX));
                list.concat(Taicpu.Op_sym_ofs_reg(A_MOV,S_L,pl,0,NR_EDX));
                a_call_name(list,target_info.Cprefix+mcountprefix+'mcount');
                list.concat(Taicpu.Op_reg(A_POP,S_L,NR_EDX));
             end;

           system_i386_linux:
             a_call_name(list,target_info.Cprefix+'mcount');

           system_i386_go32v2,system_i386_watcom:
             begin
               a_call_name(list,'MCOUNT');
             end;
           system_x86_64_linux:
             begin
               a_call_name(list,'mcount');
             end;
        end;
      end;


    procedure tcgx86.g_stackpointer_alloc(list : TAsmList;localsize : longint);
{$ifdef x86}
{$ifndef NOTARGETWIN}
      var
        href : treference;
        i : integer;
        again : tasmlabel;
{$endif NOTARGETWIN}
{$endif x86}
      begin
        if localsize>0 then
         begin
{$ifdef i386}
{$ifndef NOTARGETWIN}
           { windows guards only a few pages for stack growing,
             so we have to access every page first              }
           if (target_info.system in [system_i386_win32,system_i386_wince]) and
              (localsize>=winstackpagesize) then
             begin
               if localsize div winstackpagesize<=5 then
                 begin
                    list.concat(Taicpu.Op_const_reg(A_SUB,S_L,localsize-4,NR_ESP));
                    for i:=1 to localsize div winstackpagesize do
                      begin
                         reference_reset_base(href,NR_ESP,localsize-i*winstackpagesize);
                         list.concat(Taicpu.op_reg_ref(A_MOV,S_L,NR_EAX,href));
                      end;
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EAX));
                 end
               else
                 begin
                    current_asmdata.getjumplabel(again);
                    getcpuregister(list,NR_EDI);
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EDI));
                    list.concat(Taicpu.op_const_reg(A_MOV,S_L,localsize div winstackpagesize,NR_EDI));
                    a_label(list,again);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,NR_ESP));
                    list.concat(Taicpu.op_reg(A_PUSH,S_L,NR_EAX));
                    list.concat(Taicpu.op_reg(A_DEC,S_L,NR_EDI));
                    a_jmp_cond(list,OC_NE,again);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_L,localsize mod winstackpagesize - 4,NR_ESP));
                    reference_reset_base(href,NR_ESP,localsize-4);
                    list.concat(Taicpu.op_ref_reg(A_MOV,S_L,href,NR_EDI));
                    ungetcpuregister(list,NR_EDI);
                 end
             end
           else
{$endif NOTARGETWIN}
{$endif i386}
{$ifdef x86_64}
{$ifndef NOTARGETWIN}
           { windows guards only a few pages for stack growing,
             so we have to access every page first              }
           if (target_info.system=system_x86_64_win64) and
              (localsize>=winstackpagesize) then
             begin
               if localsize div winstackpagesize<=5 then
                 begin
                    list.concat(Taicpu.Op_const_reg(A_SUB,S_Q,localsize,NR_RSP));
                    for i:=1 to localsize div winstackpagesize do
                      begin
                         reference_reset_base(href,NR_RSP,localsize-i*winstackpagesize+4);
                         list.concat(Taicpu.op_reg_ref(A_MOV,S_L,NR_EAX,href));
                      end;
                    reference_reset_base(href,NR_RSP,0);
                    list.concat(Taicpu.op_reg_ref(A_MOV,S_L,NR_EAX,href));
                 end
               else
                 begin
                    current_asmdata.getjumplabel(again);
                    getcpuregister(list,NR_R10);
                    list.concat(Taicpu.op_const_reg(A_MOV,S_Q,localsize div winstackpagesize,NR_R10));
                    a_label(list,again);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_Q,winstackpagesize,NR_RSP));
                    reference_reset_base(href,NR_RSP,0);
                    list.concat(Taicpu.op_reg_ref(A_MOV,S_L,NR_EAX,href));
                    list.concat(Taicpu.op_reg(A_DEC,S_Q,NR_R10));
                    a_jmp_cond(list,OC_NE,again);
                    list.concat(Taicpu.op_const_reg(A_SUB,S_Q,localsize mod winstackpagesize,NR_RSP));
                    ungetcpuregister(list,NR_R10);
                 end
             end
           else
{$endif NOTARGETWIN}
{$endif x86_64}
            list.concat(Taicpu.Op_const_reg(A_SUB,tcgsize2opsize[OS_ADDR],localsize,NR_STACK_POINTER_REG));
         end;
      end;


    procedure tcgx86.g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);
      var
        stackmisalignment: longint;
      begin
{$ifdef i386}
        { interrupt support for i386 }
        if (po_interrupt in current_procinfo.procdef.procoptions) and
           { this messes up stack alignment }
           (target_info.system <> system_i386_darwin) then
          begin
            { .... also the segment registers }
            list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_GS));
            list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_FS));
            list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_ES));
            list.concat(Taicpu.Op_reg(A_PUSH,S_W,NR_DS));
            { save the registers of an interrupt procedure }
            list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EDI));
            list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_ESI));
            list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EDX));
            list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_ECX));
            list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EBX));
            list.concat(Taicpu.Op_reg(A_PUSH,S_L,NR_EAX));
          end;
{$endif i386}

        { save old framepointer }
        if not nostackframe then
          begin
            { return address }
            stackmisalignment := sizeof(aint);
            list.concat(tai_regalloc.alloc(current_procinfo.framepointer,nil));
            if current_procinfo.framepointer=NR_STACK_POINTER_REG then
              CGmessage(cg_d_stackframe_omited)
            else
              begin
                { push <frame_pointer> }
                inc(stackmisalignment,sizeof(aint));
                include(rg[R_INTREGISTER].preserved_by_proc,RS_FRAME_POINTER_REG);
                list.concat(Taicpu.op_reg(A_PUSH,tcgsize2opsize[OS_ADDR],NR_FRAME_POINTER_REG));
                { Return address and FP are both on stack }
                current_asmdata.asmcfi.cfa_def_cfa_offset(list,2*sizeof(aint));
                current_asmdata.asmcfi.cfa_offset(list,NR_FRAME_POINTER_REG,-(2*sizeof(aint)));
                list.concat(Taicpu.op_reg_reg(A_MOV,tcgsize2opsize[OS_ADDR],NR_STACK_POINTER_REG,NR_FRAME_POINTER_REG));
                current_asmdata.asmcfi.cfa_def_cfa_register(list,NR_FRAME_POINTER_REG);
              end;

            { allocate stackframe space }
            if (localsize<>0) or
               ((target_info.system in [system_i386_darwin,system_x86_64_win64]) and
                (stackmisalignment <> 0) and
                ((pi_do_call in current_procinfo.flags) or
                 (po_assembler in current_procinfo.procdef.procoptions))) then
              begin
                if (target_info.system in [system_i386_darwin,system_x86_64_win64]) then
                  localsize := align(localsize+stackmisalignment,16)-stackmisalignment;
                cg.g_stackpointer_alloc(list,localsize);
                if current_procinfo.framepointer=NR_STACK_POINTER_REG then
                  current_asmdata.asmcfi.cfa_def_cfa_offset(list,localsize+sizeof(aint));
              end;
          end;
      end;


    { produces if necessary overflowcode }
    procedure tcgx86.g_overflowcheck(list: TAsmList; const l:tlocation;def:tdef);
      var
         hl : tasmlabel;
         ai : taicpu;
         cond : TAsmCond;
      begin
         if not(cs_check_overflow in current_settings.localswitches) then
          exit;
         current_asmdata.getjumplabel(hl);
         if not ((def.typ=pointerdef) or
                ((def.typ=orddef) and
                 (torddef(def).ordtype in [u64bit,u16bit,u32bit,u8bit,uchar,
                                       bool8bit,bool16bit,bool32bit,bool64bit]))) then
           cond:=C_NO
         else
           cond:=C_NB;
         ai:=Taicpu.Op_Sym(A_Jcc,S_NO,hl);
         ai.SetCondition(cond);
         ai.is_jmp:=true;
         list.concat(ai);

         a_call_name(list,'FPC_OVERFLOW');
         a_label(list,hl);
      end;


end.
