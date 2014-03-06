{
    Copyright (c) 2010, 2013 by Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the LLVM high level code generator

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
unit hlcgllvm;

{$i fpcdefs.inc}

interface

uses
  globtype,
  aasmbase,aasmdata,
  symbase,symconst,symtype,symdef,symsym,
  cpubase, hlcgobj, cgbase, cgutils, parabase;

  type

    { thlcgllvm }

    thlcgllvm = class(thlcgobj)
      constructor create;

      function a_call_name(list : TAsmList;pd : tprocdef;const s : TSymStr; forceresdef: tdef; weak: boolean): tcgpara;override;
      procedure a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister); override;

      procedure a_load_const_reg(list : TAsmList;tosize : tdef;a : tcgint;register : tregister);override;
      procedure a_load_const_ref(list: TAsmList; tosize: tdef; a: tcgint; const ref: treference);override;
      procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
      procedure a_load_reg_reg(list : TAsmList;fromsize, tosize : tdef;reg1,reg2 : tregister);override;
      procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
      procedure a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference); override;
      procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

      procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister); override;
      procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister); override;

      procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister); override;
      procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister); override;
      procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation); override;
      procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation); override;

      procedure a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel); override;
      procedure a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;

      procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;

      procedure g_concatcopy(list : TAsmList;size: tdef; const source,dest : treference);override;

      procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister); override;
      procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference); override;
      procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister); override;

      procedure gen_proc_symbol(list: TAsmList); override;
      procedure gen_proc_symbol_end(list: TAsmList); override;
      procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean); override;
      procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean); override;

      procedure g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef); override;
      procedure g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;var ovloc : tlocation); override;

      procedure a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle); override;
      procedure a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle); override;
      procedure a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle); override;

      procedure gen_load_cgpara_loc(list: TAsmList; vardef: tdef; const para: TCGPara; var destloc: tlocation; reusepara: boolean); override;
{$ifdef cpuflags}
      { llvm doesn't have flags, but cpuflags is defined in case the real cpu
        has flags and we have to override the abstract methods to prevent
        warnings }
      procedure a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel); override;
      procedure g_flags2reg(list: TAsmList; size: tdef; const f: tresflags; reg: TRegister); override;
      procedure g_flags2ref(list: TAsmList; size: tdef; const f: tresflags; const ref: TReference); override;
{$endif cpuflags}

      { unimplemented or unnecessary routines }
      procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: tdef; src, dst: tregister); override;
      procedure g_stackpointer_alloc(list: TAsmList; size: longint); override;
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint); override;
      procedure g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint); override;
      procedure g_local_unwind(list: TAsmList; l: TAsmLabel); override;

      procedure varsym_set_localloc(list: TAsmList; vs: tabstractnormalvarsym); override;
      procedure paravarsym_set_initialloc_to_paraloc(vs: tparavarsym); override;
    protected
      { def is the type of the data stored in memory pointed to by ref, not
        a pointer to this type }
      function make_simple_ref(list: TAsmList; const ref: treference; def: tdef): treference;
    end;

  procedure create_hlcodegen;


implementation

  uses
    verbose,cutils,cclasses,globals,fmodule,constexp,
    defutil,llvmdef,llvmsym,
    aasmtai,aasmcpu,
    aasmllvm,llvmbase,tgllvm,
    symtable,
    paramgr,
    procinfo,cpuinfo,tgobj,cgobj,cgllvm,cghlcpu;

  const
    topcg2llvmop: array[topcg] of tllvmop =
     { OP_NONE  OP_MOVE     OP_ADD  OP_AND  OP_DIV   OP_IDIV  OP_IMUL OP_MUL }
      (la_none, la_none, la_add, la_and, la_udiv, la_sdiv, la_mul, la_mul,
     { OP_NEG   OP_NOT   OP_OR  OP_SAR   OP_SHL  OP_SHR   OP_SUB  OP_XOR }
       la_none, la_none, la_or, la_ashr, la_shl, la_lshr, la_sub, la_xor,
     { OP_ROL   OP_ROR }
       la_none, la_none);


  constructor thlcgllvm.create;
    begin
      inherited
    end;


  function thlcgllvm.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef; weak: boolean): tcgpara;
    begin
      { todo: we also need the parameter locations here for llvm! }
      list.concat(tai_comment.create(strpnew('call '+s)));
      result:=get_call_result_cgpara(pd,forceresdef);
    end;


  procedure thlcgllvm.a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister);
    begin
      internalerror(2012042824);
    end;


  procedure thlcgllvm.a_load_const_reg(list: TAsmList; tosize: tdef; a: tcgint; register: tregister);
    begin
      list.concat(taillvm.op_reg_size_const_size(la_bitcast,register,tosize,a,tosize))
    end;


  procedure thlcgllvm.a_load_const_ref(list: TAsmList; tosize: tdef; a: tcgint; const ref: treference);
    var
      sref: treference;
    begin
      sref:=make_simple_ref(list,ref,tosize);
      list.concat(taillvm.op_size_const_size_ref(la_store,tosize,a,getpointerdef(tosize),sref))
    end;


  function def2intdef(fromsize, tosize: tdef): tdef;
    begin
      { we cannot zero-extend from/to anything but ordinal/enum
        types }
      if not(tosize.typ in [orddef,enumdef]) then
        internalerror(2014012305);
      { will give an internalerror if def_cgsize() returns OS_NO, which is
        what we want }
      result:=cgsize_orddef(def_cgsize(fromsize));
    end;


  procedure thlcgllvm.a_load_reg_ref(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    var
      tmpref,
      sref: treference;
      hreg,
      hreg2: tregister;
      tmpsize: tdef;
    begin
      sref:=make_simple_ref(list,ref,tosize);
      hreg:=register;
      (* typecast the pointer to the value instead of the value itself if
        they have the same size but are of different kinds, because we can't
        e.g. typecast a loaded <{i32, i32}> to an i64 *)
      if (llvmaggregatetype(fromsize) or
          llvmaggregatetype(tosize)) and
         (fromsize<>tosize) then
        begin
          if fromsize.size>tosize.size then
            begin
              { if source size is larger than the target size, we have to
                truncate it before storing. Unfortunately, we cannot truncate
                records (nor bitcast them to integers), so we first have to
                store them to memory and then bitcast the pointer to them
              }
              if fromsize.typ in [arraydef,recorddef] then
                begin
                  { store struct/array-in-register to memory }
                  tmpsize:=def2intdef(fromsize,tosize);
                  tg.gethltemp(list,fromsize,fromsize.size,tt_normal,tmpref);
                  a_load_reg_ref(list,fromsize,fromsize,register,tmpref);
                  { typecast pointer to memory into pointer to integer type }
                  hreg:=getaddressregister(list,getpointerdef(tmpsize));
                  a_loadaddr_ref_reg(list,fromsize,getpointerdef(tmpsize),tmpref,hreg);
                  reference_reset_base(sref,hreg,0,tmpref.alignment);
                  { load the integer from the temp into the destination }
                  a_load_ref_ref(list,tmpsize,tosize,tmpref,sref);
                  tg.ungettemp(list,tmpref);
                end
              else
                begin
                  tmpsize:=def2intdef(tosize,fromsize);
                  hreg:=getintregister(list,tmpsize);
                  { truncate the integer }
                  a_load_reg_reg(list,fromsize,tmpsize,register,hreg);
                  { store it to memory (it will now be of the same size as the
                    struct, and hence another path will be followed in this
                    method) }
                  a_load_reg_ref(list,tmpsize,tosize,hreg,sref);
                end;
                exit;
            end
          else
            begin
              hreg2:=getaddressregister(list,getpointerdef(fromsize));
              a_loadaddr_ref_reg(list,tosize,getpointerdef(fromsize),sref,hreg2);
              reference_reset_base(sref,hreg2,0,sref.alignment);
              tosize:=fromsize;
            end;
        end
      else if fromsize<>tosize then
        begin
          hreg:=getregisterfordef(list,tosize);
          a_load_reg_reg(list,fromsize,tosize,register,hreg);
        end;
      list.concat(taillvm.op_size_reg_size_ref(la_store,tosize,hreg,getpointerdef(tosize),sref));
    end;


  procedure thlcgllvm.a_load_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    var
      fromregtyp,
      toregtyp: tregistertype;
      op: tllvmop;
    begin
      fromregtyp:=def2regtyp(fromsize);
      toregtyp:=def2regtyp(tosize);
      { int to pointer or vice versa }
      if (fromregtyp=R_ADDRESSREGISTER) and
         (toregtyp=R_INTREGISTER) then
        op:=la_ptrtoint
      else if (fromregtyp=R_INTREGISTER) and
         (toregtyp=R_ADDRESSREGISTER) then
        op:=la_inttoptr
      { int to int or ptr to ptr: need zero/sign extension, or plain bitcast? }
      else if tosize.size<>fromsize.size then
        begin
          if tosize.size<fromsize.size then
            op:=la_trunc
          else if is_signed(fromsize) then
            { fromsize is signed -> sign extension }
            op:=la_sext
          else
            op:=la_zext;
        end
      else
        op:=la_bitcast;
      { reg2 = bitcast fromsize reg1 to tosize }
      list.concat(taillvm.op_reg_size_reg_size(op,reg2,fromsize,reg1,tosize));
    end;


  procedure thlcgllvm.a_load_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    var
      tmpref,
      sref: treference;
      hreg: tregister;
      tmpsize: tdef;
    begin
      sref:=make_simple_ref(list,ref,fromsize);
      { "named register"? }
      if sref.refaddr=addr_full then
        begin
          { can't bitcast records/arrays }
          if (llvmaggregatetype(fromsize) or
              llvmaggregatetype(tosize)) and
             (fromsize<>tosize) then
            begin
              tg.gethltemp(list,fromsize,fromsize.size,tt_normal,tmpref);
              list.concat(taillvm.op_size_ref_size_ref(la_store,fromsize,sref,getpointerdef(fromsize),tmpref));
              a_load_ref_reg(list,fromsize,tosize,tmpref,register);
              tg.ungettemp(list,tmpref);
            end
          else
            list.concat(taillvm.op_reg_size_ref_size(la_bitcast,register,fromsize,sref,tosize))
        end
      else
        begin
          if ((fromsize.typ in [arraydef,recorddef]) or
              (tosize.typ in [arraydef,recorddef])) and
             (fromsize<>tosize) then
            begin
              if fromsize.size<tosize.size then
                begin
                  { if the target size is larger than the source size, we
                    have to perform the zero-extension using an integer type
                    (can't zero-extend a record/array) }
                  if fromsize.typ in [arraydef,recorddef] then
                    begin
                      { typecast the pointer to the struct into a pointer to an
                        integer of equal size }
                      tmpsize:=def2intdef(fromsize,tosize);
                      hreg:=getaddressregister(list,getpointerdef(tmpsize));
                      a_loadaddr_ref_reg(list,fromsize,getpointerdef(tmpsize),sref,hreg);
                      reference_reset_base(sref,hreg,0,sref.alignment);
                      { load that integer }
                      a_load_ref_reg(list,tmpsize,tosize,sref,register);
                    end
                  else
                    begin
                      { load the integer into an integer memory location with
                        the same size as the struct (the integer should be
                        unsigned, we don't want sign extensions here) }
                      if is_signed(fromsize) then
                        internalerror(2014012309);
                      tmpsize:=def2intdef(tosize,fromsize);
                      tg.gethltemp(list,tmpsize,tmpsize.size,tt_normal,tmpref);
                      { typecast the struct-sized integer location into the
                        struct type }
                      a_load_ref_ref(list,fromsize,tmpsize,sref,tmpref);
                      { load the struct in the register }
                      a_load_ref_reg(list,tmpsize,tosize,tmpref,register);
                      tg.ungettemp(list,tmpref);
                    end;
                  exit;
                end
              else
                begin
                  (* typecast the pointer to the value instead of the value
                     itself if they have the same size but are of different
                     kinds, because we can't e.g. typecast a loaded <{i32, i32}>
                     to an i64 *)
                  hreg:=getaddressregister(list,getpointerdef(tosize));
                  a_loadaddr_ref_reg(list,fromsize,getpointerdef(tosize),sref,hreg);
                  reference_reset_base(sref,hreg,0,sref.alignment);
                  fromsize:=tosize;
                end;
            end;
          hreg:=register;
          if fromsize<>tosize then
            hreg:=getregisterfordef(list,fromsize);
          list.concat(taillvm.op_reg_size_ref(la_load,hreg,getpointerdef(fromsize),sref));
          if hreg<>register then
            a_load_reg_reg(list,fromsize,tosize,hreg,register);
        end;
    end;


  procedure thlcgllvm.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
    var
      sdref: treference;
    begin
      if (fromsize=tosize) and
         (sref.refaddr=addr_full) then
        begin
          sdref:=make_simple_ref(list,dref,tosize);
          list.concat(taillvm.op_size_ref_size_ref(la_store,fromsize,sref,getpointerdef(tosize),sdref));
        end
      else
        inherited
    end;


  procedure thlcgllvm.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    var
      sref: treference;
    begin
      { can't take the address of a 'named register' }
      if ref.refaddr=addr_full then
        internalerror(2013102306);
      sref:=make_simple_ref(list,ref,fromsize);
      list.concat(taillvm.op_reg_size_ref_size(la_bitcast,r,getpointerdef(fromsize),sref,tosize));
    end;


  procedure thlcgllvm.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister);
    begin
      a_op_const_reg_reg(list,op,size,a,reg,reg);
    end;


  procedure thlcgllvm.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister);
    var
      tmpreg: tregister;
    begin
      if (def2regtyp(size)=R_INTREGISTER) and
         (topcg2llvmop[op]<>la_none) then
        list.concat(taillvm.op_reg_size_reg_const(topcg2llvmop[op],dst,size,src,a))
      else
        begin
          { default implementation is not SSA-safe }
          tmpreg:=getregisterfordef(list,size);
          a_load_const_reg(list,size,a,tmpreg);
          a_op_reg_reg_reg(list,op,size,tmpreg,src,dst);
        end;
    end;


  procedure thlcgllvm.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister);
    var
      orgdst,
      tmpreg1,
      tmpreg2,
      tmpreg3: tregister;
      opsize: tdef;
    begin
      orgdst:=dst;
      opsize:=size;
      { always perform using integer registers, because math operations on
        pointers are not supported (except via getelementptr, possible future
        optimization) }
      if def2regtyp(size)=R_ADDRESSREGISTER then
        begin
          opsize:=ptruinttype;

          tmpreg1:=getintregister(list,ptruinttype);
          a_load_reg_reg(list,size,ptruinttype,src1,tmpreg1);
          src1:=tmpreg1;

          tmpreg1:=getintregister(list,ptruinttype);
          a_load_reg_reg(list,size,ptruinttype,src2,tmpreg1);
          src2:=tmpreg1;

          dst:=getintregister(list,ptruinttype);
        end;
     if topcg2llvmop[op]<>la_none then
       list.concat(taillvm.op_reg_size_reg_reg(topcg2llvmop[op],dst,opsize,src2,src1))
     else
       begin
         case op of
           OP_NEG:
             { %dst = sub size 0, %src1 }
             list.concat(taillvm.op_reg_size_const_reg(la_sub,dst,opsize,0,src1));
           OP_NOT:
             { %dst = xor size -1, %src1 }
             list.concat(taillvm.op_reg_size_const_reg(la_xor,dst,opsize,-1,src1));
           OP_ROL:
             begin
               tmpreg1:=getintregister(list,opsize);
               tmpreg2:=getintregister(list,opsize);
               tmpreg3:=getintregister(list,opsize);
               { tmpreg1 := tcgsize2size[size] - src1 }
               list.concat(taillvm.op_reg_size_const_reg(la_sub,tmpreg1,opsize,opsize.size,src1));
               { tmpreg2 := src2 shr tmpreg1 }
               a_op_reg_reg_reg(list,OP_SHR,opsize,tmpreg1,src2,tmpreg2);
               { tmpreg3 := src2 shl src1 }
               a_op_reg_reg_reg(list,OP_SHL,opsize,src1,src2,tmpreg3);
               { dst := tmpreg2 or tmpreg3 }
               a_op_reg_reg_reg(list,OP_OR,opsize,tmpreg2,tmpreg3,dst);
             end;
           OP_ROR:
             begin
               tmpreg1:=getintregister(list,size);
               tmpreg2:=getintregister(list,size);
               tmpreg3:=getintregister(list,size);
               { tmpreg1 := tcgsize2size[size] - src1 }
               list.concat(taillvm.op_reg_size_const_reg(la_sub,tmpreg1,opsize,opsize.size,src1));
               { tmpreg2 := src2 shl tmpreg1 }
               a_op_reg_reg_reg(list,OP_SHL,opsize,tmpreg1,src2,tmpreg2);
               { tmpreg3 := src2 shr src1 }
               a_op_reg_reg_reg(list,OP_SHR,opsize,src1,src2,tmpreg3);
               { dst := tmpreg2 or tmpreg3 }
               a_op_reg_reg_reg(list,OP_OR,opsize,tmpreg2,tmpreg3,dst);
             end;
           else
             internalerror(2010081310);
         end;
       end;
     if dst<>orgdst then
       a_load_reg_reg(list,opsize,size,dst,orgdst);
   end;


  procedure thlcgllvm.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister);
    begin
       a_op_reg_reg_reg(list,op,size,reg1,reg2,reg2);
    end;


  procedure thlcgllvm.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister; setflags: boolean; var ovloc: tlocation);
    begin
      if not setflags then
        begin
          inherited;
          exit;
        end;
      { use xxx.with.overflow intrinsics }
      internalerror(2012111102);
    end;


  procedure thlcgllvm.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
    begin
      if not setflags then
        begin
          inherited;
          exit;
        end;
      { use xxx.with.overflow intrinsics }
      internalerror(2012111103);
    end;


  procedure thlcgllvm.a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
    var
      tmpreg : tregister;
      invert: boolean;
      falselab, tmplab: tasmlabel;
    begin
      { since all comparisons return their results in a register, we'll often
        get comparisons against true/false -> optimise }
      if (size=pasbool8type) and
         (cmp_op in [OC_EQ,OC_NE]) then
        begin
          case cmp_op of
            OC_EQ:
              invert:=a=0;
            OC_NE:
              invert:=a=1;
            end;
          current_asmdata.getjumplabel(falselab);
          if invert then
            begin
              tmplab:=l;
              l:=falselab;
              falselab:=tmplab;
            end;
          list.concat(taillvm.op_size_reg_lab_lab(la_br,pasbool8type,reg,l,falselab));
          a_label(list,falselab);
          exit;
        end;
      tmpreg:=getregisterfordef(list,size);
      a_load_const_reg(list,size,a,tmpreg);
      a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
    end;


  procedure thlcgllvm.a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
    var
      resreg: tregister;
      falselab: tasmlabel;
    begin
      if getregtype(reg1)<>getregtype(reg2) then
        internalerror(2012111105);
      resreg:=getintregister(list,pasbool8type);
      current_asmdata.getjumplabel(falselab);
      { invert order of registers. In FPC, cmp_reg_reg(reg1,reg2) means that
        e.g. OC_GT is true if "subl %reg1,%reg2" in x86 AT&T is >0. In LLVM,
        OC_GT is true if op1>op2 }
      list.concat(taillvm.op_reg_cond_size_reg_reg(la_icmp,resreg,cmp_op,size,reg2,reg1));
      list.concat(taillvm.op_size_reg_lab_lab(la_br,pasbool8type,resreg,l,falselab));
      a_label(list,falselab);
    end;


  procedure thlcgllvm.a_jmp_always(list: TAsmList; l: tasmlabel);
    begin
      { implement in tcg because required by the overridden a_label; doesn't use
        any high level stuff anyway }
      cg.a_jmp_always(list,l);
    end;


  procedure thlcgllvm.g_concatcopy(list: TAsmList; size: tdef; const source, dest: treference);
    begin
      a_load_ref_ref(list,size,size,source,dest);
    end;


  procedure thlcgllvm.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister);
    var
       tmpreg: tregister;
       href: treference;
       fromcompcurr,
       tocompcurr: boolean;
     begin
       { comp and currency are handled by the x87 in this case. They cannot
         be represented directly in llvm, and llvmdef translates them into i64
         (since that's their storage size and internally they also are int64).
         Solve this by changing the type to s80real once they are loaded into
         a register. }
       fromcompcurr:=tfloatdef(fromsize).floattype in [s64comp,s64currency];
       tocompcurr:=tfloatdef(tosize).floattype in [s64comp,s64currency];
       if tocompcurr then
         tosize:=s80floattype;
       href:=make_simple_ref(list,ref,fromsize);
       { don't generate different code for loading e.g. extended into cextended,
         but to take care of loading e.g. comp (=int64) into double }
       if (fromsize.size<>tosize.size) then
         tmpreg:=getfpuregister(list,fromsize)
       else
         tmpreg:=reg;
       { %tmpreg = load size* %ref }
       list.concat(taillvm.op_reg_size_ref(la_load,tmpreg,getpointerdef(fromsize),href));
       if tmpreg<>reg then
         if fromcompcurr then
           { treat as extended as long as it's in a register }
           list.concat(taillvm.op_reg_size_reg_size(la_sitofp,reg,fromsize,tmpreg,tosize))
         else
           a_loadfpu_reg_reg(list,fromsize,tosize,tmpreg,reg);
     end;


  procedure thlcgllvm.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference);
    var
       tmpreg: tregister;
       href: treference;
       fromcompcurr,
       tocompcurr: boolean;
     begin
       { see comment in a_loadfpu_ref_reg }
       fromcompcurr:=tfloatdef(fromsize).floattype in [s64comp,s64currency];
       tocompcurr:=tfloatdef(tosize).floattype in [s64comp,s64currency];
       if fromcompcurr then
         fromsize:=s80floattype;
       href:=make_simple_ref(list,ref,tosize);
       { don't generate different code for loading e.g. extended into cextended,
         but to take care of storing e.g. comp (=int64) into double  }
       if (fromsize.size<>tosize.size) then
         begin
           tmpreg:=getfpuregister(list,tosize);
           if tocompcurr then
             { store back an int64 rather than an extended }
             list.concat(taillvm.op_reg_size_reg_size(la_fptosi,tmpreg,fromsize,reg,tosize))
           else
             a_loadfpu_reg_reg(list,fromsize,tosize,reg,tmpreg);
         end
       else
         tmpreg:=reg;
       { store tosize tmpreg, tosize* href }
       list.concat(taillvm.op_size_reg_size_ref(la_store,tosize,tmpreg,getpointerdef(tosize),href));
     end;


  procedure thlcgllvm.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    var
      op: tllvmop;
      intfromsize,
      inttosize: longint;
    begin
      { treat comp and currency as extended in registers (see comment at start
        of a_loadfpu_ref_reg) }
      if tfloatdef(fromsize).floattype in [s64comp,s64currency] then
        fromsize:=sc80floattype;
      if tfloatdef(tosize).floattype in [s64comp,s64currency] then
        tosize:=sc80floattype;
      { at the value level, s80real and sc80real are the same }
      if fromsize<>s80floattype then
        intfromsize:=fromsize.size
      else
        intfromsize:=sc80floattype.size;
      if tosize<>s80floattype then
        inttosize:=tosize.size
      else
        inttosize:=sc80floattype.size;

      if intfromsize<inttosize then
        op:=la_fpext
       else if intfromsize>inttosize then
        op:=la_fptrunc
      else
        op:=la_bitcast;
      { reg2 = bitcast fromllsize reg1 to tollsize }
      list.concat(taillvm.op_reg_size_reg_size(op,reg2,fromsize,reg1,tosize));
    end;


  procedure thlcgllvm.gen_proc_symbol(list: TAsmList);
    var
      item: TCmdStrListItem;
      mangledname: TSymStr;
      asmsym: tasmsymbol;
    begin
      item:=TCmdStrListItem(current_procinfo.procdef.aliasnames.first);
      mangledname:=current_procinfo.procdef.mangledname;
      { predefine the real function name as local/global, so the aliases can
        refer to the symbol and get the binding correct }
      if (cs_profile in current_settings.moduleswitches) or
         (po_global in current_procinfo.procdef.procoptions) then
        asmsym:=current_asmdata.DefineAsmSymbol(mangledname,AB_GLOBAL,AT_FUNCTION)
      else
        asmsym:=current_asmdata.DefineAsmSymbol(mangledname,AB_LOCAL,AT_FUNCTION);
      while assigned(item) do
        begin
          if mangledname<>item.Str then
            list.concat(taillvmalias.create(asmsym,item.str,current_procinfo.procdef,llv_default,lll_default));
          item:=TCmdStrListItem(item.next);
        end;
      list.concat(taillvmdecl.create(asmsym,current_procinfo.procdef));
    end;


  procedure thlcgllvm.gen_proc_symbol_end(list: TAsmList);
    begin
      list.concat(Tai_symbol_end.Createname(current_procinfo.procdef.mangledname));
      { todo: darwin main proc, or handle in other way? }
    end;


  procedure thlcgllvm.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
    begin
      list.concatlist(ttgllvm(tg).alloclist)
      { rest: todo }
    end;


  procedure thlcgllvm.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
    var
      retdef: tdef;
    begin
      if current_procinfo.procdef.proctypeoption in [potype_constructor,potype_class_constructor] then
        if is_implicit_pointer_object_type(current_procinfo.procdef.struct) then
          retdef:=current_procinfo.procdef.struct
        else
          retdef:=getpointerdef(current_procinfo.procdef.struct)
      else
        retdef:=current_procinfo.procdef.returndef;

      if is_void(retdef) then
        list.concat(taillvm.op_size(la_ret,retdef))
      else
        begin
          case current_procinfo.procdef.funcretloc[calleeside].location^.loc of
            LOC_REGISTER,
            LOC_FPUREGISTER:
              list.concat(taillvm.op_size_reg(la_ret,retdef,current_procinfo.procdef.funcretloc[calleeside].location^.register))
            else
              { todo: complex returns }
              internalerror(2012111106);
          end;
        end;
    end;


  procedure thlcgllvm.g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef);
    begin
      { not possible, need ovloc }
      internalerror(2012111107);
    end;


  procedure thlcgllvm.g_overflowCheck_loc(List: TAsmList; const Loc: TLocation; def: TDef; var ovloc: tlocation);
    begin
      { todo }
      internalerror(2012111108);
    end;


  procedure thlcgllvm.a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle);
    var
      href: treference;
    begin
      if shuffle=mms_movescalar then
        a_loadfpu_ref_reg(list,fromsize,tosize,ref,reg)
      else
        begin
          { todo }
          if fromsize<>tosize then
            internalerror(2013060220);
          href:=make_simple_ref(list,ref,fromsize);
          { %reg = load size* %ref }
          list.concat(taillvm.op_reg_size_ref(la_load,reg,getpointerdef(fromsize),href));
        end;
    end;


  procedure thlcgllvm.a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle);
    var
      href: treference;
    begin
      if shuffle=mms_movescalar then
        a_loadfpu_reg_ref(list,fromsize,tosize,reg,ref)
      else
        begin
          { todo }
          if fromsize<>tosize then
            internalerror(2013060220);
          href:=make_simple_ref(list,ref,tosize);
          { store tosize reg, tosize* href }
          list.concat(taillvm.op_size_reg_size_ref(la_store,tosize,reg,getpointerdef(tosize),href))
        end;
    end;


  procedure thlcgllvm.a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle);
    begin
      if shuffle=mms_movescalar then
        a_loadfpu_reg_reg(list,fromsize,tosize,reg1,reg2)
      else
        { reg2 = bitcast fromllsize reg1 to tollsize }
        list.concat(taillvm.op_reg_size_reg_size(la_bitcast,reg2,fromsize,reg1,tosize));
    end;


  procedure thlcgllvm.a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle);
    begin
      if (op=OP_XOR) and
         (src=dst) then
        a_load_const_reg(list,size,0,dst)
      else
        { todo }
        internalerror(2013060221);
    end;


  procedure thlcgllvm.a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2013060222);
    end;


  procedure thlcgllvm.a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle);
    begin
      internalerror(2013060223);
    end;


  procedure thlcgllvm.gen_load_cgpara_loc(list: TAsmList; vardef: tdef; const para: TCGPara; var destloc: tlocation; reusepara: boolean);
    var
      href     : treference;
    begin
      { skip e.g. empty records }
      if (para.location^.loc = LOC_VOID) then
        exit;
      para.check_simple_location;
      case destloc.loc of
        LOC_REFERENCE :
          begin
            { If the parameter location is reused we don't need to copy
              anything }
            if not reusepara then
              begin
                reference_reset_symbol(href,para.location^.llvmloc,0,para.location^.def.alignment);
                if para.location^.llvmvalueloc then
                  href.refaddr:=addr_full;
                { TODO: if more than one location, use para.location^.def instead (otherwise para.def, because can be
                  zext/sext -> paraloc.location^.def will be larger) }
                a_load_ref_ref(list,para.def,para.def,href,destloc.reference);
              end;
          end;
        { TODO other possible locations }
        else
          internalerror(2013102304);
      end;
    end;


  procedure thlcgllvm.a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel);
    begin
      internalerror(2013060224);
    end;


  procedure thlcgllvm.g_flags2reg(list: TAsmList; size: tdef; const f: tresflags; reg: TRegister);
    begin
      internalerror(2013060225);
    end;


  procedure thlcgllvm.g_flags2ref(list: TAsmList; size: tdef; const f: tresflags; const ref: TReference);
    begin
      internalerror(2013060226);
    end;


  procedure thlcgllvm.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: tdef; src, dst: tregister);
    begin
      internalerror(2012090201);
    end;


  procedure thlcgllvm.g_stackpointer_alloc(list: TAsmList; size: longint);
    begin
      internalerror(2012090203);
    end;


  procedure thlcgllvm.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    begin
      internalerror(2012090204);
    end;


  procedure thlcgllvm.g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint);
    begin
      internalerror(2012090205);
    end;


  procedure thlcgllvm.g_local_unwind(list: TAsmList; l: TAsmLabel);
    begin
      internalerror(2012090206);
    end;


  function thlcgllvm.make_simple_ref(list: TAsmList; const ref: treference; def: tdef): treference;
    var
      ptrindex: tcgint;
      hreg1,
      hreg2: tregister;
      tmpref: treference;
    begin
      { already simple? }
      if (not assigned(ref.symbol) or
          (ref.base=NR_NO)) and
         (ref.index=NR_NO) and
         (ref.offset=0) then
        begin
          result:=ref;
          exit;
        end;

      hreg2:=getaddressregister(list,getpointerdef(def));
      { symbol+offset or base+offset with offset a multiple of the size ->
        use getelementptr }
      if (ref.index=NR_NO) and
         (ref.offset mod def.size=0) then
        begin
          ptrindex:=ref.offset div def.size;
          if assigned(ref.symbol) then
            reference_reset_symbol(tmpref,ref.symbol,0,ref.alignment)
          else
            reference_reset_base(tmpref,ref.base,0,ref.alignment);
          list.concat(taillvm.getelementptr_reg_size_ref_size_const(hreg2,getpointerdef(def),tmpref,ptruinttype,ptrindex,assigned(ref.symbol)));
          reference_reset_base(result,hreg2,0,ref.alignment);
          exit;
        end;
      { for now, perform all calculations using plain pointer arithmetic. Later
        we can look into optimizations based on getelementptr for structured
        accesses (if only to prevent running out of virtual registers).

        Assumptions:
          * symbol/base register: always type "def*"
          * index/offset: always type "ptruinttype" (llvm bitcode has no sign information, so sign doesn't matter) }
      hreg1:=getintregister(list,ptruinttype);
      if assigned(ref.symbol) then
        begin
          if ref.base<>NR_NO then
            internalerror(2012111301);
          reference_reset_symbol(tmpref,ref.symbol,0,ref.alignment);
          list.concat(taillvm.getelementptr_reg_size_ref_size_const(hreg1,getpointerdef(def),tmpref,ptruinttype,0,true));
        end
      else if ref.base<>NR_NO then
        begin
          a_load_reg_reg(list,getpointerdef(def),ptruinttype,ref.base,hreg1);
        end
      else
        { todo: support for absolute addresses on embedded platforms }
        internalerror(2012111302);
      if ref.index<>NR_NO then
        begin
          { SSA... }
          hreg2:=getintregister(list,ptruinttype);
          a_op_reg_reg_reg(list,OP_ADD,ptruinttype,ref.index,hreg1,hreg2);
          hreg1:=hreg2;
        end;
      if ref.offset<>0 then
        begin
          hreg2:=getintregister(list,ptruinttype);
          a_op_const_reg_reg(list,OP_ADD,ptruinttype,ref.offset,hreg1,hreg2);
          hreg1:=hreg2;
        end;
      a_load_reg_reg(list,ptruinttype,getpointerdef(def),hreg1,hreg2);
      reference_reset_base(result,hreg2,0,ref.alignment);
    end;


  procedure thlcgllvm.varsym_set_localloc(list: TAsmList; vs: tabstractnormalvarsym);
    begin
      if cs_asm_source in current_settings.globalswitches then
        begin
          case vs.initialloc.loc of
            LOC_REFERENCE :
              begin
                if assigned(vs.initialloc.reference.symbol) then
                  list.concat(Tai_comment.Create(strpnew('Var '+vs.realname+' located at '+
                     vs.initialloc.reference.symbol.name)))
                else
                  list.concat(Tai_comment.Create(strpnew('Var '+vs.realname+' located at %tmp.'+
                     tostr(getsupreg(vs.initialloc.reference.base)))));
              end;
          end;
        end;
      vs.localloc:=vs.initialloc;
      FillChar(vs.currentregloc,sizeof(vs.currentregloc),0);
    end;


  procedure thlcgllvm.paravarsym_set_initialloc_to_paraloc(vs: tparavarsym);
    var
      parasym : tasmsymbol;
    begin
      parasym:=vs.paraloc[calleeside].location^.llvmloc;
      reference_reset_symbol(vs.initialloc.reference,parasym,0,vs.paraloc[calleeside].alignment);
      if vs.paraloc[calleeside].location^.llvmvalueloc then
        vs.initialloc.reference.refaddr:=addr_full;
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgllvm.create;
      cgllvm.create_codegen
    end;

end.
