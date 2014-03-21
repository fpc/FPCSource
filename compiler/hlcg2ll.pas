{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit implements the high level code generator object for targets that
    only use the low-level code generator

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
{# @abstract(High level code generator to low level)
  This class passes the high level code generator methods through to the
  low level code generator.
}
unit hlcg2ll;

{$i fpcdefs.inc}

{ define hlcginline}

  interface

    uses
       cclasses,globtype,constexp,
       cpubase,cgbase,cgutils,parabase,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       symconst,symtype,symdef,rgobj,
       node,hlcgobj
       ;

    type
       {# @abstract(Abstract high level code generator)
          This class implements an abstract instruction generator. All
          methods of this class are generic and are mapped to low level code
          generator methods by default. They have to be overridden for higher
          level targets
       }

       { thlcg2ll }

       thlcg2ll = class(thlcgobj)
       public
          {************************************************}
          {                 basic routines                 }
          constructor create;
          procedure init_register_allocators;override;
          {# Clean up the register allocators needed for the codegenerator.}
          procedure done_register_allocators;override;
          {# Set whether live_start or live_end should be updated when allocating registers, needed when e.g. generating initcode after the rest of the code. }
          procedure set_regalloc_live_range_direction(dir: TRADirection);override;

          {# Gets a register suitable to do integer operations on.}
          function getintregister(list:TAsmList;size:tdef):Tregister;override;
          {# Gets a register suitable to do integer operations on.}
          function getaddressregister(list:TAsmList;size:tdef):Tregister;override;
          function getfpuregister(list:TAsmList;size:tdef):Tregister;override;
          { warning: only works correctly for fpu types currently }
          function getmmregister(list:TAsmList;size:tdef):Tregister;override;
          function getflagregister(list:TAsmList;size:tdef):Tregister;override;
          {Does the generic cg need SIMD registers, like getmmxregister? Or should
           the cpu specific child cg object have such a method?}

          function  uses_registers(rt:Tregistertype):boolean; inline;

          procedure do_register_allocation(list:TAsmList;headertai:tai); inline;
          procedure translate_register(var reg : tregister); inline;

          {# Allocates register r by inserting a pai_realloc record }
          procedure a_reg_alloc(list : TAsmList;r : tregister); inline;
          {# Deallocates register r by inserting a pa_regdealloc record}
          procedure a_reg_dealloc(list : TAsmList;r : tregister); inline;
          { Synchronize register, make sure it is still valid }
          procedure a_reg_sync(list : TAsmList;r : tregister); inline;

          {# Pass a parameter, which is located in a register, to a routine.

             This routine should push/send the parameter to the routine, as
             required by the specific processor ABI and routine modifiers.
             It must generate register allocation information for the cgpara in
             case it consists of cpuregisters.

             @param(size size of the operand in the register)
             @param(r register source of the operand)
             @param(cgpara where the parameter will be stored)
          }
          procedure a_load_reg_cgpara(list : TAsmList;size : tdef;r : tregister;const cgpara : TCGPara);override;
          {# Pass a parameter, which is a constant, to a routine.

             A generic version is provided. This routine should
             be overridden for optimization purposes if the cpu
             permits directly sending this type of parameter.
             It must generate register allocation information for the cgpara in
             case it consists of cpuregisters.

             @param(size size of the operand in constant)
             @param(a value of constant to send)
             @param(cgpara where the parameter will be stored)
          }
          procedure a_load_const_cgpara(list : TAsmList;tosize : tdef;a : tcgint;const cgpara : TCGPara);override;
          {# Pass the value of a parameter, which is located in memory, to a routine.

             A generic version is provided. This routine should
             be overridden for optimization purposes if the cpu
             permits directly sending this type of parameter.
             It must generate register allocation information for the cgpara in
             case it consists of cpuregisters.

             @param(size size of the operand in constant)
             @param(r Memory reference of value to send)
             @param(cgpara where the parameter will be stored)
          }
          procedure a_load_ref_cgpara(list : TAsmList;size : tdef;const r : treference;const cgpara : TCGPara);override;
          {# Pass the value of a parameter, which can be located either in a register or memory location,
             to a routine.

             A generic version is provided.

             @param(l location of the operand to send)
             @param(nr parameter number (starting from one) of routine (from left to right))
             @param(cgpara where the parameter will be stored)
          }
          procedure a_load_loc_cgpara(list : TAsmList;size : tdef; const l : tlocation;const cgpara : TCGPara);override;
          {# Pass the address of a reference to a routine. This routine
             will calculate the address of the reference, and pass this
             calculated address as a parameter.
             It must generate register allocation information for the cgpara in
             case it consists of cpuregisters.

             A generic version is provided. This routine should
             be overridden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(fromsize type of the reference we are taking the address of)
             @param(tosize type of the pointer that we get as a result)
             @param(r reference to get address from)
          }
          procedure a_loadaddr_ref_cgpara(list : TAsmList;fromsize : tdef;const r : treference;const cgpara : TCGPara);override;

          function a_call_name(list : TAsmList;pd : tprocdef;const s : TSymStr; forceresdef: tdef; weak: boolean): tcgpara;override;
          procedure a_call_reg(list : TAsmList;pd : tabstractprocdef;reg : tregister);override;
          { same as a_call_name, might be overridden on certain architectures to emit
            static calls without usage of a got trampoline }
          function a_call_name_static(list : TAsmList;pd : tprocdef;const s : TSymStr; forceresdef: tdef): tcgpara;override;

          { move instructions }
          procedure a_load_const_reg(list : TAsmList;tosize : tdef;a : tcgint;register : tregister);override;
          procedure a_load_const_ref(list : TAsmList;tosize : tdef;a : tcgint;const ref : treference);override;
          procedure a_load_const_loc(list : TAsmList;tosize : tdef;a : tcgint;const loc : tlocation);override;
          procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
          procedure a_load_reg_ref_unaligned(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);override;
          procedure a_load_reg_reg(list : TAsmList;fromsize, tosize : tdef;reg1,reg2 : tregister);override;
          procedure a_load_reg_loc(list : TAsmList;fromsize, tosize : tdef;reg : tregister;const loc: tlocation);override;
          procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
          procedure a_load_ref_reg_unaligned(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);override;
          procedure a_load_ref_ref(list : TAsmList;fromsize, tosize : tdef;const sref : treference;const dref : treference);override;
          procedure a_load_loc_reg(list : TAsmList;fromsize, tosize : tdef; const loc: tlocation; reg : tregister);override;
          procedure a_load_loc_ref(list : TAsmList;fromsize, tosize: tdef; const loc: tlocation; const ref : treference);override;
          procedure a_loadaddr_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;r : tregister);override;

          { bit scan instructions }
          procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: tdef; src, dst: tregister); override;

          { fpu move instructions }
          procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister); override;
          procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister); override;
          procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference); override;
          procedure a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1,ref2: treference);override;
          procedure a_loadfpu_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const reg: tregister);override;
          procedure a_loadfpu_reg_loc(list: TAsmList; fromsize, tosize: tdef; const reg: tregister; const loc: tlocation);override;
          procedure a_loadfpu_reg_cgpara(list : TAsmList;fromsize: tdef;const r : tregister;const cgpara : TCGPara);override;
          procedure a_loadfpu_ref_cgpara(list : TAsmList;fromsize : tdef;const ref : treference;const cgpara : TCGPara);override;

          { vector register move instructions }
          procedure a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef;reg1, reg2: tregister;shuffle : pmmshuffle); override;
          procedure a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef;const ref: treference; reg: tregister;shuffle : pmmshuffle); override;
          procedure a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef;reg: tregister; const ref: treference;shuffle : pmmshuffle); override;
          procedure a_loadmm_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const reg: tregister;shuffle : pmmshuffle);override;
          procedure a_loadmm_reg_loc(list: TAsmList; fromsize, tosize: tdef; const reg: tregister; const loc: tlocation;shuffle : pmmshuffle);override;
          procedure a_loadmm_reg_cgpara(list: TAsmList; fromsize: tdef; reg: tregister;const cgpara : TCGPara;shuffle : pmmshuffle); override;
          procedure a_loadmm_ref_cgpara(list: TAsmList; fromsize: tdef; const ref: treference;const cgpara : TCGPara;shuffle : pmmshuffle); override;
          procedure a_loadmm_loc_cgpara(list: TAsmList; fromsize: tdef; const loc: tlocation; const cgpara : TCGPara;shuffle : pmmshuffle); override;
          procedure a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size : tdef;src,dst: tregister;shuffle : pmmshuffle); override;
          procedure a_opmm_ref_reg(list: TAsmList; Op: TOpCG; size : tdef;const ref: treference; reg: tregister;shuffle : pmmshuffle); override;
          procedure a_opmm_loc_reg(list: TAsmList; Op: TOpCG; size : tdef;const loc: tlocation; reg: tregister;shuffle : pmmshuffle); override;
          procedure a_opmm_reg_ref(list: TAsmList; Op: TOpCG; size : tdef;reg: tregister;const ref: treference; shuffle : pmmshuffle); override;
          procedure a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize : tdef; intreg, mmreg: tregister; shuffle: pmmshuffle); override;
          procedure a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize : tdef; mmreg, intreg: tregister; shuffle : pmmshuffle); override;

          { basic arithmetic operations }
          { note: for operators which require only one argument (not, neg), use }
          { the op_reg_reg, op_reg_ref or op_reg_loc methods and keep in mind   }
          { that in this case the *second* operand is used as both source and   }
          { destination (JM)                                                    }
          procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister); override;
          procedure a_op_const_ref(list : TAsmList; Op: TOpCG; size: tdef; a: tcgint; const ref: TReference); override;
          procedure a_op_const_loc(list : TAsmList; Op: TOpCG; size: tdef; a: tcgint; const loc: tlocation);override;
          procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister); override;
          procedure a_op_reg_ref(list : TAsmList; Op: TOpCG; size: tdef; reg: TRegister; const ref: TReference); override;
          procedure a_op_ref_reg(list : TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister); override;
          procedure a_op_reg_loc(list : TAsmList; Op: TOpCG; size: tdef; reg: tregister; const loc: tlocation);override;
          procedure a_op_ref_loc(list : TAsmList; Op: TOpCG; size: tdef; const ref: TReference; const loc: tlocation);override;

          { trinary operations for processors that support them, 'emulated' }
          { on others. None with "ref" arguments since I don't think there  }
          { are any processors that support it (JM)                         }
          procedure a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister); override;
          procedure a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister); override;
          procedure a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister;setflags : boolean;var ovloc : tlocation); override;
          procedure a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation); override;

          {  comparison operations }
          procedure a_cmp_const_reg_label(list : TAsmList;size : tdef;cmp_op : topcmp;a : tcgint;reg : tregister;
            l : tasmlabel);override;
          procedure a_cmp_const_ref_label(list : TAsmList;size : tdef;cmp_op : topcmp;a : tcgint;const ref : treference;
            l : tasmlabel); override;
          procedure a_cmp_const_loc_label(list: TAsmList; size: tdef;cmp_op: topcmp; a: tcgint; const loc: tlocation;
            l : tasmlabel);override;
          procedure a_cmp_reg_reg_label(list : TAsmList;size : tdef;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;
          procedure a_cmp_ref_reg_label(list : TAsmList;size : tdef;cmp_op : topcmp; const ref: treference; reg : tregister; l : tasmlabel); override;
          procedure a_cmp_reg_ref_label(list : TAsmList;size : tdef;cmp_op : topcmp;reg : tregister; const ref: treference; l : tasmlabel); override;

          procedure a_cmp_loc_reg_label(list : TAsmList;size : tdef;cmp_op : topcmp; const loc: tlocation; reg : tregister; l : tasmlabel);override;
          procedure a_cmp_ref_loc_label(list: TAsmList; size: tdef;cmp_op: topcmp; const ref: treference; const loc: tlocation; l : tasmlabel);override;

          procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
{$ifdef cpuflags}
          procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;

          {# Depending on the value to check in the flags, either sets the register reg to one (if the flag is set)
             or zero (if the flag is cleared). The size parameter indicates the destination size register.
          }
          procedure g_flags2reg(list: TAsmList; size: tdef; const f: tresflags; reg: TRegister); override;
          procedure g_flags2ref(list: TAsmList; size: tdef; const f: tresflags; const ref:TReference); override;
{$endif cpuflags}

//          procedure g_maybe_testvmt(list : TAsmList;reg:tregister;objdef:tobjectdef);
          {# This should emit the opcode to copy len bytes from the source
             to destination.

             It must be overridden for each new target processor.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)

          }
          procedure g_concatcopy(list : TAsmList;size: tdef; const source,dest : treference);override;
          {# This should emit the opcode to copy len bytes from the an unaligned source
             to destination.

             It must be overridden for each new target processor.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)

          }
          procedure g_concatcopy_unaligned(list : TAsmList;size: tdef; const source,dest : treference);override;

          {# Generates overflow checking code for a node }
          procedure g_overflowcheck(list: TAsmList; const Loc:tlocation; def:tdef); override;
          procedure g_overflowCheck_loc(List:TAsmList;const Loc:TLocation;def:TDef;var ovloc : tlocation);override;

          {# Emits instructions when compilation is done in profile
             mode (this is set as a command line option). The default
             behavior does nothing, should be overridden as required.
          }
          procedure g_profilecode(list : TAsmList);override;
          {# Emits instruction for allocating @var(size) bytes at the stackpointer

             @param(size Number of bytes to allocate)
          }
          procedure g_stackpointer_alloc(list : TAsmList;size : longint);override;
          {# Emits instruction for allocating the locals in entry
             code of a routine. This is one of the first
             routine called in @var(genentrycode).

             @param(localsize Number of bytes to allocate as locals)
          }
          procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
          {# Emits instructions for returning from a subroutine.
             Should also restore the framepointer and stack.

             @param(parasize  Number of bytes of parameters to deallocate from stack)
          }
          procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean);override;

          procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;
          procedure g_adjust_self_value(list:TAsmList;procdef: tprocdef;ioffset: aint);override;

          { generate a stub which only purpose is to pass control the given external method,
          setting up any additional environment before doing so (if required).

          The default implementation issues a jump instruction to the external name. }
//          procedure g_external_wrapper(list : TAsmList; procdef: tprocdef; const externalname: string); override;

          { Generate code to exit an unwind-protected region. The default implementation
            produces a simple jump to destination label. }
          procedure g_local_unwind(list: TAsmList; l: TAsmLabel);override;

          procedure location_force_reg(list:TAsmList;var l:tlocation;src_size,dst_size:tdef;maybeconst:boolean);override;
          procedure location_force_mem(list:TAsmList;var l:tlocation;size:tdef);override;
          procedure location_force_mmregscalar(list:TAsmList;var l: tlocation;size:tdef;maybeconst:boolean);override;
//          procedure location_force_mmreg(list:TAsmList;var l: tlocation;size:tdef;maybeconst:boolean);override;

          procedure maketojumpbool(list:TAsmList; p : tnode);override;

          procedure gen_load_para_value(list:TAsmList);override;
         protected
          procedure gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation;const cgpara: tcgpara;locintsize: longint);override;
         public


          procedure gen_load_loc_cgpara(list: TAsmList; vardef: tdef; const l: tlocation; const cgpara: tcgpara); override;
          procedure gen_load_cgpara_loc(list: TAsmList; vardef: tdef; const para: TCGPara; var destloc: tlocation; reusepara: boolean); override;

         protected
          { returns the equivalent MM size for a vector register that contains
            a record, because in that case "size" will contain a cgsize
            representing an integer size}
          function getintmmcgsize(reg: tregister; size: tcgsize): tcgsize; virtual;
       end;


implementation

    uses
       globals,options,systems,
       verbose,defutil,paramgr,symsym,
       cgobj,tgobj,cutils,procinfo,
       ncgutil;

  { thlcg2ll }

  constructor thlcg2ll.create;
    begin
    end;

  procedure thlcg2ll.init_register_allocators;
    begin
      cg.init_register_allocators;
    end;

  procedure thlcg2ll.done_register_allocators;
    begin
      cg.done_register_allocators;
    end;

  procedure thlcg2ll.set_regalloc_live_range_direction(dir: TRADirection);
    begin
      cg.set_regalloc_live_range_direction(dir);
    end;

  function thlcg2ll.getintregister(list: TAsmList; size: tdef): Tregister;
    begin
      result:=cg.getintregister(list,def_cgsize(size));
    end;


  function thlcg2ll.getaddressregister(list: TAsmList; size: tdef): Tregister;
    begin
      result:=cg.getaddressregister(list);
    end;

  function thlcg2ll.getfpuregister(list: TAsmList; size: tdef): Tregister;
    begin
      result:=cg.getfpuregister(list,def_cgsize(size));
    end;

  function thlcg2ll.getmmregister(list: TAsmList; size: tdef): Tregister;
    begin
      result:=cg.getmmregister(list,def_cgsize(size));
    end;

(*
  function thlcg2ll.getmmregister(list: TAsmList; size: tdef): Tregister;
    begin
      result:=cg.getmmregister(list,def_cgsize(size));
    end;
*)
  function thlcg2ll.getflagregister(list: TAsmList; size: tdef): Tregister;
    begin
      result:=cg.getflagregister(list,def_cgsize(size));
    end;

  function thlcg2ll.uses_registers(rt: Tregistertype): boolean;
    begin
       result:=cg.uses_registers(rt);
    end;

  procedure thlcg2ll.do_register_allocation(list: TAsmList; headertai: tai);
    begin
      cg.do_register_allocation(list,headertai);
    end;

  procedure thlcg2ll.translate_register(var reg: tregister);
    begin
      cg.translate_register(reg);
    end;

  procedure thlcg2ll.a_reg_alloc(list: TAsmList; r: tregister);
    begin
      cg.a_reg_alloc(list,r);
    end;

  procedure thlcg2ll.a_reg_dealloc(list: TAsmList; r: tregister);
    begin
      cg.a_reg_dealloc(list,r);
    end;

  procedure thlcg2ll.a_reg_sync(list: TAsmList; r: tregister);
    begin
      cg.a_reg_sync(list,r);
    end;

  procedure thlcg2ll.a_load_reg_cgpara(list: TAsmList; size: tdef; r: tregister; const cgpara: TCGPara);
    begin
      cg.a_load_reg_cgpara(list,def_cgsize(size),r,cgpara);
    end;

  procedure thlcg2ll.a_load_const_cgpara(list: TAsmList; tosize: tdef; a: tcgint; const cgpara: TCGPara);
    begin
      cg.a_load_const_cgpara(list,def_cgsize(tosize),a,cgpara);
    end;

  procedure thlcg2ll.a_load_ref_cgpara(list: TAsmList; size: tdef; const r: treference; const cgpara: TCGPara);
    begin
      cg.a_load_ref_cgpara(list,def_cgsize(size),r,cgpara);
    end;

  procedure thlcg2ll.a_load_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: TCGPara);
    begin
      cg.a_load_loc_cgpara(list,l,cgpara);
    end;

  procedure thlcg2ll.a_loadaddr_ref_cgpara(list: TAsmList; fromsize: tdef; const r: treference; const cgpara: TCGPara);
    begin
      cg.a_loadaddr_ref_cgpara(list,r,cgpara);
    end;

  function thlcg2ll.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef; weak: boolean): tcgpara;
    begin
      cg.a_call_name(list,s,weak);
      result:=get_call_result_cgpara(pd,forceresdef);
    end;

  procedure thlcg2ll.a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister);
    begin
      cg.a_call_reg(list,reg);
    end;

  function thlcg2ll.a_call_name_static(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef): tcgpara;
    begin
      cg.a_call_name_static(list,s);
      result:=get_call_result_cgpara(pd,forceresdef);
    end;

  procedure thlcg2ll.a_load_const_reg(list: TAsmList; tosize: tdef; a: tcgint; register: tregister);
    begin
      cg.a_load_const_reg(list,def_cgsize(tosize),a,register);
    end;

  procedure thlcg2ll.a_load_const_ref(list: TAsmList; tosize: tdef; a: tcgint; const ref: treference);
    begin
       cg.a_load_const_ref(list,def_cgsize(tosize),a,ref);
    end;

  procedure thlcg2ll.a_load_const_loc(list: TAsmList; tosize: tdef; a: tcgint; const loc: tlocation);
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_load_const_loc(list,a,loc);
      end;
    end;

  procedure thlcg2ll.a_load_reg_ref(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    begin
      cg.a_load_reg_ref(list,def_cgsize(fromsize),def_cgsize(tosize),register,ref);
    end;

  procedure thlcg2ll.a_load_reg_ref_unaligned(list: TAsmList; fromsize, tosize: tdef; register: tregister; const ref: treference);
    begin
      cg.a_load_reg_ref_unaligned(list,def_cgsize(fromsize),def_cgsize(tosize),register,ref);
    end;

  procedure thlcg2ll.a_load_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      cg.a_load_reg_reg(list,def_cgsize(fromsize),def_cgsize(tosize),reg1,reg2);
    end;

  procedure thlcg2ll.a_load_reg_loc(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const loc: tlocation);
    var
      fromcgsize: tcgsize;
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited;
        else
          begin
            { avoid problems with 3-byte records and the like }
            if (fromsize.typ<>floatdef) and
               (fromsize=tosize) then
              fromcgsize:=loc.size
            else
              { fromsize can be a floatdef (in case the destination is an
                MMREGISTER) -> use int_cgsize rather than def_cgsize to get the
                corresponding integer cgsize of the def }
              fromcgsize:=int_cgsize(fromsize.size);
            cg.a_load_reg_loc(list,fromcgsize,reg,loc);
          end;
      end;
    end;

  procedure thlcg2ll.a_load_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    begin
      cg.a_load_ref_reg(list,def_cgsize(fromsize),def_cgsize(tosize),ref,register);
    end;

  procedure thlcg2ll.a_load_ref_reg_unaligned(list: TAsmList; fromsize, tosize: tdef; const ref: treference; register: tregister);
    begin
      cg.a_load_ref_reg_unaligned(list,def_cgsize(fromsize),def_cgsize(tosize),ref,register);
    end;

  procedure thlcg2ll.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
    begin
      cg.a_load_ref_ref(list,def_cgsize(fromsize),def_cgsize(tosize),sref,dref);
    end;

  procedure thlcg2ll.a_load_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; reg: tregister);
    var
      tocgsize: tcgsize;
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          begin
            { avoid problems with 3-byte records and the like }
            if fromsize=tosize then
              tocgsize:=loc.size
            else
              tocgsize:=def_cgsize(tosize);
            cg.a_load_loc_reg(list,tocgsize,loc,reg);
          end;
      end;
    end;

  procedure thlcg2ll.a_load_loc_ref(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const ref: treference);
    var
      tocgsize: tcgsize;
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          begin
            { avoid problems with 3-byte records and the like }
            if fromsize=tosize then
              tocgsize:=loc.size
            else
              tocgsize:=def_cgsize(tosize);
            cg.a_load_loc_ref(list,tocgsize,loc,ref);
          end;
      end;
    end;

  procedure thlcg2ll.a_loadaddr_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; r: tregister);
    begin
      cg.a_loadaddr_ref_reg(list,ref,r);
    end;

  procedure thlcg2ll.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; size: tdef; src, dst: tregister);
    begin
      cg.a_bit_scan_reg_reg(list,reverse,def_cgsize(size),src,dst);
    end;

  procedure thlcg2ll.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister);
    begin
      cg.a_loadfpu_reg_reg(list,def_cgsize(fromsize),def_cgsize(tosize),reg1,reg2);
    end;

  procedure thlcg2ll.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister);
    begin
      cg.a_loadfpu_ref_reg(list,def_cgsize(fromsize),def_cgsize(tosize),ref,reg);
    end;

  procedure thlcg2ll.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference);
    begin
      cg.a_loadfpu_reg_ref(list,def_cgsize(fromsize),def_cgsize(tosize),reg,ref);
    end;

  procedure thlcg2ll.a_loadfpu_ref_ref(list: TAsmList; fromsize, tosize: tdef; const ref1, ref2: treference);
    begin
      cg.a_loadfpu_ref_ref(list,def_cgsize(fromsize),def_cgsize(tosize),ref1,ref2);
    end;

  procedure thlcg2ll.a_loadfpu_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const reg: tregister);
    begin
{$ifdef extdebug}
      if def_cgsize(fromsize)<>loc.size then
        internalerror(2010112102);
{$endif}
      cg.a_loadfpu_loc_reg(list,def_cgsize(tosize),loc,reg);
    end;

  procedure thlcg2ll.a_loadfpu_reg_loc(list: TAsmList; fromsize, tosize: tdef; const reg: tregister; const loc: tlocation);
    var
      usesize: tcgsize;
    begin
{$ifdef extdebug}
      if def_cgsize(tosize)<>loc.size then
        internalerror(2010112101);
{$endif}
      { on some platforms, certain records are passed/returned in floating point
        registers -> def_cgsize() won't give us the result we need -> translate
        to corresponding fpu size }
      usesize:=def_cgsize(fromsize);
      if not(usesize in [OS_F32..OS_F128]) then
        usesize:=int_float_cgsize(tcgsize2size[usesize]);
      cg.a_loadfpu_reg_loc(list,usesize,reg,loc);
    end;

  procedure thlcg2ll.a_loadfpu_reg_cgpara(list: TAsmList; fromsize: tdef; const r: tregister; const cgpara: TCGPara);
    begin
      cg.a_loadfpu_reg_cgpara(list,def_cgsize(fromsize),r,cgpara);
    end;

  procedure thlcg2ll.a_loadfpu_ref_cgpara(list: TAsmList; fromsize: tdef; const ref: treference; const cgpara: TCGPara);
    begin
      cg.a_loadfpu_ref_cgpara(list,def_cgsize(fromsize),ref,cgpara);
    end;

  procedure thlcg2ll.a_loadmm_loc_reg(list: TAsmList; fromsize, tosize: tdef; const loc: tlocation; const reg: tregister;shuffle : pmmshuffle);
    var
      tmpreg: tregister;
      tocgsize: tcgsize;
    begin
      if def_cgmmsize(fromsize)<>loc.size then
        internalerror(2012071226);
      tocgsize:=getintmmcgsize(reg,def_cgmmsize(tosize));
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          begin
            tmpreg:=cg.getintregister(list,loc.size);
            a_load_loc_reg(list,fromsize,fromsize,loc,tmpreg);
            { integer register -> no def_cgmmsize but plain }
            cg.a_loadmm_intreg_reg(list,def_cgsize(fromsize),tocgsize,tmpreg,reg,shuffle);
          end
        else
          cg.a_loadmm_loc_reg(list,tocgsize,loc,reg,shuffle);
      end;
    end;

  procedure thlcg2ll.a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tdef; reg1, reg2: tregister; shuffle: pmmshuffle);
    var
      fromcgsize: tcgsize;
      tocgsize: tcgsize;
    begin
      fromcgsize:=getintmmcgsize(reg1,def_cgmmsize(fromsize));
      tocgsize:=getintmmcgsize(reg2,def_cgmmsize(tosize));
      { records may be stored in mmregisters, but def_cgsize will return an
        integer size for them... }
      cg.a_loadmm_reg_reg(list,fromcgsize,tocgsize,reg1,reg2,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle);
    var
      tocgsize: tcgsize;
    begin
      { records may be stored in mmregisters, but def_cgsize will return an
        integer size for them... }
      tocgsize:=getintmmcgsize(reg,def_cgmmsize(tosize));
      cg.a_loadmm_ref_reg(list,def_cgmmsize(fromsize),tocgsize,ref,reg,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle);
    var
      fromcgsize: tcgsize;
    begin
      { records may be stored in mmregisters, but def_cgsize will return an
        integer size for them... }
      fromcgsize:=getintmmcgsize(reg,def_cgmmsize(fromsize));
      cg.a_loadmm_reg_ref(list,fromcgsize,def_cgmmsize(tosize),reg,ref,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_reg_loc(list: TAsmList; fromsize, tosize: tdef; const reg: tregister; const loc: tlocation; shuffle: pmmshuffle);
    var
      fromcgsize: tcgsize;
    begin
      { sanity check }
      if def_cgmmsize(tosize)<>loc.size then
        internalerror(2012071216);
      { records may be stored in mmregisters, but def_cgsize will return an
        integer size for them... }
      fromcgsize:=getintmmcgsize(reg,def_cgmmsize(fromsize));
      cg.a_loadmm_reg_loc(list,fromcgsize,reg,loc,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_reg_cgpara(list: TAsmList; fromsize: tdef; reg: tregister; const cgpara: TCGPara; shuffle: pmmshuffle);
    var
      fromcgsize: tcgsize;
    begin
      { records may be stored in mmregisters, but def_cgsize will return an
        integer size for them... }
      fromcgsize:=getintmmcgsize(reg,def_cgmmsize(fromsize));
      cg.a_loadmm_reg_cgpara(list,fromcgsize,reg,cgpara,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_ref_cgpara(list: TAsmList; fromsize: tdef; const ref: treference; const cgpara: TCGPara; shuffle: pmmshuffle);
    begin
      cg.a_loadmm_ref_cgpara(list,def_cgmmsize(fromsize),ref,cgpara,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_loc_cgpara(list: TAsmList; fromsize: tdef; const loc: tlocation; const cgpara: TCGPara; shuffle: pmmshuffle);
    begin
      { sanity check }
      if def_cgmmsize(fromsize)<>loc.size then
        internalerror(2012071220);
      cg.a_loadmm_loc_cgpara(list,loc,cgpara,shuffle);
    end;

  procedure thlcg2ll.a_opmm_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; src, dst: tregister; shuffle: pmmshuffle);
    begin
      cg.a_opmm_reg_reg(list,op,def_cgmmsize(size),src,dst,shuffle);
    end;

  procedure thlcg2ll.a_opmm_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: treference; reg: tregister; shuffle: pmmshuffle);
    begin
      cg.a_opmm_ref_reg(list,op,def_cgmmsize(size),ref,reg,shuffle);
    end;

  procedure thlcg2ll.a_opmm_loc_reg(list: TAsmList; Op: TOpCG; size: tdef; const loc: tlocation; reg: tregister; shuffle: pmmshuffle);
    begin
      cg.a_opmm_loc_reg(list,op,def_cgmmsize(size),loc,reg,shuffle);
    end;

  procedure thlcg2ll.a_opmm_reg_ref(list: TAsmList; Op: TOpCG; size: tdef; reg: tregister; const ref: treference; shuffle: pmmshuffle);
    begin
      cg.a_opmm_reg_ref(list,op,def_cgmmsize(size),reg,ref,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_intreg_reg(list: TAsmList; fromsize, tosize: tdef; intreg, mmreg: tregister; shuffle: pmmshuffle);
    var
      tocgsize: tcgsize;
    begin
      { records may be stored in mmregisters, but def_cgmmsize will return an
        integer size for them... }
      tocgsize:=getintmmcgsize(mmreg,def_cgmmsize(tosize));
      cg.a_loadmm_intreg_reg(list,def_cgsize(fromsize),tocgsize,intreg,mmreg,shuffle);
    end;

  procedure thlcg2ll.a_loadmm_reg_intreg(list: TAsmList; fromsize, tosize: tdef; mmreg, intreg: tregister; shuffle: pmmshuffle);
    var
      fromcgsize: tcgsize;
    begin
      { records may be stored in mmregisters, but def_cgsize will return an
        integer size for them... }
      fromcgsize:=getintmmcgsize(mmreg,def_cgmmsize(fromsize));
      cg.a_loadmm_reg_intreg(list,fromcgsize,def_cgsize(tosize),mmreg,intreg,shuffle);
    end;

  procedure thlcg2ll.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; reg: TRegister);
    begin
      cg.a_op_const_reg(list,op,def_cgsize(size),a,reg);
    end;

  procedure thlcg2ll.a_op_const_ref(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; const ref: TReference);
    begin
      cg.a_op_const_ref(list,op,def_cgsize(size),a,ref);
    end;

  procedure thlcg2ll.a_op_const_loc(list: TAsmList; Op: TOpCG; size: tdef; a: tcgint; const loc: tlocation);
    begin
{$ifdef extdebug}
      if def_cgsize(size)<>loc.size then
        internalerror(2010112106);
{$endif}
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_op_const_loc(list,op,a,loc);
      end;
    end;

  procedure thlcg2ll.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: tdef; reg1, reg2: TRegister);
    begin
      cg.a_op_reg_reg(list,op,def_cgsize(size),reg1,reg2);
    end;

  procedure thlcg2ll.a_op_reg_ref(list: TAsmList; Op: TOpCG; size: tdef; reg: TRegister; const ref: TReference);
    begin
      cg.a_op_reg_ref(list,op,def_cgsize(size),reg,ref);
    end;

  procedure thlcg2ll.a_op_ref_reg(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; reg: TRegister);
    begin
      cg.a_op_ref_reg(list,op,def_cgsize(size),ref,reg);
    end;

  procedure thlcg2ll.a_op_reg_loc(list: TAsmList; Op: TOpCG; size: tdef; reg: tregister; const loc: tlocation);
    begin
{$ifdef extdebug}
      if def_cgsize(size)<>loc.size then
        internalerror(2010112107);
{$endif}
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_op_reg_loc(list,op,reg,loc)
      end;
    end;

  procedure thlcg2ll.a_op_ref_loc(list: TAsmList; Op: TOpCG; size: tdef; const ref: TReference; const loc: tlocation);
    begin
{$ifdef extdebug}
      if def_cgsize(size)<>loc.size then
        internalerror(2010112101);
{$endif}
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_op_ref_loc(list,op,ref,loc);
      end;
    end;

  procedure thlcg2ll.a_op_const_reg_reg(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister);
    begin
      cg.a_op_const_reg_reg(list,op,def_cgsize(size),a,src,dst);
    end;

  procedure thlcg2ll.a_op_reg_reg_reg(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister);
    begin
      cg.a_op_reg_reg_reg(list,op,def_cgsize(size),src1,src2,dst);
    end;

  procedure thlcg2ll.a_op_const_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; a: tcgint; src, dst: tregister; setflags: boolean; var ovloc: tlocation);
    begin
      cg.a_op_const_reg_reg_checkoverflow(list,op,def_cgsize(size),a,src,dst,setflags,ovloc);
    end;

  procedure thlcg2ll.a_op_reg_reg_reg_checkoverflow(list: TAsmList; op: TOpCg; size: tdef; src1, src2, dst: tregister; setflags: boolean; var ovloc: tlocation);
    begin
      cg.a_op_reg_reg_reg_checkoverflow(list,op,def_cgsize(size),src1,src2,dst,setflags,ovloc);
    end;

  procedure thlcg2ll.a_cmp_const_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; reg: tregister; l: tasmlabel);
    begin
      cg.a_cmp_const_reg_label(list,def_cgsize(size),cmp_op,a,reg,l);
    end;

  procedure thlcg2ll.a_cmp_const_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; const ref: treference; l: tasmlabel);
    begin
      cg.a_cmp_const_ref_label(list,def_cgsize(size),cmp_op,a,ref,l);
    end;

  procedure thlcg2ll.a_cmp_const_loc_label(list: TAsmList; size: tdef; cmp_op: topcmp; a: tcgint; const loc: tlocation; l: tasmlabel);
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_cmp_const_loc_label(list,def_cgsize(size),cmp_op,a,loc,l);
      end;
    end;

  procedure thlcg2ll.a_cmp_reg_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
    begin
       cg.a_cmp_reg_reg_label(list,def_cgsize(size),cmp_op,reg1,reg2,l);
    end;

  procedure thlcg2ll.a_cmp_ref_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; reg: tregister; l: tasmlabel);
    begin
      cg.a_cmp_ref_reg_label(list,def_cgsize(size),cmp_op,ref,reg,l);
    end;

  procedure thlcg2ll.a_cmp_reg_ref_label(list: TAsmList; size: tdef; cmp_op: topcmp; reg: tregister; const ref: treference; l: tasmlabel);
    begin
      cg.a_cmp_reg_ref_label(list,def_cgsize(size),cmp_op,reg,ref,l);
    end;

  procedure thlcg2ll.a_cmp_loc_reg_label(list: TAsmList; size: tdef; cmp_op: topcmp; const loc: tlocation; reg: tregister; l: tasmlabel);
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_cmp_loc_reg_label(list,def_cgsize(size),cmp_op,loc,reg,l);
      end;
    end;

  procedure thlcg2ll.a_cmp_ref_loc_label(list: TAsmList; size: tdef; cmp_op: topcmp; const ref: treference; const loc: tlocation; l: tasmlabel);
    begin
      case loc.loc of
        LOC_SUBSETREG,LOC_CSUBSETREG,
        LOC_SUBSETREF,LOC_CSUBSETREF:
          inherited
        else
          cg.a_cmp_ref_loc_label(list,def_cgsize(size),cmp_op,ref,loc,l);
      end;
    end;

  procedure thlcg2ll.a_jmp_always(list: TAsmList; l: tasmlabel);
    begin
      cg.a_jmp_always(list,l);
    end;

{$ifdef cpuflags}
  procedure thlcg2ll.a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel);
    begin
      cg.a_jmp_flags(list,f,l);
    end;

  procedure thlcg2ll.g_flags2reg(list: TAsmList; size: tdef; const f: tresflags; reg: TRegister);
    begin
      cg.g_flags2reg(list,def_cgsize(size),f,reg);
    end;

  procedure thlcg2ll.g_flags2ref(list: TAsmList; size: tdef; const f: tresflags; const ref: TReference);
    begin
      cg.g_flags2ref(list,def_cgsize(size),f,ref);
    end;
{$endif cpuflags}

  procedure thlcg2ll.g_concatcopy(list: TAsmList; size: tdef; const source, dest: treference);
    begin
      cg.g_concatcopy(list,source,dest,size.size);
    end;

  procedure thlcg2ll.g_concatcopy_unaligned(list: TAsmList; size: tdef; const source, dest: treference);
    begin
      cg.g_concatcopy_unaligned(list,source,dest,size.size);
    end;

  procedure thlcg2ll.g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef);
    begin
      cg.g_overflowcheck(list,loc,def);
    end;

  procedure thlcg2ll.g_overflowCheck_loc(List: TAsmList; const Loc: TLocation; def: TDef; var ovloc: tlocation);
    begin
      cg.g_overflowCheck_loc(list,loc,def,ovloc);
    end;

  procedure thlcg2ll.g_profilecode(list: TAsmList);
    begin
      cg.g_profilecode(list);
    end;

  procedure thlcg2ll.g_stackpointer_alloc(list: TAsmList; size: longint);
    begin
      cg.g_stackpointer_alloc(list,size);
    end;

  procedure thlcg2ll.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
    begin
      cg.g_proc_entry(list,localsize,nostackframe);
    end;

  procedure thlcg2ll.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
    begin
      cg.g_proc_exit(list,parasize,nostackframe);
    end;

  procedure thlcg2ll.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
    begin
      cg.g_intf_wrapper(list,procdef,labelname,ioffset);
    end;

  procedure thlcg2ll.g_adjust_self_value(list: TAsmList; procdef: tprocdef; ioffset: aint);
    begin
      cg.g_adjust_self_value(list,procdef,ioffset);
    end;

  procedure thlcg2ll.g_local_unwind(list: TAsmList; l: TAsmLabel);
    begin
      cg.g_local_unwind(list, l);
    end;

  procedure thlcg2ll.location_force_reg(list: TAsmList; var l: tlocation; src_size, dst_size: tdef; maybeconst: boolean);
    var
{$ifndef cpu64bitalu}
      hregisterhi,
{$endif}
      hregister : tregister;
{$ifndef cpu64bitalu}
      hreg64 : tregister64;
{$endif}
      hl: tasmlabel;
      oldloc : tlocation;
      const_location: boolean;
      dst_cgsize,tmpsize: tcgsize;
    begin
      oldloc:=l;
      dst_cgsize:=def_cgsize(dst_size);
{$ifndef cpu64bitalu}
      { handle transformations to 64bit separate }
      if dst_cgsize in [OS_64,OS_S64] then
       begin
         if not (l.size in [OS_64,OS_S64]) then
          begin
            { load a smaller size to OS_64 }
            if l.loc=LOC_REGISTER then
             begin
{$if defined(cpu8bitalu) or defined(cpu16bitalu)}
               { on avr, we cannot change the size of a register
                 due to the nature how register with size > OS8 are handled
               }
               hregister:=cg.getintregister(list,OS_32);
{$else}
               hregister:=cg.makeregsize(list,l.register64.reglo,OS_32);
{$endif}
               cg.a_load_reg_reg(list,l.size,OS_32,l.register64.reglo,hregister);
             end
            else
             hregister:=cg.getintregister(list,OS_32);
            { load value in low register }
            case l.loc of
{$ifdef cpuflags}
              LOC_FLAGS :
                begin
                  cg.g_flags2reg(list,OS_32,l.resflags,hregister);
                  cg.a_reg_dealloc(list,NR_DEFAULTFLAGS);
                end;
{$endif cpuflags}
              LOC_JUMP :
                begin
                  cg.a_label(list,current_procinfo.CurrTrueLabel);
                  cg.a_load_const_reg(list,OS_INT,1,hregister);
                  current_asmdata.getjumplabel(hl);
                  cg.a_jmp_always(list,hl);
                  cg.a_label(list,current_procinfo.CurrFalseLabel);
                  cg.a_load_const_reg(list,OS_INT,0,hregister);
                  cg.a_label(list,hl);
{$if defined(cpu8bitalu) or defined(cpu16bitalu)}
                  cg.a_load_reg_reg(list,OS_INT,OS_32,hregister,hregister);
{$endif}
                end;
              else
                a_load_loc_reg(list,src_size,u32inttype,l,hregister);
            end;
            { reset hi part, take care of the signed bit of the current value }
            hregisterhi:=cg.getintregister(list,OS_32);
            if (l.size in [OS_S8,OS_S16,OS_S32]) then
             begin
               if l.loc=LOC_CONSTANT then
                begin
                  if (longint(l.value)<0) then
                   cg.a_load_const_reg(list,OS_32,aint($ffffffff),hregisterhi)
                  else
                   cg.a_load_const_reg(list,OS_32,0,hregisterhi);
                end
               else
                begin
                  cg.a_op_const_reg_reg(list,OP_SAR,OS_32,31,hregister,
                    hregisterhi);
                end;
             end
            else
             cg.a_load_const_reg(list,OS_32,0,hregisterhi);
            location_reset(l,LOC_REGISTER,dst_cgsize);
            l.register64.reglo:=hregister;
            l.register64.reghi:=hregisterhi;
          end
         else
          begin
            { 64bit to 64bit }
            if ((l.loc=LOC_CREGISTER) and maybeconst) then
             begin
               hregister:=l.register64.reglo;
               hregisterhi:=l.register64.reghi;
               const_location := true;
             end
            else
             begin
               hregister:=cg.getintregister(list,OS_32);
               hregisterhi:=cg.getintregister(list,OS_32);
               const_location := false;
             end;
            hreg64.reglo:=hregister;
            hreg64.reghi:=hregisterhi;
            { load value in new register }
            cg64.a_load64_loc_reg(list,l,hreg64);
            if not const_location then
              location_reset(l,LOC_REGISTER,dst_cgsize)
            else
              location_reset(l,LOC_CREGISTER,dst_cgsize);
            l.register64.reglo:=hregister;
            l.register64.reghi:=hregisterhi;
          end;
       end
      else
{$endif cpu64bitalu}
        begin
          {Do not bother to recycle the existing register. The register
           allocator eliminates unnecessary moves, so it's not needed
           and trying to recycle registers can cause problems because
           the registers changes size and may need aditional constraints.

           Not if it's about LOC_CREGISTER's (JM)
           }
          const_location :=
             (maybeconst) and
             (l.loc = LOC_CREGISTER) and
             (TCGSize2Size[l.size] = TCGSize2Size[dst_cgsize]) and
             ((l.size = dst_cgsize) or
              (TCGSize2Size[l.size] = sizeof(aint)));
          if not const_location then
            hregister:=cg.getintregister(list,dst_cgsize)
          else
            hregister := l.register;
          { load value in new register }
          case l.loc of
{$ifdef cpuflags}
            LOC_FLAGS :
              begin
                cg.g_flags2reg(list,dst_cgsize,l.resflags,hregister);
                cg.a_reg_dealloc(list,NR_DEFAULTFLAGS);
              end;
{$endif cpuflags}
            LOC_JUMP :
              begin
                tmpsize:=dst_cgsize;
{$if defined(cpu8bitalu) or defined(cpu16bitalu)}
                if TCGSize2Size[dst_cgsize]>TCGSize2Size[OS_INT] then
                  tmpsize:=OS_INT;
{$endif}
                cg.a_label(list,current_procinfo.CurrTrueLabel);
                cg.a_load_const_reg(list,tmpsize,1,hregister);
                current_asmdata.getjumplabel(hl);
                cg.a_jmp_always(list,hl);
                cg.a_label(list,current_procinfo.CurrFalseLabel);
                cg.a_load_const_reg(list,tmpsize,0,hregister);
                cg.a_label(list,hl);
{$if defined(cpu8bitalu) or defined(cpu16bitalu)}
                cg.a_load_reg_reg(list,tmpsize,dst_cgsize,hregister,hregister);
{$endif}
              end;
            else
              begin
                { load_loc_reg can only handle size >= l.size, when the
                  new size is smaller then we need to adjust the size
                  of the orignal and maybe recalculate l.register for i386 }
                if (TCGSize2Size[dst_cgsize]<TCGSize2Size[l.size]) then
                 begin
                   if (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                     begin
{$if defined(cpu8bitalu) or defined(cpu16bitalu)}
                       if TCGSize2Size[dst_cgsize]<=TCGSize2Size[OS_INT] then
{$endif}
                         l.register:=cg.makeregsize(list,l.register,dst_cgsize);
                     end;
                   { for big endian systems, the reference's offset must }
                   { be increased in this case, since they have the      }
                   { MSB first in memory and e.g. byte(word_var) should  }
                   { return  the second byte in this case (JM)           }
                   if (target_info.endian = ENDIAN_BIG) and
                      (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                     begin
                       inc(l.reference.offset,TCGSize2Size[l.size]-TCGSize2Size[dst_cgsize]);
                       l.reference.alignment:=newalignment(l.reference.alignment,TCGSize2Size[l.size]-TCGSize2Size[dst_cgsize]);
                     end;
{$ifdef x86}
                   if not (l.loc in [LOC_SUBSETREG,LOC_CSUBSETREG]) then
                     begin
                       l.size:=dst_cgsize;
                       src_size:=dst_size;
                     end;
{$endif x86}
                 end;
                a_load_loc_reg(list,src_size,dst_size,l,hregister);
                if (TCGSize2Size[dst_cgsize]<TCGSize2Size[l.size])
{$ifdef x86}
                   and (l.loc in [LOC_SUBSETREG,LOC_CSUBSETREG])
{$endif x86}
                  then
                    l.size:=dst_cgsize;
              end;
          end;
          if not const_location then
            location_reset(l,LOC_REGISTER,dst_cgsize)
          else
            location_reset(l,LOC_CREGISTER,dst_cgsize);
          l.register:=hregister;
        end;
      { Release temp when it was a reference }
      if oldloc.loc=LOC_REFERENCE then
          location_freetemp(list,oldloc);
    end;

  procedure thlcg2ll.location_force_mem(list: TAsmList; var l: tlocation; size: tdef);
    var
      r: treference;
    begin
      case l.loc of
        LOC_FPUREGISTER,
        LOC_CFPUREGISTER :
          begin
            { implement here using tcg because some platforms store records
              in fpu registers in some cases, and a_loadfpu* can't deal with
              record "size" parameters }
            tg.gethltemp(list,size,size.size,tt_normal,r);
            cg.a_loadfpu_reg_ref(list,l.size,l.size,l.register,r);
            location_reset_ref(l,LOC_REFERENCE,l.size,0);
            l.reference:=r;
          end;
        LOC_MMREGISTER,
        LOC_CMMREGISTER:
          begin
            tg.gethltemp(list,size,size.size,tt_normal,r);
            cg.a_loadmm_reg_ref(list,l.size,l.size,l.register,r,mms_movescalar);
            location_reset_ref(l,LOC_REFERENCE,l.size,0);
            l.reference:=r;
          end;
        LOC_CONSTANT,
        LOC_REGISTER,
        LOC_CREGISTER :
          begin
            tg.gethltemp(list,size,size.size,tt_normal,r);
{$ifdef cpu64bitalu}
            if l.size in [OS_128,OS_S128] then
              cg128.a_load128_loc_ref(list,l,r)
            else
{$else cpu64bitalu}
            if l.size in [OS_64,OS_S64] then
              cg64.a_load64_loc_ref(list,l,r)
            else
{$endif cpu64bitalu}
              a_load_loc_ref(list,size,size,l,r);
            location_reset_ref(l,LOC_REFERENCE,l.size,0);
            l.reference:=r;
          end;
        else
          inherited;
      end;
    end;

  procedure thlcg2ll.location_force_mmregscalar(list: TAsmList; var l: tlocation; size: tdef; maybeconst: boolean);
    var
      reg : tregister;
      href : treference;
      newsize : tdef;
    begin
      if (l.loc<>LOC_MMREGISTER)  and
         ((l.loc<>LOC_CMMREGISTER) or (not maybeconst)) then
        begin
          { if it's in an fpu register, store to memory first }
          if (l.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
            begin
              tg.GetTemp(list,tcgsize2size[l.size],tcgsize2size[l.size],tt_normal,href);
              cg.a_loadfpu_reg_ref(list,l.size,l.size,l.register,href);
              location_reset_ref(l,LOC_REFERENCE,l.size,0);
              l.reference:=href;
            end;
{$ifndef cpu64bitalu}
          if (l.loc in [LOC_REGISTER,LOC_CREGISTER]) and
             (l.size in [OS_64,OS_S64]) then
            begin
              reg:=cg.getmmregister(list,OS_F64);
              cg64.a_loadmm_intreg64_reg(list,OS_F64,l.register64,reg);
              l.size:=OS_F64;
              size:=s64floattype;
            end
          else
{$endif not cpu64bitalu}
            begin
               { on ARM, CFP values may be located in integer registers,
                 and its second_int_to_real() also uses this routine to
                 force integer (memory) values in an mmregister }
               if (l.size in [OS_32,OS_S32]) then
                 begin
                   size:=cgsize_orddef(l.size);
                   newsize:=s32floattype;
                 end
               else if (l.size in [OS_64,OS_S64]) then
                 begin
                   size:=cgsize_orddef(l.size);
                   newsize:=s64floattype;
                 end
               else
                 newsize:=size;
               reg:=getmmregister(list,newsize);
               a_loadmm_loc_reg(list,size,newsize,l,reg,mms_movescalar);
               l.size:=def_cgsize(newsize);
             end;
          location_freetemp(list,l);
          location_reset(l,LOC_MMREGISTER,l.size);
          l.register:=reg;
        end;
    end;

(*
  procedure thlcg2ll.location_force_mmreg(list: TAsmList; var l: tlocation; size: tdef; maybeconst: boolean);
    begin
      ncgutil.location_force_mmreg(list,l,maybeconst);
    end;
*)
  procedure thlcg2ll.maketojumpbool(list: TAsmList; p: tnode);
    begin
      { loadregvars parameter is no longer used, should be removed from
         ncgutil version as well }
      ncgutil.maketojumpbool(list,p,lr_dont_load_regvars);
    end;

  procedure thlcg2ll.gen_load_para_value(list: TAsmList);
    begin
      ncgutil.gen_load_para_value(list);
    end;

  procedure thlcg2ll.gen_loadfpu_loc_cgpara(list: TAsmList; size: tdef; const l: tlocation; const cgpara: tcgpara; locintsize: longint);
    var
      locsize : tcgsize;
      tmploc : tlocation;
    begin
      if not(l.size in [OS_32,OS_S32,OS_64,OS_S64,OS_128,OS_S128]) then
        locsize:=l.size
      else
        locsize:=int_float_cgsize(tcgsize2size[l.size]);
      case l.loc of
        LOC_MMREGISTER,
        LOC_CMMREGISTER:
          case cgpara.location^.loc of
            LOC_REFERENCE,
            LOC_CREFERENCE,
            LOC_MMREGISTER,
            LOC_CMMREGISTER,
            LOC_REGISTER,
            LOC_CREGISTER :
              cg.a_loadmm_reg_cgpara(list,locsize,l.register,cgpara,mms_movescalar);
            LOC_FPUREGISTER,
            LOC_CFPUREGISTER:
              begin
                tmploc:=l;
                location_force_fpureg(list,tmploc,size,false);
                cg.a_loadfpu_reg_cgpara(list,tmploc.size,tmploc.register,cgpara);
              end;
            else
              internalerror(200204249);
          end;
        LOC_FPUREGISTER,
        LOC_CFPUREGISTER:
          case cgpara.location^.loc of
            LOC_MMREGISTER,
            LOC_CMMREGISTER:
              begin
                tmploc:=l;
                location_force_mmregscalar(list,tmploc,size,false);
                cg.a_loadmm_reg_cgpara(list,tmploc.size,tmploc.register,cgpara,mms_movescalar);
              end;
            { Some targets pass floats in normal registers }
            LOC_REGISTER,
            LOC_CREGISTER,
            LOC_REFERENCE,
            LOC_CREFERENCE,
            LOC_FPUREGISTER,
            LOC_CFPUREGISTER:
              cg.a_loadfpu_reg_cgpara(list,locsize,l.register,cgpara);
            else
              internalerror(2002042433);
          end;
        LOC_REFERENCE,
        LOC_CREFERENCE:
          case cgpara.location^.loc of
            LOC_MMREGISTER,
            LOC_CMMREGISTER:
              cg.a_loadmm_ref_cgpara(list,locsize,l.reference,cgpara,mms_movescalar);
            { Some targets pass floats in normal registers }
            LOC_REGISTER,
            LOC_CREGISTER,
            LOC_REFERENCE,
            LOC_CREFERENCE,
            LOC_FPUREGISTER,
            LOC_CFPUREGISTER:
              cg.a_loadfpu_ref_cgpara(list,locsize,l.reference,cgpara);
            else
              internalerror(2002042431);
          end;
        LOC_REGISTER,
        LOC_CREGISTER :
          begin
{$ifndef cpu64bitalu}
             { Only a_load_ref_cgpara supports multiple locations, when the
               value is still a const or in a register then write it
               to a reference first. This situation can be triggered
               by typecasting an int64 constant to a record of 8 bytes }
             if locsize = OS_F64 then
               begin
                 tmploc:=l;
                 location_force_mem(list,tmploc,size);
                 cg.a_load_loc_cgpara(list,tmploc,cgpara);
                 location_freetemp(list,tmploc);
               end
             else
{$endif not cpu64bitalu}
               cg.a_load_loc_cgpara(list,l,cgpara);
          end;
        else
          internalerror(2002042432);
      end;
    end;

  procedure thlcg2ll.gen_load_loc_cgpara(list: TAsmList; vardef: tdef; const l: tlocation; const cgpara: tcgpara);
    var
      tmploc: tlocation;
    begin
      { skip e.g. empty records }
      if (cgpara.location^.loc = LOC_VOID) then
        exit;

      { Handle Floating point types differently

        This doesn't depend on emulator settings, emulator settings should
        be handled by cpupara }
      if (vardef.typ=floatdef) or
         { some ABIs return certain records in an fpu register }
         (l.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) or
         (assigned(cgpara.location) and
          (cgpara.Location^.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER])) then
        begin
          gen_loadfpu_loc_cgpara(list,vardef,l,cgpara,vardef.size);
          exit;
        end;

      case l.loc of
        LOC_CONSTANT,
        LOC_REGISTER,
        LOC_CREGISTER,
        LOC_REFERENCE,
        LOC_CREFERENCE :
          begin
{$ifdef cpu64bitalu}
            { use cg128 only if no "chained" location is used }
            if is_methodpointer(cgpara.def) and (l.size in [OS_128,OS_S128]) and (cgpara.Size in [OS_128,OS_S128]) then
              cg128.a_load128_loc_cgpara(list,l,cgpara)
            else
{$else cpu64bitalu}
            { use cg64 only for int64, not for 8 byte records }
            if (l.size in [OS_64,OS_S64]) and (cgpara.Size in [OS_64,OS_S64]) then
              cg64.a_load64_loc_cgpara(list,l,cgpara)
            else
{$endif cpu64bitalu}
              begin

                { Only a_load_ref_cgpara supports multiple locations, when the
                  value is still a const or in a register then write it
                  to a reference first. This situation can be triggered
                  by typecasting an int64 constant to a record of 8 bytes }
{$ifdef cpu64bitalu}
                if l.size in [OS_128,OS_S128] then
{$else cpu64bitalu}
                if l.size in [OS_64,OS_S64] then
{$endif cpu64bitalu}
                  begin
                    tmploc:=l;
                    location_force_mem(list,tmploc,vardef);
                    a_load_loc_cgpara(list,vardef,tmploc,cgpara);
                    { do not free the tmploc in case the original value was
                      already in memory, because the caller (ncgcal) will then
                      free it again later }
                    if not(l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                      location_freetemp(list,tmploc);
                  end
                else
                  a_load_loc_cgpara(list,vardef,l,cgpara);
              end;
          end;
        LOC_MMREGISTER,
        LOC_CMMREGISTER:
          begin
            case l.size of
              OS_F32,
              OS_F64:
                cg.a_loadmm_loc_cgpara(list,l,cgpara,mms_movescalar);
              else
                cg.a_loadmm_loc_cgpara(list,l,cgpara,nil);
            end;
          end;
{$ifdef SUPPORT_MMX}
        LOC_MMXREGISTER,
        LOC_CMMXREGISTER:
          cg.a_loadmm_reg_cgpara(list,OS_M64,l.register,cgpara,nil);
{$endif SUPPORT_MMX}
        else
          internalerror(200204241);
      end;
    end;

  procedure thlcg2ll.gen_load_cgpara_loc(list: TAsmList; vardef: tdef; const para: TCGPara; var destloc: tlocation; reusepara: boolean);
    begin
      ncgutil.gen_load_cgpara_loc(list, vardef, para, destloc, reusepara);
    end;

  function thlcg2ll.getintmmcgsize(reg: tregister; size: tcgsize): tcgsize;
    begin
      result:=size;
      if getregtype(reg)=R_MMREGISTER then
        begin
          case size of
            OS_32:
              result:=OS_F32;
            OS_64:
              result:=OS_F64;
          end;
        end;
    end;


end.
