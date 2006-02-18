{
    Copyright (c) 1998-2005 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the basic code generator object

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
{# @abstract(Abstract code generator unit)
   Abstreact code generator unit. This contains the base class
   to implement for all new supported processors.

   WARNING: None of the routines implemented in these modules,
   or their descendants, should use the temp. allocator, as
   these routines may be called inside genentrycode, and the
   stack frame is already setup!
}
unit cgobj;

{$i fpcdefs.inc}

  interface

    uses
       cclasses,globtype,
       cpubase,cgbase,cgutils,parabase,
       aasmbase,aasmtai,aasmcpu,
       symconst,symbase,symtype,symdef,symtable,rgobj
       ;

    type
       talignment = (AM_NATURAL,AM_NONE,AM_2BYTE,AM_4BYTE,AM_8BYTE);

       {# @abstract(Abstract code generator)
          This class implements an abstract instruction generator. Some of
          the methods of this class are generic, while others must
          be overriden for all new processors which will be supported
          by Free Pascal. For 32-bit processors, the base class
          sould be @link(tcg64f32) and not @var(tcg).
       }
       tcg = class
       public
          alignment : talignment;
          rg        : array[tregistertype] of trgobj;
          t_times   : longint;
       {$ifdef flowgraph}
          aktflownode:word;
       {$endif}
          {************************************************}
          {                 basic routines                 }
          constructor create;

          {# Initialize the register allocators needed for the codegenerator.}
          procedure init_register_allocators;virtual;
          {# Clean up the register allocators needed for the codegenerator.}
          procedure done_register_allocators;virtual;
          {# Set whether live_start or live_end should be updated when allocating registers, needed when e.g. generating initcode after the rest of the code. }
          procedure set_regalloc_extend_backwards(b: boolean);

       {$ifdef flowgraph}
          procedure init_flowgraph;
          procedure done_flowgraph;
       {$endif}
          {# Gets a register suitable to do integer operations on.}
          function getintregister(list:Taasmoutput;size:Tcgsize):Tregister;virtual;
          {# Gets a register suitable to do integer operations on.}
          function getaddressregister(list:Taasmoutput):Tregister;virtual;
          function getfpuregister(list:Taasmoutput;size:Tcgsize):Tregister;virtual;
          function getmmregister(list:Taasmoutput;size:Tcgsize):Tregister;virtual;
          function getflagregister(list:Taasmoutput;size:Tcgsize):Tregister;virtual;abstract;
          {Does the generic cg need SIMD registers, like getmmxregister? Or should
           the cpu specific child cg object have such a method?}

          procedure add_reg_instruction(instr:Tai;r:tregister);virtual;
          procedure add_move_instruction(instr:Taicpu);virtual;

          function  uses_registers(rt:Tregistertype):boolean;virtual;
          {# Get a specific register.}
          procedure getcpuregister(list:Taasmoutput;r:Tregister);virtual;
          procedure ungetcpuregister(list:Taasmoutput;r:Tregister);virtual;
          {# Get multiple registers specified.}
          procedure alloccpuregisters(list:Taasmoutput;rt:Tregistertype;const r:Tcpuregisterset);virtual;
          {# Free multiple registers specified.}
          procedure dealloccpuregisters(list:Taasmoutput;rt:Tregistertype;const r:Tcpuregisterset);virtual;

          procedure do_register_allocation(list:Taasmoutput;headertai:tai);virtual;

          function makeregsize(list:Taasmoutput;reg:Tregister;size:Tcgsize):Tregister;

          {# Emit a label to the instruction stream. }
          procedure a_label(list : taasmoutput;l : tasmlabel);virtual;

          {# Allocates register r by inserting a pai_realloc record }
          procedure a_reg_alloc(list : taasmoutput;r : tregister);
          {# Deallocates register r by inserting a pa_regdealloc record}
          procedure a_reg_dealloc(list : taasmoutput;r : tregister);
          { Synchronize register, make sure it is still valid }
          procedure a_reg_sync(list : taasmoutput;r : tregister);

          {# Pass a parameter, which is located in a register, to a routine.

             This routine should push/send the parameter to the routine, as
             required by the specific processor ABI and routine modifiers.
             This must be overriden for each CPU target.

             @param(size size of the operand in the register)
             @param(r register source of the operand)
             @param(cgpara where the parameter will be stored)
          }
          procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const cgpara : TCGPara);virtual;
          {# Pass a parameter, which is a constant, to a routine.

             A generic version is provided. This routine should
             be overriden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(size size of the operand in constant)
             @param(a value of constant to send)
             @param(cgpara where the parameter will be stored)
          }
          procedure a_param_const(list : taasmoutput;size : tcgsize;a : aint;const cgpara : TCGPara);virtual;
          {# Pass the value of a parameter, which is located in memory, to a routine.

             A generic version is provided. This routine should
             be overriden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(size size of the operand in constant)
             @param(r Memory reference of value to send)
             @param(cgpara where the parameter will be stored)
          }
          procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const cgpara : TCGPara);virtual;
          {# Pass the value of a parameter, which can be located either in a register or memory location,
             to a routine.

             A generic version is provided.

             @param(l location of the operand to send)
             @param(nr parameter number (starting from one) of routine (from left to right))
             @param(cgpara where the parameter will be stored)
          }
          procedure a_param_loc(list : taasmoutput;const l : tlocation;const cgpara : TCGPara);
          {# Pass the address of a reference to a routine. This routine
             will calculate the address of the reference, and pass this
             calculated address as a parameter.

             A generic version is provided. This routine should
             be overriden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(r reference to get address from)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const cgpara : TCGPara);virtual;

          { Remarks:
            * If a method specifies a size you have only to take care
              of that number of bits, i.e. load_const_reg with OP_8 must
              only load the lower 8 bit of the specified register
              the rest of the register can be undefined
              if  necessary the compiler will call a method
              to zero or sign extend the register
            * The a_load_XX_XX with OP_64 needn't to be
              implemented for 32 bit
              processors, the code generator takes care of that
            * the addr size is for work with the natural pointer
              size
            * the procedures without fpu/mm are only for integer usage
            * normally the first location is the source and the
              second the destination
          }

          {# Emits instruction to call the method specified by symbol name.
             This routine must be overriden for each new target cpu.

             There is no a_call_ref because loading the reference will use
             a temp register on most cpu's resulting in conflicts with the
             registers used for the parameters (PFV)
          }
          procedure a_call_name(list : taasmoutput;const s : string);virtual; abstract;
          procedure a_call_reg(list : taasmoutput;reg : tregister);virtual;abstract;

          { move instructions }
          procedure a_load_const_reg(list : taasmoutput;size : tcgsize;a : aint;register : tregister);virtual; abstract;
          procedure a_load_const_ref(list : taasmoutput;size : tcgsize;a : aint;const ref : treference);virtual;
          procedure a_load_const_loc(list : taasmoutput;a : aint;const loc : tlocation);
          procedure a_load_reg_ref(list : taasmoutput;fromsize,tosize : tcgsize;register : tregister;const ref : treference);virtual; abstract;
          procedure a_load_reg_reg(list : taasmoutput;fromsize,tosize : tcgsize;reg1,reg2 : tregister);virtual; abstract;
          procedure a_load_reg_loc(list : taasmoutput;fromsize : tcgsize;reg : tregister;const loc: tlocation);
          procedure a_load_ref_reg(list : taasmoutput;fromsize,tosize : tcgsize;const ref : treference;register : tregister);virtual; abstract;
          procedure a_load_ref_ref(list : taasmoutput;fromsize,tosize : tcgsize;const sref : treference;const dref : treference);virtual;
          procedure a_load_loc_reg(list : taasmoutput;tosize: tcgsize; const loc: tlocation; reg : tregister);
          procedure a_load_loc_ref(list : taasmoutput;tosize: tcgsize; const loc: tlocation; const ref : treference);
          procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);virtual; abstract;

          { fpu move instructions }
          procedure a_loadfpu_reg_reg(list: taasmoutput; size:tcgsize; reg1, reg2: tregister); virtual; abstract;
          procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); virtual; abstract;
          procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); virtual; abstract;
          procedure a_loadfpu_loc_reg(list: taasmoutput; const loc: tlocation; const reg: tregister);
          procedure a_loadfpu_reg_loc(list: taasmoutput; size: tcgsize; const reg: tregister; const loc: tlocation);
          procedure a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const cgpara : TCGPara);virtual;
          procedure a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const cgpara : TCGPara);virtual;

          { vector register move instructions }
          procedure a_loadmm_reg_reg(list: taasmoutput; fromsize, tosize : tcgsize;reg1, reg2: tregister;shuffle : pmmshuffle); virtual; abstract;
          procedure a_loadmm_ref_reg(list: taasmoutput; fromsize, tosize : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle); virtual; abstract;
          procedure a_loadmm_reg_ref(list: taasmoutput; fromsize, tosize : tcgsize;reg: tregister; const ref: treference;shuffle : pmmshuffle); virtual; abstract;
          procedure a_loadmm_loc_reg(list: taasmoutput; size: tcgsize; const loc: tlocation; const reg: tregister;shuffle : pmmshuffle);
          procedure a_loadmm_reg_loc(list: taasmoutput; size: tcgsize; const reg: tregister; const loc: tlocation;shuffle : pmmshuffle);
          procedure a_parammm_reg(list: taasmoutput; size: tcgsize; reg: tregister;const cgpara : TCGPara;shuffle : pmmshuffle); virtual;
          procedure a_parammm_ref(list: taasmoutput; size: tcgsize; const ref: treference;const cgpara : TCGPara;shuffle : pmmshuffle); virtual;
          procedure a_parammm_loc(list: taasmoutput; const loc: tlocation; const cgpara : TCGPara;shuffle : pmmshuffle); virtual;
          procedure a_opmm_reg_reg(list: taasmoutput; Op: TOpCG; size : tcgsize;src,dst: tregister;shuffle : pmmshuffle); virtual;abstract;
          procedure a_opmm_ref_reg(list: taasmoutput; Op: TOpCG; size : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle); virtual;
          procedure a_opmm_loc_reg(list: taasmoutput; Op: TOpCG; size : tcgsize;const loc: tlocation; reg: tregister;shuffle : pmmshuffle); virtual;
          procedure a_opmm_reg_ref(list: taasmoutput; Op: TOpCG; size : tcgsize;reg: tregister;const ref: treference; shuffle : pmmshuffle); virtual;

          { basic arithmetic operations }
          { note: for operators which require only one argument (not, neg), use }
          { the op_reg_reg, op_reg_ref or op_reg_loc methods and keep in mind   }
          { that in this case the *second* operand is used as both source and   }
          { destination (JM)                                                    }
          procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: Aint; reg: TRegister); virtual; abstract;
          procedure a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: Aint; const ref: TReference); virtual;
          procedure a_op_const_loc(list : taasmoutput; Op: TOpCG; a: Aint; const loc: tlocation);
          procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister); virtual; abstract;
          procedure a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; reg: TRegister; const ref: TReference); virtual;
          procedure a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister); virtual;
          procedure a_op_reg_loc(list : taasmoutput; Op: TOpCG; reg: tregister; const loc: tlocation);
          procedure a_op_ref_loc(list : taasmoutput; Op: TOpCG; const ref: TReference; const loc: tlocation);

          { trinary operations for processors that support them, 'emulated' }
          { on others. None with "ref" arguments since I don't think there  }
          { are any processors that support it (JM)                         }
          procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister); virtual;
          procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg; size: tcgsize; src1, src2, dst: tregister); virtual;
          procedure a_op_const_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister;setflags : boolean;var ovloc : tlocation); virtual;
          procedure a_op_reg_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation); virtual;

          {  comparison operations }
          procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
            l : tasmlabel);virtual; abstract;
          procedure a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aint;const ref : treference;
            l : tasmlabel); virtual;
          procedure a_cmp_const_loc_label(list: taasmoutput; size: tcgsize;cmp_op: topcmp; a: aint; const loc: tlocation;
            l : tasmlabel);
          procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); virtual; abstract;
          procedure a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; const ref: treference; reg : tregister; l : tasmlabel); virtual;
          procedure a_cmp_reg_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister; const ref: treference; l : tasmlabel); virtual;
          procedure a_cmp_loc_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; const loc: tlocation; reg : tregister; l : tasmlabel);
          procedure a_cmp_reg_loc_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; reg: tregister; const loc: tlocation; l : tasmlabel);
          procedure a_cmp_ref_loc_label(list: taasmoutput; size: tcgsize;cmp_op: topcmp; const ref: treference; const loc: tlocation;
            l : tasmlabel);

          procedure a_jmp_name(list : taasmoutput;const s : string); virtual; abstract;
          procedure a_jmp_always(list : taasmoutput;l: tasmlabel); virtual; abstract;
          procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); virtual; abstract;

          {# Depending on the value to check in the flags, either sets the register reg to one (if the flag is set)
             or zero (if the flag is cleared). The size parameter indicates the destination size register.
          }
          procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister); virtual; abstract;
          procedure g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref:TReference); virtual;

          {
             This routine tries to optimize the const_reg opcode, and should be
             called at the start of a_op_const_reg. It returns the actual opcode
             to emit, and the constant value to emit. If this routine returns
             TRUE, @var(no) instruction should be emitted (.eg : imul reg by 1 )

             @param(op The opcode to emit, returns the opcode which must be emitted)
             @param(a  The constant which should be emitted, returns the constant which must
                    be emitted)
             @param(reg The register to emit the opcode with, returns the register with
                   which the opcode will be emitted)
          }
          function optimize_op_const_reg(list: taasmoutput; var op: topcg; var a : aint; var reg: tregister): boolean;virtual;

         {#
             This routine is used in exception management nodes. It should
             save the exception reason currently in the FUNCTION_RETURN_REG. The
             save should be done either to a temp (pointed to by href).
             or on the stack (pushing the value on the stack).

             The size of the value to save is OS_S32. The default version
             saves the exception reason to a temp. memory area.
          }
         procedure g_exception_reason_save(list : taasmoutput; const href : treference);virtual;
         {#
             This routine is used in exception management nodes. It should
             save the exception reason constant. The
             save should be done either to a temp (pointed to by href).
             or on the stack (pushing the value on the stack).

             The size of the value to save is OS_S32. The default version
             saves the exception reason to a temp. memory area.
          }
         procedure g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aint);virtual;
         {#
             This routine is used in exception management nodes. It should
             load the exception reason to the FUNCTION_RETURN_REG. The saved value
             should either be in the temp. area (pointed to by href , href should
             *NOT* be freed) or on the stack (the value should be popped).

             The size of the value to save is OS_S32. The default version
             saves the exception reason to a temp. memory area.
          }
         procedure g_exception_reason_load(list : taasmoutput; const href : treference);virtual;

          procedure g_maybe_testself(list : taasmoutput;reg:tregister);
          procedure g_maybe_testvmt(list : taasmoutput;reg:tregister;objdef:tobjectdef);
          {# This should emit the opcode to copy len bytes from the source
             to destination.

             It must be overriden for each new target processor.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)

          }
          procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aint);virtual; abstract;
          {# This should emit the opcode to copy len bytes from the an unaligned source
             to destination.

             It must be overriden for each new target processor.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)

          }
          procedure g_concatcopy_unaligned(list : taasmoutput;const source,dest : treference;len : aint);virtual;
          {# This should emit the opcode to a shortrstring from the source
             to destination.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)

          }
          procedure g_copyshortstring(list : taasmoutput;const source,dest : treference;len:byte);

          procedure g_incrrefcount(list : taasmoutput;t: tdef; const ref: treference);
          procedure g_decrrefcount(list : taasmoutput;t: tdef; const ref: treference);
          procedure g_initialize(list : taasmoutput;t : tdef;const ref : treference);
          procedure g_finalize(list : taasmoutput;t : tdef;const ref : treference);

          {# Generates range checking code. It is to note
             that this routine does not need to be overriden,
             as it takes care of everything.

             @param(p Node which contains the value to check)
             @param(todef Type definition of node to range check)
          }
          procedure g_rangecheck(list: taasmoutput; const l:tlocation; fromdef,todef: tdef); virtual;

          {# Generates overflow checking code for a node }
          procedure g_overflowcheck(list: taasmoutput; const Loc:tlocation; def:tdef); virtual;abstract;
          procedure g_overflowCheck_loc(List:TAasmOutput;const Loc:TLocation;def:TDef;ovloc : tlocation);virtual;

          procedure g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);virtual;
          procedure g_releasevaluepara_openarray(list : taasmoutput;const l:tlocation);virtual;

          {# Emits instructions when compilation is done in profile
             mode (this is set as a command line option). The default
             behavior does nothing, should be overriden as required.
          }
          procedure g_profilecode(list : taasmoutput);virtual;
          {# Emits instruction for allocating @var(size) bytes at the stackpointer

             @param(size Number of bytes to allocate)
          }
          procedure g_stackpointer_alloc(list : taasmoutput;size : longint);virtual; abstract;
          {# Emits instruction for allocating the locals in entry
             code of a routine. This is one of the first
             routine called in @var(genentrycode).

             @param(localsize Number of bytes to allocate as locals)
          }
          procedure g_proc_entry(list : taasmoutput;localsize : longint;nostackframe:boolean);virtual; abstract;
          {# Emits instructions for returning from a subroutine.
             Should also restore the framepointer and stack.

             @param(parasize  Number of bytes of parameters to deallocate from stack)
          }
          procedure g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);virtual;abstract;
          {# This routine is called when generating the code for the entry point
             of a routine. It should save all registers which are not used in this
             routine, and which should be declared as saved in the std_saved_registers
             set.

             This routine is mainly used when linking to code which is generated
             by ABI-compliant compilers (like GCC), to make sure that the reserved
             registers of that ABI are not clobbered.

             @param(usedinproc Registers which are used in the code of this routine)
          }
          procedure g_save_standard_registers(list:Taasmoutput);virtual;
          {# This routine is called when generating the code for the exit point
             of a routine. It should restore all registers which were previously
             saved in @var(g_save_standard_registers).

             @param(usedinproc Registers which are used in the code of this routine)
          }
          procedure g_restore_standard_registers(list:Taasmoutput);virtual;
          procedure g_intf_wrapper(list: TAAsmoutput; procdef: tprocdef; const labelname: string; ioffset: longint);virtual;abstract;
          procedure g_adjust_self_value(list:taasmoutput;procdef: tprocdef;ioffset: aint);virtual;
       end;

{$ifndef cpu64bit}
    {# @abstract(Abstract code generator for 64 Bit operations)
       This class implements an abstract code generator class
       for 64 Bit operations.
    }
    tcg64 = class
        procedure a_load64_const_ref(list : taasmoutput;value : int64;const ref : treference);virtual;abstract;
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);virtual;abstract;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);virtual;abstract;
        procedure a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);virtual;abstract;
        procedure a_load64_const_reg(list : taasmoutput;value : int64;reg : tregister64);virtual;abstract;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);virtual;abstract;
        procedure a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);virtual;abstract;
        procedure a_load64_const_loc(list : taasmoutput;value : int64;const l : tlocation);virtual;abstract;
        procedure a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);virtual;abstract;

        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);virtual;abstract;
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);virtual;abstract;
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);virtual;abstract;
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);virtual;abstract;
        procedure a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);virtual;abstract;
        procedure a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);virtual;abstract;

        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;size : tcgsize;const ref : treference;reg : tregister64);virtual;abstract;
        procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;size : tcgsize;regsrc,regdst : tregister64);virtual;abstract;
        procedure a_op64_reg_ref(list : taasmoutput;op:TOpCG;size : tcgsize;regsrc : tregister64;const ref : treference);virtual;abstract;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;size : tcgsize;value : int64;regdst : tregister64);virtual;abstract;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;size : tcgsize;value : int64;const ref : treference);virtual;abstract;
        procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;size : tcgsize;value : int64;const l: tlocation);virtual;abstract;
        procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;size : tcgsize;reg : tregister64;const l : tlocation);virtual;abstract;
        procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;size : tcgsize;const l : tlocation;reg64 : tregister64);virtual;abstract;
        procedure a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64);virtual;
        procedure a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);virtual;
        procedure a_op64_const_reg_reg_checkoverflow(list: taasmoutput;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);virtual;
        procedure a_op64_reg_reg_reg_checkoverflow(list: taasmoutput;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);virtual;

        procedure a_param64_reg(list : taasmoutput;reg64 : tregister64;const loc : TCGPara);virtual;abstract;
        procedure a_param64_const(list : taasmoutput;value : int64;const loc : TCGPara);virtual;abstract;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const loc : TCGPara);virtual;abstract;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;const loc : TCGPara);virtual;abstract;

        {
             This routine tries to optimize the const_reg opcode, and should be
             called at the start of a_op64_const_reg. It returns the actual opcode
             to emit, and the constant value to emit. If this routine returns
             TRUE, @var(no) instruction should be emitted (.eg : imul reg by 1 )

             @param(op The opcode to emit, returns the opcode which must be emitted)
             @param(a  The constant which should be emitted, returns the constant which must
                    be emitted)
             @param(reg The register to emit the opcode with, returns the register with
                   which the opcode will be emitted)
        }
        function optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : int64; var reg: tregister64): boolean;virtual;abstract;


        { override to catch 64bit rangechecks }
        procedure g_rangecheck64(list: taasmoutput; const l:tlocation; fromdef,todef: tdef);virtual;abstract;
    end;
{$endif cpu64bit}

    var
       {# Main code generator class }
       cg : tcg;
{$ifndef cpu64bit}
       {# Code generator class for all operations working with 64-Bit operands }
       cg64 : tcg64;
{$endif cpu64bit}


implementation

    uses
       globals,options,systems,
       verbose,defutil,paramgr,symsym,
       tgobj,cutils,procinfo;

    const
      { Please leave this here, this module should NOT use
        exprasmlist, the lists are always passed as arguments.
        Declaring it as string here results in an error when compiling (PFV) }
      exprasmlist = 'error';


{*****************************************************************************
                            basic functionallity
******************************************************************************}

    constructor tcg.create;
      begin
      end;


{*****************************************************************************
                                register allocation
******************************************************************************}


    procedure tcg.init_register_allocators;
      begin
        fillchar(rg,sizeof(rg),0);
        add_reg_instruction_hook:=@add_reg_instruction;
      end;


    procedure tcg.done_register_allocators;
      begin
        { Safety }
        fillchar(rg,sizeof(rg),0);
        add_reg_instruction_hook:=nil;
      end;

    {$ifdef flowgraph}
    procedure Tcg.init_flowgraph;

    begin
      aktflownode:=0;
    end;

    procedure Tcg.done_flowgraph;

    begin
    end;
    {$endif}

    function tcg.getintregister(list:Taasmoutput;size:Tcgsize):Tregister;
      begin
        if not assigned(rg[R_INTREGISTER]) then
          internalerror(200312122);
        result:=rg[R_INTREGISTER].getregister(list,cgsize2subreg(size));
      end;


    function tcg.getfpuregister(list:Taasmoutput;size:Tcgsize):Tregister;
      begin
        if not assigned(rg[R_FPUREGISTER]) then
          internalerror(200312123);
        result:=rg[R_FPUREGISTER].getregister(list,cgsize2subreg(size));
      end;


    function tcg.getmmregister(list:Taasmoutput;size:Tcgsize):Tregister;
      begin
        if not assigned(rg[R_MMREGISTER]) then
          internalerror(200312124);
        result:=rg[R_MMREGISTER].getregister(list,cgsize2subreg(size));
      end;


    function tcg.getaddressregister(list:Taasmoutput):Tregister;
      begin
        if assigned(rg[R_ADDRESSREGISTER]) then
          result:=rg[R_ADDRESSREGISTER].getregister(list,R_SUBWHOLE)
        else
          begin
            if not assigned(rg[R_INTREGISTER]) then
              internalerror(200312121);
            result:=rg[R_INTREGISTER].getregister(list,R_SUBWHOLE);
          end;
      end;


    function Tcg.makeregsize(list:Taasmoutput;reg:Tregister;size:Tcgsize):Tregister;
      var
        subreg:Tsubregister;
      begin
        subreg:=cgsize2subreg(size);
        result:=reg;
        setsubreg(result,subreg);
        { notify RA }
        if result<>reg then
          list.concat(tai_regalloc.resize(result));
      end;


    procedure tcg.getcpuregister(list:Taasmoutput;r:Tregister);
      begin
        if not assigned(rg[getregtype(r)]) then
          internalerror(200312125);
        rg[getregtype(r)].getcpuregister(list,r);
      end;


    procedure tcg.ungetcpuregister(list:Taasmoutput;r:Tregister);
      begin
        if not assigned(rg[getregtype(r)]) then
          internalerror(200312126);
        rg[getregtype(r)].ungetcpuregister(list,r);
      end;


    procedure tcg.alloccpuregisters(list:Taasmoutput;rt:Tregistertype;const r:Tcpuregisterset);
      begin
        if assigned(rg[rt]) then
          rg[rt].alloccpuregisters(list,r)
        else
          internalerror(200310092);
      end;


    procedure tcg.dealloccpuregisters(list:Taasmoutput;rt:Tregistertype;const r:Tcpuregisterset);
      begin
        if assigned(rg[rt]) then
          rg[rt].dealloccpuregisters(list,r)
        else
          internalerror(200310093);
      end;


    function tcg.uses_registers(rt:Tregistertype):boolean;
      begin
        if assigned(rg[rt]) then
          result:=rg[rt].uses_registers
        else
          result:=false;
      end;


    procedure tcg.add_reg_instruction(instr:Tai;r:tregister);
      var
        rt : tregistertype;
      begin
        rt:=getregtype(r);
        { Only add it when a register allocator is configured.
          No IE can be generated, because the VMT is written
          without a valid rg[] }
        if assigned(rg[rt]) then
          rg[rt].add_reg_instruction(instr,r);
      end;


    procedure tcg.add_move_instruction(instr:Taicpu);
      var
        rt : tregistertype;
      begin
        rt:=getregtype(instr.oper[O_MOV_SOURCE]^.reg);
        if assigned(rg[rt]) then
          rg[rt].add_move_instruction(instr)
        else
          internalerror(200310095);
      end;


    procedure tcg.set_regalloc_extend_backwards(b: boolean);
      var
        rt : tregistertype;
      begin
        for rt:=low(rg) to high(rg) do
          begin
            if assigned(rg[rt]) then
              rg[rt].extend_live_range_backwards := b;;
          end;
      end;


    procedure tcg.do_register_allocation(list:Taasmoutput;headertai:tai);
      var
        rt : tregistertype;
      begin
        for rt:=R_FPUREGISTER to R_SPECIALREGISTER do
          begin
            if assigned(rg[rt]) then
              rg[rt].do_register_allocation(list,headertai);
          end;
         { running the other register allocator passes could require addition int/addr. registers
           when spilling so run int/addr register allocation at the end }
         if assigned(rg[R_INTREGISTER]) then
           rg[R_INTREGISTER].do_register_allocation(list,headertai);
         if assigned(rg[R_ADDRESSREGISTER]) then
           rg[R_ADDRESSREGISTER].do_register_allocation(list,headertai);
      end;


    procedure tcg.a_reg_alloc(list : taasmoutput;r : tregister);
      begin
         list.concat(tai_regalloc.alloc(r,nil));
      end;


    procedure tcg.a_reg_dealloc(list : taasmoutput;r : tregister);
      begin
         list.concat(tai_regalloc.dealloc(r,nil));
      end;


    procedure tcg.a_reg_sync(list : taasmoutput;r : tregister);
      var
        instr : tai;
      begin
        instr:=tai_regalloc.sync(r);
        list.concat(instr);
        add_reg_instruction(instr,r);
      end;


    procedure tcg.a_label(list : taasmoutput;l : tasmlabel);
      begin
         list.concat(tai_label.create(l));
      end;


{*****************************************************************************
          for better code generation these methods should be overridden
******************************************************************************}

    procedure tcg.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const cgpara : TCGPara);
      var
         ref : treference;
      begin
         cgpara.check_simple_location;
         case cgpara.location^.loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_load_reg_reg(list,size,cgpara.location^.size,r,cgpara.location^.register);
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 reference_reset_base(ref,cgpara.location^.reference.index,cgpara.location^.reference.offset);
                 a_load_reg_ref(list,size,cgpara.location^.size,r,ref);
              end
            else
              internalerror(2002071004);
         end;
      end;


    procedure tcg.a_param_const(list : taasmoutput;size : tcgsize;a : aint;const cgpara : TCGPara);
      var
         ref : treference;
      begin
         cgpara.check_simple_location;
         case cgpara.location^.loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_load_const_reg(list,cgpara.location^.size,a,cgpara.location^.register);
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 reference_reset_base(ref,cgpara.location^.reference.index,cgpara.location^.reference.offset);
                 a_load_const_ref(list,cgpara.location^.size,a,ref);
              end
            else
              internalerror(2002071004);
         end;
      end;


    procedure tcg.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const cgpara : TCGPara);
      var
         ref : treference;
      begin
         cgpara.check_simple_location;
         case cgpara.location^.loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_load_ref_reg(list,size,cgpara.location^.size,r,cgpara.location^.register);
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 reference_reset(ref);
                 ref.base:=cgpara.location^.reference.index;
                 ref.offset:=cgpara.location^.reference.offset;
                 { use concatcopy, because it can also be a float which fails when
                   load_ref_ref is used }
                 g_concatcopy(list,r,ref,tcgsize2size[size]);
              end
            else
              internalerror(2002071004);
         end;
      end;


    procedure tcg.a_param_loc(list : taasmoutput;const l:tlocation;const cgpara : TCGPara);
      begin
        case l.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            a_param_reg(list,l.size,l.register,cgpara);
          LOC_CONSTANT :
            a_param_const(list,l.size,l.value,cgpara);
          LOC_CREFERENCE,
          LOC_REFERENCE :
            a_param_ref(list,l.size,l.reference,cgpara);
          else
            internalerror(2002032211);
        end;
      end;


    procedure tcg.a_paramaddr_ref(list : taasmoutput;const r : treference;const cgpara : TCGPara);
      var
         hr : tregister;
      begin
         cgpara.check_simple_location;
         hr:=getaddressregister(list);
         a_loadaddr_ref_reg(list,r,hr);
         a_param_reg(list,OS_ADDR,hr,cgpara);
      end;


{****************************************************************************
                       some generic implementations
****************************************************************************}

    procedure tcg.a_load_ref_ref(list : taasmoutput;fromsize,tosize : tcgsize;const sref : treference;const dref : treference);
      var
        tmpreg: tregister;
      begin
        { verify if we have the same reference }
        if references_equal(sref,dref) then
          exit;
        tmpreg:=getintregister(list,tosize);
        a_load_ref_reg(list,fromsize,tosize,sref,tmpreg);
        a_load_reg_ref(list,tosize,tosize,tmpreg,dref);
      end;


    procedure tcg.a_load_const_ref(list : taasmoutput;size : tcgsize;a : aint;const ref : treference);
      var
        tmpreg: tregister;
      begin
        tmpreg:=getintregister(list,size);
        a_load_const_reg(list,size,a,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
      end;


    procedure tcg.a_load_const_loc(list : taasmoutput;a : aint;const loc: tlocation);
      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_const_ref(list,loc.size,a,loc.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_const_reg(list,loc.size,a,loc.register);
          else
            internalerror(200203272);
        end;
      end;


    procedure tcg.a_load_reg_loc(list : taasmoutput;fromsize : tcgsize;reg : tregister;const loc: tlocation);
      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_reg_ref(list,fromsize,loc.size,reg,loc.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_reg(list,fromsize,loc.size,reg,loc.register);
          else
            internalerror(200203271);
        end;
      end;


    procedure tcg.a_load_loc_reg(list : taasmoutput; tosize: tcgsize; const loc: tlocation; reg : tregister);
      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_ref_reg(list,loc.size,tosize,loc.reference,reg);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_reg(list,loc.size,tosize,loc.register,reg);
          LOC_CONSTANT:
            a_load_const_reg(list,tosize,loc.value,reg);
          else
            internalerror(200109092);
        end;
      end;


    procedure tcg.a_load_loc_ref(list : taasmoutput;tosize: tcgsize; const loc: tlocation; const ref : treference);
      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_ref_ref(list,loc.size,tosize,loc.reference,ref);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_ref(list,loc.size,tosize,loc.register,ref);
          LOC_CONSTANT:
            a_load_const_ref(list,tosize,loc.value,ref);
          else
            internalerror(200109302);
        end;
      end;


    function tcg.optimize_op_const_reg(list: taasmoutput; var op: topcg; var a : aint; var reg:tregister): boolean;
      var
        powerval : longint;
      begin
        optimize_op_const_reg := false;
        case op of
          { or with zero returns same result }
          OP_OR : if a = 0 then optimize_op_const_reg := true;
          { and with max returns same result }
          OP_AND : if (a = high(a)) then optimize_op_const_reg := true;
          { division by 1 returns result }
          OP_DIV :
            begin
              if a = 1 then
                optimize_op_const_reg := true
              else if ispowerof2(int64(a), powerval) then
                begin
                  a := powerval;
                  op:= OP_SHR;
                end;
              exit;
            end;
          OP_IDIV:
            begin
              if a = 1 then
                optimize_op_const_reg := true
              else if ispowerof2(int64(a), powerval) then
                begin
                  a := powerval;
                  op:= OP_SAR;
                end;
               exit;
            end;
        OP_MUL,OP_IMUL:
            begin
               if a = 1 then
                  optimize_op_const_reg := true
               else if ispowerof2(int64(a), powerval) then
                 begin
                   a := powerval;
                   op:= OP_SHL;
                 end;
               exit;
            end;
        OP_SAR,OP_SHL,OP_SHR:
           begin
              if a = 0 then
                 optimize_op_const_reg := true;
              exit;
           end;
        end;
      end;


    procedure tcg.a_loadfpu_loc_reg(list: taasmoutput; const loc: tlocation; const reg: tregister);
      begin
        case loc.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_loadfpu_ref_reg(list,loc.size,loc.reference,reg);
          LOC_FPUREGISTER, LOC_CFPUREGISTER:
            a_loadfpu_reg_reg(list,loc.size,loc.register,reg);
          else
            internalerror(200203301);
        end;
      end;


    procedure tcg.a_loadfpu_reg_loc(list: taasmoutput; size: tcgsize; const reg: tregister; const loc: tlocation);
      begin
        case loc.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_loadfpu_reg_ref(list,size,reg,loc.reference);
          LOC_FPUREGISTER, LOC_CFPUREGISTER:
            a_loadfpu_reg_reg(list,size,reg,loc.register);
          else
            internalerror(48991);
         end;
      end;


    procedure tcg.a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const cgpara : TCGPara);
      var
         ref : treference;
      begin
         case cgpara.location^.loc of
            LOC_FPUREGISTER,LOC_CFPUREGISTER:
              begin
                cgpara.check_simple_location;
                a_loadfpu_reg_reg(list,size,r,cgpara.location^.register);
              end;
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                cgpara.check_simple_location;
                reference_reset_base(ref,cgpara.location^.reference.index,cgpara.location^.reference.offset);
                a_loadfpu_reg_ref(list,size,r,ref);
              end;
            LOC_REGISTER,LOC_CREGISTER:
              begin
                { paramfpu_ref does the check_simpe_location check here if necessary }
                tg.GetTemp(list,TCGSize2Size[size],tt_normal,ref);
                a_loadfpu_reg_ref(list,size,r,ref);
                a_paramfpu_ref(list,size,ref,cgpara);
                tg.Ungettemp(list,ref);
              end;
            else
              internalerror(2002071004);
         end;
      end;


    procedure tcg.a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const cgpara : TCGPara);
      var
         href : treference;
      begin
         cgpara.check_simple_location;
         case cgpara.location^.loc of
          LOC_FPUREGISTER,LOC_CFPUREGISTER:
            a_loadfpu_ref_reg(list,size,ref,cgpara.location^.register);
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              reference_reset_base(href,cgpara.location^.reference.index,cgpara.location^.reference.offset);
              { concatcopy should choose the best way to copy the data }
              g_concatcopy(list,ref,href,tcgsize2size[size]);
            end;
          else
            internalerror(200402201);
        end;
      end;


    procedure tcg.a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: aint; const ref: TReference);
      var
        tmpreg : tregister;
      begin
        tmpreg:=getintregister(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_op_const_reg(list,op,size,a,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
      end;


    procedure tcg.a_op_const_loc(list : taasmoutput; Op: TOpCG; a: aint; const loc: tlocation);
      begin
        case loc.loc of
          LOC_REGISTER, LOC_CREGISTER:
            a_op_const_reg(list,op,loc.size,a,loc.register);
          LOC_REFERENCE, LOC_CREFERENCE:
            a_op_const_ref(list,op,loc.size,a,loc.reference);
          else
            internalerror(200109061);
        end;
      end;


    procedure tcg.a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize;reg: TRegister;  const ref: TReference);
      var
        tmpreg : tregister;
      begin
        tmpreg:=getintregister(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_op_reg_reg(list,op,size,reg,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
      end;


    procedure tcg.a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);

      var
        tmpreg: tregister;

      begin
        case op of
          OP_NOT,OP_NEG:
            { handle it as "load ref,reg; op reg" }
            begin
              a_load_ref_reg(list,size,size,ref,reg);
              a_op_reg_reg(list,op,size,reg,reg);
            end;
          else
            begin
              tmpreg:=getintregister(list,size);
              a_load_ref_reg(list,size,size,ref,tmpreg);
              a_op_reg_reg(list,op,size,tmpreg,reg);
            end;
        end;
      end;


    procedure tcg.a_op_reg_loc(list : taasmoutput; Op: TOpCG; reg: tregister; const loc: tlocation);

      begin
        case loc.loc of
          LOC_REGISTER, LOC_CREGISTER:
            a_op_reg_reg(list,op,loc.size,reg,loc.register);
          LOC_REFERENCE, LOC_CREFERENCE:
            a_op_reg_ref(list,op,loc.size,reg,loc.reference);
          else
            internalerror(200109061);
        end;
      end;


    procedure tcg.a_op_ref_loc(list : taasmoutput; Op: TOpCG; const ref: TReference; const loc: tlocation);

      var
        tmpreg: tregister;

      begin
        case loc.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_op_ref_reg(list,op,loc.size,ref,loc.register);
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              tmpreg:=getintregister(list,loc.size);
              a_load_ref_reg(list,loc.size,loc.size,ref,tmpreg);
              a_op_reg_ref(list,op,loc.size,tmpreg,loc.reference);
            end;
          else
            internalerror(200109061);
        end;
      end;

    procedure Tcg.a_op_const_reg_reg(list:Taasmoutput;op:Topcg;size:Tcgsize;
                                     a:aint;src,dst:Tregister);

    begin
      a_load_reg_reg(list,size,size,src,dst);
      a_op_const_reg(list,op,size,a,dst);
    end;

    procedure tcg.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
        size: tcgsize; src1, src2, dst: tregister);
      var
        tmpreg: tregister;
      begin
        if (dst<>src1) then
          begin
            a_load_reg_reg(list,size,size,src2,dst);
            a_op_reg_reg(list,op,size,src1,dst);
          end
        else
          begin
            tmpreg:=getintregister(list,size);
            a_load_reg_reg(list,size,size,src2,tmpreg);
            a_op_reg_reg(list,op,size,src1,tmpreg);
            a_load_reg_reg(list,size,size,tmpreg,dst);
          end;
      end;


    procedure tcg.a_op_const_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; a: aint; src, dst: tregister;setflags : boolean;var ovloc : tlocation);
      begin
        a_op_const_reg_reg(list,op,size,a,src,dst);
        ovloc.loc:=LOC_VOID;
      end;


    procedure tcg.a_op_reg_reg_reg_checkoverflow(list: taasmoutput; op: TOpCg; size: tcgsize; src1, src2, dst: tregister;setflags : boolean;var ovloc : tlocation);
      begin
        a_op_reg_reg_reg(list,op,size,src1,src2,dst);
        ovloc.loc:=LOC_VOID;
      end;


    procedure tcg.a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aint;const ref : treference;
     l : tasmlabel);

      var
        tmpreg: tregister;
      begin
        tmpreg:=getintregister(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_cmp_const_reg_label(list,size,cmp_op,a,tmpreg,l);
      end;


    procedure tcg.a_cmp_const_loc_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aint;const loc : tlocation;
      l : tasmlabel);

      begin
        case loc.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_cmp_const_reg_label(list,size,cmp_op,a,loc.register,l);
          LOC_REFERENCE,LOC_CREFERENCE:
            a_cmp_const_ref_label(list,size,cmp_op,a,loc.reference,l);
          else
            internalerror(200109061);
        end;
      end;


    procedure tcg.a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; const ref: treference; reg : tregister; l : tasmlabel);
      var
        tmpreg: tregister;
      begin
        tmpreg:=getintregister(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
      end;


    procedure tcg.a_cmp_reg_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; reg : tregister; const ref: treference; l : tasmlabel);
      var
        tmpreg: tregister;
      begin
        tmpreg:=getintregister(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_cmp_reg_reg_label(list,size,cmp_op,reg,tmpreg,l);
      end;


    procedure tcg.a_cmp_reg_loc_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; reg: tregister; const loc: tlocation; l : tasmlabel);
      begin
        a_cmp_loc_reg_label(list,size,swap_opcmp(cmp_op),loc,reg,l);
      end;


    procedure tcg.a_cmp_loc_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; const loc: tlocation; reg : tregister; l : tasmlabel);
      begin
        case loc.loc of
          LOC_REGISTER,
          LOC_CREGISTER:
            a_cmp_reg_reg_label(list,size,cmp_op,loc.register,reg,l);
          LOC_REFERENCE,
          LOC_CREFERENCE :
            a_cmp_ref_reg_label(list,size,cmp_op,loc.reference,reg,l);
          LOC_CONSTANT:
            a_cmp_const_reg_label(list,size,cmp_op,loc.value,reg,l);
          else
            internalerror(200203231);
        end;
      end;


    procedure tcg.a_cmp_ref_loc_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;const ref: treference;const loc : tlocation;
      l : tasmlabel);
      var
        tmpreg: tregister;
      begin
        case loc.loc of
          LOC_REGISTER,LOC_CREGISTER:
            a_cmp_ref_reg_label(list,size,cmp_op,ref,loc.register,l);
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              tmpreg:=getintregister(list,size);
              a_load_ref_reg(list,size,size,loc.reference,tmpreg);
              a_cmp_ref_reg_label(list,size,cmp_op,ref,tmpreg,l);
            end
          else
            internalerror(200109061);
        end;
      end;


    procedure tcg.a_loadmm_loc_reg(list: taasmoutput; size: tcgsize; const loc: tlocation; const reg: tregister;shuffle : pmmshuffle);
      begin
        case loc.loc of
          LOC_MMREGISTER,LOC_CMMREGISTER:
            a_loadmm_reg_reg(list,loc.size,size,loc.register,reg,shuffle);
          LOC_REFERENCE,LOC_CREFERENCE:
            a_loadmm_ref_reg(list,loc.size,size,loc.reference,reg,shuffle);
          else
            internalerror(200310121);
        end;
      end;


    procedure tcg.a_loadmm_reg_loc(list: taasmoutput; size: tcgsize; const reg: tregister; const loc: tlocation;shuffle : pmmshuffle);
      begin
        case loc.loc of
          LOC_MMREGISTER,LOC_CMMREGISTER:
            a_loadmm_reg_reg(list,size,loc.size,reg,loc.register,shuffle);
          LOC_REFERENCE,LOC_CREFERENCE:
            a_loadmm_reg_ref(list,size,loc.size,reg,loc.reference,shuffle);
          else
            internalerror(200310122);
        end;
      end;


    procedure tcg.a_parammm_reg(list: taasmoutput; size: tcgsize; reg: tregister;const cgpara : TCGPara;shuffle : pmmshuffle);
      var
        href : treference;
      begin
         cgpara.check_simple_location;
         case cgpara.location^.loc of
          LOC_MMREGISTER,LOC_CMMREGISTER:
            a_loadmm_reg_reg(list,size,cgpara.location^.size,reg,cgpara.location^.register,shuffle);
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              reference_reset_base(href,cgpara.location^.reference.index,cgpara.location^.reference.offset);
              a_loadmm_reg_ref(list,size,cgpara.location^.size,reg,href,shuffle);
            end
          else
            internalerror(200310123);
        end;
      end;


    procedure tcg.a_parammm_ref(list: taasmoutput; size: tcgsize;const ref: treference;const cgpara : TCGPara;shuffle : pmmshuffle);
      var
         hr : tregister;
         hs : tmmshuffle;
      begin
         cgpara.check_simple_location;
         hr:=getmmregister(list,cgpara.location^.size);
         a_loadmm_ref_reg(list,size,cgpara.location^.size,ref,hr,shuffle);
         if realshuffle(shuffle) then
           begin
             hs:=shuffle^;
             removeshuffles(hs);
             a_parammm_reg(list,cgpara.location^.size,hr,cgpara,@hs);
           end
         else
           a_parammm_reg(list,cgpara.location^.size,hr,cgpara,shuffle);
      end;


    procedure tcg.a_parammm_loc(list: taasmoutput;const loc: tlocation; const cgpara : TCGPara;shuffle : pmmshuffle);
      begin
        case loc.loc of
          LOC_MMREGISTER,LOC_CMMREGISTER:
            a_parammm_reg(list,loc.size,loc.register,cgpara,shuffle);
          LOC_REFERENCE,LOC_CREFERENCE:
            a_parammm_ref(list,loc.size,loc.reference,cgpara,shuffle);
          else
            internalerror(200310123);
        end;
      end;


    procedure tcg.a_opmm_ref_reg(list: taasmoutput; Op: TOpCG; size : tcgsize;const ref: treference; reg: tregister;shuffle : pmmshuffle);
      var
         hr : tregister;
         hs : tmmshuffle;
      begin
         hr:=getmmregister(list,size);
         a_loadmm_ref_reg(list,size,size,ref,hr,shuffle);
         if realshuffle(shuffle) then
           begin
             hs:=shuffle^;
             removeshuffles(hs);
             a_opmm_reg_reg(list,op,size,hr,reg,@hs);
           end
         else
           a_opmm_reg_reg(list,op,size,hr,reg,shuffle);
      end;


    procedure tcg.a_opmm_reg_ref(list: taasmoutput; Op: TOpCG; size : tcgsize;reg: tregister; const ref: treference; shuffle : pmmshuffle);
      var
         hr : tregister;
         hs : tmmshuffle;
      begin
         hr:=getmmregister(list,size);
         a_loadmm_ref_reg(list,size,size,ref,hr,shuffle);
         if realshuffle(shuffle) then
           begin
             hs:=shuffle^;
             removeshuffles(hs);
             a_opmm_reg_reg(list,op,size,reg,hr,@hs);
             a_loadmm_reg_ref(list,size,size,hr,ref,@hs);
           end
         else
           begin
             a_opmm_reg_reg(list,op,size,reg,hr,shuffle);
             a_loadmm_reg_ref(list,size,size,hr,ref,shuffle);
           end;
      end;


    procedure tcg.a_opmm_loc_reg(list: taasmoutput; Op: TOpCG; size : tcgsize;const loc: tlocation; reg: tregister;shuffle : pmmshuffle);
      begin
        case loc.loc of
          LOC_CMMREGISTER,LOC_MMREGISTER:
            a_opmm_reg_reg(list,op,size,loc.register,reg,shuffle);
          LOC_CREFERENCE,LOC_REFERENCE:
            a_opmm_ref_reg(list,op,size,loc.reference,reg,shuffle);
          else
            internalerror(200312232);
        end;
      end;


    procedure tcg.g_concatcopy_unaligned(list : taasmoutput;const source,dest : treference;len : aint);
      begin
        g_concatcopy(list,source,dest,len);
      end;


    procedure tcg.g_copyshortstring(list : taasmoutput;const source,dest : treference;len:byte);
      var
        cgpara1,cgpara2,cgpara3 : TCGPara;
      begin
        cgpara1.init;
        cgpara2.init;
        cgpara3.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.getintparaloc(pocall_default,2,cgpara2);
        paramanager.getintparaloc(pocall_default,3,cgpara3);
        paramanager.allocparaloc(list,cgpara3);
        a_paramaddr_ref(list,dest,cgpara3);
        paramanager.allocparaloc(list,cgpara2);
        a_paramaddr_ref(list,source,cgpara2);
        paramanager.allocparaloc(list,cgpara1);
        a_param_const(list,OS_INT,len,cgpara1);
        paramanager.freeparaloc(list,cgpara3);
        paramanager.freeparaloc(list,cgpara2);
        paramanager.freeparaloc(list,cgpara1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        a_call_name(list,'FPC_SHORTSTR_ASSIGN');
        dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cgpara3.done;
        cgpara2.done;
        cgpara1.done;
      end;


    procedure tcg.g_incrrefcount(list : taasmoutput;t: tdef; const ref: treference);
      var
        href : treference;
        incrfunc : string;
        cgpara1,cgpara2 : TCGPara;
      begin
         cgpara1.init;
         cgpara2.init;
         paramanager.getintparaloc(pocall_default,1,cgpara1);
         paramanager.getintparaloc(pocall_default,2,cgpara2);
         if is_interfacecom(t) then
          incrfunc:='FPC_INTF_INCR_REF'
         else if is_ansistring(t) then
       {$ifdef ansistring_bits}
           begin
             case Tstringdef(t).string_typ of
               st_ansistring16:
                 incrfunc:='FPC_ANSISTR16_INCR_REF';
               st_ansistring32:
                 incrfunc:='FPC_ANSISTR32_INCR_REF';
               st_ansistring64:
                 incrfunc:='FPC_ANSISTR64_INCR_REF';
             end;
           end
       {$else}
            incrfunc:='FPC_ANSISTR_INCR_REF'
       {$endif}
         else if is_widestring(t) then
          incrfunc:='FPC_WIDESTR_INCR_REF'
         else if is_dynamic_array(t) then
          incrfunc:='FPC_DYNARRAY_INCR_REF'
         else
          incrfunc:='';
         { call the special incr function or the generic addref }
         if incrfunc<>'' then
          begin
            paramanager.allocparaloc(list,cgpara1);
            { these functions get the pointer by value }
            a_param_ref(list,OS_ADDR,ref,cgpara1);
            paramanager.freeparaloc(list,cgpara1);
            alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,incrfunc);
            dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
          end
         else
          begin
            reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
            paramanager.allocparaloc(list,cgpara2);
            a_paramaddr_ref(list,href,cgpara2);
            paramanager.allocparaloc(list,cgpara1);
            a_paramaddr_ref(list,ref,cgpara1);
            paramanager.freeparaloc(list,cgpara1);
            paramanager.freeparaloc(list,cgpara2);
            alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,'FPC_ADDREF');
            dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
          end;
         cgpara2.done;
         cgpara1.done;
      end;


    procedure tcg.g_decrrefcount(list : taasmoutput;t: tdef; const ref: treference);
      var
        href : treference;
        decrfunc : string;
        needrtti : boolean;
        cgpara1,cgpara2 : TCGPara;
        tempreg1,tempreg2 : TRegister;
      begin
        cgpara1.init;
        cgpara2.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.getintparaloc(pocall_default,2,cgpara2);
        needrtti:=false;
        if is_interfacecom(t) then
          decrfunc:='FPC_INTF_DECR_REF'
        else if is_ansistring(t) then
       {$ifdef ansistring_bits}
           begin
             case Tstringdef(t).string_typ of
               st_ansistring16:
                 decrfunc:='FPC_ANSISTR16_DECR_REF';
               st_ansistring32:
                 decrfunc:='FPC_ANSISTR32_DECR_REF';
               st_ansistring64:
                 decrfunc:='FPC_ANSISTR64_DECR_REF';
             end;
           end
       {$else}
            decrfunc:='FPC_ANSISTR_DECR_REF'
       {$endif}
         else if is_widestring(t) then
          decrfunc:='FPC_WIDESTR_DECR_REF'
         else if is_dynamic_array(t) then
          begin
            decrfunc:='FPC_DYNARRAY_DECR_REF';
            needrtti:=true;
          end
         else
          decrfunc:='';
         { call the special decr function or the generic decref }
         if decrfunc<>'' then
          begin
            if needrtti then
             begin
               reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
               tempreg2:=getaddressregister(list);
               a_loadaddr_ref_reg(list,href,tempreg2);
             end;
            tempreg1:=getaddressregister(list);
            a_loadaddr_ref_reg(list,ref,tempreg1);
            if needrtti then
              begin
                paramanager.allocparaloc(list,cgpara2);
                a_param_reg(list,OS_ADDR,tempreg2,cgpara2);
                paramanager.freeparaloc(list,cgpara2);
              end;
            paramanager.allocparaloc(list,cgpara1);
            a_param_reg(list,OS_ADDR,tempreg1,cgpara1);
            paramanager.freeparaloc(list,cgpara1);
            alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,decrfunc);
            dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
          end
         else
          begin
            reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
            paramanager.allocparaloc(list,cgpara2);
            a_paramaddr_ref(list,href,cgpara2);
            paramanager.allocparaloc(list,cgpara1);
            a_paramaddr_ref(list,ref,cgpara1);
            paramanager.freeparaloc(list,cgpara1);
            paramanager.freeparaloc(list,cgpara2);
            alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,'FPC_DECREF');
            dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
         end;
        cgpara2.done;
        cgpara1.done;
      end;


    procedure tcg.g_initialize(list : taasmoutput;t : tdef;const ref : treference);
      var
         href : treference;
         cgpara1,cgpara2 : TCGPara;
      begin
        cgpara1.init;
        cgpara2.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.getintparaloc(pocall_default,2,cgpara2);
         if is_ansistring(t) or
            is_widestring(t) or
            is_interfacecom(t) or
            is_dynamic_array(t) then
           a_load_const_ref(list,OS_ADDR,0,ref)
         else
           begin
              reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
              paramanager.allocparaloc(list,cgpara2);
              a_paramaddr_ref(list,href,cgpara2);
              paramanager.allocparaloc(list,cgpara1);
              a_paramaddr_ref(list,ref,cgpara1);
              paramanager.freeparaloc(list,cgpara1);
              paramanager.freeparaloc(list,cgpara2);
              alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
              alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
              a_call_name(list,'FPC_INITIALIZE');
              dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
              dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           end;
        cgpara1.done;
        cgpara2.done;
      end;


    procedure tcg.g_finalize(list : taasmoutput;t : tdef;const ref : treference);
      var
         href : treference;
         cgpara1,cgpara2 : TCGPara;
      begin
        cgpara1.init;
        cgpara2.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.getintparaloc(pocall_default,2,cgpara2);
         if is_ansistring(t) or
            is_widestring(t) or
            is_interfacecom(t) then
            begin
              g_decrrefcount(list,t,ref);
              a_load_const_ref(list,OS_ADDR,0,ref);
            end
         else
           begin
              reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
              paramanager.allocparaloc(list,cgpara2);
              a_paramaddr_ref(list,href,cgpara2);
              paramanager.allocparaloc(list,cgpara1);
              a_paramaddr_ref(list,ref,cgpara1);
              paramanager.freeparaloc(list,cgpara1);
              paramanager.freeparaloc(list,cgpara2);
              alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
              alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
              a_call_name(list,'FPC_FINALIZE');
              dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
              dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           end;
        cgpara1.done;
        cgpara2.done;
      end;


    procedure tcg.g_rangecheck(list: taasmoutput; const l:tlocation;fromdef,todef: tdef);
    { generate range checking code for the value at location p. The type     }
    { type used is checked against todefs ranges. fromdef (p.resulttype.def) }
    { is the original type used at that location. When both defs are equal   }
    { the check is also insert (needed for succ,pref,inc,dec)                }
      const
        aintmax=high(aint);
      var
        neglabel : tasmlabel;
        hreg : tregister;
        lto,hto,
        lfrom,hfrom : TConstExprInt;
        fromsize, tosize: cardinal;
        from_signed, to_signed: boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(fromdef.deftype in [orddef,enumdef,arraydef]) then
          exit;
{$ifndef cpu64bit}
        { handle 64bit rangechecks separate for 32bit processors }
        if is_64bit(fromdef) or is_64bit(todef) then
          begin
             cg64.g_rangecheck64(list,l,fromdef,todef);
             exit;
          end;
{$endif cpu64bit}
        { only check when assigning to scalar, subranges are different, }
        { when todef=fromdef then the check is always generated         }
        getrange(fromdef,lfrom,hfrom);
        getrange(todef,lto,hto);
        from_signed := is_signed(fromdef);
        to_signed := is_signed(todef);
        { no range check if from and to are equal and are both longint/dword }
        { (if we have a 32bit processor) or int64/qword, since such          }
        { operations can at most cause overflows (JM)                        }
        { Note that these checks are mostly processor independent, they only }
        { have to be changed once we introduce 64bit subrange types          }
{$ifdef cpu64bit}
        if (fromdef = todef) and
           (fromdef.deftype=orddef) and
           (((((torddef(fromdef).typ = s64bit) and
               (lfrom = low(int64)) and
               (hfrom = high(int64))) or
              ((torddef(fromdef).typ = u64bit) and
               (lfrom = low(qword)) and
               (hfrom = high(qword)))))) then
          exit;
{$else cpu64bit}
        if (fromdef = todef) and
           (fromdef.deftype=orddef) and
           (((((torddef(fromdef).typ = s32bit) and
               (lfrom = low(longint)) and
               (hfrom = high(longint))) or
              ((torddef(fromdef).typ = u32bit) and
               (lfrom = low(cardinal)) and
               (hfrom = high(cardinal)))))) then
          exit;
{$endif cpu64bit}

        { optimize some range checks away in safe cases }
        fromsize := fromdef.size;
        tosize := todef.size;
        if ((from_signed = to_signed) or
            (not from_signed)) and
           (lto<=lfrom) and (hto>=hfrom) and
           (fromsize <= tosize) then
          begin
            { if fromsize < tosize, and both have the same signed-ness or }
            { fromdef is unsigned, then all bit patterns from fromdef are }
            { valid for todef as well                                     }
            if (fromsize < tosize) then
              exit;
            if (fromsize = tosize) and
               (from_signed = to_signed) then
              { only optimize away if all bit patterns which fit in fromsize }
              { are valid for the todef                                      }
              begin
{$ifopt Q+}
{$defined overflowon}
{$Q-}
{$endif}
                if to_signed then
                  begin
                    if (lto = (-(int64(1) << (tosize * 4)))) and
                       (hto = (int64(1) << (tosize * 4) - 1)) then
                      exit
                  end
                else
                  begin
                    if (lto = 0) and
                       (qword(hto) = qword((int64(1) << (tosize * 8)) - 1)) then
                      exit
                  end;
{$ifdef overflowon}
{$Q+}
{$undef overflowon}
{$endif}
              end
          end;

        { generate the rangecheck code for the def where we are going to }
        { store the result                                               }

        { use the trick that                                                 }
        { a <= x <= b <=> 0 <= x-a <= b-a <=> unsigned(x-a) <= unsigned(b-a) }

        { To be able to do that, we have to make sure however that either    }
        { fromdef and todef are both signed or unsigned, or that we leave    }
        { the parts < 0 and > maxlongint out                                 }

        { is_signed now also works for arrays (it checks the rangetype) (JM) }
        if from_signed xor to_signed then
          begin
             if from_signed then
               { from is signed, to is unsigned }
               begin
                 { if high(from) < 0 -> always range error }
                 if (hfrom < 0) or
                    { if low(to) > maxlongint also range error }
                    (lto > aintmax) then
                   begin
                     a_call_name(list,'FPC_RANGEERROR');
                     exit
                   end;
                 { from is signed and to is unsigned -> when looking at to }
                 { as an signed value, it must be < maxaint (otherwise     }
                 { it will become negative, which is invalid since "to" is unsigned) }
                 if hto > aintmax then
                   hto := aintmax;
               end
             else
               { from is unsigned, to is signed }
               begin
                 if (lfrom > aintmax) or
                    (hto < 0) then
                   begin
                     a_call_name(list,'FPC_RANGEERROR');
                     exit
                   end;
                 { from is unsigned and to is signed -> when looking at to }
                 { as an unsigned value, it must be >= 0 (since negative   }
                 { values are the same as values > maxlongint)             }
                 if lto < 0 then
                   lto := 0;
               end;
          end;
        hreg:=getintregister(list,OS_INT);
        a_load_loc_reg(list,OS_INT,l,hreg);
        a_op_const_reg(list,OP_SUB,OS_INT,aint(lto),hreg);
        objectlibrary.getlabel(neglabel);
        {
        if from_signed then
          a_cmp_const_reg_label(list,OS_INT,OC_GTE,aint(hto-lto),hreg,neglabel)
        else
        }
{$ifdef cpu64bit}
        if qword(hto-lto)>qword(aintmax) then
          a_cmp_const_reg_label(list,OS_INT,OC_BE,aintmax,hreg,neglabel)
        else
{$endif cpu64bit}
          a_cmp_const_reg_label(list,OS_INT,OC_BE,aint(hto-lto),hreg,neglabel);
        a_call_name(list,'FPC_RANGEERROR');
        a_label(list,neglabel);
      end;


    procedure tcg.g_overflowCheck_loc(List:TAasmOutput;const Loc:TLocation;def:TDef;ovloc : tlocation);
      begin
        g_overflowCheck(list,loc,def);
      end;


    procedure tcg.g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref:TReference);

      var
        tmpreg : tregister;
      begin
        tmpreg:=getintregister(list,size);
        g_flags2reg(list,size,f,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
      end;


    procedure tcg.g_maybe_testself(list : taasmoutput;reg:tregister);
      var
        OKLabel : tasmlabel;
        cgpara1 : TCGPara;
      begin
        if (cs_check_object in aktlocalswitches) or
           (cs_check_range in aktlocalswitches) then
         begin
           objectlibrary.getlabel(oklabel);
           a_cmp_const_reg_label(list,OS_ADDR,OC_NE,0,reg,oklabel);
           cgpara1.init;
           paramanager.getintparaloc(pocall_default,1,cgpara1);
           paramanager.allocparaloc(list,cgpara1);
           a_param_const(list,OS_INT,210,cgpara1);
           paramanager.freeparaloc(list,cgpara1);
           a_call_name(list,'FPC_HANDLEERROR');
           a_label(list,oklabel);
           cgpara1.done;
         end;
      end;


    procedure tcg.g_maybe_testvmt(list : taasmoutput;reg:tregister;objdef:tobjectdef);
      var
        hrefvmt : treference;
        cgpara1,cgpara2 : TCGPara;
      begin
        cgpara1.init;
        cgpara2.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.getintparaloc(pocall_default,2,cgpara2);
        if (cs_check_object in aktlocalswitches) then
         begin
           reference_reset_symbol(hrefvmt,objectlibrary.newasmsymbol(objdef.vmt_mangledname,AB_EXTERNAL,AT_DATA),0);
           paramanager.allocparaloc(list,cgpara2);
           a_paramaddr_ref(list,hrefvmt,cgpara2);
           paramanager.allocparaloc(list,cgpara1);
           a_param_reg(list,OS_ADDR,reg,cgpara1);
           paramanager.freeparaloc(list,cgpara1);
           paramanager.freeparaloc(list,cgpara2);
           alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           a_call_name(list,'FPC_CHECK_OBJECT_EXT');
           dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
         end
        else
         if (cs_check_range in aktlocalswitches) then
          begin
            paramanager.allocparaloc(list,cgpara1);
            a_param_reg(list,OS_ADDR,reg,cgpara1);
            paramanager.freeparaloc(list,cgpara1);
            alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,'FPC_CHECK_OBJECT');
            dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
          end;
        cgpara1.done;
        cgpara2.done;
      end;


{*****************************************************************************
                            Entry/Exit Code Functions
*****************************************************************************}

    procedure tcg.g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);
      var
        sizereg,sourcereg,lenreg : tregister;
        cgpara1,cgpara2,cgpara3 : TCGPara;
      begin
        { because some abis don't support dynamic stack allocation properly
          open array value parameters are copied onto the heap
        }

        { calculate necessary memory }

        { read/write operations on one register make the life of the register allocator hard }
        if not(lenloc.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          begin
            lenreg:=getintregister(list,OS_INT);
            a_load_loc_reg(list,OS_INT,lenloc,lenreg);
          end
        else
          lenreg:=lenloc.register;

        sizereg:=getintregister(list,OS_INT);
        a_op_const_reg_reg(list,OP_ADD,OS_INT,1,lenreg,sizereg);
        a_op_const_reg(list,OP_IMUL,OS_INT,elesize,sizereg);
        { load source }
        sourcereg:=getaddressregister(list);
        a_loadaddr_ref_reg(list,ref,sourcereg);

        { do getmem call }
        cgpara1.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.allocparaloc(list,cgpara1);
        a_param_reg(list,OS_INT,sizereg,cgpara1);
        paramanager.freeparaloc(list,cgpara1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        a_call_name(list,'FPC_GETMEM');
        dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cgpara1.done;
        { return the new address }
        a_load_reg_reg(list,OS_ADDR,OS_ADDR,NR_FUNCTION_RESULT_REG,destreg);

        { do move call }
        cgpara1.init;
        cgpara2.init;
        cgpara3.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        paramanager.getintparaloc(pocall_default,2,cgpara2);
        paramanager.getintparaloc(pocall_default,3,cgpara3);
        { load size }
        paramanager.allocparaloc(list,cgpara3);
        a_param_reg(list,OS_INT,sizereg,cgpara3);
        { load destination }
        paramanager.allocparaloc(list,cgpara2);
        a_param_reg(list,OS_ADDR,destreg,cgpara2);
        { load source }
        paramanager.allocparaloc(list,cgpara1);
        a_param_reg(list,OS_ADDR,sourcereg,cgpara1);
        paramanager.freeparaloc(list,cgpara3);
        paramanager.freeparaloc(list,cgpara2);
        paramanager.freeparaloc(list,cgpara1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        a_call_name(list,'FPC_MOVE');
        dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cgpara3.done;
        cgpara2.done;
        cgpara1.done;
      end;


    procedure tcg.g_releasevaluepara_openarray(list : taasmoutput;const l:tlocation);
      var
        cgpara1 : TCGPara;
      begin
        { do move call }
        cgpara1.init;
        paramanager.getintparaloc(pocall_default,1,cgpara1);
        { load source }
        paramanager.allocparaloc(list,cgpara1);
        a_param_loc(list,l,cgpara1);
        paramanager.freeparaloc(list,cgpara1);
        alloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        alloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        a_call_name(list,'FPC_FREEMEM');
        dealloccpuregisters(list,R_FPUREGISTER,paramanager.get_volatile_registers_fpu(pocall_default));
        dealloccpuregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cgpara1.done;
      end;


    procedure tcg.g_save_standard_registers(list:Taasmoutput);
      var
        href : treference;
        size : longint;
        r : integer;
      begin
        { Get temp }
        size:=0;
        for r:=low(saved_standard_registers) to high(saved_standard_registers) do
          if saved_standard_registers[r] in rg[R_INTREGISTER].used_in_proc then
            inc(size,sizeof(aint));
        if size>0 then
          begin
            tg.GetTemp(list,size,tt_noreuse,current_procinfo.save_regs_ref);
            { Copy registers to temp }
            href:=current_procinfo.save_regs_ref;

            for r:=low(saved_standard_registers) to high(saved_standard_registers) do
              begin
                if saved_standard_registers[r] in rg[R_INTREGISTER].used_in_proc then
                  begin
                    a_load_reg_ref(list,OS_ADDR,OS_ADDR,newreg(R_INTREGISTER,saved_standard_registers[r],R_SUBWHOLE),href);
                    inc(href.offset,sizeof(aint));
                  end;
                include(rg[R_INTREGISTER].preserved_by_proc,saved_standard_registers[r]);
              end;
          end;
      end;


    procedure tcg.g_restore_standard_registers(list:Taasmoutput);
      var
        href : treference;
        r : integer;
        hreg : tregister;
      begin
        { Copy registers from temp }
        href:=current_procinfo.save_regs_ref;
        for r:=low(saved_standard_registers) to high(saved_standard_registers) do
          if saved_standard_registers[r] in rg[R_INTREGISTER].used_in_proc then
            begin
              hreg:=newreg(R_INTREGISTER,saved_standard_registers[r],R_SUBWHOLE);
              { Allocate register so the optimizer does remove the load }
              a_reg_alloc(list,hreg);
              a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,hreg);
              inc(href.offset,sizeof(aint));
            end;
        tg.UnGetTemp(list,current_procinfo.save_regs_ref);
      end;


    procedure tcg.g_profilecode(list : taasmoutput);
      begin
      end;


    procedure tcg.g_exception_reason_save(list : taasmoutput; const href : treference);
      begin
        a_load_reg_ref(list, OS_INT, OS_INT, NR_FUNCTION_RESULT_REG, href);
      end;


    procedure tcg.g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aint);
      begin
        a_load_const_ref(list, OS_INT, a, href);
      end;


    procedure tcg.g_exception_reason_load(list : taasmoutput; const href : treference);
      begin
        a_load_ref_reg(list, OS_INT, OS_INT, href, NR_FUNCTION_RESULT_REG);
      end;


    procedure tcg.g_adjust_self_value(list:taasmoutput;procdef: tprocdef;ioffset: aint);
      var
        hsym : tsym;
        href : treference;
        paraloc : tcgparalocation;
      begin
        { calculate the parameter info for the procdef }
        if not procdef.has_paraloc_info then
          begin
            procdef.requiredargarea:=paramanager.create_paraloc_info(procdef,callerside);
            procdef.has_paraloc_info:=true;
          end;
        hsym:=tsym(procdef.parast.search('self'));
        if not(assigned(hsym) and
               (hsym.typ=paravarsym)) then
          internalerror(200305251);
        paraloc:=tparavarsym(hsym).paraloc[callerside].location^;
        case paraloc.loc of
          LOC_REGISTER:
            cg.a_op_const_reg(list,OP_SUB,paraloc.size,ioffset,paraloc.register);
          LOC_REFERENCE:
            begin
               { offset in the wrapper needs to be adjusted for the stored
                 return address }
               reference_reset_base(href,paraloc.reference.index,paraloc.reference.offset+sizeof(aint));
               cg.a_op_const_ref(list,OP_SUB,paraloc.size,ioffset,href);
            end
          else
            internalerror(200309189);
        end;
      end;

{*****************************************************************************
                                    TCG64
*****************************************************************************}

{$ifndef cpu64bit}
    procedure tcg64.a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;size : tcgsize;value : int64; regsrc,regdst : tregister64);
      begin
        a_load64_reg_reg(list,regsrc,regdst);
        a_op64_const_reg(list,op,size,value,regdst);
      end;


    procedure tcg64.a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64);
      var
        tmpreg64 : tregister64;
      begin
        { when src1=dst then we need to first create a temp to prevent
          overwriting src1 with src2 }
        if (regsrc1.reghi=regdst.reghi) or
           (regsrc1.reglo=regdst.reghi) or
           (regsrc1.reghi=regdst.reglo) or
           (regsrc1.reglo=regdst.reglo) then
          begin
            tmpreg64.reglo:=cg.getintregister(list,OS_32);
            tmpreg64.reghi:=cg.getintregister(list,OS_32);
            a_load64_reg_reg(list,regsrc2,tmpreg64);
            a_op64_reg_reg(list,op,size,regsrc1,tmpreg64);
            a_load64_reg_reg(list,tmpreg64,regdst);
          end
        else
          begin
            a_load64_reg_reg(list,regsrc2,regdst);
            a_op64_reg_reg(list,op,size,regsrc1,regdst);
          end;
      end;


    procedure tcg64.a_op64_const_reg_reg_checkoverflow(list: taasmoutput;op:TOpCG;size : tcgsize;value : int64;regsrc,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      begin
        a_op64_const_reg_reg(list,op,size,value,regsrc,regdst);
        ovloc.loc:=LOC_VOID;
      end;


    procedure tcg64.a_op64_reg_reg_reg_checkoverflow(list: taasmoutput;op:TOpCG;size : tcgsize;regsrc1,regsrc2,regdst : tregister64;setflags : boolean;var ovloc : tlocation);
      begin
        a_op64_reg_reg_reg(list,op,size,regsrc1,regsrc2,regdst);
        ovloc.loc:=LOC_VOID;
      end;


{$endif cpu64bit}



initialization
    ;
finalization
  cg.free;
{$ifndef cpu64bit}
  cg64.free;
{$endif cpu64bit}
end.
