{
    $Id$

    Copyright (c) 1998-2002 by Florian Klaempfl
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
       cclasses,aasmbase,aasmtai,aasmcpu,symtable,
       cpubase,cpuinfo,
       cginfo,
       symconst,symbase,symtype,symdef
{$ifdef delphi}
       ,dmisc
{$endif}
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
          alignment : talignment;
          {************************************************}
          {                 basic routines                 }
          constructor create;

          { returns the tcgsize corresponding with the size of reg }
          class function reg_cgsize(const reg: tregister) : tcgsize; virtual;

          {# Emit a label to the instruction stream. }
          procedure a_label(list : taasmoutput;l : tasmlabel);virtual;

          {# Allocates register r by inserting a pai_realloc record }
          procedure a_reg_alloc(list : taasmoutput;r : tregister);
          {# Deallocates register r by inserting a pa_regdealloc record}
          procedure a_reg_dealloc(list : taasmoutput;r : tregister);

          {# Pass a parameter, which is located in a register, to a routine.

             This routine should push/send the parameter to the routine, as
             required by the specific processor ABI and routine modifiers.
             This must be overriden for each CPU target.

             @param(size size of the operand in the register)
             @param(r register source of the operand)
             @param(locpara where the parameter will be stored)
          }
          procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);virtual;
          {# Pass a parameter, which is a constant, to a routine.

             A generic version is provided. This routine should
             be overriden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(size size of the operand in constant)
             @param(a value of constant to send)
             @param(locpara where the parameter will be stored)
          }
          procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);virtual;
          {# Pass the value of a parameter, which is located in memory, to a routine.

             A generic version is provided. This routine should
             be overriden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(size size of the operand in constant)
             @param(r Memory reference of value to send)
             @param(locpara where the parameter will be stored)
          }
          procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);virtual;
          {# Pass the value of a parameter, which can be located either in a register or memory location,
             to a routine.

             A generic version is provided.

             @param(l location of the operand to send)
             @param(nr parameter number (starting from one) of routine (from left to right))
             @param(locpara where the parameter will be stored)
          }
          procedure a_param_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);
          {# Pass the address of a reference to a routine. This routine
             will calculate the address of the reference, and pass this
             calculated address as a parameter.

             A generic version is provided. This routine should
             be overriden for optimization purposes if the cpu
             permits directly sending this type of parameter.

             @param(r reference to get address from)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);virtual;

          { Copies a whole memory block to the stack, the locpara must be a memory location }
          procedure a_param_copy_ref(list : taasmoutput;size : qword;const r : treference;const locpara : tparalocation);
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

          { Copy a parameter to a (temporary) reference }
          procedure a_load_param_ref(list : taasmoutput;const locpara : tparalocation;const ref:treference);virtual;
          { Copy a parameter to a register }
          procedure a_load_param_reg(list : taasmoutput;const locpara : tparalocation;const reg:tregister);virtual;

          {# Emits instruction to call the method specified by symbol name.
             This routine must be overriden for each new target cpu.
          }
          procedure a_call_name(list : taasmoutput;const s : string);virtual; abstract;
          procedure a_call_ref(list : taasmoutput;const ref : treference);virtual;
          procedure a_call_reg(list : taasmoutput;reg : tregister);virtual;abstract;
          procedure a_call_loc(list : taasmoutput;const loc:tlocation);


          { move instructions }
          procedure a_load_const_reg(list : taasmoutput;size : tcgsize;a : aword;register : tregister);virtual; abstract;
          procedure a_load_const_ref(list : taasmoutput;size : tcgsize;a : aword;const ref : treference);virtual;
          procedure a_load_const_loc(list : taasmoutput;a : aword;const loc : tlocation);
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
          procedure a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const locpara : tparalocation);virtual;
          procedure a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const locpara : tparalocation);virtual;

          { vector register move instructions }
          procedure a_loadmm_reg_reg(list: taasmoutput; reg1, reg2: tregister); virtual; abstract;
          procedure a_loadmm_ref_reg(list: taasmoutput; const ref: treference; reg: tregister); virtual; abstract;
          procedure a_loadmm_reg_ref(list: taasmoutput; reg: tregister; const ref: treference); virtual; abstract;
          procedure a_parammm_reg(list: taasmoutput; reg: tregister); virtual; abstract;

          { basic arithmetic operations }
          { note: for operators which require only one argument (not, neg), use }
          { the op_reg_reg, op_reg_ref or op_reg_loc methods and keep in mind   }
          { that in this case the *second* operand is used as both source and   }
          { destination (JM)                                                    }
          procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; reg: TRegister); virtual; abstract;
          procedure a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference); virtual;
          procedure a_op_const_loc(list : taasmoutput; Op: TOpCG; a: AWord; const loc: tlocation);
          procedure a_op_reg_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister); virtual; abstract;
          procedure a_op_reg_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; reg: TRegister; const ref: TReference); virtual;
          procedure a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister); virtual;
          procedure a_op_reg_loc(list : taasmoutput; Op: TOpCG; reg: tregister; const loc: tlocation);
          procedure a_op_ref_loc(list : taasmoutput; Op: TOpCG; const ref: TReference; const loc: tlocation);

          { trinary operations for processors that support them, 'emulated' }
          { on others. None with "ref" arguments since I don't think there  }
          { are any processors that support it (JM)                         }
          procedure a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
            size: tcgsize; a: aword; src, dst: tregister); virtual;
          procedure a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
            size: tcgsize; src1, src2, dst: tregister); virtual;

          {  comparison operations }
          procedure a_cmp_const_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
            l : tasmlabel);virtual; abstract;
          procedure a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
            l : tasmlabel); virtual;
          procedure a_cmp_const_loc_label(list: taasmoutput; size: tcgsize;cmp_op: topcmp; a: aword; const loc: tlocation;
            l : tasmlabel);
          procedure a_cmp_reg_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); virtual; abstract;
          procedure a_cmp_ref_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; const ref: treference; reg : tregister; l : tasmlabel); virtual;
          procedure a_cmp_loc_reg_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp; const loc: tlocation; reg : tregister; l : tasmlabel);
          procedure a_cmp_ref_loc_label(list: taasmoutput; size: tcgsize;cmp_op: topcmp; const ref: treference; const loc: tlocation;
            l : tasmlabel);

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
          function optimize_op_const_reg(list: taasmoutput; var op: topcg; var a : aword; var reg: tregister): boolean;virtual;

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
         procedure g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aword);virtual;
         {#
             This routine is used in exception management nodes. It should
             load the exception reason to the FUNCTION_RETURN_REG. The saved value
             should either be in the temp. area (pointed to by href , href should
             *NOT* be freed) or on the stack (the value should be popped).

             The size of the value to save is OS_S32. The default version
             saves the exception reason to a temp. memory area.
          }
         procedure g_exception_reason_load(list : taasmoutput; const href : treference);virtual;

          procedure g_load_parent_framepointer(list:taasmoutput;parentsymtable:tsymtable;reg:tregister);
          procedure g_save_parent_framepointer_param(list:taasmoutput);virtual;

          procedure g_maybe_testself(list : taasmoutput;reg:tregister);
          procedure g_maybe_testvmt(list : taasmoutput;reg:tregister;objdef:tobjectdef);
          {# This should emit the opcode to copy len bytes from the source
             to destination, if loadref is true, it assumes that it first must load
             the source address from the memory location where
             source points to.

             It must be overriden for each new target processor.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)
             @param(delsource Indicates if the source reference's resources should be freed)
             @param(loadref Is the source reference a pointer to the actual source (TRUE), is it the actual source address (FALSE))

          }
          procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword;delsource,loadref : boolean);virtual; abstract;
          {# This should emit the opcode to a shortrstring from the source
             to destination, if loadref is true, it assumes that it first must load
             the source address from the memory location where
             source points to.

             @param(source Source reference of copy)
             @param(dest Destination reference of copy)
             @param(delsource Indicates if the source reference's resources should be freed)
             @param(loadref Is the source reference a pointer to the actual source (TRUE), is it the actual source address (FALSE))

          }
          procedure g_copyshortstring(list : taasmoutput;const source,dest : treference;len:byte;delsource,loadref : boolean);

          procedure g_incrrefcount(list : taasmoutput;t: tdef; const ref: treference;loadref : boolean);
          procedure g_decrrefcount(list : taasmoutput;t: tdef; const ref: treference;loadref : boolean);
          procedure g_initialize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);
          procedure g_finalize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);

          {# Emits the call to the stack checking routine of
             the runtime library. The default behavior
             does not need to be modified, as it is generic
             for all platforms.

             @param(stackframesize Number of bytes which will be allocated on the stack)
          }
          procedure g_stackcheck(list : taasmoutput;stackframesize : longint);virtual;

          {# Generates range checking code. It is to note
             that this routine does not need to be overriden,
             as it takes care of everything.

             @param(p Node which contains the value to check)
             @param(todef Type definition of node to range check)
          }
          procedure g_rangecheck(list: taasmoutput; const l:tlocation; fromdef,todef: tdef); virtual;

          {# Generates overflow checking code for a node }
          procedure g_overflowcheck(list: taasmoutput; const l:tlocation; def:tdef); virtual; abstract;

          procedure g_copyvaluepara_openarray(list : taasmoutput;const ref, lenref:treference;elesize:integer);virtual;abstract;
          {# Emits instructions which should be emitted when entering
             a routine declared as @var(interrupt). The default
             behavior does nothing, should be overriden as required.
          }
          procedure g_interrupt_stackframe_entry(list : taasmoutput);virtual;

          {# Emits instructions which should be emitted when exiting
             a routine declared as @var(interrupt). The default
             behavior does nothing, should be overriden as required.
          }
          procedure g_interrupt_stackframe_exit(list : taasmoutput;accused,acchiused:boolean);virtual;

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
          procedure g_stackframe_entry(list : taasmoutput;localsize : longint);virtual; abstract;
          {# Emits instructiona for restoring the frame pointer
             at routine exit. For some processors, this routine
             may do nothing at all.
          }
          procedure g_restore_frame_pointer(list : taasmoutput);virtual; abstract;
          {# Emits instructions for returning from a subroutine.
             Should also restore the stack.

             @param(parasize  Number of bytes of parameters to deallocate from stack)
          }
          procedure g_return_from_proc(list : taasmoutput;parasize : aword);virtual; abstract;
          {# This routine is called when generating the code for the entry point
             of a routine. It should save all registers which are not used in this
             routine, and which should be declared as saved in the std_saved_registers
             set.

             This routine is mainly used when linking to code which is generated
             by ABI-compliant compilers (like GCC), to make sure that the reserved
             registers of that ABI are not clobbered.

             @param(usedinproc Registers which are used in the code of this routine)
          }
          procedure g_save_standard_registers(list:Taasmoutput;usedinproc:Tsuperregisterset);virtual;abstract;
          {# This routine is called when generating the code for the exit point
             of a routine. It should restore all registers which were previously
             saved in @var(g_save_standard_registers).

             @param(usedinproc Registers which are used in the code of this routine)
          }
          procedure g_restore_standard_registers(list:Taasmoutput;usedinproc:Tsuperregisterset);virtual;abstract;
          procedure g_save_all_registers(list : taasmoutput);virtual;abstract;
          procedure g_restore_all_registers(list : taasmoutput;accused,acchiused:boolean);virtual;abstract;
       end;

    {# @abstract(Abstract code generator for 64 Bit operations)
       This class implements an abstract code generator class
       for 64 Bit operations.
    }
    tcg64 = class
        { Allocates 64 Bit register r by inserting a pai_realloc record }
        procedure a_reg_alloc(list : taasmoutput;r : tregister64);virtual;abstract;
        { Deallocates 64 Bit register r by inserting a pa_regdealloc record}
        procedure a_reg_dealloc(list : taasmoutput;r : tregister64);virtual;abstract;
        procedure a_load64_const_ref(list : taasmoutput;value : qword;const ref : treference);virtual;abstract;
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);virtual;abstract;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64;delete : boolean);virtual;abstract;
        procedure a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64;delete : boolean);virtual;abstract;
        procedure a_load64_const_reg(list : taasmoutput;value : qword;reg : tregister64);virtual;abstract;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64;delete : boolean);virtual;abstract;
        procedure a_load64_loc_ref(list : taasmoutput;const l : tlocation;const ref : treference);virtual;abstract;
        procedure a_load64_const_loc(list : taasmoutput;value : qword;const l : tlocation);virtual;abstract;
        procedure a_load64_reg_loc(list : taasmoutput;reg : tregister64;const l : tlocation);virtual;abstract;

        procedure a_load64high_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);virtual;abstract;
        procedure a_load64low_reg_ref(list : taasmoutput;reg : tregister;const ref : treference);virtual;abstract;
        procedure a_load64high_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);virtual;abstract;
        procedure a_load64low_ref_reg(list : taasmoutput;const ref : treference;reg : tregister);virtual;abstract;
        procedure a_load64high_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);virtual;abstract;
        procedure a_load64low_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister);virtual;abstract;

        procedure a_op64_ref_reg(list : taasmoutput;op:TOpCG;const ref : treference;reg : tregister64);virtual;abstract;
        procedure a_op64_reg_reg(list : taasmoutput;op:TOpCG;regsrc,regdst : tregister64);virtual;abstract;
        procedure a_op64_reg_ref(list : taasmoutput;op:TOpCG;regsrc : tregister64;const ref : treference);virtual;abstract;
        procedure a_op64_const_reg(list : taasmoutput;op:TOpCG;value : qword;regdst : tregister64);virtual;abstract;
        procedure a_op64_const_ref(list : taasmoutput;op:TOpCG;value : qword;const ref : treference);virtual;abstract;
        procedure a_op64_const_loc(list : taasmoutput;op:TOpCG;value : qword;const l: tlocation);virtual;abstract;
        procedure a_op64_reg_loc(list : taasmoutput;op:TOpCG;reg : tregister64;const l : tlocation);virtual;abstract;
        procedure a_op64_loc_reg(list : taasmoutput;op:TOpCG;const l : tlocation;reg64 : tregister64);virtual;abstract;
        procedure a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword;regsrc,regdst : tregister64);virtual;
        procedure a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);virtual;

        procedure a_param64_reg(list : taasmoutput;reg64 : tregister64;const loc : tparalocation);virtual;abstract;
        procedure a_param64_const(list : taasmoutput;value : qword;const loc : tparalocation);virtual;abstract;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const loc : tparalocation);virtual;abstract;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;const loc : tparalocation);virtual;abstract;

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
        function optimize64_op_const_reg(list: taasmoutput; var op: topcg; var a : qword; var reg: tregister64): boolean;virtual;abstract;


        { override to catch 64bit rangechecks }
        procedure g_rangecheck64(list: taasmoutput; const l:tlocation; fromdef,todef: tdef);virtual;abstract;
    end;

    var
       {# Main code generator class }
       cg : tcg;
       {# Code generator class for all operations working with 64-Bit operands }
       cg64 : tcg64;

  implementation

    uses
       globals,globtype,options,systems,cgbase,
       verbose,defutil,paramgr,symsym,
       rgobj,cutils;

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

    procedure tcg.a_reg_alloc(list : taasmoutput;r : tregister);

      begin
         list.concat(tai_regalloc.alloc(r));
      end;

    procedure tcg.a_reg_dealloc(list : taasmoutput;r : tregister);

      begin
         list.concat(tai_regalloc.dealloc(r));
      end;

    procedure tcg.a_label(list : taasmoutput;l : tasmlabel);

      begin
         list.concat(tai_label.create(l));
      end;


{*****************************************************************************
          for better code generation these methods should be overridden
******************************************************************************}

    procedure tcg.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);
      var
         ref : treference;
      begin
         case locpara.loc of
            LOC_REGISTER,LOC_CREGISTER:
              a_load_reg_reg(list,size,locpara.size,r,locpara.register);
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 if locpara.sp_fixup<>0 then
                   a_op_const_reg(list,OP_ADD,OS_ADDR,locpara.sp_fixup,NR_STACK_POINTER_REG);
                 reference_reset(ref);
                 ref.base:=locpara.reference.index;
                 ref.offset:=locpara.reference.offset;
                 a_load_reg_ref(list,size,size,r,ref);
              end
            else
              internalerror(2002071004);
         end;
      end;

    procedure tcg.a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);

      var
         hr : tregister;

      begin
         hr:=rg.getregisterint(list,size);
         a_load_const_reg(list,size,a,hr);
         a_param_reg(list,size,hr,locpara);
         rg.ungetregisterint(list,hr);
      end;

    procedure tcg.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);
      var
         hr : tregister;
      begin
         hr:=rg.getregisterint(list,size);
         a_load_ref_reg(list,size,size,r,hr);
         a_param_reg(list,size,hr,locpara);
         rg.ungetregisterint(list,hr);
      end;


    procedure tcg.a_param_loc(list : taasmoutput;const l:tlocation;const locpara : tparalocation);
      begin
        case l.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            a_param_reg(list,l.size,l.register,locpara);
          LOC_CONSTANT :
            a_param_const(list,l.size,l.value,locpara);
          LOC_CREFERENCE,
          LOC_REFERENCE :
            a_param_ref(list,l.size,l.reference,locpara);
        else
          internalerror(2002032211);
        end;
      end;


    procedure tcg.a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);
      var
         hr : tregister;
      begin
         hr:=rg.getaddressregister(list);
         a_loadaddr_ref_reg(list,r,hr);
         a_param_reg(list,OS_ADDR,hr,locpara);
         rg.ungetregisterint(list,hr);
      end;


    procedure tcg.a_param_copy_ref(list : taasmoutput;size : qword;const r : treference;const locpara : tparalocation);
      var
        ref : treference;
      begin
         if not(locpara.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           internalerror(2003010901);
         if locpara.sp_fixup<>0 then
           cg.g_stackpointer_alloc(list,locpara.sp_fixup);
         reference_reset_base(ref,locpara.reference.index,locpara.reference.offset);
         cg.g_concatcopy(list,r,ref,size,false,false);
      end;


    procedure tcg.a_load_param_ref(list : taasmoutput;const locpara : tparalocation;const ref:treference);
      begin
        case locpara.loc of
          LOC_CREGISTER,
          LOC_REGISTER:
            begin
              if (locpara.size in [OS_S64,OS_64]) then
                begin
{$ifdef cpu64bit}
                  rg.ungetregisterint(list,locpara.register64);
{$else cpu64bit}
                  rg.getexplicitregisterint(list,locpara.registerlow);
                  rg.getexplicitregisterint(list,locpara.registerhigh);
                  rg.ungetregisterint(list,locpara.registerlow);
                  rg.ungetregisterint(list,locpara.registerhigh);
{$endif cpu64bit}
                  cg64.a_load64_reg_ref(list,locpara.register64,ref)
                end
              else
                begin
                  rg.getexplicitregisterint(list,locpara.register);
                  rg.ungetregisterint(list,locpara.register);
                  cg.a_load_reg_ref(list,locpara.size,locpara.size,locpara.register,ref);
                end;
            end;
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER:
            cg.a_loadfpu_reg_ref(list,locpara.size,locpara.register,ref);
          else
            internalerror(2002081302);
        end;
      end;


    procedure tcg.a_load_param_reg(list : taasmoutput;const locpara : tparalocation;const reg:tregister);
      var
        href : treference;
      begin
        case locpara.loc of
          LOC_CREGISTER,
          LOC_REGISTER:
            begin
              if not(locpara.size in [OS_S64,OS_64]) then
                begin
                  rg.getexplicitregisterint(list,locpara.register);
                  rg.ungetregisterint(list,locpara.register);
                  rg.getexplicitregisterint(list,reg);
                  cg.a_load_reg_reg(list,locpara.size,locpara.size,locpara.register,reg)
                end
              else
                internalerror(2003053011);
            end;
          LOC_CFPUREGISTER,
          LOC_FPUREGISTER:
            cg.a_loadfpu_reg_reg(list,locpara.size,locpara.register,reg);
          LOC_REFERENCE,
          LOC_CREFERENCE:
            begin
              reference_reset_base(href,locpara.reference.index,locpara.reference.offset);
              rg.getexplicitregisterint(list,reg);
              cg.a_load_ref_reg(list,locpara.size,locpara.size,href,reg);
            end;
          else
            internalerror(2003053010);
        end
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
        tmpreg:=rg.getregisterint(list,tosize);
        a_load_ref_reg(list,fromsize,tosize,sref,tmpreg);
        a_load_reg_ref(list,tosize,tosize,tmpreg,dref);
        rg.ungetregisterint(list,tmpreg);
      end;


    procedure tcg.a_load_const_ref(list : taasmoutput;size : tcgsize;a : aword;const ref : treference);
      var
        tmpreg: tregister;
      begin
        tmpreg:=rg.getregisterint(list,size);
        a_load_const_reg(list,size,a,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
        rg.ungetregisterint(list,tmpreg);
      end;


    procedure tcg.a_load_const_loc(list : taasmoutput;a : aword;const loc: tlocation);
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


    procedure tcg.a_call_ref(list : taasmoutput;const ref:treference);
      var
        tmpreg: tregister;
      begin
        tmpreg:=rg.getaddressregister(list);
        a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,tmpreg);
        a_call_reg(list,tmpreg);
        rg.ungetaddressregister(list,tmpreg);
      end;


    procedure tcg.a_call_loc(list : taasmoutput;const loc:tlocation);
      begin
        case loc.loc of
           LOC_REGISTER,LOC_CREGISTER:
             cg.a_call_reg(list,loc.register);
           LOC_REFERENCE,LOC_CREFERENCE :
             cg.a_call_ref(list,loc.reference);
           else
             internalerror(200203311);
        end;
      end;


    function tcg.optimize_op_const_reg(list: taasmoutput; var op: topcg; var a : aword; var reg:tregister): boolean;
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


    procedure tcg.a_paramfpu_reg(list : taasmoutput;size : tcgsize;const r : tregister;const locpara : tparalocation);
      var
         ref : treference;
      begin
         case locpara.loc of
            LOC_FPUREGISTER,LOC_CFPUREGISTER:
              a_loadfpu_reg_reg(list,size,r,locpara.register);
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 if locpara.sp_fixup<>0 then
                   a_op_const_reg(list,OP_ADD,OS_ADDR,locpara.sp_fixup,NR_STACK_POINTER_REG);
                 reference_reset(ref);
                 ref.base:=locpara.reference.index;
                 ref.offset:=locpara.reference.offset;
                 a_loadfpu_reg_ref(list,size,r,ref);
              end
            else
              internalerror(2002071004);
         end;
      end;


    procedure tcg.a_paramfpu_ref(list : taasmoutput;size : tcgsize;const ref : treference;const locpara : tparalocation);
      var
         hr : tregister;
      begin
         hr:=rg.getregisterfpu(list,size);
         a_loadfpu_ref_reg(list,size,ref,hr);
         a_paramfpu_reg(list,size,hr,locpara);
         rg.ungetregisterfpu(list,hr,size);
      end;


    procedure tcg.a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference);

      var
        tmpreg: tregister;

      begin
        tmpreg:=rg.getregisterint(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_op_const_reg(list,op,size,a,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
        rg.ungetregisterint(list,tmpreg);
      end;


    procedure tcg.a_op_const_loc(list : taasmoutput; Op: TOpCG; a: AWord; const loc: tlocation);

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
        tmpreg: tregister;

      begin
        tmpreg:=rg.getregisterint(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_op_reg_reg(list,op,size,reg,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
        rg.ungetregisterint(list,tmpreg);
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
              tmpreg:=rg.getregisterint(list,size);
              a_load_ref_reg(list,size,size,ref,tmpreg);
              a_op_reg_reg(list,op,size,tmpreg,reg);
              rg.ungetregisterint(list,tmpreg);
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
              tmpreg:=rg.getregisterint(list,loc.size);
              a_load_ref_reg(list,loc.size,loc.size,ref,tmpreg);
              a_op_reg_ref(list,op,loc.size,tmpreg,loc.reference);
              rg.ungetregisterint(list,tmpreg);
            end;
          else
            internalerror(200109061);
        end;
      end;

    procedure tcg.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
        size: tcgsize; a: aword; src, dst: tregister);
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
            tmpreg := rg.getregisterint(list,size);
            a_load_reg_reg(list,size,size,src2,tmpreg);
            a_op_reg_reg(list,op,size,src1,tmpreg);
            a_load_reg_reg(list,size,size,tmpreg,dst);
            rg.ungetregisterint(list,tmpreg);
          end;
      end;



    procedure tcg.a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
     l : tasmlabel);

      var
        tmpreg: tregister;

      begin
        tmpreg:=rg.getregisterint(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_cmp_const_reg_label(list,size,cmp_op,a,tmpreg,l);
        rg.ungetregisterint(list,tmpreg);
      end;

    procedure tcg.a_cmp_const_loc_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const loc : tlocation;
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
        tmpreg:=rg.getregisterint(list,size);
        a_load_ref_reg(list,size,size,ref,tmpreg);
        a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
        rg.ungetregisterint(list,tmpreg);
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
              tmpreg := rg.getregisterint(list,size);
              a_load_ref_reg(list,size,size,loc.reference,tmpreg);
              a_cmp_ref_reg_label(list,size,cmp_op,ref,tmpreg,l);
              rg.ungetregisterint(list,tmpreg);
            end
          else
            internalerror(200109061);
        end;
      end;


    class function tcg.reg_cgsize(const reg: tregister) : tcgsize;
      begin
        reg_cgsize := OS_INT;
      end;


    procedure tcg.g_load_parent_framepointer(list:taasmoutput;parentsymtable:tsymtable;reg:tregister);
      var
        href : treference;
        i : integer;
      begin
        { make a reference }
        reference_reset_base(href,current_procinfo.framepointer,PARENT_FRAMEPOINTER_OFFSET);
        cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,reg);
        { walk parents }
        i:=current_procinfo.procdef.parast.symtablelevel-1;
        while (i>parentsymtable.symtablelevel) do
          begin
             { make a reference }
             reference_reset_base(href,reg,PARENT_FRAMEPOINTER_OFFSET);
             cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,reg);
             dec(i);
          end;
      end;


    procedure tcg.g_save_parent_framepointer_param(list:taasmoutput);
      begin
      end;


    procedure tcg.g_copyshortstring(list : taasmoutput;const source,dest : treference;len:byte;delsource,loadref : boolean);
      begin
{$ifdef FPC}
        {$warning FIX ME!}
{$endif}
        a_paramaddr_ref(list,dest,paramanager.getintparaloc(list,3));
        if loadref then
          a_param_ref(list,OS_ADDR,source,paramanager.getintparaloc(list,2))
        else
          a_paramaddr_ref(list,source,paramanager.getintparaloc(list,2));
        if delsource then
         reference_release(list,source);
        a_param_const(list,OS_INT,len,paramanager.getintparaloc(list,1));
        paramanager.freeintparaloc(list,3);
        paramanager.freeintparaloc(list,2);
        paramanager.freeintparaloc(list,1);
        rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
        a_call_name(list,'FPC_SHORTSTR_ASSIGN');
        rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
      end;


    procedure tcg.g_incrrefcount(list : taasmoutput;t: tdef; const ref: treference;loadref : boolean);
      var
        href : treference;
        incrfunc : string;
      begin
         { These functions should not change the registers (they use
           the saveregister proc directive }
         if is_interfacecom(t) then
          incrfunc:='FPC_INTF_INCR_REF'
         else if is_ansistring(t) then
          incrfunc:='FPC_ANSISTR_INCR_REF'
         else if is_widestring(t) then
          incrfunc:='FPC_WIDESTR_INCR_REF'
         else if is_dynamic_array(t) then
          incrfunc:='FPC_DYNARRAY_INCR_REF'
         else
          incrfunc:='';
         { call the special incr function or the generic addref }
         if incrfunc<>'' then
          begin
            { these functions get the pointer by value }
            a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(list,1));
            paramanager.freeintparaloc(list,1);
            rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,incrfunc);
            rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
          end
         else
          begin
            reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
            a_paramaddr_ref(list,href,paramanager.getintparaloc(list,2));
            if loadref then
              a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(list,1))
            else
              a_paramaddr_ref(list,ref,paramanager.getintparaloc(list,1));
            paramanager.freeintparaloc(list,1);
            paramanager.freeintparaloc(list,2);
            rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,'FPC_ADDREF');
            rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
         end;
      end;


    procedure tcg.g_decrrefcount(list : taasmoutput;t: tdef; const ref: treference; loadref:boolean);
      var
        href : treference;
        decrfunc : string;
        needrtti : boolean;
      begin
         needrtti:=false;
         if is_interfacecom(t) then
          decrfunc:='FPC_INTF_DECR_REF'
         else if is_ansistring(t) then
          decrfunc:='FPC_ANSISTR_DECR_REF'
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
               a_paramaddr_ref(list,href,paramanager.getintparaloc(list,2));
             end;
            if loadref then
              a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(list,1))
            else
              a_paramaddr_ref(list,ref,paramanager.getintparaloc(list,1));
            paramanager.freeintparaloc(list,1);
            rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,decrfunc);
            rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            if needrtti then
              paramanager.freeintparaloc(list,2);
          end
         else
          begin
            reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
            a_paramaddr_ref(list,href,paramanager.getintparaloc(list,2));
            if loadref then
              a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(list,1))
            else
              a_paramaddr_ref(list,ref,paramanager.getintparaloc(list,1));
            paramanager.freeintparaloc(list,1);
            paramanager.freeintparaloc(list,2);
            rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,'FPC_DECREF');
            rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
         end;
      end;


    procedure tcg.g_initialize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);
      var
         href : treference;
      begin
         if is_ansistring(t) or
            is_widestring(t) or
            is_interfacecom(t) then
           a_load_const_ref(list,OS_ADDR,0,ref)
         else
           begin
              reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
              a_paramaddr_ref(list,href,paramanager.getintparaloc(list,2));
              if loadref then
                a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(list,1))
              else
                a_paramaddr_ref(list,ref,paramanager.getintparaloc(list,1));
              paramanager.freeintparaloc(list,1);
              paramanager.freeintparaloc(list,2);
              rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
              a_call_name(list,'FPC_INITIALIZE');
              rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
           end;
      end;


    procedure tcg.g_finalize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);
      var
         href : treference;
      begin
         if is_ansistring(t) or
            is_widestring(t) or
            is_interfacecom(t) then
           g_decrrefcount(list,t,ref,loadref)
         else
           begin
              reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
              a_paramaddr_ref(list,href,paramanager.getintparaloc(list,2));
              if loadref then
                a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(list,1))
              else
                a_paramaddr_ref(list,ref,paramanager.getintparaloc(list,1));
              paramanager.freeintparaloc(list,1);
              paramanager.freeintparaloc(list,2);
              rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
              a_call_name(list,'FPC_FINALIZE');
              rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
           end;
      end;


    procedure tcg.g_rangecheck(list: taasmoutput; const l:tlocation;fromdef,todef: tdef);
    { generate range checking code for the value at location p. The type     }
    { type used is checked against todefs ranges. fromdef (p.resulttype.def) }
    { is the original type used at that location. When both defs are equal   }
    { the check is also insert (needed for succ,pref,inc,dec)                }
      const
{$ifdef ver1_0}
        awordsignedmax=high(longint);
{$else}
        awordsignedmax=high(aword) div 2;
{$endif}
      var
        neglabel : tasmlabel;
        hreg : tregister;
        lto,hto,
        lfrom,hfrom : TConstExprInt;
        from_signed: boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(fromdef.deftype in [orddef,enumdef,arraydef]) then
          exit;
        if is_64bit(fromdef) or is_64bit(todef) then
          begin
             cg64.g_rangecheck64(list,l,fromdef,todef);
             exit;
          end;
        { only check when assigning to scalar, subranges are different, }
        { when todef=fromdef then the check is always generated         }
        getrange(fromdef,lfrom,hfrom);
        getrange(todef,lto,hto);
        { no range check if from and to are equal and are both longint/dword }
        { (if we have a 32bit processor) or int64/qword, since such          }
        { operations can at most cause overflows (JM)                        }
        { Note that these checks are mostly processor independent, they only }
        { have to be changed once we introduce 64bit subrange types          }
        if (fromdef = todef) and
           (fromdef.deftype=orddef) and
           (((sizeof(aword) = 4) and
             (((torddef(fromdef).typ = s32bit) and
               (lfrom = low(longint)) and
               (hfrom = high(longint))) or
              ((torddef(fromdef).typ = u32bit) and
               (lfrom = low(cardinal)) and
               (hfrom = high(cardinal)))))) then
          exit;
        if todef<>fromdef then
         begin
           { if the from-range falls completely in the to-range, no check }
           { is necessary                                                 }
           if (lto<=lfrom) and (hto>=hfrom) then
            exit;
         end;
        { generate the rangecheck code for the def where we are going to }
        { store the result                                               }

        { use the trick that                                                 }
        { a <= x <= b <=> 0 <= x-a <= b-a <=> unsigned(x-a) <= unsigned(b-a) }

        { To be able to do that, we have to make sure however that either    }
        { fromdef and todef are both signed or unsigned, or that we leave    }
        { the parts < 0 and > maxlongint out                                 }

        { is_signed now also works for arrays (it checks the rangetype) (JM) }
        from_signed := is_signed(fromdef);
        if from_signed xor is_signed(todef) then
          if from_signed then
            { from is signed, to is unsigned }
            begin
              { if high(from) < 0 -> always range error }
              if (hfrom < 0) or
                 { if low(to) > maxlongint also range error }
                 (lto > awordsignedmax) then
                begin
                  a_call_name(list,'FPC_RANGEERROR');
                  exit
                end;
              { from is signed and to is unsigned -> when looking at from }
              { as an unsigned value, it must be < maxlongint (otherwise  }
              { it's negative, which is invalid since "to" is unsigned)   }
              if hto > awordsignedmax then
                hto := awordsignedmax;
            end
          else
            { from is unsigned, to is signed }
            begin
              if (lfrom > awordsignedmax) or
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
        hreg:=rg.getregisterint(list,OS_INT);
        a_load_loc_reg(list,OS_INT,l,hreg);
        a_op_const_reg(list,OP_SUB,OS_INT,aword(lto),hreg);
        objectlibrary.getlabel(neglabel);
        a_cmp_const_reg_label(list,OS_INT,OC_BE,aword(hto-lto),hreg,neglabel);
        { !!! should happen right after the compare (JM) }
        rg.ungetregisterint(list,hreg);
        a_call_name(list,'FPC_RANGEERROR');
        a_label(list,neglabel);
      end;


    procedure tcg.g_stackcheck(list : taasmoutput;stackframesize : longint);

      begin
         a_param_const(list,OS_32,stackframesize,paramanager.getintparaloc(list,1));
         a_call_name(list,'FPC_STACKCHECK');
         paramanager.freeintparaloc(list,1);
      end;


    procedure tcg.g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref:TReference);

      var
        tmpreg : tregister;
      begin
        tmpreg:=rg.getregisterint(list,size);
        g_flags2reg(list,size,f,tmpreg);
        a_load_reg_ref(list,size,size,tmpreg,ref);
        rg.ungetregisterint(list,tmpreg);
      end;


    procedure tcg.g_maybe_testself(list : taasmoutput;reg:tregister);
      var
        OKLabel : tasmlabel;
      begin
        if (cs_check_object in aktlocalswitches) or
           (cs_check_range in aktlocalswitches) then
         begin
           objectlibrary.getlabel(oklabel);
           a_cmp_const_reg_label(list,OS_ADDR,OC_NE,0,reg,oklabel);
           a_param_const(list,OS_INT,210,paramanager.getintparaloc(list,1));
           a_call_name(list,'FPC_HANDLEERROR');
           paramanager.freeintparaloc(list,1);
           a_label(list,oklabel);
         end;
      end;


    procedure tcg.g_maybe_testvmt(list : taasmoutput;reg:tregister;objdef:tobjectdef);
      var
        hrefvmt : treference;
      begin
        if (cs_check_object in aktlocalswitches) then
         begin
           reference_reset_symbol(hrefvmt,objectlibrary.newasmsymboldata(objdef.vmt_mangledname),0);
           a_paramaddr_ref(list,hrefvmt,paramanager.getintparaloc(list,2));
           a_param_reg(list,OS_ADDR,reg,paramanager.getintparaloc(list,1));
           paramanager.freeintparaloc(list,2);
           paramanager.freeintparaloc(list,1);
           rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
           a_call_name(list,'FPC_CHECK_OBJECT_EXT');
           rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
         end
        else
         if (cs_check_range in aktlocalswitches) then
          begin
            a_param_reg(list,OS_ADDR,reg,paramanager.getintparaloc(list,1));
            paramanager.freeintparaloc(list,1);
            rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            a_call_name(list,'FPC_CHECK_OBJECT');
            rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
          end;
      end;


{*****************************************************************************
                            Entry/Exit Code Functions
*****************************************************************************}

    procedure tcg.g_interrupt_stackframe_entry(list : taasmoutput);
      begin
      end;


    procedure tcg.g_interrupt_stackframe_exit(list : taasmoutput;accused,acchiused:boolean);
      begin
      end;


    procedure tcg.g_profilecode(list : taasmoutput);
      begin
      end;


    procedure tcg.g_exception_reason_save(list : taasmoutput; const href : treference);
      begin
        a_load_reg_ref(list, OS_S32, OS_32, NR_FUNCTION_RETURN_REG, href);
      end;


    procedure tcg.g_exception_reason_save_const(list : taasmoutput; const href : treference; a: aword);
      begin
        a_load_const_ref(list, OS_S32, a, href);
      end;


    procedure tcg.g_exception_reason_load(list : taasmoutput; const href : treference);
      begin
        a_load_ref_reg(list, OS_S32, OS_S32, href, NR_FUNCTION_RETURN_REG);
      end;


    procedure tcg64.a_op64_const_reg_reg(list: taasmoutput;op:TOpCG;value : qword; regsrc,regdst : tregister64);
      begin
        a_load64_reg_reg(list,regsrc,regdst,false);
        a_op64_const_reg(list,op,value,regdst);
      end;


    procedure tcg64.a_op64_reg_reg_reg(list: taasmoutput;op:TOpCG;regsrc1,regsrc2,regdst : tregister64);
      begin
        a_load64_reg_reg(list,regsrc2,regdst,false);
        a_op64_reg_reg(list,op,regsrc1,regdst);
      end;


initialization
    ;
finalization
  cg.free;
  cg64.free;
end.
{
  $Log$
  Revision 1.119  2003-09-07 22:09:34  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.118  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.117  2003/09/03 11:18:36  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.116.2.4  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.116.2.3  2003/08/28 18:35:07  peter
    * tregister changed to cardinal

  Revision 1.116.2.2  2003/08/27 20:23:55  peter
    * remove old ra code

  Revision 1.116.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.116  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.115  2003/07/23 11:01:14  jonas
    * several rg.allocexplicitregistersint/rg.deallocexplicitregistersint
      pairs round calls to helpers

  Revision 1.114  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.113  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.112  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.111  2003/06/12 21:11:10  peter
    * ungetregisterfpu gets size parameter

  Revision 1.110  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.109  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.108  2003/06/06 14:43:02  peter
    * g_copyopenarrayvalue gets length reference
    * don't copy open arrays for cdecl

  Revision 1.107  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.106  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.105  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.104  2003/06/01 01:02:39  peter
    * generic a_call_ref

  Revision 1.103  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.102  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.101  2003/05/30 21:40:00  jonas
    * fixed bug in a_load_loc_ref (the source instead of dest size was passed
      to a_load_reg_ref in case of a register)

  Revision 1.100  2003/05/30 12:36:13  jonas
    * use as little different registers on the ppc until newra is released,
      since every used register must be saved

  Revision 1.99  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.98  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.97  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.96  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.95  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.94  2003/05/01 12:23:46  jonas
    * fix for op_reg_reg_reg in case the destination is the same as the first
      source register

  Revision 1.93  2003/04/29 07:28:52  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.92  2003/04/27 11:21:32  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.91  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.90  2003/04/26 20:57:17  florian
    * fixed para locations of fpc_class_new helper call

  Revision 1.89  2003/04/26 17:21:08  florian
    * fixed passing of fpu values by fpu register

  Revision 1.88  2003/04/23 20:16:03  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.87  2003/04/23 14:42:07  daniel
    * Further register allocator work. Compiler now smaller with new
      allocator than without.
    * Somebody forgot to adjust ppu version number

  Revision 1.86  2003/04/23 13:20:34  peter
    * fix self passing to fpc_help_fail

  Revision 1.85  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.84  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.83  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.82  2003/04/22 10:09:34  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.81  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.80  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.79  2003/03/22 18:07:18  jonas
    + add used scratch registers to usedintbyproc for non-i386

  Revision 1.78  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.77  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.76  2003/01/31 22:47:48  peter
    * maybe_testself now really uses the passed register

  Revision 1.75  2003/01/30 21:46:35  peter
    * maybe_testvmt added

  Revision 1.74  2003/01/17 12:45:40  daniel
    * Fixed internalerror 200301081 problem

  Revision 1.73  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.72  2003/01/09 22:00:53  florian
    * fixed some PowerPC issues

  Revision 1.71  2003/01/09 20:41:10  florian
    * fixed broken PowerPC compiler

  Revision 1.70  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.69  2002/12/24 15:56:50  peter
    * stackpointer_alloc added for adjusting ESP. Win32 needs
      this for the pageprotection

  Revision 1.68  2002/12/20 18:14:04  peter
    * removed some runerror and writeln

  Revision 1.67  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.66  2002/11/25 17:43:16  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.65  2002/11/17 16:27:31  carl
    * document flags2reg

  Revision 1.64  2002/11/16 17:06:28  peter
    * return error 210 for failed self test

  Revision 1.63  2002/11/15 01:58:46  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.62  2002/10/16 19:01:43  peter
    + $IMPLICITEXCEPTIONS switch to turn on/off generation of the
      implicit exception frames for procedures with initialized variables
      and for constructors. The default is on for compatibility

  Revision 1.61  2002/10/05 12:43:23  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.60  2002/10/02 18:20:52  peter
    * Copy() is now internal syssym that calls compilerprocs

  Revision 1.59  2002/09/17 18:54:02  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.58  2002/09/09 19:29:29  peter
    * fixed dynarr_decr_ref call

  Revision 1.57  2002/09/07 15:25:01  peter
    * old logs removed and tabs fixed

  Revision 1.56  2002/09/01 21:04:47  florian
    * several powerpc related stuff fixed

  Revision 1.55  2002/09/01 17:05:43  florian
    + added abstract tcg.g_removevaluepara_openarray

  Revision 1.54  2002/09/01 12:09:27  peter
    + a_call_reg, a_call_loc added
    * removed exprasmlist references

  Revision 1.53  2002/08/19 18:17:48  carl
    + optimize64_op_const_reg implemented (optimizes 64-bit constant opcodes)
    * more fixes to m68k for 64-bit operations

  Revision 1.52  2002/08/17 22:09:43  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.51  2002/08/17 09:23:33  florian
    * first part of procinfo rewrite

  Revision 1.50  2002/08/16 14:24:57  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.49  2002/08/15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.48  2002/08/14 19:26:02  carl
    + routine to optimize opcodes with constants

  Revision 1.47  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.46  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.45  2002/08/10 17:15:20  jonas
    * register parameters are now LOC_CREGISTER instead of LOC_REGISTER

  Revision 1.44  2002/08/09 19:10:05  carl
    - moved new_exception and free_exception to ncgutils

  Revision 1.43  2002/08/05 18:27:48  carl
    + more more more documentation
    + first version include/exclude (can't test though, not enough scratch for i386 :()...

  Revision 1.42  2002/08/04 19:08:21  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.41  2002/07/30 20:50:43  florian
    * the code generator knows now if parameters are in registers

  Revision 1.40  2002/07/29 21:16:02  florian
    * some more ppc fixes

  Revision 1.39  2002/07/28 15:56:00  jonas
    + tcg64.a_op64_const_reg_reg() and tcg64.a_op64_reg_reg_reg() methods +
      generic implementation

  Revision 1.38  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.37  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

}
