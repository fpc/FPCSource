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
}
unit cgobj;

{$i fpcdefs.inc}

  interface

    uses
       cclasses,aasmbase,aasmtai,aasmcpu,symtable,
       cpubase,cpuinfo,cpupara,
       cginfo,
       symconst,symbase,symtype,node;

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
          scratch_register_array_pointer : aword;
          {# List of currently unused scratch registers }
          unusedscratchregisters : tregisterset;

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

          {# @abstract(Returns an int register for use as scratch register)
             This routine returns a register which can be used by
             the code generator as a general purpose scratch register.
             Since scratch_registers are scarce resources, the register
             should be freed by calling @link(free_scratch_reg) as
             soon as it is no longer required.
          }
          function get_scratch_reg_int(list : taasmoutput) : tregister;virtual;
          {# @abstract(Returns an address register for use as scratch register)
             This routine returns a register which can be used by
             the code generator as a pointer scratch register.
             Since scratch_registers are scarce resources, the register
             should be freed by calling @link(free_scratch_reg) as
             soon as it is no longer required.
          }
          function get_scratch_reg_address(list : taasmoutput) : tregister;virtual;
          {# @abstract(Releases a scratch register)

             Releases a scratch register.
             This routine is used to free a register which
             was previously allocated using @link(get_scratch_reg).
          }
          procedure free_scratch_reg(list : taasmoutput;r : tregister);

          { passing parameters, per default the parameter is pushed }
          { nr gives the number of the parameter (enumerated from   }
          { left to right), this allows to move the parameter to    }
          { register, if the cpu supports register calling          }
          { conventions                                             }

          {# Pass a parameter, which is located in a register, to a routine.

             This routine should push/send the parameter to the routine, as
             required by the specific processor ABI. This must be overriden for
             each CPU target.

             @param(size size of the operand in the register)
             @param(r register source of the operand)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);virtual;
          {# Pass a parameter, which is a constant, to a routine.

             A generic version is provided.

             @param(size size of the operand in constant)
             @param(a value of constant to send)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);virtual;
          {# Pass the value of a parameter, which is located in memory, to a routine.

             A generic version is provided.

             @param(size size of the operand in constant)
             @param(r Memory reference of value to send)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);virtual;
          {# Pass the value of a parameter, which can be located either in a register or memory location,
             to a routine.

             A generic version is provided.

             @param(l location of the operand to send)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_param_loc(list : taasmoutput;const l : tlocation;const locpara : tparalocation);
          {# Pass the address of a reference to a routine.

             A generic version is provided.

             @param(r reference to get address from)
             @param(nr parameter number (starting from one) of routine (from left to right))
          }
          procedure a_paramaddr_ref(list : taasmoutput;const r : treference;const locpara : tparalocation);virtual;

          {**********************************}
          { these methods must be overriden: }

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
          }
          procedure a_call_name(list : taasmoutput;const s : string);virtual; abstract;
          procedure a_call_ref(list : taasmoutput;const ref : treference);virtual; abstract;

          { move instructions }
          procedure a_load_const_reg(list : taasmoutput;size : tcgsize;a : aword;register : tregister);virtual; abstract;
          procedure a_load_const_ref(list : taasmoutput;size : tcgsize;a : aword;const ref : treference);virtual;
          procedure a_load_const_loc(list : taasmoutput;a : aword;const loc : tlocation);
          procedure a_load_reg_ref(list : taasmoutput;size : tcgsize;register : tregister;const ref : treference);virtual; abstract;
          procedure a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);virtual; abstract;
          procedure a_load_reg_loc(list : taasmoutput;size : tcgsize;reg : tregister;const loc: tlocation);
          procedure a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref : treference;register : tregister);virtual; abstract;
          procedure a_load_ref_ref(list : taasmoutput;size : tcgsize;const sref : treference;const dref : treference);virtual;
          procedure a_load_loc_reg(list : taasmoutput;const loc: tlocation; reg : tregister);
          procedure a_load_loc_ref(list : taasmoutput;const loc: tlocation; const ref : treference);
          procedure a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister);virtual; abstract;
          procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);virtual; abstract;

          { fpu move instructions }
          procedure a_loadfpu_reg_reg(list: taasmoutput; reg1, reg2: tregister); virtual; abstract;
          procedure a_loadfpu_ref_reg(list: taasmoutput; size: tcgsize; const ref: treference; reg: tregister); virtual; abstract;
          procedure a_loadfpu_reg_ref(list: taasmoutput; size: tcgsize; reg: tregister; const ref: treference); virtual; abstract;
          procedure a_loadfpu_loc_reg(list: taasmoutput; const loc: tlocation; const reg: tregister);
          procedure a_loadfpu_reg_loc(list: taasmoutput; size: tcgsize; const reg: tregister; const loc: tlocation);

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
          procedure a_op_const_reg(list : taasmoutput; Op: TOpCG; a: AWord; reg: TRegister); virtual; abstract;
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

          procedure g_flags2reg(list: taasmoutput; size: TCgSize; const f: tresflags; reg: TRegister); virtual; abstract;
          procedure g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref:TReference); virtual;

          { some processors like the PPC doesn't allow to change the stack in }
          { a procedure, so we need to maintain an extra stack for the        }
          { result values of setjmp in exception code                         }
          { this two procedures are for pushing an exception value,           }
          { they can use the scratch registers                                }
          procedure g_push_exception(list : taasmoutput;const exceptbuf:treference;l:AWord; exceptlabel:TAsmLabel);virtual;abstract;
          procedure g_pop_exception(list : taasmoutput;endexceptlabel:tasmlabel);virtual;abstract;

          procedure g_maybe_loadself(list : taasmoutput);virtual;
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

          procedure g_incrrefcount(list : taasmoutput;t: tdef; const ref: treference);
          procedure g_decrrefcount(list : taasmoutput;t: tdef; const ref: treference);
          procedure g_initialize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);
          procedure g_finalize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);

          {# Emits the call to the stack checking routine of
             the runtime library. The default behavior
             does not need to be modified, as it is generic
             for all platforms.
          }
          procedure g_stackcheck(list : taasmoutput;stackframesize : longint);virtual;

          {# Generates range checking code. It is to note
             that this routine does not need to be overriden,
             as it takes care of everything.

             @param(p Node which contains the value to check)
             @param(todef Type definition of node to range check)
          }
          procedure g_rangecheck(list: taasmoutput; const p: tnode;
            const todef: tdef); virtual;

          { generates overflow checking code for a node }
          procedure g_overflowcheck(list: taasmoutput; const p: tnode); virtual; abstract;

          {**********************************}
          {    entry/exit code helpers       }

          procedure g_copyvaluepara_openarray(list : taasmoutput;const ref:treference;elesize:integer); virtual; abstract;
          {# Emits instructions which should be emitted when entering
             a routine declared as @var(interrupt). The default
             behavior does nothing, should be overriden as required.
          }
          procedure g_interrupt_stackframe_entry(list : taasmoutput);virtual;

          {# Emits instructions which should be emitted when exiting
             a routine declared as @var(interrupt). The default
             behavior does nothing, should be overriden as required.
          }
          procedure g_interrupt_stackframe_exit(list : taasmoutput;selfused,accused,acchiused:boolean);virtual;

          {# Emits instructions when compilation is done in profile
             mode (this is set as a command line option). The default
             behavior does nothing, should be overriden as required.
          }
          procedure g_profilecode(list : taasmoutput);virtual;
          procedure g_stackframe_entry(list : taasmoutput;localsize : longint);virtual; abstract;
          { restores the frame pointer at procedure exit }
          procedure g_restore_frame_pointer(list : taasmoutput);virtual; abstract;
          procedure g_return_from_proc(list : taasmoutput;parasize : aword);virtual; abstract;
          procedure g_call_constructor_helper(list : taasmoutput);virtual;
          procedure g_call_destructor_helper(list : taasmoutput);virtual;
          procedure g_call_fail_helper(list : taasmoutput);virtual;
          procedure g_save_standard_registers(list : taasmoutput);virtual;abstract;
          procedure g_restore_standard_registers(list : taasmoutput);virtual;abstract;
          procedure g_save_all_registers(list : taasmoutput);virtual;abstract;
          procedure g_restore_all_registers(list : taasmoutput;selfused,accused,acchiused:boolean);virtual;abstract;
       end;

    {# @abstract(Abstract code generator for 64 Bit operations)
       This class implements an abstract code generator class
       for 64 Bit operations.
    }
    tcg64 = class
        procedure a_load64_const_ref(list : taasmoutput;value : qword;const ref : treference);virtual;abstract;
        procedure a_load64_reg_ref(list : taasmoutput;reg : tregister64;const ref : treference);virtual;abstract;
        procedure a_load64_ref_reg(list : taasmoutput;const ref : treference;reg : tregister64);virtual;abstract;
        procedure a_load64_reg_reg(list : taasmoutput;regsrc,regdst : tregister64);virtual;abstract;
        procedure a_load64_const_reg(list : taasmoutput;value : qword;reg : tregister64);virtual;abstract;
        procedure a_load64_loc_reg(list : taasmoutput;const l : tlocation;reg : tregister64);virtual;abstract;
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

        procedure a_param64_reg(list : taasmoutput;reg64 : tregister64;const loc : tparalocation);virtual;abstract;
        procedure a_param64_const(list : taasmoutput;value : qword;const loc : tparalocation);virtual;abstract;
        procedure a_param64_ref(list : taasmoutput;const r : treference;const loc : tparalocation);virtual;abstract;
        procedure a_param64_loc(list : taasmoutput;const l : tlocation;const loc : tparalocation);virtual;abstract;

        { override to catch 64bit rangechecks }
        procedure g_rangecheck64(list: taasmoutput; const p: tnode;
          const todef: tdef);virtual;abstract;
    end;

    var
       {# Main code generator class }
       cg : tcg;
       {# Code generator class for all operations working with 64-Bit operands }
       cg64 : tcg64;

  implementation

    uses
       globals,globtype,options,systems,cgbase,
       verbose,defbase,tgobj,symdef,paramgr,
       rgobj;

    const
      max_scratch_regs = high(scratch_regs) - low(scratch_regs) + 1;

{*****************************************************************************
                            basic functionallity
******************************************************************************}

    constructor tcg.create;

      var
         i : longint;

      begin
         scratch_register_array_pointer:=1;
         for i:=low(scratch_regs) to high(scratch_regs) do
           include(unusedscratchregisters,scratch_regs[i]);
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

    function tcg.get_scratch_reg_int(list : taasmoutput) : tregister;

      var
         r : tregister;
         i : longint;

      begin
         if unusedscratchregisters=[] then
           internalerror(68996);

         for i:=scratch_register_array_pointer to
                (scratch_register_array_pointer+max_scratch_regs-1) do
           if scratch_regs[(i mod max_scratch_regs)+1] in unusedscratchregisters then
             begin
                r:=scratch_regs[(i mod max_scratch_regs)+1];
                break;
             end;
         exclude(unusedscratchregisters,r);
         inc(scratch_register_array_pointer);
         if scratch_register_array_pointer>max_scratch_regs then
           scratch_register_array_pointer:=1;
         a_reg_alloc(list,r);
         get_scratch_reg_int:=r;
      end;

    { the default behavior simply returns a general purpose register }
    function tcg.get_scratch_reg_address(list : taasmoutput) : tregister;
     begin
       get_scratch_reg_address := get_scratch_reg_int(list);
     end;


    procedure tcg.free_scratch_reg(list : taasmoutput;r : tregister);

      begin
         include(unusedscratchregisters,rg.makeregsize(r,OS_INT));
         a_reg_dealloc(list,r);
      end;

{*****************************************************************************
          for better code generation these methods should be overridden
******************************************************************************}

    procedure tcg.a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;const locpara : tparalocation);

      var
         ref : treference;

      begin
         case locpara.loc of
            LOC_REGISTER:
              a_load_reg_reg(list,size,r,locpara.register);
            LOC_REFERENCE:
              begin
                 reference_reset(ref);
                 ref.base:=locpara.reference.index;
                 ref.offset:=locpara.reference.offset;
                 a_load_reg_ref(list,size,r,ref);
                 {!!!! FIX ME!, take sp_fixup into account }
                 internalerror(2002071005);
              end
            else
              internalerror(2002071004);
         end;
      end;

    procedure tcg.a_param_const(list : taasmoutput;size : tcgsize;a : aword;const locpara : tparalocation);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg_int(list);
         a_load_const_reg(list,size,a,hr);
         a_param_reg(list,size,hr,locpara);
         free_scratch_reg(list,hr);
      end;

    procedure tcg.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;const locpara : tparalocation);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg_int(list);
         a_load_ref_reg(list,size,r,hr);
         a_param_reg(list,size,hr,locpara);
         free_scratch_reg(list,hr);
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
         hr:=get_scratch_reg_address(list);
         a_loadaddr_ref_reg(list,r,hr);
         a_param_reg(list,OS_ADDR,hr,locpara);
         free_scratch_reg(list,hr);
      end;


{****************************************************************************
                       some generic implementations
****************************************************************************}

    procedure tcg.a_load_ref_ref(list : taasmoutput;size : tcgsize;const sref : treference;const dref : treference);

      var
        tmpreg: tregister;
{$ifdef i386}
        pushed_reg: tregister;
{$endif i386}

      begin
{$ifdef i386}
        { the following is done with defines to avoid a speed penalty,  }
        { since all this is only necessary for the 80x86 (because EDI   }
        { doesn't have an 8bit component which is directly addressable) }
        pushed_reg := R_NO;
        if size in [OS_8,OS_S8] then
          if (rg.countunusedregsint = 0) then
            begin
              if (dref.base <> R_EBX) and
                 (dref.index <> R_EBX) then
                pushed_reg := R_EBX
              else if (dref.base <> R_EAX) and
                      (dref.index <> R_EAX) then
                pushed_reg := R_EAX
              else pushed_reg := R_ECX;
              tmpreg := rg.makeregsize(pushed_reg,OS_8);
              list.concat(taicpu.op_reg(A_PUSH,S_L,pushed_reg));
            end
          else
            tmpreg := rg.getregisterint(exprasmlist)
        else
{$endif i386}
        tmpreg := get_scratch_reg_int(list);
        tmpreg:=rg.makeregsize(tmpreg,size);
        a_load_ref_reg(list,size,sref,tmpreg);
        a_load_reg_ref(list,size,tmpreg,dref);
{$ifdef i386}
        if size in [OS_8,OS_S8] then
          begin
            if (pushed_reg <> R_NO) then
              list.concat(taicpu.op_reg(A_POP,S_L,pushed_reg))
            else
              rg.ungetregister(exprasmlist,tmpreg)
          end
        else
{$endif i386}
        free_scratch_reg(list,tmpreg);
      end;


    procedure tcg.a_load_const_ref(list : taasmoutput;size : tcgsize;a : aword;const ref : treference);

      var
        tmpreg: tregister;

      begin
        tmpreg := get_scratch_reg_int(list);
        a_load_const_reg(list,size,a,tmpreg);
        a_load_reg_ref(list,size,tmpreg,ref);
        free_scratch_reg(list,tmpreg);
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


    procedure tcg.a_load_reg_loc(list : taasmoutput;size : tcgsize;reg : tregister;const loc: tlocation);
      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_reg_ref(list,size,reg,loc.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_reg(list,size,reg,loc.register);
          else
            internalerror(200203271);
        end;
      end;


    procedure tcg.a_load_loc_reg(list : taasmoutput;const loc: tlocation; reg : tregister);

      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_ref_reg(list,loc.size,loc.reference,reg);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_reg(list,loc.size,loc.register,reg);
          LOC_CONSTANT:
            a_load_const_reg(list,loc.size,loc.value,reg);
          else
            internalerror(200109092);
        end;
      end;


    procedure tcg.a_load_loc_ref(list : taasmoutput;const loc: tlocation; const ref : treference);

      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_ref_ref(list,loc.size,loc.reference,ref);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_ref(list,loc.size,loc.register,ref);
          LOC_CONSTANT:
            a_load_const_ref(list,loc.size,loc.value,ref);
          else
            internalerror(200109302);
        end;
      end;


    procedure tcg.a_loadfpu_loc_reg(list: taasmoutput; const loc: tlocation; const reg: tregister);

      begin
        case loc.loc of
          LOC_REFERENCE, LOC_CREFERENCE:
            a_loadfpu_ref_reg(list,loc.size,loc.reference,reg);
          LOC_FPUREGISTER, LOC_CFPUREGISTER:
            a_loadfpu_reg_reg(list,loc.register,reg);
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
            a_loadfpu_reg_reg(list,reg,loc.register);
          else
            internalerror(48991);
         end;
      end;


    procedure tcg.a_op_const_ref(list : taasmoutput; Op: TOpCG; size: TCGSize; a: AWord; const ref: TReference);

      var
        tmpreg: tregister;

      begin
        tmpreg := get_scratch_reg_int(list);
        a_load_ref_reg(list,size,ref,tmpreg);
        a_op_const_reg(list,op,a,tmpreg);
        a_load_reg_ref(list,size,tmpreg,ref);
        free_scratch_reg(list,tmpreg);
      end;


    procedure tcg.a_op_const_loc(list : taasmoutput; Op: TOpCG; a: AWord; const loc: tlocation);

      begin
        case loc.loc of
          LOC_REGISTER, LOC_CREGISTER:
            a_op_const_reg(list,op,a,loc.register);
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
        tmpreg := get_scratch_reg_int(list);
        a_load_ref_reg(list,size,ref,tmpreg);
        a_op_reg_reg(list,op,size,reg,tmpreg);
        a_load_reg_ref(list,size,tmpreg,ref);
        free_scratch_reg(list,tmpreg);
      end;


    procedure tcg.a_op_ref_reg(list : taasmoutput; Op: TOpCG; size: TCGSize; const ref: TReference; reg: TRegister);

      var
        tmpreg: tregister;

      begin
        case op of
          OP_NOT,OP_NEG:
            { handle it as "load ref,reg; op reg" }
            begin
              a_load_ref_reg(list,size,ref,reg);
              a_op_reg_reg(list,op,size,reg,reg);
            end;
          else
            begin
              tmpreg := get_scratch_reg_int(list);
              a_load_ref_reg(list,size,ref,tmpreg);
              a_op_reg_reg(list,op,size,tmpreg,reg);
              free_scratch_reg(list,tmpreg);
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
              tmpreg := get_scratch_reg_int(list);
              tmpreg:=rg.makeregsize(tmpreg,loc.size);
              a_load_ref_reg(list,loc.size,ref,tmpreg);
              a_op_reg_ref(list,op,loc.size,tmpreg,loc.reference);
              free_scratch_reg(list,tmpreg);
            end;
          else
            internalerror(200109061);
        end;
      end;

    procedure tcg.a_op_const_reg_reg(list: taasmoutput; op: TOpCg;
        size: tcgsize; a: aword; src, dst: tregister);
      begin
        a_load_reg_reg(list,size,src,dst);
        a_op_const_reg(list,op,a,dst);
      end;

    procedure tcg.a_op_reg_reg_reg(list: taasmoutput; op: TOpCg;
        size: tcgsize; src1, src2, dst: tregister);
      begin
        a_load_reg_reg(list,size,src2,dst);
        a_op_reg_reg(list,op,size,src1,dst);
      end;



    procedure tcg.a_cmp_const_ref_label(list : taasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;const ref : treference;
     l : tasmlabel);

      var
        tmpreg: tregister;

      begin
        tmpreg := get_scratch_reg_int(list);
        a_load_ref_reg(list,size,ref,tmpreg);
        a_cmp_const_reg_label(list,size,cmp_op,a,tmpreg,l);
        free_scratch_reg(list,tmpreg);
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
        tmpreg := get_scratch_reg_int(list);
        a_load_ref_reg(list,size,ref,tmpreg);
        a_cmp_reg_reg_label(list,size,cmp_op,tmpreg,reg,l);
        free_scratch_reg(list,tmpreg);
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
{$ifdef i386}
              { the following is done with defines to avoid a speed penalty,  }
              { since all this is only necessary for the 80x86 (because EDI   }
              { doesn't have an 8bit component which is directly addressable) }
              if size in [OS_8,OS_S8] then
                tmpreg := rg.getregisterint(exprasmlist)
              else
{$endif i386}
              tmpreg := get_scratch_reg_int(list);
              tmpreg := rg.makeregsize(tmpreg,size);
              a_load_ref_reg(list,size,loc.reference,tmpreg);
              a_cmp_ref_reg_label(list,size,cmp_op,ref,tmpreg,l);
{$ifdef i386}
              if size in [OS_8,OS_S8] then
                rg.ungetregister(exprasmlist,tmpreg)
              else
{$endif i386}
              free_scratch_reg(list,tmpreg);
            end
          else
            internalerror(200109061);
        end;
      end;


    function tcg.reg_cgsize(const reg: tregister) : tcgsize;
      begin
        reg_cgsize := OS_INT;
      end;


    procedure tcg.g_copyshortstring(list : taasmoutput;const source,dest : treference;len:byte;delsource,loadref : boolean);
      begin
        {$warning FIX ME!}
        a_paramaddr_ref(list,dest,paramanager.getintparaloc(3));
        if loadref then
          a_param_ref(list,OS_ADDR,source,paramanager.getintparaloc(2))
        else
          a_paramaddr_ref(list,source,paramanager.getintparaloc(2));
        if delsource then
         reference_release(list,source);
        a_param_const(list,OS_INT,len,paramanager.getintparaloc(1));
        a_call_name(list,'FPC_SHORTSTR_COPY');
        g_maybe_loadself(list);
      end;


    procedure tcg.g_incrrefcount(list : taasmoutput;t: tdef; const ref: treference);
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
            a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(1));
            a_call_name(list,incrfunc);
          end
         else
          begin
            reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
            a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
            a_paramaddr_ref(list,ref,paramanager.getintparaloc(1));
            a_call_name(list,'FPC_ADDREF');
         end;
      end;


    procedure tcg.g_decrrefcount(list : taasmoutput;t: tdef; const ref: treference);
      var
        href : treference;
        decrfunc : string;
      begin
         if is_interfacecom(t) then
          decrfunc:='FPC_INTF_DECR_REF'
         else if is_ansistring(t) then
          decrfunc:='FPC_ANSISTR_DECR_REF'
         else if is_widestring(t) then
          decrfunc:='FPC_WIDESTR_DECR_REF'
         else if is_dynamic_array(t) then
          decrfunc:='FPC_DYNARRAY_INCR_REF'
         else
          decrfunc:='';
         { call the special decr function or the generic decref }
         if decrfunc<>'' then
          begin
            a_paramaddr_ref(list,ref,paramanager.getintparaloc(1));
            a_call_name(list,decrfunc);
          end
         else
          begin
            reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
            a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
            a_paramaddr_ref(list,ref,paramanager.getintparaloc(1));
            a_call_name(list,'FPC_DECREF');
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
              a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
              if loadref then
                a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(1))
              else
                a_paramaddr_ref(list,ref,paramanager.getintparaloc(1));
              a_call_name(list,'FPC_INITIALIZE');
           end;
      end;


    procedure tcg.g_finalize(list : taasmoutput;t : tdef;const ref : treference;loadref : boolean);
      var
         href : treference;
      begin
         if is_ansistring(t) or
            is_widestring(t) or
            is_interfacecom(t) then
           g_decrrefcount(list,t,ref)
         else
           begin
              reference_reset_symbol(href,tstoreddef(t).get_rtti_label(initrtti),0);
              a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
              if loadref then
                a_param_ref(list,OS_ADDR,ref,paramanager.getintparaloc(1))
              else
                a_paramaddr_ref(list,ref,paramanager.getintparaloc(1));
              a_call_name(list,'FPC_FINALIZE');
           end;
      end;


    procedure tcg.g_rangecheck(list: taasmoutput; const p: tnode;const todef: tdef);
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
        fromdef : tdef;
        lto,hto,
        lfrom,hfrom : TConstExprInt;
        from_signed: boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(todef.deftype in [orddef,enumdef,arraydef]) then
          exit;
        if is_64bitint(p.resulttype.def) or is_64bitint(todef) then
          begin
             cg64.g_rangecheck64(list,p,todef);
             exit;
          end;
        { only check when assigning to scalar, subranges are different, }
        { when todef=fromdef then the check is always generated         }
        fromdef:=p.resulttype.def;
        getrange(p.resulttype.def,lfrom,hfrom);
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

        hreg := get_scratch_reg_int(list);
        if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          a_op_const_reg_reg(list,OP_SUB,def_cgsize(p.resulttype.def),
           aword(lto),p.location.register,hreg)
        else
          begin
            a_load_ref_reg(list,def_cgsize(p.resulttype.def),
              p.location.reference,hreg);
            a_op_const_reg(list,OP_SUB,aword(lto),hreg);
          end;
        getlabel(neglabel);
        a_cmp_const_reg_label(list,OS_INT,OC_BE,aword(hto-lto),hreg,neglabel);
        { !!! should happen right after the compare (JM) }
        free_scratch_reg(list,hreg);
        a_call_name(list,'FPC_RANGEERROR');
        a_label(list,neglabel);
      end;


    procedure tcg.g_stackcheck(list : taasmoutput;stackframesize : longint);

      begin
         a_param_const(list,OS_32,stackframesize,paramanager.getintparaloc(1));
         a_call_name(list,'FPC_STACKCHECK');
      end;


    procedure tcg.g_flags2ref(list: taasmoutput; size: TCgSize; const f: tresflags; const ref:TReference);

      var
        tmpreg : tregister;
      begin
        tmpreg := get_scratch_reg_int(list);
        g_flags2reg(list,size,f,tmpreg);
        a_load_reg_ref(list,size,tmpreg,ref);
        free_scratch_reg(list,tmpreg);
      end;


    procedure tcg.g_maybe_loadself(list : taasmoutput);
      var
         hp : treference;
         p : pprocinfo;
         i : longint;
      begin
         if assigned(procinfo^._class) then
           begin
              list.concat(tai_regalloc.Alloc(SELF_POINTER_REG));
              if lexlevel>normal_function_level then
                begin
                   reference_reset_base(hp,procinfo^.framepointer,procinfo^.framepointer_offset);
                   a_load_ref_reg(list,OS_ADDR,hp,SELF_POINTER_REG);
                   p:=procinfo^.parent;
                   for i:=3 to lexlevel-1 do
                     begin
                        reference_reset_base(hp,SELF_POINTER_REG,p^.framepointer_offset);
                        a_load_ref_reg(list,OS_ADDR,hp,SELF_POINTER_REG);
                        p:=p^.parent;
                     end;
                   reference_reset_base(hp,SELF_POINTER_REG,p^.selfpointer_offset);
                   a_load_ref_reg(list,OS_ADDR,hp,SELF_POINTER_REG);
                end
              else
                begin
                   reference_reset_base(hp,procinfo^.framepointer,procinfo^.selfpointer_offset);
                   a_load_ref_reg(list,OS_ADDR,hp,SELF_POINTER_REG);
                end;
           end;
      end;



{*****************************************************************************
                            Entry/Exit Code Functions
*****************************************************************************}
    procedure tcg.g_call_constructor_helper(list : taasmoutput);
     var
      href : treference;
      hregister : tregister;
     begin
        if is_class(procinfo^._class) then
          begin
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            { parameter 2 : self pointer / flag }
            {!! this is a terrible hack, normally the helper should get three params : }
            {    one with self register, one with flag and one with VMT pointer        }
            {reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset+POINTER_SIZE);}
            a_param_reg(list, OS_ADDR, SELF_POINTER_REG, paramanager.getintparaloc(2));

            { parameter 1 : vmt pointer (stored at the selfpointer address on stack)  }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset);
            a_param_ref(list, OS_ADDR,href,paramanager.getintparaloc(1));
            a_call_name(list,'FPC_NEW_CLASS');
            a_load_reg_reg(list,OS_ADDR,accumulator,SELF_POINTER_REG);
            { save the self pointer result }
            a_load_reg_ref(list,OS_ADDR,SELF_POINTER_REG,href);
            a_cmp_const_reg_label(list,OS_ADDR,OC_EQ,0,accumulator,faillabel);
          end
        else if is_object(procinfo^._class) then
          begin
            { parameter 3 :vmt_offset     }
            a_param_const(list, OS_32, procinfo^._class.vmt_offset, paramanager.getintparaloc(3));
            { parameter 2 : address of pointer to vmt }
            {  this is the first(?) parameter which was pushed to the constructor }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset-POINTER_SIZE);
            hregister:=get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list, href, hregister);
            a_param_reg(list, OS_ADDR,hregister,paramanager.getintparaloc(2));
            free_scratch_reg(list, hregister);
            { parameter 1 : address of self pointer   }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset);
            hregister:=get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list, href, hregister);
            a_param_reg(list, OS_ADDR,hregister,paramanager.getintparaloc(1));
            free_scratch_reg(list, hregister);
            a_call_name(list,'FPC_HELP_CONSTRUCTOR');
            a_load_reg_reg(list,OS_ADDR,accumulator,SELF_POINTER_REG);
            a_cmp_const_reg_label(list,OS_ADDR,OC_EQ,0,accumulator,faillabel);
          end
        else
          internalerror(200006161);
     end;


    procedure tcg.g_call_destructor_helper(list : taasmoutput);
      var
        nofinal : tasmlabel;
        href : treference;
      hregister : tregister;
      begin
        if is_class(procinfo^._class) then
         begin
           { 2nd parameter  : flag }
           reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset+POINTER_SIZE);
           a_param_ref(list, OS_ADDR,href,paramanager.getintparaloc(2));
           { 1st parameter to destructor : self }
           reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset);
           a_param_ref(list, OS_ADDR,href,paramanager.getintparaloc(1));
           a_call_name(list,'FPC_DISPOSE_CLASS')
         end
        else if is_object(procinfo^._class) then
         begin
           { must the object be finalized ? }
           if procinfo^._class.needs_inittable then
            begin
              getlabel(nofinal);
              reference_reset_base(href,procinfo^.framepointer,target_info.first_parm_offset);
              a_cmp_const_ref_label(list,OS_ADDR,OC_EQ,0,href,nofinal);
              reference_reset_base(href,SELF_POINTER_REG,0);
              g_finalize(list,procinfo^._class,href,false);
              a_label(list,nofinal);
            end;
           { actually call destructor }
            { parameter 3 :vmt_offset     }
            a_param_const(list, OS_32, procinfo^._class.vmt_offset, paramanager.getintparaloc(3));
            { parameter 2 : pointer to vmt }
            {  this is the first parameter which was pushed to the destructor }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset-POINTER_SIZE);
            a_param_ref(list, OS_ADDR, href ,paramanager.getintparaloc(2));
            { parameter 1 : address of self pointer   }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset);
            hregister:=get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list, href, hregister);
            a_param_reg(list, OS_ADDR,hregister,paramanager.getintparaloc(1));
            free_scratch_reg(list, hregister);
            a_call_name(list,'FPC_HELP_DESTRUCTOR');
         end
        else
         internalerror(200006162);
      end;


    procedure tcg.g_call_fail_helper(list : taasmoutput);
      var
        href : treference;
        hregister : tregister;
      begin
        if is_class(procinfo^._class) then
          begin
            {
              Dispose of the class then set self_pointer to nil
              both in stack and in self register.
            }
            { 2nd parameter  : flag }
            a_param_const(list,OS_32,1,paramanager.getintparaloc(2));
            { 1st parameter to destructor : self }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset);
            a_param_ref(list, OS_ADDR,href,paramanager.getintparaloc(1));
            a_call_name(list,'FPC_DISPOSE_CLASS');
            { SET SELF TO NIL }
            a_load_const_reg(list,OS_ADDR,0,SELF_POINTER_REG);
            { set the self pointer in the stack to nil }
            a_load_reg_ref(list,OS_ADDR,SELF_POINTER_REG,href);
          end
        else if is_object(procinfo^._class) then
          begin
            { parameter 3 :vmt_offset     }
            a_param_const(list, OS_32, procinfo^._class.vmt_offset, paramanager.getintparaloc(3));
            { parameter 2 : address of pointer to vmt }
            {  this is the first(?) parameter which was pushed to the constructor }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset-POINTER_SIZE);
            hregister:=get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list, href, hregister);
            a_param_reg(list, OS_ADDR,hregister,paramanager.getintparaloc(2));
            free_scratch_reg(list, hregister);
            { parameter 1 : address of self pointer   }
            reference_reset_base(href, procinfo^.framepointer,procinfo^.selfpointer_offset);
            hregister:=get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list, href, hregister);
            a_param_reg(list, OS_ADDR,hregister,paramanager.getintparaloc(1));
            free_scratch_reg(list, hregister);
            a_call_name(list,'FPC_HELP_FAIL');
            { SET SELF TO NIL }
            a_load_const_reg(list,OS_ADDR,0,SELF_POINTER_REG);
          end
        else
          internalerror(200006163);
      end;


    procedure tcg.g_interrupt_stackframe_entry(list : taasmoutput);
      begin
      end;


    procedure tcg.g_interrupt_stackframe_exit(list : taasmoutput;selfused,accused,acchiused:boolean);
      begin
      end;


    procedure tcg.g_profilecode(list : taasmoutput);
      begin
      end;

finalization
  cg.free;
  cg64.free;
end.
{
  $Log$
  Revision 1.38  2002-07-27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.37  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.36  2002/07/11 14:41:27  florian
    * start of the new generic parameter handling

  Revision 1.35  2002/07/07 10:16:29  florian
    * problems with last commit fixed

  Revision 1.33  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.32  2002/07/06 20:09:10  carl
  * updated generic constructor / destructor calling

  Revision 1.31  2002/07/02 11:40:00  jonas
    * fixed cg64 memory leak

  Revision 1.30  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.29  2002/07/01 16:23:52  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.28  2002/06/06 18:53:17  jonas
    * fixed internalerror(10) with -Or for i386 (a_load_ref_ref now saves
      a general purpose register if it needs one but none are available)

  Revision 1.27  2002/05/22 19:02:16  carl
  + generic FPC_HELP_FAIL
  + generic FPC_HELP_DESTRUCTOR instated (original from Pierre)
  + generic FPC_DISPOSE_CLASS
  + TEST_GENERIC define

  Revision 1.26  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.25  2002/05/18 13:34:05  peter
    * readded missing revisions

  Revision 1.24  2002/05/16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.23  2002/05/14 19:34:40  peter
    * removed old logs and updated copyright year

  Revision 1.22  2002/05/13 19:54:36  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.21  2002/05/12 19:57:16  carl
  * maybe_loadself portable

  Revision 1.20  2002/05/12 16:53:04  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.19  2002/04/26 15:19:04  peter
    * use saveregisters for incr routines, saves also problems with
      the optimizer

  Revision 1.18  2002/04/25 20:16:38  peter
    * moved more routines from cga/n386util

  Revision 1.17  2002/04/22 16:30:05  peter
    * fixed @methodpointer

  Revision 1.16  2002/04/21 15:25:30  carl
  + a_jmp_cond -> a_jmp_always (a_jmp_cond is NOT portable)
  + changeregsize -> rg.makeregsize

  Revision 1.15  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.14  2002/04/15 19:44:18  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.13  2002/04/07 13:22:11  carl
  + more documentation

  Revision 1.12  2002/04/07 09:12:46  carl
  + documentation

  Revision 1.11  2002/04/06 18:10:42  jonas
    * several powerpc-related additions and fixes

  Revision 1.10  2002/04/04 19:05:54  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.9  2002/04/02 17:11:27  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.8  2002/03/31 20:26:33  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.7  2002/03/04 19:10:11  peter
    * removed compiler warnings

}
