{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl
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
unit cgobj;

{$i defines.inc}

  interface

    uses
       cclasses,aasm,symtable,
       cpuasm,cpubase,cpuinfo,
       cginfo,
       symconst,symbase,symtype,node;

    type
       talignment = (AM_NATURAL,AM_NONE,AM_2BYTE,AM_4BYTE,AM_8BYTE);

       tcg = class
          scratch_register_array_pointer : aword;
          unusedscratchregisters : tregisterset;

          alignment : talignment;
          {************************************************}
          {                 basic routines                 }
          constructor create;

          procedure a_label(list : taasmoutput;l : tasmlabel);virtual;

          { allocates register r by inserting a pai_realloc record }
          procedure a_reg_alloc(list : taasmoutput;r : tregister);
          { deallocates register r by inserting a pa_regdealloc record}
          procedure a_reg_dealloc(list : taasmoutput;r : tregister);

          { returns a register for use as scratch register }
          function get_scratch_reg(list : taasmoutput) : tregister;
          { releases a scratch register }
          procedure free_scratch_reg(list : taasmoutput;r : tregister);

          {************************************************}
          { code generation for subroutine entry/exit code }

          { initilizes data of type t                           }
          { if is_already_ref is true then the routines assumes }
          { that r points to the data to initialize             }
          procedure g_initialize(list : taasmoutput;t : tdef;const ref : treference;is_already_ref : boolean);

          { finalizes data of type t                            }
          { if is_already_ref is true then the routines assumes }
          { that r points to the data to finalizes              }
          procedure g_finalize(list : taasmoutput;t : tdef;const ref : treference;is_already_ref : boolean);

          { helper routines }
          procedure g_initialize_data(list : taasmoutput;p : tsym);
          procedure g_incr_data(list : taasmoutput;p : tsym);
          procedure g_finalize_data(list : taasmoutput;p : tnamedindexitem);
          procedure g_copyvalueparas(list : taasmoutput;p : tnamedindexitem);
          procedure g_finalizetempansistrings(list : taasmoutput);

          procedure g_entrycode(alist : TAAsmoutput;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);

          procedure g_exitcode(list : taasmoutput;parasize : longint;
            nostackframe,inlined : boolean);

          { string helper routines }
          procedure g_decrstrref(list : taasmoutput;const ref : treference;t : tdef);

          procedure g_removetemps(list : taasmoutput;p : tlinkedlist);

          { passing parameters, per default the parameter is pushed }
          { nr gives the number of the parameter (enumerated from   }
          { left to right), this allows to move the parameter to    }
          { register, if the cpu supports register calling          }
          { conventions                                             }
          procedure a_param_reg(list : taasmoutput;size : tcgsize;r : tregister;nr : longint);virtual; abstract;
          procedure a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);virtual;
          procedure a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);virtual;
          procedure a_param_loc(list : taasmoutput;const l : tlocation;nr : longint);
          procedure a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);virtual;

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

          procedure a_call_name(list : taasmoutput;const s : string;
            offset : longint);virtual; abstract;

          { move instructions }
          procedure a_load_const_reg(list : taasmoutput;size : tcgsize;a : aword;register : tregister);virtual; abstract;
          procedure a_load_const_ref(list : taasmoutput;size : tcgsize;a : aword;const ref : treference);virtual;
          procedure a_load_const_loc(list : taasmoutput;a : aword;const loc : tlocation);
          procedure a_load_reg_ref(list : taasmoutput;size : tcgsize;register : tregister;const ref : treference);virtual; abstract;
          procedure a_load_reg_reg(list : taasmoutput;size : tcgsize;reg1,reg2 : tregister);virtual; abstract;
          procedure a_load_reg_loc(list : taasmoutput;reg : tregister;const loc: tlocation);
          procedure a_load_ref_reg(list : taasmoutput;size : tcgsize;const ref : treference;register : tregister);virtual; abstract;
          procedure a_load_loc_reg(list : taasmoutput;const loc: tlocation; reg : tregister);
          procedure a_load_loc_ref(list : taasmoutput;const loc: tlocation; const ref : treference);
          procedure a_load_sym_ofs_reg(list: taasmoutput; const sym: tasmsymbol; ofs: longint; reg: tregister);virtual; abstract;

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

          { basic arithmetic operations }
          { note: for operators which require only one argument (not, neg), use }
          { the op_reg_reg, op_reg_reg or op_reg_loc methods and keep in mind   }
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

          procedure a_jmp_cond(list : taasmoutput;cond : TOpCmp;l: tasmlabel); virtual; abstract;
          procedure a_jmp_flags(list : taasmoutput;const f : TResFlags;l: tasmlabel); virtual; abstract;

          procedure g_flags2reg(list: taasmoutput; const f: tresflags; reg: TRegister); virtual; abstract;

          procedure a_loadaddr_ref_reg(list : taasmoutput;const ref : treference;r : tregister);virtual; abstract;
          procedure g_stackframe_entry(list : taasmoutput;localsize : longint);virtual; abstract;
          { restores the frame pointer at procedure exit, for the }
          { i386 it generates a simple leave                      }
          procedure g_restore_frame_pointer(list : taasmoutput);virtual; abstract;

          { some processors like the PPC doesn't allow to change the stack in }
          { a procedure, so we need to maintain an extra stack for the        }
          { result values of setjmp in exception code                         }
          { this two procedures are for pushing an exception value,           }
          { they can use the scratch registers                                }
          procedure g_push_exception_value_reg(list : taasmoutput;reg : tregister);virtual; abstract;
          procedure g_push_exception_value_const(list : taasmoutput;reg : tregister);virtual; abstract;
          { that procedure pops a exception value                             }
          procedure g_pop_exception_value_reg(list : taasmoutput;reg : tregister);virtual; abstract;
          procedure g_return_from_proc(list : taasmoutput;parasize : aword);virtual; abstract;
          {********************************************************}
          { these methods can be overriden for extra functionality }

          { the following methods do nothing: }
          procedure g_interrupt_stackframe_entry(list : taasmoutput);virtual;
          procedure g_interrupt_stackframe_exit(list : taasmoutput);virtual;

          procedure g_profilecode(list : taasmoutput);virtual;
          procedure g_stackcheck(list : taasmoutput;stackframesize : longint);virtual;

          procedure g_maybe_loadself(list : taasmoutput);virtual; abstract;
          { copies len bytes from the source to destination, if }
          { loadref is true, it assumes that it first must load }
          { the source address from the memory location where   }
          { source points to                                    }
          procedure g_concatcopy(list : taasmoutput;const source,dest : treference;len : aword;delsource,loadref : boolean);virtual; abstract;

          { generates rangechecking code for a node }
          procedure g_rangecheck(list: taasmoutput; const p: tnode;
            const todef: tdef); virtual;

          { returns the tcgsize corresponding with the size of reg }
          class function reg_cgsize(const reg: tregister) : tcgsize; virtual;

{$ifdef i386}
         { this one is only necessary due the the restrictions of the 80x86, }
         { so make it a special case (JM)                                    }
          function makeregsize(var reg: tregister; size: tcgsize): topsize; virtual; abstract;
{$endif i386}
       end;

    var
       cg : tcg; { this is the main code generator class }

  implementation

    uses
       globals,globtype,options,systems,cgbase,
       verbose,types,tgobj,symdef,cga,tainst,rgobj;

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
         list.concat(tairegalloc.alloc(r));
      end;

    procedure tcg.a_reg_dealloc(list : taasmoutput;r : tregister);

      begin
         list.concat(tairegalloc.dealloc(r));
      end;

    procedure tcg.a_label(list : taasmoutput;l : tasmlabel);

      begin
         list.concat(tai_label.create(l));
      end;

    function tcg.get_scratch_reg(list : taasmoutput) : tregister;

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
         get_scratch_reg:=r;
      end;

    procedure tcg.free_scratch_reg(list : taasmoutput;r : tregister);

      begin
{$ifdef i386}
         include(unusedscratchregisters,makereg32(r));
{$else i386}
         include(unusedscratchregisters,r);
{$endif i386}
         a_reg_dealloc(list,r);
      end;

{*****************************************************************************
            this methods must be overridden for extra functionality
******************************************************************************}

    procedure tcg.g_interrupt_stackframe_entry(list : taasmoutput);

      begin
      end;

    procedure tcg.g_interrupt_stackframe_exit(list : taasmoutput);

      begin
      end;

    procedure tcg.g_profilecode(list : taasmoutput);

      begin
      end;

{*****************************************************************************
          for better code generation these methods should be overridden
******************************************************************************}

    procedure tcg.a_param_const(list : taasmoutput;size : tcgsize;a : aword;nr : longint);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_load_const_reg(list,size,a,hr);
         a_param_reg(list,size,hr,nr);
         free_scratch_reg(list,hr);
      end;

    procedure tcg.a_param_ref(list : taasmoutput;size : tcgsize;const r : treference;nr : longint);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_load_ref_reg(list,size,r,hr);
         a_param_reg(list,size,hr,nr);
         free_scratch_reg(list,hr);
      end;


    procedure tcg.a_param_loc(list : taasmoutput;const l:tlocation;nr : longint);

      begin
        case l.loc of
          LOC_REGISTER,
          LOC_CREGISTER :
            a_param_reg(list,l.size,l.register,nr);
          LOC_CONSTANT :
            a_param_const(list,l.size,l.value,nr);
          LOC_CREFERENCE,
          LOC_REFERENCE :
            a_param_ref(list,l.size,l.reference,nr);
        else
          internalerror(2002032211);
        end;
      end;


    procedure tcg.a_paramaddr_ref(list : taasmoutput;const r : treference;nr : longint);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_loadaddr_ref_reg(list,r,hr);
         a_param_reg(list,OS_ADDR,hr,nr);
         free_scratch_reg(list,hr);
      end;


    procedure tcg.g_stackcheck(list : taasmoutput;stackframesize : longint);

      begin
         a_param_const(list,OS_32,stackframesize,1);
         a_call_name(list,'FPC_STACKCHECK',0);
      end;

{*****************************************************************************
                         String helper routines
*****************************************************************************}

    procedure tcg.g_removetemps(list : taasmoutput;p : tlinkedlist);

(*
      var
         hp : ptemptodestroy;
         pushedregs : tpushed;
*)

      begin
(*
         hp:=ptemptodestroy(p^.first);
         if not(assigned(hp)) then
           exit;
         tg.pushusedregisters(pushedregs,$ff);
         while assigned(hp) do
           begin
              if is_ansistring(hp^.typ) then
                begin
                   g_decrstrref(list,hp^.address,hp^.typ);
                   tg.ungetiftemp(hp^.address);
                end;
              hp:=ptemptodestroy(hp^.next);
           end;
         tg.popusedregisters(pushedregs);
*)
        runerror(211);
      end;

    procedure tcg.g_decrstrref(list : taasmoutput;const ref : treference;t : tdef);

{      var
         pushedregs : tpushedsaved; }

      begin
(*
         tg.pushusedregisters(pushedregs,$ff);
         a_param_ref_addr(list,ref,1);
         if is_ansistring(t) then
           a_call_name(list,'FPC_ANSISTR_DECR_REF',0)
         else if is_widestring(t) then
           a_call_name(list,'FPC_WIDESTR_DECR_REF',0)
         else internalerror(58993);
         tg.popusedregisters(pushedregs);
*)
        runerror(211);
      end;

{*****************************************************************************
                  Code generation for subroutine entry- and exit code
 *****************************************************************************}

    { initilizes data of type t                           }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to initialize             }
    procedure tcg.g_initialize(list : taasmoutput;t : tdef;const ref : treference;is_already_ref : boolean);

{      var
         hr : treference; }

      begin
(*
         if is_ansistring(t) or
           is_widestring(t) then
           a_load_const_ref(list,OS_8,0,ref)
         else
           begin
              reset_reference(hr);
              hr.symbol:=t^.get_inittable_label;
              a_param_ref_addr(list,hr,2);
              if is_already_ref then
                a_param_ref(list,OS_ADDR,ref,1)
              else
                a_param_ref_addr(list,ref,1);
              a_call_name(list,'FPC_INITIALIZE',0);
           end;
*)
        runerror(211);
      end;

    procedure tcg.g_finalize(list : taasmoutput;t : tdef;const ref : treference;is_already_ref : boolean);

{      var
         r : treference; }

      begin
(*
         if is_ansistring(t) or
           is_widestring(t) then
           begin
              g_decrstrref(list,ref,t);
           end
         else
           begin
              reset_reference(r);
              r.symbol:=t^.get_inittable_label;
              a_param_ref_addr(list,r,2);
              if is_already_ref then
                a_paramaddr_ref(list,ref,1)
              else
                a_param_ref_addr(list,ref,1);
              a_call_name(list,'FPC_FINALIZE',0);
           end;
*)
        runerror(211);
      end;

    { generates the code for initialisation of local data }
    procedure tcg.g_initialize_data(list : taasmoutput;p : tsym);

{      var
         hr : treference; }

      begin
(*
         if (tsym(p)^.typ=varsym) and
            assigned(pvarsym(p)^.vartype.def) and
            not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
              pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
            pvarsym(p)^.vartype.def^.needs_inittable then
           begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              if tsym(p)^.owner^.symtabletype=localsymtable then
                begin
                   hr.base:=procinfo^.framepointer;
                   hr.offset:=-pvarsym(p)^.address;
                end
              else
                begin
                   hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
                end;
              g_initialize(list,pvarsym(p)^.vartype.def,hr,false);
           end;
*)
        runerror(211);
      end;


    { generates the code for incrementing the reference count of parameters }
    procedure tcg.g_incr_data(list : taasmoutput;p : tsym);

{      var
         hr : treference; }

      begin
(*
         if (tsym(p)^.typ=varsym) and
            not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
              pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
            pvarsym(p)^.vartype.def^.needs_inittable and
            ((pvarsym(p)^.varspez=vs_value)) then
           begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              hr.symbol:=pvarsym(p)^.vartype.def^.get_inittable_label;
              a_param_ref_addr(list,hr,2);
              reset_reference(hr);
              hr.base:=procinfo^.framepointer;
              hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              a_param_ref_addr(list,hr,1);
              reset_reference(hr);
              a_call_name(list,'FPC_ADDREF',0);
           end;
*)
        runerror(211);
      end;


    { generates the code for finalisation of local data }
    procedure tcg.g_finalize_data(list : taasmoutput;p : tnamedindexitem);

 {     var
         hr : treference; }

      begin
(*
         if (tsym(p)^.typ=varsym) and
            assigned(pvarsym(p)^.vartype.def) and
            not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
            pvarsym(p)^.vartype.def^.needs_inittable then
           begin
              { not all kind of parameters need to be finalized  }
              if (tsym(p)^.owner^.symtabletype=parasymtable) and
                ((pvarsym(p)^.varspez=vs_var)  or
                 (pvarsym(p)^.varspez=vs_const) { and
                 (dont_copy_const_param(pvarsym(p)^.definition)) } ) then
                exit;
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              case tsym(p)^.owner^.symtabletype of
                 localsymtable:
                   begin
                      hr.base:=procinfo^.framepointer;
                      hr.offset:=-pvarsym(p)^.address;
                   end;
                 parasymtable:
                   begin
                      hr.base:=procinfo^.framepointer;
                      hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
                   end;
                 else
                   hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
              end;
              g_finalize(list,pvarsym(p)^.vartype.def,hr,false);
           end;
*)
        runerror(211);
      end;


    { generates the code to make local copies of the value parameters }
    procedure tcg.g_copyvalueparas(list : taasmoutput;p : tnamedindexitem);
      begin
         runerror(255);
      end;

(*
    var
       _list : taasmoutput;

    { wrappers for the methods, because TP doesn't know procedures }
    { of objects                                                   }

    procedure _copyvalueparas(s : tnamedindexitem);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_copyvalueparas(_list,s);
      end;
*)

    procedure tcg.g_finalizetempansistrings(list : taasmoutput);

(*
      var
         hp : ptemprecord;
         hr : treference;
*)

      begin
(*
         hp:=tg.templist;
         while assigned(hp) do
           begin
              if hp^.temptype in [tt_ansistring,tt_freeansistring] then
                begin
                   procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
                   reset_reference(hr);
                   hr.base:=procinfo^.framepointer;
                   hr.offset:=hp^.pos;
                   a_param_ref_addr(list,hr,1);
                   a_call_name(list,'FPC_ANSISTR_DECR_REF',0);
                end;
              hp:=hp^.next;
           end;
*)
        runerror(211);
     end;

(*
    procedure _finalize_data(s : tnamedindexitem);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_finalize_data(_list,s);
      end;

    procedure _incr_data(s : tnamedindexitem);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_incr_data(_list,tsym(s));
      end;

    procedure _initialize_data(s : tnamedindexitem);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_initialize_data(_list,tsym(s));
      end;
*)

    { generates the entry code for a procedure }
    procedure tcg.g_entrycode(alist : TAAsmoutput;make_global:boolean;
                     stackframe:longint;
                     var parasize:longint;var nostackframe:boolean;
                     inlined : boolean);


(*
      var
         hs : string;
         hp : pused_unit;
         initcode : taasmoutput;
{$ifdef GDB}
         stab_function_name : Pai_stab_function_name;
{$endif GDB}
         hr : treference;
         r : tregister;
*)

      begin
(*
         { Align }
         if (not inlined) then
           begin
              { gprof uses 16 byte granularity !! }
              if (cs_profile in aktmoduleswitches) then
                list^.insert(new(pai_align,init(16)))
              else
                if not(cs_littlesize in aktglobalswitches) then
                  list^.insert(new(pai_align,init(4)));
          end;
         { save registers on cdecl }
         if (po_savestdregs in aktprocsym^.definition^.procoptions) then
           begin
              for r:=firstreg to lastreg do
                begin
                   if (r in registers_saved_on_cdecl) then
                     if (r in (tg.availabletempregsint+
                               tg.availabletempregsfpu+
                               tg.availabletempregsmm)) then
                       begin
                          if not(r in tg.usedinproc) then
                            {!!!!!!!!!!!! a_push_reg(list,r) }
                       end
                     else
                       {!!!!!!!! a_push_reg(list,r) };
                end;
           end;
        { omit stack frame ? }
        if not inlined then
          if procinfo^.framepointer=stack_pointer then
            begin
               CGMessage(cg_d_stackframe_omited);
               nostackframe:=true;
               if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-pointersize;
            end
          else
            begin
               if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-pointersize*2;
               nostackframe:=false;
               if (po_interrupt in aktprocsym^.definition^.procoptions) then
                 g_interrupt_stackframe_entry(list);

               g_stackframe_entry(list,stackframe);

               if (cs_check_stack in aktlocalswitches) and
                 (tf_supports_stack_checking in target_info.flags) then
                 g_stackcheck(@initcode,stackframe);
            end;

         if cs_profile in aktmoduleswitches then
           g_profilecode(@initcode);
          if (not inlined) and (aktprocsym^.definition^.proctypeoption in [potype_unitinit]) then
            begin

              { needs the target a console flags ? }
              if tf_needs_isconsole in target_info.flags then
                begin
                   hr.symbol:=newasmsymbol('U_'+target_info.system_unit+'_ISCONSOLE');
                   if apptype=at_cui then
                     a_load_const_ref(list,OS_8,1,hr)
                   else
                     a_load_const_ref(list,OS_8,0,hr);
                   dispose(hr.symbol,done);
                end;

              hp:=pused_unit(usedunits.first);
              while assigned(hp) do
                begin
                   { call the unit init code and make it external }
                   if (hp^.u^.flags and uf_init)<>0 then
                     a_call_name(list,
                       'INIT$$'+hp^.u^.modulename^,0);
                    hp:=Pused_unit(hp^.next);
                end;
           end;

{$ifdef dummy}
         { a constructor needs a help procedure }
         if (aktprocsym^.definition^.options and poconstructor)<>0 then
           begin
             if procinfo^._class^.isclass then
               begin
                 list^.concat(new(paicpu,op_sym(A_CALL,S_NO,newasmsymbol('FPC_NEW_CLASS'))));
                 list^.concat(new(paicpu,op_cond_sym(A_Jcc,C_Z,S_NO,quickexitlabel)));
               end
             else
               begin
                 {
                 list^.insert(new(pai_labeled,init(A_JZ,quickexitlabel)));
                 list^.insert(new(paicpu,op_csymbol(A_CALL,S_NO,
                   newcsymbol('FPC_HELP_CONSTRUCTOR',0))));
                 list^.insert(new(paicpu,op_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI)));
                 concat_external('FPC_HELP_CONSTRUCTOR',EXT_NEAR);
                 }
               end;
           end;
{$endif dummy}
  {$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) then
           list^.insert(new(pai_force_line,init));
  {$endif GDB}

         { initialize return value }
         if assigned(procinfo^.returntype.def) and
           is_ansistring(procinfo^.returntype.def) or
           is_widestring(procinfo^.returntype.def) then
           begin
              reset_reference(hr);
              hr.offset:=procinfo^.return_offset;
              hr.base:=procinfo^.framepointer;
              a_load_const_ref(list,OS_32,0,hr);
           end;

         _list:=list;
         { generate copies of call by value parameters }
         if (po_assembler in aktprocsym^.definition^.procoptions) then
            aktprocsym^.definition^.parast^.foreach({$ifdef FPCPROCVAR}@{$endif}_copyvalueparas);

         { initialisizes local data }
         aktprocsym^.definition^.localst^.foreach({$ifdef FPCPROCVAR}@{$endif}_initialize_data);
         { add a reference to all call by value/const parameters }
         aktprocsym^.definition^.parast^.foreach({$ifdef FPCPROCVAR}@{$endif}_incr_data);

         if (cs_profile in aktmoduleswitches) or
           (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
           (assigned(procinfo^._class) and (procinfo^._class^.owner^.symtabletype=globalsymtable)) then
           make_global:=true;
         if not inlined then
           begin
              hs:=proc_names.get;

  {$ifdef GDB}
              if (cs_debuginfo in aktmoduleswitches) and target_os.use_function_relative_addresses then
                stab_function_name := new(pai_stab_function_name,init(strpnew(hs)));
  {$endif GDB}

              { insert the names for the procedure }
              while hs<>'' do
                begin
                   if make_global then
                     exprasmlist^.insert(new(pai_symbol,initname_global(hs,0)))
                   else
                     exprasmlist^.insert(new(pai_symbol,initname(hs,0)));

  {$ifdef GDB}
                   if (cs_debuginfo in aktmoduleswitches) then
                     begin
                       if target_os.use_function_relative_addresses then
                         list^.insert(new(pai_stab_function_name,init(strpnew(hs))));
                    end;
  {$endif GDB}

                  hs:=proc_names.get;
               end;
          end;

  {$ifdef GDB}
         if (not inlined) and (cs_debuginfo in aktmoduleswitches) then
           begin
              if target_os.use_function_relative_addresses then
                  list^.insert(stab_function_name);
              if make_global or ((procinfo^.flags and pi_is_global) <> 0) then
                  aktprocsym^.is_global := True;
              list^.insert(new(pai_stabs,init(aktprocsym^.stabstring)));
              aktprocsym^.isstabwritten:=true;
            end;
  {$endif GDB}
*)
      runerror(211);
    end;

    procedure tcg.g_exitcode(list : taasmoutput;parasize:longint;nostackframe,inlined:boolean);
(*
      var
  {$ifdef GDB}
         mangled_length : longint;
         p : pchar;
  {$endif GDB}
         nofinal,noreraiselabel : tasmlabel;
         hr : treference;
         r : tregister;
*)
      begin
(*
         if aktexitlabel^.is_used then
           list^.insert(new(pai_label,init(aktexitlabel)));

         { call the destructor help procedure }
         if (aktprocsym^.definition^.proctypeoption=potype_destructor) then
           begin
             if procinfo^._class^.is_class then
               a_call_name(list,'FPC_DISPOSE_CLASS',0)
             else
               begin
                  if procinfo^._class^.needs_inittable then
                    begin
                       getlabel(nofinal);
                       {!!!!!!!!!!
                       reset_reference(hr);
                       hr.base:=R_EBP;
                       hr.offset:=8;
                       a_cmp_reg_const_label(list,OS_ADDR,OZ_EQ,
                       }
                       reset_reference(hr);
                       hr.symbol:=procinfo^._class^.get_inittable_label;
                       a_paramaddr_ref(list,hr,2);
                       a_param_reg(list,OS_ADDR,self_pointer,1);
                       a_call_name(list,'FPC_FINALIZE',0);
                       a_label(list,nofinal);
                    end;
                  { vmt_offset_reg can be a scratch register, }
                  { but it must be always the same            }
                  a_reg_alloc(list,vmt_offset_reg);
                  a_load_const_reg(list,OS_32,procinfo^._class^.vmt_offset,vmt_offset_reg);
                  a_call_name(list,'FPC_HELP_DESTRUCTOR',0);
                  a_reg_dealloc(list,vmt_offset_reg);
               end;
           end;

         { finalize temporary data }
         g_finalizetempansistrings(list);

         _list:=list;

         { finalize local data }
         aktprocsym^.definition^.localst^.foreach({$ifdef FPCPROCVAR}@{$endif}_finalize_data);

         { finalize paras data }
         if assigned(aktprocsym^.definition^.parast) then
           aktprocsym^.definition^.parast^.foreach({$ifdef FPCPROCVAR}@{$endif}_finalize_data);

         { do we need to handle exceptions because of ansi/widestrings ? }
         if (procinfo^.flags and pi_needs_implicit_finally)<>0 then
           begin
              getlabel(noreraiselabel);

              a_call_name(list,'FPC_POPADDRSTACK',0);
              a_reg_alloc(list,accumulator);
              g_pop_exception_value_reg(list,accumulator);
              a_cmp_const_reg_label(list,OS_32,OC_EQ,0,accumulator,noreraiselabel);
              a_reg_dealloc(list,accumulator);

              { must be the return value finalized before reraising the exception? }
              if (procinfo^.returntype.def<>tdef(voiddef)) and
                (procinfo^.returntype.def^.needs_inittable) and
                ((procinfo^.returntype.def^.deftype<>objectdef) or
                not(pobjectdef(procinfo^.returntype.def)^.is_class)) then
                begin
                   reset_reference(hr);
                   hr.offset:=procinfo^.return_offset;
                   hr.base:=procinfo^.framepointer;
                   g_finalize(list,procinfo^.returntype.def,hr,ret_in_param(procinfo^.returntype.def));
                end;

              a_call_name(list,'FPC_RERAISE',0);
              a_label(list,noreraiselabel);
           end;

         { call __EXIT for main program }
         if (not DLLsource) and (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
           a_call_name(list,'FPC_DO_EXIT',0);

         { handle return value }
         if not(po_assembler in aktprocsym^.definition^.procoptions) then
             if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
               { handle_return_value(inlined) }
             else
               begin
                  { return self in EAX }
                  a_label(list,quickexitlabel);
                  a_reg_alloc(list,accumulator);
                  a_load_reg_reg(list,OS_ADDR,self_pointer,accumulator);
                  a_reg_dealloc(list,self_pointer);
                  a_label(list,quickexitlabel);
                  { we can't clear the zero flag because the Alpha     }
                  { for example doesn't have flags, we have to compare }
                  { the accu. in the caller                            }
               end;

         { stabs uses the label also ! }
         if aktexit2label^.is_used or
            ((cs_debuginfo in aktmoduleswitches) and not inlined) then
           a_label(list,aktexit2label);

{$ifdef dummy}
         { should we restore edi ? }
         { for all i386 gcc implementations }
         {!!!!!!!!!!! I don't know how to handle register saving yet }
         if (po_savestdregs in aktprocsym^.definition^.procoptions) then
           begin
             if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
              exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_EBX)));
             exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ESI)));
             exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDI)));
             { here we could reset R_EBX
               but that is risky because it only works
               if genexitcode is called after genentrycode
               so lets skip this for the moment PM
             aktprocsym^.definition^.usedregisters:=
               aktprocsym^.definition^.usedregisters or not ($80 shr byte(R_EBX));
             }
           end;
{$endif dummy}
         if not(nostackframe) and not inlined then
           g_restore_frame_pointer(list);
         { at last, the return is generated }

         if not inlined then
           if po_interrupt in aktprocsym^.definition^.procoptions then
             g_interrupt_stackframe_exit(list)
         else
           g_return_from_proc(list,parasize);
         list^.concat(new(pai_symbol_end,initname(aktprocsym^.definition^.mangledname)));

    {$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and not inlined  then
             begin
                aktprocsym^.concatstabto(list);
                if assigned(procinfo^._class) then
                  if (not assigned(procinfo^.parent) or
                     not assigned(procinfo^.parent^._class)) then
                    list^.concat(new(pai_stabs,init(strpnew(
                     '"$t:v'+procinfo^._class^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.selfpointer_offset)))));
                  {!!!!!!!!!!!!
                  else
                    list^.concat(new(pai_stabs,init(strpnew(
                     '"$t:r'+procinfo^._class^.numberstring+'",'+
                     tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[R_ESI])))));
                  }
                if (tdef(aktprocsym^.definition^.rettype.def) <> tdef(voiddef)) then
                  begin
                    if ret_in_param(aktprocsym^.definition^.rettype.def) then
                      list^.concat(new(pai_stabs,init(strpnew(
                       '"'+aktprocsym^.name+':X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                       tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                    else
                      list^.concat(new(pai_stabs,init(strpnew(
                       '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                       tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                    if (m_result in aktmodeswitches) then
                      if ret_in_param(aktprocsym^.definition^.rettype.def) then
                        list^.concat(new(pai_stabs,init(strpnew(
                         '"RESULT:X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                         tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                      else
                        list^.concat(new(pai_stabs,init(strpnew(
                         '"RESULT:X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                         tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                  end;
                mangled_length:=length(aktprocsym^.definition^.mangledname);
                getmem(p,mangled_length+50);
                strpcopy(p,'192,0,0,');
                strpcopy(strend(p),aktprocsym^.definition^.mangledname);
                list^.concat(new(pai_stabn,init(strnew(p))));
                {list^.concat(new(pai_stabn,init(strpnew('192,0,0,'
                 +aktprocsym^.definition^.mangledname))));
                p[0]:='2';p[1]:='2';p[2]:='4';
                strpcopy(strend(p),'_end');}
                freemem(p,mangled_length+50);
                list^.concat(new(pai_stabn,init(
                  strpnew('224,0,0,'+aktexit2label^.name))));
                 { strpnew('224,0,0,'
                 +aktprocsym^.definition^.mangledname+'_end'))));}
             end;
    {$endif GDB}
*)
        runerror(211);
      end;

{*****************************************************************************
                       some generic implementations
 ****************************************************************************}


    procedure tcg.a_load_const_ref(list : taasmoutput;size : tcgsize;a : aword;const ref : treference);

    var
      tmpreg: tregister;

      begin
        tmpreg := get_scratch_reg(list);
        a_load_const_reg(list,size,a,tmpreg);
        a_load_reg_ref(list,size,tmpreg,ref);
        free_scratch_reg(list,tmpreg);
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


    procedure tcg.a_load_reg_loc(list : taasmoutput;reg : tregister;const loc: tlocation);
      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            a_load_reg_ref(list,loc.size,reg,loc.reference);
          LOC_REGISTER,LOC_CREGISTER:
            a_load_reg_reg(list,loc.size,reg,loc.register);
          else
            internalerror(200203271);
        end;
      end;


    procedure tcg.a_load_loc_ref(list : taasmoutput;const loc: tlocation; const ref : treference);

      var
        tmpreg: tregister;

      begin
        case loc.loc of
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
{$ifdef i386}
              case loc.size of
                OS_8,OS_S8:
                  tmpreg := reg32toreg8(rg.getregisterint(exprasmlist));
                OS_16,OS_S16:
                  tmpreg := reg32toreg16(get_scratch_reg(list));
                else
                  tmpreg := get_scratch_reg(list);
              end;
{$else i386}
              tmpreg := get_scratch_reg(list);
{$endif i386}
              a_load_ref_reg(list,loc.size,loc.reference,tmpreg);
              a_load_reg_ref(list,loc.size,tmpreg,ref);
{$ifdef i386}
              if not (loc.size in [OS_32,OS_S32]) then
                rg.ungetregister(exprasmlist,tmpreg)
              else
{$endif i386}
              free_scratch_reg(list,tmpreg);
            end;
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
        tmpreg := get_scratch_reg(list);
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
        tmpreg := get_scratch_reg(list);
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
              tmpreg := get_scratch_reg(list);
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
              tmpreg := get_scratch_reg(list);
{$ifdef i386}
              makeregsize(tmpreg,loc.size);
{$endif i386}
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
        tmpreg := get_scratch_reg(list);
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
        tmpreg := get_scratch_reg(list);
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
              tmpreg := get_scratch_reg(list);
{$ifdef i386}
              makeregsize(tmpreg,size);
{$endif i386}
              a_load_ref_reg(list,size,loc.reference,tmpreg);
              a_cmp_ref_reg_label(list,size,cmp_op,ref,tmpreg,l);
{$ifdef i386}
              if makereg32(tmpreg) <> R_EDI then
                rg.ungetregister(exprasmlist,tmpreg)
              else
{$endif i386}
              free_scratch_reg(list,tmpreg);
            end
          else
            internalerror(200109061);
        end;
      end;


    procedure tcg.g_rangecheck(list: taasmoutput; const p: tnode;
        const todef: tdef);
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
          { then fromdef and todef can only be orddefs }
           (((sizeof(aword) = 4) and
             (((torddef(fromdef).typ = s32bit) and
               (lfrom = low(longint)) and
               (hfrom = high(longint))) or
              ((torddef(fromdef).typ = u32bit) and
               (lfrom = low(cardinal)) and
               (hfrom = high(cardinal))))) or
            is_64bitint(fromdef)) then
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
        { a <= x <= b <=> 0 <= x-a <= b-a <=> cardinal(x-a) <= cardinal(b-a) }

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
                  a_call_name(list,'FPC_RANGEERROR',0);
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
                  a_call_name(list,'FPC_RANGEERROR',0);
                  exit
                end;
              { from is unsigned and to is signed -> when looking at to }
              { as an unsigned value, it must be >= 0 (since negative   }
              { values are the same as values > maxlongint)             }
              if lto < 0 then
                lto := 0;
            end;

        hreg := get_scratch_reg(list);
        if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          a_op_const_reg_reg(list,OP_SUB,def_cgsize(p.resulttype.def),
           aword(longint(lto and $ffffffff)),p.location.register,hreg)
        else
          begin
            a_load_ref_reg(list,def_cgsize(p.resulttype.def),
              p.location.reference,hreg);
            a_op_const_reg(list,OP_SUB,aword(longint(lto and $ffffffff)),hreg);
          end;
        getlabel(neglabel);
        a_cmp_const_reg_label(list,OS_INT,OC_BE,aword(longint((hto-lto) and $ffffffff)),hreg,neglabel);
        { !!! should happen right after the compare (JM) }
        free_scratch_reg(list,hreg);
        a_call_name(list,'FPC_RANGEERROR',0);
        a_label(list,neglabel);
      end;


    function tcg.reg_cgsize(const reg: tregister) : tcgsize;
      begin
        reg_cgsize := OS_INT;
      end;



finalization
  cg.free;
end.
{
  $Log$
  Revision 1.10  2002-04-04 19:05:54  peter
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

  Revision 1.6  2001/12/30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.5  2001/12/29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.4  2001/09/30 21:26:42  peter
    * removed obsolete newst defines

  Revision 1.3  2001/09/30 16:17:17  jonas
    * made most constant and mem handling processor independent

  Revision 1.2  2001/09/28 20:39:32  jonas
    * changed all flow control structures (except for exception handling
      related things) to processor independent code (in new ncgflw unit)
    + generic cgobj unit which contains lots of code generator helpers with
      global "cg" class instance variable
    + cgcpu unit for i386 (implements processor specific routines of the above
      unit)
    * updated cgbase and cpubase for the new code generator units
    * include ncgflw unit in cpunode unit

  Revision 1.5  2001/09/09 17:10:26  jonas
    * some more things implemented

  Revision 1.4  2001/09/06 15:25:55  jonas
    * changed type of tcg from object to class ->  abstract methods are now
      a lot cleaner :)
    + more updates: load_*_loc methods, op_*_* methods, g_flags2reg method
      (if possible with geenric implementation and necessary ppc
       implementations)
    * worked a bit further on cgflw, now working on exitnode

  Revision 1.3  2001/09/05 20:21:03  jonas
    * new cgflow based on n386flw with all nodes until forn "translated"
    + a_cmp_loc_*_label methods for tcg
    + base implementatino for a_cmp_ref_*_label methods
    * small bugfixes to powerpc cg

  Revision 1.2  2001/08/26 13:37:04  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.1  2000/07/13 06:30:07  michael
    + Initial import

}
