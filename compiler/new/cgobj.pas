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

  interface

    uses
       cobjects,aasm,symtable,cpuasm,cpubase,cgbase,cpuinfo,tainst
       {$IFDEF NEWST}
       {$ELSE}
       ,symconst
       {$ENDIF NEWST};

    type
       talignment = (AM_NATURAL,AM_NONE,AM_2BYTE,AM_4BYTE,AM_8BYTE);

       pcg = ^tcg;
       tcg = object
          scratch_register_array_pointer : aword;
          unusedscratchregisters : tregisterset;

          alignment : talignment;
          {************************************************}
          {                 basic routines                 }
          constructor init;
          destructor done;virtual;

          procedure a_label(list : paasmoutput;l : pasmlabel);virtual;

          { allocates register r by inserting a pai_realloc record }
          procedure a_reg_alloc(list : paasmoutput;r : tregister);
          { deallocates register r by inserting a pa_regdealloc record}
          procedure a_reg_dealloc(list : paasmoutput;r : tregister);

          { returns a register for use as scratch register }
          function get_scratch_reg(list : paasmoutput) : tregister;
          { releases a scratch register }
          procedure free_scratch_reg(list : paasmoutput;r : tregister);

          {************************************************}
          { code generation for subroutine entry/exit code }

          { initilizes data of type t                           }
          { if is_already_ref is true then the routines assumes }
          { that r points to the data to initialize             }
          procedure g_initialize(list : paasmoutput;t : pdef;const ref : treference;is_already_ref : boolean);

          { finalizes data of type t                            }
          { if is_already_ref is true then the routines assumes }
          { that r points to the data to finalizes              }
          procedure g_finalize(list : paasmoutput;t : pdef;const ref : treference;is_already_ref : boolean);

          { helper routines }
          procedure g_initialize_data(list : paasmoutput;p : psym);
          procedure g_incr_data(list : paasmoutput;p : psym);
          procedure g_finalize_data(list : paasmoutput;p : pnamedindexobject);
          procedure g_copyvalueparas(list : paasmoutput;p : pnamedindexobject);
          procedure g_finalizetempansistrings(list : paasmoutput);

          procedure g_entrycode(list : paasmoutput;
            const proc_names : tstringcontainer;make_global : boolean;
            stackframe : longint;var parasize : longint;
            var nostackframe : boolean;inlined : boolean);

          procedure g_exitcode(list : paasmoutput;parasize : longint;
            nostackframe,inlined : boolean);

          { string helper routines }
          procedure g_decrstrref(list : paasmoutput;const ref : treference;t : pdef);

          procedure g_removetemps(list : paasmoutput;p : plinkedlist);

          { passing parameters, per default the parameter is pushed }
          { nr gives the number of the parameter (enumerated from   }
          { left to right), this allows to move the parameter to    }
          { register, if the cpu supports register calling          }
          { conventions                                             }
          procedure a_param_reg(list : paasmoutput;size : tcgsize;r : tregister;nr : longint);virtual;
          procedure a_param_const(list : paasmoutput;size : tcgsize;a : aword;nr : longint);virtual;
          procedure a_param_ref(list : paasmoutput;size : tcgsize;const r : treference;nr : longint);virtual;
          procedure a_paramaddr_ref(list : paasmoutput;const r : treference;nr : longint);virtual;

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

          procedure a_call_name(list : paasmoutput;const s : string;
            offset : longint);virtual;

          { move instructions }
          procedure a_load_const_reg(list : paasmoutput;size : tcgsize;a : aword;register : tregister);virtual;
          procedure a_load_const_ref(list : paasmoutput;size : tcgsize;a : aword;const ref : treference);virtual;
          procedure a_load_reg_ref(list : paasmoutput;size : tcgsize;register : tregister;const ref : treference);virtual;
          procedure a_load_ref_reg(list : paasmoutput;size : tcgsize;const ref : treference;register : tregister);virtual;
          procedure a_load_reg_reg(list : paasmoutput;size : tcgsize;reg1,reg2 : tregister);virtual;

          {  comparison operations }
          procedure a_cmp_reg_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
            l : pasmlabel);virtual;
          procedure a_cmp_reg_reg_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : pasmlabel);
          procedure a_cmp_reg_ref_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister;l : pasmlabel);
          procedure a_cmp_ref_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
            l : pasmlabel);

          procedure a_jmp_cond(list : paasmoutput;cond : TOpCmp;l: pasmlabel);

          procedure a_loadaddress_ref_reg(list : paasmoutput;const ref : treference;r : tregister);virtual;
          procedure g_stackframe_entry(list : paasmoutput;localsize : longint);virtual;
          { restores the frame pointer at procedure exit, for the }
          { i386 it generates a simple leave                      }
          procedure g_restore_frame_pointer(list : paasmoutput);virtual;

          { some processors like the PPC doesn't allow to change the stack in }
          { a procedure, so we need to maintain an extra stack for the        }
          { result values of setjmp in exception code                         }
          { this two procedures are for pushing an exception value,           }
          { they can use the scratch registers                                }
          procedure g_push_exception_value_reg(list : paasmoutput;reg : tregister);virtual;
          procedure g_push_exception_value_const(list : paasmoutput;reg : tregister);virtual;
          { that procedure pops a exception value                             }
          procedure g_pop_exception_value_reg(list : paasmoutput;reg : tregister);virtual;
          procedure g_return_from_proc(list : paasmoutput;parasize : aword);virtual;
          {********************************************************}
          { these methods can be overriden for extra functionality }

          { the following methods do nothing: }
          procedure g_interrupt_stackframe_entry(list : paasmoutput);virtual;
          procedure g_interrupt_stackframe_exit(list : paasmoutput);virtual;

          procedure g_profilecode(list : paasmoutput);virtual;
          procedure g_stackcheck(list : paasmoutput;stackframesize : longint);virtual;

          procedure g_maybe_loadself(list : paasmoutput);virtual;
          { copies len bytes from the source to destination, if }
          { loadref is true, it assumes that it first must load }
          { the source address from the memory location where   }
          { source points to                                    }
          procedure g_concatcopy(list : paasmoutput;const source,dest : treference;len : aword;loadref : boolean);virtual;

          { uses the addr of ref as param, was emitpushreferenceaddr }
          procedure a_param_ref_addr(list : paasmoutput;r : treference;nr : longint);virtual;
       end;

    var
       cg : pcg; { this is the main code generator class }

  implementation

    uses
       strings,globals,globtype,options,files,gdb,systems,
       ppu,verbose,types,tgobj,tgcpu
       {$IFDEF NEWST}
       ,symbols,defs,symtablt
       {$ENDIF NEWST};

{*****************************************************************************
                            basic functionallity
******************************************************************************}

    constructor tcg.init;

      var
         i : longint;

      begin
         scratch_register_array_pointer:=1;
         for i:=1 to max_scratch_regs do
           include(unusedscratchregisters,scratch_regs[i]);
      end;

    destructor tcg.done;

      begin
      end;

    procedure tcg.a_reg_alloc(list : paasmoutput;r : tregister);

      begin
         list^.concat(new(pairegalloc,alloc(r)));
      end;

    procedure tcg.a_reg_dealloc(list : paasmoutput;r : tregister);

      begin
         list^.concat(new(pairegalloc,dealloc(r)));
      end;

    procedure tcg.a_label(list : paasmoutput;l : pasmlabel);

      begin
         list^.concat(new(pai_label,init(l)));
      end;

    function tcg.get_scratch_reg(list : paasmoutput) : tregister;

      var
         r : tregister;
         i : longint;

      begin
         if unusedscratchregisters=[] then
           internalerror(68996);

         for i:=scratch_register_array_pointer to
                (scratch_register_array_pointer+max_scratch_regs) do
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

    procedure tcg.free_scratch_reg(list : paasmoutput;r : tregister);

      begin
         include(unusedscratchregisters,r);
         a_reg_dealloc(list,r);
      end;

{*****************************************************************************
            this methods must be overridden for extra functionality
******************************************************************************}

    procedure tcg.g_interrupt_stackframe_entry(list : paasmoutput);

      begin
      end;

    procedure tcg.g_interrupt_stackframe_exit(list : paasmoutput);

      begin
      end;

    procedure tcg.g_profilecode(list : paasmoutput);

      begin
      end;

{*****************************************************************************
          for better code generation these methods should be overridden
******************************************************************************}

    procedure tcg.a_param_const(list : paasmoutput;size : tcgsize;a : aword;nr : longint);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_load_const_reg(list,size,a,hr);
         a_param_reg(list,size,hr,nr);
         free_scratch_reg(list,hr);
      end;

    procedure tcg.a_param_ref(list : paasmoutput;size : tcgsize;const r : treference;nr : longint);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_load_ref_reg(list,size,r,hr);
         a_param_reg(list,size,hr,nr);
         free_scratch_reg(list,hr);
      end;

    procedure tcg.a_param_ref_addr(list : paasmoutput;r : treference;nr : longint);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_loadaddress_ref_reg(list,r,hr);
         a_param_reg(list,OS_ADDR,hr,nr);
         free_scratch_reg(list,hr);
      end;

    procedure tcg.g_stackcheck(list : paasmoutput;stackframesize : longint);

      begin
         a_param_const(list,OS_32,stackframesize,1);
         a_call_name(list,'FPC_STACKCHECK',0);
      end;

    procedure tcg.a_load_const_ref(list : paasmoutput;size : tcgsize;a : aword;const ref : treference);

      var
         hr : tregister;

      begin
         hr:=get_scratch_reg(list);
         a_load_const_reg(list,size,a,hr);
         a_load_reg_ref(list,size,hr,ref);
         free_scratch_reg(list,hr);
      end;


    procedure tcg.g_concatcopy(list : paasmoutput;const source,dest : treference;len : aword;loadref : boolean);

      begin
         abstract;
      end;


{*****************************************************************************
                         String helper routines
*****************************************************************************}

    procedure tcg.g_removetemps(list : paasmoutput;p : plinkedlist);

      var
         hp : ptemptodestroy;
         pushedregs : tpushed;

      begin
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
      end;

    procedure tcg.g_decrstrref(list : paasmoutput;const ref : treference;t : pdef);

      var
         pushedregs : tpushed;

      begin
         tg.pushusedregisters(pushedregs,$ff);
         a_param_ref_addr(list,ref,1);
         if is_ansistring(t) then
           a_call_name(list,'FPC_ANSISTR_DECR_REF',0)
         else if is_widestring(t) then
           a_call_name(list,'FPC_WIDESTR_DECR_REF',0)
         else internalerror(58993);
         tg.popusedregisters(pushedregs);
      end;

{*****************************************************************************
                  Code generation for subroutine entry- and exit code
 *****************************************************************************}

    { initilizes data of type t                           }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to initialize             }
    procedure tcg.g_initialize(list : paasmoutput;t : pdef;const ref : treference;is_already_ref : boolean);

      var
         hr : treference;

      begin
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
      end;

    procedure tcg.g_finalize(list : paasmoutput;t : pdef;const ref : treference;is_already_ref : boolean);

      var
         r : treference;

      begin
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
      end;

    { generates the code for initialisation of local data }
    procedure tcg.g_initialize_data(list : paasmoutput;p : psym);

      var
         hr : treference;

      begin
      {$IFDEF NEWST}
         if (typeof(p^)=typeof(Tvarsym)) and
            assigned(pvarsym(p)^.definition) and
            not((typeof((pvarsym(p)^.definition^))=typeof(Tobjectdef)) and
              (oo_is_class in pobjectdef(pvarsym(p)^.definition)^.options)) and
            pvarsym(p)^.definition^.needs_inittable then
           begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              if typeof((psym(p)^.owner^))=typeof(Tprocsymtable) then
                begin
                   hr.base:=procinfo^.framepointer;
                   hr.offset:=-pvarsym(p)^.address;
                end
              else
                begin
                   hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
                end;
              g_initialize(list,pvarsym(p)^.definition,hr,false);
           end;
      {$ELSE}
         if (psym(p)^.typ=varsym) and
            assigned(pvarsym(p)^.vartype.def) and
            not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
              pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
            pvarsym(p)^.vartype.def^.needs_inittable then
           begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              if psym(p)^.owner^.symtabletype=localsymtable then
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
      {$ENDIF NEWST}
      end;


    { generates the code for incrementing the reference count of parameters }
    procedure tcg.g_incr_data(list : paasmoutput;p : psym);

      var
         hr : treference;

      begin
      {$IFDEF NEWST}
         if (typeof((psym(p)^))=typeof(Tparamsym)) and
            not((typeof((Pparamsym(p)^.definition^))=typeof(Tobjectdef)) and
              (oo_is_class in pobjectdef(pvarsym(p)^.definition)^.options)) and
            Pparamsym(p)^.definition^.needs_inittable and
            ((Pparamsym(p)^.varspez=vs_value)) then
           begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              hr.symbol:=pvarsym(p)^.definition^.get_inittable_label;
              a_param_ref_addr(list,hr,2);
              reset_reference(hr);
              hr.base:=procinfo^.framepointer;
              hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              a_param_ref_addr(list,hr,1);
              reset_reference(hr);
              a_call_name(list,'FPC_ADDREF',0);
           end;
      {$ELSE}
         if (psym(p)^.typ=varsym) and
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
      {$ENDIF NEWST}
      end;


    { generates the code for finalisation of local data }
    procedure tcg.g_finalize_data(list : paasmoutput;p : pnamedindexobject);

      var
         hr : treference;

      begin
      {$IFDEF NEWST}
         if (typeof((psym(p)^))=typeof(Tvarsym)) and
            assigned(pvarsym(p)^.definition) and
            not((typeof((pvarsym(p)^.definition^))=typeof(Tobjectdef)) and
            (oo_is_class in pobjectdef(pvarsym(p)^.definition)^.options)) and
            pvarsym(p)^.definition^.needs_inittable then
           begin
              { not all kind of parameters need to be finalized  }
              if (typeof((psym(p)^.owner^))=typeof(Tprocsymtable)) and
                ((pparamsym(p)^.varspez=vs_var)  or
                 (Pparamsym(p)^.varspez=vs_const) { and
                 (dont_copy_const_param(pvarsym(p)^.definition)) } ) then
                exit;
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              if typeof((Psym(p)^.owner^))=typeof(Tprocsymtable) then
                 begin
                    hr.base:=procinfo^.framepointer;
                    hr.offset:=-pvarsym(p)^.address;
                 end
              else if typeof((Psym(p)^.owner^))=typeof(Tprocsymtable) then
                 begin
                    hr.base:=procinfo^.framepointer;
                    hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
                 end
               else
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
              g_finalize(list,pvarsym(p)^.definition,hr,false);
           end;
      {$ELSE}
         if (psym(p)^.typ=varsym) and
            assigned(pvarsym(p)^.vartype.def) and
            not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
            pvarsym(p)^.vartype.def^.needs_inittable then
           begin
              { not all kind of parameters need to be finalized  }
              if (psym(p)^.owner^.symtabletype=parasymtable) and
                ((pvarsym(p)^.varspez=vs_var)  or
                 (pvarsym(p)^.varspez=vs_const) { and
                 (dont_copy_const_param(pvarsym(p)^.definition)) } ) then
                exit;
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              case psym(p)^.owner^.symtabletype of
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
      {$ENDIF NEWST}
      end;


    { generates the code to make local copies of the value parameters }
    procedure tcg.g_copyvalueparas(list : paasmoutput;p : pnamedindexobject);
      begin
         runerror(255);
      end;

    var
       _list : paasmoutput;

    { wrappers for the methods, because TP doesn't know procedures }
    { of objects                                                   }

    {$IFNDEF NEWST}
    procedure _copyvalueparas(s : pnamedindexobject);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_copyvalueparas(_list,s);
      end;
    {$ENDIF NEWST}

    procedure tcg.g_finalizetempansistrings(list : paasmoutput);

      var
         hp : ptemprecord;
         hr : treference;

      begin
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
     end;

 {$IFDEF NEWST}
    procedure _initialize_local(s:Pnamedindexobject);{$IFNDEF FPC}far;{$ENDIF}

    begin
        if typeof(s^)=typeof(Tparamsym) then
            cg^.g_incr_data(_list,Psym(s))
        else
            cg^.g_initialize_data(_list,Psym(s));
    end;

    procedure _finalize_data(s : pnamedindexobject);{$ifndef FPC}far;{$endif}

    begin
        if typeof(s^)=typeof(Tvarsym) then
            cg^.g_finalize_data(_list,s);
    end;

 {$ELSE}
    procedure _finalize_data(s : pnamedindexobject);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_finalize_data(_list,s);
      end;

    procedure _incr_data(s : pnamedindexobject);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_incr_data(_list,psym(s));
      end;

    procedure _initialize_data(s : pnamedindexobject);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_initialize_data(_list,psym(s));
      end;
 {$ENDIF NEWST}

    { generates the entry code for a procedure }
    procedure tcg.g_entrycode(list : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
       stackframe:longint;var parasize:longint;var nostackframe:boolean;
       inlined : boolean);


    {$IFDEF NEWST}
        procedure _copyvalueparas(s:Pparamsym);{$ifndef FPC}far;{$endif}

        begin
            cg^.g_copyvalueparas(_list,s);
        end;
    {$ENDIF NEWST}

      var
         hs : string;
         hp : pused_unit;
         initcode : taasmoutput;
{$ifdef GDB}
         stab_function_name : Pai_stab_function_name;
{$endif GDB}
         hr : treference;
         r : tregister;

      begin
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
         {$IFDEF NEWST}
         if (posavestdregs in aktprocdef^.options) then
         {$ELSE}
         if (po_savestdregs in aktprocsym^.definition^.procoptions) then
         {$ENDIF NEWST}
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
            {$IFDEF NEWST}
               if (aktprocdef^.proctype in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocdef^.localst^.paramdatasize+procinfo^.para_offset-pointersize;
            {$ELSE}
               if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-pointersize;
            {$ENDIF NEWST}
            end
          else
            begin
            {$IFDEF NEWST}
               if (aktprocdef^.proctype in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocdef^.localst^.paramdatasize+procinfo^.para_offset-pointersize*2;
            {$ELSE}
               if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-pointersize*2;
            {$ENDIF}
               nostackframe:=false;
            {$IFDEF NEWST}
               if (pointerrupt in aktprocdef^.options) then
                 g_interrupt_stackframe_entry(list);
            {$ELSE}
               if (po_interrupt in aktprocsym^.definition^.procoptions) then
                 g_interrupt_stackframe_entry(list);
            {$ENDIF NEWST}

               g_stackframe_entry(list,stackframe);

               if (cs_check_stack in aktlocalswitches) and
                 (tf_supports_stack_checking in target_info.flags) then
                 g_stackcheck(@initcode,stackframe);
            end;

         if cs_profile in aktmoduleswitches then
           g_profilecode(@initcode);
         {$IFDEF NEWST}
          if (not inlined) and (aktprocdef^.proctype in [potype_unitinit]) then
         {$ELSE}
          if (not inlined) and (aktprocsym^.definition^.proctypeoption in [potype_unitinit]) then
         {$ENDIF NEWST}
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

        {$IFDEF NEWST}
         { initialize return value }
         if assigned(procinfo^.retdef) and
           is_ansistring(procinfo^.retdef) or
           is_widestring(procinfo^.retdef) then
           begin
              reset_reference(hr);
              hr.offset:=procinfo^.return_offset;
              hr.base:=procinfo^.framepointer;
              a_load_const_ref(list,OS_32,0,hr);
           end;
        {$ELSE}
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
        {$ENDIF}

         _list:=list;
         { generate copies of call by value parameters }
        {$IFDEF NEWST}
         if (poassembler in aktprocdef^.options) then
            aktprocdef^.parameters^.foreach(@_copyvalueparas);
        {$ELSE}
         if (po_assembler in aktprocsym^.definition^.procoptions) then
            aktprocsym^.definition^.parast^.foreach({$ifdef FPC}@{$endif FPC}_copyvalueparas);
        {$ENDIF NEWST}

        {$IFDEF NEWST}
         { initialisizes local data }
         aktprocdef^.localst^.foreach({$ifdef FPC}@{$endif FPC}_initialize_local);
        {$ELSE}
         { initialisizes local data }
         aktprocsym^.definition^.localst^.foreach({$ifdef FPC}@{$endif FPC}_initialize_data);
         { add a reference to all call by value/const parameters }
         aktprocsym^.definition^.parast^.foreach({$ifdef FPC}@{$endif FPC}_incr_data);
        {$ENDIF NEWST}

        {$IFDEF NEWST}
         if (cs_profile in aktmoduleswitches) or
           (typeof(aktprocdef^.owner^)=typeof(Tglobalsymtable)) or
           (typeof(aktprocdef^.owner^)=typeof(Timplsymtable)) or
           (assigned(procinfo^._class) and
           (typeof(procinfo^._class^.owner^)=typeof(Tglobalsymtable)) or
           (typeof(procinfo^._class^.owner^)=typeof(Timplsymtable))) then
           make_global:=true;
        {$ELSE}
         if (cs_profile in aktmoduleswitches) or
           (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
           (assigned(procinfo^._class) and (procinfo^._class^.owner^.symtabletype=globalsymtable)) then
           make_global:=true;
        {$ENDIF NEWST}
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
    end;

    procedure tcg.g_exitcode(list : paasmoutput;parasize:longint;nostackframe,inlined:boolean);

      var
  {$ifdef GDB}
         mangled_length : longint;
         p : pchar;
  {$endif GDB}
         nofinal,noreraiselabel : pasmlabel;
         hr : treference;
         r : tregister;

      begin
         if aktexitlabel^.is_used then
           list^.insert(new(pai_label,init(aktexitlabel)));

         { call the destructor help procedure }
         {$IFDEF NEWST}
         if (aktprocdef^.proctype=potype_destructor) then
         {$ELSE}
         if (aktprocsym^.definition^.proctypeoption=potype_destructor) then
         {$ENDIF}
           begin
           {$IFDEF NEWST}
             if oo_is_class in procinfo^._class^.options then
           {$ELSE NEWST}
             if procinfo^._class^.is_class then
           {$ENDIF}
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
         {$IFDEF NEWST}
         aktprocdef^.localst^.foreach({$ifndef TP}@{$endif}_finalize_data);
         {$ELSE}
         aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}_finalize_data);
         {$ENDIF}

         {$IFNDEF NEWST}
         { finalize paras data }
         if assigned(aktprocsym^.definition^.parast) then
           aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}_finalize_data);
         {$ENDIF NEWST}

         { do we need to handle exceptions because of ansi/widestrings ? }
         if (procinfo^.flags and pi_needs_implicit_finally)<>0 then
           begin
              getlabel(noreraiselabel);

              a_call_name(list,'FPC_POPADDRSTACK',0);
              a_reg_alloc(list,accumulator);
              g_pop_exception_value_reg(list,accumulator);
              a_cmp_reg_const_label(list,OS_32,OC_EQ,0,accumulator,noreraiselabel);
              a_reg_dealloc(list,accumulator);

           {$IFDEF NEWST}
              { must be the return value finalized before reraising the exception? }
              if (procinfo^.retdef<>pdef(voiddef)) and
                (procinfo^.retdef^.needs_inittable) and
                ((typeof(procinfo^.retdef^)<>typeof(Tobjectdef)) or
                not(oo_is_class in pobjectdef(procinfo^.retdef)^.options)) then
                begin
                   reset_reference(hr);
                   hr.offset:=procinfo^.return_offset;
                   hr.base:=procinfo^.framepointer;
                   g_finalize(list,procinfo^.retdef,hr,not (dp_ret_in_acc in procinfo^.retdef^.properties));
                end;
           {$ELSE}
              { must be the return value finalized before reraising the exception? }
              if (procinfo^.returntype.def<>pdef(voiddef)) and
                (procinfo^.returntype.def^.needs_inittable) and
                ((procinfo^.returntype.def^.deftype<>objectdef) or
                not(pobjectdef(procinfo^.returntype.def)^.is_class)) then
                begin
                   reset_reference(hr);
                   hr.offset:=procinfo^.return_offset;
                   hr.base:=procinfo^.framepointer;
                   g_finalize(list,procinfo^.returntype.def,hr,ret_in_param(procinfo^.returntype.def));
                end;
           {$ENDIF}

              a_call_name(list,'FPC_RERAISE',0);
              a_label(list,noreraiselabel);
           end;

         { call __EXIT for main program }
      {$IFDEF NEWST}
         if (not DLLsource) and (not inlined) and (aktprocdef^.proctype=potype_proginit) then
           a_call_name(list,'FPC_DO_EXIT',0);
      {$ELSE}
         if (not DLLsource) and (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
           a_call_name(list,'FPC_DO_EXIT',0);
      {$ENDIF NEWST}

         { handle return value }
      {$IFDEF NEWST}
         if not(poassembler in aktprocdef^.options) then
             if (aktprocdef^.proctype<>potype_constructor) then
      {$ELSE}
         if not(po_assembler in aktprocsym^.definition^.procoptions) then
             if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
      {$ENDIF NEWST}
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
         {$IFDEF NEWST}
           if pointerrupt in aktprocdef^.options then
         {$ELSE}
           if po_interrupt in aktprocsym^.definition^.procoptions then
         {$ENDIF NEWST}
             g_interrupt_stackframe_exit(list)
         else
           g_return_from_proc(list,parasize);
    {$IFDEF NEWST}
         list^.concat(new(pai_symbol_end,initname(aktprocdef^.mangledname)));
    {$ELSE NEWST}
         list^.concat(new(pai_symbol_end,initname(aktprocsym^.definition^.mangledname)));
    {$ENDIF NEWST}

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
                if (pdef(aktprocsym^.definition^.rettype.def) <> pdef(voiddef)) then
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
      end;

{*****************************************************************************
                       some abstract definitions
 ****************************************************************************}

    procedure tcg.a_call_name(list : paasmoutput;const s : string;
      offset : longint);

      begin
         abstract;
      end;

    procedure tcg.g_stackframe_entry(list : paasmoutput;localsize : longint);

      begin
         abstract;
      end;

    procedure tcg.g_maybe_loadself(list : paasmoutput);

      begin
         abstract;
      end;

    procedure tcg.g_restore_frame_pointer(list : paasmoutput);

      begin
         abstract;
      end;

    procedure g_return_from_proc(list : paasmoutput;parasize : aword);

      begin
         abstract;
      end;

    procedure tcg.a_loadaddress_ref_reg(list : paasmoutput;const ref : treference;r : tregister);

      begin
         abstract;
      end;

    procedure tcg.g_push_exception_value_reg(list : paasmoutput;reg : tregister);

      begin
         abstract;
      end;

    procedure tcg.g_push_exception_value_const(list : paasmoutput;reg : tregister);

      begin
         abstract;
      end;

    procedure tcg.g_pop_exception_value_reg(list : paasmoutput;reg : tregister);

      begin
         abstract;
      end;

    procedure tcg.a_load_const_reg(list : paasmoutput;size : tcgsize;a : aword;register : tregister);

      begin
         abstract;
      end;

    procedure tcg.a_load_reg_ref(list : paasmoutput;size : tcgsize;register : tregister;const ref : treference);

      begin
         abstract;
      end;

    procedure tcg.a_load_ref_reg(list : paasmoutput;size : tcgsize;const ref : treference;register : tregister);

      begin
         abstract;
      end;

    procedure tcg.a_load_reg_reg(list : paasmoutput;size : tcgsize;reg1,reg2 : tregister);

      begin
         abstract;
      end;

    procedure tcg.a_cmp_reg_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
      l : pasmlabel);

      begin
         abstract;
      end;

    procedure tcg.a_cmp_reg_reg_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : pasmlabel);

      begin
         abstract;
      end;

    procedure tcg.a_cmp_reg_ref_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister;l : pasmlabel);

      begin
         abstract;
      end;

    procedure tcg.a_cmp_ref_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
     l : pasmlabel);

      begin
         abstract;
      end;

    procedure tcg.a_jmp_cond(list : paasmoutput;cond : TOpCmp;l: pasmlabel);

      begin
        abstract;
      end;

    procedure tcg.g_return_from_proc(list : paasmoutput;parasize : aword);

      begin
         abstract;
      end;

    procedure tcg.a_param_reg(list : paasmoutput;size : tcgsize;r : tregister;nr : longint);

      begin
         abstract;
      end;

    procedure tcg.a_paramaddr_ref(list : paasmoutput;const r : treference;nr : longint);

      begin
         abstract;
      end;

end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:07  michael
  + Initial import

  Revision 1.38  2000/04/29 09:01:06  jonas
    * nmem compiles again (at least for powerpc)

  Revision 1.37  2000/04/22 14:25:03  jonas
    * aasm.pas: pai_align instead of pai_align_abstract if cpu <> i386
    + systems.pas: info for macos/ppc
    * new/cgobj.pas: compiles again without newst define
    * new/powerpc/cgcpu: generate different entry/exit code depending on
      whether target_os is MacOs or Linux

  Revision 1.36  2000/03/11 21:11:24  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

  Revision 1.35  2000/03/01 15:36:13  florian
    * some new stuff for the new cg

  Revision 1.34  2000/02/20 20:49:46  florian
    * newcg is compiling
    * fixed the dup id problem reported by Paul Y.

  Revision 1.33  2000/01/07 01:14:53  peter
    * updated copyright to 2000

  Revision 1.32  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.31  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.30  1999/11/05 07:05:56  jonas
    + a_jmp_cond()

  Revision 1.29  1999/10/21 16:41:41  florian
    * problems with readln fixed: esi wasn't restored correctly when
      reading ordinal fields of objects futher the register allocation
      didn't take care of the extra register when reading ordinal values
    * enumerations can now be used in constant indexes of properties

  Revision 1.28  1999/10/12 21:20:46  florian
    * new codegenerator compiles again

  Revision 1.27  1999/09/29 11:46:20  florian
    * fixed bug 292 from bugs directory

  Revision 1.26  1999/09/14 11:16:09  florian
    * only small updates to work with the current compiler

  Revision 1.25  1999/09/03 13:09:09  jonas
    * fixed typo regarding scratchregs pointer

  Revision 1.24  1999/08/26 14:51:54  jonas
    * changed get_scratch_reg so it actually uses the
      scratch_reg_array_pointer

  Revision 1.23  1999/08/25 12:00:11  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.22  1999/08/18 17:05:55  florian
    + implemented initilizing of data for the new code generator
      so it should compile now simple programs

  Revision 1.21  1999/08/07 14:21:08  florian
    * some small problems fixed

  Revision 1.20  1999/08/06 18:05:52  florian
    * implemented some stuff for assignments

  Revision 1.19  1999/08/06 17:00:54  florian
    + definition of concatcopy

  Revision 1.18  1999/08/06 16:37:45  jonas
    * completed bugfix done by Florian o I wouldn't get conflicts :)

  Revision 1.17  1999/08/06 16:27:26  florian
    * for Jonas: else he will get conflicts

  Revision 1.16  1999/08/06 16:04:05  michael
  + introduced tainstruction

  Revision 1.15  1999/08/06 15:53:50  florian
    * made the alpha version compilable

  Revision 1.14  1999/08/06 14:15:51  florian
    * made the alpha version compilable

  Revision 1.13  1999/08/06 13:26:50  florian
    * more changes ...

  Revision 1.12  1999/08/05 17:10:56  florian
    * some more additions, especially procedure
      exit code generation

  Revision 1.11  1999/08/05 14:58:11  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.10  1999/08/04 00:23:52  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.9  1999/08/02 23:13:21  florian
    * more changes to compile for the Alpha

  Revision 1.8  1999/08/02 17:14:07  florian
    + changed the temp. generator to an object

  Revision 1.7  1999/08/01 23:05:55  florian
    * changes to compile with FPC

  Revision 1.6  1999/08/01 18:22:33  florian
   * made it again compilable

  Revision 1.5  1999/01/23 23:29:46  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.4  1999/01/13 22:52:36  florian
    + YES, finally the new code generator is compilable, but it doesn't run yet :(

  Revision 1.3  1998/12/26 15:20:30  florian
    + more changes for the new version

  Revision 1.2  1998/12/15 22:18:55  florian
    * some code added

  Revision 1.1  1998/12/15 16:32:58  florian
    + first version, derived from old routines

}