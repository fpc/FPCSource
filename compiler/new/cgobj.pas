{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
       cobjects,aasm,symtable,symconst,cpuasm,cpubase;

    type
       qword = comp;

       pcg = ^tcg;
       tcg = object
          constructor init;
          destructor done;virtual;

          procedure a_call_name_ext(list : paasmoutput;const s : string;
            offset : longint);

          {************************************************}
          { code generation for subroutine entry/exit code }

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
          procedure g_decransiref(const ref : treference);

          procedure g_removetemps(list : paasmoutput;p : plinkedlist);

          {**********************************}
          { these methods must be overriden: }
          procedure a_push_reg(list : paasmoutput;r : tregister);virtual;
          procedure a_call_name(list : paasmoutput;const s : string;
            offset : longint);virtual;

          procedure a_load8_const_reg(list : paasmoutput;b : byte;register : tregister);virtual;
          procedure a_load16_const_reg(list : paasmoutput;w : word;register : tregister);virtual;
          procedure a_load32_const_reg(list : paasmoutput;l : longint;register : tregister);virtual;
          procedure a_load64_const_reg(list : paasmoutput;q : qword;register : tregister);virtual;

          procedure a_load8_reg_ref(list : paasmoutput;register : tregister;const ref : treference);virtual;
          procedure a_load16_reg_ref(list : paasmoutput;register : tregister;const ref : treference);virtual;
          procedure a_load32_reg_ref(list : paasmoutput;register : tregister;const ref : treference);virtual;
          procedure a_load64_reg_ref(list : paasmoutput;register : tregister;const ref : treference);virtual;

          procedure a_load8_ref_reg(list : paasmoutput;const ref : treference;register : tregister);virtual;
          procedure a_load16_ref_reg(list : paasmoutput;const ref : treference;register : tregister);virtual;
          procedure a_load32_ref_reg(list : paasmoutput;const ref : treference;register : tregister);virtual;
          procedure a_load64_ref_reg(list : paasmoutput;const ref : treference;register : tregister);virtual;

          procedure a_loadaddress_ref_reg(list : paasmoutput;ref : treference;r : tregister);virtual;

          procedure g_stackframe_entry(list : paasmoutput;localsize : longint);virtual;
          procedure g_maybe_loadself(list : paasmoutput);virtual;

          {********************************************************}
          { these methods can be overriden for extra functionality }

          { the following methods do nothing: }
          procedure g_interrupt_stackframe_entry(list : paasmoutput);virtual;
          procedure g_interrupt_stackframe_exit(list : paasmoutput);virtual;

          procedure g_profilecode(list : paasmoutput);virtual;
          procedure g_stackcheck(list : paasmoutput;stackframesize : longint);virtual;

          procedure a_load8_const_ref(list : paasmoutput;b : byte;const ref : treference);virtual;
          procedure a_load16_const_ref(list : paasmoutput;w : word;const ref : treference);virtual;
          procedure a_load32_const_ref(list : paasmoutput;l : longint;const ref : treference);virtual;
          procedure a_load64_const_ref(list : paasmoutput;q : qword;const ref : treference);virtual;
          { passing parameters, per default the parameter is pushed }
          { nr gives the number of the parameter (enumerated from   }
          { left to right), this allows to move the parameter to    }
          { register, if the cpu supports register calling          }
          { conventions                                             }
          procedure a_param_reg(list : paasmoutput;r : tregister;nr : longint);virtual;
          procedure a_param8_const(list : paasmoutput;b : byte;nr : longint);virtual;
          procedure a_param16_const(list : paasmoutput;w : word;nr : longint);virtual;
          procedure a_param32_const(list : paasmoutput;l : longint;nr : longint);virtual;
          procedure a_param64_const(list : paasmoutput;q : qword;nr : longint);virtual;
          { uses the addr of ref as param, was emitpushreferenceaddr }
          procedure a_param_ref_addr(list : paasmoutput;r : treference;nr : longint);virtual;
       end;

    var
       cg : pcg; { this is the main code generator class }

  implementation

    uses
       globals,globtype,options,files,gdb,systems,
       ppu,cgbase,verbose,types,tgobj,tgcpu
       ;

    constructor tcg.init;

      begin
      end;

    destructor tcg.done;

      begin
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

    procedure tcg.a_param_reg(list : paasmoutput;r : tregister;nr : longint);

      begin
         a_push_reg(list,r);
      end;

    procedure tcg.a_param8_const(list : paasmoutput;b : byte;nr : longint);

      begin
         {!!!!!!!! a_push_const8(list,b); }
      end;

    procedure tcg.a_param16_const(list : paasmoutput;w : word;nr : longint);

      begin
         {!!!!!!!! a_push_const16(list,w); }
      end;

    procedure tcg.a_param32_const(list : paasmoutput;l : longint;nr : longint);

      begin
         {!!!!!!!! a_push_const32(list,l); }
      end;

    procedure tcg.a_param64_const(list : paasmoutput;q : qword;nr : longint);

      begin
         {!!!!!!!! a_push_const64(list,q); }
      end;

    procedure tcg.a_param_ref_addr(list : paasmoutput;r : treference;nr : longint);

      begin
         a_loadaddress_ref_reg(list,r,scratch_register);
         a_param_reg(list,scratch_register,nr);
      end;

    procedure tcg.g_stackcheck(list : paasmoutput;stackframesize : longint);

      begin
         a_param32_const(list,stackframesize,1);
         a_call_name_ext(list,'FPC_STACKCHECK',0);
      end;

    procedure tcg.a_call_name_ext(list : paasmoutput;const s : string;
      offset : longint);

      begin
         a_call_name(list,s,offset);
      end;

    procedure tcg.a_load8_const_ref(list : paasmoutput;b : byte;const ref : treference);

      begin
         a_load8_const_reg(list,b,scratch_register);
         a_load8_reg_ref(list,scratch_register,ref);
      end;

    procedure tcg.a_load16_const_ref(list : paasmoutput;w : word;const ref : treference);

      begin
         a_load16_const_reg(list,w,scratch_register);
         a_load16_reg_ref(list,scratch_register,ref);
      end;

    procedure tcg.a_load32_const_ref(list : paasmoutput;l : longint;const ref : treference);

      begin
         a_load32_const_reg(list,l,scratch_register);
         a_load32_reg_ref(list,scratch_register,ref);
      end;

    procedure tcg.a_load64_const_ref(list : paasmoutput;q : qword;const ref : treference);

      begin
         a_load64_const_reg(list,q,scratch_register);
         a_load64_reg_ref(list,scratch_register,ref);
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
                   g_decransiref(hp^.address);
                   tg.ungetiftemp(hp^.address);
                end;
              hp:=ptemptodestroy(hp^.next);
           end;
         tg.popusedregisters(pushedregs);
      end;

    procedure tcg.g_decransiref(const ref : treference);

      begin
         {!!!!!!!!!}
         { emitpushreferenceaddr(exprasmlist,ref);
         emitcall('FPC_ANSISTR_DECR_REF',true); }
      end;

{*****************************************************************************
                  Code generation for subroutine entry- and exit code
 *****************************************************************************}

    { generates the code for initialisation of local data }
    procedure tcg.g_initialize_data(list : paasmoutput;p : psym);

      begin
         runerror(255);
      end;

    { generates the code for incrementing the reference count of parameters }
    procedure tcg.g_incr_data(list : paasmoutput;p : psym);

      var
         hr : treference;

      begin
         if (psym(p)^.typ=varsym) and
            not((pvarsym(p)^.definition^.deftype=objectdef) and
              pobjectdef(pvarsym(p)^.definition)^.is_class) and
            pvarsym(p)^.definition^.needs_inittable and
            ((pvarsym(p)^.varspez=vs_value)) then
           begin
              procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
              reset_reference(hr);
              hr.symbol:=pvarsym(p)^.definition^.get_inittable_label;
              a_param_ref_addr(list,hr,2);
              reset_reference(hr);
              hr.base:=procinfo.framepointer;
              hr.offset:=pvarsym(p)^.address+procinfo.call_offset;
              a_param_ref_addr(list,hr,1);
              reset_reference(hr);
              a_call_name(list,'FPC_ADDREF',0);
           end;
      end;

    { generates the code for finalisation of local data }
    procedure tcg.g_finalize_data(list : paasmoutput;p : pnamedindexobject);

      begin
         runerror(255);
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

    procedure _copyvalueparas(s : pnamedindexobject);{$ifndef FPC}far;{$endif}

      begin
         cg^.g_copyvalueparas(_list,s);
      end;

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
                   procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
                   reset_reference(hr);
                   hr.base:=procinfo.framepointer;
                   hr.offset:=hp^.pos;
                   a_param_ref_addr(list,hr,1);
                   a_call_name(list,'FPC_ANSISTR_DECR_REF',0);
                end;
              hp:=hp^.next;
           end;
     end;

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

    { generates the entry code for a procedure }
    procedure tcg.g_entrycode(list : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
       stackframe:longint;var parasize:longint;var nostackframe:boolean;
       inlined : boolean);

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
                            a_push_reg(list,r)
                       end
                     else
                       a_push_reg(list,r);
                end;
           end;
        { omit stack frame ? }
        if not inlined then
          if procinfo.framepointer=stack_pointer then
            begin
               CGMessage(cg_d_stackframe_omited);
               nostackframe:=true;
               if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo.call_offset-pointersize;
            end
          else
            begin
               if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo.call_offset-pointersize*2;
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
                     a_load8_const_ref(list,1,hr)
                   else
                     a_load8_const_ref(list,0,hr);
                   dispose(hr.symbol,done);
                end;

              hp:=pused_unit(usedunits.first);
              while assigned(hp) do
                begin
                   { call the unit init code and make it external }
                   if (hp^.u^.flags and uf_init)<>0 then
                     a_call_name_ext(list,
                       'INIT$$'+hp^.u^.modulename^,0);
                    hp:=Pused_unit(hp^.next);
                end;
           end;

{$ifdef dummy}
         { a constructor needs a help procedure }
         if (aktprocsym^.definition^.options and poconstructor)<>0 then
           begin
             if procinfo._class^.isclass then
               begin
                 list^.concat(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol('FPC_NEW_CLASS'))));
                 list^.concat(new(pai386,op_cond_sym(A_Jcc,C_Z,S_NO,quickexitlabel)));
               end
             else
               begin
                 {
                 list^.insert(new(pai_labeled,init(A_JZ,quickexitlabel)));
                 list^.insert(new(pai386,op_csymbol(A_CALL,S_NO,
                   newcsymbol('FPC_HELP_CONSTRUCTOR',0))));
                 list^.insert(new(pai386,op_const_reg(A_MOV,S_L,procinfo._class^.vmt_offset,R_EDI)));
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
         if is_ansistring(procinfo.retdef) or
           is_widestring(procinfo.retdef) then
           begin
              reset_reference(hr);
              hr.offset:=procinfo.retoffset;
              hr.base:=procinfo.framepointer;
              a_load32_const_ref(list,0,hr);
           end;

         _list:=list;
         { generate copies of call by value parameters }
         if (po_assembler in aktprocsym^.definition^.procoptions) then
            aktprocsym^.definition^.parast^.foreach({$ifdef FPC}@{$endif FPC}_copyvalueparas);

         { initialisizes local data }
         aktprocsym^.definition^.localst^.foreach({$ifdef FPC}@{$endif FPC}_initialize_data);
         { add a reference to all call by value/const parameters }
         aktprocsym^.definition^.parast^.foreach({$ifdef FPC}@{$endif FPC}_incr_data);

         if (cs_profile in aktmoduleswitches) or
           (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
           (assigned(procinfo._class) and (procinfo._class^.owner^.symtabletype=globalsymtable)) then
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
              if make_global or ((procinfo.flags and pi_is_global) <> 0) then
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
         noreraiselabel : pasmlabel;

      begin
         if aktexitlabel^.is_used then
           list^.insert(new(pai_label,init(aktexitlabel)));

         { call the destructor help procedure }
         if (aktprocsym^.definition^.proctypeoption=potype_destructor) then
           begin
             if procinfo._class^.is_class then
               a_call_name(list,'FPC_DISPOSE_CLASS',0)
             else
               begin
                  a_load32_const_reg(list,procinfo._class^.vmt_offset,scratch_register);
                  a_call_name(list,'FPC_HELP_DESTRUCTOR',0);
               end;
           end;

         { finalize temporary data }
         g_finalizetempansistrings(list);

         _list:=list;

         { finalize local data }
         aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}_finalize_data);

         { finalize paras data }
         if assigned(aktprocsym^.definition^.parast) then
           aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}_finalize_data);

         { do we need to handle exceptions because of ansi/widestrings ? }
         if (procinfo.flags and pi_needs_implicit_finally)<>0 then
           begin
              getlabel(noreraiselabel);

              a_call_name(list,'FPC_POPADDRSTACK',0);

              list^.concat(new(pai386,
                op_reg(A_POP,S_L,R_EAX)));
              list^.concat(new(pai386,
                op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
              emitjmp(C_E,noreraiselabel);
              { must be the return value finalized before reraising the exception? }
              if (procinfo.retdef<>pdef(voiddef)) and
                (procinfo.retdef^.needs_inittable) and
                ((procinfo.retdef^.deftype<>objectdef) or
                not(pobjectdef(procinfo.retdef)^.is_class)) then
                begin
                   reset_reference(hr);
                   hr.offset:=procinfo.retoffset;
                   hr.base:=procinfo.framepointer;
                   finalize(procinfo.retdef,hr,ret_in_param(procinfo.retdef));
                end;

              a_call_name(list,'FPC_RERAISE',0);
              exprasmlist^.concat(new(pai_label,init(noreraiselabel)));
           end;

         { call __EXIT for main program }
         if (not DLLsource) and (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
           a_call_name(list,'FPC_DO_EXIT',0);

         { handle return value }
         if not(po_assembler in aktprocsym^.definition^.procoptions) then
             if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
               handle_return_value(inlined)
             else
                 begin
                     { successful constructor deletes the zero flag }
                     { and returns self in eax                   }
                     exprasmlist^.concat(new(pai_label,init(quickexitlabel)));
                     { eax must be set to zero if the allocation failed !!! }
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_ESI,R_EAX)));
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,R_EAX,R_EAX)));
                 end;

         { stabs uses the label also ! }
         if aktexit2label^.is_used or
            ((cs_debuginfo in aktmoduleswitches) and not inlined) then
           exprasmlist^.concat(new(pai_label,init(aktexit2label)));
         { gives problems for long mangled names }
         {list^.concat(new(pai_symbol,init(aktprocsym^.definition^.mangledname+'_end')));}

         { should we restore edi ? }
         { for all i386 gcc implementations }
         if (po_savestdregs in aktprocsym^.definition^.procoptions) then
           begin
             if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
              exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EBX)));
             exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
             exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));
             { here we could reset R_EBX
               but that is risky because it only works
               if genexitcode is called after genentrycode
               so lets skip this for the moment PM
             aktprocsym^.definition^.usedregisters:=
               aktprocsym^.definition^.usedregisters or not ($80 shr byte(R_EBX));
             }
           end;

         if not(nostackframe) and not inlined then
             exprasmlist^.concat(new(pai386,op_none(A_LEAVE,S_NO)));
{$ifdef i386}
         { parameters are limited to 65535 bytes because }
         { ret allows only imm16                    }
         if (parasize>65535) and not(pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
          CGMessage(cg_e_parasize_too_big);
{$endif i386}
         { at last, the return is generated }

         if not inlined then
           if (po_interrupt in aktprocsym^.definition^.procoptions) then
             generate_interrupt_stackframe_exit
         else
          begin
          {Routines with the poclearstack flag set use only a ret.}
          { also routines with parasize=0     }
            if (parasize=0) or (pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
             exprasmlist^.concat(new(pai386,op_none(A_RET,S_NO)))
            else
             exprasmlist^.concat(new(pai386,op_const(A_RET,S_NO,parasize)));
          end;

         exprasmlist^.concat(new(pai_symbol_end,initname(aktprocsym^.definition^.mangledname)));

    {$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and not inlined  then
             begin
                 aktprocsym^.concatstabto(exprasmlist);
                 if assigned(procinfo._class) then
                   if (not assigned(procinfo.parent) or
                      not assigned(procinfo.parent^._class)) then
                     exprasmlist^.concat(new(pai_stabs,init(strpnew(
                      '"$t:v'+procinfo._class^.numberstring+'",'+
                      tostr(N_PSYM)+',0,0,'+tostr(procinfo.esi_offset)))))
                   else
                     exprasmlist^.concat(new(pai_stabs,init(strpnew(
                      '"$t:r'+procinfo._class^.numberstring+'",'+
                      tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[R_ESI])))));

                 if (pdef(aktprocsym^.definition^.retdef) <> pdef(voiddef)) then
                   if ret_in_param(aktprocsym^.definition^.retdef) then
                     exprasmlist^.concat(new(pai_stabs,init(strpnew(
                      '"'+aktprocsym^.name+':X*'+aktprocsym^.definition^.retdef^.numberstring+'",'+
                      tostr(N_PSYM)+',0,0,'+tostr(procinfo.retoffset)))))
                   else
                     exprasmlist^.concat(new(pai_stabs,init(strpnew(
                      '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.retdef^.numberstring+'",'+
                      tostr(N_PSYM)+',0,0,'+tostr(procinfo.retoffset)))));

                 mangled_length:=length(aktprocsym^.definition^.mangledname);
                 getmem(p,mangled_length+50);
                 strpcopy(p,'192,0,0,');
                 strpcopy(strend(p),aktprocsym^.definition^.mangledname);
                 exprasmlist^.concat(new(pai_stabn,init(strnew(p))));
                 {list^.concat(new(pai_stabn,init(strpnew('192,0,0,'
                  +aktprocsym^.definition^.mangledname))));
                 p[0]:='2';p[1]:='2';p[2]:='4';
                 strpcopy(strend(p),'_end');}
                 freemem(p,mangled_length+50);
                 exprasmlist^.concat(new(pai_stabn,init(
                   strpnew('224,0,0,'+aktexit2label^.name))));
                  { strpnew('224,0,0,'
                  +aktprocsym^.definition^.mangledname+'_end'))));}
             end;
    {$endif GDB}
         exprasmlist:=oldexprasmlist;

      end;
{*****************************************************************************
                       some abstract definitions
 ****************************************************************************}

    procedure tcg.a_push_reg(list : paasmoutput;r : tregister);

      begin
         abstract;
      end;

    procedure tcg.a_call_name(list : paasmoutput;const s : string;
      offset : longint);

      begin
         abstract;
      end;

    procedure tcg.a_load_const8_ref(list : paasmoutput;b : byte;const ref : treference);

      begin
         abstract;
      end;

    procedure tcg.a_load_const16_ref(list : paasmoutput;w : word;const ref : treference);

      begin
         abstract;
      end;

    procedure tcg.a_load_const32_ref(list : paasmoutput;l : longint;const ref : treference);

      begin
         abstract;
      end;

    procedure tcg.a_load_const64_ref(list : paasmoutput;q : qword;const ref : treference);

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

    procedure tcg.a_loadaddress_ref_reg(list : paasmoutput;ref : treference;r : tregister);

      begin
         abstract;
      end;

end.
{
  $Log$
  Revision 1.12  1999-08-05 17:10:56  florian
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

