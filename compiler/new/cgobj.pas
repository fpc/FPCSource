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
       aasm;

    type
       pcg = ^tcg;
       tcg = object
          procedure a_call_name_ext(list : paasmoutput;const s : string;
            offset : longint;m : texternaltyp);

          procedure g_entrycode(list : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                              stackframe:longint;
                              var parasize:longint;var nostackframe:boolean;
                              inlined : boolean);

          { these methods must be overriden: }
          procedure a_push_reg(list : paasmoutput;r : tregister);virtual;
          procedure a_call_name(list : paasmoutput;const s : string;
            offset : longint);virtual;

          procedure a_load_const8_ref(list : paasmoutput;b : byte;ref : treference);virtual;
          procedure a_load_const16_ref(list : paasmoutput;w : word;ref : treference);virtual;
          procedure a_load_const32_ref(list : paasmoutput;l : longint;ref : treference);virtual;
          procedure a_load_const64_ref(list : paasmoutput;{ q : qword; }ref : treference);virtual;

          procedure g_stackframe_entry(list : paasmoutput;localsize : longint);

          { these methods can be overriden for extra functionality }

          { the following methods do nothing: }
          procedure g_interrupt_stackframe_entry(list : paasmoutput);virtual;
          procedure g_interrupt_stackframe_exit(list : paasmoutput);virtual;

          procedure g_profilecode(list : paasmoutput);virtual;
          procedure g_stackcheck(list : paasmoutput);virtual;

          { passing parameters, per default the parameter is pushed }
          { nr gives the number of the parameter (enumerated from   }
          { left to right), this allows to move the parameter to    }
          { register, if the cpu supports register calling          }
          { conventions                                             }
          procedure a_param_reg(list : paasmoutput;r : tregister;nr : longint);virtual;
       end;

  implementation

    procedure tcg.g_interrupt_stackframe_entry(list : paasmoutput);

      begin
      end;

    procedure tcg.g_interrupt_stackframe_exit(list : paasmoutput);

      begin
      end;

    procedure tcg.a_param_reg(list : paasmoutput;r : tregister;nr : longint);

      begin
         a_push_reg(list,r);
      end;

    procedure tcg.g_stackcheck(list : paasmoutput);

      begin
         a_param_reg(list,stackframe,1);
         a_call_name(list,'FPC_STACKCHECK',0,EXT_NEAR);
      end;

    procedure tcg.a_call_name_ext(list : paasmoutput;const s : string;
      offset : longint;m : texternaltyp);

      begin
         a_call_name(list,s,offset);
         concat_external(s,m);
      end;

    { generates the entry code for a procedure }
    procedure tcg.g_entrycode(list : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
       stackframe:longint;var parasize:longint;var nostackframe:boolean;
       inlined : boolean);

      var
         hs : string;
         hp : Pused_unit;
         unitinits,initcode : taasmoutput;
{$ifdef GDB}
         stab_function_name : Pai_stab_function_name;
{$endif GDB}
         hr : preference;

      begin
         { Align }
         if (not inlined) then
           begin
              { gprof uses 16 byte granularity !! }
              if (cs_profile in aktmoduleswitches) then
                list^.insert(new(pai_align,init_op(16,$90)))
              else
                if not(cs_littlesize in aktglobalswitches) then
                  list^.insert(new(pai_align,init(4)));
          end;
          curlist:=list;
          if (not inlined) and ((aktprocsym^.definition^.options and poproginit)<>0) then
            begin

              { needs the target a console flags ? }
              if tf_needs_isconsole in target_info.flags then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.symbol:=stringdup('U_'+target_info.system_unit+'_ISCONSOLE');
                   if apptype=at_cui then
                     a_load_const8_ref(list,1,hr);
                   else
                     a_load_const8_ref(list,0,hr);
                end;

              { Call the unit init procedures }
              unitinits.init;

              hp:=pused_unit(usedunits.first);
              while assigned(hp) do
                begin
                   { call the unit init code and make it external }
                   if (hp^.u^.flags and uf_init)<>0 then
                     a_call_name_ext(@unitinits,
                       'INIT$$'+hp^.u^.modulename^,0,EXT_NEAR);
                    hp:=Pused_unit(hp^.next);
                end;
              list^.insertlist(@unitinits);
              unitinits.done;
           end;

         { a constructor needs a help procedure }
         if (aktprocsym^.definition^.options and poconstructor)<>0 then
           begin
             if procinfo._class^.isclass then
               begin
                 list^.insert(new(pai_labeled,init(A_JZ,quickexitlabel)));
                 list^.insert(new(pai386,op_csymbol(A_CALL,S_NO,
                   newcsymbol('FPC_NEW_CLASS',0))));
                 concat_external('FPC_NEW_CLASS',EXT_NEAR);
               end
             else
               begin
                 list^.insert(new(pai_labeled,init(A_JZ,quickexitlabel)));
                 list^.insert(new(pai386,op_csymbol(A_CALL,S_NO,
                   newcsymbol('FPC_HELP_CONSTRUCTOR',0))));
                 list^.insert(new(pai386,op_const_reg(A_MOV,S_L,procinfo._class^.vmt_offset,R_EDI)));
                 concat_external('FPC_HELP_CONSTRUCTOR',EXT_NEAR);
               end;
           end;

  {$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) then
           list^.insert(new(pai_force_line,init));
  {$endif GDB}

         { save registers on cdecl }
         if ((aktprocsym^.definition^.options and pocdecl)<>0) then
           begin
              for r:=firstregister to lastregister do
                begin
                   if (r in registers_saved_on_cdecl) then
                     if (r in general_registers) then
                       begin
                          if (r in usedregisters) then
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
               if (aktprocsym^.definition^.options and (pounitinit or poproginit or pounitfinalize)<>0) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo.call_offset-4;
            end
          else
            begin
               if (aktprocsym^.definition^.options and (pounitinit or poproginit or pounitfinalize)<>0) then
                 parasize:=0
               else
                 parasize:=aktprocsym^.definition^.parast^.datasize+procinfo.call_offset-8;
               nostackframe:=false;

               if (aktprocsym^.definition^.options and pointerrupt)<>0 then
                 generate_interrupt_stackframe_entry;

               g_stackframe_entry(list,stackframe);

               if (cs_check_stack in aktlocalswitches) and
                 (target_info.flags in tf_supports_stack_check) then
                 g_stackcheck(@initcode);
            end;

         if cs_profile in aktmoduleswitches then
           g_profilecode(@initcode);

         { initialize return value }
         if is_ansistring(procinfo.retdef) or
           is_widestring(procinfo.retdef) then
           begin
              new(hr);
              reset_reference(hr^);
              hr^.offset:=procinfo.retoffset;
              hr^.base:=procinfo.framepointer;
              curlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,0,hr)));
           end;

         { generate copies of call by value parameters }
         if (aktprocsym^.definition^.options and poassembler=0) then
           begin
  {$ifndef VALUEPARA}
              aktprocsym^.definition^.parast^.foreach(copyopenarrays);
  {$else}
              aktprocsym^.definition^.parast^.foreach(copyvalueparas);
  {$endif}
           end;

         { initialisizes local data }
         aktprocsym^.definition^.localst^.foreach(initialize_data);
         { add a reference to all call by value/const parameters }
         aktprocsym^.definition^.parast^.foreach(incr_data);

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
                     list^.insert(new(pai_symbol,init_global(hs)))
                   else
                     list^.insert(new(pai_symbol,init(hs)));

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

        curlist:=nil;
    end;

end.
{
  $Log$
  Revision 1.2  1998-12-15 22:18:55  florian
    * some code added

  Revision 1.1  1998/12/15 16:32:58  florian
    + first version, derived from old routines

}

