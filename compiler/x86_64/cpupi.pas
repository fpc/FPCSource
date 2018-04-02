{
    Copyright (c) 2002-2006 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

interface

    uses
       globtype,
       psub,procinfo,aasmbase,aasmdata;

    type
       tcpuprocinfo = class(tcgprocinfo)
       private
         scopes: TAsmList;
         scopecount: longint;
         unwindflags: byte;
       public
         procedure set_first_temp_offset;override;
         procedure generate_parameter_info;override;
         function calc_stackframe_size:longint;override;
         procedure add_finally_scope(startlabel,endlabel,handler:TAsmSymbol;implicit:Boolean);
         procedure add_except_scope(trylabel,exceptlabel,endlabel,filter:TAsmSymbol);
         procedure dump_scopes(list:TAsmList);
         destructor destroy;override;
       end;

    function x86_64_use_ms_abi(proccall: tproccalloption): boolean;

implementation

    uses
      systems,
      globals,
      cutils,
      symconst,
      symtable,
      aasmtai,
      tgobj,
      fmodule;

    const
      SCOPE_FINALLY=0;
      SCOPE_CATCHALL=1;
      SCOPE_IMPLICIT=2;

    procedure tcpuprocinfo.set_first_temp_offset;
      begin
        if target_info.system=system_x86_64_win64 then
          begin
            { Fixes the case when there are calls done by low-level means
              (cg.a_call_name) but no child callnode }
            if (pi_do_call in flags) and
              not (po_nostackframe in procdef.procoptions) then
              allocate_push_parasize(32);

            if not(po_assembler in procdef.procoptions) and
              (tg.direction > 0) then
            { maxpushedparasize already contains 32 bytes of spilling area }
              tg.setfirsttemp(tg.direction*maxpushedparasize);
          end
        else
          tg.setfirsttemp(tg.direction*maxpushedparasize);
      end;


    procedure tcpuprocinfo.generate_parameter_info;
      begin
        inherited generate_parameter_info;
        if target_info.system=system_x86_64_win64 then
          para_stack_size:=0;
      end;


    function tcpuprocinfo.calc_stackframe_size:longint;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,16));
        { Note 1: when tg.direction>0, tg.lasttemp is already offset by maxpushedparasize
                  (because tg.setfirsttemp also sets lasttemp)
          Note 2: Align to 8 bytes here. The final 16-byte alignment is handled in
                  tcgx86.g_proc_entry, which considers saved rbp and the misalignment
                  caused by the call itself. }
        if (tg.direction>0) then
          result:=Align(tg.lasttemp,8)
        else
          result:=Align(tg.direction*tg.lasttemp+maxpushedparasize,8);
      end;

    procedure tcpuprocinfo.add_finally_scope(startlabel,endlabel,handler:TAsmSymbol;implicit:Boolean);
      begin
        unwindflags:=unwindflags or 2;
        if implicit then  { also needs catch functionality }
          unwindflags:=unwindflags or 1;
        inc(scopecount);
        if scopes=nil then
          scopes:=TAsmList.Create;

        if implicit then
          scopes.concat(tai_const.create_32bit(SCOPE_IMPLICIT))
        else
          scopes.concat(tai_const.create_32bit(SCOPE_FINALLY));
        scopes.concat(tai_const.create_rva_sym(startlabel));
        scopes.concat(tai_const.create_rva_sym(endlabel));
        scopes.concat(tai_const.create_rva_sym(handler));
      end;

    procedure tcpuprocinfo.add_except_scope(trylabel,exceptlabel,endlabel,filter:TAsmSymbol);
      begin
        unwindflags:=unwindflags or 3;
        inc(scopecount);
        if scopes=nil then
          scopes:=TAsmList.Create;

        if Assigned(filter) then
          scopes.concat(tai_const.create_rva_sym(filter))
        else
          scopes.concat(tai_const.create_32bit(SCOPE_CATCHALL));
        scopes.concat(tai_const.create_rva_sym(trylabel));
        scopes.concat(tai_const.create_rva_sym(exceptlabel));
        scopes.concat(tai_const.create_rva_sym(endlabel));
      end;

    procedure tcpuprocinfo.dump_scopes(list: TAsmList);
      var
        hdir: tai_seh_directive;
      begin
        if (scopecount=0) then
          exit;
        hdir:=cai_seh_directive.create_name(ash_handler,'__FPC_specific_handler');
        if not systemunit.iscurrentunit then
          current_module.add_extern_asmsym('__FPC_specific_handler',AB_EXTERNAL,AT_FUNCTION);
        hdir.data.flags:=unwindflags;
        list.concat(hdir);
        list.concat(cai_seh_directive.create(ash_handlerdata));
        list.concat(tai_const.create_32bit(scopecount));
        list.concatlist(scopes);
        { return to text, required for GAS compatibility }
        { This creates a tai_align which is redundant here (although harmless) }
        new_section(list,sec_code,lower(procdef.mangledname),0);
      end;

    destructor tcpuprocinfo.destroy;
      begin
        scopes.free;
        inherited destroy;
      end;


    function x86_64_use_ms_abi(proccall: tproccalloption): boolean;
      begin
        result:=
          ((target_info.system=system_x86_64_win64) and
            not(proccall in [pocall_sysv_abi_default,pocall_sysv_abi_cdecl])) or
          (proccall in [pocall_ms_abi_default,pocall_ms_abi_cdecl]);
      end;


begin
   cprocinfo:=tcpuprocinfo;
end.
