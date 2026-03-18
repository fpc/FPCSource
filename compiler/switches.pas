{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements the parsing of the switches like $I-

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
unit switches;

{$i fpcdefs.inc}

interface

uses
  systems,globtype,compilerbase;

procedure HandleSwitch(switch,state:char);
function CheckSwitch(switch,state:char):boolean;

procedure recordpendingverbosityswitch(sw: char; state: char);
procedure recordpendingmessagestate(msg: longint; state: tmsgstate);
procedure recordpendinglocalswitch(sw: tlocalswitch; state: char);
procedure recordpendinglocalfullswitch(const switches: tlocalswitches);
procedure recordpendingverbosityfullswitch(verbosity: longint);
procedure recordpendingcallingswitch(const str: shortstring);
procedure recordpendingalignmentfullswitch(const alignment : talignmentinfo);
procedure recordpendingsetalloc(alloc:shortint);
procedure recordpendingpackenum(size:shortint);
procedure recordpendingpackrecords(size:shortint);
procedure recordpendingasmmode(asmmode:tasmmode);
procedure recordpendingoptimizerswitches(optimizerswitches:toptimizerswitches);
procedure flushpendingswitchesstate;

implementation
uses
  cpuinfo,
{$ifdef llvm}
  { override optimizer switches }
  llvminfo,
{$endif llvm}
  globals,verbose,comphook,dirparse,cclasses,compiler,
  fmodule;

{****************************************************************************
                          Main Switches Parsing
****************************************************************************}

type
  TSwitchType=(ignoredsw,localsw,modulesw,globalsw,illegalsw,unsupportedsw,alignsw,optimizersw,packenumsw,pentiumfdivsw,targetsw);
  SwitchRec=record
    typesw : TSwitchType;
    setsw  : byte;
  end;
  SwitchRecTable = array['A'..'Z'] of SwitchRec;

const
  turboSwitchTable: SwitchRecTable =(
   {A} (typesw:alignsw; setsw:ord(cs_localnone)),
   {B} (typesw:localsw; setsw:ord(cs_full_boolean_eval)),
   {C} (typesw:localsw; setsw:ord(cs_do_assertion)),
   {D} (typesw:modulesw; setsw:ord(cs_debuginfo)),
   {E} (typesw:modulesw; setsw:ord(cs_fp_emulation)),
{$ifdef i8086}
   {F} (typesw:localsw; setsw:ord(cs_force_far_calls)),
{$else i8086}
   {F} (typesw:ignoredsw; setsw:ord(cs_localnone)),
{$endif i8086}
   {G} (typesw:localsw; setsw:ord(cs_imported_data)),
   {H} (typesw:localsw; setsw:ord(cs_refcountedstrings)),
   {I} (typesw:localsw; setsw:ord(cs_check_io)),
   {J} (typesw:localsw; setsw:ord(cs_typed_const_writable)),
{$ifdef i8086}
   {K} (typesw:modulesw; setsw:ord(cs_win16_smartcallbacks)),
{$else i8086}
   {K} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
{$endif i8086}
   {L} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {M} (typesw:localsw; setsw:ord(cs_generate_rtti)),
   {N} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {O} (typesw:optimizersw; setsw:ord(cs_opt_level2)),
   {P} (typesw:localsw; setsw:ord(cs_openstring)),
   {Q} (typesw:localsw; setsw:ord(cs_check_overflow)),
   {R} (typesw:localsw; setsw:ord(cs_check_range)),
   {S} (typesw:localsw; setsw:ord(cs_check_stack)),
   {T} (typesw:localsw; setsw:ord(cs_typed_addresses)),
   {U} (typesw:pentiumfdivsw; setsw:ord(cs_localnone)),
   {V} (typesw:localsw; setsw:ord(cs_strict_var_strings)),
{$ifdef i8086}
   {W} (typesw:targetsw; setsw:ord(ts_x86_far_procs_push_odd_bp)),
{$else i8086}
   {W} (typesw:localsw; setsw:ord(cs_generate_stackframes)),
{$endif i8086}
   {X} (typesw:modulesw; setsw:ord(cs_extsyntax)),
   {Y} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {Z} (typesw:packenumsw; setsw:ord(cs_localnone))
    );


  macSwitchTable: SwitchRecTable =(
   {A} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {B} (typesw:localsw; setsw:ord(cs_full_boolean_eval)),
   {C} (typesw:localsw; setsw:ord(cs_do_assertion)),
   {D} (typesw:modulesw; setsw:ord(cs_debuginfo)),
   {E} (typesw:modulesw; setsw:ord(cs_fp_emulation)),
{$ifdef i8086}
   {F} (typesw:localsw; setsw:ord(cs_force_far_calls)),
{$else i8086}
   {F} (typesw:ignoredsw; setsw:ord(cs_localnone)),
{$endif i8086}
   {G} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {H} (typesw:localsw; setsw:ord(cs_refcountedstrings)),
   {I} (typesw:localsw; setsw:ord(cs_check_io)),
   {J} (typesw:localsw; setsw:ord(cs_external_var)),
{$ifdef i8086}
   {K} (typesw:modulesw; setsw:ord(cs_win16_smartcallbacks)),
{$else i8086}
   {K} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
{$endif i8086}
   {L} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {M} (typesw:localsw; setsw:ord(cs_generate_rtti)),
   {N} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {O} (typesw:optimizersw; setsw:ord(cs_opt_level2)),
   {P} (typesw:localsw; setsw:ord(cs_openstring)),
   {Q} (typesw:localsw; setsw:ord(cs_check_overflow)),
   {R} (typesw:localsw; setsw:ord(cs_check_range)),
   {S} (typesw:localsw; setsw:ord(cs_check_stack)),
   {T} (typesw:localsw; setsw:ord(cs_typed_addresses)),
   {U} (typesw:illegalsw; setsw:ord(cs_localnone)),
   {V} (typesw:localsw; setsw:ord(cs_strict_var_strings)),
{$ifdef i8086}
   {W} (typesw:targetsw; setsw:ord(ts_x86_far_procs_push_odd_bp)),
{$else i8086}
   {W} (typesw:localsw; setsw:ord(cs_generate_stackframes)),
{$endif i8086}
   {X} (typesw:modulesw; setsw:ord(cs_extsyntax)),
   {Y} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {Z} (typesw:localsw; setsw:ord(cs_externally_visible))
    );

procedure HandleSwitch(switch,state:char);
var
  compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }

var
  switchTablePtr: ^SwitchRecTable;

begin
  switch:=upcase(switch);
{ Is the Switch in the letters ? }
  if not ((switch in ['A'..'Z']) and (state in ['-','+'])) then
   begin
     compiler.verbose.Message(scan_w_illegal_switch);
     exit;
   end;

{ Select switch table }
  if m_mac in current_settings.modeswitches  then
    switchTablePtr:= @macSwitchTable
  else
    switchTablePtr:= @turboSwitchTable;

{ Handle the switch }
   with switchTablePtr^[switch] do
   begin
     case typesw of
       alignsw:
         begin
           if state='+' then
             recordpendingpackrecords(4)
           else
             recordpendingpackrecords(1);
         end;
       optimizersw :
         begin
           if state='+' then
             current_settings.optimizerswitches:=level2optimizerswitches
           else
             current_settings.optimizerswitches:=[];
         end;
       ignoredsw :
         compiler.verbose.Message1(scan_n_ignored_switch,'$'+switch);
       illegalsw :
         compiler.verbose.Message1(scan_w_illegal_switch,'$'+switch);
       unsupportedsw :
         compiler.verbose.Message1(scan_w_unsupported_switch,'$'+switch);
       localsw :
         recordpendinglocalswitch(tlocalswitch(setsw),state);
       modulesw :
         begin
           if current_module.in_global then
            begin
{$ifndef cpufpemu}
              if tmoduleswitch(setsw)=cs_fp_emulation then
                begin
                  compiler.verbose.Message1(scan_w_unsupported_switch_by_target,'$'+switch);
                end
              else
{$endif cpufpemu}
                begin
                  if state='+' then
                    include(current_settings.moduleswitches,tmoduleswitch(setsw))
                  else
                    begin
                      { Turning off debuginfo when lineinfo is requested
                        is not possible }
                      if not((cs_use_lineinfo in current_settings.globalswitches) and
                             (tmoduleswitch(setsw)=cs_debuginfo)) then
                        exclude(current_settings.moduleswitches,tmoduleswitch(setsw));
                    end;
                end;
            end
           else
            compiler.verbose.Message(scan_w_switch_is_global);
         end;
       globalsw :
         begin
           if current_module.in_global and (current_module=main_module) then
            begin
              if state='+' then
               include(current_settings.globalswitches,tglobalswitch(setsw))
              else
               exclude(current_settings.globalswitches,tglobalswitch(setsw));
            end
           else
            compiler.verbose.Message(scan_w_switch_is_global);
         end;
       packenumsw:
         begin
           if state='-' then
             current_settings.packenum:=1
           else
             current_settings.packenum:=4;
         end;
       pentiumfdivsw:
         begin
           { Switch u- means pentium-safe fdiv off -> fpc default. We don't }
           { support u+                                                     }
           if state='+' then
             compiler.verbose.Message1(scan_w_unsupported_switch,'$'+switch);
         end;
       targetsw:
         UpdateTargetSwitchStr(TargetSwitchStr[ttargetswitch(setsw)].name+state,current_settings.targetswitches,current_module.in_global);
     end;
   end;
end;


function CheckSwitch(switch,state:char):boolean;
var
  compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }

var
  found : boolean;
  switchTablePtr: ^SwitchRecTable;

begin
  switch:=upcase(switch);
{ Is the Switch in the letters ? }
  if not ((switch in ['A'..'Z']) and (state in ['-','+'])) then
   begin
     compiler.verbose.Message(scan_w_illegal_switch);
     CheckSwitch:=false;
     exit;
   end;

{ Select switch table }
  if m_mac in current_settings.modeswitches then
    switchTablePtr:= @macSwitchTable
  else
    switchTablePtr:= @turboSwitchTable;

{ Check the switch }
   with switchTablePtr^[switch] do
   begin
     case typesw of
      localsw : found:=(tlocalswitch(setsw) in current_settings.localswitches);
     modulesw : found:=(tmoduleswitch(setsw) in current_settings.moduleswitches);
     globalsw : found:=(tglobalswitch(setsw) in current_settings.globalswitches);
     packenumsw : found := (current_settings.packenum = 4);
     else
      found:=false;
     end;
     if state='-' then
      found:=not found;
     CheckSwitch:=found;
   end;
end;


procedure recordpendingverbosityswitch(sw: char; state: char);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextverbositystr:=compiler.globals.pendingstate.nextverbositystr+sw+state;
  end;


procedure recordpendingmessagestate(msg: longint; state: tmsgstate);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  var
    pstate : pmessagestaterecord;
  begin
    new(pstate);
    {$IFDEF DEBUG_MESSAGESTATE}
    pstate^.owner:=current_module; { nil for global option }
    {$ENDIF}
    pstate^.next:=compiler.globals.pendingstate.nextmessagerecord;
    pstate^.value:=msg;
    pstate^.state:=state;
    compiler.globals.pendingstate.nextmessagerecord:=pstate;
  end;


procedure recordpendinglocalswitch(sw: tlocalswitch; state: char);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    if not (psf_local_switches_changed in compiler.globals.pendingstate.flags) then
       compiler.globals.pendingstate.nextlocalswitches:=current_settings.localswitches;
    if state='-' then
      exclude(compiler.globals.pendingstate.nextlocalswitches,sw)
    else if state='+' then
      include(compiler.globals.pendingstate.nextlocalswitches,sw)
    else { state = '*' }
      begin
        if sw in init_settings.localswitches then
         include(compiler.globals.pendingstate.nextlocalswitches,sw)
        else
         exclude(compiler.globals.pendingstate.nextlocalswitches,sw);
      end;
    include(compiler.globals.pendingstate.flags,psf_local_switches_changed);
  end;


procedure recordpendingalignmentfullswitch(const alignment : talignmentinfo);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextalignment:=alignment;
    include(compiler.globals.pendingstate.flags,psf_alignment_changed);
  end;


procedure recordpendinglocalfullswitch(const switches: tlocalswitches);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextlocalswitches:=switches;
    include(compiler.globals.pendingstate.flags,psf_local_switches_changed);
  end;


procedure recordpendingverbosityfullswitch(verbosity: longint);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextverbositystr:='';
    compiler.globals.pendingstate.nextverbosityfullswitch:=verbosity;
    include(compiler.globals.pendingstate.flags,psf_verbosity_full_switched);
  end;

procedure recordpendingcallingswitch(const str: shortstring);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextcallingstr:=str;
  end;


procedure recordpendingsetalloc(alloc:shortint);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextsetalloc:=alloc;
    include(compiler.globals.pendingstate.flags,psf_setalloc_changed);
  end;


procedure recordpendingasmmode(asmmode:tasmmode);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextasmmode:=asmmode;
    include(compiler.globals.pendingstate.flags,psf_asmmode_changed);
  end;


procedure recordpendingpackenum(size:shortint);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextpackenum:=size;
    include(compiler.globals.pendingstate.flags,psf_packenum_changed);
  end;


procedure recordpendingpackrecords(size:shortint);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextpackrecords:=size;
    include(compiler.globals.pendingstate.flags,psf_packrecords_changed);
  end;


procedure recordpendingoptimizerswitches(optimizerswitches:toptimizerswitches);
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  begin
    compiler.globals.pendingstate.nextoptimizerswitches:=optimizerswitches;
    include(compiler.globals.pendingstate.flags,psf_optimizerswitches_changed);
  end;


procedure flushpendingswitchesstate;
  var
    compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
  var
    tmpproccal: tproccalloption;
    fstate, pstate : pmessagestaterecord;
    msgset : thashset;
    msgfound : boolean;
  begin
    { process pending localswitches (range checking, etc) }
    if psf_local_switches_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.localswitches:=compiler.globals.pendingstate.nextlocalswitches;
        exclude(compiler.globals.pendingstate.flags,psf_local_switches_changed);
      end;
    { process pending verbosity changes (warnings on, etc) }
    if psf_verbosity_full_switched in compiler.globals.pendingstate.flags then
      begin
        status.verbosity:=compiler.globals.pendingstate.nextverbosityfullswitch;
        exclude(compiler.globals.pendingstate.flags,psf_verbosity_full_switched);
      end;
    if psf_alignment_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.alignment:=compiler.globals.pendingstate.nextalignment;
        exclude(compiler.globals.pendingstate.flags,psf_alignment_changed);
      end;
    if psf_packenum_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.packenum:=compiler.globals.pendingstate.nextpackenum;
        exclude(compiler.globals.pendingstate.flags,psf_packenum_changed);
      end;
    if psf_packrecords_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.packrecords:=compiler.globals.pendingstate.nextpackrecords;
        exclude(compiler.globals.pendingstate.flags,psf_packrecords_changed);
      end;
    if psf_setalloc_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.setalloc:=compiler.globals.pendingstate.nextsetalloc;
        exclude(compiler.globals.pendingstate.flags,psf_setalloc_changed);
      end;
    if psf_asmmode_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.asmmode:=compiler.globals.pendingstate.nextasmmode;
        exclude(compiler.globals.pendingstate.flags,psf_asmmode_changed);
      end;
    if psf_optimizerswitches_changed in compiler.globals.pendingstate.flags then
      begin
        current_settings.optimizerswitches:=compiler.globals.pendingstate.nextoptimizerswitches;
        exclude(compiler.globals.pendingstate.flags,psf_optimizerswitches_changed);
      end;
    { process pending verbosity changes (warnings on, etc) }
    if compiler.globals.pendingstate.nextverbositystr<>'' then
      begin
        compiler.verbose.setverbosity(compiler.globals.pendingstate.nextverbositystr);
        compiler.globals.pendingstate.nextverbositystr:='';
      end;
    msgset:=thashset.create(10,false,false);
    { we need to start from a clean slate }
    if not assigned(current_settings.pmessage) then
      compiler.verbose.RestoreLocalVerbosity(nil);
    fstate:=compiler.globals.pendingstate.nextmessagerecord;
    pstate:=compiler.globals.pendingstate.nextmessagerecord;
    while assigned(pstate) do
      begin
        {$IFDEF DEBUG_MESSAGESTATE}
        if assigned(pstate^.owner) and (pstate^.owner<>current_module) then
          begin
            writeln('flushpendingswitchesstate cur: ',current_module.modulename^,' ',current_module.statestr);
            writeln('flushpendingswitchesstate pstate: ',tmodule(pstate^.owner).modulename^,' ',tmodule(pstate^.owner).statestr);
            Internalerror(2026030701);
          end;
        {$ENDIF}
        compiler.globals.pendingstate.nextmessagerecord:=pstate^.next;
        { the message records are ordered newest to oldest, so only apply the newest change }
        msgfound:=false;
        if not assigned(msgset.findoradd(@pstate^.value,sizeof(pstate^.value),msgfound)) or
            not msgfound then
          compiler.verbose.SetMessageVerbosity(pstate^.value,pstate^.state);
        if not assigned(pstate^.next) then
          begin
            pstate^.next:=current_settings.pmessage;
            current_settings.pmessage:=fstate;
            pstate:=nil;
          end
        else
          pstate:=pstate^.next;
        compiler.globals.pendingstate.nextmessagerecord:=nil;
      end;
    msgset.free;
    msgset := nil;
    { process pending calling convention changes (calling x) }
    if compiler.globals.pendingstate.nextcallingstr<>'' then
      begin
        if not SetAktProcCall(compiler.globals.pendingstate.nextcallingstr,tmpproccal) then
          compiler.verbose.Message1(parser_w_unknown_proc_directive_ignored,compiler.globals.pendingstate.nextcallingstr)
        else if not(tmpproccal in supported_calling_conventions) then
          compiler.verbose.Message1(parser_e_illegal_calling_convention,compiler.globals.pendingstate.nextcallingstr)
        else
          current_settings.defproccall:=tmpproccal;
        compiler.globals.pendingstate.nextcallingstr:='';
      end;
  end;


end.
