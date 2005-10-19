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

procedure HandleSwitch(switch,state:char);
function CheckSwitch(switch,state:char):boolean;


implementation
uses
  globtype,systems,
  globals,verbose,fmodule;

{****************************************************************************
                          Main Switches Parsing
****************************************************************************}

type
  TSwitchType=(ignoredsw,localsw,modulesw,globalsw,illegalsw,unsupportedsw,alignsw);
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
   {F} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {G} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {H} (typesw:localsw; setsw:ord(cs_ansistrings)),
   {I} (typesw:localsw; setsw:ord(cs_check_io)),
   {J} (typesw:localsw; setsw:ord(cs_typed_const_writable)),
   {K} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {L} (typesw:modulesw; setsw:ord(cs_local_browser)),
   {M} (typesw:localsw; setsw:ord(cs_generate_rtti)),
   {N} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {O} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {P} (typesw:modulesw; setsw:ord(cs_openstring)),
   {Q} (typesw:localsw; setsw:ord(cs_check_overflow)),
   {R} (typesw:localsw; setsw:ord(cs_check_range)),
   {S} (typesw:localsw; setsw:ord(cs_check_stack)),
   {T} (typesw:localsw; setsw:ord(cs_typed_addresses)),
   {U} (typesw:illegalsw; setsw:ord(cs_localnone)),
   {V} (typesw:localsw; setsw:ord(cs_strict_var_strings)),
   {W} (typesw:localsw; setsw:ord(cs_generate_stackframes)),
   {X} (typesw:modulesw; setsw:ord(cs_extsyntax)),
   {Y} (typesw:modulesw; setsw:ord(cs_browser)),
   {Z} (typesw:illegalsw; setsw:ord(cs_localnone))
    );


  macSwitchTable: SwitchRecTable =(
   {A} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {B} (typesw:localsw; setsw:ord(cs_full_boolean_eval)),
   {C} (typesw:localsw; setsw:ord(cs_do_assertion)),
   {D} (typesw:modulesw; setsw:ord(cs_debuginfo)),
   {E} (typesw:modulesw; setsw:ord(cs_fp_emulation)),
   {F} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {G} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {H} (typesw:localsw; setsw:ord(cs_ansistrings)),
   {I} (typesw:localsw; setsw:ord(cs_check_io)),
   {J} (typesw:localsw; setsw:ord(cs_external_var)),
   {K} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {L} (typesw:modulesw; setsw:ord(cs_local_browser)),
   {M} (typesw:localsw; setsw:ord(cs_generate_rtti)),
   {N} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {O} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {P} (typesw:modulesw; setsw:ord(cs_openstring)),
   {Q} (typesw:localsw; setsw:ord(cs_check_overflow)),
   {R} (typesw:localsw; setsw:ord(cs_check_range)),
   {S} (typesw:localsw; setsw:ord(cs_check_stack)),
   {T} (typesw:localsw; setsw:ord(cs_typed_addresses)),
   {U} (typesw:illegalsw; setsw:ord(cs_localnone)),
   {V} (typesw:localsw; setsw:ord(cs_strict_var_strings)),
   {W} (typesw:localsw; setsw:ord(cs_generate_stackframes)),
   {X} (typesw:modulesw; setsw:ord(cs_extsyntax)),
   {Y} (typesw:modulesw; setsw:ord(cs_browser)),
   {Z} (typesw:localsw; setsw:ord(cs_externally_visible))
    );

procedure HandleSwitch(switch,state:char);

var
  switchTablePtr: ^SwitchRecTable;

begin
  switch:=upcase(switch);
{ Is the Switch in the letters ? }
  if not ((switch in ['A'..'Z']) and (state in ['-','+'])) then
   begin
     Message(scan_w_illegal_switch);
     exit;
   end;

{ Select switch table }
  if m_mac in aktmodeswitches  then
    switchTablePtr:= @macSwitchTable
  else
    switchTablePtr:= @turboSwitchTable;

{ Handle the switch }
   with switchTablePtr^[switch] do
   begin
     case typesw of
       alignsw:
         if state='+' then
           aktpackrecords:=4
         else
           aktpackrecords:=1;
       ignoredsw :
         Message1(scan_n_ignored_switch,'$'+switch);
       illegalsw :
         Message1(scan_w_illegal_switch,'$'+switch);
       unsupportedsw :
         Message1(scan_w_unsupported_switch,'$'+switch);
       localsw :
         begin
           if not localswitcheschanged then
             nextaktlocalswitches:=aktlocalswitches;
           if state='+' then
            include(nextaktlocalswitches,tlocalswitch(setsw))
           else
            exclude(nextaktlocalswitches,tlocalswitch(setsw));
           localswitcheschanged:=true;
         end;
       modulesw :
         begin
           if current_module.in_global then
            begin
              if state='+' then
                include(aktmoduleswitches,tmoduleswitch(setsw))
              else
                begin
                  { Turning off debuginfo when lineinfo is requested
                    is not possible }
                  if not((cs_gdb_lineinfo in aktglobalswitches) and
                         (tmoduleswitch(setsw)=cs_debuginfo)) then
                    exclude(aktmoduleswitches,tmoduleswitch(setsw));
                end;
            end
           else
            Message(scan_w_switch_is_global);
         end;
       globalsw :
         begin
           if current_module.in_global and (current_module=main_module) then
            begin
              if state='+' then
               include(aktglobalswitches,tglobalswitch(setsw))
              else
               exclude(aktglobalswitches,tglobalswitch(setsw));
            end
           else
            Message(scan_w_switch_is_global);
         end;
      end;
   end;
end;


function CheckSwitch(switch,state:char):boolean;

var
  found : boolean;
  switchTablePtr: ^SwitchRecTable;

begin
  switch:=upcase(switch);
{ Is the Switch in the letters ? }
  if not ((switch in ['A'..'Z']) and (state in ['-','+'])) then
   begin
     Message(scan_w_illegal_switch);
     CheckSwitch:=false;
     exit;
   end;

{ Select switch table }
  if m_mac in aktmodeswitches then
    switchTablePtr:= @macSwitchTable
  else
    switchTablePtr:= @turboSwitchTable;

{ Check the switch }
   with switchTablePtr^[switch] do
   begin
     case typesw of
      localsw : found:=(tlocalswitch(setsw) in aktlocalswitches);
     modulesw : found:=(tmoduleswitch(setsw) in aktmoduleswitches);
     globalsw : found:=(tglobalswitch(setsw) in aktglobalswitches);
     else
      found:=false;
     end;
     if state='-' then
      found:=not found;
     CheckSwitch:=found;
   end;
end;


end.
