{
    $Id$
    Copyright (c) 1998 by Peter Vreman

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
interface

procedure HandleSwitch(switch,state:char);
function CheckSwitch(switch,state:char):boolean;


implementation
uses globals,verbose,files,systems;

{****************************************************************************
                          Main Switches Parsing
****************************************************************************}

type
  TSwitchType=(localsw,modulesw,globalsw,illegalsw,unsupportedsw);
  SwitchRec=record
    typesw : TSwitchType;
    setsw  : byte;
  end;
const
  SwitchTable:array['A'..'Z'] of SwitchRec=(
   {A} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {B} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {C} (typesw:localsw; setsw:ord(cs_do_assertion)),
   {D} (typesw:modulesw; setsw:ord(cs_debuginfo)),
   {E} (typesw:globalsw; setsw:ord(cs_fp_emulation)),
   {F} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {G} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {H} (typesw:localsw; setsw:ord(cs_ansistrings)),
   {I} (typesw:localsw; setsw:ord(cs_check_io)),
   {J} (typesw:illegalsw; setsw:ord(cs_localnone)),
   {K} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {L} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {M} (typesw:localsw; setsw:ord(cs_generate_rtti)),
   {N} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {O} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {P} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {Q} (typesw:localsw; setsw:ord(cs_check_overflow)),
   {R} (typesw:localsw; setsw:ord(cs_check_range)),
   {S} (typesw:localsw; setsw:ord(cs_check_stack)),
   {T} (typesw:localsw; setsw:ord(cs_typed_addresses)),
   {U} (typesw:illegalsw; setsw:ord(cs_localnone)),
   {V} (typesw:localsw; setsw:ord(cs_strict_var_strings)),
   {W} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {X} (typesw:modulesw; setsw:ord(cs_extsyntax)),
   {Y} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {Z} (typesw:illegalsw; setsw:ord(cs_localnone))
    );

procedure HandleSwitch(switch,state:char);
begin
  switch:=upcase(switch);
{ Is the Switch in the letters ? }
  if not ((switch in ['A'..'Z']) and (state in ['-','+'])) then
   begin
     Message(scan_w_illegal_switch);
     exit;
   end;
{ Handle the switch }
  with SwitchTable[switch] do
   begin
     case typesw of
       illegalsw : Message1(scan_w_illegal_switch,'$'+switch);
   unsupportedsw : Message1(scan_w_unsupported_switch,'$'+switch);
       localsw : begin
                   if state='+' then
                    aktlocalswitches:=aktlocalswitches+[tlocalswitch(setsw)]
                   else
                    aktlocalswitches:=aktlocalswitches-[tlocalswitch(setsw)];
                 end;
      modulesw : begin
                   if current_module^.in_main then
                    begin
                      if state='+' then
                       aktmoduleswitches:=aktmoduleswitches+[tmoduleswitch(setsw)]
                      else
                       aktmoduleswitches:=aktmoduleswitches-[tmoduleswitch(setsw)];
                    end
                   else
                    Message(scan_w_switch_is_global);
                 end;
      globalsw : begin
                   if current_module^.in_main and (current_module=main_module) then
                    begin
                      if state='+' then
                       aktglobalswitches:=aktglobalswitches+[tglobalswitch(setsw)]
                      else
                       aktglobalswitches:=aktglobalswitches-[tglobalswitch(setsw)];
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
begin
  switch:=upcase(switch);
{ Is the Switch in the letters ? }
  if not ((switch in ['A'..'Z']) and (state in ['-','+'])) then
   begin
     Message(scan_w_illegal_switch);
     CheckSwitch:=false;
     exit;
   end;
{ Check the switch }
  with SwitchTable[switch] do
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
{
  $Log$
  Revision 1.8  1998-08-10 14:50:27  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.7  1998/07/24 22:17:00  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.6  1998/07/18 17:11:13  florian
    + ansi string constants fixed
    + switch $H partial implemented

  Revision 1.5  1998/06/04 23:52:00  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.4  1998/05/21 19:33:36  peter
    + better procedure directive handling and only one table

  Revision 1.3  1998/05/01 07:43:56  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.2  1998/04/28 11:45:53  florian
    * make it compilable with TP
    + small COM problems solved to compile classes.pp

  Revision 1.1  1998/04/27 23:13:53  peter
    + the new files for the scanner

}
