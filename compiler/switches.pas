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
                       Special functions for some switches
****************************************************************************}

{$ifndef FPC}
  {$F+}
{$endif}

procedure sw_stackcheck;
begin
{$ifdef i386}
  if target_info.target=target_Linux then
   Message(scan_n_stack_check_global_under_linux);
{$endif}

end;

{$ifndef FPC}
  {$F-}
{$endif}

{****************************************************************************
                          Main Switches Parsing
****************************************************************************}

type
  TSwitchType=(local,unitglobal,programglobal,illegal,unsupported);
  SwitchRec=record
    typesw : TSwitchType;
    setsw  : tcswitch;
    proc   : procedure;
  end;
const
  SwitchTable:array['A'..'Z'] of SwitchRec=(
   {A} (typesw:unsupported; setsw:cs_none; proc:nil),
   {B} (typesw:unsupported; setsw:cs_none; proc:nil),
   {C} (typesw:illegal; setsw:cs_none; proc:nil),
   {D} (typesw:unitglobal; setsw:cs_debuginfo; proc:nil),
   {E} (typesw:programglobal; setsw:cs_fp_emulation; proc:nil),
   {F} (typesw:unsupported; setsw:cs_none; proc:nil),
   {G} (typesw:unsupported; setsw:cs_none; proc:nil),
   {H} (typesw:local; setsw:cs_ansistrings; proc:nil),
   {I} (typesw:local; setsw:cs_iocheck; proc:nil),
   {J} (typesw:illegal; setsw:cs_none; proc:nil),
   {K} (typesw:unsupported; setsw:cs_none; proc:nil),
   {L} (typesw:unsupported; setsw:cs_none; proc:nil),
   {M} (typesw:local; setsw:cs_generate_rtti; proc:nil),
   {N} (typesw:unsupported; setsw:cs_none; proc:nil),
   {O} (typesw:unsupported; setsw:cs_none; proc:nil),
   {P} (typesw:unsupported; setsw:cs_none; proc:nil),
   {Q} (typesw:local; setsw:cs_check_overflow; proc:nil),
   {R} (typesw:local; setsw:cs_rangechecking; proc:nil),
   {S} (typesw:local; setsw:cs_check_stack; proc:nil),
   {T} (typesw:local; setsw:cs_typed_addresses; proc:nil),
   {U} (typesw:illegal; setsw:cs_none; proc:nil),
   {V} (typesw:local; setsw:cs_strict_var_strings; proc:nil),
   {W} (typesw:unsupported; setsw:cs_none; proc:nil),
   {X} (typesw:unitglobal; setsw:cs_extsyntax; proc:nil),
   {Y} (typesw:unsupported; setsw:cs_none; proc:nil),
   {Z} (typesw:illegal; setsw:cs_none; proc:nil)
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
       illegal : Message1(scan_w_illegal_switch,'$'+switch);
   unsupported : Message1(scan_w_unsupported_switch,'$'+switch);
    unitglobal,
 programglobal,
         local : begin
                   if (typesw=local) or
                      ((typesw=unitglobal) and current_module^.in_main) or
                      ((typesw=programglobal) and current_module^.in_main and (current_module=main_module)) then
                    begin
                      if state='+' then
                       aktswitches:=aktswitches+[setsw]
                      else
                       aktswitches:=aktswitches-[setsw];
                    end
                   else
                    Message(scan_w_switch_is_global);

                    {$ifdef FPC}
                    if assigned(proc) then
                      proc();
                    {$else}
                    if @proc<>nil then
                      proc;
                    {$endif}
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
     found:=(setsw in aktswitches);
     if state='-' then
      found:=not found;
     CheckSwitch:=found;
   end;
end;


end.
{
  $Log$
  Revision 1.6  1998-07-18 17:11:13  florian
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
