{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
uses
  globtype,systems,
  globals,verbose,files;

{****************************************************************************
                          Main Switches Parsing
****************************************************************************}

type
  TSwitchType=(ignoredsw,localsw,modulesw,globalsw,illegalsw,unsupportedsw);
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
   {F} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {G} (typesw:ignoredsw; setsw:ord(cs_localnone)),
   {H} (typesw:localsw; setsw:ord(cs_ansistrings)),
   {I} (typesw:localsw; setsw:ord(cs_check_io)),
   {J} (typesw:unsupportedsw; setsw:ord(cs_typed_const_not_changeable)),
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
   {W} (typesw:unsupportedsw; setsw:ord(cs_localnone)),
   {X} (typesw:modulesw; setsw:ord(cs_extsyntax)),
   {Y} (typesw:modulesw; setsw:ord(cs_browser)),
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
     ignoredsw : Message1(scan_n_ignored_switch,'$'+switch);
     illegalsw : Message1(scan_w_illegal_switch,'$'+switch);
 unsupportedsw : Message1(scan_w_unsupported_switch,'$'+switch);
       localsw : begin
                   if state='+' then
                    aktlocalswitches:=aktlocalswitches+[tlocalswitch(setsw)]
                   else
                    aktlocalswitches:=aktlocalswitches-[tlocalswitch(setsw)];
                 { Message for linux which has global checking only }
                   if (switch='S') and (
{$ifdef i386}
                      (target_info.target = target_i386_linux)
{$else}
{$ifdef m68k}
                      (target_info.target = target_m68k_linux)
{$else}
                       True
{$endif m68k}
{$endif i386}
                       ) then
                       Message(scan_n_stack_check_global_under_linux);
                 end;
      modulesw : begin
                   if current_module^.in_global then
                    begin
                      if state='+' then
                        aktmoduleswitches:=aktmoduleswitches+[tmoduleswitch(setsw)]
                      else
                        aktmoduleswitches:=aktmoduleswitches-[tmoduleswitch(setsw)];
                      { can't have local browser when no global browser
                        moved to end of global section
                      if (cs_local_browser in aktmoduleswitches) and
                         not(cs_browser in aktmoduleswitches) then
                        aktmoduleswitches:=aktmoduleswitches-[cs_local_browser];}
                    end
                   else
                    Message(scan_w_switch_is_global);
                 end;
      globalsw : begin
                   if current_module^.in_global and (current_module=main_module) then
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
  Revision 1.25  2000-01-07 01:14:39  peter
    * updated copyright to 2000

  Revision 1.24  1999/11/06 14:34:26  peter
    * truncated log to 20 revs

  Revision 1.23  1999/09/16 11:34:58  pierre
   * typo correction

  Revision 1.22  1999/08/01 23:35:06  michael
  * Alpha changes

  Revision 1.21  1999/04/25 22:34:59  pierre
   + cs_typed_const_not_changeable added but not implemented yet !

  Revision 1.20  1999/02/22 13:07:06  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.19  1999/02/18 13:43:20  peter
    * no localbrowser when browser is turned off

  Revision 1.18  1998/12/11 00:03:47  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.17  1998/11/27 14:50:46  peter
    + open strings, $P switch support

  Revision 1.16  1998/10/13 16:50:22  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.15  1998/10/13 13:10:29  peter
    * new style for m68k/i386 infos and enums

  Revision 1.14  1998/10/13 08:19:41  pierre
    + source_os is now set correctly for cross-processor compilers
      (tos contains all target_infos and
       we use CPU86 and CPU68 conditionals to
       get the source operating system
       this only works if you do not undefine
       the source target  !!)
    * several cg68k memory leaks fixed
    + started to change the code so that it should be possible to have
      a complete compiler (both for m68k and i386 !!)

  Revision 1.13  1998/09/22 17:13:52  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.12  1998/09/01 12:52:05  peter
    + a lot of delphi switches

  Revision 1.11  1998/08/18 20:52:21  peter
    * renamed in_main to in_global which is more logical

  Revision 1.10  1998/08/14 18:14:57  peter
    * forgot to check the target for linux for $S switch message

  Revision 1.9  1998/08/10 15:47:08  peter
    * reinstantited stackcheck note for linux

  Revision 1.8  1998/08/10 14:50:27  peter
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

}
