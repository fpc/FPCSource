{******************************************************************************
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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

 ****************************************************************************}
unit rgcpu;
{ This unit implements the processor specific class for the register allocator}
{$INCLUDE fpcdefs.inc}
interface
uses
  cpubase,
  cpuinfo,
  aasmcpu,
  aasmtai,
  cclasses,globtype,cgbase,aasmbase,rgobj;
type
{This class implements the cpu spaecific register allocator. It is used by the
code generator to allocate and free registers which might be valid across
nodes. It also contains utility routines related to registers. Some of the
methods in this class overrides generic implementations in rgobj.pas.}
  trgcpu=class(trgobj)
    function GetExplicitRegisterInt(list:taasmoutput;Reg:tregister):tregister;override;
    procedure UngetregisterInt(list:taasmoutput;Reg:tregister);override;
  end;
implementation
uses
  cgobj;
function trgcpu.GetExplicitRegisterInt(list:taasmoutput;reg:tregister):tregister;
  begin
    if reg in [R_O7,R_I7]
    then
      begin
        cg.a_reg_alloc(list,Reg);
        result := Reg;
      end
    else result := inherited GetExplicitRegisterInt(list,reg);
  end;
procedure trgcpu.UngetregisterInt(list: taasmoutput; reg: tregister);
  begin
    if reg in [R_O7,R_I7]
    then
      cg.a_reg_dealloc(list,reg)
    else
      inherited ungetregisterint(list,reg);
  end;
initialization
  rg := trgcpu.create;
end.
{
  $Log$
  Revision 1.4  2002-10-13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.3  2002/10/12 19:03:23  mazen
  * Get/Unget expilit registers to be re-examined

}
