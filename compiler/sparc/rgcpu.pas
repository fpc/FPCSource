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
    function GetExplicitRegisterInt(list:taasmoutput;Reg:Tnewregister):tregister;override;
    procedure UngetregisterInt(list:taasmoutput;Reg:tregister);override;
  end;
implementation
uses
  cgobj,verbose;
function TRgCpu.GetExplicitRegisterInt(list:TAasmOutput;reg:TNewRegister):TRegister;
  var
    r:TRegister;
  begin
    if(reg=RS_O7)or(reg=NR_I7)
    then
      begin
        r.enum:=R_INTREGISTER;
        r.number:=reg;
        cg.a_reg_alloc(list,r);
        result:=r;
      end
    else
      result:=inherited GetExplicitRegisterInt(list,reg);
  end;
procedure trgcpu.UngetRegisterInt(list:taasmoutput;reg:tregister);
  begin
    if reg.enum<>R_INTREGISTER
    then
      internalerror(200302191);
    if (reg.number=RS_O7) or (reg.number=NR_I7)
    then
      cg.a_reg_dealloc(list,reg)
    else
      inherited ungetregisterint(list,reg);
  end;
begin
  rg := trgcpu.create(24); {24 registers.}
end.
{
  $Log$
  Revision 1.9  2003-04-22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.8  2003/03/15 22:51:58  mazen
  * remaking sparc rtl compile

  Revision 1.7  2003/03/10 21:59:54  mazen
  * fixing index overflow in handling new registers arrays.

  Revision 1.6  2003/02/19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.5  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.4  2002/10/13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.3  2002/10/12 19:03:23  mazen
  * Get/Unget expilit registers to be re-examined

}
