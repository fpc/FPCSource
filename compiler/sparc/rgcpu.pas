{******************************************************************************
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the i386 specific class for the register
    allocator

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
{$INCLUDE fpcdefs.inc}
interface
uses
  cpubase,
  cpuinfo,
  aasmcpu,
  aasmtai,
  cclasses,globtype,cgbase,aasmbase,rgobj;
type
  trgcpu=class(trgobj)
    { to keep the same allocation order as with the old routines }
    procedure UngetregisterInt(list:taasmoutput;Reg:tregister);override;
    function GetExplicitRegisterInt(list:taasmoutput;Reg:tregister):tregister;override;
  end;
implementation
uses
  cgobj;
function trgcpu.GetExplicitRegisterInt(list:taasmoutput;reg:tregister):tregister;
  begin
    if reg = R_i0
    then
      begin
        cg.a_reg_alloc(list,Reg);
        result := Reg;
      end
        else result := inherited GetExplicitRegisterInt(list,reg);
      end;
procedure trgcpu.UngetregisterInt(list: taasmoutput; reg: tregister);
  begin
    if reg = R_i0
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
  Revision 1.2  2002-10-11 13:35:14  mazen
  *** empty log message ***

}
