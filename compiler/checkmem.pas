{
    $Id$
    Copyright (c) 2002 by Pierre Muller

    This unit is just used to check that all memory is freed

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
Unit CheckMem;

{$i fpcdefs.inc}

Interface

Implementation

Const
  EntryMemUsed : longint = 0;

  Procedure CheckMemory(LostMemory : longint);
    var
      st : string;
  begin
    if LostMemory<>0 then
      begin
        str(LostMemory,st);
        Writeln('Memory Lost = ',st);
      end;
  end;


initialization
  EntryMemUsed:=system.HeapSize-MemAvail;

finalization
  CheckMemory(system.HeapSize-MemAvail-EntryMemUsed);
end.

{
  $Log$
  Revision 1.3  2003-04-22 14:33:38  peter
    * removed some notes/hints

  Revision 1.2  2002/11/15 12:23:49  peter
    * new unit

}
