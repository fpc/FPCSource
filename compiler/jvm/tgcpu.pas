{
    Copyright (C) 2010 by Jonas Maebe

    This unit handles the temporary variables for the JVM

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
{
  This unit handles the temporary variables for the JVM.
}
unit tgcpu;

{$i fpcdefs.inc}

  interface

    uses
       tgobj;

    type

       { ttgjvm }

       ttgjvm = class(ttgobj)
        protected
         function alloctemp(list: TAsmList; size, alignment: longint; temptype: ttemptype; def: tdef): longint; override;
       end;

implementation

{ ttgjvm }

function ttgjvm.alloctemp(list: TAsmList; size, alignment: longint; temptype: ttemptype; def: tdef): longint;
begin
  { the JVM only support s1 slot (= 4 bytes in FPC) and 2 slot (= 8 bytes in
    FPC) temps on the stack. double and int64 are 2 slots, the rest is one slot.
    There are no problems with reusing the same slot for a vakue of a different
    type. There are no alignment requirements either. }
  if size<4 then
    size:=4;
  if not(size in [4,8]) then
    internalerror(2010121401);
  Result:=inherited alloctemp(list, size div 4, 1, temptype, def);
end;

begin
  tgclass:=ttgjvm;
end.
