{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This units implements some code generator helper routines

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
unit cgbase;

  interface

    uses
       globtype,cobjects,symtable,aasm
{$I cpuunit.inc}
       ;
    { clears a location record }
    procedure clear_location(var loc : tlocation);
    { copies a location, takes care of the symbol }
    procedure set_location(var destloc,sourceloc : tlocation);
    { swaps two locations }
    procedure swap_location(var destloc,sourceloc : tlocation);

  implementation

    procedure clear_location(var loc : tlocation);

      begin
        if ((loc.loc=LOC_MEM) or (loc.loc=LOC_REFERENCE)) and
           assigned(loc.reference.symbol) then
          stringdispose(loc.reference.symbol);
        loc.loc:=LOC_INVALID;
      end;

    procedure set_location(var destloc,sourceloc : tlocation);

      begin
         { this is needed if you want to be able to delete }
         { the string with the nodes                       }
         if assigned(destloc.reference.symbol) then
           stringdispose(destloc.reference.symbol);
         destloc:= sourceloc;
         if sourceloc.loc in [LOC_MEM,LOC_REFERENCE] then
           begin
              if assigned(sourceloc.reference.symbol) then
                destloc.reference.symbol:=
                  stringdup(sourceloc.reference.symbol^);
           end
         else
           destloc.reference.symbol:=nil;
      end;

    procedure swap_location(var destloc,sourceloc : tlocation);

      var
         swapl : tlocation;

      begin
         swapl:=destloc;
         destloc:=sourceloc;
         sourceloc:=swapl;
      end;

end.
{
  $Log$
  Revision 1.1  1998-12-15 22:18:55  florian
    * some code added

}