{
    $Id$
    Copyright (c) 1993-98 by FPC development team

    Simple unit to add source line and column to each
    memory allocation made with heaptrc unit

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
unit ppheap;

  interface

    uses heaptrc;

  implementation

    uses
       globtype,globals,files;

    procedure ppextra_info(p : pointer);
      begin
         longint(p^):=aktfilepos.line;
         plongint(cardinal(p)+4)^:=aktfilepos.column;
         if assigned(current_module) then
          plongint(cardinal(p)+8)^:=current_module^.unit_index*100000+aktfilepos.fileindex
         else
          plongint(cardinal(p)+8)^:=aktfilepos.fileindex
      end;

  begin
     SetExtraInfo(12,ppextra_info);
  end.


