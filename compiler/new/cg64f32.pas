{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for 64 bit int
    arithmethics on 32 bit processors

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

unit cgi64f32;

  interface

    uses
       cgobj;

  implementation

    uses
       nmem;

    procedure int64f32_assignment_int64_reg(p : passignmentnode);

      begin
      end;

begin
   p2_assignment:=@int64f32_assignement_int64;
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:07  michael
  + Initial import

  Revision 1.1  2000/03/01 15:36:13  florian
    * some new stuff for the new cg

}