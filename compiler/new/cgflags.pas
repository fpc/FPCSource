{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl
    Member of the Free Pascal development team

    This unit implements the code generation for things regarding
    flags, this unit applies of course only for cpus support flags

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

unit cgflags;

  interface

    uses
       cgobj;

  implementation

    uses
       cgobj,nmem;

    procedure flags_assignment_flags(p : passignmentnode);

      begin
         if loc=LOC_CREGISTER then
           emit_flag2reg(p^.right^.location.resflags,p^.left^.location.register)
         else
           begin
             ai:=new(paicpu,op_ref(A_Setcc,S_B,newreference(p^.left^.location.reference)));
             ai^.SetCondition(flag_2_cond[p^.right^.location.resflags]);
             exprasmlist^.concat(ai);
           end;
         del_reference(p^.left^.location.reference);
      end;

begin
   p2_assignment_flags:=@flags_assignment_flags;
end.
{
  $Log$
  Revision 1.1  2000-03-01 15:36:13  florian
    * some new stuff for the new cg

}