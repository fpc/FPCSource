{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit does node transformations

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

unit transn;

  interface

  implementation

{ The following stuff needs to be implemented: }

{$ifdef dummy}
blockn:


         count:=0;
         hp:=p^.left;
         while assigned(hp) do
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                   { Codeumstellungen }

                   { Funktionsresultate an exit anh„ngen }
                   { this is wrong for string or other complex
                     result types !!! }
                   if ret_in_acc(procinfo^.retdef) and
                      assigned(hp^.left) and
                      (hp^.left^.right^.treetype=exitn) and
                      (hp^.right^.treetype=assignn) and
                      (hp^.right^.left^.treetype=funcretn) then
                      begin
                         if assigned(hp^.left^.right^.left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              hp^.left^.right^.left:=getcopy(hp^.right^.right);
                              disposetree(hp^.right);
                              hp^.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp^.right^.treetype in
                     [exitn,breakn,continuen,goton]) and
                     assigned(hp^.left) and
                     (hp^.left^.treetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp^.left^.fileinfo;
                        disposetree(hp^.left);
                        hp^.left:=nil;
                        CGMessage(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp^.right^.fileinfo;
                     end;
                end;
              hp:=hp^.left;
           end;
{$endif dummy}

end.

{
  $Log$
  Revision 1.3  2000-01-07 01:14:55  peter
    * updated copyright to 2000

  Revision 1.2  1999/10/12 21:20:47  florian
    * new codegenerator compiles again

  Revision 1.1  1999/01/23 23:35:02  florian
    + first versions

}

