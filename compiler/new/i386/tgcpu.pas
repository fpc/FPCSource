{
    $Id$
    Copyright (C) 1993-99 by Florian Klaempfl

    This unit handles the temporary variables stuff for i386

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
unit tgcpu;

  interface

    uses
       cobjects,globals,tree,hcodegen,verbose,files,aasm
       ,i386base,i386asm,tgobj
{$ifdef dummy}
       end
{$endif}
       ;

    const
       countusablereg : byte = 4;

       { this value is used in tsaved, if the register isn't saved }
       reg_not_saved = $7fffffff;
       usableregmmx : byte = 8;

    type
       ttgobji386 = object(ttgobj)
          procedure ungetregister(r : tregister);virtual;
          function istemp(const ref : treference) : boolean;virtual;
          procedure del_reference(const ref : treference);virtual;
       end;

    var
       tg : ttgobji386;
       reg_pushes : array[R_EAX..R_MM6] of longint;
       is_reg_var : array[R_EAX..R_MM6] of boolean;

  implementation

    procedure ttgobji386.ungetregister(r : tregister);

      begin
      end;

    function ttgobji386.istemp(const ref : treference) : boolean;

      begin
      end;

    procedure ttgobji386.del_reference(const ref : treference);

      begin
      end;


begin
   tg.init;
end.
{
  $Log$
  Revision 1.2  1999-08-02 23:13:24  florian
    * more changes to compile for the Alpha

  Revision 1.1  1999/08/02 17:14:14  florian
    + changed the temp. generator to an object
}