{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

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
       cobjects,globals,tree,hcodegen,verbose,files,
       aasm,cpubase,cpuasm,tgobj;

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
  Revision 1.6  2000-01-07 01:14:57  peter
    * updated copyright to 2000

  Revision 1.5  1999/09/15 20:35:47  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.4  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.3  1999/08/03 17:09:50  florian
    * the alpha compiler can be compiled now

  Revision 1.2  1999/08/02 23:13:24  florian
    * more changes to compile for the Alpha

  Revision 1.1  1999/08/02 17:14:14  florian
    + changed the temp. generator to an object
}
