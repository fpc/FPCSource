{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the powerpc specific class for the register
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

 ****************************************************************************
}

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
         function getexplicitregisterint(list: taasmoutput; reg: Tnewregister): tregister; override;
         procedure ungetregisterint(list: taasmoutput; reg: tregister); override;
       end;

  implementation

    uses
      cgobj;

    function trgcpu.getexplicitregisterint(list: taasmoutput; reg: Tnewregister): tregister;

    var r:Tregister;

      begin
        if reg = NR_R0 then
          begin
            r.enum:=R_INTREGISTER;
            r.number:=NR_R0;
            cg.a_reg_alloc(list,r);
            result := r;
          end
        else result := inherited getexplicitregisterint(list,reg);
      end;


    procedure trgcpu.ungetregisterint(list: taasmoutput; reg: tregister);

      begin
        if reg.enum = R_0 then
          cg.a_reg_dealloc(list,reg)
        else
          inherited ungetregisterint(list,reg);
      end;

initialization
  rg := trgcpu.create(32);  {PPC has 32 registers.}
end.

{
  $Log$
  Revision 1.6  2003-04-22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.5  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.4  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.3  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.2  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
