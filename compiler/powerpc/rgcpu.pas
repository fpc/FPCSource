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
         function getexplicitregisterint(list: taasmoutput; reg: tregister): tregister; override;
         procedure ungetregisterint(list: taasmoutput; reg: tregister); override;
       end;

  implementation

    uses
      cgobj;

    function trgcpu.getexplicitregisterint(list: taasmoutput; reg: tregister): tregister;

      begin
        if reg = R_0 then
          begin
            cg.a_reg_alloc(list,reg);
            result := reg;
          end
        else result := inherited getexplicitregisterint(list,reg);
      end;


    procedure trgcpu.ungetregisterint(list: taasmoutput; reg: tregister);

      begin
        if reg = R_0 then
          cg.a_reg_dealloc(list,reg)
        else
          inherited ungetregisterint(list,reg);
      end;

initialization
  rg := trgcpu.create;
end.

{
  $Log$
  Revision 1.3  2002-07-07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.2  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2002/04/06 18:13:02  jonas
    * several powerpc-related additions and fixes

}
