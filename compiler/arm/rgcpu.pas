{
    $Id$
    Copyright (c) 1998-2003 by Florian Klaempfl

    This unit implements the arm specific class for the register
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
       cginfo,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
       {
         function getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;override;
         procedure ungetregisterfpu(list: taasmoutput; r : tregister; size:TCGsize);override;
         procedure cleartempgen; override;
       private
         usedpararegs: Tsupregset;
         usedparafpuregs: tregisterset;
       }
       end;

  implementation

    uses
      cgobj, verbose, cutils;

    {
    function trgcpu.getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;
      begin
        if (r in [R_F0..R_F3]) and
           not is_reg_var_other[r] then
          begin
            if r in usedparafpuregs then
              internalerror(2003060902);
            include(usedparafpuregs,r);
            result.enum := r;
            cg.a_reg_alloc(list,result);
          end
        else
          result:=inherited getexplicitregisterfpu(list,r);
      end;


    procedure trgcpu.ungetregisterfpu(list: taasmoutput; r : tregister; size:TCGsize);
      begin
        if (r.enum in [R_F0..R_F3]) and
           not is_reg_var_other[r.enum] then
          begin
            if not(r.enum in usedparafpuregs) then
              internalerror(2003060903);
            exclude(usedparafpuregs,r.enum);
            cg.a_reg_dealloc(list,r);
          end
        else
          inherited ungetregisterfpu(list,r,size);
      end;


    procedure trgcpu.cleartempgen;

      begin
        inherited cleartempgen;
        usedpararegs := [];
        usedparafpuregs := [];
      end;

    }

initialization
  rg := trgcpu.create(last_int_supreg-first_int_supreg+1);
end.

{
  $Log$
  Revision 1.3  2003-09-04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.2  2003/08/25 23:20:38  florian
    + started to implement FPU support for the ARM
    * fixed a lot of other things

  Revision 1.1  2003/08/16 13:23:01  florian
    * several arm related stuff fixed
}
