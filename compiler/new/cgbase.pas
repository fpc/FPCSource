{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

    type
       TOpCg = (OP_ADD,OP_AND,OP_DIV,OP_IDIV,OP_IMUL,OP_MUL,OP_NEG,OP_NOT,
                   OP_OR,OP_SAR,OP_SHL,OP_SHR,OP_SUB,OP_XOR);

       TOpCmp = (OC_NONE,OC_EQ,OC_GT,OC_LT,OC_GTE,OC_LTE,OC_NE,OC_BE,OC_B,
                 OC_AE,OC_A);

       TCgSize = (OS_NO,OS_8,OS_16,OS_32,OS_64);

    const
       { defines the default address size for a processor }
       { and defines the natural int size for a processor }
{$ifdef i386}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
{$endif i386}
{$ifdef alpha}
       OS_ADDR = OS_64;
       OS_INT = OS_64;
{$endif alpha}
{$ifdef powerpc}
       OS_ADDR = OS_32;
       OS_INT = OS_32;
{$endif powercc}
{$ifdef ia64}
       OS_ADDR = OS_64;
       OS_INT = OS_64;
{$endif ia64}

    function inverse_opcmp(opcmp: topcmp): topcmp;

    function commutativeop(op: topcg): boolean;

  implementation

    function inverse_opcmp(opcmp: topcmp): topcmp;
      const
        list: array[TOpCg] of TOpCmp =
          (OC_NONE,OC_NE,OC_LE,OC_GTE,OC_LT,OC_GT,OC_EQ,OC_A,OC_AE,
           OC_B,OC_BE);
      begin
        inverse_opcmp := list[opcmp];
      end;


    function commutativeop(op: topcg): boolean;
      const
        list: array[topcg] of boolean =
          (true,true,false,false,true,true,false,false,
           true,false,false,false,false,true);
      begin
        commutativeop := list[op];
      end;
end.
{
  $Log$
  Revision 1.3  2001-09-06 15:25:55  jonas
    * changed type of tcg from object to class ->  abstract methods are now
      a lot cleaner :)
    + more updates: load_*_loc methods, op_*_* methods, g_flags2reg method
      (if possible with geenric implementation and necessary ppc
       implementations)
    * worked a bit further on cgflw, now working on exitnode

  Revision 1.2  2000/11/29 00:30:51  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.1  2000/07/13 06:30:07  michael
  + Initial import

}
