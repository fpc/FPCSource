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

  implementation


end.
{
  $Log$
  Revision 1.2  2000-11-29 00:30:51  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.1  2000/07/13 06:30:07  michael
  + Initial import

  Revision 1.19  2000/03/11 21:11:24  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

  Revision 1.18  2000/02/28 17:23:58  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.17  2000/02/20 20:49:46  florian
    * newcg is compiling
    * fixed the dup id problem reported by Paul Y.

  Revision 1.16  2000/02/17 14:48:36  florian
     * updated to use old firstpass

  Revision 1.15  2000/01/07 01:14:52  peter
    * updated copyright to 2000

  Revision 1.14  1999/12/24 22:47:42  jonas
    * added OC_NONE to the compare forms (to allow unconditional jumps)

  Revision 1.13  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.12  1999/11/05 13:15:00  florian
    * some fixes to get the new cg compiling again

  Revision 1.11  1999/10/14 14:57:54  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.10  1999/10/12 21:20:46  florian
    * new codegenerator compiles again

  Revision 1.9  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.8  1999/08/06 13:26:49  florian
    * more changes ...

  Revision 1.7  1999/08/05 14:58:10  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.6  1999/08/04 00:23:51  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.5  1999/08/01 18:22:32  florian
   * made it again compilable

  Revision 1.4  1999/01/23 23:29:45  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.3  1999/01/06 22:58:48  florian
    + some stuff for the new code generator

  Revision 1.2  1998/12/26 15:20:28  florian
    + more changes for the new version

  Revision 1.1  1998/12/15 22:18:55  florian
    * some code added
}
