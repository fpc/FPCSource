{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl

    This units declares some code generator specific constants

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
unit cgconst;

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
  Revision 1.1  2001-08-26 13:36:37  florian
    * some cg reorganisation
    * some PPC updates

}