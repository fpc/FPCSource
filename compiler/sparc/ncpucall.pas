{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate sparc assembler for in call nodes

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
unit ncpucall;

{$i fpcdefs.inc}

interface

    uses
      ncgcal;

    type
       tsparccallnode = class(tcgcallnode)
       end;


implementation

    uses
      ncal;

begin
  ccallnode:=TSparcCallNode;
end.
{
  $Log$
  Revision 1.13  2003-07-06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.12  2003/06/13 21:19:32  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.11  2003/04/28 09:49:58  mazen
  - InternalError removed from TSparcCallNode.push_framepointer as it is called by common coplier code.

  Revision 1.10  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.9  2003/04/04 15:38:56  peter
    * moved generic code from n386cal to ncgcal, i386 now also
      uses the generic ncgcal

  Revision 1.8  2003/02/05 22:44:55  mazen
  * making UNIT lower case.

  Revision 1.7  2003/02/04 21:50:54  mazen
  * fixing internal errors related to notn when compiling RTL

  Revision 1.6  2003/01/22 22:30:03  mazen
  - internal errors rmoved from a_loar_reg_reg when reg sizes differs from 32

  Revision 1.5  2002/11/14 21:42:08  mazen
  * fixing return value variable address

  Revision 1.4  2002/10/10 19:57:52  mazen
  * Just to update repsitory

  Revision 1.3  2002/09/30 19:12:14  mazen
  * function prologue fixed

  Revision 1.2  2002/08/30 13:16:23  mazen
  *call parameter handling is now based on the new param manager

  Revision 1.2  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite

  Revision 1.1  2002/08/13 21:40:59  florian
    * more fixes for ppc calling conventions
}

