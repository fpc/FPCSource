{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Implements the PowerPC specific part of call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
unit nppccal;

{$i fpcdefs.inc}

interface

    uses
      symdef,node,ncal,ncgcal;

    type
       tppccallnode = class(tcgcallnode)
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defutil,paramgr,
{$ifdef GDB}
  {$ifdef delphi}
      sysutils,
  {$else}
      strings,
  {$endif}
      gdb,
{$endif GDB}
      cgbase,pass_2,
      cpuinfo,cpubase,aasmbase,aasmtai,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cgobj,tgobj,regvars,rgobj,rgcpu,
      cg64f32,cgcpu,cpupi,procinfo;

begin
   ccallnode:=tppccallnode;
end.
{
  $Log$
  Revision 1.22  2003-10-01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.21  2003/09/03 19:35:24  peter
    * powerpc compiles again

  Revision 1.20  2003/07/08 21:24:59  peter
    * sparc fixes

  Revision 1.19  2003/07/06 20:25:03  jonas
    * fixed ppc compiler

  Revision 1.18  2003/06/13 21:19:32  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.17  2003/06/04 11:58:58  jonas
    * calculate localsize also in g_return_from_proc since it's now called
      before g_stackframe_entry (still have to fix macos)
    * compilation fixes (cycle doesn't work yet though)

  Revision 1.16  2003/05/25 14:32:42  jonas
    * fixed register numbering bug

  Revision 1.15  2003/05/24 11:47:27  jonas
    * fixed framepointer storage: it's now always stored at r1+12, which is
      a place in the link area reserved for compiler use.

  Revision 1.14  2003/05/23 18:51:26  jonas
    * fixed support for nested procedures and more parameters than those
      which fit in registers (untested/probably not working: calling a
      nested procedure from a deeper nested procedure)

  Revision 1.13  2003/05/18 15:15:59  florian
    + added abi field to tsysteminfo

  Revision 1.12  2003/05/16 23:15:51  jonas
    * workaround for nested procedures until Peter fixes it properly :)

  Revision 1.11  2003/05/16 20:00:39  jonas
    * powerpc nested procedure fixes, should work completely now if all
      local variables of the parent procedure are declared before the
      nested procedures are declared

  Revision 1.10  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.9  2003/04/27 10:41:47  florian
    * fixed nested procedures to get them working as before

  Revision 1.8  2003/04/27 07:48:05  peter
    * updated for removed lexlevel

  Revision 1.7  2003/04/24 11:24:00  florian
    * fixed several issues with nested procedures

  Revision 1.6  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.5  2003/04/04 15:38:56  peter
    * moved generic code from n386cal to ncgcal, i386 now also
      uses the generic ncgcal

  Revision 1.4  2002/12/05 14:28:12  florian
    * some variant <-> dyn. array stuff

  Revision 1.3  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.2  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite

  Revision 1.1  2002/08/13 21:40:59  florian
    * more fixes for ppc calling conventions
}

