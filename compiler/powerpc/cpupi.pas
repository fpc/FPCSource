{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

  interface

    uses
       cutils,
       cgbase,cpuinfo,psub;

    type
       tppcprocinfo = class(tcgprocinfo)
          { overall size of allocated stack space, currently this is used for the PowerPC only }
          localsize : aword;
          { max. of space need for parameters, currently used by the PowerPC port only }
          maxpushedparasize : aword;

          constructor create(aparent:tprocinfo);override;
          procedure handle_body_start;override;
          procedure after_pass1;override;
          procedure allocate_push_parasize(size: longint);override;
          function calc_stackframe_size:longint;override;
       end;


  implementation

    uses
       globtype,globals,systems,
       cpubase,
       aasmtai,
       tgobj,
       symconst, symsym,paramgr;

    constructor tppcprocinfo.create(aparent:tprocinfo);

      begin
         inherited create(aparent);
         maxpushedparasize:=0;
         localsize:=0;
      end;


    procedure tppcprocinfo.handle_body_start;
      var
         ofs : aword;
      begin
        if not(po_assembler in procdef.procoptions) then
          begin
            case target_info.abi of
              abi_powerpc_aix:
                ofs:=align(maxpushedparasize+LinkageAreaSizeAIX,16);
              abi_powerpc_sysv:
                ofs:=align(maxpushedparasize+LinkageAreaSizeSYSV,16);
            end;
            inc(procdef.parast.address_fixup,ofs);
            procdef.localst.address_fixup:=procdef.parast.address_fixup+procdef.parast.datasize;
          end;
        inherited handle_body_start;
      end;


    procedure tppcprocinfo.after_pass1;
      begin
         if not(po_assembler in procdef.procoptions) then
           begin
             if cs_asm_source in aktglobalswitches then
               aktproccode.insert(Tai_comment.Create(strpnew('Parameter copies start at: r1+'+tostr(procdef.parast.address_fixup))));

             if cs_asm_source in aktglobalswitches then
               aktproccode.insert(Tai_comment.Create(strpnew('Locals start at: r1+'+tostr(procdef.localst.address_fixup))));

             firsttemp_offset:=align(procdef.localst.address_fixup+procdef.localst.datasize,16);
             if cs_asm_source in aktglobalswitches then
               aktproccode.insert(Tai_comment.Create(strpnew('Temp. space start: r1+'+tostr(firsttemp_offset))));

             //!!!! tg.setfirsttemp(firsttemp_offset);
             tg.firsttemp:=firsttemp_offset;
             tg.lasttemp:=firsttemp_offset;
             inherited after_pass1;
           end;
      end;


    procedure tppcprocinfo.allocate_push_parasize(size:longint);
      begin
        if size>maxpushedparasize then
          maxpushedparasize:=size;
      end;


    function tppcprocinfo.calc_stackframe_size:longint;
      var
        savearea : longint;
      begin
        { more or less copied from cgcpu.pas/g_stackframe_entry }
        result := align(align((31-13+1)*4+(31-14+1)*8,16)+tg.lasttemp,16);
      end;


begin
   cprocinfo:=tppcprocinfo;
end.
{
  $Log$
  Revision 1.26  2003-08-16 14:26:44  jonas
    * set correct localsymtable fixup already in handle_body_start instead
      of in after_pass1, as it's necessary to get the correct offsets for
      the calleeside paralocs (and those are now setup in the generic
      handle_body_start)

  Revision 1.25  2003/08/08 15:52:50  olle
    * merged macos entry/exit code generation into the general one.

  Revision 1.24  2003/07/06 20:25:03  jonas
    * fixed ppc compiler

  Revision 1.23  2003/06/13 21:19:32  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.22  2003/06/02 21:42:05  jonas
    * function results can now also be regvars
    - removed tprocinfo.return_offset, never use it again since it's invalid
      if the result is a regvar

  Revision 1.21  2003/05/24 11:47:27  jonas
    * fixed framepointer storage: it's now always stored at r1+12, which is
      a place in the link area reserved for compiler use.

  Revision 1.20  2003/05/23 18:51:26  jonas
    * fixed support for nested procedures and more parameters than those
      which fit in registers (untested/probably not working: calling a
      nested procedure from a deeper nested procedure)

  Revision 1.19  2003/05/22 21:34:11  peter
    * inherite from tcgprocinfo

  Revision 1.18  2003/05/17 14:05:30  jonas
    * fixed para/localst calculations (note to self: don't commit at
      extremely late/early hours :)

  Revision 1.17  2003/05/16 23:15:51  jonas
    * workaround for nested procedures until Peter fixes it properly :)

  Revision 1.16  2003/05/16 20:00:39  jonas
    * powerpc nested procedure fixes, should work completely now if all
      local variables of the parent procedure are declared before the
      nested procedures are declared

  Revision 1.15  2003/05/15 19:39:09  florian
    * fixed ppc compiler which was broken by Peter's changes

  Revision 1.14  2003/05/10 23:57:23  florian
    * vmtpointer_offset must be adjusted in after_pass1 as well

  Revision 1.13  2003/05/09 19:00:30  jonas
    * call inherited after_header as well

  Revision 1.12  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.11  2003/04/27 07:48:05  peter
    * updated for removed lexlevel

  Revision 1.10  2003/04/26 11:31:00  florian
    * fixed the powerpc to work with the new function result handling

  Revision 1.9  2003/04/24 11:24:00  florian
    * fixed several issues with nested procedures

  Revision 1.8  2003/04/06 16:39:11  jonas
    * don't generate entry/exit code for assembler procedures

  Revision 1.7  2003/04/05 21:09:32  jonas
    * several ppc/generic result offset related fixes. The "normal" result
      offset seems now to be calculated correctly and a lot of duplicate
      calculations have been removed. Nested functions accessing the parent's
      function result don't work at all though :(

  Revision 1.6  2002/12/15 19:22:01  florian
    * fixed some crashes and a rte 201

  Revision 1.5  2002/11/18 17:32:01  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.4  2002/09/10 20:30:42  florian
    * fixed offset calculation for symtables etc.

  Revision 1.3  2002/09/07 17:54:59  florian
    * first part of PowerPC fixes

  Revision 1.2  2002/08/18 20:06:30  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.1  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite
}

