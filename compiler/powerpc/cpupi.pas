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
       cgbase,cpuinfo;

    type
       tppcprocinfo = class(tprocinfo)
          { overall size of allocated stack space, currently this is used for the PowerPC only }
          localsize : aword;

          { max. of space need for parameters, currently used by the PowerPC port only }
          maxpushedparasize : aword;

          constructor create;override;
          procedure after_header;override;
          procedure after_pass1;override;
       end;


  implementation

    uses
       globtype,globals,
       aasmtai,
       tgobj;

    constructor tppcprocinfo.create;

      begin
         inherited create;
         maxpushedparasize:=0;
         localsize:=0;
      end;

    procedure tppcprocinfo.after_header;
      begin
         { this value is necessary for nested procedures }
         procdef.localst.address_fixup:=align(procdef.parast.datasize,16);
      end;

    procedure tppcprocinfo.after_pass1;
      begin
         procdef.parast.address_fixup:=align(maxpushedparasize,16);
         if cs_asm_source in aktglobalswitches then
           aktproccode.insert(Tai_comment.Create(strpnew('Parameter copies start at: r1+'+tostr(procdef.parast.address_fixup))));
         procdef.localst.address_fixup:=align(procdef.parast.address_fixup+procdef.parast.datasize,16);
         if cs_asm_source in aktglobalswitches then
           aktproccode.insert(Tai_comment.Create(strpnew('Locals start at: r1+'+tostr(procdef.localst.address_fixup))));
         procinfo.firsttemp_offset:=align(procdef.localst.address_fixup+procdef.localst.datasize,16);
         if cs_asm_source in aktglobalswitches then
           aktproccode.insert(Tai_comment.Create(strpnew('Temp. space start: r1+'+tostr(procinfo.firsttemp_offset))));

         //!!!! tg.setfirsttemp(procinfo.firsttemp_offset);
         tg.firsttemp:=procinfo.firsttemp_offset;
         tg.lasttemp:=procinfo.firsttemp_offset;
      end;

begin
   cprocinfo:=tppcprocinfo;
end.
{
  $Log$
  Revision 1.2  2002-08-18 20:06:30  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.1  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite
}


