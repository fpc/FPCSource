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
       tarmprocinfo = class(tcgprocinfo)
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
       symconst,symsym,paramgr;

    constructor tarmprocinfo.create(aparent:tprocinfo);

      begin
         inherited create(aparent);
         maxpushedparasize:=0;
      end;


    procedure tarmprocinfo.handle_body_start;
      var
         ofs : aword;
      begin
        if not(po_assembler in procdef.procoptions) then
          begin
            {!!!!!!!!
            case target_info.abi of
              abi_powerpc_aix:
                ofs:=align(maxpushedparasize+LinkageAreaSizeAIX,16);
              abi_powerpc_sysv:
                ofs:=align(maxpushedparasize+LinkageAreaSizeSYSV,16);
            end;
            }
            inc(procdef.parast.address_fixup,ofs);
            procdef.localst.address_fixup:=procdef.parast.address_fixup+procdef.parast.datasize;
          end;
        inherited handle_body_start;
      end;


    procedure tarmprocinfo.after_pass1;
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


    procedure tarmprocinfo.allocate_push_parasize(size:longint);
      begin
        if size>maxpushedparasize then
          maxpushedparasize:=size;
      end;


    function tarmprocinfo.calc_stackframe_size:longint;
      begin
        { more or less copied from cgcpu.pas/g_stackframe_entry }
        if not (po_assembler in procdef.procoptions) then
          result := align(align((31-13+1)*4+(31-14+1)*8,16)+tg.lasttemp,16)
        else
          result := align(tg.lasttemp,16);
      end;


begin
   cprocinfo:=tarmprocinfo;
end.
{
  $Log$
  Revision 1.1  2003-08-20 15:50:13  florian
    * more arm stuff
}

