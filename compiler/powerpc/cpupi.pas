{
    $Id: cpupi.pas,v 1.37 2005/02/14 17:13:10 peter Exp $
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
       procinfo,cpuinfo,psub;

    type
       tppcprocinfo = class(tcgprocinfo)
          { offset where the frame pointer from the outer procedure is stored. }
          parent_framepointer_offset : longint;
          constructor create(aparent:tprocinfo);override;
          procedure set_first_temp_offset;override;
          procedure allocate_push_parasize(size: longint);override;
          function calc_stackframe_size:longint;override;
       end;


  implementation

    uses
       globtype,globals,systems,
       cpubase,
       aasmtai,
       tgobj,
       symconst,symsym,paramgr,symutil,
       verbose;

    constructor tppcprocinfo.create(aparent:tprocinfo);

      begin
         inherited create(aparent);
         maxpushedparasize:=0;
      end;


    procedure tppcprocinfo.set_first_temp_offset;
      var
         ofs : aword;
         locals: longint;
      begin
        if not(po_assembler in procdef.procoptions) then
          begin
            case target_info.abi of
              abi_powerpc_aix:
                ofs:=align(maxpushedparasize+LinkageAreaSizeAIX,16);
              abi_powerpc_sysv:
                ofs:=align(maxpushedparasize+LinkageAreaSizeSYSV,16);
              else
                internalerror(200402191);
            end;
            tg.setfirsttemp(ofs);
          end
        else
          begin
            locals := 0;
            current_procinfo.procdef.localst.foreach_static(@count_locals,@locals);
            if locals <> 0 then
              { at 0(r1), the previous value of r1 will be stored }
              tg.setfirsttemp(4);
          end;
      end;


(*
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
*)


    procedure tppcprocinfo.allocate_push_parasize(size:longint);
      begin
        if size>maxpushedparasize then
          maxpushedparasize:=size;
      end;


    function tppcprocinfo.calc_stackframe_size:longint;
      var
        first_save_fpu_register: longint;
      begin
        { more or less copied from cgcpu.pas/g_stackframe_entry }
        { FIXME: has to be R_F14 instad of R_F8 for SYSV-64bit }
        case target_info.abi of
          abi_powerpc_aix:
            first_save_fpu_register := 14;
          abi_powerpc_sysv:
            first_save_fpu_register := 9;
          else
            internalerror(2003122903);
        end;
        if not (po_assembler in procdef.procoptions) then
          result := align(align((31-13+1)*4+(31-first_save_fpu_register+1)*8,16)+tg.lasttemp,16)
        else
          result := align(tg.lasttemp,16);
      end;


begin
   cprocinfo:=tppcprocinfo;
end.
{
  $Log: cpupi.pas,v $
  Revision 1.37  2005/02/14 17:13:10  peter
    * truncate log

}

