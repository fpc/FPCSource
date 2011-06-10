{
    Copyright (c) 2002-2006 by Florian Klaempfl

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
       psub,procinfo;

    type
       tx86_64procinfo = class(tcgprocinfo)
         procedure set_first_temp_offset;override;
         procedure generate_parameter_info;override;
         function calc_stackframe_size:longint;override;
       end;


implementation

    uses
      systems,
      globtype,
      globals,
      cutils,
      symconst,
      tgobj;


    procedure tx86_64procinfo.set_first_temp_offset;
      begin
        if target_info.system=system_x86_64_win64 then
          begin
            { Fixes the case when there are calls done by low-level means
              (cg.a_call_name) but no child callnode }
            if (pi_do_call in flags) then
              allocate_push_parasize(32);

            if not(po_assembler in procdef.procoptions) and
              (tg.direction > 0) then
            { maxpushedparasize already contains 32 bytes of spilling area }
              tg.setfirsttemp(tg.direction*maxpushedparasize);
          end
        else
          tg.setfirsttemp(tg.direction*maxpushedparasize);
      end;


    procedure tx86_64procinfo.generate_parameter_info;
      begin
        inherited generate_parameter_info;
        if target_info.system=system_x86_64_win64 then
          para_stack_size:=0;
      end;


    function tx86_64procinfo.calc_stackframe_size:longint;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,16));
        { Note 1: when tg.direction>0, tg.lasttemp is already offset by maxpushedparasize
                  (because tg.setfirsttemp also sets lasttemp)
          Note 2: Align to 8 bytes here. The final 16-byte alignment is handled in
                  tcgx86.g_proc_entry, which considers saved rbp and the misalignment
                  caused by the call itself. }
        if (tg.direction>0) then
          result:=Align(tg.lasttemp,8)
        else
          result:=Align(tg.direction*tg.lasttemp+maxpushedparasize,8);
      end;


begin
   cprocinfo:=tx86_64procinfo;
end.
