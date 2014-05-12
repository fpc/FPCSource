{
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
       cutils,globtype,
       cgbase,aasmdata,
       procinfo,cpuinfo,psub;

    type
       tppcprocinfo = class(tcgprocinfo)
          needstackframe: boolean;

          { offset where the frame pointer from the outer procedure is stored. }
          parent_framepointer_offset : longint;
          constructor create(aparent:tprocinfo);override;
          procedure set_first_temp_offset;override;
          function calc_stackframe_size:longint;override;

          function uses_stack_temps: boolean;
          procedure allocate_got_register(list: TAsmList);override;
         private
          first_save_int_reg, first_save_fpu_reg: tsuperregister;
         public
          needs_frame_pointer: boolean;

          property get_first_save_int_reg: tsuperregister read first_save_int_reg;
          property get_first_save_fpu_reg: tsuperregister read first_save_fpu_reg;
          procedure postprocess_code;override;
       end;


  implementation

    uses
       globals,systems,
       cpubase,
       aasmtai,
       tgobj,cgobj,
       symconst,symsym,paramgr,symutil,symtable,
       verbose,
       aasmcpu;

    constructor tppcprocinfo.create(aparent:tprocinfo);

      begin
         inherited create(aparent);
         first_save_int_reg:=32;
         first_save_fpu_reg:=32;
         needs_frame_pointer:=false;
      end;


    procedure tppcprocinfo.set_first_temp_offset;
      var
         ofs : aword;
      begin
        if not(po_assembler in procdef.procoptions) then
          begin
            case target_info.abi of
              abi_powerpc_aix:
                ofs:=maxpushedparasize+LinkageAreaSizeAIX;
              abi_powerpc_sysv:
                ofs:=maxpushedparasize+LinkageAreaSizeSYSV;
              else
                internalerror(200402191);
            end;
            tg.setfirsttemp(ofs);
          end
        else
          begin
            if (current_procinfo.procdef.localst.symtabletype=localsymtable) and
               (tabstractlocalsymtable(current_procinfo.procdef.localst).count_locals <> 0) then
              begin
                { at 0(r1), the previous value of r1 will be stored }
                tg.setfirsttemp(4);
              end
          end;
      end;


(*
    procedure tppcprocinfo.after_pass1;
      begin
         if not(po_assembler in procdef.procoptions) then
           begin
             if cs_asm_source in current_settings.globalswitches then
               aktproccode.insert(Tai_comment.Create(strpnew('Parameter copies start at: r1+'+tostr(procdef.parast.address_fixup))));

             if cs_asm_source in current_settings.globalswitches then
               aktproccode.insert(Tai_comment.Create(strpnew('Locals start at: r1+'+tostr(procdef.localst.address_fixup))));
             firsttemp_offset:=align(procdef.localst.address_fixup+procdef.localst.datasize,16);
             if cs_asm_source in current_settings.globalswitches then
               aktproccode.insert(Tai_comment.Create(strpnew('Temp. space start: r1+'+tostr(firsttemp_offset))));

             //!!!! tg.setfirsttemp(firsttemp_offset);
             tg.firsttemp:=firsttemp_offset;
             tg.lasttemp:=firsttemp_offset;
             inherited after_pass1;
           end;
      end;
*)


    function tppcprocinfo.uses_stack_temps: boolean;
      begin
        result := tg.firsttemp <> tg.lasttemp;
      end;


    function tppcprocinfo.calc_stackframe_size:longint;
      var
        low_nonvol_fpu_reg, regcounter: tsuperregister;
      begin
        if not (po_assembler in procdef.procoptions) then
          begin
            first_save_fpu_reg := 32;
            first_save_int_reg := 32;
            { FIXME: has to be R_F14 instead of R_F8 for SYSV-64bit }
            case target_info.abi of
              abi_powerpc_aix:
                low_nonvol_fpu_reg := RS_F14;
              abi_powerpc_sysv:
                low_nonvol_fpu_reg := RS_F14;
              else
                internalerror(2003122903);
            end;
            for regcounter := low_nonvol_fpu_reg to RS_F31 do
              begin
                if regcounter in cg.rg[R_FPUREGISTER].used_in_proc then
                  begin
                    first_save_fpu_reg := ord(regcounter) - ord(RS_F0);
                    break;
                  end;
              end;
            for regcounter := RS_R13 to RS_R31 do
              begin
                if regcounter in cg.rg[R_INTREGISTER].used_in_proc then
                  begin
                    first_save_int_reg := ord(regcounter) - ord(RS_R0);
                    break;
                  end;
              end;
            if not(pi_do_call in flags) and
               (not uses_stack_temps) and
               (((target_info.abi = abi_powerpc_aix) and
                 ((32-first_save_int_reg)*4+(32-first_save_fpu_reg)*8 <= 220)) or
                ((target_info.abi = abi_powerpc_sysv) and
                 (first_save_int_reg + first_save_fpu_reg = 64))) then
              begin
                { don't allocate a stack frame }
                result := (32-first_save_int_reg)*4+(32-first_save_fpu_reg)*8;
                needstackframe := false;
              end
            else
              begin
                result := (32-first_save_int_reg)*4+(32-first_save_fpu_reg)*8+tg.lasttemp;
                result := align(result,16);
                needstackframe := result<>0;
              end;
          end
        else
          begin
            result := align(tg.lasttemp,16);
            needstackframe := result<>0;
          end;
      end;


    procedure tppcprocinfo.allocate_got_register(list: TAsmList);
      begin
        if (target_info.system = system_powerpc_darwin) and
           (cs_create_pic in current_settings.moduleswitches) then
          begin
            got := cg.getaddressregister(list);
          end;
      end;


    procedure tppcprocinfo.postprocess_code;
      begin
        fixup_jmps(aktproccode);
      end;


begin
   cprocinfo:=tppcprocinfo;
end.

