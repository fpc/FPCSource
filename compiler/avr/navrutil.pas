{
    Copyright (c) 2015 by Jeppe Johansen

    AVR version of some node tree helper routines

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
unit navrutil;

{$i fpcdefs.inc}

interface

  uses
    cclasses,
    fmodule,
    node,nbas,
    ngenutil,
    symtype,symconst,symsym,symdef;


  type

    { tavrnodeutils }

    tavrnodeutils = class(tnodeutils)
    protected
      class procedure insert_init_final_table(main: tmodule; entries:tfplist); override;
    public
      class procedure InsertMemorySizes; override;
    end;

implementation

    uses
      verbose,cutils,globtype,globals,constexp,
      aasmdata,aasmtai,aasmcpu,aasmcnst,aasmbase,
      cpubase,
      symbase,symcpu,symtable,defutil,
      ncnv,ncon,ninl,ncal,nld,nmem,
      systems,
      CPUInfo,
      ppu,
      pass_1;


  class procedure tavrnodeutils.insert_init_final_table(main: tmodule; entries:tfplist);
    var
      op : TAsmOp;
      initList, finalList, header: TAsmList;
      entry : pinitfinalentry;
      i : longint;
    begin
      initList:=TAsmList.create;
      finalList:=TAsmList.create;

      if CPUAVR_HAS_JMP_CALL in cpu_capabilities[current_settings.cputype] then
        op:=A_CALL
      else
        op:=A_RCALL;

      for i:=0 to entries.count-1 do
        begin
          entry:=pinitfinalentry(entries[i]);
          if entry^.finifunc<>'' then
            finalList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(entry^.finifunc,AT_FUNCTION)));
          if entry^.initfunc<>'' then
            initList.Concat(taicpu.op_sym(op,current_asmdata.RefAsmSymbol(entry^.initfunc,AT_FUNCTION)));
        end;

      initList.Concat(taicpu.op_none(A_RET));
      finalList.Concat(taicpu.op_none(A_RET));

      begin
        header:=TAsmList.create;
        new_section(header, sec_code, 'FPC_INIT_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_INIT_FUNC_TABLE',AT_FUNCTION,0,voidcodepointertype));

        initList.insertList(header);
        header.free;

        current_asmdata.AsmLists[al_procedures].concatList(initList);
      end;

      begin
        header:=TAsmList.create;
        new_section(header, sec_code, 'FPC_FINALIZE_FUNC_TABLE', 1);
        header.concat(tai_symbol.Createname_global('FPC_FINALIZE_FUNC_TABLE',AT_FUNCTION,0,voidcodepointertype));

        finalList.insertList(header);
        header.free;

        current_asmdata.AsmLists[al_procedures].concatList(finalList);
      end;

      initList.Free;
      finalList.Free;

      inherited insert_init_final_table(main,entries);
    end;

  class procedure tavrnodeutils.InsertMemorySizes;
    var
      tcb: ttai_typedconstbuilder;
      notename, strtable: shortstring;
      sym: tasmsymbol;
      defstr, defu32: tdef;
    begin
      { Store device information in the .note.gnu.avr.deviceinfo section.

        Layout of.note.gnu.avr.deviceinfo:
         note_name_len: dword
         note_desc_len: dword // Size of the Tavr_desc data record
         note_type: dword = 1
         Tavr_desc = record
           note_name: char[note_name_len] = 'AVR'#0;
           flash_start,
           flash_size,
           sram_start,
           sram_size,
           eeprom_start,
           eeprom_size: dword;
           offset_table_len: dword;
           offset_table: char[] = #0+mcuname#0+#0;
         end }

      notename:='AVR'#0;
      strtable:=#0+lower(embedded_controllers[current_settings.controllertype].controllertypestr)+#0#0;
      tcb:=ctai_typedconstbuilder.create([tcalo_no_dead_strip]);
      defu32:=corddef.create(u32bit,0,$FFFFFFFF,false);
      tcb.maybe_begin_aggregate(defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(length(notename)),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(length(strtable)+32),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(1),defu32);
      defstr:=carraydef.getreusable(cansichartype,length(notename));
      tcb.maybe_begin_aggregate(defstr);
      tcb.emit_tai(Tai_string.Create(notename),defstr);
      tcb.maybe_begin_aggregate(defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(embedded_controllers[current_settings.controllertype].flashbase),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(embedded_controllers[current_settings.controllertype].flashsize),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(embedded_controllers[current_settings.controllertype].srambase),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(embedded_controllers[current_settings.controllertype].sramsize),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(embedded_controllers[current_settings.controllertype].eeprombase),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(embedded_controllers[current_settings.controllertype].eepromsize),defu32);
      tcb.emit_tai(tai_const.Create_32bit_unaligned(8),defu32);  // Size of string offset table
      tcb.emit_tai(tai_const.Create_32bit_unaligned(1),defu32);  // Offset of string in table
      tcb.maybe_begin_aggregate(defstr);
      tcb.emit_tai(Tai_string.Create(strtable),defstr);
      tcb.maybe_end_aggregate(defstr);
      tcb.maybe_end_aggregate(defu32);
      tcb.maybe_end_aggregate(defstr);
      tcb.maybe_end_aggregate(defu32);

      sym:=current_asmdata.DefineAsmSymbol('__AVR_deviceinfo',AB_LOCAL,AT_DATA,defstr);
      current_asmdata.asmlists[al_globals].concatlist(
        tcb.get_final_asmlist(sym,defstr,sec_note,'.gnu.avr.deviceinfo',const_align(32))
      );
      defu32.free;
      tcb.free;

      inherited InsertMemorySizes;
    end;

begin
  cnodeutils:=tavrnodeutils;
end.

