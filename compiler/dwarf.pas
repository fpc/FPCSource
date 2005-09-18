{
    Copyright (c) 2003-2004 by Peter Vreman and Florian Klaempfl

    This units contains special support for DWARF debug info

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
unit dwarf;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      globtype,
      cgbase,cpubase,
      aasmbase,aasmtai;

    const
      maxdwarfops = 2;

    type
      tdwarfoperenc=(doe_uleb,doe_sleb,doe_ptr,doe_32bit,doe_16bit,doe_8bit);
      tdwarfopertype=(dop_reg,dop_const,dop_sym,dop_reloffset);

      tdwarfoper=record
        enc : tdwarfoperenc;
        case typ:tdwarfopertype of
          dop_reg : (register:tregister);
          dop_const : (value:int64);
          dop_sym : (sym:tasmsymbol);
          dop_reloffset : (beginsym,endsym:tasmsymbol);
      end;

      tdwarfitem=class(TLinkedListItem)
        op   : byte;
        ops  : byte;
        oper : array[0..maxdwarfops-1] of tdwarfoper;
        constructor create(aop:longint);
        constructor create_reg(aop:longint;enc1:tdwarfoperenc;reg:tregister);
        constructor create_const(aop:longint;enc1:tdwarfoperenc;val:int64);
        constructor create_reloffset(aop:longint;enc1:tdwarfoperenc;beginlab,endlab:tasmsymbol);
        constructor create_reg_const(aop:longint;enc1:tdwarfoperenc;reg:tregister;enc2:tdwarfoperenc;val:longint);
        procedure generate_code(list:taasmoutput);
      end;

      tdwarf=class
      private
        Fal_dwarf : TLinkedList;
      public
        constructor create;
        destructor destroy;override;
        property al_dwarf:TlinkedList read Fal_dwarf;
      end;

      tdwarfcfi=class(tdwarf)
      private
        FFrameStartLabel,
        FFrameEndLabel,
        FLastloclabel : tasmlabel;
        procedure cfa_advance_loc(list:taasmoutput);
      protected
        code_alignment_factor,
        data_alignment_factor : shortint;
      public
        constructor create;
        procedure generate_code(list:taasmoutput);
        { operations }
        procedure start_frame(list:taasmoutput);
        procedure end_frame(list:taasmoutput);
        procedure cfa_offset(list:taasmoutput;reg:tregister;ofs:longint);
        procedure cfa_restore(list:taasmoutput;reg:tregister);
        procedure cfa_def_cfa_register(list:taasmoutput;reg:tregister);
        procedure cfa_def_cfa_offset(list:taasmoutput;ofs:longint);
      end;


    var
      dwarfcfi : tdwarfcfi;

    function dwarf_reg(r:tregister):longint;


implementation

    uses
      verbose;

    const
      { Call frame information }
      DW_CFA_set_loc          = $01;
      DW_CFA_advance_loc1     = $02;
      DW_CFA_advance_loc2     = $03;
      DW_CFA_advance_loc4     = $04;
      DW_CFA_offset_extended  = $05;
      DW_CFA_restore_extended = $06;
      DW_CFA_def_cfa          = $0c;
      DW_CFA_def_cfa_register = $0d;
      DW_CFA_def_cfa_offset   = $0e;
      { Own additions }
      DW_CFA_start_frame = $f0;
      DW_CFA_end_frame   = $f1;


{****************************************************************************
                                  Helpers
****************************************************************************}

    function dwarf_reg(r:tregister):longint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
      end;


{****************************************************************************
                                  TDWARF
****************************************************************************}

    constructor tdwarf.create;
      begin
        Fal_dwarf:=TLinkedList.Create;
      end;


    destructor tdwarf.destroy;
      begin
        Fal_dwarf.Free;
      end;


{****************************************************************************
                                TDWARFITEM
****************************************************************************}

    constructor tdwarfitem.create(aop:longint);
      begin
        inherited create;
        op:=aop;
        ops:=0;
      end;


    constructor tdwarfitem.create_reg(aop:longint;enc1:tdwarfoperenc;reg:tregister);
      begin
        inherited create;
        op:=aop;
        ops:=1;
        oper[0].typ:=dop_reg;
        oper[0].enc:=enc1;
        oper[0].register:=reg;
      end;


    constructor tdwarfitem.create_const(aop:longint;enc1:tdwarfoperenc;val:int64);
      begin
        inherited create;
        op:=aop;
        ops:=1;
        oper[0].typ:=dop_const;
        oper[0].enc:=enc1;
        oper[0].value:=val;
      end;


    constructor tdwarfitem.create_reloffset(aop:longint;enc1:tdwarfoperenc;beginlab,endlab:tasmsymbol);
      begin
        inherited create;
        op:=aop;
        ops:=1;
        { relative offsets are passed }
        oper[0].typ:=dop_reloffset;
        oper[0].enc:=enc1;
        oper[0].beginsym:=beginlab;
        oper[0].endsym:=endlab;
      end;


    constructor tdwarfitem.create_reg_const(aop:longint;enc1:tdwarfoperenc;reg:tregister;enc2:tdwarfoperenc;val:longint);
      begin
        inherited create;
        op:=aop;
        ops:=2;
        oper[0].typ:=dop_reg;
        oper[0].enc:=enc1;
        oper[0].register:=reg;
        oper[1].typ:=dop_const;
        oper[1].enc:=enc2;
        oper[1].value:=val;
      end;


    procedure tdwarfitem.generate_code(list:taasmoutput);
      const
        enc2ait_const : array[tdwarfoperenc] of taitype = (
          ait_const_uleb128bit,ait_const_sleb128bit,ait_const_ptr,
          ait_const_32bit,ait_const_16bit,ait_const_8bit
        );
      var
        i : integer;
      begin
        list.concat(tai_const.create_8bit(op));
        for i:=0 to ops-1 do
          begin
            case oper[i].typ of
              dop_const :
                list.concat(tai_const.create(enc2ait_const[oper[i].enc],oper[i].value));
              dop_sym :
                begin
                  if oper[i].enc<>doe_ptr then
                    internalerror(200404127);
                  list.concat(tai_const.create_sym(oper[i].sym));
                end;
              dop_reloffset :
                list.concat(tai_const.create_rel_sym(enc2ait_const[oper[i].enc],oper[i].beginsym,oper[i].endsym));
              dop_reg :
                list.concat(tai_const.create(enc2ait_const[oper[i].enc],regdwarf_table[findreg_by_number(oper[i].register)]));
              else
                internalerror(200404128);
            end;
          end;
      end;


{****************************************************************************
                                 TDWARFCFI
****************************************************************************}

    constructor tdwarfcfi.create;
      begin
        inherited create;
        FFrameStartLabel:=nil;
        FFrameEndLabel:=nil;
        FLastLocLabel:=nil;
        code_alignment_factor:=1;
        data_alignment_factor:=-1;
      end;


    procedure tdwarfcfi.generate_code(list:taasmoutput);
      var
        hp : tdwarfitem;
        cielabel,
        lenstartlabel,
        lenendlabel    : tasmlabel;
        tc             : tai_const;
      begin
        new_section(list,sec_debug_frame,'',0);
        { CIE
           DWORD   length
           DWORD   CIE_Id = 0xffffffff
           BYTE    version = 1
           STRING  augmentation = "" = BYTE 0
           ULEB128 code alignment factor = 1
           ULEB128 data alignment factor = -1
           BYTE    return address register
           <...>   start sequence
        }
        objectlibrary.getjumplabel(cielabel);
        list.concat(tai_label.create(cielabel));
        objectlibrary.getjumplabel(lenstartlabel);
        objectlibrary.getjumplabel(lenendlabel);
        list.concat(tai_const.create_rel_sym(ait_const_32bit,lenstartlabel,lenendlabel));
        list.concat(tai_label.create(lenstartlabel));
        list.concat(tai_const.create_32bit(longint($ffffffff)));
        list.concat(tai_const.create_8bit(1));
        list.concat(tai_const.create_8bit(0)); { empty string }
        list.concat(tai_const.create_uleb128bit(code_alignment_factor));
        list.concat(tai_const.create_sleb128bit(data_alignment_factor));
        list.concat(tai_const.create_8bit(dwarf_reg(NR_RETURN_ADDRESS_REG)));
        { Generate standard code
            def_cfa(stackpointer,sizeof(aint))
            cfa_offset_extended(returnaddres,-sizeof(aint))
        }
{$warning TODO This needs to be target dependent}
        list.concat(tai_const.create_8bit(DW_CFA_def_cfa));
        list.concat(tai_const.create_uleb128bit(dwarf_reg(NR_STACK_POINTER_REG)));
        list.concat(tai_const.create_uleb128bit(sizeof(aint)));
        list.concat(tai_const.create_8bit(DW_CFA_offset_extended));
        list.concat(tai_const.create_uleb128bit(dwarf_reg(NR_RETURN_ADDRESS_REG)));
        list.concat(tai_const.create_uleb128bit((-sizeof(aint)) div data_alignment_factor));
        list.concat(cai_align.create_zeros(4));
        list.concat(tai_label.create(lenendlabel));
        lenstartlabel:=nil;
        lenendlabel:=nil;

        hp:=TDwarfItem(al_dwarf.first);
        while assigned(hp) do
          begin
            case hp.op of
              DW_CFA_Start_Frame :
                begin
                  if assigned(lenstartlabel) then
                    internalerror(200404125);
                  if (hp.ops<>1) or
                     (hp.oper[0].typ<>dop_reloffset) then
                    internalerror(200404126);
                  objectlibrary.getjumplabel(lenstartlabel);
                  objectlibrary.getjumplabel(lenendlabel);
                  { FDE
                     DWORD length
                     DWORD CIE-pointer = cielabel
                     PTRSIZE initial location = oper[0]
                     PTRSIZE function size = oper[1]
                  }
                  list.concat(tai_const.create_rel_sym(ait_const_32bit,lenstartlabel,lenendlabel));
                  list.concat(tai_label.create(lenstartlabel));
                  { force label offset to 32bit }
                  tc:=tai_const.create_sym(cielabel);
                  tc.typ:=ait_const_32bit;
                  list.concat(tc);
                  list.concat(tai_const.create_sym(hp.oper[0].beginsym));
                  list.concat(tai_const.create_rel_sym(ait_const_ptr,hp.oper[0].beginsym,hp.oper[0].endsym));
                end;
              DW_CFA_End_Frame :
                begin
                  list.concat(cai_align.create_zeros(4));
                  list.concat(tai_label.create(lenendlabel));
                  lenstartlabel:=nil;
                  lenendlabel:=nil;
                end;
              else
                hp.generate_code(list);
            end;
            hp:=TDwarfItem(hp.next);
          end;
        { Check for open frames }
        if assigned(lenstartlabel) then
          internalerror(2004041210);
        { al_dwarf is processed, remove items }
        al_dwarf.Clear;
      end;


    procedure tdwarfcfi.start_frame(list:taasmoutput);
      begin
        if assigned(FFrameStartLabel) then
          internalerror(200404129);
        objectlibrary.getjumplabel(FFrameStartLabel);
        objectlibrary.getjumplabel(FFrameEndLabel);
        FLastloclabel:=FFrameStartLabel;
        list.concat(tai_label.create(FFrameStartLabel));
        al_dwarf.concat(tdwarfitem.create_reloffset(DW_CFA_start_frame,doe_32bit,FFrameStartLabel,FFrameEndLabel));
      end;


    procedure tdwarfcfi.end_frame(list:taasmoutput);
      begin
        if not assigned(FFrameStartLabel) then
          internalerror(2004041213);
        al_dwarf.concat(tdwarfitem.create(DW_CFA_end_frame));
        list.concat(tai_label.create(FFrameEndLabel));
        FFrameStartLabel:=nil;
        FFrameEndLabel:=nil;
        FLastLocLabel:=nil;
      end;


    procedure tdwarfcfi.cfa_advance_loc(list:taasmoutput);
      var
        currloclabel : tasmlabel;
      begin
        if FLastloclabel=nil then
          internalerror(200404082);
        objectlibrary.getjumplabel(currloclabel);
        list.concat(tai_label.create(currloclabel));
        al_dwarf.concat(tdwarfitem.create_reloffset(DW_CFA_advance_loc4,doe_32bit,FLastloclabel,currloclabel));
        FLastloclabel:=currloclabel;
      end;


    procedure tdwarfcfi.cfa_offset(list:taasmoutput;reg:tregister;ofs:longint);
      begin
        cfa_advance_loc(list);
{$warning TODO check if ref is a temp}
        { offset must be positive }
        al_dwarf.concat(tdwarfitem.create_reg_const(DW_CFA_offset_extended,doe_uleb,reg,doe_uleb,ofs div data_alignment_factor));
      end;


    procedure tdwarfcfi.cfa_restore(list:taasmoutput;reg:tregister);
      begin
        cfa_advance_loc(list);
        al_dwarf.concat(tdwarfitem.create_reg(DW_CFA_restore_extended,doe_uleb,reg));
      end;


    procedure tdwarfcfi.cfa_def_cfa_register(list:taasmoutput;reg:tregister);
      begin
        cfa_advance_loc(list);
        al_dwarf.concat(tdwarfitem.create_reg(DW_CFA_def_cfa_register,doe_uleb,reg));
      end;


    procedure tdwarfcfi.cfa_def_cfa_offset(list:taasmoutput;ofs:longint);
      begin
        cfa_advance_loc(list);
        al_dwarf.concat(tdwarfitem.create_const(DW_CFA_def_cfa_offset,doe_uleb,ofs));
      end;


begin
{$warning TODO Maybe initialize per module}
  dwarfcfi:=tdwarfcfi.create;
end.
