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
unit cfidwarf;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      globtype,
      cgbase,cpubase,
      aasmbase,aasmcfi,aasmtai,aasmdata;

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
        constructor create(aop:byte);
        constructor create_reg(aop:byte;enc1:tdwarfoperenc;reg:tregister);
        constructor create_const(aop:byte;enc1:tdwarfoperenc;val:int64);
        constructor create_reloffset(aop:byte;enc1:tdwarfoperenc;beginlab,endlab:tasmsymbol);
        constructor create_reg_const(aop:byte;enc1:tdwarfoperenc;reg:tregister;enc2:tdwarfoperenc;val:longint);
        procedure generate_code(list:TAsmList);
      end;

      TDwarfAsmCFI=class(TAsmCFI)
        constructor create;override;
      end;

      TDwarfAsmCFILowLevel=class(TDwarfAsmCFI)
      private
        FDwarfList : TLinkedList;
        FFrameStartLabel,
        FFrameEndLabel,
        FLastloclabel : tasmlabel;
        procedure cfa_advance_loc(list:TAsmList);
        procedure generate_initial_instructions(list:TAsmList);virtual;
      protected
        code_alignment_factor,
        data_alignment_factor : shortint;
        property DwarfList:TlinkedList read FDwarfList;
      public
        constructor create;override;
        destructor destroy;override;
        procedure generate_code(list:TAsmList);override;
        { operations }
        procedure start_frame(list:TAsmList);override;
        procedure end_frame(list:TAsmList);override;
        procedure cfa_offset(list:TAsmList;reg:tregister;ofs:longint);override;
        procedure cfa_restore(list:TAsmList;reg:tregister);override;
        procedure cfa_def_cfa_register(list:TAsmList;reg:tregister);override;
        procedure cfa_def_cfa_offset(list:TAsmList;ofs:longint);override;
      end;


      TDwarfAsmCFIHighLevel=class(TDwarfAsmCFILowLevel)
      public
        procedure generate_code(list:TAsmList);override;

        { operations }
        procedure start_frame(list:TAsmList);override;
        procedure end_frame(list:TAsmList);override;
        procedure cfa_offset(list:TAsmList;reg:tregister;ofs:longint);override;
        procedure cfa_restore(list:TAsmList;reg:tregister);override;
        procedure cfa_def_cfa_register(list:TAsmList;reg:tregister);override;
        procedure cfa_def_cfa_offset(list:TAsmList;ofs:longint);override;
      end;

implementation

    uses
      systems,
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

      DW_LNS_copy            = $01;
      DW_LNS_advance_pc      = $02;
      DW_LNS_advance_line    = $03;
      DW_LNS_set_file        = $04;
      DW_LNS_set_column      = $05;
      DW_LNS_negate_stmt     = $06;
      DW_LNS_set_basic_block = $07;
      DW_LNS_const_add_pc    = $08;

      DW_LNS_fixed_advance_pc   = $09;
      DW_LNS_set_prologue_end   = $0a;
      DW_LNS_set_epilogue_begin = $0b;
      DW_LNS_set_isa            = $0c;

      DW_LNE_end_sequence = $01;
      DW_LNE_set_address  = $02;
      DW_LNE_define_file  = $03;
      DW_LNE_lo_user      = $80;
      DW_LNE_hi_user      = $ff;


{****************************************************************************
                                TDWARFITEM
****************************************************************************}

    constructor tdwarfitem.create(aop:byte);
      begin
        inherited create;
        op:=aop;
        ops:=0;
      end;


    constructor tdwarfitem.create_reg(aop:byte;enc1:tdwarfoperenc;reg:tregister);
      begin
        inherited create;
        op:=aop;
        ops:=1;
        oper[0].typ:=dop_reg;
        oper[0].enc:=enc1;
        oper[0].register:=reg;
      end;


    constructor tdwarfitem.create_const(aop:byte;enc1:tdwarfoperenc;val:int64);
      begin
        inherited create;
        op:=aop;
        ops:=1;
        oper[0].typ:=dop_const;
        oper[0].enc:=enc1;
        oper[0].value:=val;
      end;


    constructor tdwarfitem.create_reloffset(aop:byte;enc1:tdwarfoperenc;beginlab,endlab:tasmsymbol);
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


    constructor tdwarfitem.create_reg_const(aop:byte;enc1:tdwarfoperenc;reg:tregister;enc2:tdwarfoperenc;val:longint);
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


    procedure tdwarfitem.generate_code(list:TAsmList);
      const
        enc2ait_const : array[tdwarfoperenc] of taiconst_type = (
          aitconst_uleb128bit,aitconst_sleb128bit,aitconst_ptr,
          aitconst_32bit,aitconst_16bit,aitconst_8bit
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
                list.concat(tai_const.create(enc2ait_const[oper[i].enc],dwarf_reg(oper[i].register)));
              else
                internalerror(200404128);
            end;
          end;
      end;


{****************************************************************************
                                 TDwarfAsmCFI
****************************************************************************}

    constructor TDwarfAsmCFI.create;
      begin
        inherited;
      end;



{****************************************************************************
                             TDwarfAsmCFILowLevel
****************************************************************************}

    constructor TDwarfAsmCFILowLevel.create;
      begin
        inherited create;
        FFrameStartLabel:=nil;
        FFrameEndLabel:=nil;
        FLastLocLabel:=nil;
        code_alignment_factor:=1;
        data_alignment_factor:=-4;
        FDwarfList:=TLinkedList.Create;
      end;


    destructor TDwarfAsmCFILowLevel.destroy;
      begin
        FDwarfList.Free;
      end;


{$if defined(i386)}
    { if more cpu dependend stuff is implemented, this needs more refactoring }
    procedure TDwarfAsmCFILowLevel.generate_initial_instructions(list:TAsmList);
      begin
        list.concat(tai_const.create_8bit(DW_CFA_def_cfa));
        list.concat(tai_const.create_uleb128bit(dwarf_reg(NR_STACK_POINTER_REG)));
        list.concat(tai_const.create_uleb128bit(sizeof(aint)));
        list.concat(tai_const.create_8bit(DW_CFA_offset_extended));
        list.concat(tai_const.create_uleb128bit(dwarf_reg(NR_RETURN_ADDRESS_REG)));
        list.concat(tai_const.create_uleb128bit((-sizeof(aint)) div data_alignment_factor));
      end;
{$elseif defined(avr)}
    procedure TDwarfAsmCFILowLevel.generate_initial_instructions(list:TAsmList);
      begin
        list.concat(tai_const.create_8bit(DW_CFA_def_cfa));
        list.concat(tai_const.create_uleb128bit(32));
        list.concat(tai_const.create_uleb128bit(2));
        list.concat(tai_const.create_8bit(DW_CFA_offset_extended));
        list.concat(tai_const.create_uleb128bit(36));
        list.concat(tai_const.create_uleb128bit((-1) div data_alignment_factor));
      end;
{$else}
    { if more cpu dependend stuff is implemented, this needs more refactoring }
    procedure TDwarfAsmCFILowLevel.generate_initial_instructions(list:TAsmList);
      begin
        list.concat(tai_const.create_8bit(DW_CFA_def_cfa));
        list.concat(tai_const.create_uleb128bit(dwarf_reg(NR_STACK_POINTER_REG)));
        list.concat(tai_const.create_uleb128bit(sizeof(aint)));
        list.concat(tai_const.create_8bit(DW_CFA_offset_extended));
        list.concat(tai_const.create_uleb128bit(dwarf_reg(NR_RETURN_ADDRESS_REG)));
        list.concat(tai_const.create_uleb128bit((-sizeof(aint)) div data_alignment_factor));
      end;
{$endif i386}

    procedure TDwarfAsmCFILowLevel.generate_code(list:TAsmList);
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
        current_asmdata.getlabel(cielabel,alt_dbgframe);
        list.concat(tai_label.create(cielabel));
        current_asmdata.getlabel(lenstartlabel,alt_dbgframe);
        current_asmdata.getlabel(lenendlabel,alt_dbgframe);
        list.concat(tai_const.create_rel_sym(aitconst_32bit,lenstartlabel,lenendlabel));
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
        generate_initial_instructions(list);

        list.concat(cai_align.create_zeros(sizeof(pint)));
        list.concat(tai_label.create(lenendlabel));
        lenstartlabel:=nil;
        lenendlabel:=nil;

        hp:=TDwarfItem(DwarfList.first);
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
                  current_asmdata.getlabel(lenstartlabel,alt_dbgframe);
                  current_asmdata.getlabel(lenendlabel,alt_dbgframe);
                  { FDE
                     DWORD length
                     DWORD CIE-pointer = cielabel relative to section start
                     PTRSIZE initial location = oper[0]
                     PTRSIZE function size = oper[1]
                  }
                  list.concat(tai_const.create_rel_sym(aitconst_32bit,lenstartlabel,lenendlabel));
                  list.concat(tai_label.create(lenstartlabel));
                  tc:=tai_const.create_sym(cielabel);
                  { force label offset to secrel32 for windows systems }
                  if (target_info.system in systems_windows+systems_wince) then
                    tc.consttype:=aitconst_secrel32_symbol;
                  list.concat(tc);
                  list.concat(tai_const.create_sym(hp.oper[0].beginsym));
                  list.concat(tai_const.create_rel_sym(aitconst_ptr,hp.oper[0].beginsym,hp.oper[0].endsym));
                end;
              DW_CFA_End_Frame :
                begin
                  list.concat(cai_align.create_zeros(sizeof(pint)));
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
        { DwarfList is processed, remove items }
        DwarfList.Clear;
      end;


    procedure TDwarfAsmCFILowLevel.start_frame(list:TAsmList);
      begin
        if assigned(FFrameStartLabel) then
          internalerror(200404129);
        current_asmdata.getlabel(FFrameStartLabel,alt_dbgframe);
        current_asmdata.getlabel(FFrameEndLabel,alt_dbgframe);
        FLastloclabel:=FFrameStartLabel;
        list.concat(tai_label.create(FFrameStartLabel));
        DwarfList.concat(tdwarfitem.create_reloffset(DW_CFA_start_frame,doe_32bit,FFrameStartLabel,FFrameEndLabel));
      end;


    procedure TDwarfAsmCFILowLevel.end_frame(list:TAsmList);
      begin
        if not assigned(FFrameStartLabel) then
          internalerror(2004041213);
        DwarfList.concat(tdwarfitem.create(DW_CFA_end_frame));
        list.concat(tai_label.create(FFrameEndLabel));
        FFrameStartLabel:=nil;
        FFrameEndLabel:=nil;
        FLastLocLabel:=nil;
      end;


    procedure TDwarfAsmCFILowLevel.cfa_advance_loc(list:TAsmList);
      var
        currloclabel : tasmlabel;
      begin
        if FLastloclabel=nil then
          internalerror(200404082);
        current_asmdata.getlabel(currloclabel,alt_dbgframe);
        list.concat(tai_label.create(currloclabel));
        DwarfList.concat(tdwarfitem.create_reloffset(DW_CFA_advance_loc4,doe_32bit,FLastloclabel,currloclabel));
        FLastloclabel:=currloclabel;
      end;


    procedure TDwarfAsmCFILowLevel.cfa_offset(list:TAsmList;reg:tregister;ofs:longint);
      begin
        cfa_advance_loc(list);
{ TODO: check if ref is a temp}
        { offset must be positive }
        DwarfList.concat(tdwarfitem.create_reg_const(DW_CFA_offset_extended,doe_uleb,reg,doe_uleb,ofs div data_alignment_factor));
      end;


    procedure TDwarfAsmCFILowLevel.cfa_restore(list:TAsmList;reg:tregister);
      begin
        cfa_advance_loc(list);
        DwarfList.concat(tdwarfitem.create_reg(DW_CFA_restore_extended,doe_uleb,reg));
      end;


    procedure TDwarfAsmCFILowLevel.cfa_def_cfa_register(list:TAsmList;reg:tregister);
      begin
        cfa_advance_loc(list);
        DwarfList.concat(tdwarfitem.create_reg(DW_CFA_def_cfa_register,doe_uleb,reg));
      end;


    procedure TDwarfAsmCFILowLevel.cfa_def_cfa_offset(list:TAsmList;ofs:longint);
      begin
        cfa_advance_loc(list);
        DwarfList.concat(tdwarfitem.create_const(DW_CFA_def_cfa_offset,doe_uleb,ofs));
      end;


{****************************************************************************
                             TDwarfAsmCFILowLevel
****************************************************************************}


    procedure TDwarfAsmCFIHighLevel.generate_code(list: TAsmList);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
      end;


    procedure TDwarfAsmCFIHighLevel.start_frame(list: TAsmList);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
        list.concat(tai_cfi_op_none.create(cfi_startproc));
      end;


    procedure TDwarfAsmCFIHighLevel.end_frame(list: TAsmList);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
        list.concat(tai_cfi_op_none.create(cfi_endproc));
      end;


    procedure TDwarfAsmCFIHighLevel.cfa_offset(list: TAsmList; reg: tregister; ofs: longint);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
        list.concat(tai_cfi_op_reg_val.create(cfi_offset,reg,ofs));
      end;


    procedure TDwarfAsmCFIHighLevel.cfa_restore(list: TAsmList; reg: tregister);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
        list.concat(tai_cfi_op_reg.create(cfi_restore,reg));
      end;


    procedure TDwarfAsmCFIHighLevel.cfa_def_cfa_register(list: TAsmList; reg: tregister);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
        list.concat(tai_cfi_op_reg.create(cfi_def_cfa_register,reg));
      end;


    procedure TDwarfAsmCFIHighLevel.cfa_def_cfa_offset(list: TAsmList; ofs: longint);
      begin
        if not(tf_use_hlcfi in target_info.flags) then
          begin
            inherited;
            exit;
          end;
        list.concat(tai_cfi_op_val.create(cfi_def_cfa_offset,ofs));
      end;


begin
  CAsmCFI:=TDwarfAsmCFIHighLevel;
end.
