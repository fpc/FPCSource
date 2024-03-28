{
    Copyright (c) 1998-2022 by the Free Pascal development team

    AArch64 version of some node tree helper routines

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
unit naarch64util;

{$i fpcdefs.inc}

interface

uses
    cclasses, ngenutil, fmodule;

type
    TAArch64NodeUtils = class(TNodeUtils)
        class procedure InsertObjectInfo; override;
        class procedure Insert_Init_Final_Table(main: tmodule; Entries: TFPList); override;
    end;

implementation

uses
    verbose,
    systems,
    globals,
    cpuinfo,cpubase,
    cgbase,cgutils,
    aasmbase,aasmdata,aasmtai,aasmcpu,
    symdef;

class procedure TAArch64NodeUtils.InsertObjectInfo;
begin
    inherited InsertObjectInfo;
end;

{
    TODO: This is a simple skeleton, not nearly as complex as the
    ARM (32-bit) version in compiler/arm/narmutil.pas
}
class procedure TAArch64NodeUtils.Insert_Init_Final_Table(main: tmodule; Entries: TFPList);

    procedure GenEntry(List: TAsmList);
    var
        Ref: TReference;
        pairreg: TRegister;
        rt: TRegisterType;
        sub: TSubRegister;
        sr: TSuperRegister;
    begin
        { generate `stp x29, x30, [sp, #-16]!` }
        reference_reset_base(ref, NR_SP, -16, ctempposinvalid, 16, []);
        ref.addressmode := AM_PREINDEXED;

        rt := R_INTREGISTER;
        sub := R_SUBWHOLE;

        sr := RS_X29;
        pairreg := newreg(rt, sr, sub);

        sr := RS_X30;
        List.Concat(taicpu.op_reg_reg_ref(A_STP, pairreg, newreg(rt, sr, sub), ref));

        { TODO: generate `mov x29, sp` maybe? }
    end;

    procedure GenExit(List: TAsmList);
    var
        Ref: TReference;
        pairreg: TRegister;
        rt: TRegisterType;
        sub: TSubRegister;
        sr: TSuperRegister;
    begin
        { generate `ldp x29, x30, [sp], #16` }
        reference_reset_base(ref, NR_SP, 16, ctempposinvalid, 16, []);
        ref.addressmode := AM_POSTINDEXED;

        rt := R_INTREGISTER;
        sub := R_SUBWHOLE;

        { these are backwards compared to GenEntry intentionally }
        sr := RS_X30;
        pairreg := newreg(rt, sr, sub);

        sr := RS_X29;
        List.Concat(taicpu.op_reg_reg_ref(A_LDP, newreg(rt, sr, sub), pairreg, ref));

        { generate `ret` }
        List.Concat(taicpu.op_none(A_RET));
    end;

var
    InitList, FinalList, Header: TAsmList;
    Entry : PInitFinalEntry;
    i : longint;
begin
    if not(tf_init_final_units_by_calls in target_info.flags) then
    begin
        inherited insert_init_final_table(main,Entries);
        exit;
    end;

    InitList := TAsmList.Create;
    FinalList := TAsmList.Create;

    GenEntry(finalList);
    GenEntry(initList);

    for i := 0 to Entries.Count - 1 do
    begin
        Entry := PInitFinalEntry(Entries[i]);
        if Entry^.finifunc <> '' then
            finalList.Concat(taicpu.op_sym(A_BL, current_asmdata.RefAsmSymbol(entry^.finifunc, AT_FUNCTION)));
        if Entry^.initfunc <> '' then
            initList.Concat(taicpu.op_sym(A_BL, current_asmdata.RefAsmSymbol(entry^.initfunc, AT_FUNCTION)));
    end;

    GenExit(finalList);
    GenExit(initList);

    Header := TAsmList.Create;
    New_Section(Header, Sec_Code, 'FPC_INIT_FUNC_TABLE', 1);
    Header.Concat(TAI_Symbol.CreateName_Global('FPC_INIT_FUNC_TABLE', AT_FUNCTION, 0, VoidCodePointerType));

    InitList.InsertList(Header);
    Header.Free;

    current_asmdata.AsmLists[al_procedures].concatList(initList);

    Header := TAsmList.Create;
    New_Section(Header, Sec_Code, 'FPC_FINALIZE_FUNC_TABLE', 1);
    Header.Concat(TAI_Symbol.CreateName_Global('FPC_FINALIZE_FUNC_TABLE', AT_FUNCTION, 0, VoidCodePointerType));

    FinalList.InsertList(Header);
    Header.Free;

    current_asmdata.AsmLists[al_procedures].concatList(finalList);

    InitList.Free;
    FinalList.Free;

    inherited Insert_Init_Final_Table(main,entries);
end;

begin
    cnodeutils := TAArch64NodeUtils;
end.
