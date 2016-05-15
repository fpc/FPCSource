{

  Copyright (C) <avx-testfile-generator> <Torsten Grundke>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}

unit asmtestgenerator;

interface

uses BaseList, Classes;

type
  TOpType = (otUnknown, otXMMReg, otXMMRM, otXMMRM16, otXMMRM8, otYMMReg, otYMMRM, otEAX, otRAX, otMem32,
             otMem8, otMem16, otMem64, otMem128, otMem256, otREG64, otREG32, otRM32, otRM64, otIMM8,
             otXMEM32, otXMEM64, otYMEM32, otYMEM64);

  TOperandListItem = class(TObject)
  private
    FOpActive: boolean;
    FOpNumber: integer;
    FOpTyp: TOpType;
    FValues: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property OpNumber: integer read FOpNumber write FOpNumber;
    property OpTyp: TOpType read FOpTyp write FOpTyp;
    property OpActive: boolean read FOpActive write FOpActive;

    property Values: TStringList read FValues;
  end;

  TOperandList = class(TBaseList)
  private
    function GetItems(aIndex: integer): TOperandListItem;

  public
    function Add(aItem: TOperandListItem): integer;

    property Items[aIndex: integer]: TOperandListItem read GetItems;
  end;


  { TAsmTestGenerator }

  TAsmTestGenerator = class(TObject)
  private
    FReg32Base     : TStringList;
    FReg32Index    : TStringList;
    FReg64Base     : TStringList;
    FReg64Index    : TStringList;
    FReg6432Base   : TStringList;
    FReg6432Index  : TStringList;
    FReg32XMMIndex : TStringList;
    FReg32YMMIndex : TStringList;
    FReg64XMMIndex : TStringList;
    FReg64YMMIndex : TStringList;

    Fx64: boolean;

    procedure MemRegBaseIndexCombi(const aPrefix: String; aSLBaseReg, aSLIndexReg, aRList: TStringList);
    procedure VectorMemRegBaseIndexCombi(const aPrefix: String; aSLBaseReg, aSLIndexReg, aRList: TStringList);

    function InternalCalcTestData(const aInst, aOp1, aOp2, aOp3, aOp4: String): TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure CalcTestData(aX64: boolean; const aInst, aOp1, aOp2, aOp3, aOp4: String; aSL: TStringList);

    property x64: boolean read Fx64;
  end;

implementation

uses SysUtils, Dialogs;

{ TOperandListItem }

constructor TOperandListItem.Create;
begin
  inherited;

  FOpActive := false;
  FOpNumber := -1;
  FOpTyp    := otUnknown;
  FValues := TStringList.Create;
end;

destructor TOperandListItem.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

{ TOperandList }

function TOperandList.Add(aItem: TOperandListItem): integer;
begin
  result := FList.Add(aItem);
end;

function TOperandList.GetItems(aIndex: integer): TOperandListItem;
begin
  result := TOperandListItem(FList[aIndex]);
end;

{ TAsmTestGenerator }

function TAsmTestGenerator.InternalCalcTestData(const aInst, aOp1, aOp2, aOp3,
  aOp4: String): TStringList;
var
  Item: TOperandListItem;
  OItem1: TOperandListItem;
  OItem2: TOperandListItem;
  OItem3: TOperandListItem;
  OItem4: TOperandListItem;

  il_Op: integer;
  il_Op1: integer;
  il_Op2: integer;
  il_Op3: integer;
  il_Op4: integer;

  sl_Operand: String;
  sl_Inst   : String;
  sl_RegCombi: String;
  sl_Prefix: String;
  UsePrefix: boolean;
  il_Operands: integer;
  UsedParams: cardinal;
  UseDefault: boolean;
  sl_RegCombi1: string;
  sl_RegCombi2: string;
  sl_RegCombi3: string;

  function PrepareOperandTyp(const aTyp: String): String;
  begin
    result := aTyp;
    if copy(result, length(result), 1) = '*' then result := copy(result, 1, length(result) - 1);
    if result = 'XMMRM128' then result := 'XMMRM';
    if result = 'YMMRM256' then result := 'YMMRM';
  end;


begin
  result := TStringList.Create;

  OItem1 := TOperandListItem.Create;
  try
    OItem2 := TOperandListItem.Create;
    try
      OItem3 := TOperandListItem.Create;
      try
        OItem4 := TOperandListItem.Create;
        try

          UsePrefix := (aInst = 'VCVTPD2DQ') OR
                       (aInst = 'VCVTPD2PS') OR
                       (aInst = 'VCVTSI2SD') OR
                       (aInst = 'VCVTSI2SS') OR
                       (aInst = 'VCVTTPD2DQ');

          for il_Op := 1 to 4 do
          begin
            sl_Prefix := '';

            case il_Op of
              1: begin
                   Item := OItem1;
                   sl_Operand := aOp1;
                 end;
              2: begin
                   Item := OItem2;
                   sl_Operand := aOp2;
                 end;
              3: begin
                   Item := OItem3;
                   sl_Operand := aOp3;
                 end;
              4: begin
                   Item := OItem4;
                   sl_Operand := aOp4;
                 end;
            end;

            sl_Operand := PrepareOperandTyp(sl_Operand);

            if AnsiSameText(sl_Operand, 'XMMREG') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMReg;
              Item.OpActive := true;

              Item.Values.Add('XMM0');
              Item.Values.Add('XMM1');
              Item.Values.Add('XMM2');
              Item.Values.Add('XMM3');
              Item.Values.Add('XMM4');
              Item.Values.Add('XMM5');
              Item.Values.Add('XMM6');
              Item.Values.Add('XMM7');

              if x64 then
              begin
                if aOp4 <> 'XMMREG' then
                begin
                  Item.Values.Add('XMM8');
                  Item.Values.Add('XMM9');
                  Item.Values.Add('XMM10');
                  Item.Values.Add('XMM11');
                  Item.Values.Add('XMM12');
                  Item.Values.Add('XMM13');
                  Item.Values.Add('XMM14');
                  Item.Values.Add('XMM15');
                end
                else
                begin
                  Item.Values.Clear;

                  Item.Values.Add('XMM0');
                  Item.Values.Add('XMM7');
                  Item.Values.Add('XMM8');
                  Item.Values.Add('XMM15');
                end;
              end;
            end
            else if AnsiSameText(sl_Operand, 'XMMRM') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              Item.Values.Add('XMM0');
              Item.Values.Add('XMM1');
              Item.Values.Add('XMM2');
              Item.Values.Add('XMM3');
              Item.Values.Add('XMM4');
              Item.Values.Add('XMM5');
              Item.Values.Add('XMM6');
              Item.Values.Add('XMM7');

              if x64 then
              begin
                Item.Values.Add('XMM8');
                Item.Values.Add('XMM9');
                Item.Values.Add('XMM10');
                Item.Values.Add('XMM11');
                Item.Values.Add('XMM12');
                Item.Values.Add('XMM13');
                Item.Values.Add('XMM14');
                Item.Values.Add('XMM15');

                //Item.Values.Add('[RIP]');
                //Item.Values.Add('[RIP + 16]');

                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'XMMRM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM8;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'byte ';

              Item.Values.Add('XMM0');
              Item.Values.Add('XMM1');
              Item.Values.Add('XMM2');
              Item.Values.Add('XMM3');
              Item.Values.Add('XMM4');
              Item.Values.Add('XMM5');
              Item.Values.Add('XMM6');
              Item.Values.Add('XMM7');

              if x64 then
              begin
                Item.Values.Add('XMM8');
                Item.Values.Add('XMM9');
                Item.Values.Add('XMM10');
                Item.Values.Add('XMM11');
                Item.Values.Add('XMM12');
                Item.Values.Add('XMM13');
                Item.Values.Add('XMM14');
                Item.Values.Add('XMM15');

                //Item.Values.Add('[RIP]');
                //Item.Values.Add('[RIP + 16]');

                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'XMMRM16') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMMRM16;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'word ';

              Item.Values.Add('XMM0');
              Item.Values.Add('XMM1');
              Item.Values.Add('XMM2');
              Item.Values.Add('XMM3');
              Item.Values.Add('XMM4');
              Item.Values.Add('XMM5');
              Item.Values.Add('XMM6');
              Item.Values.Add('XMM7');

              if x64 then
              begin
                Item.Values.Add('XMM8');
                Item.Values.Add('XMM9');
                Item.Values.Add('XMM10');
                Item.Values.Add('XMM11');
                Item.Values.Add('XMM12');
                Item.Values.Add('XMM13');
                Item.Values.Add('XMM14');
                Item.Values.Add('XMM15');

                //Item.Values.Add('[RIP]');
                //Item.Values.Add('[RIP + 16]');

                MemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'YMMREG') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMReg;
              Item.OpActive := true;

              Item.Values.Add('YMM0');
              Item.Values.Add('YMM1');
              Item.Values.Add('YMM2');
              Item.Values.Add('YMM3');
              Item.Values.Add('YMM4');
              Item.Values.Add('YMM5');
              Item.Values.Add('YMM6');
              Item.Values.Add('YMM7');

              if x64 then
              begin
                if aOp4 <> 'YMMREG' then
                begin
                  Item.Values.Add('YMM8');
                  Item.Values.Add('YMM9');
                  Item.Values.Add('YMM10');
                  Item.Values.Add('YMM11');
                  Item.Values.Add('YMM12');
                  Item.Values.Add('YMM13');
                  Item.Values.Add('YMM14');
                  Item.Values.Add('YMM15');
                end
                else
                begin
                  Item.Values.Clear;

                  Item.Values.Add('YMM0');
                  Item.Values.Add('YMM7');
                  Item.Values.Add('YMM8');
                  Item.Values.Add('YMM15');
                end;
              end;
            end
            else if AnsiSameText(sl_Operand, 'YMMRM') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMMRM;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              Item.Values.Add('YMM0');
              Item.Values.Add('YMM1');
              Item.Values.Add('YMM2');
              Item.Values.Add('YMM3');
              Item.Values.Add('YMM4');
              Item.Values.Add('YMM5');
              Item.Values.Add('YMM6');
              Item.Values.Add('YMM7');

              if x64 then
              begin
                Item.Values.Add('YMM8');
                Item.Values.Add('YMM9');
                Item.Values.Add('YMM10');
                Item.Values.Add('YMM11');
                Item.Values.Add('YMM12');
                Item.Values.Add('YMM13');
                Item.Values.Add('YMM14');
                Item.Values.Add('YMM15');

                MemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg32Base, FReg32Index, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'MEM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM8;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'byte ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM16') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM16;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'word ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_Prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'dword ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'qword ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM128') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM128;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'MEM256') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otMEM256;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'REG32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG32;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg32Base);
              end
              else Item.Values.AddStrings(FReg32Base);
            end
            else if AnsiSameText(sl_Operand, 'REG64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otREG64;
              Item.OpActive := true;

              if x64 then
              begin
                Item.Values.AddStrings(FReg64Base);
              end;
            end
            else if AnsiSameText(sl_Operand, 'RM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;

              Item.Values.AddStrings(FReg32Base);

              if UsePrefix then sl_Prefix := 'dword ';

              if x64 then
              begin
                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'RM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otRM32;
              Item.OpActive := true;



              if UsePrefix then sl_Prefix := 'qword ';

              if x64 then
              begin
                Item.Values.AddStrings(FReg64Base);
                MemRegBaseIndexCombi(sl_Prefix, FReg64Base, FReg64Index, Item.Values);
                //MemRegBaseIndexCombi(FReg6432Base, FReg6432Index, Item.Values);
              end
              else MemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32Index, Item.Values);
            end
            else if AnsiSameText(sl_Operand, 'IMM8') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otIMM8;
              Item.OpActive := true;

              Item.Values.Add('0');
            end
            else if AnsiSameText(sl_Operand, 'XMEM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64XMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32XMMIndex, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'XMEM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otXMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'oword ';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64XMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32XMMIndex, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'YMEM32') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM32;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64YMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32YMMIndex, Item.Values);
              end;
            end
            else if AnsiSameText(sl_Operand, 'YMEM64') then
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otYMEM64;
              Item.OpActive := true;

              if UsePrefix then sl_Prefix := 'yword ';

              if x64 then
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg64Base, FReg64YMMIndex, Item.Values);
              end
              else
              begin
                VectorMemRegBaseIndexCombi(sl_prefix, FReg32Base, FReg32YMMIndex, Item.Values);
              end;
            end


            else
            begin
              Item.OpNumber := il_Op;
              Item.OpTyp    := otUnknown;
              Item.OpActive := false;

              Item.Values.Add('');
            end

          end;

          sl_RegCombi := '';


          il_Operands := 0;
          UsedParams  := 0;

          if OItem1.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 1;
          end;

          if OItem2.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 2;
          end;

          if OItem3.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 4;
          end;

          if OItem4.OpActive then
          begin
            inc(il_Operands);
            UsedParams := UsedParams or 8;
          end;

          case il_Operands of
              1: UseDefault := UsedParams <> 1;
              2: UseDefault := UsedParams <> 3;
              3: UseDefault := UsedParams <> 7;
              4: UseDefault := UsedParams <> 15;
            else UseDefault := true;
          end;

          //UseDefault := true;

          if UseDefault then
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    sl_RegCombi := '';

                    if OItem1.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem1.Values[il_Op1];
                    end;

                    if OItem2.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem2.Values[il_Op2];
                    end;

                    if OItem3.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem3.Values[il_Op3];
                    end;

                    if OItem4.OpActive then
                    begin
                      if sl_RegCombi <> '' then sl_RegCombi := sl_RegCombi + ', ';
                      sl_RegCombi := sl_RegCombi + OItem4.Values[il_Op4];
                    end;

                    if sl_RegCombi <> '' then
                    begin
                      //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                      result.Add(sl_Inst + sl_RegCombi);
                      sl_RegCombi := '';
                    end;
                  end;
                end;
              end;
            end;
          end
          else
          begin
            sl_Inst := format('%-20s', [aInst]);

            for il_Op1 := 0 to OItem1.Values.Count - 1 do
            begin
              if OItem1.OpActive then
              begin
                sl_RegCombi1 := OItem1.Values[il_Op1];
              end
              else sl_RegCombi1 := '';

              for il_Op2 := 0 to OItem2.Values.Count - 1 do
              begin
                if OItem2.OpActive then
                begin
                  sl_RegCombi2 := sl_RegCombi1 + ', ' + OItem2.Values[il_Op2];
                end
                else sl_RegCombi2 := sl_RegCombi1;

                for il_Op3 := 0 to OItem3.Values.Count - 1 do
                begin
                  if OItem3.OpActive then
                  begin
                    sl_RegCombi3 := sl_RegCombi2 + ', ' + OItem3.Values[il_Op3];
                  end
                  else sl_RegCombi3 := sl_RegCombi2;

                  for il_Op4 := 0 to OItem4.Values.Count - 1 do
                  begin
                    if OItem4.OpActive then
                    begin
                      sl_RegCombi := sl_RegCombi3 + ', ' + OItem4.Values[il_Op4];
                    end
                    else sl_RegCombi := sl_RegCombi3;

                    if sl_RegCombi <> '' then
                    begin
                      //result.Add(format('%-20s%s', [aInst, sl_RegCombi]));
                      result.Add(sl_Inst + sl_RegCombi);
                      sl_RegCombi := '';
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          FreeAndNil(OItem4);
        end;
      finally
        FreeAndNil(OItem3);
      end;
    finally
      FreeAndNil(OItem2);
    end;
  finally
    FreeAndNil(OItem1);
  end;
end;

constructor TAsmTestGenerator.Create;
begin
  inherited;

  FX64 := true;

  FReg32Base     := TStringList.Create;
  FReg32Index    := TStringList.Create;
  FReg64Base     := TStringList.Create;
  FReg64Index    := TStringList.Create;
  FReg6432Base   := TStringList.Create;
  FReg6432Index  := TStringList.Create;
  FReg32XMMIndex := TStringList.Create;
  FReg32YMMIndex := TStringList.Create;
  FReg64XMMIndex := TStringList.Create;
  FReg64YMMIndex := TStringList.Create;


  FReg32Base.Add('EAX');
  FReg32Base.Add('EBX');
  FReg32Base.Add('ECX');
  FReg32Base.Add('EDX');
  FReg32Base.Add('ESP');
  FReg32Base.Add('EBP');
  FReg32Base.Add('EDI');
  FReg32Base.Add('ESI');


  FReg32Index.Add('EAX');
  FReg32Index.Add('EBX');
  FReg32Index.Add('ECX');
  FReg32Index.Add('EDX');
  FReg32Index.Add('EBP');
  FReg32Index.Add('EDI');
  FReg32Index.Add('ESI');


  FReg64Base.Add('RAX');
  FReg64Base.Add('RBX');
  FReg64Base.Add('RCX');
  FReg64Base.Add('RDX');
  FReg64Base.Add('RSP');
  FReg64Base.Add('RBP');
  FReg64Base.Add('RDI');
  FReg64Base.Add('RSI');
  FReg64Base.Add('R8');
  FReg64Base.Add('R9');
  FReg64Base.Add('R10');
  FReg64Base.Add('R11');
  FReg64Base.Add('R12');
  FReg64Base.Add('R13');
  FReg64Base.Add('R14');
  FReg64Base.Add('R15');

  FReg64Index.Add('RAX');
  FReg64Index.Add('RBX');
  FReg64Index.Add('RCX');
  FReg64Index.Add('RDX');
  FReg64Index.Add('RBP');
  FReg64Index.Add('RDI');
  FReg64Index.Add('RSI');
  FReg64Index.Add('R8');
  FReg64Index.Add('R9');
  FReg64Index.Add('R10');
  FReg64Index.Add('R11');
  FReg64Index.Add('R12');
  FReg64Index.Add('R13');
  FReg64Index.Add('R14');
  FReg64Index.Add('R15');

  FReg6432Base.Add('EAX');
  FReg6432Base.Add('EBX');
  FReg6432Base.Add('ECX');
  FReg6432Base.Add('EDX');
  FReg6432Base.Add('ESP');
  FReg6432Base.Add('EBP');
  FReg6432Base.Add('EDI');
  FReg6432Base.Add('ESI');
  FReg6432Base.Add('R8D');
  FReg6432Base.Add('R9D');
  FReg6432Base.Add('R10D');
  FReg6432Base.Add('R11D');
  FReg6432Base.Add('R12D');
  FReg6432Base.Add('R13D');
  FReg6432Base.Add('R14D');
  FReg6432Base.Add('R15D');

  FReg6432Index.Add('EAX');
  FReg6432Index.Add('EBX');
  FReg6432Index.Add('ECX');
  FReg6432Index.Add('EDX');
  FReg6432Index.Add('EBP');
  FReg6432Index.Add('EDI');
  FReg6432Index.Add('ESI');
  FReg6432Index.Add('R8D');
  FReg6432Index.Add('R9D');
  FReg6432Index.Add('R10D');
  FReg6432Index.Add('R11D');
  FReg6432Index.Add('R12D');
  FReg6432Index.Add('R13D');
  FReg6432Index.Add('R14D');
  FReg6432Index.Add('R15D');

  FReg32XMMIndex.ADD('XMM0');
  FReg32XMMIndex.ADD('XMM1');
  FReg32XMMIndex.ADD('XMM2');
  FReg32XMMIndex.ADD('XMM3');
  FReg32XMMIndex.ADD('XMM4');
  FReg32XMMIndex.ADD('XMM5');
  FReg32XMMIndex.ADD('XMM6');
  FReg32XMMIndex.ADD('XMM7');

  FReg32YMMIndex.ADD('YMM0');
  FReg32YMMIndex.ADD('YMM1');
  FReg32YMMIndex.ADD('YMM2');
  FReg32YMMIndex.ADD('YMM3');
  FReg32YMMIndex.ADD('YMM4');
  FReg32YMMIndex.ADD('YMM5');
  FReg32YMMIndex.ADD('YMM6');
  FReg32YMMIndex.ADD('YMM7');

  FReg64XMMIndex.ADD('XMM0');
  FReg64XMMIndex.ADD('XMM1');
  FReg64XMMIndex.ADD('XMM2');
  FReg64XMMIndex.ADD('XMM3');
  FReg64XMMIndex.ADD('XMM4');
  FReg64XMMIndex.ADD('XMM5');
  FReg64XMMIndex.ADD('XMM6');
  FReg64XMMIndex.ADD('XMM7');
  FReg64XMMIndex.ADD('XMM8');
  FReg64XMMIndex.ADD('XMM9');
  FReg64XMMIndex.ADD('XMM10');
  FReg64XMMIndex.ADD('XMM11');
  FReg64XMMIndex.ADD('XMM12');
  FReg64XMMIndex.ADD('XMM13');
  FReg64XMMIndex.ADD('XMM14');
  FReg64XMMIndex.ADD('XMM15');


  FReg64YMMIndex.ADD('YMM0');
  FReg64YMMIndex.ADD('YMM1');
  FReg64YMMIndex.ADD('YMM2');
  FReg64YMMIndex.ADD('YMM3');
  FReg64YMMIndex.ADD('YMM4');
  FReg64YMMIndex.ADD('YMM5');
  FReg64YMMIndex.ADD('YMM6');
  FReg64YMMIndex.ADD('YMM7');
  FReg64YMMIndex.ADD('YMM8');
  FReg64YMMIndex.ADD('YMM9');
  FReg64YMMIndex.ADD('YMM10');
  FReg64YMMIndex.ADD('YMM11');
  FReg64YMMIndex.ADD('YMM12');
  FReg64YMMIndex.ADD('YMM13');
  FReg64YMMIndex.ADD('YMM14');
  FReg64YMMIndex.ADD('YMM15');

end;

destructor TAsmTestGenerator.Destroy;
begin
  FreeAndNil(FReg32Base);
  FreeAndNil(FReg32Index);
  FreeAndNil(FReg64Base);
  FreeAndNil(FReg64Index);
  FreeAndNil(FReg6432Base);
  FreeAndNil(FReg6432Index);

  FreeAndNil(FReg32XMMIndex);
  FreeAndNil(FReg32YMMIndex);
  FreeAndNil(FReg64XMMIndex);
  FreeAndNil(FReg64YMMIndex);

  inherited;
end;

procedure TAsmTestGenerator.MemRegBaseIndexCombi(const aPrefix: String; aSLBaseReg,
  aSLIndexReg, aRList: TStringList);
var
  il_Base: integer;
  il_Index: integer;
begin

  for il_Base := 0 to aSLBaseReg.Count - 1 do
  begin
    aRList.Add(format(aPrefix + '[%s]', [aSLBaseReg[il_Base]]));

    for il_Index := 0 to aSLIndexReg.Count - 1 do
    begin
      aRList.Add(format(aPrefix + '[%s + %s]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));

      aRList.Add(format(aPrefix + '[%s + %s * 2]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 4]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 8]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));

      aRList.Add(format(aPrefix + '[%s + %s * 2 + 16]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 4 + 32]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 8 + 48]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
    end;
  end;
end;

procedure TAsmTestGenerator.VectorMemRegBaseIndexCombi(const aPrefix: String;
  aSLBaseReg, aSLIndexReg, aRList: TStringList);
var
  il_Base: integer;
  il_Index: integer;
begin

  //for il_Index := 0 to aSLIndexReg.Count - 1 do
  //begin
  //  aRList.Add(format(aPrefix + '[%s]', [aSLIndexReg[il_Index]]));
  //
  //  aRList.Add(format(aPrefix + '[%s * 2]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 4]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 8]', [aSLIndexReg[il_Index]]));
  //
  //  aRList.Add(format(aPrefix + '[%s * 2 + 16]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 4 + 32]', [aSLIndexReg[il_Index]]));
  //  aRList.Add(format(aPrefix + '[%s * 8 + 48]', [aSLIndexReg[il_Index]]));
  //end;


  for il_Base := 0 to aSLBaseReg.Count - 1 do
  begin
    //aRList.Add(format(aPrefix + '[%s]', [aSLBaseReg[il_Base]]));

    for il_Index := 0 to aSLIndexReg.Count - 1 do
    begin
      aRList.Add(format(aPrefix + '[%s + %s]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));

      aRList.Add(format(aPrefix + '[%s + %s * 2]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 4]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 8]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));

      aRList.Add(format(aPrefix + '[%s + %s * 2 + 16]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 4 + 32]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));
      aRList.Add(format(aPrefix + '[%s + %s * 8 + 48]', [aSLBaseReg[il_Base], aSLIndexReg[il_Index]]));


      aRList.Add(format(aPrefix + '[%s + %s]', [aSLIndexReg[il_Index], aSLBaseReg[il_Base]]));

      aRList.Add(format(aPrefix + '[%s + %s + 16]', [aSLIndexReg[il_Index], aSLBaseReg[il_Base]]));
    end;
  end;
end;

class procedure TAsmTestGenerator.CalcTestData(aX64: boolean; const aInst, aOp1, aOp2, aOp3,
  aOp4: String; aSL: TStringList);
var
  sl: TStringList;
begin
  with TAsmTestGenerator.Create do
  try
    Fx64 := aX64;

    sl := InternalCalcTestData(aInst, aOp1, aOp2, aOp3, aOp4);
    try
      aSL.AddStrings(sl);
    finally
      FreeAndNil(sl);
    end;
  finally
    Free;
  end;
end;

end.
