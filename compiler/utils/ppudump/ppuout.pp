{
    Copyright (c) 2013 by Yury Sidorov and the FPC Development Team

    Base classes for a custom output of a PPU File

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

 ****************************************************************************}

unit ppuout;
{$mode objfpc}{$H+}

interface

uses SysUtils, Classes;

type
  TPpuDefType = (dtNone, dtUnit, dtClass, dtRecord, dtProc, dtField, dtProp, dtParam, dtVar,
                 dtType, dtConst, dtProcType, dtEnum, dtSet);

  TPpuDef = class;
  TPpuContainerDef = class;

  { TPpuOutput }

  TPpuOutput = class
  private
    FOutFile: ^Text;
    FIndent: integer;
    FIndentSize: integer;
    FIndStr: string;
    FNoIndent: boolean;
    procedure SetIndent(AValue: integer);
    procedure SetIndentSize(AValue: integer);
  protected
    procedure WriteDefStart(Def: TPpuDef); virtual;
    procedure WriteDefEnd(Def: TPpuDef); virtual;
    procedure WriteSubItemsStart(Def: TPpuContainerDef); virtual;
    procedure WriteSubItemsEnd(Def: TPpuContainerDef); virtual;
    procedure WriteStr(const AName, AValue: string); virtual;
    procedure WriteInt(const AName: string; AValue: Int64); virtual;
    procedure WriteFloat(const AName: string; AValue: extended); virtual;
    procedure WriteBool(const AName: string; AValue: boolean); virtual;
    procedure WriteArrayStart(const AName: string); virtual;
    procedure WriteArrayEnd(const AName: string); virtual;
  public
    constructor Create(var OutFile: Text); virtual;
    destructor Destroy; override;
    procedure Write(const s: string);
    procedure WriteLn(const s: string = '');
    procedure IncI; virtual;
    procedure DecI; virtual;
    property Indent: integer read FIndent write SetIndent;
    property IndentSize: integer read FIndentSize write SetIndentSize;
  end;

  { TPpuDef }

  TPpuDef = class
  private
    FParent: TPpuContainerDef;
    function GetDefTypeName: string;
    procedure SetProps(AValue: TStringList);

  protected
    procedure WriteDef(Output: TPpuOutput); virtual;

  public
    DefType: TPpuDefType;
    Name: string;
    DefId: integer;

    constructor Create(AParent: TPpuContainerDef); virtual; reintroduce;
    destructor Destroy; override;
    procedure Write(Output: TPpuOutput);
    property Parent: TPpuContainerDef read FParent;
    property DefTypeName: string read GetDefTypeName;
  end;

  { TPpuContainerDef }

  TPpuContainerDef = class(TPpuDef)
  private
    FItems: TList;
    function GetCount: integer;
    function GetItem(Index: Integer): TPpuDef;
    procedure SetItem(Index: Integer; AValue: TPpuDef);

  protected
    procedure WriteDef(Output: TPpuOutput); override;

  public
    ItemsName: string;

    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
    function Add(Def: TPpuDef): integer;
    property Items[Index: Integer]: TPpuDef read GetItem write SetItem; default;
    property Count: integer read GetCount;
  end;

  { TPpuUnitDef }

  TPpuUnitDef = class(TPpuContainerDef)
  protected
    procedure WriteDef(Output: TPpuOutput); override;
  public
    Version: cardinal;
    Crc, IntfCrc: cardinal;
    TargetOS, TargetCPU: string;
    UsedUnits: TPpuContainerDef;
    RefUnits: array of string;
    SourceFiles: TPpuContainerDef;
    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
  end;

  { TPpuSrcFile }

  TPpuSrcFile = class(TPpuDef)
  protected
    procedure WriteDef(Output: TPpuOutput); override;
  public
    FileTime: TDateTime;
  end;

implementation

const
  DefTypeNames: array[TPpuDefType] of string =
    ('', 'unit', 'class', 'record', 'procedure', 'field', 'property', 'parameter', 'variable',
     'type', 'constant', 'proctype', 'enum', 'set');

{ TPpuSrcFile }

procedure TPpuSrcFile.WriteDef(Output: TPpuOutput);
begin
  inherited WriteDef(Output);
  Output.WriteStr('Time', FormatDateTime('yyyy"-"mm"-"dd hh":"nn":"ss', FileTime));
end;

{ TPpuOutput }

procedure TPpuOutput.SetIndent(AValue: integer);
begin
  if FIndent=AValue then Exit;
  FIndent:=AValue;
  if FIndent < 0 then
    FIndent:=0;
  SetLength(FIndStr, FIndent*IndentSize);
  if FIndent > 0 then
    FillChar(FIndStr[1], FIndent*IndentSize, ' ');
end;

procedure TPpuOutput.SetIndentSize(AValue: integer);
begin
  if FIndentSize=AValue then Exit;
  FIndentSize:=AValue;
end;

procedure TPpuOutput.WriteDefStart(Def: TPpuDef);
begin
end;

procedure TPpuOutput.WriteDefEnd(Def: TPpuDef);
begin
end;

procedure TPpuOutput.WriteSubItemsStart(Def: TPpuContainerDef);
begin
end;

procedure TPpuOutput.WriteSubItemsEnd(Def: TPpuContainerDef);
begin
end;

procedure TPpuOutput.WriteStr(const AName, AValue: string);
begin
end;

procedure TPpuOutput.WriteInt(const AName: string; AValue: Int64);
begin
  WriteStr(AName, IntToStr(AValue));
end;

procedure TPpuOutput.WriteFloat(const AName: string; AValue: extended);
var
  s: string;
begin
  Str(AValue, s);
  WriteStr(AName, s);
end;

procedure TPpuOutput.WriteBool(const AName: string; AValue: boolean);
var
  s: string;
begin
  if AValue then
    s:='1'
  else
    s:='0';
  WriteStr(AName, s);
end;

procedure TPpuOutput.WriteArrayStart(const AName: string);
begin
end;

procedure TPpuOutput.WriteArrayEnd(const AName: string);
begin
end;

constructor TPpuOutput.Create(var OutFile: Text);
begin
  FOutFile:=@OutFile;
  FIndentSize:=2;
end;

destructor TPpuOutput.Destroy;
begin
  inherited Destroy;
end;

procedure TPpuOutput.Write(const s: string);
begin
  if not FNoIndent then
    System.Write(FOutFile^, FIndStr);
  System.Write(FOutFile^, s);
  FNoIndent:=True;
end;

procedure TPpuOutput.WriteLn(const s: string);
begin
  Self.Write(s + LineEnding);
  FNoIndent:=False;
end;

procedure TPpuOutput.IncI;
begin
  Indent:=Indent + 1;
end;

procedure TPpuOutput.DecI;
begin
  Indent:=Indent - 1;
end;

{ TPpuUnitDef }

procedure TPpuUnitDef.WriteDef(Output: TPpuOutput);
var
  i: integer;
begin
  with Output do begin
    if Version <> 0 then
      WriteInt('Version', Version);
    if TargetCPU <> '' then
      WriteStr('TargetCPU', TargetCPU);
    if TargetOS <> '' then
      WriteStr('TargetOS', TargetOS);
    if Crc <> 0 then
      WriteStr('Crc', hexStr(Crc, 8));
    if IntfCrc <> 0 then
      WriteStr('InterfaceCrc', hexStr(IntfCrc, 8));
    UsedUnits.WriteDef(Output);
    if Length(RefUnits) > 0 then begin
      WriteArrayStart('RefUnits');
      for i:=0 to High(RefUnits) do
        WriteStr('', RefUnits[i]);
      WriteArrayEnd('RefUnits');
    end;
    SourceFiles.WriteDef(Output);
  end;
  inherited WriteDef(Output);
end;

constructor TPpuUnitDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtUnit;
  UsedUnits:=TPpuContainerDef.Create(nil);
  UsedUnits.FParent:=Self;
  UsedUnits.ItemsName:='UsedUnits';
  SourceFiles:=TPpuContainerDef.Create(nil);
  SourceFiles.FParent:=Self;
  SourceFiles.ItemsName:='SrcFiles';
end;

destructor TPpuUnitDef.Destroy;
begin
  UsedUnits.Free;
  SourceFiles.Free;
  inherited Destroy;
end;

{ TPpuContainerDef }

function TPpuContainerDef.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TPpuContainerDef.GetItem(Index: Integer): TPpuDef;
begin
  Result:=TPpuDef(FItems[Index]);
end;

procedure TPpuContainerDef.SetItem(Index: Integer; AValue: TPpuDef);
begin
  FItems[Index]:=AValue;
end;

procedure TPpuContainerDef.WriteDef(Output: TPpuOutput);
var
  i: integer;
begin
  inherited WriteDef(Output);
  if Count = 0 then
    exit;
  Output.WriteSubItemsStart(Self);
  if Parent <> nil then
    Output.IncI;
  for i:=0 to Count - 1 do
    Items[i].Write(Output);
  if Parent <> nil then
    Output.DecI;
  Output.WriteSubItemsEnd(Self);
end;

constructor TPpuContainerDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  FItems:=TList.Create;
  ItemsName:='Contents';
end;

destructor TPpuContainerDef.Destroy;
var
  i: integer;
begin
  for i:=0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Free;
  inherited Destroy;
end;

function TPpuContainerDef.Add(Def: TPpuDef): integer;
begin
  Result:=FItems.Add(Def);
  Def.FParent:=Self;
end;

{ TPpuDef }

function TPpuDef.GetDefTypeName: string;
begin
  Result:=DefTypeNames[DefType];
end;

procedure TPpuDef.SetProps(AValue: TStringList);
begin

end;

procedure TPpuDef.WriteDef(Output: TPpuOutput);
begin
  with Output do begin
    if DefId >= 0 then
      WriteInt('Id', DefId);
  end;
end;

constructor TPpuDef.Create(AParent: TPpuContainerDef);
begin
  DefId:=-1;
  if AParent <> nil then
    AParent.Add(Self);
end;

destructor TPpuDef.Destroy;
begin
  inherited Destroy;
end;

procedure TPpuDef.Write(Output: TPpuOutput);
begin
  Output.WriteDefStart(Self);
  WriteDef(Output);
  Output.WriteDefEnd(Self);
end;

end.

