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
{$I+}

interface

uses SysUtils, cclasses, Classes;

type
  TPpuDefType = (dtNone, dtUnit, dtObject, dtRecord, dtProc, dtField, dtProp, dtParam, dtVar,
                 dtTypeRef, dtConst, dtProcType, dtEnum, dtSet);

  TPpuDef = class;
  TPpuContainerDef = class;
  TPpuUnitDef = class;

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
    procedure WriteObjectStart(const AName: string; Def: TPpuDef = nil); virtual;
    procedure WriteObjectEnd(Def: TPpuDef = nil); virtual;
    procedure WriteArrayStart(const AName: string); virtual;
    procedure WriteArrayEnd; virtual;
    procedure WriteStr(const AName, AValue: string); virtual;
    procedure WriteInt(const AName: string; AValue: Int64); virtual;
    procedure WriteFloat(const AName: string; AValue: extended); virtual;
    procedure WriteBool(const AName: string; AValue: boolean); virtual;
    procedure WriteNull(const AName: string); virtual;
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

  { TPpuRef }
  TPpuRef = class
  private
    FId: cardinal;
    function GetId: cardinal;
    function GetIsSymId: boolean;
    procedure SetId(AValue: cardinal);
    procedure SetIsSymId(AValue: boolean);
  public
    UnitIndex: word;
    constructor Create;
    procedure Write(Output: TPpuOutput; const RefName: string);
    property Id: cardinal read GetId write SetId;
    property IsSymId: boolean read GetIsSymId write SetIsSymId;
    function IsCurUnit: boolean; inline;
    function IsNull: boolean; inline;
  end;

  TPpuFilePos = record
    FileIndex: dword;
    Line, Col: integer;
  end;

  TPpuDefVisibility = (dvPublic, dvPublished, dvProtected, dvPrivate);

  { TPpuDef }

  TPpuDef = class
  private
    FId: cardinal;
    FParent: TPpuContainerDef;
    FParentUnit: TPpuUnitDef;
    function GetDefTypeName: string;
    function GetId: cardinal;
    function GetParentUnit: TPpuUnitDef;
    procedure SetId(AValue: cardinal);
    procedure SetParent(AValue: TPpuContainerDef);

  protected
    procedure WriteDef(Output: TPpuOutput); virtual;

  public
    DefType: TPpuDefType;
    Name: string;
    FilePos: TPpuFilePos;
    // Symbol/definition reference
    Ref: TPpuRef;
    Visibility: TPpuDefVisibility;

    constructor Create(AParent: TPpuContainerDef); virtual; reintroduce;
    destructor Destroy; override;
    procedure Write(Output: TPpuOutput);
    function CanWrite: boolean; virtual;
    procedure SetSymId(AId: integer);
    property Parent: TPpuContainerDef read FParent write SetParent;
    property ParentUnit: TPpuUnitDef read GetParentUnit;
    property Id: cardinal read GetId write SetId;
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
    procedure BeforeWriteItems(Output: TPpuOutput); virtual;

  public
    ItemsName: string;

    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
    function Add(Def: TPpuDef): integer;
    property Items[Index: Integer]: TPpuDef read GetItem write SetItem; default;
    property Count: integer read GetCount;
  end;

  { TPpuTypeRef }
  TPpuTypeRef = class(TPpuDef)
  protected
    procedure WriteDef(Output: TPpuOutput); override;
  public
    constructor Create(AParent: TPpuContainerDef); override;
  end;

  { TPpuUnitDef }
  TPpuUnitDef = class(TPpuContainerDef)
  private
    FIndexById: THashSet;
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
    function FindById(AId: integer; FindSym: boolean = False): TPpuDef;
  end;

  { TPpuSrcFile }
  TPpuSrcFile = class(TPpuDef)
  protected
    procedure WriteDef(Output: TPpuOutput); override;
  public
    FileTime: TDateTime;
  end;

  TPpuProcOption = (poProcedure, poFunction, poConstructor, poDestructor, poOperator,
                    poClassMethod, poVirtual, poAbstract, poOverriding, poOverload, poInline);
  TPpuProcOptions = set of TPpuProcOption;

  { TPpuProcDef }
  TPpuProcDef = class(TPpuContainerDef)
  protected
    procedure BeforeWriteItems(Output: TPpuOutput); override;
  public
    ReturnType: TPpuRef;
    Options: TPpuProcOptions;
    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
  end;

  { TPpuProcTypeDef }
  TPpuProcTypeDef = class(TPpuProcDef)
  public
    constructor Create(AParent: TPpuContainerDef); override;
  end;

  { TPpuVarDef }
  TPpuVarDef = class(TPpuDef)
  protected
    procedure WriteDef(Output: TPpuOutput); override;
  public
    VarType: TPpuRef;
    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
  end;

  TPpuParamSpez = (psValue, psVar, psOut, psConst, psConstRef, psHidden);

  { TPpuParamDef }
  TPpuParamDef = class(TPpuVarDef)
  protected
    procedure WriteDef(Output: TPpuOutput); override;
  public
    Spez: TPpuParamSpez;
    constructor Create(AParent: TPpuContainerDef); override;
    function CanWrite: boolean; override;
  end;

  TPpuObjType = (otUnknown, otClass, otObject, otInterface, otHelper);
  TPpuObjOption = (ooIsAbstract);
  TPpuObjOptions = set of TPpuObjOption;

  { TPpuObjectDef }
  TPpuObjectDef = class(TPpuContainerDef)
  protected
    procedure BeforeWriteItems(Output: TPpuOutput); override;
  public
    ObjType: TPpuObjType;
    Ancestor: TPpuRef;
    Options: TPpuObjOptions;
    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
    function CanWrite: boolean; override;
  end;

  { TPpuFieldDef }
  TPpuFieldDef = class(TPpuVarDef)
  public
    constructor Create(AParent: TPpuContainerDef); override;
  end;

  { TPpuPropDef }
  TPpuPropDef = class(TPpuContainerDef)
  protected
    procedure BeforeWriteItems(Output: TPpuOutput); override;
  public
    PropType: TPpuRef;
    Getter, Setter: TPpuRef;
    constructor Create(AParent: TPpuContainerDef); override;
    destructor Destroy; override;
  end;


implementation

const
  DefTypeNames: array[TPpuDefType] of string =
    ('', 'unit', 'obj', 'rec', 'proc', 'field', 'prop', 'param', 'var',
     'type', 'const', 'proctype', 'enum', 'set');

  ProcOptionNames: array[TPpuProcOption] of string =
    ('procedure', 'function', 'constructor', 'destructor', 'operator',
     'classmethod', 'virtual', 'abstract', 'overriding', 'overload', 'inline');

  DefVisibilityNames: array[TPpuDefVisibility] of string =
    ('public', 'published', 'protected', 'private');

  ParamSpezNames: array[TPpuParamSpez] of string =
    ('value', 'var', 'out', 'const', 'constref', '');

  ObjTypeNames: array[TPpuObjType] of string =
    ('', 'class', 'object', 'interface', 'helper');

  ObjOptionNames: array[TPpuObjOption] of string =
    ('abstract');

  SymIdBit = $80000000;
  InvalidId = cardinal(-1);
  InvalidUnit = word(-1);

function IsSymId(Id: cardinal): boolean; inline;
begin
  Result:=Id and SymIdBit <> 0;
end;

{ TPpuPropDef }

procedure TPpuPropDef.BeforeWriteItems(Output: TPpuOutput);
begin
  inherited BeforeWriteItems(Output);
  PropType.Write(Output, 'PropType');
  Getter.Write(Output, 'Getter');
  Setter.Write(Output, 'Setter');
end;

constructor TPpuPropDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtProp;
  ItemsName:='Params';
  PropType:=TPpuRef.Create;
  Getter:=TPpuRef.Create;
  Setter:=TPpuRef.Create;
end;

destructor TPpuPropDef.Destroy;
begin
  Getter.Free;
  Setter.Free;
  PropType.Free;
  inherited Destroy;
end;

{ TPpuTypeRef }

procedure TPpuTypeRef.WriteDef(Output: TPpuOutput);
begin
  inherited WriteDef(Output);
  Ref.Write(Output, 'Ref');
end;

constructor TPpuTypeRef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtTypeRef;
end;

{ TPpuFieldDef }

constructor TPpuFieldDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtField;
end;

{ TPpuParamDef }

procedure TPpuParamDef.WriteDef(Output: TPpuOutput);
begin
  inherited WriteDef(Output);
  if Spez <> psValue then
    Output.WriteStr('Spez', ParamSpezNames[Spez]);
end;

constructor TPpuParamDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtParam;
  Spez:=psValue;
end;

function TPpuParamDef.CanWrite: boolean;
begin
  Result:=Spez <> psHidden;
end;

{ TPpuVarDef }

procedure TPpuVarDef.WriteDef(Output: TPpuOutput);
begin
  inherited WriteDef(Output);
  VarType.Write(Output, 'VarType');
end;

constructor TPpuVarDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtVar;
  VarType:=TPpuRef.Create;
end;

destructor TPpuVarDef.Destroy;
begin
  VarType.Free;
  inherited Destroy;
end;

{ TPpuObjectDef }

procedure TPpuObjectDef.BeforeWriteItems(Output: TPpuOutput);
var
  opt: TPpuObjOption;
begin
  inherited BeforeWriteItems(Output);
  if Options <> [] then begin
    Output.WriteArrayStart('Options');
    for opt:=Low(opt) to High(opt) do
      if opt in Options then
        Output.WriteStr('', ObjOptionNames[opt]);
    Output.WriteArrayEnd;
  end;
  Output.WriteStr('ObjType', ObjTypeNames[ObjType]);
  Ancestor.Write(Output, 'Ancestor');
end;

constructor TPpuObjectDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtObject;
  ItemsName:='Fields';
  ObjType:=otUnknown;
  Ancestor:=TPpuRef.Create;
end;

destructor TPpuObjectDef.Destroy;
begin
  Ancestor.Free;
  inherited Destroy;
end;

function TPpuObjectDef.CanWrite: boolean;
begin
  Result:=ObjType <> otUnknown;
end;

{ TPpuRef }

function TPpuRef.GetId: cardinal;
begin
  if FId = InvalidId then
    Result:=InvalidId
  else
    Result:=FId and not SymIdBit;
end;

function TPpuRef.GetIsSymId: boolean;
begin
  Result:=FId and SymIdBit <> 0;
end;

procedure TPpuRef.SetId(AValue: cardinal);
begin
  if (FId = InvalidId) or (AValue = InvalidId) then
    FId:=AValue
  else
    FId:=AValue or (FId and SymIdBit);
end;

procedure TPpuRef.SetIsSymId(AValue: boolean);
begin
  if AValue then
    FId:=FId or SymIdBit
  else
    FId:=FId and not SymIdBit;
end;

constructor TPpuRef.Create;
begin
  UnitIndex:=InvalidUnit;
  FId:=InvalidId;
end;

procedure TPpuRef.Write(Output: TPpuOutput; const RefName: string);
begin
  with Output do
    if IsNull then
      WriteNull(RefName)
    else begin
      WriteObjectStart(RefName);
      if not IsCurUnit then
        WriteInt('Unit', UnitIndex);
      if IsSymId then
        WriteInt('SymId', Id)
      else
        WriteInt('Id', Id);
      WriteObjectEnd;
    end;
end;

function TPpuRef.IsCurUnit: boolean;
begin
  Result:=UnitIndex = InvalidUnit;
end;

function TPpuRef.IsNull: boolean;
begin
  Result:=Id = InvalidId;
end;

{ TPpuProcTypeDef }

constructor TPpuProcTypeDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtProcType;
end;

{ TPpuProcDef }

procedure TPpuProcDef.BeforeWriteItems(Output: TPpuOutput);
var
  opt: TPpuProcOption;
begin
  inherited BeforeWriteItems(Output);
  if Options <> [] then begin
    Output.WriteArrayStart('Options');
    for opt:=Low(opt) to High(opt) do
      if opt in Options then
        Output.WriteStr('', ProcOptionNames[opt]);
    Output.WriteArrayEnd;
  end;
  if Options*[poProcedure, poDestructor] = [] then
    ReturnType.Write(Output, 'RetType');
end;

constructor TPpuProcDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtProc;
  ItemsName:='Params';
  ReturnType:=TPpuRef.Create;
end;

destructor TPpuProcDef.Destroy;
begin
  ReturnType.Free;
  inherited Destroy;
end;

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
begin
  if AValue then
    WriteStr(AName, '1')
  else
    WriteStr(AName, '0');
end;

procedure TPpuOutput.WriteNull(const AName: string);
begin
  WriteStr(AName, '');
end;

procedure TPpuOutput.WriteArrayStart(const AName: string);
begin
  IncI;
end;

procedure TPpuOutput.WriteArrayEnd;
begin
  DecI;
end;

procedure TPpuOutput.WriteObjectStart(const AName: string; Def: TPpuDef);
begin
  IncI;
  if Def = nil then
    exit;
  if Def.DefType <> dtNone then
    WriteStr('Type', Def.DefTypeName);
  if Def.Name <> '' then
    WriteStr('Name', Def.Name);
end;

procedure TPpuOutput.WriteObjectEnd(Def: TPpuDef);
begin
  DecI;
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
      WriteStr('CRC', hexStr(Crc, 8));
    if IntfCrc <> 0 then
      WriteStr('InterfaceCRC', hexStr(IntfCrc, 8));
    UsedUnits.WriteDef(Output);
    if Length(RefUnits) > 0 then begin
      WriteArrayStart('Units');
      for i:=0 to High(RefUnits) do
        WriteStr('', RefUnits[i]);
      WriteArrayEnd;
    end;
    SourceFiles.WriteDef(Output);
  end;
  inherited WriteDef(Output);
end;

constructor TPpuUnitDef.Create(AParent: TPpuContainerDef);
begin
  inherited Create(AParent);
  DefType:=dtUnit;
  ItemsName:='Interface';
  UsedUnits:=TPpuContainerDef.Create(nil);
  UsedUnits.FParent:=Self;
  UsedUnits.ItemsName:='Uses';
  SourceFiles:=TPpuContainerDef.Create(nil);
  SourceFiles.FParent:=Self;
  SourceFiles.ItemsName:='Files';
  FIndexById:=THashSet.Create(64, True, False);
end;

destructor TPpuUnitDef.Destroy;
begin
  UsedUnits.Free;
  SourceFiles.Free;
  FIndexById.Free;
  inherited Destroy;
end;

function TPpuUnitDef.FindById(AId: integer; FindSym: boolean): TPpuDef;
var
  h: PHashSetItem;
  i: cardinal;
begin
  Result:=nil;
  if AId = -1 then
    exit;
  i:=AId;
  if FindSym then
    i:=i or SymIdBit;
  h:=FIndexById.Find(@i, SizeOf(i));
  if h <> nil then
    Result:=TPpuDef(h^.Data)
  else
    Result:=nil;
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
  BeforeWriteItems(Output);
  if Count = 0 then
    exit;
  Output.WriteArrayStart(ItemsName);
  for i:=0 to Count - 1 do
    Items[i].Write(Output);
  Output.WriteArrayEnd;
end;

procedure TPpuContainerDef.BeforeWriteItems(Output: TPpuOutput);
begin
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

function TPpuDef.GetId: cardinal;
begin
  if FId = InvalidId then
    Result:=InvalidId
  else
    Result:=FId and not SymIdBit;
end;

function TPpuDef.GetParentUnit: TPpuUnitDef;
var
  d: TPpuContainerDef;
begin
  if FParentUnit = nil then begin
    d:=Parent;
    while (d <> nil) and (d.DefType <> dtUnit) do
      d:=d.Parent;
    FParentUnit:=TPpuUnitDef(d);
  end;
  Result:=FParentUnit;
end;

procedure TPpuDef.SetId(AValue: cardinal);
var
  h: PHashSetItem;
  u: TPpuUnitDef;
begin
  if FId = AValue then Exit;
  u:=ParentUnit;
  if (FId <> InvalidId) and (u <> nil) then begin
    h:=u.FIndexById.Find(@FId, SizeOf(FId));
    if h <> nil then
      u.FIndexById.Remove(h);
  end;
  FId:=AValue;
  if (FId <> InvalidId) and (u <> nil) then begin;
    h:=u.FIndexById.FindOrAdd(@FId, SizeOf(FId));
    h^.Data:=Self;
  end;
end;

procedure TPpuDef.SetParent(AValue: TPpuContainerDef);
var
  i: cardinal;
begin
  if FParent=AValue then Exit;
  if FParent <> nil then
    raise Exception.Create('Parent can not be modified.');
  AValue.Add(Self);
  if FId <> InvalidId then begin
    i:=FId;
    FId:=InvalidId;
    SetId(i);
  end;
end;

procedure TPpuDef.SetSymId(AId: integer);
begin
  Id:=cardinal(AId) or SymIdBit;
end;

procedure TPpuDef.WriteDef(Output: TPpuOutput);
begin
  with Output do begin
    if FId <> InvalidId then
      if IsSymId(FId) then
        WriteInt('SymId', Id)
      else begin
        WriteInt('Id', Id);
        if not Ref.IsNull then
          WriteInt('SymId', Ref.Id);
      end;
    if FilePos.Line > 0 then begin
      WriteObjectStart('Pos');
      if FilePos.FileIndex > 0 then
        WriteInt('File', FilePos.FileIndex);
      WriteInt('Line', FilePos.Line);
      WriteInt('Col', FilePos.Col);
      WriteObjectEnd;
    end;
    if Visibility <> dvPublic then
      WriteStr('Visibility', DefVisibilityNames[Visibility]);
  end;
end;

constructor TPpuDef.Create(AParent: TPpuContainerDef);
begin
  FId:=InvalidId;
  Ref:=TPpuRef.Create;
  Visibility:=dvPublic;
  if AParent <> nil then
    AParent.Add(Self);
end;

destructor TPpuDef.Destroy;
begin
  Ref.Free;
  inherited Destroy;
end;

procedure TPpuDef.Write(Output: TPpuOutput);
begin
  if not CanWrite then
    exit;
  if Parent <> nil then
    Output.WriteObjectStart('', Self);
  WriteDef(Output);
  if Parent <> nil then
    Output.WriteObjectEnd(Self);
end;

function TPpuDef.CanWrite: boolean;
begin
  Result:=True;
end;

end.

