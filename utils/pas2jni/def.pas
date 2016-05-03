{
    pas2jni - JNI bridge generator for Pascal.

    Copyright (c) 2013 by Yury Sidorov.

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

unit def;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TDefType = (dtNone, dtUnit, dtClass, dtRecord, dtProc, dtField, dtProp, dtParam, dtVar,
              dtType, dtConst, dtProcType, dtEnum, dtSet);

  TDefClass = class of TDef;
  { TDef }

  TDef = class
  private
    FAliasName: string;
    FRefCnt: integer;
    FItems: TObjectList;
    FInSetUsed: boolean;
    procedure CheckItems;
    function GetAliasName: string;
    function GetCount: integer;
    function GetIsUsed: boolean;
    function GetItem(Index: Integer): TDef;
    procedure SetItem(Index: Integer; const AValue: TDef);
  protected
    procedure SetIsUsed(const AValue: boolean); virtual;
    function ResolveDef(d: TDef; ExpectedClass: TDefClass = nil): TDef;
    procedure AddRef;
    procedure DecRef;
    procedure SetExtUsed(ExtDef: TDef; AUsed: boolean; var HasRef: boolean);
  public
    DefType: TDefType;
    DefId: integer;
    SymId: integer;
    Name: string;
    Parent: TDef;
    Tag: integer;
    IsPrivate: boolean;

    constructor Create; virtual; overload;
    constructor Create(AParent: TDef; AType: TDefType); virtual; overload;
    destructor Destroy; override;
    function Add(ADef: TDef): integer;
    function Insert(Index: integer; ADef: TDef): integer;
    function FindDef(ADefId: integer; Recursive: boolean = True): TDef;
    procedure ResolveDefs; virtual;
    procedure SetNotUsed;
    property Items[Index: Integer]: TDef read GetItem write SetItem; default;
    property Count: integer read GetCount;
    property IsUsed: boolean read GetIsUsed write SetIsUsed;
    property RefCnt: integer read FRefCnt;
    property AliasName: string read GetAliasName write FAliasName;
  end;

  { TClassDef }

  TClassDef = class(TDef)
  private
    FHasClassRef: boolean;
  protected
    procedure SetIsUsed(const AValue: boolean); override;
  public
    AncestorClass: TClassDef;
    HasAbstractMethods: boolean;
    HasReplacedItems: boolean;
    ImplementsReplacedItems: boolean;
    procedure ResolveDefs; override;
  end;

  TRecordDef = class(TDef)
  public
    Size: integer;
  end;

  TBasicType = (btVoid, btByte, btShortInt, btWord, btSmallInt, btLongWord, btLongInt, btInt64,
                btSingle, btDouble, btString, btWideString, btBoolean, btChar, btWideChar, btEnum, btPointer,
                btGuid);

  { TTypeDef }

  TTypeDef = class(TDef)
  protected
    procedure SetIsUsed(const AValue: boolean); override;
  public
    BasicType: TBasicType;
  end;

  { TReplDef }

  TReplDef = class(TDef)
  protected
    procedure SetIsUsed(const AValue: boolean); override;
  public
    IsReplaced: boolean;
    IsReplImpl: boolean;
    ReplacedItem: TReplDef;

    function CanReplaced: boolean; virtual;
    function IsReplacedBy(d: TReplDef): boolean; virtual;
    procedure CheckReplaced;
  end;

  TVarOption = (voRead, voWrite, voConst, voVar, voOut);
  TVarOptions = set of TVarOption;

  { TVarDef }

  TVarDef = class(TReplDef)
  private
    FHasTypeRef: boolean;
  protected
    procedure SetIsUsed(const AValue: boolean); override;
  public
    VarOpt: TVarOptions;
    VarType: TDef;
    constructor Create; override;
    procedure ResolveDefs; override;
    function IsReplacedBy(d: TReplDef): boolean; override;
    function CanReplaced: boolean; override;
  end;

  TProcType = (ptProcedure, ptFunction, ptConstructor, ptDestructor);
  TProcOption = (poOverride, poOverload, poMethodPtr, poPrivate, poProtected);
  TProcOptions = set of TProcOption;

  { TProcDef }

  TProcDef = class(TReplDef)
  private
    FHasRetTypeRef: boolean;
  protected
    procedure SetIsUsed(const AValue: boolean); override;
  public
    ProcType: TProcType;
    ReturnType: TDef;
    ProcOpt: TProcOptions;
    procedure ResolveDefs; override;
    function IsReplacedBy(d: TReplDef): boolean; override;
    function CanReplaced: boolean; override;
  end;

  TUnitDef = class(TDef)
  public
    OS: string;
    CPU: string;
    IntfCRC: string;
    PPUVer: integer;
    UsedUnits: array of TUnitDef;
    Processed: boolean;
  end;

  TConstDef = class(TVarDef)
  public
    Value: string;
  end;

  { TSetDef }

  TSetDef = class(TDef)
  private
    FHasElTypeRef: boolean;
  protected
    procedure SetIsUsed(const AValue: boolean); override;
  public
    Size: integer;
    Base: integer;
    ElMax: integer;
    ElType: TTypeDef;
  end;

const
  ReplDefs  = [dtField, dtProp, dtProc];

implementation

function IsSameType(t1, t2: TDef): boolean;
begin
  Result:=t1 = t2;
  if Result then
    exit;
  if (t1 = nil) or (t2 = nil) or (t1.DefType <> t2.DefType) then
    exit;
  if t1.DefType <> dtType then
    exit;
  Result:=TTypeDef(t1).BasicType = TTypeDef(t2).BasicType;
end;

{ TReplDef }

procedure TReplDef.SetIsUsed(const AValue: boolean);
var
  i: integer;
begin
  i:=RefCnt;
  inherited SetIsUsed(AValue);
  if (i = 0) and (RefCnt > 0) then
    CheckReplaced;
end;

function TReplDef.CanReplaced: boolean;
begin
  Result:=not (IsPrivate or (Parent = nil) or (Parent.DefType <> dtClass));
end;

function TReplDef.IsReplacedBy(d: TReplDef): boolean;
begin
  Result:=d.CanReplaced and (CompareText(Name, d.Name) = 0);
end;

procedure TReplDef.CheckReplaced;

  function _Scan(cls: TClassDef): boolean;
  var
    i: integer;
    d: TReplDef;
    c: TClassDef;
  begin
    Result:=False;
    c:=cls.AncestorClass;
    if c = nil then
      exit;
    for i:=0 to c.Count - 1 do begin
      d:=TReplDef(c[i]);
      if (d.DefType in ReplDefs) and IsReplacedBy(d) then begin
        d.IsReplaced:=True;
        ReplacedItem:=d;
        Result:=True;
        break;
      end;
    end;
    if not Result then
      Result:=_Scan(c);
    if Result then begin
      cls.ImplementsReplacedItems:=True;
      c.HasReplacedItems:=True;
    end;
  end;

begin
  if not CanReplaced then
    exit;
  if _Scan(TClassDef(Parent)) then
    IsReplImpl:=True;
end;

{ TSetDef }

procedure TSetDef.SetIsUsed(const AValue: boolean);
begin
  inherited SetIsUsed(AValue);
  SetExtUsed(ElType, AValue, FHasElTypeRef);
end;

{ TTypeDef }

procedure TTypeDef.SetIsUsed(const AValue: boolean);
begin
  if BasicType in [btEnum] then
    inherited SetIsUsed(AValue)
  else
    if AValue then
      AddRef
    else
      DecRef;
end;

{ TProcDef }

procedure TProcDef.SetIsUsed(const AValue: boolean);
var
  i: integer;
begin
  if IsPrivate then
    exit;
  if AValue and (RefCnt = 0) then begin
    for i:=0 to Count - 1 do
      if TVarDef(Items[i]).VarType = nil then
        exit; // If procedure has unsupported parameters, don't use it
  end;
  inherited SetIsUsed(AValue);
  if ReturnType <> Parent then
    SetExtUsed(ReturnType, AValue, FHasRetTypeRef);
end;

procedure TProcDef.ResolveDefs;
begin
  inherited ResolveDefs;
  ReturnType:=ResolveDef(ReturnType);
end;

function TProcDef.IsReplacedBy(d: TReplDef): boolean;
var
  i: integer;
  p: TProcDef;
begin
  Result:=False;
  if d.DefType <> dtProc then
    exit;
  p:=TProcDef(d);
  if (Count = p.Count) and inherited IsReplacedBy(p) then begin
    // Check parameter types
    for i:=0 to Count - 1 do
      if not IsSameType(TVarDef(Items[i]).VarType, TVarDef(p.Items[i]).VarType) then
        exit;
    Result:=True;
  end;
end;

function TProcDef.CanReplaced: boolean;
begin
  Result:=inherited CanReplaced and (ProcType = ptFunction);
end;

{ TClassDef }

procedure TClassDef.SetIsUsed(const AValue: boolean);
begin
  inherited SetIsUsed(AValue);
  SetExtUsed(AncestorClass, AValue, FHasClassRef);
end;

procedure TClassDef.ResolveDefs;
begin
  inherited ResolveDefs;
  AncestorClass:=TClassDef(ResolveDef(AncestorClass, TClassDef));
end;

{ TVarDef }

procedure TVarDef.SetIsUsed(const AValue: boolean);
begin
  if IsPrivate then
    exit;
  inherited SetIsUsed(AValue);
  SetExtUsed(VarType, AValue, FHasTypeRef);
end;

procedure TVarDef.ResolveDefs;
begin
  inherited ResolveDefs;
  VarType:=ResolveDef(VarType);
end;

function TVarDef.IsReplacedBy(d: TReplDef): boolean;
begin
  Result:=(d.DefType in [dtProp, dtField]) and not IsSameType(VarType, TVarDef(d).VarType) and inherited IsReplacedBy(d);
end;

function TVarDef.CanReplaced: boolean;
begin
  Result:=(voRead in VarOpt) and inherited CanReplaced;
end;

constructor TVarDef.Create;
begin
  inherited Create;
  VarOpt:=[voRead, voWrite];
end;

{ TDef }

procedure TDef.CheckItems;
begin
  if FItems = nil then
    FItems:=TObjectList.Create(True);
end;

function TDef.GetAliasName: string;
begin
  if FAliasName <> '' then
    Result:=FAliasName
  else
    Result:=Name;
end;

function TDef.GetCount: integer;
begin
  if FItems = nil then
    Result:=0
  else begin
    CheckItems;
    Result:=FItems.Count;
  end;
end;

function TDef.GetIsUsed: boolean;
begin
  Result:=FRefCnt > 0;
end;

function TDef.GetItem(Index: Integer): TDef;
begin
  CheckItems;
  Result:=TDef(FItems[Index]);
end;

procedure TDef.SetIsUsed(const AValue: boolean);
var
  i: integer;
  f: boolean;
begin
  if FInSetUsed or (DefType = dtNone) or IsPrivate then
    exit;
  if AValue then begin
    AddRef;
    f:=FRefCnt = 1;
  end
  else begin
    if FRefCnt = 0 then
      exit;
    DecRef;
    f:=FRefCnt = 0;
  end;
  if f then begin
    // Update userd mark of children only once
    FInSetUsed:=True;
    try
      for i:=0 to Count - 1 do
        Items[i].IsUsed:=AValue;
    finally
      FInSetUsed:=False;
    end;
    // Update parent's used mark
    if (Parent <> nil) and (Parent.DefType = dtUnit) then
      if AValue then
        Parent.AddRef
      else
        Parent.DecRef;
  end;
end;

function TDef.ResolveDef(d: TDef; ExpectedClass: TDefClass): TDef;
begin
  if (d = nil) or (d.DefType <> dtNone) then begin
    Result:=d;
    exit;
  end;
  Result:=d.Parent.FindDef(d.DefId);
  if (ExpectedClass <> nil) and (Result <> nil) then
    if not (Result is ExpectedClass) then
      raise Exception.CreateFmt('Unexpected class. Expected: %s, got: %s', [ExpectedClass.ClassName, Result.ClassName]);
end;

procedure TDef.AddRef;
begin
  Inc(FRefCnt);
end;

procedure TDef.DecRef;
begin
  if FRefCnt > 0 then
    Dec(FRefCnt);
end;

procedure TDef.SetExtUsed(ExtDef: TDef; AUsed: boolean; var HasRef: boolean);
var
  OldRefCnt: integer;
begin
  if ExtDef = nil then
    exit;
  if AUsed then begin
    if HasRef then
      exit;
    OldRefCnt:=ExtDef.RefCnt;
    ExtDef.IsUsed:=True;
    HasRef:=OldRefCnt <> ExtDef.RefCnt;
  end
  else
    if HasRef and not IsUsed then begin
      ExtDef.IsUsed:=False;
      HasRef:=False;
    end;
end;

procedure TDef.SetItem(Index: Integer; const AValue: TDef);
begin
  CheckItems;
  FItems[Index]:=AValue;
end;

constructor TDef.Create;
begin
  DefId:=-1;
  DefType:=dtNone;
end;

constructor TDef.Create(AParent: TDef; AType: TDefType);
begin
  Create;
  if AParent <> nil then
    AParent.Add(Self);
  DefType:=AType;
end;

destructor TDef.Destroy;
begin
  FreeAndNil(FItems);
  if (Parent <> nil) and (Parent.FItems <> nil) then begin
    Parent.FItems.OwnsObjects:=False;
    try
      Parent.FItems.Remove(Self);
    finally
      Parent.FItems.OwnsObjects:=True;
    end;
  end;
  inherited Destroy;
end;

function TDef.Add(ADef: TDef): integer;
begin
  Result:=Insert(Count, ADef);
end;

function TDef.Insert(Index: integer; ADef: TDef): integer;
begin
  CheckItems;
  Result:=Index;
  FItems.Insert(Result, ADef);
  ADef.Parent:=Self;
end;

function TDef.FindDef(ADefId: integer; Recursive: boolean): TDef;

  function _Find(d: TDef): TDef;
  var
    i: integer;
  begin
    Result:=nil;
    for i:=0 to d.Count - 1 do
      with d[i] do begin
        if (DefType <> dtNone) and (DefId = ADefId) then begin
          Result:=d[i];
          break;
        end;
        if Recursive and (Count > 0) then begin
          Result:=_Find(d[i]);
          if Result <> nil then
            break;
        end;
      end;
  end;

begin
  Result:=_Find(Self);
end;

procedure TDef.ResolveDefs;
var
  i: integer;
begin
  for i:=0 to Count - 1 do
    Items[i].ResolveDefs;
end;

procedure TDef.SetNotUsed;
begin
  if FRefCnt = 0 then
    exit;
  FRefCnt:=1;
  IsUsed:=False;
end;

end.

