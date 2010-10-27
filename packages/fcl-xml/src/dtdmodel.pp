{
    This file is part of the Free Component Library

    Object model for DTD.
    Copyright (c) 2010 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dtdmodel;

{$ifdef fpc}
{$MODE objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, xmlutils;

type
  TCPType = (ctName, ctChoice, ctSeq);
  TCPQuant = (cqOnce, cqZeroOrOnce, cqZeroOrMore, cqOnceOrMore);

  TContentParticle = class(TObject)
  private
    FParent: TContentParticle;
    FChildren: TFPList;
    FIndex: Integer;
    FDef: TObject;
    FCPType: TCPType;
    FCPQuant: TCPQuant;
    function GetChildCount: Integer;
    function GetChild(Index: Integer): TContentParticle;
  public
    destructor Destroy; override;
    function Add: TContentParticle;
    function IsRequired: Boolean;
    function FindFirst(aDef: TObject): TContentParticle;
    function FindNext(aDef: TObject; ChildIdx: Integer): TContentParticle;
    function MoreRequired(ChildIdx: Integer): Boolean;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TContentParticle read GetChild;
    property Def: TObject read FDef write FDef;
    property CPType: TCPType read FCPType write FCPType;
    property CPQuant: TCPQuant read FCPQuant write FCPQuant;
  end;

  TDTDObject = class(TObject)
  private
    FExternallyDeclared: Boolean;
  public
    property ExternallyDeclared: Boolean read FExternallyDeclared write FExternallyDeclared;
  end;

  TAttrDefault = (
    adImplied,
    adDefault,
    adRequired,
    adFixed
  );

  TAttributeDef = class(TDTDObject)
  private
    FData: PNodeData;
    FDataType: TAttrDataType;
    FDefault: TAttrDefault;
    FTag: Cardinal;
    FIsNamespaceDecl: Boolean;
    FEnumeration: array of WideString;
  public
    constructor Create(aName: PHashItem; aColonPos: Integer);
    destructor Destroy; override;
    function AddEnumToken(Buf: PWideChar; Len: Integer): Boolean;
    function HasEnumToken(const aValue: WideString): Boolean;
    property Data: PNodeData read FData;
    property Default: TAttrDefault read FDefault write FDefault;
    property DataType: TAttrDataType read FDataType write FDataType;
    property Tag: Cardinal read FTag write FTag;
    property IsNamespaceDecl: Boolean read FIsNamespaceDecl;
  end;

  TElementContentType = (
    ctUndeclared,
    ctAny,
    ctEmpty,
    ctMixed,
    ctChildren
  );

  TElementDecl = class(TDTDObject)
  private
    FAttrDefs: TFPList;
    FNeedsDefaultPass: Boolean;
    function GetAttrDefCount: Integer;
    function AttrDefByIndex(index: Integer): TAttributeDef;
  public
    ContentType: TElementContentType;
    IDAttr: TAttributeDef;
    NotationAttr: TAttributeDef;
    RootCP: TContentParticle;
    destructor Destroy; override;
    function GetAttrDef(aName: PHashItem): TAttributeDef;
    procedure AddAttrDef(aDef: TAttributeDef);
    property AttrDefCount: Integer read GetAttrDefCount;
    property AttrDefs[index: Integer]: TAttributeDef read AttrDefByIndex;
    property NeedsDefaultPass: Boolean read FNeedsDefaultPass;
  end;


implementation

{ TContentParticle }

function TContentParticle.Add: TContentParticle;
begin
  if FChildren = nil then
    FChildren := TFPList.Create;
  Result := TContentParticle.Create;
  Result.FParent := Self;
  Result.FIndex := FChildren.Add(Result);
end;

destructor TContentParticle.Destroy;
var
  I: Integer;
begin
  if Assigned(FChildren) then
    for I := FChildren.Count-1 downto 0 do
      TObject(FChildren[I]).Free;
  FChildren.Free;
  inherited Destroy;
end;

function TContentParticle.GetChild(Index: Integer): TContentParticle;
begin
  Result := TContentParticle(FChildren[Index]);
end;

function TContentParticle.GetChildCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TContentParticle.IsRequired: Boolean;
var
  I: Integer;
begin
  Result := (CPQuant = cqOnce) or (CPQuant = cqOnceOrMore);
  // do not return True if all children are optional
  if (CPType <> ctName) and Result then
  begin
    for I := 0 to ChildCount-1 do
    begin
      Result := Children[I].IsRequired;
      if Result then Exit;
    end;
  end;
end;

function TContentParticle.MoreRequired(ChildIdx: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if CPType = ctSeq then
  begin
    for I := ChildIdx + 1 to ChildCount-1 do
    begin
      Result := Children[I].IsRequired;
      if Result then Exit;
    end;
  end;
  if Assigned(FParent) then
    Result := FParent.MoreRequired(FIndex);
end;

function TContentParticle.FindFirst(aDef: TObject): TContentParticle;
var
  I: Integer;
begin
  Result := nil;
  case CPType of
    ctSeq:
      for I := 0 to ChildCount-1 do with Children[I] do
      begin
        Result := FindFirst(aDef);
        if Assigned(Result) or IsRequired then
          Exit;
      end;
    ctChoice:
      for I := 0 to ChildCount-1 do with Children[I] do
      begin
        Result := FindFirst(aDef);
        if Assigned(Result) then
          Exit;
      end;
  else // ctName
    if aDef = Self.Def then
      Result := Self
  end;
end;

function TContentParticle.FindNext(aDef: TObject;
  ChildIdx: Integer): TContentParticle;
var
  I: Integer;
begin
  Result := nil;
  if CPType = ctSeq then   // search sequence to its end
  begin
    for I := ChildIdx + 1 to ChildCount-1 do with Children[I] do
    begin
      Result := FindFirst(aDef);
      if (Result <> nil) or IsRequired then
        Exit;
    end;
  end;
  if (CPQuant = cqZeroOrMore) or (CPQuant = cqOnceOrMore) then
    Result := FindFirst(aDef);
  if (Result = nil) and Assigned(FParent) then
    Result := FParent.FindNext(aDef, FIndex);
end;

{ TElementDecl }

function TElementDecl.GetAttrDefCount: Integer;
begin
  if Assigned(FAttrDefs) then
    Result := FAttrDefs.Count
  else
    Result := 0;
end;

function TElementDecl.AttrDefByIndex(index: Integer): TAttributeDef;
begin
  if Assigned(FAttrDefs) then
    Result := TAttributeDef(FAttrDefs[index])
  else
    Result := nil;
end;

destructor TElementDecl.Destroy;
var
  i: Integer;
begin
  RootCP.Free;
  if Assigned(FAttrDefs) then
  begin
    for i := FAttrDefs.Count-1 downto 0 do
      TObject(FAttrDefs.List^[i]).Free;
    FAttrDefs.Free;
  end;
  inherited Destroy;
end;

function TElementDecl.GetAttrDef(aName: PHashItem): TAttributeDef;
var
  i: Integer;
begin
  if Assigned(FAttrDefs) then
  begin
    for i := 0 to FAttrDefs.Count-1 do
    begin
      Result := TAttributeDef(FAttrDefs.List^[i]);
      if Result.FData^.FQName = aName then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TElementDecl.AddAttrDef(aDef: TAttributeDef);
begin
  if FAttrDefs = nil then
    FAttrDefs := TFPList.Create;
  FAttrDefs.Add(aDef);
  if aDef.Default in [adRequired, adDefault, adFixed] then
    FNeedsDefaultPass := True;
end;

{ TAttributeDef }

constructor TAttributeDef.Create(aName: PHashItem; aColonPos: Integer);
begin
  New(FData);
  FillChar(FData^, sizeof(TNodeData), 0);
  FData^.FIsDefault := True;
  FData^.FQName := aName;
  FData^.FColonPos := aColonPos;
  FData^.FTypeInfo := Self;
  FIsNamespaceDecl := ((Length(aName^.Key) = 5) or (aColonPos = 6)) and
    (Pos(WideString('xmlns'), aName^.Key) = 1);
end;

destructor TAttributeDef.Destroy;
var
  curr, tmp: PNodeData;
begin
  curr := FData;
  while Assigned(curr) do
  begin
    tmp := curr^.FNext;
    Dispose(curr);
    curr := tmp;
  end;
  inherited Destroy;
end;

function TAttributeDef.AddEnumToken(Buf: PWideChar; Len: Integer): Boolean;
var
  I, L: Integer;
begin
  // TODO: this implementaion is the slowest possible...
  Result := False;
  L := Length(FEnumeration);
  for I := 0 to L-1 do
  begin
    if (Len = Length(FEnumeration[i])) and
      CompareMem(Pointer(FEnumeration[i]), Buf, Len*sizeof(WideChar)) then
        Exit;
  end;
  SetLength(FEnumeration, L+1);
  SetString(FEnumeration[L], Buf, Len);
  Result := True;
end;

function TAttributeDef.HasEnumToken(const aValue: WideString): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Length(FEnumeration) = 0 then
    Exit;
  for I := 0 to Length(FEnumeration)-1 do
  begin
    if FEnumeration[I] = aValue then
      Exit;
  end;
  Result := False;
end;

end.
