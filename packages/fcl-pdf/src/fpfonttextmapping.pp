{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 by Graeme Geldenhuys

    This unit defines classes that manage font glyph IDs and unicode
    character codes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPFontTextMapping;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs;

type

  TTextMapping = class(TObject)
  private
    FCharID: uint32;
    FGlyphID: uint32;
    FNewGlyphID: uint32;
    FGlyphData: TStream;
    FIsCompoundGlyph: boolean;
  public
    constructor Create;
    class function NewTextMap(const ACharID, AGlyphID: uint32): TTextMapping;
    property    CharID: uint32 read FCharID write FCharID;
    property    GlyphID: uint32 read FGlyphID write FGlyphID;
    property    NewGlyphID: uint32 read FNewGlyphID write FNewGlyphID;
    property    GlyphData: TStream read FGlyphData write FGlyphData;
    property    IsCompoundGlyph: boolean read FIsCompoundGlyph write FIsCompoundGlyph;
  end;


  TTextMappingList = class(TObject)
  private
    FList: TFPObjectList;
    function    GetCount: Integer;
  protected
    function    GetItem(AIndex: Integer): TTextMapping; virtual;
    procedure   SetItem(AIndex: Integer; AValue: TTextMapping); virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Add(AObject: TTextMapping): Integer; overload;
    function    Add(const ACharID, AGlyphID: uint32): Integer; overload;
    function    Contains(const AGlyphID: uint32): boolean;
    function    ContainsCharID(const AID: uint32): boolean;
    function    GetNewGlyphID(const ACharID: uint32): uint32;
    function    GetMaxCharID: uint32;
    function    GetMaxGlyphID: uint32;
    procedure   Insert(const AIndex: integer; const ACharID, AGlyphID: uint32);
    procedure   Sort;
    property    Count: Integer read GetCount;
    property    Items[AIndex: Integer]: TTextMapping read GetItem write SetItem; default;
  end;


implementation

{ TTextMapping }

constructor TTextMapping.Create;
begin
  FGlyphData := nil;
  FCharID := 0;
  FGlyphID := 0;
  FNewGlyphID := 0;
  FIsCompoundGlyph := False;
end;

class function TTextMapping.NewTextMap(const ACharID, AGlyphID: uint32): TTextMapping;
begin
  Result := TTextMapping.Create;
  Result.CharID := ACharID;
  Result.GlyphID := AGlyphID;
end;

{ TTextMappingList }

function TTextMappingList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTextMappingList.GetItem(AIndex: Integer): TTextMapping;
begin
  Result := TTextMapping(FList.Items[AIndex]);
end;

procedure TTextMappingList.SetItem(AIndex: Integer; AValue: TTextMapping);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TTextMappingList.Create;
begin
  FList := TFPObjectList.Create(True);
end;

destructor TTextMappingList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TTextMappingList.Add(AObject: TTextMapping): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FList.Count-1 do
  begin
    if TTextMapping(FList.Items[i]).CharID = AObject.CharID then
      Exit; // mapping already exists
  end;
  Result := FList.Add(AObject);
end;

function TTextMappingList.Add(const ACharID, AGlyphID: uint32): Integer;
var
  o: TTextMapping;
begin
  o := TTextMapping.Create;
  o.CharID := ACharID;
  o.GlyphID := AGlyphID;
  Result := Add(o);
  if Result = -1 then
    o.Free;
end;

function TTextMappingList.Contains(const AGlyphID: uint32): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    if Items[i].GlyphID = AGlyphID then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TTextMappingList.ContainsCharID(const AID: uint32): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    if Items[i].CharID = AID then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TTextMappingList.GetNewGlyphID(const ACharID: uint32): uint32;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    if Items[i].CharID = ACharID then
    begin
      Result := Items[i].NewGlyphID;
      Exit;
    end;
  end;
end;

function TTextMappingList.GetMaxCharID: uint32;
begin
  Sort;
  Result := Items[Count-1].CharID;
end;

function TTextMappingList.GetMaxGlyphID: uint32;
var
  gid: uint32;
  i: integer;
begin
  gid := 0;
  for i := 0 to Count-1 do
  begin
    if Items[i].GlyphID > gid then
      gid := Items[i].GlyphID;
  end;
  result := gid;
end;

procedure TTextMappingList.Insert(const AIndex: integer; const ACharID, AGlyphID: uint32);
var
  o: TTextMapping;
begin
  o := TTextMapping.Create;
  o.CharID := ACharID;
  o.GlyphID := AGlyphID;
  FList.Insert(AIndex, o);
end;

function CompareByCharID(A, B: TTextMapping): Integer; inline;
begin
  if A.CharID < B.CharID then
    Result := -1
  else if A.CharID > B.CharID then
    Result := 1
  else
    Result := 0;
end;

function CompareByCharIDPtr(A, B: Pointer): Integer;
begin
  Result := CompareByCharID(TTextMapping(A), TTextMapping(B));
end;

procedure TTextMappingList.Sort;
begin
  FList.Sort(@CompareByCharIDPtr);
end;

end.
