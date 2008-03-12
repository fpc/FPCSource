{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Classes used by version information resource

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit versiontypes;

{$MODE OBJFPC}

interface

uses
  Classes, SysUtils, resource;
  
type
  EVersionStringTableException = class(Exception);
  ENameNotAllowedException = class(EVersionStringTableException);
  EKeyNotFoundException = class(EVersionStringTableException);
  EDuplicateKeyException = class(EVersionStringTableException);

resourcestring
  SVerStrTableNameNotAllowed = 'Name ''%s'' is not a valid 8-cipher hex sequence';
  SVerStrTableKeyNotFound    = 'Key ''%s'' not found';
  SVerStrTableDuplicateKey   = 'Duplicate key ''%s''';

type
  TFileProductVersion = array[0..3] of word;
  
  TVerTranslationInfo = packed record
    language : word;
    codepage : word;
  end;
  PVerTranslationInfo = ^TVerTranslationInfo;

type

  { TVersionFixedInfo }

  TVersionFixedInfo = class
  private
    fFileVersion : TFileProductVersion;
    fProductVersion : TFileProductVersion;
    fFileFlagsMask : longword;
    fFileFlags : longword;
    fFileOS : longword;
    fFileType : longword;
    fFileSubType : longword;
    fFileDate : qword;
  protected
  public
    constructor Create;
    property FileVersion : TFileProductVersion read fFileVersion write fFileVersion;
    property ProductVersion : TFileProductVersion read fProductVersion write fProductVersion;
    property FileFlagsMask : longword read fFileFlagsMask write fFileFlagsMask;
    property FileFlags : longword read fFileFlags write fFileFlags;
    property FileOS : longword read fFileOS write fFileOS;
    property FileType : longword read fFileType write fFileType;
    property FileSubType : longword read fFileSubType write fFileSubType;
    property FileDate : qword read fFileDate write fFileDate;
  end;

  { TVersionStringTable }

  TVersionStringTable = class
  private
    fName : string;
    fKeys : TStringList;
    fValues : TStringList;
    function GetCount : integer;
    function GetKey(index : integer) : string;
    function GetValue(index : integer) : string; overload;
    function GetValue(aKey : string) : string; overload;
    function CheckName(const aName : string) : boolean;
    function KeyToIndex(const aKey : string) : integer;
    procedure SetValue(index : integer; aValue : string); overload;
    procedure SetValue(aKey : string; aValue : string); overload;
  protected
  public
    constructor Create(const aName : string);
    destructor Destroy; override;
    procedure Add(const aKey,aValue : string);
    procedure Clear;
    procedure Delete(const aIndex : integer); overload;
    procedure Delete(const aKey : string); overload;
    property Name : string read fName;
    property Count : integer read GetCount;
    property Keys[index : integer] : string read GetKey;
    property ValuesByIndex[index : integer] : string read GetValue write SetValue;
    property Values[Key : string] : string read GetValue write SetValue; default;
  end;


  { TVersionStringFileInfo }

  TVersionStringFileInfo = class
  private
    fList : TFPList;
  protected
    function GetCount : integer;
    function GetItem(index : integer) : TVersionStringTable;
    procedure SetItem(index : integer; aValue : TVersionStringTable);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aStrTable : TVersionStringTable);
    procedure Clear;
    procedure Delete(const aIndex : integer);
    property Count : integer read GetCount;
    property Items[index : integer] : TVersionStringTable read GetItem write SetItem; default;
  end;
  
  { TVersionVarFileInfo }

  TVersionVarFileInfo = class
  private
    fList : TFPList;
  protected
    function GetCount : integer;
    function GetItem(index : integer) : TVerTranslationInfo;
    procedure SetItem(index : integer; aValue : TVerTranslationInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aInfo : TVerTranslationInfo);
    procedure Clear;
    procedure Delete(const aIndex : integer);
    property Count : integer read GetCount;
    property Items[index : integer] : TVerTranslationInfo read GetItem write SetItem; default;
  end;


implementation

uses
  versionconsts;

{ TVersionStringTable }

function TVersionStringTable.GetCount: integer;
begin
  Result:=fKeys.Count;
end;

function TVersionStringTable.GetKey(index: integer): string;
begin
  Result:=fKeys[index];
end;

function TVersionStringTable.GetValue(index: integer): string;
begin
  Result:=fValues[index];
end;

function TVersionStringTable.GetValue(aKey: string): string;
var idx : integer;
begin
  idx:=KeyToIndex(aKey);
  if idx=-1 then
    raise EKeyNotFoundException.CreateFmt(SVerStrTableKeyNotFound,[aKey]);
  Result:=fValues[idx];
end;

procedure TVersionStringTable.SetValue(index: integer; aValue: string);
begin
  fValues[index]:=aValue;
end;

procedure TVersionStringTable.SetValue(aKey: string; aValue: string);
var idx : integer;
begin
  idx:=KeyToIndex(aKey);
  if idx=-1 then
    raise EKeyNotFoundException.CreateFmt(SVerStrTableKeyNotFound,[aKey]);
  fValues[idx]:=aValue;
end;

function TVersionStringTable.CheckName(const aName: string): boolean;
var i : integer;
begin
  Result:=false;
  if length(aName)<>8 then exit;
  for i:=1 to 8 do
    if not (aName[i] in ['0'..'9','A'..'F','a'..'f']) then exit;
  Result:=true;
end;

function TVersionStringTable.KeyToIndex(const aKey: string): integer;
var i : integer;
begin
  for i:=0 to fKeys.Count-1 do
    if fKeys[i]=aKey then
    begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

constructor TVersionStringTable.Create(const aName: string);
begin
  fKeys:=TStringList.Create;
  fValues:=TStringList.Create;
  if not CheckName(aName) then
    raise ENameNotAllowedException.CreateFmt(SVerStrTableNameNotAllowed,[aName]);
  fName:=aName;
end;

destructor TVersionStringTable.Destroy;
begin
  fKeys.Free;
  fValues.Free;
end;

procedure TVersionStringTable.Add(const aKey, aValue: string);
begin
  if KeyToIndex(aKey)<>-1 then
    raise EDuplicateKeyException.CreateFmt(SVerStrTableDuplicateKey,[aKey]);
  fKeys.Add(aKey);
  fValues.Add(aValue);
end;

procedure TVersionStringTable.Clear;
begin
  fKeys.Clear;
  fValues.Clear;
end;

procedure TVersionStringTable.Delete(const aIndex: integer);
begin
  fKeys.Delete(aIndex);
  fValues.Delete(aIndex);
end;

procedure TVersionStringTable.Delete(const aKey: string);
var idx : integer;
begin
  idx:=KeyToIndex(aKey);
  if idx=-1 then
    raise EKeyNotFoundException.CreateFmt(SVerStrTableKeyNotFound,[aKey]);
  Delete(idx);
end;

{ TVersionStringFileInfo }

function TVersionStringFileInfo.GetCount: integer;
begin
  Result:=fList.Count;
end;

function TVersionStringFileInfo.GetItem(index: integer): TVersionStringTable;
begin
  Result:=TVersionStringTable(fList[index]);
end;

procedure TVersionStringFileInfo.SetItem(index: integer;
  aValue: TVersionStringTable);
begin
  fList[index]:=aValue;
end;

constructor TVersionStringFileInfo.Create;
begin
  fList:=TFPList.Create;
end;

destructor TVersionStringFileInfo.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure TVersionStringFileInfo.Add(aStrTable: TVersionStringTable);
begin
  fList.Add(aStrTable);
end;

procedure TVersionStringFileInfo.Clear;
var i : integer;
begin
  for i:=0 to fList.Count-1 do
    TVersionStringTable(fList[i]).Free;
  fList.Clear;
end;

procedure TVersionStringFileInfo.Delete(const aIndex: integer);
begin
  TVersionStringTable(fList[aIndex]).Free;
  fList.Delete(aIndex);
end;

{ TVersionFixedInfo }

constructor TVersionFixedInfo.Create;
begin
  FillByte(fFileVersion,Sizeof(TFileProductVersion),0);
  FillByte(fProductVersion,Sizeof(TFileProductVersion),0);
  fFileFlagsMask:=VS_FFI_FILEFLAGSMASK;
  fFileFlags:=0;
  fFileOS:=VOS_NT_WINDOWS32;
  fFileType:=VFT_APP;
  fFileSubType:=VFT2_UNKNOWN;
  fFileDate:=0;
end;

{ TVersionVarFileInfo }

function TVersionVarFileInfo.GetCount: integer;
begin
  Result:=fList.Count;
end;

function TVersionVarFileInfo.GetItem(index: integer): TVerTranslationInfo;
begin
  Result:=PVerTranslationInfo(fList[index])^;
end;

procedure TVersionVarFileInfo.SetItem(index: integer;
  aValue: TVerTranslationInfo);
var p1,p2 : PVerTranslationInfo;
begin
  p1:=PVerTranslationInfo(fList[index]);
  FreeMem(p1);
  GetMem(p2,sizeof(TVerTranslationInfo));
  p2^:=aValue;
  fList[index]:=p2;
end;

constructor TVersionVarFileInfo.Create;
begin
  fList:=TFPList.Create;
end;

destructor TVersionVarFileInfo.Destroy;
begin
  Clear;
  fList.Free;
end;

procedure TVersionVarFileInfo.Add(aInfo: TVerTranslationInfo);
var p : PVerTranslationInfo;
begin
  GetMem(p,sizeof(TVerTranslationInfo));
  p^:=aInfo;
  fList.Add(p);
end;

procedure TVersionVarFileInfo.Clear;
var p : PVerTranslationInfo;
    i : integer;
begin
  for i:=0 to fList.Count-1 do
  begin
    p:=PVerTranslationInfo(fList[i]);
    FreeMem(p);
  end;
  fList.Clear;
end;

procedure TVersionVarFileInfo.Delete(const aIndex: integer);
var p : PVerTranslationInfo;
begin
  p:=PVerTranslationInfo(fList[aIndex]);
  FreeMem(p);
  fList.Delete(aIndex);
end;

end.
