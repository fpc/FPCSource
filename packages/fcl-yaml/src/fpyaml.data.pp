{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML data classes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpyaml.data;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.DateUtils, System.Classes, System.Contnrs, fpyaml.types;
{$ELSE}
uses SysUtils, DateUtils, Classes, Contnrs, fpyaml.types, fpjson;
{$ENDIF}


Type

  TYAMLMapItem = Class;

  TYAMLEnumerator = Class
    function GetCurrent: TYAMLMapItem; virtual; abstract;
    function MoveNext : Boolean; virtual; abstract;
    property Current: TYAMLMapItem read GetCurrent;
  end;

  { TYAMLData }

  TYAMLData = class
  private
    FAnchor: TYAMLString;
    FTag: TYAMLString;
  Protected
    function GetItem(aIndex : Integer): TYAMLData; virtual;
    function GetItemCount: Integer; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsDouble: Double; virtual;
    function GetAsInt64: Int64; virtual;
    function GetAsInteger: Integer; virtual;
    function GetIsNull: Boolean; virtual;
    function GetAsString: TYAMLString; virtual;
    function GetAsDateTime: TDateTime; virtual;
    class function allowAnchor : Boolean; virtual;
    class function allowTag : Boolean; virtual;
    class function DefaultTag : TYAMLString; virtual;
  Public
    constructor Create(); virtual;
    constructor Create(const aAnchor : TYAMLString; const aTag : TYAMLString = '');
    Function Clone : TYAMLData; virtual; abstract;
    Function Equals(aData : TYAMLData; aStrict : Boolean = False) : Boolean; overload;virtual;
    Function GetEnumerator: TYAMLEnumerator; virtual; abstract;
    Property Anchor : TYAMLString Read FAnchor;
    Property Tag : TYAMLString Read FTag;
    Property Items[aIndex : Integer] : TYAMLData Read GetItem; default;
    Property Count : Integer Read GetItemCount;
    Property AsString : TYAMLString Read GetAsString;
    Property AsDouble : Double Read GetAsDouble;
    Property AsInteger : Integer Read GetAsInteger;
    Property AsInt64 : Int64 Read GetAsInt64;
    Property AsBoolean : Boolean Read GetAsBoolean;
    Property AsDateTime : TDateTime Read GetAsDateTime;
    Property IsNull : Boolean Read GetIsNull;
  end;
  TYAMLDataClass = Class of TYAMLData;

  { TYAMLTagData }

  TYAMLTagData = class(TYAMLData)
  private
    FData: TYAMLTag;
  type

    { TYAMLTagDataEnumerator }

    TYAMLTagDataEnumerator = Class(TYAMLEnumerator)
      FOld,
      FCurrent : TYAMLMapItem;
      Destructor Destroy; override;
      function GetCurrent: TYAMLMapItem; override;
      function MoveNext : Boolean; override;
      Constructor Create(aScalar : TYAMLTagData);
    end;
  Public
    function Clone : TYAMLData; override;
    function GetEnumerator: TYAMLEnumerator; override;
    Property Data : TYAMLTag Read FData Write FData;
  end;

  { TYAMLScalar }

  TYAMLScalar = Class(TYAMLData)
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsDouble: Double; override;
    function GetAsInt64: Int64; override;
    function GetAsInteger: Integer; override;
    function GetAsString: TYAMLString; override;
    function GetIsNull: Boolean; override;
    function GetAsDateTime : TDateTime; override;
    Type
       TYAMLScalarEnumerator = Class(TYAMLEnumerator)
         FOld,
         FCurrent : TYAMLMapItem;
         Destructor Destroy; override;
         function GetCurrent: TYAMLMapItem; override;
         function MoveNext : Boolean; override;
         Constructor Create(aScalar : TYAMLScalar);
       end;
  private
    FKind: TYAMLScalarKind;
    FValue: TYAMLString;
  Public
    constructor Create(const aValue: TYAMLString; const aTag: TYAMLTagType = yttString; aKind: TYAMLScalarKind = yskPlain);
    Constructor Create(aValue : Integer; aStyle : TYAMLScalarKind = yskPlain);
    Constructor Create(aValue : Double; aStyle : TYAMLScalarKind = yskPlain);
    Constructor Create(aValue : Int64; aStyle : TYAMLScalarKind = yskPlain);
    Constructor Create(aValue : Boolean; aStyle : TYAMLScalarKind = yskPlain);
    Constructor CreateDateTime(aValue : TDateTime; aStyle : TYAMLScalarKind = yskPlain);
    Constructor Create(aStyle : TYAMLScalarKind = yskPlain);
    Function Clone : TYAMLData; override;
    Function GetEnumerator: TYAMLEnumerator; override;
    function Equals(aData: TYAMLData; aStrict: Boolean=False): Boolean; override;
    Property Value : TYAMLString Read FValue Write FValue;
    Property Kind : TYAMLScalarKind Read FKind Write FKind;
  end;

  { TYAMLDataList }

  TYAMLDataList = class(TYAMLData)
  Private
    FList : TFPObjectList;
  Protected
    Type
       TYAMLListEnumerator = Class(TYAMLEnumerator)
         FIndex : Integer;
         FKey : TYAMLScalar;
         FList : TYAMLDataList;
         FItem : TYAMLMapItem;
         function GetCurrent: TYAMLMapItem; override;
         function MoveNext : Boolean; override;
         Constructor Create(aList : TYAMLDataList);
         destructor destroy; override;
       end;
  Protected
    function GetItem(aIndex : Integer): TYAMLData; override;
    function GetItemCount: Integer; override;
  Public
    constructor Create; override; overload;
    destructor destroy; override;
    Function Clone : TYAMLData; override;
    Function GetEnumerator: TYAMLEnumerator; override;
    Function Equals(aData : TYAMLData; aStrict : Boolean = False) : Boolean; override;
    Function Extract(aIndex : Integer): TYAMLData;
    procedure Delete(aIndex : Integer);
    Function Add(aData : TYAMLData) : TYAMLData;
    Function Add(const aData : TYAMLString) : TYAMLData;
    Function Add(aData : Integer) : TYAMLData;
    Function Add(aData : Double) : TYAMLData;
    Function Add(aData : Boolean) : TYAMLData;
    Function Add(aData : Int64) : TYAMLData;
  end;

  { TYAMLSequence }
  TYAMLSequence = Class(TYAMLDataList)
  private
    FKind: TYAMLCollectionKind;
  Protected
    Class Function DefaultTag: TYAMLString; override;
  Public
    Function Clone : TYAMLData; override;
    Property Kind : TYAMLCollectionKind Read FKind Write FKind;
  end;


  TYAMLMapItem = class
  Private
    Fkey,Fvalue : TYAMLData;
  Public
    constructor Create(aKey,aValue : TYAMLData);
    destructor Destroy; override;
    Property Key : TYAMLData Read FKey;
    Property Value : TYAMLData Read FValue;
  end;

  { TYAMLMapping }

  TYAMLMapping = Class(TYAMLData)
  Protected
  type
      TYAMLMappingEnumerator = Class(TYAMLEnumerator)
        FIndex : Integer;
        FList : TYAMLMapping;
        function GetCurrent: TYAMLMapItem; override;
        function MoveNext : Boolean; override;
        Constructor Create(aList : TYAMLMapping);
      end;

      { TYAMLMappingKeyEnumerator }

      TYAMLMappingKeyEnumerator = Class
        FIndex : Integer;
        FList : TYAMLMApping;
        function GetCurrent: TYAMLData;
        function MoveNext : Boolean;
        Constructor Create(aList : TYAMLMapping);
      end;
  Private
    FKind: TYAMLCollectionKind;
    FList : TFPObjectList;
    Function IndexOfKey(aKey : TYAMLData): Integer;
    Function IndexOfKeyValue(aKey : TYAMLData): Integer;
    function GetByKey(aKey : TYAMLData): TYAMLData;
    function GetByKeyValue(aKey : TYAMLData): TYAMLData;
    function GetKey(aIndex : Integer): TYAMLData;
    function GetMapping(aIndex : Integer): TYAMLMapItem;
  Protected
    function GetItem(aIndex : Integer): TYAMLData; override;
    function GetItemCount: Integer; override;
    class function DefaultTag : TYAMLString; override;
    property Mapping[aIndex : Integer] : TYAMLMapItem Read GetMapping;
  Public
    constructor Create; override;
    destructor destroy; override;
    Procedure Add(aKey,aValue : TYAMLData);
    Function Equals(aData : TYAMLData; aStrict : Boolean = False) : Boolean; override;
    Function GetEnumerator: TYAMLEnumerator; override;
    Procedure Delete(aIndex : Integer);
    Procedure Delete(aKey : TYAMLData);
    Procedure Delete(const aKey : TYAMLString);
    Function Extract(aIndex : Integer) : TYAMLData;
    Function Extract(aKey : TYAMLData) : TYAMLData;
    function Extract(const aKey : TYAMLString) : TYAMLData;
    Function Keys: TYamlMappingKeyEnumerator;
    Function Clone : TYAMLData; override;
    Function Add(const aKey : TYAMLString; aData : TYAMLData) : TYAMLData;
    Function Add(const aKey : TYAMLString; const aData : TYAMLString) : TYAMLData;
    Function Add(const aKey : TYAMLString; aData : Integer) : TYAMLData;
    Function Add(const aKey : TYAMLString; aData : Double) : TYAMLData;
    Function Add(const aKey : TYAMLString; aData : Boolean) : TYAMLData;
    Function Add(const aKey : TYAMLString; aData : Int64) : TYAMLData;
    Property Kind : TYAMLCollectionKind Read FKind Write FKind;
    property Key[aIndex : Integer] : TYAMLData Read GetKey;
    // Find by pointer
    property ValueByKey[aKey : TYAMLData] : TYAMLData Read GetByKey;
    // Find by value.
    property ValueByKeyValue[aKey : TYAMLData] : TYAMLData Read GetByKeyValue;
  end;

  { TYAMLGlobalDataList }

  TYAMLGlobalDataList = class(TYAMLDataList)
    class function allowAnchor : Boolean; override;
    class function allowTag : Boolean; override;
  end;

  { TYAMLDocument }

  TYAMLDocument = Class(TYAMLGlobalDataList)
  Private
    FVersion: TYAMLVersion;
  Public
    function Clone : TYAMLData; override;
    Property Version : TYAMLVersion read FVersion Write FVersion;
  end;
  TYAMLDocumentArray = Array of TYAMLDocument;

  { TYAMLStream }

  TYAMLStream = class(TYAMLGlobalDataList)
  private
    function GetDocuments: TYAMLDocumentArray;
    function GetDocumentCount: Integer;
  Public
    Property Documents : TYAMLDocumentArray Read GetDocuments;
    Property DocumentCount : Integer Read GetDocumentCount;
  end;


var
  YAMLFormatSettings : TFormatSettings;

implementation

uses fpyaml.strings;

{ TYAMLData }

function TYAMLData.GetAsBoolean: Boolean;

begin
  Raise EYAML.CreateFmt(SErrIsNotA,[ClassName,'boolean']);
  Result:=False; // Avoid compiler warning;
end;


function TYAMLData.GetAsDouble: Double;

begin
  Raise EYAML.CreateFmt(SErrIsNotA,[ClassName,'double']);
  Result:=0.0; // Avoid compiler warning;
end;


function TYAMLData.GetAsInt64: Int64;

begin
  Raise EYAML.CreateFmt(SErrIsNotA,[ClassName,'int64']);
  Result:=0; // Avoid compiler warning;
end;


function TYAMLData.GetAsInteger: Integer;

begin
  Raise EYAML.CreateFmt(SErrIsNotA,[ClassName,'integer']);
  Result:=0; // Avoid compiler warning;
end;


function TYAMLData.GetAsString: TYAMLString;

begin
  Raise EYAML.CreateFmt(SErrIsNotA,[ClassName,'TYAMLString']);
  Result:=''; // Avoid compiler warning;
end;


function TYAMLData.GetAsDateTime: TDateTime;

begin
  Raise EYAML.CreateFmt(SErrIsNotA,[ClassName,'TDateTime']);
  Result:=0; // Avoid compiler warning
end;


function TYAMLData.GetIsNull: Boolean;

begin
  Result:=False;
end;


function TYAMLData.GetItem(aIndex : Integer): TYAMLData;

begin
  Raise EListError.CreateFmt(SErrIndexOutOfBounds ,[aIndex]);
  Result:=Nil; // Avoid compiler warning
end;


function TYAMLData.GetItemCount: Integer;

begin
  Result:=0;
end;


class function TYAMLData.allowAnchor: Boolean;

begin
  Result:=True;
end;


class function TYAMLData.allowTag: Boolean;

begin
  Result:=True;
end;


class function TYAMLData.DefaultTag: TYAMLString;

begin
  Result:='';
end;


constructor TYAMLData.Create();

begin
  if AllowTag then
    FTag:=DefaultTag;
end;


constructor TYAMLData.Create(const aAnchor: TYAMLString; const aTag: TYAMLString);

begin
  Create;
  if allowAnchor then
    FAnchor:=aAnchor;
  if allowTag then
    FTag:=aTag;
end;


function TYAMLData.Equals(aData: TYAMLData; aStrict: Boolean): Boolean;

begin
  Result:=(aData.Anchor=Anchor) and (aData.Tag=Tag);
  if aStrict then; // Avoid compiler warning
end;

{ TYAMLTagData }

function TYAMLTagData.Clone: TYAMLData;

begin
  Result:=TYAMLTagData(Self.ClassType).Create('',Self.Tag);
  TYAMLTagData(Result).Data:=Self.Data;
end;


function TYAMLTagData.GetEnumerator: TYAMLEnumerator;

begin
  Result:=TYAMLTagDataEnumerator.Create(Self);
end;


{ TYAMLTagData.TYAMLTagDataEnumerator }

destructor TYAMLTagData.TYAMLTagDataEnumerator.Destroy;

begin
  MoveNext; // Make sure value is in old
  FOld.FValue:=nil;
  FreeAndNil(FOld);
end;


function TYAMLTagData.TYAMLTagDataEnumerator.GetCurrent: TYAMLMapItem;

begin
  Result:=FOld;
end;


function TYAMLTagData.TYAMLTagDataEnumerator.MoveNext: Boolean;

begin
  Result:=Assigned(FCurrent);
  if Result then
    begin
    FOld:=FCurrent;
    FCurrent:=Nil;
    end;
end;


constructor TYAMLTagData.TYAMLTagDataEnumerator.Create(aScalar: TYAMLTagData);

begin
  FCurrent:=TYAMLMapItem.Create(TYAMLScalar.Create('',yttCustom,yskPlain),aScalar);
end;


{ TYAMLScalar }

function TYAMLScalar.GetAsBoolean: Boolean;

begin
  Result:=StrToBool(Value);
end;


function TYAMLScalar.GetAsDouble: Double;

begin
  Result:=StrToFloat(Value,YAMLFormatSettings);
end;


function TYAMLScalar.GetAsInt64: Int64;

begin
  Result:=StrToInt64(Value);
end;


function TYAMLScalar.GetAsInteger: Integer;

begin
  Result:=StrToInt(Value);
end;


function TYAMLScalar.GetAsString: TYAMLString;

begin
  Result:=Value;
end;


function TYAMLScalar.GetIsNull: Boolean;

begin
  Result:=(Value='') and (Tag=YAMLTagNames[yttNull]);
end;


function TYAMLScalar.GetAsDateTime: TDateTime;

begin
  Result:=ISO8601ToDate(Value);
end;


constructor TYAMLScalar.Create(const aValue: TYAMLString; const aTag: TYAMLTagType; aKind: TYAMLScalarKind);

begin
  Inherited Create('',YAMLTagNames[aTag]);
  FValue:=aValue;
  FKind:=aKind;
end;


constructor TYAMLScalar.Create(aValue: Integer; aStyle: TYAMLScalarKind);

begin
  Create(IntToStr(aValue),yttInteger,aStyle);
end;


constructor TYAMLScalar.Create(aValue: Double; aStyle: TYAMLScalarKind);

begin
  Create(FloatToStr(aValue,YAMLFormatSettings),yttFloat,aStyle);
end;


constructor TYAMLScalar.Create(aValue: Int64; aStyle: TYAMLScalarKind);

begin
  Create(IntToStr(aValue),yttInteger,aStyle);
end;


constructor TYAMLScalar.Create(aValue: Boolean; aStyle: TYAMLScalarKind);

const
  BoolStrs : Array[Boolean] of TYAMLString = ('false','true');

begin
  Create(BoolStrs[aValue],yttBoolean,aStyle);
end;


constructor TYAMLScalar.CreateDateTime(aValue: TDateTime; aStyle: TYAMLScalarKind);

begin
  Create(DateToISO8601(aValue),yttTimeStamp,AStyle)
end;


constructor TYAMLScalar.Create(aStyle: TYAMLScalarKind);

begin
  Create('',yttNull,aStyle);
end;


function TYAMLScalar.Clone: TYAMLData;

var
  aScalar : TYAMLScalar absolute result;

begin
  Result:=TYAMLDataClass(Self.ClassType).Create('',Self.Tag);
  aScalar.Value:=Self.Value;
  aScalar.Kind:=Self.Kind;
end;


function TYAMLScalar.GetEnumerator: TYAMLEnumerator;

begin
  Result:=TYAMLScalarEnumerator.Create(Self);
end;


function TYAMLScalar.Equals(aData: TYAMLData; aStrict: Boolean): Boolean;

begin
  if aStrict then
    Result:=inherited Equals(aData, aStrict)
  else
    Result:=True;
  Result:=Result and (aData is TYAMLScalar) and (FValue=TYAMLScalar(aData).FValue)
end;


{ TYAMLScalar.TYAMLScalarEnumerator }

destructor TYAMLScalar.TYAMLScalarEnumerator.Destroy;

begin
  MoveNext; // Make sure value is in old
  FOld.FValue:=nil;
  FreeAndNil(FOld);
end;


function TYAMLScalar.TYAMLScalarEnumerator.GetCurrent: TYAMLMapItem;

begin
  Result:=FOld;
end;


function TYAMLScalar.TYAMLScalarEnumerator.MoveNext: Boolean;

begin
  Result:=Assigned(FCurrent);
  if Result then
    begin
    FOld:=FCurrent;
    FCurrent:=Nil;
    end;
end;


constructor TYAMLScalar.TYAMLScalarEnumerator.Create(aScalar: TYAMLScalar);

begin
  FCurrent:=TYAMLMapItem.Create(TYAMLScalar.Create('',yttCustom,yskPlain),aScalar);
end;


{ TYAMLDataList }

function TYAMLDataList.GetItem(aIndex: Integer): TYAMLData;

begin
  Result:=TYAMLData(FList[aIndex]);
end;


function TYAMLDataList.GetItemCount: Integer;

begin
  Result:=FList.Count;
end;


function TYAMLDataList.Equals(aData: TYAMLData; aStrict: Boolean): Boolean;

var
  aList : TYAMLDataList absolute aData;
  I : Integer;

begin
  Result:=(aData=Self);
  if Result then exit;
  Result:=Inherited Equals(aData,aStrict);
  if not Result then exit;
  Result:=(aData.ClassType=Self.ClassType);
  if not Result then exit;
  Result:=(aList.Count=Self.Count);
  if not Result then exit;
  I:=0;
  While Result and (I<Count) do
    begin
    Result:=Items[i].Equals(aList.Items[i]);
    Inc(I);
    end;
end;

function TYAMLDataList.Extract(aIndex: Integer): TYAMLData;

begin
  Result:=TYAMLData(FList.Extract(FList[aIndex]));
end;


procedure TYAMLDataList.Delete(aIndex: Integer);

begin
  FList.Delete(aIndex);
end;


constructor TYAMLDataList.Create;

begin
  inherited Create;
  FList:=TFPObjectList.Create(True);
end;


destructor TYAMLDataList.destroy;

begin
  FreeAndNil(FList);
  Inherited;
end;


function TYAMLDataList.Clone: TYAMLData;

var
  aList : TYAMLDataList absolute Result;
  I : Integer;

begin
  Result:=TYAMLDataClass(Self.ClassType).Create('',Self.Tag);
  For I:=0 to Count-1 do
    aList.Add(GetItem(I).Clone);
end;


function TYAMLDataList.GetEnumerator: TYAMLEnumerator;

begin
  Result:=TYAMLListEnumerator.Create(Self);
end;


function TYAMLDataList.Add(aData: TYAMLData): TYAMLData;

begin
  Result:=aData;
  FList.Add(Result);
end;


function TYAMLDataList.Add(const aData: TYAMLString): TYAMLData;

begin
  Result:=Add(TYamlScalar.Create(aData))
end;


function TYAMLDataList.Add(aData: Integer): TYAMLData;

begin
  Result:=Add(TYamlScalar.Create(aData))
end;


function TYAMLDataList.Add(aData: Double): TYAMLData;

begin
  Result:=Add(TYamlScalar.Create(aData))
end;


function TYAMLDataList.Add(aData: Boolean): TYAMLData;

begin
  Result:=Add(TYamlScalar.Create(aData))
end;


function TYAMLDataList.Add(aData: Int64): TYAMLData;

begin
  Result:=Add(TYamlScalar.Create(aData))
end;


{ TYAMLDataList.TYAMLListEnumerator }

function TYAMLDataList.TYAMLListEnumerator.GetCurrent: TYAMLMapItem;

begin
  FItem.FValue:=FList.Items[FIndex];
  Result:=FItem;
end;


function TYAMLDataList.TYAMLListEnumerator.MoveNext: Boolean;

begin
  Inc(FIndex);
  Result:=(FIndex<FList.Count)
end;


constructor TYAMLDataList.TYAMLListEnumerator.Create(aList: TYAMLDataList);

begin
  Findex:=-1;
  FList:=aList;
  FKey:=TYAMLScalar.Create('',yttCustom,yskPlain);
  FItem:=TYAMLMapItem.Create(FKey,Nil);
end;


destructor TYAMLDataList.TYAMLListEnumerator.destroy;

begin
  FItem.FValue:=Nil;
  FKey:=Nil;
  FreeAndNil(FItem); // will free ey
  inherited destroy;
end;

{ TYAMLSequence }

class function TYAMLSequence.DefaultTag: TYAMLString;

begin
  Result:=YAMLTagNames[yttSequence];
end;


function TYAMLSequence.Clone: TYAMLData;

begin
  Result:=inherited Clone;
  TYAMLSequence(Result).Kind:=Self.Kind;
end;


{ TYAMLMapping }

function TYAMLMapping.GetMapping(aIndex : Integer): TYAMLMapItem;

begin
  Result:=TYAMLMapItem(FList[aIndex]);
end;


function TYAMLMapping.GetKey(aIndex : Integer): TYAMLData;

begin
  Result:=Mapping[aIndex].Key;
end;


function TYAMLMapping.IndexOfKey(aKey: TYAMLData): Integer;

begin
  Result:=0;
  While (Result<Count) and (Key[Result]<>aKey) do
    Inc(Result);
  if Result>=Count then
    Result:=-1;
end;


function TYAMLMapping.IndexOfKeyValue(aKey: TYAMLData): Integer;

begin
  Result:=IndexOfKey(aKey);
  if Result<>-1 then
    Exit;
  Result:=0;
  While (Result<Count) and not Key[Result].Equals(aKey) do
    Inc(Result);
  if Result>=Count then
    Result:=-1;
end;

function TYAMLMapping.GetByKey(aKey : TYAMLData): TYAMLData;

var
  Idx : Integer;

begin
  Idx:=IndexOfKey(aKey);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=Items[Idx];
end;


function TYAMLMapping.GetByKeyValue(aKey : TYAMLData): TYAMLData;

var
  I : Integer;

begin
  if aKey=nil then
    Exit(Nil);
  Result:=ValueByKey[aKey];
  if Result=Nil then
    For I:=0 to Count-1 do
      if Mapping[i].Key.Equals(aKey) then
        Exit(Mapping[i].Value);
end;


function TYAMLMapping.GetItem(aIndex: Integer): TYAMLData;

begin
  Result:=Mapping[aIndex].Value;
end;


function TYAMLMapping.GetItemCount: Integer;

begin
  Result:=FList.Count;
end;


class function TYAMLMapping.DefaultTag: TYAMLString;
begin
  Result:=YAMLTagNames[yttMap];
end;


constructor TYAMLMapping.Create;

begin
  inherited Create;
  FList:=TFPObjectList.Create(True);
end;


destructor TYAMLMapping.destroy;

begin
  FreeAndNil(FList);
  inherited destroy;
end;


procedure TYAMLMapping.Add(aKey, aValue: TYAMLData);

var
  lMap : TYAMLMapItem;

begin
  lMap:=TYAMLMapItem.Create(aKey,aValue);
  FList.Add(lMap);
end;


function TYAMLMapping.Equals(aData: TYAMLData; aStrict: Boolean): Boolean;

var
  aMapping : TYAMLMapping absolute aData;
  aValue : TYAMLData;
  I : Integer;

begin
  Result:=(aData=Self);
  if Result then exit;
  if aStrict then
    begin
    Result:=Inherited Equals(aData,aStrict);
    if not Result then
      exit;
    end;
  Result:=(aData.ClassType=Self.ClassType);
  if not Result then
    exit;
  Result:=(aMapping.Count=Self.Count);
  if not Result then
    exit;
  I:=0;
  While Result and (I<Count) do
    begin
    aValue:=aMapping.ValueByKeyValue[Key[i]];
    Result:=Assigned(aValue) and Items[i].Equals(aValue,aStrict);
    Inc(I);
    end;
end;


function TYAMLMapping.GetEnumerator: TYAMLEnumerator;

begin
  Result:=TYAMLMappingEnumerator.Create(Self);
end;


procedure TYAMLMapping.Delete(aIndex: Integer);

begin
  FList.Delete(aIndex);
end;


procedure TYAMLMapping.Delete(aKey: TYAMLData);

var
  Idx : Integer;

begin
  Idx:=IndexOfKeyValue(aKey);
  if (Idx<>-1) then
    FList.Delete(Idx);
end;


procedure TYAMLMapping.Delete(const aKey: TYAMLString);

var
  lKey : TYAMLScalar;

begin
  lKey:=TYAMLScalar.Create(aKey);
  try
    Delete(lKey);
  finally
    lKey.Free;
  end;
end;


function TYAMLMapping.Extract(aIndex: Integer): TYAMLData;

var
  M : TYAMLMapItem;
begin
  Result:=Nil;
  M:=Mapping[aIndex];
  M:=TYAMLMapItem(FList.Extract(M));
  if Assigned(M) then
    begin
    Result:=M.Value;
    M.Fvalue:=Nil;
    M.Free;
    end;
end;


function TYAMLMapping.Extract(aKey: TYAMLData): TYAMLData;

var
  Idx : Integer;
begin
  Idx:=IndexOfKeyValue(aKey);
  If Idx=-1 then
    Result:=Nil
  else
    Result:=Extract(Idx);
end;


function TYAMLMapping.Extract(const aKey: TYAMLString): TYAMLData;

var
  lKey : TYAMLScalar;

begin
  lKey:=TYAMLScalar.Create(aKey);
  try
    Result:=Extract(lKey);
  finally
    lKey.Free;
  end;
end;


function TYAMLMapping.Keys: TYamlMappingKeyEnumerator;

begin
  Result:=TYAMLMappingKeyEnumerator.Create(Self);
end;


function TYAMLMapping.Clone: TYAMLData;

var
  I : Integer;
  lKey,lValue : TYAMLData;
  aMap : TYAMLMapping absolute Result;

begin
  Result:=TYAMLDataClass(Self.ClassType).Create('',Self.tag);
  aMap.Kind:=Self.Kind;
  For I:=0 to Count-1 do
    With Mapping[i] do
      begin
      lKey:=Key.Clone;
      lValue:=Value.Clone;
      aMap.Add(lKey,lValue);
      end;
end;

function TYAMLMapping.Add(const aKey: TYAMLString; aData: TYAMLData): TYAMLData;

begin
  Result:=aData;
  Add(TYAMLScalar.Create(aKey),Result);
end;


function TYAMLMapping.Add(const aKey: TYAMLString; const aData: TYAMLString): TYAMLData;

begin
  Result:=TYAMLScalar.Create(aData);
  Add(TYAMLScalar.Create(aKey),Result);
end;


function TYAMLMapping.Add(const aKey: TYAMLString; aData: Integer): TYAMLData;

begin
  Result:=TYAMLScalar.Create(aData);
  Add(TYAMLScalar.Create(aKey),Result);
end;


function TYAMLMapping.Add(const aKey: TYAMLString; aData: Double): TYAMLData;

begin
  Result:=TYAMLScalar.Create(aData);
  Add(TYAMLScalar.Create(aKey),Result);
end;


function TYAMLMapping.Add(const aKey: TYAMLString; aData: Boolean): TYAMLData;

begin
  Result:=TYAMLScalar.Create(aData);
  Add(TYAMLScalar.Create(aKey),Result);
end;


function TYAMLMapping.Add(const aKey: TYAMLString; aData: Int64): TYAMLData;

begin
  Result:=TYAMLScalar.Create(aData);
  Add(TYAMLScalar.Create(aKey),Result);
end;

{ TYAMLMapping.TYAMLMappingtEnumerator }

function TYAMLMapping.TYAMLMappingEnumerator.GetCurrent: TYAMLMapItem;

begin
  Result:=FList.Mapping[FIndex];
end;


function TYAMLMapping.TYAMLMappingEnumerator.MoveNext: Boolean;

begin
  Inc(FIndex);
  Result:=FIndex<FList.Count;
end;


constructor TYAMLMapping.TYAMLMappingEnumerator.Create(aList: TYAMLMapping);

begin
  FIndex:=-1;
  FList:=aList;
end;

{ TYAMLMapping.TYAMLMappingKeyEnumerator }

function TYAMLMapping.TYAMLMappingKeyEnumerator.GetCurrent: TYAMLData;

begin
  Result:=FList.Mapping[FIndex].Value;
end;


function TYAMLMapping.TYAMLMappingKeyEnumerator.MoveNext: Boolean;

begin
  Inc(FIndex);
  Result:=FIndex<FList.Count;
end;


constructor TYAMLMapping.TYAMLMappingKeyEnumerator.Create(aList: TYAMLMapping);

begin
  FIndex:=-1;
  FList:=aList;
end;


{ TYAMLGlobalDataList }

class function TYAMLGlobalDataList.allowAnchor: Boolean;

begin
  Result:=False;
end;


class function TYAMLGlobalDataList.allowTag: Boolean;

begin
  Result:=False;
end;

{ TYAMLDocument }

function TYAMLDocument.Clone: TYAMLData;

begin
  Result:=inherited Clone;
  TYAMLDocument(Result).Version:=Self.Version;
end;


{ TYAMLMapping.TMapItem }

constructor TYAMLMapItem.Create(aKey, aValue: TYAMLData);

begin
  FKey:=aKey;
  FValue:=aValue;
end;


destructor TYAMLMapItem.destroy;

begin
  FreeAndNil(FKey);
  FreeAndNil(FValue);
  inherited destroy;
end;


{ TYAMLStream }

function TYAMLStream.GetDocuments: TYAMLDocumentArray;

var
  I, lCount : Integer;

begin
  Result:=[];
  SetLength(Result,GetDocumentCount);
  lCount:=0;
  I:=0;
  While (I<Count) do
    begin
    If (Items[i] is TYAMLDocument) then
      begin
      Result[lCount]:=TYAMLDocument(Items[i]);
      inc(lCount);
      end;
    Inc(I);
    end;
end;


function TYAMLStream.GetDocumentCount: Integer;

var
  I : Integer;

begin
  Result:=0;
  for I:=0 to Count-1 do
    If Items[i] is TYAMLDocument then
      Inc(Result);
end;


initialization
  YAMLFormatSettings:=DefaultFormatSettings;
  YAMLFormatSettings.DecimalSeparator:='.';
end.

