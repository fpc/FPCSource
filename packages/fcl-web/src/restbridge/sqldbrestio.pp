{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST Dispatcher basic I/O environment.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sqldbrestio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, sqldb, db, httpdefs, sqldbrestschema;

Type
  TVariableSource = (vsNone,vsQuery,vsContent,vsRoute,vsHeader);
  TVariableSources = Set of TVariableSource;

  TRestOutputOption = (ooMetadata,ooSparse,ooHumanReadable);
  TRestOutputOptions = Set of TRestOutputOption;

  TNullBoolean = (nbNone,nbFalse,nbTrue);
  TNullBooleans = set of TNullBoolean;

Const
  AllVariableSources = [Low(TVariableSource)..High(TVariableSource)];
  allOutputOptions = [Low(TRestOutputOption)..High(TRestOutputOption)];


Type
  TRestStringProperty = (rpDateFormat,
                         rpDateTimeFormat,
                         rpTimeFormat,
                         rpDataRoot,
                         rpMetaDataRoot,
                         rpErrorRoot,
                         rpFieldNameProp,
                         rpFieldTypeProp,
                         rpFieldDateFormatProp,
                         rpFieldMaxLenProp,
                         rpHumanReadable,
                         rpFieldList,
                         rpExcludeFieldList,
                         rpConnection,
                         rpResource,
                         rpIncludeMetadata,
                         rpSparse,
                         rpRowName,
                         rpMetaDataFields,
                         rpMetaDataField,
                         rpErrorCode,
                         rpErrorMessage,
                         rpFilterEqual,
                         rpFilterLessThan,
                         rpFilterGreaterThan,
                         rpFilterLessThanEqual,
                         rpFilterGreaterThanEqual,
                         rpFilterIsNull,
                         rpLimit,
                         rpOffset,
                         rpOrderBy,
                         rpMetadataResourceName,
                         rpInputFormat,
                         rpOutputFormat,
                         rpCustomViewResourceName,
                         rpCustomViewSQLParam,
                         rpXMLDocumentRoot
                         );
  TRestStringProperties = Set of TRestStringProperty;

  TRestGetVariableEvent = Procedure (Sender : TObject; Const aName : UTF8String; Out aVal : UTF8String) of object;

  { TRestStringsConfig }

  TRestStringsConfig = Class(TPersistent)
  private
    FValues : Array[TRestStringProperty] of UTF8String;
    function GetRestPropName(AIndex: Integer): UTF8String;
    procedure SetRestPropName(AIndex: Integer; AValue: UTF8String);
  Public
    Class Function GetDefaultString(aString : TRestStringProperty) :UTF8String;
    Function GetRestString(aString : TRestStringProperty) :UTF8String;
    Procedure SetRestString(aString : TRestStringProperty; AValue :UTF8String);
    Procedure Assign(aSource : TPersistent); override;
  Published
    // Indexes here MUST match TRestProperty
    Property RESTDateFormat : UTF8String Index ord(rpDateFormat) Read GetRestPropName Write SetRestPropName;
    Property RESTDateTimeFormat : UTF8String Index ord(rpDateTimeFormat)  Read GetRestPropName Write SetRestPropName;
    Property RESTTimeFormat : UTF8String Index ord(rpTimeFormat)  Read GetRestPropName Write SetRestPropName;
    Property DataProperty : UTF8String Index ord(rpDataRoot) Read GetRestPropName Write SetRestPropName;
    Property MetaDataRoot : UTF8String Index ord(rpMetaDataRoot) Read GetRestPropName Write SetRestPropName;
    Property ErrorProperty : UTF8String Index ord(rpErrorRoot) Read GetRestPropName Write SetRestPropName;
    Property FieldNameProperty : UTF8String Index ord(rpFieldNameProp) Read GetRestPropName Write SetRestPropName;
    Property FieldTypeProperty : UTF8String Index ord(rpFieldTypeProp) Read GetRestPropName Write SetRestPropName;
    Property DateFormatProperty : UTF8String Index ord(rpFieldDateFormatProp) Read GetRestPropName Write SetRestPropName;
    Property MaxLenProperty : UTF8String Index ord(rpFieldMaxLenProp) Read GetRestPropName Write SetRestPropName;
    Property HumanReadableParam : UTF8String Index ord(rpHumanReadable) Read GetRestPropName Write SetRestPropName;
    Property FieldListParam : UTF8String Index ord(rpFieldList) Read GetRestPropName Write SetRestPropName;
    Property ExcludeFieldListParam : UTF8String Index ord(rpExcludeFieldList) Read GetRestPropName Write SetRestPropName;
    Property ConnectionParam : UTF8String Index Ord(rpConnection) Read GetRestPropName Write SetRestPropName;
    Property ResourceParam : UTF8String Index ord(rpResource) Read GetRestPropName Write SetRestPropName;
    Property IncludeMetadataParam : UTF8String Index ord(rpIncludeMetadata) Read GetRestPropName Write SetRestPropName;
    Property SparseParam : UTF8String Index Ord(rpSparse) Read GetRestPropName Write SetRestPropName;
    Property RowName : UTF8String Index Ord(rpRowName) Read GetRestPropName Write SetRestPropName;
    Property MetadataFields : UTF8String Index Ord(rpMetadataFields) Read GetRestPropName Write SetRestPropName;
    Property MetadataField : UTF8String Index Ord(rpMetadataField) Read GetRestPropName Write SetRestPropName;
    Property ErrorCode : UTF8String Index ord(rpErrorCode) Read GetRestPropName Write SetRestPropName;
    Property ErrorMessage : UTF8String Index ord(rpErrorMessage) Read GetRestPropName Write SetRestPropName;
    Property FilterParamEqual : UTF8String Index ord(rpFilterEqual) Read GetRestPropName Write SetRestPropName;
    Property FilterParamLessThan : UTF8String Index ord(rpFilterLessThan) Read GetRestPropName Write SetRestPropName;
    Property FilterParamGreaterThan : UTF8String Index ord(rpFilterGreaterThan) Read GetRestPropName Write SetRestPropName;
    Property FilterParamLessThanEqual : UTF8String Index ord(rpFilterLessThanEqual) Read GetRestPropName Write SetRestPropName;
    Property FilterParamGreaterThanEqual : UTF8String Index ord(rpFilterGreaterThanEqual) Read GetRestPropName Write SetRestPropName;
    Property FilterParamIsNull : UTF8String Index ord(rpFilterIsNull) Read GetRestPropName Write SetRestPropName;
    Property LimitParam : UTF8string Index ord(rpLimit) Read GetRestPropName Write SetRestPropName;
    Property OffsetParam : UTF8string Index ord(rpOffset) Read GetRestPropName Write SetRestPropName;
    Property SortParam : UTF8string Index ord(rpOrderBy) Read GetRestPropName Write SetRestPropName;
    Property MetadataResourceName : UTF8string Index ord(rpMetadataResourceName) Read GetRestPropName Write SetRestPropName;
    Property InputFormatParam : UTF8string Index ord(rpInputFormat) Read GetRestPropName Write SetRestPropName;
    Property OutputFormatParam : UTF8string Index ord(rpOutputFormat) Read GetRestPropName Write SetRestPropName;
    Property CustomViewResourceName : UTF8string Index ord(rpCustomViewResourceName) Read GetRestPropName Write SetRestPropName;
    Property CustomViewSQLParam : UTF8string Index ord(rpCustomViewSQLParam) Read GetRestPropName Write SetRestPropName;
    Property XMLDocumentRoot : UTF8string Index ord(rpXMLDocumentRoot) Read GetRestPropName Write SetRestPropName;
  end;

  { TRestStreamer }

  TRestStreamer = Class(TObject)
  private
    FStream: TStream;
    FOnGetVar : TRestGetVariableEvent;
    FStrings: TRestStringsConfig;
  Public
    // Registry
    Class Function GetContentType : String; virtual;
    Constructor Create(aStream : TStream;aStrings : TRestStringsConfig;aOnGetVar : TRestGetVariableEvent);
    Function GetString(aString : TRestStringProperty) : UTF8String;
    Property Strings : TRestStringsConfig Read FStrings;
    procedure InitStreaming; virtual; abstract;
    Function GetVariable(const aName : UTF8String) : UTF8String;
    Property Stream : TStream Read FStream;
  end;
  TRestStreamerClass = Class of TRestStreamer;

  TRestInputStreamer = Class(TRestStreamer)
  Public
    // Select input object aIndex. Must return False if no such object in input
    // Currently aIndex=0, but for batch operations this may later become nonzero.
    Function SelectObject(aIndex : Integer) : Boolean; virtual; abstract;
    // Return Nil if none found. If result is non-nil, caller will free.
    Function GetContentField(aName : UTF8string) : TJSONData; virtual; abstract;
    Class Procedure RegisterStreamer(Const aName : String);
    Class Procedure UnRegisterStreamer(Const aName : String);
  end;
  TRestInputStreamerClass = Class of TRestInputStreamer;

  { TRestOutputStreamer }

  TRestOutputStreamer = Class(TRestStreamer)
  private
    FOutputOptions: TRestOutputOptions;
  Protected
    procedure SetOutputOptions(AValue: TRestOutputOptions); virtual;
  Public
    Class Procedure RegisterStreamer(Const aName : String);
    Class Procedure UnRegisterStreamer(Const aName : String);
    function RequireMetadata : Boolean; virtual;
    Function FieldToString(aFieldType : TRestFieldType; F : TField) : UTF8string; virtual;
    function FieldToBase64(F: TField): UTF8String; virtual;
    Function HasOption(aOption : TRestOutputOption) : Boolean;
    Procedure CreateErrorContent(aCode : Integer; Const aMessage: String); virtual; abstract;
    Procedure CreateErrorContent(aCode : Integer; Const Fmt: String; Const Args : Array of const);
    Procedure WriteMetadata(aFieldList : TRestFieldPairArray); virtual; abstract;
    Procedure StartData; virtual; abstract;
    Procedure StartRow; virtual; abstract;
    Procedure WriteField(aPair : TRestFieldPair); virtual; abstract;
    Procedure EndRow; virtual; abstract;
    Procedure EndData; virtual; abstract;
    Procedure FinalizeOutput; virtual; abstract;
    // Set before InitStreaming is called;
    Property OutputOptions : TRestOutputOptions Read FOutputOptions Write SetOutputOptions;
  end;
  TRestOutputStreamerClass = class of TRestOutputStreamer;

  { TRestIO }

  TRestIO = Class
  private
    FConn: TSQLConnection;
    FCOnnection: UTF8String;
    FInput: TRestInputStreamer;
    FOperation: TRestOperation;
    FOutput: TRestOutputStreamer;
    FRequest: TRequest;
    FResource: TSQLDBRestResource;
    FResourceName: UTF8String;
    FResponse: TResponse;
    FRestStrings: TRestStringsConfig;
    FSchema: UTF8String;
    FTrans: TSQLTransaction;
    FContentStream : TStream;
    FUserID: String;
  Protected
  Public
    Constructor Create(aRequest : TRequest; aResponse : TResponse); virtual;
    Destructor Destroy; override;
    // Set things.
    Procedure SetIO(aInput : TRestInputStreamer;aOutput : TRestOutputStreamer);
    Procedure SetConn(aConn : TSQLConnection; ATrans : TSQLTransaction);
    Procedure SetResource(aResource : TSQLDBRestResource);
    procedure SetOperation(aOperation : TRestOperation);
    Procedure SetRestStrings(aValue : TRestStringsConfig);
    // Get things
    class function StrToNullBoolean(S: String; Strict: Boolean): TNullBoolean;
    Procedure DoGetVariable(Sender : TObject; Const aName : UTF8String; Out aVal : UTF8String);
    Function GetVariable (Const aName : UTF8String; Out aVal : UTF8String; AllowedSources : TVAriableSources = AllVariableSources) : TVariableSource; virtual;
    function GetFilterVariable(const aName: UTF8String; AFilter: TRestFieldFilter; out aValue: UTF8String): TVariableSource;
    Function GetBooleanVar(Const aName : UTF8String; aStrict : Boolean = False) : TNullBoolean;
    function GetRequestOutputOptions(aDefault: TRestOutputOptions): TRestOutputOptions;
    function GetLimitOffset(aEnforceLimit: Int64; out aLimit, aOffset: Int64): boolean;
    // Create error response in output
    Procedure CreateErrorResponse;
    Property Operation : TRestOperation Read FOperation;
    // Not owned by TRestIO
    Property Request : TRequest read FRequest;
    Property Response : TResponse read FResponse;
    Property Connection : TSQLConnection Read FConn Write FConn;
    Property Transaction : TSQLTransaction Read FTrans Write FTrans;
    Property Resource : TSQLDBRestResource Read FResource;
    Property RestStrings : TRestStringsConfig Read FRestStrings;
    // owned by TRestIO
    Property RESTInput : TRestInputStreamer read FInput;
    Property RESTOutput : TRestOutputStreamer read FOutput;
    Property RequestContentStream : TStream Read FContentStream;
    // For informative purposes
    Property ResourceName : UTF8String Read FResourceName;
    Property Schema : UTF8String Read FSchema;
    Property ConnectionName : UTF8String Read FCOnnection;
    Property UserID : String Read FUserID Write FUserID;
  end;
  TRestIOClass = Class of TRestIO;


  { TStreamerDef }

  TStreamerDef = Class (TCollectionItem)
  private
    FClass: TRestStreamerClass;
    FName: String;
  Public
    Property MyClass : TRestStreamerClass Read FClass Write FClass;
    Property MyName : String Read FName Write Fname;
  end;

  { TStreamerDefList }

  TStreamerDefList = Class(TCollection)
  private
    function GetD(aIndex : integer): TStreamerDef;
  Public
    Function IndexOfStreamer(const aName : string) : Integer;
    Function IndexOfStreamerContentType(const aContentType : string) : Integer;
    Property Defs[aIndex : integer] : TStreamerDef Read GetD; default;
  end;

  { TStreamerFactory }
  TRestStreamerType = (rstInput,rstOutput);

  TStreamerFactory = Class (TObject)
  Private
    class var FGlobal : TStreamerFactory;
  Private
    FDefs : Array[TRestStreamerType] of TStreamerDefList;
  Protected
    Function FindDefByName(aType : TRestStreamerType; aName : String) : TStreamerDef;
    Function FindDefByContentType(aType : TRestStreamerType; aContentType : String) : TStreamerDef;
    Function IndexOfStreamer(aType : TRestStreamerType; const aName : string) : Integer;
    Function IndexOfStreamerContentType(aType : TRestStreamerType; const aContentType : string) : Integer;
    Procedure RegisterStreamer(aType : TRestStreamerType; Const aName : String; aClass : TRestStreamerClass);
    Procedure UnRegisterStreamer(aType : TRestStreamerType; Const aName : String);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Function Instance : TStreamerFactory;
    Class Procedure GetStreamerList(aList : TStrings; atype : TRestStreamerType);
    Procedure GetStreamerDefNames(aList : TStrings; atype : TRestStreamerType);
    Function FindStreamerByName(aType : TRestStreamerType; const aName : string) : TStreamerDef;
    Function FindStreamerByContentType(aType : TRestStreamerType; const aContentType : string) : TStreamerDef;
  end;

implementation

uses base64, dateutils, sqldbrestconst;

Const

  DefaultPropertyNames :  Array[TRestStringProperty] of UTF8String = (
    ISODateFormat,     { rpDateFormat }
    ISODateTimeFormat, { rpDateTimeFormat }
    ISOTimeFormat,     { rpTimeFormat }
    'data',            { rpDataRoot}
    'metaData',        { rpMetaDataRoot }
    'error',           { rpErrorRoot }
    'name',            { rpFieldNameProp }
    'type',            { rpFieldTypeProp }
    'format',          { rpFieldDateFormatProp }
    'maxLen',          { rpFieldMaxLenProp }
    'humanreadable',   { rpHumanReadable }
    'fl',              { rpFieldList }
    'xl',              { rpExcludeFieldList }
    'Connection',      { rpConnection }
    'Resource',        { rpResource }
    'metadata',        { rpIncludeMetadata }
    'sparse',          { rpSparse }
    'row',             { rpRowName }
    'fields',          { rpMetaDataFields }
    'field',           { rpMetaDataField }
    'code',            { rpErrorCode }
    'message',         { rpErrorMessage }
    '',                { rpFilterEqual }
    '_lt',             { rpFilterLessThan }
    '_gt',             { rpFilterGreaterThan }
    '_lte',            { rpFilterLessThanEqual }
    '_gte',            { rpFilterGreaterThanEqual }
    '_null',           { rpFilterIsNull }
    'limit',           { rpLimit }
    'offset',          { rpOffset }
    'sort',            { rpOrderBy }
    'metadata',        { rpMetadataResourceName }
    'fmtin',           { rpInputFormat }
    'fmt',             { rpOutputFormat }
    'customview',      { rpCustomViewResourceName }
    'sql',             { rpCustomViewSQLParam }
    'datapacket'       { rpXMLDocumentRoot}
  );

{ TStreamerDefList }

function TStreamerDefList.GetD(aIndex : integer): TStreamerDef;
begin
  Result:=TStreamerDef(Items[aIndex])
end;

function TStreamerDefList.IndexOfStreamer(const aName: string): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetD(Result).MyName,aName) do
    Dec(Result);
end;

function TStreamerDefList.IndexOfStreamerContentType(const aContentType: string): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetD(Result).MyClass.GetContentType, aContentType) do
    Dec(Result);
end;

{ TStreamerFactory }

function TStreamerFactory.FindDefByName(aType : TRestStreamerType; aName: String): TStreamerDef;

Var
  Idx : integer;

begin
  Idx:=FDefs[aType].IndexOfStreamer(aName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=FDefs[aType][Idx];
end;

function TStreamerFactory.FindDefByContentType(aType : TRestStreamerType;  aContentType: String): TStreamerDef;
Var
  Idx : integer;

begin
  Idx:=FDefs[aType].IndexOfStreamerContentType(aContentType);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=FDefs[aType][Idx];
end;

procedure TStreamerFactory.RegisterStreamer(aType : TRestStreamerType;  const aName: String; aClass: TRestStreamerClass);

Var
  D : TStreamerDef;

begin
  D:=FindDefByName(atype,aName);
  if D=Nil then
    begin
    D:=FDefs[atype].Add as TStreamerDef;
    D.MyName:=aName;
    end;
  D.MyClass:=aClass;
end;

procedure TStreamerFactory.UnRegisterStreamer(aType : TRestStreamerType;  const aName: String);

begin
  FindDefByName(aType,aName).Free;
end;

constructor TStreamerFactory.Create;

Var
  T : TRestStreamerType;

begin
  for T in TRestStreamerType do
    FDefs[T]:=TStreamerDefList.Create(TStreamerDef);
end;

destructor TStreamerFactory.Destroy;

Var
  T : TRestStreamerType;

begin
  for T in TRestStreamerType do
    FreeAndNil(FDefs[T]);
  inherited Destroy;
end;


class function TStreamerFactory.Instance: TStreamerFactory;
begin
  if FGlobal=Nil then
    FGlobal:=TStreamerFactory.Create;
  Result:=FGlobal;
end;

class procedure TStreamerFactory.GetStreamerList(aList: TStrings;
  atype: TRestStreamerType);
begin
  TStreamerFactory.Instance.GetStreamerDefNames(aList,aType);
end;

procedure TStreamerFactory.GetStreamerDefNames(aList: TStrings; atype: TRestStreamerType);

var
  I : Integer;
begin
  aList.Clear;
  For I:=0 to FDefs[aType].Count-1 do
    aList.Add(FDefs[aType][I].MyName);
end;

function TStreamerFactory.IndexOfStreamer(aType : TRestStreamerType; const aName: string): Integer;
begin
  Result:=FDefs[aType].IndexOfStreamer(aName);
end;


function TStreamerFactory.IndexOfStreamerContentType(aType : TRestStreamerType; const aContentType: string): Integer;
begin
  Result:=FDefs[aType].IndexOfStreamerContentType(aContentType);
end;


function TStreamerFactory.FindStreamerByName(aType : TRestStreamerType; const aName: string): TStreamerDef;

begin
  Result:=FindDefByName(aType,aName);
end;

function TStreamerFactory.FindStreamerByContentType(aType : TRestStreamerType; const aContentType: string): TStreamerDef;
begin
  Result:=FindDefByContentType(aType,aContentType);
end;



{ TRestStringsConfig }

function TRestStringsConfig.GetRestPropName(AIndex: Integer): UTF8String;
begin
  Result:=FValues[TRestStringProperty(AIndex)];
  if (Result='') then
    Result:=DefaultPropertyNames[TRestStringProperty(AIndex)]
end;

procedure TRestStringsConfig.SetRestPropName(AIndex: Integer; AValue: UTF8String);
begin
  FValues[TRestStringProperty(AIndex)]:=aValue;
end;

class function TRestStringsConfig.GetDefaultString(aString: TRestStringProperty): UTF8String;
begin
  Result:=DefaultPropertyNames[aString]
end;

function TRestStringsConfig.GetRestString(aString: TRestStringProperty): UTF8String;
begin
  Result:=FValues[aString];
  if (Result='') then
    Result:=GetDefaultString(aString);
end;

procedure TRestStringsConfig.SetRestString(aString: TRestStringProperty; AValue: UTF8String);
begin
  FValues[AString]:=aValue;
end;

procedure TRestStringsConfig.Assign(aSource: TPersistent);
Var
  R : TRestStringsConfig;
  S : TRestStringProperty;

begin
  if (aSource is TRestStringsConfig) then
    begin
    R:=aSource as TRestStringsConfig;
    For S in TRestStringProperty do
      FValues[S]:=R.FValues[S];
    end;
  inherited Assign(aSource);
end;

{ TRestOutputStreamer }

procedure TRestOutputStreamer.SetOutputOptions(AValue: TRestOutputOptions);
begin
  if FOutputOptions=AValue then Exit;
  FOutputOptions:=AValue;
end;

procedure TRestOutputStreamer.CreateErrorContent(aCode: Integer;
  const Fmt: String; const Args: array of const);

Var
  S : String;

begin
  Try
    S:=Format(Fmt,Args);
  except
    On E : Exception do
      begin
      S:=Format('Error formatting string "%s" with %d arguments. Original code: %d',[Fmt,Length(Args),aCode]);
      aCode:=500;
      end;
  end;
  CreateErrorContent(aCode,S);
end;

function TRestOutputStreamer.HasOption(aOption: TRestOutputOption): Boolean;
begin
  Result:=aOption in OutputOptions;
end;


Function TRestOutputStreamer.FieldToBase64(F : TField) : UTF8String;

var
  BF : TBlobField absolute F;
  Src : TStream;
  Dest : TStringStream;
  E : TBase64EncodingStream;

begin
  Src:=Nil;
  Dest:=nil;
  E:=Nil;
  Try
    if f is TBlobField then
      begin
      Src:=TMemoryStream.Create;
      Src.Size:=BF.DataSize;
      BF.SaveToStream(Src);
      end
    else
      Src:=TStringStream.Create(F.AsString);
    Src.Position:=0;
    Dest:=TStringStream.Create(''{,CP_UTF8});
    E:=TBase64EncodingStream.Create(Dest);
    E.CopyFrom(Src,0);
    FreeAndNil(E); // Will flush
    Result:=Dest.DataString;
  Finally
    Src.Free;
    Dest.Free;
  end;
end;


{ TRestStreamer }

constructor TRestStreamer.Create(aStream: TStream; aStrings: TRestStringsConfig; aOnGetVar: TRestGetVariableEvent);
begin
  FStream:=aStream;
  FOnGetVar:=aOnGetVar;
  FStrings:=aStrings;
end;

function TRestStreamer.GetString(aString: TRestStringProperty): UTF8String;
begin
  If Assigned(FStrings) then
    Result:=FStrings.GetRestString(aString)
  else
    Result:=DefaultPropertyNames[aString];
end;


function TRestStreamer.GetVariable(const aName: UTF8String): UTF8String;
begin
  Result:='';
  if Assigned(FOnGetVar) then
     FOnGetVar(Self,aName,Result);
end;

Class function TRestStreamer.GetContentType: String;
begin
  Result:='text/html';
end;

class procedure TRestInputStreamer.RegisterStreamer(const aName: String);
begin
  TStreamerFactory.Instance.RegisterStreamer(rstInput,aName,Self)
end;

class procedure TRestInputStreamer.UnRegisterStreamer(const aName: String);
begin
  TStreamerFactory.Instance.UnRegisterStreamer(rstInput,aName);
end;

class procedure TRestOutputStreamer.RegisterStreamer(const aName: String);
begin
  TStreamerFactory.Instance.RegisterStreamer(rstOutput,aName,Self)
end;

class procedure TRestOutPutStreamer.UnRegisterStreamer(const aName: String);
begin
  TStreamerFactory.Instance.UnRegisterStreamer(rstOutput,aName)
end;

function TRestOutputStreamer.RequireMetadata: Boolean;
begin
  Result:=False;
end;

function TRestOutputStreamer.FieldToString(aFieldType : TRestFieldType; F: TField): UTF8string;
begin
  Case aFieldType of
    rftInteger : Result:=F.AsString;
    rftLargeInt : Result:=F.AsString;
    rftFloat : Result:=F.AsString;
    rftDate : Result:=FormatDateTime(GetString(rpDateFormat),DateOf(F.AsDateTime));
    rftTime : Result:=FormatDateTime(GetString(rpTimeFormat),TimeOf(F.AsDateTime));
    rftDateTime : Result:=FormatDateTime(GetString(rpDateTimeFormat),F.AsDateTime);
    rftString : Result:=F.AsString;
    rftBoolean : Result:=BoolToStr(F.AsBoolean,'true','false');
    rftBlob : Result:=FieldToBase64(F);
  end;
end;

{ TRestIO }

procedure TRestIO.SetIO(aInput: TRestInputStreamer; aOutput: TRestOutputStreamer);
begin
  Finput:=aInput;
  Finput.FOnGetVar:=@DoGetVariable;
  Foutput:=aOutput;
  FOutput.FOnGetVar:=@DoGetVariable;
end;

procedure TRestIO.SetConn(aConn: TSQLConnection; ATrans: TSQLTransaction);
begin
  FConn:=aConn;
  FTrans:=aTrans;
end;

procedure TRestIO.SetResource(aResource: TSQLDBRestResource);
begin
  Fresource:=AResource;
end;

procedure TRestIO.SetOperation(aOperation: TRestOperation);
begin
  FOperation:=aOperation;
end;

procedure TRestIO.SetRestStrings(aValue: TRestStringsConfig);
begin
  FRestStrings:=aValue;
end;

procedure TRestIO.DoGetVariable(Sender: TObject; const aName: UTF8String; out
  aVal: UTF8String);
begin
  GetVariable(aName,aVal);
end;

constructor TRestIO.Create(aRequest: TRequest; aResponse: TResponse);
begin
  FRequest:=aRequest;
  FResponse:=aResponse;
  FContentStream:=TStringStream.Create(aRequest.Content);
end;

destructor TRestIO.Destroy;
begin
  if Assigned(FInput) then
    Finput.FOnGetVar:=Nil;
  if Assigned(Foutput) then
  FOutput.FOnGetVar:=Nil;
  FreeAndNil(FContentStream) ;
  FreeAndNil(Finput);
  FreeAndNil(Foutput);
  inherited Destroy;
end;

function TRestIO.GetVariable(const aName: UTF8String; out aVal: UTF8String;
  AllowedSources: TVAriableSources): TVariableSource;

  Function FindInList(aSource : TVariableSource;L : TStrings) : Boolean;

  Var
    I : Integer;
    N,V : String;
  begin
    Result:=(aSource in AllowedSources);
    if Result then
      begin
      I:=L.IndexOfName(aName);
      Result:=I<>-1;
      if Result then
        begin
        L.GetNameValue(I,N,V);
        aVal:=V;
        GetVariable:=aSource;
        end;
      end;
  end;

begin
  Result:=vsNone;
  With Request do
    if not FIndInList(vsQuery,QueryFields) then
      if not FindInList(vsContent,ContentFields) then
        begin
        aVal:=RouteParams[aName];
        if (aVal<>'') then
          result:=vsRoute
        else
          FindInList(vsHeader,CustomHeaders);
        end;
end;

function TRestIO.GetFilterVariable(const aName: UTF8String; AFilter: TRestFieldFilter;out aValue: UTF8String) : TVariableSource;

Const
  FilterStrings : Array[TRestFieldFilter] of TRestStringProperty =
   (rpFilterEqual,rpFilterLessThan,rpFilterGreaterThan,rpFilterLessThanEqual,rpFilterGreaterThanEqual,rpFilterIsNull);

begin
  aValue:='';
  Result:=GetVariable(aName+FRestStrings.GetRestString(FilterStrings[aFilter]),aValue,[vsQuery]);
end;

Class function TRestIO.StrToNullBoolean(S: String; Strict: Boolean): TNullBoolean;

begin
  result:=nbNone;
  s:=lowercase(s);
  if (s<>'') then
    if (s='1') or (s='t') or (s='true') or (s='y') then
      Result:=nbTrue
    else
      if (s='0') or (s='f') or (s='false') or (s='n') then
        Result:=nbFalse
      else if not Strict then
        Result:=nbNone
      else
        Raise EConvertError.CreateFmt('Not a correct boolean value: "%s"',[S])
end;

function TRestIO.GetBooleanVar(const aName: UTF8String; aStrict : Boolean = False): TNullBoolean;

Var
  S : UTF8String;

begin
  result:=nbNone;
  if GetVariable(aName,S)=vsNone then
    Result:=nbNone
  else
    Result:=StrToNullBoolean(S,aStrict);
end;

Function TRestIO.GetRequestOutputOptions(aDefault : TRestOutputOptions) : TRestOutputOptions;

  Procedure CheckParam(aName : String; aOption: TRestOutputOption);
  begin
    Case GetBooleanVar(aName) of
     nbFalse : Exclude(Result,aOption);
     nbTrue : Include(Result,aOption);
    else
     // nbNull: keep default
    end
  end;

begin
  Result:=aDefault;
  CheckParam(FRestStrings.GetRestString(rpHumanReadable),ooHumanReadable);
  CheckParam(FRestStrings.GetRestString(rpSparse),ooSparse);
  CheckParam(FRestStrings.GetRestString(rpIncludeMetadata),ooMetadata);
end;

function TRestIO.GetLimitOffset(aEnforceLimit : Int64; out aLimit, aOffset: Int64): boolean;

Var
  P,S : UTF8String;

begin
  aLimit:=0;
  aOffset:=0;
  P:=RestStrings.GetRestString(rpLimit);
  Result:=GetVariable(P,S,[vsQuery])<>vsNone;
  if Not Result then
    Exit;
  if (S<>'') and not TryStrToInt64(S,aLimit) then
    Raise ESQLDBRest.CreateFmt(400,SErrInvalidParam,[P]);
  P:=RestStrings.GetRestString(rpOffset);
  if GetVariable(P,S,[vsQuery])<>vsNone then
    if (S<>'') and not TryStrToInt64(S,aOffset) then
      Raise ESQLDBRest.CreateFmt(400,SErrInvalidParam,[P]);
  if (aEnforceLimit>0) and (aLimit>aEnforceLimit) then
    aLimit:=aEnforceLimit;
end;

procedure TRestIO.CreateErrorResponse;
begin
  RestOutput.CreateErrorContent(Response.Code,Response.CodeText);
end;

finalization
  FreeAndNil(TStreamerFactory.Fglobal);
end.

