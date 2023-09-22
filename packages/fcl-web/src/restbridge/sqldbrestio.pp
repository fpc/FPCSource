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
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestio;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpJson.Data, Data.BufDataset, Data.Sqldb, Data.Db, FpWeb.Http.Defs, FpWeb.RestBridge.Schema;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpjson, bufdataset, sqldb, db, httpdefs, sqldbrestschema;
{$ENDIF FPC_DOTTEDUNITS}


Type
  TRestOutputOption = (ooMetadata,ooSparse,ooHumanReadable);
  TRestOutputOptions = Set of TRestOutputOption;

  TNullBoolean = (nbNone,nbFalse,nbTrue);
  TNullBooleans = set of TNullBoolean;

Const
  AllVariableSources = [Low(TVariableSource)..High(TVariableSource)];
  allOutputOptions = [Low(TRestOutputOption)..High(TRestOutputOption)];


Type
  TRestIO = Class;

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
                         rpXMLDocumentRoot,
                         rpConnectionResourceName,
                         rpParametersResourceName,
                         rpParametersRoutePart,
                         rpAttachment
                         );
  TRestStringProperties = Set of TRestStringProperty;

  TRestGetVariableEvent = Procedure (Sender : TObject; Const aName : UTF8String; Out aVal : UTF8String) of object;

  { TRestStringsConfig }

  TRestStringsConfig = Class(TPersistent)
  private
    FValues : Array[TRestStringProperty] of UTF8String;
    function GetRestPropName(AIndex: Integer): UTF8String;
    function IsRestStringStored(AIndex: Integer): Boolean;
    procedure SetRestPropName(AIndex: Integer; AValue: UTF8String);
  Public
    Class Function GetDefaultString(aString : TRestStringProperty) :UTF8String;
    Function GetRestString(aString : TRestStringProperty) :UTF8String;
    Procedure SetRestString(aString : TRestStringProperty; AValue :UTF8String);
    Procedure Assign(aSource : TPersistent); override;
  Published
    // Indexes here MUST match TRestProperty
    Property RESTDateFormat : UTF8String Index ord(rpDateFormat) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property RESTDateTimeFormat : UTF8String Index ord(rpDateTimeFormat)  Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property RESTTimeFormat : UTF8String Index ord(rpTimeFormat)  Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property DataProperty : UTF8String Index ord(rpDataRoot) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MetaDataRoot : UTF8String Index ord(rpMetaDataRoot) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ErrorProperty : UTF8String Index ord(rpErrorRoot) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FieldNameProperty : UTF8String Index ord(rpFieldNameProp) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FieldTypeProperty : UTF8String Index ord(rpFieldTypeProp) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property DateFormatProperty : UTF8String Index ord(rpFieldDateFormatProp) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MaxLenProperty : UTF8String Index ord(rpFieldMaxLenProp) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property HumanReadableParam : UTF8String Index ord(rpHumanReadable) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FieldListParam : UTF8String Index ord(rpFieldList) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ExcludeFieldListParam : UTF8String Index ord(rpExcludeFieldList) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ConnectionParam : UTF8String Index Ord(rpConnection) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ResourceParam : UTF8String Index ord(rpResource) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property IncludeMetadataParam : UTF8String Index ord(rpIncludeMetadata) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property SparseParam : UTF8String Index Ord(rpSparse) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property RowName : UTF8String Index Ord(rpRowName) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MetadataFields : UTF8String Index Ord(rpMetadataFields) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MetadataField : UTF8String Index Ord(rpMetadataField) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ErrorCode : UTF8String Index ord(rpErrorCode) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ErrorMessage : UTF8String Index ord(rpErrorMessage) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FilterParamEqual : UTF8String Index ord(rpFilterEqual) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FilterParamLessThan : UTF8String Index ord(rpFilterLessThan) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FilterParamGreaterThan : UTF8String Index ord(rpFilterGreaterThan) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FilterParamLessThanEqual : UTF8String Index ord(rpFilterLessThanEqual) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FilterParamGreaterThanEqual : UTF8String Index ord(rpFilterGreaterThanEqual) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property FilterParamIsNull : UTF8String Index ord(rpFilterIsNull) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property LimitParam : UTF8string Index ord(rpLimit) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property OffsetParam : UTF8string Index ord(rpOffset) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property SortParam : UTF8string Index ord(rpOrderBy) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MetadataResourceName : UTF8string Index ord(rpMetadataResourceName) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MetadataParametersName : UTF8string Index ord(rpParametersResourceName) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property MetadataParametersRoutePart : UTF8string Index ord(rpParametersRoutePart) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property InputFormatParam : UTF8string Index ord(rpInputFormat) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property OutputFormatParam : UTF8string Index ord(rpOutputFormat) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property CustomViewResourceName : UTF8string Index ord(rpCustomViewResourceName) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property CustomViewSQLParam : UTF8string Index ord(rpCustomViewSQLParam) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property XMLDocumentRoot : UTF8string Index ord(rpXMLDocumentRoot) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property ConnectionResourceName : UTF8string Index ord(rpConnectionResourceName) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
    Property AttachmentParam : UTF8String Index ord(rpAttachment) Read GetRestPropName Write SetRestPropName Stored IsRestStringStored;
  end;

  TRestStatus = (rsError,                   // Internal logic/unexpected error (500)
                 rsGetOK,                   // GET command completed OK (200)
                 rsPostOK,                  // POST command completed OK (204)
                 rsPutOK,                   // PUT command completed OK (200)
                 rsDeleteOK,                // DELETE command completed OK (204)
                 rsInvalidParam,            // Something wrong/missing in Query parameters (400)
                 rsCORSOK,                  // CORS request completed OK (200)
                 rsCORSNotAllowed,          // CORS request not allowed (403)
                 rsUnauthorized,            // Authentication failed (401)
                 rsResourceNotAllowed,      // Resource request not allowed (403)
                 rsRestOperationNotAllowed, // Resource operation (method) not allowed (405)
                 rsInvalidMethod,           // Invalid HTTP method (400)
                 rsUnknownResource,         // Unknown resource (404)
                 rsNoResourceSpecified,     // Unable to determine resource (404)
                 rsNoConnectionSpecified,   // Unable to determine connection for (400)
                 rsRecordNotFound,          // Query did not return record for single resource (404)
                 rsInvalidContent,          // Invalid content for POST/PUT operation (400)
                 rsPatchOK                  // PATCH command completed OK (200)

                 );
  TRestStatuses = set of TRestStatus;

  { TRestStatusConfig }

  TRestStatusConfig = Class(TPersistent)
  private
    FStatus : Array[TRestStatus] of Word;
    function GetStatus(AIndex: Integer): Word;
    function IsStatusStored(AIndex: Integer): Boolean;
    procedure SetStatus(AIndex: Integer; AValue: Word);
  Public
    Procedure Assign(aSource : TPersistent); override;
    function GetStatusCode(aStatus : TRestStatus): Word;
  Published
    // Internal logic/unexpected error (500)
    Property Error : Word Index Ord(rsError) Read GetStatus Write SetStatus Stored IsStatusStored;
    // GET command completed OK (200)
    Property GetOK : Word Index Ord(rsGetOK) Read GetStatus Write SetStatus Stored IsStatusStored;
    // POST command completed OK (204)
    Property PostOK : Word Index Ord(rsPostOK) Read GetStatus Write SetStatus Stored IsStatusStored;
    // PUT command completed OK (200)
    Property PutOK : Word Index Ord(rsPutOK) Read GetStatus Write SetStatus Stored IsStatusStored;
    // DELETE command completed OK (204)
    Property DeleteOK : Word Index Ord(rsDeleteOK) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Something wrong/missing in Query parameters (400)
    Property InvalidParam : Word Index Ord(rsInvalidParam) Read GetStatus Write SetStatus Stored IsStatusStored;
    // CORS request completed OK (200)
    Property CORSOK : Word Index Ord(rsCORSOK) Read GetStatus Write SetStatus Stored IsStatusStored;
    // CORS request not allowed (403)
    Property CORSNotAllowed : Word Index Ord(rsCORSNotAllowed) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Authentication failed (401)
    Property Unauthorized : Word Index Ord(rsUnauthorized) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Resource request not allowed (403)
    Property ResourceNotAllowed : Word Index Ord(rsResourceNotAllowed) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Resource operation (method) not allowed (405)
    Property RestOperationNotAllowed : Word Index Ord(rsRestOperationNotAllowed) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Invalid HTTP method (400)
    Property InvalidMethod : Word Index Ord(rsInvalidMethod) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Unknown resource (404)
    Property UnknownResource : Word Index Ord(rsUnknownResource) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Unable to determine resource (404)
    Property NoResourceSpecified : Word Index Ord(rsNoResourceSpecified) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Unable to determine connection for (400)
    Property NoConnectionSpecified : Word Index Ord(rsNoConnectionSpecified) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Query did not return record for single resource (404)
    Property RecordNotFound : Word Index Ord(rsRecordNotFound) Read GetStatus Write SetStatus Stored IsStatusStored;
    // Invalid content for POST/PUT operation (400)
    Property InvalidContent : Word Index Ord(rsInvalidContent) Read GetStatus Write SetStatus Stored IsStatusStored;
  end;

  { TRestStreamer }

  TRestStreamer = Class(TObject)
  private
    FStream: TStream;
    FOnGetVar : TRestGetVariableEvent;
    FStrings: TRestStringsConfig;
    FStatuses : TRestStatusConfig;
  Public
    // Registry
    Class Function GetContentType : String; virtual;
    Constructor Create(aStream : TStream;aStrings : TRestStringsConfig;aStatus : TRestStatusConfig; aOnGetVar : TRestGetVariableEvent);
    Function GetString(aString : TRestStringProperty) : UTF8String;
    Property Strings : TRestStringsConfig Read FStrings;
    Property Statuses : TRestStatusConfig Read FStatuses;
    procedure InitStreaming; virtual; abstract;
    Function GetVariable(const aName : UTF8String) : UTF8String;
    Property Stream : TStream Read FStream;
  end;
  TRestStreamerClass = Class of TRestStreamer;

  { TRestInputStreamer }

  TRestInputStreamer = Class(TRestStreamer)
  Public
    // Select input object aIndex. Must return False if no such object in input
    // Currently aIndex=0, but for batch operations this may later become nonzero.
    Function SelectObject(aIndex : Integer) : Boolean; virtual; abstract;
    // Return Nil if none found. If result is non-nil, caller will free.
    Function GetContentField(aName : UTF8string) : TJSONData; virtual; abstract;
    Function HaveInputData(aName : UTF8string) : Boolean; virtual;

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
    Class Function FileExtension : String; virtual;
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

  { TRestContext }

  TRestContext = Class(TBaseRestContext)
  Private
    FIO : TRestIO;
  Protected
    function GetConnection: TSQLConnection; override;
    function GetTransaction: TSQLTransaction; override;
    Function DoGetInputData(const aName : UTF8string) : TJSONData; override;
    Procedure DoSetInputData(aName : UTF8string; aValue : TJSONData); override;
    Function GetUpdateData : TDataset; override;
    property IO : TRestIO Read FIO;
  Public
    Function GetVariable(Const aName : UTF8String; aSources : TVariableSources; Out aValue : UTF8String) : Boolean; override;
  end;

  { TRestIO }
  TSQLLogNotifyEvent = Procedure (Sender : TObject; EventType : TDBEventType; Const Msg : String) of object;

  TRestIO = Class
  private
    FConn: TSQLConnection;
    FCOnnection: UTF8String;
    FInput: TRestInputStreamer;
    FOnSQLLog: TSQLLogNotifyEvent;
    FOperation: TRestOperation;
    FOutput: TRestOutputStreamer;
    FRequest: TRequest;
    FResource: TSQLDBRestResource;
    FResponse: TResponse;
    FRestContext: TRestContext;
    FRestStatuses: TRestStatusConfig;
    FRestStrings: TRestStringsConfig;
    FSchema: UTF8String;
    FTrans: TSQLTransaction;
    FContentStream : TStream;
    FUpdatedData: TBufDataset;
    FCustomInputData : TJSONObject;
    function GetResourceName: UTF8String;
    function GetUserID: String;
    procedure SetUserID(const AValue: String);
  Protected
  Public
    Constructor Create(aRequest : TRequest; aResponse : TResponse); virtual;
    Destructor Destroy; override;
    // Log callback for SQL. Rerouted here, because we need IO
    procedure DoSQLLog(Sender: TSQLConnection;  EventType: TDBEventType; const Msg: String);
    // Set things.
    Procedure SetIO(aInput : TRestInputStreamer;aOutput : TRestOutputStreamer);
    Procedure SetConn(aConn : TSQLConnection; ATrans : TSQLTransaction);
    Procedure SetResource(aResource : TSQLDBRestResource);
    procedure SetOperation(aOperation : TRestOperation);
    Procedure SetRestStrings(aValue : TRestStringsConfig);
    Procedure SetRestStatuses(aValue : TRestStatusConfig);
    Procedure SetCustomInputData(Const aName : UTF8String; aValue : TJSONData);
    // Get things
    class function StrToNullBoolean(const S: String; Strict: Boolean): TNullBoolean;
    Procedure DoGetVariable(Sender : TObject; Const aName : UTF8String; Out aVal : UTF8String);
    function GetCustomInputData(const aName : UTF8String) : TJSONData;
    // You must free the result of this function !
    Function GetContentField(const aName : UTF8string) : TJSONData; virtual;
    Function GetVariable (Const aName : UTF8String; Out aVal : UTF8String; AllowedSources : TVAriableSources = AllVariableSources) : TVariableSource; virtual;
    function GetFilterVariable(const aName: UTF8String; AFilter: TRestFieldFilter; out aValue: UTF8String): TVariableSource;
    Function GetBooleanVar(Const aName : UTF8String; aStrict : Boolean = False) : TNullBoolean;
    function GetRequestOutputOptions(aDefault: TRestOutputOptions): TRestOutputOptions;
    function GetLimitOffset(aEnforceLimit: Int64; out aLimit, aOffset: Int64): boolean;
    // Create error response in output
    function CreateRestContext: TRestContext; virtual;
    Procedure CreateErrorResponse;
    Property Operation : TRestOperation Read FOperation;
    // Not owned by TRestIO
    Property Request : TRequest read FRequest;
    Property Response : TResponse read FResponse;
    Property Connection : TSQLConnection Read FConn Write FConn;
    Property Transaction : TSQLTransaction Read FTrans Write FTrans;
    Property Resource : TSQLDBRestResource Read FResource;
    Property RestStrings : TRestStringsConfig Read FRestStrings;
    Property RestStatuses : TRestStatusConfig Read FRestStatuses;
    // owned by TRestIO
    Property UpdatedData : TBufDataset Read FUpdatedData;
    Property RESTInput : TRestInputStreamer read FInput;
    Property RESTOutput : TRestOutputStreamer read FOutput;
    Property RequestContentStream : TStream Read FContentStream;
    Property RestContext : TRestContext Read FRestContext;
    // For informative purposes
    Property ResourceName : UTF8String Read GetResourceName;
    Property Schema : UTF8String Read FSchema;
    Property ConnectionName : UTF8String Read FCOnnection;
    Property UserID : String Read GetUserID Write SetUserID;
    // For logging
    Property OnSQLLog :TSQLLogNotifyEvent Read FOnSQLLog Write FOnSQLLog;

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
    Function FindDefByName(aType : TRestStreamerType; const aName : String) : TStreamerDef;
    Function FindDefByContentType(aType : TRestStreamerType; const aContentType : String) : TStreamerDef;
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

  { TRestBufDataset }

  TRestBufDataset = class (TBufDataset)
  protected
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;
  end;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Base64, System.DateUtils, FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses base64, dateutils, sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}

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
    'datapacket',      { rpXMLDocumentRoot}
    '_connection',     { rpConnectionResourceName }
    '_parameters',     { rpParametersResourceName }
    'parameters',      { rpParametersRoutePart }
    'att'              { rpAttachment }
  );
  DefaultStatuses : Array[TRestStatus] of Word = (
    500, { rsError }
    200, { rsGetOK }
    201, { rsPostOK }
    200, { rsPutOK }
    204, { rsDeleteOK }
    400, { rsInvalidParam }
    200, { rsCORSOK}
    403, { rsCORSNotallowed}
    401, { rsUnauthorized }
    403, { rsResourceNotAllowed }
    405, { rsRestOperationNotAllowed }
    400, { rsInvalidMethod }
    404, { rsUnknownResource }
    404, { rsNoResourceSpecified }
    400, { rsNoConnectionSpecified }
    404, { rsRecordNotFound }
    400, { rsInvalidContent }
    200  { rsPatchOK }
  );

{ TRestBufDataset }

procedure TRestBufDataset.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
  If (FieldDef=Nil) or (aBlobBuf=Nil) then
    exit;
end;

{ TRestStatusConfig }

function TRestStatusConfig.GetStatus(AIndex: Integer): Word;
begin
  Result:=GetStatusCode(TRestStatus(aIndex));
end;

function TRestStatusConfig.IsStatusStored(AIndex: Integer): Boolean;

Var
  W : Word;

begin
  W:=FStatus[TRestStatus(aIndex)];
  Result:=(W<>0) and (W<>DefaultStatuses[TRestStatus(aIndex)]);
end;

procedure TRestStatusConfig.SetStatus(AIndex: Integer; AValue: Word);
begin
  if (aValue<>DefaultStatuses[TRestStatus(aIndex)]) then
    aValue:=0;
  FStatus[TRestStatus(aIndex)]:=aValue;
end;

procedure TRestStatusConfig.Assign(aSource: TPersistent);

Var
  C : TRestStatusConfig;
  S : TRestStatus;

begin
  if aSource is TRestStatusConfig then
    begin
    C:=aSource as TRestStatusConfig;
    for S in TRestStatus do
      FStatus[S]:=C.FStatus[S];
    end
  else
    inherited Assign(aSource);
end;

function TRestStatusConfig.GetStatusCode(aStatus: TRestStatus): Word;
begin
  Result:=FStatus[aStatus];
  if Result=0 then
    Result:=DefaultStatuses[aStatus];
end;

{ TRestContext }

function TRestContext.GetVariable(const aName: UTF8String; aSources : TVariableSources; out aValue: UTF8String): Boolean;

Var
  D : TJSONData;

begin
  Result:=FIO.GetVariable(aName,aValue,aSources)<>vsNone;
  if Not Result and (vsData in aSources) then
    begin
    // Will be freed.
    D:=GetInputData(aName);
    Result:=Assigned(D);
    if Result then
      if D.JSONType in StructuredJSONTypes then
        aValue:=D.AsJSON
      else
        aValue:=D.AsString;
    end;
end;

function TRestContext.GetConnection: TSQLConnection;
begin
  Result:=IO.Connection;
end;

function TRestContext.GetTransaction: TSQLTransaction;
begin
  Result:=IO.Transaction;
end;

function TRestContext.DoGetInputData(const aName: UTF8string): TJSONData;
begin
  Result:=IO.RESTInput.GetContentField(aName);
end;

procedure TRestContext.DoSetInputData(aName: UTF8string; aValue: TJSONData);
begin
  IO.SetCustomInputData(aName,aValue);
end;

function TRestContext.GetUpdateData: TDataset;
begin
  Result:=IO.UpdatedData;
end;


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

function TStreamerFactory.FindDefByName(aType : TRestStreamerType; const aName: String): TStreamerDef;

Var
  Idx : integer;

begin
  Idx:=FDefs[aType].IndexOfStreamer(aName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=FDefs[aType][Idx];
end;

function TStreamerFactory.FindDefByContentType(aType : TRestStreamerType; Const aContentType: String): TStreamerDef;
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

function TRestStringsConfig.IsRestStringStored(AIndex: Integer): Boolean;

Var
  V : UTF8String;

begin
  V:=FValues[TRestStringProperty(AIndex)];
  Result:=(V<>'') and (V<>DefaultPropertyNames[TRestStringProperty(AIndex)]);
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
  if RequireMetadata then
    Include(FOutputOptions,ooMetadata);
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
      aCode:=Statuses.GetStatusCode(rsError);
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

constructor TRestStreamer.Create(aStream: TStream; aStrings: TRestStringsConfig; aStatus : TRestStatusConfig; aOnGetVar: TRestGetVariableEvent);
begin
  FStream:=aStream;
  FOnGetVar:=aOnGetVar;
  FStrings:=aStrings;
  FStatuses:=aStatus;
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

function TRestInputStreamer.HaveInputData(aName: UTF8string): Boolean;

Var
  D : TJSONData;

begin
  D:=GetContentField(aName);
  Result:=D<>Nil;
  D.Free;
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

class procedure TRestOutputStreamer.UnRegisterStreamer(const aName: String);
begin
  TStreamerFactory.Instance.UnRegisterStreamer(rstOutput,aName)
end;

class function TRestOutputStreamer.FileExtension: String;
begin
  Result:='';
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
  else
    Result:='';
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

procedure TRestIO.SetRestStatuses(aValue: TRestStatusConfig);
begin
  FRestStatuses:=aValue;
end;

procedure TRestIO.SetCustomInputData(const aName: UTF8String; aValue: TJSONData);
begin
  if FCustomInputData=Nil then
    FCustomInputData:=TJSONObject.Create([aName,aValue])
  else
    FCustomInputData.Elements[aName]:=aValue;
end;

function TRestIO.GetCustomInputData(const aName: UTF8String): TJSONData;

var
  Idx : Integer;

begin
  Result:=Nil;
  if (FCustomInputData<>Nil) then
    begin
    Idx:=FCustomInputData.IndexOfName(aName,True);
    if Idx<>-1 then
      Result:=FCustomInputData.Items[idx];
    end;
end;

procedure TRestIO.DoGetVariable(Sender: TObject; const aName: UTF8String; out
  aVal: UTF8String);
begin
  GetVariable(aName,aVal);
end;

function TRestIO.GetContentField(const aName: UTF8string): TJSONData;

var
  Idx : Integer;

begin
  Idx:=-1;
  if Assigned(FCustomInputData) then
    Idx:=FCustomInputData.IndexOfName(aName);
  if Idx<>-1 then
    Result:=FCustomInputData.Items[Idx].Clone
  else
    Result:=RESTInput.GetContentField(aName);
end;


procedure TRestIO.SetUserID(const AValue: String);
begin
  if (UserID=AValue) then Exit;
  FRestContext.UserID:=AValue;
end;

function TRestIO.GetUserID: String;
begin
  Result:=FRestContext.UserID;
end;

function TRestIO.GetResourceName: UTF8String;
begin
  if Assigned(FResource) then
    Result:=FResource.ResourceName
  else
    Result:='?';
end;

constructor TRestIO.Create(aRequest: TRequest; aResponse: TResponse);
begin
  FRequest:=aRequest;
  FResponse:=aResponse;
  FContentStream:=TStringStream.Create(aRequest.Content);
  FRestContext:=CreateRestContext;
  FRestContext.FIO:=Self;
  FUpdatedData:=TRestBufDataset.Create(Nil);
end;

destructor TRestIO.Destroy;
begin
  FreeAndNil(FCustomInputData);
  FreeAndNil(FUpdatedData);
  FreeAndNil(FRestContext);
  if Assigned(FInput) then
    Finput.FOnGetVar:=Nil;
  if Assigned(Foutput) then
  FOutput.FOnGetVar:=Nil;
  FreeAndNil(FContentStream) ;
  FreeAndNil(Finput);
  FreeAndNil(Foutput);
  inherited Destroy;
end;

procedure TRestIO.DoSQLLog(Sender: TSQLConnection; EventType: TDBEventType;  const Msg: String);

begin
  If Assigned(OnSQLLog) then
    FOnSQLLog(Self,EventType,Msg);
end;

function TRestIO.CreateRestContext : TRestContext;

begin
  Result:=TRestContext.Create;
end;

function TRestIO.GetVariable(const aName: UTF8String; out aVal: UTF8String; AllowedSources: TVariableSources): TVariableSource;

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

class function TRestIO.StrToNullBoolean(const S: String; Strict: Boolean): TNullBoolean;

var
  ls : string;

begin
  result:=nbNone;
  ls:=lowercase(s);
  if (ls<>'') then
    if (ls='1') or (ls='t') or (ls='true') or (ls='y') then
      Result:=nbTrue
    else
      if (ls='0') or (ls='f') or (ls='false') or (ls='n') then
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

function TRestIO.GetRequestOutputOptions(aDefault: TRestOutputOptions
  ): TRestOutputOptions;

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
    Raise ESQLDBRest.CreateFmt(RestStatuses.GetStatusCode(rsInvalidParam),SErrInvalidParam,[P]);
  P:=RestStrings.GetRestString(rpOffset);
  if GetVariable(P,S,[vsQuery])<>vsNone then
    if (S<>'') and not TryStrToInt64(S,aOffset) then
      Raise ESQLDBRest.CreateFmt(RestStatuses.GetStatusCode(rsInvalidParam),SErrInvalidParam,[P]);
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

