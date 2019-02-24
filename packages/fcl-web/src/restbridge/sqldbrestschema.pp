unit sqldbrestschema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, fpjson;

Type

  TRestFieldType = (rftUnknown,rftInteger,rftLargeInt,rftFloat,rftDate,rftTime,rftDateTime,rftString,rftBoolean,rftBlob);
  TRestFieldTypes = set of TRestFieldType;

  TRestFieldOption = (foInKey,foInInsert, foInUpdate,foRequired,foFilter,foOrderBy,foOrderByDesc);
  TRestFieldOptions = Set of TRestFieldOption;

  TRestFieldFilter = (rfEqual,rfLessThan,rfGreaterThan,rfLessThanEqual,rfGreaterThanEqual,rfNull);
  TRestFieldFilters = set of TRestFieldFilter;

  TSQLKind = (skSelect,skInsert,skUpdate,skDelete); // Must follow Index used below.
  TSQLKinds = set of TSQLKind;

  TRestOperation = (roUnknown,roGet,roPost,roPut,roDelete,roOptions,roHead); // add roPatch, roMerge ?
  TRestOperations = Set of TRestOperation;

  TFieldListKind = (flSelect,flInsert,flInsertParams,flUpdate,flWhereKey,flFilter,flOrderby);
  TFieldListKinds = set of TFieldListKind;


Const
  AllRestOperations = [Succ(Low(TRestOperation))..High(TRestOperation)];
  AllFieldFilters = [Low(TRestFieldFilter)..High(TRestFieldFilter)];
  JSONSchemaRoot = 'schema';
  JSONResourcesRoot = 'resources';
  JSONConnectionsRoot = 'connections';

Type

  { ESQLDBRest }

  ESQLDBRest = Class(Exception)
  private
    FResponseCode: Integer;
  Public
    Constructor Create(aCode : Integer; Const aMessage : String);
    Constructor CreateFmt(aCode : Integer; Const Fmt : String; COnst Args: Array of const);
    Property ResponseCode : Integer Read FResponseCode Write FResponseCode;
  end;

  TRestSQLQuery = Class(TSQLQuery)
  Public
    Property TableName;
  end;

  TSQLDBRestSchema = Class;


  { TSQLDBRestField }

  TSQLDBRestField = class(TCollectionItem)
  private
    FFieldName: UTF8String;
    FFieldType: TRestFieldType;
    FFilters: TRestFieldFilters;
    fGeneratorName: String;
    FMaxLen: Integer;
    FNativeFieldType: TFieldType;
    FOptions: TRestFieldOptions;
    FPublicName: UTF8String;
    function GetPublicName: UTF8String;
  Protected
    Function GetDisplayName: string; override;
  Public
    Constructor Create(ACollection: TCollection); override;
    Procedure Assign(Source: TPersistent); override;
    Function UseInFieldList(aListKind : TFieldListKind) : Boolean; virtual;
  Published
    Property FieldName : UTF8String Read FFieldName Write FFieldName;
    Property PublicName : UTF8String Read GetPublicName Write FPublicName;
    Property GeneratorName : String Read fGeneratorName Write FGeneratorName;
    Property FieldType : TRestFieldType Read FFieldType Write FFieldType;
    Property NativeFieldType : TFieldType Read FNativeFieldType Write FNativeFieldType;
    Property Options : TRestFieldOptions Read FOptions Write FOptions;
    Property Filters : TRestFieldFilters Read FFilters Write FFilters default AllFieldFilters;
    Property MaxLen : Integer Read FMaxLen Write FMaxLen;
  end;
  TSQLDBRestFieldClass = Class of TSQLDBRestField;
  TSQLDBRestFieldArray = Array of TSQLDBRestField;

  TRestFieldPair = Record
    DBField : TField;
    RestField :TSQLDBRestField;
  end;
  TRestFieldPairArray = Array of TRestFieldPair;

  TRestFieldOrderPair = Record
    RestField :TSQLDBRestField;
    Desc : Boolean;
  end;
  TRestFieldOrderPairArray = Array of TRestFieldOrderPair;

  { TSQLDBRestFieldList }

  TSQLDBRestFieldList = class(TCollection)
  private
    function GetFields(aIndex : Integer): TSQLDBRestField;
    procedure SetFields(aIndex : Integer; AValue: TSQLDBRestField);
  Public
    Function AddField(Const aFieldName : UTF8String; aFieldType : TRestFieldType; aOptions : TRestFieldOptions) : TSQLDBRestField;
    function indexOfFieldName(const aFieldName: UTF8String): Integer;
    Function FindByFieldName(const aFieldName: UTF8String):TSQLDBRestField;
    function indexOfPublicName(const aPublicName: UTF8String): Integer;
    Function FindByPublicName(const aFieldName: UTF8String):TSQLDBRestField;
    Property Fields[aIndex : Integer] : TSQLDBRestField read GetFields write SetFields; default;
  end;
  TSQLDBRestFieldListClass = Class of TSQLDBRestFieldList;

  { TSQLDBRestResource }
  TSQLDBRestGetDatasetEvent = Procedure (aSender : TObject; aFieldList : TRestFieldPairArray; aOrderBy : TRestFieldOrderPairArray; aLimit, aOffset : Int64; Var aDataset : TDataset) of object;
  TSQLDBRestCheckParamsEvent = Procedure (aSender : TObject; aOperation : TRestOperation; Params : TParams) of object;
  TSQLDBRestAllowRecordEvent = Procedure (aSender : TObject; aDataSet : TDataset; var allowRecord : Boolean) of object;
  TProcessIdentifier = Function (const S: UTF8String): UTF8String of object;

  TSQLDBRestResource = class(TCollectionItem)
  private
    FAllowedOperations: TRestOperations;
    FConnectionName: UTF8String;
    FEnabled: Boolean;
    FFields: TSQLDBRestFieldList;
    FInMetadata: Boolean;
    FOnAllowRecord: TSQLDBRestAllowRecordEvent;
    FOnCheckParams: TSQLDBRestCheckParamsEvent;
    FOnGetDataset: TSQLDBRestGetDatasetEvent;
    FResourceName: UTF8String;
    FTableName: UTF8String;
    FSQL : Array[TSQLKind] of TStrings;
    function GetResourceName: UTF8String;
    function GetSQL(AIndex: Integer): TStrings;
    function GetSQLTyped(aKind : TSQLKind): TStrings;
    procedure SetFields(AValue: TSQLDBRestFieldList);
    procedure SetSQL(AIndex: Integer; AValue: TStrings);
  Protected
    Function GetDisplayName: string; override;
  Public
    Class var
      DefaultFieldListClass : TSQLDBRestFieldListClass;
      DefaultFieldClass: TSQLDBRestFieldClass;
    Class function CreateFieldList : TSQLDBRestFieldList; virtual;
    Class function FieldTypeToRestFieldType(aFieldType: TFieldType): TRestFieldType; virtual;
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure CheckParams(aOperation : TRestoperation; P : TParams);
    Function GetDataset(aFieldList : TRestFieldPairArray; aOrderBy : TRestFieldOrderPairArray; aLimit, aOffset : Int64) : TDataset;
    Function GetSchema : TSQLDBRestSchema;
    function GenerateDefaultSQL(aKind: TSQLKind): UTF8String; virtual;
    Procedure Assign(Source: TPersistent); override;
    Function AllowRecord(aDataset : TDataset) : Boolean;
    Function GetHTTPAllow : String; virtual;
    function GetFieldList(aListKind: TFieldListKind): UTF8String;
    function GetFieldArray(aListKind: TFieldListKind): TSQLDBRestFieldArray;
    Function GetResolvedSQl(aKind : TSQLKind; Const AWhere : UTF8String; Const aOrderBy : UTF8String = ''; aLimit : UTF8String = '') : UTF8String;
    Procedure PopulateFieldsFromFieldDefs(Defs : TFieldDefs; aIndexFields : TStringArray; aProcessIdentifier : TProcessIdentifier; aMinFieldOpts : TRestFieldOptions);
    Property SQL [aKind : TSQLKind] : TStrings Read GetSQLTyped;
  Published
    Property Fields : TSQLDBRestFieldList Read FFields Write SetFields;
    Property Enabled : Boolean Read FEnabled Write FEnabled default true;
    Property InMetadata : Boolean Read FInMetadata Write FInMetadata default true;
    Property ConnectionName : UTF8String read FConnectionName Write FConnectionName;
    Property TableName : UTF8String read FTableName Write FTableName;
    Property ResourceName : UTF8String read GetResourceName Write FResourceName;
    Property AllowedOperations : TRestOperations Read FAllowedOperations Write FAllowedOperations;
    Property SQLSelect : TStrings Index 0 Read GetSQL Write SetSQL;
    Property SQLInsert : TStrings Index 1 Read GetSQL Write SetSQL;
    Property SQLUpdate : TStrings Index 2 Read GetSQL Write SetSQL;
    Property SQLDelete : TStrings Index 3 Read GetSQL Write SetSQL;
    Property OnGetDataset : TSQLDBRestGetDatasetEvent Read FOnGetDataset Write FOnGetDataset;
    Property OnCheckParams : TSQLDBRestCheckParamsEvent Read FOnCheckParams Write FOnCheckParams;
    Property OnAllowRecord : TSQLDBRestAllowRecordEvent Read FOnAllowRecord Write FOnAllowRecord;
  end;

  { TSQLDBRestResourceList }

  TSQLDBRestResourceList = Class(TOwnedCollection)
  private
    function GetResource(aIndex : Integer): TSQLDBRestResource;
    procedure SetResource(aIndex : Integer; AValue: TSQLDBRestResource);
  Public
    Function Schema : TSQLDBRestSchema;
    Function AddResource(Const aTableName : UTF8String; Const aResourceName : UTF8String) : TSQLDBRestResource;
    Function indexOfTableName(Const aTableName : UTF8String) : Integer;
    Function indexOfResourceName(Const aResourceName : UTF8String) : Integer;
    Function FindResourceByName(Const aResourceName : UTF8String) : TSQLDBRestResource;
    Function FindResourceByTableName(Const aTableName : UTF8String) : TSQLDBRestResource;
    Procedure SaveToFile(Const aFileName : UTF8String);
    Procedure SaveToStream(Const aStream : TStream);
    function AsJSON(const aPropName: UTF8String=''): TJSONData;
    Procedure LoadFromFile(Const aFileName : UTF8String);
    Procedure LoadFromStream(Const aStream : TStream);
    Procedure FromJSON(aData: TJSONData;const aPropName: UTF8String='');
    Property Resources[aIndex : Integer] : TSQLDBRestResource read GetResource write SetResource; default;
  end;

  { TSQLDBRestSchema }

  TSQLDBRestSchema = Class(TComponent)
  private
    FConnectionName: UTF8String;
    FResources: TSQLDBRestResourceList;
    procedure SetResources(AValue: TSQLDBRestResourceList);
  Protected
    function CreateResourceList: TSQLDBRestResourceList; virtual;
    function GetPrimaryIndexFields(Q: TSQLQuery): TStringArray; virtual;
    function ProcessIdentifier(const S: UTF8String): UTF8String; virtual;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure SaveToFile(Const aFileName : UTF8String);
    Procedure SaveToStream(Const aStream : TStream);
    function AsJSON(const aPropName: UTF8String=''): TJSONData;
    Procedure LoadFromFile(Const aFileName : UTF8String);
    Procedure LoadFromStream(Const aStream : TStream);
    Procedure FromJSON(aData: TJSONData;const aPropName: UTF8String='');
    procedure PopulateResourceFields(aConn: TSQLConnection; aRes: TSQLDBRestResource; aMinFieldOpts : TRestFieldOptions = []); virtual;
    procedure PopulateResources(aConn: TSQLConnection; aTables: array of string; aMinFieldOpts: TRestFieldOptions= []);
    Procedure PopulateResources(aConn : TSQLConnection; aTables : TStrings = Nil; aMinFieldOpts : TRestFieldOptions = []);
  Published
    Property Resources : TSQLDBRestResourceList Read FResources Write SetResources;
    Property ConnectionName : UTF8String Read FConnectionName Write FConnectionName;
  end;

  TCustomViewResource = Class(TSQLDBRestResource)
  end;

Const
  TypeNames : Array[TRestFieldType] of string = ('?','int','bigint','float','date','time','datetime','string','bool','blob');

implementation

uses strutils, fpjsonrtti,dbconst, sqldbrestconst;


{ ESQLDBRest }

constructor ESQLDBRest.Create(aCode: Integer; const aMessage: String);
begin
  FResponseCode:=aCode;
  HelpContext:=aCode;
  Inherited create(aMessage);
end;

constructor ESQLDBRest.CreateFmt(aCode: Integer; const Fmt: String;
  const Args: array of const);
begin
  FResponseCode:=aCode;
  HelpContext:=aCode;
  Inherited CreateFmt(Fmt,Args);
end;


{ TSQLDBRestSchema }

procedure TSQLDBRestSchema.SetResources(AValue: TSQLDBRestResourceList);
begin
  if FResources=AValue then Exit;
  FResources.Assign(AValue);
end;

constructor TSQLDBRestSchema.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResources:=CreateResourceList;
end;

Function TSQLDBRestSchema.CreateResourceList :  TSQLDBRestResourceList;

begin
  Result:=TSQLDBRestResourceList.Create(Self,TSQLDBRestResource);
end;

destructor TSQLDBRestSchema.Destroy;
begin
  FreeAndNil(FResources);
  inherited Destroy;
end;

procedure TSQLDBRestSchema.SaveToFile(const aFileName: UTF8String);
Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TSQLDBRestSchema.SaveToStream(const aStream: TStream);

Var
  D : TJSONData;
  S : TJSONStringType;

begin
  D:=asJSON(JSONSchemaRoot);
  try
    S:=D.FormatJSON();
  finally
    D.Free;
  end;
  aStream.WriteBuffer(S[1],Length(S)*SizeOf(TJSONCharType));
end;

function TSQLDBRestSchema.AsJSON(const aPropName: UTF8String): TJSONData;

begin
  Result:=TJSONObject.Create([JSONResourcesRoot,Resources.AsJSON(),'connectionName',ConnectionName]);
  if (aPropName<>'') then
    Result:=TJSONObject.Create([aPropName,Result]);
end;

procedure TSQLDBRestSchema.LoadFromFile(const aFileName: UTF8String);
Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TSQLDBRestSchema.LoadFromStream(const aStream: TStream);
Var
  D : TJSONData;

begin
  D:=GetJSON(aStream);
  try
    FromJSON(D,JSONSchemaRoot);
  finally
    D.Free;
  end;
end;

procedure TSQLDBRestSchema.FromJSON(aData: TJSONData; const aPropName: UTF8String);

Var
  J : TJSONObject;

begin
  J:=aData as TJSONObject;
  Resources.FromJSON(J,JSONResourcesRoot);
  ConnectionName:=J.Get(aPropName,'');
end;

Function TSQLDBRestSchema.ProcessIdentifier(Const S : UTF8String) : UTF8String;

begin
  Result:=S;
end;


Function TSQLDBRestSchema.GetPrimaryIndexFields(Q : TSQLQuery) : TStringArray;

Var
  C,I : Integer;
  Fields : UTF8String;


begin
  Result:=Default(TStringArray);
  Q.ServerIndexDefs.Update;
  I:=0;
  Fields:='';
  With Q.ServerIndexDefs do
    While (Fields='') and (i<Count) do
      begin
      if (ixPrimary in Items[i].Options) then
        Fields:=Items[i].Fields;
      Inc(I);
      end;
  C:=WordCount(Fields,[';',' ']);
  SetLength(Result,C);
  For I:=1 to C do
    Result[I-1]:=ExtractWord(I,Fields,[';',' ']);
end;

procedure TSQLDBRestSchema.PopulateResourceFields(aConn : TSQLConnection; aRes : TSQLDBRestResource; aMinFieldOpts : TRestFieldOptions = []);

Var
  Q : TRestSQLQuery;
  IndexFields : TStringArray;


begin
  IndexFields:=Default(TStringArray);
  Q:=TRestSQLQuery.Create(Self);
  try
    Q.Database:=aConn;
    Q.ParseSQL:=True; // we want the table name
    if (aRes.SQLSelect.Count=0) then
      Q.SQL.Text:='SELECT * FROM '+aRes.TableName+' WHERE (1=0)' // Not very efficient :(
    else
      Q.SQL.Text:=aRes.GetResolvedSQL(skSelect,'(1=0)','');
    Q.TableName:=aRes.TableName;
    Q.UniDirectional:=True;
    Q.UsePrimaryKeyAsKey:=False;
    Q.Open;
    if (Q.TableName<>'') then
      IndexFields:=GetPrimaryIndexFields(Q);
    aRes.PopulateFieldsFromFieldDefs(Q.FieldDefs,IndexFields,@ProcessIdentifier,aMinFieldOpts)
  finally
    Q.Free;
  end;
end;

procedure TSQLDBRestSchema.PopulateResources(aConn: TSQLConnection; aTables : Array of string; aMinFieldOpts : TRestFieldOptions = []);

Var
  L : TStringList;
  S : String;

begin
  L:=TStringList.Create;
  try
    L.Capacity:=Length(aTables);
    For S in aTables do
      L.Add(S);
    L.Sorted:=True;
    PopulateResources(aConn,L,aMinFieldOpts);
  finally
    l.Free;
  end;
end;

procedure TSQLDBRestSchema.PopulateResources(aConn: TSQLConnection; aTables : TStrings = Nil; aMinFieldOpts : TRestFieldOptions = []);

Var
  L : TStrings;
  S,N : UTF8String;
  R : TSQLDBRestResource;


begin
  L:=TStringList.Create;
  try
    aConn.Connected:=True;
    aConn.GetTableNames(L);
    For S in L do
      begin
      N:=ProcessIdentifier(S);
      if SameStr(N,S) then // No SameText, Allow to change case
        N:='';
      if (aTables=Nil) or (aTables.IndexOf(S)=-1) then
        begin
        R:=Resources.AddResource(S,N);
        PopulateResourceFields(aConn,R,aMinFieldOpts);
        end;
      end;
  finally
    L.Free;
  end;
end;

{ TSQLDBRestResourceList }

function TSQLDBRestResourceList.GetResource(aIndex : Integer): TSQLDBRestResource;
begin
  Result:=TSQLDBRestResource(Items[aIndex])
end;

procedure TSQLDBRestResourceList.SetResource(aIndex : Integer; AValue: TSQLDBRestResource);
begin
  Items[aIndex]:=aValue;
end;

function TSQLDBRestResourceList.Schema: TSQLDBRestSchema;
begin
  If (Owner is  TSQLDBRestSchema) then
    Result:=Owner as  TSQLDBRestSchema
  else
    Result:=Nil;
end;

function TSQLDBRestResourceList.AddResource(const aTableName: UTF8String; const aResourceName: UTF8String): TSQLDBRestResource;

Var
  N : UTF8String;

begin
  N:=aResourceName;
  if N='' then
    N:=aTableName;
  if (N='') then
    Raise ESQLDBRest.Create(500,SErrResourceNameEmpty);
  if indexOfResourceName(N)<>-1 then
    Raise ESQLDBRest.CreateFmt(500,SErrDuplicateResource,[N]);
  Result:=add as TSQLDBRestResource;
  Result.TableName:=aTableName;
  Result.ResourceName:=aResourceName;
end;

function TSQLDBRestResourceList.indexOfTableName(const aTableName: UTF8String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aTableName,GetResource(Result).TableName) do
    Dec(Result);
end;

function TSQLDBRestResourceList.indexOfResourceName(const aResourceName: UTF8String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aResourceName,GetResource(Result).ResourceName) do
    Dec(Result);
end;

function TSQLDBRestResourceList.FindResourceByName(const aResourceName: UTF8String): TSQLDBRestResource;

Var
  Idx : Integer;

begin
  idx:=indexOfResourceName(aResourceName);
  if Idx=-1 then
    Result:=nil
  else
    Result:=GetResource(Idx);
end;

function TSQLDBRestResourceList.FindResourceByTableName(const aTableName: UTF8String): TSQLDBRestResource;
Var
  Idx : Integer;

begin
  idx:=indexOfTableName(aTableName);
  if Idx=-1 then
    Result:=nil
  else
    Result:=GetResource(Idx);
end;

procedure TSQLDBRestResourceList.SaveToFile(const aFileName: UTF8String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TSQLDBRestResourceList.SaveToStream(const aStream: TStream);

Var
  D : TJSONData;
  S : TJSONStringType;

begin
  D:=asJSON(JSONResourcesRoot);
  try
    S:=D.FormatJSON();
  finally
    D.Free;
  end;
  aStream.WriteBuffer(S[1],Length(S)*SizeOf(TJSONCharType));
end;

function TSQLDBRestResourceList.AsJSON(const aPropName: UTF8String = ''): TJSONData;

Var
  S : TJSONStreamer;
  A : TJSONArray;

begin
  S:=TJSONStreamer.Create(Nil);
  try
    A:=S.StreamCollection(Self);
  finally
    S.Free;
  end;
  if aPropName='' then
    Result:=A
  else
    Result:=TJSONObject.Create([aPropName,A]);
end;

procedure TSQLDBRestResourceList.LoadFromFile(const aFileName: UTF8String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TSQLDBRestResourceList.LoadFromStream(const aStream: TStream);

Var
  D : TJSONData;

begin
  D:=GetJSON(aStream);
  try
    FromJSON(D,JSONResourcesRoot);
  finally
    D.Free;
  end;
end;

procedure TSQLDBRestResourceList.FromJSON(aData: TJSONData; const aPropName: UTF8String);

Var
  A : TJSONArray;
  D : TJSONDestreamer;

begin
  if (aPropName<>'') then
    A:=(aData as TJSONObject).Arrays[aPropName]
  else
    A:=aData as TJSONArray;
  D:=TJSONDestreamer.Create(Nil);
  try
    Clear;
    D.JSONToCollection(A,Self);
  finally
    D.Free;
  end;
end;

{ TSQLDBRestResource }

function TSQLDBRestResource.GetResourceName: UTF8String;
begin
  Result:=FResourceName;
  if Result='' then
    Result:=FTableName;
end;

function TSQLDBRestResource.GetSQL(AIndex: Integer): TStrings;
begin
  Result:=FSQL[TSQLKind(aIndex)];
end;

function TSQLDBRestResource.GetSQLTyped(aKind : TSQLKind): TStrings;
begin
  Result:=FSQL[aKind];
end;

procedure TSQLDBRestResource.SetFields(AValue: TSQLDBRestFieldList);
begin
  if FFields=AValue then Exit;
  FFields:=AValue;
end;

procedure TSQLDBRestResource.SetSQL(AIndex: Integer; AValue: TStrings);
begin
  FSQL[TSQLKind(aIndex)].Assign(aValue);
end;

function TSQLDBRestResource.GetDisplayName: string;
begin
  Result:=ResourceName;
end;

constructor TSQLDBRestResource.Create(ACollection: TCollection);

Var
  K : TSQLKind;

begin
  inherited Create(ACollection);
  FFields:=CreateFieldList;
  FEnabled:=True;
  FInMetadata:=True;
  for K in TSQLKind do
    FSQL[K]:=TStringList.Create;
  FAllowedOperations:=AllRestOperations;
end;

destructor TSQLDBRestResource.Destroy;

Var
  K : TSQLKind;

begin
  FreeAndNil(FFields);
  for K in TSQLKind do
    FreeAndNil(FSQL[K]);
  inherited Destroy;
end;

procedure TSQLDBRestResource.CheckParams(aOperation: TRestoperation; P: TParams);
begin
  if Assigned(FOnCheckParams) then
    FOnCheckParams(Self,aOperation,P);
end;

function TSQLDBRestResource.GetDataset(aFieldList: TRestFieldPairArray; aOrderBy: TRestFieldOrderPairArray; aLimit, aOffset: Int64): TDataset;
begin
  Result:=Nil;
  If Assigned(FOnGetDataset) then
    FOnGetDataset(Self,aFieldList,aOrderBy,aLimit,aOffset,Result);
end;

function TSQLDBRestResource.GetSchema: TSQLDBRestSchema;
begin
  If Assigned(Collection) and (Collection is TSQLDBRestResourceList) then
    Result:=TSQLDBRestResourceList(Collection).Schema
  else
    Result:=Nil;
end;

procedure TSQLDBRestResource.Assign(Source: TPersistent);

Var
  R : TSQLDBRestResource;
  K : TSQLKind;

begin
  if (Source is TSQLDBRestResource) then
    begin
    R:=Source as TSQLDBRestResource;
    for K in TSQLKind do
      SQL[K].Assign(R.SQL[K]);
    Fields.Assign(R.Fields);
    TableName:=R.TableName;
    FResourceName:=R.FResourceName;
    ConnectionName:=R.ConnectionName;
    Enabled:=R.Enabled;
    InMetadata:=R.InMetadata;
    end
  else
    inherited Assign(Source);
end;

function TSQLDBRestResource.AllowRecord(aDataset: TDataset): Boolean;
begin
  Result:=True;
  if Assigned(FOnAllowRecord) then
    FOnAllowRecord(Self,aDataset,Result);
end;

function TSQLDBRestResource.GetHTTPAllow: String;

  Procedure AddR(s : String);

  begin
    if (Result<>'') then
      Result:=Result+', ';
    Result:=Result+S;
  end;

Const
  Methods : Array[TRestOperation] of string = ('','GET','POST','PUT','DELETE','OPTIONS','HEAD');

Var
  O : TRestOperation;

begin
  Result:='';
  For O in TRestOperation do
    if (O<>roUnknown) and (O in AllowedOperations) then
      AddR(Methods[O]);
end;

function TSQLDBRestResource.GetFieldList(aListKind : TFieldListKind) : UTF8String;

Const
  SepComma = ', ';
  SepAND = ' AND ';
  SepSpace = ' ';

Const
  Seps : Array[TFieldListKind] of string = (sepComma,sepComma,sepComma,sepComma,sepAnd,sepSpace,sepComma);

Const
  Wheres = [flWhereKey];
  Colons = Wheres + [flInsertParams];
  UseEqual = Wheres+[flUpdate];

Var
  Term,Res,Prefix : UTF8String;
  I : Integer;
  F : TSQLDBRestField;

begin
  Prefix:='';
  Res:='';
  If aListKind in Colons then
    Prefix:=':';
  For I:=0 to Fields.Count-1 do
    begin
    Term:='';
    F:=Fields[i];
    if F.UseInFieldList(aListKind) then
      begin
      Term:=Prefix+F.FieldName;
      if aListKind in UseEqual then
        begin
        Term := F.FieldName+' = '+Term;
        if (aListKind in Wheres) then
          Term:='('+Term+')';
        end;
      end;
    if (Term<>'') then
      begin
      If (Res<>'') then
        Res:=Res+Seps[aListKind];
      Res:=Res+Term;
      end;
    end;
  Result:=Res;
end;

function TSQLDBRestResource.GetFieldArray(aListKind: TFieldListKind
  ): TSQLDBRestFieldArray;
Var
  I,aCount : Integer;
  F : TSQLDBRestField;
begin
  Result:=Default(TSQLDBRestFieldArray);
  aCount:=0;
  SetLength(Result,Fields.Count);
  For I:=0 to Fields.Count-1 do
    begin
    F:=Fields[i];
    if F.UseInFieldList(aListKind) then
      begin
      Result[aCount]:=F;
      Inc(aCount);
      end;
    end;
  SetLength(Result,aCount);
end;

function TSQLDBRestResource.GenerateDefaultSQL(aKind: TSQLKind) : UTF8String;

begin
  Case aKind of
    skSelect :
      Result:='SELECT '+GetFieldList(flSelect)+' FROM '+TableName+' %FULLWHERE% %FULLORDERBY% %LIMIT%';
    skInsert :
      Result:='INSERT INTO '+TableName+' ('+GetFieldList(flInsert)+') VALUES ('+GetFieldList(flInsertParams)+')';
    skUpdate :
      Result:='UPDATE '+TableName+' SET '+GetFieldList(flUpdate)+' %FULLWHERE%';
    skDelete :
      Result:='DELETE FROM '+TableName+' %FULLWHERE%';
  else
    Raise ESQLDBRest.CreateFmt(500,SErrUnknownStatement,[Ord(aKind)]);
  end;
end;

function TSQLDBRestResource.GetResolvedSQl(aKind: TSQLKind;
  const AWhere: UTF8String; const aOrderBy: UTF8String; aLimit: UTF8String
  ): UTF8String;

Var
  S : UTF8String;

begin
  Result:=SQL[aKind].Text;
  if (Result='') then
    Result:=GenerateDefaultSQL(aKind);
  if (aWhere<>'') then
    S:='WHERE '+aWhere
  else
    S:='';
  Result:=StringReplace(Result,'%FULLWHERE%',S,[rfReplaceAll]);
  if (aWhere<>'') then
    S:=aWhere
  else
    S:='(1=0)';
  Result:=StringReplace(Result,'%REQUIREDWHERE%',S,[rfReplaceAll]);
  if (aWhere<>'') then
    S:='('+aWhere+')'
  else
    S:='';
  Result:=StringReplace(Result,'%WHERE%',S,[rfReplaceAll]);
  if (aOrderBy<>'') then
    S:='ORDER BY '+AOrderBy
  else
    S:='';
  Result:=StringReplace(Result,'%FULLORDERBY%',S,[rfReplaceAll]);
  Result:=StringReplace(Result,'%ORDERBY%',aOrderBy,[rfReplaceAll]);
  Result:=StringReplace(Result,'%LIMIT%',aLimit,[rfReplaceAll]);
end;

class function TSQLDBRestResource.FieldTypeToRestFieldType(
  aFieldType: TFieldType): TRestFieldType;

Const
  Map : Array[TFieldType] of TRestFieldType =
    (rftUnknown, rftString, rftInteger, rftInteger, rftInteger,                // ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
     rftBoolean, rftFloat, rftFloat, rftFloat, rftDate, rftTime, rftDateTime, // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
     rftBlob, rftBlob, rftInteger, rftBlob, rftString, rftUnknown, rftString, // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
     rftUnknown, rftUnknown, rftUnknown, rftUnknown, rftString,                // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
     rftString, rftLargeInt, rftUnknown, rftUnknown, rftUnknown,              // ftWideString, ftLargeint, ftADT, ftArray, ftReference,
     rftUnknown, rftBlob, rftBlob, rftUnknown, rftUnknown,                    //  ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
     rftUnknown, rftString, rftDateTime, rftFloat, rftString, rftString       /// ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo
     );

begin
  Result:=Map[aFieldType];
end;

procedure TSQLDBRestResource.PopulateFieldsFromFieldDefs(Defs: TFieldDefs; aIndexFields: TStringArray;
  aProcessIdentifier: TProcessIdentifier; aMinFieldOpts: TRestFieldOptions);

Var
  I : Integer;
  F : TSQLDBRestField;
  FN,PN : UTF8String;
  O : TRestFieldOptions;
  RFT : TRestFieldType;
  FD : TFieldDef;

begin
  For I:=0 to Defs.Count-1 do
    begin
    FD:=Defs[i];
    RFT:=FieldTypeToRestFieldType(FD.DataType);
    if RFT=rftUnknown then
      Continue;
    FN:=FD.Name;
    if Assigned(aProcessIdentifier) then
      PN:=aProcessIdentifier(FN);
    if SameStr(PN,FN) then // No SameText, Allow to change case
      PN:='';
    O:=aMinFieldOpts;
    if FD.Required then
       Include(O,foRequired);
    If AnsiIndexStr(FN,aIndexFields)<>-1 then
      begin
      Include(O,foInKey);
      Exclude(O,foFilter);
      end;
    F:=Fields.AddField(FN,RFT,O);
    if F.FieldType=rftString then
      F.MaxLen:=FD.Size;
    F.PublicName:=PN;
    end;
end;

class function TSQLDBRestResource.CreateFieldList: TSQLDBRestFieldList;

begin
  Result:=DefaultFieldListClass.Create(DefaultFieldClass);
end;

{ TSQLDBRestFieldList }

function TSQLDBRestFieldList.GetFields(aIndex: Integer): TSQLDBRestField;
begin
  Result:=TSQLDBRestField(Items[aIndex])
end;

procedure TSQLDBRestFieldList.SetFields(aIndex : Integer; AValue: TSQLDBRestField);
begin
  Items[aIndex]:=aValue;
end;

function TSQLDBRestFieldList.AddField(const aFieldName: UTF8String; aFieldType: TRestFieldType; aOptions: TRestFieldOptions
  ): TSQLDBRestField;
begin
  if IndexOfFieldName(aFieldName)<>-1 then
    Raise ESQLDBRest.CreateFmt(500,SDuplicateFieldName,[aFieldName]);
  Result:=Add as TSQLDBRestField;
  Result.FieldName:=aFieldName;
  Result.FieldType:=aFieldType;
  Result.Options:=aOptions;
end;

function TSQLDBRestFieldList.indexOfFieldName(const aFieldName : UTF8String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aFieldName,GetFields(Result).FieldName) do
    Dec(Result);
end;

function TSQLDBRestFieldList.FindByFieldName(const aFieldName: UTF8String
  ): TSQLDBRestField;
Var
  I : Integer;
begin
  I:=indexOfFieldName(aFieldName);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetFields(I);
end;

function TSQLDBRestFieldList.indexOfPublicName(const aPublicName : UTF8String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aPublicName,GetFields(Result).PublicName) do
    Dec(Result);
end;

function TSQLDBRestFieldList.FindByPublicName(const aFieldName: UTF8String
  ): TSQLDBRestField;
Var
  I : Integer;
begin
  I:=indexOfPublicName(aFieldName);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetFields(I);
end;

{ TSQLDBRestField }

function TSQLDBRestField.GetPublicName: UTF8String;
begin
  Result:=FPublicName;
  if (Result='') then
    Result:=FFieldName;
end;

constructor TSQLDBRestField.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFilters:=AllFieldFilters;
end;

procedure TSQLDBRestField.Assign(Source: TPersistent);

Var
  F : TSQLDBRestField;

begin
  if (Source is TSQLDBRestField) then
    begin
    F:=source as TSQLDBRestField;
    FieldName:=F.FieldName;
    FPublicName:=F.FPublicName;
    FieldType:=F.FieldType;
    NativeFieldType:=F.NativeFieldType;
    Options:=F.Options;
    Filters:=F.Filters;
    MaxLen:=F.MaxLen;
    GeneratorName:=F.GeneratorName;
    end
  else
    inherited Assign(Source);
end;

function TSQLDBRestField.GetDisplayName: string;
begin
  Result:=PublicName;
end;

function TSQLDBRestField.UseInFieldList(aListKind: TFieldListKind): Boolean;
begin
  Result:=True;
  Case aListKind of
    flSelect        : Result:=True;
    flInsert        : Result:=foInInsert in Options;
    flInsertParams  : Result:=(foInInsert in Options) and not (NativeFieldType=ftAutoInc);
    flUpdate        : Result:=foInUpdate in Options;
    flWhereKey      : Result:=foInKey in Options;
    flFilter        : Result:=foFilter in Options;
    flOrderby : Result:=([foOrderBy,foOrderByDesc]*options)<>[];
  end;
end;

end.

