{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Base SQL Builder.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Sql;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, Data.Db,
{$ELSE}
  Classes, SysUtils, typinfo, DB,
{$ENDIF}
  Opf.Types, Opf.Registry, Opf.Fields, Opf.Driver;

Const
  MaxStringLen = 32*1024;

Type

  TOPSQLBuilder = Class;
  TOPSQLBuilderClass = Class of TOPSQLBuilder;

  TSeparatorType = (stComma,stEqual,stAnd);

  TIdentifierType = (itField,itTable);

  TSQLBuildOption = (sboSpaceAfterSeparator,sboKeyWordsLowerCase,sboQuoteIdentifiers,sboAlwaysPrependTable);
  TSQLBuildOptions = Set of TSQLBuildOption;

  TSQLSelectOption = (soFieldListOnly,soPrependTable);
  TSQLSelectOptions = Set of TSQLSelectOption;

  TSQLInsertOption = (ioSkipFieldList,ioPrependTable);
  TSQLInsertOptions = set of TSQLInsertOption;

  TSQLUpdateOption = (uoFieldListOnly,uoPrependTable,uoSkipWhere);
  TSQLUpdateOptions = set of TSQLUpdateOption;

  TSQLDeleteOption = (doPrependTable);
  TSQLDeleteOptions = set of TSQLDeleteOption;

  TSQLWhereOption = (woPrependTable);
  TSQLWhereOptions = set of TSQLWhereOption;

  TSQLConstraintType = (ctPrimaryKey, ctForeignKey, ctUnique);
  TSQLConstraintTypes = Set of TSQLConstraintType;

  { TOPSQLBuilder }

  TOPSQLBuilder = Class(TOPBaseComponent)
  Private
    Class var
       _InstanceClass : TOPSQLBuilderClass;
       _Instance : TOPSQLBuilder;
  private
    FDriver: TSQLDriver;
    FDriverOwned: Boolean;
    FOptions: TSQLBuildOptions;
    FQuoteChar: String;
    FRegistry: TOPRegistry;
    FSQLDialect: TSQLDialect;
    function GetDriver: TSQLDriver;
    function GetQuoteChar: String;
    function GetRegistry: TOPRegistry;
    function IsQuoteCharStored: Boolean;
    procedure SetDriver(AValue: TSQLDriver);
    procedure SetRegistry(AValue: TOPRegistry);
    procedure SetSQLDialect(AValue: TSQLDialect);
  Protected
    // Add separator character/string to S.
    function AddSep(var S : UTF8string; aType : TSeparatorType) : Boolean; virtual;
    // Maybe quote an identifier.
    Function MaybeQuoteIdentifier(Const S : UTF8string; aType : TIdentifierType = itField; aForce : Boolean = False) : UTF8String; virtual;
    // Format keyword
    Function KeyWord(aKeyWord : TSQLKeyword) : UTF8String; virtual;
    // Based on fieldname, get param name. The field name itself when it is a
    // plain identifier outside the reserved opf_ prefix, otherwise a sanitized name.
    function GetParamName(const F: TOPFieldMap): String; virtual;
    // Transform update options to where clause.
    Function UpdateToWhereOptions(aUpdateOptions : TSQLUpdateOptions) : TSQLWhereOptions; virtual;
    // Transform delete options to where clause.
    Function DeleteToWhereOptions(aDeleteOptions : TSQLDeleteOptions) : TSQLWhereOptions; virtual;
    // Patterns that are not allowed in SQL identifiers. Override to adapt per dialect.
    Function GetUnsafeIdentifierPatterns : TUTF8StringArray; virtual;
  Public
    // Frees the shared builder when the program ends
    Class Destructor cleanup;
    // Sets the class used to create the shared builder (before it is created)
    Class Procedure SetInstanceClass(aClass: TOPSQLBuilderClass);
    // The shared builder, created on first use
    Class Function Instance : TOPSQLBuilder;
    // Frees the shared builder
    Class Procedure ClearInstance;
  Public
    // Creates a new SQL builder
    Constructor Create(aOwner : TComponent); override;
    // Frees the SQL builder
    Destructor Destroy; override;
    // The exception class raised for SQL building errors
    class function GetExceptionClass: EOPFClass; override;

    // Auto-increment support: does this dialect support INSERT ... RETURNING?
    Function SupportsReturningClause: Boolean; virtual;
    // Auto-increment support: generate RETURNING clause for auto-inc fields
    Function GetReturningClause(aMap : TOPFieldMapArray): UTF8String; virtual;
    // aInsertSQL with what the dialect needs to return the generated key
    Function AddAutoIncRetrieval(Const aInsertSQL : UTF8String; aMap : TOPFieldMapArray): UTF8String; virtual;
    // Auto-increment support: dialect-specific post-INSERT query to retrieve generated ID
    Function GetPostInsertValueSQL: UTF8String; virtual;
    // Sequence support: dialect-specific SQL to fetch next sequence value
    Function GetSequenceNextValSQL(const aSequenceName: UTF8String): UTF8String; virtual;
    // Create Select SQL. No where clause. If FieldListOnly is True, only the list of fields is created
    Function GetSelectSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; aSelectOptions : TSQLSelectOptions = []) : UTF8string; virtual;
    // Create insert SQL.
    Function GetInsertSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; aInsertOptions : TSQLInsertOptions = []) : UTF8string; virtual;
    // Create update SQL. Default is to add Where (key), but this can be disabled.
    Function GetUpdateSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; aUpdateOptions : TSQLUpdateOptions = []) : UTF8string; virtual;
    // Generate DELETE FROM table WHERE key_fields SQL.
    Function GetDeleteSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; aDeleteOptions : TSQLDeleteOptions = []) : UTF8string; virtual;
    // Generate UPDATE <table> SET <softDeleteCol> = :opf_sd WHERE <pk> for soft delete
    Function GetSoftDeleteSQL(Const aTable: UTF8String; aMap: TOPFieldMapArray; aSoftDeleteField: TOPFieldMap): UTF8String; virtual;
    // Name of the parameter for field F in generated INSERT/UPDATE/DELETE SQL
    Function FieldParamName(const F: TOPFieldMap): String;
    // Construct a where clause based on the key fields in aMap. Only fields with foInKey are considered.
    function GetWhereKey(Const aTable: UTF8String; aMap: TOPFieldMapArray; aWhereOptions : TSQLWhereOptions = []): UTF8string;
    // Identifier for use in hand-built SQL: validated, quoted or transformed
    // according to Options and the driver's reserved word strategy.
    Function Identifier(Const aName : UTF8String; aType : TIdentifierType = itField) : UTF8String;
    // table.field, both parts formatted with Identifier
    Function QualifiedIdentifier(Const aTable, aField : UTF8String) : UTF8String;
    // Same as Identifier(aName), usable as a TOPIdentifierFormatter
    Function FormatFieldIdentifier(Const aName : UTF8String) : UTF8String;
    { The name of aName as the database stores it, without quotes: the name
      that Identifier emits after the reserved word strategy (e.g. Order_ for
      Order with rwsAppendChar). Use it to find a column in a result set or in
      the database catalog. Quoting never changes the name. }
    Function PhysicalName(Const aName : UTF8String) : UTF8String; virtual;
    // The SQL driver for dialect-specific behavior. Auto-created from SQLDialect when not explicitly set.
    Property Driver : TSQLDriver Read GetDriver Write SetDriver;
  Published
    // Registry to look up classes
    Property Registry : TOPRegistry Read GetRegistry Write SetRegistry;
    // Options controlling how SQL statements are formatted
    Property Options : TSQLBuildOptions Read FOptions Write FOptions;
    // Quote character when quoting identifiers. Empty: use the driver's quoting.
    Property QuoteChar : String Read GetQuoteChar Write FQuoteChar stored IsQuoteCharStored;
    // The kind of database the statements are written for
    Property SQLDialect : TSQLDialect Read FSQLDialect Write SetSQLDialect;
  end;

// The shared SQL builder
Function OPSQLBuilder : TOPSQLBuilder;

// Default parameter name for a column: the column name itself when it is a
// plain identifier outside the reserved opf_ / _opf_ prefixes, otherwise
// OPFEncodedParamName(aFieldName).
Function OPFFieldParamName(const aFieldName : String) : String;
// Parameter name opf_c_<sanitized name>_<hash> for a column
Function OPFEncodedParamName(const aFieldName : String) : String;

Const
  // uppercased only
  NeedFloatDialects = [dVanilla];


implementation

uses Opf.Consts, Opf.Logging;

function OPSQLBuilder: TOPSQLBuilder;
begin
  Result:=TOPSQLBuilder.Instance;
end;

function OPFFieldParamName(const aFieldName: String): String;

  function IsPlainParamName(const S: String): Boolean;
  var
    I: Integer;
  begin
    Result:=(S<>'') and (S[1] in ['A'..'Z','a'..'z','_']);
    for I:=2 to Length(S) do
      if not (S[I] in ['A'..'Z','a'..'z','0'..'9','_']) then
        Result:=False;
  end;

begin
  Result:=aFieldName;
  // Internal parameters (opf_sd, opf_pk, opf_oldversion, ...) use these prefixes
  if IsPlainParamName(Result)
     and not SameText(Copy(Result, 1, 4), 'opf_')
     and not SameText(Copy(Result, 1, 5), '_opf_') then
    Exit;
  Result:=OPFEncodedParamName(aFieldName);
end;

function OPFEncodedParamName(const aFieldName: String): String;

var
  I: Integer;
  S: String;

begin
  S:='';
  for I:=1 to Length(aFieldName) do
    if aFieldName[I] in ['A'..'Z','a'..'z','0'..'9','_'] then
      S:=S+aFieldName[I]
    else
      S:=S+'_';
  Result:='opf_c_'+S+'_'+OPFNameHash(aFieldName);
end;

{ TOPSQLBuilder }

function TOPSQLBuilder.GetDriver: TSQLDriver;
begin
  if FDriver=Nil then
  begin
    FDriver:=TSQLDriver.CreateDriver(FSQLDialect);
    FDriverOwned:=True;
  end;
  Result:=FDriver;
end;

procedure TOPSQLBuilder.SetDriver(AValue: TSQLDriver);
begin
  if FDriver=AValue then Exit;
  if FDriverOwned then
    FreeAndNil(FDriver);
  FDriver:=AValue;
  FDriverOwned:=False;
  if Assigned(FDriver) then
    FSQLDialect:=FDriver.Dialect;
end;

procedure TOPSQLBuilder.SetSQLDialect(AValue: TSQLDialect);
begin
  if FSQLDialect=AValue then Exit;
  FSQLDialect:=AValue;
  if FDriverOwned then
    FreeAndNil(FDriver);
  // Driver will be re-created on next access via GetDriver
end;

function TOPSQLBuilder.KeyWord(aKeyWord: TSQLKeyword): UTF8String;

begin
  Result:=SQLKeyWords[aKeyWord];
  if (sboKeywordsLowerCase in Options) then
    Result:=LowerCase(Result);
end;


function TOPSQLBuilder.GetUnsafeIdentifierPatterns: TUTF8StringArray;
begin
  Result:=TUTF8StringArray.Create(';','''','--','/*',#10,#13);
end;

function TOPSQLBuilder.MaybeQuoteIdentifier(const S: UTF8string; aType : TIdentifierType = itField; aForce : Boolean = False): UTF8String;
var
  P: UTF8String;
begin
  for P in GetUnsafeIdentifierPatterns do
    if Pos(P,S)>0 then
      RaiseError('MaybeQuoteIdentifier', SErrUnsafeIdentifier, [S]);
  aForce:=aForce or (sboQuoteIdentifiers in Options);
  if aForce and (FQuoteChar<>'') then
    Result:=FQuoteChar+StringReplace(S,FQuoteChar,FQuoteChar+FQuoteChar,[rfReplaceAll])+FQuoteChar
  else
    Result:=Driver.FormatIdentifier(S,aForce);
end;

function TOPSQLBuilder.Identifier(const aName: UTF8String; aType: TIdentifierType): UTF8String;
begin
  Result:=MaybeQuoteIdentifier(aName,aType);
end;

function TOPSQLBuilder.QualifiedIdentifier(const aTable, aField: UTF8String): UTF8String;
begin
  Result:=MaybeQuoteIdentifier(aTable,itTable)+'.'+MaybeQuoteIdentifier(aField,itField);
end;

function TOPSQLBuilder.FormatFieldIdentifier(const aName: UTF8String): UTF8String;
begin
  Result:=MaybeQuoteIdentifier(aName,itField);
end;

function TOPSQLBuilder.PhysicalName(const aName: UTF8String): UTF8String;
begin
  Result:=aName;
  // Same decision as MaybeQuoteIdentifier: forced quoting skips the strategy.
  // Only rwsAppendChar renames; the other strategies quote or keep the name.
  if (sboQuoteIdentifiers in Options) or (Driver.ReservedWordStrategy<>rwsAppendChar) then
    Exit;
  Result:=Driver.TransformIdentifier(aName);
end;

function TOPSQLBuilder.GetQuoteChar: String;
begin
  Result:=FQuoteChar;
  if Result='' then
    Result:=Driver.QuoteChar;
end;

function TOPSQLBuilder.IsQuoteCharStored: Boolean;
begin
  Result:=FQuoteChar<>'';
end;

function TOPSQLBuilder.AddSep(var S: UTF8string; aType: TSeparatorType): Boolean;

Const
  Seps : Array[TSeparatorType] of UTF8String = (',','=','');
  KWSeps = [stAnd];
  KW : Array[TSeparatorType] of TSQLKeyWord = (kwNone,kwNone,kwAnd);

begin
  Result:=S<>'';
  if (Not Result) then
    Exit;
  if aType in KWSeps then
    S:=S+' '+Keyword(KW[aType])+' '
  else
    begin
    if (sboSpaceAfterSeparator in Options) and (aType = stEqual) then
      S:=S+' ';
    S:=S+Seps[aType];
    if sboSpaceAfterSeparator in Options then
      S:=S+' ';
    end;
end;

constructor TOPSQLBuilder.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FQuoteChar:=''; // Use the driver's quoting
end;

destructor TOPSQLBuilder.Destroy;
begin
  if FDriverOwned then
    FDriver.Free;
  inherited Destroy;
end;

function TOPSQLBuilder.GetSelectSQL(const aTable: UTF8string; aMap: TOPFieldMapArray; aSelectOptions: TSQLSelectOptions
  ): UTF8string;

Var
  F : TOPFieldMap;

begin
  if (aTable='') and not (soFieldListOnly in aSelectOptions) then
    RaiseError('GetSelectSQL', SErrEmptyTableName);
  Result:='';
  For F in aMap do
    if foInSelect in F.Options then
      begin
      AddSep(Result,stComma);
      if (sboAlwaysPrependTable in Options) or (soPrependTable in aSelectOptions) then
        Result:=Result+MaybeQuoteIdentifier(aTable,itTable)+'.';
      Result:=Result+MaybeQuoteIdentifier(F.FieldName);
      end;
  if  (Result<>'') and Not (soFieldListOnly in aSelectOptions) then
    Result:=KeyWord(kwSelect)+' '+Result+' '+Keyword(kwFrom)+' '+MaybeQuoteIdentifier(aTable,itTable);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkSQL) then
    TOPLogger(GetLogger).Log(Self, lkSQL, Result);
  {$ENDIF}
end;

function TOPSQLBuilder.GetParamName(const F : TOPFieldMap) : String;

begin
  Result:=OPFFieldParamName(F.FieldName);
  if not Driver.IsValidParamName(Result) then
    Result:=OPFEncodedParamName(F.FieldName);
end;

function TOPSQLBuilder.FieldParamName(const F: TOPFieldMap): String;
begin
  Result:=GetParamName(F);
end;

function TOPSQLBuilder.UpdateToWhereOptions(aUpdateOptions: TSQLUpdateOptions): TSQLWhereOptions;

begin
  Result:=[];
  if uoPrependTable in aUpdateOptions then
    Result:=Result+[woPrependTable];
end;

class destructor TOPSQLBuilder.cleanup;
begin
  ClearInstance;
end;

class function TOPSQLBuilder.GetExceptionClass: EOPFClass;
begin
  Result := EOPFSQL;
end;

class procedure TOPSQLBuilder.SetInstanceClass(aClass: TOPSQLBuilderClass);
begin
  if Assigned(_Instance) then
     _Instance.RaiseError('SetInstanceClass', SErrSQLBuilderCreated);
  _InstanceClass:=aClass;
end;

class function TOPSQLBuilder.Instance: TOPSQLBuilder;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
         begin
         if _InstanceClass=Nil then
            _InstanceClass:=TOPSQLBuilder;
         _Instance:=_InstanceClass.Create(Nil);
         end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPSQLBuilder.ClearInstance;
begin
  FreeAndNil(_Instance);
end;

function TOPSQLBuilder.SupportsReturningClause: Boolean;
begin
  Result:=Driver.SupportsReturningClause;
end;

function TOPSQLBuilder.AddAutoIncRetrieval(const aInsertSQL: UTF8String; aMap: TOPFieldMapArray): UTF8String;
var
  F: TOPFieldMap;
  Col: UTF8String;
begin
  Col:='';
  for F in aMap do
    if (foAutoInc in F.Options) and (Col='') then
      Col:=MaybeQuoteIdentifier(F.FieldName);
  Result:=aInsertSQL;
  if Col='' then
    Exit;
  case Driver.AutoIncRetrieval of
    airSameBatch:
      // One batch, so SCOPE_IDENTITY() sees the insert; NOCOUNT keeps the
      // row count message of the insert from hiding the result set. It is
      // switched off again before the SELECT: NOCOUNT is a session setting,
      // and with it on, RowsAffected of later statements is -1, which would
      // disable the optimistic locking and delete row checks. When an error
      // aborts the batch, the executor runs Driver.GetSameBatchResetSQL.
      // (OUTPUT INSERTED is not used: it fails on tables with triggers.)
      Result:='SET NOCOUNT ON; '+aInsertSQL+'; SET NOCOUNT OFF; '+GetPostInsertValueSQL;
    airReturningInto:
      Result:=aInsertSQL+' RETURNING '+Col+' INTO :opf_ret';
  else
    Result:=aInsertSQL+GetReturningClause(aMap);
  end;
end;

function TOPSQLBuilder.GetReturningClause(aMap: TOPFieldMapArray): UTF8String;
var
  F: TOPFieldMap;
begin
  Result:='';
  if not SupportsReturningClause then
    Exit;
  for F in aMap do
    if foAutoInc in F.Options then
    begin
      AddSep(Result, stComma);
      Result:=Result+MaybeQuoteIdentifier(F.FieldName);
    end;
  if Result<>'' then
    Result:=' RETURNING '+Result;
end;

function TOPSQLBuilder.GetPostInsertValueSQL: UTF8String;
begin
  Result:=Driver.GetPostInsertValueSQL;
end;

function TOPSQLBuilder.GetSequenceNextValSQL(const aSequenceName: UTF8String): UTF8String;
begin
  Result:=Driver.GetSequenceNextValSQL(aSequenceName);
end;

function TOPSQLBuilder.GetInsertSQL(const aTable: UTF8string; aMap: TOPFieldMapArray; aInsertOptions: TSQLInsertOptions
  ): UTF8string;

Var
  F : TOPFieldMap;
  FL,VL,AutoIncCol : UTF8String;

begin
  if aTable='' then
    RaiseError('GetInsertSQL', SErrEmptyTableName);
  Result:='';
  FL:='';
  VL:='';
  For F in aMap do
    if (foInInsert in F.Options) and not (foAutoInc in F.Options) then
      begin
      AddSep(FL,stComma);
      // Firebird, SQL Server support this. Postgres, Sqlite not.
      if (ioPrependTable in aInsertOptions) or (sboAlwaysPrependTable in Options) then
        FL:=FL+MaybeQuoteIdentifier(aTable,itTable)+'.';
      FL:=FL+MaybeQuoteIdentifier(F.FieldName);
      AddSep(VL,stComma);
      VL:=VL+':'+GetParamName(F);
      end;
  if (FL<>'') then
    begin
    if not (ioSkipFieldList in aInsertOptions) then
      Result:=' ('+FL+')';
    Result:=KeyWord(kwInsert)+' '+KeyWord(kwInto)+' '+MaybeQuoteIdentifier(aTable,itTable)+Result+' '+Keyword(kwValues)+' ('+VL+')';
    end
  else
    begin
    // Only auto-increment columns: every column gets its default
    AutoIncCol:='';
    For F in aMap do
      if (foAutoInc in F.Options) and (AutoIncCol='') then
        AutoIncCol:=MaybeQuoteIdentifier(F.FieldName);
    Result:=Driver.GetEmptyInsertSQL(MaybeQuoteIdentifier(aTable,itTable),AutoIncCol);
    end;
  Result:=AddAutoIncRetrieval(Result, aMap);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkSQL) then
    TOPLogger(GetLogger).Log(Self, lkSQL, Result);
  {$ENDIF}
end;

function TOPSQLBuilder.DeleteToWhereOptions(aDeleteOptions: TSQLDeleteOptions): TSQLWhereOptions;

begin
  Result:=[];
  if doPrependTable in aDeleteOptions then
    Result:=Result+[woPrependTable];
end;

function TOPSQLBuilder.GetDeleteSQL(const aTable: UTF8string; aMap: TOPFieldMapArray; aDeleteOptions: TSQLDeleteOptions): UTF8string;

Var
  W : UTF8String;

begin
  if aTable='' then
    RaiseError('GetDeleteSQL', SErrEmptyTableName);
  W:=GetWhereKey(aTable,aMap,DeleteToWhereOptions(aDeleteOptions));
  if W='' then
    RaiseError('GetDeleteSQL', SErrDeleteNoKeyFields);
  Result:=KeyWord(kwDelete)+' '+KeyWord(kwFrom)+' '+MaybeQuoteIdentifier(aTable,itTable)+
          ' '+KeyWord(kwWhere)+' '+W;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkSQL) then
    TOPLogger(GetLogger).Log(Self, lkSQL, Result);
  {$ENDIF}
end;

function TOPSQLBuilder.GetSoftDeleteSQL(const aTable: UTF8String; aMap: TOPFieldMapArray; aSoftDeleteField: TOPFieldMap): UTF8String;
var
  W: UTF8String;
begin
  if aTable = '' then
    RaiseError('GetSoftDeleteSQL', SErrEmptyTableName);
  W := GetWhereKey(aTable, aMap);
  if W = '' then
    RaiseError('GetSoftDeleteSQL', SErrDeleteNoKeyFields);
  Result := KeyWord(kwUpdate) + ' ' + MaybeQuoteIdentifier(aTable, itTable) + ' ' +
            KeyWord(kwSet) + ' ' + MaybeQuoteIdentifier(aSoftDeleteField.FieldName) +
            ' = :opf_sd ' + KeyWord(kwWhere) + ' ' + W;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkSQL) then
    TOPLogger(GetLogger).Log(Self, lkSQL, Result);
  {$ENDIF}
end;

function TOPSQLBuilder.GetUpdateSQL(const aTable: UTF8string; aMap: TOPFieldMapArray; aUpdateOptions: TSQLUpdateOptions
  ): UTF8string;

Var
  F : TOPFieldMap;
  W : UTF8String;

begin
  if aTable='' then
    RaiseError('GetUpdateSQL', SErrEmptyTableName);
  Result:='';
  For F in aMap do
    if (foInUpdate in F.Options)  then
      begin
      AddSep(Result,stComma);
      if (uoPrependTable in aUpdateOptions) or (sboAlwaysPrependTable in Options) then
        Result:=Result+MaybeQuoteIdentifier(aTable,itTable)+'.';
      Result:=Result+MaybeQuoteIdentifier(F.FieldName);
      AddSep(Result,stEqual);
      Result:=Result+':'+GetParamName(F);
      end;
  if Result='' then
    RaiseError('GetUpdateSQL', SErrNoUpdatableFields, [aTable]);
  If not (uoFieldListOnly in aUpdateOptions) then
    begin
    Result:=KeyWord(kwUpdate)+' '+MaybeQuoteIdentifier(aTable,itTable)+' '+KeyWord(kwSet)+' '+Result;
    If not (uoSkipWhere in aUpdateOptions) then
      begin
      W:=GetWhereKey(aTable,aMap,UpdateToWhereOptions(aUpdateOptions));
      if W='' then
        RaiseError('GetUpdateSQL', SErrUpdateNoKeyFields);
      Result:=Result+' '+KeyWord(kwWhere)+' '+W;
      end;
    end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkSQL) then
    TOPLogger(GetLogger).Log(Self, lkSQL, Result);
  {$ENDIF}
end;

function TOPSQLBuilder.GetWhereKey(const aTable: UTF8String; aMap: TOPFieldMapArray; aWhereOptions: TSQLWhereOptions): UTF8string;

Var
  F : TOPFieldMap;

begin
  Result:='';
  for F in aMap do
    if foInKey in F.Options then
      begin
      AddSep(Result,stAnd);
      Result:=Result+'(';
      if (woPrependTable in aWhereOptions) or (sboAlwaysPrependTable in Options) then
        Result:=Result+MaybeQuoteIdentifier(aTable,itTable)+'.';
      Result:=Result+MaybeQuoteIdentifier(F.FieldName);
      AddSep(Result,stEqual);
      Result:=Result+':'+GetParamName(F)+')';
      end;
end;

function TOPSQLBuilder.GetRegistry: TOPRegistry;
begin
  Result := FRegistry;
  if Result = nil then
    Result := TOPRegistry.Instance;
end;


procedure TOPSQLBuilder.SetRegistry(AValue: TOPRegistry);

begin
  if FRegistry=AValue then Exit;
  if Assigned(FRegistry) then
    FRegistry.RemoveFreeNotification(Self);
  FRegistry:=AValue;
  if Assigned(FRegistry) then
    FRegistry.FreeNotification(Self);
end;


end.
