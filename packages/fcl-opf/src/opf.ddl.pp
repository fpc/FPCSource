{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    SQL generation for DDL operations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.DDL;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, Data.Db, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, typinfo, db, sqldb,
{$ENDIF}
  Opf.Types, Opf.Driver, Opf.Sql, Opf.Registry, Opf.Fields, Opf.Relations, Opf.M2M, Opf.Schema;

Type

  TOPDDLBuilder = Class;
  TOPDDLBuilderClass = Class of TOPDDLBuilder;

  TSQLCreateOption = (coForceQuotes,coForceNewLine,coIfNotExists);
  TSQLCreateOptions = set of TSQLCreateOption;

  TSchemaCreateOption = (scoForeignKeys, scoIndexes, scoJunctionTables);
  TSchemaCreateOptions = set of TSchemaCreateOption;

  // Called inside the DDL transaction, after the statements and before the commit
  TOPDDLTransactionEvent = procedure(aConnection: TSQLConnection; aTransaction: TSQLTransaction) of object;

  { TOPDDLBuilder }

  TOPDDLBuilder = Class(TOPBaseComponent)
  Private
    Class var
       _InstanceClass : TOPDDLBuilderClass;
       _Instance : TOPDDLBuilder;
  private
    FDefaultStringFieldLength: Integer;
    FDriver: TSQLDriver;
    FDriverOwned: Boolean;
    FFKPrefix: String;
    FMaxStringFieldLength: Integer;
    FOptions: TSQLBuildOptions;
    FPKSuffix: String;
    FQuoteChar: String;
    FRegistry: TOPRegistry;
    FSQLDialect: TSQLDialect;
    FUQPrefix: String;
    function GetDriver: TSQLDriver;
    function GetQuoteChar: String;
    function GetRegistry: TOPRegistry;
    function IsQuoteCharStored: Boolean;
    procedure SetDriver(AValue: TSQLDriver);
    procedure SetRegistry(AValue: TOPRegistry);
    procedure SetSQLDialect(AValue: TSQLDialect);
  Protected
    // PostgreSQL: run aStatement only if aTable has no constraint aConstraint (both as written in SQL)
    function PGIfNoConstraint(const aTable, aConstraint, aStatement: UTF8String): UTF8String;
    // Run statements and aInTransaction in one transaction of our own
    procedure RunDDL(aConnection: TSQLConnection; const aStatements: array of UTF8String;
      aInTransaction: TOPDDLTransactionEvent; aNumbered: Boolean);
    // Add separator character/string to S.
    function AddSep(var S : UTF8string; aType : TSeparatorType) : Boolean; virtual;
    // Format keyword
    Function KeyWord(aKeyWord : TSQLKeyword) : UTF8String; virtual;
    // Format a type name obtained from the driver like a keyword
    Function TypeSQL(const aType : UTF8String) : UTF8String;
    // 'ALTER TABLE <table> '
    Function AlterTablePrefix(const aTable : UTF8String) : UTF8String;
    // NULL or NOT NULL
    Function NullClause(aNullable : Boolean) : UTF8String;
    // Dialect keyword resolution - delegates to Driver
    function GetBooleanKeyword: TSQLKeyWord; virtual;
    // Column type for a 32-bit whole number: the driver type or the INT keyword
    function IntTypeSQL: UTF8String;
    // Column type for a 16-bit whole number: the driver type or the SMALLINT keyword
    function SmallIntTypeSQL: UTF8String;
    function GetBlobKeyword: TSQLKeyWord; virtual;
    function GetMemoKeyword: TSQLKeyWord; virtual;
    function GetMemoSuffix: UTF8String; virtual;
    function NeedsFloatOverDecimal: Boolean; virtual;
    function SupportsIfNotExists: Boolean; virtual;
    // The following are used when creating a CREATE sql statement
    Function BlobField : String; virtual;
    Function MemoField : String; virtual;
    function GetClassField(aField: TOPFieldMap; const aTable: UTF8string): UTF8string; virtual;
    function GetFloatField(aField: TOPFieldMap; const aTable: UTF8string): UTF8string; virtual;
    function GetStringField(aField: TOPFieldMap; const aTable: UTF8string): UTF8String; virtual;
    function GetIntegerField(aField: TOPFieldMap; const aTable: String): UTF8String; virtual;
    function GetBooleanField(aField: TOPFieldMap; const aTable: UTF8string): UTF8string; virtual;
    // ' DEFAULT <value>' for a field with a default value, empty otherwise
    function GetDefaultClauseSQL(aField: TOPFieldMap): UTF8String; virtual;
    // MySQL MODIFY COLUMN repeats the whole column: type, null constraint,
    // default and AUTO_INCREMENT (these are lost otherwise)
    function GetMySQLColumnRedefinition(const aTable: UTF8String; aField: TOPFieldMap; aNullable: Boolean): UTF8String; virtual;
    // Generated constraint name, shortened to the dialect's identifier length.
    // For a foreign key aObjectName is the parent table.
    Function GetConstraintName(Const aTableName,aObjectName : String; aType : TSQLConstraintType) : UTF8String; virtual;
    // A user supplied name: quoted as needed, raises if too long for the dialect
    Function CheckedName(Const aName : UTF8String) : UTF8String;
    // The foreign key of GetForeignKeySQL as a table constraint for CREATE TABLE
    // (CONSTRAINT ... FOREIGN KEY ...), empty if it cannot be derived
    Function GetInlineForeignKeySQL(aRelation: TOPRelationDef; aRegistry: TOPRegistry;
      aCreateOptions: TSQLCreateOptions): UTF8String; virtual;
    // Patterns that are not allowed in SQL identifiers. Override to adapt per dialect.
    Function GetUnsafeIdentifierPatterns : TUTF8StringArray; virtual;
  Public
    // Frees the shared builder instance when the program ends
    Class Destructor cleanup;
    // Sets the class used to create the shared builder instance
    Class Procedure SetInstanceClass(aClass: TOPDDLBuilderClass);
    // Returns the shared builder instance, creating it when needed
    Class Function Instance : TOPDDLBuilder;
    // Frees the shared builder instance
    Class Procedure ClearInstance;
  Public
    // Creates a table builder with default settings
    Constructor Create(aOwner : TComponent); override;
    // Frees the builder and the driver it created
    Destructor Destroy; override;
    // The exception class raised for builder errors
    class function GetExceptionClass: EOPFClass; override;

    // Identifier for use in DDL: validated, quoted (aForce or sboQuoteIdentifiers)
    // or transformed according to the driver's reserved word strategy.
    Function MaybeQuoteIdentifier(Const S : UTF8string; aType : TIdentifierType = itField; aForce : Boolean = False) : UTF8String; virtual;
    // Column definition of the field, as used when creating the table
    function GetPropertyDefSQL(Const aTable: UTF8string; aField: TOPFieldMap): UTF8String; virtual;
    // Statement that creates the table
    Function GetCreateSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; aCreateOptions : TSQLCreateOptions = []) : UTF8string; virtual;
    // Create table SQL with the given name for the primary key constraint
    Function GetCreateSQL(Const aTable : UTF8string; aMap : TOPFieldMapArray; const aPKConstraintName: UTF8String; aCreateOptions : TSQLCreateOptions = []) : UTF8string; virtual;
    // Runs one statement that changes the database structure, in a transaction of its own.
    // When the connection's transaction is active, it is left alone: on PostgreSQL and
    // Firebird the statement runs beside it, other databases raise EOPFDDL.
    Procedure ExecDDL(aConnection: TSQLConnection; const aSQL: UTF8String); virtual;
    // Execute DDL statements. When the dialect supports transactional DDL, all
    // statements and aInTransaction run in one transaction: a failure leaves
    // nothing applied. Otherwise each statement is committed on its own and
    // aInTransaction runs in a transaction of its own after the last one.
    Procedure ExecDDLList(aConnection: TSQLConnection; const aStatements: TUTF8StringArray;
      aInTransaction: TOPDDLTransactionEvent = Nil); virtual;
    // Generate and execute CREATE TABLE with automatic transaction management
    Procedure CreateTable(aConnection: TSQLConnection; const aTable: UTF8String; aMap: TOPFieldMapArray; aCreateOptions: TSQLCreateOptions = []); virtual;
    // Generate ALTER TABLE ADD CONSTRAINT FOREIGN KEY SQL
    Function GetAddForeignKeySQL(const aChildTable, aFKFieldName, aParentTable, aPKFieldName: UTF8String;
      aDBCascade: Boolean; const aConstraintName: UTF8String = ''; aCreateOptions: TSQLCreateOptions = []): UTF8String; virtual;
    // ALTER TABLE ADD CONSTRAINT for a relation, metadata resolved from the registry
    Function GetForeignKeySQL(aRelation: TOPRelationDef; aRegistry: TOPRegistry;
      aCreateOptions: TSQLCreateOptions = []): UTF8String; virtual;
    // Adds the foreign key of a relation to the database
    Procedure CreateForeignKey(aConnection: TSQLConnection; aRelation: TOPRelationDef; aRegistry: TOPRegistry;
      aCreateOptions: TSQLCreateOptions = []); virtual;
    // Statement that creates the link table of a many-to-many relation
    Function GetCreateJunctionTableSQL(aDef: TOPManyToManyDef; aRegistry: TOPRegistry;
      aCreateOptions: TSQLCreateOptions = []): UTF8String; virtual;
    // Creates the link table of a many-to-many relation in the database
    Procedure CreateJunctionTable(aConnection: TSQLConnection; aDef: TOPManyToManyDef;
      aRegistry: TOPRegistry; aCreateOptions: TSQLCreateOptions = []); virtual;
    // Generate CREATE INDEX SQL
    Function GetCreateIndexSQL(const aTable: UTF8String; aIndex: TOPIndexDef; aCreateOptions: TSQLCreateOptions = []): UTF8String; virtual;
    // Execute CREATE INDEX for each index definition
    Procedure CreateIndexes(aConnection: TSQLConnection; const aTable: UTF8String; aIndexes: TOPIndexDefArray;
      aCreateOptions: TSQLCreateOptions = []); virtual;
    // The registered classes in the order their tables can be created: parents before children
    Function GetCreationOrder(aRegistry: TOPRegistry; aRelationRegistry: TOPRelationRegistry): TClassArray; virtual; overload;
    // aAllowCycles: classes in a relation cycle are added in registration order
    // instead of raising. Only valid when foreign keys are added after all tables.
    Function GetCreationOrder(aRegistry: TOPRegistry; aRelationRegistry: TOPRelationRegistry; aAllowCycles: Boolean): TClassArray; virtual; overload;
    // The statements of CreateSchema, in execution order. SQLite cannot add a
    // foreign key to an existing table: there they are part of CREATE TABLE.
    Function GetCreateSchemaSQL(aRegistry: TOPRegistry; aRelationRegistry: TOPRelationRegistry;
      aM2MRegistry: TOPManyToManyRegistry; aCreateOptions: TSQLCreateOptions;
      aSchemaOptions: TSchemaCreateOptions): TUTF8StringArray; virtual;
    // Create complete schema: tables, indexes, foreign keys, junction tables.
    // On PostgreSQL and SQLite a failure leaves nothing created.
    Procedure CreateSchema(aConnection: TSQLConnection; aRegistry: TOPRegistry;
      aRelationRegistry: TOPRelationRegistry; aM2MRegistry: TOPManyToManyRegistry;
      aCreateOptions: TSQLCreateOptions = []; aSchemaOptions: TSchemaCreateOptions = [scoForeignKeys, scoIndexes, scoJunctionTables]); virtual;
    // Statement that adds a column to a table
    Function GetAddColumnSQL(const aTable: UTF8String; aField: TOPFieldMap): UTF8String; virtual;
    // Statement to run after GetAddColumnSQL, empty if none. SQLite cannot add
    // a UNIQUE column: the column is added without it, and this creates a
    // unique index (named like a unique constraint) instead.
    Function GetAddUniqueIndexSQL(const aTable: UTF8String; aField: TOPFieldMap): UTF8String; virtual;
    // Statement that removes a column from a table
    Function GetDropColumnSQL(const aTable, aColumn: UTF8String): UTF8String; virtual;
    // Statement that changes the type of a column to match the field
    Function GetAlterColumnTypeSQL(const aTable, aColumn: UTF8String; aField: TOPFieldMap): UTF8String; virtual;
    // Statement to run after GetAlterColumnTypeSQL for an auto-increment key,
    // empty if none. PostgreSQL: a SERIAL sequence keeps its type when the
    // column becomes BIGINT, so it is changed too (PostgreSQL 10 and later).
    Function GetAlterAutoIncTypeSQL(const aTable, aColumn: UTF8String; aField: TOPFieldMap): UTF8String; virtual;
    // The name as the database stores it (apart from case): the reserved word
    // strategy applied, without quotes. Used to compare names with the database.
    Function PhysicalIdentifier(const aName: UTF8String; aType: TIdentifierType = itField): UTF8String; virtual;
    // aField is needed for MySQL and SQL Server, which redefine the whole column
    Function GetAlterColumnNullableSQL(const aTable, aColumn: UTF8String; aNullable: Boolean;
      aField: TOPFieldMap = Nil): UTF8String; virtual;
    // Statement that removes a table
    Function GetDropTableSQL(const aTable: UTF8String): UTF8String; virtual;
    // aTable is needed for MySQL and SQL Server
    Function GetDropIndexSQL(const aIndexName: UTF8String; const aTable: UTF8String = ''): UTF8String; virtual;
    // Statement that removes a foreign key constraint from a table
    Function GetDropForeignKeySQL(const aTable, aConstraintName: UTF8String): UTF8String; virtual;
    // Generate migration SQL from schema diff array
    Function GenerateMigrationSQL(const aDiffs: TOPSchemaDiffArray;
      aRegistry: TOPRegistry; aRelationRegistry: TOPRelationRegistry;
      aM2MRegistry: TOPManyToManyRegistry): TUTF8StringArray; virtual;
    // Schema introspection - reads actual database schema into an in-memory model
    Function IntrospectSchema(aConnection: TSQLConnection): TOPSchemaInfo; virtual;
    // The SQL driver for dialect-specific behavior. Auto-created from SQLDialect when not explicitly set.
    Property Driver : TSQLDriver Read GetDriver Write SetDriver;
  Published
    // Registry to look up classes
    Property Registry : TOPRegistry Read GetRegistry Write SetRegistry;
    // Options that control how statements are written
    Property Options : TSQLBuildOptions Read FOptions Write FOptions;
    // Quote character when quoting identifiers. Empty: use the driver's quoting.
    Property QuoteChar : String Read GetQuoteChar Write FQuoteChar stored IsQuoteCharStored;
    // Suffix for primary key definition
    Property PKSuffix : String Read FPKSuffix Write FPKSuffix;
    // Prefix for foreign key constraint names
    Property FKPrefix : String Read FFKPrefix Write FFKPrefix;
    // Prefix for unique constraint names
    Property UQPrefix : String Read FUQPrefix Write FUQPrefix;
    // If no size was given for a string field, take this size
    Property DefaultStringFieldLength : Integer Read FDefaultStringFieldLength Write FDefaultStringFieldLength default 255;
    // If a string size is larger than this, a memo field is used instead of a string field.
    Property MaxStringFieldLength : Integer Read FMaxStringFieldLength Write FMaxStringFieldLength default MaxStringLen;
    // The kind of database the statements are written for
    Property SQLDialect : TSQLDialect Read FSQLDialect Write SetSQLDialect;
  end;

Function OPDDLBuilder : TOPDDLBuilder;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Variants,
{$ELSE}
  Variants,
{$ENDIF}
  Opf.Consts, Opf.Logging, Opf.Transaction, Opf.FieldValue;

var
  SQLFormatSettings: TFormatSettings;

function OPDDLBuilder: TOPDDLBuilder;
begin
  Result:=TOPDDLBuilder.Instance;
end;

{ TOPDDLBuilder }

function TOPDDLBuilder.GetDriver: TSQLDriver;
begin
  if FDriver=Nil then
  begin
    FDriver:=TSQLDriver.CreateDriver(FSQLDialect);
    FDriverOwned:=True;
  end;
  Result:=FDriver;
end;

procedure TOPDDLBuilder.SetDriver(AValue: TSQLDriver);
begin
  if FDriver=AValue then Exit;
  if FDriverOwned then
    FreeAndNil(FDriver);
  FDriver:=AValue;
  FDriverOwned:=False;
  if Assigned(FDriver) then
    FSQLDialect:=FDriver.Dialect;
end;

procedure TOPDDLBuilder.SetSQLDialect(AValue: TSQLDialect);
begin
  if FSQLDialect=AValue then Exit;
  FSQLDialect:=AValue;
  if FDriverOwned then
    FreeAndNil(FDriver);
end;

function TOPDDLBuilder.KeyWord(aKeyWord: TSQLKeyword): UTF8String;

begin
  Result:=aKeyWord.ToString;
  if (sboKeywordsLowerCase in Options) then
    Result:=LowerCase(Result);
end;


function TOPDDLBuilder.GetUnsafeIdentifierPatterns: TUTF8StringArray;
begin
  Result:=TUTF8StringArray.Create(';','''','--','/*',#10,#13);
end;

function TOPDDLBuilder.MaybeQuoteIdentifier(const S: UTF8string; aType : TIdentifierType = itField; aForce : Boolean = False): UTF8String;
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

function TOPDDLBuilder.GetQuoteChar: String;
begin
  Result:=FQuoteChar;
  if Result='' then
    Result:=Driver.QuoteChar;
end;

function TOPDDLBuilder.IsQuoteCharStored: Boolean;
begin
  Result:=FQuoteChar<>'';
end;

function TOPDDLBuilder.AddSep(var S: UTF8string; aType: TSeparatorType): Boolean;

Const
  Seps : Array[TSeparatorType] of UTF8String = (', ','=','');
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

constructor TOPDDLBuilder.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FQuoteChar:=''; // Use the driver's quoting
  FPKSuffix:='_PK';
  FFKPrefix:='fk_';
  FUQPrefix:='uq_';
  FDefaultStringFieldLength:=255;
  FMaxStringFieldLength:=MaxStringLen;
end;

destructor TOPDDLBuilder.Destroy;
begin
  if FDriverOwned then
    FDriver.Free;
  inherited Destroy;
end;

function TOPDDLBuilder.GetRegistry: TOPRegistry;
begin
  Result := FRegistry;
  if Result = nil then
    Result := TOPRegistry.Instance;
end;

procedure TOPDDLBuilder.SetRegistry(AValue: TOPRegistry);

begin
  if FRegistry=AValue then Exit;
  if Assigned(FRegistry) then
    FRegistry.RemoveFreeNotification(Self);
  FRegistry:=AValue;
  if Assigned(FRegistry) then
    FRegistry.FreeNotification(Self);
end;

class destructor TOPDDLBuilder.cleanup;
begin
  ClearInstance;
end;

class function TOPDDLBuilder.GetExceptionClass: EOPFClass;
begin
  Result := EOPFDDL;
end;

class procedure TOPDDLBuilder.SetInstanceClass(aClass: TOPDDLBuilderClass);
begin
  if Assigned(_Instance) then
     _Instance.RaiseError('SetInstanceClass', SErrSQLBuilderCreated);
  _InstanceClass:=aClass;
end;

class function TOPDDLBuilder.Instance: TOPDDLBuilder;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
         begin
         if _InstanceClass=Nil then
            _InstanceClass:=TOPDDLBuilder;
         _Instance:=_InstanceClass.Create(Nil);
         end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPDDLBuilder.ClearInstance;
begin
  FreeAndNil(_Instance);
end;

function TOPDDLBuilder.IntTypeSQL: UTF8String;
begin
  Result:=TypeSQL(Driver.GetIntTypeSQL);
  if Result='' then
    Result:=KeyWord(kwINT);
end;

function TOPDDLBuilder.SmallIntTypeSQL: UTF8String;
begin
  Result:=TypeSQL(Driver.GetSmallIntTypeSQL);
  if Result='' then
    Result:=KeyWord(kwSmallInt);
end;

function TOPDDLBuilder.GetBooleanKeyword: TSQLKeyWord;
begin
  Result:=Driver.GetBooleanKeyword;
end;

function TOPDDLBuilder.GetBlobKeyword: TSQLKeyWord;
begin
  Result:=Driver.GetBlobKeyword;
end;

function TOPDDLBuilder.GetMemoKeyword: TSQLKeyWord;
begin
  Result:=Driver.GetMemoKeyword;
end;

function TOPDDLBuilder.GetMemoSuffix: UTF8String;
begin
  Result:=Driver.GetMemoSuffix;
end;

function TOPDDLBuilder.NeedsFloatOverDecimal: Boolean;
begin
  Result:=Driver.NeedsFloatOverDecimal;
end;

function TOPDDLBuilder.SupportsIfNotExists: Boolean;
begin
  Result:=Driver.SupportsIfNotExists;
end;

function TOPDDLBuilder.GetIntegerField(aField: TOPFieldMap; const aTable: String): UTF8String;

begin
  if aField.MaxSize<0 then
    RaiseError('GetIntegerField', SErrUnsupportedIntegerBound, [aTable,aField.FieldName,aField.MaxSize]);
  // Unsigned 32-bit values above High(Integer) need a 64-bit column, unless
  // the size (an Integer) limits the values
  if OPFIsUnsigned32(aField) and (aField.MaxSize=0) then
    Result:=TypeSQL(Driver.GetBigIntTypeSQL)
  else if (aField.MaxSize>maxSmallint) or (aField.MaxSize=0) then
    Result:=IntTypeSQL
  else
    Result:=SmallIntTypeSQL
end;

function TOPDDLBuilder.GetStringField(aField : TOPFieldMap;const aTable: UTF8string) : UTF8String;

Var
  L,N : Integer;

begin
  Result:='';
  L:=aField.MaxSize;
  N:=aField.MinSize;
  if L=0 then
    begin
    if aField.PropertyKind=tkSString then
      L:=255
    else if aField.PropertyKind in [tkChar,tkUChar,tkWChar] then
      begin
      L:=1;
      N:=1;
      end
    else
      L:=DefaultStringFieldLength;
    end;
  if L<=0 then
    RaiseError('GetStringField', SErrUnableToGetStringLength, [aTable,aField.FieldName]);
  if L>MaxStringFieldLength then
    Result:=MemoField
  else
    begin
    // The driver's own type (SQL Server: NVARCHAR with UnicodeStrings)
    Result:=TypeSQL(Driver.GetStringTypeSQL((N>0) and (N=L), L));
    if Result<>'' then
      Exit;
    if (N>0) and (N=L) then
      Result:=KeyWord(kwCHAR)
    else
      Result:=KeyWord(kwVarCHAR);
    Result:=Result+'('+IntToStr(L)+')';
    end;
end;


function TOPDDLBuilder.GetFloatField(aField: TOPFieldMap;const aTable: UTF8string): UTF8string;

var
  Kind: TOPFloatKind;

begin
  if aField.MaxSize<0 then
    RaiseError('GetFloatField', SErrUnsupportedFloatBound, [aTable, aField.FieldName, aField.MaxSize]);
  Kind:=OPFFloatKind(aField);
  // Date/time: a native column type unless numeric storage is requested
  if OPFIsNativeDateTime(aField) then
    Exit(TypeSQL(Driver.GetDateTimeTypeSQL(Kind)));
  // An explicit precision wins
  if (aField.MaxSize>0) and not NeedsFloatOverDecimal then
    begin
    Result:=KeyWord(kwDecimal);
    Result:=Result+Format('(%d,%d)',[aField.MaxSize,aField.minSize]);
    Exit;
    end;
  case Kind of
    fkCurrency:
      Result:=TypeSQL(Driver.GetCurrencyTypeSQL);
    fkSingle:
      Result:=TypeSQL(Driver.GetSingleTypeSQL);
    fkComp:
      Result:=TypeSQL(Driver.GetBigIntTypeSQL);
    fkDouble, fkExtended, fkDateTime, fkDate, fkTime:
      Result:=TypeSQL(Driver.GetDoubleTypeSQL);
  else
    // No type information: generic float
    Result:=KeyWord(kwFloat);
  end;
end;


function TOPDDLBuilder.GetBooleanField(aField: TOPFieldMap;const aTable: UTF8string): UTF8string;

begin
  if GetBooleanKeyword=kwSmallInt then
    Result:=SmallIntTypeSQL
  else
    Result:=KeyWord(GetBooleanKeyword);
end;

function TOPDDLBuilder.GetClassField(aField: TOPFieldMap; const aTable: UTF8string): UTF8string;


Var
  CT : TClass;
  MP : TOPFieldMapList;
  PD : TPersisterDef;
  aMap : TOPFieldMap;

begin
  CT:=GetTypeData(AField.PropertyTypeInfo)^.ClassType;
  if CT.InheritsFrom(TStrings) then
    Result:=MemoField
  else if CT.InheritsFrom(TStream) then
    Result:=BlobField
  else
    begin
    PD:=Registry.FindPersister(CT);
    if (PD=Nil) then
      RaiseError('GetClassField', SErrNoFieldsForClassRefPersister, [aField.PropertyName,CT.ClassName,aTable]);
    aMap:=PD.FieldMap.FindByPropertyName(aField.ReferencedPropertyName);
    if AMap=Nil then
      RaiseError('GetClassField', SErrNoFieldsForClassRefField, [aField.PropertyName,CT.ClassName,aTable,aField.ReferencedPropertyName]);
    Result:=GetPropertyDefSQL(PD.TableName,aMap);
    end;
end;

function TOPDDLBuilder.GetConstraintName(const aTableName, aObjectName: String; aType: TSQLConstraintType): UTF8String;
var
  N: UTF8String;
begin
  case aType of
    ctPrimaryKey : N:=aTableName+PKSuffix;
    ctForeignKey : N:=FKPrefix+aTableName+'_'+aObjectName;
    ctUnique     : N:=UQPrefix+aTableName+'_'+aObjectName;
  else
    RaiseError('GetConstraintName', SErrUnsupportedConstraintType, [Ord(aType)]);
  end;
  // Generated names are shortened to what the database accepts
  Result:=MaybeQuoteIdentifier(OPFShortenIdentifier(N, Driver.MaxIdentifierLength));
end;

function TOPDDLBuilder.CheckedName(const aName: UTF8String): UTF8String;
begin
  if (Driver.MaxIdentifierLength>0) and (Length(aName)>Driver.MaxIdentifierLength) then
    RaiseError('CheckedName', SErrIdentifierTooLong, [aName, Driver.MaxIdentifierLength, SQLDialect.ToString]);
  Result:=MaybeQuoteIdentifier(aName);
end;

function TOPDDLBuilder.BlobField: String;

begin
  Result:=TypeSQL(Driver.GetBlobTypeSQL);
  if Result='' then
    Result:=Keyword(GetBlobKeyword);
end;


function TOPDDLBuilder.TypeSQL(const aType: UTF8String): UTF8String;

begin
  Result:=aType;
  if (sboKeywordsLowerCase in Options) then
    Result:=LowerCase(Result);
end;


function TOPDDLBuilder.MemoField: String;

begin
  Result:=TypeSQL(Driver.GetMemoTypeSQL);
  if Result<>'' then
    Exit;
  Result:=Keyword(GetMemoKeyword);
  Result:=Result+GetMemoSuffix;
end;


function TOPDDLBuilder.GetPropertyDefSQL(const aTable: UTF8string; aField: TOPFieldMap): UTF8String;

begin
  SetLength(Result,0);
  Case aField.PropertyKind of
    tkInteger : Result:=GetIntegerField(aField,aTable);
    tkInt64,
    tkQWord : Result:=TypeSQL(Driver.GetBigIntTypeSQL);
    tkBool : Result:=GetBooleanField(aField,aTable);
    tkSString,
    tkChar,
    tkLString,
    tkAString,
    tkUChar,
    tkUString,
    tkWChar,
    tkWString: Result:=GetStringField(aField,aTable);
    tkFloat : Result:=GetFloatField(aField,aTable);
    tkSet:
      if foAsString in aField.Options then
        Result:=GetStringField(aField,aTable)
      else
        Result:=IntTypeSQL;
    tkEnumeration:
      if foAsString in aField.Options then
        Result:=GetStringField(aField,aTable)
      else
        Result:=SmallIntTypeSQL;
    tkVariant:
      Result:=MemoField;
    tkClass : Result:=GetClassField(aField,aTable);
  else
    RaiseError('GetPropertyDefSQL', SErrUnsupportedPropertyType, [GetEnumName(TypeInfo(TTypeKind),Ord(aField.PropertyKind))]);
  end;
  if (foRequired in aField.Options) then
    Result:=Result+' '+keyword(kwNot)+' '+keyword(kwNull);
  if (foUnique in aField.Options) then
    Result:=Result+' '+keyword(kwUnique);
  Result:=Result+GetDefaultClauseSQL(aField);
end;

function TOPDDLBuilder.GetDefaultClauseSQL(aField: TOPFieldMap): UTF8String;
begin
  Result:='';
  if not VarIsNull(aField.DefaultValue) and not VarIsEmpty(aField.DefaultValue) then
  begin
    Result:=' '+keyword(kwDefault)+' ';
    // Date fields stored natively get a date literal, also for a float default
    if (aField.PropertyKind=tkFloat) and OPFIsNativeDateTime(aField)
       and ((VarType(aField.DefaultValue) and varTypeMask) in [varDate, varSingle, varDouble, varCurrency]) then
      Result:=Result+Driver.GetDateTimeLiteral(OPFFloatKind(aField), VarToDateTime(aField.DefaultValue))
    else
    case VarType(aField.DefaultValue) and varTypeMask of
      varString, varOleStr, varUString:
        Result:=Result+Driver.QuoteStringLiteral(VarToStr(aField.DefaultValue));
      varDate:
        Result:=Result+Driver.GetDateTimeLiteral(fkDateTime, VarToDateTime(aField.DefaultValue));
      varBoolean:
        if GetBooleanKeyword = kwBoolean then
        begin
          if aField.DefaultValue then Result:=Result+KeyWord(kwTrue)
          else Result:=Result+KeyWord(kwFalse);
        end
        else
        begin
          if aField.DefaultValue then Result:=Result+'1'
          else Result:=Result+'0';
        end;
      varSingle, varDouble, varCurrency:
        Result:=Result+FloatToStr(Double(aField.DefaultValue), SQLFormatSettings);
    else
      Result:=Result+VarToStr(aField.DefaultValue);
    end;
  end;
end;

function TOPDDLBuilder.GetCreateSQL(const aTable: UTF8string; aMap: TOPFieldMapArray; aCreateOptions: TSQLCreateOptions
  ): UTF8string;
begin
  Result:=GetCreateSQL(aTable, aMap, '', aCreateOptions);
end;

function TOPDDLBuilder.GetCreateSQL(const aTable: UTF8string; aMap: TOPFieldMapArray;
  const aPKConstraintName: UTF8String; aCreateOptions: TSQLCreateOptions): UTF8string;

  Function MaybeNewLine : String;
  begin
    if (coForceNewLine in aCreateOptions) then
      Result:=sLineBreak+'  '
    else
      Result:='';
  end;

Var
  F : TOPFieldMap;
  PK,aDef,aFieldName,ConstraintStr : UTF8String;
  KeyCount : Integer;
  HasAutoIncKey : Boolean;

begin
  if aTable='' then
    RaiseError('GetCreateSQL', SErrEmptyTableName);
  if Length(aMap)=0 then
    RaiseError('GetCreateSQL', SErrDDLEmptyFieldMap);
  if (coIfNotExists in aCreateOptions) and not SupportsIfNotExists then
    RaiseError('GetCreateSQL', SErrIfNotExistsNotSupported, [SQLDialect.ToString]);
  // An auto-increment key is declared inline: it cannot be combined with other key fields
  KeyCount:=0;
  HasAutoIncKey:=False;
  for F in aMap do
    if foInKey in F.Options then
      begin
      Inc(KeyCount);
      if foAutoInc in F.Options then
        HasAutoIncKey:=True;
      end;
  if HasAutoIncKey and (KeyCount>1) then
    RaiseError('GetCreateSQL', SErrAutoIncCompositeKey, [aTable]);
  Result:='';
  PK:='';
  for F in aMap do
    begin
    aFieldName:=MaybeQuoteIdentifier(F.FieldName,itField,(coForceQuotes in aCreateOptions));
    if AddSep(Result,stComma) then
      Result:=Result+MaybeNewLine;
    if (foAutoInc in F.Options) and (foInKey in F.Options) then
      Result:=Result+Driver.GetAutoIncColumnSQL(aFieldName, OPFIsWideOrdinal(F))
    else
      begin
      aDef:=GetPropertyDefSQL(aTable,F);
      // Firebird requires primary key columns to be declared NOT NULL
      if (foInKey in F.Options) and not (foRequired in F.Options)
         and (FSQLDialect in [dFirebird, dFirebird3]) then
        aDef:=aDef+' '+KeyWord(kwNot)+' '+KeyWord(kwNull);
      Result:=Result+aFieldName+' '+aDef;
      if foInKey in F.Options then
        begin
        AddSep(PK,stComma);
        PK:=PK+aFieldName;
        end;
      end;
    end;
  if (PK<>'') then
  begin
    if aPKConstraintName<>'' then
      ConstraintStr:=CheckedName(aPKConstraintName)
    else
      ConstraintStr:=GetConstraintName(aTable,'',ctPrimaryKey);
    Result:=Result+', '+MaybeNewLine+KeyWord(kwConstraint)+' '+ConstraintStr+' '+keyword(kwPrimaryKey)+'('+PK+')';
  end;
  if coIfNotExists in aCreateOptions then
    Result:=keyword(kwCreate)+' '+keyword(kwTable)+' IF NOT EXISTS '+MaybeQuoteIdentifier(aTable,itTable,(coForceQuotes in aCreateOptions))+' ('+MaybeNewLine+Result+MaybeNewLine+')'
  else
    Result:=keyword(kwCreate)+' '+keyword(kwTable)+' '+MaybeQuoteIdentifier(aTable,itTable,(coForceQuotes in aCreateOptions))+' ('+MaybeNewLine+Result+MaybeNewLine+')';
end;

procedure TOPDDLBuilder.ExecDDL(aConnection: TSQLConnection; const aSQL: UTF8String);
begin
  RunDDL(aConnection, [aSQL], Nil, False);
end;

procedure TOPDDLBuilder.ExecDDLList(aConnection: TSQLConnection;
  const aStatements: TUTF8StringArray; aInTransaction: TOPDDLTransactionEvent);
var
  I: Integer;
begin
  if Driver.SupportsTransactionalDDL then
    RunDDL(aConnection, aStatements, aInTransaction, True)
  else
  begin
    for I:=0 to High(aStatements) do
      RunDDL(aConnection, [aStatements[I]], Nil, False);
    if Assigned(aInTransaction) then
      RunDDL(aConnection, [], aInTransaction, False);
  end;
end;

{ Run aStatements and aInTransaction in one transaction of our own, swapped
  onto the connection and always given back. aNumbered: name the failing
  statement in the error. }

procedure TOPDDLBuilder.RunDDL(aConnection: TSQLConnection; const aStatements: array of UTF8String;
  aInTransaction: TOPDDLTransactionEvent; aNumbered: Boolean);
var
  Query: TSQLQuery;
  Trans: TOPFTransaction;
  SavedTrans: TSQLTransaction;
  Msg: String;
  I, Current: Integer;
begin
  if aConnection = nil then
    RaiseError('ExecDDL', 'Connection is nil');
  if not aConnection.Connected then
    aConnection.Open;
  SavedTrans:=aConnection.Transaction;
  // sqldb cannot replace an active transaction of the connection. Then ours
  // runs beside it, where the database allows several transactions per
  // connection; elsewhere (SQLite, MySQL, ...) a new transaction would fail or
  // commit the caller's work.
  if Assigned(SavedTrans) and SavedTrans.Active
     and not (FSQLDialect in [dPostGres, dFirebird, dFirebird3]) then
    RaiseError('ExecDDL', 'The transaction of connection %s is active: %s cannot run DDL beside it. Commit or roll it back first.',
      [aConnection.Name, SQLDialect.ToString]);
  Trans:=TOPFTransaction.Create(nil);
  try
    Trans.Database:=aConnection;
    if not (Assigned(SavedTrans) and SavedTrans.Active) then
      aConnection.Transaction:=Trans;
    Current:=-1;
    try
      try
        Trans.StartTransaction;
        Query:=TSQLQuery.Create(nil);
        try
          Query.Database:=aConnection;
          Query.Transaction:=Trans;
          for I:=0 to High(aStatements) do
          begin
            Current:=I;
            Query.SQL.Text:=aStatements[I];
            Query.ExecSQL;
          end;
          Current:=-1;
        finally
          Query.Free;
        end;
        if Assigned(aInTransaction) then
          aInTransaction(aConnection, Trans);
        // Firebird reports many DDL errors only here
        Trans.Commit;
      except
        on E: Exception do
        begin
          Msg:=E.Message;
          if aNumbered and (Current>=0) then
            Msg:=Format(SErrDDLStatementFailed, [Current+1, Length(aStatements), aStatements[Current], Msg]);
          if Trans.Active then
            try
              Trans.Rollback;
            except
              // Keep the DDL error, report the rollback failure with it
              on RE: Exception do
              begin
                TOPLogger(GetLogger).LogWarning(Self, 'ExecDDL: '+SErrRollbackFailed, [RE.Message]);
                Msg:=Msg+LineEnding+Format(SErrRollbackFailed, [RE.Message]);
              end;
            end;
          // A failed rollback leaves it active: it could not be detached or freed
          if Trans.Active then
            Trans.ForceInactive;
          if E is EDatabaseError then
            RaiseError('ExecDDL', Msg)
          else
            raise;
        end;
      end;
    finally
      // Always give the connection its own transaction back
      if aConnection.Transaction=Trans then
        aConnection.Transaction:=SavedTrans;
    end;
  finally
    Trans.Free;
  end;
end;

procedure TOPDDLBuilder.CreateTable(aConnection: TSQLConnection; const aTable: UTF8String;
  aMap: TOPFieldMapArray; aCreateOptions: TSQLCreateOptions);
begin
  ExecDDL(aConnection, GetCreateSQL(aTable, aMap, aCreateOptions));
end;

function TOPDDLBuilder.GetAddForeignKeySQL(const aChildTable, aFKFieldName,
  aParentTable, aPKFieldName: UTF8String; aDBCascade: Boolean;
  const aConstraintName: UTF8String; aCreateOptions: TSQLCreateOptions): UTF8String;
var
  CN: UTF8String;
  Q: Boolean;
begin
  Q:=coForceQuotes in aCreateOptions;
  if aConstraintName<>'' then
    CN:=CheckedName(aConstraintName)
  else
    CN:=GetConstraintName(aChildTable, aParentTable, ctForeignKey);
  Result:=KeyWord(kwAlter)+' '+KeyWord(kwTable)+' '+MaybeQuoteIdentifier(aChildTable, itTable, Q)+
    ' '+KeyWord(kwAdd)+' '+KeyWord(kwConstraint)+' '+CN+
    ' '+KeyWord(kwForeign)+' '+KeyWord(kwKey)+
    ' ('+MaybeQuoteIdentifier(aFKFieldName, itField, Q)+')'+
    ' '+KeyWord(kwReferences)+' '+MaybeQuoteIdentifier(aParentTable, itTable, Q)+
    '('+MaybeQuoteIdentifier(aPKFieldName, itField, Q)+')';
  if aDBCascade then
    Result:=Result+' '+KeyWord(kwOn)+' '+KeyWord(kwDelete)+' '+KeyWord(kwCascade);
  // PostgreSQL has no ADD CONSTRAINT IF NOT EXISTS: check the catalog first
  if (coIfNotExists in aCreateOptions) and (FSQLDialect=dPostGres) then
    Result:=PGIfNoConstraint(MaybeQuoteIdentifier(aChildTable, itTable, Q), CN, Result);
end;

{ Wrap aStatement so it only runs when table aTable (as written in SQL) has no
  constraint aConstraint (as written in SQL). PostgreSQL only.
  No colon in the statement: sqldb would take it for a parameter. }

function TOPDDLBuilder.PGIfNoConstraint(const aTable, aConstraint, aStatement: UTF8String): UTF8String;
var
  CName: UTF8String;
begin
  // The catalog stores unquoted names in lower case, quoted ones as written
  if (Length(aConstraint)>1) and (aConstraint[1]='"') then
    CName:=StringReplace(Copy(aConstraint, 2, Length(aConstraint)-2), '""', '"', [rfReplaceAll])
  else
    CName:=LowerCase(aConstraint);
  Result:='DO $opf$BEGIN IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = '+
    Driver.QuoteStringLiteral(CName)+' AND conrelid = to_regclass('+Driver.QuoteStringLiteral(aTable)+')) THEN '+
    aStatement+'; END IF; END$opf$';
end;

function TOPDDLBuilder.GetForeignKeySQL(aRelation: TOPRelationDef; aRegistry: TOPRegistry;
  aCreateOptions: TSQLCreateOptions): UTF8String;
var
  ChildDef, ParentDef: TPersisterDef;
  PKField: TOPFieldMap;
begin
  ChildDef:=aRegistry.FindPersister(aRelation.ChildClass);
  if ChildDef=nil then
    RaiseError('CreateForeignKey', SErrFKChildNotRegistered, [aRelation.ChildClass.ClassName]);
  ParentDef:=aRegistry.FindPersister(aRelation.ParentClass);
  if ParentDef=nil then
    RaiseError('CreateForeignKey', SErrFKParentNotRegistered, [aRelation.ParentClass.ClassName]);
  PKField:=ParentDef.FieldMap.FindPrimaryKeyField;
  if PKField=nil then
    RaiseError('CreateForeignKey', SErrFKParentNoPrimaryKey, [aRelation.ParentClass.ClassName]);
  Result:=GetAddForeignKeySQL(
    ChildDef.TableName,
    aRelation.ForeignKeyFieldName,
    ParentDef.TableName,
    PKField.FieldName,
    aRelation.DBCascade,
    aRelation.ConstraintName,
    aCreateOptions);
end;

function TOPDDLBuilder.GetInlineForeignKeySQL(aRelation: TOPRelationDef; aRegistry: TOPRegistry;
  aCreateOptions: TSQLCreateOptions): UTF8String;
var
  ChildDef: TPersisterDef;
  Prefix: UTF8String;
begin
  // Part of CREATE TABLE: no existence check
  Result:=GetForeignKeySQL(aRelation, aRegistry, aCreateOptions-[coIfNotExists]);
  ChildDef:=aRegistry.FindPersister(aRelation.ChildClass);
  Prefix:=KeyWord(kwAlter)+' '+KeyWord(kwTable)+' '+
    MaybeQuoteIdentifier(ChildDef.TableName, itTable, coForceQuotes in aCreateOptions)+' '+KeyWord(kwAdd)+' ';
  if Copy(Result, 1, Length(Prefix))=Prefix then
    Result:=Copy(Result, Length(Prefix)+1, Length(Result))
  else
    Result:='';
end;

procedure TOPDDLBuilder.CreateForeignKey(aConnection: TSQLConnection;
  aRelation: TOPRelationDef; aRegistry: TOPRegistry; aCreateOptions: TSQLCreateOptions);
begin
  ExecDDL(aConnection, GetForeignKeySQL(aRelation, aRegistry, aCreateOptions));
end;

function TOPDDLBuilder.GetCreateJunctionTableSQL(aDef: TOPManyToManyDef;
  aRegistry: TOPRegistry; aCreateOptions: TSQLCreateOptions): UTF8String;
var
  DefA, DefB: TPersisterDef;
  PKFieldA, PKFieldB: TOPFieldMap;
  TypeA, TypeB, FKA, FKB, TblName, ConstraintStr: UTF8String;
  NotNull: UTF8String;
  ParentTableA, ParentTableB, PKNameA, PKNameB: UTF8String;
  Q: Boolean;
begin
  Q:=coForceQuotes in aCreateOptions;
  DefA:=aRegistry.FindPersister(aDef.ClassA);
  if DefA=nil then
    RaiseError('GetCreateJunctionTableSQL', SErrJunctionClassANotRegistered, [aDef.ClassA.ClassName]);
  DefB:=aRegistry.FindPersister(aDef.ClassB);
  if DefB=nil then
    RaiseError('GetCreateJunctionTableSQL', SErrJunctionClassBNotRegistered, [aDef.ClassB.ClassName]);
  PKFieldA:=DefA.FieldMap.FindPrimaryKeyField;
  if PKFieldA=nil then
    RaiseError('GetCreateJunctionTableSQL', SErrJunctionClassANoPrimaryKey, [aDef.ClassA.ClassName]);
  PKFieldB:=DefB.FieldMap.FindPrimaryKeyField;
  if PKFieldB=nil then
    RaiseError('GetCreateJunctionTableSQL', SErrJunctionClassBNoPrimaryKey, [aDef.ClassB.ClassName]);

  TblName:=aDef.JunctionTable;
  FKA:=MaybeQuoteIdentifier(aDef.ForeignKeyA, itField, Q);
  FKB:=MaybeQuoteIdentifier(aDef.ForeignKeyB, itField, Q);
  TypeA:=GetPropertyDefSQL(TblName, PKFieldA);
  TypeB:=GetPropertyDefSQL(TblName, PKFieldB);
  // Strip trailing constraints from type strings - we add our own NOT NULL unconditionally.
  // GetPropertyDefSQL may append NOT NULL, UNIQUE, and DEFAULT clauses that are not
  // appropriate for junction table FK columns. Extract only the base type by finding
  // the first appended constraint keyword.
  NotNull:=' '+KeyWord(kwNot)+' '+KeyWord(kwNull);
  if Pos(NotNull, TypeA) > 0 then
    TypeA:=Copy(TypeA, 1, Pos(NotNull, TypeA)-1);
  if Pos(NotNull, TypeB) > 0 then
    TypeB:=Copy(TypeB, 1, Pos(NotNull, TypeB)-1);
  if Pos(' '+KeyWord(kwUnique), TypeA) > 0 then
    TypeA:=Copy(TypeA, 1, Pos(' '+KeyWord(kwUnique), TypeA)-1);
  if Pos(' '+KeyWord(kwUnique), TypeB) > 0 then
    TypeB:=Copy(TypeB, 1, Pos(' '+KeyWord(kwUnique), TypeB)-1);
  if Pos(' '+KeyWord(kwDefault), TypeA) > 0 then
    TypeA:=Copy(TypeA, 1, Pos(' '+KeyWord(kwDefault), TypeA)-1);
  if Pos(' '+KeyWord(kwDefault), TypeB) > 0 then
    TypeB:=Copy(TypeB, 1, Pos(' '+KeyWord(kwDefault), TypeB)-1);
  ParentTableA:=DefA.TableName;
  ParentTableB:=DefB.TableName;
  PKNameA:=PKFieldA.FieldName;
  PKNameB:=PKFieldB.FieldName;

  if (coIfNotExists in aCreateOptions) and not SupportsIfNotExists then
    RaiseError('GetCreateJunctionTableSQL', SErrIfNotExistsNotSupported, [SQLDialect.ToString]);

  // Build column definitions
  Result:=FKA+' '+TypeA+NotNull+', '+
          FKB+' '+TypeB+NotNull;

  // Add composite primary key constraint
  ConstraintStr:=GetConstraintName(TblName, '', ctPrimaryKey);
  Result:=Result+', '+KeyWord(kwConstraint)+' '+ConstraintStr+' '+KeyWord(kwPrimaryKey)+
    '('+FKA+', '+FKB+')';

  // Add FK constraints when DBCascade is True
  if aDef.DBCascade then
  begin
    Result:=Result+', '+KeyWord(kwForeign)+' '+KeyWord(kwKey)+' ('+FKA+') '+
      KeyWord(kwReferences)+' '+MaybeQuoteIdentifier(ParentTableA, itTable, Q)+
      '('+MaybeQuoteIdentifier(PKNameA, itField, Q)+') '+KeyWord(kwOn)+' '+KeyWord(kwDelete)+' '+KeyWord(kwCascade);
    Result:=Result+', '+KeyWord(kwForeign)+' '+KeyWord(kwKey)+' ('+FKB+') '+
      KeyWord(kwReferences)+' '+MaybeQuoteIdentifier(ParentTableB, itTable, Q)+
      '('+MaybeQuoteIdentifier(PKNameB, itField, Q)+') '+KeyWord(kwOn)+' '+KeyWord(kwDelete)+' '+KeyWord(kwCascade);
  end;

  // Wrap in CREATE TABLE
  if coIfNotExists in aCreateOptions then
    Result:=KeyWord(kwCreate)+' '+KeyWord(kwTable)+' IF NOT EXISTS '+
      MaybeQuoteIdentifier(TblName, itTable, (coForceQuotes in aCreateOptions))+' ('+Result+')'
  else
    Result:=KeyWord(kwCreate)+' '+KeyWord(kwTable)+' '+
      MaybeQuoteIdentifier(TblName, itTable, (coForceQuotes in aCreateOptions))+' ('+Result+')';
end;

procedure TOPDDLBuilder.CreateJunctionTable(aConnection: TSQLConnection;
  aDef: TOPManyToManyDef; aRegistry: TOPRegistry; aCreateOptions: TSQLCreateOptions);
begin
  ExecDDL(aConnection, GetCreateJunctionTableSQL(aDef, aRegistry, aCreateOptions));
end;

function TOPDDLBuilder.GetCreateIndexSQL(const aTable: UTF8String; aIndex: TOPIndexDef; aCreateOptions: TSQLCreateOptions): UTF8String;
var
  Fields, FN: UTF8String;
  Parts: specialize TArray<String>;
  I: Integer;
  Q: Boolean;
begin
  Q:=coForceQuotes in aCreateOptions;
  Parts:=String(aIndex.FieldNames).Split([',']);
  Fields:='';
  for I:=0 to High(Parts) do
  begin
    FN:=Trim(Parts[I]);
    if FN='' then Continue;
    if Fields<>'' then
      Fields:=Fields+', ';
    Fields:=Fields+MaybeQuoteIdentifier(FN, itField, Q);
  end;
  // Only SQLite and PostgreSQL have CREATE INDEX IF NOT EXISTS: elsewhere
  // coIfNotExists does not apply to indexes
  if (coIfNotExists in aCreateOptions) and (FSQLDialect in [dSQLite, dPostGres]) then
    Result:='CREATE INDEX IF NOT EXISTS '
  else
    Result:='CREATE INDEX ';
  Result:=Result+CheckedName(aIndex.IndexName)+' ON '+MaybeQuoteIdentifier(aTable, itTable, Q)+' ('+Fields+')';
end;

procedure TOPDDLBuilder.CreateIndexes(aConnection: TSQLConnection; const aTable: UTF8String;
  aIndexes: TOPIndexDefArray; aCreateOptions: TSQLCreateOptions);
var
  I: Integer;
begin
  for I:=0 to High(aIndexes) do
    ExecDDL(aConnection, GetCreateIndexSQL(aTable, aIndexes[I], aCreateOptions));
end;

function TOPDDLBuilder.GetCreationOrder(aRegistry: TOPRegistry;
  aRelationRegistry: TOPRelationRegistry): TClassArray;
begin
  Result:=GetCreationOrder(aRegistry, aRelationRegistry, False);
end;

function TOPDDLBuilder.GetCreationOrder(aRegistry: TOPRegistry;
  aRelationRegistry: TOPRelationRegistry; aAllowCycles: Boolean): TClassArray;
var
  I, J, Cnt, QHead, QTail: Integer;
  Done: Array of Boolean;
  InDegree: Array of Integer;
  AdjList: Array of Array of Integer;
  Queue: Array of Integer;
  Rel: TOPRelationDef;
  ParentIdx, ChildIdx: Integer;

  function IndexOfClass(aClass: TClass): Integer;
  var K: Integer;
  begin
    for K:=0 to aRegistry.PersisterCount-1 do
      if aRegistry.Persisters[K].InstanceClass=aClass then
        Exit(K);
    Result:=-1;
  end;

begin
  Cnt:=aRegistry.PersisterCount;
  SetLength(Result, Cnt);
  if Cnt=0 then Exit;

  SetLength(InDegree, Cnt);
  SetLength(AdjList, Cnt);
  SetLength(Queue, Cnt);
  SetLength(Done, Cnt);
  for I:=0 to Cnt-1 do
  begin
    InDegree[I]:=0;
    Done[I]:=False;
    SetLength(AdjList[I], 0);
  end;

  // Build adjacency list from relations (skip self-referencing)
  for I:=0 to aRelationRegistry.RelationCount-1 do
  begin
    Rel:=aRelationRegistry.Relations[I];
    if Rel.ParentClass=Rel.ChildClass then Continue;
    ParentIdx:=IndexOfClass(Rel.ParentClass);
    ChildIdx:=IndexOfClass(Rel.ChildClass);
    if (ParentIdx<0) or (ChildIdx<0) then Continue;
    // Add edge: parent → child (child depends on parent)
    J:=Length(AdjList[ParentIdx]);
    SetLength(AdjList[ParentIdx], J+1);
    AdjList[ParentIdx][J]:=ChildIdx;
    InDegree[ChildIdx]:=InDegree[ChildIdx]+1;
  end;

  // Seed queue with zero in-degree classes (in registration order)
  QHead:=0;
  QTail:=0;
  for I:=0 to Cnt-1 do
    if InDegree[I]=0 then
    begin
      Queue[QTail]:=I;
      Inc(QTail);
    end;

  // Process queue (Kahn's algorithm)
  J:=0;
  while QHead<QTail do
  begin
    I:=Queue[QHead];
    Inc(QHead);
    Result[J]:=aRegistry.Persisters[I].InstanceClass;
    Done[I]:=True;
    Inc(J);
    for ParentIdx:=0 to High(AdjList[I]) do
    begin
      ChildIdx:=AdjList[I][ParentIdx];
      Dec(InDegree[ChildIdx]);
      if InDegree[ChildIdx]=0 then
      begin
        Queue[QTail]:=ChildIdx;
        Inc(QTail);
      end;
    end;
  end;

  if (J<Cnt) and aAllowCycles then
    for I:=0 to Cnt-1 do
      if not Done[I] then
      begin
        Result[J]:=aRegistry.Persisters[I].InstanceClass;
        Inc(J);
      end;
  if J<Cnt then
  begin
    SetLength(Result, 0);
    RaiseError('GetCreationOrder', SErrCircularDependency, []);
  end;

  SetLength(Result, J);
end;

procedure TOPDDLBuilder.CreateSchema(aConnection: TSQLConnection;
  aRegistry: TOPRegistry; aRelationRegistry: TOPRelationRegistry;
  aM2MRegistry: TOPManyToManyRegistry; aCreateOptions: TSQLCreateOptions;
  aSchemaOptions: TSchemaCreateOptions);
begin
  ExecDDLList(aConnection, GetCreateSchemaSQL(aRegistry, aRelationRegistry, aM2MRegistry,
    aCreateOptions, aSchemaOptions));
end;

function TOPDDLBuilder.GetCreateSchemaSQL(aRegistry: TOPRegistry;
  aRelationRegistry: TOPRelationRegistry; aM2MRegistry: TOPManyToManyRegistry;
  aCreateOptions: TSQLCreateOptions; aSchemaOptions: TSchemaCreateOptions): TUTF8StringArray;
var
  Order: TClassArray;
  I, J: Integer;
  Def: TPersisterDef;
  S, FK: UTF8String;
  InlineFK: Boolean;
  Rel: TOPRelationDef;

  procedure Add(const aSQL: UTF8String);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)]:=aSQL;
  end;

begin
  Result:=Nil;
  // Step 1: Get dependency-ordered class list. Foreign keys are added after
  // all tables, so classes that reference each other are no problem.
  Order:=GetCreationOrder(aRegistry, aRelationRegistry, True);
  // SQLite cannot add a foreign key to an existing table: the foreign keys
  // are part of CREATE TABLE (a referenced table need not exist yet)
  InlineFK:=(FSQLDialect=dSQLite) and (scoForeignKeys in aSchemaOptions);

  // Step 2: Create entity tables in order
  for I:=0 to High(Order) do
  begin
    Def:=aRegistry.FindPersister(Order[I]);
    S:=GetCreateSQL(Def.TableName, Def.FieldMap.AsArray, Def.PKConstraintName, aCreateOptions);
    if InlineFK then
      for J:=0 to aRelationRegistry.RelationCount-1 do
      begin
        Rel:=aRelationRegistry.Relations[J];
        if Rel.ChildClass<>Order[I] then
          Continue;
        FK:=GetInlineForeignKeySQL(Rel, aRegistry, aCreateOptions);
        if FK='' then
          RaiseError('GetCreateSchemaSQL', 'Cannot declare the foreign key %s of table %s in CREATE TABLE',
            [Rel.ForeignKeyFieldName, Def.TableName]);
        // GetCreateSQL ends with the closing parenthesis
        S:=Copy(S, 1, Length(S)-1)+', '+FK+')';
      end;
    Add(S);
  end;

  // Step 3: Create indexes
  if scoIndexes in aSchemaOptions then
    for I:=0 to High(Order) do
    begin
      Def:=aRegistry.FindPersister(Order[I]);
      for J:=0 to High(Def.Indexes) do
        Add(GetCreateIndexSQL(Def.TableName, Def.Indexes[J], aCreateOptions));
    end;

  // Step 4: Create foreign keys
  if (scoForeignKeys in aSchemaOptions) and not InlineFK then
    for I:=0 to aRelationRegistry.RelationCount-1 do
      Add(GetForeignKeySQL(aRelationRegistry.Relations[I], aRegistry, aCreateOptions));

  // Step 5: Create junction tables
  if scoJunctionTables in aSchemaOptions then
    for I:=0 to aM2MRegistry.DefCount-1 do
      Add(GetCreateJunctionTableSQL(aM2MRegistry.Defs[I], aRegistry, aCreateOptions));
end;

function TOPDDLBuilder.AlterTablePrefix(const aTable: UTF8String): UTF8String;
begin
  Result:=KeyWord(kwAlter)+' '+KeyWord(kwTable)+' '+MaybeQuoteIdentifier(aTable, itTable)+' ';
end;

function TOPDDLBuilder.NullClause(aNullable: Boolean): UTF8String;
begin
  if aNullable then
    Result:=KeyWord(kwNull)
  else
    Result:=KeyWord(kwNot)+' '+KeyWord(kwNull);
end;

{ The column must be NOT NULL: required, or part of the primary key }

function IsNotNullField(aField: TOPFieldMap): Boolean;
begin
  Result:=([foRequired, foInKey, foAutoInc]*aField.Options)<>[];
end;

function TOPDDLBuilder.GetAddColumnSQL(const aTable: UTF8String; aField: TOPFieldMap): UTF8String;
var
  ColDef: UTF8String;
begin
  ColDef:=MaybeQuoteIdentifier(aField.FieldName)+' '+GetPropertyDefSQL(aTable, aField);
  // SQLite cannot add a UNIQUE column: GetAddUniqueIndexSQL adds the constraint
  if (FSQLDialect=dSQLite) and (foUnique in aField.Options) then
    ColDef:=StringReplace(ColDef, ' '+KeyWord(kwUnique), '', []);
  case FSQLDialect of
    // ADD without COLUMN
    dFirebird, dFirebird3, dSQLServer:
      Result:=AlterTablePrefix(aTable)+KeyWord(kwAdd)+' '+ColDef;
    dOracle:
      Result:=AlterTablePrefix(aTable)+KeyWord(kwAdd)+' ('+ColDef+')';
  else
    Result:=AlterTablePrefix(aTable)+KeyWord(kwAdd)+' '+KeyWord(kwColumn)+' '+ColDef;
  end;
end;

function TOPDDLBuilder.GetAddUniqueIndexSQL(const aTable: UTF8String; aField: TOPFieldMap): UTF8String;
begin
  Result:='';
  if (FSQLDialect<>dSQLite) or not (foUnique in aField.Options) then
    Exit;
  Result:=KeyWord(kwCreate)+' '+KeyWord(kwUnique)+' '+KeyWord(kwIndex)+' '+
    GetConstraintName(aTable, aField.FieldName, ctUnique)+' '+KeyWord(kwOn)+' '+
    MaybeQuoteIdentifier(aTable, itTable)+' ('+MaybeQuoteIdentifier(aField.FieldName)+')';
end;

function TOPDDLBuilder.GetDropColumnSQL(const aTable, aColumn: UTF8String): UTF8String;
begin
  if FSQLDialect in [dFirebird, dFirebird3] then
    Result:=AlterTablePrefix(aTable)+KeyWord(kwDrop)+' '+MaybeQuoteIdentifier(aColumn)
  else
    Result:=AlterTablePrefix(aTable)+KeyWord(kwDrop)+' '+KeyWord(kwColumn)+' '+MaybeQuoteIdentifier(aColumn);
end;

function TOPDDLBuilder.GetMySQLColumnRedefinition(const aTable: UTF8String; aField: TOPFieldMap;
  aNullable: Boolean): UTF8String;
begin
  // UNIQUE is not repeated: it would add a second unique index
  Result:=ExtractBaseType(GetPropertyDefSQL(aTable, aField))+' '+NullClause(aNullable);
  if ([foAutoInc, foInKey]*aField.Options)=[foAutoInc, foInKey] then
    // The column stays the primary key: only AUTO_INCREMENT is repeated
    Result:=Result+' AUTO_INCREMENT'
  else
    Result:=Result+GetDefaultClauseSQL(aField);
end;

function TOPDDLBuilder.GetAlterColumnTypeSQL(const aTable, aColumn: UTF8String; aField: TOPFieldMap): UTF8String;
var
  BaseType, Col: UTF8String;
begin
  if FSQLDialect=dSQLite then
    RaiseError('GetAlterColumnTypeSQL', SErrAlterColumnNotSupported, [FSQLDialect.ToString]);
  BaseType:=ExtractBaseType(GetPropertyDefSQL(aTable, aField));
  Col:=MaybeQuoteIdentifier(aColumn);
  case FSQLDialect of
    // These redefine the whole column: repeat the null constraint, or it is lost
    dMySQL:
      Result:=AlterTablePrefix(aTable)+'MODIFY '+KeyWord(kwColumn)+' '+Col+' '+
        GetMySQLColumnRedefinition(aTable, aField, not IsNotNullField(aField));
    dSQLServer:
      Result:=AlterTablePrefix(aTable)+KeyWord(kwAlter)+' '+KeyWord(kwColumn)+' '+Col+' '+BaseType+' '+
        NullClause(not IsNotNullField(aField));
    dOracle:
      Result:=AlterTablePrefix(aTable)+'MODIFY ('+Col+' '+BaseType+')';
  else
    Result:=AlterTablePrefix(aTable)+KeyWord(kwAlter)+' '+KeyWord(kwColumn)+' '+Col+' TYPE '+BaseType;
  end;
end;

function TOPDDLBuilder.GetAlterAutoIncTypeSQL(const aTable, aColumn: UTF8String; aField: TOPFieldMap): UTF8String;
var
  Col, ColName: UTF8String;
begin
  Result:='';
  if (FSQLDialect<>dPostGres) or (([foAutoInc, foInKey]*aField.Options)<>[foAutoInc, foInKey])
     or not OPFIsWideOrdinal(aField) then
    Exit;
  // pg_get_serial_sequence parses the table name like SQL does, but takes
  // the column name as stored: lower case unless it was quoted
  Col:=MaybeQuoteIdentifier(aColumn);
  ColName:=PhysicalIdentifier(aColumn);
  if Col=ColName then
    ColName:=LowerCase(ColName);
  // Sequences have a type since PostgreSQL 10; before, they were all 64-bit.
  // No colon in the statement: sqldb would take it for a parameter.
  Result:='DO $opf$DECLARE s text; BEGIN SELECT pg_get_serial_sequence('+
    Driver.QuoteStringLiteral(MaybeQuoteIdentifier(aTable, itTable))+', '+
    Driver.QuoteStringLiteral(ColName)+') INTO s; '+
    'IF s IS NOT NULL AND CAST(current_setting(''server_version_num'') AS integer)>=100000 THEN '+
    'EXECUTE ''ALTER SEQUENCE ''||s||'' AS BIGINT''; END IF; END$opf$';
end;

function TOPDDLBuilder.PhysicalIdentifier(const aName: UTF8String; aType: TIdentifierType): UTF8String;
var
  QE: Char;
begin
  Result:=MaybeQuoteIdentifier(aName, aType);
  if (Result=aName) or (Length(Result)<2) then
    Exit;
  case Result[1] of
    '[': QE:=']';
    '"', '`': QE:=Result[1];
  else
    Exit;
  end;
  if Result[Length(Result)]<>QE then
    Exit;
  Result:=StringReplace(Copy(Result, 2, Length(Result)-2), QE+QE, QE, [rfReplaceAll]);
end;

function TOPDDLBuilder.GetAlterColumnNullableSQL(const aTable, aColumn: UTF8String; aNullable: Boolean;
  aField: TOPFieldMap): UTF8String;
var
  Col: UTF8String;
begin
  // SQLite cannot alter a column; Firebird before 3.0 has no SET/DROP NOT NULL
  if FSQLDialect in [dSQLite, dFirebird] then
    RaiseError('GetAlterColumnNullableSQL', SErrAlterColumnNotSupported, [FSQLDialect.ToString]);
  // MySQL and SQL Server need the column type to change the null constraint
  if (FSQLDialect in [dMySQL, dSQLServer]) and (aField=Nil) then
    RaiseError('GetAlterColumnNullableSQL', SErrAlterColumnNotSupported, [FSQLDialect.ToString]);
  Col:=MaybeQuoteIdentifier(aColumn);
  case FSQLDialect of
    dMySQL:
      Result:=AlterTablePrefix(aTable)+'MODIFY '+KeyWord(kwColumn)+' '+Col+' '+
        GetMySQLColumnRedefinition(aTable, aField, aNullable);
    dSQLServer:
      Result:=AlterTablePrefix(aTable)+KeyWord(kwAlter)+' '+KeyWord(kwColumn)+' '+Col+' '+
        ExtractBaseType(GetPropertyDefSQL(aTable, aField))+' '+NullClause(aNullable);
    dOracle:
      Result:=AlterTablePrefix(aTable)+'MODIFY ('+Col+' '+NullClause(aNullable)+')';
  else
    if aNullable then
      Result:=AlterTablePrefix(aTable)+KeyWord(kwAlter)+' '+KeyWord(kwColumn)+' '+Col+
        ' '+KeyWord(kwDrop)+' '+KeyWord(kwNot)+' '+KeyWord(kwNull)
    else
      Result:=AlterTablePrefix(aTable)+KeyWord(kwAlter)+' '+KeyWord(kwColumn)+' '+Col+
        ' SET '+KeyWord(kwNot)+' '+KeyWord(kwNull);
  end;
end;

function TOPDDLBuilder.GetDropTableSQL(const aTable: UTF8String): UTF8String;
begin
  Result:=KeyWord(kwDrop)+' '+KeyWord(kwTable)+' '+MaybeQuoteIdentifier(aTable, itTable);
end;

function TOPDDLBuilder.GetDropIndexSQL(const aIndexName: UTF8String; const aTable: UTF8String): UTF8String;
begin
  Result:=KeyWord(kwDrop)+' '+KeyWord(kwIndex)+' '+MaybeQuoteIdentifier(aIndexName);
  // Index names are per table in MySQL and SQL Server
  if FSQLDialect in [dMySQL, dSQLServer] then
  begin
    if aTable='' then
      RaiseError('GetDropIndexSQL', SErrDropIndexNeedsTable, [aIndexName, FSQLDialect.ToString]);
    Result:=Result+' '+KeyWord(kwOn)+' '+MaybeQuoteIdentifier(aTable, itTable);
  end;
end;

function TOPDDLBuilder.GetDropForeignKeySQL(const aTable, aConstraintName: UTF8String): UTF8String;
begin
  if FSQLDialect=dMySQL then
    Result:=KeyWord(kwAlter)+' '+KeyWord(kwTable)+' '+MaybeQuoteIdentifier(aTable, itTable)+
      ' '+KeyWord(kwDrop)+' '+KeyWord(kwForeign)+' '+KeyWord(kwKey)+' '+MaybeQuoteIdentifier(aConstraintName)
  else
    Result:=KeyWord(kwAlter)+' '+KeyWord(kwTable)+' '+MaybeQuoteIdentifier(aTable, itTable)+
      ' '+KeyWord(kwDrop)+' '+KeyWord(kwConstraint)+' '+MaybeQuoteIdentifier(aConstraintName);
end;

function TOPDDLBuilder.GenerateMigrationSQL(const aDiffs: TOPSchemaDiffArray;
  aRegistry: TOPRegistry; aRelationRegistry: TOPRelationRegistry;
  aM2MRegistry: TOPManyToManyRegistry): TUTF8StringArray;

  function FindPersisterByTable(const aTableName: UTF8String): TPersisterDef;
  var
    K: Integer;
  begin
    for K:=0 to aRegistry.PersisterCount-1 do
      if SameText(aRegistry.Persisters[K].TableName, aTableName) then
        Exit(aRegistry.Persisters[K]);
    Result:=nil;
  end;

  function FindIndexInPersister(aPD: TPersisterDef; const aIndexName: UTF8String): Integer;
  var
    K: Integer;
  begin
    for K:=0 to High(aPD.Indexes) do
      if SameText(aPD.Indexes[K].IndexName, aIndexName) then
        Exit(K);
    Result:=-1;
  end;

  function FindRelationByFK(const aChildTable, aFKColumn: UTF8String): TOPRelationDef;
  var
    K: Integer;
    ChildPD: TPersisterDef;
  begin
    for K:=0 to aRelationRegistry.RelationCount-1 do
    begin
      ChildPD:=aRegistry.FindPersister(aRelationRegistry.Relations[K].ChildClass);
      if (ChildPD<>nil) and SameText(ChildPD.TableName, aChildTable) and
         SameText(aRelationRegistry.Relations[K].ForeignKeyFieldName, aFKColumn) then
        Exit(aRelationRegistry.Relations[K]);
    end;
    Result:=nil;
  end;

  function FindM2MByJunction(const aJunctionTable: UTF8String): TOPManyToManyDef;
  var
    K: Integer;
  begin
    for K:=0 to aM2MRegistry.DefCount-1 do
      if SameText(aM2MRegistry.Defs[K].JunctionTable, aJunctionTable) then
        Exit(aM2MRegistry.Defs[K]);
    Result:=nil;
  end;

var
  I, IdxPos: Integer;
  PD: TPersisterDef;
  FM: TOPFieldMap;
  RelDef: TOPRelationDef;
  M2MDef: TOPManyToManyDef;
  ParentPD: TPersisterDef;
  PKField: TOPFieldMap;
  // A statement that must follow the one of the same difference
  Extra: TUTF8StringArray;
begin
  SetLength(Result, Length(aDiffs));
  SetLength(Extra, Length(aDiffs));
  for I:=0 to High(aDiffs) do
  begin
    case aDiffs[I].Kind of
      sdkAddTable:
        begin
          PD:=FindPersisterByTable(aDiffs[I].TableName);
          if PD=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationPersisterNotFound, [aDiffs[I].TableName]);
          Result[I]:=GetCreateSQL(PD.TableName, PD.FieldMap.AsArray, PD.PKConstraintName);
        end;
      sdkDropTable:
        Result[I]:=GetDropTableSQL(aDiffs[I].TableName);
      sdkAddColumn:
        begin
          PD:=FindPersisterByTable(aDiffs[I].TableName);
          if PD=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationPersisterNotFound, [aDiffs[I].TableName]);
          FM:=PD.FieldMap.FindByFieldName(aDiffs[I].ColumnName);
          if FM=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationFieldNotFound, [aDiffs[I].ColumnName, aDiffs[I].TableName]);
          Result[I]:=GetAddColumnSQL(PD.TableName, FM);
          Extra[I]:=GetAddUniqueIndexSQL(PD.TableName, FM);
        end;
      sdkDropColumn:
        Result[I]:=GetDropColumnSQL(aDiffs[I].TableName, aDiffs[I].ColumnName);
      sdkAlterColumnType:
        begin
          PD:=FindPersisterByTable(aDiffs[I].TableName);
          if PD=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationPersisterNotFound, [aDiffs[I].TableName]);
          FM:=PD.FieldMap.FindByFieldName(aDiffs[I].ColumnName);
          if FM=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationFieldNotFound, [aDiffs[I].ColumnName, aDiffs[I].TableName]);
          Result[I]:=GetAlterColumnTypeSQL(PD.TableName, aDiffs[I].ColumnName, FM);
          Extra[I]:=GetAlterAutoIncTypeSQL(PD.TableName, aDiffs[I].ColumnName, FM);
        end;
      sdkAlterColumnNullable:
        begin
          FM:=Nil;
          PD:=FindPersisterByTable(aDiffs[I].TableName);
          if PD<>nil then
            FM:=PD.FieldMap.FindByFieldName(aDiffs[I].ColumnName);
          Result[I]:=GetAlterColumnNullableSQL(aDiffs[I].TableName, aDiffs[I].ColumnName,
            aDiffs[I].Detail='NULL', FM);
        end;
      sdkAddIndex:
        begin
          PD:=FindPersisterByTable(aDiffs[I].TableName);
          if PD=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationPersisterNotFound, [aDiffs[I].TableName]);
          IdxPos:=FindIndexInPersister(PD, aDiffs[I].Detail);
          if IdxPos<0 then
            RaiseError('GenerateMigrationSQL', SErrMigrationIndexNotFound, [aDiffs[I].Detail, aDiffs[I].TableName]);
          Result[I]:=GetCreateIndexSQL(PD.TableName, PD.Indexes[IdxPos]);
        end;
      sdkDropIndex:
        Result[I]:=GetDropIndexSQL(aDiffs[I].Detail, aDiffs[I].TableName);
      sdkAddForeignKey:
        begin
          RelDef:=FindRelationByFK(aDiffs[I].TableName, aDiffs[I].ColumnName);
          if RelDef=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationRelationNotFound, [aDiffs[I].ColumnName, aDiffs[I].TableName]);
          ParentPD:=aRegistry.FindPersister(RelDef.ParentClass);
          if ParentPD=nil then
            RaiseError('GenerateMigrationSQL', SErrFKParentNotRegistered, [RelDef.ParentClass.ClassName]);
          PD:=aRegistry.FindPersister(RelDef.ChildClass);
          if PD=nil then
            RaiseError('GenerateMigrationSQL', SErrFKChildNotRegistered, [RelDef.ChildClass.ClassName]);
          PKField:=ParentPD.FieldMap.FindPrimaryKeyField;
          if PKField=nil then
            RaiseError('GenerateMigrationSQL', SErrFKParentNoPrimaryKey, [RelDef.ParentClass.ClassName]);
          Result[I]:=GetAddForeignKeySQL(PD.TableName, RelDef.ForeignKeyFieldName,
            ParentPD.TableName, PKField.FieldName, RelDef.DBCascade, RelDef.ConstraintName);
        end;
      sdkDropForeignKey:
        Result[I]:=GetDropForeignKeySQL(aDiffs[I].TableName, aDiffs[I].Detail);
      sdkAddJunctionTable:
        begin
          M2MDef:=FindM2MByJunction(aDiffs[I].TableName);
          if M2MDef=nil then
            RaiseError('GenerateMigrationSQL', SErrMigrationM2MNotFound, [aDiffs[I].TableName]);
          Result[I]:=GetCreateJunctionTableSQL(M2MDef, aRegistry);
        end;
    else
      RaiseError('GenerateMigrationSQL', 'Unsupported schema diff kind: %d', [Ord(aDiffs[I].Kind)]);
    end;
  end;
  // The extra statements follow the statement of their difference
  for I:=High(Extra) downto 0 do
    if Extra[I]<>'' then
      Insert(Extra[I], Result, I+1);
end;

function TOPDDLBuilder.IntrospectSchema(aConnection: TSQLConnection): TOPSchemaInfo;
begin
  Result:=Driver.IntrospectSchema(aConnection);
end;

initialization
  SQLFormatSettings:=DefaultFormatSettings;
  SQLFormatSettings.DecimalSeparator:='.';

end.
