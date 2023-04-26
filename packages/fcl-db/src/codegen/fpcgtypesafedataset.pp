{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    Typesafe dataset  

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpcgtypesafedataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpddcodegen,fpDataDict, fpcgfieldmap;

Const
  SNonInterfacedParentClass = 'TTypeSafeDatasetAccess';
  SInterfacedParentClass = 'TInterfacedTypeSafeDatasetAccess';

Type
  TTypeSafeDatasetOption = (tsdInterfaced,       // Create easy access interface
                            tsdFieldMapPublic,   // Create publicly accessible fieldmap object
                            tsdIsNullProperty,   // Create *IsNull property for each property
                            tsdFieldNameConsts,  // Define fieldnames.
                            tsdGetQuery,         // Create GetQuery() calls
                            tsdSQLPublic         // Create SQL statement as constant in interface.
                            );
  TTypeSafeDatasetOptions = set of TTypeSafeDatasetOption;

  { TGenTypeSafeDatasetOptions }

  TGenTypeSafeDatasetOptions = class(TGenFieldMapOptions)
  private
    FConnectionClass: String;
    FFieldNameConstPrefix: String;
    FMapAncestorName,
    FMapName,
    FInterfaceGUID,
    FInterfaceName: String;
    FMemoryStreamClass: String;
    FQueryClass: String;
    FTransactionClass: String;
    FTypeSafeAccesOptions: TTypeSafeDatasetOptions;
    function GetConnectionClass: String;
    function GetFieldNameConstPrefix: String;
    function GetInterfaceGUID: String;
    function GetInterfaceName: String;
    function GetQueryClass: String;
    function GetTransactionClass: String;
    procedure SetInterfaceGUID(const AValue: String);
    procedure SetInterfaceName(const AValue: String);
    procedure SetTypeSafeAccesOptions(AValue: TTypeSafeDatasetOptions);
  Protected
    function GetMapAncestorName: String; override;
    function GetMapName: String; override;
    procedure SetMapAncestorName(const AValue: String); override;
    procedure SetMapClassName(const AValue: String); override;
  Public
    Constructor Create; override;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property TypeSafeAccesOptions : TTypeSafeDatasetOptions Read FTypeSafeAccesOptions Write SetTypeSafeAccesOptions;
    Property InterfaceName : String Read GetInterfaceName Write SetInterfaceName;
    Property InterfaceGUID : String Read GetInterfaceGUID Write SetInterfaceGUID;
    Property MemoryStreamClass : String Read FMemoryStreamClass Write FMemoryStreamClass;
    Property FieldNameConstPrefix : String Read GetFieldNameConstPrefix Write FFieldNameConstPrefix;
    Property ConnectionClass : String Read GetConnectionClass Write FConnectionClass;
    Property TransactionClass : String Read GetTransactionClass Write FTransactionClass;
    Property QueryClass : String Read GetQueryClass Write FQueryClass;
    Property AncestorClass;
  end;

  { TTSAFieldPropDef }

  TTSAFieldPropDef = Class (TFieldPropDef)
  Protected
    Procedure InitFromField(F : TField); override;
    Procedure InitFromDDFieldDef(F : TDDFieldDef);override;
    procedure SetFieldType(AValue: TFieldType); override;
  Public
    Constructor Create(ACollection: TCollection); override;
    function FieldIsNullGetterName: String;
    function FieldIsNullSetterName: String;
    function FieldIsNullPropertyName: String;
  end;

  { TDDTypeSafeDatasetCodeGenerator }

  TDDTypeSafeDatasetCodeGenerator = class(TDDBaseFieldMapCodeGenerator)
  private
    FMySQL : TStringList;
    function GetSafeOpts: TGenTypeSafeDatasetOptions;virtual;
    function GetTSAFieldPropDefs(aIndex : Integer): TTSAFieldPropDef;
  Protected
    function GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    procedure CreateApplyUpdatesImplementation(Strings: TStrings); virtual;
    procedure CreateGetQueryImplementation(Strings: TStrings; ASQLArgument: Boolean);virtual;
    procedure CreateCreateQueryImplementation(Strings: TStrings; ASQLArgument: Boolean); virtual;
    procedure CreateSQLConst(Strings: TStrings; Full : Boolean);virtual;
    function CreateQueryDeclaration(aFull : Boolean; ASQLArgument: Boolean): String;virtual;
    function GetQueryDeclaration(aFull : Boolean; ASQLArgument: Boolean): String;virtual;
    function NeedConstSection: Boolean; virtual;
    procedure WriteConstSection(Strings: TStrings); virtual;
    function MyMapExpr: String;
    Function GetClassInterfaces: String; override;
    Function GetInterfaceUsesClause : string; override;
    procedure CreateTypeSafeInterfaceDeclaration(Strings: TStrings; const aInterfaceName, aInterfaceGUID: String);virtual;
    function GetFieldAccessor(F: TFieldPropDef): String;
    procedure GenerateFieldMapClass(Strings: TStrings); virtual;
    procedure GenerateMyMap(Strings: TStrings); virtual;
    procedure WriteBlobChangeEvent(Strings: TStrings; F: TFieldPropDef); virtual;
    procedure WritePropertyIsNullGetter(Strings: TStrings; F: TFieldPropDef);virtual;
    procedure WritePropertyIsNullSetter(Strings: TStrings; F: TFieldPropDef);virtual;
    Function NeedsConstructor : Boolean; override;
    Function NeedsDestructor : Boolean; override;
    procedure WritePropertyDeclaration(Strings: TStrings; F: TFieldPropDef); override;
    procedure WriteVisibilityStart(V: TVisibility; Strings: TStrings); override;
    procedure WriteVisibilityEnd(V: TVisibility; Strings: TStrings); override;
    procedure WriteFieldCreate(Strings: TStrings; F: TFieldPropDef); override;
    procedure WriteFieldDestroy(Strings: TStrings; F: TFieldPropDef); override;
    Function CreateFieldPropDefs : TFieldPropDefs; override;
    Function CreateOptions : TCodeGeneratorOptions; override;
    procedure WritePropertyGetterImpl(Strings: TStrings; F: TFieldPropDef); override;
    procedure WritePropertySetterImpl(Strings: TStrings; F: TFieldPropDef); override;
    procedure WritePrivateFields(Strings: TStrings); override;
    procedure DoBeforeTypeSection(Strings: TStrings); override;
    procedure DoBeforeClassDeclaration(Strings: TStrings); override;
    procedure DoAfterClassImplementation(Strings: TStrings); override;
    procedure DoBeforeClassImplementation(Strings: TStrings); override;
    Procedure DoAfterDestructor(Strings: TStrings); override;
    Property TSAFieldPropDefs[aIndex : Integer] : TTSAFieldPropDef read GetTSAFieldPropDefs;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function NeedsSQL: Boolean; override;
    Class Function NeedsFieldDefs : Boolean; override;
    Property SafeOpts : TGenTypeSafeDatasetOptions Read GetSafeOpts;
  end;

implementation

{ TTSAFieldPropDef }

procedure TTSAFieldPropDef.InitFromField(F: TField);
begin
  inherited InitFromField(F);
  If FieldType in ftBlobTypes then
    PropertyAccess:=paReadonly;
end;

procedure TTSAFieldPropDef.InitFromDDFieldDef(F: TDDFieldDef);
begin
  inherited InitFromDDFieldDef(F);
  If FieldType in ftBlobTypes then
    PropertyAccess:=paReadonly;
end;

procedure TTSAFieldPropDef.SetFieldType(AValue: TFieldType);
begin
  inherited SetFieldType(AValue);
  If FieldType in ftBlobTypes then
    PropertyAccess:=paReadonly;
end;

constructor TTSAFieldPropDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  PropertyAccess:=paReadWrite;
  PropSetters:=[psRead,psWrite];
end;

function TTSAFieldPropDef.FieldIsNullGetterName: String;
begin
  Result:='Get'+PropertyName+'IsNull';
end;

function TTSAFieldPropDef.FieldIsNullSetterName: String;
begin
  Result:='Set'+PropertyName+'IsNull';
end;

function TTSAFieldPropDef.FieldIsNullPropertyName: String;
begin
  Result:=PropertyName+'IsNull';
end;

{ TDDTypeSafeDatasetCodeGenerator }

function TDDTypeSafeDatasetCodeGenerator.GetSafeOpts: TGenTypeSafeDatasetOptions;
begin
  Result:=CodeOptions as TGenTypeSafeDatasetOptions;
end;

function TDDTypeSafeDatasetCodeGenerator.GetTSAFieldPropDefs(aIndex : Integer
  ): TTSAFieldPropDef;
begin
  Result:=Fields[aIndex] as TTSAFieldPropDef;
end;

function TDDTypeSafeDatasetCodeGenerator.GetSQL: TStrings;
begin
  Result:=FMySQL;
end;

procedure TDDTypeSafeDatasetCodeGenerator.SetSQL(const AValue: TStrings);
begin
  if (AValue=FMySQL) then exit;
  FMySQL.Assign(aValue);
end;

function TDDTypeSafeDatasetCodeGenerator.NeedsDestructor: Boolean;
begin
  Result:=True;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WritePropertyDeclaration(Strings: TStrings; F: TFieldPropDef);
begin
  inherited WritePropertyDeclaration(Strings, F);
  if tsdIsNullProperty in SafeOpts.TypeSafeAccesOptions then
    With F as TTSAFieldPropDef do
      AddLn(Strings,'Property %s : Boolean Read %s Write %s;',[FieldIsNullPropertyName,FieldIsNullGetterName,FieldIsNullSetterName]);
end;

function TDDTypeSafeDatasetCodeGenerator.MyMapExpr: String;

begin
  if tsdFieldMapPublic in SafeOpts.TypeSafeAccesOptions then
    Result:='MyMap'
  else
    Result:=SafeOpts.MapClassName+'(FieldMap)';
end;

function TDDTypeSafeDatasetCodeGenerator.GetClassInterfaces: String;
begin
  Result:=inherited GetClassInterfaces;
  if tsdInterfaced in SafeOpts.TypeSafeAccesOptions then
    begin
    if (Result<>'') then
      Result:=Result+',';
    Result:=Result+SafeOpts.InterfaceName;
    end;
end;

function TDDTypeSafeDatasetCodeGenerator.GetInterfaceUsesClause: string;
begin
  Result:=inherited GetInterfaceUsesClause;
  if tsdGetQuery in SafeOpts.TypeSafeAccesOptions then
    Result:=Result+', sqldb';
end;

procedure TDDTypeSafeDatasetCodeGenerator.WriteVisibilityStart(V: TVisibility;
  Strings: TStrings);
begin
  Inherited;
  If (V=vProtected) then
    begin
    AddLn(Strings,'Class Function FieldMapClass : TFieldMapClass; override;');
    if tsdFieldMapPublic in SafeOpts.TypeSafeAccesOptions then
      AddLn(Strings,'Function MyMap : %s; inline;',[SafeOpts.MapClassName]);
    end;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WriteVisibilityEnd(V: TVisibility;
  Strings: TStrings);
begin
  inherited WriteVisibilityEnd(V, Strings);
  if (v=vPublic) and (tsdGetQuery in SafeOpts.TypeSafeAccesOptions) then
    begin
    AddLn(Strings,'Procedure ApplyUpdates; override;');
    AddLn(Strings,CreateQueryDeclaration(False,True));
    AddLn(Strings,CreateQueryDeclaration(False,False));
    If (tsdInterfaced in SafeOpts.TypeSafeAccesOptions) then
      begin
      AddLn(Strings,GetQueryDeclaration(False,True));
      AddLn(Strings,GetQueryDeclaration(False,False));
      end;
    end;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WriteFieldCreate(Strings: TStrings;
  F: TFieldPropDef);
begin
  // Do nothing
  Assert(TObject(Strings)<>TObject(F),'Just avoiding compiler warning here');
end;

procedure TDDTypeSafeDatasetCodeGenerator.WriteFieldDestroy(Strings: TStrings;
  F: TFieldPropDef);
begin
  if F.FieldType in ftBlobTypes then
    Addln(Strings, 'FreeAndNil(FBlob%s);',[F.PropertyName])
end;

function TDDTypeSafeDatasetCodeGenerator.GetFieldAccessor(F: TFieldPropDef
  ): String;

begin
  Case F.PropertyType of
    ptDouble : Result:='AsFLoat';
    ptByte : Result:='AsInteger';
    ptShortInt : Result:='AsInteger';
    ptInt64 : Result:='AsLargeInt';
    ptWord : Result:='AsInteger';
    ptSmallInt : Result:='AsInteger';
    ptLongint : Result:='AsInteger';
    ptCardinal : Result:='AsInteger';
    ptQWord : Result:='AsLargeInt';
    ptDateTime : Result:='AsDateTime';
  else
    Result:='As'+PropTypeNames[F.PropertyType];
  end;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WriteBlobChangeEvent(Strings: TStrings; F: TFieldPropDef);

Var
  S : String;
begin
  S:=Format('Procedure %s.Do%sChanged(Sender : TObject);',[SafeOpts.ObjectClassName, F.PropertyName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  IncIndent;
  AddLn(Strings,'If Dataset.State in dsEditModes then');
  IncIndent;
  AddLn(Strings,'TBlobField(%s.%s).LoadFromStream(TStream(Sender));',[MyMapExpr,F.FieldName,F.PropertyName]);
  DecIndent;
  DecIndent;
  EndMethod(Strings,S);
end;

function TDDTypeSafeDatasetCodeGenerator.NeedsConstructor: Boolean;
begin
  Result:=False;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WritePropertyGetterImpl(
  Strings: TStrings; F: TFieldPropDef);

Var
  S : String;

begin
  If (F.FieldType in ftBlobTypes) then
    WriteBlobChangeEvent(Strings,F);
  S:=PropertyGetterDeclaration(F,True);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  IncIndent;
  If not (F.FieldType in ftBlobTypes) then
    AddLn(Strings,'Result:=%s.%s.%s;',[MyMapExpr,F.FieldName,GetFieldAccessor(F)])
  else
    begin
    AddLn(Strings,'if not Assigned(FBlob%s) then',[F.PropertyName]);
    IncIndent;
    AddLn(Strings,'begin');
    AddLn(Strings,'FBlob%s:=%s.Create;',[F.PropertyName,SafeOpts.MemoryStreamClass]);
    AddLn(Strings,'FBlob%s.OnChange:=@Do%sChanged;',[F.PropertyName,F.PropertyName]);
    AddLn(Strings,'end;');
    DecIndent;
    AddLn(Strings,'FBlob%s.Size:=0;',[F.PropertyName]);
    AddLn(Strings,'FBlob%s.Position:=0;',[F.PropertyName]);
    AddLn(Strings,'if not FBlob%s.Updating then',[F.PropertyName]);
    IncIndent;
    AddLn(Strings,'begin');
    AddLn(Strings,'TBlobField(%s.%s).SaveToStream(FBlob%s);',[MyMapExpr,F.FieldName,F.PropertyName]);
    AddLn(Strings,'FBlob%s.Position:=0;',[F.PropertyName]);
    AddLn(Strings,'end;');
    DecIndent;
    AddLn(Strings,'Result:=FBlob%s;',[F.PropertyName]);
    end;
  DecIndent;
  EndMethod(Strings,S);
  if tsdIsNullProperty in SafeOpts.TypeSafeAccesOptions then
    begin
    WritePropertyIsNullGetter(Strings,F);
    // We do it here, because for blob fields, WritePropertySetterImpl is not called.
    WritePropertyIsNullSetter(Strings,F);
    end;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WritePropertyIsNullGetter(Strings : TStrings; F : TFieldPropDef);

Var
  S : String;

begin
  S:=Format('Function %s.Get%sIsNull: boolean;',[SafeOpts.ObjectClassName, F.PropertyName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  incIndent;
  AddLn(Strings,'Result:=%s.%s.IsNull;',[MyMapExpr,F.FieldName]);
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TDDTypeSafeDatasetCodeGenerator.WritePropertySetterImpl(
  Strings: TStrings; F: TFieldPropDef);

Var
  S : String;

begin
  S:=PropertySetterDeclaration(F,True);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  incIndent;
  If not (F.FieldType in ftBlobTypes) then
    AddLn(Strings,'%s.%s.%s:=aValue;',[MyMapExpr,F.FieldName,GetFieldAccessor(F)]);
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TDDTypeSafeDatasetCodeGenerator.WritePropertyIsNullSetter(Strings : TStrings; F : TFieldPropDef);

Var
  S : String;

begin
  S:=Format('Procedure %s.Set%sIsNull(aValue : boolean);',[Safeopts.ObjectClassName, F.PropertyName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  IncIndent;
  AddLn(Strings,'If aValue then ');
  IncIndent;
  AddLn(Strings,'%s.%s.Clear;',[MyMapExpr,F.FieldName]);
  DecIndent;
  DecIndent;
  EndMethod(Strings,S);
end;



procedure TDDTypeSafeDatasetCodeGenerator.WritePrivateFields(Strings: TStrings);

Var
  I : Integer;
  F : TTSAFieldPropDef;

begin
  IncIndent;
//  AddLn(Strings,'FMap : %s;',[SafeOpts.MapClassName]);
  For I:=0 to Fields.Count-1 do
    begin
    F:=TSAFieldPropDefs[I];
    if F.FieldType in ftBlobTypes then
      AddLn(Strings,'FBlob%s : %s;',[F.PropertyName,SafeOpts.MemoryStreamClass]);
    end;
  // Change handlers for blobfields
  For I:=0 to Fields.Count-1 do
    begin
    F:=TSAFieldPropDefs[I];
    if F.FieldType in ftBlobTypes then
      AddLn(Strings,'Procedure Do%sChanged(Sender : TObject);',[F.PropertyName]);
    end;
  // Getters/Setters for IsNull
  if (tsdIsNullProperty in SafeOpts.TypeSafeAccesOptions) then
    For I:=0 to Fields.Count-1 do
      begin
      F:=TSAFieldPropDefs[I];
      AddLn(Strings,'Function %s : Boolean;',[F.FieldIsNullGetterName]);
      AddLn(Strings,'Procedure %s (aValue : Boolean);',[F.FieldIsNullSetterName]);
      end;
  DecIndent;
end;

procedure TDDTypeSafeDatasetCodeGenerator.WriteConstSection(Strings: TStrings);

Const
  OptsWriteSQL = [tsdGetQuery,tsdSQLPublic];

Var
  Pre : String;
  I : integer;

begin
  AddLn(Strings,'Const');
  IncIndent;
  if (tsdInterfaced in SafeOpts.TypeSafeAccesOptions) then
    AddLn(Strings,'IID_%s = ''%s'';',[SafeOpts.CleanObjectClassName,SafeOpts.InterfaceGUID]);
  AddLn(Strings);
  if (tsdFieldNameConsts in SafeOpts.TypeSafeAccesOptions) then
    begin
    AddLn(Strings,'// Field names');
    Pre:=SafeOpts.FieldNameConstPrefix;
    For I:=0 to Fields.Count-1 do
      if Fields[i].Enabled then
        AddLn(Strings,'%s = ''%s'';',[Pre+MakeIdentifier(Fields[i].FieldName),Fields[i].FieldName]);
    AddLn(Strings);
    end;
  if OptsWriteSQL * SafeOpts.TypeSafeAccesOptions = OptsWriteSQL then
    CreateSQLConst(Strings,False);
  DecIndent;
  AddLn(Strings);
end;

function TDDTypeSafeDatasetCodeGenerator.NeedConstSection: Boolean;


begin
  Result:=([tsdInterfaced,tsdFieldNameConsts,tsdSQLPublic] * SafeOpts.TypeSafeAccesOptions)<>[];
end;

procedure TDDTypeSafeDatasetCodeGenerator.DoBeforeTypeSection(Strings: TStrings);

begin
  inherited DoBeforeTypeSection(Strings);
  If NeedConstSection then
    WriteConstSection(Strings);
end;

function TDDTypeSafeDatasetCodeGenerator.CreateFieldPropDefs: TFieldPropDefs;
begin
  Result:=TFieldPropDefs.Create(TTSAFieldPropDef);
end;


function TDDTypeSafeDatasetCodeGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TGenTypeSafeDatasetOptions.Create;
end;

procedure TDDTypeSafeDatasetCodeGenerator.DoBeforeClassDeclaration(Strings: TStrings);

begin
  DecIndent;
  AddLn(Strings,'{$INLINE ON}');
  IncIndent;
  With SafeOpts do
    begin
    if (tsdFieldMapPublic in TypeSafeAccesOptions) then
      begin
      DecIndent;
      CreatefieldMapDeclaration(Strings,'',MapClassName,MapAncestorName);
      IncIndent;
      end;
    if (tsdInterfaced in TypeSafeAccesOptions) then
      CreateTypeSafeInterfaceDeclaration(Strings,InterfaceName,InterfaceGUID);
    end;
//  CreateDeclaration(Strings);
end;


procedure TDDTypeSafeDatasetCodeGenerator.CreateTypeSafeInterfaceDeclaration(
  Strings: TStrings; const aInterfaceName, aInterfaceGUID: String);

Var
  I : Integer;
  F : TTSAFieldPropDef;

begin
  Addln(Strings);
  Addln(Strings, '{ %s }',[aInterfaceName]);
  Addln(Strings);
  AddLn(Strings,'%s = Interface(ITypeSafeDatasetAccess) [IID_%s]',[aInterfaceName,SafeOpts.CleanObjectClassName]);
  IncIndent;
  // Getter/Setter
  for I:=0 to Fields.Count-1 do
    begin
    F:=TSAFieldPropDefs[i];
    if F.Enabled then
      begin
      AddLn(Strings,'Function %s : %s;',[F.ObjPasReadDef,PropTypeNames[F.PropertyType]]);
      if not (F.FieldType in ftBlobTypes) then
        AddLn(Strings,'Procedure %s (aValue : %s);',[F.ObjPasWriteDef,PropTypeNames[F.PropertyType]]);
      if tsdIsNullProperty in SafeOpts.TypeSafeAccesOptions then
        begin
        AddLn(Strings,'Function %s : Boolean;',[F.FieldIsNullGetterName]);
        AddLn(Strings,'Procedure %s (aValue : Boolean);',[F.FieldIsNullSetterName]);
        end;
      end;
    end;
  // Property
  for I:=0 to Fields.Count-1 do
    begin
    F:=TSAFieldPropDefs[i];
    if F.Enabled then
      begin
      AddLn(Strings,PropertyDeclaration(Strings,F)+';');
      if tsdIsNullProperty in SafeOpts.TypeSafeAccesOptions then
        AddLn(Strings,'Property %s : Boolean Read %s Write F%;',[F.FieldIsNullPropertyName,F.FieldIsNullGetterName,F.FieldIsNullSetterName]);
      end;
    end;
  DecIndent;
  AddLn(Strings,'end;');
end;

procedure TDDTypeSafeDatasetCodeGenerator.GenerateFieldMapClass(
  Strings: TStrings);

Var
  S : String;

begin
  S:=Format('Class Function %s.FieldMapClass : TFieldMapClass;',[SafeOpts.ObjectClassName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  incIndent;
  AddLn(Strings,'Result:=%s;',[Safeopts.MapClassName]);
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TDDTypeSafeDatasetCodeGenerator.GenerateMyMap(
  Strings: TStrings);

Var
  S : String;

begin
  S:=Format('Function %s.MyMap : %s;',[SafeOpts.ObjectClassName,SafeOpts.MapClassName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  incIndent;
  AddLn(Strings,'Result:=%s(FieldMap);',[Safeopts.MapClassName]);
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TDDTypeSafeDatasetCodeGenerator.DoAfterClassImplementation(
  Strings: TStrings);
begin
  inherited;
  GenerateFieldMapClass(Strings);
  With SafeOpts do
    CreateFieldMapImplementation(Strings,ObjectClassName,MapClassName);
  if tsdGetQuery in SafeOpts.TypeSafeAccesOptions then
    begin
    if not (tsdSQLPublic in SafeOpts.TypeSafeAccesOptions) then
      CreateSQLConst(Strings,True);
    CreateCreateQueryImplementation(Strings,False);
    CreateCreateQueryImplementation(Strings,True);
    If (tsdInterfaced in SafeOpts.TypeSafeAccesOptions) then
      begin
      CreateGetQueryImplementation(Strings,False);
      CreateGetQueryImplementation(Strings,True);
      end;
    CreateApplyUpdatesImplementation(Strings);
    end;
end;

Procedure TDDTypeSafeDatasetCodeGenerator.CreateApplyUpdatesImplementation(Strings: TStrings);

Var
  S : String;

begin
  S:=Format('Procedure %s.ApplyUpdates;',[SafeOpts.ObjectClassName]);
  BeginMethod(Strings,S);
  Addln(Strings);
  Addln(Strings,'begin');
  IncIndent;
  AddLn(Strings,'If Dataset is TSQLQuery then');
  IncIndent;
  AddLn(Strings,'(Dataset as TSQLQuery).ApplyUpdates;');
  DecIndent;
  DecIndent;
  EndMethod(Strings,S);
end;

function TDDTypeSafeDatasetCodeGenerator.CreateQueryDeclaration(aFull : Boolean; ASQLArgument: Boolean): String;

Var
  S : String;
begin
  if Not aSQLArgument then
    S:='CreateQuery(aConnection : %s; aTransaction : %s) : %s;'
  else
    S:='CreateQuery(aSQL : String; aConnection : %s; aTransaction : %s) : %s;';
  With SafeOpts do
    S:=Format(S,[ConnectionClass,TransactionClass,ObjectClassName]);
  if AFull then
    S:=SafeOpts.ObjectClassName+'.'+S
  else
    S:=S+' overload;';
  Result:='Class Function '+S;
end;

function TDDTypeSafeDatasetCodeGenerator.GetQueryDeclaration(aFull : Boolean; ASQLArgument: Boolean): String;

Var
  S : String;
begin
  if Not aSQLArgument then
    S:='GetQuery(aConnection : %s; aTransaction : %s) : %s;'
  else
    S:='GetQuery(aSQL : String; aConnection : %s; aTransaction : %s) : %s;';
  With SafeOpts do
    S:=Format(S,[ConnectionClass,TransactionClass,InterfaceName]);
  if AFull then
    S:=SafeOpts.ObjectClassName+'.'+S
  else
    S:=S+' overload;';
  Result:='Class Function '+S;
end;

procedure TDDTypeSafeDatasetCodeGenerator.CreateSQLConst(Strings : TStrings; Full : Boolean);

Var
  I : Integer;
  L : String;

begin
  if Full then
    begin
    AddLn(Strings,'Const');
    IncIndent;
    end;
  AddLn(Strings,'SQL%s = ',[SafeOpts.CleanObjectClassName]);
  IncIndent;
  if Not (Assigned(SQL) and (SQL.Count>0)) then
    AddLn(Strings,''''';')
  else
    For I:=0 to SQL.Count-1 do
      begin
      L:=CreatePascalString(SQL[I],True);
      if I<SQL.Count-1 then
        L:=L+' + sLineBreak +'
      else
        L:=L+';';
      Addln(Strings,L);
      end;
  DecIndent;
  if Full then
    DecIndent;
  Addln(Strings);
end;

procedure TDDTypeSafeDatasetCodeGenerator.CreateCreateQueryImplementation(
  Strings: TStrings; ASQLArgument: Boolean);

Var
  S : String;

begin
  S:=CreateQueryDeclaration(True,aSQLArgument);
  BeginMethod(Strings,S);
  if aSQLArgument then
    begin
    AddLn(Strings,'Var');
    IncIndent;
    AddLn(Strings,'Q : %s;',[SafeOpts.QueryClass]);
    AddLn(Strings,'MySQL : String;');
    DecIndent;
    end;
  AddLn(Strings,'begin');
  IncIndent;
  if Not aSQLArgument then
     Addln(Strings,'Result:=CreateQuery(SQL%s,aConnection,aTransaction);',[SafeOpts.CleanObjectClassName])
  else
    begin
    AddLn(Strings,'If aSQL='''' then');
    IncIndent;
    AddLn(Strings,'MySQL:=SQL%s ',[SafeOpts.CleanObjectClassName]);
    DecIndent;
    AddLn(Strings,'else');
    IncIndent;
    AddLn(Strings,'MySQL:=aSQL;');
    DecIndent;
    AddLn(Strings,'Q:=%s.Create(aConnection);',[SafeOpts.QueryClass]);
    AddLn(Strings,'If aTransaction<>Nil then');
    IncIndent;
    AddLn(Strings,'Q.Transaction:=aTransaction;');
    DecIndent;
    AddLn(Strings,'Q.Database:=aConnection;');
    AddLn(Strings,'Q.SQL.Text:=MySQL;');
    AddLn(Strings,'Result:=%s.Create(Q,True);',[SafeOpts.ObjectClassName]);
    end;
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TDDTypeSafeDatasetCodeGenerator.CreateGetQueryImplementation(
  Strings: TStrings; ASQLArgument: Boolean);

Var
  S : String;

begin
  S:=GetQueryDeclaration(True,aSQLArgument);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  IncIndent;
  if Not aSQLArgument then
     Addln(Strings,'Result:=CreateQuery(aConnection,aTransaction);',[SafeOpts.CleanObjectClassName])
  else
     Addln(Strings,'Result:=CreateQuery(aSQL,aConnection,aTransaction);',[SafeOpts.CleanObjectClassName]);
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TDDTypeSafeDatasetCodeGenerator.DoBeforeClassImplementation(
  Strings: TStrings);
begin
  inherited DoBeforeClassImplementation(Strings);
  With SafeOpts do
    if Not (tsdFieldMapPublic in TypeSafeAccesOptions) then
      begin
      AddLn(Strings,'Type');
      CreatefieldMapDeclaration(Strings,'',MapClassName,MapAncestorName);
      end;
end;

procedure TDDTypeSafeDatasetCodeGenerator.DoAfterDestructor(Strings: TStrings);
begin
  inherited DoAfterDestructor(Strings);
  if tsdFieldMapPublic in SafeOpts.TypeSafeAccesOptions then
    GenerateMyMap(Strings);
end;

constructor TDDTypeSafeDatasetCodeGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMySQL:=TStringlist.Create;
end;

destructor TDDTypeSafeDatasetCodeGenerator.Destroy;
begin
  FreeAndNil(FMySQL);
  inherited Destroy;
end;

class function TDDTypeSafeDatasetCodeGenerator.NeedsSQL: Boolean;
begin
  Result:=True;
end;

Class Function TDDTypeSafeDatasetCodeGenerator.NeedsFieldDefs : Boolean; 

begin
  Result:=True;
end;

{ TGenTypeSafeDatasetOptions }

function TGenTypeSafeDatasetOptions.GetInterfaceName: String;
begin
  Result:=FInterfaceName;
  If Result='' then
    Result:='I'+CleanObjectClassName;
end;

function TGenTypeSafeDatasetOptions.GetQueryClass: String;
begin
  Result:=FQueryClass;
  if Result='' then
    Result:='TSQLQuery';
end;

function TGenTypeSafeDatasetOptions.GetTransactionClass: String;
begin
  Result:=FConnectionClass;
  if Result='' then
    Result:='TSQLTransaction';
end;

function TGenTypeSafeDatasetOptions.GetInterfaceGUID: String;

Var
  G : TGUID;

begin
  if FInterfaceGUID='' then
    begin
    CreateGUID(G);
    FInterfaceGUID:=GUIDToString(G);
    end;
  Result:=FInterfaceGUID;
end;

function TGenTypeSafeDatasetOptions.GetFieldNameConstPrefix: String;
begin
  Result:=FFieldNameConstPrefix;
  if (Result='') then
    Result:='FLD_'+CleanObjectClassName+'_';
end;

function TGenTypeSafeDatasetOptions.GetConnectionClass: String;
begin
  Result:=FConnectionClass;
  if Result='' then
    Result:='TSQLConnection';
end;

procedure TGenTypeSafeDatasetOptions.SetInterfaceGUID(const AValue: String);

Var
  G : TGUID;

begin
  G:=StringToGUID(AValue);
  FInterfaceGUID:=GUIDToString(G);
end;

procedure TGenTypeSafeDatasetOptions.SetInterfaceName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FInterfaceName:=AValue;
end;

procedure TGenTypeSafeDatasetOptions.SetTypeSafeAccesOptions(
  AValue: TTypeSafeDatasetOptions);
begin
  if FTypeSafeAccesOptions=AValue then Exit;
  FTypeSafeAccesOptions:=AValue;
  if (tsdInterfaced in TypeSafeAccesOptions) then
    begin
    if (AncestorClass=SNonInterfacedParentClass) then
      AncestorClass:=SNonInterfacedParentClass;
    end
  else
    begin
    if (AncestorClass=SInterfacedParentClass) then
      AncestorClass:=SNonInterfacedParentClass;
    end
end;


function TGenTypeSafeDatasetOptions.GetMapAncestorName: String;
begin
  Result:=FMapAncestorName;
end;

function TGenTypeSafeDatasetOptions.GetMapName: String;
begin
  Result:=FMapName;
  if Result='' then
    Result:=ObjectClassName+'Map';
end;

procedure TGenTypeSafeDatasetOptions.SetMapAncestorName(const AValue: String);
begin
  CheckIdentifier(aValue,False);
  FMapAncestorName:=AValue;
end;

procedure TGenTypeSafeDatasetOptions.SetMapClassName(const AValue: String);
begin
  CheckIdentifier(aValue,False);
  FMapName:=AValue;
end;


constructor TGenTypeSafeDatasetOptions.Create;
Var
  G : TGUID;
begin
  inherited Create;
  AncestorClass:=SNonInterfacedParentClass;
  ObjectClassName:='TMyTypeSafeAccess';
  MapAncestorName:='TFieldMap';
  FMemoryStreamClass:='TBlobProxyStream';
  TypeSafeAccesOptions:=[tsdIsNullProperty];
  if CreateGUID(G)=0 then
    InterfaceGUID:=GuidToString(G);
end;

procedure TGenTypeSafeDatasetOptions.Assign(Source: TPersistent);

Var
  O : TGenTypeSafeDatasetOptions;

begin
  if Source is TGenTypeSafeDatasetOptions then
    begin
    O:=Source as TGenTypeSafeDatasetOptions;
    TypeSafeAccesOptions:=O.TypeSafeAccesOptions;
    FInterfaceName:=O.FInterfaceName; // Don't use public accessor
    FInterfaceGUID:=O.FInterfaceGUID; // Don't use public accessor
    MemoryStreamClass:=O.MemoryStreamClass;
    FFieldNameConstPrefix:=O.FFieldNameConstPrefix; // Don't use public accessor
    end;
  Inherited;
end;

Initialization
  RegisterCodeGenerator('TypesafeAccess','Type safe dataset access class and interface.',TDDTypeSafeDatasetCodeGenerator);

Finalization
  UnRegisterCodeGenerator(TDDTypeSafeDatasetCodeGenerator);
end.

