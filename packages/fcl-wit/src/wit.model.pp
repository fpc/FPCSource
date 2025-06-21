{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    classes that describe WIT document elements.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit WIT.Model;

{$mode objfpc}
{$H+}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs;
  {$ELSE}
  sysutils, classes, contnrs;
  {$ENDIF}

type
  EWIT = class(Exception)
    errno : integer;
  end;

  TWITTypeKind = (
    wtVoid, // Not an actual type, signifies none
    wtIdentifier,
    wtRecord,
    wtFlags,
    wtEnum,
    wtResource,
    wtTuple,
    wtList,
    wtOption,
    wtResult,
    wtVariant,
    wtUnion,
    wtString,
    wtU8,
    wtU16,
    wtU32,
    wtU64,
    wtS8,
    wtS16,
    wtS32,
    wtS64,
    wtFloat32,
    wtFloat64,
    wtBool,
    wtChar,
    wtStream,
    wtFuture,
    wtFunction
  );

    { TGFPObjectList }

  generic TGFPObjectList<T : TObject> = class (TFPObjectList)
  private
    Type

       { TObjectEnum }

       TObjectEnum = Class
         FList : TFPObjectList;
         FIdx : Integer;
         constructor create(aList : TFPObjectList);
         function GetCurrent : T;
         function MoveNext: Boolean;
         property Current : T read GetCurrent;
       end;

    function GetElement(aIndex: Integer): T;
    procedure SetElement(aIndex: Integer; AValue: T);
  Public
    function getenumerator : TObjectEnum;
    function add(aElement : T) : integer;
    property Elements[aIndex: Integer] : T read GetElement Write SetElement; default;
  end;

  { TWITAnnotationArgument }

  TWITAnnotationArgument = class (TObject)
  private
    FMember: string;
    FValue: string;
  public
    constructor Create(const aMember, aValue: string);
    destructor Destroy; override;
    property Member: string read FMember;
    property Value: string read FValue;
  end;

  TWITAnnotationArgumentList = Class(Specialize TGFPObjectList<TWITAnnotationArgument>);

  TWITAnnotation = class (TObject)
  private
    FName: string;
    FArguments: TWITAnnotationArgumentList;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Arguments: TWITAnnotationArgumentList read FArguments;
  end;

 TWITAnnotationList = Class(Specialize TGFPObjectList<TWITAnnotation>);

  { TWITBaseElement }

  TWITBaseElement = class (TObject)
  private
    FAnnotations: TWITAnnotationList;
  Public
    constructor create; virtual;
    destructor destroy; override;
    procedure AddAnnotation(aAnnotation : TWITAnnotation);
    Property Annotations : TWITAnnotationList read FAnnotations;
  end;

  { TWITType }

  TWITType = class (TWITBaseElement)
  private
    FKind: TWITTypeKind;
  protected
    // Will be called if IsNamed is true
    function ToString(const aName : string) : string; virtual;
    // Is this a named type: flags, enum, tuple, record, variant ?
    class function IsNamed : Boolean; virtual;
  public
    constructor Create(Kind: TWITTypeKind); reintroduce;
    function ToString: string; override;
    property Kind: TWITTypeKind read FKind;
  end;

  { TWITNamedType }

  TWITNamedType = class(TWITType)
  private
    FName: string;
  public
    constructor Create(const aName: string; aKind : TWITTypeKind); reintroduce;
    property Name: string read FName;
  end;

  { TWITIdentifierType }

  TWITIdentifierType = class(TWITNamedType)
  Public
    constructor Create(const aName : String);
    function ToString: string; override;
  end;

  { TWITHandleType }

  TWITHandleType = class(TWITIdentifierType)
  private
    FBorrowed: Boolean;
  public
    constructor Create(const aName : String; aBorrowed : Boolean = false);
    function ToString: string; override;
    property Borrowed : Boolean Read FBorrowed;
  end;

  { TWITTypeDef }

  TWITTypeDef = class(TWITNamedType)
  private
    FTypeDef: TWitType;
  Public
    constructor Create(const aName: string; aTypeDef : TWitType);
    destructor destroy; override;
    function ToString : string; override;
    property TypeDef : TWitType Read FTypeDef;
  end;
  TWITTypeDefList = specialize TGFPObjectList<TWITTypeDef>;


  { TWITRecordField }

  TWITRecordField = class(TWITBaseElement)
  private
    FName: string;
    FType: TWITType;
  public
    Constructor Create(const aName : String; aType : TWITType);  reintroduce;
    Destructor Destroy; override;
    function ToString : string; override;
    Property Name : string Read FName;
    Property FieldType : TWITType Read FType;
  end;

  TWITRecordFieldList = class(Specialize TGFPObjectList<TWITRecordField>);

  { TWITRecordType }

  TWITRecordType = class(TWITType)
  private
    FFields: TWitRecordFieldList;
  protected
    function ToString(const aName: string): string; override;
    class function IsNamed: Boolean; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddField(Field: TWITRecordField);
    function ToString: string; override;
    property Fields: TWITRecordFieldList read FFields;
  end;



  { TWITFlagsType }

  TWITFlagsType = class(TWITType)
  private
    FFlags: TStrings;
  protected
    function ToString(const aName: string): string; override;
    class function IsNamed: Boolean; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddFlag(const aFlag : string);
    function ToString: string; override;
    property Flags: TStrings read FFLags;
  end;

  { TWITEnumType }

  TWITEnumType = class(TWITType)
  private
    FCases: TStrings;
  protected
    function ToString(const aName: string): string; override;
    class function IsNamed: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCase(const aCase: String);
    function ToString: string; override;
    property Cases: TStrings read FCases;
  end;

  TWITTypeList = Class(Specialize TGFPObjectList<TWITType>);

  { TWITTupleType }

  TWITTupleType = class(TWITType)
  private
    FItems: TWITTypeList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(aItem: TWITType);
    function ToString: string; override;
    property Items: TWITTypeList read FItems;
  end;


  { TWITItemType }

  TWITItemType = class(TWITType)
  Private
    FItemType: TWITType;
  protected
    class function MyKind : TWitTypeKind; virtual; abstract;
    function EnvelopeName: String; virtual;
    function ItemToString: String;
  Public
    constructor Create(aItemType: TWITType);
    destructor destroy; override;
    function ToString : string; override;
    property ItemType: TWITType read FItemType;
  end;

    { TWITListType }

  TWITListType = class(TWITItemType)
  private
    FItemCount: integer;
  protected
    class function MyKind : TWitTypeKind; override;
  public
    constructor Create(aItemType: TWITType; aItemCount : integer = 0);
    function ToString: string; override;
    property ItemCount : integer Read FItemCount Write FItemCount;
  end;

  { TWITOptionType }

  TWITOptionType = class(TWITItemType)
  protected
    class function MyKind : TWitTypeKind; override;
  end;

  { TWITStreamType }

  TWITStreamType = class(TWITItemType)
  protected
    class function MyKind : TWitTypeKind; override;
  end;

  { TWITStreamType }

  { TWITFutureType }

  TWITFutureType = class(TWITItemType)
  protected
    class function MyKind : TWitTypeKind; override;
  end;


  { TWITResultType }

  TWITResultType = class(TWITType)
  private
    FOkType: TWITType;
    FErrorType: TWITType;
  public
    constructor Create(aOkType: TWITType; aErrorType: TWITType); reintroduce;
    destructor Destroy; override;
    function ToString: string; override;
    property OkType: TWITType read FOkType;
    property ErrorType: TWITType read FErrorType;
  end;

  { TWITVariantCase }

  TWITVariantCase = class (TObject)
   private
     FName: string;
     FType: TWITType;
   public
     constructor Create(const aName: string; aOptionalType: TWITType = nil);
     destructor destroy; override;
     function ToString : string;
     property Name: string read FName;
     property OptionalType: TWITType read FType;
   end;
  TWITVariantCaseList = class(specialize TGFPObjectList<TWITVariantCase>);

  { TWITVariantType }

  TWITVariantType = class(TWITType)
  private
    FCases: TWITVariantCaseList;
  protected
    class function IsNamed: Boolean; override;
    function ToString(const aName : string): string; override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure AddCase(aCase: TWITVariantCase);
    function ToString: string; override;
    property Cases: TWITVariantCaseList read FCases;
  end;

  { TWITFuncParam }

  TWITFuncParam = class(TWITBaseElement)
  private
    FName: string;
    FType: TWITType;
  public
    constructor Create(const aName: string; aType : TWITType); reintroduce;
    destructor Destroy; override;
    function ToString : string; override;
    property Name: string read FName write FName;
    property ParamType: TWITType read FType write FType;
  end;
  TWITFuncParamList = class(specialize TGFPObjectList<TWITFuncParam>);

  // Placeholder for your WIT function model (replace with your actual function type)

  TWITFunctionFlag = (ffAsync, ffStatic, ffConstructor);
  TWITFunctionFlags = set of TWITFunctionFlag;
  { TWITFunctionType }

  TWITFunctionType = class(TWITType)
  private
    FFlags: TWITFunctionFlags;
    FParams : TWITFuncParamList;
    FResultType: TWITType;
    procedure SetResultType(AValue: TWITType);
  Public
    Constructor Create; Reintroduce;
    destructor Destroy; override;
    function ToString : String; override;
    Property Parameters : TWITFuncParamList Read FParams;
    Property Flags : TWITFunctionFlags Read FFlags Write FFlags;
    Property ResultType: TWITType Read FResultType Write SetResultType;
  end;

  { TWITFunction }

  TWITFunction = class(TWITBaseElement)
  private
    FName: string;
    FTypeDef : TWITFunctionType;
  public
    constructor Create(const aName: string; aType : TWITFunctionType); reintroduce;
    destructor Destroy; override;
    function ToString : String; override;
    property Name: string read FName;
    property TypeDef : TWITFunctionType Read FTypeDef;
  end;

  TWITFunctionList = class(specialize TGFPObjectList<TWITFunction>);

  { TWITResourceType }

  TWITResourceType = class (TWITType)
  private
    FName: string;
    FFunctions: TWITFunctionList;
  public
    constructor Create(const aName: string);
    destructor Destroy; override;
    function ToString : string; override;
    property Name: string read FName write FName;
    property Functions: TWITFunctionList read FFunctions;
    procedure AddFunction(const aFunction: TWITFunction);
  end;

  { TWITUsePath }

  TWITUsePath = class(TObject)
  Private
    FNamespaces: TStringList;
    FPackageName: string;
    FVersion: string;
    FIdentifier: String;
  Public
    constructor Create; virtual;
    destructor Destroy; override;
    function ToString : string; override;
    procedure AddNamespace(const aNamespace: string);
    property Namespaces: TStringList read FNamespaces;
    property PackageName: string read FPackageName write FPackageName;
    property Version: string read FVersion write FVersion;
    property Identifier: string read FIdentifier write FIdentifier;
  end;

  TWITBaseUse = class (TWITBaseElement)
  private
    FPath : TWITUsePath;
    FRename: string;
    procedure AddNamespace(const Namespace: string);
  public
    constructor Create;override;
    destructor Destroy; override;
    function ToString : string; override;
    property Path : TWITUsePath read FPath;
    property Rename: string read FRename write FRename;
  end;

  TWITTopLevelUse = Class(TWITBaseUse);
  TWITTopLevelUsesList = class(specialize TGFPObjectList<TWITTopLevelUse>);

  { TWitUseItem }

  TWitUseItem = class(TWITBaseElement)
  private
    FAlias: string;
    FName: string;
  public
    constructor Create(const aName : string; const aAlias : String = ''); reintroduce;
    function ToString : string; override;
    property Name : string Read FName;
    property Alias : string Read FAlias;
  end;
  TWITUseItemList = class(specialize TGFPObjectList<TWITUseItem>);

  { TWITUse }

  TWITUse = class (TWITBaseUse)
  Private
    FItems : TWITUseItemList;
    FUses: TWITUseItemList;
  public
    constructor create; override;
    destructor destroy; override;
    function ToString : string; override;
    procedure AddItem(aItem : TWITUseItem);
    procedure AddItem(const aName : string; aAlias : string = '');
    Property Items : TWITUseItemList Read FUses;
  end;
  TWITInterfaceUseList = class(specialize TGFPObjectList<TWITUse>);


  { TWITInterface }

  TWITInterface = class (TWITBaseElement)
  private
    FName: string;
    FFunctions: TWITFunctionList;
    FTypes: TWITTypeDefList;
    FUseList : TWITInterfaceUseList;
  public
    constructor Create(const aName: string); reintroduce;
    destructor Destroy; override;
    procedure AddFunction(aFunction: TWITFunction);
    procedure AddType(aType: TWITTypeDef);
    procedure AddUses(aUse: TWITUse);
    function ToString: String override;
    property Name: string read FName write FName;
    property Functions: TWITFunctionList read FFunctions;
    property Types: TWITTypeDefList read FTypes;
    property UseList : TWITInterfaceUseList Read FUseList;
  end;
  TWITInterfaceList = class(specialize TGFPObjectList<TWITInterface>);



  TExchangeType = (xtImport,xtExport);

  { TWITExchange }

  TWITExchange = class (TWITBaseElement)
  private
    FName: string;
    FType: TExChangeType;
  public
    constructor Create(aType : TExchangeType; const aName: string); reintroduce;
    property Name: string read FName write FName;
    Property ExtrangeType : TExChangeType Read FType;
  end;
  TWITImportList = class(specialize TGFPObjectList<TWITExchange>);
  TWITExportList = class(specialize TGFPObjectList<TWITExchange>);

  { TWITExportIdentifier }

  { TWITExchangeIdentifier }
  TWITExchangeIdentifier = class (TWITExchange)
  private
    FUse: TWITUsePath;
  public
    constructor Create(aType : TExchangeType; aPath : string);
    constructor Create(aType : TExchangeType; aUse: TWITUsePath);
    destructor destroy; override;
    function ToString : string; override;
    property UseDef : TWITUsePath read FUse;
  end;

  { TWITExportFunc }

  TWITExchangeFunc = class (TWITExchange)
  private
    FTypeDef: TWITFunctionType;
  public
    constructor Create(aType : TExchangeType; const aName: string; aFunc : TWITFunctionType);
    destructor Destroy; override;
    property TypeDef : TWITFunctionType read FTypeDef;
  end;

    { TWITExportFunc }

  { TWITExportInterface }

  { TWITExchangeInterface }

  TWITExchangeInterface = class (TWITExchange)
  private
    FTypeDef: TWITInterface;
  public
    constructor Create(aType : TExchangeType; const aName: string; aIntf : TWITInterface);
    destructor Destroy; override;
    property TypeDef : TWITInterface read FTypeDef;
  end;



  { TWitIncludeItem }

  TWitIncludeItem = class(TWITBaseElement)
  private
    FAlias: string;
    FName: string;
  public
    constructor Create(const aName : string; const aAlias : String = ''); reintroduce;
    function ToString : string; override;
    property Name : string Read FName;
    property Alias : string Read FAlias;
  end;
  TWITIncludeItemList = class(specialize TGFPObjectList<TWITIncludeItem>);

  TWITInclude = class(TWITBaseElement)
  private
    FItems: TWitIncludeItemList;
    FPath: TWITUsePath;
  Public
    Constructor create; override;
    destructor destroy; override;
    function ToString : string; override;
    function AddItem(const aItem : String; aAlias : String = '') : TWitIncludeItem;
    Property Path : TWITUsePath Read FPath;
    Property Items : TWitIncludeItemList Read FItems;
  end;
  TWITIncludeList = class(specialize TGFPObjectList<TWITInclude>);

  { TWITWorld }

  TWITWorld = class (TWITBaseElement)
  private
    FIncludes: TWITIncludeList;
    FName: string;
    FImports: TWITImportList;
    FExports: TWITExportList;
    FTypeDefs: TWITTypeDefList;
    FUses: TWITInterfaceUseList;
  public
    constructor Create(const Name: string); reintroduce;
    destructor Destroy; override;
    function ToString : string; override;
    procedure AddImport(Import: TWITExchange);
    procedure AddExport(Export: TWITExchange);
    procedure AddInclude(aInclude: TWITInclude);
    procedure AddUses(aUses: TWITUse);
    procedure AddTypeDef(aTypeDef: TWITTypeDef);
    property Name: string read FName write FName;
    property Imported: TWITImportList read FImports;
    property Exported: TWITExportList read FExports;
    property TypeDefs: TWITTypeDefList read FTypeDefs;
    property UsesList: TWITInterfaceUseList read FUses;
    property Includes: TWITIncludeList read FIncludes;
  end;
  TWITWorldList = class(specialize TGFPObjectList<TWITWorld>);

  { TWITPackage }

  TWITPackage = class (TWITBaseELement)
  private
    FIsNested: Boolean;
    FNamespace: string;
    FPackageName: string;
    FVersion: string;
    FImports: TStringList;
    FExports: TStringList;
    FUseStatements: TWitTopLevelUsesList;
    FInterfaces: TWITInterfaceList;
    FWorlds: TWITWorldList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ToString : string; override;
    property Namespace: string read FNamespace write FNamespace;
    property PackageName: string read FPackageName write FPackageName;
    property Version: string read FVersion write FVersion;
    property ImportList: TStringList read FImports;
    property ExportList: TStringList read FExports;
    property UseStatements: TWitTopLevelUsesList read FUseStatements;
    property Interfaces: TWITInterfaceList read FInterfaces;
    property Worlds: TWITWorldList read FWorlds;
    Property IsNested : Boolean Read FIsNested Write FIsNested;
  end;
  TWITPackageList = class(specialize TGFPObjectList<TWITPackage>);



  { TWITDocument }

  TWITDocument = class  (TWITBaseElement)
  private
    FDefaultPackage: TWitPackage;
    FPackages: TWITPackageList;
    function GetDefaultPackage: TWITPackage;
    function GetInterfaces: TWITInterfaceList;
    function GetUseStatements: TWITTopLevelUsesList;
    function GetWorlds: TWITWorldList;
    procedure SetDefaultPackage(const aValue: TWITPackage);
  public
    constructor Create; override;
    destructor Destroy; override;
    property DefaultPackage : TWITPackage Read GetDefaultPackage Write SetDefaultPackage;
    property Packages: TWITPackageList read FPackages;
    property Interfaces: TWITInterfaceList read GetInterfaces;
    property UseStatements: TWITTopLevelUsesList read GetUseStatements;
    property Worlds: TWITWorldList read GetWorlds;
    procedure AddInterface(aInterface: TWITInterface);
    procedure AddUse(aUse: TWITTopLevelUse);
    procedure AddWorld(aWorld: TWITWorld);
  end;

implementation


{ TWITType }

function TWITType.ToString(const aName: string): string;
begin
  Result:=ToString()+' '+aname+';';
end;

class function TWITType.IsNamed: Boolean;
begin
  Result:=False;
end;

constructor TWITType.Create(Kind: TWITTypeKind);
begin
  inherited create;
  FKind := Kind;
end;

function TWITType.ToString: string;
begin
  WriteStr(Result,fkind);
  Delete(Result,1,2);
  Result:=LowerCase(Result);
end;

{ TWITNamedType }

constructor TWITNamedType.Create(const aName: string; aKind: TWITTypeKind);
begin
  inherited Create(Akind);
  FName := aName;
end;

{ TWITIdentifierType }

constructor TWITIdentifierType.Create(const aName: String);
begin
  Inherited Create(aName,wtIdentifier);
end;

function TWITIdentifierType.ToString: string;
begin
  Result := FName;
end;

{ TWITHandleType }

constructor TWITHandleType.Create(const aName: String; aBorrowed: Boolean);
begin
  Inherited create(aName);
  FBorrowed:=aBorrowed;
end;

function TWITHandleType.ToString: string;
begin
  Result:=inherited ToString;
  if FBorrowed then
    Result:='borrow<'+Result+'>'
  else
    Result:='own<'+Result+'>';
end;

{ TWITNamedType }

constructor TWITTypeDef.Create(const aName: string; aTypeDef: TWitType);
begin
  Inherited Create(aName,aTypeDef.Kind);
  FTypeDef:=aTypeDef;
end;

destructor TWITTypeDef.destroy;
begin
  FreeAndNil(FTypeDef);
  inherited destroy;
end;

function TWITTypeDef.ToString: string;

begin
  if TypeDef.IsNamed then
    Result:=TypeDef.ToString(Name)
  else
    Result:='type '+Name+' = '+TypeDef.ToString+';'
end;

{ TWITRecordField }

constructor TWITRecordField.Create(const aName: String; aType: TWITType);
begin
  Inherited Create;
  FName:=aName;
  FType:=aType;
end;

destructor TWITRecordField.Destroy;
begin
  FreeAndNil(FType);
  inherited Destroy;
end;

function TWITRecordField.ToString: string;
begin
  Result:=name+' : '+FieldType.ToString;
end;

{ TGFPObjectList }

function TGFPObjectList.GetElement(aIndex: Integer): T;
begin
  Result:=T(Items[aIndex]);
end;

procedure TGFPObjectList.SetElement(aIndex: Integer; AValue: T);
begin
  Items[aIndex]:=aValue;
end;

function TGFPObjectList.getenumerator: TObjectEnum;
begin
  Result:=TObjectEnum.Create(Self);

end;

function TGFPObjectList.add(aElement: T): integer;
begin
  Result:=Inherited add(aElement);
end;

{ TGFPObjectList.TObjectEnum }

constructor TGFPObjectList.TObjectEnum.create(aList: TFPObjectList);
begin
  FList:=aList;
  FIdx:=-1;
end;

function TGFPObjectList.TObjectEnum.GetCurrent: T;
begin
  If FIdx<0 then
    result:=Nil
  else
    Result:=T(FList[FIdx]);
end;

function TGFPObjectList.TObjectEnum.MoveNext: Boolean;
begin
  Inc(FIdx);
  Result:=FIdx<FList.Count;

end;

{ TWITAnnotationArgument }

constructor TWITAnnotationArgument.Create(const aMember, aValue: string);
begin
  FMember:=aMember;
  FValue:=aValue;
end;

destructor TWITAnnotationArgument.Destroy;
begin
  inherited Destroy;
end;

{ TWITRecordType }

function TWITRecordType.ToString(const aName: string): string;

var
  Field : TWITRecordField;
begin
  Result:='';
  for Field in FFields do
  begin
    if Result<>'' then
      Result:=Result+','+sLineBreak;
    Result := Result + '  '+Field.ToString ;
  end;
  if Result<>'' then
    Result:=sLineBreak+Result+sLineBreak;
  Result := 'record '+aName+' {'+Result+'}';

end;

class function TWITRecordType.IsNamed: Boolean;
begin
  Result:=True;
end;

constructor TWITRecordType.Create;
begin
  inherited Create(wtRecord);
  FFields := TWITRecordFieldList.Create;
end;

destructor TWITRecordType.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TWITRecordType.AddField(Field: TWITRecordField);
begin
  FFields.Add(Field);
end;

function TWITRecordType.ToString: string;

begin
  Result:=ToString('');
end;

{ TWITFlagsType }

function TWITFlagsType.ToString(const aName: string): string;
var
  I : integer;
begin
  Result:='';
  for I:=0 to Flags.Count-1 do
    begin
    if I>0 then
      Result:=Result+','+sLineBreak;
    Result:=Result+'  '+Flags[i];
    end;
  if Result<>'' then
    Result:=sLinebreak+Result+sLinebreak;
  Result:='flags '+aName+' {'+Result+'}';
end;

class function TWITFlagsType.IsNamed: Boolean;
begin
  Result:=True;
end;

constructor TWITFlagsType.Create;
begin
  Inherited Create(wtFlags);
  FFlags:=TStringList.Create;
end;

destructor TWITFlagsType.Destroy;
begin
  FreeAndNil(FFlags);
  inherited Destroy;
end;

procedure TWITFlagsType.AddFlag(const aFlag: string);
begin
  FFlags.Add(aFlag);
end;

function TWITFlagsType.ToString: string;
begin
  Result:=ToString('');
end;

{ TWITEnumType }

function TWITEnumType.ToString(const aName: string): string;
var
  I : integer;
begin
  Result:='';
  for I:=0 to FCases.Count-1 do
    begin
    if I>0 then
      Result:=Result+','+sLineBreak;
    Result:=Result+'  '+FCases[i]
    end;
  if Result<>'' then
    Result:=sLinebreak+Result+sLinebreak;
  Result:='enum '+aName+' {'+Result+'}';
end;

class function TWITEnumType.IsNamed: Boolean;
begin
  Result:=true;
end;

constructor TWITEnumType.Create;
begin
  Inherited create(wtEnum);
  FCases:=TStringList.Create;
end;

destructor TWITEnumType.Destroy;
begin
  FreeAndNil(FCases);
  inherited Destroy;
end;

procedure TWITEnumType.AddCase(const aCase: String);
begin
  FCases.Add(aCase);
end;

function TWITEnumType.ToString: string;
begin
  Result:=ToString('');
end;

{ TWITTupleType }

constructor TWITTupleType.Create;
begin
  Inherited create(wtTuple);
  FItems:=TWITTypelist.Create(True);
end;

destructor TWITTupleType.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TWITTupleType.AddItem(aItem: TWITType);
begin
  FItems.Add(aItem);
end;

function TWITTupleType.ToString: string;
var
  i : integer;
begin
  Result:='';
  for I:=0 to Items.Count-1 do
    begin
    if I>0 then
      Result:=Result+',';
    Result:=Result+Items[i].ToString;
    end;
  Result:='tuple<'+Result+'>';
end;

{ TWITItemType }

function TWITItemType.EnvelopeName: String;
begin
  WriteStr(Result,Kind);
  Result:=LowerCase(Copy(Result,3,Length(Result)-2));
end;

function TWITItemType.ItemToString: String;
begin
  Result:='';
  if Assigned(ItemType) then
    Result:=ItemType.ToString;
end;

constructor TWITItemType.Create(aItemType: TWITType);
begin
  Inherited Create(MyKind);
  FItemType:=aItemType;
end;

destructor TWITItemType.destroy;
begin
  FreeAndNil(FItemType);
  inherited destroy;
end;

function TWITItemType.ToString: string;

begin
  Result:=EnvelopeName+'<'+ItemToString+'>';
end;

{ TWITListType }

class function TWITListType.MyKind: TWitTypeKind;
begin
  Result:=wtList;
end;

constructor TWITListType.Create(aItemType: TWITType; aItemCount: integer);
begin
  Inherited create(aItemType);
  FItemCount:=aItemCount;
end;

function TWITListType.ToString: string;

begin
  Result:=EnvelopeName+'<'+ItemToString;
  if ItemCount>0 then
    Result:=Result+','+IntTostr(ItemCount);
  Result:=Result+'>';
end;

{ TWITOptionType }

class function TWITOptionType.MyKind: TWitTypeKind;
begin
  Result:=wtOption;
end;


{ TWITStreamType }

class function TWITStreamType.MyKind: TWitTypeKind;
begin
  Result:=wtStream;
end;


{ TWITFutureType }

class function TWITFutureType.MyKind: TWitTypeKind;
begin
  Result:=wtFuture;
end;

{ TWITResultType }

constructor TWITResultType.Create(aOkType: TWITType; aErrorType: TWITType);
begin
  inherited create(wtResult);
  FOkType:=aOkType;
  FErrorType:=aErrorType;
end;

destructor TWITResultType.Destroy;
begin
  FreeAndNil(FOkType);
  FreeAndNil(FErrorType);
  inherited Destroy;
end;

function TWITResultType.ToString: string;
begin
  Result:='';
  if Assigned(OkType) then
    result:=OKType.ToString;
  if Assigned(ErrorType) then
    begin
    if Result='' then
      Result:='_';
    Result:=Result+','+ErrorType.ToString;
    end;
  if Result<>'' then
    Result:='<'+result+'>';
  Result:='result'+Result;
end;

{ TWITVariantCase }

constructor TWITVariantCase.Create(const aName: string; aOptionalType: TWITType);
begin
  FName:=aName;
  FType:=aOptionalType;
end;

destructor TWITVariantCase.destroy;
begin
  FreeAndNil(FType);
  inherited destroy;
end;

function TWITVariantCase.ToString: string;
begin
  Result:=Name;
  if Assigned(OptionalType) then
    Result:= Result+'('+OptionalType.ToString+')';
end;

{ TWITVariantType }

class function TWITVariantType.IsNamed: Boolean;
begin
  Result:=True
end;

function TWITVariantType.ToString(const aName: string): string;
var
  I : integer;
begin
  Result:='';
  for I:=0 to Cases.Count-1 do
    begin
    if Result<>'' then
      Result:=Result+','+sLinebreak;
    Result:=Result+'  '+Cases[I].ToString;
    end;
  if Result<>'' then
    Result:=sLineBreak+Result+sLineBreak;
  Result:='variant '+aName+' {'+Result+'}';
end;

constructor TWITVariantType.Create;
begin
  inherited create(wtVariant);
  FCases:=TWITVariantCaseList.Create(True);
end;

destructor TWITVariantType.Destroy;
begin
  FreeAndNil(FCases);
  inherited Destroy;
end;

procedure TWITVariantType.AddCase(aCase: TWITVariantCase);
begin
  FCases.Add(aCase);
end;

function TWITVariantType.ToString: string;
begin
  Result:=tostring('');
end;

{ TWITFuncParam }

constructor TWITFuncParam.Create(const aName: string; aType: TWITType);
begin
  FName:=aName;
  FType:=aType;
end;

destructor TWITFuncParam.Destroy;
begin
  FreeAndNil(FType);
  inherited Destroy;
end;

function TWITFuncParam.ToString: string;
begin
  Result:=Name+': '+ParamType.ToString;
end;

{ TWITFunction }

constructor TWITFunctionType.Create();
begin
  Inherited Create(wtFunction);
  FParams:=TWITFuncParamList.Create(True);
end;

procedure TWITFunctionType.SetResultType(AValue: TWITType);
begin
  if FResultType=AValue then Exit;
  FreeAndNil(FResultType);
  FResultType:=AValue;
end;

destructor TWITFunctionType.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FResultType);
  inherited Destroy;
end;

function TWITFunctionType.ToString: String;
var
  LParam : TWITFuncParam;
begin
  Result:='';
  for lParam in Parameters do
    begin
    if Result<>'' then
      Result:=Result+', ';
    Result:=Result+lParam.ToString;
    end;
  Result:='('+Result+')';
  if ResultType<>nil then
    Result:=Result+' -> '+ResultType.ToString;
  if ffConstructor in FFlags then
    Result:='constructor'+Result
  else
    Result:='func'+Result;
  if ffAsync in FFlags then
    Result:='async '+Result
  else if ffStatic in FFlags then
    Result:='static '+Result;
end;

constructor TWITFunction.Create(const aName: string; aType: TWITFunctionType);
begin
  Inherited create;
  FName:=aName;
  FTypeDef:=aType;
end;

destructor TWITFunction.Destroy;
begin
  FreeAndNil(FTypeDef);
  Inherited;
end;

function TWITFunction.ToString: String;
begin
  Result:=TypeDef.ToString;
  if not (ffConstructor in TypeDef.Flags) then
    Result:=Name+' : '+Result;
  Result:=Result+';';
end;


{ TWITResourceType }

constructor TWITResourceType.Create(const aName: string);
begin
  inherited create(wtResource);
  FName := aName;
  FFunctions := TWITFunctionList.Create; // Initialize functions list
end;

destructor TWITResourceType.Destroy;
begin
  FFunctions.Free;
  inherited Destroy;
end;

function TWITResourceType.ToString: string;
var
  I : Integer;
begin
  Result:='';
  for I:=0 to Functions.Count-1 do
    begin
    if Result<>'' then
      Result:=Result+sLineBreak;
    Result:=Result+'  '+Functions[i].ToString;
    end;
  if Result<>'' then
    Result:=' {'+sLineBreak+Result+sLineBreak+'}'
  else
    Result:=';';
  Result:='resource '+Name+Result;
end;

procedure TWITResourceType.AddFunction(const aFunction: TWITFunction);
begin
  FFunctions.Add(aFunction);
end;

{ TWITPackage }

constructor TWITPackage.Create;
begin
  Inherited Create;
  FImports := TStringList.Create;
  FExports := TStringList.Create;
  FWorlds:=TWITWorldList.Create;
  FUseStatements := TWITTopLevelUsesList.Create;
  FInterfaces := TWITInterfaceList.Create;
end;

destructor TWITPackage.Destroy;
begin
  FreeAndNil(FImports);
  FreeAndNil(FExports);
  FreeAndNil(FUseStatements);
  FreeAndNil(FInterfaces);
  FreeAndNil(FWorlds);
  inherited Destroy;
end;

function TWITPackage.ToString: string;
var
  i : integer;
begin
  Result:=PackageName;
  if Namespace<>'' then
    Result:=Namespace+':'+Result;
  if Version<>'' then
    Result:=Result+'@'+Version;

  Result:='package '+ Result;
  if not isNested then
    begin
    Result:=Result+';';
    if (UseStatements.Count>0) or (Worlds.Count>0) or (Interfaces.Count>0) then
      Result:=Result+sLineBreak;
    end
  else
    Result:=Result+' {'+sLineBreak;
  for I:=0 to UseStatements.Count-1 do
     Result:=Result+UseStatements[i].ToString+sLineBreak;
  for I:=0 to Worlds.Count-1 do
     Result:=Result+worlds[i].ToString+sLineBreak;
  for I:=0 to Interfaces.Count-1 do
     Result:=Result+Interfaces[i].ToString+sLineBreak;
  if isNested then
    Result:=Result+'}'+sLineBreak;
end;

{ TWITAnnotation }

constructor TWITAnnotation.Create(const Name: string);
begin
  FName := Name;
  FArguments := TWITAnnotationArgumentList.Create(true);
end;

destructor TWITAnnotation.Destroy;
begin
  FArguments.Free;
  inherited Destroy;
end;

{ TWITBaseElement }

constructor TWITBaseElement.create;
begin
  FAnnotations:=TWITAnnotationList.Create(True);
end;

destructor TWITBaseElement.destroy;
begin
  FAnnotations.Free;
  Inherited;
end;

procedure TWITBaseElement.AddAnnotation(aAnnotation: TWITAnnotation);
begin
  FAnnotations.Add(aAnnotation);
end;

{ TWITInterface }

constructor TWITInterface.Create(const aName: string);
begin
  inherited create;
  FName := aName;
  FFunctions := TWITFunctionList.Create;
  FTypes:= TWITTypeDefList.Create;
  FUseList:=TWITInterfaceUseList.Create;
end;

destructor TWITInterface.Destroy;
begin
  FreeAndNil(FUseList);
  FreeAndNil(FFunctions);
  FreeAndNil(FTypes);
  inherited Destroy;
end;

procedure TWITInterface.AddFunction(aFunction: TWITFunction);
begin
  FFunctions.Add(aFunction);
end;

procedure TWITInterface.AddType(aType: TWITTypeDef);
begin
  FTypes.Add(aType);
end;

procedure TWITInterface.AddUses(aUse: TWITUse);
begin
  FUseList.Add(aUse);
end;

function TWITInterface.ToString: String;

  procedure AddToResult(aDef : String);
  begin
    if Result<>'' then
      Result:=Result+sLineBreak;
    Result:=Result+'  '+aDef;
  end;

var
  LType : TWITTypeDef;
  lFunc : TWITFunction;
  LUse : TWITUse;

begin
  Result:='';
  For LUse in UseList do
    AddToResult(LUse.ToString);
  For LType in Types do
    AddToResult(lType.ToString);
  For lFunc in Functions do
    AddToResult(lFunc.ToString);
  if Result<>'' then
    Result:=sLineBreak+Result+sLineBreak;
  Result:='interface '+Name+' {'+Result+'}';
end;

{ TWITUsePath }

constructor TWITUsePath.Create;
begin
  FNamespaces:=TStringList.Create;
end;

destructor TWITUsePath.Destroy;
begin
  FreeAndNil(FNameSpaces);
  Inherited;
end;

function TWITUsePath.ToString: string;
var
  i : Integer;
begin
  Result:='';
  For I:=0 to Namespaces.Count-1 do
    begin
    if I>0 then
      Result:=Result+':';
    Result:=Result+NameSpaces[i]
    end;
  if PackageName<>'' then
    begin
    if Result<>'' then
      Result:=Result+':';
    Result:=Result+PackageName;
    end;
  if Identifier<>'' then
    begin
    if Result<>'' then
      Result:=Result+'/';
    Result:=Result+Identifier;
    end;
  if Version<>'' then
    Result:=Result+'@'+Version;
end;

procedure TWITUsePath.AddNamespace(const aNamespace: string);
begin
  FNamespaces.Add(aNameSpace);
end;


{ TWITDocument }

function TWITDocument.GetInterfaces: TWITInterfaceList;
begin
  Result:=DefaultPackage.Interfaces;
end;

function TWITDocument.GetUseStatements: TWITTopLevelUsesList;
begin
  Result:=DefaultPackage.UseStatements;
end;

function TWITDocument.GetDefaultPackage: TWITPackage;
begin
  if FDefaultPackage=Nil then
    begin
    FDefaultPackage:=TWITPackage.Create;
    FPackages.Add(FDefaultPackage);
    end;
  Result:=FDefaultPackage;
end;

function TWITDocument.GetWorlds: TWITWorldList;
begin
  Result:=DefaultPackage.Worlds;
end;

procedure TWITDocument.SetDefaultPackage(const aValue: TWITPackage);
begin
  if FDefaultPackage<>Nil then
    Raise EWIT.Create('Default package already set.');
  if aValue.IsNested then
    Raise EWIT.Create('Default package cannot be nested.');
  FDefaultPackage:=aValue;
end;

constructor TWITDocument.Create;
begin
  Inherited create;
  FPackages := TWITPackageList.Create(True);
end;

destructor TWITDocument.Destroy;

begin
  FPackages.Free;
  inherited Destroy;
end;

procedure TWITDocument.AddInterface(aInterface: TWITInterface);
begin
  DefaultPackage.Interfaces.Add(aInterface);
end;

procedure TWITDocument.AddUse(aUse: TWITTopLevelUse);
begin
  DefaultPackage.UseStatements.Add(aUse);
end;

procedure TWITDocument.AddWorld(aWorld : TWITWorld);
begin
  DefaultPackage.Worlds.Add(aWorld);
end;



{ TWITExchange }

constructor TWITExchange.Create(aType: TExchangeType; const aName: string);
begin
  inherited create;
  FName := aName;
  FType:=aType;
end;

{ TWITExportIdentifier }

constructor TWITExchangeIdentifier.Create(aType: TExchangeType; aPath: string);
begin
  Inherited Create(aType,aPath);
  FUse:=TWITUsePath.Create;
  FUse.Identifier:=aPath;
end;

constructor TWITExchangeIdentifier.Create(aType: TExchangeType; aUse: TWITUsePath);
begin
  Inherited Create(aType,aUse.ToString);
  FUse:=aUse;
  FType:=aType;
end;


destructor TWITExchangeIdentifier.destroy;
begin
  FreeAndNil(FUse);
  inherited destroy;
end;

function TWITExchangeIdentifier.ToString: string;

begin
  Result:=UseDef.ToString+';';
  if FType=xtImport then
    Result:='import '+Result
  else
    Result:='export '+Result
end;

{ TWITExportFunc }

constructor TWITExchangeFunc.Create(aType : TExchangeType; const aName: string; aFunc: TWITFunctionType);
begin
  inherited create(aType,aName);
  FTypeDef:=aFunc;
end;

destructor TWITExchangeFunc.Destroy;
begin
  FreeAndNil(FTypeDef);
  inherited Destroy;
end;

{ TWITExportInterface }

constructor TWITExchangeInterface.Create(aType: TExchangeType; const aName: string; aIntf: TWITInterface);
begin
  Inherited create(aType,aName);
  FTypeDef:=aIntf;
end;

destructor TWITExchangeInterface.Destroy;
begin
  FreeAndNil(FTypeDef);
  inherited Destroy;
end;

{ TWITWorld }

constructor TWITWorld.Create(const Name: string);
begin
  Inherited Create;
  FName := Name;
  FImports := TWITImportList.Create(True);
  FExports := TWITExportList.Create(True);
//  FInterfaces := TWITInterfaceList.Create(True);
  FTypeDefs := TWITTypeDefList.Create(True);
  FUses := TWITInterfaceUseList.Create(True);
  FIncludes := TWITIncludeList.Create(True);
end;

destructor TWITWorld.Destroy;
begin
  FreeAndNil(FIncludes);
  FreeAndNil(FTypeDefs);
//  FreeAndNil(FInterfaces);
  FreeAndNil(FImports);
  FreeAndNil(FExports);
  FreeAndNil(FUses);
  inherited Destroy;
end;

function TWITWorld.ToString: string;

  procedure AddToResult(aLine : string);

  begin
    if Result<>'' then
      Result:=Result+sLineBreak;
    Result:=Result+'  '+aLine;
  end;

var
  lEx : TWITExchange;
  lUse : TWITBaseUse;
  lType : TWITTypeDef;
  lInclude : TWITInclude;


begin
  Result:='';
  For lInclude in Includes do
    AddToResult(lInclude.ToString);
  For lUse in UsesList do
    AddToResult(lUse.ToString);
  for lType in TypeDefs do
    AddToResult(lType.ToString);
  For lEx in Imported do
    AddToResult(lEx.ToString);
  For lEx in Exported do
    AddToResult(lEx.ToString);

  if Result<>'' then
    Result:=sLineBreak+Result+sLineBreak;
  Result:='world '+Name+' {'+Result+'}';
end;

procedure TWITWorld.AddImport(Import: TWITExchange);
begin
  FImports.Add(Import);
end;

procedure TWITWorld.AddExport(Export: TWITExchange);
begin
  FExports.Add(Export);
end;

procedure TWITWorld.AddInclude(aInclude: TWITInclude);
begin
  FIncludes.add(aInclude);
end;


procedure TWITWorld.AddUses(aUses: TWITUse);
begin
  FUses.add(aUses);
end;

procedure TWITWorld.AddTypeDef(aTypeDef: TWITTypeDef);
begin
  FTypeDefs.Add(aTypeDef);
end;


{ TWITBaseUse }

constructor TWITBaseUse.Create;
begin
  Inherited;
  FPath:=TWITUsePath.Create;
  FRename := '';
end;

destructor TWITBaseUse.Destroy;
begin
  FPath.Free;
  inherited Destroy;
end;

function TWITBaseUse.ToString: string;
begin
  Result:=FPath.ToString;
  if Rename<>'' then
    Result:=Result+' as '+Rename;
  Result:='use '+Result+';';
end;

procedure TWITBaseUse.AddNamespace(const Namespace: string);
begin
  FPath.AddNamespace(Namespace);
end;

{ TWitUseItem }

constructor TWitUseItem.Create(const aName: string; const aAlias: String);
begin
  inherited create;
  FName:=aName;
  FAlias:=aAlias;
end;

function TWitUseItem.ToString: string;
begin
  Result:=Name;
  if Alias<>'' then
    Result:=Result+' as '+Alias;
end;

{ TWITUse }

constructor TWITUse.create;
begin
  inherited create;
  FItems:=TWITUseItemList.Create(True);
end;

destructor TWITUse.destroy;
begin
  FreeAndNil(FITems);
  inherited destroy;
end;

function TWITUse.ToString: string;
var
  lItem : TWITUseItem;
begin
  result:='';
  for lItem in FItems do
    begin
    if Result<>'' then
      Result:=Result+', ';
    Result:=Result+lItem.ToString;
    end;
  Result:='use '+FPath.ToString+'.{'+Result+'};';
end;

procedure TWITUse.AddItem(aItem: TWITUseItem);
begin
  FItems.Add(aItem);
end;

procedure TWITUse.AddItem(const aName: string; aAlias: string);
begin
  AddItem(TWITUseItem.Create(aName,aAlias));
end;

{ TWitIncludeItem }

constructor TWitIncludeItem.Create(const aName: string; const aAlias: String = '');
begin
  Inherited Create;
  FName:=aName;
  FAlias:=aAlias;
end;

function TWitIncludeItem.ToString: string;
begin
  Result:=Name;
  if Alias<>'' then
    Result:=Result+' as '+Alias;
end;

constructor TWITInclude.create();
begin
  inherited Create;
  FPath:=TWITUsePath.Create;
  FItems:=TWitIncludeItemList.Create(True);
end;

destructor TWITInclude.destroy;
begin
  FreeAndNil(FPath);
  FreeAndNil(FItems);
  inherited destroy;
end;

function TWITInclude.ToString: string;
var
  I : integer;
begin
  Result:='';
  For I:=0 to Items.Count-1 do
    begin
    if Result<>'' then
      Result:=Result+' , ';
    Result:=Result+Items[i].ToString;
    end;
  if Result<>'' then
    Result:=' with {'+Result+'}'
  else
    Result:=';';
  Result:='include '+Path.ToString+Result;
end;

function TWITInclude.AddItem(const aItem: String; aAlias: String): TWitIncludeItem;
begin
  Result:=TWitIncludeItem.Create(aItem,aAlias);
  FItems.Add(Result);
end;

end.
