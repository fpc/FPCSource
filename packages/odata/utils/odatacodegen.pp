unit odatacodegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pastree, restcodegen, inifiles;

Type
  EEDMX2PasConverter = Class(Exception);

  // Extra set of keywords to take into account when cleaning a property name.
  TExtraKeyWords = (ekwNone,ekwObject,ekwEntity,ekwEntitySet,ekwEntityContainer,ekwservice);

  TODataVersion = (ODataV2,ODataV4);
  TEnumerationMode = (emScoped,emPrefixTypeName,emPlain);

  { TPropertySetter }
  TPropertyFlag = (pfRequired,pfNavigation, pfIndexed, pfReadOnly, pfNeedSetter, pfNeedGetter, pfInkey);
  TPropertyFlags = Set of TPropertyFlag;

  TResultType = (rtNone,rtSimple,rtObject,rtArraySimple,rtArrayObject);

  // Specialized TPasElement classes.
  // Using these tells the code generator what kind of code it must generate for an identifier.
  TPropertySetter = Class(TPasProcedure)
  private
    FProp: TPasElement;
  Public
    Property TheProperty : TPasElement Read FProp Write FProp;
  end;
  TPropertyGetter = Class(TPasFunction)
  private
    FProp: TPasElement;
  Public
    Property TheProperty : TPasElement Read FProp Write FProp;
  end;
  TGetRestKind = Class(TPasProcedure);
  TObjectRestKind = Class(TPasClassFunction);
  TExportPropertyName = class(TPasClassFunction);
  TCreateContainer = Class(TPasFunction);
  TCreateEntitySet = Class(TPasFunction);
  TEntityClassFunction = Class(TPasClassFunction);
  TGetNavigationProperty = Class(TPasFunction);
  TGetSingleton = Class(TPasFunction);
  TGetContainedSingleton = Class(TPasFunction);
  TKeyAsURLPart = Class(TPasFunction);
  TEntityMethod = Class(TPasFunction);
  TSetArrayLength = Class(TPasProcedure);
  TGetStream = Class(TPasProcedure);
  TSetStream = Class(TPasProcedure);
  TBoundFunction = Class(TPasFunction);
  TBoundActionProc = Class(TPasProcedure);
  TBoundActionFunc = Class(TPasFunction);
  TUnBoundFunction  = Class(TPasFunction)
  private
    FPath: STring;
  Public
    Property ExportPath : STring Read FPath Write FPath;
  end;
  TUnBoundActionFunc  = Class(TPasFunction)
  private
    FPath: STring;
  Public
    Property ExportPath : STring Read FPath Write FPath;
  end;
  TUnBoundActionProc  = Class(TPasProcedure)
  private
    FPath: STring;
  Public
    Property ExportPath : STring Read FPath Write FPath;
  end;
  TEntityGet = Class(TEntityMethod);
  TEntityList = Class(TEntityMethod);
  TEntityListAll = Class(TEntityList);
  TEntityPut = Class(TEntityMethod);
  TEntityPatch = Class(TEntityMethod);
  TEntityPost = Class(TEntityMethod);
  TEntityDelete = Class(TEntityMethod);
  TServiceClass = Class(TPasClassType);
  TComplexClass = Class(TPasClassType);
  TEntityClass = Class(TPasClassType);
  TEntityContainerClass = Class(TPasClassType);
  TEntitySetClass = Class(TPasClassType);

  { TODataCodeGenerator }

  TODataCodeGenerator = class(TRestCodeGenerator)
  private
    FAliases: TStrings;
    FBaseComplexType: String;
    FBaseEntityContainerType: String;
    FBaseEntitySetType: String;
    FBaseEntityType: String;
    FBaseServiceType: String;
    FEnumerationMode: TEnumerationMode;
    FFieldPrefix: String;
    FSchemaAncestor: String;
    FServiceSuffix: String;
    FReservedWords : TStringList;
    FIdentifierMap : TStrings;
    procedure SetAliases(AValue: TStrings);
    function GetReservedWords: TStrings;
    procedure SetReservedWords(AValue: TStrings);
  Protected
    procedure EmitOptions; virtual;
    function ConvertTypeToStringExpr(const ExprName, ExprType: String): String;
    Function GetResultType(Const aType: String; Out AElementType : String): TResultType;
    function GetBaseClassName(El: TPasClassType): String;
    Procedure RegisterBaseTypes; virtual;
    function IsSimpleType(const aType: String): Boolean;
    function FlattenName(const AName: String): String;
    procedure WriteProcedureDecl(P: TPasProcedure);
    function CleanPropertyName(const AName: String; UseExtra: TExtraKeyWords): string;
    function CleanPropertyName(const AName: UnicodeString; UseExtra: TExtraKeyWords): string;
    Function CountProperties(C: TPasClassType): Integer;
    Property IdentifierMap : TStrings Read FIdentifierMap;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Class Function WTOA(Const S : UnicodeString) : String;
    Function is26Only(P: TPasProcedure): Boolean;
    Function BaseUnits : String; override;
    Class function IndentStrings(S: TStrings; aindent: Integer): string;
    Class Function ODataVersion : TODataVersion; virtual; abstract;
  Published
    Property BaseComplexType : String Read FBaseComplexType Write FBaseComplexType;
    Property BaseEntityType : String Read FBaseEntityType Write FBaseEntityType;
    Property BaseEntityContainerType : String Read FBaseEntityContainerType Write FBaseEntityContainerType;
    Property BaseServiceType : String Read FBaseServiceType Write FBaseServiceType;
    Property BaseEntitySetType : String Read FBaseEntitySetType Write FBaseEntitySetType;
    Property Aliases : TStrings Read FAliases Write SetAliases;
    Property SchemaAncestor : String Read FSchemaAncestor Write FSchemaAncestor;
    Property FieldPrefix: String Read FFieldPrefix Write FFieldPrefix;
    Property ServiceSuffix : String Read FServiceSuffix Write FServiceSuffix;
    property EnumerationMode : TEnumerationMode Read FEnumerationMode Write FEnumerationMode;
    Property ReservedWords : TStrings Read GetReservedWords Write SetReservedWords;

  end;

implementation

{ TODataCodeGenerator }

procedure TODataCodeGenerator.SetAliases(AValue: TStrings);
begin
  if FAliases=AValue then Exit;
  FAliases.Assign(AValue);
end;

constructor TODataCodeGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BaseClassName:='TODataObject';
  BaseComplexType:='TODataObject';
  BaseEntityType:='TODataEntity';
  BaseEntityContainerType:='TODataEntityContainer';
  BaseServiceType:='TODataService';
  BaseEntitySetType:='TODataEntitySet';
  SchemaAncestor:='TObject';
  FieldPrefix:='F';
  ServiceSuffix:='_';
  FAliases:=TStringlist.Create;
  FIdentifierMap:=THashedStringList.Create;
end;

destructor TODataCodeGenerator.destroy;
begin
  FreeAndNil(FAliases);
  FreeAndNil(FReservedWords);
  FreeAndNil(FIdentifierMap);
  Inherited;
end;

function TODataCodeGenerator.BaseUnits: String;
begin
  Result:='fpjson, restbase, odatabase, odataservice';
end;

function TODataCodeGenerator.GetReservedWords: TStrings;
begin
  if (FReservedWords=Nil) then
    begin
    FReservedWords:=TStringList.Create;
    FReservedWords.Sorted:=True;
    end;
  Result:=FReservedWords;
end;

procedure TODataCodeGenerator.SetReservedWords(AValue: TStrings);

begin
  if AValue=FReservedwords then exit;
  if AValue.Count=0 then
    FreeAndNil(FReservedWords)
  else
    ReservedWords.Assign(AValue);
end;

class function TODataCodeGenerator.WTOA(const S: UnicodeString): String;
begin
  Result:=AnsiString(S);
end;

function TODataCodeGenerator.is26Only(P: TPasProcedure): Boolean;

begin
  Result:=P is TSetArrayLength;
end;

class function TODataCodeGenerator.IndentStrings(S: TStrings; aindent: Integer
  ): string;

Var
  I,CurrLen,CurrPos : Integer;


begin
  Result:='';
  CurrLen:=0;
  CurrPos:=0;
  For I:=0 to S.Count-1 do
    begin
    CurrLen:=Length(S[i]);
    If (CurrLen+CurrPos)>72 then
      begin
      Result:=Result+LineEnding+StringOfChar(' ',aIndent);
      CurrPos:=aIndent;
      end;
    Result:=Result+S[i];
    CurrPos:=CurrPos+CurrLen;
    end;
end;

procedure TODataCodeGenerator.WriteProcedureDecl(P : TPasProcedure);

Var
  S : TStrings;
  R: TPasResultElement;
  T : String;
  B : Boolean;

begin
  S:=TStringList.Create;
  try
    S.Add(P.TypeName+' '+P.Name);
    P.ProcType.GetArguments(S);
    if P.ProcType.InheritsFrom(TPasFunctionType) then
      If Assigned(TPasFunctionType(P.ProcType).ResultEl) then
        begin
        R:=TPasFunctionType(P.ProcType).ResultEl;
        T:=' : ';
        If (R.ResultType.Name<>'') then
          T:=T+R.ResultType.Name
        else
          T:=T+R.ResultType.GetDeclaration(False);
        S.Add(T);
        end;
    P.GetModifiers(S);
    B:=Is26Only(P);
    if B then
      AddLn('{$IFDEF VER2_6}');
    AddLn(IndentStrings(S,Length(S[0]))+';');
    if B then
      AddLn('{$ENDIF VER2_6}');
  finally
    S.Free;
  end;
end;

function TODataCodeGenerator.CleanPropertyName(const AName: String;
  UseExtra: TExtraKeyWords): string;
Const
   // Pascal keywords
   KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
       'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
       'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
       'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'+
       'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
       'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
       'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
       'private;published;length;setlength;result;';
   // Reserved words (methods)
   RWComponent = ';post;component;name;notification;componentcount;';
   RWOdataObject = 'destroy;loadPropertyfromjson;makekeystring;allowadditionalproperties;odataannotations;odataannotationvalues;odataannotationcount;';
   RWEntity = 'baseurl;keyasurlpart;delete;basepath;post;put;patch;';
   RWEntitySet = RWComponent+'getbaseurl;checkservice;checkcontainer;notification;getsingle;getmulti;containerurl;containedpath;service;objectrestkind;entityclass;getservice;container;';
   RWEntityContainer = RWComponent+'checkservice;objectrestkind;entitycontainername;defaultservice;createentityset;service;';
   RWService = RWComponent+'dolog;composeurl;service;jsontoodataerror;resptoerror;objectrestkind;servicename;registerservice;registerentitycontainers;addtoquery;'+
              'queryparamstostring;servicecall;getstream;setstream;arrayservicecall;getmulti;createentitycontainer;getentityclass;onlog;webclient;serviceurl;apineedsauth;'+
              'odatarequestheaders;lastresponseheaders;odatametadata;';


Var
  I : Integer;
  RW : String;

begin
  Result:=Aname;
  For I:=Length(Result) downto 1 do
    If Not ((Upcase(Result[i]) in ['_','A'..'Z'])
             or ((I>1) and (Result[i] in (['0'..'9'])))) then
     Delete(Result,i,1);
  if Pos(';'+lowercase(Result)+';',KW)<>0 then
    Result:='_'+Result;
  if UseExtra<>ekwNone then
    begin
    Case useExtra of
      ekwObject : RW:=RWOdataObject;
      ekwEntity : RW:=RWEntity;
      ekwEntitySet : RW:=RWEntitySet;
      ekwEntityContainer : RW:=RWEntityContainer;
      ekwservice : RW:=RWService;
    end;
    if Pos(';'+lowercase(Result)+';',RW)<>0 then
      Result:='_'+Result;
    if Assigned(FReservedWords) then
       if FReservedWords.IndexOf(Result)<>-1 then
         Result:='_'+Result;
    end;
end;

function TODataCodeGenerator.CleanPropertyName(const AName: UnicodeString;
  UseExtra: TExtraKeyWords): string;
begin
  Result:=CleanpropertyName(WTOA(AName),UseExtra);
end;

function TODataCodeGenerator.FlattenName(const AName: String): String;

begin
  Result:=StringReplace(AName,'.','_',[rfReplaceAll]);
end;

function TODataCodeGenerator.IsSimpleType(const aType: String): Boolean;

begin
  Case LowerCase(aType) of
    'boolean': Result:=True;
    'byte' : Result:=True;
    'tsbyte': Result:=True;
    'shortint': Result:=True;
    'int16': Result:=True;
    'smallint': Result:=True;
    'word': Result:=True;
    'int32': Result:=True;
    'integer': Result:=True;
    'cardinal': Result:=True;
    'dword': Result:=True;
    'int64': Result:=True;
    'qwordl': Result:=True;
    'tint16': Result:=True;
    'tint32': Result:=True;
    'tint64': Result:=True;
    'string': Result:=True;
    'guidstring': Result:=True;
    'tguidstring': Result:=True;
    'double': Result:=True;
    'single': Result:=True;
  else
    Result:=False;
  end;
end;

procedure TODataCodeGenerator.RegisterBaseTypes;

Const
  TypeCount = 68;

  BaseTypes : Array[1..TypeCount,1..2] of String =
  (('Edm.Byte','Byte'), ('Collection(Edm.Byte)','TByteArray'),
   ('Edm.SByte','TSByte'), ('Collection(Edm.SByte)','TShortintArray'),
   ('Edm.int16','TInt16'), ('Collection(Edm.int16)','TInt16Array'),
   ('Edm.int32','TInt32'), ('Collection(Edm.int32)','TInt32Array'),
   ('Edm.int64','int64'), ('Collection(Edm.int64)','TInt64Array'),
   ('Edm.string','string'), ('Collection(Edm.string)','TStringArray'),
   ('Edm.Guid','TGUIDString'), ('Collection(Edm.guid)','TGuidStringArray'),
   ('Edm.Duration','TDuration'), ('Collection(Edm.Duration)','TStringArray'),
   ('Edm.Boolean','boolean'), ('Collection(Edm.boolean)','TBooleanArray'),
   ('Edm.Date','TDate'), ('Collection(Edm.Date)','TDateArray'),
   ('Edm.DateTime','TDateTime'), ('Collection(Edm.DateTime)','TDateTimeArray'),
   ('Edm.Time','TTime'), ('Collection(Edm.Time)','TTimeArray'),
   ('Edm.TimeOfDay','TTimeOfDay'), ('Collection(Edm.TimeOfDay)','TTimeOfDayArray'),
   ('Edm.DateTimeOffset','TDateTime'), ('Collection(Edm.DateTimeOffcset)','TDateTimeArray'),
   ('Edm.Decimal','double'), ('Collection(Edm.Decimal)','TDoubleArray'),
   ('Edm.Double','Double'), ('Collection(Edm.Double)','TDoubleArray'),
   ('Edm.Single','Single'), ('Collection(Edm.Single)','TSingleArray'),
   ('Edm.Binary','TBinary'), ('Collection(Edm.Binary)','TBinaryArray'),
   ('Edm.Stream','TStream'), ('Collection(Edm.Stream)','TByteArrayArray'),
   ('Edm.Geography','TGeography'), ('Collection(Edm.Geography)','TGeographyArray'),
   ('Edm.GeographyPoint','TGeographyPoint'), ('Collection(Edm.GeographyPoint)','TGeographyPointArray'),
   ('Edm.GeographyPolygon','TGeographyPolygon'), ('Collection(Edm.GeographyPolygon)','TGeographyPolygonArray'),
   ('Edm.GeographyLineString','TGeographyLineString'), ('Collection(Edm.GeographyLineString)','TGeographyLineStringArray'),
   ('Edm.GeographyMultiPoint','TGeographyMultiPoint'), ('Collection(Edm.GeographyMultiPoint)','TGeographyMultiPointArray'),
   ('Edm.GeographyMultiString','TGeographyMultiLineString'), ('Collection(Edm.GeographyMultiLineString)','TGeographyMultiLineStringArray'),
   ('Edm.GeographyMultiPolygon','TGeographyMultiPolygon'), ('Collection(Edm.GeographyMultiPolygon)','TGeographyMultiPolygonArray'),
   ('Edm.Geometry','TGeometry'), ('Collection(Edm.Geometry)','TGeometryArray'),
   ('Edm.GeometryPoint','TGeometryPoint'), ('Collection(Edm.GeometryPoint)','TGeometryPointArray'),
   ('Edm.GeometryPolygon','TGeometryPolygon'), ('Collection(Edm.GeometryPolygon)','TGeometryPolygonArray'),
   ('Edm.GeometryLineString','TGeometryLineString'), ('Collection(Edm.GeometryLineString)','TGeometryLineStringArray'),
   ('Edm.GeometryMultiPoint','TGeometryMultiPoint'), ('Collection(Edm.GeometryMultiPoint)','TGeometryMultiPointArray'),
   ('Edm.GeometryMultiString','TGeometryMultiLineString'), ('Collection(Edm.GeometryMultiLineString)','TGeometryMultiLineStringArray'),
   ('Edm.GeometryMultiPolygon','TGeometryMultiPolygon'), ('Collection(Edm.GeometryMultiPolygon)','TGeometryMultiPolygonArray'),
   ('Edm.GeographyCollection','TGeographyArray'), ('Edm.GeometryCollection','TGeometryArray')
  );


Var
  I : integer;
begin
  For I:=1 to TypeCount do
     FIdentifierMap.Add(LowerCase(BaseTypes[I,1])+'='+BaseTypes[I,2]);
end;

function TODataCodeGenerator.GetBaseClassName(El: TPasClassType): String;

begin
  Result:='';
  if Assigned(EL.AncestorType) then
    Result:=EL.AncestorType.Name;
  if (Result='') then
    begin
    if EL.InheritsFrom(TServiceClass) then
      Result:=BaseServiceType
    else if EL.InheritsFrom(TEntityContainerClass) then
      Result:=BaseEntityContainerType
    else if EL.InheritsFrom(TEntitySetClass) then
      Result:=BaseEntitySetType
    else if EL.InheritsFrom(TEntityClass) then
      Result:=BaseEntityType
    else if EL.InheritsFrom(TComplexClass) then
      Result:=BaseComplexType
    else
      Result:=BaseClassName;
    end;
end;

function TODataCodeGenerator.CountProperties(C: TPasClassType): Integer;

Var
  I : Integer;

begin
  Result:=0;
  While (C<>Nil) do
    begin
    For I:=0 to C.Members.Count-1 do
     If TObject(C.Members[i]) is TPasProperty then
       Inc(Result);
    if C.AncestorType is TPasClassType then
      C:=C.AncestorType as TPasClassType
    else
      C:=Nil;
    end;
end;

function TODataCodeGenerator.GetResultType(const aType: String; out
  AElementType: String): TResultType;

Var
  P : Integer;
  EN : String;

begin
  P:=Pos('array',lowercase(aType));
  if (aType='') then
    Result:=rtNone
  else if IsSimpleType(AType) then
    Result:=rtSimple
  else if P>0 then
    begin
    AElementType:=Copy(atype,1,P-1);
    EN:=AElementType;
    if (EN<>'') and (EN[1]='T') then
      Delete(EN,1,1);
    if IsSimpleType(EN) then
      begin
      Result:=rtArraySimple;
      AElementType:=EN;
      end
    else
      Result:=rtArrayObject;
    end
  else
    Result:=rtObject;
end;

function TODataCodeGenerator.ConvertTypeToStringExpr(const ExprName,
  ExprType: String): String;

begin
  Case LowerCase(ExprType) of
    'boolean' : Result:='BoolToStr('+ExprName+',''true'',''false'')';
    'byte' : Result:='IntToStr('+ExprName+')';
    'tsbyte': Result:='IntToStr('+ExprName+')';
    'int16': Result:='IntToStr('+ExprName+')';
    'int32': Result:='IntToStr('+ExprName+')';
    'int64': Result:='IntToStr('+ExprName+')';
    'tint16': Result:='IntToStr('+ExprName+')';
    'tint32': Result:='IntToStr('+ExprName+')';
    'tint64': Result:='IntToStr('+ExprName+')';
    'string': Result:='TODataObject.MakeKeyString('+ExprName+')';
    'tguidstring': Result:='TODataObject.MakeKeyString('+ExprName+')';
    'tdatetime': Result:='FormatDateTime(''yyyy-mm-dd"T"hhmmss'','+ExprName+')';
    'double': Result:='FloatToStr('+ExprName+')';
    'single': Result:='FloatToStr('+ExprName+')';
    'tbinary' :  Result:='BinaryToString('+ExprName+')';
  else
    Raise EEDMX2PasConverter.CreateFmt('GET : Unsupported key type "%s" for %s',[ExprType,ExprName]);
  end;
end;

procedure TODataCodeGenerator.EmitOptions;

Var
  I : Integer;
  S : String;


begin
  Addln('(*');
  IncIndent;
  Addln('Options used to generate: ');
  Str(ODataVersion,S);
  Addln('OData version : '+S);
  Addln('BasecomplexType : '+BaseComplexType);
  Addln('BaseEntityType : '+BaseEntityType);
  Addln('BaseEntityContainerType : '+BaseEntityContainerType);
  Addln('BaseServiceType : '+BaseServiceType);
  Addln('BaseEntitySetType : '+BaseEntitySetType);
  For I:=0 to Aliases.Count-1 do
   Addln('Aliases[%d] : %s',[i,Aliases[i]]);
  Addln('SchemaAncestor : '+SchemaAncestor);
  Addln('FieldPrefix : '+FieldPrefix);
  Addln('ServiceSuffix : '+ServiceSuffix);
  Str(EnumerationMode,S);
  Addln('EnumerationMode : '+S);
  decIndent;
  Addln('*)');
end;


end.

