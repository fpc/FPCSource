{$ifdef USECSDL}
unit csdl2pas;
{$ELSE}
unit edmx2pas;
{$ENDIF}

{$mode objfpc}{$H+}

interface


uses
  typinfo, Classes, contnrs, SysUtils, restcodegen, odatacodegen,
  {$IFDEF USECSDL} csdl, {$ELSE} edm, {$ENDIF} pastree, base_service_intf, xml_serializer;

Const
  IndexShift = 3; // Number of bits reserved for flags.

Type
  {$IFNDEF USECSDL}
  // EDM type names
  TSchema = Schema;
  EntityContainer = TEntityContainer;
  TComplexTypeProperty = TProperty;
  TEntityProperty = TProperty;
  {$ELSE}
  TEntitySet = EntityContainer_EntitySet_Type;
  TEntityType_KeyArray = TEntityKeyElement;
  TFunctionImport = EntityContainer_FunctionImport_Type;
  {$ENDIF}

  { TImplicitEntitySet }

  TImplicitEntitySet = CLass(TEntitySet)
  private
    FIsColl: Boolean;
    FNavigationProperty: TNavigationProperty;
  Public
    Constructor Create(AProperty : TNavigationProperty; ATypeName : String; AIsColl : Boolean); reintroduce;
    Property NavigationProperty : TNavigationProperty Read FNavigationProperty;
    Property IsColl : Boolean Read FIsColl;
  end;

  { TIdentifier }

  TIdentifier = Class(TObject)
  private
    FEL: TPasElement;
    FIsEntitySet: Boolean;
    FName: String;
    FSchema: TSchema;
  Public
    Constructor Create(Const AName : String; ASchema : TSchema; El : TPasElement);
    Destructor Destroy; override;
    Property IdentifierName : String Read FName;
    Property Schema : TSchema Read FSchema;
    Property Element : TPasElement Read FEL;
    Property IsEntitySet : Boolean Read FIsEntitySet Write FIsEntitySet;
  end;


  { TEDMX2PasConverter }

  TEDMX2PasConverter = Class(TODataCodeGenerator)
  private
    FXML: TStream;
    FFreeObjects : TFPObjectList;
    FSchemaList : TFPObjectList;
    FIdentifierList : TFPObjectList;
    FIdentifierHash : TFPObjectHashTable;
  Protected
    // Identifier management
{$IFDEF USECSDL}
    Function FindAssociatedTypeInSchema(ASchema: TSchema; Const ARelation, ARole: String): String;
    Function FindAssociatedType(Var APreferredSchema: TSchema; Const ARelation, ARole: String): String;
{$ENDIF}
    function UseExtraIdentifierProtection(D: TObject): TExtraKeywords;
    Function ExtractBaseTypeName(ASchema: TSchema; ATypeName: String; Out IsColl: Boolean): String;
    Function ExtractBaseTypeName(ASchema: TSchema; ATypeName: UnicodeString; Out IsColl: Boolean): String;
    Function FindEntitySetForEntity(ASchema: TSchema; AName: String): TIdentifier;
    Function FindProperty(C: TPasClassType; APropertyName: String): TEntityProperty;
    Function FindProperty(C: TPasClassType; APropertyName: UnicodeString): TEntityProperty;
    Function GetEntityKey(C: TPasClassType): TEntityType_KeyArray;
    Function FindQualifiedIdentifier(AName: String): TIdentifier;
    Function FindIdentifier(ASchema : TSchema; AName: String): TIdentifier;
    Function FindIdentifier(ASchema : TSchema; AName: UnicodeString): TIdentifier;
    Function GetNameSpace(ASchema: TSchema): String;
    Function GetNativeTypeName(O: TObject): String;
    Function NeedWriteSetter(P: TComplexTypeProperty): Boolean;
    Function ResolveNameSpace(ASchema: TSchema; ATypeName: String): String;
    Function ResolveType(ASchema: TSchema; Const ATypeName: String): TPasType;
    Function ResolveType(ASchema: TSchema; Const ATypeName: UnicodeString): TPasType;
    // EDMX
    // Identifier generation
    procedure SchemaToIdentifiers;virtual;
    Procedure AddIdentifier(AIDentifier : TIdentifier);
    Function AddIdentifier(Const AName : String; ASchema : TSchema; El : TPasElement) : TIdentifier;
    Function AddIdentifier(Const AName : UnicodeString; ASchema : TSchema; El : TPasElement) : TIdentifier;
    procedure EntityContainerToIdentifiers(ASchema: TSchema; EC: EntityContainer);virtual;
    Procedure CompleteIdentifiers;virtual;
    Procedure GenerateBaseClass(ID: TIDentifier);virtual;
    Procedure CheckNavigationPropertyEntity(ASchema: TSchema; AEntity: TEntityType);virtual;
    Procedure AddExportPropertyName(ID: TIdentifier);virtual;
    Procedure AddContainerToSchema(ID: TIdentifier; AIndex: Integer; E: EntityContainer);virtual;
    procedure AddEntitySet(ID: TIDentifier; ES: TEntitySet; AIndex : Integer);virtual;
    Procedure AddEntityGet(ID, EID: TIdentifier);virtual;
    Procedure AddEntityList(ID: TIdentifier; ArgType: String; ListAll: Boolean);virtual;
    Function AddGetStream(ID: TIDentifier): TGetStream;
    Function AddSetStream(ID: TIDentifier): TSetStream;
    Function AddGetKeyAsURLPart(ID: TIdentifier; Key: TEntityKeyElement ): TPasFunction;virtual;
    function CreateIdentifierName(ASchema: TSchema; const APrefix, AName: String): String;virtual;
    function CreateIdentifierName(ASchema: TSchema; const APrefix, AName: UnicodeString): String;virtual;
    function CreateIdentifierName(ASchema: TSchema; const APrefix : String; AName: UnicodeString): String;virtual;
    Function CreatePropertyGetter(AParent: TPasElement; PN: String; indexed: Boolean; T: TPasType): TPropertyGetter;virtual;
    Function CreatePropertySetter(AParent: TPasElement; PN: String; indexed: Boolean; T: TPasType): TPropertySetter;virtual;
    // Return true if the actual property name differs from the property name in the Edm
    Function AddProperty(ID: TIdentifier; APropertyIndex : integer; Const APropertyName, APropertyType: String; Flags: TPropertyFlags; ACustomData : TObject) : Boolean;virtual;
    Function AddNavigationProperty(ID: TIDentifier; P: TNavigationProperty): TPasFunction;virtual;
    procedure AddImportFunction(ID: TIdentifier; AFun: TFunctionImport);
    {$IFNDEF USECSDL}
    procedure AddImportAction(ID : TIdentifier; Act : TActionImport; AIndex : Integer);
    Function  AddUnboundFunction(ID : TIdentifier; APath : String; Fun : TFunction; AIndex : Integer) : TPasFunction;
    Function  CheckBoundFunction(ASchema: TSchema; Fun: TFunction): TPasFunction;
    Function  AddUnboundAction(ID : TIdentifier; APath : String; Act : TAction; AIndex : integer) : TPasProcedure;
    Function  CheckBoundAction(ASchema: TSchema; Act: TAction): TPasProcedure;
    procedure AddSingleTon(ID: TIDentifier; S: TSingleton; AIndex : integer);virtual;
    {$ENDIF}
    Procedure AddSetArrayLength(ID: TIdentifier); virtual;
    procedure CompleteContainer(ID: TIdentifier);virtual;
    Procedure CompleteEnumerator(ID: TIdentifier);virtual;
    Procedure CompleteComplexType(ID: TIdentifier);virtual;
    Procedure CompleteEntityType(ID: TIdentifier);virtual;
    Procedure CompleteEntitySet(ID: TIdentifier);virtual;
    procedure CompleteSchema(ID: TIdentifier);virtual;
    // Code generation
    procedure EmitInterface;virtual;
    procedure EmitImplementation;virtual;
    procedure EmitForwardDeclaration;virtual;
    procedure EmitEnumTypes;virtual;
    procedure EmitClassDeclarations;virtual;
    procedure EmitClassDeclaration(ID : TIDentifier);virtual;
    procedure EmitClassImplementation(ID : TIDentifier);virtual;
    procedure EmitClassDeclarationSection(El: TPasClassType;  V: TPasMemberVisibility);virtual;
    Procedure EmitMethodHeader(AClassName, AMethodName: String; PT: TPasProcedureType; RT: String);
    procedure EmitObjectRestKind(CT: TPasClassType; R: TObjectRestKind);virtual;
    procedure EmitGetSingleton(CT: TPasClassType; S: TGetSingleTon);virtual;
    procedure EmitGetKeyAsURLPart(CT: TPasClassType; ASchema : TSchema; P: TKeyAsURLPart);virtual;
    procedure EmitPropertySetter(Const CN: String; P: TPropertySetter);virtual;
    procedure EmitPropertyGetter(Const CN: String; P: TPropertyGetter);virtual;
    procedure EmitCreateContainer(Const CN: String; CC: TCreateContainer);virtual;
    procedure EmitCreateEntitySet(Const CN: String; CE: TCreateEntitySet);virtual;
    Procedure EmitGetStream(Const CN: String; G: TGetStream);virtual;
    Procedure EmitSetStream(Const CN: String; G: TSetStream);virtual;
    Procedure EmitSetArrayLength(CT : TPasClassType; A : TSetArrayLength); virtual;
    {$IFNDEF USECSDL}
    Procedure EmitFunctionCall(ServiceName,ReturnType : String; ResultType : TResultType);
    Procedure EmitMethodPath(PT: TPasProcedureType; MethodPath : String; GlobalService : Boolean);
    Procedure EmitPreparePostObject(Act: TPasProcedure; ActionPath : String; GlobalService,AllocateArray : Boolean);
    Procedure EmitBoundFunction(CT: TPasClassType; ASchema : TSchema; Fun: TBoundFunction);virtual;
    Procedure EmitBoundAction(CT: TPasClassType; ASchema : TSchema; Act: TPasProcedure);virtual;
    Procedure EmitUnBoundFunction(CT: TPasClassType; Fun: TUnBoundFunction);virtual;
    Procedure EmitUnBoundAction(CT: TPasClassType; Act: TPasProcedure);virtual;
    Procedure EmitActionServiceCall(Const AReturnType,AElementType : String; GlobalService : Boolean; ResultType : TResultType);
    {$endif}
    procedure EmitEntityClassFunction(CT: TPasClassType; ASchema: TSchema; CE: TEntityClassFunction);virtual;
    procedure EmitGetContainedSingleton(CT: TPasClassType; E: TGetContainedSingleton);virtual;
    procedure EmitNavigationProperty(CT: TPasClassType; E: TGetNavigationProperty);virtual;
    procedure EmitExportPropertyName(CT: TPasClassType; E: TExportPropertyName);virtual;
    procedure EmitEntityGet(CT: TPasClassType; E: TEntityGet);virtual;
    procedure EmitEntityList(CT: TPasClassType; E: TEntityList);virtual;
    procedure EmitEntityMethod(CT: TPasClassType; E: TEntityMethod);virtual;
//    Function GetPropertyTypeName(Decl: TDOMELement): String;
    procedure AnalyseXML; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function ODataVersion : TODataVersion; override;
    Procedure LoadFromStream(Const AStream : TStream); override;
    Procedure Execute; override;
  end;

implementation

{ TImplicitEntitySet }

Constructor TImplicitEntitySet.Create(AProperty: TNavigationProperty;
  ATypeName: String; AIsColl: Boolean);
begin
  Inherited Create;
  FNavigationProperty:=AProperty;
  EntityType:=ATypeName;
  FIsColl:=AIsColl;
end;

{ TIdentifier }

Constructor TIdentifier.Create(Const AName: String; ASchema: TSchema;
  El: TPasElement);

Var
  N : String;

begin
  FName:=AName;
  FSchema:=ASchema;
  FEl:=El;
  if (FName='') then
    begin
    if (FSchema=Nil) or (FEl=Nil) then
      Raise EEDMX2PasConverter.Create('No identifier name specified, no element and schema specified');
    N:=GetStrProp(EL.CustomData,'Name');
    if (N='') then
      N:=GetStrProp(EL.CustomData,'TypeName');
    FName:=TODataCodeGenerator.WTOA(FSchema.Namespace)+'.'+N;
    end;
//  Writeln('Identifier '+FName,' created (',El.ClassName,': ',el.Name,')');
end;

Destructor TIdentifier.Destroy;
begin
//  Writeln('Destroying ',FEL.Name,' : ',Fel.RefCount);
//  Flush(output);
  FEl.Release;
  inherited Destroy;
end;



constructor TEDMX2PasConverter.Create(AOwner: TComponent);

begin
  inherited Create(AOWner);
  FFreeObjects:=TFPObjectList.Create(True);
  FSchemaList:=TFPObjectList.Create(True);
  FIdentifierList:=TFPObjectList.Create(True);
  FIdentifierHash:=TFPObjectHashTable.Create(False);
  FXML:=TStringStream.Create('');
end;

destructor TEDMX2PasConverter.Destroy;
begin
  FreeAndNil(FXML);
  FreeAndNil(FSchemaList);
  FreeAndNil(FIdentifierList);
  FreeAndNil(FFreeObjects);
  FreeAndNil(FIdentifierHash);
  inherited Destroy;
end;


procedure TEDMX2PasConverter.EmitClassDeclarations;

Var
  I : Integer;
  ID : TIdentifier;

begin
  For I:=0 to FIdentifierList.Count-1 do
    begin
    ID:=FIdentifierList[i] as TIdentifier;
    if ID.Element.InheritsFrom(TPasClassType) then
      EmitClassDeclaration(ID);
    end;
end;



procedure TEDMX2PasConverter.EmitClassDeclarationSection(El: TPasClassType; V: TPasMemberVisibility);

Var
  I : integer;
  M : TPasElement;
  PP : TPasProperty;
  S : String;

begin
  // Variables (fields);
  For I:=0 to El.Members.Count-1 do
    begin
    M:=TPasElement(El.Members[i]);
    if (M.Visibility=v) and (M.ClassType=TPasvariable) then // Do not use InheritsFrom or Is !!
      AddLn(M.GetDeclaration(True)+';');
    end;
  // Methods
  For I:=0 to El.Members.Count-1 do
    begin
    M:=TPasElement(El.Members[i]);
    if (M.Visibility=v) and (M is TPasProcedure) then
      WriteProcedureDecl(M as TPasProcedure);
    end;
  // Properties
  For I:=0 to El.Members.Count-1 do
    begin
    M:=TPasElement(El.Members[i]);
    if (M.Visibility=v) and (M is TPasProperty) then
      begin
      PP:=M as TPasProperty;
      S:=Format('Property %s : %s',[PP.Name,PP.VarType.Name]);
      if Assigned(PP.IndexExpr) then
        S:=S+Format(' index %s',[(PP.IndexExpr as TPrimitiveExpr).Value]);
      S:=S+Format(' read %s',[PP.ReadAccessorName]);
      if (PP.WriteAccessorName<>'') then
        S:=S+Format(' write %s',[PP.WriteAccessorName]);
      AddLn(S+';');
      end;
    end;
end;

function TEDMX2PasConverter.GetNativeTypeName(O: TObject): String;

begin
  if O.InheritsFrom(TSchema) then
    Result:=WTOA(TSchema(O).Namespace)
  else
    Result:=GetStrProp(O,'Name');
end;

procedure TEDMX2PasConverter.EmitClassDeclaration(ID: TIDentifier);


  Function CountElementsForVisibility(Alist : TFPList; V : TPasMemberVisibility) : integer;

  Var
    I : Integer;

  begin
    Result:=0;
    For I:=0 to AList.Count-1 do
      if TPasElement(AList[I]).Visibility=V then
        Inc(Result);
  end;

Var
  NN,PC,S : String;
  El : TPasClassType;
  Empty : Boolean;
  V : TPasMemberVisibility;

begin
  EL:=ID.Element as TPasClassType;
  NN:=GetNativeTypeName(EL.CustomData);
  ClassHeader(WTOA(ID.Schema.NameSpace)+': '+NN);
  Empty:=not (Assigned(EL.Members) and (EL.Members.Count>0));
  PC:=GetBaseClassName(EL);
  S:=Format('%s = Class(%s)',[EL.Name,PC]);
  if empty then
    S:=S+';';
  AddLn(S);
  if Empty then
    exit;
  for v in TPasMemberVisibility do
    if CountElementsForVisibility(El.Members,V)>0 then
      begin
      if V<>visDefault then
        AddLn(VisibilityNames[v]);
      IncIndent;
      EmitClassDeclarationSection(EL,V);
      DecIndent;
      end;
  Addln('end;');
  AddLn('');
end;

procedure TEDMX2PasConverter.EmitPropertyGetter(const CN: String;
  P: TPropertyGetter);

Var
  TN,FN : String;
  D : TObject;

begin
  TN:=(P.TheProperty as TPasProperty).VarType.Name;
  EmitMethodHeader(CN,P.Name,P.ProcType,TN);
  AddLn('');
  AddLn('begin');
  IncIndent;
  FN:=FieldPrefix+P.TheProperty.Name;
  D:=P.TheProperty.CustomData;
  if (D is EntityContainer)
     or (D is TEntitySet)
     {$IFNDEF USECSDL} OR (D is TSingleton){$ENDIF} then
    begin
    AddLn('If Not Assigned(%s) then',[FN]);
    IncIndent;
    if D is EntityContainer then
      AddLn('%s:=%s(CreateEntityContainer(%s));',[FN,TN,TN])
{$IFNDEF USECSDL}
    else if D is TSIngleton then
      AddLn('%s:=Fetch%s;',[FN,P.TheProperty.Name])
{$ENDIF}
    else if D is TEntitySet then
      AddLn('%s:=%s(CreateEntitySet(%s));',[FN,TN,TN]);
    DecIndent;
    end;
  AddLn('Result:=%s;',[FN]);
  DecIndent;
  AddLn('end;');
  AddLn('');

end;

procedure TEDMX2PasConverter.EmitCreateContainer(const CN: String;
  CC: TCreateContainer);
Var
  TN : String;

begin
  TN:=(CC.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  AddLn('Function %s.%s : %s; ',[CN,CC.Name,TN]);
  SimpleMethodBody([  Format('Result:=%s(CreateEntityContainer(%s));',[TN,TN])]);
end;

procedure TEDMX2PasConverter.EmitCreateEntitySet(const CN: String;
  CE: TCreateEntitySet);
Var
  TN : String;

begin
  TN:=(CE.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  AddLn('Function %s.%s : %s; ',[CN,CE.Name,TN]);
  SimpleMethodBody([  Format('Result:=%s(CreateEntitySet(%s));',[TN,TN])]);
end;

procedure TEDMX2PasConverter.EmitGetStream(const CN: String; G: TGetStream);

Var
  S : String;
  I : Integer;

begin
  EmitMethodHeader(CN,G.Name,G.ProcType,'');
  S:='';
  For i:=0 to G.ProcType.Args.Count-1 do
    begin
    If (S<>'') then
      S:=S+',';
    S:=S+TPasArgument(G.ProcType.Args[i]).Name;
    end;
  SimpleMethodBody([Format('DoGetStream(%s);',[S])]);
end;

procedure TEDMX2PasConverter.EmitMethodHeader(AClassName, AMethodName: String;
  PT: TPasProcedureType; RT: String);

Var
  Args : TStrings;
  I : Integer;
  S : String;

begin
  Args:=TStringList.Create;
  try
    Args.Clear;
    Addln('');
    PT.GetArguments(Args);
    S:='';
    For i:=0 to Args.Count-1 do
      S:=S+Args[i];
    If (RT<>'') then
      AddLn('Function %s.%s%s : %s; ',[AClassName,AMethodName,S,RT])
    else
      AddLn('Procedure %s.%s%s; ',[AClassName,AMethodName,S]);
    Addln('');
  finally
    Args.Free;
  end;
end;

{$IFNDEF USECSDL}
procedure TEDMX2PasConverter.EmitMethodPath(PT: TPasProcedureType;
  MethodPath: String; GlobalService: Boolean);

Var
  FirstIndex,I : Integer;
  AP : TPasArgument;
  KP : String;

begin
  Addln('Var');
  IncIndent;
  AddLn('_Res : String;');
  AddLn('_Path : String;');
  DecIndent;
  Addln('begin');
  IncIndent;
  if GLobalService then
    AddLn('CheckService;');
  FirstIndex:=Ord(Not GlobalService);
  // 0 is service
  For I:=FirstIndex to PT.Args.Count-1 do
    begin
    AP:=TPasArgument(PT.Args[i]);
    KP:=ConvertTypeToStringExpr(AP.Name,AP.argType.Name);
    KP:=''''+TActionFunctionParameter(AP.CustomData).Name+'=''+'+KP; // Do not add spaces !!
    if I>FirstIndex then
      AddLn('_Path:=_Path+'',''+'+KP+';')
    else
      AddLn('_Path:='+KP+';');
    end;
  AddLn('_Path:=''(''+_Path+'')'';');
  AddLn('_Path:='''+MethodPath+'''+_Path;');
  if Not GlobalService then
    AddLn('_Path:=BaseURL(AService)+''/''+_Path;');
end;

procedure TEDMX2PasConverter.EmitFunctionCall(ServiceName, ReturnType: String;
  ResultType: TResultType);

Var
  P : Integer;

begin
  if (ServiceName<>'') then
    ServiceName:=ServiceName+'.';
  if ResultType=rtSimple then
    begin
    Addln('_Res:='+ServiceName+'ServiceCall(''GET'',_Path,'''');');
    ReturnType:=ConvertTypeToStringExpr('_Res',ReturnType);
    Addln('Result:='+ReturnType+';');
    end
  else
    begin
    // Somewhat of a shortcut, need to use ExtractBaseTypeName and ResolveType
    P:=Pos('array',LowerCase(ReturnType));
    if (P<>0) then
      Addln('Result:=%s('+ServiceName+'GetMulti(_Path,'''',%s,True,_Res));',[ReturnType,Copy(ReturnType,1,P-1)])
    else
      Addln('Result:=%s('+ServiceName+'SingleServiceCall(_Path,'''',%s));',[ReturnType,ReturnType])
    end;
end;

procedure TEDMX2PasConverter.EmitBoundFunction(CT: TPasClassType;
  ASchema: TSchema; Fun: TBoundFunction);

Var
  CN,RT : String;
  ResultType : TResultType;

begin
  RT:=TPasFunctionType(Fun.ProcType).ResultEl.ResultType.Name;
  if IsSimpleType(RT) then
    ResultType:=rtSimple
  else
    ResultType:=rtObject;
  CN:=CT.Name;
  EmitMethodHeader(CN,Fun.Name,Fun.ProcType,RT);
  // Does indent
  EmitMethodPath(Fun.ProcType,ASchema.NameSpace+'.'+Fun.Name,False);
  EmitFunctionCall('AService',RT,ResultType);
  Decindent;
  AddLn('end;');
  AddLn('');
end;


procedure TEDMX2PasConverter.EmitUnBoundFunction(CT: TPasClassType;
  Fun: TUnBoundFunction);

Var
  CN,RTN : String;
  ResultType : TResultType;

begin
  RTN:=TPasFunctionType(Fun.ProcType).ResultEl.ResultType.Name;
  if IsSimpleType(RTN) then
    ResultType:=rtSimple
  else
    ResultType:=rtObject;
  CN:=CT.Name;
  EmitMethodHeader(CN,Fun.Name,Fun.ProcType,RTN);
  // Does indent
  EmitMethodPath(Fun.ProcType,Fun.ExportPath,True);
  EmitFunctionCall('Service',RTN,ResultType);
  Decindent;
  AddLn('end;');
  AddLn('');
end;

procedure TEDMX2PasConverter.EmitPreparePostObject(Act: TPasProcedure;
  ActionPath: String; GlobalService, AllocateArray: Boolean);

Var
  I : Integer;
  AP : TPasArgument;
  MN,ETN : String;
  HaveData : Boolean;
  AT : TResultType;

begin
  HaveData:= Ord(Not GlobalService) < Act.ProcType.Args.Count;
  Addln('Var');
  IncIndent;
  if HaveData then
    AddLn('_JSON : TJSONObject;');
  if AllocateArray then
    begin
    AddLn('_ARR : TJSONArray;');
    AddLn('_res : String;');
    end;
  AddLn('_data : String;');
  AddLn('_Path : String;');
  DecIndent;
  Addln('begin');
  IncIndent;
  if GLobalService then
    AddLn('CheckService;');
  if Not HaveData then
    AddLn('_data:='''';')
  else
    begin
    AddLn('_JSON:=TJSONObject.Create;');
    AddLn('try');
    IncIndent;
    // 0 is service
    For I:=Ord(Not GlobalService) to Act.ProcType.Args.Count-1 do
      begin
      AP:=TPasArgument(Act.ProcType.Args[i]);
      MN:=TActionFunctionParameter(AP.CustomData).Name;
      AT:=GetResultType(AP.ArgType.Name,ETN);
      Case AT of
        rtSimple :
          AddLn('_JSON.Add(''%s'',%s);',[MN,AP.Name]);
        rtObject :
          AddLn('_JSON.Add(''%s'',%s.SaveToJSON);',[MN,AP.Name]);
        rtArraySimple:
          AddLn('_JSON.Add(''%s'',DynArrayToJSONArray(Pointer(%s),''%s'',Nil));',[MN,AP.Name,ETN]);
        rtArrayObject:
          AddLn('_JSON.Add(''%s'',DynArrayToJSONArray(Pointer(%s),'''',%s));',[MN,AP.Name,ETN,ETN]);
      end;
      end;
    AddLn('_data:=_JSON.AsJSON;');
    DecIndent;
    Addln('finally');
    IncIndent;
    AddLn('FreeAndNil(_JSON);');
    DecIndent;
    Addln('end;');
    end;
  if GlobalService then
    AddLn('_Path:=''/%s'';',[ActionPath])
  else
    AddLn('_Path:=BaseURL(AService)+''/%s'';',[ActionPath]);
end;


procedure TEDMX2PasConverter.EmitActionServiceCall(const AReturnType,
  AElementType: String; GlobalService: Boolean; ResultType: TResultType);

var
  SN,KP : String;


begin
  SN:='Service';
  If Not GlobalService then
    SN:='A'+SN;
  Case ResultType of
    rtNone:
      Addln(SN+'.ServiceCall(''POST'',_Path,'''',_Data);');
    rtSimple:
      begin
      Addln('_Res:='+SN+'.ServiceCall(''POST'',_Path,'''',_Data);');
      KP:=ConvertTypeToStringExpr('_Res',AReturnType);
      Addln('Result:='+KP+';');
      end;
    rtArraySimple,
    rtArrayObject:
      begin
//      Delete(AElementType,1,1);
      Addln('_Res:='+SN+'.ServiceCall(''POST'',_Path,'''',_Data);');
      Addln('_arr:=GetJSON(_res) as TJSONArray;');
      Addln('try');
      IncIndent;
      if ResultType=rtArraySimple then
        Addln('Result:=%s(JSONArrayToDynArray(_arr,''%s'',Nil));',[AReturnType,AElementType])
      else
        Addln('Result:=%s(JSONArrayToDynArray(_arr,'''',%s));',[AReturnType,AElementType]);
      DecIndent;
      Addln('finally');
      IncIndent;
      Addln('_arr.Free;');
      DecIndent;
      Addln('end');
      end;
    rtObject:
      Addln('Result:=%s(%s.SingleServiceCall(''POST'',_Path,'''',_data,%s));',[AReturnType,SN,AReturnType]);
    end;
end;

procedure TEDMX2PasConverter.EmitUnBoundAction(CT: TPasClassType; Act: TPasProcedure);
Var
  ETN,APath,CN,RTN : String;
  ResultType : TResultType;


begin
  if Act.ProcType is TPasFunctionType then
    RTN:=TPasFunctionType(Act.ProcType).ResultEl.ResultType.Name
  else
    RTN:='';
  ResultType:=GetResultType(RTN,ETN);
  CN:=CT.Name;
  EmitMethodHeader(CN,Act.Name,Act.ProcType,RTN);
  if (Act is TUnboundActionProc) then
    APath:=TUnboundActionProc(Act).ExportPath
  else
    APath:=TUnboundActionFunc(Act).ExportPath;
  EmitPreparePostObject(Act,APath,True,ResultType=rtArraySimple);
  EmitActionServiceCall(RTN,ETN,True,ResultType);
  Decindent;
  AddLn('end;');
  AddLn('');
end;

procedure TEDMX2PasConverter.EmitBoundAction(CT: TPasClassType;
  ASchema: TSchema; Act: TPasProcedure);

Var
  AEN,CN,RTN : String;
  ResultType : TResultType;

begin
  if Act.ProcType is TPasFunctionType then
    RTN:=TPasFunctionType(Act.ProcType).ResultEl.ResultType.Name
  else
    RTN:='';
  ResultType:=GetResultType(RTN,AEN);
  CN:=CT.Name;
  EmitMethodHeader(CN,Act.Name,Act.ProcType,RTN);
  EmitPreparePostObject(Act,ASchema.NameSpace+'.'+Act.Name,False,ResultType=rtArraySimple);
  EmitActionServiceCall(RTN,AEN,False,ResultType);
  Decindent;
  AddLn('end;');
  AddLn('');
end;
{$endif}

procedure TEDMX2PasConverter.EmitSetStream(const CN: String; G: TSetStream);
Var
  S : String;
  I : Integer;

begin
  EmitMethodHeader(CN,G.Name,G.ProcType,'');
  S:='';
  For i:=0 to G.ProcType.Args.Count-1 do
    begin
    If (S<>'') then
      S:=S+',';
    S:=S+TPasArgument(G.ProcType.Args[i]).Name;
    end;
  SimpleMethodBody([Format('DoSetStream(%s);',[S])]);
end;

procedure TEDMX2PasConverter.EmitSetArrayLength(CT: TPasClassType;
  A: TSetArrayLength);

Var
  I : integer;
  P : TPasProperty;

begin
  Addln('{$IFDEF VER2_6}');
  EmitMethodHeader(CT.Name,A.Name,A.ProcType,'');
  Addln('begin');
  IncIndent;
  AddLn('Case aName of');
  for I:=0 to CT.Members.Count-1 do
    if TObject(CT.Members[i]) is TPasProperty then
      begin
      P:=TPasProperty(CT.Members[i]);
      if (Copy(P.VarType.Name ,Length(P.VarType.Name)-4,5)='Array') then
        begin
        Addln('''%s'' : SetLength(%s,aLength);',[LowerCase(P.Name),P.ReadAccessorName]);
        end;
      end;
  AddLn('else');
  incIndent;
  AddLn('inherited SetArrayLength(aName,ALength);');
  decIndent;
  AddLn('end;');
  decIndent;
  AddLn('end;');
  Addln('{$ENDIF VER2_6}');
  AddLn('');
end;

procedure TEDMX2PasConverter.EmitEntityClassFunction(CT: TPasClassType; ASchema: TSchema; CE: TEntityClassFunction);

Var
  ES:TEntitySet;
  TN : String;
  P : TPasType;

begin
  TN:=(CE.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  ES:=CE.CustomData as TEntitySet;
  AddLn('Class Function %s.%s : %s; ',[CT.Name,CE.Name,TN]);
  P:=ResolveType(ASchema,ES.EntityType);
  try
    SimpleMethodBody([Format('Result:=%s;',[P.Name])]);
  finally
    P.Release;
  end;
end;

procedure TEDMX2PasConverter.EmitPropertySetter(const CN: String;
  P: TPropertySetter);

Var
  FN: String;
begin
  EmitMethodHeader(CN,P.Name,P.ProcType,'');
  FN:=FieldPrefix+P.TheProperty.Name;
  SimpleMethodBody([Format('If (%s=AValue) then exit;',[FN]),
                    Format('%s:=AValue;',[FN]),
                    'MarkPropertyChanged(AIndex);']);
end;


procedure TEDMX2PasConverter.EmitObjectRestKind(CT: TPasClassType; R : TObjectRestKind);

Var
  NN,CN : string;
  O : TObject;

begin
  CN:=CT.Name;
  O:=CT.CustomData;
  NN:=GetNativeTypeName(O);
  Addln('');
  AddLn('Class Function %s.%s : String; ',[CN,R.Name]);
  SimpleMethodBody([Format('Result:=%s;',[MakePascalString(NN,True)])]);
end;

procedure TEDMX2PasConverter.EmitGetSingleton(CT: TPasClassType; S: TGetSingleTon);

Var
  PN,TN,NN,CN : string;
  O : TObject;
begin
  CN:=CT.Name;
  O:=S.CustomData;
  NN:=GetNativeTypeName(O);
  TN:=(S.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  Addln('');
  AddLn('Function %s.%s : %s; ',[CN,S.Name,TN]);
  PN:=MakePascalString(NN,True);
  SimpleMethodBody(['CheckService;',
                    Format('Result:=%s(Service.SingleServiceCall(%s,'''',%s));',[TN,PN,TN]),
                    Format('Result.BasePath:=%s;',[PN])]);
end;

procedure TEDMX2PasConverter.EmitGetKeyAsURLPart(CT: TPasClassType;
  ASchema: TSchema; P: TKeyAsURLPart);

Var
  CN,KP : String;
  EK : TEntityKeyElement;
  I : integer;
  EP : TEntityProperty;
  T : TPasType;

begin
  CN:=CT.Name;
  EK:=P.CustomData as TEntityKeyElement;
  Addln('');
  AddLn('Function %s.KeyAsURLPart : string;',[CN]);
  Addln('');
  Addln('begin');
  IncIndent;
    For I:=0 to EK.Length-1 do
     begin
     EP:=FindProperty(CT,EK[i].Name);
     T:=ResolveType(ASchema,EP._Type);
     KP:=ConvertTypeToStringExpr(CleanPropertyName(EK[i].Name,ekwNone),T.Name);
     T.Release;
     if I>0 then
       AddLn('Result:=Result+'',''+'+KP+';')
     else
       AddLn('Result:='+KP+';');
     end;
  Decindent;
  Addln('end;');
  Addln('');

end;

procedure TEDMX2PasConverter.EmitExportPropertyName(CT: TPasClassType; E : TExportPropertyName);

Var
  PN,CN : String;
  P : TPasProperty;
  D : TObject;
  I : integer;

begin
  CN:=CT.Name;
  Addln('');
  AddLn('Class Function %s.%s(Const AName : String) :String;',[CN,E.Name]);
  Addln('');
  AddLn('begin');
  IncIndent;
  AddLn('Case AName of');
  for I:=0 to CT.Members.Count-1 do
    if TObject(CT.Members[i]).InheritsFrom(TPasProperty) then
      begin
      P:=TPasProperty(CT.Members[i]);
      D:=P.CustomData;
      if D is TEntityProperty then
        PN:=WTOA(TEntityProperty(D).Name)
      else if D is TComplexTypeProperty then
        PN:=WTOA(TComplexTypeProperty(D).Name)
      else if D=Nil then
        Raise EEDMX2PasConverter.CreateFmt('Unrecognized property type for %d %s.%s : NIL',[I,CN,P.Name])
      else
        Raise EEDMX2PasConverter.CreateFmt('Unrecognized property type for %d %s.%s : NIL',[I,CN,P.Name,D.ClassName]);
      if (CompareText(PN,P.Name)<>0) then
        AddLn('''%s'' : Result:=''%s'';',[P.Name,PN]);
      end;
  AddLn('else');
  IncIndent;
  AddLn('Result:=Inherited ExportPropertyName(AName);');
  DecIndent;
  AddLn('end;');
  DecIndent;
  AddLn('end;');
  Addln('');
end;

procedure TEDMX2PasConverter.EmitClassImplementation(ID: TIDentifier);

Var
  CN : String;
  I : Integer;
  E : TPasElement;
  CT : TPasClassType;

begin
  CN:=ID.Element.Name;
  DoLog('Generating class implementation for %s',[CN]);
  ClassHeader(CN);
  CT:=ID.Element as TPasClassType;
  for I:=0 to CT.Members.Count-1 do
    begin
    E:=TPasElement(CT.Members[i]);
    If E is TPropertySetter then
      EmitPropertySetter(CN,E as TPropertySetter)
    else if E is TPropertyGetter then
      EmitPropertyGetter(CN,E as TPropertyGetter)
    else if E is TCreateContainer then
      EmitCreateContainer(CN,E as TCreateContainer)
    else if E is TCreateEntitySet then
      EmitCreateEntitySet(CN,E as TCreateEntitySet)
    else If E is TObjectRestKind then
      EmitObjectRestKind(CT,E as TObjectRestKind)
    else If E is TGetSingleton then
      EmitGetSingleTon(CT,E as TGetSingleton)
    else If E is TENtityClassFunction then
      EmitEntityClassFunction(CT,ID.Schema,E as TENtityClassFunction)
    else If E is TExportPropertyName then
      EmitExportPropertyName(CT,E As TExportPropertyName)
    else If E is TGetNavigationProperty then
      EmitNavigationProperty(CT,E as TGetNavigationProperty)
    else If E is TGetContainedSingleton then
      EmitGetContainedSingleton(CT,E as TGetContainedSingleton)
    else If E is TKeyAsURLPart then
      EmitGetKeyAsURLPart(CT,ID.Schema,E as TKeyAsURLPart)
    else If E is TGetStream then
      EmitGetStream(CN,E as TGetStream)
    else If E is TSetStream then
      EmitSetStream(CN,E as TSetStream)
    else If E is TSetArrayLength then
      EmitSetArrayLength(CT,E as TSetArrayLength)
{$IFNDEF USECSDL}
    else If E is TUnBoundFunction then
      EmitUnBoundFunction(CT,E as TUnBoundFunction)
    else If E is TBoundFunction then
      EmitBoundFunction(CT,ID.Schema,E as TBoundFunction)
    else If (E is TUnBoundActionProc) or (E is TUnBoundActionFunc) then
      EmitUnBoundAction(CT,E as TPasProcedure)
    else If (E is TBoundActionProc) or (E is TBoundActionFunc) then
      EmitBoundAction(CT,ID.Schema,E as TPasProcedure)
{$ENDIF }
    else If E is TEntityMethod then
      EmitEntityMethod(CT,E As TEntityMethod);
    end;
end;

procedure TEDMX2PasConverter.EmitGetContainedSingleton(CT: TPasClassType; E: TGetContainedSingleton);

Var
  CN,TN,PN : String;

begin
  CN:=CT.Name;
  TN:=(E.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  PN:=WTOA((E.CustomData as TNavigationProperty).Name);
  EmitMethodHeader(CN,E.Name,E.ProcType,TN);
  SimpleMethodBody([Format('Result:=%s(GetContainedSingleTon(AService,''%s'', %s));',[TN,PN,TN])]);
end;

procedure TEDMX2PasConverter.EmitNavigationProperty(CT : TPasClassType; E : TGetNavigationProperty);

Var
  CN,TN,PN : String;

begin
  CN:=CT.Name;
  TN:=(E.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  PN:=WTOA((E.CustomData as TNavigationProperty).Name);
  EmitMethodHeader(CN,E.Name,E.ProcType,TN);
  SimpleMethodBody([Format('Result:=%s(CreateContainedEntitySet(AService,''%s'', %s));',[TN,PN,TN])]);
end;

procedure TEDMX2PasConverter.EmitEntityMethod(CT : TPasClassType; E : TEntityMethod);

begin
  if E is TEntityGet then
    EmitEntityGet(CT,E as TEntityGet)
  else if E is TEntityList then
    EmitEntityList(CT,E as TEntityList);
end;

procedure TEDMX2PasConverter.EmitEntityGet(CT : TPasClassType; E : TEntityGet);

Var
  CN,TN,S,SV,AN : String;
  I : integer;
  Arg : TPasArgument;

begin
  CN:=CT.Name;
  TN:=(E.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  EmitMethodHeader(CN,E.Name,E.ProcType,TN);
  S:='';
  For I:=0 to E.ProcType.Args.Count-1 do
    begin
    Arg:=TPasArgument(E.ProcType.Args[i]);
    AN:=Arg.Name;
    SV:=AN;
    SV:=ConvertTypeToStringExpr(AN,Arg.ArgType.Name);
    if (S<>'') then
      S:=S+'+'',''+';
    S:=S+SV;
    end;
  if S='' then
    S:='''''';
  SimpleMethodBody([  Format('Result:=%s(GetSingle(%s));',[TN,S])]);
end;



procedure TEDMX2PasConverter.EmitEntityList(CT: TPasClassType; E: TEntityList);

Var
  CN,TN: String;
  isListAll : Boolean;
  F,NL : String;

begin
  isListAll:=E is TEntityListAll;
  CN:=CT.Name;
  TN:=(E.ProcType as TPasFunctionType).ResultEl.ResultType.Name;
  EmitMethodHeader(CN,E.Name,E.ProcType,TN);
  if isListAll then
    begin
    AddLn('var N : String;');
    NL:='N';
    F:='True';
    end
  else
    begin
    NL:='NextLink';
    F:='False';
    end;
  SimpleMethodBody([Format('Result:=%s(GetMulti(AQuery,%s,%s));',[TN,F,NL])]);
end;


procedure TEDMX2PasConverter.EmitForwardDeclaration;

Var
  CN : String;
  I : Integer;
  ID : TIdentifier;

begin
  For I:=0 to  FIdentifierList.Count-1 do
    begin
    ID:=FIdentifierList[i] as TIdentifier;
    If ID.Element.InheritsFrom(TPasClassType) then
      begin
      CN:=ID.Element.Name;
      AddLn('%s = class;',[CN]);
      AddLn('%sArray = Array of %s;',[CN,CN]);
      end;
    end;
end;


procedure TEDMX2PasConverter.EmitInterface;

begin
  Addln('type');
  IncIndent;
  Comment(' Needed for binary data');
  Addln('TByteArray = Array of byte;');
  Addln('TInt16Array = Array of SmallInt;');
  Comment('');
  EmitForwardDeclaration;
  Comment('');
  EmitEnumTypes;
  EmitClassDeclarations;
  DecIndent;
end;


class function TEDMX2PasConverter.ODataVersion: TODataVersion;
begin
{$IFDEF USECSDL}
  Result:=oDataV2;
{$ELSE}
  Result:=ODataV4;
{$ENDIF USECSDL}
end;

procedure TEDMX2PasConverter.EmitEnumTypes;

Var
  Id : TIdentifier;
  PE : TPasEnumType;
  I : integer;

begin
  AddLn('');
  Comment(' Enumerations');
  AddLn('');
  if EnumerationMode=emScoped then
    AddLn('{$SCOPEDENUMS ON}');
  For I:=0 to FIdentifierList.Count-1 do
    begin
    Id:=TIdentifier(FIdentifierList[i]);
    if ID.Element.InheritsFrom(TPasEnumType) then
      begin
      PE:=ID.Element as TPasEnumType;
      AddLn(PE.GetDeclaration(True)+';');
      AddLn(PE.Name+'Array = Array of '+PE.Name+';');
      end;
    end;
end;


procedure TEDMX2PasConverter.AnalyseXML;

Const
  EdmxScopeOld ='http://schemas.microsoft.com/ado/2007/06/edmx';
  DataservicesScopeOld ='http://schemas.microsoft.com/ado/2007/06/edmx';
  EdmxScopeNew ='http://docs.oasis-open.org/odata/ns/edmx';
  DataservicesScopeNew ='http://docs.oasis-open.org/odata/ns/edm';


Var
  F : IXMLFormatter;
  Count : Integer;
  i : Integer;
  ScopeName :String;
  ASchema : TSchema;
  EdmxScope,
  DataservicesScope: String;

begin
  F:=TXmlFormatter.Create();
  f.LoadFromStream(FXml);
  f.PrepareForRead();
  if ODataVersion=ODataV2 then
    begin
    EdmxScope:=EdmxScopeOld;
    DataservicesScope:=DataservicesScopeOld;
    end
  else
    begin
    EdmxScope:=EdmxScopeNew;
    DataservicesScope:=DataservicesScopeNew;
    end;
  if (f.BeginScopeRead('Edmx',EdmxScope) <= 0) then
    Raise EEDMX2PasConverter.Create('Not a valid Edmx XML document');
  Count:=f.BeginScopeRead('DataServices',EdmxScope );
  if Count<=0 then
    Raise EEDMX2PasConverter.Create('No DataServices found');
  ScopeName:=DataservicesScope;
  Count:=f.BeginArrayRead(ScopeName,TypeInfo(Schema),asEmbeded,'Schema');
  if Count<=0 then
    Raise EEDMX2PasConverter.Create('No schema found');
  for i := 0 to Count-1 do
    begin
    ASchema:=TSchema.Create();
    FSchemaList.Add(ASchema);
    end;
  for i := 0 to Count-1 do
    begin
    ASchema:=TSchema(FSchemaList[i]);
    if Not f.Get(TypeInfo(TSchema),ScopeName,ASchema) then
      Raise EEDMX2PasConverter.CreateFmt('Schema[%d] not found',[i]);
    DoLog('Found schema : %s',[ASchema.Namespace]);
    end;
end;

function TEDMX2PasConverter.GetNameSpace(ASchema: TSchema): String;

begin
  Result:=WTOA(Aschema.Namespace);
  If Aliases.IndexOfName(Result)<>-1 then
    Result:=Aliases.Values[Result];
end;

function TEDMX2PasConverter.CreateIdentifierName(ASchema: TSchema;
  const APrefix, AName: String): String;

Var
  N : String;

begin
  Result:='T'+APrefix+ServiceSuffix+CleanPropertyName(AName,ekwNone);
  N:=LowerCase(GetNameSpace(ASchema)+'.'+AName);
  IdentifierMap.Add(N+'='+Result);
  // Add array as wel, for collection.
  IdentifierMap.Add('collection('+N+')='+Result+'Array');
end;

function TEDMX2PasConverter.CreateIdentifierName(ASchema: TSchema;
  const APrefix, AName: UnicodeString): String;
begin
  Result:=CreateIdentifierName(ASchema,WTOA(APrefix),WTOA(AName));
end;

function TEDMX2PasConverter.CreateIdentifierName(ASchema: TSchema;
  const APrefix: String; AName: UnicodeString): String;
begin
  Result:=CreateIdentifierName(ASchema,APrefix,WTOA(AName));
end;

function TEDMX2PasConverter.NeedWriteSetter(P: TComplexTypeProperty): Boolean;

begin
  Result:=(P<>Nil)
end;

function TEDMX2PasConverter.ResolveNameSpace(ASchema: TSchema; ATypeName: String
  ): String;

Const
  SCollection = 'Collection(';
  LCollection = Length(SCollection);

Var
  NS : String;
  IsColl : Boolean;
  L : Integer;

begin
  Result:=ATypeName;
  NS:=GetNameSpace(Aschema);
  if NS=ASchema.Namespace then
    exit;
  IsColl:=Copy(Result,1,LCollection)=SCollection;
  if IsColl then
    Delete(Result,1,LCollection);
  L:=Length(ASchema.Namespace);
  if (Copy(Result,1,L)=ASchema.Namespace) then
    begin
    Delete(Result,1,L);
    Result:=NS+Result;
    end;
  if isColl then
    Result:=SCollection+Result;
end;

function TEDMX2PasConverter.ResolveType(ASchema: TSchema;
  const ATypeName: String): TPasType;

Var
  CN,RN : String;

begin
  CN:=IdentifierMap.Values[LowerCase(ATypeName)];
  if (CN='') then
    begin
    RN:=ResolveNameSpace(ASchema,ATypeName);
    if RN<>ATypeName then
      CN:=IdentifierMap.Values[LowerCase(RN)]
    else
      begin
      RN:=GetNameSpace(ASchema)+'.'+ATypeName;
      CN:=IdentifierMap.Values[LowerCase(RN)];
      end;
    end;
  if (CN='') then
    Raise EEDMX2PasConverter.CreateFmt('Could not resolve Type %s (Schema: %s)',[ATypeName,ASchema.NameSpace]);
  Result:=TPasUnresolvedSymbolRef.Create(CN,Nil);
end;

function TEDMX2PasConverter.ResolveType(ASchema: TSchema;
  const ATypeName: UnicodeString): TPasType;
begin
  Result:=ResolveType(ASchema,WTOA(ATypeName));
end;

function TEDMX2PasConverter.CreatePropertyGetter(AParent: TPasElement;
  PN: String; indexed: Boolean; T: TPasType): TPropertyGetter;

Var
  PA : TPasArgument;
  GN : String;
  F  : TPasFunctionType;

begin
  GN:='Get'+PN;
  Result:=TPropertyGetter.Create(GN,AParent);
  Result.Visibility:=visPrivate;
  F:=TPasFunctionType.Create('',Result);
  Result.ProcType:=F;
  if Indexed then
    begin
    // AIndex
    PA:=TPasArgument.Create('AIndex',Result.ProcType);
    PA.ArgType:=TPasUnresolvedTypeRef.Create('Integer',PA);
    Result.ProcType.Args.Add(PA);
    end;
  // Result
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=T;
end;

function TEDMX2PasConverter.CreatePropertySetter(AParent: TPasElement;
  PN: String; indexed: Boolean; T: TPasType): TPropertySetter;

Var
  PA : TPasArgument;
  SN : String;

begin
  SN:='Set'+PN;
  Result:=TPropertySetter.Create(SN,AParent);
  Result.Visibility:=visPrivate;
  Result.ProcType:=TPasProcedureType.Create('',Result);
  if Indexed then
    begin
    // AIndex
    PA:=TPasArgument.Create('AIndex',Result.ProcType);
    PA.ArgType:=TPasUnresolvedTypeRef.Create('Integer',PA);
    Result.ProcType.Args.Add(PA);
    end;
  // Actual argument
  PA:=TPasArgument.Create('AValue',Result.ProcType);
  PA.ArgType:=T;
  PA.Access:=argConst;
  Result.ProcType.Args.Add(PA);
end;

function TEDMX2PasConverter.UseExtraIdentifierProtection(D: TObject
  ): TExtraKeywords;

begin
  result:=ekwNone;
  if Assigned(D) then
    begin
    if D is EntityContainer then
      result:=ekwEntityContainer;
    if D is TEntitySet then
      Result:=ekwEntitySet
    else if D is TEntityType then
      Result:=ekwEntity
    else if D is TComplexType then
      Result:=ekwObject
    end;
end;

function TEDMX2PasConverter.AddProperty(ID: TIdentifier;
  APropertyIndex: integer; const APropertyName, APropertyType: String;
  Flags: TPropertyFlags; ACustomData: TObject): Boolean;

Var
  PP : TPasProperty;
  PS : TPropertySetter;
  PG : TPropertyGetter;
  PV : TPasVariable;
  GN,SN,PN : String;
  T : TPasType;
  C : TPasClassType;

begin
  DoLog('Adding property [%d] %s : %s',[APropertyIndex,APropertyName,APropertyType]);
  C:=ID.Element as TPasClassType;
  // Construct property.
  PN:=CleanPropertyName(APropertyName,UseExtraIdentifierProtection(C.CustomData));
  Result:=CompareText(PN,APropertyName)<>0;
  PG:=NIl;
  PS:=Nil;
  // Field
  PV:=TPasVariable.Create(FieldPrefix+PN,C);
  T:=ResolveType(ID.Schema,APropertyType);
  PS:=Nil;
  PV.VarType:=T;
  PV.Visibility:=visPrivate;
  C.Members.Add(PV);
  // Getter, if needed
  if Not (pfNeedGetter in Flags) then
    GN:=FieldPRefix+PN
  else
    begin
    T.AddRef;
    PG:=CreatePropertyGetter(C,PN,pfIndexed in flags,T);
    C.Members.Add(PG);
    GN:=PG.Name;
    end;
  if not (pfReadOnly in Flags) then
    begin
    if Not (pfNeedSetter in Flags) then     // Setter, if needed
      SN:=FieldPRefix+PN
    else
      begin
      T.AddRef;
      PS:=CreatePropertySetter(C,PN,pfIndexed in flags,T);
      C.Members.Add(PS);
      SN:=PS.Name;
      end;
    end;
  // And finally, the actual property
  PP:=TPasProperty.Create(PN,C);
  PP.CustomData:=ACustomData;
  PP.ReadAccessorName:=GN;
  PP.WriteAccessorName:=SN;
  PP.Visibility:=visPublished;
  PP.VarType:=T;
  If (pfindexed in Flags) then
    begin
    PP.IndexExpr:=TPrimitiveExpr.Create(PP,pekNumber,eopNone);
    TPrimitiveExpr(PP.IndexExpr).Value:=IntToStr(APropertyIndex shl IndexShift);
    end;
  if Assigned(PS) then
    PS.TheProperty:=PP;
  if Assigned(PG) then
    PG.TheProperty:=PP;
  T.AddRef;
  C.Members.Add(PP);
end;

procedure TEDMX2PasConverter.AddExportPropertyName(ID: TIdentifier);


Var
  PC : TPasClassType;
  E : TExportPropertyName;
  F : TPasFunctionType;
  PA : TPasArgument;

begin
  // Class Function ExportPropertyName(Const AName : String) : string; virtual;
  PC:=ID.Element as TPasClassType;
  E:=TExportPropertyName.Create('ExportPropertyName',PC);
  E.Modifiers:=[pmOverride];
  E.Visibility:=visPublic;
  F:=TPasFunctionType.Create('ExportPropertyName',E);
  E.ProcType:=F;
  // Actual argument
  PA:=TPasArgument.Create('AName',F);
  PA.ArgType:=TPasUnresolvedTypeRef.Create('String',PA);
  PA.Access:=argConst;
  F.Args.Add(PA);
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=TPasUnresolvedTypeRef.Create('String',F.ResultEl);
  PC.Members.Add(E);
end;

procedure TEDMX2PasConverter.CompleteComplexType(ID: TIdentifier);

Var
  P : TComplexTypeProperty;
  I : Integer;
  C : TPasClassType;
  CT : TComplexType;
  Flags : TPropertyFlags;
  isArray,HaveArray,B : Boolean;
  PropertyIndexOffset : Integer;
  PE : TPasType;
  {$IFNDEF USECSDL }
  PID : TIdentifier;
  {$ENDIF}

begin
  B:=False;
  C:=ID.Element as TPasClassType;
  CT:=ID.Element.CustomData as TComplexType;
  {$IFNDEF USECSDL }
  if (CT.BaseType<>'') then
    begin
    PID:=FindIdentifier(Nil,CT.BaseType);
    if PID=NIl then
      Raise EEDMX2PasConverter.CreateFmt('Could not resolve parent type for entity type %s',[CT.Name]);
    PE:=PID.Element as TPasClassType;
    PropertyIndexOffset:=CountProperties(PE as TPasClassType);
    PE.AddRef;
    end
  else
  {$ENDIF}
    begin
    PE:=TPasUnresolvedTypeRef.Create(BaseEntityType,Nil);
    PropertyIndexOffset:=0;
    end;
  HaveArray:=False;
  C.AncestorType:=PE;
  B:=False;
  For I:=0 to CT._Property.Length-1 do
    begin
    P:=CT._Property[i];
    Flags:=[pfNeedSetter,pfIndexed];
    if not P.Nullable then
      Include(Flags,pfRequired);
    if P._Type='' then
      Raise EEDMX2PasConverter.CreateFmt('Identity type %s: No type for property: %s',[CT.Name,P.Name]);
    // Construct property.
    ExtractBaseTypeName(ID.Schema,P._Type,isArray);
    haveArray:=haveArray or isArray;
    B:=AddProperty(ID,PropertyIndexOffset+I,WTOA(P.Name),WTOA(P._Type),Flags,P) or B;
    end;
  if haveArray then
    AddSetArrayLength(ID);
  If B then
    AddExportPropertyName(ID);
end;


procedure TEDMX2PasConverter.CompleteEntityType(ID: TIdentifier);

Var
  P : TEntityProperty;
  I,J : Integer;
  C : TPasClassType;
  CT : TEntityType;
  Flags : TPropertyFlags;
  PID : TIdentifier;
  PE : TPasType;
  PropertyIndexOffset : Integer;
  Key : TEntityKeyElement;
  B,isArray,HaveArray : Boolean;

begin
  C:=ID.Element as TPasClassType;
  CT:=ID.Element.CustomData as TEntityType;
  if (CT.BaseType='') then
    begin
    PE:=TPasUnresolvedTypeRef.Create(BaseEntityType,Nil);
    PropertyIndexOffset:=0;
    end
  else
    begin
    PID:=FindIdentifier(Nil,CT.BaseType);
    if PID=NIl then
      Raise EEDMX2PasConverter.CreateFmt('Could not resolve parent type for entity type %s',[CT.Name]);
    PE:=PID.Element as TPasClassType;
    PropertyIndexOffset:=CountProperties(PE as TPasClassType);
    PE.AddRef;
    end;
  HaveArray:=False;
  C.AncestorType:=PE;
  B:=False;
  For I:=0 to CT._Property.Length-1 do
    begin
    P:=CT._Property[i];
    if (PE is TPasClassType) then
      if FindProperty(PE as TPasClassType,P.Name)<>Nil then
        continue;
    Flags:=[pfIndexed,pfNeedSetter];
    if not P.Nullable then
      Include(Flags,pfRequired);
{$IFDEF USECSDL}
    if Assigned(CT.Key) then
      for J:=0 to CT.Key.Length-1 do
        if (CT.Key.Item[J].Name=P.Name) then
          Include(Flags,pfInKey);
{$ELSE}
     if Assigned(CT.Key) and (CT.Key.Length=1) then
         for J:=0 to CT.Key.Item[0].Length-1 do
          if (CT.Key.Item[0].Item[J].Name=P.Name) then
             Include(Flags,pfInKey);
{$ENDIF}
    // Construct property.
    if P._Type='' then
      Raise EEDMX2PasConverter.CreateFmt('Identity type %s: No type for property: %s',[CT.Name,P.Name]);
    ExtractBaseTypeName(ID.Schema,P._Type,isArray);
    haveArray:=haveArray or isArray;
    B:=AddProperty(ID,PropertyIndexOffset+I,WTOA(P.Name),WTOA(P._Type),Flags,P) or B;
    end;
  if haveArray then
    AddSetArrayLength(ID);
  if B then
    AddExportPropertyName(ID);
  Key:=Nil;
  if Assigned(CT.Key) then
  {$IFDEF USECSDL}
     if (CT.Key.Length>0) then
       Key:=CT.Key;
  {$ELSE}
     if (CT.Key.Length=1) then
       if (CT.Key.Item[0].Length>0) then
         Key:=CT.Key.Item[0];
  {$ENDIF}
  if Assigned(Key) then
     AddGetKeyAsURLPart(ID,Key);
  For I:=0 to CT.NavigationProperty.Length-1 do
    AddNavigationproperty(ID,CT.NavigationProperty[i]);
  {$IFNDEF USECSDL}
  if CT.HasStream then
    begin
    AddGetStream(ID);
    AddSetStream(ID);
    end;
  {$ENDIF}
end;

function TEDMX2PasConverter.AddGetStream(ID: TIDentifier): TGetStream;

Var
  C : TPasClassType;
  F : TPasProcedureType;
  A : TPasArgument;

begin
  C:=ID.Element as TPasClassType;
  Result:=TGetStream.Create('GetStream',C);
  C.Members.Add(Result);
  F:=TPasProcedureType.Create('GetStream',Result);
  Result.ProcType:=F;
  Result.Visibility:=visPublic;
  // Service argument
  A:=TPasArgument.Create('AService',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('TODataService',A);
  F.Args.Add(A);
  // ContentType argument
  A:=TPasArgument.Create('AContentType',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('String',A);
  F.Args.Add(A);
  // Stream into which to copy the data.
  A:=TPasArgument.Create('AStream',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('TStream',A);
  F.Args.Add(A);
end;

function TEDMX2PasConverter.AddSetStream(ID: TIDentifier): TSetStream;
Var
  C : TPasClassType;
  F : TPasProcedureType;
  A : TPasArgument;

begin
  C:=ID.Element as TPasClassType;
  Result:=TSetStream.Create('SetStream',C);
  C.Members.Add(Result);
  F:=TPasProcedureType.Create('SetStream',Result);
  Result.ProcType:=F;
  Result.Visibility:=visPublic;
  // Service argument
  A:=TPasArgument.Create('AService',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('TODataService',A);
  F.Args.Add(A);
  // ContentType argument
  A:=TPasArgument.Create('AContentType',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('String',A);
  F.Args.Add(A);
  // Stream into which to copy the data.
  A:=TPasArgument.Create('AStream',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('TStream',A);
  F.Args.Add(A);
end;

function TEDMX2PasConverter.AddGetKeyAsURLPart(ID: TIdentifier;
  Key: TEntityKeyElement): TPasFunction;

Var
  C : TPasClassType;
  F : TPasFunctionType;

begin
  C:=ID.Element as TPasClassType;
  Result:=TKeyAsURLPart.Create('KeyAsURLPart',C);
  Result.Visibility:=visPublic;
  Result.CustomData:=Key;
  F:=TPasFunctionType.Create('KeyAsURLPart',Result);
  Result.ProcType:=F;
  Result.Modifiers:=[pmOverride];
  // Result type
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=TPasUnresolvedTypeRef.Create('String',F.ResultEl);
  C.Members.Add(Result);
end;

{$IFDEF USECSDL}


Function TEDMX2PasConverter.FindAssociatedTypeInSchema(ASchema : TSchema; Const ARelation,ARole : String) : String;

Var
  I,J : integer;
  A : TAssociation;

begin
  Result:='';
  I:=ASchema.Association.Length-1;
  While (Result='') and (I>=0) do
    begin
    A:=ASchema.Association[i];
    If (ASchema.NameSpace+'.'+A.Name=ARelation) then
      begin
      J:=A._End.Length-1;
      While (Result='') and (J>=0) do
        begin
        If A._End[j].Role=ARole then
          Result:=WTOA(A._End[j]._Type);
        Dec(J);
        end;
      end;
    Dec(I);
    end;
end;

Function TEDMX2PasConverter.FindAssociatedType(Var APreferredSchema : TSchema; Const ARelation,ARole : String) : String;

Var
  i : Integer;
  S : TSchema;

begin
  Result:=FindAssociatedTypeInSchema(APreferredSchema,ARelation,ARole);
  if (Result='') then
    begin
    I:=0;
    While (Result='') and (I<FSchemaList.Count) do
      begin
      S:=TSchema(FSchemaList[i]);
      if S<>APreferredSchema then
        begin
        Result:=FindAssociatedTypeInSchema(S,ARelation,ARole);
        If Result<>'' then
          APreferredSchema:=S;
        end;
      Inc(I);
      end;
    end;
  If (Result='') then
    Raise EEDMX2PasConverter.CreateFmt('Could not determine type of relation "%s", role "%s"',[ARelation,ARole]);
end;
{$ENDIF}

function TEDMX2PasConverter.AddNavigationProperty(ID: TIDentifier;
  P: TNavigationProperty): TPasFunction;

Var
  C : TPasClassType;
  F : TPasFunctionType;
  A : TPasArgument;
  ResType : TPasType;
  ATS : TSchema;
  BTN,TN,NP : String;
  ESI : TIDentifier;
  IsColl : Boolean;

begin
  C:=ID.Element as TPasClassType;
  NP:=CleanPropertyName(P.Name,UseExtraIdentifierProtection(C.CustomData));
  ATS:=ID.Schema; // Schema of associated type
{$IFNDEF USECSDL}
  TN:=WTOA(P._Type);
  ATS:=ID.Schema;
{$ELSE}
  TN:=FindAssociatedType(ATS,WTOA(P.Relationship),WTOA(P.ToRole));
{$ENDIF}
  BTN:=ExtractBaseTypeName(ID.Schema,TN,isColl);
  if Not IsColl then
    begin
    DoLog('Adding singleton navigation property %s (%s) : %s',[P.Name,NP,BTN]);
    Result:=TGetContainedSingleton.Create(NP,C);
    ResType:=ResolveType(ID.Schema,BTN);
    end
  else
    begin
    ESI:=FindEntitySetForEntity(ID.Schema,BTN);
    if (ESI = Nil) then
      Raise EEDMX2PasConverter.CreateFmt('Could not find navigation property %s : %s entity set.',[P.Name,TN]);
    DoLog('Adding navigation property %s (%s) : %s',[P.Name,NP,ESI.Element.Name]);
    Result:=TGetNavigationProperty.Create(NP,C);
    ResType:=ESI.Element as TPasClassType;
    ResType.AddRef;
    end;
  Result.Visibility:=visPublic;
  Result.CustomData:=P;
  F:=TPasFunctionType.Create(NP,Result);
  Result.ProcType:=F;
  // Service argument
  A:=TPasArgument.Create('AService',F);
  A.ArgType:=TPasUnresolvedTypeRef.Create('TODataService',A);
  F.Args.Add(A);
  // Result type
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=ResType;
  C.Members.Add(Result);
end;

function TEDMX2PasConverter.FindEntitySetForEntity(ASchema: TSchema;
  AName: String): TIdentifier;

Var
   I,C : Integer;
   S : String;
   ES : TEntitySet;

begin
  if Pos('.',AName)<>0 then
    S:=AName
  else if Assigned(ASchema) then
    S:=WTOA(ASchema.Namespace)+'.'+AName
  else
    S:=AName;
  I:=0;
  C:=FIdentifierList.Count;
  Result:=Nil;
  While (I<C) and (Result=Nil) do
    begin
    Result:=TIdentifier(FIdentifierList[i]);
    if Not (Result.Element.CustomData is TEntitySet) then
      Result:=Nil
    else
      begin
      ES:=Result.Element.CustomData as TEntitySet;
      // Writeln('Comparing ',TIdentifier(FIdentifierList[i]).IdentifierName,'=',S,' ?');
      If CompareText(WTOA(ES.EntityType),S)<>0 then
        Result:=Nil;
      end;
    Inc(I);
    end;
end;


function TEDMX2PasConverter.FindQualifiedIdentifier(AName: String): TIdentifier;

begin
  Result:=Nil;
  Result:=TIdentifier(FIdentifierHash.Items[LowerCase(AName)]);
end;

function TEDMX2PasConverter.FindIdentifier(ASchema: TSchema; AName: String
  ): TIdentifier;

Var
  I : Integer;
  S : String;
begin
  Result:=Nil;
  I:=0;
  if Pos('.',AName)<>0 then
    Result:=FindQualifiedIdentifier(AName);
  if Not Assigned(ASchema) then
    begin
    While (Result=Nil) and (I<FSchemaList.Count) do
      begin
      Result:=FindIdentifier(TSchema(FSchemaList[i]),AName);
      Inc(i);
      end;
    Exit;
    end;
//  Writeln('Searching namespace : ',ASchema.NameSpace,' for ',AName);
  S:=WTOA(ASchema.Namespace)+'.'+AName;
  Result:=FindQualifiedIdentifier(S);
end;

function TEDMX2PasConverter.FindIdentifier(ASchema: TSchema;
  AName: UnicodeString): TIdentifier;
begin
  Result:=FindIdentifier(ASchema,WTOA(AName));
end;

function TEDMX2PasConverter.FindProperty(C: TPasClassType; APropertyName: String
  ): TEntityProperty;

Var
  I : Integer;
  ET : TEntityType;

begin
  Result:=Nil;
  Repeat
    ET:=C.CustomData as TEntityType;
    I:=ET._Property.Length-1;
    While (I>=0) and (Result=Nil) do
      begin
      if CompareText(WTOA(ET._Property[i].Name),APropertyName)=0 then
        Result:=ET._Property[i];
      Dec(i);
      end;
    if C.AncestorType is TPasClassType then
      C:=C.AncestorType as TPasClassType
    else
      C:=Nil;
  until (Result<>Nil) or (C=nil);
end;

function TEDMX2PasConverter.FindProperty(C: TPasClassType;
  APropertyName: UnicodeString): TEntityProperty;
begin
  Result:=FindProperty(C,WTOA(APropertyName));
end;

function TEDMX2PasConverter.GetEntityKey(C: TPasClassType
  ): TEntityType_KeyArray;

Var
  ET : TEntityType;

begin
  Result:=Nil;
  Repeat
    // Writeln('Finding key of ',C.Name,' (',C.CustomData.ClassName,')');
    ET:=C.CustomData as TEntityType;
    Result:=ET.Key;
    if Result.Length=0 then
      Result:=Nil;
    if C.AncestorType is TPasClassType then
      C:=C.AncestorType as TPasClassType
    else
      C:=Nil;
  until (Result<>Nil) or (C=Nil);
end;

procedure TEDMX2PasConverter.AddEntityGet(ID, EID: TIdentifier);

Var
  FN : String;
  F  : TPasFunctionType;
  C  : TPasClassType;
  EM : TEntityMethod;
  ES : TEntitySet;
  PA : TPasArgument;
  I : Integer;
  AN : String;
  EP : TEntityProperty;
  AKey : TEntityType_KeyArray;

begin
  C:=ID.Element as TPasClassType;
  ES:=C.CustomData as TEntitySet;
  // Get function
  FN:='Get';
  EM:=TEntityGet.Create(FN,C);
  EM.CustomData:=ES;
  EM.Visibility:=visPublic;
  F:=TPasFunctionType.Create(FN,EM);
  // Construct arguments based on key, if available
  AKey:=GetEntityKey(EID.Element as TPasClassType);
  if Assigned(AKey) then
    begin
    for I:=0 to AKey.Length-1 do
      begin
{$IFDEF USECSDL}
      AN:=WTOA(AKey.Item[I].Name);
{$ELSE}
      if AKey.Item[i].Length>0 then
        AN:=WTOA(AKey.Item[I].Item[0].Name)
      else
        Raise EEDMX2PasConverter.CreateFmt('Empty key definition for %s type of entityset %s',[ES.EntityType,ES.Name]);
{$ENDIF}
      PA:=TPasArgument.Create(CleanPropertyName(AN,ekwEntitySet),F);
      EP:=FindProperty(EID.Element as TPasClassType,AN);
      if Assigned(EP) then
        PA.ArgType:=ResolveType(ID.Schema,EP._Type)
      else
        PA.ArgType:=TPasUnresolvedTypeRef.Create('String',PA);
      PA.Access:=argConst;
      {$IFDEF USECSDL}
        PA.CustomData:=AKey.Item[I];
      {$ELSE}
        PA.CustomData:=AKey.Item[I].Item[0];
      {$ENDIF}
      F.Args.Add(PA);
      end;
     end
   else
     begin
     // Fake string argument
     PA:=TPasArgument.Create('AKey',F);
     PA.ArgType:=TPasUnresolvedTypeRef.Create('String',PA);
     PA.Access:=argConst;
     F.Args.Add(PA);
     end;
  EM.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=ResolveType(ID.Schema,ES.EntityType);
  C.Members.Add(EM);
end;

procedure TEDMX2PasConverter.AddEntityList(ID: TIdentifier;
  ArgType: String; ListAll: Boolean);

Var
  FN : String;
  F  : TPasFunctionType;
  C  : TPasClassType;
  EM : TEntityMethod;
  ES : TEntitySet;
  PA : TPasArgument;

begin
  C:=ID.Element as TPasClassType;
  ES:=C.CustomData as TEntitySet;
  // List function, string version
  if ListAll then
    begin
    FN:='ListAll';
    EM:=TEntityListAll.Create(FN,C);
    end
  else
    begin
    FN:='List';
    EM:=TEntityList.Create(FN,C);
    end;
  EM.CustomData:=ES;
  EM.Visibility:=visPublic;
  F:=TPasFunctionType.Create(FN,EM);
  // Query argument (String or TQueryParam)
  PA:=TPasArgument.Create('AQuery',F);
  PA.ArgType:=TPasUnresolvedTypeRef.Create(ArgType,PA);
  PA.Access:=argConst;
  F.Args.Add(PA);
  if not ListAll then
    begin
    PA:=TPasArgument.Create('NextLink',F);
    PA.ArgType:=TPasUnresolvedTypeRef.Create('String',PA);
    PA.Access:=argOut;
    F.Args.Add(PA);
    end;
  EM.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=ResolveType(ID.Schema,'Collection('+ES.EntityType+')');
  C.Members.Add(EM);
end;

procedure TEDMX2PasConverter.CompleteEntitySet(ID: TIdentifier);

Var
  FN : String;
  EC : TEntityClassFunction;
  F  : TPasFunctionType;
  C  : TPasClassType;
  ES : TEntitySet;
  EID : TIDentifier;
  Multi : Boolean;

begin
  C:=ID.Element as TPasClassType;
  ES:=C.CustomData as TEntitySet;
  Multi:=Not (ES is TImplicitEntitySet);
  If Not Multi then
    Multi:=TImplicitEntitySet(ES).IsColl;
  // Class function
  FN:='EntityClass';
  EC:=TEntityClassFunction.Create(FN,C);
  EC.CustomData:=ES;
  EC.Visibility:=visPublic;
  F:=TPasFunctionType.Create(FN,EC);
  EC.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=TPasUnresolvedTypeRef.Create('TODataEntityClass',F.ResultEl);
  EC.Modifiers:=[pmOverride];
  C.Members.Add(EC);
  EID:=FindIdentifier(Nil,ES.EntityType);// Qualified name
  if EID=Nil then
    Raise EEDMX2PasConverter.CreateFmt('Cannot find type definition %s for entityset %s, to add getter',[ES.EntityType,ES.Name]);
  AddEntityGet(ID,EID);
  if Multi then
    begin
    // List function, string version
    AddEntityList(ID,'String',False);
    AddEntityList(ID,'TQueryParams',False);
    // ListAll
    AddEntityList(ID,'String',True);
    AddEntityList(ID,'TQueryParams',True);
    end;
end;

procedure TEDMX2PasConverter.EntityContainerToIdentifiers(ASchema : TSchema; EC : EntityContainer);

Var
  I : Integer;
  ONS,NS, CN, SchemaPrefix : String;
  P : TPasType;
  ES : TEntitySet;
  EID : TIdentifier;


begin
  ONS:='"'+WTOA(ASchema.Namespace)+'"';
  NS:=GetNameSpace(ASchema);
  if NS<>ONS then
    ONS:=ONS+' as "'+NS+'"';
  SchemaPrefix:=FlattenName(NS);
  For i:=0 to EC.EntitySet.Length-1 do
    begin
    ES:=EC.EntitySet.Item[I];
    CN:=CreateIdentifierName(ASchema,SchemaPrefix,ES.Name+'EntitySet');
    P:=TEntitySetClass.Create(CN,Nil);
    P.CustomData:=ES;
    DoLog('Converting entity set (Schema %s, EntitySet: %s) to %s',[ONS,ES.Name,CN]);
    AddIdentifier(ASchema.Namespace+'.'+ES.Name+'.EntitySet',ASchema,P);
    EID:=Nil;
    EID:=FindIdentifier(Nil,ES.EntityType);// Qualified name
    if EID=Nil then
      Raise EEDMX2PasConverter.CreateFmt('Cannot find type definition %s for entityset %s to mark as identify set',[ES.EntityType,ES.Name]);
    EID.IsEntitySet:=True;
    end;
end;

procedure TEDMX2PasConverter.SchemaToIdentifiers;

Var
  I,J : Integer;
  CN,SchemaPrefix : String;
  ASchema : TSchema;
  CT : TComplexType;
  ENUT : TEnumType;
  ET : TEntityType;
  EC : EntityContainer;
  B : Boolean;
  ONS,NS : String;
  P : TPasType;

begin
  For I:=0 to FSchemaList.Count-1 do
    begin
    ASchema:=TSchema(FSchemaList[i]);
    ONS:='"'+WTOA(ASchema.NameSpace)+'"';
    DoLog('Converting Schema %s, pass 1, enums, complex and entitytypes',[ONS]);
    NS:=GetNameSpace(ASchema);
    if NS<>ONS then
      ONS:=ONS+' as "'+NS+'"';
    // Writeln('Examining ',NS);
    SchemaPrefix:=FlattenName(NS);
    For J:=0 to ASchema.EnumType.Length-1 do
      begin
      ENUT:=ASchema.EnumType.Item[J];
      CN:=CreateIdentifierName(ASchema,SchemaPrefix,ENUT.Name);
      P:=TPasEnumType.Create(CN,Nil);
      P.CustomData:=ENUT;
      AddIdentifier(ASchema.NameSpace+'.'+ENut.Name,ASchema,P);
      end;
    For J:=0 to ASchema.ComplexType.Length-1 do
      begin
      CT:=ASchema.ComplexType.Item[J];
      CN:=CreateIdentifierName(ASchema,SchemaPrefix,CT.Name);
      DoLog('Converting complex type (Schema %s, ComplexType: %s) to %s',[ONS,CT.Name,CN]);
      P:=TComplexClass.Create(CN,Nil);
      P.CustomData:=CT;
      AddIdentifier(ASchema.NameSpace+'.'+CT.Name,ASchema,P);
      end;
    For J:=0 to ASchema.EntityType.Length-1 do
      begin
      ET:=ASchema.EntityType.Item[J];
      CN:=CreateIdentifierName(ASchema,SchemaPrefix,WTOA(ET.Name));
      DoLog('Converted entity type (Schema: %s, EntityType: %s) to %s',[ONS,ET.Name,CN]);
      P:=TEntityClass.Create(CN,Nil);
      P.CustomData:=ET;
      AddIdentifier(ASchema.NameSpace+'.'+ET.Name,ASchema,P);
      end;
    end;
  For I:=0 to FSchemaList.Count-1 do
    begin
    ASchema:=TSchema(FSchemaList[i]);
    DoLog('Converting Schema %s, pass 2, containers,entitytypes, Navigation properties',[ONS]);
    NS:=GetNameSpace(ASchema);
    // Writeln('Examining ',NS);
    SchemaPrefix:=FlattenName(NS);
    {$IFDEF USECSDL}
    EC:=ASchema.EntityContainer;
    if Assigned(EC) and (EC.Name<>'') then
      begin
      CN:=CreateIdentifierName(ASchema,SchemaPrefix,WTOA(EC.Name));
      DoLog('Converted entity container (Schema: %s, EntityContainer: %s) to %s',[ONS,EC.Name,CN]);
      P:=TEntityContainerClass.Create(CN,Nil);
      P.CustomData:=EC;
      AddIdentifier(ASchema.NameSpace+'.'+EC.Name,ASchema,P);
      EntityContainerToIdentifiers(ASchema,EC);
      end;
    {$ELSE}
    For J:=0 to ASchema.EntityContainer.Length-1 do
      begin
      EC:=ASchema.EntityContainer.Item[j];
      CN:=CreateIdentifierName(ASchema,SchemaPrefix,EC.Name);
      DoLog('Converted entity container (Schema: %s", EntityContainer: %s) to %s',[ONS,EC.Name,CN]);
      P:=TEntityContainerClass.Create(CN,Nil);
      P.CustomData:=EC;
      AddIdentifier(ASchema.NameSpace+'.'+EC.Name,ASchema,P);
      EntityContainerToIdentifiers(ASchema,EC);
      end;
    {$ENDIF}
    // Extra loop: Implicit entity sets for contained entities
    For J:=0 to ASchema.EntityType.Length-1 do
      CheckNavigationPropertyEntity(ASchema,ASchema.EntityType[J]);
    {$IFNDEF USECSDL}
    For J:=0 to ASchema._Function.Length-1 do
      if ASchema._Function[J].isBound then
        CheckBoundFunction(ASchema,ASchema._Function[J]);
    For J:=0 to ASchema.Action.Length-1 do
      if ASchema.Action[J].isBound then
        CheckBoundAction(ASchema,ASchema.Action[J]);
    {$ENDIF USECSDL}
    end;
  For I:=0 to FSchemaList.Count-1 do
    begin
    ASchema:=TSchema(FSchemaList[i]);
    {$IFDEF USECSDL}
    B:=Assigned(ASchema.EntityContainer) and (ASchema.EntityContainer.Name<>'');
    {$ELSE}
    B:=ASchema.EntityContainer.Length>0;
    {$ENDIF}
    if B then
      begin
      // Add service.
      CN:='T'+FlattenName(GetNameSpace(ASchema))+'Service';
      DoLog('Service name : %s',[CN]);
      P:=TServiceClass.Create(CN,Nil);
      P.CustomData:=ASchema;
      AddIdentifier(ASchema.Namespace,ASchema,P);
      end;
    end;
end;

procedure TEDMX2PasConverter.AddIdentifier(AIDentifier: TIdentifier);
begin
  //Writeln('Adding identifier : ',AIdentifier.IdentifierName);
  FIdentifierList.add(AIDentifier);
  FIdentifierHash.Add(LowerCase(AIDentifier.IdentifierName),AIdentifier);
end;

function TEDMX2PasConverter.AddIdentifier(const AName: String;
  ASchema: TSchema; El: TPasElement): TIdentifier;
begin
  Result:=TIdentifier.Create(AName,ASchema,El);
  AddIdentifier(Result);
end;

function TEDMX2PasConverter.AddIdentifier(const AName: UnicodeString;
  ASchema: TSchema; El: TPasElement): TIdentifier;
begin
  Result:=AddIdentifier(WTOA(ANAme),ASchema,El);
end;

{$IFNDEF USECSDL}
function TEDMX2PasConverter.CheckBoundFunction(ASchema: TSchema; Fun: TFunction
  ): TPasFunction;

Var
  I : Integer;
  FID : TIdentifier;
  CT : TPasClassType;
  F : TPasFunctionType;
  A : TPasArgument;
  FN : String;
  UEIP : TExtraKeyWords;

begin
   DoLog('Bound function: %s ',[Fun.Name]);
   If Fun.Parameter.Length=0 then
       Raise EEDMX2PasConverter.CreateFmt('Error in EDMX : Bound function %s without parameters',[Fun.Name]);
   FID:=FindIdentifier(Nil,Fun.Parameter[0]._Type);
   If (FID=Nil) then
     Raise EEDMX2PasConverter.CreateFmt('Could not find type %s for bound function %s',[Fun.Parameter[0]._Type,Fun.Name]);
   CT:=FID.Element as TPasClassType;
   UEIP:=UseExtraIdentifierProtection(CT.CustomData);
   FN:=CleanPropertyName(Fun.Name,UEIP);
   Result:=TBoundFunction.Create(FN,CT);
   Result.visibility:=visPublic;
   Result.CustomData:=Fun;
   F:=TPasFunctionType.Create(FN,Result);
   Result.ProcType:=F;
   CT.Members.Add(Result);
   A:=TPasArgument.Create('AService',F);
   F.Args.Add(A);
   A.ArgType:=TPasUnresolvedTypeRef.Create('TODataService',A);
   For I:=1 to Fun.Parameter.Length-1 do
     begin
     A:=TPasArgument.Create(CleanPropertyName(Fun.Parameter[I].Name,UEIP),F);
     F.Args.Add(A);
     A.ArgType:=ResolveType(ASchema,Fun.Parameter[i]._Type);
     A.CustomData:=Fun.Parameter[i];
     end;
   F.ResultEl:=TPasResultElement.Create('Result',F);
   F.ResultEl.ResultType:=ResolveType(ASchema,Fun.ReturnType._Type);
end;

function TEDMX2PasConverter.CheckBoundAction(ASchema: TSchema; Act: TAction
  ): TPasProcedure;

Var
  I : Integer;
  FID : TIdentifier;
  CT : TPasClassType;
  HasResult : Boolean;
  F : TPasFunctionType;
  P : TPasProcedureType;
  A : TPasArgument;
  UEIP : TExtraKeywords;
  AN : String;

begin
   DoLog('Adding Bound Action: %s ',[Act.Name]);
   If Act.Parameter.Length=0 then
       Raise EEDMX2PasConverter.CreateFmt('Error in EDMX : Bound action %s without parameters',[Act.Name]);
   FID:=FindIdentifier(Nil,WTOA(Act.Parameter[0]._Type));
   If (FID=Nil) then
     Raise EEDMX2PasConverter.CreateFmt('Could not find type %s for bound action %s',[Act.Parameter[0]._Type,Act.Name]);
   CT:=FID.Element as TPasClassType;
   UEIP:=UseExtraIdentifierProtection(CT.CustomData);
   AN:=CleanPropertyName(Act.Name,UEIP);
   HasResult:=Assigned(Act.ReturnType) and (Act.ReturnType._Type<>'');
   if HasResult then
     begin
     Result:=TBoundActionFunc.Create(AN,CT);
     F:=TPasFunctionType.Create(AN,Result);
     P:=F;
     end
   else
     begin
     Result:=TBoundActionProc.Create(AN,CT);
     F:=Nil;
     P:=TPasProcedureType.Create(AN,Result);
     end;
   Result.visibility:=visPublic;
   Result.CustomData:=Act;
   Result.ProcType:=P;
   CT.Members.Add(Result);
   A:=TPasArgument.Create('AService',P);
   P.Args.Add(A);
   A.ArgType:=TPasUnresolvedTypeRef.Create('TODataService',A);
   For I:=1 to Act.Parameter.Length-1 do
     begin
     A:=TPasArgument.Create(CleanPropertyName(WTOA(Act.Parameter[I].Name),UEIP),P);
     P.Args.Add(A);
     A.ArgType:=ResolveType(ASchema,Act.Parameter[i]._Type);
     A.CustomData:=Act.Parameter[i];
     end;
   if HasResult then
     begin
     F.ResultEl:=TPasResultElement.Create('Result',F);
     F.ResultEl.ResultType:=ResolveType(ASchema,Act.ReturnType._Type);
     end;
end;
{$ENDIF}

function TEDMX2PasConverter.ExtractBaseTypeName(ASchema: TSchema;
  ATypeName: String; out IsColl: Boolean): String;

Const
  SCollection = 'Collection(';
  LCollection = Length(SCollection);

Var
  L : Integer;

begin
  Result:=ATypeName;
  IsColl:=Copy(Result,1,LCollection)=SCollection;
  if IsColl then
    begin
    Delete(Result,1,LCollection);
    Delete(Result,Length(Result),1);
    end;
  L:=Length(ASchema.Namespace);
  if (Copy(Result,1,L)=ASchema.Namespace) then
    Delete(Result,1,L+1);
end;

function TEDMX2PasConverter.ExtractBaseTypeName(ASchema: TSchema;
  ATypeName: UnicodeString; out IsColl: Boolean): String;
begin
  Result:=ExtractBaseTypeName(ASchema,WTOA(ATypeName),isColl);
end;

procedure TEDMX2PasConverter.CheckNavigationPropertyEntity(ASchema: TSchema;
  AEntity: TEntityType);

Var
  i : integer;
  NP : TNavigationProperty;
  BTN,SchemaPrefix,ONS,NS,ESN,CN,TN : String;
  ESI : TIDentifier;
  P : TEntitySetClass;
  IsColl : Boolean;
  ES : TImplicitEntitySet;
  ATS : TSchema;

begin
  ONS:='"'+WTOA(ASchema.NameSpace)+'"';
  NS:=GetNameSpace(ASchema);
  if NS<>ONS then
    ONS:=ONS+' as "'+NS+'"';
  SchemaPrefix:=FlattenName(NS);
  For I:=0 to AEntity.NavigationProperty.Length-1 do
    begin
    ATS:=ASchema;
    NP:=AEntity.NavigationProperty[i];
//    Writeln('Schema ',ASchema.NameSpace,' type ',AEntity.Name,', Investigating ',I,' : ',NP.Name);
{$IFNDEF USECSDL}
    TN:=WTOA(NP._Type);
{$ELSE}
    TN:=FindAssociatedType(ATS,WTOA(NP.Relationship),WTOA(NP.ToRole));
{$ENDIF}
    BTN:=ExtractBaseTypeName(ATS,TN,isColl);
    ESI:=FindEntitySetForEntity(ATS,BTN);
    If (ESI=Nil) then
      begin
      ESN:=BTN+'ImplicitEntitySet';
      CN:=CreateIdentifierName(ATS,SchemaPrefix,ESN);
      P:=TEntitySetClass.Create(CN,Nil);
      ES:=TImplicitEntitySet.Create(NP,WTOA(ATS.NameSpace)+'.'+BTN,isColl);
      FFreeObjects.Add(ES);
      ES.Name:=ESN;
      P.CustomData:=ES;
      DoLog('Converting implicit entity set for navigation property (Schema: %s, Entity: %s, Property: %s, Type: %s, Type namespace: %s) to %s',[ONS,AEntity.Name, NP.Name,TN,ATS.Namespace,CN]);
      AddIdentifier(NS+'.'+ESN,ATS,P);
      end;
    end;
end;

procedure TEDMX2PasConverter.CompleteEnumerator(ID: TIdentifier);

Var
  I : integer;
  PE : TPasEnumType;
  PV : TPasEnumValue;
  XE : TEnumType;
  XM : TEnumTypeMember;
  EN : String;

begin
  PE:=ID.Element as TPasEnumType;
  XE:=PE.CustomData as TEnumType;
  For I:=0 to XE.Member.Length-1 do
    begin
    XM:=XE.Member[I];
    EN:=WTOA(XM.Name);
    if EnumerationMode = emPrefixTypeName then
      EN:=PE.Name+'_'+EN;
    PV:=TPasEnumValue.Create(EN,PE);
    PE.Values.Add(PV);
    end;
end;

procedure TEDMX2PasConverter.GenerateBaseClass(ID: TIDentifier);

Var
  PC : TPAsClassType;
  K : TObjectRestKind;
  F : TPasFunctionType;

begin
  PC:=ID.Element as TPasClassType;
  K:=TObjectRestKind.Create('ObjectRestKind',PC);
  K.Modifiers:=[pmOverride];
  F:=TPasFunctionType.Create('ObjectRestKind',K);
  K.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=TPasUnresolvedTypeRef.Create('String',F);
  K.Visibility:=visPublic;
  PC.Members.Add(K);
end;

procedure TEDMX2PasConverter.CompleteIdentifiers;

Var
  I : Integer;
  Id : TIdentifier;
  El : TPasElement;

begin
  For I:=0 to FIdentifierList.Count-1 do
    begin
    Id:=FIdentifierList[i] as TIdentifier;
    El:=Id.Element;
    if Assigned(EL) then
      begin
      DoLog('Completing identifier %d : %s',[I,EL.Name]);
      if El.InheritsFrom(TPasEnumType) then
        CompleteEnumerator(ID);
      if El.InheritsFrom(TPasClassType) then
        begin
        GenerateBaseClass(ID);
        if El.CustomData.InheritsFrom(EntityContainer) then
          CompleteContainer(ID)
        else if El.CustomData.InheritsFrom(TComplexType) then
          CompleteComplexType(ID)
        else if El.CustomData.InheritsFrom(TEntityType) then
          CompleteEntityType(ID)
        end;
      end;
    end;
  For I:=0 to FIdentifierList.Count-1 do
    begin
    Id:=FIdentifierList[i] as TIdentifier;
    El:=Id.Element;
    if Assigned(EL) then
      begin
      DoLog('Completing identifier %d : %s',[I,EL.Name]);
      if El.CustomData.InheritsFrom(TEntitySet) then
        CompleteEntitySet(ID)
      else if El.CustomData.InheritsFrom(TSchema) then
        CompleteSchema(ID);
      end;
    end;
  DoLog('Done completing identifiers');
end;

procedure TEDMX2PasConverter.LoadFromStream(const AStream: TStream);

begin
  FXML.CopyFrom(AStream,0);
  FXML.Position:=0;
end;

procedure TEDMX2PasConverter.AddContainerToSchema(ID: TIdentifier;
  AIndex: Integer; E: EntityContainer);


Var
  F : TPasFunctionType;
  CC :  TCreateContainer;
  CN,FN : string;
  ST : TPasClassType;

begin
  CN:=CleanPropertyName(E.Name,ekwService);
  // Creator function
  ST:=ID.Element as TPasClassType;
  FN:='CreateNew'+CN;
  CC:=TCreateContainer.Create(FN,ST);
  CC.Visibility:=visPublic;
  F:=TPasFunctionType.Create(FN,CC);
  CC.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=ResolveType(ST.CustomData as TSchema,E.Name);
  ST.Members.Add(CC);
  // Property
  AddProperty(ID,AIndex,CN,WTOA(E.Name),[pfNeedGetter,pfNeedSetter,pfReadOnly],E);
end;

procedure TEDMX2PasConverter.CompleteSchema(ID : TIdentifier);

Var
  C : TPasClassType;
  ASchema : TSchema;
  EC : EntityContainer;
  {$IFNDEF USECSDL}
  I : Integer;
  {$ENDIF}

begin
  C:=ID.Element as TPasClassType;
  ASchema:=C.CustomData as TSchema;
  {$IFDEF USECSDL}
  EC:=ASchema.EntityContainer;
  if Assigned(EC) then
    AddContainerToSchema(ID,0,EC);
  {$ELSE}
  For I:=0 to ASchema.EntityContainer.Length-1 do
    begin
    EC:=ASchema.EntityContainer.Item[I];
    AddContainerToSchema(ID,I,EC);
    end;
  {$ENDIF}
end;

procedure TEDMX2PasConverter.AddEntitySet(ID: TIDentifier; ES: TEntitySet;
  AIndex: Integer);

Var
  C : TPasClassType;
  F : TPasFunctionType;
  CC :  TCreateEntitySet;
  EN,FN : string;

begin
  C:=ID.Element as TPasClassType;
  EN:=CleanPropertyName(ES.Name,ekwEntityContainer);
  // Creator function
  FN:='CreateNew'+EN;
  CC:=TCreateEntitySet.Create(FN,C);
  CC.Visibility:=visPublic;
  F:=TPasFunctionType.Create(FN,CC);
  CC.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=ResolveType(ID.Schema,EN+'EntitySet');
  C.Members.Add(CC);
  // Property
  AddProperty(ID,AIndex,EN,EN+'EntitySet',[pfNeedGetter,pfReadOnly],ES);

end;

{$IFNDEF USECSDL}
procedure TEDMX2PasConverter.AddSingleTon(ID: TIDentifier; S : TSingleton; AIndex : integer);

Var
  C : TPasClassType;
  GS : TGetSingleton;
  SN,FN : string;
  F: TPasFunctionType;

begin
  C:=ID.Element as TPasClassType;
  // Writeln('Examining ',NS);
  SN:=CleanPropertyName(S.Name,UseExtraIdentifierProtection(C.CustomData));
  FN:='Fetch'+SN;
  GS:=TGetSingleton.Create(FN,C);
  GS.Visibility:=visPublic;
  GS.CustomData:=S;
  F:=TPasFunctionType.Create(FN,GS);
  GS.ProcType:=F;
  F.ResultEl:=TPasResultElement.Create('Result',F);
  F.ResultEl.ResultType:=ResolveType(ID.Schema,S._type);
  C.Members.Add(GS);
  AddProperty(ID,Aindex,S.Name,S._type,[pfNeedGetter,pfReadOnly],S);
end;
{$ENDIF}

procedure TEDMX2PasConverter.CompleteContainer(ID : TIdentifier);

Var
  C : TPasClassType;
  CT : EntityContainer;
  I : integer;

begin
  C:=ID.Element as TPasClassType;
  CT:=ID.Element.CustomData as EntityContainer;
  C.AncestorType:=TPasUnresolvedTypeRef.Create(BaseEntityContainerType,Nil);
  for I:=0 to CT.EntitySet.Length-1 do
    AddEntitySet(ID,CT.EntitySet[i],I);
{$IFNDEF USECSDL}
  for I:=0 to CT.Singleton.Length-1 do
    AddSingleton(ID,CT.Singleton[i],I);
  For i:=0 to CT.ActionImport.Length-1 do
    AddImportAction(ID,CT.ActionImport[I],i);
{$ENDIF}
  For i:=0 to CT.FunctionImport.Length-1 do
    AddImportFunction(ID,CT.FunctionImport[I]);
end;

procedure TEDMX2PasConverter.AddSetArrayLength(ID: TIdentifier);
Var
  CT : TPasClassType;
  P : TPasProcedureType;
  A : TPasArgument;
  SAR : TSetArrayLength;

begin
   DoLog('Adding AddSetArrayLength for class %s',[ID.Element.Name]);
   CT:=ID.Element as TPasClassType;
  //  Procedure SetArrayLength(const AName : String; ALength : Longint); virtual;
   SAR:=TSetArrayLength.Create('SetArrayLength',CT);
   SAR.visibility:=visProtected;
   SAR.CustomData:=CT.CustomData;
   P:=TPasProcedureType.Create('SetArrayLength',SAR);
   SAR.ProcType:=P;
   SAR.Modifiers:=[pmOverride];
   CT.Members.Add(SAR);
   // Arguments: AName: String
   A:=TPasArgument.Create('AName',P);
   A.Access:=argConst;
   P.Args.Add(A);
   A.ArgType:=TPasUnresolvedTypeRef.Create('String',A);
   // Arguments: ALength : Longint;
   A:=TPasArgument.Create('ALength',P);
   P.Args.Add(A);
   A.ArgType:=TPasUnresolvedTypeRef.Create('Longint',A);
end;


{$IFDEF USECSDL}
procedure TEDMX2PasConverter.AddImportFunction(ID : TIdentifier; AFun : TFunctionImport);


begin
  // Just some code to make the compiler happy
  if Not (Assigned(ID) and Assigned(AFun)) then
    exit

end;


{$ELSE}


function TEDMX2PasConverter.AddUnboundFunction(ID: TIdentifier; APath: String;
  Fun: TFunction; AIndex: Integer): TPasFunction;

Var
  I : Integer;
  CT : TPasClassType;
  F : TPasFunctionType;
  A : TPasArgument;
  UEIP : TExtraKeywords;
  FN : String;

begin
   DoLog('Adding Unbound function [%d]: %s ',[AIndex,Fun.Name]);
   CT:=ID.Element as TPasClassType;
   UEIP:=UseExtraIdentifierProtection(CT.CustomData);
   FN:=CleanPropertyName(Fun.Name,UEIP);
   Result:=TUnBoundFunction.Create(FN,CT);
   TUnBoundFunction(Result).ExportPath:=APath;
   Result.visibility:=visPublic;
   Result.CustomData:=Fun;
   F:=TPasFunctionType.Create(FN,Result);
   Result.ProcType:=F;
   CT.Members.Add(Result);
   For I:=0 to Fun.Parameter.Length-1 do
     begin
     A:=TPasArgument.Create(CleanPropertyName(WTOA(Fun.Parameter[I].Name),UEIP),F);
     F.Args.Add(A);
     A.ArgType:=ResolveType(ID.Schema,Fun.Parameter[i]._Type);
     A.CustomData:=Fun.Parameter[i];
     end;
   F.ResultEl:=TPasResultElement.Create('Result',F);
   F.ResultEl.ResultType:=ResolveType(ID.Schema,Fun.ReturnType._Type);
end;

function TEDMX2PasConverter.AddUnboundAction(ID: TIdentifier; APath: String;
  Act: TAction; AIndex: integer): TPasProcedure;

Var
  I : Integer;
  CT : TPasClassType;
  F : TPasFunctionType;
  P : TPasProcedureType;
  A : TPasArgument;
  HasResult : Boolean;
  UEIP : TExtraKeywords;
  AN : String;

begin
   DoLog('Adding Unbound Action [%d]: %s ',[AIndex,Act.Name]);
   CT:=ID.Element as TPasClassType;
   UEIP:=UseExtraIdentifierProtection(CT.CustomData);
   AN:=CleanPropertyName(Act.Name,UEIP);
   HasResult:=Assigned(Act.ReturnType) and (Act.ReturnType._Type<>'');
   if HasResult then
     begin
     Result:=TUnBoundActionFunc.Create(AN,CT);
     TUnBoundActionFunc(Result).ExportPath:=APath;
     F:=TPasFunctionType.Create(AN,Result);
     P:=F;
     end
   else
     begin
     Result:=TUnBoundActionProc.Create(AN,CT);
     TUnBoundActionProc(Result).ExportPath:=APath;
     F:=Nil;
     P:=TPasProcedureType.Create(AN,Result);
     end;
   Result.visibility:=visPublic;
   Result.CustomData:=Act;
   Result.ProcType:=P;
   CT.Members.Add(Result);
   For I:=0 to Act.Parameter.Length-1 do
     begin
     A:=TPasArgument.Create(AN,F);
     F.Args.Add(A);
     A.ArgType:=ResolveType(ID.Schema,Act.Parameter[i]._Type);
     A.CustomData:=Act.Parameter[i];
     end;
   If Assigned(F) then
     begin
     F.ResultEl:=TPasResultElement.Create('Result',F);
     F.ResultEl.ResultType:=ResolveType(ID.Schema,Act.ReturnType._Type);
     end;
end;

procedure TEDMX2PasConverter.AddImportFunction(ID : TIdentifier; AFun : TFunctionImport);

Var
  I : Integer;
  L : TFPList;

begin
  L:=TFPList.Create;
  try
    For I:=0 to ID.Schema._Function.Length-1 do
      if (ID.Schema.Namespace+'.'+ID.Schema._Function[i].Name=AFun._Function) then
        L.Add(ID.Schema._Function[i]);
    if L.Count=0 then
       Raise EEDMX2PasConverter.CreateFmt('No function name %s found for importfunction %s',[AFun._Function,AFun.Name]);
    For I:=0 to L.Count-1 do
     AddUnBoundFunction(ID,AFun.Name,TFunction(L[i]),I);
  finally
    L.Free;
  end;
end;

procedure TEDMX2PasConverter.AddImportAction(ID : TIdentifier; Act : TActionImport; AIndex : Integer);

Var
  I : Integer;
  L : TFPList;

begin
  L:=TFPList.Create;
  try
    For I:=0 to ID.Schema.action.Length-1 do
      if (ID.Schema.Namespace+'.'+ID.Schema.action[i].Name=Act.Action) then
        L.Add(ID.Schema.Action[i]);
    if L.Count=0 then
       Raise EEDMX2PasConverter.CreateFmt('No Action name %s found for importaction %d: %s',[Act.Action,AIndex, Act.Name]);
    For I:=0 to L.Count-1 do
      AddUnBoundAction(ID,Act.Name,TAction(L[i]),I);
  finally
    L.Free;
  end;
end;
{$ENDIF}

procedure TEDMX2PasConverter.EmitImplementation;

Var
  ID : TIdentifier;
  I : integer;

begin
  For I:=0 to FIdentifierList.Count-1 do
    begin
    ID:=TIdentifier(FIdentifierList[I]);
    If ID.Element is TPasClasstype then
      EmitClassImplementation(ID);
    end;
end;


procedure TEDMX2PasConverter.Execute;

begin
  AnalyseXML;
  RegisterBaseTypes;
  SchemaToIdentifiers;
  CompleteIdentifiers;
  Source.Clear;
  Addln('unit '+OutputUnitName+';');
  CreateHeader;
  EmitOptions;
  EmitInterface;
  AddLn('');
  AddLn('implementation');
  AddLn('');
  EmitImplementation;
  AddLn('end.');
  DoLog('All done');
end;

end.

