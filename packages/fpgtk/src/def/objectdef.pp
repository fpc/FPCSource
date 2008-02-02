{$mode delphi}{$h+}
unit ObjectDef;
{_$define writecreate}{_$define loaddebug}
interface

uses
  sysutils, Classes;

const
  VersionNumber = '1.08';

type

  TLukStepitProc = procedure of Object;
  TLukStepitMaxProc = procedure (Max : integer) of Object;

  TInterfaceSection = (isPrivate,isProtected,isPublic,isPublished);
  TPropType = (ptField,ptProperty,ptFunction,ptProcedure,ptSignal,
               ptHelperProc,ptHelperFunc,ptSignalType,ptDeclarations,ptTypeDecl,
               ptConstructor,ptDestructor,ptInitialization, ptFinalization);
  TpropFuncType = (pftGtkFunc,pftObjField,pftObjFunc,pftField,pftProc,pftNotImplemented,
                   pftGtkMacro,pftExistingProc);
  TParamType = (ptNone,ptVar,ptConst);
  TProcType = (ptOverride, ptVirtual, ptDynamic, ptAbstract, ptCdecl,
               ptOverload, ptReintroduce);
  TProcTypeSet = set of TProcType;

  TObjectDefs = class;
  TObjectItem = class;
  TPropertyItem = class;


  TParameterItem = class (TCollectionItem)
  private
    FName : string;
    FConvert: boolean;
    FpascalType: string;
    FParamType: TParamType;
  protected
    function GetDisplayName : string; override;
    procedure SetDisplayName(Const Value : string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create (ACollection : TCollection); override;
    destructor destroy; override;
  published
    property Name : string read FName write FName;
    { De naam van de parameter }
    property PascalType : string read FpascalType write FPascalType;
    { Zijn type }
    property Convert : boolean read FConvert write FConvert default false;
    { geeft aan of er een omzetting dient te gebeuren voor het gebruiken }
    property ParamType : TParamType read FParamType write FParamType default ptNone;
    { het type van parameter : var, const of niets }
  end;

  TParamCollection = class (TCollection)
  private
    FProcedure : TPropertyItem;
    function GetItem(Index: Integer): TParameterItem;
    procedure SetItem(Index: Integer; const Value: TParameterItem);
  protected
    function GetOwner : TPersistent; override;
  public
    constructor create (AOwner : TPropertyItem);
    property Items[Index: Integer]: TParameterItem read GetItem write SetItem; default;
  end;


  TPropertyItem = class (TCollectionItem)
  private
    FPropType : TPropType;
    FName: string;
    FSection: TInterfaceSection;
    FPascalType: string;
    FParameters: TParamCollection;
    FGtkName: string;
    FWriteProcType: TpropFuncType;
    FReadFuncType: TPropFuncType;
    FWriteGtkName: string;
    FCode: TStringList;
    FWriteCode: TStringList;
    FProctypes: TProcTypeSet;
    FWriteConvert: boolean;
    FReadConvert: boolean;
    procedure SetCode(const Value: TStringList);
    procedure SetWriteCode(const Value: TStringList);
    procedure SetPropType(const Value: TPropType);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor create (ACollection : TCollection); override;
    destructor destroy; override;
  published
    property PropType : TPropType read FPropType write SetPropType default ptProcedure;
    { wat voor iets het is } // Moet voor DisplayName staan voor goede inleesvolgorde
    property Name : string read FName write FName;
    { Naam van de property/functie/proc/veld/... }
    property Section : TInterfaceSection read FSection write FSection default isPublic;
    { waar het geplaats moet worden private, public, ... }
    property PascalType : string read FPascalType write FPascalType;
    { het type van property, functie, veld, signal (moet dan wel gedefinieerd zijn) }
    property Parameters : TParamCollection read FParameters write FParameters;
    { de parameters die doorgegeven moeten worden via de functie/procedure/signaltype }
    property GtkName : string read FGtkName write FGtkName;
    { de naam zoals GTK die gebruikt (waarschijnlijk met _ in) }
    property Code : TStringList read FCode write SetCode;
  { Property specifiek }
    // ReadGtkName wordt weggeschreven in GtkName
    // ReadCode wordt weggeschreven in Code
    // parameters worden gebruikt om indexen aan te geven
    property ReadFuncType : TPropFuncType read FReadFuncType write FReadFuncType default pftGtkFunc;
    { hoe de read functie moet werken : gtk-functie, object-veld, object-functie, eigen functie }
    property ReadConvert : boolean read FReadConvert write FReadConvert default false;
    { Geeft aan of de waarde voor toekenning aan result moet omgezet worden }
    property WriteProcType : TpropFuncType read FWriteProcType write FWriteProcType default pftGtkFunc;
    { hoe de write functie moet werken : gtk-proc, object-veld, object-proc, eigen proc }
    property WriteGtkName : string read FWriteGtkName write FWriteGtkName;
    { de naam zoals gtk of object die gebruikt. Gebruikt in write, voor read zie GtkName }
    property WriteConvert : boolean read FWriteConvert write FWriteConvert default false;
    { Geeft aan of de waarde moet omgezet worden voor het doorgeven }
    property WriteCode : TStringList read FWriteCode write SetWriteCode;
  { procedure specifiek } //gebruikt code
    property ProcTypes : TProcTypeSet read FProctypes write FProcTypes default [];
    { Duid het type procedure/functie aan : abstract, virtual, ... }
  end;

  TPropertyCollection = class (TCollection)
  private
    FObject : TobjectItem;
    function GetItem(Index: Integer): TPropertyItem;
    procedure SetItem(Index: Integer; const Value: TPropertyItem);
  protected
    function GetOwner : TPersistent; override;
  public
    constructor create (AOwner : TObjectItem);
    property Items[Index: Integer]: TPropertyItem read GetItem write SetItem; default;
  end;


  TObjectItem = class (TCollectionItem)
  private
    FInherit: string;
    FName: string;
    FProps: TPropertyCollection;
    FGtkFuncName: string;
    FWithPointer: boolean;
    FCreateObject: boolean;
    FGtkName: string;
    FCreateParams: string;
    procedure SetProps(const Value: TPropertyCollection);
    procedure SetGtkFuncName(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor create (ACollection : TCollection); override;
    destructor destroy; override;
  published
    property Name : string read FName write FName;
    { Naam van het object }
    property Inherit : string read FInherit write FInherit;
    { De naam van het object dat ancester is }
    property GtkFuncName : string read FGtkFuncName write SetGtkFuncName;
    { Naam van het object in gtk zoals het in de functies en procedures gebruikt wordt }
    property GtkName : string read FGtkName write FGtkName;
    { Naam van het objectrecord in gtk zoals gebruikt in typedeclaraties}
    property Props : TPropertyCollection read FProps write SetProps;
    { De verschillende properties, procedures, ... van en voor het object }
    property WithPointer : boolean read FWithPointer write FWithPointer default false;
    { duid aan of er ook een pointerdefinitie moet zijn }
    property CreateObject : boolean read FCreateObject write FCreateObject default false;
    { duid aan of er een CreateGtkObject procedure moet aangemaakt worden }
    property CreateParams : string read FCreateParams write FCreateParams;
    { Geeft de parameters die meegeven moeten worden aan de _New functie }
  end;

  TObjectCollection = class (TCollection)
  private
    FGtkDEf : TObjectDefs;
    function GetItem(Index: Integer): TObjectItem;
    procedure SetItem(Index: Integer; const Value: TObjectItem);
  protected
    function GetOwner : TPersistent; override;
  public
    constructor create (AOwner : TObjectDefs);
    property Items[Index: Integer]: TObjectItem read GetItem write SetItem; default;
  end;


  TObjectDefs = class(TComponent)
  private
    FDefinition: TObjectCollection;
    FGtkPrefix,
    FUsesList,
    FUnitName: string;
    {$IFNDEF Delphi}
    FTop, FLeft : integer;
    {$ENDIF}
    procedure SetDefinition(const Value: TObjectCollection);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor create (AOwner : TComponent); override;
    destructor destroy; override;
    procedure Write (TheUnit : TStrings; StepIt : TLukStepItProc; StepItMax : TLukStepItMaxProc);
    procedure Save (List : TStrings);
    procedure Load (List : TStrings);
  published
    { Published declarations }
    property Definition : TObjectCollection read FDefinition write SetDefinition;
    property GtkPrefix : string read FGtkPrefix write FGtkPrefix;
    property UnitName : string read FUnitName write FUnitName;
    property UsesList : string read FUsesList write FUsesList;
    {$IFNDEF delphi}
    // Compatibiliteit met Delphi
    property Left : integer read FLeft write FLeft;
    property Top : integer read FTop write FTop;
    {$ENDIF}
  end;

var
  GtkPrefix : string = 'gtk';
  ObjectsPrefix : string = 'FPgtk';

procedure Register;

implementation

//uses dsgnIntf;

const
  SectPublic = [isPublic,isPublished];
  SectPriv = [isPrivate,isProtected];
  CRLF = #13#10;
  PropUsesGtkName = [pftProc, pftExistingProc];

var
  lowerObjectsPrefix : string;
  ObjectsPrefixLength : integer;

procedure Register;
begin
  RegisterComponents('Luk', [TObjectDefs]);
end;

{ TParamCollection }

constructor TParamCollection.create(AOwner: TPropertyItem);
begin
  inherited Create (TParameterItem);
  FProcedure := AOwner;
end;

function TParamCollection.GetItem(Index: Integer): TParameterItem;
begin
  result := TParameterItem (inherited Items[index]);
end;

function TParamCollection.GetOwner: TPersistent;
begin
  result := FProcedure;
end;

procedure TParamCollection.SetItem(Index: Integer;
  const Value: TParameterItem);
begin
  inherited Items[Index] := Value;
end;

{ TParameterItem }

procedure TParameterItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TParameterItem then
    with TParameterItem(Dest) do
      begin
      FName := Self.FName;
      FConvert := Self.FConvert;
      FpascalType := Self.FpascalType;
      FParamType := Self.FParamType;
      end
  else
    inherited;
end;

constructor TParameterItem.Create(ACollection: TCollection);
begin
  inherited;
  FConvert := False;
  FParamType := ptNone;
end;

destructor TParameterItem.destroy;
begin
  inherited;
end;

function TParameterItem.GetDisplayName: string;
begin
  result := FName;
end;

procedure TParameterItem.SetDisplayName(const Value: string);
begin
  FName := Value;
end;


{ TPropertyItem }

procedure TPropertyItem.AssignTo(Dest: TPersistent);
var r : integer;
begin
  if Dest is TPropertyItem then
    with TPropertyItem(Dest) do
      begin
      FPropType := Self.FPropType;
      FName := Self.FName;
      FSection := Self.FSection;
      FPascalType := Self.FPascalType;
      FParameters.clear;
      for r := 0 to pred(self.FParameters.count) do
        FParameters.Add.assign (self.FParameters[r]);
      FGtkName := Self.FGtkName;
      FWriteProcType := Self.FWriteProcType;
      FReadFuncType := Self.FReadFuncType;
      FWriteGtkName := Self.FWriteGtkName;
      FCode.Assign(Self.FCode);
      FWriteCode.assign(Self.FWriteCode);
      FProctypes := Self.FProctypes;
      FWriteConvert := Self.FWriteConvert;
      FReadConvert := Self.FReadConvert;
      end
  else
    inherited;
end;

constructor TPropertyItem.create(ACollection: TCollection);
begin
  inherited;
  FParameters := TParamCollection.Create (Self);
  FPropType := ptProcedure;
  FSection := isPublic;
  FCode := TStringList.Create;
  FWriteCode := TStringList.Create;
  {$IFDEF writecreate}
  writeln ('Property Item created');
  {$ENDIF}
end;

destructor TPropertyItem.destroy;
begin
  FParameters.Free;
  inherited;
end;

const
  DispPropType : array [TPropType] of string =
    ('Field','Property','Function','Procedure', 'Signal',
     'HelperProc','HelperFunc','SignalType','Declarations', 'TypeDeclaration',
     'Constructor','Destructor','Initialization','Finilization');

function TPropertyItem.GetDisplayName: string;
begin
  if FPropType = ptDeclarations then
    if Section = ispublished then
      result := 'Interface code before'
    else if Section = ispublic then
      result := 'Interface code after'
    else
      result := 'Implementation code'
  else
    begin
    result := DispProptype[FPropType];
    if FPropType in [ptInitialization, ptFinalization] then
      result := result + ' code'
    else
      result := FName + ' (' + result + ')';
    end;
end;

procedure TPropertyItem.SetCode(const Value: TStringList);
begin
  FCode.assign (Value);
end;

procedure TPropertyItem.SetDisplayName(const Value: string);
begin
  FName := Value;
end;

procedure TPropertyItem.SetPropType(const Value: TPropType);
begin
  FPropType := Value;
end;

procedure TPropertyItem.SetWriteCode(const Value: TStringList);
begin
  FWriteCode.assign (Value);
end;

{ TPropertyCollection }

constructor TPropertyCollection.create (AOwner : TObjectItem);
begin
  inherited create (TPropertyItem);
  FObject := AOwner;
end;

function TPropertyCollection.GetItem(Index: Integer): TPropertyItem;
begin
  result := TPropertyItem(inherited items[index]);
end;

function TPropertyCollection.GetOwner: TPersistent;
begin
  result := FObject;
end;

procedure TPropertyCollection.SetItem(Index: Integer;
  const Value: TPropertyItem);
begin
  Inherited Items[index] := Value;
end;


{ TObjectItem }

procedure TObjectItem.AssignTo(Dest: TPersistent);
var r : integer;
begin
  if Dest is TObjectItem then
    with TObjectItem(Dest) do
      begin
      FName := self.FName;
      FProps.clear;
      for r := 0 to pred(Self.FProps.count) do
        FProps.Add.assign (self.FProps[r]);
      FInherit := Self.FInherit;
      FGtkFuncName := Self.FGtkFuncName;
      FWithPointer := Self.FWithPointer;
      FCreateObject := Self.FCreateObject;
      FGtkName := Self.FGtkName;
      FCreateParams := Self.FCreateParams;
      end
  else
    inherited;
end;

constructor TObjectItem.create(ACollection: TCollection);
begin
  inherited create (ACollection);
  FProps := TpropertyCollection.Create (Self);
end;

destructor TObjectItem.destroy;
begin
  FProps.Free;
  inherited;
end;

function TObjectItem.GetDisplayName: string;
begin
  result := FName;
end;

procedure TObjectItem.SetDisplayName(const Value: string);
begin
  FName := Value;
end;

procedure TObjectItem.SetGtkFuncName(const Value: string);
begin
  FGtkFuncName := Value;
  {$IFDEF writecreate}
  writeln ('GtkFuncname = ', Value);
  {$ENDIF}
end;

procedure TObjectItem.SetProps(const Value: TPropertyCollection);
begin
  FProps.assign(Value);
end;

{ TObjectCollection }

constructor TObjectCollection.create (AOwner : TObjectDefs);
begin
  inherited create (TObjectItem);
  FGtkDef := AOwner;
end;

function TObjectCollection.GetItem(Index: Integer): TObjectItem;
begin
  result := TObjectItem(inherited Items[index]);
end;

function TObjectCollection.GetOwner: TPersistent;
begin
  result := FGtkDef;
end;

procedure TObjectCollection.SetItem(Index: Integer;
  const Value: TObjectItem);
begin
  inherited items[index] := Value;
end;


{ TObjectDefs }

constructor TObjectDefs.create (AOwner : TComponent);
begin
  inherited create (AOwner);
  FDefinition := TObjectCollection.Create (self);
  FgtkPrefix := 'gtk';
end;

destructor TObjectDefs.destroy;
begin
  FDefinition.Free;
  inherited;
end;

procedure TObjectDefs.SetDefinition(const Value: TObjectCollection);
begin
  FDefinition.assign(Value);
end;

const
  DispPropFuncType : array [TPropFuncType] of string = ('GtkFunc','ObjField',
      'ObjFunc','Field','Proc','NotImplemented','GtkMacro','ExistingProc');
  DispProcType : array [TProcType] of string = ('Override', 'Virtual', 'Dynamic',
      'Abstract', 'Cdecl', 'Overload', 'Reintroduce');

procedure TObjectDefs.Save (List : TStrings);

  procedure WriteParameter (AParameter : TParameterItem);
  begin
    with AParameter do
      begin
      List.Add ('      Param=' + FName);
      if FConvert then
        List.Add ('        Convert');
      if FpascalType <> '' then
        List.Add ('        PascalType=' + FpascalType);
      if FParamType = ptVar then
        List.Add ('        ParamType=Var')
      else if FParamType = ptConst then
        List.Add ('        ParamType=Const');
      end;
  end;

  procedure WriteProperty (AProperty : TPropertyItem);
  var r : integer;
      pt : TProcType;
  begin
    with AProperty do
      begin
      List.Add ('    Prop=' + FName);
      List.Add ('      PropType='+DispPropType[FPropType]);
      if FSection = isprivate then
        List.Add ('      Section=Private')
      else if FSection = isprotected then
        List.Add ('      Section=Protected')
      else if FSection = isPublished then
        List.Add ('      Section=Published');
      if FPascalType <> '' then
        List.Add ('      PascalType=' + FPascalType);
      if FGtkName <> '' then
        List.Add ('      GtkName=' + FGtkName);
      if Fcode.count > 0 then
        List.Add ('      Code='+FCode.Commatext);
      if FReadConvert then
        List.Add ('      ReadConvert');
      if FReadFuncType <> pftGtkFunc then
        List.Add ('      ReadFuncType='+ DispPropFuncType[FReadFuncType]);
      if FWriteProcType <> pftGtkFunc then
        List.Add ('      WriteProcType='+ DispPropFuncType[FWriteProcType]);
      if FWriteGtkName <> '' then
        List.Add ('      WriteGtkName=' + FWriteGtkName);
      if FWritecode.count > 0 then
        List.Add ('      WriteCode='+FWriteCode.Commatext);
      if FWriteConvert then
        List.Add ('      WriteConvert');
      if FProcTypes <> [] then
        for pt := low(TProcType) to high(TProcType) do
          if pt in FProcTypes then
            List.Add ('      '+DispProcType[pt]);
      with FParameters do
        begin
        List.Add ('      Count='+inttostr(Count));
        for r := 0 to count-1 do
          WriteParameter (Items[r]);
        end;
      end;
  end;

  procedure WriteObject (AnObject : TObjectItem);
  var r : integer;
  begin
    with AnObject do
      begin
      List.Add ('  Object=' + FName);
      if FInherit <> '' then
        List.Add ('    Inherit=' + FInherit);
      if FGtkFuncName <> '' then
        List.Add ('    GtkFuncName=' + FGtkFuncName);
      if FGtkName <> '' then
        List.Add ('    GtkName=' + FGtkName);
      if FCreateParams <> '' then
        List.Add ('    CreateParams=' + FCreateParams);
      if FWithPointer then
        List.Add ('    WithPointer');
      if FCreateObject then
        List.Add ('    CreateObject');
      with FProps do
        begin
        List.Add ('    Count='+inttostr(count));
        for r := 0 to count-1 do
          WriteProperty (Items[r]);
        end;
      end;
  end;

var r : integer;
begin
  List.Add ('definition');
  if FGtkPrefix <> '' then
    List.Add ('  GtkPrefix=' + FGtkPrefix);
  if FUsesList <> '' then
    List.Add ('  UsesList=' + FUsesList);
  if FUnitName <> '' then
    List.Add ('  UnitName=' + FUnitName);
  with Definition do
    begin
    List.Add ('  Count=' + inttostr(count));
    for r := 0 to count-1 do
      WriteObject (Items[r])
    end;
end;

resourcestring
  sErrWrongFirstLine = 'Error: First line doesn''t contain correct word';
  sErrCountExpected = 'Error: "Count" expected on line %d';
  sErrObjectExpected = 'Error: "Object" expected on line %d';
  sErrPropertyExpected = 'Error: "Prop" expected on line %d';
  sErrProptypeExpected = 'Error: "PropType" expected on line %d';
  sErrParameterExpected = 'Error: "Param" expected on line %d';

procedure TObjectDefs.Load (List : TStrings);

var line : integer;
    item, value : string;
    HasLine : boolean;

  procedure SplitNext;
  var p : integer;
  begin
    inc (line);
    HasLine := (line < List.Count);
    if HasLine then
      begin
      item := List[Line];
      p := pos ('=', item);
      if p = 0 then
        value := ''
      else
        begin
        value := copy(item, p+1, maxint);
        item := copy(item, 1, p-1);
        end;
      end
    else
      begin
      Item := '';
      value := '';
      end;
  end;

  procedure ReadParameter (AParameter : TParameterItem);
  begin
    with AParameter do
      begin
      if HasLine and (item = '      Param') then
        begin
        FName := value;
        {$ifdef LoadDebug}writeln ('    Parameter Name ', FName);{$endif}
        SplitNext;
        end
      else
        raise exception.CreateFmt (sErrParameterExpected, [line]);
      if HasLine then
        begin
        FConvert := (item = '        Convert');
        {$ifdef LoadDebug}writeln ('              Convert ', FConvert);{$endif}
        if FConvert then
          SplitNext;
        end;
      if HasLine and (item = '        PascalType') then
        begin
        FPascalType := value;
        {$ifdef LoadDebug}writeln ('              PascalType ', FPascalType);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '        ParamType') then
        begin
        if Value = 'Var' then
          FParamType := ptVar
        else if Value = 'Const' then
          FParamType := ptConst;
        {$ifdef LoadDebug}writeln ('              ParamType ', ord(FParamtype));{$endif}
        SplitNext;
        end;
      end;
  end;

  procedure ReadProperty (AProperty : TPropertyItem);
  var RProcType : TProcType;
      Rproptype : TPropType;
      RpropFuncType : TpropFuncType;
      counter : integer;
      s : string;
  begin
    with AProperty do
      begin
      if HasLine and (item = '    Prop') then
        begin
        FName := value;
        {$ifdef LoadDebug}writeln ('  Property Name ', FName);{$endif}
        SplitNext;
        end
      else
        raise exception.CreateFmt (sErrPropertyExpected, [line]);
      if HasLine and (item = '      PropType') then
        begin
        RProptype := high(TPropType);
        while (RPropType > low(TPropType)) and (DispPropType[RPropType] <> value) do
          dec (RPropType);
        FPropType := RPropType;
        {$ifdef LoadDebug}writeln ('           PropType ', ord(FPropType));{$endif}
        SplitNext;
        end
      else
        raise exception.CreateFmt (sErrPropTypeExpected, [Line]);
      Section := isPublic;
      if HasLine and (item = '      Section') then
        begin
        if value = 'Private' then
          Section := isPrivate
        else if value = 'Protected' then
          FSection := isprotected
        else if value = 'Published' then
          FSection := isPublished;
        SplitNext;
        {$ifdef LoadDebug}writeln ('           Section ', ord(FSection));{$endif}
        end;
      if HasLine and (item = '      PascalType') then
        begin
        FPascalType := value;
        {$ifdef LoadDebug}writeln ('           PascalType ', FPascalType);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '      GtkName') then
        begin
        FGtkName := value;
        {$ifdef LoadDebug}writeln ('           GtkName ', FGtkName);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '      Code') then
        begin
        FCode.Commatext := value;
        {$ifdef LoadDebug}writeln ('           Code set');{$endif}
        SplitNext;
        end;
      if HasLine then
        begin
        FReadConvert := (item = '      ReadConvert');
        {$ifdef LoadDebug}writeln ('           ReadConvert ', FReadConvert);{$endif}
        if FReadConvert then
          SplitNext;
        end;
      if HasLine and (item = '      ReadFuncType') then
        begin
        RpropFuncType := high(TpropFuncType);
        while (RpropFuncType > low(TpropFuncType)) and
              (value <> DispPropFuncType[RpropFuncType]) do
          dec (RpropFuncType);
        FReadFuncType := RpropFuncType;
        {$ifdef LoadDebug}writeln ('           ReadFuncType ', ord(FReadFunctype));{$endif}
        if RpropFuncType > low(TpropFuncType) then
          Splitnext;
        end;
      if HasLine and (item = '      WriteProcType') then
        begin
        RpropFuncType := high(TpropFuncType);
        while (RpropFuncType > low(TpropFuncType)) and
              (value <> DispPropFuncType[RpropFuncType]) do
          dec (RpropFuncType);
        FWriteProcType := RpropFuncType;
        {$ifdef LoadDebug}writeln ('           WriteProcType ', ord(FWriteProcType));{$endif}
        if RpropFuncType > low(TpropFuncType) then
          Splitnext;
        end;
      if HasLine and (item = '      WriteGtkName') then
        begin
        FWriteGtkName := value;
        {$ifdef LoadDebug}writeln ('           WriteGtkName ', FWriteGtkName);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '      WriteCode') then
        begin
        FWriteCode.Commatext := value;
        {$ifdef LoadDebug}writeln ('           WriteCode set');{$endif}
        SplitNext;
        end;
      if HasLine then
        begin
        FWriteConvert := (item = '      WriteConvert');
        {$ifdef LoadDebug}writeln ('           WriteConvert ', FWriteConvert);{$endif}
        if FWriteConvert then
          SplitNext;
        end;
      FProcTypes := [];
      if HasLine then
        begin
        s := copy(item, 7, 35);
        for RProcType := low(TProcType) to high(TProcType) do
          if s = DispProcType[RProcType] then
            begin
            FProcTypes := FProcTypes + [RProcType];
            {$ifdef LoadDebug}writeln ('           ProcType added ', s);{$endif}
            SplitNext;
            s := copy(item, 7, 35);
            end;
        end;
      if HasLine and (Item = '      Count') then
        with FParameters do
          begin
          counter := strtoint(value);
          {$ifdef LoadDebug}writeln ('           Counter ', Counter);{$endif}
          SplitNext;
          while (Counter > 0) do
            begin
            ReadParameter (Add as TParameterItem);
            dec (counter);
            end;
          end
      else
        raise exception.CreateFmt (sErrCountExpected, [line]);
      end;
  end;

  procedure ReadObject (AnObject : TObjectItem);
  var counter : integer;
  begin
    with AnObject do
      begin
      if HasLine and (item = '  Object') then
        begin
        FName := value;
        {$ifdef LoadDebug}writeln ('Object name ', FName);{$endif}
        SplitNext;
        end
      else
        raise exception.CreateFmt (sErrObjectExpected, [line]);
      if HasLine and (item = '    Inherit') then
        begin
        FInherit := value;
        {$ifdef LoadDebug}writeln ('       Inherit ', FInherit);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '    GtkFuncName') then
        begin
        FGtkFuncName := value;
        {$ifdef LoadDebug}writeln ('       GtkFuncName ', FGtkFuncName);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '    GtkName') then
        begin
        FGtkName := value;
        {$ifdef LoadDebug}writeln ('       GtkName ', FGtkName);{$endif}
        SplitNext;
        end;
      if HasLine and (item = '    CreateParams') then
        begin
        FCreateParams := value;
        {$ifdef LoadDebug}writeln ('       CreateParams ', FCreateParams);{$endif}
        SplitNext;
        end;
      if HasLine then
        begin
        FWithPointer := (item = '    WithPointer');
        {$ifdef LoadDebug}writeln ('       WithPointer ', FWithPointer);{$endif}
        if FWithPointer then
          SplitNext;
        end;
      if HasLine then
        begin
        FCreateObject := (item = '    CreateObject');
        {$ifdef LoadDebug}writeln ('       CreateObject ', FCreateObject);{$endif}
        if FCreateObject then
          SplitNext;
        end;
      if HasLine and (Item = '    Count') then
        with FProps do
          begin
          counter := strtoint(value);
          {$ifdef LoadDebug}writeln ('       Counter ', counter);{$endif}
          SplitNext;
          while (Counter > 0) do
            begin
            ReadProperty (Add as TPropertyItem);
            dec (counter);
            end;
          end
      else
        raise exception.CreateFmt (sErrCountExpected, [line]);
      end;
  end;

var counter : integer;
begin
  {$ifdef LoadDebug}writeln ('Start load');{$endif}
  if List[0] <> 'definition' then
    raise Exception.Create (sErrWrongFirstLine);
  {$ifdef LoadDebug}writeln ('Correct startline');{$endif}
  line := 0;
  {$ifdef LoadDebug}writeln ('Calling SplitNext');{$endif}
  SplitNext;
  if HasLine and (Item = '  GtkPrefix') then
    begin
    {$ifdef LoadDebug}writeln ('GtkPrefix=',value);{$endif}
    FGtkPrefix := value;
    SplitNext;
    end
  else
    FGtkPrefix := '';
  if HasLine and (Item = '  UsesList') then
    begin
    {$ifdef LoadDebug}writeln ('UsesList=',value);{$endif}
    FUsesList := value;
    SplitNext;
    end
  else
    FUsesList := '';
  if HasLine and (Item = '  UnitName') then
    begin
    {$ifdef LoadDebug}writeln ('UnitName=',value);{$endif}
    FUnitName := value;
    SplitNext;
    end
  else
    FUnitName := '';
  if HasLine and (Item = '  Count') then
    begin
    counter := strtoint(value);
    {$ifdef LoadDebug}writeln ('Counter ', counter);{$endif}
    if assigned(FDefinition) then
      begin
      {$ifdef LoadDebug}writeln ('Clearing ObjectDefinitions');{$endif}
      FDefinition.Clear;
      end
    else
      begin
      {$ifdef LoadDebug}writeln ('Creating ObjectDefinitions');{$endif}
      FDefinition := TObjectCollection.Create (self);
      end;
    SplitNext;
    while (Counter > 0) do
      begin
      ReadObject (Definition.Add as TObjectItem);
      dec (counter);
      end;
    end
  else
    raise exception.CreateFmt (sErrCountExpected, [line]);
end;

procedure TObjectDefs.Write(TheUnit : TStrings; StepIt : TLukStepItProc; StepItMax : TLukStepItMaxProc);

  procedure DoStepIt;
  begin
    if assigned (StepIt) then
      StepIt;
  end;

  procedure DoStepItMax (Max : integer);
  begin
    if assigned (StepItMax) then
      StepItMax (Max);
  end;

  procedure WriteObjectForward (Obj : TObjectItem);
  begin
    with obj do
      TheUnit.add ('  T'+ObjectsPrefix+Name+' = class;');
  end;

  function CalcProcTypes (ProcTypes : TProcTypeSet; InImplementation:boolean) : string; overload;
  begin
    if not InImplementation then
      begin
      if ptOverride in ProcTypes then
        result := ' Override;'
      else
        begin
        if ptVirtual in ProcTypes then
          result := ' Virtual;'
        else if ptDynamic in ProcTypes then
          result := ' Dynamic;'
        else
          result := '';
        if (result <> '') and (ptAbstract in ProcTypes) then
          result := result + ' Abstract;';
        end;
      if ptreintroduce in ProcTypes then
        result := result + ' Reintroduce;';
      end;
    if ptCDecl in ProcTypes then
      result := result + ' Cdecl;';
    if ptOverload in ProcTypes then
      result := result + ' Overload;';
  end;

  function CalcProcTypes (ProcTypes : TProcTypeSet) : string; overload;
  begin
    result := CalcProcTypes (ProcTypes, False);
  end;

  type
    TConvType = (ToGtk, ToLuk, ToFPgtk);

  function ConvertType (PascalType : string; ConvType : TConvType) : string;
  begin
    PascalType := lowercase (PascalType);
    if ConvType = ToGtk then
      begin
      if PascalType = 'string' then
        result := 'pgChar'
      else if copy(PascalType,1,ObjectsPrefixLength+1) = 't'+LowerObjectsPrefix then
        result := 'PGtk' + copy (PascalType, ObjectsPrefixLength+2, maxint)
      else if PascalType = 'longbool' then
        result := 'gint'
      else
        result := PascalType;
      end
    else
      begin
      if PascalType = 'pgChar' then
        result := 'string'
      else if copy(PascalType,1,4) = 'pgtk' then
        result := 'T'+ObjectsPrefix + copy (PascalType, 5, maxint)
      else if PascalType = 'gint' then
        result := 'longbool'
      else
        result := PascalType;
      end;
  end;

  function DoConvert (Variable, PascalType : string; ConvType : TConvType) : string;
  var s : string;
  begin
    result := variable;
    PascalType := lowercase (PascalType);
    if PascalType = 'string' then
      begin
      if ConvType <> ToLuk then
        result := 'ConvertToPgchar('+result+')'
      end
    else if copy(PascalType,1,4)='pgtk' then
      begin
      if ConvType = ToLuk then
        begin
        s := 'T'+ObjectsPrefix + copy(PascalType, 5, maxint);
        result := 'GetPascalInstance(PGtkObject('+result+'),'+s+') as '+ s
        end
      else
        result := PascalType+'(ConvertToGtkObject('+result+'))'
      end
    else if Copy(PascalType,1,ObjectsPrefixLength+1)='t'+LowerObjectsPrefix then
      begin
      if ConvType = ToLuk then
        result := 'GetPascalInstance(PGtkObject('+result+'),'+PascalType+') as '+PascalType
      else
        result := 'PGtk'+copy(PascalType,ObjectsPrefixLength+2,maxint)+'(ConvertToGtkObject('+result+'))'
      end
    else if PascalType = 'boolean' then
      begin
      if (copy(variable,1,4)='gtk.') and
              (ConvType = ToLuk) then
        result := 'boolean('+variable+')'
      else if  ConvType = ToFPGtk then
        result := 'guint('+variable+')'
      end
    else if PascalType = 'longbool' then
      begin
      if (copy(variable,1,4)='gtk.') and
              (ConvType = ToLuk) then
        result := 'longbool('+variable+')'
      else if ConvType in [ToFPGtk,ToGtk] then
        result := 'gint('+variable+')';
      end;
  end;

  function CalcParam (param : TParameterItem; Declaration : boolean; ConvType : TConvType) : string;
  begin
    with Param do
      begin
      if Declaration then
        begin
        case param.ParamType of
          ptVar   : result := 'var ';
          ptconst : result := 'const ';
          else      result := '';
        end;
        result := result + Name + ':' + PascalType;
        end
      else
        if Convert then
          result := DoConvert (Name, PascalType, convType)
        else
          result := name;
      end;
  end;

  type
    TParamListType = (plDecl, plImpl, plImplCl, plImplLukCl);

  function CalcParameterList (params : TParamCollection; PLType : TParamListType) : string; overload;
  var r : integer;
      Sep : string[2];
      ct : TConvType;
  begin
    if PLType = plDecl then
      Sep := '; '
    else
      Sep := ', ';
    if PLType = plImplLukCl then
      ct := ToLuk
    else
      ct := ToGtk;
    with params do
      if count = 0 then
        result := ''
      else
        begin
        result := CalcParam (Items[0], (PLType=plDecl), ct);
        for r := 1 to count-1 do
          result := result + Sep + CalcParam (items[r], (PLType=plDecl), ct);
        if PLType <> plImpl then
          result := ' (' + result + ')';
        end;
  end;

  function CalcParameterList (params : TParamCollection) : string; overload;
  var r : integer;
  begin
    with params do
      if count = 0 then
        result := ''
      else
        begin
        with Items[0] do
          result := Name + ':' + PascalType;
        for r := 1 to count-1 do
          with Items[r] do
            result := result + '; ' + Name + ':' + PascalType;
        end;
  end;

  var  Lpublic, LProt, LPriv, LPublish : TStrings;

  procedure WriteObjectInterface (Obj : TObjectItem);
  var r : integer;
      TheList : TStrings;
      I, N, s : string;
  begin
    Lpublic.Clear;
    LProt.Clear;
    LPriv.Clear;
    LPublish.clear;
    with obj do
      begin
      // Signal declarations
      with props do
        begin
        for r := 0 to count-1 do
          with Items[r] do
            begin
            if (PropType = ptSignalType) then
              if PascalType = '' then
                TheUnit.add ('  T'+ObjectsPrefix+Name+'Function = procedure' +
                            CalcParameterList(parameters,plDecl)+' of Object;')
              else
                TheUnit.add ('  T'+ObjectsPrefix+Name+'Function = function' +
                            CalcParameterList(parameters,plDecl)+': '+PascalType+' of Object;')
            else if (PropType = ptTypeDecl) then
              TheUnit.AddStrings (Code);
            end;
        end;
      TheUnit.Add ('');
      // Class definition
      if WithPointer then
        TheUnit.Add ('  P'+ObjectsPrefix+Name+' = ^T'+ObjectsPrefix+Name+';');
      if Inherit = '' then
        TheUnit.add ('  T'+ObjectsPrefix+Name+' = class')
      else
        begin
        if inherit[1] = '*' then
          s := copy(inherit, 2, maxint)
        else
          s := ObjectsPrefix + Inherit;
        TheUnit.add ('  T'+ObjectsPrefix+Name+' = class (T'+s+')');
        end;
      { Filling the 4 sections with the properties }
      for r := 0 to props.count-1 do
        with Props[r] do
          begin
          case Section of
            isPrivate : TheList := LPriv;
            isProtected : TheList := LProt;
            isPublic : TheList := LPublic;
            else TheList := LPublish;
          end;
          case PropType of
            ptField :
              TheList.Insert(0,'    ' + Name + ':' + PascalType + ';');
            ptProperty :
              begin
              s := '    property ' + Name;
              if (ReadFuncType <> pftNotImplemented) or
                 (WriteProcType <> pftNotImplemented) then
                begin
                if Parameters.Count > 0 then
                  begin
                  I := CalcParameterlist(parameters);
                  s := s + ' ['+I+'] ';
                  end;
                s := s + ' : ' + PascalType;
                if (ReadFuncType <> pftNotImplemented) then
                  begin
                  s := s + ' read ';
                  if ReadFuncType = pftField then
                    begin
                    if GtkName <> '' then
                      N := GtkName
                    else
                      N := 'F' + Name;
                    LPriv.insert (0, '    ' + N + ' : ' + PascalType + ';');
                    end
                  else
                    begin
                    if (ReadFuncType in PropUsesGtkName) and (GtkName <> '') then
                      N := GtkName
                    else
                      N := 'Get' + Name;
                    if (ReadFuncType <> pftExistingProc) then
                      begin
                      if parameters.count > 0 then
                        LPriv.Add ('    function '+N+'('+I+') : '+PascalType+';')
                      else
                        LPriv.Add ('    function '+N+' : '+PascalType+';');
                      end;
                    end;
                  s := s + N;
                  end;
                if (WriteProcType <> pftNotImplemented) then
                  begin
                  s := s + ' write ';
                  if WriteProcType = pftField then
                    begin
                    if GtkName <> '' then
                      N := GtkName
                    else
                      N := 'F' + Name;
                    if (ReadFuncType <> pftField) then
                      LPriv.insert (0, '    ' + N + ' : ' + PascalType + ';');
                    end
                  else
                    begin
                    if (WriteProcType in PropUsesGtkName) and (WriteGtkName <> '') then
                      N := WriteGtkName
                    else
                      N := 'Set' + Name;
                    if (WriteProcType <> pftExistingProc) then
                      begin
                      if parameters.count > 0 then
                        LPriv.Add ('    procedure '+N+' ('+I+'; TheValue : '+PascalType+');')
                      else
                        LPriv.Add ('    procedure '+N+' (TheValue : '+PascalType+');');
                      end;
                    end;
                  s := s + N;
                  end;
                end;
              TheList.Add (s+';');
              end;
            ptFunction :
              Thelist.Add ('    function ' + Name + CalcParameterList(Parameters, plDecl)
                         + ' : ' + PascalType+';' + CalcProcTypes(ProcTypes));
            ptProcedure :
              TheList.Add ('    procedure ' + Name + CalcParameterList(Parameters, plDecl)
                         + ';' + CalcProcTypes(ProcTypes));
            ptSignal :
              begin
              TheList.Add ('    function Connect'+Name+' (proc:T'+ObjectsPrefix+PascalType+'Function; data:pointer) : guint;');
              TheList.Add ('    function ConnectAfter'+Name+' (proc:T'+ObjectsPrefix+PascalType+'Function; data:pointer) : guint;');
              end;
            ptSignalType :
              begin
              TheList.Add ('    function ' + Name + 'Connect (Signal:string; Proc:T'+ObjectsPrefix+Name+'Function; data:pointer) : guint;');
              TheList.Add ('    function ' + Name + 'ConnectAfter (Signal:string; Proc:T'+ObjectsPrefix+Name+'Function; data:pointer) : guint;');
              end;
            ptConstructor :
              TheList.Add ('    constructor ' + Name + CalcParameterList(Parameters, plDecl)
                         + ';' + CalcProcTypes(ProcTypes));
            ptDestructor :
              TheList.Add ('    destructor ' + Name + CalcParameterList(Parameters, plDecl)
                         + ';' + CalcProcTypes(ProcTypes));
          end;
          end;
      { Adding the sections }
      if LPriv.count > 0 then
        begin
        TheUnit.add ('  Private');
        TheUnit.AddStrings (Lpriv);
        end;
      if (LProt.count > 0) or CreateObject then
        begin
        TheUnit.add ('  Protected');
        if CreateObject then
          TheUnit.add ('    procedure CreateGtkObject; override;');
        if LProt.Count >= 0 then
          TheUnit.AddStrings (Lprot);
        end;
      if (GtkFuncName <> '') or (LPublic.count >= 0) then
        begin
        TheUnit.add ('  Public');
        if (GtkFuncName <> '') then
          TheUnit.add ('    function TheGtkObject : PGtk'+Name+';');
        if LPublic.count >= 0 then
          TheUnit.AddStrings (Lpublic);
        end;
      if LPublish.count > 0 then
        begin
        TheUnit.add ('  Publish');
        TheUnit.AddStrings (Lpublish);
        end;
      end;
    TheUnit.Add ('  end;');
    TheUnit.add ('');
    DoStepIt;
  end;

  procedure WriteObjectImplementation (Obj : TObjectItem);
  var gn, n, s, start, midden, eind, res : string;
      r, l, p : integer;
  begin
    with Obj, TheUnit do
      begin
      n := Name;
      gn := GtkFuncName;
      add (' { T'+ObjectsPrefix+N+' }'+CRLF);
      if gn <> '' then
        // Functie voor alle objecten en header
        add ('function T'+ObjectsPrefix+N+'.TheGtkObject : PGtk'+N+';'+CRLF+
             'begin'+CRLF+
             '  result := P'+GtkPrefix+N+'(FGtkObject);'+CRLF+
             'end;'+CRLF);
      if CreateObject then
        begin
        eind := CreateParams;
        if eind <> '' then
          eind := ' (' + eind + ')';
        add ('procedure T'+ObjectsPrefix+N+'.CreateGtkObject;'+CRLF+
             'begin'+CRLF+
             '  FGtkObject := PGtkObject(gtk_'+gn+'_new'+eind+');'+CRLF+
             'end;'+CRLF);
        end;
      // Declarations toevoegen
      for r := 0 to Props.count-1 do
        with Props[r] do
          if (PropType = ptDeclarations) and (Section in sectPriv) then
            AddStrings (Code);
      // Properties toevoegen
      add ('');
      for r := 0 to props.count-1 do
        with Props[r] do
          begin
          case PropType of
            ptFunction :
              if not (ptAbstract in ProcTypes) then
                begin
                Add ('function T'+ObjectsPrefix + N + '.' + Name +
                     CalcParameterList(Parameters, plDecl) +
                     ' : ' + PascalType+';' + CalcProcTypes(ProcTypes,true));
                if GtkName = '' then
                  AddStrings (Code)
                else
                  begin
                  s := CalcParameterList (Parameters, plImpl);
                  if s <> '' then
                    s := ', ' + s;
                  Add ('begin' + CRLF +
                       '  result := ' + GtkPrefix + '_' + GN + '_' + GtkName +
                           ' (TheGtkObject' + s + ');' + CRLF +
                       'end;');
                  end;
                add ('');
                end;
            ptHelperFunc :
              begin
              Add ('function ' + Name + CalcParameterList(Parameters, plDecl) +
                ' : ' + PascalType+';'+CalcProcTypes(ProcTypes)+CRLF+Code.Text+CRLF);
              end;
            ptProcedure :
              if not (ptAbstract in ProcTypes) then
                begin
                Add ('procedure T'+ObjectsPrefix + N + '.' + Name+
                     CalcParameterList(Parameters,plDecl) + ';' +
                     CalcProcTypes(ProcTypes, True));
                if GtkName = '' then
                  AddStrings (Code)
                else
                  begin
                  s := CalcParameterList (Parameters, plImpl);
                  if s <> '' then
                    s := ', ' + s;
                  Add ('begin' + CRLF +
                       '  ' + GtkPrefix + '_' + GN + '_' + GtkName +
                                ' (TheGtkObject' + s + ');' + CRLF +
                       'end;');
                  end;
                add ('');
                end;
            ptHelperProc :
              Add ('procedure ' + Name + CalcParameterList(Parameters, plDecl) +
                   ';'+CalcProcTypes(ProcTypes)+CRLF+Code.Text+CRLF);
            ptConstructor :
              Add ('constructor T'+ObjectsPrefix + N + '.' + Name+
                   CalcParameterList(Parameters,plDecl) + ';'+CRLF+Code.Text+CRLF);
            ptDestructor :
              Add ('destructor T'+ObjectsPrefix + N + '.' + Name+
                   CalcParameterList(Parameters,plDecl) + ';'+CRLF+Code.Text+CRLF);
            ptSignal :
              begin
              start := 'function T'+ObjectsPrefix + N + '.Connect';
              midden := Name + ' (proc:T'+ObjectsPrefix + PascalType + 'Function; data:pointer) : guint;'+CRLF+
                        'begin' + CRLF +
                        '  result := ' + PascalType + 'Connect';
              eind := ' (sg' + Name + ', proc, data);' + CRLF +
                      'end;'+CRLF;
              Add (start+midden+eind);
              Add (start+'After'+midden+'After'+eind);
              end;
            ptSignalType :
              begin
              midden := '';
              with parameters do
                begin
                if count > 0 then
                  begin
                  {if lowercase(Items[0].Name) = 'sender' then
                    l := 1
                  else
                    l := 0;
                  p := count - 1;
                  if lowercase(Items[p].name) = 'data' then
                    dec (p);
                  }
                  // s = ParameterList for call; midden = parameter for declaration
                  //s := DoConvert ('TheWidget',ConvertType(Items[0].PascalType,ToGtk),ToLuk);
                  s := 'TheWidget as ' + Items[0].PascalType;
                  midden := Items[0].Name+':'+ConvertType(Items[0].PascalType,ToGtk);
                  for l := 1 to count-2 do
                    begin
                    case Items[l].ParamType of
                      ptVar : start := 'var ';
                      ptconst : start := 'const ';
                      else  start := '';
                    end;
                    with Items[l] do
                      if Convert then
                        begin
                        midden := midden+'; '+start+Name+':'+ConvertType(PascalType, ToGtk);
                        s := s+', '+DoConvert (Name,ConvertType(PascalType,ToGtk),ToLuk);
                        end
                      else
                        begin
                        midden := midden+'; '+start+Name+':'+PascalType;
                        s := s+', '+Name;
                        end
                    end;
                  p := count - 1;
                  midden := midden+'; '+Items[p].Name+':'+ConvertType(Items[p].PascalType, ToGtk);
                  s := s+', TheData';
                  end
                else
                  begin
                  s := '';
                  midden := '';
                  end;
                end;
              if PascalType = '' then
                begin
                start := 'procedure';
                eind := '';
                res := '';
                end
              else
                begin
                start := 'function';
                eind := 'result := ';
                res := ' : '+PascalType;
                end;
              Add (start+' '+Name+copy(start,1,4)+' ('+midden+')'+res+'; cdecl;'+CRLF+
                   'var p : T'+ObjectsPrefix+Name+'Function;'+CRLF+
                   'begin'+CRLF+
                   'with PSignalData(data)^ do'+CRLF+
                   '  begin'+CRLF+
                   '  p := T'+ObjectsPrefix+Name+'Function (TheSignalProc);'+CRLF+
                   '  '+eind+'p ('+s+')'+CRLF+
                   '  end;'+CRLF+
                   'end;'+CRLF);
              midden := ' (signal:string; proc:T'+ObjectsPrefix+Name+
                                              'Function; data:pointer) : guint;'+CRLF+
                   'begin'+CRLF+
                   '  result := '+GtkPrefix+'_signal_connect';
              eind:= ' (FGtkObject, pgChar(signal), '+GtkPrefix+'_signal_func(@'+Name+copy(start,1,4)+'), '+
                           'ConvertSignalData(T'+ObjectsPrefix+'SignalFunction(proc), data, true));'+CRLF+

                   'end;'+CRLF;
              start := 'function T'+ObjectsPrefix+N+'.'+Name+'Connect';
              Add (start+midden+eind);
              Add (start+'After'+midden+'_After'+eind);
              end;
            ptProperty :
              begin
              midden := Name;
              if parameters.count > 0 then
                start := ','+CalcParameterList (parameters, plImpl)
              else
                start := '';
              if parameters.count > 0 then
                eind := CalcParameterList (parameters)
              else
                eind := '';
              // Read Function
              if ReadFuncType = pftProc then
                begin
                s := Code.Text;
                if GtkName <> '' then
                  midden := GtkName
                else
                  midden := 'Get' + midden;
                end
              else if ReadFuncType in [pftGtkFunc, pftObjField, pftObjFunc, pftGtkMacro] then
                begin
                midden := 'Get'+midden;
                case ReadFuncType of
                  pftGtkFunc : s := GtkPrefix+'_'+gn+'_get_'+GtkName+'(TheGtkObject'+start+')';
                  pftObjField: s := 'TheGtkObject^.'+GtkName;
                  pftObjFunc : s := 'gtk.'+GtkName+'(TheGtkObject^'+start+')';
                  pftGtkMacro: s := GtkPrefix+'_'+gn+'_'+GtkName+'(TheGtkObject'+start+')';
                end;
                  if ReadConvert then
                    s := DoConvert (s, PascalType, ToLuk);
                  s := 'begin'+CRLF+'  result := '+s+';'+CRLF+'end;'+CRLF;
                end
              else
                s := '';
              if s <> '' then
                begin
                if eind = '' then
                  Add ('function T'+ObjectsPrefix+N+'.'+midden+' : '+PascalType+';'+CRLF+s)
                else
                  Add ('function T'+ObjectsPrefix+N+'.'+midden+' ('+eind+') : '+PascalType+';'+CRLF+s);
                end;
              // Write procedure
              midden := Name;
              if (WriteProcType in [pftGtkFunc,pftObjField,pftObjFunc,pftGtkMacro]) then
                begin
                midden := 'Set' + midden;
                if WriteConvert then
                  if WriteProcType in [pftObjField, pftObjFunc] then
                    s := DoConvert ('TheValue', PascalType, ToFPGtk)
                  else
                    s := DoConvert ('TheValue', PascalType, ToGtk)
                else
                  s := 'TheValue';
                case WriteProcType of
                  pftGtkFunc : s := GtkPrefix+'_'+gn+'_set_'+writeGtkName+'(TheGtkObject'+start+','+s+');';
                  pftGtkMacro: s := GtkPrefix+'_'+gn+'_'+writeGtkName+'(TheGtkObject'+start+','+s+');';
                  pftObjField: s := 'TheGtkObject^.'+writeGtkName+' := '+s+';';
                  pftObjFunc : s := 'gtk.'+'Set_'+WriteGtkName+'(TheGtkObject^'+start+','+s+')';
                end;
                s := 'begin'+CRLF+'  '+s+CRLF+'end;'+CRLF;
                end
              else if WriteProcType = pftProc then
                begin
                s := WriteCode.Text;
                if writegtkname <> '' then
                  midden := writegtkname
                else
                  midden := 'Set' + midden;
                end
              else
                s := '';
              if s <> '' then
                begin
                if eind = '' then
                  Add ('procedure T'+ObjectsPrefix+N+'.'+midden+' ('+'TheValue:' + PascalType+');'+CRLF+s)
                else
                  Add ('procedure T'+ObjectsPrefix+N+'.'+midden+' ('+eind+'; TheValue:' + PascalType+');'+CRLF+s);
                end;
              end;
          end;
          end;
      end;
    DoStepIt;
  end;

var r, t : integer;
    Need : boolean;
    UsedSignals : TStringList;

begin
  LPublic := TStringList.Create;
  LPublish := TStringList.Create;
  LPriv := TStringList.Create;
  LProt := TStringList.Create;
  UsedSignals := TStringList.Create;
  UsedSignals.Sorted := True;
  lowerObjectsPrefix := lowercase (ObjectsPrefix);
  ObjectsPrefixLength := length(lowerObjectsPrefix);
  with TheUnit do
    try
      DoStepItMax (FDefinition.Count * 2 + 4);
      clear;
      capacity := 70 * FDefinition.Count;
      add ('{$mode objfpc}{$h+} {$ifdef win32}{$define gtkwin}{$endif}'+CRLF+
           'UNIT '+UnitName+';'+CRLF+CRLF+
           '// Generated with GtkWrite by Luk Vandelaer (version '+versionnumber+')'+CRLF+CRLF+
           'INTERFACE'+CRLF+CRLF+
           'USES '+UsesList+';');
      // public declarations before classtypes
      for r := 0 to pred(FDefinition.count) do
        with FDefinition[r] do
          begin
          Need := True;
          for t := 0 to Props.count-1 do
            with Props[t] do
              if (PropType = ptDeclarations) and (Section = ispublished) then
                begin
                if Need then
                  begin
                  add ('{ T'+ObjectsPrefix + FDefinition[r].Name + ' }');
                  Need := False;
                  end;
                AddStrings (Code);
                end;
          end;
      DoStepIt;
      Add (CRLF+'TYPE'+CRLF);
      //Forward en implementation moeten in dezelfde Type block zitten
        // Forward declarations
        for r := 0 to pred(FDefinition.count) do
          WriteObjectForward (FDefinition[r]);
        // class declaration
        add ('');
        DoStepIt;
        for r := 0 to pred(FDefinition.count) do
          WriteObjectInterface (FDefinition[r]);
      // public declarations after classtypes
      for r := 0 to pred(FDefinition.count) do
        with FDefinition[r] do
          begin
          Need := True;
          for t := 0 to Props.count-1 do
            with Props[t] do
              if (PropType = ptDeclarations) and (Section = ispublic) then
                begin
                if Need then
                  begin
                  add ('{ T'+ObjectsPrefix + FDefinition[r].Name + ' }');
                  Need := False;
                  end;
                AddStrings (Code);
                end;
          end;
      // declaration of signal constants
      Add (CRLF+'Const');
      for r := 0 to pred(FDefinition.count) do
        with FDefinition[r] do
          begin
          Need := True;
          for t := 0 to Props.count-1 do
            with Props[t] do
              if (Section <> isPrivate) and
                 (PropType = ptsignal) and
                 (UsedSignals.indexof (Name) < 0) then
                begin
                if Need then
                  begin
                  add ('// T'+ObjectsPrefix + FDefinition[r].Name);
                  Need := False;
                  end;
                Add ('  sg' + Name + ' = ''' + lowercase(GtkName)+ ''';');
                UsedSignals.Add (Name);
                end;
          end;
      Add ('');
      // public helper functions en procedures
      for r := 0 to pred(FDefinition.count) do
        with FDefinition[r] do
          begin
          Need := True;
          for t := 0 to Props.count-1 do
            with Props[t] do
              if (Section in sectPublic) then
                if (PropType = ptHelperFunc) then
                  begin
                  if Need then
                    begin
                    add ('// T'+ObjectsPrefix + FDefinition[r].Name);
                    Need := False;
                    end;
                  Add ('function ' + Name + CalcParameterList(Parameters, plDecl)
                           + ' : ' + PascalType+';' + CalcProcTypes(ProcTypes));
                  end
                else if (PropType = ptHelperProc) then
                  begin
                  if Need then
                    begin
                    add ('// T'+ObjectsPrefix + FDefinition[r].Name);
                    Need := False;
                    end;
                  Add ('procedure ' + Name + CalcParameterList(Parameters, plDecl)
                         + ';' + CalcProcTypes(ProcTypes));
                  end;
          end;
      // Start implementation
      add (CRLF+'IMPLEMENTATION'+CRLF);
      // Object implementations
      for r := 0 to pred(FDefinition.count) do
        WriteObjectImplementation (FDefinition[r]);
      // Initializations
      Add ('INITIALIZATION');
      DoStepIt;
      for r := 0 to pred(FDefinition.count) do
        with FDefinition[r] do
          begin
          for t := 0 to Props.count-1 do
            with Props[t] do
              if (PropType = ptInitialization) then
                AddStrings (Code);
          end;
      // Finalizations
      Add (CRLF+'FINALIZATION');
      DoStepIt;
      for r := 0 to pred(FDefinition.count) do
        with FDefinition[r] do
          begin
          for t := 0 to Props.count-1 do
            with Props[t] do
              if (PropType = ptFinalization) then
                AddStrings (Code);
          end;
      add (CRLF+'End.');
    finally
      LPublic.Free;
      LPublish.Free;
      LPriv.Free;
      LProt.Free;
      UsedSignals.Free;
    end;
end;

end.
