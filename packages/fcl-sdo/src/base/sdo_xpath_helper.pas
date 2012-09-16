{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements an XPATH helper

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_xpath_helper;

interface
uses
  SysUtils, Contnrs,
  sdo_types, sdo;

type

  EXPathException = class(ESDOException)
  end;

  TXPathNode = class;
  TXPathExecContext = class;

  TXPathExpression = class
  private
    FRoot: TXPathNode;
  public
    destructor Destroy();override;
    property Root : TXPathNode read FRoot;
    procedure SetRoot(const ARoot : TXPathNode);
  end;

  TXPathNode = class
  private
    FNext: TXPathNode;
  public
    destructor Destroy();override;
    property Next : TXPathNode read FNext;
  end;

  TXPathAbstractSymbolNode = class(TXPathNode) end;
    TXPathLeftSquareBraketNode = class(TXPathAbstractSymbolNode) end;
    TXPathRigthSquareBraketNode = class(TXPathAbstractSymbolNode) end;
    TXPathSymbolNode = class(TXPathAbstractSymbolNode) end;
      TXPathSlashNode = class(TXPathSymbolNode) end;

  TXPathPredicateCode = ( xpcEqual );
  TXPathPredicateNode = class(TXPathSymbolNode)
  public
    class function GetCode() : TXPathPredicateCode;virtual;abstract;
  end;

  TXPathValueNode = class;
  TXPathEqualNode = class(TXPathPredicateNode)
  private
    FLeft: TXPathValueNode;
    FRight: TXPathValueNode;
  public
    destructor Destroy();override;
    class function GetCode() : TXPathPredicateCode;override;
    property Left : TXPathValueNode read FLeft write FLeft;
    property Right : TXPathValueNode read FRight write FRight;
  end;

  TXPathValueNode = class(TXPathNode) end;
  TXPathValueSymbolNode = class(TXPathValueNode)
  private
    FSymbol: string;
  public
    property Symbol : string read FSymbol write FSymbol;
  end;
  TXPathValuePropertySymbolNode = class(TXPathValueSymbolNode)
  end;
  TXPathValueFunctionSymbolNode = class(TXPathValueSymbolNode)
  public
    //property Arguments
  end;

  TXPathAtomicDataType = ( xadtString, xadtBoolean, xadtNumber );
  TXPathValueBuffer = record
    StringValue : string;
    case Kind : TXPathAtomicDataType of
      xadtString  : ();
      xadtBoolean : ( BoolValue : TSDOBoolean );
      xadtNumber  : ( NumberValue : Extended );
  end;

  TXPathTypedValueNode = class(TXPathValueNode)
  public
    class function GetDataType() : TXPathAtomicDataType;virtual;abstract;
    function Evaluate() : TXPathValueBuffer;virtual;abstract;
  end;

  TXPathStringValueNode = class(TXPathTypedValueNode)
  public
    class function GetDataType() : TXPathAtomicDataType;override;
  end;

  TXPathNumberValueNode = class(TXPathTypedValueNode)
  public
    class function GetDataType() : TXPathAtomicDataType;override;
  end;

  TXPathBooleanValueNode = class(TXPathTypedValueNode)
  public
    class function GetDataType() : TXPathAtomicDataType;override;
  end;

  TXPathStringConstantNode = class(TXPathStringValueNode)
  private
    FValue: string;
  public
    constructor Create(const AValue : string);
    function Evaluate() : TXPathValueBuffer;override;
    property Value : string read FValue write FValue;
  end;

  TXPathNumberConstantNode = class(TXPathNumberValueNode)
  private
    FValue: Extended;
  public
    constructor Create(const AValue : Extended);
    function Evaluate() : TXPathValueBuffer;override;
    property Value : Extended read FValue write FValue;
  end;

  TXPathBooleanConstantNode = class(TXPathBooleanValueNode)
  private
    FValue: TSDOBoolean;
  public
    constructor Create(const AValue : TSDOBoolean);
    function Evaluate() : TXPathValueBuffer;override;
    property Value : TSDOBoolean read FValue write FValue;
  end;

  TXPathInstructionCode = ( xicContextChange, xicMove, xicLocate );
  TXPathInstructionStepNode = class(TXPathNode)
  public
    class function GetCode() : TXPathInstructionCode;virtual;abstract;
  end;

  TXPathContextSwitch = ( xcsSelf, xcsParent, xcsRoot, xcsProperty );
  TXPathContextStepNode = class(TXPathInstructionStepNode)
  private
    FSwitch: TXPathContextSwitch;
    FPropertyName: string;
  public
    class function GetCode() : TXPathInstructionCode;override;
    property Switch : TXPathContextSwitch read FSwitch;
    property PropertyName : string read FPropertyName;
  end;

  TXPathMoveInstructionStepNode = class(TXPathInstructionStepNode)
  private
    FDistance: TXPathTypedValueNode;
  public
    class function GetCode() : TXPathInstructionCode;override;
    destructor Destroy();override;
    property Distance : TXPathTypedValueNode read FDistance;
  end;

  TXPathLocateInstructionStepNode = class(TXPathInstructionStepNode)
  private
    FCondition: TXPathPredicateNode;
  public
    class function GetCode() : TXPathInstructionCode;override;
    destructor Destroy();override;
    property Condition : TXPathPredicateNode read FCondition;
  end;

  TXPathExecContentKind = ( xckNull, xckObject, xckList, xckValue, xckListFiltered );
  TXPathExecContentKinds = set of TXPathExecContentKind;
  TXPathExecContext = class
  private
    FObjectItem : Pointer; // do not add a reference count
    FListItem : Pointer; // do not add a reference count
    FContentKind: TXPathExecContentKind;
    FCurrentProperty: ISDOProperty;
    FPropertyOwner: Pointer;
    FListFiltered : ISDODataObjectList;
  private
    procedure CheckKind(const AKind : TXPathExecContentKind);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckKind(const AKinds : TXPathExecContentKinds);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetListItem: ISDODataObjectList;
    function GetObjectItem: ISDODataObject;
    function GetListFilteredItem : ISDODataObjectList;
    function GetPropertyOwner: ISDODataObject;
  public
    destructor Destroy(); override;
    property ContentKind : TXPathExecContentKind read FContentKind;
    property ObjectItem : ISDODataObject read GetObjectItem;
    property ListItem : ISDODataObjectList read GetListItem;
    property ListFiltered : ISDODataObjectList read GetListFilteredItem;
    property CurrentProperty : ISDOProperty read FCurrentProperty;
    property PropertyOwner : ISDODataObject read GetPropertyOwner;
    procedure SetObject(const AValue : ISDODataObject; const AProperty : ISDOProperty);
    procedure SetList(const AValue : ISDODataObjectList; const AProperty : ISDOProperty);
    procedure SetNull(const AProperty : ISDOProperty);
    procedure SetValue(const AProperty : ISDOProperty);
  end;

  TXPathProcessorOption = ( xppoEvaluateSet );
  TXPathProcessorOptions = set of TXPathProcessorOption;
  TXPathProcessor = class
  private
    FExpression : TXPathExpression;
    FContext: TXPathExecContext;
    FCurrentNode : TXPathNode;
    FOptions: TXPathProcessorOptions;
  private
    procedure Clear(const AClearContext : Boolean);
    procedure WalkTree();
    procedure WalkNode(const ANode : TXPathInstructionStepNode);
    function Evaluate(
      const ATarget : ISDODataObject;
      const AValueNode : TXPathValueNode
    ) : TXPathValueBuffer;
  public
    constructor Create(const AOptions : TXPathProcessorOptions = []);
    destructor Destroy();override;
    function IsTrue(
      const ATarget : ISDODataObject;
      const ACondition : TXPathPredicateNode
    ) : Boolean;
    property Context : TXPathExecContext read FContext;
    procedure Execute(AExpression : TXPathExpression);

    property Options : TXPathProcessorOptions read FOptions;
  end;

  TXPathToken = (
    xtkEof,
    xtkNull,
    xtkTrue,
    xtkFalse,
    xtkNumber,
    xtkString,
    xtkSymbol,
    xtkAt,                    // @
    xtkSlash,                 // "/"
    xtkPeriod,                // "."
    xtkDoublePeriod,          // ".."
    xtkSquaredBraceLeft,      // '['
    xtkSquaredBraceRigth,     // ']'
    xtkEqual                  // =
  );
  TXPathScanner = class
  private
    FBuffer : string;
    FTokenString : string;
    FPosition : PtrInt;
    FToken: TXPathToken;
    FLength : PtrInt;
  public
    constructor Create(const AXPathString : string);
    function NextToken() : TXPathToken;
    function TokenInt() : PtrInt;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function TokenNumber() : Extended;{$IFDEF USE_INLINE}inline;{$ENDIF}
    property Token : TXPathToken read FToken;
    property TokenString : string read FTokenString;
    property Position : PtrInt read FPosition;
  end;

  TXPathParser = class
  private
    FScanner : TXPathScanner;
    FTermStack : TObjectStack;
    FSymbolStack : TObjectStack;
    FRootNode : TXPathNode;
    FXPath : string;
    FInConditionExpression : PtrInt;
  private
    procedure BeginConditionExpression();
    function IsInConditionExpression() : Boolean;
    procedure EndConditionExpression();
    procedure Clear();
    procedure ReadSymbol();
    procedure ReadStringConst();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure ReadBooleanConst();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure ReadNumberConst();{$IFDEF USE_INLINE}inline;{$ENDIF}
    function InternalTryReduction():Boolean;
    procedure TryReduction();{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(const AXPathString : string);
    destructor Destroy();override;
    function Parse() : TXPathNode;
    property RootNode : TXPathNode read FRootNode;
  end;

  function ParseXPath(const AString : string) : TXPathNode;

  function getXpath(const ADataObject: ISDODataObject): string;
  function IndexOf(const AObject : ISDODataObject; const AList : ISDODataObjectList) : PtrInt;

implementation

uses
  TypInfo;

function ParseXPath(const AString : string) : TXPathNode;
var
  locParser : TXPathParser;
begin
  locParser := TXPathParser.Create(AString);
  try
    Result := locParser.Parse();
  finally
    FreeAndNil(locParser);
  end;
end;

function IndexOf(const AObject : ISDODataObject; const AList : ISDODataObjectList) : PtrInt;
var
  crs : ISDOCursor;
  oldPos : ISDOCursorBookmark;
begin
  crs := AList.getCursor();
  if crs.IsPosValid() and ( AList.getDataObject() = AObject ) then begin
    Result := crs.GetPosition();
  end else begin
    Result := -1;
    oldPos := crs.GetBookmark();
    try
      crs.Reset();
      while crs.MoveNext() do begin
        if ( AList.getDataObject() = AObject ) then begin
          Result := crs.GetPosition();
          Break;
        end;
      end;
    finally
      crs.GotoBookmark(oldPos);
    end;
  end;
end;

function getXpath(const ADataObject: ISDODataObject): string;
var
  locBuffer : string;
  obj : ISDODataObject;
  prop : ISDOProperty;
begin
  if Assigned(ADataObject) then begin
    locBuffer := '';
    obj := ADataObject;
    repeat
      if ( obj.getContainer() <> nil ) then begin
        prop := obj.getContainmentProperty();
        if prop.isMany() then
          locBuffer := Format(
                         '%s[%d]/%s',
                         [prop.getName(),IndexOf(obj,obj.getContainer().getList(prop)),locBuffer]
                       )
        else
          locBuffer := prop.getName() + '/' + locBuffer;
      end;
      obj := obj.getContainer();
    until ( obj = nil );
  end;
  if ( Length(locBuffer) > 0 ) and ( locBuffer[Length(locBuffer)] = '/' ) then
    Delete(locBuffer,Length(locBuffer),1);
  Result := locBuffer;
end;

{ TXPathNode }

destructor TXPathNode.Destroy();
begin
  FreeAndNil(FNext);
  inherited;
end;

{ TXPathExecContext }

procedure TXPathExecContext.CheckKind(const AKind: TXPathExecContentKind);
const   //xckNull, xckObject, xckList, xckValue, xckListFiltered
  ACCESS_METHOD_MAP : array[TXPathExecContentKind] of string = ('GetNull','GetObjectItem','GetListItem','GetValue','GetListFilteredItem');
begin
  if ( FContentKind <> AKind ) then
    raise ESDOInvalidStateOperationException.Create('TXPathExecContext.' + ACCESS_METHOD_MAP[AKind]);
end;

procedure TXPathExecContext.CheckKind(const AKinds: TXPathExecContentKinds);
begin
  if not ( FContentKind in AKinds ) then
    raise ESDOInvalidStateOperationException.Create('TXPathExecContext.');
end;

destructor TXPathExecContext.Destroy();
begin
  FListFiltered := nil;
  inherited;
end;

function TXPathExecContext.GetListFilteredItem: ISDODataObjectList;
begin
  Result := FListFiltered;
end;

function TXPathExecContext.GetListItem() : ISDODataObjectList;
begin
  Result := ISDODataObjectList(FListItem);
end;

function TXPathExecContext.GetObjectItem() : ISDODataObject;
begin
  Result := ISDODataObject(FObjectItem);
end;

function TXPathExecContext.GetPropertyOwner() : ISDODataObject;
begin
  Result := ISDODataObject(FPropertyOwner);
end;

procedure TXPathExecContext.SetList(const AValue: ISDODataObjectList; const AProperty : ISDOProperty);
begin
  FPropertyOwner := FObjectItem;
  //FObjectItem := nil;
  FListItem := Pointer(AValue);
  FCurrentProperty := AProperty;
  if ( FListItem <> nil ) then
    FContentKind := xckList
  else
    FContentKind := xckNull;
end;

procedure TXPathExecContext.SetNull(const AProperty : ISDOProperty);
begin
  if ( FContentKind <> xckList ) then
    FPropertyOwner := FObjectItem;
  FObjectItem := nil;
  FListItem := nil;
  FCurrentProperty := AProperty;
  FContentKind := xckNull;
end;

procedure TXPathExecContext.SetObject(const AValue: ISDODataObject; const AProperty : ISDOProperty);
begin
  if ( FContentKind = xckList ) then begin
    //if ( ListItem <> nil ) then
     // ListItem.
    //FPropertyOwner := f
  end else begin
    FPropertyOwner := FObjectItem;
  end;
  FObjectItem := Pointer(AValue);
  FCurrentProperty := AProperty;
  //FListItem := nil;
  if ( FObjectItem <> nil ) then
    FContentKind := xckObject
  else
    FContentKind := xckNull;
end;

procedure TXPathExecContext.SetValue(const AProperty: ISDOProperty);
begin
  CheckKind(xckObject);
  FPropertyOwner := FObjectItem;
  FContentKind := xckValue;
  FCurrentProperty := AProperty;
end;

{ TXPathProcessor }

procedure TXPathProcessor.Clear(const AClearContext : Boolean);
begin
  FExpression := nil;
  FCurrentNode := nil;
  if ( FContext <> nil ) and AClearContext then
    FContext.SetNull(nil);
end;

constructor TXPathProcessor.Create(const AOptions : TXPathProcessorOptions);
begin
  FOptions := AOptions;
  FContext := TXPathExecContext.Create();
end;

destructor TXPathProcessor.Destroy;
begin
  FExpression := nil;
  FreeAndNil(FContext);
  inherited;
end;

function TXPathProcessor.Evaluate(
  const ATarget: ISDODataObject;
  const AValueNode: TXPathValueNode
) : TXPathValueBuffer;

  function EvaluateProperty() : TXPathValueBuffer;
  var
    propNode : TXPathValuePropertySymbolNode;
    prop : ISDOProperty;
  begin
    propNode := TXPathValuePropertySymbolNode(AValueNode);
    prop := ATarget.getType().getProperty(propNode.Symbol);
    case prop.getTypeEnum() of
      BooleanType :
        begin
          Result.Kind := xadtBoolean;
          Result.BoolValue := ATarget.getBoolean(prop);
        end;
      ByteType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getByte(prop);
        end;
{$IFDEF HAS_SDO_BYTES}
      BytesType :
        begin
          Result.Kind := xadtString;
          Result.StringValue := TSDOConvertHelper.BytesToString(ATarget.getBytes(prop));
        end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
      CharacterType :
        begin
          Result.Kind := xadtString;
          Result.StringValue := ATarget.getCharacter(prop);
        end;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
      CurrencyType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getCurrency(prop);
        end;
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
      DoubleType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getDouble(prop);
        end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
      FloatType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getFloat(prop);
        end;
{$ENDIF HAS_SDO_FLOAT}
      DateTimeType :
        begin
          Result.Kind := xadtString;
          Result.StringValue := TSDOConvertHelper.DateToString(ATarget.getDate(prop));
        end;
      IntegerType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getInteger(prop);
        end;
{$IFDEF HAS_SDO_LONG}
      LongType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getLong(prop);
        end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
      ShortType :
        begin
          Result.Kind := xadtNumber;
          Result.NumberValue := ATarget.getShort(prop);
        end;
{$ENDIF HAS_SDO_SHORT}
      StringType  :
        begin
          Result.Kind := xadtString;
          Result.StringValue := ATarget.getString(prop);
        end;
      else
        raise ESDONotImplementedException.CreateFmt('TXPathProcessor.Evaluate.EvaluateProperty() not implemented for this type : "%s".',[prop.getType().getName()]);
    end;
  end;

begin
  if AValueNode.InheritsFrom(TXPathTypedValueNode) then begin
    Result := TXPathTypedValueNode(AValueNode).Evaluate();
  end else if AValueNode.InheritsFrom(TXPathValuePropertySymbolNode) then begin
    Result := EvaluateProperty();
  end else begin
    raise EXPathException.CreateFmt('Unhandle value node type : "%s".',[AValueNode.ClassName]);
  end;
end;

procedure TXPathProcessor.Execute(AExpression: TXPathExpression);
begin
  if ( AExpression = nil ) or ( AExpression.Root = nil ) then
    raise EXpathException.Create('Invalid XPath tree.');
  if not AExpression.Root.InheritsFrom(TXPathInstructionStepNode) then
    raise EXpathException.Create('Invalid XPath tree : the root node must be an instruction one.');
  Clear(False);
  FExpression := AExpression;
  FCurrentNode := FExpression.Root;
  WalkTree();
end;

function TXPathProcessor.IsTrue(
  const ATarget: ISDODataObject;
  const ACondition: TXPathPredicateNode
): Boolean;

  function HandleEqual(const ACond : TXPathEqualNode) : Boolean;
  var
    lval, rval : TXPathValueBuffer;
  begin
    Result := False;
    lval := Evaluate(ATarget,ACond.Left);
    rval := Evaluate(ATarget,ACond.Right);
    case lval.Kind of
      xadtString :
        begin
          case rval.Kind of
            xadtString   : Result := ( lval.StringValue = rval.StringValue );
            xadtBoolean  : Result := ( lval.StringValue = TSDOConvertHelper.BoolToString(rval.BoolValue) );
            xadtNumber   : Result := ( lval.StringValue = TSDOConvertHelper.FloatToString(rval.NumberValue) );
          end;
        end;
      xadtBoolean :
        begin
          case rval.Kind of
            xadtString   : Result := ( lval.BoolValue = TSDOConvertHelper.StringToBool(rval.StringValue) );
            xadtBoolean  : Result := ( lval.BoolValue = rval.BoolValue );
            xadtNumber   : Result := ( lval.BoolValue = ( rval.NumberValue <> 0 ) );
          end;
        end;
      xadtNumber :
        begin
          case rval.Kind of
            xadtString   : Result := ( lval.NumberValue = TSDOConvertHelper.StringToFloat(rval.StringValue) );
            xadtBoolean  : Result := ( lval.NumberValue = TSDOConvertHelper.BoolToInteger(rval.BoolValue) );
            xadtNumber   : Result := ( lval.NumberValue = rval.NumberValue );
          end;
        end;
    end;
  end;

var
  locRes : Boolean;
  locNode : TXPathPredicateNode;
begin
  Result := False;
  if ( ATarget <> nil ) then begin
    if ( ACondition = nil ) then begin
      Result := True;
    end else begin
      locRes := False;
      locNode := ACondition;
      while ( locNode <> nil ) do begin
        case locNode.GetCode() of
          xpcEqual : locRes := HandleEqual(TXPathEqualNode(locNode));
          else
            raise EXPathException.CreateFmt('Unhandle predicate type : "%s".',[GetEnumName(TypeInfo(TXPathPredicateCode), Ord(locNode.GetCode()))]);
        end;
        if not locRes then
          Break;
        locNode := locNode.Next as TXPathPredicateNode;
      end;
      Result := locRes;
    end;
  end;
end;

procedure TXPathProcessor.WalkNode(const ANode: TXPathInstructionStepNode);
  procedure ExecContextNode();
  var
    ctxNode : TXPathContextStepNode;
    tmpObj, lastObj : ISDODataObject;
    prop : ISDOProperty;
  begin
    ctxNode := TXPathContextStepNode(ANode);
    case ctxNode.Switch of
      xcsSelf : ; // nothing to do
      xcsParent :
        begin
          Context.CheckKind(xckObject);
          Context.SetObject(Context.ObjectItem.getContainer(),nil);
        end;
      xcsRoot :
        begin
          Context.CheckKind(xckObject);
          tmpObj := Context.ObjectItem;
          repeat
            lastObj := tmpObj;
            tmpObj := tmpObj.getContainer();
          until ( tmpObj = nil );
          Context.SetObject(lastObj,nil);
        end;
      xcsProperty :
        begin
          Context.CheckKind([xckObject,xckList]);
          prop := Context.ObjectItem.getProperty(ctxNode.PropertyName);
          if prop.isMany() then begin
            Context.SetList(Context.ObjectItem.getList(prop),prop)
          end else begin
            if prop.getType().isDataObjectType() then
              Context.SetObject(Context.ObjectItem.getDataObject(prop),prop)
            else
              Context.SetValue(prop);
          end;
        end;
    end;
  end;

  procedure ExecMove();
  var
    moveNode : TXPathMoveInstructionStepNode;
    dist : PtrInt;
  begin
    moveNode := TXPathMoveInstructionStepNode(ANode);
    dist := Round(moveNode.Distance.Evaluate().NumberValue);
    if ( dist >= 0 ) then begin
      if ( Context.ContentKind <> xckList ) then
        raise EXpathException.Create('Invalid context : list expected.');
      if ( Context.CurrentProperty = nil ) then
        raise EXpathException.Create('Invalid context : no available property.');
      if not Context.CurrentProperty.isMany() then
        raise EXpathException.CreateFmt('Invalid context property indexing : "%s" is not a multi-value property.',[Context.CurrentProperty.getName()]);
      if not Context.ListItem.getCursor().MoveTo(dist) then
        raise EXpathException.CreateFmt('Unable to reach this position in the list : %s[%d]',[Context.CurrentProperty.getName(),dist]);
      if Context.CurrentProperty.getType().isDataObjectType() then
        Context.SetObject(Context.ListItem.getDataObject(),Context.CurrentProperty)
    end;
  end;

  procedure ExecLocate();
  var
    locateNode : TXPathLocateInstructionStepNode;
    conditionNode : TXPathPredicateNode;
    crs : ISDOCursor;
    ls : ISDODataObjectList;
    found : Boolean;
  begin
    if ( Context.CurrentProperty = nil ) then
      raise EXPathException.Create('Invalid context : no property set.');
    if not Context.CurrentProperty.isMany() then
      raise EXPathException.CreateFmt('Invalid context , multi-valued property expected : "%s".',[Context.CurrentProperty.getName()]);
    locateNode := TXPathLocateInstructionStepNode(ANode);
    conditionNode := locateNode.Condition;
    ls := Context.ListItem;
    crs := ls.getCursor();
    crs.Reset();
    found :=False;
    while crs.MoveNext() do begin
      if IsTrue(ls.getDataObject(),conditionNode) then begin
        Context.SetObject(ls.getDataObject(),Context.CurrentProperty);
        found := True;
        Break;
      end;
    end;
    if not found then
      Context.SetNull(Context.CurrentProperty);
  end;

begin
  case ANode.GetCode() of
    xicContextChange : ExecContextNode();
    xicMove          : ExecMove();
    xicLocate        : ExecLocate();
  end;
end;

procedure TXPathProcessor.WalkTree();
begin
  while ( FCurrentNode <> nil ) do begin
    WalkNode(FCurrentNode as TXPathInstructionStepNode);
    FCurrentNode := FCurrentNode.Next;
  end;
end;

{ TXPathExpression }

destructor TXPathExpression.Destroy();
begin
  FreeAndNil(FRoot);
  inherited;
end;

procedure TXPathExpression.SetRoot(const ARoot: TXPathNode);
begin
  FreeAndNil(FRoot);
  FRoot := ARoot;
end;

{ TXPathScanner }

constructor TXPathScanner.Create(const AXPathString: string);
begin
  FBuffer := AXPathString;
  FLength := Length(FBuffer);
  FPosition := 1;
  FToken := xtkEof;
end;

function TXPathScanner.NextToken() : TXPathToken;
var
  locPosStart : PtrInt;
begin
  FTokenString := '';
  FToken := xtkEof;
  //skip blanks
  while ( FPosition <= FLength ) and ( FBuffer[FPosition] in [' ',#9,#10,#13] ) do begin
    Inc(FPosition);
  end;
  if ( FPosition <= FLength ) then begin
    while ( FPosition <= FLength ) do begin
      case FBuffer[FPosition] of
        '[' :
          begin
            FToken := xtkSquaredBraceLeft;
            FTokenString := '[';
            Inc(FPosition);
            Break;
          end;
        ']' :
          begin
            FToken := xtkSquaredBraceRigth;
            FTokenString := ']';
            Inc(FPosition);
            Break;
          end;
        '=' :
          begin
            FToken := xtkEqual;
            FTokenString := '=';
            Inc(FPosition);
            Break;
          end;
        '"' :
          begin
            Inc(FPosition);
            locPosStart := FPosition;
            while ( FPosition <= FLength ) and ( FBuffer[FPosition] <> '"' ) do begin
              Inc(FPosition);
            end;
            if ( FBuffer[FPosition] <> '"' ) then
              raise EXpathException.Create('Not terminated string at the end of the expression.');
            FTokenString := Copy(FBuffer,locPosStart,( FPosition - locPosStart ) );
            Inc(FPosition);
            FToken := xtkString;
            Break;
          end;
        '''' :
          begin
            Inc(FPosition);
            locPosStart := FPosition;
            while ( FPosition <= FLength ) and ( FBuffer[FPosition] <> '''' ) do begin
              Inc(FPosition);
            end;
            if ( FBuffer[FPosition] <> '''' ) then
              raise EXpathException.Create('Not terminated string at the end of the expression.');
            FTokenString := Copy(FBuffer,locPosStart,( FPosition - locPosStart ) );
            Inc(FPosition);
            FToken := xtkString;
            Break;
          end;
        '-', '0'..'9' :
          begin
            locPosStart := FPosition;
            Inc(FPosition);
            while ( FPosition <= FLength ) and ( FBuffer[FPosition] in ['0'..'9'] ) do begin
              Inc(FPosition);
            end;
            if ( FPosition < FLength ) then begin
              if ( FBuffer[FPosition] = '.' ) then begin
                Inc(FPosition);
                while ( FPosition < FLength ) and ( FBuffer[FPosition] in ['0'..'9'] ) do begin
                  Inc(FPosition);
                end;
              end;
            end;
            if ( locPosStart = ( FPosition + 1 ) ) then
              raise EXpathException.Create('Invalid character at the end of the expression : "-".');
            FTokenString := Copy(FBuffer,locPosStart,( FPosition - locPosStart ));
            FToken := xtkNumber;
            Break;
          end;
        '/' :
          begin
            FToken := xtkSlash;
            FTokenString := '/';
            Inc(FPosition);
            Break;
          end;
        '.' :
          begin
            if ( FPosition < FLength ) then begin
              if ( FBuffer[( FPosition + 1 )] = '.' ) then begin
                FToken := xtkDoublePeriod;
                FTokenString := '..';
                Inc(FPosition);
              end else if ( FBuffer[( FPosition + 1 )] in  ['0'..'9'] ) then begin
                locPosStart := FPosition;
                Inc(FPosition);
                while ( FPosition <= FLength ) and ( FBuffer[FPosition] in  ['0'..'9'] ) do begin
                  Inc(FPosition);
                end;
                FTokenString := '0' + Copy(FBuffer,locPosStart,( FPosition - locPosStart ) );
                Dec(FPosition);
                FToken := xtkNumber;
              end;
            end else begin
              FTokenString := '.';
              FToken := xtkPeriod;
            end;
            Inc(FPosition);
            Break;
          end;
        'a'..'z','A'..'Z', '_' :
          begin
            locPosStart := FPosition;
            while ( FPosition <= FLength ) and ( FBuffer[FPosition] in ['a'..'z','A'..'Z', '_', '0'..'9'] ) do begin
              Inc(FPosition);
            end;
            FTokenString := Copy(FBuffer,locPosStart,( FPosition - locPosStart ));
            if ( FTokenString = 'true' ) then
              FToken := xtkTrue
            else if ( FTokenString = 'false' ) then
              FToken := xtkFalse
            else if ( FTokenString = 'null' ) then
              FToken := xtkNull
            else
              FToken := xtkSymbol;
            Break;
          end;
        '@' :
          begin
            FToken := xtkAt;
            FTokenString := '@';
            Inc(FPosition);
            Break;
          end;
        else
          raise EXpathException.CreateFmt('Invalid character at %d : "%s".',[FPosition,FBuffer[FPosition]]);
      end;
    end;
  end;
  Result := FToken;
end;

function TXPathScanner.TokenInt() : PtrInt;
begin
  Result := StrToInt(TokenString);
end;

function TXPathScanner.TokenNumber() : Extended;
begin
  Result := TSDOConvertHelper.StringToFloat(TokenString);
end;

{ TXPathParser }

procedure TXPathParser.BeginConditionExpression();
begin
  Inc(FInConditionExpression);
end;

procedure TXPathParser.Clear();
var
  c, i : PtrInt;
begin
  if ( FTermStack <> nil ) then begin
    c := FTermStack.Count;
    if ( FRootNode <> nil ) then
      c := c - 1;
    for i  := 1 to c do begin
      FTermStack.Pop().Free();
    end;
    if ( FRootNode <> nil ) and ( FTermStack.Count > 0 ) then
      FTermStack.Pop(); // <<-- FRootNode
  end;
  if ( FSymbolStack <> nil ) then begin
    c := FSymbolStack.Count;
    for i  := 1 to c do begin
      FSymbolStack.Pop().Free();
    end;
  end;
  FreeAndNil(FRootNode);
end;

constructor TXPathParser.Create(const AXPathString: string);
begin
  FXPath := AXPathString;
  FScanner := TXPathScanner.Create(FXPath);
  FTermStack := TObjectStack.Create();
  FSymbolStack := TObjectStack.Create();
end;

destructor TXPathParser.Destroy();
begin
  Clear();
  FreeAndNil(FTermStack);
  FreeAndNil(FSymbolStack);
  FreeAndNil(FScanner);
  inherited;
end;

procedure TXPathParser.EndConditionExpression();
begin
  Dec(FInConditionExpression);
  if ( FInConditionExpression < 0 ) then
    raise EXPathException.Create('Invalid condition ending.');
end;

function TXPathParser.InternalTryReduction() : Boolean;
var
  locSymbol : TXPathSymbolNode;
  locFreeSym : Boolean;

  function ReduceSlash() : Boolean;
  var
    a, b : TXPathNode;
  begin
    Result := False;
    if ( FTermStack.Count = 0 ) then begin
      a := TXPathContextStepNode.Create();
      FTermStack.Push(a);
      TXPathContextStepNode(a).FSwitch := xcsRoot;
      FRootNode := a;
    end else if FTermStack.AtLeast(2) then begin
      FSymbolStack.Pop();
      b := FTermStack.Pop() as TXPathNode;
      if b.InheritsFrom(TXPathContextStepNode) then begin
        a := FTermStack.Pop() as TXPathNode;
        if ( a.FNext <> nil ) then begin
          FreeAndNil(a);
          FreeAndNil(b);
          raise EXpathException.CreateFmt('nil expected but "%s" found.',[a.ClassName]);
        end;
        a.FNext := b;
        FTermStack.Push(b);
      end else begin
        FreeAndNil(b);
        raise EXpathException.CreateFmt('A context step expected but "%s" found.',[b.ClassName]);
      end;
      Result := True;
      locFreeSym := True;
    end;
  end;

  function ReduceEqual() : Boolean;
  var
    left, right : TXPathNode;
    eqlNode : TXPathEqualNode;
  begin
    if not FTermStack.AtLeast(2) then
      raise EXpathException.Create('Invalid "=" expression.');
    right := FTermStack.Pop() as TXPathNode;
    if not right.InheritsFrom(TXPathValueNode) then begin
      FTermStack.Push(right);
      raise EXpathException.Create('"Right" member of an "Equal" expression must be a value expression.');
    end;
    left := FTermStack.Pop() as TXPathNode;
    if not left.InheritsFrom(TXPathValueNode) then begin
      FTermStack.Push(left);
      raise EXpathException.Create('"Left" member of an "Equal" expression must be a value expression.');
    end;
    eqlNode := locSymbol as TXPathEqualNode;
    eqlNode.Left := left as TXPathValueNode;
    eqlNode.Right := right as TXPathValueNode;
    FTermStack.Push(eqlNode);
    Result := True;
    locFreeSym := False;
    FSymbolStack.Pop();
  end;

  function ReducePredicate() : Boolean;
  var
    locExp, locTmpNode : TXPathNode;
    distNode : TXPathMoveInstructionStepNode;
    locateNode : TXPathLocateInstructionStepNode;
  begin
    if not FTermStack.AtLeast(3) then
      raise EXpathException.Create('A predicate needs a expression to evaluate as its condition.');
    locTmpNode := FTermStack.Pop() as TXPathNode;
    if not locTmpNode.InheritsFrom(TXPathRigthSquareBraketNode) then begin
      FTermStack.Push(locTmpNode);
      raise EXpathException.Create('Predicate begining expected.');
    end;
    FreeAndNil(locTmpNode);
    locExp := FTermStack.Pop() as TXPathNode;
    if locExp.InheritsFrom(TXPathTypedValueNode) then begin
      locTmpNode := FTermStack.Pop() as TXPathNode;
      if not locTmpNode.InheritsFrom(TXPathLeftSquareBraketNode) then begin
        FTermStack.Push(locTmpNode);
        FTermStack.Push(locExp);
        raise EXpathException.Create('Predicate ending expected.');
      end;
      FreeAndNil(locTmpNode);
      case TXPathTypedValueNode(locExp).GetDataType() of
        xadtBoolean : ;
        xadtNumber :
          begin
            distNode := TXPathMoveInstructionStepNode.Create();
            if FTermStack.AtLeast(1) then
              locTmpNode := FTermStack.Pop() as TXPathNode
            else
              locTmpNode := nil;
            FTermStack.Push(distNode);
            if ( locTmpNode <> nil ) then begin
              locTmpNode.FNext := distNode;
            end;
            distNode.FDistance := TXPathTypedValueNode(locExp);
          end;
        else
          raise EXpathException.Create('A predicate''s condition expression must be of type "boolean" or "number".');
      end;
      locFreeSym := True;
    end else if locExp.InheritsFrom(TXPathPredicateNode) then begin
      locTmpNode := FTermStack.Pop() as TXPathNode;
      if not locTmpNode.InheritsFrom(TXPathLeftSquareBraketNode) then begin
        FTermStack.Push(locTmpNode);
        FTermStack.Push(locExp);
        raise EXpathException.Create('Predicate ending expected.');
      end;
      FreeAndNil(locTmpNode);
      if FTermStack.AtLeast(1) then
        locTmpNode := FTermStack.Pop() as TXPathNode
      else
        locTmpNode := nil;
      locateNode := TXPathLocateInstructionStepNode.Create();
      locateNode.FCondition := TXPathPredicateNode(locExp);
      FTermStack.Push(locateNode);
      if ( locTmpNode <> nil ) then begin
        locTmpNode.FNext := locateNode;
      end;
      locFreeSym := False;
    end else begin
      FTermStack.Push(locExp);
      raise EXpathException.Create('A predicate needs a typed expression to evaluate as its condition.');
    end;
    EndConditionExpression();
    Result := True;
  end;

var
  locRes : Boolean;
begin
  locRes := False;
  if FSymbolStack.AtLeast(1) then begin
    locFreeSym := False;
    locSymbol := FSymbolStack.Peek() as TXPathSymbolNode;
    try
      if locSymbol.InheritsFrom(TXPathSlashNode) then begin
        locRes := ReduceSlash();
      end else if locSymbol.InheritsFrom(TXPathEqualNode) then begin
        locRes := ReduceEqual();
      end;
    finally
      if locFreeSym then
        FreeAndNil(locSymbol);
    end;
  end;
  if ( not locRes ) and FTermStack.AtLeast(1) then begin
    if FTermStack.Peek().InheritsFrom(TXPathRigthSquareBraketNode) then
      locRes := ReducePredicate();
  end;
  Result := locRes;
end;

function TXPathParser.IsInConditionExpression() : Boolean;
begin
  Result := ( FInConditionExpression > 0 );
end;

function TXPathParser.Parse() : TXPathNode;
begin
  Clear();
  if ( FScanner.NextToken() = xtkEof ) then
    raise EXpathException.Create('Invalid XPath expression.');
  Result := nil;
  try
    while True do begin
      case FScanner.Token of
        xtkEof : Break;
        xtkTrue,
        xtkFalse :
          begin
            ReadBooleanConst();
            TryReduction();
          end;
        xtkNumber :
          begin
            ReadNumberConst();
            TryReduction();
          end;
        xtkString :
          begin
            ReadStringConst();
            TryReduction();
          end;
        xtkSymbol :
          begin
            ReadSymbol();
            TryReduction();
          end;
        xtkSlash :
          begin
            FSymbolStack.Push(TXPathSlashNode.Create());
            if ( FTermStack.Count = 0 ) then
              TryReduction();
          end;
        xtkSquaredBraceLeft :
          begin
            BeginConditionExpression();
            FTermStack.Push(TXPathLeftSquareBraketNode.Create());
          end;
        xtkSquaredBraceRigth :
          begin
            FTermStack.Push(TXPathRigthSquareBraketNode.Create());
            TryReduction();
          end;
        xtkEqual : FSymbolStack.Push(TXPathEqualNode.Create());
      end;
      FScanner.NextToken();
    end;
    if ( FTermStack.Count <> 1 ) then
      raise EXpathException.CreateFmt('Invalid xpath string : "%s".',[FXPath]);
    if ( FRootNode = nil ) then
      raise EXpathException.Create('Invalid xpath string');
    if not FRootNode.InheritsFrom(TXPathContextStepNode) then
      raise EXpathException.Create('Invalid xpath string');
    FTermStack.Pop();
    Result := FRootNode;
    FRootNode := nil;
  finally
    Clear();
  end;
end;

procedure TXPathParser.ReadBooleanConst();
begin
  FTermStack.Push(TXPathBooleanConstantNode.Create(FScanner.Token = xtkTrue));
end;

procedure TXPathParser.ReadNumberConst();
begin
  FTermStack.Push(TXPathNumberConstantNode.Create(FScanner.TokenNumber));
end;

procedure TXPathParser.ReadStringConst();
begin
  FTermStack.Push(TXPathStringConstantNode.Create(FScanner.TokenString));
end;

procedure TXPathParser.ReadSymbol();

  procedure ReadContextSymbol();
  var
    locCtxNode : TXPathContextStepNode;
  begin
    locCtxNode := TXPathContextStepNode.Create();
    locCtxNode.FSwitch := xcsProperty;
    locCtxNode.FPropertyName := FScanner.TokenString;
    FTermStack.Push(locCtxNode);
    if ( FRootNode = nil ) then
      FRootNode := locCtxNode;
  end;

  procedure ReadConditionSymbol();
  var
    locProp : TXPathValuePropertySymbolNode;
  begin
    locProp := TXPathValuePropertySymbolNode.Create();
    locProp.Symbol := FScanner.TokenString;
    FTermStack.Push(locProp);
  end;

begin
  if IsInConditionExpression() then
    ReadConditionSymbol()
  else
    ReadContextSymbol();
end;

procedure TXPathParser.TryReduction();
begin
  while InternalTryReduction() do begin
    ;// nothing!!
  end;
end;

{ TXPathStringConstantNode }

constructor TXPathStringConstantNode.Create(const AValue: string);
begin
  FValue := AValue;
end;

function TXPathStringConstantNode.Evaluate(): TXPathValueBuffer;
begin
  Result.Kind := GetDataType();
  Result.StringValue := Value;
end;

{ TXPathNumberConstantNode }

constructor TXPathNumberConstantNode.Create(const AValue: Extended);
begin
  FValue := AValue;
end;

function TXPathNumberConstantNode.Evaluate(): TXPathValueBuffer;
begin
  Result.Kind := GetDataType();
  Result.NumberValue := Value;
end;

{ TXPathBooleanConstantNode }

constructor TXPathBooleanConstantNode.Create(const AValue: TSDOBoolean);
begin
  FValue := AValue;
end;

function TXPathBooleanConstantNode.Evaluate(): TXPathValueBuffer;
begin
  Result.Kind := GetDataType();
  Result.BoolValue := Value;
end;

{ TXPathMoveInstructionStepNode }

destructor TXPathMoveInstructionStepNode.Destroy();
begin
  FreeAndNil(FDistance);
  inherited;
end;

class function TXPathMoveInstructionStepNode.GetCode() : TXPathInstructionCode;
begin
  Result := xicMove;
end;

{ TXPathContextStepNode }

class function TXPathContextStepNode.GetCode() : TXPathInstructionCode;
begin
  Result := xicContextChange;
end;

{ TXPathStringValueNode }

class function TXPathStringValueNode.GetDataType() : TXPathAtomicDataType;
begin
  Result := xadtString;
end;

{ TXPathNumberValueNode }

class function TXPathNumberValueNode.GetDataType() : TXPathAtomicDataType;
begin
  Result := xadtNumber;
end;

{ TXPathBooleanValueNode }

class function TXPathBooleanValueNode.GetDataType() : TXPathAtomicDataType;
begin
  Result := xadtBoolean;
end;


{ TXPathEqualNode }

destructor TXPathEqualNode.Destroy();
begin
  FreeAndNil(FRight);
  FreeAndNil(FLeft);
  inherited;
end;

class function TXPathEqualNode.GetCode() : TXPathPredicateCode;
begin
  Result := xpcEqual;
end;

{ TXPathLocateInstructionStepNode }

destructor TXPathLocateInstructionStepNode.Destroy;
begin
  FreeAndNil(FCondition);
  inherited;
end;

class function TXPathLocateInstructionStepNode.GetCode() : TXPathInstructionCode;
begin
  Result := xicLocate;
end;

end.
