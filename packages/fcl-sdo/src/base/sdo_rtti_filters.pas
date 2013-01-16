{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements RTTI filters.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_rtti_filters;

interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  sdo_cursor_intf, sdo_types;

type

  ERttiFilterException = class(Exception)
  end;

  TNumericFilterOperator = (
    nfoEqual, nfoGreater, nfoLesser, nfoNotEqual,
    nfoGreaterOrEqual, nfoLesserOrEqual
  );
  TStringFilterOperator = ( sfoEqualCaseSensitive, sfoEqualCaseInsensitive, sfoNotEqual );

  TRttiFilterCreatorTarget = TPersistent;
  TRttiFilterCreatorTargetClass = class of TRttiFilterCreatorTarget;

  TRttiExpNodeItem = class;
  TRttiExpNode = class;

  TClearAction = ( clrNone, clrFreeObjects );

  { TRttiFilterCreator }
  TRttiFilterCreator = class
  private
    FRoot : TRttiExpNode;
    FCurrent : TRttiExpNode;
    FTargetClass: TRttiFilterCreatorTargetClass;
    FCurrentStack : TObjectStack;
  private
    procedure AddNode(
      const ANode       : TRttiExpNodeItem;
      const AConnector  : TFilterConnector
    );
    procedure PushCurrent(ACurrent : TRttiExpNode);
    function PopCurrent() : TRttiExpNode;
  public
    constructor Create(const ATargetClass : TRttiFilterCreatorTargetClass);
    destructor Destroy();override;
    procedure Clear(const AFreeObjects : TClearAction);
    function AddCondition(
      const APropertyName : string;
      const AOperator     : TNumericFilterOperator;
      const AValue        : Integer;
      const AConnector    : TFilterConnector
    ) : TRttiFilterCreator;overload;
    function AddCondition(
      const APropertyName : string;
      const AOperator     : TNumericFilterOperator;
      const AEnumValue    : string;
      const AConnector    : TFilterConnector
    ) : TRttiFilterCreator;overload;

    function AddCondition(
      const APropertyName : string;
      const AOperator     : TStringFilterOperator;
      const AValue        : AnsiString;
      const AConnector    : TFilterConnector
    ) : TRttiFilterCreator;overload;
    function AddCondition(
      const APropertyName : string;
      const AOperator     : TStringFilterOperator;
      const AValue        : WideString;
      const AConnector    : TFilterConnector
    ) : TRttiFilterCreator;overload;
{$IFDEF USE_UNICODE}
    function AddCondition(
      const APropertyName : string;
      const AOperator     : TStringFilterOperator;
      const AValue        : UnicodeString;
      const AConnector    : TFilterConnector
    ) : TRttiFilterCreator;overload;
{$ENDIF USE_UNICODE}

    function BeginGroup(const AConnector : TFilterConnector):TRttiFilterCreator;
    function EndGroup():TRttiFilterCreator;
    property TargetClass : TRttiFilterCreatorTargetClass read FTargetClass;
    property Root : TRttiExpNode read FRoot;
  end;


  TRttiExpNodeItem = class
  public
    function Evaluate(AInstance : TRttiFilterCreatorTarget):Boolean;virtual;abstract;
  end;

  { TRttiObjectFilter }

  TRttiObjectFilter = class(TInterfacedObject,IObjectFilter)
  private
    FFilterRoot : TRttiExpNodeItem;
    FOnDestroyFilterAction : TClearAction;
  protected
    function Evaluate(const AObject : TObject) : Boolean;
  public
    constructor Create(
            AFilterRoot            : TRttiExpNodeItem;
      const AOnDestroyFilterAction : TClearAction
    );
    destructor Destroy();override;
  end;

  { TRttiExpNode }

  TRttiExpNode = class(TRttiExpNodeItem)
  private
    FConnector: TFilterConnector;
    FLeft: TRttiExpNodeItem;
    FRight: TRttiExpNodeItem;
  private
    procedure SetConnector(const AValue: TFilterConnector);
    procedure SetLeft(const AValue: TRttiExpNodeItem);
    procedure SetRight(const AValue: TRttiExpNodeItem);
  public
    destructor Destroy();override;
    function Evaluate(AInstance : TRttiFilterCreatorTarget):Boolean;override;
    property Connector : TFilterConnector read FConnector write SetConnector;
    property Left : TRttiExpNodeItem read FLeft write SetLeft;
    property Right : TRttiExpNodeItem read FRight write SetRight;
  end;

  { TRttiExpConcreteNodeItem }

  TRttiExpConcreteNodeItem = class(TRttiExpNodeItem)
  private
    FPropInfo: PPropInfo;
  public
    constructor Create(const APropInfo : PPropInfo);
    property PropInfo : PPropInfo read FPropInfo;
  end;

  { TRttiExpNumericNodeItem }

  TRttiExpNumericNodeItem = class(TRttiExpConcreteNodeItem)
  private
    FOperation: TNumericFilterOperator;
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TNumericFilterOperator
    );
    property Operation : TNumericFilterOperator read FOperation;
  end;

  { TRttiExpIntegerNodeItem }

  TRttiExpIntegerNodeItem = class(TRttiExpNumericNodeItem)
  private
    FComparedValue: Integer;
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TNumericFilterOperator;
      const AComparedValue : Integer
    );
    function Evaluate(AInstance : TRttiFilterCreatorTarget):Boolean;override;
    property ComparedValue : Integer read FComparedValue;
  end;

  { TRttiExpEnumNodeItem }

  TRttiExpEnumNodeItem = class(TRttiExpIntegerNodeItem)
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TNumericFilterOperator;
      const AComparedValue : string
    );
  end;

  { TRttiExpStringNodeItem }

  TRttiExpStringNodeItem = class(TRttiExpConcreteNodeItem)
  private
    FOperation: TStringFilterOperator;
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TStringFilterOperator
    );
    property Operation : TStringFilterOperator read FOperation;
  end;

  { TRttiExpAnsiStringNodeItem }

  TRttiExpAnsiStringNodeItem = class(TRttiExpStringNodeItem)
  private
    FComparedValue: AnsiString;
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TStringFilterOperator;
      const AComparedValue : AnsiString
    );
    function Evaluate(AInstance : TRttiFilterCreatorTarget):Boolean;override;
    property ComparedValue : AnsiString read FComparedValue;
  end;

  { TRttiExpWideStringNodeItem }

  TRttiExpWideStringNodeItem = class(TRttiExpStringNodeItem)
  private
    FComparedValue: WideString;
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TStringFilterOperator;
      const AComparedValue : WideString
    );
    function Evaluate(AInstance : TRttiFilterCreatorTarget):Boolean;override;
    property ComparedValue : WideString read FComparedValue;
  end;

{$IFDEF USE_UNICODE}
  { TRttiExpUnicodeStringNodeItem }

  TRttiExpUnicodeStringNodeItem = class(TRttiExpStringNodeItem)
  private
    FComparedValue: UnicodeString;
  public
    constructor Create(
      const APropInfo      : PPropInfo;
      const AOperation     : TStringFilterOperator;
      const AComparedValue : UnicodeString
    );
    function Evaluate(AInstance : TRttiFilterCreatorTarget):Boolean;override;
    property ComparedValue : UnicodeString read FComparedValue;
  end;
{$ENDIF USE_UNICODE}

  procedure ParseFilter(const AFilterText: string; AFltrCrtr : TRttiFilterCreator);overload;
  function ParseFilter(
    const AFilterText  : string;
          ATargetClass : TRttiFilterCreatorTargetClass
  ) : IObjectFilter;overload;

implementation

function ParseFilter(
  const AFilterText  : string;
        ATargetClass : TRttiFilterCreatorTargetClass
) : IObjectFilter;
var
  fltr : TRttiFilterCreator;
begin
  Result := nil;
  fltr := TRttiFilterCreator.Create(ATargetClass);
  try
    try
      ParseFilter(AFilterText,fltr);
      Result := TRttiObjectFilter.Create(fltr.Root,clrFreeObjects);
      fltr.Clear(clrNone);
    except
      fltr.Clear(clrFreeObjects);
      raise;
    end;
  finally
    FreeAndNil(fltr);
  end;
end;

procedure ParseFilter(const AFilterText: string; AFltrCrtr : TRttiFilterCreator);
const
  tkn_LeftParenthesis  = '('; tkn_RigthParenthesis = ')';
  tkn_Equal = '='; tkn_NotEqual = '<>';
  tkn_Sup = '>'; tkn_Inf = '<';
  tkn_And = 'and'; tkn_Or = 'or';
var
  strm : TStringStream;
  prsr : TParser;

  procedure MoveNext();
  begin
    prsr.NextToken();
    if ( prsr.Token = toEOF ) then
      raise ERttiFilterException.Create('Unexpected end of filter.');
  end;

var
  propName : string;
  propInfo : PPropInfo;
  lastCntr : TFilterConnector;

  procedure Handle_String();
  var
    s : string;
    ws : WideString;
{$IFDEF USE_UNICODE}
    us : UnicodeString;
{$ENDIF USE_UNICODE}
    fltrOp : TStringFilterOperator;
  begin
    MoveNext();
    s := prsr.TokenString();
    if ( s = tkn_Equal ) then
      fltrOp := sfoEqualCaseInsensitive
    else if ( s = tkn_NotEqual ) then
      fltrOp := sfoNotEqual
    else if ( s = tkn_Inf ) then begin
      MoveNext();
      if ( prsr.Token = tkn_Sup ) then
        fltrOp := sfoNotEqual
      else
        raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
    end else begin
      raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
    end;
    MoveNext();
    prsr.CheckToken(toString);
    case propInfo^.PropType^.Kind of
      tkString, tkLString :
        begin
          AFltrCrtr.AddCondition(propName,fltrOp,AnsiString(prsr.TokenString()),lastCntr);
        end;
      tkWString :
        begin
          ws := prsr.TokenString();
          AFltrCrtr.AddCondition(propName,fltrOp,ws,lastCntr);
        end;
{$IFDEF USE_UNICODE}
      tkUString :
        begin
          us := prsr.TokenString();
          AFltrCrtr.AddCondition(propName,fltrOp,us,lastCntr);
        end;
{$ENDIF USE_UNICODE}
      else
        begin
          s := prsr.TokenString();
          AFltrCrtr.AddCondition(propName,fltrOp,s,lastCntr);
        end;
    end;
  end;

  procedure Handle_Integer();
  var
    s : string;
    fltrOp : TNumericFilterOperator;
  begin
    MoveNext();
    s := prsr.TokenString();
    if ( s = tkn_Equal ) then
      fltrOp := nfoEqual
    else if ( s = tkn_NotEqual ) then
      fltrOp := nfoNotEqual
    else if ( s = tkn_Inf ) then
      fltrOp := nfoLesser
    else if ( s = tkn_Sup ) then
      fltrOp := nfoGreater
    else
      raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
    MoveNext();
    if ( prsr.Token = tkn_Equal ) then begin
      case fltrOp of
        nfoGreater : fltrOp := nfoGreaterOrEqual;
        nfoLesser  : fltrOp := nfoLesserOrEqual;
        else
          raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
      end;
      MoveNext();
    end else if ( prsr.Token = tkn_Sup ) then begin
      if ( fltrOp = nfoLesser ) then
        fltrOp := nfoNotEqual
      else
        raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
      MoveNext();
    end;
    prsr.CheckToken(toInteger);
    AFltrCrtr.AddCondition(propName,fltrOp,prsr.TokenInt(),lastCntr);
  end;

  procedure Handle_Enum();
  var
    s : string;
    fltrOp : TNumericFilterOperator;
  begin
    MoveNext();
    s := prsr.TokenString();
    if ( s = tkn_Equal ) then
      fltrOp := nfoEqual
    else if ( s = tkn_NotEqual ) then
      fltrOp := nfoNotEqual
    else if ( s = tkn_Inf ) then
      fltrOp := nfoLesser
    else if ( s = tkn_Sup ) then
      fltrOp := nfoGreater
    else
      raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
    MoveNext();
    if ( prsr.Token = tkn_Equal ) then begin
      case fltrOp of
        nfoGreater : fltrOp := nfoGreaterOrEqual;
        nfoLesser  : fltrOp := nfoLesserOrEqual;
        else
          raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
      end;
      MoveNext();
    end else if ( prsr.Token = tkn_Sup ) then begin
      if ( fltrOp = nfoLesser ) then
        fltrOp := nfoNotEqual
      else
        raise ERttiFilterException.CreateFmt('Unexpected symbol : "%s".',[s]);
      MoveNext();
    end;
    if (prsr.Token = toInteger) then begin
      AFltrCrtr.AddCondition(propName,fltrOp,prsr.TokenInt,lastCntr)
    end else begin
      prsr.CheckToken(toSymbol);
      AFltrCrtr.AddCondition(propName,fltrOp,prsr.TokenString(),lastCntr);
    end;
  end;

var
  s : string;
begin
  lastCntr := fcAnd;
  AFltrCrtr.Clear(clrFreeObjects);
  strm := TStringStream.Create(Trim(AFilterText));
  try
    prsr := TParser.Create(strm);
    while ( prsr.Token <> toEOF ) do begin
      s := prsr.TokenString();
      if SameText(s,tkn_LeftParenthesis) then
        AFltrCrtr.BeginGroup(lastCntr)
      else if SameText(s,tkn_RigthParenthesis) then
        AFltrCrtr.EndGroup()
      else if SameText(s,tkn_And) then
        lastCntr := fcAnd
      else if SameText(s,tkn_Or) then
        lastCntr := fcOr
      else begin
        prsr.CheckToken(toSymbol);
        propName := prsr.TokenString();
        propInfo := GetPropInfo(AFltrCrtr.TargetClass,propName);
        if ( propInfo = nil ) then
          raise ERttiFilterException.CreateFmt('Invalid property : "%s"',[propName]);
        if ( propInfo^.PropType^.Kind in
             [ {$IFDEF FPC}tkSString,tkAString,{$ENDIF}
               {$IFDEF USE_UNICODE}tkUString,{$ENDIF}
               tkLString,tkWString
             ]
           )
        then
          Handle_String()
        else if ( propInfo^.PropType^.Kind in [tkInteger,tkInt64{$IFDEF HAS_QWORD},tkQWord{$ENDIF}] ) then
          Handle_Integer()
        else if ( propInfo^.PropType^.Kind in [tkEnumeration {$IFDEF HAS_TKBOOL},tkBool{$ENDIF}] ) then
          Handle_Enum()
        else
          raise ERttiFilterException.CreateFmt('Type not handled : "%s"',[GetEnumName(TypeInfo(TTypeKind),Ord(propInfo^.PropType^.Kind))]);
      end;
      prsr.NextToken();
    end;
  finally
    FreeAndNil(prsr);
    FreeAndNil(strm);
  end;
end;

procedure ClearObject(ARoot : TRttiExpNodeItem);
begin
  if Assigned(ARoot) then begin
    if ARoot.InheritsFrom(TRttiExpNode) then begin
      with TRttiExpNode(ARoot) do begin
        ClearObject(Right);
        Right := nil;
        ClearObject(Left);
        Left := nil;
      end;
    end;
    ARoot.Free();
  end;
end;

{ TRttiExpNode }

procedure TRttiExpNode.SetRight(const AValue: TRttiExpNodeItem);
begin
  if ( Connector = fcNone ) and ( AValue <> nil ) then
    raise ERttiFilterException.Create('"Connector" must be set before "Right".');
  //FreeAndNil(FRight);
  FRight := AValue;
end;

procedure TRttiExpNode.SetConnector(const AValue: TFilterConnector);
begin
  if ( AValue = fcNone ) and ( FRight <> nil ) then
    raise ERttiFilterException.Create('"Right" must be set to "nil" before "Connector" can be set to "none".');
  FConnector := AValue;
end;

procedure TRttiExpNode.SetLeft(const AValue: TRttiExpNodeItem);
begin
  if ( FRight <> nil ) and ( AValue = nil ) then
    raise ERttiFilterException.Create('"Right" must be set to "nil" before "Left" can be set to "none".');
  //FreeAndNil(FLeft);
  FLeft := AValue;
end;

destructor TRttiExpNode.Destroy();
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  inherited Destroy();
end;

function TRttiExpNode.Evaluate(AInstance: TRttiFilterCreatorTarget): Boolean;
begin
  if Assigned(Left) then begin
    Result := Left.Evaluate(AInstance);
    if not Assigned(Right) then
      Exit;
    if Result and ( Connector = fcOr ) then
      Exit;
    if ( not Result ) and ( Connector = fcAnd ) then
      Exit;
    Result := Right.Evaluate(AInstance);
  end else begin
    Result := False;
  end;
end;

{ TRttiExpConcreteNodeItem }

constructor TRttiExpConcreteNodeItem.Create(const APropInfo : PPropInfo);
begin
  Assert(Assigned(APropInfo));
  FPropInfo := APropInfo;
end;

{ TRttiExpIntegerNodeItem }

constructor TRttiExpIntegerNodeItem.Create(
  const APropInfo      : PPropInfo;
  const AOperation     : TNumericFilterOperator;
  const AComparedValue : Integer
);
begin
  Assert(Assigned(APropInfo));
  if not ( APropInfo^.PropType^.Kind in [tkInteger,tkInt64,tkEnumeration{$IFDEF HAS_QWORD},tkQWord{$ENDIF}{$IFDEF HAS_TKBOOL},tkBool{$ENDIF}] ) then
    raise ERttiFilterException.CreateFmt('Invalid property data type. "%s" excpeted.',['Integer']);
  inherited Create(APropInfo,AOperation);
  FComparedValue := AComparedValue;
end;

function TRttiExpIntegerNodeItem.Evaluate(AInstance: TRttiFilterCreatorTarget): Boolean;
begin
  case Operation of
    nfoEqual          :  Result := ( GetOrdProp(AInstance,PropInfo) =  ComparedValue );
    nfoGreater        :  Result := ( GetOrdProp(AInstance,PropInfo) >  ComparedValue );
    nfoLesser         :  Result := ( GetOrdProp(AInstance,PropInfo) <  ComparedValue );
    nfoNotEqual       :  Result := ( GetOrdProp(AInstance,PropInfo) <> ComparedValue );
    nfoGreaterOrEqual :  Result := ( GetOrdProp(AInstance,PropInfo) >=  ComparedValue );
    nfoLesserOrEqual  :  Result := ( GetOrdProp(AInstance,PropInfo) <=  ComparedValue );
    else begin
      Assert(False);
      Result := False;
    end;
  end;
end;

{ TRttiFilterCreator }

procedure TRttiFilterCreator.AddNode(
  const ANode       : TRttiExpNodeItem;
  const AConnector  : TFilterConnector
);
var
  tmpNode : TRttiExpNode;
begin
  Assert(Assigned(ANode));
  if not Assigned(FRoot) then begin
    FRoot := TRttiExpNode.Create();
    FCurrent := FRoot;
  end;
  if not Assigned(FCurrent.Left) then begin
    FCurrent.Left := ANode;
    FCurrent.Connector := AConnector;
    Exit;
  end;
  if ( AConnector <= fcNone ) then
    raise ERttiFilterException.Create('Invalid connector value.');
  if not Assigned(FCurrent.Right) then begin
    FCurrent.Right := ANode;
    FCurrent.Connector := AConnector;
    Exit;
  end;
  tmpNode := TRttiExpNode.Create();
  tmpNode.Left := FCurrent.Right;
  FCurrent.Right := tmpNode;
  FCurrent := tmpNode;
  FCurrent.Connector := AConnector;
  FCurrent.Right := ANode;
end;

procedure TRttiFilterCreator.PushCurrent(ACurrent: TRttiExpNode);
begin
  FCurrentStack.Push(FCurrent);
  FCurrent := ACurrent;
end;

function TRttiFilterCreator.PopCurrent(): TRttiExpNode;
begin
  if not FCurrentStack.AtLeast(1) then
    raise ERttiFilterException.Create('"BeginGroup" must be called before "EndGroup".');
  Result := FCurrentStack.Pop() as TRttiExpNode;
  FCurrent := Result;
end;

constructor TRttiFilterCreator.Create(const ATargetClass: TRttiFilterCreatorTargetClass);
begin
  Assert(Assigned(ATargetClass));
  FTargetClass := ATargetClass;
  FCurrentStack := TObjectStack.Create();
end;

destructor TRttiFilterCreator.Destroy();
begin
  FreeAndNil(FCurrentStack);
  inherited Destroy();
end;

procedure TRttiFilterCreator.Clear(const AFreeObjects: TClearAction);
var
  i : Integer;
begin
  if ( AFreeObjects = clrFreeObjects ) then
    ClearObject(FRoot);
  for i := 0 to Pred(FCurrentStack.Count) do
    FCurrentStack.Pop();
  FRoot := nil;
  FCurrent := nil;
end;

function TRttiFilterCreator.AddCondition(
  const APropertyName    : string;
  const AOperator        : TNumericFilterOperator;
  const AValue           : Integer;
  const AConnector       : TFilterConnector
) : TRttiFilterCreator;
begin
  AddNode(
    TRttiExpIntegerNodeItem.Create(GetPropInfo(TargetClass,APropertyName),AOperator,AValue),
    AConnector
  );
  Result := Self;
end;

function TRttiFilterCreator.AddCondition(
  const APropertyName : string;
  const AOperator : TNumericFilterOperator;
  const AEnumValue : string;
  const AConnector : TFilterConnector
) : TRttiFilterCreator;
begin
  AddNode(
    TRttiExpEnumNodeItem.Create(GetPropInfo(TargetClass,APropertyName),AOperator,AEnumValue),
    AConnector
  );
  Result := Self;
end;

function TRttiFilterCreator.AddCondition(
  const APropertyName : string;
  const AOperator     : TStringFilterOperator;
  const AValue        : AnsiString;
  const AConnector    : TFilterConnector
): TRttiFilterCreator;
begin
  AddNode(
    TRttiExpAnsiStringNodeItem.Create(GetPropInfo(TargetClass,APropertyName),AOperator,AValue),
    AConnector
  );
  Result := Self;
end;

function TRttiFilterCreator.AddCondition(
  const APropertyName : string;
  const AOperator     : TStringFilterOperator;
  const AValue        : WideString;
  const AConnector    : TFilterConnector
): TRttiFilterCreator;
begin
  AddNode(
    TRttiExpWideStringNodeItem.Create(GetPropInfo(TargetClass,APropertyName),AOperator,AValue),
    AConnector
  );
  Result := Self;
end;

{$IFDEF USE_UNICODE}
function TRttiFilterCreator.AddCondition(
  const APropertyName: string;
  const AOperator: TStringFilterOperator;
  const AValue: UnicodeString;
  const AConnector: TFilterConnector
) : TRttiFilterCreator;
begin
  AddNode(
    TRttiExpUnicodeStringNodeItem.Create(GetPropInfo(TargetClass,APropertyName),AOperator,AValue),
    AConnector
  );
  Result := Self;
end;
{$ENDIF USE_UNICODE}

function TRttiFilterCreator.BeginGroup(const AConnector: TFilterConnector):TRttiFilterCreator;
var
  gn : TRttiExpNode;
begin
  {if not Assigned(FCurrent) then
    AddNode(TRttiExpNode.Create(),fcNone);}
  gn := TRttiExpNode.Create();
  AddNode(gn,AConnector);
  PushCurrent(gn);
  Result := Self;
end;

function TRttiFilterCreator.EndGroup(): TRttiFilterCreator;
begin
  PopCurrent();
  Result := Self;
end;

{ TRttiObjectFilter }

function TRttiObjectFilter.Evaluate(const AObject: TObject): Boolean;
begin
  Result := FFilterRoot.Evaluate(TRttiFilterCreatorTarget(AObject));
end;

constructor TRttiObjectFilter.Create(
        AFilterRoot              : TRttiExpNodeItem;
  const AOnDestroyFilterAction   : TClearAction
);
begin
  Assert(Assigned(AFilterRoot));
  FFilterRoot := AFilterRoot;
  FOnDestroyFilterAction := AOnDestroyFilterAction;
end;

destructor TRttiObjectFilter.Destroy();
begin
  if ( FOnDestroyFilterAction = clrFreeObjects ) then
    ClearObject(FFilterRoot);
  inherited Destroy();
end;

{ TRttiExpNumericNodeItem }

constructor TRttiExpNumericNodeItem.Create(
  const APropInfo: PPropInfo;
  const AOperation: TNumericFilterOperator
);
begin
  Assert(Assigned(APropInfo));
  inherited Create(APropInfo);
  FOperation := AOperation;
end;

{ TRttiExpStringNodeItem }

constructor TRttiExpStringNodeItem.Create(
  const APropInfo: PPropInfo;
  const AOperation: TStringFilterOperator
);
begin
  Assert(Assigned(APropInfo));
  inherited Create(APropInfo);
  FOperation := AOperation;
end;

{ TRttiExpAnsiStringNodeItem }

constructor TRttiExpAnsiStringNodeItem.Create(
  const APropInfo: PPropInfo;
  const AOperation: TStringFilterOperator;
  const AComparedValue: AnsiString
);
begin
  Assert(Assigned(APropInfo));
  if not ( APropInfo^.PropType^.Kind in [{$IFDEF FPC}tkSString,tkAString,{$ENDIF}tkLString] ) then
    raise ERttiFilterException.CreateFmt('Invalid property data type. "%s" excpeted.',['AnsiString']);
  inherited Create(APropInfo,AOperation);
  FComparedValue := AComparedValue;
end;

function TRttiExpAnsiStringNodeItem.Evaluate(AInstance: TRttiFilterCreatorTarget): Boolean;
begin
  case Operation of
    sfoEqualCaseSensitive   :  Result := ( GetStrProp(AInstance,PropInfo) = ComparedValue );
    sfoEqualCaseInsensitive :  Result := AnsiSameText(GetStrProp(AInstance,PropInfo),ComparedValue);
    sfoNotEqual             :  Result := ( GetStrProp(AInstance,PropInfo) <> ComparedValue);
    else begin
      Assert(False);
      Result := False;
    end;
  end;
end;

{ TRttiExpWideStringNodeItem }

constructor TRttiExpWideStringNodeItem.Create(
  const APropInfo: PPropInfo;
  const AOperation: TStringFilterOperator;
  const AComparedValue: WideString
);
begin
  Assert(Assigned(APropInfo));
  if not ( APropInfo^.PropType^.Kind in [tkWString{$IFDEF USE_UNICODE},tkUString{$ENDIF}] ) then
    raise ERttiFilterException.CreateFmt('Invalid property data type. "%s" excpeted, got "%s".',['WideString',GetEnumName(TypeInfo(TTypeKind),Ord(APropInfo^.PropType^.Kind))]);
  inherited Create(APropInfo,AOperation);
  FComparedValue := AComparedValue;
end;

function TRttiExpWideStringNodeItem.Evaluate(AInstance: TRttiFilterCreatorTarget): Boolean;
begin
  case Operation of
    sfoEqualCaseSensitive   :  Result := ( GetWideStrProp(AInstance,PropInfo) = ComparedValue );
    sfoEqualCaseInsensitive :  Result := ( LowerCase(GetWideStrProp(AInstance,PropInfo)) = LowerCase(ComparedValue) );
    sfoNotEqual             :  Result := not SameText(GetWideStrProp(AInstance,PropInfo),ComparedValue);
    else begin
      Assert(False);
      Result := False;
    end;
  end;
end;

{$IFDEF USE_UNICODE}
{ TRttiExpUnicodeStringNodeItem }

constructor TRttiExpUnicodeStringNodeItem.Create(
  const APropInfo: PPropInfo;
  const AOperation: TStringFilterOperator;
  const AComparedValue: UnicodeString
);
begin
  Assert(Assigned(APropInfo));
  if not ( APropInfo^.PropType^.Kind in [tkUString,tkWString] ) then
    raise ERttiFilterException.CreateFmt('Invalid property data type. "%s" excpeted, got "%s".',['UnicodeString',GetEnumName(TypeInfo(TTypeKind),Ord(APropInfo^.PropType^.Kind))]);
  inherited Create(APropInfo,AOperation);
  FComparedValue := AComparedValue;
end;

function TRttiExpUnicodeStringNodeItem.Evaluate(
  AInstance: TRttiFilterCreatorTarget
): Boolean;
begin
  case Operation of
    sfoEqualCaseSensitive   :  Result := ( GetUnicodeStrProp(AInstance,PropInfo) = ComparedValue );
    sfoEqualCaseInsensitive :  Result := ( LowerCase(GetUnicodeStrProp(AInstance,PropInfo)) = LowerCase(ComparedValue));
    sfoNotEqual             :  Result := not SameText(GetUnicodeStrProp(AInstance,PropInfo),ComparedValue);
    else
      Assert(False);
  end;
end;
{$ENDIF USE_UNICODE}

{ TRttiExpEnumNodeItem }

constructor TRttiExpEnumNodeItem.Create(
  const APropInfo : PPropInfo;
  const AOperation : TNumericFilterOperator;
  const AComparedValue : string
);
{$IFDEF HAS_TKBOOL}
var
  locEnumOrder : Integer;
  locBoolVal : Boolean;
begin
  Assert(Assigned(APropInfo));
  if not ( APropInfo^.PropType^.Kind in [tkEnumeration,tkBool] ) then
    raise ERttiFilterException.CreateFmt('Invalid property data type. "%s" excpeted.',['Enumeration']);
  if ( APropInfo^.PropType^.Kind = tkBool ) then begin
    if not TryStrToBool(AComparedValue,locBoolVal) then
      raise ERttiFilterException.CreateFmt('Unknown boolean value : "%s", type : %s .',[AComparedValue,APropInfo^.PropType^.Name]);
    locEnumOrder := Ord(locBoolVal);
  end else begin
    locEnumOrder := GetEnumValue(APropInfo^.PropType,AComparedValue);
    if ( locEnumOrder < 0 ) then
      raise ERttiFilterException.CreateFmt('Unknown enumeration value : "%s", type : %s .',[AComparedValue,APropInfo^.PropType^.Name]);
  end;
  inherited Create(APropInfo,AOperation,locEnumOrder);
end;
{$ELSE}
var
  locEnumOrder : Integer;
begin
  Assert(Assigned(APropInfo));
  if not ( APropInfo^.PropType^.Kind = tkEnumeration ) then
    raise ERttiFilterException.CreateFmt('Invalid property data type. "%s" excpeted.',['Enumeration']);
  locEnumOrder := GetEnumValue(APropInfo^.PropType{$IFDEF DELPHI}^{$ENDIF},AComparedValue);
  if ( locEnumOrder < 0 ) then
    raise ERttiFilterException.CreateFmt('Unknown enumeration value : "%s", type : %s .',[AComparedValue,APropInfo^.PropType^.Name]);
  inherited Create(APropInfo,AOperation,locEnumOrder);
end;
{$ENDIF}

end.