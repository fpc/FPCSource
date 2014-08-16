{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements serializing SDO objects to XML

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_serialization_xml;

interface
uses
  SysUtils, Classes, Contnrs,
  sdo_types, sdo, sdo_consts, sdo_serialization_utils
{$IFDEF DELPHI}
  , xmldom, sdo_win_xml
{$ENDIF DELPHI}
{$IFDEF FPC}
  , DOM, sdo_fpc_xml, XMLRead, XMLWrite
{$ENDIF FPC}
  ;

type

  TSDOXMLDocument = {$IFDEF DELPHI}sdo_win_xml.TXMLDocument{$ELSE}TXMLDocument{$ENDIF};

  { TStackItem }

  TStackItem = class
  private
    FEmbeddedScopeCount: Integer;
    FNameSpace: string;
    FScopeObject: TDOMNode;
    FScopeType: TScopeType;
  protected
    constructor Create();overload; virtual;
    function GetItemsCount(const AStyle : TSerializationStyle) : Integer;virtual;
    procedure CopyTo(const AClone : TStackItem);virtual;
  public
    constructor Create(AScopeObject : TDOMNode;AScopeType : TScopeType);overload;
    function Clone() : TStackItem;virtual;
    function FindNode(var ANodeName : string):TDOMNode;virtual;abstract;
    procedure SetNameSpace(const ANameSpace : string);
    property ScopeObject : TDOMNode Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property NameSpace : string Read FNameSpace;
    property ItemsCount[const AStyle : TSerializationStyle] : Integer read GetItemsCount;

    property EmbeddedScopeCount : Integer read FEmbeddedScopeCount;
    function BeginEmbeddedScope() : Integer;
    function EndEmbeddedScope() : Integer;

    function GetScopeItemNames(
      const AItemStyle : TSerializationStyle;
      const AReturnList : TStrings
    ) : Integer;virtual;

    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  end;

  TStackItemClass = class of TStackItem;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  public
    function FindNode(var ANodeName : string):TDOMNode;override;
  end;

  { TAbstractArrayStackItem }

  TAbstractArrayStackItem = class(TStackItem)
  private
    FItemList : TDOMNodeList;
    FIndex : Integer;
    FItemName : string;
  protected
    procedure CopyTo(const AClone : TStackItem);override;
    procedure EnsureListCreated();
    function GetItemsCount(const AStyle : TSerializationStyle) : Integer;override;
    function CreateList(const ANodeName : string):TDOMNodeList;virtual;abstract;
  public
    constructor Create(
            AScopeObject : TDOMNode;
      const AScopeType   : TScopeType;
      const AItemName    : string
    );
    destructor Destroy();override;
    function FindNode(var ANodeName : string):TDOMNode;override;
  end;

  { TScopedArrayStackItem }

  TScopedArrayStackItem = class(TAbstractArrayStackItem)
  protected
    function CreateList(const ANodeName : string):TDOMNodeList;override;
  public
    destructor Destroy();override;  
  end;

  { TEmbeddedArrayStackItem }

  TEmbeddedArrayStackItem = class(TAbstractArrayStackItem)
  protected
    function CreateList(const ANodeName : string):TDOMNodeList;override;
  end;

  TStreamXMLBookmark = class(TStreamBookmark)
  private
    FNameStyle: TNameStyle;
    FStack: TObjectStackEx;
    FDoc: TSDOXMLDocument;
    FSerializationStyle: TSerializationStyle;
  public
    destructor Destroy();override;
    property SerializationStyle : TSerializationStyle read FSerializationStyle;
    property NameStyle : TNameStyle read FNameStyle;
    property Doc : TSDOXMLDocument read FDoc;
    property Stack : TObjectStackEx read FStack;
  end;

  TSDOSerializerStreamXML = class(TInterfacedObject,IInterface,ISDOSerializerStream)
  private
    FSerializationStyle : TSerializationStyle;
    FNameStyle : TNameStyle;
    FDoc : TSDOXMLDocument;
    FStack : TObjectStackEx;
    FPreferedShortNames : TStrings;
    FNameSpaceCounter : PtrInt;
  private
    procedure ClearStack();{$IFDEF USE_INLINE}inline;{$ENDIF}
    function StackTop():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStack(AScopeObject : TDOMNode):TStackItem;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStack(
      const AScopeObject : TDOMNode;
      const AItemName    : string;
      const AStyle       : TArrayStyle = asEmbeded
    ) : TStackItem;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PopStack():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function HasScope():Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckScope();{$IFDEF USE_INLINE}inline;{$ENDIF}
    //procedure AddScopeAttribute(const AName,AValue : string);
    procedure InternalClear(const ACreateDoc : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetCurrentScopeObject():TDOMElement;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function NextNameSpaceCounter():Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function FindAttributeByValueInScope(const AAttValue : string) : string;
    function FindAttributeByValueInNode(
      const AAttValue : string;
      const ANode     : TDOMNode;
      out   AResAtt   : string
    ) : Boolean;
    function FindNameSpace(const AShortName : string; out ARes : string) : Boolean;
    procedure BeginScope(
      const AScopeName,
            ANameSpace  : string;
      const AScopeType  : TScopeType;
      const AStyle      : TArrayStyle = asEmbeded
    );
    function InternalBeginScopeRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType;
      const AScopeType : TScopeType;
      const AItemName  : string;
      const AStyle     : TArrayStyle = asEmbeded
    ):Integer;
  private
    function InternalPutData(
      const AName : string;
      const AData : TSDOString; 
      const ANameSpace : string
    ) : TDOMNode;
    function PutBoolean(const AName : string; const AData : TSDOBoolean; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutByte(const AName : string; const AData : TSDOByte; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_BYTES}
    function PutBytes(const AName : string; const AData : TSDOBytes; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    function PutChar(const AName : string; const AData : TSDOChar; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function PutCurrency(const AName : string; const AData : TSDOCurrency; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
    function PutDate(const AName : string; const AData : TSDODateTime; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_DOUBLE}
    function PutDouble(const AName : string; const AData : TSDODouble; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function PutFloat(const AName : string; const AData : TSDOFloat; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_FLOAT}
    function PutInteger(const AName : string; const AData : TSDOInteger; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_LONG}
    function PutLong(const AName : string; const AData : TSDOLong; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function PutShort(const AName : string; const AData : TSDOShort; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}
    function PutString(const AName : string; const AData : TSDOString; const ANameSpace : string):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetNodeValue(const ANameSpace : string; var AName : string; out ARes : DOMString) : Boolean;
    function GetBoolean(var AName : string;var AData : TSDOBoolean; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetByte(var AName : string;var AData : TSDOByte; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_BYTES}
    function GetBytes(var AName : string;var AData : TSDOBytes; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    function GetChar(var AName : string;var AData : TSDOChar; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function GetCurrency(var AName : string;var AData : TSDOCurrency; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
    function GetDate(var AName : string;var AData : TSDODateTime; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_DOUBLE}
    function GetDouble(var AName : string;var AData : TSDODouble; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function GetFloat(var AName : string;var AData : TSDOFloat; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_FLOAT}
    function GetInteger(var AName : string;var AData : TSDOInteger; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_LONG}
    function GetLong(var AName : string;var AData : TSDOLong; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function GetShort(var AName : string;var AData : TSDOShort; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}
    function GetString(var AName : string;var AData : TSDOString; const ANameSpace : string) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function GetFormatName() : string;
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    procedure SetNameStyle(const AValue : TNameStyle);
    function GetNameStyle() : TNameStyle;
    function GetCurrentScope():string;
    procedure Clear();
    procedure Initialize();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : ISDOType
    );
    procedure BeginArray(
      const AName         : string;
      const AItemTypeInfo : ISDOType;
      const ABounds       : array of Integer
    );
    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    function BeginObjectRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType
    ) : Integer;
    function BeginArrayRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType;
      const AItemName  : string
    ):Integer;
    function GetScopeItemNames(
      const AItemStyle : TSerializationStyle;
      const AReturnList : TStrings
    ) : Integer;
    procedure EndScopeRead();

    procedure Put(
      const AName     : string;
      const ATypeInfo : ISDOType;
      const AData
    );overload;
    procedure Put(
      const ANameSpace,
            AName     : string;
      const ATypeInfo : ISDOType;
      const AData
    );overload;
    procedure PutScopeInnerValue(
      const ATypeInfo : ISDOType;
      const AData
    );
    function Get(
      const ATypeInfo : ISDOType;
      var   AName     : string;
      var   AData
    ) : Boolean;overload;
    function Get(
      const ANameSpace : string;
      const ATypeInfo  : ISDOType;
      var   AName      : string;
      var   AData
    ) : Boolean;overload;
    function GetScopeInnerValue(
      const ATypeInfo : ISDOType;
      var   AData
    ) : Boolean;
    function ReadBuffer(const AName : string) : string;
    //Please use this method if and _only_ if you do not have another way achieve your aim!
    procedure WriteBuffer(const AValue : string);

    procedure SaveToStream(AStream : TStream);overload;
    procedure SaveToFile(const AFileName : string);overload;
    procedure LoadFromStream(AStream : TStream);overload;
    procedure LoadFromFile(const AFileName : string);overload;

    function GetBookMark() : TStreamBookmark;
    function GotoBookmark(const AValue : TStreamBookmark) : Boolean;
    
    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  public
    constructor Create();
    destructor Destroy();override;
    property PreferedShortNames : TStrings read FPreferedShortNames;
  end;

resourcestring
  SMSG_ExpectingValidArrayStyle = 'Expecting valid array style.';

implementation

uses
  sdo_imp_utils;

function ExtractNameSpaceShortName(const ANameSpaceDeclaration : string):string;
var
  i : integer;
begin
  i := AnsiPos(s_xml_ns,ANameSpaceDeclaration);
  if ( i > 0 ) then begin
    Result := Copy(ANameSpaceDeclaration, (i + Length(s_xml_ns) + 1 ), MaxInt );
  end else begin
    Result := '';
  end;
end;

{ TStackItem }

function TStackItem.GetItemsCount(const AStyle : TSerializationStyle): Integer;
begin
  if ( AStyle = ssAttibuteSerialization ) then
    Result := GetNodeListCount(ScopeObject.Attributes)
  else
    Result := GetNodeItemsCount(ScopeObject);
end;

constructor TStackItem.Create(AScopeObject: TDOMNode; AScopeType: TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

procedure TStackItem.SetNameSpace(const ANameSpace: string);
begin
  FNameSpace := ANameSpace;
end;

function TStackItem.GetScopeItemNames(
  const AItemStyle : TSerializationStyle;
  const AReturnList: TStrings
): Integer;
var
  c, i : PtrInt;
begin
  AReturnList.Clear();
  if ( AItemStyle = ssNodeSerialization ) then begin
    c := GetItemsCount(ssNodeSerialization);
    for i := 0 to Pred(c) do begin
      AReturnList.Add(ScopeObject.childNodes.Item[i].NodeName);
    end;
  end else begin
    c := GetNodeListCount(ScopeObject.Attributes);
    for i := 0 to Pred(c) do begin
      AReturnList.Add(ScopeObject.Attributes.Item[i].nodeName);
    end;
  end;
  Result := AReturnList.Count;
end;

procedure TStackItem.Error(const AMsg: string);
begin
  raise ESDOSerializationException.Create(AMsg);
end;

procedure TStackItem.Error(const AMsg: string; const AArgs: array of const);
begin
  raise ESDOSerializationException.CreateFmt(AMsg,AArgs);
end;

function TStackItem.BeginEmbeddedScope(): Integer;
begin
  Inc(FEmbeddedScopeCount);
  Result := FEmbeddedScopeCount;
end;

function TStackItem.EndEmbeddedScope(): Integer;
begin
  if ( FEmbeddedScopeCount < 1 ) then begin
    raise ESDOSerializationException.Create('Invalid operation on scope, their are no embedded scope.');
  end;
  Dec(FEmbeddedScopeCount);
  Result := FEmbeddedScopeCount;
end;


procedure TStackItem.CopyTo(const AClone: TStackItem);
begin
  AClone.FEmbeddedScopeCount := Self.FEmbeddedScopeCount;
  AClone.FNameSpace := Self.FNameSpace;
  AClone.FScopeObject := Self.FScopeObject;
  AClone.FScopeType := Self.FScopeType;
end;

constructor TStackItem.Create();
begin

end;

function TStackItem.Clone() : TStackItem;
begin

  Result := TStackItemClass(Self.ClassType).Create();
  try
    Self.CopyTo(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TObjectStackItem }

function TObjectStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
{$IFNDEF FPC}
  Result := sdo_win_xml.FindNode(ScopeObject,ANodeName);
{$ELSE}
  Result := ScopeObject.FindNode(ANodeName);
{$ENDIF}
end;

{ TAbstractArrayStackItem }

procedure TAbstractArrayStackItem.EnsureListCreated();
begin
  if (FItemList = nil) then
    FItemList := CreateList(FItemName);
end;

function TAbstractArrayStackItem.GetItemsCount(const AStyle : TSerializationStyle): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

constructor TAbstractArrayStackItem.Create(
        AScopeObject : TDOMNode;
  const AScopeType   : TScopeType;
  const AItemName    : string
);
begin
  inherited Create(AScopeObject,AScopeType);
  FItemName := AItemName;
end;

destructor TAbstractArrayStackItem.Destroy();
begin
  if (FItemList <> nil) then
    ReleaseDomNode(FItemList);
  inherited Destroy();
end;

function TAbstractArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    Error('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  Inc(FIndex);
  ANodeName := Result.NodeName;
end;

procedure TAbstractArrayStackItem.CopyTo(const AClone: TStackItem);
var
  locClone : TAbstractArrayStackItem;
begin
  inherited;
  locClone := AClone as TAbstractArrayStackItem;
  locClone.FIndex := Self.FIndex;
  locClone.FItemList := Self.FItemList;
  locClone.FItemName := Self.FItemName;
end;

{ TScopedArrayStackItem }

destructor TScopedArrayStackItem.Destroy();  
begin
  if ( FItemList <> nil ) then
    FItemList := nil;
  inherited Destroy();  
end;

function TScopedArrayStackItem.CreateList(const ANodeName : string): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := ScopeObject.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

{ TEmbeddedArrayStackItem }

function TEmbeddedArrayStackItem.CreateList(const ANodeName: string): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := FilterList(ScopeObject,ANodeName);
  end else begin
    Result := nil;
  end;
end;

{ TSDOSerializerStreamXML }

{procedure TSDOSerializerStreamXML.AddScopeAttribute(const AName, AValue: string);
begin
  CheckScope();
  GetCurrentScopeObject().SetAttribute(AName,AValue);
end;}

procedure TSDOSerializerStreamXML.BeginArray(
  const AName         : string;
  const AItemTypeInfo : ISDOType;
  const ABounds       : array of Integer
);
var
  typData : ISDOType;
  i,j, k : Integer;
  ns : string;
begin
  if ( Length(ABounds) < 2 ) then begin
    Error('Invalid array bounds.');
  end;
  i := ABounds[0];
  j := ABounds[1];
  k := j - i + 1;
  typData := AItemTypeInfo;
  if not Assigned(typData) then begin
    Error('Typeinfo not provided.');
  end;

  if HasScope() then
    ns := StackTop.NameSpace
  else
    ns := '';// raise Exception ????
  BeginScope(AName,'',stArray);
  StackTop().SetNameSpace(ns);
end;

function TSDOSerializerStreamXML.BeginArrayRead(
  var   AScopeName : string;
  const ATypeInfo  : ISDOType;
  const AItemName  : string
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stArray,AItemName);
end;

procedure TSDOSerializerStreamXML.BeginObject(const AName: string; const ATypeInfo: ISDOType);
var
  typData : ISDOType;
  nmspc, nmspcSH : string;
  mustAddAtt : Boolean;
  strNodeName : string;
begin
  typData := ATypeInfo;
  if not Assigned(typData) then
    Error('Typeinfo not provided.');
  mustAddAtt := False;
  nmspc := typData.getURI();
  if ( FNameStyle = nsQualified ) then begin
    if IsStrEmpty(nmspc) then
      nmspcSH := 'tns'
    else begin
      nmspcSH := FindAttributeByValueInScope(nmspc);
      if IsStrEmpty(nmspcSH) then begin
        nmspcSH := PreferedShortNames.Values[nmspc];
        if IsStrEmpty(nmspcSH) then
          nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
        if HasScope() then
          FDoc.DocumentElement.SetAttribute('xmlns:'+nmspcSH, nmspc)
        else begin
          mustAddAtt := True;
        end;
      end else begin
        nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
      end;
    end;

    if not IsStrEmpty(nmspcSH) then begin
      strNodeName := nmspcSH + ':' + AName;
    end else begin
      strNodeName := AName;
    end;
  end else begin
    strNodeName := AName;
  end;

  BeginScope(strNodeName,'',stObject);
  if mustAddAtt then
    FDoc.DocumentElement.SetAttribute('xmlns:'+nmspcSH, nmspc);
  StackTop().SetNameSpace(nmspc);
end;

function TSDOSerializerStreamXML.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : ISDOType
) : Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stObject,'');
end;

procedure TSDOSerializerStreamXML.BeginScope(
  const AScopeName,
        ANameSpace  : string;
  const AScopeType  : TScopeType;
  const AStyle      : TArrayStyle
);
var
  nsStr, scpStr : string;
  e : TDOMElement;
  hasNmspc, addAtt : Boolean;
begin
  if ( AScopeType = stObject ) or
     ( ( AScopeType = stArray ) and ( AStyle = asScoped ) )
  then begin
    addAtt := False;
    scpStr := AScopeName;
    hasNmspc := not IsStrEmpty(ANameSpace);
    if hasNmspc then begin
      nsStr := FindAttributeByValueInScope(ANameSpace);
      addAtt := IsStrEmpty(nsStr);
      if addAtt then begin
        nsStr := PreferedShortNames.Values[ANameSpace];
        if IsStrEmpty(nsStr) then
          nsStr := 'ns' + IntToStr(NextNameSpaceCounter());
      end else begin
        nsStr := Copy(nsStr,Succ(AnsiPos(':',nsStr)),MaxInt);
      end;
      scpStr := nsStr + ':' + scpStr;
    end;

    e := FDoc.CreateElement(scpStr);
    if HasScope() then
      GetCurrentScopeObject().AppendChild(e)
    else
      FDoc.AppendChild(e);
    if ( AScopeType = stObject ) then begin
      PushStack(e);
    end else begin
      PushStack(e,'');
    end;
    if hasNmspc and addAtt then begin
      e.SetAttribute('xmlns:'+nsStr,ANameSpace);
      StackTop().SetNameSpace(ANameSpace);
    end;
  end else if ( ( AScopeType = stArray ) and ( AStyle = asEmbeded ) ) then begin
    StackTop().BeginEmbeddedScope();
  end;
end;

procedure TSDOSerializerStreamXML.CheckScope();
begin
  if not HasScope() then
    Error('There is no scope.');
end;

procedure TSDOSerializerStreamXML.Clear();
begin
  InternalClear(True);
end;

procedure TSDOSerializerStreamXML.ClearStack();
var
  i, c : Integer;
begin
  c := FStack.Count;
  for I := 1 to c do
    FStack.Pop().Free();
end;

constructor TSDOSerializerStreamXML.Create();
var
  ls : TStringList;
begin
  Inherited Create();
  FStack := TObjectStackEx.Create();
  ls := TStringList.Create();
  FPreferedShortNames := ls;
  ls.Duplicates := dupIgnore;
  ls.Values[sdo_namespace] := 'sdo';
  ls.Values[s_xsi_namespace] := 'xsi';
  ls.Values[s_xsd_namespace] := 'xdi';
  ls.Sorted := True;
  FDoc := CreateDoc();
end;

destructor TSDOSerializerStreamXML.Destroy();
begin
  ReleaseDomNode(FDoc);
  ClearStack();
  FStack.Free();
  FPreferedShortNames.Free();
  inherited Destroy();
end;

procedure TSDOSerializerStreamXML.EndScope();
begin
  CheckScope();
  if ( StackTop().EmbeddedScopeCount = 0 ) then begin
    FStack.Pop().Free();
  end else begin
    StackTop().EndEmbeddedScope();
  end;
end;

procedure TSDOSerializerStreamXML.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TSDOSerializerStreamXML.Error(const AMsg: string; const AArgs: array of const);
begin
  raise ESDOSerializationException.CreateFmt(AMsg,AArgs);
end;

procedure TSDOSerializerStreamXML.Error(const AMsg: string);
begin
  raise ESDOSerializationException.Create(AMsg);
end;

function TSDOSerializerStreamXML.FindAttributeByValueInNode(
  const AAttValue : string;
  const ANode     : TDOMNode;
  out   AResAtt   : string
) : Boolean;
var
  i,c : Integer;
begin
  AResAtt := '';
  if Assigned(ANode) and
     Assigned(ANode.Attributes) and
     ( ANode.Attributes.Length > 0 )
  then begin
    c := Pred(ANode.Attributes.Length);
    for i := 0 to c do begin
      if AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) then begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TSDOSerializerStreamXML.FindAttributeByValueInScope(const AAttValue : string) : string;
var
  tmpNode : TDOMNode;
begin
  if HasScope() then begin
    tmpNode := GetCurrentScopeObject();
    while Assigned(tmpNode) do begin
      if FindAttributeByValueInNode(AAttValue,tmpNode,Result) then
        Exit;
      tmpNode := tmpNode.ParentNode;
    end;
  end;
  Result := '';
end;

function TSDOSerializerStreamXML.FindNameSpace(
  const AShortName : string;
  out   ARes : string
) : Boolean;
var
  tmpNode, attNode : TDOMNode;
  attName : string;
begin
  Result := False;
  if HasScope() then begin
    attName := Format('%s:%s',[s_xml_ns,AShortName]);
    tmpNode := GetCurrentScopeObject();
    while Assigned(tmpNode) do begin
      if Assigned(tmpNode.Attributes) then begin
        attNode := tmpNode.Attributes.GetNamedItem(attName);
        if Assigned(attNode) then begin
          ARes := attNode.NodeValue;
          Result := True;
          Break;
        end;
      end;
      tmpNode := tmpNode.ParentNode;
    end;
  end;
end;

function TSDOSerializerStreamXML.Get(
  const ATypeInfo : ISDOType;
  var   AName     : string;
  var   AData
) : Boolean;
begin
  Result := Get('',ATypeInfo,AName,AData);
end;

function TSDOSerializerStreamXML.Get(
  const ANameSpace: string;
  const ATypeInfo: ISDOType;
  var AName: string;
  var AData
) : Boolean;
var
  valBuffer : TValueBuffer;
  strData : TSDOString;
  bytesData : TSDOBytes;
begin
  FillChar(valBuffer,SizeOf(valBuffer),#0);
  case ATypeInfo.getTypeEnum() of
    BooleanType :
      begin
        Result := GetBoolean(AName,valBuffer.BooleanValue,ANameSpace);
        if Result then
          TSDOBoolean(AData) := valBuffer.BooleanValue;
      end;
    ByteType :
      begin
        Result := GetByte(AName,valBuffer.ByteValue,ANameSpace);
        if Result then
          TSDOByte(AData) := valBuffer.ByteValue;
      end;
{$IFDEF HAS_SDO_BYTES}
    BytesType :
      begin
        Result := GetBytes(AName,bytesData,ANameSpace);
        if Result then
          TSDOBytes(AData) := bytesData;
      end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType :
      begin
        Result := GetChar(AName,valBuffer.CharValue,ANameSpace);
        if Result then
          TSDOChar(AData) := valBuffer.CharValue;
      end;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType :
      begin
        Result := GetCurrency(AName,valBuffer.CurrencyValue,ANameSpace);
        if Result then
          TSDOCurrency(AData) := valBuffer.CurrencyValue;
      end;
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType :
      begin
        Result := GetDate(AName,valBuffer.DateValue,ANameSpace);
        if Result then
          TSDODateTime(AData) := valBuffer.DateValue;
      end;
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType :
      begin
        Result := GetDouble(AName,valBuffer.DoubleValue,ANameSpace);
        if Result then
          TSDODouble(AData) := valBuffer.DoubleValue;
      end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType :
      begin
        Result := GetFloat(AName,valBuffer.FloatValue,ANameSpace);
        if Result then
          TSDOFloat(AData) := valBuffer.FloatValue;
      end;
{$ENDIF HAS_SDO_FLOAT}
    IntegerType :
      begin
        Result := GetInteger(AName,valBuffer.IntegerValue,ANameSpace);
        if Result then
          TSDOInteger(AData) := valBuffer.IntegerValue;
      end;
{$IFDEF HAS_SDO_LONG}
    LongType :
      begin
        Result := GetLong(AName,valBuffer.LongValue,ANameSpace);
        if Result then
          TSDOLong(AData) := valBuffer.LongValue;
      end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType :
      begin
        Result := GetShort(AName,valBuffer.ShortValue,ANameSpace);
        if Result then
          TSDOShort(AData) := valBuffer.ShortValue;
      end;
{$ENDIF HAS_SDO_SHORT}
    StringType  :
      begin
        strData := TSDOString(AData);
        Result := GetString(AName,strData,ANameSpace);
        if Result then
          TSDOString(AData) := strData;
      end;
    else
      Result := False;
  end;
end;

function CopyStackItem(const AItem : TObject) : TObject;
begin
  if ( AItem <> nil ) then
    Result := TStackItem(AItem).Clone()
  else
    Result := nil;
end;

function TSDOSerializerStreamXML.GetBookMark() : TStreamBookmark;
var
  locRes : TStreamXMLBookmark;
begin
  locRes := TStreamXMLBookmark.Create();
  try
    locRes.FNameStyle := Self.FNameStyle;
    locRes.FStack := Self.FStack.Clone({$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyStackItem);
    locRes.FDoc := Self.FDoc;
    locRes.FSerializationStyle := Self.FSerializationStyle;
    Result := locRes;
  except
    FreeAndNil(locRes);
    raise;
  end;
end;

function TSDOSerializerStreamXML.GetBoolean(var AName: string;var AData: TSDOBoolean; const ANameSpace : string) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace, AName,locBuffer);
  if Result then begin
    locBuffer := LowerCase(Trim(locBuffer));
    if IsStrEmpty(locBuffer) then
      AData := False
    else
      AData := TSDOConvertHelper.StringToBool(locBuffer);
  end;
end;

function TSDOSerializerStreamXML.GetCurrentScope() : string;
begin
  CheckScope();
  Result:= GetCurrentScopeObject().NodeName;
end;

function TSDOSerializerStreamXML.GetCurrentScopeObject() : TDOMElement;
begin
  Result := StackTop().ScopeObject As TDOMElement;
end;

function TSDOSerializerStreamXML.GetFormatName() : string;
begin
  Result := 'XML';
end;

function TSDOSerializerStreamXML.GetInteger(var AName: string;var AData: TSDOInteger; const ANameSpace : string) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToInteger(locBuffer);
end;

function TSDOSerializerStreamXML.GetNameStyle() : TNameStyle;
begin
  Result := FNameStyle;
end;

function TSDOSerializerStreamXML.GetNodeValue(const ANameSpace : string; var AName : string; out ARes : DOMString) : Boolean;
var
  locElt : TDOMNode;
  namespaceShortName, strNodeName, s, locNS : string;
begin
  strNodeName := AName;
  if ( FNameStyle = nsQualified ) then begin
    if IsStrEmpty(ANameSpace) then
      locNS := StackTop().NameSpace
    else
      locNS := ANameSpace;
    namespaceShortName := FindAttributeByValueInScope(locNS);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName) As TDOMElement;
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;

  if Assigned(locElt) then begin
    if locElt.HasChildNodes then
      ARes := locElt.FirstChild.NodeValue
    else
      ARes := locElt.NodeValue;
    Result := True;
  end else begin
    ARes := '';
    Result := False;
  end;
end;

function TSDOSerializerStreamXML.GetScopeInnerValue(
  const ATypeInfo: ISDOType;
  var AData
): Boolean;
var
  dataBuffer : string;
  nd : TDOMNode;
begin
  CheckScope();
  nd := StackTop().ScopeObject;
  if nd.HasChildNodes() then
    dataBuffer := nd.FirstChild.NodeValue
  else
    dataBuffer := StackTop().ScopeObject.NodeValue;
  case ATypeInfo.getTypeEnum() of
    BooleanType : TSDOBoolean(AData) := TSDOConvertHelper.StringToBool(dataBuffer);
    ByteType    : TSDOByte(AData) := TSDOConvertHelper.StringToByte(dataBuffer);
    IntegerType : TSDOInteger(AData) := TSDOConvertHelper.StringToInteger(dataBuffer);
    StringType  : TSDOString(AData) := dataBuffer;
    else
      Assert(False);
  end;
  Result := True;
end;

function TSDOSerializerStreamXML.GetScopeItemNames(
  const AItemStyle : TSerializationStyle;
  const AReturnList: TStrings
): Integer;
var
  i, c, j : PtrInt;
  buffer, localName, space, expandedName, s : string;
begin
  CheckScope();
  Result := StackTop().GetScopeItemNames(AItemStyle,AReturnList);
  c := AReturnList.Count;
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      buffer := AReturnList[i];
      j := AnsiPos(':',buffer);
      if ( j < 1 ) then begin
        expandedName := buffer;
      end else begin
        s := Copy(buffer,1,Pred(j));
        localName := Copy(buffer,Succ(j),Length(buffer));
        FindNameSpace(s,space);
        expandedName := Format('%s#%s',[space,localName]);
      end;
      AReturnList[i] := expandedName;
    end;
  end;
end;

function TSDOSerializerStreamXML.GetSerializationStyle() : TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

function TSDOSerializerStreamXML.GetString(var AName: string; var AData: TSDOString; const ANameSpace : string) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData := locBuffer;
end;

function TSDOSerializerStreamXML.GotoBookmark(const AValue: TStreamBookmark): Boolean;
var
  locBM : TStreamXMLBookmark;
begin
  Result := False;
  if ( AValue <> nil ) then begin
    locBM := AValue as TStreamXMLBookmark;
    if ( locBM.Doc = Self.FDoc ) then begin
      ClearStack();
      FreeAndNil(FStack);
      FStack := locBM.FStack.Clone({$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyStackItem);
      FSerializationStyle := locBM.SerializationStyle;
      FNameStyle := locBM.NameStyle;
      Result := True;
    end;
  end;
end;

function TSDOSerializerStreamXML.HasScope() : Boolean;
begin
  Result := FStack.AtLeast(1);
end;

function TSDOSerializerStreamXML.InternalBeginScopeRead(
  var AScopeName   : string;
  const ATypeInfo  : ISDOType;
  const AScopeType : TScopeType;
  const AItemName  : string;
  const AStyle     : TArrayStyle
) : Integer;
var
  locNode : TDOMNode;
  stk : TStackItem;
  typData : ISDOType;
  nmspc,nmspcSH : string;
  strNodeName : string;
begin
  typData := ATypeInfo;
  if not Assigned(typData) then begin
    Error('Typeinfo not provided.');
  end;
  nmspc := typData.getURI();
  if ( FNameStyle = nsQualified ) then begin
    if IsStrEmpty(nmspc) then begin
      nmspcSH := ''
    end else begin
      nmspcSH := FindAttributeByValueInScope(nmspc);
      if not IsStrEmpty(nmspcSH) then begin
        nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
      end;
    end;
    if IsStrEmpty(nmspcSH) then begin
      strNodeName := AScopeName
    end else begin
      if ( Pos(':',AScopeName) < 1 ) then begin
        strNodeName := nmspcSH + ':' + AScopeName
      end else begin
        strNodeName := AScopeName;
      end;
    end;
  end else begin
    strNodeName := AScopeName;
  end;

  stk := StackTop();
  if ( AScopeType = stObject ) or
     ( ( AScopeType = stArray ) and ( AStyle = asScoped ) )
  then begin
    locNode := stk.FindNode(strNodeName);
  end else begin
    locNode := stk.ScopeObject;
  end;
  if ( locNode = nil ) then begin
    Result := -1;
  end else begin
    if ( AScopeType = stObject ) then begin
      PushStack(locNode);
    end else begin
      PushStack(locNode,AItemName);
    end;
    StackTop().SetNameSpace(nmspc);
    Result := StackTop().GetItemsCount(GetSerializationStyle());
  end;
end;

procedure TSDOSerializerStreamXML.InternalClear(const ACreateDoc: Boolean);
begin
  ClearStack();
  FNameSpaceCounter := 0;
  FNameStyle := nsUnqualified;
  FSerializationStyle := ssNodeSerialization;
  ReleaseDomNode(FDoc);
  FDoc := nil;
  if ACreateDoc then
    FDoc := CreateDoc();
end;

function TSDOSerializerStreamXML.InternalPutData(
  const AName : string;
  const AData : TSDOString; 
  const ANameSpace : string
): TDOMNode;
var
  namespaceShortName, strNodeName, s, locNS : TSDOString;
begin
  strNodeName := AName;
  if ( FNameStyle = nsQualified ) then begin
    if IsStrEmpty(ANameSpace) then
      locNS := StackTop().NameSpace
    else
      locNS := ANameSpace;
    namespaceShortName := FindAttributeByValueInScope(locNS);
    if not IsStrEmpty(namespaceShortName) then begin
      s := ExtractNameSpaceShortName(namespaceShortName);
      if not IsStrEmpty(s) then
        strNodeName := s + ':' + strNodeName;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    Result := FDoc.CreateElement(strNodeName);
    Result.AppendChild(FDoc.CreateTextNode(AData));
    GetCurrentScopeObject().AppendChild(Result);
  end else begin
    Result := GetCurrentScopeObject();
    (Result as TDOMElement).SetAttribute(strNodeName,AData);
  end;
end;

procedure TSDOSerializerStreamXML.Initialize();
begin
  ClearStack();
  if ( FDoc <> nil ) and ( FDoc.DocumentElement <> nil ) then
    PushStack(FDoc.DocumentElement);
end;

function TSDOSerializerStreamXML.IsCurrentScopeNil() : Boolean;
var
  s,nsShortName,nilName : String;
begin
  CheckScope();
  nsShortName := FindAttributeByValueInScope(s_xsi_namespace);
  Result := False;
  if IsStrEmpty(nsShortName) then begin
    nilName := s_nil;
  end else begin
    nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
    if not IsStrEmpty(nsShortName) Then
      nsShortName := nsShortName + ':';
    nilName := nsShortName + s_nil;
  end;
  s := Trim(GetCurrentScopeObject().GetAttribute(nilName));
  if ( Length(s) > 0 ) and ( AnsiSameText(s,'true') or AnsiSameText(s,'"true"') ) then begin
    Result := True;
  end;
end;

procedure TSDOSerializerStreamXML.LoadFromFile(const AFileName: string);
var
  stream : TFileStream;
begin
  if not FileExists(AFileName) then
    raise ESDOSerializationException.CreateFmt('File not found : "%s".',[AFileName]);
  stream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream);
  finally
    stream.Free();
  end;
end;

procedure TSDOSerializerStreamXML.LoadFromStream(AStream: TStream);
var
  nd : TDOMNode;
begin
  InternalClear(False);
  ReadXMLFile(FDoc,AStream);
  nd := FDoc.DocumentElement;
  if Assigned(nd) then
    PushStack(nd);
end;

function TSDOSerializerStreamXML.NextNameSpaceCounter() : Integer;
begin
  Inc(FNameSpaceCounter);
  Result := FNameSpaceCounter;
end;

procedure TSDOSerializerStreamXML.NilCurrentScope();
var
  nmspcSH : string;
begin
  CheckScope();
  nmspcSH := FindAttributeByValueInScope(s_xsi_namespace);
  if IsStrEmpty(nmspcSH) then begin
    nmspcSH := PreferedShortNames.Values[s_xsi_namespace];
    if IsStrEmpty(nmspcSH) then begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
    end;
    FDoc.DocumentElement.SetAttribute('xmlns:'+nmspcSH, s_xsi_namespace);
  end else begin
    nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
  end;
  GetCurrentScopeObject().SetAttribute(nmspcSH + ':' + s_nil,'true');
end;

function TSDOSerializerStreamXML.PopStack() : TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

function TSDOSerializerStreamXML.PushStack(AScopeObject: TDOMNode): TStackItem;
begin
  Result := FStack.Push(TObjectStackItem.Create(AScopeObject,stObject)) as TStackItem;
end;

function TSDOSerializerStreamXML.PushStack(
  const AScopeObject : TDOMNode;
  const AItemName    : string;
  const AStyle       : TArrayStyle
): TStackItem;
begin
  case AStyle of
    asScoped  : Result := FStack.Push(TScopedArrayStackItem.Create(AScopeObject,stArray,AItemName)) as TStackItem;
    asEmbeded : Result := FStack.Push(TEmbeddedArrayStackItem.Create(AScopeObject,stArray,AItemName)) as TStackItem;
    else
      raise ESDOSerializationException.Create(SMSG_ExpectingValidArrayStyle);
  end;
end;

procedure TSDOSerializerStreamXML.Put(
  const AName     : string;
  const ATypeInfo : ISDOType;
  const AData
);
begin
  Put('',AName,ATypeInfo,AData);
end;

procedure TSDOSerializerStreamXML.Put(
  const ANameSpace,
        AName     : string;
  const ATypeInfo : ISDOType;
  const AData
);
var
  valBuffer : TValueBuffer;
  strData : TSDOString;
  bytesData : TSDOBytes;
begin
  case ATypeInfo.getTypeEnum() Of
    BooleanType :
      begin
        valBuffer.BooleanValue := TSDOBoolean(AData);
        PutBoolean(AName,valBuffer.BooleanValue,ANameSpace);
      end;
    ByteType :
      begin
        valBuffer.ByteValue := TSDOByte(AData);
        PutByte(AName,valBuffer.ByteValue,ANameSpace);
      end;
{$IFDEF HAS_SDO_BYTES}
     BytesType :
      begin
        bytesData := TSDOBytes(AData);
        PutBytes(AName,bytesData,ANameSpace);
      end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType :
      begin
        valBuffer.CharValue := TSDOChar(AData);
        PutChar(AName,valBuffer.CharValue,ANameSpace);
      end;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType :
      begin
        valBuffer.CurrencyValue := TSDOCurrency(AData);
        PutCurrency(AName,valBuffer.CurrencyValue,ANameSpace);
      end;
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType :
      begin
        valBuffer.DateValue := TSDODateTime(AData);
        PutDate(AName,valBuffer.DateValue,ANameSpace);
      end;
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType :
      begin
        valBuffer.DoubleValue := TSDODouble(AData);
        PutDouble(AName,valBuffer.DoubleValue,ANameSpace);
      end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType :
      begin
        valBuffer.FloatValue := TSDOFloat(AData);
        PutFloat(AName,valBuffer.FloatValue,ANameSpace);
      end;
{$ENDIF HAS_SDO_FLOAT}
    IntegerType :
      begin
        valBuffer.IntegerValue := TSDOInteger(AData);
        PutInteger(AName,valBuffer.IntegerValue,ANameSpace);
      end;
{$IFDEF HAS_SDO_LONG}
    LongType :
      begin
        valBuffer.LongValue := TSDOLong(AData);
        PutLong(AName,valBuffer.LongValue,ANameSpace);
      end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType :
      begin
        valBuffer.ShortValue := TSDOShort(AData);
        PutShort(AName,valBuffer.ShortValue,ANameSpace);
      end;
{$ENDIF HAS_SDO_SHORT}
    StringType  :
      begin
        strData := TSDOString(AData);
        PutString(AName,strData,ANameSpace);
      end;
    else
      Assert(False);
  end;
end;

function TSDOSerializerStreamXML.PutBoolean(const AName: string; const AData: TSDOBoolean; const ANameSpace : string): TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.BoolToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.PutInteger(const AName: string;const AData: TSDOInteger; const ANameSpace : string): TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.IntegerToString(AData),ANameSpace);
end;

procedure TSDOSerializerStreamXML.PutScopeInnerValue(
  const ATypeInfo: ISDOType;
  const AData
);
var
  dataBuffer : string;
begin
  case ATypeInfo.getTypeEnum() Of
    BooleanType : dataBuffer := TSDOConvertHelper.BoolToString(TSDOBoolean(AData));
{$IFDEF HAS_SDO_BYTES}
    BytesType   : dataBuffer := TSDOConvertHelper.BytesToString(TSDOBytes(AData));
{$ENDIF HAS_SDO_BYTES}
    ByteType    : dataBuffer := TSDOConvertHelper.ByteToString(TSDOByte(AData));
{$IFDEF HAS_SDO_CHAR}
    CharacterType : dataBuffer := TSDOChar(AData);
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType : dataBuffer := TSDOConvertHelper.CurrencyToString(TSDOCurrency(AData));
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType: dataBuffer := TSDOConvertHelper.DateToString(TSDODateTime(AData));
    IntegerType : dataBuffer := TSDOConvertHelper.IntegerToString(TSDOInteger(AData));
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType  : dataBuffer := TSDOConvertHelper.FloatToString(TSDODouble(AData));
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType   : dataBuffer := TSDOConvertHelper.FloatToString(TSDOFloat(AData));
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    LongType    : dataBuffer := TSDOConvertHelper.LongToString(TSDOLong(AData));
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType   : dataBuffer := TSDOConvertHelper.ShortToString(TSDOShort(AData));
{$ENDIF HAS_SDO_SHORT}
    StringType  : dataBuffer := TSDOString(AData);
    else
      Assert(False);
  end;
  StackTop().ScopeObject.AppendChild(FDoc.CreateTextNode(dataBuffer));
end;

function TSDOSerializerStreamXML.PutString(const AName: string;const AData: TSDOString; const ANameSpace : string): TDOMNode;
begin
  Result := InternalPutData(AName,AData,ANameSpace);
end;

function TSDOSerializerStreamXML.ReadBuffer(const AName: string): string;
var
  locElt : TDOMNode;
  namespaceShortName, strNodeName, s : string;
begin
  strNodeName := AName;
  namespaceShortName := FindAttributeByValueInScope(StackTop().NameSpace);
  namespaceShortName := Copy(namespaceShortName,AnsiPos(':',namespaceShortName) + 1,MaxInt);
  if not IsStrEmpty(namespaceShortName) then begin
    s := ExtractNameSpaceShortName(namespaceShortName);
    if not IsStrEmpty(s) then
      strNodeName := s + ':' + strNodeName;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName);
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;
  if ( locElt = nil ) and ( not AnsiSameStr(AName,strNodeName) ) then begin
    strNodeName := AName;
    if ( FSerializationStyle = ssNodeSerialization ) then begin
      locElt := StackTop().FindNode(strNodeName);
    end else begin
      locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
    end;
  end;

  if Assigned(locElt) then
    Result := NodeToBuffer(locElt)
  else
    Result := '';
end;

procedure TSDOSerializerStreamXML.SaveToFile(const AFileName: string);
var
  stream : TFileStream;
begin
  stream := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.Free();
  end;
end;

procedure TSDOSerializerStreamXML.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc,AStream);
end;

procedure TSDOSerializerStreamXML.SetNameStyle(const AValue: TNameStyle);
begin
  FNameStyle := AValue;
end;

procedure TSDOSerializerStreamXML.SetSerializationStyle(const ASerializationStyle: TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TSDOSerializerStreamXML.StackTop() : TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

procedure TSDOSerializerStreamXML.WriteBuffer(const AValue: string);
var
  strm : TStringStream;
  locDoc : TSDOXMLDocument;
  locNode : TDOMNode;
begin
  CheckScope();
  locDoc := nil;
  strm := TStringStream.Create(AValue);
  try
    ReadXMLFile(locDoc,strm);
    locNode := locDoc.DocumentElement.CloneNode(True {$IFDEF FPC}, StackTop().ScopeObject.OwnerDocument{$ENDIF});
    StackTop().ScopeObject.AppendChild(locNode);
  finally
    ReleaseDomNode(locDoc);
    strm.Free();
  end;
end;


{ TStreamXMLBookmark }

destructor TStreamXMLBookmark.Destroy();
var
  i : PtrInt;
begin
  if ( FStack <> nil ) and ( FStack.Count > 0 ) then begin
    for i := 0 to Pred(FStack.Count) do
      FStack.Pop().Free();
  end;
  FreeAndNil(FStack);
  inherited;
end;

function TSDOSerializerStreamXML.PutByte(
  const AName: string;
  const AData: TSDOByte;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.ByteToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetByte(
  var AName: string;
  var AData: TSDOByte;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToByte(locBuffer);
end;

{$IFDEF HAS_SDO_BYTES}
function TSDOSerializerStreamXML.PutBytes(
  const AName: string;
  const AData: TSDOBytes;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.BytesToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetBytes(
  var AName: string;
  var AData: TSDOBytes;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToBytes(locBuffer);
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
function TSDOSerializerStreamXML.PutChar(
  const AName: string;
  const AData: TSDOChar;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,AData,ANameSpace);
end;

function TSDOSerializerStreamXML.GetChar(
  var AName: string;
  var AData: TSDOChar;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToChar(locBuffer);
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
function TSDOSerializerStreamXML.PutCurrency(
  const AName: string;
  const AData: TSDOCurrency;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.CurrencyToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetCurrency(
  var AName: string;
  var AData: TSDOCurrency;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToCurrency(locBuffer);
end;
{$ENDIF HAS_SDO_CURRENCY}

function TSDOSerializerStreamXML.PutDate(
  const AName: string;
  const AData: TSDODateTime;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.DateToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetDate(
  var AName: string;
  var AData: TSDODateTime;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToDate(locBuffer);
end;

{$IFDEF HAS_SDO_DOUBLE}
function TSDOSerializerStreamXML.PutDouble(
  const AName: string;
  const AData: TSDODouble;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.FloatToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetDouble(
  var AName: string;
  var AData: TSDODouble;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToFloat(locBuffer);
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
function TSDOSerializerStreamXML.PutFloat(
  const AName: string;
  const AData: TSDOFloat;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.FloatToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetFloat(
  var AName: string;
  var AData: TSDOFloat;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToFloat(locBuffer);
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
function TSDOSerializerStreamXML.PutLong(
  const AName: string;
  const AData: TSDOLong;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.LongToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetLong(
  var AName: string;
  var AData: TSDOLong;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToLong(locBuffer);
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
function TSDOSerializerStreamXML.PutShort(
  const AName: string;
  const AData: TSDOShort;
  const ANameSpace: string
) : TDOMNode;
begin
  Result := InternalPutData(AName,TSDOConvertHelper.ShortToString(AData),ANameSpace);
end;

function TSDOSerializerStreamXML.GetShort(
  var AName: string;
  var AData: TSDOShort;
  const ANameSpace: string
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result and ( locBuffer <> '' ) then
    AData := TSDOConvertHelper.StringToShort(locBuffer);
end;
{$ENDIF HAS_SDO_SHORT}

end.
