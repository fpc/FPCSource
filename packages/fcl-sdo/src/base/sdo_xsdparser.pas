{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements an XSD parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_xsdparser;

interface
uses
  Classes, SysUtils, Contnrs,
{$IFDEF DELPHI}
  xmldom, sdo_win_xml,
{$ENDIF DELPHI}
{$IFDEF FPC}
  DOM, sdo_fpc_xml,
{$ENDIF FPC}
  sdo_cursor_intf, sdo_rtti_filters, sdo_logger_intf,
  sdo_types, sdo;

type

  EXsdParserException = class(Exception)
  end;

  EXsdParserAssertException = class(EXsdParserException)
  end;

  EXsdTypeNotFoundException = class(EXsdParserException)
  end;

  EXsdInvalidDefinitionException = class(EXsdParserException)
  end;

  EXsdInvalidTypeDefinitionException = class(EXsdInvalidDefinitionException)
  end;

  EXsdInvalidElementDefinitionException = class(EXsdInvalidDefinitionException)
  end;

  TOnParserMessage = procedure (const AMsgType : TMessageType; const AMsg : string) of object;


  TNameSpaceValueType = ( nvtExpandValue, nvtShortSynonym );
  TSearchSpace = ( ssCurrentModule, ssGlobal );

  IDocumentLocator = interface
    ['{D364A50B-64B1-461C-ADDE-B5918CAB0FE8}']
    function Find(
      const ADocLocation : string;
      out   ADoc : TXMLDocument
    ) : Boolean;
  end;

  IParserContext = interface
    ['{3E924ECE-A9B9-4FBB-BC12-4E728B7E34C5}']
    function GetXsShortNames() : TStrings;
    function GetSymbolTable() : ISDODataObject;
    function FindNameSpace(const AShortName : string; out AResult : string) : Boolean;
    function FindShortNamesForNameSpace(const ANameSpace : string) : TStrings;
    function GetTargetNameSpace() : string;
    function GetTargetModule() : ISDODataObject;
    function GetDocumentLocator() : IDocumentLocator;
    procedure SetDocumentLocator(const ALocator : IDocumentLocator);
  end;

  IXsdParser = interface
    ['{F0CEC726-A068-4CCC-B1E7-D31F018415B2}']
    function FindParser(const ANamespace : string) : IXsdParser;
    function ParseType(
      const AName,
            ATypeKind : string { ATypeKind "ComplexType", "SimpleType", "Element" }
    ) : ISDODataObject; overload;
    function ParseType(
      const AName     : string;
      const ATypeNode : TDOMNode
    ) : ISDODataObject; overload;
    procedure ParseTypes();
    procedure SetNotifier(ANotifier : TOnParserMessage);
  end;

  TAbstractTypeParserClass = class of TAbstractTypeParser;

  { TAbstractTypeParser }

  TAbstractTypeParser = class
  private
    FContext : IParserContext;
    FTypeNode : TDOMNode;
    FSymbols : ISDODataObject;
    FTypeName : string;
    FEmbededDef : Boolean;
  private
    function GetModule: ISDODataObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function FindElementNS(
      const ANameSpace,
            ALocalName : string;
      const ASpaceType : TNameSpaceValueType
    ) : ISDODataObject;
    function FindElementWithHint(const AName, AHint : string; const ASpace : TSearchSpace) : ISDODataObject;
    function ExtractTypeHint(AElement : TDOMNode) : string;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetAsEmbeddedType(AType : ISDODataObject; const AValue : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsEmbeddedType(AType : ISDODataObject) : Boolean;
    procedure AddUnresolvedLink(
            AElement,
            ATarget  : ISDODataObject;
      const ALinkKind,
            AName    : string
    );
{$IFDEF SDO_HANDLE_DOC}
    procedure ParseDocumentation(AType : ISDODataObject);
{$ENDIF SDO_HANDLE_DOC}
  public
    constructor Create(
            AOwner       : IParserContext;
            ATypeNode    : TDOMNode;
      const ATypeName    : string;
      const AEmbededDef  : Boolean
    );
    class function ExtractEmbeddedTypeFromElement(
            AOwner       : IParserContext;
            AEltNode     : TDOMNode;
            ASymbols     : ISDODataObject;
      const ATypeName    : string
    ) : ISDODataObject;
    class function GetParserSupportedStyle():string;virtual;abstract;
    class procedure RegisterParser(AParserClass : TAbstractTypeParserClass);
    class function GetRegisteredParserCount() : Integer;
    class function GetRegisteredParser(const AIndex : Integer):TAbstractTypeParserClass;
    function Parse():ISDODataObject;virtual;abstract;
    property Module : ISDODataObject read GetModule;
    property Context : IParserContext read FContext;
  end;

  TDerivationMode = ( dmNone, dmExtension, dmRestriction );
  TSequenceType = ( stElement, stAll );
  TParserTypeHint = ( pthDeriveFromSoapArray );
  TParserTypeHints = set of TParserTypeHint;

  { TComplexTypeParser }

  TComplexTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FContentNode : TDOMNode;
    FContentType : string;
    FBaseType : ISDODataObject;
    FDerivationMode : TDerivationMode;
    FDerivationNode : TDOMNode;
    FSequenceType : TSequenceType;
    FHints : TParserTypeHints;
  private
    //helper routines
    function ExtractElementCursor(
      out AAttCursor : IObjectCursor;
      out AAnyNode, AAnyAttNode : TDOMNode
    ):IObjectCursor;
    procedure ExtractExtendedMetadata(const AItem : ISDODataObject; const ANode : TDOMNode);
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    procedure ExtractContentType();
    procedure ExtractBaseType();
    function ParseSimpleContent(const ATypeName : string) : ISDODataObject;
    function ParseEmptyContent(const ATypeName : string):ISDODataObject;
    function ParseComplexContent(const ATypeName : string):ISDODataObject;virtual;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():ISDODataObject;override;
  end;

  { TSimpleTypeParser }

  TSimpleTypeParser = class(TAbstractTypeParser)
  private
    FAttCursor : IObjectCursor;
    FChildCursor : IObjectCursor;
    FBaseName : string;
    FBaseNameSpace : string;
    FRestrictionNode : TDOMNode;
    FIsEnum : Boolean;
  private
    procedure CreateNodeCursors();
    procedure ExtractTypeName();
    function ExtractContentType() : Boolean;
    function ParseEnumContent():ISDODataObject;
    function ParseOtherContent():ISDODataObject;
  public
    class function GetParserSupportedStyle():string;override;
    function Parse():ISDODataObject;override;
  end;

  { TCustomXsdSchemaParser }

  TCustomXsdSchemaParser = class(TInterfacedObject, IInterface, IParserContext, IXsdParser)
  private
    FDoc : TXMLDocument;
    FParentContext : Pointer;//IParserContext;
    FSymbols : ISDODataObject;
    FModuleName : string;
    FModule : ISDODataObject;
    FTargetNameSpace : string;
    FSchemaNode : TDOMNode;
  private
    FNameSpaceList : TStringList;
    FXSShortNames : TStrings;
    FChildCursor : IObjectCursor;
    FOnMessage: TOnParserMessage;
    FDocumentLocator : IDocumentLocator;
    FImportParsed : Boolean;
    FXsdParsers : TStringList;
  private
    procedure DoOnMessage(const AMsgType : TMessageType; const AMsg : string);
  private
    function FindNamedNode(AList : IObjectCursor; const AName : WideString; const AOrder : Integer = 0):TDOMNode;
    function GetParentContext() : IParserContext;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Prepare();
    function FindElementNS(
      const ANameSpace,
            ALocalName : string;
      const ASpaceType : TNameSpaceValueType
    ) : ISDODataObject;
  protected
    function GetXsShortNames() : TStrings;
    function GetSymbolTable() : ISDODataObject;
    function FindNameSpace(const AShortName : string; out AResult : string) : Boolean;
    function FindShortNamesForNameSpaceLocal(const ANameSpace : string) : TStrings;
    function FindShortNamesForNameSpace(const ANameSpace : string) : TStrings;
    function GetDocumentLocator() : IDocumentLocator;
    procedure SetDocumentLocator(const ALocator : IDocumentLocator);

    procedure SetNotifier(ANotifier : TOnParserMessage);
    function InternalParseType(
      const AName : string;
      const ATypeNode : TDOMNode
    ) : ISDODataObject;
    procedure CreateImportParsers();
    procedure ParseImportDocuments(); virtual;
    procedure HandleUnresolvedLinks();
  public
    constructor Create(
      ADoc           : TXMLDocument;
      ASchemaNode    : TDOMNode;
      ASymbols       : ISDODataObject;
      AParentContext : IParserContext
    );
    destructor Destroy();override;
    function FindParser(const ANamespace : string) : IXsdParser;
    function ParseType(
      const AName,
            ATypeKind : string { ATypeKind "ComplexType", "SimpleType", "Element" }
    ) : ISDODataObject; overload;
    function ParseType(
      const AName     : string;
      const ATypeNode : TDOMNode
    ) : ISDODataObject; overload;

    procedure ParseTypes();

    function GetTargetNameSpace() : string;
    function GetTargetModule() : ISDODataObject;

    property SymbolTable : ISDODataObject read FSymbols;
    property Module : ISDODataObject read FModule;
    property OnMessage : TOnParserMessage read FOnMessage write FOnMessage;
  end;
  TCustomXsdSchemaParserClass = class of TCustomXsdSchemaParser;

  TXsdParser = class(TCustomXsdSchemaParser)
  public
    constructor Create(
            ADoc : TXMLDocument;
            ASymbols : ISDODataObject;
      const AModuleName : string;
      const ANotifier : TOnParserMessage = nil
    );
  end;


  procedure ParseSchema(
    const AFileName : string;
          ATypeTree : ISDODataObject
  );


implementation
uses
{$IFDEF FPC}
  xmlread,
{$ENDIF FPC}
  sdo_dom_cursors, sdo_parserutils, StrUtils, xsd_consts, sdo_consts,
  sdo_xsdintf, sdo_utils;

procedure ParseSchema(
  const AFileName : string;
        ATypeTree : ISDODataObject
);
var
  locDoc : TXMLDocument;
  locParser : IXsdParser;
begin
  ReadXMLFile(locDoc,AFileName);
  locParser := TXsdParser.Create(locDoc,ATypeTree,'');
  locParser.ParseTypes();
end;


function NodeValue(const ANode : TDOMNode) : DOMString;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ( ANode = nil ) then
    Result := ''
  else
    Result := ANode.NodeValue;
end;

function CreateUnresolvedType(
  const ATypeTree  : ISDODataObject;
  const ANamespace,
        AName      : string
) : ISDODataObject;
var
  locRes : ISDODataObject;
begin
  locRes := ATypeTree.createDataObject(s_Unresolved);
    locRes.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
    locRes.setBoolean(s_Unresolved,True);
    locRes.setString(s_NameSpace,ANamespace);
    locRes.setString(s_Name,AName);
    ATypeTree.getList(s_Unresolved).append(locRes);
  Result := locRes;
end;

{ TAbstractTypeParser }

constructor TAbstractTypeParser.Create(
        AOwner       : IParserContext;
        ATypeNode    : TDOMNode;
  const ATypeName    : string;
  const AEmbededDef  : Boolean
);
var
  symtbl : ISDODataObject;
begin
  Assert(Assigned(AOwner));
  Assert(Assigned(ATypeNode));
  symtbl := AOwner.GetSymbolTable();
  Assert(Assigned(symtbl));
  FContext := AOwner;
  FTypeNode := ATypeNode;
  FSymbols := symtbl;
  FTypeName := ATypeName;
  FEmbededDef := AEmbededDef;
end;

class function TAbstractTypeParser.ExtractEmbeddedTypeFromElement(
        AOwner       : IParserContext;
        AEltNode     : TDOMNode;
        ASymbols     : ISDODataObject;
  const ATypeName    : string
) : ISDODataObject;

  function ExtractTypeName() : string;
  var
    locCrs : IObjectCursor;
  begin
    locCrs := CreateCursorOn(
                CreateAttributesCursor(AEltNode,cetRttiNode),
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserException.Create(SERR_UnableToFindNameTagInNode);
    Result := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(Result) then begin
      raise EXsdParserException.Create(SERR_InvalidTypeName);
    end;
  end;

  function FindParser(out AFoundTypeNode : TDOMNode):TAbstractTypeParserClass;
  var
    k : Integer;
    locPrsClss : TAbstractTypeParserClass;
    locFilter : string;
    locCrs : IObjectCursor;
  begin
    Result := nil;
    AFoundTypeNode := nil;
    for k := 0 to Pred(GetRegisteredParserCount()) do begin
      locPrsClss := GetRegisteredParser(k);
      locFilter := locPrsClss.GetParserSupportedStyle();
      if not IsStrEmpty(locFilter) then begin
        locFilter := CreateQualifiedNameFilterStr(locFilter,AOwner.GetXsShortNames());
        locCrs := CreateCursorOn(CreateChildrenCursor(AEltNode,cetRttiNode),ParseFilter(locFilter,TDOMNodeRttiExposer));
        locCrs.Reset();
        if locCrs.MoveNext() then begin
          AFoundTypeNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          Result := locPrsClss;
          Break;
        end;
      end;
    end;
  end;

var
  typName : string;
  prsClss : TAbstractTypeParserClass;
  prs : TAbstractTypeParser;
  typNode : TDOMNode;
begin
  if not AEltNode.HasChildNodes() then begin;
    raise EXsdParserException.Create(SERR_InvalidTypeDef_NoChild);
  end;
  typName := ATypeName;
  if IsStrEmpty(typName) then begin
    typName := ExtractTypeName();
  end;
  prsClss := FindParser(typNode);
  if ( prsClss = nil ) then begin;
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_TypeStyleNotSupported,[typName]);
  end;
  prs := prsClss.Create(AOwner,typNode,typName,True);
  try
    Result := prs.Parse();
  finally
    FreeAndNil(prs);
  end;
end;

var
  FTypeParserList : TClassList = nil;
class procedure TAbstractTypeParser.RegisterParser(AParserClass: TAbstractTypeParserClass);
begin
  if ( FTypeParserList = nil ) then begin
    FTypeParserList := TClassList.Create();
  end;
  if ( FTypeParserList.IndexOf(AParserClass) < 0 ) then begin
    FTypeParserList.Add(AParserClass);
  end;
end;

class function TAbstractTypeParser.GetRegisteredParserCount(): Integer;
begin
  if Assigned(FTypeParserList) then begin
    Result := FTypeParserList.Count;
  end else begin
    Result := 0;
  end;
end;

class function TAbstractTypeParser.GetRegisteredParser(const AIndex: Integer): TAbstractTypeParserClass;
begin
  Result := TAbstractTypeParserClass(FTypeParserList[AIndex]);
end;

function TAbstractTypeParser.FindElementNS(
  const ANameSpace,
        ALocalName : string;
  const ASpaceType : TNameSpaceValueType
) : ISDODataObject;
var
  locNS : string;
begin
  if ( ASpaceType = nvtExpandValue ) then begin
    locNS := ANameSpace
  end else begin
    if not Context.FindNameSpace(ANameSpace,locNS) then
      raise EXsdParserAssertException.CreateFmt(SERR_CannotResolveNamespace,[ANameSpace]);
  end;
  Result := Find(FSymbols,locNS,ALocalName);
end;

function TAbstractTypeParser.GetModule : ISDODataObject;
begin
  Result := Context.GetTargetModule();
end;

function TAbstractTypeParser.FindElementWithHint(
  const AName,
        AHint      : string;
  const ASpace : TSearchSpace
) : ISDODataObject;
begin
  Result := nil;
  if ( ASpace = ssCurrentModule ) then begin
    if ( Length(AHint) > 0 ) then
      Result := Find(FSymbols,AHint);
    if ( Result = nil ) then
      Result := Find(FSymbols,AName);
  end else if ( ASpace = ssGlobal ) then begin
    if ( Length(AHint) > 0 ) then
      Result := Find(FSymbols,AHint);
    if ( Result = nil ) then
      Result := Find(FSymbols,AName);
  end;
end;

function TAbstractTypeParser.ExtractTypeHint(AElement: TDOMNode): string;
begin
  if not sdo_findCustomAttributeXsd(Context.GetXsShortNames(),AElement,s_SDO_typeHint,Result) then
    Result := '';
end;

procedure TAbstractTypeParser.SetAsEmbeddedType(AType : ISDODataObject; const AValue : Boolean);
begin
  AType.setBoolean(s_Embedded,AValue);
end;

function TAbstractTypeParser.IsEmbeddedType(AType : ISDODataObject) : Boolean;
begin
  Result := AType.getBoolean(s_Embedded)
end;

{$IFDEF SDO_HANDLE_DOC}
procedure TAbstractTypeParser.ParseDocumentation(AType : ISDODataObject);
var
  tmpCursor : IObjectCursor;
  props : TStrings;
  docString : string;
  i : PtrInt;
  tempNode : TDOMNode;
begin
  if FTypeNode.HasChildNodes() then begin
    tmpCursor := CreateCursorOn(
                   CreateChildrenCursor(FTypeNode,cetRttiNode),
                   ParseFilter(CreateQualifiedNameFilterStr(s_annotation,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                 );
    if ( tmpCursor <> nil ) then begin
      tmpCursor.Reset();
      if tmpCursor.MoveNext() then begin
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(TDOMNodeRttiExposer(tmpCursor.GetCurrent()).InnerObject,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_documentation,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        if ( tmpCursor <> nil ) then begin
          tmpCursor.Reset();
          if tmpCursor.MoveNext() then begin
            tempNode := TDOMNodeRttiExposer(tmpCursor.GetCurrent()).InnerObject.FirstChild;
            if ( tempNode <> nil ) then
              docString := tempNode.NodeValue
            else
              docString := '';
            props := FSymbols.Properties.FindList(AType);
            if IsStrEmpty(docString) then begin
              if ( props <> nil ) then begin
                i := props.IndexOfName(s_documentation);
                if ( i >= 0 ) then
                  props.Values[s_documentation] := '';
              end
            end else begin
              if ( props = nil ) then
                props := FSymbols.Properties.GetList(AType);
              props.Values[s_documentation] := EncodeLineBreak(docString);
            end;
          end;
        end;
      end;
    end;
  end;
end;
{$ENDIF SDO_HANDLE_DOC}

procedure TAbstractTypeParser.AddUnresolvedLink(
        AElement,
        ATarget   : ISDODataObject;
  const ALinkKind,
        AName     : string
);
var
  locObj : ISDODataObject;
begin
  locObj := FSymbols.createDataObject(s_UnresolvedLink);
    locObj.setDataObject(s_Element,AElement);
    locObj.setDataObject(s_Target,ATarget);
    locObj.setString(s_LinkKind,ALinkKind);
    locObj.setString(s_Name,AName);
  FSymbols.getList(s_UnresolvedLink).append(locObj);
end;

{ TComplexTypeParser }

function TComplexTypeParser.ExtractElementCursor(
  out AAttCursor : IObjectCursor;
  out AAnyNode, AAnyAttNode : TDOMNode
) : IObjectCursor;
var
  frstCrsr : IObjectCursor;

  function ParseContent_ALL() : IObjectCursor;
  var
    locTmpCrs : IObjectCursor;
    locTmpNode : TDOMNode;
  begin
    Result := nil;
    locTmpCrs := CreateCursorOn(
                   frstCrsr.Clone() as IObjectCursor,
                   ParseFilter(CreateQualifiedNameFilterStr(s_all,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                 );
    locTmpCrs.Reset();
    if locTmpCrs.MoveNext() then begin
      FSequenceType := stElement;
      locTmpNode := (locTmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if  locTmpNode.HasChildNodes() then begin
        locTmpCrs := CreateCursorOn(
                       CreateChildrenCursor(locTmpNode,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_element,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        Result := locTmpCrs;
      end;
    end;
  end;

  function ParseContent_SEQUENCE(out ARes : IObjectCursor) : Boolean;
  var
    tmpCursor : IObjectCursor;
    tmpNode : TDOMNode;
  begin
    ARes := nil;
    tmpCursor := CreateCursorOn(
                   frstCrsr.Clone() as IObjectCursor,
                   ParseFilter(CreateQualifiedNameFilterStr(s_sequence,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                 );
    tmpCursor.Reset();
    Result := tmpCursor.MoveNext();
    if Result then begin
      FSequenceType := stElement;
      tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      if  tmpNode.HasChildNodes() then begin
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(tmpNode,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_element,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        ARes := tmpCursor;
        tmpCursor := CreateCursorOn(
                       CreateChildrenCursor(tmpNode,cetRttiNode),
                       ParseFilter(CreateQualifiedNameFilterStr(s_any,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                     );
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then
          AAnyNode := TDOMNodeRttiExposer(tmpCursor.GetCurrent()).InnerObject;
      end;
    end
  end;

var
  parentNode : TDOMNode;
  crs : IObjectCursor;
begin
  Result := nil;
  AAttCursor := nil;
  AAnyNode := nil;
  AAnyAttNode := nil;
  case FDerivationMode of
    dmNone          : parentNode := FContentNode;
    dmRestriction,
    dmExtension     : parentNode := FDerivationNode;
  end;
  if parentNode.HasChildNodes() then begin;
    AAttCursor := CreateCursorOn(
                   CreateChildrenCursor(parentNode,cetRttiNode),
                   ParseFilter(CreateQualifiedNameFilterStr(s_attribute,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                 );
    crs := CreateChildrenCursor(parentNode,cetRttiNode);
    if ( crs <> nil ) then begin
      crs := CreateCursorOn(
               crs,
               ParseFilter(CreateQualifiedNameFilterStr(s_anyAttribute,Context.GetXsShortNames()),TDOMNodeRttiExposer)
             );
      if ( crs <> nil ) then begin
        crs.Reset();
        if crs.MoveNext() then
          AAnyAttNode := TDOMNodeRttiExposer(crs.GetCurrent()).InnerObject;
      end;
    end;
    frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
    if not ParseContent_SEQUENCE(Result) then
      Result := ParseContent_ALL();
  end;
end;

procedure TComplexTypeParser.ExtractExtendedMetadata(
  const AItem : ISDODataObject;
  const ANode : TDOMNode
);
var
  ls : TDOMNamedNodeMap;
  e : TDOMNode;
  k, q : PtrInt;
  ns_short, ns_long, localName, locBuffer, locBufferNS, locBufferNS_long, locBufferLocalName : string;
begin
  if ( ANode.Attributes <> nil ) and ( GetNodeListCount(ANode.Attributes) > 0 ) then begin
    ls := ANode.Attributes;
    q := GetNodeListCount(ANode.Attributes);
    for k := 0 to ( q - 1 ) do begin
      e := ls.Item[k];
      if ( Pos(':', e.NodeName) > 1 ) then begin
        ExplodeQName(e.NodeName,localName,ns_short);
        if Context.FindNameSpace(ns_short, ns_long) then begin
          locBuffer := e.NodeValue;
          ExplodeQName(locBuffer,locBufferLocalName,locBufferNS);
          if IsStrEmpty(locBufferNS) then
            locBuffer := locBufferLocalName
          else if Context.FindNameSpace(locBufferNS, locBufferNS_long) then
            locBuffer := Format('%s#%s',[locBufferNS_long,locBufferLocalName]);
          SetTagValue(AItem,Format('%s#%s',[ns_long,localName]),locBuffer);
        end;
      end;
    end;
  end;
end;

procedure TComplexTypeParser.CreateNodeCursors();
begin
  FAttCursor := CreateAttributesCursor(FTypeNode,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FTypeNode,cetRttiNode);
end;

procedure TComplexTypeParser.ExtractTypeName();
var
  locCrs : IObjectCursor;
begin
  if not FEmbededDef then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserException.Create(SERR_UnableToFindNameTagInNode);
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EXsdParserException.Create(SERR_InvalidTypeName);
end;

procedure TComplexTypeParser.ExtractContentType();
var
  locCrs : IObjectCursor;
begin
  FContentType := '';
  if Assigned(FChildCursor) then begin
    locCrs := CreateCursorOn(
                FChildCursor.Clone() as IObjectCursor,
                ParseFilter(CreateQualifiedNameFilterStr(s_complexContent,Context.GetXsShortNames()),TDOMNodeRttiExposer)
              );
    if Assigned(locCrs) then begin
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        FContentType := FContentNode.NodeName;
      end else begin
        locCrs := CreateCursorOn(
                    FChildCursor.Clone() as IObjectCursor,
                    ParseFilter(CreateQualifiedNameFilterStr(s_simpleContent,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                  );
        locCrs.Reset();
        if locCrs.MoveNext() then begin
          FContentNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          FContentType := FContentNode.NodeName;
        end else begin
          FContentNode := FTypeNode;
          FContentType := s_complexContent;
        end;
      end;
      FContentType := ExtractNameFromQName(FContentType);
    end;
  end;
end;

procedure TComplexTypeParser.ExtractBaseType();
var
  locContentChildCrs, locCrs : IObjectCursor;
  locSymbol : ISDODataObject;
  locBaseTypeLocalSpace, locBaseTypeLocalName, locFilterStr : string;
  locBaseTypeLocalSpaceExpanded : string;
begin
  locFilterStr := CreateQualifiedNameFilterStr(s_extension,Context.GetXsShortNames());
  locContentChildCrs := CreateChildrenCursor(FContentNode,cetRttiNode);
  locCrs := CreateCursorOn(
              locContentChildCrs.Clone() as IObjectCursor,
              ParseFilter(locFilterStr,TDOMNodeRttiExposer)
            );
  locCrs.Reset();
  if locCrs.MoveNext() then begin
    FDerivationMode := dmExtension;
    FDerivationNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
  end else begin
    locFilterStr := CreateQualifiedNameFilterStr(s_restriction,Context.GetXsShortNames());
    locCrs := CreateCursorOn(
                locContentChildCrs.Clone() as IObjectCursor,
                ParseFilter(locFilterStr,TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if locCrs.MoveNext() then begin
      FDerivationMode := dmRestriction;
      FDerivationNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    end else begin
      FDerivationMode := dmNone;
      FDerivationNode := nil;
   end;
  end;
  if ( FDerivationMode > dmNone ) then begin
    locCrs := CreateCursorOn(
      CreateAttributesCursor(FDerivationNode,cetRttiNode),
      ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_base)]),TDOMNodeRttiExposer)
    );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserException.CreateFmt(SERR_InvalidTypeDef_BaseAttributeNotFound,[FTypeName]);
    ExplodeQName((locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locBaseTypeLocalName,locBaseTypeLocalSpace);
    locSymbol := FindElementNS(locBaseTypeLocalSpace,locBaseTypeLocalName,nvtShortSynonym);
    if Assigned(locSymbol) then begin
      if (locSymbol.getByte(s_ElementKind) = ELEMENT_KIND_VARIABLE) then
        locSymbol := locSymbol.getDataObject(s_DataType);
      if (locSymbol.getByte(s_ElementKind) = ELEMENT_KIND_TYPE) then begin
        FBaseType := locSymbol;
        { TODO -cXSD : Handle "Complex Type" that extend ""Simple Type }
        {if FBaseType.InheritsFrom(TPasNativeSimpleType) then begin
          Assert(Assigned(TPasNativeSimpleType(FBaseType).ExtendableType));
          FBaseType := TPasNativeSimpleType(FBaseType).ExtendableType;
        end else if FBaseType.InheritsFrom(TPasNativeClassType) then begin
          if Assigned(TPasNativeClassType(FBaseType).ExtendableType) then
            FBaseType := TPasNativeClassType(FBaseType).ExtendableType;
        end; }
      end else begin
        raise EXsdParserException.CreateFmt(SERR_ExpectedTypeDefinition,[locSymbol.getString(s_Name)]);
      end;
    end else begin
      if ( FDerivationMode = dmRestriction ) and
         ( locBaseTypeLocalName = 'Array' ) and
         ( Context.FindNameSpace(locBaseTypeLocalSpace,locBaseTypeLocalSpaceExpanded) and
           ( locBaseTypeLocalSpaceExpanded = s_soapEncodingNameSpace )
         )
      then begin
        FHints := FHints + [pthDeriveFromSoapArray];
      end else begin
        if IsStrEmpty(locBaseTypeLocalSpaceExpanded) then begin
          if not Context.FindNameSpace(locBaseTypeLocalSpace,locBaseTypeLocalSpaceExpanded) then
            locBaseTypeLocalSpaceExpanded := Self.Module.getString(s_Name);
        end;
        FBaseType := CreateUnresolvedType(FSymbols,locBaseTypeLocalSpaceExpanded,locBaseTypeLocalName);
      end;
    end;
  end;
end;

function TComplexTypeParser.ParseComplexContent(const ATypeName : string) : ISDODataObject;
var
  classDef : ISDODataObject;
  isArrayDef : Boolean;

  function IsCollectionArray(AElement : TDOMNode) : Boolean;
  var
    strBuffer : string;
  begin
    Result := sdo_findCustomAttributeXsd(Context.GetXsShortNames(),AElement,s_SDO_collection,strBuffer) and SameText('true',Trim(strBuffer));
  end;

  procedure ParseElement(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locTypeNameSpace : string;
    locType : ISDODataObject;
    locProp : ISDODataObject;
    locMinOccur, locMaxOccur : Integer;
    locMaxOccurUnbounded : Boolean;
    locStrBuffer : string;
    locIsRefElement : Boolean;
    locTypeHint : string;
    locVariable : ISDODataObject;
  begin
    locType := nil;
    locTypeName := '';
    locTypeHint := '';
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    locIsRefElement := False;
    if not locPartCursor.MoveNext() then begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_ref)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if not locPartCursor.MoveNext() then begin
        raise EXsdParserException.Create(SERR_InvalidElementDef_MissingNameOrRef);
      end;
      locIsRefElement := True;
    end;
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if locIsRefElement then begin
      locName := ExtractNameFromQName(locName);
    end;
    if IsStrEmpty(locName) then
      raise EXsdParserException.Create(SERR_InvalidElementDef_EmptyName);
    if locIsRefElement then begin
      locVariable := FindVariable(FSymbols,locName);
      if (locVariable <> nil) then begin
        if not locVariable.getBoolean(Format('%s/%s',[s_DataType,s_Unresolved])) then
          locType := locVariable.getDataObject(s_DataType);
        locTypeName := locVariable.getString(Format('%s/%s',[s_DataType,s_Name]));// locName;
      end else begin
        locTypeName := locName;
      end;
    end else begin
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        ExplodeQName(
          TDOMNodeRttiExposer(locPartCursor.GetCurrent()).NodeValue,
          locTypeName,locTypeNameSpace
        );
        locTypeName := ExtractNameFromQName(TDOMNodeRttiExposer(locPartCursor.GetCurrent()).NodeValue);
        locTypeHint := ExtractTypeHint(AElement);
      end else begin
        locTypeName := Format('%s_%s_Type',[FTypeName,locName]);
        locType := TAbstractTypeParser.ExtractEmbeddedTypeFromElement(Context,AElement,FSymbols,locTypeName);
        if ( locType = nil ) then begin
          raise EXsdInvalidElementDefinitionException.CreateFmt(SERR_InvalidElementDef_Type,[FTypeName,locName]);
        end;
        //Self.Module.getList(s_Type).append(locType);
      end;
    end;
    if IsStrEmpty(locTypeName) then
      raise EXsdInvalidElementDefinitionException.Create(SERR_InvalidElementDef_EmptyType);
    if (locType = nil) then
      locType := FindElementNS(locTypeNameSpace,locTypeName,nvtShortSynonym);
    if (locType = nil) then
      locType := FindElementWithHint(locTypeName,locTypeHint,ssGlobal);
    if (locType = nil) then
      locType := CreateUnresolvedType(FSymbols,Self.Module.getString(s_NameSpace),locTypeName);

    locProp := classDef.createDataObject(s_Property);
    locProp.setByte(s_ElementKind,ELEMENT_KIND_PROPERTY);
    locProp.setString(s_Name,locName);
    locProp.setDataObject(s_DataType,locType);
    classDef.getList(s_Property).append(locProp);
    if locType.getBoolean(s_Unresolved) then
      AddUnresolvedLink(classDef,locType,LINK_KIND_PROP_TYPE,locProp.getString(s_Name));

    if SameText(s_attribute,ExtractNameFromQName(AElement.NodeName)) then begin
      locProp.setBoolean(s_IsAttribute,True);
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        locStrBuffer := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
        if IsStrEmpty(locStrBuffer) then
          raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyUse);
        case AnsiIndexText(locStrBuffer,[s_required,s_optional,s_prohibited]) of
          0 : locMinOccur := 1;
          1 : locMinOccur := 0;
          2 : locMinOccur := -1;
          else
            raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidAttributeDef_InvalidUse,[locStrBuffer]);
        end;
      end else begin
        locMinOccur := 0;
      end;
    end else begin
      locMinOccur := 1;
      locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_minOccurs)]),TDOMNodeRttiExposer));
      locPartCursor.Reset();
      if locPartCursor.MoveNext() then begin
        if not TryStrToInt((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue,locMinOccur) then
          raise EXsdParserException.CreateFmt(SERR_InvalidMinOccursValue,[FTypeName,locName]);
        if ( locMinOccur < 0 ) then
          raise EXsdParserException.CreateFmt(SERR_InvalidMinOccursValue,[FTypeName,locName]);
      end;
    end;
    locProp.setInteger(s_PropertyMinOccurs,locMinOccur);

    locMaxOccur := 1;
    locMaxOccurUnbounded := False;
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_maxOccurs)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      locStrBuffer := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
      if SameText(locStrBuffer,s_unbounded) then begin
        locMaxOccurUnbounded := True;
      end else begin
        if not TryStrToInt(locStrBuffer,locMaxOccur) then
          raise EXsdParserException.CreateFmt(SERR_InvalidMaxOccursValue,[FTypeName,locName]);
        if ( locMaxOccur < 0 ) then
          raise EXsdParserException.CreateFmt(SERR_InvalidMaxOccursValue,[FTypeName,locName]);
      end;
    end;
    if locMaxOccurUnbounded then
      locMaxOccur := MaxInt - 1;
    locProp.setInteger(s_PropertyMaxOccurs,locMaxOccur);
    isArrayDef := locMaxOccurUnbounded or ( locMaxOccur > 1 );
    if isArrayDef then
      SetTagValue(locProp,s_SDO_collection,TSDOConvertHelper.BoolToString(IsCollectionArray(AElement)));
    if not isArrayDef then begin
      SetTagValue(
        locProp,s_attribute,
        TSDOConvertHelper.BoolToString(SameText(s_attribute,ExtractNameFromQName(AElement.NodeName)))
      );
    end;
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_default)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then
      locProp.setString(s_DefaultValue,(locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    ExtractExtendedMetadata(locProp,AElement);
  end;

  function IsRecordType() : Boolean;
  var
    strBuffer : string;
  begin
    Result := sdo_findCustomAttributeXsd(Context.GetXsShortNames(),FTypeNode,s_SDO_record,strBuffer) and SameText('true',Trim(strBuffer));
  end;

  procedure ParseElementsAndAttributes(AEltCrs, AEltAttCrs : IObjectCursor);
  begin
    if Assigned(AEltCrs) then begin
      AEltCrs.Reset();
      while AEltCrs.MoveNext() do begin
        ParseElement((AEltCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
    if Assigned(AEltAttCrs) then begin
      AEltAttCrs.Reset();
      while AEltAttCrs.MoveNext() do begin
        ParseElement((AEltAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
      end;
    end;
  end;

  procedure CopyExtendedMetaData(ASource,ADesc : ISDODataObject);
  begin
    CopySimpleList(ASource.getList(s_Tag),ADesc.getList(s_Tag),StringType);
  end;

  procedure ProcessXsdAnyDeclarations(AAnyNode, AAnyAttNode : TDOMNode; AType : ISDODataObject);
  var
    anyElt : TDOMElement;
    ls : TStringList;
    anyDec : string;
  begin
    if ( AAnyNode <> nil ) then begin
      anyElt := AAnyNode as TDOMElement;
      ls := TStringList.Create();
      try
        if anyElt.hasAttribute(s_processContents) then
          ls.Values[s_processContents] := anyElt.GetAttribute(s_processContents);
        if anyElt.hasAttribute(s_minOccurs) then
          ls.Values[s_minOccurs] := anyElt.GetAttribute(s_minOccurs);
        if anyElt.hasAttribute(s_maxOccurs) then
          ls.Values[s_maxOccurs] := anyElt.GetAttribute(s_maxOccurs);
        if ( ls.Count > 0 ) then begin
          ls.Delimiter := ';';
          anyDec := ls.DelimitedText;
        end;
      finally
        ls.Free();
      end;
      SetTagValue(AType,Format('%s#%s',[s_xs,s_any]),anyDec);
    end;
    if ( AAnyAttNode <> nil ) then begin
      anyDec := '';
      anyElt := AAnyAttNode as TDOMElement;
      if anyElt.hasAttribute(s_processContents) then
        anyDec := anyElt.GetAttribute(s_processContents);
      SetTagValue(AType,Format('%s#%s',[s_xs,s_anyAttribute]),Format('%s=%s',[s_processContents,anyDec]));
    end;
  end;

var
  eltCrs, eltAttCrs : IObjectCursor;
  locAnyNode, locAnyAttNode : TDOMNode;
begin
  ExtractBaseType();
  eltCrs := ExtractElementCursor(eltAttCrs,locAnyNode,locAnyAttNode);

  classDef := Module.createDataObject(s_Type);
  classDef.setBoolean(s_IsComplex,True);
  classDef.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
  classDef.setString(s_Name,ATypeName);
  Module.getList(s_Type).append(classDef);
  Result := classDef;
  if (FDerivationMode in [dmExtension, dmRestriction]) and
     (FBaseType <> nil)
  then begin
    classDef.setDataObject(s_BaseType,FBaseType);
    if FBaseType.getBoolean(s_Unresolved) then
      AddUnresolvedLink(classDef,FBaseType,LINK_KIND_BASE_TYPE,'');
  end;
  if Assigned(eltCrs) or Assigned(eltAttCrs) then begin
    isArrayDef := False;
    ParseElementsAndAttributes(eltCrs,eltAttCrs);
  end;
  if ( locAnyNode <> nil ) or ( locAnyAttNode <> nil ) then
    ProcessXsdAnyDeclarations(locAnyNode,locAnyAttNode,Result);
end;

function TComplexTypeParser.ParseSimpleContent(const ATypeName : string) : ISDODataObject;

  function ExtractAttributeCursor():IObjectCursor;
  var
    frstCrsr, tmpCursor : IObjectCursor;
    parentNode, tmpNode : TDOMNode;
    locFilterStr : string;
    xsShortNameList : TStrings;
  begin
    Result := nil;
    parentNode := FContentNode;
    if parentNode.HasChildNodes() then begin;
      xsShortNameList := Context.GetXsShortNames();
      frstCrsr := CreateChildrenCursor(parentNode,cetRttiNode);
      locFilterStr := CreateQualifiedNameFilterStr(s_extension,xsShortNameList) + ' or ' +
                      CreateQualifiedNameFilterStr(s_restriction,xsShortNameList) ;
      tmpCursor := CreateCursorOn(frstCrsr.Clone() as IObjectCursor,ParseFilter(locFilterStr,TDOMNodeRttiExposer));
      if Assigned(tmpCursor) then begin
        tmpCursor.Reset();
        if tmpCursor.MoveNext() then begin
          tmpNode := (tmpCursor.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
          if tmpNode.HasChildNodes() then begin
            locFilterStr := CreateQualifiedNameFilterStr(s_attribute,xsShortNameList);
            tmpCursor := CreateCursorOn(CreateChildrenCursor(tmpNode,cetRttiNode),ParseFilter(locFilterStr,TDOMNodeRttiExposer));
            if Assigned(tmpCursor) then begin
              Result := tmpCursor;
              Result.Reset();
            end;
          end;
        end;
      end;
    end else begin
      Result := nil;
    end;
  end;

var
  locClassDef : ISDODataObject;

  procedure ParseAttribute(AElement : TDOMNode);
  var
    locAttCursor, locPartCursor : IObjectCursor;
    locName, locTypeName, locTypeNameSpace, locStoreOpt : string;
    locType : ISDODataObject;
    locStoreOptIdx : Integer;
    locAttObj : ISDODataObject;
  begin
    locAttCursor := CreateAttributesCursor(AElement,cetRttiNode);
    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then
      raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_MissingName);
    locName := (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
    if IsStrEmpty(locName) then
      raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyName);

    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if not locPartCursor.MoveNext() then
      raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_MissingType);
    ExplodeQName(
      (locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue,
      locTypeName,locTypeNameSpace
    ); //locTypeName := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
    if IsStrEmpty(locTypeName) then
      raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyType);
    locType := FindElementNS(locTypeNameSpace,locTypeName,nvtShortSynonym);
    //locType := FSymbols.FindElement(locTypeName) as TPasType;
    if not Assigned(locType) then begin
      if not IsStrEmpty(locTypeNameSpace) then begin
        if not Context.FindNameSpace(Copy(locTypeNameSpace,1,Length(locTypeNameSpace)),locTypeNameSpace) then
          locTypeNameSpace := Module.getString(s_NameSpace);
      end;
      if IsStrEmpty(locTypeNameSpace) then
        locTypeNameSpace := Module.getString(s_NameSpace);
      locType := CreateUnresolvedType(FSymbols,locTypeNameSpace,locTypeName);
      //locType := TPasUnresolvedTypeRef(FSymbols.CreateElement(TPasUnresolvedTypeRef,locTypeName,Self.Module.InterfaceSection,visPublic,'',0));
    end;

    locPartCursor := CreateCursorOn(locAttCursor.Clone() as IObjectCursor,ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_use)]),TDOMNodeRttiExposer));
    locPartCursor.Reset();
    if locPartCursor.MoveNext() then begin
      locStoreOpt := ExtractNameFromQName((locPartCursor.GetCurrent() as TDOMNodeRttiExposer).NodeValue);
      if IsStrEmpty(locStoreOpt) then
        raise EXsdInvalidDefinitionException.Create(SERR_InvalidAttributeDef_EmptyUse);
      locStoreOptIdx := AnsiIndexText(locStoreOpt,[s_required,s_optional,s_prohibited]);
      if ( locStoreOptIdx < 0 ) then
        raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidAttributeDef_InvalidUse,[locStoreOpt]);
    end else begin
      locStoreOptIdx := 1{optional by default!}; //0;
    end;

    locAttObj := locClassDef.createDataObject(s_Property);
    locAttObj.setByte(s_ElementKind,ELEMENT_KIND_PROPERTY);
    locClassDef.getList(s_Property).append(locAttObj);
    locAttObj.setString(s_Name,locName);
    locAttObj.setDataObject(s_DataType,locType);
    locAttObj.setBoolean(s_IsAttribute,True);
    if locType.getBoolean(s_Unresolved) then
      AddUnresolvedLink(locClassDef,locType,LINK_KIND_PROP_TYPE,locAttObj.getString(s_Name));
    case locStoreOptIdx of
      0 :
        begin
          locAttObj.setInteger(s_PropertyMinOccurs,1);
          locAttObj.setInteger(s_PropertyMaxOccurs,1);
        end;
      1 :
        begin
          locAttObj.setInteger(s_PropertyMinOccurs,0);
          locAttObj.setInteger(s_PropertyMaxOccurs,1);
        end;
      2 :
        begin
          locAttObj.setInteger(s_PropertyMinOccurs,0);
          locAttObj.setInteger(s_PropertyMaxOccurs,0);
        end;
    end;
  end;

var
  locAttCrs : IObjectCursor;
begin
  ExtractBaseType();
  if not ( FDerivationMode in [dmExtension, dmRestriction] ) then
    raise EXsdInvalidTypeDefinitionException.Create(SERR_InvalidComplexSimpleTypeDef_NoRestOrExt);

  locAttCrs := ExtractAttributeCursor();
  locClassDef := Module.createDataObject(s_Type);
  locClassDef.setBoolean(s_IsComplex,True);
  locClassDef.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
  Module.getList(s_Type).append(locClassDef);
  locClassDef.setString(s_Name,ATypeName);
  Result := locClassDef;
  if ( FDerivationMode in [dmExtension, dmRestriction] ) then begin
    if (FBaseType <> nil) then
      locClassDef.setDataObject(s_BaseType,FBaseType);
  end;
  if ( locAttCrs <> nil ) then begin
    locAttCrs.Reset();
    while locAttCrs.MoveNext() do begin
      ParseAttribute((locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
    end;
  end;
end;

function TComplexTypeParser.ParseEmptyContent(const ATypeName: string): ISDODataObject;
begin
  Result := Module.createDataObject(s_Type);
  Result.setBoolean(s_IsComplex,True);
  Result.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
  Module.getList(s_Type).append(Result);
  Result.setString(s_Name,ATypeName);
end;

class function TComplexTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_complexType;
end;

function TComplexTypeParser.Parse() : ISDODataObject;
var
  locSym : ISDODataObject;
  locContinue : Boolean;
begin
  if not SameText(ExtractNameFromQName(FTypeNode.NodeName),s_complexType) then
    raise EXsdParserAssertException.CreateFmt(SERR_ExpectedButFound,[s_complexType,ExtractNameFromQName(FTypeNode.NodeName)]);
  Result := nil;
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FindInModule(Module,FTypeName); //FSymbols.FindElement(FTypeName);
  if (locSym = nil) then
    locSym := Find(FSymbols,FTypeName);
  if Assigned(locSym) then begin
    if (locSym.getByte(s_ElementKind) <> ELEMENT_KIND_TYPE) then
      raise EXsdParserException.CreateFmt(SERR_ExpectedTypeDefinition,[FTypeName]);
    locContinue := locSym.getBoolean(s_Unresolved) or
                   (IsEmbeddedType(locSym) <> FEmbededDef);
    if not locContinue then;
      Result := locSym;
  end;
  if locContinue then begin
    ExtractContentType();
    if IsStrEmpty(FContentType) then begin
      Result := ParseEmptyContent(FTypeName);
    end else begin
      if SameText(FContentType,s_complexContent) then
        Result := ParseComplexContent(FTypeName)
      else
        Result := ParseSimpleContent(FTypeName);
    end;
    if ( Result <> nil ) then begin
      if ( IsEmbeddedType(Result) <> FEmbededDef ) then
        SetAsEmbeddedType(Result,FEmbededDef);
    end;
{$IFDEF SDO_HANDLE_DOC}
    if ( Result <> nil ) then
      ParseDocumentation(Result);
{$ENDIF SDO_HANDLE_DOC}
  end;
end;

{ TSimpleTypeParser }

procedure TSimpleTypeParser.CreateNodeCursors();
begin
  FAttCursor := CreateAttributesCursor(FTypeNode,cetRttiNode);
  FChildCursor := CreateChildrenCursor(FTypeNode,cetRttiNode);
end;

procedure TSimpleTypeParser.ExtractTypeName();
var
  locCrs : IObjectCursor;
begin
  if not FEmbededDef then begin
    locCrs := CreateCursorOn(
                FAttCursor.Clone() as IObjectCursor,
                ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer)
              );
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdParserAssertException.Create(SERR_UnableToFindNameTagInNode);
    FTypeName := (locCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue;
  end;
  if IsStrEmpty(FTypeName) then
    raise EXsdParserAssertException.Create(SERR_InvalidTypeName);
end;

function TSimpleTypeParser.ExtractContentType() : Boolean;
var
  locCrs, locAttCrs : IObjectCursor;
  tmpNode : TDOMNode;
  spaceShort : string;
begin
  locCrs := CreateCursorOn(
              FChildCursor.Clone() as IObjectCursor,
              ParseFilter(CreateQualifiedNameFilterStr(s_restriction,Context.GetXsShortNames()),TDOMNodeRttiExposer)
            );
  locCrs.Reset();
  if locCrs.MoveNext() then begin
    FRestrictionNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    tmpNode := nil;
    locAttCrs := CreateAttributesCursor(FRestrictionNode,cetRttiNode);
    if Assigned(locAttCrs) then begin
      locAttCrs := CreateCursorOn(locAttCrs,ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_base)]),TDOMNodeRttiExposer));
      locAttCrs.Reset();
      if locAttCrs.MoveNext() then begin
        tmpNode := (locAttCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      end;
    end;
    FBaseName := '';
    FBaseNameSpace := '';
    if Assigned(tmpNode) then begin
      ExplodeQName(tmpNode.NodeValue,FBaseName,spaceShort);
      if not Context.FindNameSpace(spaceShort,FBaseNameSpace) then
        raise EXsdParserAssertException.CreateFmt(SERR_CannotResolveNamespace,[spaceShort]);
    end;
    locCrs := CreateChildrenCursor(FRestrictionNode,cetRttiNode) as IObjectCursor;
    if Assigned(locCrs) then begin
      locCrs := CreateCursorOn(
                  locCrs,
                  ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,Context.GetXsShortNames()),TDOMNodeRttiExposer)
                );
      locCrs.Reset();
      if locCrs.MoveNext() then begin
        FIsEnum := True;
      end else begin
        if IsStrEmpty(FBaseName) then
          raise EXsdParserAssertException.CreateFmt(SERR_BaseTypeNotSpecfifiedForSimpleType,[FTypeName]);
        FIsEnum := False
      end;
    end else begin
      if IsStrEmpty(FBaseName) then
        raise EXsdParserAssertException.CreateFmt(SERR_BaseTypeNotSpecfifiedForSimpleType,[FTypeName]);
      FIsEnum := False
    end;
    Result := True;
  end else begin
    //raise EWslParserException.CreateFmt('The parser only support "Restriction" mode simple type derivation, parsing : "%s".',[FTypeName]);
    Result := False;
  end;
end;

function TSimpleTypeParser.ParseEnumContent(): ISDODataObject;

  function ExtractEnumCursor():IObjectCursor ;
  begin
    Result := CreateCursorOn(
                CreateChildrenCursor(FRestrictionNode,cetRttiNode),
                ParseFilter(CreateQualifiedNameFilterStr(s_enumeration,Context.GetXsShortNames()),TDOMNodeRttiExposer)
              );
  end;

var
  locRes : ISDODataObject;
  locEnumValueList : ISDODataObjectList;

  procedure ParseEnumItem(AItemNode : TDOMNode);
  var
    tmpNode : TDOMNode;
    locCrs : IObjectCursor;
  begin
    locCrs := CreateCursorOn(CreateAttributesCursor(AItemNode,cetRttiNode),ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_value)]),TDOMNodeRttiExposer)) as IObjectCursor;
    if not Assigned(locCrs) then
      raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidEnumItemNode_NoValueAttribute,[FTypeName]);
    locCrs.Reset();
    if not locCrs.MoveNext() then
      raise EXsdInvalidDefinitionException.CreateFmt(SERR_InvalidEnumItemNode_NoValueAttribute,[FTypeName]);
    tmpNode := (locCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    locEnumValueList.append(tmpNode.nodeValue);
  end;

var
  locEnumCrs : IObjectCursor;
begin
  locEnumCrs := ExtractEnumCursor();

  locRes := Module.createDataObject(s_Type);
  locRes.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
  locRes.setString(s_Name,FTypeName);
  Module.getList(s_Type).append(locRes);
  locEnumValueList := locRes.getList(s_EnumValue);
  locEnumCrs.Reset();
  while locEnumCrs.MoveNext() do begin
    ParseEnumItem((locEnumCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject);
  end;
  Result := locRes;
end;

function TSimpleTypeParser.ParseOtherContent(): ISDODataObject;
var
  locRes, locBaseType : ISDODataObject;
begin  // todo : implement TSimpleTypeParser.ParseOtherContent
  if IsStrEmpty(FBaseName) then
    raise EXsdInvalidTypeDefinitionException.CreateFmt(SERR_BaseTypeNotSpecfifiedForSimpleType,[FTypeName]);
  locRes := Module.createDataObject(s_Type);
  locRes.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
  locRes.setString(s_Name,FTypeName);
  Module.getList(s_Type).append(locRes);
  locBaseType := FindElementNS(FBaseNameSpace,FBaseName,nvtExpandValue);
  if (locBaseType <> nil) then
    locRes.setDataObject(s_BaseType,locBaseType);
  Result := locRes;
end;

class function TSimpleTypeParser.GetParserSupportedStyle(): string;
begin
  Result := s_simpleType;
end;

function TSimpleTypeParser.Parse(): ISDODataObject;
var
  locSym : ISDODataObject;
  locContinue : Boolean;
begin
  if not SameText(ExtractNameFromQName(FTypeNode.NodeName),s_simpleType) then
    raise EXsdParserAssertException.CreateFmt(SERR_ExpectedButFound,[s_simpleType,ExtractNameFromQName(FTypeNode.NodeName)]);
  Result := nil;
  CreateNodeCursors();
  ExtractTypeName();
  locContinue := True;
  locSym := FindInModule(Module,FTypeName);
  if Assigned(locSym) then begin
    if (locSym.getByte(s_ElementKind) <> ELEMENT_KIND_TYPE) then
      raise EXsdParserAssertException.CreateFmt(SERR_ExpectedTypeDefinition,[FTypeName]);
    locContinue := locSym.getBoolean(s_Unresolved);
    if not locContinue then
      Result := locSym;
  end;
  if locContinue then begin
    if ExtractContentType() then begin
      if FIsEnum then begin
        Result := ParseEnumContent()
      end else begin
        Result := ParseOtherContent();
      end;
    end else begin
      FBaseName := 'string';
      FBaseNameSpace := s_xs;
      Result := ParseOtherContent();
    end;
    if ( Result <> nil ) then begin
      if ( IsEmbeddedType(Result) <> FEmbededDef ) then
        SetAsEmbeddedType(Result,FEmbededDef);
    end;
{$IFDEF SDO_HANDLE_DOC}
    if ( Result <> nil ) then
      ParseDocumentation(Result);
{$ENDIF SDO_HANDLE_DOC}
  end;
end;

{ TCustomXsdSchemaParser }

constructor TCustomXsdSchemaParser.Create(
  ADoc           : TXMLDocument;
  ASchemaNode    : TDOMNode;
  ASymbols       : ISDODataObject;
  AParentContext : IParserContext
);
begin
  if ( ADoc = nil ) then
    raise EXsdParserAssertException.Create(SERR_InvalidDomDocument);
  if ( ASchemaNode = nil ) then
    raise EXsdParserAssertException.Create(SERR_InvalidSchemaNode);
  if ( ASymbols = nil ) then
    raise EXsdParserAssertException.Create(SERR_InvalidSymbolTable);

  FDoc := ADoc;
  FParentContext := Pointer(AParentContext);
  FSymbols := ASymbols;
  FSchemaNode := ASchemaNode;

  FNameSpaceList := TStringList.Create();
  FNameSpaceList.Duplicates := dupError;
  FNameSpaceList.Sorted := True;

  Prepare();
end;

destructor TCustomXsdSchemaParser.Destroy();

  procedure FreeList(AList : TStrings);
  var
    j : PtrInt;
  begin
    if Assigned(AList) then begin
      for j := 0  to Pred(AList.Count) do begin
        AList.Objects[j].Free();
        AList.Objects[j] := nil;
      end;
      AList.Free();
    end;
  end;

begin
  FParentContext := nil;
  FreeList(FNameSpaceList);
  FreeList(FXsdParsers);
  inherited;
end;

function TCustomXsdSchemaParser.FindParser(const ANamespace : string) : IXsdParser;
var
  i : PtrInt;
  p, p1 : IXsdParser;
begin
  Result := nil;
  if (ANamespace = FTargetNameSpace) then begin
    Result := Self;
    Exit;
  end;
  if (FXsdParsers = nil) then
    CreateImportParsers();
  if (FXsdParsers = nil) then
    Exit;
  i := FXsdParsers.IndexOf(ANamespace);
  if ( i >= 0 ) then begin
    Result := (FXsdParsers.Objects[i] as TIntfObjectRef).Intf as IXsdParser;
  end else begin
    for i := 0 to Pred(FXsdParsers.Count) do begin
      p := (FXsdParsers.Objects[i] as TIntfObjectRef).Intf as IXsdParser;
      p1 := p.FindParser(ANamespace);
      if (p1 <> nil) then begin
        Result := p1;
        Break;
      end;
    end;
  end;
end;

procedure TCustomXsdSchemaParser.DoOnMessage(
  const AMsgType: TMessageType;
  const AMsg: string
);
begin
  if Assigned(FOnMessage) then begin
    FOnMessage(AMsgType,AMsg);
  end else if IsConsole and HasLogger() then begin
    GetLogger().Log(AMsgType, AMsg);
  end;
end;

procedure TCustomXsdSchemaParser.ParseImportDocuments();
var
  locOldCurrentModule : ISDODataObject;
  i : Integer;
  p : IXsdParser;
begin
  if FImportParsed then
    Exit;
  CreateImportParsers();
  if (FXsdParsers = nil) then
    Exit;

  FImportParsed := True;
  if Assigned(FChildCursor) then begin
    locOldCurrentModule := SymbolTable.getDataObject(s_CurrentModule);
    try
      for i := 0 to FXsdParsers.Count - 1 do begin
        p := TIntfObjectRef(FXsdParsers.Objects[i]).Intf as IXsdParser;
        p.ParseTypes();
      end;
    finally
      SymbolTable.setDataObject(s_CurrentModule,locOldCurrentModule);
    end;
  end;
end;

function TCustomXsdSchemaParser.FindNamedNode(
        AList : IObjectCursor;
  const AName : WideString;
  const AOrder : Integer
): TDOMNode;
var
  attCrs, crs : IObjectCursor;
  curObj : TDOMNodeRttiExposer;
  fltr : IObjectFilter;
  locOrder : Integer;
begin
  Result := nil;
  if Assigned(AList) then begin
    fltr := ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer);
    AList.Reset();
    locOrder := AOrder;
    while AList.MoveNext() do begin
      curObj := AList.GetCurrent() as TDOMNodeRttiExposer;
      attCrs := CreateAttributesCursor(curObj.InnerObject,cetRttiNode);
      if Assigned(attCrs) then begin
        crs := CreateCursorOn(attCrs,fltr);
        crs.Reset();
        if crs.MoveNext() and SameText(AName,TDOMNodeRttiExposer(crs.GetCurrent()).NodeValue) then begin
          Dec(locOrder);
          if ( locOrder <= 0 ) then begin
            Result := curObj.InnerObject;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomXsdSchemaParser.FindNameSpace(
  const AShortName : string;
  out   AResult : string
) : Boolean;
var
  i : PtrInt;
  ls : TStrings;
  pc : IParserContext;
begin
  AResult := '';
  Result := False;
  for i := 0 to Pred(FNameSpaceList.Count) do begin
    ls := FNameSpaceList.Objects[i] as TStrings;
    if ( ls.IndexOf(AShortName) >= 0 ) then begin
      AResult := FNameSpaceList[i];
      Result := True;
      Break;
    end;
  end;
  if not Result then begin
    pc := GetParentContext();
    if ( pc <> nil ) then
      Result := GetParentContext().FindNameSpace(AShortName,AResult);
  end;
end;

function TCustomXsdSchemaParser.FindShortNamesForNameSpace(const ANameSpace: string): TStrings;
var
  prtCtx : IParserContext;
begin
  Result := FindShortNamesForNameSpaceLocal(ANameSpace);
  if ( Result = nil ) then begin
    prtCtx := GetParentContext();
    if Assigned(prtCtx) then
      Result := prtCtx.FindShortNamesForNameSpace(ANameSpace);
  end;
end;

function TCustomXsdSchemaParser.GetDocumentLocator(): IDocumentLocator;
begin
  Result := FDocumentLocator;
  if (Result = nil) and (FParentContext <> nil) then
    Result := GetParentContext().GetDocumentLocator();
end;

procedure TCustomXsdSchemaParser.SetDocumentLocator(const ALocator: IDocumentLocator);
begin
  FDocumentLocator := ALocator;
end;

procedure TCustomXsdSchemaParser.SetNotifier(ANotifier: TOnParserMessage);
begin
  FOnMessage := ANotifier;
end;

function TCustomXsdSchemaParser.FindShortNamesForNameSpaceLocal(const ANameSpace: string): TStrings;
var
  i : PtrInt;
begin
  i := FNameSpaceList.IndexOf(ANameSpace);
  if ( i >= 0 ) then
    Result := FNameSpaceList.Objects[i] as TStrings
  else
    Result := nil;
end;

function TCustomXsdSchemaParser.GetParentContext() : IParserContext;
begin
  Result := IParserContext(FParentContext);
end;

function TCustomXsdSchemaParser.GetSymbolTable() : ISDODataObject;
begin
  Result := FSymbols;
end;

function TCustomXsdSchemaParser.GetTargetModule() : ISDODataObject;
begin
  Result := FModule;
end;

function TCustomXsdSchemaParser.GetTargetNameSpace() : string;
begin
  Result := FTargetNameSpace;
end;

function TCustomXsdSchemaParser.GetXsShortNames() : TStrings;
begin
  Result := FXSShortNames;
end;

function TCustomXsdSchemaParser.ParseType(const AName, ATypeKind : string): ISDODataObject;
begin
  Result := InternalParseType(AName,nil);
end;

function TCustomXsdSchemaParser.ParseType(
  const AName : string;
  const ATypeNode : TDOMNode
) : ISDODataObject;
begin
  Result := InternalParseType(AName,ATypeNode);
end;

function TCustomXsdSchemaParser.InternalParseType(
  const AName : string;
  const ATypeNode : TDOMNode
): ISDODataObject;
var
  crsSchemaChild : IObjectCursor;
  typNd : TDOMNode;
  typName : string;
  embededType : Boolean;
  localTypeName : string;

  procedure Init();
  begin
    crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
  end;

  function ExtractTypeHint(AElement: TDOMNode): string;
  begin
    if not sdo_findCustomAttributeXsd(FXSShortNames,AElement,s_SDO_typeHint,Result) then
      Result := '';
  end;

  function FindTypeNode(out ASimpleTypeAlias : ISDODataObject) : Boolean;
  var
    nd, oldTypeNode : TDOMNode;
    crs : IObjectCursor;
    locStrFilter : string;
    locLocalName, locNamespace : string;
  begin
    ASimpleTypeAlias := nil;
    Result := True;
    if ( ATypeNode <> nil ) then
      typNd := ATypeNode
    else
      typNd := FindNamedNode(crsSchemaChild,localTypeName);
    if not Assigned(typNd) then
      raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['1',AName]);
    if SameText(ExtractNameFromQName(typNd.NodeName),s_element) then begin
      crs := CreateCursorOn(CreateAttributesCursor(typNd,cetRttiNode),ParseFilter(Format('%s = %s',[s_NODE_NAME,QuotedStr(s_type)]),TDOMNodeRttiExposer));
      crs.Reset();
      if crs.MoveNext() then begin
        nd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        ExplodeQName(nd.NodeValue,locLocalName,locNamespace);
        ASimpleTypeAlias := FindElementNS(locNamespace,locLocalName,nvtShortSynonym); //ASimpleTypeAlias := FindElement(ExtractNameFromQName(nd.NodeValue));
        if Assigned(ASimpleTypeAlias) then begin
          Result := False;
        end else begin
          oldTypeNode := typNd;
          typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue));
          if not Assigned(typNd) then
            raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['2',AName]);
          embededType := False;
          if ( typNd = oldTypeNode ) then begin
            typNd := FindNamedNode(crsSchemaChild,ExtractNameFromQName(nd.NodeValue),2);
            if not Assigned(typNd) then
              raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['2.1',AName]);
          end;
        end;
      end else begin
        //locStrFilter := Format('%s = %s or %s = %s ',[s_NODE_NAME,QuotedStr(s_complexType),s_NODE_NAME,QuotedStr(s_simpleType)]);
        locStrFilter := CreateQualifiedNameFilterStr(s_complexType,FXSShortNames) + ' or ' +
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames);
        crs := CreateCursorOn(CreateChildrenCursor(typNd,cetRttiNode),ParseFilter(locStrFilter,TDOMNodeRttiExposer));
        crs.Reset();
        if not crs.MoveNext() then begin
          raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeDefinitionNotFound,['3',AName]);
        end;
        typNd := (crs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        typName := ExtractNameFromQName(AName);
        embededType := True;
      end;
    end;
  end;

  function ParseComplexType():ISDODataObject;
  var
    locParser : TComplexTypeParser;
  begin
    locParser := TComplexTypeParser.Create(Self,typNd,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

  function ParseSimpleType():ISDODataObject;
  var
    locParser : TSimpleTypeParser;
  begin
    locParser := TSimpleTypeParser.Create(Self,typNd,typName,embededType);
    try
      Result := locParser.Parse();
    finally
      FreeAndNil(locParser);
    end;
  end;

  {function CreateTypeAlias(const ABase : ISDODataObject): ISDODataObject;
  begin
    Result := Module.createDataObject(s_Type);
    Result.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
    Result.setString(s_Name,ExtractNameFromQName(AName));
    Module.getList(s_Type).append(Result);
    Result.setDataObject(s_BaseType,ABase);
  end;}

  function CreateTypeAlias(const ABase : ISDODataObject): ISDODataObject;
  begin
    Result := Module.createDataObject(s_Variable);
    Result.setByte(s_ElementKind,ELEMENT_KIND_VARIABLE);
    Result.setString(s_Name,ExtractNameFromQName(AName));
    Module.getList(s_Variable).append(Result);
    Result.setDataObject(s_DataType,ABase);
  end;

  function CreateUnresolveType(): ISDODataObject;
  var
    locTypeLocalName, locTypeLocalSpaceShort, locTypeLocalSpaceExpanded : string;
  begin
    ExplodeQName(AName,locTypeLocalName,locTypeLocalSpaceShort);
    if not IsStrEmpty(locTypeLocalSpaceShort) then begin
      if not FindNameSpace(locTypeLocalSpaceShort,locTypeLocalSpaceExpanded) then
        locTypeLocalSpaceExpanded := locTypeLocalSpaceShort;
    end;
    Result := CreateUnresolvedType(SymbolTable,locTypeLocalSpaceExpanded,locTypeLocalName);
  end;

var
  frwType, aliasType : ISDODataObject;
  shortNameSpace, longNameSpace : string;
  typeModule : ISDODataObject;
  locTypeNodeFound : Boolean;
  i : Integer;
begin
  if not FImportParsed then
    ParseImportDocuments();
  DoOnMessage(mtInfo, Format(SERR_Parsing,[AName]));
  try
    embededType := False;
    aliasType := nil;
    typNd := nil;
    ExplodeQName(AName,localTypeName,shortNameSpace);
    if IsStrEmpty(shortNameSpace) then begin
      typeModule := FModule;
    end else begin
      if not FindNameSpace(shortNameSpace,longNameSpace) then
        raise EXsdParserAssertException.CreateFmt(SERR_UnableToResolveNamespace,[shortNameSpace]);
      typeModule := FindModule(SymbolTable,longNameSpace);
    end;
    if ( typeModule = nil ) then
      raise EXsdTypeNotFoundException.Create(AName);
    Result := FindInModule(typeModule,localTypeName);
    if (Result <> nil) and (not Result.getBoolean(s_Unresolved)) then
      Exit;
    Init();
    locTypeNodeFound := FindTypeNode(aliasType);
    if ( Result <> nil ) and ( typeModule = FModule ) and
       ( not Result.getBoolean(s_Unresolved) )
    then begin
      if locTypeNodeFound and (embededType <> Result.getBoolean(s_Embedded)) then
        Result := nil;
    end;
    if ( ( Result = nil ) or Result.getBoolean(s_Unresolved) ) and
       ( typeModule = FModule )
    then begin
      frwType := Result;
      Result := nil;
      Init();
      if locTypeNodeFound {FindTypeNode(aliasType)} then begin
        if (ExtractNameFromQName(typNd.NodeName)=s_complexType) then begin
          Result := ParseComplexType();
        end else if (ExtractNameFromQName(typNd.NodeName)=s_simpleType) then begin
          Result := ParseSimpleType();
        end;
        if not Assigned(Result) then
          raise EXsdTypeNotFoundException.CreateFmt(SERR_TypeNodeFoundButUnableToParseIt,[AName]);
      end else begin
        Result := CreateTypeAlias(aliasType);
      end;
      if ( frwType <> nil ) then begin
        i := indexOf(frwType,SymbolTable.getList(s_Unresolved));
        if (i >= 0) then
          SymbolTable.getList(s_Unresolved).delete(i);
      end;
    end;
  except
    on e : EXsdTypeNotFoundException do begin
      Result := CreateUnresolveType();
    end;
  end;
end;

procedure TCustomXsdSchemaParser.CreateImportParsers();
var
  crsSchemaChild : IObjectCursor;
  strFilter, locFileName, locNameSpace : string;
  importNode : TDOMElement;
  importDoc : TXMLDocument;
  locParser : IXsdParser;
  locOldCurrentModule : ISDODataObject;
  locContinue : Boolean;
  locLocator : IDocumentLocator;
begin
  if FImportParsed then
    Exit;
  locLocator := GetDocumentLocator();
  if (locLocator = nil) then
    Exit;

  if Assigned(FChildCursor) then begin
    locOldCurrentModule := SymbolTable.getDataObject(s_CurrentModule);
    try
      crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
      strFilter := CreateQualifiedNameFilterStr(s_import,FXSShortNames);
      crsSchemaChild := CreateCursorOn(crsSchemaChild,ParseFilter(strFilter,TDOMNodeRttiExposer));
      crsSchemaChild.Reset();
      while crsSchemaChild.MoveNext() do begin
        importNode := (crsSchemaChild.GetCurrent() as TDOMNodeRttiExposer).InnerObject as TDOMElement;
        if ( importNode.Attributes <> nil ) and ( importNode.Attributes.Length > 0 ) then begin
          locFileName := NodeValue(importNode.Attributes.GetNamedItem(s_schemaLocation));
          if ( not IsStrEmpty(locFileName) ) and
             locLocator.Find(locFileName,importDoc)
          then begin
            locNameSpace := NodeValue(importNode.Attributes.GetNamedItem(s_namespace));
            locContinue := IsStrEmpty(locNameSpace) or (FXsdParsers = nil) or (FXsdParsers.IndexOf(locNameSpace) = -1);//( SymbolTable.FindModule(locNameSpace) = nil );
            if locContinue then begin
              if (FXsdParsers = nil) then begin
                FXsdParsers := TStringList.Create();
                FXsdParsers.Duplicates := dupIgnore;
                FXsdParsers.Sorted := True;
              end;
              locParser := TCustomXsdSchemaParserClass(Self.ClassType).Create(
                             importDoc,
                             importDoc.DocumentElement,
                             SymbolTable,
                             Self as IParserContext
                           );
              FXsdParsers.AddObject(locNameSpace,TIntfObjectRef.Create(locParser));
              locParser.SetNotifier(FOnMessage);
              //locParser.ParseTypes();
            end;
          end;
        end;
      end;
    finally
      SymbolTable.setDataObject(s_CurrentModule,locOldCurrentModule);
    end;
  end;
end;

procedure TCustomXsdSchemaParser.ParseTypes();
var
  crsSchemaChild, typTmpCrs : IObjectCursor;
  typFilterStr : string;
  typNode : TDOMNode;
begin
  if Assigned(FChildCursor) then begin
    crsSchemaChild := FChildCursor.Clone() as IObjectCursor;
    typFilterStr := Format(
                      '%s or %s or %s',
                      [ CreateQualifiedNameFilterStr(s_complexType,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_simpleType,FXSShortNames),
                        CreateQualifiedNameFilterStr(s_element,FXSShortNames)
                      ]
                    );
    crsSchemaChild := CreateCursorOn(crsSchemaChild,ParseFilter(typFilterStr,TDOMNodeRttiExposer));
    crsSchemaChild.Reset();
    while crsSchemaChild.MoveNext() do begin
      typNode := (crsSchemaChild.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
      typTmpCrs := CreateAttributesCursor(typNode,cetRttiNode);
      if Assigned(typTmpCrs) then begin
        typTmpCrs.Reset();
        typTmpCrs := CreateCursorOn(typTmpCrs,ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_name)]),TDOMNodeRttiExposer));
        typTmpCrs.Reset();
        if typTmpCrs.MoveNext() then begin
          InternalParseType(
            (typTmpCrs.GetCurrent() as TDOMNodeRttiExposer).NodeValue,
            typNode
          );
        end;
      end;
    end;
    HandleUnresolvedLinks();
  end;
end;

procedure TCustomXsdSchemaParser.Prepare();
var
  locAttCursor : IObjectCursor;
  prntCtx : IParserContext;
  nd : TDOMNode;
  i : PtrInt;
  ls : TStrings;
begin
  if ( FSchemaNode.Attributes = nil ) or ( GetNodeListCount(FSchemaNode.Attributes) = 0 ) then
    raise EXsdParserAssertException.CreateFmt(SERR_SchemaNodeRequiredAttribute,[s_targetNamespace]);
  nd := FSchemaNode.Attributes.GetNamedItem(s_targetNamespace);
  if ( nd = nil ) then
    raise EXsdParserAssertException.CreateFmt(SERR_SchemaNodeRequiredAttribute,[s_targetNamespace]);
  FTargetNameSpace := nd.NodeValue;
  if IsStrEmpty(FModuleName) then
    FModuleName := ExtractIdentifier(FTargetNameSpace);
  if (FindModule(SymbolTable,s_xs) = nil) then
    AddXsdTypes(SymbolTable);
  FChildCursor := CreateChildrenCursor(FSchemaNode,cetRttiNode);

  locAttCursor := CreateAttributesCursor(FSchemaNode,cetRttiNode);
  BuildNameSpaceList(locAttCursor,FNameSpaceList);
  FXSShortNames := FindShortNamesForNameSpaceLocal(s_xs);
  prntCtx := GetParentContext();
  if ( FXSShortNames = nil ) then begin
    if ( prntCtx = nil ) then
      raise EXsdParserAssertException.CreateFmt(SERR_InvalidSchemaDoc_NamespaceNotFound,[s_xs]);
    FXSShortNames := prntCtx.FindShortNamesForNameSpace(s_xs);
    if ( FXSShortNames = nil ) then
      raise EXsdParserAssertException.CreateFmt(SERR_InvalidSchemaDoc_NamespaceNotFoundShort,[s_xs]);
  end;

  if Assigned(prntCtx) then begin
    for i:= 0 to Pred(FNameSpaceList.Count) do begin
      ls := prntCtx.FindShortNamesForNameSpace(FNameSpaceList[i]);
      if Assigned(ls) then
        (FNameSpaceList.Objects[i] as TStrings).AddStrings(ls);
    end;
  end;

  FModule := FindModule(SymbolTable,FTargetNameSpace);
  if ( FModule = nil ) then begin
    FModule := SymbolTable.createDataObject(s_Module);
    FModule.setString(s_Name,FModuleName);
    FModule.setString(s_NameSpace,FTargetNameSpace);
    SymbolTable.getList(s_Module).append(FModule);
  end;
  SymbolTable.setDataObject(s_CurrentModule,FModule);
end;

procedure TCustomXsdSchemaParser.HandleUnresolvedLinks();
var
  locCursor : ISDOCursor;
  locList, locUnresolvedList : ISDODataObjectList;
  locLink, locType, locUnresolved, locTmp : ISDODataObject;
  i, c, j : PtrInt;
begin
  locList := FSymbols.getList(s_UnresolvedLink);
  locCursor := locList.getCursor();
  locCursor.Reset();
  while locCursor.MoveNext() do begin
    locLink := locList.getDataObject();
    if locLink.getBoolean(s_Resolved) then
      Continue;
    locUnresolved := locLink.getDataObject(s_Target);
    locType := Find(FSymbols,locUnresolved.getString(s_NameSpace),locUnresolved.getString(s_Name));
    if (locType <> nil) and
       (not locType.getBoolean(s_Unresolved))
    then begin
      if SameText(locLink.getString(s_LinkKind),LINK_KIND_BASE_TYPE) then begin
        locLink.getDataObject(s_Element).setDataObject(s_BaseType,locType);
        locLink.setBoolean(s_Resolved,True);
      end else if SameText(locLink.getString(s_LinkKind),LINK_KIND_PROP_TYPE) then begin
        locTmp := locLink.getDataObject(Format('%s/%s[%s=%S]',[s_Element,s_Property,s_Name,QuotedStr(locLink.getString(s_Name))]));
        if (locTmp <> nil) then
          locTmp.setDataObject(s_DataType,locType);
        locLink.setBoolean(s_Resolved,True);
      end;
    end;
  end;
  c := locList.size();
  if (c > 0) then begin
    locUnresolvedList := FSymbols.getList(s_Unresolved);
    for i := (c - 1) downto 0 do begin
      locLink := locList.getDataObject(i);
      if locLink.getBoolean(s_Resolved) then begin
        j := indexOf(locLink.getDataObject(s_Target),locUnresolvedList);
        locList.delete(i);
        if (j >= 0) then
          locUnresolvedList.delete(j);
      end;
    end;
  end;
end;

function TCustomXsdSchemaParser.FindElementNS(
  const ANameSpace,
        ALocalName : string;
  const ASpaceType : TNameSpaceValueType
) : ISDODataObject;
var
  locNS : string;
begin
  if ( ASpaceType = nvtExpandValue ) then begin
    locNS := ANameSpace
  end else begin
    if not FindNameSpace(ANameSpace,locNS) then
      raise EXsdParserAssertException.CreateFmt(SERR_CannotResolveNamespace,[ANameSpace]);
  end;
  Result := Find(FSymbols,locNS,ALocalName);
end;

{ TXsdParser }

constructor TXsdParser.Create(
        ADoc : TXMLDocument;
        ASymbols : ISDODataObject;
  const AModuleName : string;
  const ANotifier : TOnParserMessage
);
var
  locName : string;
begin
  inherited Create(ADoc,ADoc.DocumentElement,ASymbols,nil);
  if Assigned(ANotifier) then
    FOnMessage := ANotifier;
  if not IsStrEmpty(AModuleName) then begin
    locName := ExtractIdentifier(AModuleName);
    if not IsStrEmpty(locName) then begin
      FModuleName := locName;
      Module.setString(s_Name,FModuleName);
    end;
  end;
end;

initialization
  TAbstractTypeParser.RegisterParser(TSimpleTypeParser);
  TAbstractTypeParser.RegisterParser(TComplexTypeParser);

finalization
  FreeAndNil(FTypeParserList);

end.
