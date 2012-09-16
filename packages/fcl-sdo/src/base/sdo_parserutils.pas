{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements XSD parser utils

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_parserutils;

interface

uses
  SysUtils, Classes
  {$IFNDEF FPC}, xmldom, sdo_win_xml{$ELSE},DOM{$ENDIF},
  sdo_cursor_intf, sdo_dom_cursors, xsd_consts, sdo_types;

type
  TNotFoundAction = ( nfaNone, nfaRaiseException );

const
  sNEW_LINE = sLineBreak;

type

  { TIntfObjectRef }

  TIntfObjectRef = class
  private
    FIntf: IInterface;
  public
    constructor Create(AIntf : IInterface);
    destructor Destroy();override;
    property Intf : IInterface read FIntf;
  end; 
  
  { TQualifiedNameObjectFilter }

  TQualifiedNameObjectFilter = class(TInterfacedObject,IObjectFilter)
  private
    FNameSpace : string;
    FName : string;
  protected
    function Evaluate(const AObject : TObject) : Boolean;
  public
    constructor Create(const AName,ANameSpace : string);
  end;

  function IsStrEmpty(Const AStr : String):Boolean;
  function ExtractIdentifier(const AValue : string) : string ;
  function GetToken(var ABuffer : string; const ADelimiter : string) : string;
{$IFDEF SDO_HANDLE_DOC}
  function EncodeLineBreak(const AInStr : string) : string;
  function DecodeLineBreak(const AInStr : string) : string;
{$ENDIF}

  function IsReservedKeyWord(const AValue : string):Boolean ;

  procedure ExtractNameSpaceShortNamesNested(
          ANode         : TDOMNode;
          AResList      : TStrings;
    const ANameSpace    : WideString
  );
  function CreateQualifiedNameFilterStr(
    const AName        : WideString;
          APrefixList  : TStrings
  ) : string;
  function ExtractNameFromQName(const AQName : string):string ;
  procedure ExtractNameSpaceShortNames(
          AAttribCursor   : IObjectCursor;
          AResList        : TStrings;
    const ANameSpace      : WideString;
    const ANotFoundAction : TNotFoundAction;
    const AClearBefore    : Boolean;
    const AExceptionClass : ExceptClass
  );
  function AddNameSpace(const AValue: string; ANameSpaceList : TStrings): TStrings;
  procedure BuildNameSpaceList(AAttCursor : IObjectCursor; ANameSpaceList : TStrings);
  procedure ExplodeQName(const AQName : string; out ALocalName, ANameSpace : string) ;
  
  function sdo_findCustomAttribute(
          AWsdlShortNames : TStrings;
          ANode      : TDOMNode;
    const AAttribute : string;
    out   AValue     : string
  ) : Boolean;
  function sdo_findCustomAttributeXsd(
          AXsdShortNames : TStrings;
          ANode      : TDOMNode;
    const AAttribute : string;
    out   AValue     : string
  ) : Boolean;

implementation

uses StrUtils, sdo_rtti_filters;

const LANGAGE_TOKEN : array[0..127] of string = (
  'ABSOLUTE', 'ABSTRACT', 'AND', 'ARRAY', 'AS', 'ASM',
  'BEGIN', 'BOOLEAN', 'BYTE',
  'CASE', 'CDECL', 'CHAR', 'CLASS', 'COMP', 'CONST', 'CONSTRUCTOR', 'CONTAINS',
  'CURRENCY', 'DEFAULT', 'DEPRECATED', 'DESTRUCTOR', 'DISPINTERFACE', 'DISPOSE', 'DIV', 'DO',
  'DOUBLE', 'DOWNTO', 'DYNAMIC', 'END', 'EXCEPT', 'EXIT', 'EXPORT', 'EXPORTS',
  'EXTERNAL', 'FALSE', 'FAR', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR',
  'FORWARD', 'FUNCTION', 'GOTO', 'ELSE', 'EXCEPT', 'EXTENDED',
  'IF', 'IMPLEMENTATION', 'IMPLEMENTS', 'IN', 'INHERITED', 'INLINE', 'INT64',
  'INITIALIZATION', 'INTEGER', 'INTERFACE', 'IS',
  'LABEL', 'LIBRARY', 'LOCAL', 'LONGINT', 'LONGWORD',
  'MOD', 'NEAR', 'NEW', 'NIL', 'NODEFAULT', 'NOT',
  'OBJECT', 'OF', 'OLEVARIANT', 'ON', 'OPERATOR', 'OR', 'OUT', 'OVERLOAD',
  'OVERRIDE','PACKAGE', 'PACKED', 'PASCAL', 'PCHAR', 'PRIVATE', 'PROCEDURE',
  'PROGRAM', 'PROPERTY', 'PROTECTED', 'PUBLIC', 'PUBLISHED',
  'RAISE', 'READ', 'REAL', 'RECORD', 'REGISTER', 'REINTRODUCE', 'REPEAT',
  'REQUIRES', 'RESOURCESTRING', 'RESULT', 'SAFECALL', 'SELF', 'SET', 'SHL',
  'SHORTINT', 'SHR', 'SINGLE', 'SMALLINT', 'STDCALL', 'STORED', 'STRING',
  'THEN', 'THREADVAR', 'TO', 'TRUE', 'TRY', 'TYPE', 'UNIT', 'UNTIL', 'USES',
  'VAR', 'VARARGS', 'VARIANT', 'VIRTUAL', 'WHILE', 'WIDECHAR', 'WITH', 'WORD',
  'WRITE', 'XOR'
);
const SDO_RESERVED_TOKEN : array[0..1] of string = ( 'Item', 'Item' );
function IsReservedKeyWord(const AValue : string):Boolean ;
begin
  Result := AnsiMatchText(AValue,LANGAGE_TOKEN) or
            AnsiMatchText(AValue,SDO_RESERVED_TOKEN);
end;

function IsStrEmpty(Const AStr : String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

function ExtractIdentifier(const AValue : string) : string ;
var
  i, c : Integer;
  s : string;
begin
  Result := '';
  s := Trim(AValue);
  c := Length(s);
  if ( c > 0 ) then begin
    if not ( s[1] in ['A'..'Z', 'a'..'z', '_'] ) then begin
      Result := '_';
    end;
    for i := 1 to c do begin
      if ( s[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] ) then begin
        Result := Result + s[i];
      end else begin
        if ( Length(Result) > 0 ) and ( Result[Length(Result)] <> '_' ) then begin
          Result := Result + '_';
        end;
      end;
    end;
    if (Length(Result) > 1) and (Result[Length(Result)] = '_') then
      Delete(Result,Length(Result),1);
  end;
end;

function GetToken(
  var   ABuffer    : string;
  const ADelimiter : string
) : string;
var
  locDelPos, locDelLength : Integer;
begin
  Result := '';
  if IsStrEmpty(ABuffer) then begin
    ABuffer := '';
  end else begin
    locDelPos := Pos(ADelimiter,ABuffer);
    if ( locDelPos < 1 ) then begin
      Result := ABuffer;
      ABuffer := '';
    end else begin
      locDelLength := Length(ADelimiter);
      if ( locDelPos = 1 ) then begin
        ABuffer := Copy(ABuffer,(locDelLength + 1),(Length(ABuffer) - locDelLength));
      end else begin
        Result := Copy(ABuffer,1,(locDelPos - 1));
        ABuffer := Copy(ABuffer,(locDelPos + locDelLength),(Length(ABuffer) - locDelLength));
      end;
    end;
  end;
end;

{$IFDEF SDO_HANDLE_DOC}
const
  REPLACE_CHAR_A = '#';  TARGET_SEQUENCE_A = sLineBreak;
  REPLACE_CHAR_B = '|';  TARGET_SEQUENCE_B = #10;

function EncodeLineBreak(const AInStr : string) : string;
begin
  Result :=
    StringReplace(
      StringReplace(AInStr,REPLACE_CHAR_A,(REPLACE_CHAR_A + REPLACE_CHAR_A),[rfReplaceAll]),
      TARGET_SEQUENCE_A,REPLACE_CHAR_A,[rfIgnoreCase,rfReplaceAll]
    );
  Result :=
    StringReplace(
      StringReplace(Result,REPLACE_CHAR_B,(REPLACE_CHAR_B + REPLACE_CHAR_B),[rfReplaceAll]),
      TARGET_SEQUENCE_B,REPLACE_CHAR_B,[rfIgnoreCase,rfReplaceAll]
    );
end;

function DecodeLineBreak(const AInStr : string) : string;
var
  i, c : PtrInt;
  pc : PChar;
  tmp, res : string;
begin
  res := '';
  pc := PChar(AInStr);
  i := 1;
  c := Length(AInStr);
  while ( i <= c ) do begin
    if ( pc^ = REPLACE_CHAR_B ) then begin
      if ( i < c ) then begin
        Inc(pc); Inc(i);
        if ( pc^ = REPLACE_CHAR_B ) then
          res := res + REPLACE_CHAR_B
        else
          res := res + TARGET_SEQUENCE_B + pc^;
      end else begin
        res := res + TARGET_SEQUENCE_B;
      end;
    end else begin
      res := res  + pc^;
    end;
    Inc(pc); Inc(i);
  end;

  tmp := res;
  res := '';
  pc := PChar(tmp);
  i := 1;
  c := Length(tmp);
  while ( i <= c ) do begin
    if ( pc^ = REPLACE_CHAR_A ) then begin
      if ( i < c ) then begin
        Inc(pc); Inc(i);
        if ( pc^ = REPLACE_CHAR_A ) then
          res := res + REPLACE_CHAR_A
        else
          res := res + TARGET_SEQUENCE_A + pc^;
      end else begin
        res := res + TARGET_SEQUENCE_A;
      end;
    end else begin
      res := res  + pc^;
    end;
    Inc(pc); Inc(i);
  end;

  Result := res;
end;
{$ENDIF SDO_HANDLE_DOC}

function ExtractNameFromQName(const AQName : string):string ;
var
  i : Integer;
begin
  Result := Trim(AQName);
  i := Pos(':',Result);
  if ( i > 0 ) then
    Result := Copy(Result,( i + 1 ), MaxInt);
end;

function CreateQualifiedNameFilterStr(
  const AName        : WideString;
        APrefixList  : TStrings
) : string;
var
  k : Integer;
  locStr : string;
  locWStr : WideString;
begin
  Result := '';
  if ( APrefixList.Count > 0 ) then begin
    for k := 0 to Pred(APrefixList.Count) do begin
      if IsStrEmpty(APrefixList[k]) then begin
        locWStr := ''
      end else begin
        locWStr := APrefixList[k] + ':';
      end;
      locWStr := locWStr + AName;
      locStr := s_NODE_NAME;
      Result := Result + ' or ' + locStr + ' = ' + QuotedStr(locWStr);
    end;
    if ( Length(Result) > 0 ) then begin
      Delete(Result,1,Length(' or'));
    end;
  end else begin
    Result := Format('%s = %s',[s_NODE_NAME,QuotedStr(AName)]);
  end;
end;

procedure ExtractNameSpaceShortNamesNested(
        ANode         : TDOMNode;
        AResList      : TStrings;
  const ANameSpace    : WideString
);
var
  nd : TDOMNode;
begin
  AResList.Clear();
  nd := ANode;
  while Assigned(nd) do begin
    if Assigned(nd.Attributes) and ( nd.Attributes.Length > 0 ) then begin
      ExtractNameSpaceShortNames(CreateAttributesCursor(nd,cetRttiNode),AResList,ANameSpace,nfaNone,False,nil);
    end;
    nd := nd.ParentNode;
  end;
end;

procedure ExtractNameSpaceShortNames(
        AAttribCursor   : IObjectCursor;
        AResList        : TStrings;
  const ANameSpace      : WideString;
  const ANotFoundAction : TNotFoundAction;
  const AClearBefore    : Boolean;
  const AExceptionClass : ExceptClass
);
var
  crs : IObjectCursor;
  locObj : TDOMNodeRttiExposer;
  wStr : WideString;
  i : Integer;
  ec : ExceptClass;
begin
  if AClearBefore then begin
    AResList.Clear();
  end;
  AAttribCursor.Reset();
  crs := CreateCursorOn(AAttribCursor,ParseFilter(Format('%s=%s',[s_NODE_VALUE,QuotedStr(ANameSpace)]),TDOMNodeRttiExposer));
  crs.Reset();
  if crs.MoveNext() then begin
    repeat
      locObj := crs.GetCurrent() as TDOMNodeRttiExposer;
      wStr := Trim(locObj.NodeName);
      i := AnsiPos(s_xmlns + ':',wStr);
      if ( i > 0 ) then begin
        i := AnsiPos(':',wStr);
        AResList.Add(Copy(wStr,( i + 1 ), MaxInt));
      end else begin
        if ( AResList.IndexOf('') = -1 ) then
          AResList.Add('');
      end;
    until not crs.MoveNext();
  end else begin
    if ( ANotFoundAction = nfaRaiseException ) then begin
      if Assigned(AExceptionClass) then
        ec := AExceptionClass
      else
        ec := Exception;
      raise ec.CreateFmt('Namespace not found : "%s"',[ANameSpace]);
    end;
  end;
end;

function sdo_findCustomAttribute(
        AWsdlShortNames : TStrings;
        ANode      : TDOMNode;
  const AAttribute : string;
  out   AValue     : string
) : Boolean;
var
  nd : TDOMNode;
  tmpCrs : IObjectCursor;
begin
  Result := False;
  tmpCrs := CreateCursorOn(
              CreateChildrenCursor(ANode,cetRttiNode),
              ParseFilter(CreateQualifiedNameFilterStr(s_document,AWsdlShortNames),TDOMNodeRttiExposer)
            );
  tmpCrs.Reset();
  if tmpCrs.MoveNext() then begin
    nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
    if nd.HasChildNodes() then begin
      tmpCrs := CreateCursorOn(
                  CreateChildrenCursor(nd,cetRttiNode),
                  ParseFilter(Format('%s=%s',[s_NODE_NAME,QuotedStr(s_customAttributes)]),TDOMNodeRttiExposer)
                );
      tmpCrs.Reset();
      if tmpCrs.MoveNext() then begin
        nd := (tmpCrs.GetCurrent() as TDOMNodeRttiExposer).InnerObject;
        if ( nd.Attributes <> nil ) then begin
          nd := nd.Attributes.GetNamedItem(AAttribute);
          if Assigned(nd) then begin
            Result := True;
            AValue := nd.NodeValue;
          end;
        end;
      end;
    end;
  end;
end;

function sdo_findCustomAttributeXsd(
        AXsdShortNames : TStrings;
        ANode      : TDOMNode;
  const AAttribute : string;
  out   AValue     : string
) : Boolean;
var
  nd : TDOMNode;
begin
  Result := False;
  if Assigned(ANode) and ( ANode.Attributes <> nil ) then begin
    nd := ANode.Attributes.GetNamedItem(Format('%s:%s',[s_SDO,AAttribute]));
    if Assigned(nd) then begin
      Result := True;
      AValue := nd.NodeValue;
    end;
  end;
end;

procedure ExplodeQName(const AQName : string; out ALocalName, ANameSpace : string) ;
var
  i : PtrInt;
begin
  i := Pos(':',AQName);
  if ( i > 0 ) then begin
    ANameSpace := Copy(AQName,1,Pred(i));
    ALocalName := Copy(AQName,Succ(i),Length(AQName));
  end else begin
    ANameSpace := '';
    ALocalName := AQName;
  end;
end;

function AddNameSpace(const AValue: string; ANameSpaceList : TStrings): TStrings;
var
  i : PtrInt;
  s : string;
  ls : TStringList;
begin
  s := Trim(AValue);
  i := ANameSpaceList.IndexOf(s);
  if ( i < 0 ) then begin
    i := ANameSpaceList.Add(s);
    ls := TStringList.Create();
    ANameSpaceList.Objects[i] := ls;
    ls.Duplicates := dupIgnore;
    ls.Sorted := True;
    Result := ls;
  end else begin
    Result := ANameSpaceList.Objects[i] as TStrings;
  end;
end;

procedure BuildNameSpaceList(AAttCursor : IObjectCursor; ANameSpaceList : TStrings);
var
  locObj : TDOMNodeRttiExposer;
  locNameSpace, locNameSpaceShort : string;
  tmpXmlNs : string;
  found : Boolean;
begin
  if Assigned(AAttCursor) then begin
    tmpXmlNs := s_xmlns + ':';
    AAttCursor.Reset();
    while AAttCursor.MoveNext() do begin
      found := False;
      locObj := AAttCursor.GetCurrent() as TDOMNodeRttiExposer;
      if AnsiSameText(s_xmlns,locObj.NodeName) then begin
        found := True;
        locNameSpace := locObj.NodeValue;
        locNameSpaceShort := '';
      end else if AnsiStartsText(tmpXmlNs,locObj.NodeName) then begin
        found := True;
        locNameSpace := locObj.NodeValue;
        locNameSpaceShort := locObj.NodeName;
        locNameSpaceShort := Copy(locNameSpaceShort,Pos(':',locNameSpaceShort) + 1, Length(locNameSpaceShort));
      end;
      if found then
        AddNameSpace(locNameSpace,ANameSpaceList).Add(locNameSpaceShort);
    end;
  end;
end;


{ TQualifiedNameObjectFilter }

function TQualifiedNameObjectFilter.Evaluate(const AObject: TObject): Boolean;
var
  locObj : TDOMNodeRttiExposer;
  startPos, i : PtrInt;
  shortNameSpace : string;
  locContinue : Boolean;
  tmpNode : TDOMNode;
begin
  Result := False;
  if ( AObject <> nil ) then begin
    locObj := TDOMNodeRttiExposer(AObject);
    i := Length(FName);
    startPos := ( Length(locObj.NodeName) - i + 1 );
    if ( startPos > 0 ) and ( FName = Copy(locObj.NodeName,startPos,i) ) then begin
      if ( startPos = 1 ) then begin
        shortNameSpace := 'xmlns';
        locContinue := True;
      end else begin
        locContinue := ( startPos > 2 ) and ( locObj.NodeName[startPos-1] = ':' );
        if locContinue then
          shortNameSpace := 'xmlns:' + Copy(locObj.NodeName,1,( startPos - 2 ));
      end;
      if locContinue then begin
        if ( locObj.InnerObject.Attributes <> nil ) then begin
          tmpNode := locObj.InnerObject.Attributes.GetNamedItem(shortNameSpace);
          if ( tmpNode <> nil ) and ( tmpNode.NodeValue = FNameSpace ) then
            Result := True;
        end;
      end;
    end;
  end;
end;

constructor TQualifiedNameObjectFilter.Create(const AName, ANameSpace: string);
begin
  FName := AName;
  FNameSpace := ANameSpace;;
end;

{ TIntfObjectRef }

constructor TIntfObjectRef.Create(AIntf: IInterface);
begin
  Assert(Assigned(AIntf));
  FIntf := AIntf;
end;

destructor TIntfObjectRef.Destroy();
begin
  FIntf := nil;
  inherited Destroy();
end;

end.
