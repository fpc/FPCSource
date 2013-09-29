{   Parser of the CLDR collation xml files.

    Copyright (c) 2013 by Inoussa OUEDRAOGO

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit cldrxml;

{$mode objfpc}{$H+}
{$TypedAddress on}
interface

uses
  Classes, SysUtils, DOM,
  cldrhelper;

  procedure ParseInitialDocument(ASequence : POrderedCharacters; ADoc : TDOMDocument);overload;
  procedure ParseInitialDocument(ASequence : POrderedCharacters; AFileName : string);overload;

  procedure ParseCollationDocument(
    ADoc       : TDOMDocument;
    ACollation : TCldrCollation;
    AMode      : TCldrParserMode
  );overload;
  procedure ParseCollationDocument(
    const AFileName  : string;
          ACollation : TCldrCollation;
          AMode      : TCldrParserMode
  );overload;

  procedure ParseCollationDocument(
    const AFileName  : string;
          ACollation : TCldrCollationItem;
          AType      : string
  );overload;
  procedure ParseCollationDocument(
    ADoc       : TDOMDocument;
    ACollation : TCldrCollationItem;
    AType      : string
  );overload;

resourcestring
  sCaseNothandled = 'This case is not handled : "%s", Position = %d.';
  sCodePointExpected = 'Code Point node expected as child at this position "%d".';
  sCollationsNodeNotFound = '"collations" node not found.';
  sCollationTypeNotFound = 'collation "Type" not found : "%s".';
  sHexAttributeExpected = '"hex" attribute expected at this position "%d".';
  sInvalidResetClause = 'Invalid "Reset" clause.';
  sNodeNameAssertMessage = 'Expected NodeName "%s", got "%s".';
  sRulesNodeNotFound = '"rules" node not found.';
  sTextNodeChildExpected = '(Child) text node expected at this position "%d", but got "%s".';
  sUniqueChildNodeExpected = 'Unique child node expected at this position "%d".';
  sUnknownResetLogicalPosition = 'Unknown reset logical position : "%s".';

implementation
uses
  typinfo, XMLRead, XPath, Helper, unicodeset;

const
  s_AT     = 'at';
  //s_BEFORE = 'before';
  s_CODEPOINT = 'codepoint';
  s_COLLATION = 'collation';
  s_COLLATIONS = 'collations';
  s_CONTEXT = 'context';
  //s_DEFAULT    = 'default';
  s_EXTEND = 'extend';
  s_HEX       = 'hex';
  s_POSITION = 'position';
  s_RESET = 'reset';
  s_RULES = 'rules';
  //s_STANDART = 'standard';
  s_TYPE     = 'type';

procedure CheckNodeName(ANode : TDOMNode; const AExpectedName : DOMString);
begin
  if (ANode.NodeName <> AExpectedName) then
    raise Exception.CreateFmt(sNodeNameAssertMessage,[AExpectedName,ANode.NodeName]);
end;

function CharToReorderWeigthKind(const AChar : Char) : TReorderWeigthKind;inline;
begin
  case AChar of
    'p' : Result := TReorderWeigthKind.PriMary;
    's' : Result := TReorderWeigthKind.Secondary;
    't' : Result := TReorderWeigthKind.Tertiary;
    'i' : Result := TReorderWeigthKind.Identity;
    else
     Result := TReorderWeigthKind.Identity;
  end;
end;

function DomString2UnicodeCodePointArray(const AValue : DOMString): TUnicodeCodePointArray;
var
  u4str : UCS4String;
  k : Integer;
begin
  if (Length(AValue) = 0) then
    exit(nil);
  if (Length(AValue) = 1) then begin
    SetLength(Result,1);
    Result[0] := Ord(AValue[1])
  end else begin
    u4str := WideStringToUCS4String(AValue);
    k := Length(u4str) - 1; // remove the last #0
    SetLength(Result,k);
    for k := 0 to k - 1 do
      Result[k] := u4str[k];
  end;
end;

function TryStrToLogicalReorder(
  const AValue  : string;
  out   AResult : TReorderLogicalReset
) : Boolean;
var
  s : string;
  i : Integer;
begin
  s := StringReplace(AValue,' ','',[rfReplaceAll]);
  s := StringReplace(s,'_','',[rfReplaceAll]);
  i := GetEnumValue(TypeInfo(TReorderLogicalReset),s);
  Result := (i > -1);
  if Result then
    AResult := TReorderLogicalReset(i);
end;

function ParseStatement(
      ARules         : TDOMElement;
      AStartPosition : Integer;
      AStatement     : PReorderSequence;
  var ANextPos       : Integer
) : Boolean;
var
  startPosition : Integer;
  statement : PReorderSequence;
  elementActualCount : Integer;
  list : TDOMNodeList;
  inBlock : Boolean;

  procedure SkipComments();
  begin
    while (startPosition < list.Count) do begin
      if (list[startPosition].NodeType <> COMMENT_NODE) then
        Break;
      Inc(startPosition);
    end;
  end;

  function parse_reset() : Integer;
  var
    n, t : TDOMNode;
    s : string;
    logicalPos : TReorderLogicalReset;
  begin
    SkipComments();
    n := list[startPosition];
    CheckNodeName(n,s_RESET);
    if n.HasChildNodes() then begin
      n := n.FirstChild;
      if (n.NodeType = TEXT_NODE) then begin
        statement^.Reset := DomString2UnicodeCodePointArray(Trim(TDOMText(n).Data));
        Result := startPosition+1;
      end else begin
        if not TryStrToLogicalReorder(n.NodeName,logicalPos) then
          raise Exception.CreateFmt(sUnknownResetLogicalPosition,[n.NodeName]);
        statement^.LogicalPosition := logicalPos;
        Result := startPosition+1;
      end;
    end else if not n.HasChildNodes() then begin
      if (list[startPosition+1].NodeName = s_POSITION) then begin
        s := list[startPosition+1].Attributes.GetNamedItem(s_AT).NodeValue;
        if not TryStrToLogicalReorder(s,logicalPos) then
          raise Exception.CreateFmt(sUnknownResetLogicalPosition,[s]);
        statement^.LogicalPosition := logicalPos;
        Result := startPosition+2;
      end else begin
        t := list[startPosition+1];
        {if (t.NodeType <> TEXT_NODE) then
          raise Exception.CreateFmt(sTextNodeChildExpected,[(startPosition+1),(t.NodeName+'('+t.ClassName+')')]);}
        if (t.NodeType = TEXT_NODE) then
          statement^.Reset := DomString2UnicodeCodePointArray(Trim(TDOMText(t).Data))
        else
          statement^.Reset := DomString2UnicodeCodePointArray(' ');
        Result := startPosition+2;
      end;
    end;
    if (statement^.LogicalPosition = TReorderLogicalReset.None) and
      (Length(statement^.Reset) = 0)
    then
      raise Exception.Create(sInvalidResetClause);
  end;

  procedure EnsureElementLength(const ALength : Integer);
  var
    k, d : Integer;
  begin
    k := Length(statement^.Elements);
    if (k < ALength) then begin
      k := ALength;
      if (k = 0) then begin
        k := 50;
      end else begin
        if (k < 10) then
          d := 10
        else
          d := 2;
        k := k * d;
      end;
     SetLength(statement^.Elements,k);
    end;
  end;

  procedure AddElement(
    const AChars      : array of UCS4Char;
    const AWeigthKind : TReorderWeigthKind;
    const AContext    : DOMString
  );overload;
  var
    kp : PReorderUnit;
    k : Integer;
  begin
    EnsureElementLength(elementActualCount+1);
    kp := @statement^.Elements[elementActualCount];
    SetLength(kp^.Characters,Length(AChars));
    for k := 0 to Length(AChars) - 1 do
     kp^.Characters[k] := AChars[k];
    kp^.WeigthKind := AWeigthKind;
    elementActualCount := elementActualCount + 1;
    if (AContext <> '') then
      kp^.Context := DomString2UnicodeCodePointArray(AContext);
  end;

  procedure ReadChars(
        ANode    : TDOMNode;
        APos     : Integer;
    var AChars   : UCS4String
  );
  var
    t : TDOMNode;
    u4str : UCS4String;
    s : DOMString;
  begin
    if not ANode.HasChildNodes() then begin
      SetLength(AChars,1);
      AChars[0] := Ord(UnicodeChar(' '));
      exit;
      //raise Exception.CreateFmt(sCodePointExpected + ANode.ClassName,[APos]);
    end;
    t := ANode.FindNode(s_CODEPOINT);
    if (t = nil) then begin
      if (ANode.ChildNodes.Count <> 1) then
        raise Exception.CreateFmt(sUniqueChildNodeExpected,[APos]);
      t := ANode.ChildNodes[0];
      if not t.InheritsFrom(TDOMText) then
        raise Exception.CreateFmt(sTextNodeChildExpected,[APos,(t.NodeName+'('+t.ClassName+')')]);
      s := TDOMText(t).Data;
      if (Length(s) = 1) then begin
        SetLength(AChars,1);
        AChars[0] := Ord(s[1]);
      end else begin
        u4str := WideStringToUCS4String(s);
        AChars := u4str;
        SetLength(AChars,Length(AChars)-1);
      end;
    end else begin
      t := t.Attributes.GetNamedItem(s_HEX);
      if (t = nil) then
        raise Exception.CreateFmt(sHexAttributeExpected,[APos]);
      SetLength(AChars,1);
      AChars[0] := StrToInt('$'+t.NodeValue);
    end
  end;

  procedure AddPrefixChars(const APrefix : array of UCS4Char; var ADest : TUnicodeCodePointArray);
  var
    k : Integer;
  begin
    k := Length(ADest);
    SetLength(ADest,(k+Length(APrefix)));
    Move(ADest[0],ADest[k+1],(SizeOf(k*ADest[0])));
    for k := 0 to k - 1 do
      ADest[k] := APrefix[k];
  end;

  function ReadNextItem(const APos : Integer) : Integer;
  var
    n, t : TDOMNode;
    contextStr : DOMString;
    w : TReorderWeigthKind;
    isSimpleCharTag : Boolean;
    simpleCharTag : AnsiChar;
    last : PReorderUnit;
    u4str : UCS4String;
    k : Integer;
  begin
    contextStr := '';
    Result := APos;
    n := list[APos];
    isSimpleCharTag := (Length(n.NodeName) = 1) and (Ord(n.NodeName[1])<=127);
    if isSimpleCharTag then begin
      simpleCharTag := AnsiChar(n.NodeName[1]);
      if (simpleCharTag = 'x') then begin
        inBlock := True;
        n := n.FirstChild;
        if (n.NodeName = s_CONTEXT) then begin
          if n.HasChildNodes() then begin
            t := n.FirstChild;
            if (t.NodeType = TEXT_NODE) then
              contextStr := TDOMText(t).Data;
          end;
          n := n.NextSibling;
        end;
        isSimpleCharTag := (Length(n.NodeName) = 1) and (Ord(n.NodeName[1])<=127);
        if isSimpleCharTag then
          simpleCharTag := AnsiChar(n.NodeName[1]);
      end;
    end;
    if isSimpleCharTag and (simpleCharTag in ['p','s','t','i']) then begin
      w := CharToReorderWeigthKind(AnsiChar(n.NodeName[1]));
      ReadChars(n,APos,u4str);
      AddElement(u4str,w,contextStr);
      Result := Result + 1;
      if not inBlock then
        exit;
      last := @statement^.Elements[elementActualCount-1];
      n := n.NextSibling;
      if (n <> nil) and (n.NodeName = s_EXTEND) then begin
        ReadChars(n,APos,u4str);
        SetLength(last^.ExpansionChars,Length(u4str));
        for k := 0 to Length(u4str) - 1 do
          last^.ExpansionChars[k] := u4str[k];
      end;
      exit;
    end;
    if (Length(n.NodeName) = 2) and (n.NodeName[2] = 'c') and
       (Ord(n.NodeName[1])<=127) and (AnsiChar(n.NodeName[1]) in ['p','s','t','i'])
    then begin
      w := CharToReorderWeigthKind(AnsiChar(n.NodeName[1]));
      ReadChars(n,APos,u4str);
      for k := Low(u4str) to High(u4str) do
        AddElement(u4str[k],w,contextStr);
      Result := Result + 1;
      exit;
    end;
    raise Exception.CreateFmt(sCaseNothandled,[n.NodeName,APos]);
  end;

var
  i, c : Integer;
  n : TDOMNode;
begin
  Result := False;
  inBlock := False;
  elementActualCount := 0;
  if (AStartPosition <= 0) then
    startPosition := 0
  else
    startPosition := AStartPosition;
  i := startPosition;
  list := ARules.ChildNodes;
  c := list.Count;
  if (c <= i) then
    exit;
  statement := AStatement;
  statement^.Clear();
  n := list[i];
  i := parse_reset();
  while (i < c) do begin
    n := list[i];
    if (n.NodeName = s_RESET) then
      Break;
    i := ReadNextItem(i);
  end;
  SetLength(statement^.Elements,elementActualCount);
  Result := (i > startPosition);
  if Result then
    ANextPos := i;
end;

procedure ParseInitialDocument(ASequence : POrderedCharacters; ADoc : TDOMDocument);
var
  n : TDOMNode;
  rulesElement : TDOMElement;
  i, c, nextPost : Integer;
  statement : TReorderSequence;
  p : PReorderUnit;
begin
  n := ADoc.DocumentElement.FindNode(s_RULES);
  if (n = nil) then
    raise Exception.Create(sRulesNodeNotFound);
  rulesElement := n as TDOMElement;
  c := rulesElement.ChildNodes.Count;
  ASequence^.Clear();
  SetLength(ASequence^.Data,c+100);
  nextPost := 0;
  i := 0;
  while (i < c) do begin
    statement.Clear();
    if not ParseStatement(rulesElement,i,@statement,nextPost) then
      Break;
    i := nextPost;
    try
      ASequence^.ApplyStatement(@statement);
    except
      on e : Exception do begin
        e.Message := Format('%s  Position = %d',[e.Message,i]);
        raise;
      end;
    end;
  end;
  if (ASequence^.ActualLength > 0) then begin
    p := @ASequence^.Data[0];
    for i := 0 to ASequence^.ActualLength - 1 do begin
      p^.Changed := False;
      Inc(p);
    end;
  end;
end;

procedure ParseInitialDocument(ASequence : POrderedCharacters; AFileName : string);
var
  doc : TXMLDocument;
begin
  ReadXMLFile(doc,AFileName);
  try
    ParseInitialDocument(ASequence,doc);
  finally
    doc.Free();
  end;
end;

function EvaluateXPathStr(const AExpression : string; AContextNode : TDOMNode): DOMString;
var
  xv : TXPathVariable;
begin
  xv := EvaluateXPathExpression(AExpression,AContextNode);
  try
    if (xv <> nil) then
      Result := xv.AsText
    else
      Result := '';
  finally
    xv.Free();
  end;
end;

function ParseDeletion(
  const APattern  : DOMString;
        ASequence : PReorderSequence
) : Integer;
var
  r : array of TReorderUnit;
  c, i : Integer;
  uset : TUnicodeSet;
  it : TUnicodeSet.TIterator;
  p : PReorderUnit;
begin
  if (APattern = '') then
    exit(0);
  it := nil;
  uset := TUnicodeSet.Create();
  try
    uset.AddPattern(APattern);
    it := uset.CreateIterator();
    c := 0;
    it.Reset();
    while it.MoveNext() do begin
      Inc(c);
    end;
    SetLength(r,c);
    p := @r[0];
    i := 0;
    it.Reset();
    while it.MoveNext() do begin
      p^.Clear();
      p^.WeigthKind := TReorderWeigthKind.Deletion;
      p^.Characters := Copy(it.GetCurrent());
      Inc(p);
      Inc(i);
    end;
    ASequence^.Clear();
    ASequence^.Elements := r;
  finally
    it.Free();
    uset.Free();
  end;
  SetLength(r,0);
end;

procedure ParseCollationItem(
  ACollationNode : TDOMElement;
  AItem          : TCldrCollationItem;
  AMode          : TCldrParserMode
);
var
  n : TDOMNode;
  rulesElement : TDOMElement;
  i, c, nextPos : Integer;
  statementList : TReorderSequenceArray;
  sal : Integer;//statement actual length
  statement : PReorderSequence;
  s : DOMString;
begin
  AItem.TypeName := ACollationNode.GetAttribute(s_TYPE);
  AItem.Base := EvaluateXPathStr('base',ACollationNode);
  AItem.Backwards := (EvaluateXPathStr('settings/@backwards',ACollationNode) = 'on');
  if AItem.Backwards then
    AItem.ChangedFields := AItem.ChangedFields + [TCollationField.BackWard];
  AItem.Rules := nil;
  if (AMode = TCldrParserMode.FullParsing) then begin
    SetLength(statementList,15);
    sal := 0;
    statement := @statementList[0];
    s := EvaluateXPathStr('suppress_contractions',ACollationNode);
    if (s <> '') then begin
      if (ParseDeletion(s,statement) > 0) then begin
        Inc(sal);
        Inc(statement);
      end else begin
        statement^.Clear();
      end;
    end;
    n := ACollationNode.FindNode(s_RULES);
    if (n <> nil) then begin
      rulesElement := n as TDOMElement;
      c := rulesElement.ChildNodes.Count;
      nextPos := 0;
      i := 0;
      while (i < c) do begin
        statement^.Clear();
        if not ParseStatement(rulesElement,i,statement,nextPos) then
          Break;
        i := nextPos;
        Inc(statement);
        Inc(sal);
        if (sal >= Length(statementList)) then begin
          SetLength(statementList,(sal*2));
          statement := @statementList[(sal-1)];
        end;
      end;
    end;
    SetLength(statementList,sal);
    AItem.Rules := statementList;
  end;
end;

procedure ParseCollationDocument(
  ADoc       : TDOMDocument;
  ACollation : TCldrCollation;
  AMode      : TCldrParserMode
);
var
  n : TDOMNode;
  collationsElement : TDOMElement;
  i, c : Integer;
  item : TCldrCollationItem;
  nl : TDOMNodeList;
begin
  n := ADoc.DocumentElement.FindNode(s_COLLATIONS);
  if (n = nil) then
    raise Exception.Create(sCollationsNodeNotFound);
  collationsElement := n as TDOMElement;
  ACollation.Clear();
  ACollation.Language := EvaluateXPathStr('identity/language/@type',ADoc.DocumentElement);
  ACollation.Version := EvaluateXPathStr('identity/version/@number',ADoc.DocumentElement);
  ACollation.DefaultType := EvaluateXPathStr('collations/default/@type',ADoc.DocumentElement);
  if collationsElement.HasChildNodes() then begin
    nl := collationsElement.ChildNodes;
    c := nl.Count;
    item := nil;
    try
      for i := 0 to c - 1 do begin
        n := nl[i];
        if (n.NodeName = s_COLLATION) then begin
          item := TCldrCollationItem.Create();
          ParseCollationItem((n as TDOMElement),item,AMode);
          ACollation.Add(item);
          item := nil;
        end
      end;
    except
      FreeAndNil(item);
      raise;
    end;
  end;
end;

procedure ParseCollationDocument(
  ADoc       : TDOMDocument;
  ACollation : TCldrCollationItem;
  AType      : string
);
var
  xv : TXPathVariable;
begin
  xv := EvaluateXPathExpression(Format('collations/collation[@type=%s]',[QuotedStr(AType)]),ADoc.DocumentElement);
  try
    if (xv.AsNodeSet.Count = 0) then
      raise Exception.CreateFmt(sCollationTypeNotFound,[AType]);
    ACollation.Clear();
    ParseCollationItem((TDOMNode(xv.AsNodeSet[0]) as TDOMElement),ACollation,TCldrParserMode.FullParsing);
  finally
    xv.Free();
  end
end;

function ReadXMLFile(f: TStream) : TXMLDocument;
var
  src : TXMLInputSource;
  parser: TDOMParser;
begin
  src := TXMLInputSource.Create(f);
  Result := TXMLDocument.Create;
  parser := TDOMParser.Create();
  try
    parser.Options.IgnoreComments := True;
    parser.Parse(src, Result);
  finally
    src.Free();
    parser.Free;
  end;
end;

function ReadXMLFile(const AFilename: String) : TXMLDocument;
var
  FileStream: TStream;
begin
  Result := nil;
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    Result := ReadXMLFile(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure ParseCollationDocument(
  const AFileName  : string;
        ACollation : TCldrCollation;
        AMode      : TCldrParserMode
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AFileName);
  try
    ParseCollationDocument(doc,ACollation,AMode);
    ACollation.LocalID := ExtractFileName(ChangeFileExt(AFileName,''));
  finally
    doc.Free();
  end;
end;

procedure ParseCollationDocument(
  const AFileName  : string;
        ACollation : TCldrCollationItem;
        AType      : string
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AFileName);
  try
    ParseCollationDocument(doc,ACollation,AType);
  finally
    doc.Free();
  end;
end;

end.
