{   Parser of the CLDR collation xml files.

    Copyright (c) 2013, 2014, 2015 by Inoussa OUEDRAOGO

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

{ The procedure whoses names lasted by 'XML' (ParseInitialDocumentXML,
  ParseCollationDocumentXML, ...) are for older CLDR versions (CDLR <= 23); The
  old version was unsing a XML syntax for collation's rules specifications.
  The new versions (and going forward) will be using the text syntax.
}

unit cldrxml;

{$mode delphi}{$H+}
{$TypedAddress on}
interface

uses
  Classes, SysUtils, DOM,
  cldrhelper;

type

  { TCldrCollationFileLoader }

  TCldrCollationFileLoader = class(TInterfacedObject,ICldrCollationLoader)
  private
    FPath : string;
  private
    procedure SetPath(APath : string);
    function BuildFileName(ALanguage  : string) : string;
    procedure CheckFile(AFileName : string);
  protected
    procedure LoadCollation(
      const ALanguage  : string;
            ACollation : TCldrCollation;
            AMode      : TCldrParserMode
    );
    procedure LoadCollationType(
      const ALanguage,
            ATypeName : string;
            AType     : TCldrCollationItem
    );
  public
    constructor Create(APath : string);
  end;

  { TCldrCollationStreamLoader }

  TCldrCollationStreamLoader = class(TInterfacedObject,ICldrCollationLoader)
  private
    FLanguages : array of string;
    FStreams   : array of TStream;
  private
    procedure CheckContent(ALanguage : string);
    function IndexOf(ALanguage : string) : Integer;
  protected
    procedure LoadCollation(
      const ALanguage  : string;
            ACollation : TCldrCollation;
            AMode      : TCldrParserMode
    );
    procedure LoadCollationType(
      const ALanguage,
            ATypeName : string;
            AType     : TCldrCollationItem
    );
  public
    constructor Create(
      const ALanguages : array of string;
      const AStreams   : array of TStream
    );
    destructor Destroy();override;
  end;

  procedure ParseInitialDocumentXML(ASequence : POrderedCharacters; ADoc : TDOMDocument);overload;
  procedure ParseInitialDocumentXML(ASequence : POrderedCharacters; AFileName : string);overload;

  procedure ParseCollationDocumentXML(
    ADoc       : TDOMDocument;
    ACollation : TCldrCollation;
    AMode      : TCldrParserMode
  );overload;
  procedure ParseCollationDocumentXML(
    ADoc       : TDOMDocument;
    ACollation : TCldrCollationItem;
    AType      : string
  );overload;
  procedure ParseCollationDocumentXML(
    const AFileName  : string;
          ACollation : TCldrCollation;
          AMode      : TCldrParserMode
  );overload;
  procedure ParseCollationDocumentXML(
    const AFileName  : string;
          ACollation : TCldrCollationItem;
          AType      : string
  );overload;

  //-----------------------------------------------------
  procedure ParseCollationDocument2(
    ADoc       : TDOMDocument;
    ACollation : TCldrCollation;
    AMode      : TCldrParserMode
  );overload;
  procedure ParseCollationDocument2(
    const AFileName  : string;
          ACollation : TCldrCollation;
          AMode      : TCldrParserMode
  );overload;
  procedure ParseCollationDocument2(
    AStream    : TStream;
    ACollation : TCldrCollation;
    AMode      : TCldrParserMode
  );overload;

  procedure ParseCollationDocument2(
    const AFileName  : string;
          ACollation : TCldrCollationItem;
          AType      : string
  );overload;
  procedure ParseCollationDocument2(
    ADoc       : TDOMDocument;
    ACollation : TCldrCollationItem;
    AType      : string
  );overload;
  procedure ParseCollationDocument2(
    AStream    : TStream;
    ACollation : TCldrCollationItem;
    AType      : string
  );overload;

implementation
uses
  typinfo, RtlConsts, XMLRead, XPath, Helper, unicodeset, cldrtxt;

const
  s_ALT    = 'alt';
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

  s_CR = 'cr';

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

function ParseStatementXML(
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

procedure ParseInitialDocumentXML(ASequence : POrderedCharacters; ADoc : TDOMDocument);
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
    if not ParseStatementXML(rulesElement,i,@statement,nextPost) then
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

procedure ParseInitialDocumentXML(ASequence : POrderedCharacters; AFileName : string);
var
  doc : TXMLDocument;
begin
  ReadXMLFile(doc,AFileName);
  try
    ParseInitialDocumentXML(ASequence,doc);
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
  c : Integer;
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
    it.Reset();
    while it.MoveNext() do begin
      p^.Clear();
      p^.WeigthKind := TReorderWeigthKind.Deletion;
      p^.Characters := Copy(it.GetCurrent());
      Inc(p);
    end;
    ASequence^.Clear();
    ASequence^.Elements := r;
  finally
    it.Free();
    uset.Free();
  end;
  r := nil;
  Result := c;
end;

procedure ParseCollationItemXML(
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
    AItem.ChangedFields := AItem.ChangedFields + [TCollationField.BackWards];
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
        if not ParseStatementXML(rulesElement,i,statement,nextPos) then
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

function NextPart(
  const ABuffer    : string;
  const AStartPos  : Integer;
  const ASeparator : Char;
    out ANextStart : Integer
) : string;
var
  c, sp, i : Integer;
begin
  c := Length(ABuffer);
  if (c < 1) or (AStartPos > c) then begin
    ANextStart := c+1;
    Result := '';
    exit;
  end;
  if (AStartPos > 0) then
    sp := AStartPos
  else
    sp := 1;
  i := sp;
  while (i <= c) do begin
    if (ABuffer[i] = ASeparator) then
      break;
    i := i+1;
  end;
  Result := Copy(ABuffer,sp,(i-sp));
  if (i <= c) then
    i := i+1;
  ANextStart := i;
end;

procedure HandleSetting_Import(
  AItem    : TCldrCollationItem;
  ASetting : PSettingRec
);
var
  buffer, lang, col, s : UTF8String;
  i, ns : Integer;
begin
  if (Length(ASetting^.Values) <> 1) then begin
    buffer := '';
    if (Length(ASetting^.Values) > 0) then begin
      for i := 0 to Length(ASetting^.Values)-1 do
        buffer := Format('%s + "%s"',[ASetting^.Values[i]]);
    end;
    raise Exception.CreateFmt(sInvalidImportStatement,[buffer]);
  end;
  buffer := ASetting^.Values[0];
  lang := NextPart(buffer,1,'-',ns);
  i := ns;
  col := '';
  s := NextPart(buffer,i,'-',ns);
  if (s <> '') then begin
    if (s <> 'u') then
      raise Exception.CreateFmt(sInvalidImportStatement,[buffer]);
    i := ns;
    s := NextPart(buffer,i,'-',ns);
    if (s <> 'co') then
      raise Exception.CreateFmt(sInvalidImportStatement,[buffer]);
    s := Trim(Copy(buffer,ns,(Length(buffer)-ns+1)));
    if (s = '') then
      raise Exception.CreateFmt(sInvalidImportStatement,[buffer]);
    col := s;
  end;
  if (col = '') then
    col := COLLATION_ITEM_DEFAULT;
  if (LowerCase(lang) = 'und') then
    lang := 'root';
  AItem.Imports.Add(lang,col);
  ASetting^.Understood := True;
end;

procedure HandleSetting_Backwards(
  AItem    : TCldrCollationItem;
  ASetting : PSettingRec
);
var
  buffer : UTF8String;
  i : Integer;
begin
  if (Length(ASetting^.Values) <> 1) then begin
    buffer := '';
    if (Length(ASetting^.Values) > 0) then begin
      for i := 0 to Length(ASetting^.Values)-1 do
        buffer := Format('%s + "%s"',[ASetting^.Values[i]]);
    end;
    raise Exception.CreateFmt(sInvalidBackwardsStatement,[buffer]);
  end;
  if (ASetting^.Values[0] = '2') then
    AItem.Backwards := True
  else
    raise Exception.CreateFmt(
                      sInvalidSettingValue,
                      [SETTING_OPTION_STRINGS[ASetting^.OptionValue],ASetting^.Values[0]]
                    );
  AItem.ChangedFields := AItem.ChangedFields+[TCollationField.BackWards];
  ASetting^.Understood := True;
end;

procedure HandleSetting_Alternate(
  AItem    : TCldrCollationItem;
  ASetting : PSettingRec
);
var
  buffer : UTF8String;
  i : Integer;
begin
  if (Length(ASetting^.Values) <> 1) then begin
    buffer := '';
    if (Length(ASetting^.Values) > 0) then begin
      for i := 0 to Length(ASetting^.Values)-1 do
        buffer := Format('%s + "%s"',[ASetting^.Values[i]]);
    end;
    raise Exception.CreateFmt(sInvalidAlternateStatement,[buffer]);
  end;
  buffer := UTF8String(LowerCase(UnicodeString(ASetting^.Values[0])));
  if (buffer = 'non-ignorable') then
    AItem.VariableWeight := ucaNonIgnorable
  else if (buffer = 'shifted') then
    AItem.VariableWeight := ucaShifted
  else
    raise Exception.CreateFmt(
                      sInvalidSettingValue,
                      [SETTING_OPTION_STRINGS[ASetting^.OptionValue],ASetting^.Values[0]]
                    );
  AItem.ChangedFields := AItem.ChangedFields+[TCollationField.Alternate];
  ASetting^.Understood := True;
end;

procedure HandleSetting_Normalization(
  AItem    : TCldrCollationItem;
  ASetting : PSettingRec
);
var
  buffer : UTF8String;
  i : Integer;
begin
  if (Length(ASetting^.Values) <> 1) then begin
    buffer := '';
    if (Length(ASetting^.Values) > 0) then begin
      for i := 0 to Length(ASetting^.Values)-1 do
        buffer := Format('%s + "%s"',[ASetting^.Values[i]]);
    end;
    raise Exception.CreateFmt(sInvalidNormalizationStatement,[buffer]);
  end;
  buffer := UTF8String(LowerCase(UnicodeString(ASetting^.Values[0])));
  if (buffer = 'off') then
    AItem.Normalization := False
  else if (buffer = 'on') then
    AItem.Normalization := True
  else
    raise Exception.CreateFmt(
                      sInvalidSettingValue,
                      [SETTING_OPTION_STRINGS[ASetting^.OptionValue],ASetting^.Values[0]]
                    );
  AItem.ChangedFields := AItem.ChangedFields+[TCollationField.Normalization];
  ASetting^.Understood := True;
end;

procedure HandleSetting_Strength(
  AItem    : TCldrCollationItem;
  ASetting : PSettingRec
);
var
  buffer : UTF8String;
  i : Integer;
begin
  if (Length(ASetting^.Values) <> 1) then begin
    buffer := '';
    if (Length(ASetting^.Values) > 0) then begin
      for i := 0 to Length(ASetting^.Values)-1 do
        buffer := Format('%s + "%s"',[ASetting^.Values[i]]);
    end;
    raise Exception.CreateFmt(sInvalidStrengthStatement,[buffer]);
  end;
  buffer := UTF8String(LowerCase(UnicodeString(ASetting^.Values[0])));
  if (buffer = '1') then
    AItem.Strength := TComparisonStrength.Primary
  else if (buffer = '2') then
    AItem.Strength := TComparisonStrength.Secondary
  else if (buffer = '3') then
    AItem.Strength := TComparisonStrength.Tertiary
  else if (buffer = '4') then
    AItem.Strength := TComparisonStrength.Quaternary
  else if (buffer = 'i') then
    AItem.Strength := TComparisonStrength.Identity
  else
    raise Exception.CreateFmt(
                      sInvalidSettingValue,
                      [SETTING_OPTION_STRINGS[ASetting^.OptionValue],ASetting^.Values[0]]
                    );
  AItem.ChangedFields := AItem.ChangedFields+[TCollationField.Strength];
  ASetting^.Understood := True;
end;

procedure HandleSetting_EMPTY_PROC(
  AItem    : TCldrCollationItem;
  ASetting : PSettingRec
);
begin
  //
end;

type
  TSettingHandlerProc = procedure (
                          AItem    : TCldrCollationItem;
                          ASetting : PSettingRec
                        );
const
  SETTING_HANDLERS : array[TSettingOption] of TSettingHandlerProc =(
    HandleSetting_EMPTY_PROC, HandleSetting_Strength,      HandleSetting_Alternate,
  //Unknown,                  Strength,                    Alternate,
    HandleSetting_Backwards,  HandleSetting_Normalization, HandleSetting_EMPTY_PROC,
  //Backwards,                Normalization,               CaseLevel,
    HandleSetting_EMPTY_PROC, HandleSetting_EMPTY_PROC,    HandleSetting_EMPTY_PROC,
  //CaseFirst,                HiraganaQ,                   NumericOrdering,
    HandleSetting_EMPTY_PROC, HandleSetting_EMPTY_PROC,    HandleSetting_Import,
  //Reorder,                  MaxVariable                  Import
    HandleSetting_EMPTY_PROC,
  //SuppressContractions has a special handling see Process_SuppressContractions
    HandleSetting_EMPTY_PROC
  //Optimize
  );
procedure HandleSettings(AItem : TCldrCollationItem);
var
  i, c : Integer;
  p : PSettingRec;
begin
  c := Length(AItem.Settings);
  if (c < 1) then
    exit;
  p := @AItem.Settings[0];
  for i := 0 to c-1 do begin
    SETTING_HANDLERS[p^.OptionValue](AItem,p);
    Inc(p);
  end;
end;

function Process_SuppressContractions(
  ASetting   : PSettingRec;
  AStatement : PReorderSequence
) : Boolean;
var
  buffer : UTF8String;
  i : Integer;
begin
  if (Length(ASetting^.Values) <> 1) then begin
    buffer := '';
    if (Length(ASetting^.Values) > 0) then begin
      for i := 0 to Length(ASetting^.Values)-1 do
        buffer := Format('%s + "%s"',[ASetting^.Values[i]]);
    end;
    raise Exception.CreateFmt(sInvalidSuppressContractionsStatement,[buffer]);
  end;
  Result := (ParseDeletion(DOMString(ASetting^.Values[0]),AStatement) > 0);
  ASetting.Understood := Result;
end;

procedure ParseCollationItem2(
  ACollationNode : TDOMElement;
  AItem          : TCldrCollationItem;
  AMode          : TCldrParserMode
);
var
  statementList : TReorderSequenceArray;
  sal : Integer;//statement actual length
  statement : PReorderSequence;

  procedure AddStatementToArray();
  begin
    Inc(statement);
    Inc(sal);
    if (sal >= Length(statementList)) then begin
      SetLength(statementList,(sal*2));
      statement := @statementList[(sal-1)];
    end;
  end;

var
  n : TDOMNode;
  rulesElement : TDOMCDATASection;
  i, c, nextPos : Integer;
  parsedStatement : TParsedStatement;
  s : DOMString;
  u8 : UTF8String;
  buffer : PAnsiChar;
  lineCount : Integer;
  settingArray : TSettingRecArray;
begin
  AItem.TypeName := ACollationNode.GetAttribute(s_TYPE);
  AItem.Alt := ACollationNode.GetAttribute(s_ALT);
  AItem.Settings := nil;
  AItem.Rules := nil;
  AItem.Mode := AMode;
  if (AMode = TCldrParserMode.FullParsing) then begin
    SetLength(statementList,15);
    sal := 0;
    statement := @statementList[0];
    n := ACollationNode.FindNode(s_CR);
    if (n <> nil) then begin
      n := (n as TDOMElement).FirstChild;
      rulesElement := n as TDOMCDATASection;
      s := rulesElement.Data;
      u8 := UTF8Encode(s);
      c := Length(u8);
      buffer := @u8[1];
      nextPos := 0;
      i := 0;
      lineCount := 0;
      Clear(parsedStatement);
      settingArray := AItem.Settings;
      while (i < c) do begin
        statement^.Clear();
        if not ParseStatement(buffer,i,c,@parsedStatement,nextPos,lineCount) then
          Break;
        if (parsedStatement.Kind = TStatementKind.Sequence) then begin
          statement^.Assign(@parsedStatement.ReorderSequence);
          AddStatementToArray();
        end else if (parsedStatement.Kind = TStatementKind.Setting) then begin
          if (parsedStatement.Setting.OptionValue = TSettingOption.SuppressContractions) then begin
            if Process_SuppressContractions(@parsedStatement.Setting,statement) then
              AddStatementToArray()
            else
              statement^.Clear();
          end;
          AddItem(settingArray,@parsedStatement.Setting);
        end;
        i := nextPos;
      end;
      AItem.Settings := settingArray;
      if (Length(AItem.Settings) > 0) then
        HandleSettings(AItem);
    end;
    SetLength(statementList,sal);
    AItem.Rules := statementList;
  end;
end;

procedure ParseCollationDocumentXML(
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
  ACollation.Mode := AMode;
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
          ParseCollationItemXML((n as TDOMElement),item,AMode);
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

procedure ParseCollationDocumentXML(
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
    ParseCollationItemXML((TDOMNode(xv.AsNodeSet[0]) as TDOMElement),ACollation,TCldrParserMode.FullParsing);
  finally
    xv.Free();
  end
end;

procedure ParseCollationDocument2(
  ADoc       : TDOMDocument;
  ACollation : TCldrCollation;
  AMode      : TCldrParserMode
);
var
  n : TDOMNode;
  collationsElement : TDOMElement;
  i, c : Integer;
  item, tempItem : TCldrCollationItem;
  nl : TDOMNodeList;
  isnew : boolean;
begin
  n := ADoc.DocumentElement.FindNode(s_COLLATIONS);
  if (n = nil) then
    raise Exception.Create(sCollationsNodeNotFound);
  collationsElement := n as TDOMElement;
  //ACollation.Clear();
  ACollation.Mode := AMode;
  ACollation.Language := EvaluateXPathStr('identity/language/@type',ADoc.DocumentElement);
  ACollation.Version := EvaluateXPathStr('identity/version/@number',ADoc.DocumentElement);
  ACollation.DefaultType := EvaluateXPathStr('collations/defaultCollation',ADoc.DocumentElement);
  if collationsElement.HasChildNodes() then begin
    nl := collationsElement.ChildNodes;
    c := nl.Count;
    tempItem := TCldrCollationItem.Create();
    try
      item := nil;
      try
        for i := 0 to c - 1 do begin
          n := nl[i];
          if (n.NodeName = s_COLLATION) then begin
            tempItem.Clear();
            ParseCollationItem2((n as TDOMElement),tempItem,TCldrParserMode.HeaderParsing);
            item := ACollation.Find(tempItem.TypeName);
            isnew := (item = nil);
            if isnew then
              item := TCldrCollationItem.Create();
            if isnew or (item.Mode < AMode) then
              ParseCollationItem2((n as TDOMElement),item,AMode);
            if isnew then
              ACollation.Add(item);
            item := nil;
          end
        end;
      except
        FreeAndNil(item);
        raise;
      end;
    finally
      tempItem.Free();
    end;
  end;
end;

procedure ParseCollationDocument2(
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
    ParseCollationItem2((TDOMNode(xv.AsNodeSet[0]) as TDOMElement),ACollation,TCldrParserMode.FullParsing);
  finally
    xv.Free();
  end
end;

function ReadXMLFile(f: TStream) : TXMLDocument;overload;
var
  src : TXMLInputSource;
  parser: TDOMParser;
begin
  src := TXMLInputSource.Create(f);
  parser := TDOMParser.Create();
  try
    parser.Options.IgnoreComments := True;
    parser.Parse(src, Result);
  finally
    src.Free();
    parser.Free;
  end;
end;

function ReadXMLFile(const AFilename: String) : TXMLDocument;overload;
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

procedure ParseCollationDocumentXML(
  const AFileName  : string;
        ACollation : TCldrCollation;
        AMode      : TCldrParserMode
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AFileName);
  try
    ParseCollationDocumentXML(doc,ACollation,AMode);
    ACollation.LocalID := ExtractFileName(ChangeFileExt(AFileName,''));
  finally
    doc.Free();
  end;
end;

procedure ParseCollationDocumentXML(
  const AFileName  : string;
        ACollation : TCldrCollationItem;
        AType      : string
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AFileName);
  try
    ParseCollationDocumentXML(doc,ACollation,AType);
  finally
    doc.Free();
  end;
end;

procedure ParseCollationDocument2(
  const AFileName  : string;
        ACollation : TCldrCollation;
        AMode      : TCldrParserMode
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AFileName);
  try
    ParseCollationDocument2(doc,ACollation,AMode);
    ACollation.LocalID := ExtractFileName(ChangeFileExt(AFileName,''));
  finally
    doc.Free();
  end;
end;

procedure ParseCollationDocument2(
  AStream    : TStream;
  ACollation : TCldrCollation;
  AMode      : TCldrParserMode
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AStream);
  try
    ParseCollationDocument2(doc,ACollation,AMode);
  finally
    doc.Free();
  end;
end;

procedure ParseCollationDocument2(
  const AFileName  : string;
        ACollation : TCldrCollationItem;
        AType      : string
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AFileName);
  try
    ParseCollationDocument2(doc,ACollation,AType);
  finally
    doc.Free();
  end;
end;

procedure ParseCollationDocument2(
  AStream    : TStream;
  ACollation : TCldrCollationItem;
  AType      : string
);
var
  doc : TXMLDocument;
begin
  doc := ReadXMLFile(AStream);
  try
    ParseCollationDocument2(doc,ACollation,AType);
  finally
    doc.Free();
  end;
end;

{ TCldrCollationStreamLoader }

procedure TCldrCollationStreamLoader.CheckContent(ALanguage: string);
begin
  if not FileExists(ALanguage) then
    raise EFOpenError.CreateFmt(SFOpenError,[ALanguage]);
end;

function TCldrCollationStreamLoader.IndexOf(ALanguage: string): Integer;
var
  i : Integer;
begin
  for i := Low(FLanguages) to High(FLanguages) do begin
    if (FLanguages[i] = ALanguage) then begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

procedure TCldrCollationStreamLoader.LoadCollation(
  const ALanguage  : string;
        ACollation : TCldrCollation;
        AMode      : TCldrParserMode
);
var
  i : Integer;
  locStream : TStream;
begin
  i := IndexOf(ALanguage);
  if (i < 0) then
    CheckContent(ALanguage);
  locStream := FStreams[i];
  locStream.Position := 0;
  ParseCollationDocument2(locStream,ACollation,AMode);
end;

procedure TCldrCollationStreamLoader.LoadCollationType(
  const ALanguage,
        ATypeName  : string;
        AType      : TCldrCollationItem
);
var
  i : Integer;
  locStream : TStream;
begin
  i := IndexOf(ALanguage);
  if (i < 0) then
    CheckContent(ALanguage);
  locStream := FStreams[i];
  locStream.Position := 0;
  ParseCollationDocument2(locStream,AType,ATypeName);
end;

constructor TCldrCollationStreamLoader.Create(
  const ALanguages : array of string;
  const AStreams   : array of TStream
);
var
  c, i : Integer;
begin
  c := Length(ALanguages);
  if (Length(AStreams) < c) then
    c := Length(AStreams);
  SetLength(FLanguages,c);
  SetLength(FStreams,c);
  for i := Low(ALanguages) to High(ALanguages) do begin
    FLanguages[i] := ALanguages[i];
    FStreams[i] := AStreams[i];
  end;
end;

destructor TCldrCollationStreamLoader.Destroy();
var
  i : Integer;
begin
  for i := Low(FStreams) to High(FStreams) do
    FreeAndNil(FStreams[i]);
end;

{ TCldrCollationFileLoader }

procedure TCldrCollationFileLoader.SetPath(APath: string);
var
  s : string;
begin
  if (APath = '') then
    s := ''
  else
    s := IncludeTrailingPathDelimiter(APath);
  if (s <> FPath) then
    FPath := s;
end;

function TCldrCollationFileLoader.BuildFileName(ALanguage: string): string;
begin
  Result := Format('%s%s.xml',[FPath,ALanguage]);
end;

procedure TCldrCollationFileLoader.CheckFile(AFileName: string);
begin
  if not FileExists(AFileName) then
    raise EFOpenError.CreateFmt(SFOpenError,[AFileName]);
end;

procedure TCldrCollationFileLoader.LoadCollation(
  const ALanguage  : string;
        ACollation : TCldrCollation;
        AMode      : TCldrParserMode
);
var
  locFileName : string;
begin
  locFileName := BuildFileName(ALanguage);
  CheckFile(locFileName);
  //ACollation.Clear();
  ParseCollationDocument2(locFileName,ACollation,AMode);
end;

procedure TCldrCollationFileLoader.LoadCollationType(
  const ALanguage,
        ATypeName : string;
        AType     : TCldrCollationItem
);
var
  locFileName : string;
begin
  locFileName := BuildFileName(ALanguage);
  CheckFile(locFileName);
  //AType.Clear();
  ParseCollationDocument2(locFileName,AType,ATypeName);
end;

constructor TCldrCollationFileLoader.Create(APath: string);
begin
  SetPath(APath);
end;

end.
