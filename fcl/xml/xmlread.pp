{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 Sebastian Guenther

    XML reading routines.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit xmlread;

interface

uses DOM, debug;

function ReadXMLFile(var f: File): TXMLDocument;
function ReadDTDFile(var f: File): TXMLDocument;


implementation

uses sysutils;

const

  Letter = ['A'..'Z', 'a'..'z'];
  Digit = ['0'..'9'];
  PubidChars: set of Char = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];

  NmToken: set of Char = Letter + Digit + ['.', '-', '_', ':'];

type

  TSetOfChar = set of Char;

  TXMLReader = class
  protected
    doc: TXMLDocument;
    buf: PChar;

    procedure RaiseExc(descr: String);
    function  SkipWhitespace: Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(s: String);
    function  CheckFor(s: PChar): Boolean;
    function  GetString(ValidChars: TSetOfChar): String;

    function  GetName(var s: String): Boolean;
    function  ExpectName: String;					// [5]
    procedure ExpectAttValue(attr: TDOMAttr);				// [10]
    function  ExpectPubidLiteral: String;				// [12]
    function  ParseComment: Boolean;					// [15]
    function  ParsePI: Boolean;						// [16]
    procedure ExpectProlog;			    			// [22]
    function  ParseEq: Boolean;						// [25]
    procedure ExpectEq;
    procedure ParseMisc;						// [27]
    function  ParseMarkupDecl: Boolean;					// [29]
    function  ParseElement(owner: TDOMNode): Boolean;			// [39]
    procedure ExpectElement(owner: TDOMNode);
    function  ParseReference: Boolean;					// [67]
    procedure ExpectReference;
    function  ParsePEReference: Boolean;				// [69]
    function  ParseExternalID: Boolean;					// [75]
    procedure ExpectExternalID;
    function  ParseEncodingDecl: String;    				// [80]
  public
    function ProcessXML(ABuf: PChar): TXMLDocument;    			// [1]
    function ProcessDTD(ABuf: PChar): TXMLDocument;			// ([29])
  end;



procedure TXMLReader.RaiseExc(descr: String);
begin
  WriteLn('Throwing exception: ', descr);
  raise Exception.Create('In XML reader: ' + descr);
end;

function TXMLReader.SkipWhitespace: Boolean;
begin
  dbg_push('SkipWhitespace');
  Result := False;
  while buf[0] in [#9, #10, #13, ' '] do begin
    Inc(buf);
    Result := True;
  end;
  dbg_pop;
end;

procedure TXMLReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    RaiseExc('Expected whitespace');
end;

procedure TXMLReader.ExpectString(s: String);
var
  i: Integer;
  s2: PChar;
  s3: String;
begin
  dbg_push('ExpectString');
  for i := 1 to Length(s) do
    if buf[i - 1] <> s[i] then begin
      GetMem(s2, Length(s) + 1);
      StrLCopy(s2, buf, Length(s));
      s3 := StrPas(s2);
      FreeMem(s2, Length(s) + 1);
      RaiseExc('Expected "' + s + '", found "' + s3 + '"');
    end;
  Inc(buf, Length(s));
  dbg_pop;
end;

function TXMLReader.CheckFor(s: PChar): Boolean;
begin
  dbg_push('CheckFor');
  if buf[0] = #0 then exit(False);
  if StrLComp(buf, s, StrLen(s)) = 0 then begin
    Inc(buf, StrLen(s));
    Result := True;
  end else
    Result := False;
  dbg_pop;
end;

function TXMLReader.GetString(ValidChars: TSetOfChar): String;
begin
  dbg_push('GetString');
  Result := '';
  while buf[0] in ValidChars do begin
    Result := Result + buf[0];
    Inc(buf);
  end;
  dbg_pop;
end;

function TXMLReader.ProcessXML(ABuf: PChar): TXMLDocument;    // [1]
var
  LastNodeBeforeDoc: TDOMNode;
begin
  dbg_push('ProcessXML');
  buf := ABuf;

  doc := TXMLDocument.Create;
  ExpectProlog;
  LastNodeBeforeDoc := doc.LastChild;
  ExpectElement(doc);
  ParseMisc;

  if buf[0] <> #0 then begin
    WriteLn('=== Unparsed: ===');
    //WriteLn(buf);
    WriteLn(StrLen(buf), ' chars');
  end;

  Result := doc;
  dbg_pop;
end;


function TXMLReader.GetName(var s: String): Boolean;    // [5]
begin
  dbg_push('GetName. buf[0]=' + buf[0]);
  s := '';
  if not (buf[0] in (Letter + ['_', ':'])) then
    exit(False);

  s := buf[0];
  Inc(buf);
  s := s + GetString(Letter + ['0'..'9', '.', '-', '_', ':']);
  Result := True;
  dbg_pop;
end;

function TXMLReader.ExpectName: String;    // [5]
begin
  dbg_push('ExpectName. buf[0]=' + buf[0]);
  if not (buf[0] in (Letter + ['_', ':'])) then
    RaiseExc('Expected letter, "_" or ":" for name, found "' + buf[0] + '"');

  Result := buf[0];
  Inc(buf);
  Result := Result + GetString(Letter + ['0'..'9', '.', '-', '_', ':']);
  dbg_pop;
end;

procedure TXMLReader.ExpectAttValue(attr: TDOMAttr);    // [10]
var
  strdel: array[0..1] of Char;
  s: String;
begin
  dbg_push('ExpectAttValue');
  if (buf[0] <> '''') and (buf[0] <> '"') then
    RaiseExc('Expected quotation marks');
  strdel[0] := buf[0];
  strdel[1] := #0;
  Inc(buf);
  s := '';
  while not CheckFor(strdel) do
    if not ParseReference then begin
      s := s + buf[0];
      Inc(buf);
    end else begin
      if s <> '' then begin
        attr.AppendChild(doc.CreateTextNode(s));
        s := '';
      end;
    end;

  if s <> '' then
    attr.AppendChild(doc.CreateTextNode(s));

  dbg_pop;
end;

function TXMLReader.ExpectPubidLiteral: String;
begin
  dbg_push('ExpectPubidLiteral');
  Result := '';
  if CheckFor('''') then begin
    GetString(PubidChars - ['''']);
    ExpectString('''');
  end else if CheckFor('"') then begin
    GetString(PubidChars - ['"']);
    ExpectString('"');
  end else
    RaiseExc('Expected quotation marks');
  dbg_pop;
end;

function TXMLReader.ParseComment: Boolean;    // [15]
begin
  dbg_push('ParseComment');
  if CheckFor('<!--') then begin
    while (buf[0] <> #0) and (buf[1] <> #0) and
      ((buf[0] <> '-') or (buf[1] <> '-')) do Inc(buf);
    ExpectString('-->');
    Result := True;
  end else
    Result := False;
  dbg_pop;
end;

function TXMLReader.ParsePI: Boolean;    // [16]
var
  checkbuf: array[0..3] of char;
begin
  dbg_push('ParsePI');
  if CheckFor('<?') then begin
    StrLCopy(checkbuf, buf, 3);
    if UpCase(StrPas(checkbuf)) = 'XML' then
      RaiseExc('"<?XML" processing instruction not allowed here');
    ExpectName;
    if SkipWhitespace then
      while (buf[0] <> #0) and (buf[1] <> #0) and
        (buf[0] <> '?') and (buf[1] <> '>') do Inc(buf);
    ExpectString('?>');
  end else
    Result := False;
  dbg_pop;
end;

procedure TXMLReader.ExpectProlog;    // [22]

  procedure ParseVersionNum;
  begin
    doc.XMLVersion :=
      GetString(['a'..'z', 'A'..'Z', '0'..'9', '_', '.', ':', '-']);
  end;

begin
  dbg_push('ExpectProlog');
  if CheckFor('<?xml') then begin
    // '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'

    // VersionInfo: S 'version' Eq (' VersionNum ' | " VersionNum ")
    SkipWhitespace;
    ExpectString('version');
    ParseEq;
    if buf[0] = '''' then begin
      Inc(buf);
      ParseVersionNum;
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      ParseVersionNum;
      ExpectString('"');
    end else
      RaiseExc('Expected single or double quotation mark');

    // EncodingDecl?
    ParseEncodingDecl;

    // SDDecl?
    SkipWhitespace;
    if CheckFor('standalone') then begin
      ExpectEq;
      if buf[0] = '''' then begin
        Inc(buf);
	if not (CheckFor('yes''') or CheckFor('no''')) then
	  RaiseExc('Expected ''yes'' or ''no''');
      end else if buf[0] = '''' then begin
        Inc(buf);
	if not (CheckFor('yes"') or CheckFor('no"')) then
	  RaiseExc('Expected "yes" or "no"');
      end;
      SkipWhitespace;
    end;

    ExpectString('?>');
  end;

  // Check for "Misc*"
  ParseMisc;

  // Check for "(doctypedecl Misc*)?"
  if CheckFor('<!DOCTYPE') then begin
    SkipWhitespace;
    ExpectName;
    SkipWhitespace;
    ParseExternalID;
    SkipWhitespace;
    if CheckFor('[') then begin
      repeat
        SkipWhitespace;
      until not (ParseMarkupDecl or ParsePEReference);
      ExpectString(']');
      SkipWhitespace;
    end;
    ParseMisc;
  end;

  dbg_pop;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
var
  savedbuf: PChar;
begin
  dbg_push('ParseEq');
  savedbuf := buf;
  SkipWhitespace;
  if buf[0] = '=' then begin
    Inc(buf);
    SkipWhitespace;
    Result := True;
  end else begin
    buf := savedbuf;
    Result := False;
  end;
  dbg_pop;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    RaiseExc('Expected "="');
end;


// Parse "Misc*": 
//   Misc ::= Comment | PI | S

procedure TXMLReader.ParseMisc;    // [27]
begin
  dbg_push('ParseMisc');
  repeat
    SkipWhitespace;
  until not (ParseComment or ParsePI);
  dbg_pop;
end;

function TXMLReader.ParseMarkupDecl: Boolean;    // [29]

  function ParseElementDecl: Boolean;    // [45]

    procedure ExpectChoiceOrSeq;    // [49], [50]

      procedure ExpectCP;    // [48]
      begin
        dbg_push('ExpectCP');
        if CheckFor('(') then
	  ExpectChoiceOrSeq
	else
	  ExpectName;
	if CheckFor('?') then
	else if CheckFor('*') then
	else if CheckFor('+') then;
	dbg_pop;
      end;

    var
      delimiter: Char;
    begin
      dbg_push('ExpectChoiceOrSeq');
      SkipWhitespace;
      ExpectCP;
      SkipWhitespace;
      delimiter := #0;
      while not CheckFor(')') do begin
        if delimiter = #0 then begin
	  if (buf[0] = '|') or (buf[0] = ',') then
	    delimiter := buf[0]
	  else
	    RaiseExc('Expected "|" or ","');
	  Inc(buf);
	end else
	  ExpectString(delimiter);
	SkipWhitespace;
	ExpectCP;
      end;
      dbg_pop;
    end;

  begin
    dbg_push('ParseElementDecl');
    if CheckFor('<!ELEMENT') then begin
      ExpectWhitespace;
      WriteLn('Element decl: ', ExpectName);
      ExpectWhitespace;

      // Get contentspec [46]

      if CheckFor('EMPTY') then
      else if CheckFor('ANY') then
      else if CheckFor('(') then begin
	SkipWhitespace;
	if CheckFor('#PCDATA') then begin
          // Parse Mixed section [51]
  	  SkipWhitespace;
	  if not CheckFor(')') then
	    repeat
	      ExpectString('|');
	      SkipWhitespace;
	      ExpectName;
	    until CheckFor(')*');
	end else begin
	  // Parse Children section [47]

	  ExpectChoiceOrSeq;

	  if CheckFor('?') then
	  else if CheckFor('*') then
	  else if CheckFor('+') then;
	end;
      end else
        RaiseExc('Invalid content specification');

      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
    dbg_pop;
  end;

  function ParseAttlistDecl: Boolean;    // [52]
  var
    attr: TDOMAttr;
  begin
    dbg_push('ParseAttlistDecl');
    if CheckFor('<!ATTLIST') then begin
      ExpectWhitespace;
      ExpectName;
      SkipWhitespace;
      while not CheckFor('>') do begin
        ExpectName;
	ExpectWhitespace;

        // Get AttType [54], [55], [56]
	if CheckFor('CDATA') then
	else if CheckFor('ID') then
	else if CheckFor('IDREF') then
	else if CheckFor('IDREFS') then
	else if CheckFor('ENTITTY') then
	else if CheckFor('ENTITIES') then
	else if CheckFor('NMTOKEN') then
	else if CheckFor('NMTOKENS') then
	else if CheckFor('NOTATION') then begin   // [57], [58]
	  ExpectWhitespace;
	  ExpectString('(');
	  SkipWhitespace;
	  ExpectName;
	  SkipWhitespace;
	  while not CheckFor(')') do begin
	    ExpectString('|');
	    SkipWhitespace;
	    ExpectName;
	    SkipWhitespace;
	  end;
	end else if CheckFor('(') then begin    // [59]
	  SkipWhitespace;
	  GetString(Nmtoken);
	  SkipWhitespace;
	  while not CheckFor(')') do begin
	    ExpectString('|');
	    SkipWhitespace;
	    GetString(Nmtoken);
	    SkipWhitespace;
          end;
	end else
	  RaiseExc('Invalid tokenized type');

	ExpectWhitespace;

	// Get DefaultDecl [60]
	if CheckFor('#REQUIRED') then
	else if CheckFor('#IMPLIED') then
	else begin
	  if CheckFor('#FIXED') then
	    SkipWhitespace;
	  attr := doc.CreateAttribute('');
	  ExpectAttValue(attr);
	end;

        SkipWhitespace;
      end;
      Result := True;
    end else
      Result := False;
    dbg_pop;
  end;

  function ParseEntityDecl: Boolean;    // [70]

    function ParseEntityValue: Boolean;    // [9]
    var
      strdel: array[0..1] of Char;
    begin
      if (buf[0] <> '''') and (buf[0] <> '"') then exit(False);
      dbg_push('ParseEntityValue');
      strdel[0] := buf[0];
      strdel[1] := #0;
      Inc(buf);
      while not CheckFor(strdel) do
        if ParsePEReference then
	else if ParseReference then
	else
	  RaiseExc('Expected reference or PE reference');
      Result := True;
      dbg_pop;
    end;

  begin
    dbg_push('ParseEntityDecl');
    if CheckFor('<!ENTITY') then begin
      ExpectWhitespace;
      if CheckFor('%') then begin    // [72]
        ExpectWhitespace;
	ExpectName;
	ExpectWhitespace;
	// Get PEDef [74]
	if ParseEntityValue then
	else if ParseExternalID then
	else
	  RaiseExc('Expected entity value or external ID');
      end else begin    // [71]
        ExpectName;
	ExpectWhitespace;
	// Get EntityDef [73]
	if ParseEntityValue then
	else begin
	  ExpectExternalID;
	  // Get NDataDecl [76]
	  ExpectWhitespace;
	  ExpectString('NDATA');
	  ExpectWhitespace;
	  ExpectName;
	end;
      end;
      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
    dbg_pop;
  end;

  function ParseNotationDecl: Boolean;    // [82]
  begin
    dbg_push('ParseNotationDecl');
    if CheckFor('<!NOTATION') then begin
      ExpectWhitespace;
      ExpectName;
      ExpectWhitespace;
      if ParseExternalID then
      else if CheckFor('PUBLIC') then begin    // [83]
        ExpectWhitespace;
	ExpectPubidLiteral;
      end else
        RaiseExc('Expected external or public ID');
      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
    dbg_pop;
  end;

begin
  dbg_push('ParseMarkupDecl');
  Result := False;
  while ParseElementDecl or ParseAttlistDecl or ParseEntityDecl or
    ParseNotationDecl or ParsePI or ParseComment or SkipWhitespace do Result := True;
  dbg_pop;
end;

function TXMLReader.ProcessDTD(ABuf: PChar): TXMLDocument;    // [1]
begin
  dbg_push('ProcessDTD');
  buf := ABuf;

  doc := TXMLDocument.Create;
  ParseMarkupDecl;

  if buf[0] <> #0 then begin
    WriteLn('=== Unparsed: ===');
    //WriteLn(buf);
    WriteLn(StrLen(buf), ' chars');
  end;

  Result := doc;
  dbg_pop;
end;

function TXMLReader.ParseElement(owner: TDOMNode): Boolean;    // [39] [40] [44]
var
  NewElem: TDOMElement;

  function ParseCharData: Boolean;    // [14]
  var
    s: String;
    i: Integer;
  begin
    dbg_push('ParseCharData');
    s := '';
    while not (buf[0] in [#0, '<', '&']) do begin
      s := s + buf[0];
      Inc(buf);
    end;
    if s <> '' then begin
      // Strip whitespace from end of s
      i := Length(s);
      while (i > 0) and (s[i] in [#10, #13, ' ']) do Dec(i);
      NewElem.AppendChild(doc.CreateTextNode(Copy(s, 1, i)));
      Result := True;
    end else
      Result := False;
    dbg_pop;
  end;

  function ParseCDSect: Boolean;    // [18]
  begin
    dbg_push('ParseCDSect');
    if CheckFor('<![CDATA[') then begin
      while not CheckFor(']]>') do Inc(buf);
      Result := True;
    end else
      Result := False;
    dbg_pop;
  end;

var
  IsEmpty: Boolean;
  name: String;
  oldpos: PChar;

  attr: TDOMAttr;
begin
  dbg_push('ParseElement');
  oldpos := buf;
  if CheckFor('<') then begin
    if not GetName(name) then begin
      buf := oldpos;
      dbg_pop;
      exit(False);
    end;

    NewElem := doc.CreateElement(name);
    owner.AppendChild(NewElem);

    dbg_push('Processing element ' + name);
    SkipWhitespace;
    IsEmpty := False;
    dbg_push('Reading until end of tag');
    while True do begin
      if CheckFor('/>') then begin
        IsEmpty := True;
        break;
      end;
      if CheckFor('>') then break;

      // Get Attribute [41]
      attr := doc.CreateAttribute(ExpectName);
      NewElem.Attributes.SetNamedItem(attr);
      ExpectEq;
      ExpectAttValue(attr);

      SkipWhitespace;
    end;
    dbg_pop;

    if not IsEmpty then begin
      // Get content
      dbg_push('Reading content');
      while SkipWhitespace or ParseCharData or ParseCDSect or ParsePI or
        ParseComment or ParseElement(NewElem) or ParseReference do;

      // Get ETag [42]
      dbg_pop_push('Reading end tag');
      ExpectString('</');
      ExpectName;
      SkipWhitespace;
      ExpectString('>');
      dbg_pop;
    end;

    dbg_pop;
    Result := True;
  end else
    Result := False;
  dbg_pop;
end;

procedure TXMLReader.ExpectElement(owner: TDOMNode);
begin
  if not ParseElement(owner) then
    RaiseExc('Expected element');
end;

function TXMLReader.ParsePEReference: Boolean;
begin
  dbg_push('ParsePEReference');
  if CheckFor('%') then begin
    ExpectName;
    ExpectString(';');
    Result := True;
  end else
    Result := False;
  dbg_pop;
end;

function TXMLReader.ParseReference: Boolean;    // [67] [68] [69]
begin
  if (buf[0] <> '&') and (buf[0] <> '%') then exit(False);
  dbg_push('ParseReference ' + buf);
  Inc(buf);
  ExpectName;
  ExpectString(';');
  Result := True;
  dbg_pop;
end;

procedure TXMLReader.ExpectReference;
begin
  if not ParseReference then
    RaiseExc('Expected reference ("&Name;" or "%Name;")');
end;


function TXMLReader.ParseExternalID: Boolean;    // [75]

  function GetSystemLiteral: String;
  begin
    dbg_push('GetSystemLiteral');
    if buf[0] = '''' then begin
      Inc(buf);
      Result := '';
      while (buf[0] <> '''') and (buf[0] <> #0) do begin
        Result := Result + buf[0];
	Inc(buf);
      end;
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      Result := '';
      while (buf[0] <> '"') and (buf[0] <> #0) do begin
        Result := Result + buf[0];
	Inc(buf);
      end;
      ExpectString('"');
    end;
    dbg_pop;
  end;

begin
  dbg_push('ParseExternalID');
  if CheckFor('SYSTEM') then begin
    ExpectWhitespace;
    GetSystemLiteral;
    Result := True;
  end else if CheckFor('PUBLIC') then begin
    ExpectWhitespace;
    ExpectPubidLiteral;
    ExpectWhitespace;
    GetSystemLiteral;
    Result := True;
  end else
    Result := False;
  dbg_pop;
end;

procedure TXMLReader.ExpectExternalID;
begin
  if not ParseExternalID then
    RaiseExc('Expected external ID');
end;

function TXMLReader.ParseEncodingDecl: String;    // [80]

  function ParseEncName: String;
  begin
    dbg_push('ParseEncName');
    if not (buf[0] in ['A'..'Z', 'a'..'z']) then
      RaiseExc('Expected character (A-Z, a-z)');
    Result := buf[0];
    Inc(buf);
    Result := Result + GetString(['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']);
    dbg_pop;
  end;

begin
  dbg_push('ParseEncodingDecl');
  Result := '';
  SkipWhitespace;
  if CheckFor('encoding') then begin
    ExpectEq;
    if buf[0] = '''' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectString('"');
    end;
  end;
  dbg_pop;
end;


function ReadXMLFile(var f: File): TXMLDocument;
var
  reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then exit(nil);

  reader := TXMLReader.Create;
  GetMem(buf, BufSize);
  BlockRead(f, buf^, BufSize - 1);
  buf[BufSize - 1] := #0;
  Result := reader.ProcessXML(buf);
  FreeMem(buf, BufSize);
  reader.Free;
end;

function ReadDTDFile(var f: File): TXMLDocument;
var
  reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then exit(nil);

  reader := TXMLReader.Create;
  GetMem(buf, BufSize + 1);
  BlockRead(f, buf^, BufSize - 1);
  buf[BufSize - 1] := #0;
  Result := reader.ProcessDTD(buf);
  FreeMem(buf, BufSize);
  reader.Free;
end;


end.


{
  $Log$
  Revision 1.1  1999-07-09 08:35:09  michael
  + Initial implementation by Sebastian Guenther

}
