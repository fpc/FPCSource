{
    $Id$

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * Output string definitions
    * Basic writer (output generator) class

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit dWriter;

{$MODE objfpc}
{$H+}

interface

uses Classes, DOM, dGlobals, PasTree;

resourcestring
  SErrFileWriting = 'An error occured during writing of file "%s": %s';

  SErrInvalidShortDescr = 'Invalid short description';
  SErrInvalidDescr = 'Invalid description (illegal XML element: "%s")';
  SErrInvalidParaContent = 'Invalid paragraph content';
  SErrInvalidElementInList = 'Invalid element in list - only "li" allowed';
  SErrInvalidListContent = 'Invalid list content';
  SErrInvalidRemarkContent = 'Invalid <remark> content (illegal XML element: "%s")';
  SErrListIsEmpty = 'List is empty - need at least one "li" element';
  SErrInvalidDefinitionTermContent = 'Invalid content in definition term';
  SErrDefinitionEntryMissing = 'Definition entry after definition term is missing';
  SErrInvalidBorderValue = 'Invalid "border" value for %s';
  SErrInvalidTableContent = 'Invalid table content';
  SErrTableRowEmpty = 'Table row is empty (no "td" elements found)';
  SErrInvalidContentBeforeSectionTitle = 'Invalid content before section title';
  SErrSectionTitleExpected = 'Section title ("title" element) expected';

  SErrDescrTagUnknown = 'Warning: Unknown tag "%s" in description';
  SErrUnknownEntityReference = 'Warning: Unknown entity reference "&%s;" found';
  SErrUnknownLinkID = 'Warning: Target ID of <link> is unknown: "%s"';
  SErrUnknownPrintShortID = 'Warning: Target ID of <printshort> is unknown: "%s"';
  SErrUnknownLink = 'Could not resolve link to "%s"';


type

  TFPDocWriter = class
  private
    FEngine: TFPDocEngine;
  protected

    procedure Warning(AContext: TPasElement; const AMsg: String);
    procedure Warning(AContext: TPasElement; const AMsg: String;
      const Args: array of const);

    // function FindShortDescr(const Name: String): TDOMElement;

    // Description conversion
    function IsDescrNodeEmpty(Node: TDOMNode): Boolean;
    function IsExtShort(Node: TDOMNode): Boolean;
    function ConvertShort(AContext: TPasElement; El: TDOMElement): Boolean;
    function ConvertBaseShort(AContext: TPasElement; Node: TDOMNode): Boolean;
    procedure ConvertBaseShortList(AContext: TPasElement; Node: TDOMNode;
      MayBeEmpty: Boolean);
    procedure ConvertLink(AContext: TPasElement; El: TDOMElement);
    function ConvertExtShort(AContext: TPasElement; Node: TDOMNode): Boolean;
    procedure ConvertDescr(AContext: TPasElement; El: TDOMElement;
      AutoInsertBlock: Boolean);
    function ConvertNonSectionBlock(AContext: TPasElement;
      Node: TDOMNode): Boolean;
    procedure ConvertExtShortOrNonSectionBlocks(AContext: TPasElement;
      Node: TDOMNode);
    function ConvertSimpleBlock(AContext: TPasElement; Node: TDOMNode): Boolean;

    procedure DescrWriteText(const AText: DOMString); virtual; abstract;
    procedure DescrBeginBold; virtual; abstract;
    procedure DescrEndBold; virtual; abstract;
    procedure DescrBeginItalic; virtual; abstract;
    procedure DescrEndItalic; virtual; abstract;
    procedure DescrBeginEmph; virtual; abstract;
    procedure DescrEndEmph; virtual; abstract;
    procedure DescrWriteFileEl(const AText: DOMString); virtual; abstract;
    procedure DescrWriteKeywordEl(const AText: DOMString); virtual; abstract;
    procedure DescrWriteVarEl(const AText: DOMString); virtual; abstract;
    procedure DescrBeginLink(const AId: DOMString); virtual; abstract;
    procedure DescrEndLink; virtual; abstract;
    procedure DescrWriteLinebreak; virtual; abstract;
    procedure DescrBeginParagraph; virtual; abstract;
    procedure DescrEndParagraph; virtual; abstract;
    procedure DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String); virtual; abstract;
    procedure DescrWriteCodeLine(const ALine: String); virtual; abstract;
    procedure DescrEndCode; virtual; abstract;
    procedure DescrBeginOrderedList; virtual; abstract;
    procedure DescrEndOrderedList; virtual; abstract;
    procedure DescrBeginUnorderedList; virtual; abstract;
    procedure DescrEndUnorderedList; virtual; abstract;
    procedure DescrBeginDefinitionList; virtual; abstract;
    procedure DescrEndDefinitionList; virtual; abstract;
    procedure DescrBeginListItem; virtual; abstract;
    procedure DescrEndListItem; virtual; abstract;
    procedure DescrBeginDefinitionTerm; virtual; abstract;
    procedure DescrEndDefinitionTerm; virtual; abstract;
    procedure DescrBeginDefinitionEntry; virtual; abstract;
    procedure DescrEndDefinitionEntry; virtual; abstract;
    procedure DescrBeginSectionTitle; virtual; abstract;
    procedure DescrBeginSectionBody; virtual; abstract;
    procedure DescrEndSection; virtual; abstract;
    procedure DescrBeginRemark; virtual; abstract;
    procedure DescrEndRemark; virtual; abstract;
    procedure DescrBeginTable(ColCount: Integer; HasBorder: Boolean); virtual; abstract;
    procedure DescrEndTable; virtual; abstract;
    procedure DescrBeginTableCaption; virtual; abstract;
    procedure DescrEndTableCaption; virtual; abstract;
    procedure DescrBeginTableHeadRow; virtual; abstract;
    procedure DescrEndTableHeadRow; virtual; abstract;
    procedure DescrBeginTableRow; virtual; abstract;
    procedure DescrEndTableRow; virtual; abstract;
    procedure DescrBeginTableCell; virtual; abstract;
    procedure DescrEndTableCell; virtual; abstract;
  public
    constructor Create(AEngine: TFPDocEngine);
    property Engine: TFPDocEngine read FEngine;
  end;


implementation

uses SysUtils;


constructor TFPDocWriter.Create(AEngine: TFPDocEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;


// ===================================================================
//   Generic documentation node conversion
// ===================================================================

function IsContentNodeType(Node: TDOMNode): Boolean;
begin
  Result := (Node.NodeType = ELEMENT_NODE) or (Node.NodeType = TEXT_NODE) or
    (Node.NodeType = ENTITY_REFERENCE_NODE);
end;


procedure TFPDocWriter.Warning(AContext: TPasElement; const AMsg: String);
begin
  WriteLn('[', AContext.PathName, '] ', AMsg);
end;

procedure TFPDocWriter.Warning(AContext: TPasElement; const AMsg: String;
  const Args: array of const);
begin
  Warning(AContext, Format(AMsg, Args));
end;

function TFPDocWriter.IsDescrNodeEmpty(Node: TDOMNode): Boolean;
var
  Child: TDOMNode;
begin
  if (not Assigned(Node)) or (not Assigned(Node.FirstChild)) then
    Result := True
  else
  begin
    Child := Node.FirstChild;
    while Assigned(Child) do
    begin
      if (Child.NodeType = ELEMENT_NODE) or (Child.NodeType = TEXT_NODE) or
        (Child.NodeType = ENTITY_REFERENCE_NODE) then
      begin
        Result := False;
        exit;
      end;
      Child := Child.NextSibling;
    end;
  end;
  Result := True;
end;

{ Check wether the nodes starting with the node given as argument make up an
  'extshort' production. }
function TFPDocWriter.IsExtShort(Node: TDOMNode): Boolean;
begin
  while Assigned(Node) do
  begin
    if Node.NodeType = ELEMENT_NODE then
      if (Node.NodeName <> 'br') and
         (Node.NodeName <> 'link') and
         (Node.NodeName <> 'b') and
         (Node.NodeName <> 'file') and
         (Node.NodeName <> 'i') and
         (Node.NodeName <> 'kw') and
         (Node.NodeName <> 'printshort') and
         (Node.NodeName <> 'var') then
      begin
        Result := False;
	exit;
      end;
    Node := Node.NextSibling;
  end;
  Result := True;
end;

function TFPDocWriter.ConvertShort(AContext: TPasElement;
 El: TDOMElement): Boolean;
var
  Node: TDOMNode;
begin
  Result := False;
  if not Assigned(El) then
    exit;

  Node := El.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'link') then
      ConvertLink(AContext, TDOMElement(Node))
    else
      if not ConvertBaseShort(AContext, Node) then
        exit;
    Node := Node.NextSibling;
  end;
  Result := True;
end;

function TFPDocWriter.ConvertBaseShort(AContext: TPasElement;
  Node: TDOMNode): Boolean;

  function ConvertText: DOMString;
  var
    s: String;
    i: Integer;
  begin
    if Node.NodeType = TEXT_NODE then
    begin
      s := Node.NodeValue;
      i := 1;
      SetLength(Result, 0);
      while i <= Length(s) do
        if s[i] = #13 then
	begin
	  Result := Result + ' ';
	  Inc(i);
	  if s[i] = #10 then
	    Inc(i);
	end else if s[i] = #10 then
	begin
	  Result := Result + ' ';
	  Inc(i);
	end else
	begin
	  Result := Result + s[i];
	  Inc(i);
	end;
    end else if Node.NodeType = ENTITY_REFERENCE_NODE then
      if Node.NodeName = 'fpc' then
        Result := 'Free Pascal'
      else if Node.NodeName = 'delphi' then
        Result := 'Delphi'
      else
      begin
        Warning(AContext, Format(SErrUnknownEntityReference, [Node.NodeName]));
        Result := Node.NodeName;
      end
    else if Node.NodeType = ELEMENT_NODE then
      SetLength(Result, 0);
  end;

  function ConvertTextContent: DOMString;
  begin
    SetLength(Result, 0);
    Node := Node.FirstChild;
    while Assigned(Node) do
    begin
      Result := Result + ConvertText;
      Node := Node.NextSibling;
    end;
  end;

var
  El, DescrEl: TDOMElement;
  FPEl: TPasElement;
begin
  Result := True;
  if Node.NodeType = ELEMENT_NODE then
    if Node.NodeName = 'b' then
    begin
      DescrBeginBold;
      ConvertBaseShortList(AContext, Node, False);
      DescrEndBold;
    end else
    if Node.NodeName = 'i' then
    begin
      DescrBeginItalic;
      ConvertBaseShortList(AContext, Node, False);
      DescrEndItalic;
    end else
    if Node.NodeName = 'em' then
    begin
      DescrBeginEmph;
      ConvertBaseShortList(AContext, Node, False);
      DescrEndEmph;
    end else
    if Node.NodeName = 'file' then
      DescrWriteFileEl(ConvertTextContent)
    else if Node.NodeName = 'kw' then
      DescrWriteKeywordEl(ConvertTextContent)
    else if Node.NodeName = 'printshort' then
    begin
      El := TDOMElement(Node);
      DescrEl := Engine.FindShortDescr(AContext.GetModule, El['id']);
      if Assigned(DescrEl) then
	ConvertShort(AContext, DescrEl)
      else
      begin
	Warning(AContext, Format(SErrUnknownPrintShortID, [El['id']]));
	DescrBeginBold;
	DescrWriteText('#ShortDescr:' + El['id']);
	DescrEndBold;
      end;
    end else if Node.NodeName = 'var' then
      DescrWriteVarEl(ConvertTextContent)
    else
      Result := False
  else
    DescrWriteText(ConvertText);
end;

procedure TFPDocWriter.ConvertBaseShortList(AContext: TPasElement;
  Node: TDOMNode; MayBeEmpty: Boolean);
var
  Child: TDOMNode;
begin
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    if not ConvertBaseShort(AContext, Child) then
      Warning(AContext, SErrInvalidShortDescr)
    else
      MayBeEmpty := True;
    Child := Child.NextSibling;
  end;
  if not MayBeEmpty then
    Warning(AContext, SErrInvalidShortDescr)
end;

procedure TFPDocWriter.ConvertLink(AContext: TPasElement; El: TDOMElement);
begin
  DescrBeginLink(El['id']);
  if not IsDescrNodeEmpty(El) then
    ConvertBaseShortList(AContext, El, True)
  else
    DescrWriteText(El['id']);
  DescrEndLink;
end;

function TFPDocWriter.ConvertExtShort(AContext: TPasElement;
  Node: TDOMNode): Boolean;
begin
  Result := False;

  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'link') then
      ConvertLink(AContext, TDOMElement(Node))
    else if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'br') then
      DescrWriteLinebreak
    else
      if not ConvertBaseShort(AContext, Node) then
        exit;
    Node := Node.NextSibling;
  end;
  Result := True;
end;

procedure TFPDocWriter.ConvertDescr(AContext: TPasElement; El: TDOMElement;
  AutoInsertBlock: Boolean);
var
  Node, Child: TDOMNode;
  ParaCreated: Boolean;
begin
  if AutoInsertBlock then
    if IsExtShort(El.FirstChild) then
      DescrBeginParagraph
    else
      AutoInsertBlock := False;

  Node := El.FirstChild;
  if not ConvertExtShort(AContext, Node) then
  begin
    while Assigned(Node) do
    begin
      if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'section') then
      begin
        DescrBeginSectionTitle;
	Child := Node.FirstChild;
	while Assigned(Child) and (Child.NodeType <> ELEMENT_NODE) do
	begin
	  if not IsDescrNodeEmpty(Child) then
	    Warning(AContext, SErrInvalidContentBeforeSectionTitle);
	  Child := Child.NextSibling;
	end;
	if not Assigned(Child) or (Child.NodeName <> 'title') then
	  Warning(AContext, SErrSectionTitleExpected)
	else
	  ConvertShort(AContext, TDOMElement(Child));

	DescrBeginSectionBody;

	if IsExtShort(Child) then
	begin
	  DescrBeginParagraph;
	  ParaCreated := True;
	end else
	  ParaCreated := False;

	ConvertExtShortOrNonSectionBlocks(AContext, Child.NextSibling);

	if ParaCreated then
	  DescrEndParagraph;
	DescrEndSection;
      end else if not ConvertNonSectionBlock(AContext, Node) then
        Warning(AContext, SErrInvalidDescr, [Node.NodeName]);
      Node := Node.NextSibling;
    end;
  end else
    if AutoInsertBlock then
      DescrEndParagraph;
end;

procedure TFPDocWriter.ConvertExtShortOrNonSectionBlocks(AContext: TPasElement;
  Node: TDOMNode);
begin
  if not ConvertExtShort(AContext, Node) then
    while Assigned(Node) do
    begin
      if not ConvertNonSectionBlock(AContext, Node) then
        Warning(AContext, SErrInvalidDescr, [Node.NodeName]);
      Node := Node.NextSibling;
    end;
end;

function TFPDocWriter.ConvertNonSectionBlock(AContext: TPasElement;
  Node: TDOMNode): Boolean;

  procedure ConvertCells(Node: TDOMNode);
  var
    Child: TDOMNode;
    IsEmpty: Boolean;
  begin
    Node := Node.FirstChild;
    IsEmpty := True;
    while Assigned(Node) do
    begin
      if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'td') then
      begin
        DescrBeginTableCell;
	Child := Node.FirstChild;
	if not ConvertExtShort(AContext, Child) then
	  while Assigned(Child) do
	  begin
	    if not ConvertSimpleBlock(AContext, Child) then
	      Warning(AContext, SErrInvalidTableContent);
	    Child := Child.NextSibling;
	  end;
	DescrEndTableCell;
	IsEmpty := False;
      end else
        if IsContentNodeType(Node) then
	  Warning(AContext, SErrInvalidTableContent);
      Node := Node.NextSibling;
    end;
    if IsEmpty then
      Warning(AContext, SErrTableRowEmpty);
  end;

  procedure ConvertTable;

    function GetColCount(Node: TDOMNode): Integer;
    begin
      Result := 0;
      Node := Node.FirstChild;
      while Assigned(Node) do
      begin
        if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'td') then
	  Inc(Result);
        Node := Node.NextSibling;
      end;
    end;

  var
    s: String;
    HasBorder, CaptionPossible, HeadRowPossible: Boolean;
    ColCount, ThisRowColCount: Integer;
    Subnode: TDOMNode;
  begin
    s := TDOMElement(Node)['border'];
    if s = '1' then
      HasBorder := True
    else
    begin
      HasBorder := False;
      if (Length(s) <> 0) and (s <> '0') then
        Warning(AContext, SErrInvalidBorderValue, ['<table>']);
    end;

    // Determine the number of columns
    ColCount := 0;
    Subnode := Node.FirstChild;
    while Assigned(Subnode) do
    begin
      if Subnode.NodeType = ELEMENT_NODE then
        if (Subnode.NodeName = 'caption') or (Subnode.NodeName = 'th') or
	  (Subnode.NodeName = 'tr') then
	begin
	  ThisRowColCount := GetColCount(Subnode);
	  if ThisRowColCount > ColCount then
	    ColCount := ThisRowColCount;
	end;
      Subnode := Subnode.NextSibling;
    end;

    DescrBeginTable(ColCount, HasBorder);

    Node := Node.FirstChild;
    CaptionPossible := True;
    HeadRowPossible := True;
    while Assigned(Node) do
    begin
      if Node.NodeType = ELEMENT_NODE then
        if CaptionPossible and (Node.NodeName = 'caption') then
	begin
	  DescrBeginTableCaption;
	  if not ConvertExtShort(AContext, Node.FirstChild) then
	    Warning(AContext, SErrInvalidTableContent);
	  DescrEndTableCaption;
	  CaptionPossible := False;
	end else if HeadRowPossible and (Node.NodeName = 'th') then
	begin
	  DescrBeginTableHeadRow;
	  ConvertCells(Node);
	  DescrEndTableHeadRow;
	  CaptionPossible := False;
	  HeadRowPossible := False;
	end else if Node.NodeName = 'tr' then
	begin
	  DescrBeginTableRow;
	  ConvertCells(Node);
	  DescrEndTableRow;
	end else
	  Warning(AContext, SErrInvalidTableContent)
      else if IsContentNodeType(Node) then
        Warning(AContext, SErrInvalidTableContent);
      Node := Node.NextSibling;
    end;
    DescrEndTable;
  end;

begin
  if Node.NodeType <> ELEMENT_NODE then
  begin
    Result := Node.NodeType = COMMENT_NODE;
    exit;
  end;
  if Node.NodeName = 'remark' then
  begin
    DescrBeginRemark;
    Node := Node.FirstChild;
    if not ConvertExtShort(AContext, Node) then
      while Assigned(Node) do
      begin
	if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'table') then
	  ConvertTable
	else
	  if not ConvertSimpleBlock(AContext, Node) then
	    Warning(AContext, SErrInvalidRemarkContent, [Node.NodeName]);
	Node := Node.NextSibling;
      end;
    DescrEndRemark;
    Result := True;
  end else if Node.NodeName = 'table' then
  begin
    ConvertTable;
    Result := True;
  end else
    Result := ConvertSimpleBlock(AContext, Node);
end;

function TFPDocWriter.ConvertSimpleBlock(AContext: TPasElement;
  Node: TDOMNode): Boolean;

  procedure ConvertListItems;
  var
    Empty: Boolean;
  begin
    Node := Node.FirstChild;
    Empty := True;
    while Assigned(Node) do
    begin
      if (Node.NodeType = TEXT_NODE) or (Node.NodeType = ENTITY_REFERENCE_NODE)
        then
	Warning(AContext, SErrInvalidListContent)
      else if Node.NodeType = ELEMENT_NODE then
        if Node.NodeName = 'li' then
	begin
	  DescrBeginListItem;
	  ConvertExtShortOrNonSectionBlocks(AContext, Node.FirstChild);
	  DescrEndListItem;
	  Empty := False;
	end else
          Warning(AContext, SErrInvalidElementInList);
      Node := Node.NextSibling;
    end;
    if Empty then
      Warning(AContext, SErrListIsEmpty);
  end;

  procedure ConvertDefinitionList;
  var
    Empty, ExpectDTNext: Boolean;
  begin
    Node := Node.FirstChild;
    Empty := True;
    ExpectDTNext := True;
    while Assigned(Node) do
    begin
      if (Node.NodeType = TEXT_NODE) or (Node.NodeType = ENTITY_REFERENCE_NODE)
        then
	Warning(AContext, SErrInvalidListContent)
      else if Node.NodeType = ELEMENT_NODE then
        if ExpectDTNext and (Node.NodeName = 'dt') then
	begin
	  DescrBeginDefinitionTerm;
	  if not ConvertShort(AContext, TDOMElement(Node)) then
	    Warning(AContext, SErrInvalidDefinitionTermContent);
	  DescrEndDefinitionTerm;
	  Empty := False;
	  ExpectDTNext := False;
	end else if not ExpectDTNext and (Node.NodeName = 'dd') then
	begin
	  DescrBeginDefinitionEntry;
	  ConvertExtShortOrNonSectionBlocks(AContext, Node.FirstChild);
	  DescrEndDefinitionEntry;
	  ExpectDTNext := True;
	end else
          Warning(AContext, SErrInvalidElementInList);
      Node := Node.NextSibling;
    end;
    if Empty then
      Warning(AContext, SErrListIsEmpty)
    else if not ExpectDTNext then
      Warning(AContext, SErrDefinitionEntryMissing);
  end;

  procedure ProcessCodeBody(Node: TDOMNode);
  var
    s: String;
    i, j: Integer;
  begin
    Node := Node.FirstChild;
    SetLength(s, 0);
    while Assigned(Node) do
    begin
      if Node.NodeType = TEXT_NODE then
      begin
        s := s + Node.NodeValue;
	j := 1;
	for i := 1 to Length(s) do
	  // In XML, linefeeds are normalized to #10 by the parser!
	  if s[i] = #10 then
	  begin
	    DescrWriteCodeLine(Copy(s, j, i - j));
	    j := i + 1;
	  end;
	if j > 1 then
	  s := Copy(s, j, Length(s));
      end;
      Node := Node.NextSibling;
    end;
    if Length(s) > 0 then
      DescrWriteCodeLine(s);
  end;

var
  s: String;
  HasBorder: Boolean;
begin
  if Node.NodeType <> ELEMENT_NODE then
  begin
    Result := False;
    exit;
  end;
  if Node.NodeName = 'p' then
  begin
    DescrBeginParagraph;
    if not ConvertExtShort(AContext, Node.FirstChild) then
      Warning(AContext, SErrInvalidParaContent);
    DescrEndParagraph;
    Result := True;
  end else if Node.NodeName = 'code' then
  begin
    s := TDOMElement(Node)['border'];
    if s = '1' then
      HasBorder := True
    else
    begin
      if (Length(s) > 0) and (s <> '0') then
        Warning(AContext, SErrInvalidBorderValue, ['<code>']);
    end;

    DescrBeginCode(HasBorder, TDOMElement(Node)['highlighter']);
    ProcessCodeBody(Node);
    DescrEndCode;
    Result := True;
  end else if Node.NodeName = 'pre' then
  begin
    DescrBeginCode(False, 'none');
    ProcessCodeBody(Node);
    DescrEndCode;
    Result := True;
  end else if Node.NodeName = 'ul' then
  begin
    DescrBeginUnorderedList;
    ConvertListItems;
    DescrEndUnorderedList;
    Result := True;
  end else if Node.NodeName = 'ol' then
  begin
    DescrBeginOrderedList;
    ConvertListItems;
    DescrEndOrderedList;
    Result := True;
  end else if Node.NodeName = 'dl' then
  begin
    DescrBeginDefinitionList;
    ConvertDefinitionList;
    DescrEndDefinitionList;
    Result := True;
  end else
    Result := False;
end;


end.


{
  $Log$
  Revision 1.1  2003-03-17 23:03:20  michael
  + Initial import in CVS

  Revision 1.9  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.8  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.7  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.6  2001/07/27 10:21:42  sg
  * Just a new, improved version ;)
    (detailed changelogs will be provided again with the next commits)

  Revision 1.5  2000/11/13 00:18:42  sg
  * Comments within descriptions should be ignored now in all cases

  Revision 1.4  2000/11/11 23:53:56  sg
  * Added <pre> tag (with unified handling of <pre> and <code>)

  Revision 1.3  2000/10/30 21:19:59  sg
  * Changed syntax highlighting attribute in 'code' element from
    'highlight' to 'highlighter'
}
