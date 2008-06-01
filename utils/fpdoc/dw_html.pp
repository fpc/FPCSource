{
    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * HTML/XHTML output generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$mode objfpc}
{$H+}

unit dw_HTML;

interface

uses Classes, DOM, DOM_HTML, dGlobals, PasTree, dWriter, ChmWriter, ChmBase;

const
  // Subpage indices for modules
  ResstrSubindex = 1;
  ConstsSubindex = 2;
  TypesSubindex = 3;
  ClassesSubindex = 4;
  ProcsSubindex = 5;
  VarsSubindex = 6;
  // Maybe needed later for topic overview ??
  TopicsSubIndex = 7;
  IndexSubIndex = 8;

  // Subpage indices for classes
  PropertiesByInheritanceSubindex = 1;
  PropertiesByNameSubindex = 2;
  MethodsByInheritanceSubindex = 3;
  MethodsByNameSubindex = 4;
  EventsByInheritanceSubindex = 5;
  EventsByNameSubindex = 6;

type

  TFileAllocator = class
  public
    procedure AllocFilename(AElement: TPasElement; ASubindex: Integer); virtual;
    function GetFilename(AElement: TPasElement;
      ASubindex: Integer): String; virtual; abstract;
    function GetRelativePathToTop(AElement: TPasElement): String; virtual;
    function GetCSSFilename(ARelativeTo: TPasElement): DOMString; virtual;
  end;

  TShortNameFileAllocator = class(TFileAllocator)
  private
    FExtension: String;
  public
    constructor Create(const AExtension: String);
    procedure AllocFilename(AElement: TPasElement; ASubindex: Integer); override;
    property Extension: String read FExtension;
  end;

  TLongNameFileAllocator = class(TFileAllocator)
  private
    FExtension: String;
  public
    constructor Create(const AExtension: String);
    function GetFilename(AElement: TPasElement;
      ASubindex: Integer): String; override;
    function GetRelativePathToTop(AElement: TPasElement): String; override;
    property Extension: String read FExtension;
  end;


  TPageInfo = class
    Element: TPasElement;
    SubpageIndex: Integer;
  end;


  { THTMLWriter }

  THTMLWriter = class(TFPDocWriter)
  private
    FOnTest: TNotifyEvent;
    FPackage: TPasPackage;
    FCharSet : String;
    function GetPageCount: Integer;
    procedure SetOnTest(const AValue: TNotifyEvent);
  protected
    FAllocator: TFileAllocator;
    Procedure CreateAllocator; virtual;
    CurDirectory: String;       // relative to curdir of process
    BaseDirectory: String;      // relative path to package base directory
    PageInfos: TObjectList;     // list of TPageInfo objects

    Doc: THTMLDocument;
    BodyElement, TitleElement: TDOMElement;

    Module: TPasModule;

    OutputNodeStack: TList;
    CurOutputNode: TDOMNode;
    InsideHeadRow, DoPasHighlighting: Boolean;
    HighlighterFlags: Byte;

    FooterFile: string;
    FIDF : Boolean;
    FDateFormat: String;
    FIndexColCount : Integer;
    FSearchPage : String;
    FBaseImageURL : String;
    function ResolveLinkID(const Name: String): DOMString;
    function ResolveLinkIDInUnit(const Name,UnitName: String): DOMString;
    function ResolveLinkWithinPackage(AElement: TPasElement;
      ASubpageIndex: Integer): String;

    // Helper functions for creating DOM elements
    function CreateEl(Parent: TDOMNode; const AName: DOMString): THTMLElement;
    function CreatePara(Parent: TDOMNode): THTMLElement;
    function CreateH1(Parent: TDOMNode): THTMLElement;
    function CreateH2(Parent: TDOMNode): THTMLElement;
    function CreateH3(Parent: TDOMNode): THTMLElement;
    function CreateTable(Parent: TDOMNode): THTMLElement;
    function CreateContentTable(Parent: TDOMNode): THTMLElement;
    function CreateTR(Parent: TDOMNode): THTMLElement;
    function CreateTD(Parent: TDOMNode): THTMLElement;
    function CreateTD_vtop(Parent: TDOMNode): THTMLElement;
    function CreateLink(Parent: TDOMNode; const AHRef: DOMString): THTMLElement;
    function CreateAnchor(Parent: TDOMNode; const AName: DOMString): THTMLElement;
    function CreateCode(Parent: TDOMNode): THTMLElement;
    function CreateWarning(Parent: TDOMNode): THTMLElement;

    // Description node conversion
    procedure PushOutputNode(ANode: TDOMNode);
    procedure PopOutputNode;
    procedure DescrWriteText(const AText: DOMString); override;
    procedure DescrBeginBold; override;
    procedure DescrEndBold; override;
    procedure DescrBeginItalic; override;
    procedure DescrEndItalic; override;
    procedure DescrBeginEmph; override;
    procedure DescrEndEmph; override;
    procedure DescrWriteImageEl(const AFileName, ACaption, ALinkName : DOMString); override;
    procedure DescrWriteFileEl(const AText: DOMString); override;
    procedure DescrWriteKeywordEl(const AText: DOMString); override;
    procedure DescrWriteVarEl(const AText: DOMString); override;
    procedure DescrBeginLink(const AId: DOMString); override;
    procedure DescrEndLink; override;
    procedure DescrWriteLinebreak; override;
    procedure DescrBeginParagraph; override;
    procedure DescrEndParagraph; override;
    procedure DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String); override;
    procedure DescrWriteCodeLine(const ALine: String); override;
    procedure DescrEndCode; override;
    procedure DescrBeginOrderedList; override;
    procedure DescrEndOrderedList; override;
    procedure DescrBeginUnorderedList; override;
    procedure DescrEndUnorderedList; override;
    procedure DescrBeginDefinitionList; override;
    procedure DescrEndDefinitionList; override;
    procedure DescrBeginListItem; override;
    procedure DescrEndListItem; override;
    procedure DescrBeginDefinitionTerm; override;
    procedure DescrEndDefinitionTerm; override;
    procedure DescrBeginDefinitionEntry; override;
    procedure DescrEndDefinitionEntry; override;
    procedure DescrBeginSectionTitle; override;
    procedure DescrBeginSectionBody; override;
    procedure DescrEndSection; override;
    procedure DescrBeginRemark; override;
    procedure DescrEndRemark; override;
    procedure DescrBeginTable(ColCount: Integer; HasBorder: Boolean); override;
    procedure DescrEndTable; override;
    procedure DescrBeginTableCaption; override;
    procedure DescrEndTableCaption; override;
    procedure DescrBeginTableHeadRow; override;
    procedure DescrEndTableHeadRow; override;
    procedure DescrBeginTableRow; override;
    procedure DescrEndTableRow; override;
    procedure DescrBeginTableCell; override;
    procedure DescrEndTableCell; override;


    procedure AppendText(Parent: TDOMNode; const AText: DOMString);
    procedure AppendNbSp(Parent: TDOMNode; ACount: Integer);
    procedure AppendSym(Parent: TDOMNode; const AText: DOMString);
    procedure AppendKw(Parent: TDOMNode; const AText: DOMString);
    function AppendPasSHFragment(Parent: TDOMNode; const AText: String;
      AShFlags: Byte): Byte;
    Procedure AppendShortDescr(AContext : TPasElement;Parent: TDOMNode; DocNode : TDocNode);
    procedure AppendShortDescr(Parent: TDOMNode; Element: TPasElement);
    procedure AppendDescr(AContext: TPasElement; Parent: TDOMNode;
      DescrNode: TDOMElement; AutoInsertBlock: Boolean);
    procedure AppendDescrSection(AContext: TPasElement; Parent: TDOMNode;
      DescrNode: TDOMElement; const ATitle: DOMString);
    procedure AppendShortDescrCell(Parent: TDOMNode; Element: TPasElement);
    function AppendHyperlink(Parent: TDOMNode; Element: TPasElement): TDOMElement;
    function AppendType(CodeEl, TableEl: TDOMElement;
      Element: TPasType; Expanded: Boolean;
      NestingLevel: Integer = 0): TDOMElement;
    function AppendProcType(CodeEl, TableEl: TDOMElement;
      Element: TPasProcedureType; Indent: Integer): TDOMElement;
    procedure AppendProcExt(CodeEl: TDOMElement; Element: TPasProcedure);
    procedure AppendProcDecl(CodeEl, TableEl: TDOMElement;
      Element: TPasProcedureBase);
    procedure AppendProcArgsSection(Parent: TDOMNode;
      Element: TPasProcedureType);
    function AppendRecordType(CodeEl, TableEl: TDOMElement;
      Element: TPasRecordType; NestingLevel: Integer): TDOMElement;

    procedure AppendTitle(const AText: DOMString);
    procedure AppendMenuBar(ASubpageIndex: Integer);
    procedure AppendTopicMenuBar(Topic : TTopicElement);
    procedure AppendSourceRef(AElement: TPasElement);
    procedure FinishElementPage(AElement: TPasElement);
    Procedure AppendSeeAlsoSection(AElement : TPasElement;DocNode : TDocNode);
    Procedure AppendExampleSection(AElement : TPasElement;DocNode : TDocNode);
    procedure AppendFooter;
    procedure CreateIndexPage(L : TStringList);
    procedure CreateModuleIndexPage(AModule: TPasModule);
    procedure CreatePageBody(AElement: TPasElement; ASubpageIndex: Integer); virtual;
    procedure CreatePackagePageBody;
    procedure CreatePackageIndex;
    procedure AddModuleIdentifiers(AModule : TPasModule; L : TStrings);
    Procedure CreateTopicPageBody(AElement : TTopicElement);
    procedure CreateModulePageBody(AModule: TPasModule; ASubpageIndex: Integer);
    procedure CreateConstPageBody(AConst: TPasConst);
    procedure CreateTypePageBody(AType: TPasType);
    procedure CreateClassPageBody(AClass: TPasClassType; ASubpageIndex: Integer);
    procedure CreateClassMemberPageBody(AElement: TPasElement);
    procedure CreateVarPageBody(AVar: TPasVariable);
    procedure CreateProcPageBody(AProc: TPasProcedureBase);
    Procedure CreateTopicLinks(Node : TDocNode; PasElement : TPasElement);
  public
    constructor Create(APackage: TPasPackage; AEngine: TFPDocEngine); override;
    destructor Destroy; override;

    // Single-page generation
    function CreateHTMLPage(AElement: TPasElement;
      ASubpageIndex: Integer): TXMLDocument;
    function CreateXHTMLPage(AElement: TPasElement;
      ASubpageIndex: Integer): TXMLDocument;

    // For producing complete package documentation
    procedure WriteHTMLPages; virtual;
    procedure WriteXHTMLPages;

    Function InterPretOption(Const Cmd,Arg : String) : boolean; override;
    Procedure WriteDoc; override;
    class procedure Usage(List: TStrings); override;
    Property SearchPage: String Read FSearchPage Write FSearchPage;
    property Allocator: TFileAllocator read FAllocator;
    property Package: TPasPackage read FPackage;
    property PageCount: Integer read GetPageCount;
    Property IncludeDateInFooter : Boolean Read FIDF Write FIDF;
    Property DateFormat : String Read FDateFormat Write FDateFormat;
    property OnTest: TNotifyEvent read FOnTest write SetOnTest;
    Property CharSet : String Read FCharSet Write FCharSet;
    Property IndexColCount : Integer Read FIndexColCount write FIndexColCount;
    Property BaseImageURL : String Read FBaseImageURL Write FBaseImageURL;
  end;

  THTMWriter = class(THTMLWriter)
  Protected
    Procedure CreateAllocator; override;
  end;

  {$DEFINE chmInterface}
  {$I dw_htmlchm.inc}
  {$UNDEF chmInterface}

implementation

uses SysUtils, XHTML, XMLRead, XMLWrite, HTMWrite, sh_pas,chmsitemap;


Function FixHTMLpath(S : String) : STring;

begin
  Result:=StringReplace(S,'\','/',[rfReplaceAll]);
end;

{$I dw_htmlchm.inc}

procedure TFileAllocator.AllocFilename(AElement: TPasElement;
  ASubindex: Integer);
begin
end;

function TFileAllocator.GetRelativePathToTop(AElement: TPasElement): String;
begin
  SetLength(Result, 0);
end;

function TFileAllocator.GetCSSFilename(ARelativeTo: TPasElement): DOMString;
begin
  Result := GetRelativePathToTop(ARelativeTo) + 'fpdoc.css';
end;


constructor TShortNameFileAllocator.Create(const AExtension: String);
begin
  inherited Create;
  FExtension := AExtension;
end;

procedure TShortNameFileAllocator.AllocFilename(AElement: TPasElement;
  ASubindex: Integer);
begin
  // !!!: Add element to file list
end;


constructor TLongNameFileAllocator.Create(const AExtension: String);
begin
  inherited Create;
  FExtension := AExtension;
end;

function TLongNameFileAllocator.GetFilename(AElement: TPasElement;
  ASubindex: Integer): String;
var
  s: String;
  i: Integer;
begin
  if AElement.ClassType = TPasPackage then
    Result := 'index'
  else if AElement.ClassType = TPasModule then
    Result := LowerCase(AElement.Name) + PathDelim + 'index'
  else
  begin
    if AElement is TPasOperator then
    begin
      Result := LowerCase(AElement.Parent.PathName) + '.op-';
      s := Copy(AElement.Name, Pos(' ', AElement.Name) + 1, Length(AElement.Name));
      s := Copy(s, 1, Pos('(', s) - 1);
      if s = ':=' then
        s := 'assign'
      else if s = '+' then
        s := 'add'
      else if s = '-' then
        s := 'sub'
      else if s = '*' then
        s := 'mul'
      else if s = '/' then
        s := 'div'
      else if s = '**' then
        s := 'power'
      else if s = '=' then
        s := 'equal'
      else if s = '<>' then
        s := 'unequal'
      else if s = '<' then
        s := 'less'
      else if s = '<=' then
        s := 'lessequal'
      else if s = '>' then
        s := 'greater'
      else if s = '>=' then
        s := 'greaterthan'
      else if s = '><' then
        s := 'symmetricdifference';
      Result := Result + s + '-';
      s := '';
      i := 1;
      while AElement.Name[i] <> '(' do
        Inc(i);
      Inc(i);
      while AElement.Name[i] <> ')' do
      begin
        if AElement.Name[i] = ',' then
	begin
	  s := s + '-';
	  Inc(i);
	end else
	  s := s + AElement.Name[i];
        Inc(i);
      end;
      Result := Result + LowerCase(s) + '-' + LowerCase(Copy(AElement.Name,
        Pos('):', AElement.Name) + 3, Length(AElement.Name)));
    end else
      Result := LowerCase(AElement.PathName);
    i := 1;
    if (Length(Result) > 0) and (Result[1] = '#') then
    begin
      while Result[i] <> '.' do
        Inc(i);
      Result := Copy(Result, i + 1, Length(Result));
    end;
    i := 1;
    while (i <= Length(Result)) and (Result[i] <> '.') do
      Inc(i);
    if (i <= Length(Result)) and (i > 0) then
      Result[i] := PathDelim;
  end;

  if ASubindex > 0 then
    Result := Result + '-' + IntToStr(ASubindex);
  Result := Result + Extension;
end;

function TLongNameFileAllocator.GetRelativePathToTop(AElement: TPasElement): String;
begin
  if (AElement.ClassType=TPasPackage) then
    Result := ''
  else if (AElement.ClassType=TTopicElement) then
    begin
    If (AElement.Parent.ClassType=TTopicElement) then
      Result:='../'+GetRelativePathToTop(AElement.Parent)
    else if (AElement.Parent.ClassType=TPasPackage) then
      Result:=''
    else if (AElement.Parent.ClassType=TPasModule) then
      Result:='../';
    end
  else
    Result := '../';
end;



constructor THTMLWriter.Create(APackage: TPasPackage; AEngine: TFPDocEngine);

  procedure AddPage(AElement: TPasElement; ASubpageIndex: Integer);
  var
    PageInfo: TPageInfo;
  begin
    PageInfo := TPageInfo.Create;
    PageInfo.Element := AElement;
    PageInfo.SubpageIndex := ASubpageIndex;
    PageInfos.Add(PageInfo);
    Allocator.AllocFilename(AElement, ASubpageIndex);
    if ASubpageIndex = 0 then
      Engine.AddLink(AElement.PathName,
        Allocator.GetFilename(AElement, ASubpageIndex));
  end;

  procedure AddTopicPages(AElement: TPasElement);

  var
    PreviousTopic,
    TopicElement : TTopicElement;
    PageInfo : TPageInfo;
    DocNode,
    TopicNode : TDocNode;

  begin
    DocNode:=Engine.FindDocNode(AElement);
    If not Assigned(DocNode) then
      exit;
    TopicNode:=DocNode.FirstChild;
    PreviousTopic:=Nil;
    While Assigned(TopicNode) do
      begin
      If TopicNode.TopicNode then
        begin
        TopicElement:=TTopicElement.Create(TopicNode.Name,AElement);
        Topics.Add(TopicElement);
        TopicElement.TopicNode:=TopicNode;
        TopicElement.Previous:=PreviousTopic;
        If Assigned(PreviousTopic) then
          PreviousTopic.Next:=TopicElement;
        PreviousTopic:=TopicElement;
        if AElement is TTopicElement then
          TTopicElement(AElement).SubTopics.Add(TopicElement);
        PageInfo := TPageInfo.Create;
        PageInfo.Element := TopicElement;
        PageInfo.SubpageIndex := 0;
        PageInfos.Add(PageInfo);
        Allocator.AllocFilename(TopicElement,0);
        Engine.AddLink(TopicElement.PathName, Allocator.GetFilename(TopicElement,0));
        if AElement is TTopicElement then
          TTopicElement(AElement).SubTopics.Add(TopicElement)
        else // Only one level of recursion.
          AddTopicPages(TopicElement);
        end;
      TopicNode:=TopicNode.NextSibling;
      end;
  end;

  procedure AddPages(AElement: TPasElement; ASubpageIndex: Integer;
    AList: TList);
  var
    i: Integer;
  begin
    if AList.Count > 0 then
    begin
      AddPage(AElement, ASubpageIndex);
      for i := 0 to AList.Count - 1 do
        AddPage(TPasElement(AList[i]), 0);
    end;
  end;

  procedure ScanModule(AModule: TPasModule);
  var
    i, j, k: Integer;
    s: String;
    ClassEl: TPasClassType;
    FPEl, AncestorMemberEl: TPasElement;
    DocNode: TDocNode;
    ALink : DOMString;
    DidAutolink: Boolean;
  begin
    AddPage(AModule, 0);
    AddPage(AModule,IndexSubIndex);
    AddTopicPages(AModule);
    with AModule do
    begin
      if InterfaceSection.ResStrings.Count > 0 then
      begin
        AddPage(AModule, ResstrSubindex);
        s := Allocator.GetFilename(AModule, ResstrSubindex);
        for i := 0 to InterfaceSection.ResStrings.Count - 1 do
          with TPasResString(InterfaceSection.ResStrings[i]) do
            Engine.AddLink(PathName, s + '#' + LowerCase(Name));
      end;
      AddPages(AModule, ConstsSubindex, InterfaceSection.Consts);
      AddPages(AModule, TypesSubindex, InterfaceSection.Types);
      if InterfaceSection.Classes.Count > 0 then
      begin
        AddPage(AModule, ClassesSubindex);
        for i := 0 to InterfaceSection.Classes.Count - 1 do
        begin
          ClassEl := TPasClassType(InterfaceSection.Classes[i]);
          AddPage(ClassEl, 0);
          // !!!: Only add when there are items
          AddPage(ClassEl, PropertiesByInheritanceSubindex);
          AddPage(ClassEl, PropertiesByNameSubindex);
          AddPage(ClassEl, MethodsByInheritanceSubindex);
          AddPage(ClassEl, MethodsByNameSubindex);
          AddPage(ClassEl, EventsByInheritanceSubindex);
          AddPage(ClassEl, EventsByNameSubindex);

          for j := 0 to ClassEl.Members.Count - 1 do
          begin
            FPEl := TPasElement(ClassEl.Members[j]);
            if ((FPEl.Visibility = visPrivate) and Engine.HidePrivate) or
              ((FPEl.Visibility = visProtected) and Engine.HideProtected) then
              continue;

            DocNode := Engine.FindDocNode(FPEl);
            if Assigned(DocNode) then
            begin
              ALink:=DocNode.Node['link'];
              If (ALink<>'') then
                Engine.AddLink(FPEl.PathName,ResolveLinkIDInUnit(ALink,AModule.name))
              else
                AddPage(FPEl, 0);
            end else  
            begin
              DidAutolink := False;
              if Assigned(ClassEl.AncestorType) and
                (ClassEl.AncestorType.ClassType = TPasClassType) then
              begin
                for k := 0 to TPasClassType(ClassEl.AncestorType).Members.Count - 1 do
                begin
                  AncestorMemberEl :=
                    TPasElement(TPasClassType(ClassEl.AncestorType).Members[k]);
                  if AncestorMemberEl.Name = FPEl.Name then
                  begin
                    DocNode := Engine.FindDocNode(AncestorMemberEl);
                    if Assigned(DocNode) then
                    begin
                      DidAutolink := True;
                      Engine.AddLink(FPEl.PathName,
                        Engine.FindAbsoluteLink(AncestorMemberEl.PathName));
                      break;
                    end;
                  end;
                end;
              end;
              if not DidAutolink then
                AddPage(FPEl, 0);
            end;
          end;
        end;
      end;
      AddPages(AModule, ProcsSubindex, InterfaceSection.Functions);
      AddPages(AModule, VarsSubindex, InterfaceSection.Variables);
    end;
  end;

var
  i: Integer;
begin
  inherited ;
  IndexColCount:=3;
  Charset:='iso-8859-1';
  CreateAllocator;
  FPackage := APackage;
  OutputNodeStack := TList.Create;

  PageInfos := TObjectList.Create;

  // Allocate page for the package itself, if a name is given (i.e. <> '#')
  if Length(Package.Name) > 1 then
    begin
    AddPage(Package, 0);
    AddPage(Package,IndexSubIndex);
    AddTopicPages(Package);
    end;

  for i := 0 to Package.Modules.Count - 1 do
    ScanModule(TPasModule(Package.Modules[i]));
end;

destructor THTMLWriter.Destroy;
begin
  PageInfos.Free;
  OutputNodeStack.Free;
  inherited Destroy;
end;

function THTMLWriter.CreateHTMLPage(AElement: TPasElement;
  ASubpageIndex: Integer): TXMLDocument;
var
  HTMLEl: THTMLHtmlElement;
  HeadEl: THTMLHeadElement;
  El: TDOMElement;
begin
  Doc := THTMLDocument.Create;
  Result := Doc;
  Doc.AppendChild(Doc.CreateProcessingInstruction(
    'DOCTYPE', 'HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"'));

  HTMLEl := Doc.CreateHtmlElement;
  Doc.AppendChild(HTMLEl);

  HeadEl := Doc.CreateHeadElement;
  HTMLEl.AppendChild(HeadEl);
  El := Doc.CreateElement('meta');
  HeadEl.AppendChild(El);
  El['http-equiv'] := 'Content-Type';
  
  El['content'] := Format('text/html; charset=%s',[charset]);
  TitleElement := Doc.CreateElement('title');
  HeadEl.AppendChild(TitleElement);
  El := Doc.CreateElement('link');

  BodyElement := Doc.CreateElement('body');
  HTMLEl.AppendChild(BodyElement);

  CreatePageBody(AElement, ASubpageIndex);
  AppendFooter;

  HeadEl.AppendChild(El);
  El['rel'] := 'stylesheet';
  El['type'] := 'text/css';
  El['href'] := FixHtmlPath(Allocator.GetCSSFilename(AElement));
end;

function THTMLWriter.CreateXHTMLPage(AElement: TPasElement;
  ASubpageIndex: Integer): TXMLDocument;
begin
  Result := nil;
end;

procedure CreatePath(const AFilename: String);
var
  EndIndex: Integer;
  Path: String;
begin
  EndIndex := Length(AFilename);
  if EndIndex = 0 then
    exit;
  while not (AFilename[EndIndex] in AllowDirectorySeparators) do
  begin
    Dec(EndIndex);
    if EndIndex = 0 then
      exit;
  end;

  Path := Copy(AFilename, 1, EndIndex - 1);
  if not DirectoryExists(Path) then
  begin
    CreatePath(Path);
    MkDir(Path);
  end;
end;

procedure THTMLWriter.WriteHTMLPages;
var
  i: Integer;
  PageDoc: TXMLDocument;
  Filename: String;
begin
  if Engine.Output <> '' then
    Engine.Output := IncludeTrailingBackSlash(Engine.Output);
  for i := 0 to PageInfos.Count - 1 do
    with TPageInfo(PageInfos[i]) do
    begin
      PageDoc := CreateHTMLPage(Element, SubpageIndex);
      try
        Filename := Engine.Output + Allocator.GetFilename(Element, SubpageIndex);
        try
          CreatePath(Filename);
          WriteHTMLFile(PageDoc, Filename);
        except
	  on E: Exception do
            WriteLn(Format(SErrCouldNotCreateFile, [FileName, e.Message]));
        end;
      finally
        PageDoc.Free;
      end;
    end;
end;

procedure THTMLWriter.WriteXHTMLPages;
begin
end;

{procedure THTMLWriter.CreateDoc(const ATitle: DOMString;
  AElement: TPasElement; const AFilename: String);
var
  El: TDOMElement;
  DocInfo: TDocInfo;
  CSSName: String;

begin
  Doc := TXHTMLDocument.Create;
  with TXHTMLDocument(Doc) do
  begin
    Encoding := 'ISO8859-1';
    CSSName := 'fpdoc.css';
    if Assigned(Module) then
      CSSName := '../' + CSSName;
$IFNDEF ver1_0
    StylesheetType := 'text/css';
    StylesheetHRef := CSSName;
$ENDIF
    CreateRoot(xhtmlStrict);
    with RequestHeadElement do
    begin
      AppendText(RequestTitleElement, ATitle);
      El := CreateElement('link');
      AppendChild(El);
      El['rel'] := 'stylesheet';
      El['type'] := 'text/css';
      El['href'] := FixHtmlPath(CSSName);
    end;
    Self.BodyElement := RequestBodyElement('en');
  end;

  if Length(AFilename) > 0 then
  begin
    DocInfo := TDocInfo.Create;
    DocInfos.Add(DocInfo);
    DocInfo.Element := AElement;
    DocInfo.Filename := AFilename;
  end;
end;
}


{ Used for:
  - <link> elements in descriptions
  - "see also" entries
  - AppendHyperlink (for unresolved parse tree element links)
}

function THTMLWriter.ResolveLinkIDInUnit(const Name,UnitName: String): DOMString;

begin
  Result:=ResolveLinkID(Name);
  If (Result='') and (UnitName<>'')  then
    Result:=ResolveLinkID(UnitName+'.'+Name);
end;

function THTMLWriter.ResolveLinkID(const Name: String): DOMString;
var
  i: Integer;
  ThisPackage: TLinkNode;
begin
  if Length(Name) = 0 then
  begin
    SetLength(Result, 0);
    exit;
  end;

  if Name[1] = '#' then
    Result := Engine.FindAbsoluteLink(Name)
  else
  begin
    SetLength(Result, 0);
    { Try all packages }
    ThisPackage := Engine.RootLinkNode.FirstChild;
    while Assigned(ThisPackage) do
    begin
      Result := Engine.FindAbsoluteLink(ThisPackage.Name + '.' + Name);
      if Length(Result) = 0 then
      begin
        if Assigned(Module) then
          begin
          Result := Engine.FindAbsoluteLink(Module.PathName + '.' + Name);
//          WriteLn('Searching for ', Module.PathName + '.' + Name, ' => ', Result);
          end;
        if Length(Result) = 0 then
          for i := Length(Name) downto 1 do
            if Name[i] = '.' then
            begin
              Result := ResolveLinkID(Copy(Name, 1, i - 1));
              exit;
            end;
      end;
      ThisPackage := ThisPackage.NextSibling;
    end;
  end;

  if Length(Result) > 0 then
    if Copy(Result, 1, Length(CurDirectory) + 1) = CurDirectory + '/' then
      Result := Copy(Result, Length(CurDirectory) + 2, Length(Result))
    else if not IsLinkAbsolute(Result) then
      Result := BaseDirectory + Result;
end;

function THTMLWriter.ResolveLinkWithinPackage(AElement: TPasElement;
  ASubpageIndex: Integer): String;
var
  ParentEl: TPasElement;
begin
  ParentEl := AElement;
  while Assigned(ParentEl) and not (ParentEl.ClassType = TPasPackage) do
    ParentEl := ParentEl.Parent;
  if Assigned(ParentEl) and (TPasPackage(ParentEl) = Engine.Package) then
  begin
    Result := Allocator.GetFilename(AElement, ASubpageIndex);
    if Copy(Result, 1, Length(CurDirectory) + 1) = CurDirectory + '/' then
      Result := Copy(Result, Length(CurDirectory) + 2, Length(Result))
    else
      Result := BaseDirectory + Result;
  end else
    SetLength(Result, 0);
end;

function THTMLWriter.CreateEl(Parent: TDOMNode;
  const AName: DOMString): THTMLElement;
begin
  Result := Doc.CreateElement(AName);
  Parent.AppendChild(Result);
end;

function THTMLWriter.CreatePara(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'p');
end;

function THTMLWriter.CreateH1(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'h1');
end;

function THTMLWriter.CreateH2(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'h2');
end;

function THTMLWriter.CreateH3(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'h3');
end;

function THTMLWriter.CreateTable(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'table');
  Result['cellspacing'] := '0';
  Result['cellpadding'] := '0';
end;

function THTMLWriter.CreateContentTable(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'table');
end;

function THTMLWriter.CreateTR(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'tr');
end;

function THTMLWriter.CreateTD(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'td');
end;

function THTMLWriter.CreateTD_vtop(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'td');
  Result['valign'] := 'top';
end;

function THTMLWriter.CreateLink(Parent: TDOMNode;
  const AHRef: DOMString): THTMLElement;
begin
  Result := CreateEl(Parent, 'a');
  Result['href'] := FixHtmlPath(AHRef);
end;

function THTMLWriter.CreateAnchor(Parent: TDOMNode;
  const AName: DOMString): THTMLElement;
begin
  Result := CreateEl(Parent, 'a');
  Result['name'] := AName;
end;

function THTMLWriter.CreateCode(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(CreateEl(Parent, 'tt'), 'span');
  Result['class'] := 'code';
end;

function THTMLWriter.CreateWarning(Parent: TDOMNode): THTMLElement;
begin
  Result := CreateEl(Parent, 'span');
  Result['class'] := 'warning';
end;


procedure THTMLWriter.PushOutputNode(ANode: TDOMNode);
begin
  OutputNodeStack.Add(CurOutputNode);
  CurOutputNode := ANode;
end;

procedure THTMLWriter.PopOutputNode;
begin
  CurOutputNode := TDOMNode(OutputNodeStack[OutputNodeStack.Count - 1]);
  OutputNodeStack.Delete(OutputNodeStack.Count - 1);
end;

procedure THTMLWriter.DescrWriteText(const AText: DOMString);
begin
  AppendText(CurOutputNode, AText);
end;

procedure THTMLWriter.DescrBeginBold;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'b'));
end;

procedure THTMLWriter.DescrEndBold;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginItalic;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'i'));
end;

procedure THTMLWriter.DescrEndItalic;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginEmph;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'em'));
end;

procedure THTMLWriter.DescrEndEmph;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrWriteImageEl(const AFileName, ACaption, ALinkName : DOMString);

Var
  Pel,Cel,Lel : TDOMNode;
  El :TDomElement;
  D : String;
  L : Integer;
   
begin
  // Determine parent node.
  If (ACaption='') then
    Pel:=CurOutputNode
  else
    begin
    Cel:=CreateTable(CurOutputNode);
    Pel:=CreateTD(CreateTR(Cel));
    Cel:=CreateTD(CreateTR(Cel));
    If (ALinkName<>'') then
      Cel:=CreateAnchor(Cel,ALinkName);
    AppendText(Cel,ACaption);
    end;
  // Determine URL for image.  
  D:=BaseImageURL;
  If (D='') then
    begin
    If (Module=Nil) then
      D:=Allocator.GetRelativePathToTop(Package)
    else 
      D:=Allocator.GetRelativePathToTop(Module);
    L:=Length(D);  
    If (L>0) and (D[L]<>'/') then
      D:=D+'/';
    D:=D+'images/';
    end
  else  
    L:=Length(D);  
    If (L>0) and (D[L]<>'/') then
      D:=D+'/';
  // Create image node.  
  El:=CreateEl(Pel,'img');
  EL['src']:=D+AFileName;
  El['alt']:=ACaption;
end;

procedure THTMLWriter.DescrWriteFileEl(const AText: DOMString);
var
  NewEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'span');
  NewEl['class'] := 'file';
  AppendText(NewEl, AText);
end;

procedure THTMLWriter.DescrWriteKeywordEl(const AText: DOMString);
var
  NewEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'span');
  NewEl['class'] := 'kw';
  AppendText(NewEl, AText);
end;

procedure THTMLWriter.DescrWriteVarEl(const AText: DOMString);
begin
  AppendText(CreateEl(CurOutputNode, 'var'), AText);
end;

procedure THTMLWriter.DescrBeginLink(const AId: DOMString);
var
  a,s: String;
begin
  a:=AId;
  s := ResolveLinkID(a);
  if Length(s) = 0 then
  begin

    WriteLn(Format(SErrUnknownLinkID, [a]));
    PushOutputNode(CreateEl(CurOutputNode, 'b'));
  end else
    PushOutputNode(CreateLink(CurOutputNode, s));
end;

procedure THTMLWriter.DescrEndLink;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrWriteLinebreak;
begin
  CreateEl(CurOutputNode, 'br');
end;

procedure THTMLWriter.DescrBeginParagraph;
begin
  PushOutputNode(CreatePara(CurOutputNode));
end;

procedure THTMLWriter.DescrEndParagraph;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String);
begin
  DoPasHighlighting := (AHighlighterName = '') or (AHighlighterName = 'Pascal');
  HighlighterFlags := 0;
  PushOutputNode(CreateEl(CurOutputNode, 'pre'));
end;

procedure THTMLWriter.DescrWriteCodeLine(const ALine: String);
begin
  if DoPasHighlighting then
  begin
    HighlighterFlags := AppendPasSHFragment(CurOutputNode, ALine,
      HighlighterFlags);
    AppendText(CurOutputNode, #10);
  end else
    AppendText(CurOutputNode, ALine + #10);
end;

procedure THTMLWriter.DescrEndCode;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginOrderedList;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'ol'));
end;

procedure THTMLWriter.DescrEndOrderedList;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginUnorderedList;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'ul'));
end;

procedure THTMLWriter.DescrEndUnorderedList;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginDefinitionList;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'dl'));
end;

procedure THTMLWriter.DescrEndDefinitionList;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginListItem;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'li'));
end;

procedure THTMLWriter.DescrEndListItem;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginDefinitionTerm;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'dt'));
end;

procedure THTMLWriter.DescrEndDefinitionTerm;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginDefinitionEntry;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'dd'));
end;

procedure THTMLWriter.DescrEndDefinitionEntry;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginSectionTitle;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'h3'));
end;

procedure THTMLWriter.DescrBeginSectionBody;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrEndSection;
begin
end;

procedure THTMLWriter.DescrBeginRemark;
var
  NewEl, TDEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'table');
  NewEl['width'] := '100%';
  NewEl['border'] := '0';
  NewEl['CellSpacing'] := '0';
  NewEl['class'] := 'remark';
  NewEl := CreateTR(NewEl);
  TDEl := CreateTD(NewEl);
  TDEl['valign'] := 'top';
  TDEl['class'] := 'pre';
  AppendText(CreateEl(TDEl, 'b'), SDocRemark);
  PushOutputNode(CreateTD(NewEl));
end;

procedure THTMLWriter.DescrEndRemark;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTable(ColCount: Integer; HasBorder: Boolean);
var
  Table: TDOMElement;
begin
  Table := CreateEl(CurOutputNode, 'table');
  Table['border'] := IntToStr(Ord(HasBorder));
  PushOutputNode(Table);
end;

procedure THTMLWriter.DescrEndTable;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableCaption;
begin
  PushOutputNode(CreateEl(CurOutputNode, 'caption'));
end;

procedure THTMLWriter.DescrEndTableCaption;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableHeadRow;
begin
  PushOutputNode(CreateTr(CurOutputNode));
  InsideHeadRow := True;
end;

procedure THTMLWriter.DescrEndTableHeadRow;
begin
  InsideHeadRow := False;
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableRow;
begin
  PushOutputNode(CreateTR(CurOutputNode));
end;

procedure THTMLWriter.DescrEndTableRow;
begin
  PopOutputNode;
end;

procedure THTMLWriter.DescrBeginTableCell;
begin
  if InsideHeadRow then
    PushOutputNode(CreateEl(CurOutputNode, 'th'))
  else
    PushOutputNode(CreateTD(CurOutputNode));
end;

procedure THTMLWriter.DescrEndTableCell;
begin
  PopOutputNode;
end;




procedure THTMLWriter.AppendText(Parent: TDOMNode; const AText: DOMString);
begin
  Parent.AppendChild(Doc.CreateTextNode(AText));
end;

procedure THTMLWriter.AppendNbSp(Parent: TDOMNode; ACount: Integer);
begin
  while ACount > 0 do
  begin
    Parent.AppendChild(Doc.CreateEntityReference('nbsp'));
    Dec(ACount);
  end;
end;

procedure THTMLWriter.AppendSym(Parent: TDOMNode; const AText: DOMString);
var
  El: TDOMElement;
begin
  El := CreateEl(Parent, 'span');
  El['class'] := 'sym';
  AppendText(El, AText);
end;

procedure THTMLWriter.AppendKw(Parent: TDOMNode; const AText: DOMString);
var
  El: TDOMElement;
begin
  El := CreateEl(Parent, 'span');
  El['class'] := 'kw';
  AppendText(El, AText);
end;

function THTMLWriter.AppendPasSHFragment(Parent: TDOMNode;
  const AText: String; AShFlags: Byte): Byte;


var
  Line, Last, p: PChar;
  IsInSpecial: Boolean;
  lastwasasm : boolean;
  El: TDOMElement;

  Procedure MaybeOutput;

  Var
    CurParent: TDomNode;

  begin
    If (Last<>Nil) then
      begin
      If (el<>Nil) then
        CurParent:=El
      else
        CurParent:=Parent;
      AppendText(CurParent,Last);
      El:=Nil;
      Last:=Nil;
      end;
  end;

  Function NewEl(Const ElType,Attr,AttrVal : String) : TDomElement;

  begin
    Result:=CreateEl(Parent,ElType);
    Result[Attr]:=AttrVal;
  end;

  Function NewSpan(Const AttrVal : String) : TDomElement;

  begin
    Result:=CreateEl(Parent,'span');
    Result['class']:=AttrVal;
  end;

begin
  GetMem(Line, Length(AText) * 3 + 4);
  Try
  DoPascalHighlighting(AShFlags, PChar(AText), Line);
  Result := AShFlags;
  IsInSpecial := False;
  Last := Nil;
  p := Line;
  el:=nil;
  while p[0] <> #0 do
  begin
    if p[0] = LF_ESCAPE then
      begin
      p[0] := #0;
      MaybeOutput;
      case Ord(p[1]) of
        shDefault:    El:=Nil;
        shInvalid:    El:=newel('font','color','red');
        shSymbol :    El:=newspan('sym');
        shKeyword:    El:=newspan('kw');
        shComment:    El:=newspan('cmt');
        shDirective:  El:=newspan('dir');
        shNumbers:    El:=newspan('num');
        shCharacters: El:=newspan('chr');
        shStrings:    El:=newspan('str');
        shAssembler:  El:=newspan('asm');
      end;
      Inc(P);
      end
    else If (Last=Nil) then
      Last:=P;
    Inc(p);
  end;
  MaybeOutput;
  Finally
    FreeMem(Line);
  end;
end;

Procedure THTMLWriter.AppendShortDescr(AContext: TPasElement; Parent: TDOMNode; DocNode : TDocNode);

begin
  if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
    begin
    PushOutputNode(Parent);
    try
      if not ConvertShort(AContext,TDomElement(DocNode.ShortDescr)) then
        Warning(AContext, SErrInvalidShortDescr)
    finally
      PopOutputNode;
    end;
    end;
end;

procedure THTMLWriter.AppendShortDescr(Parent: TDOMNode; Element: TPasElement);

begin
  AppendShortDescr(Element,Parent,Engine.FindDocNode(Element));
end;

procedure THTMLWriter.AppendDescr(AContext: TPasElement; Parent: TDOMNode;
  DescrNode: TDOMElement; AutoInsertBlock: Boolean);
begin
  if Assigned(DescrNode) then
  begin
    PushOutputNode(Parent);
    try
      ConvertDescr(AContext, DescrNode, AutoInsertBlock);
    finally
      PopOutputNode;
    end;
  end;
end;

procedure THTMLWriter.AppendDescrSection(AContext: TPasElement;
  Parent: TDOMNode; DescrNode: TDOMElement; const ATitle: DOMString);
begin
  if not IsDescrNodeEmpty(DescrNode) then
  begin
    If (ATitle<>'') then // Can be empty for topic.
      AppendText(CreateH2(Parent), ATitle);
    AppendDescr(AContext, Parent, DescrNode, True);
  end;
end;


procedure THTMLWriter.AppendShortDescrCell(Parent: TDOMNode;
  Element: TPasElement);
var
  ParaEl: TDOMElement;
begin
  if Assigned(Engine.FindShortDescr(Element)) then
  begin
    AppendNbSp(CreatePara(CreateTD(Parent)), 2);
    ParaEl := CreatePara(CreateTD(Parent));
    ParaEl['class'] := 'cmt';
    AppendShortDescr(ParaEl, Element);
  end;
end;

function THTMLWriter.AppendHyperlink(Parent: TDOMNode;
  Element: TPasElement): TDOMElement;
var
  s: String;
  UnitList: TList;
  i: Integer;
  ThisPackage: TLinkNode;
begin
  if Assigned(Element) then
  begin
    if Element.InheritsFrom(TPasUnresolvedTypeRef) then
    begin
      s := ResolveLinkID(Element.Name);
      if Length(s) = 0 then
      begin
        { Try all packages }
        ThisPackage := Engine.RootLinkNode.FirstChild;
        while Assigned(ThisPackage) do
        begin
          s := ResolveLinkID(ThisPackage.Name + '.' + Element.Name);
          if Length(s) > 0 then
            break;
          ThisPackage := ThisPackage.NextSibling;
        end;
        if Length(s) = 0 then
        begin
          { Okay, then we have to try all imported units of the current module }
          UnitList := Module.InterfaceSection.UsesList;
          for i := UnitList.Count - 1 downto 0 do
          begin
            { Try all packages }
            ThisPackage := Engine.RootLinkNode.FirstChild;
            while Assigned(ThisPackage) do
            begin
              s := ResolveLinkID(ThisPackage.Name + '.' +
                TPasType(UnitList[i]).Name + '.' + Element.Name);
              if Length(s) > 0 then
                break;
              ThisPackage := ThisPackage.NextSibling;
            end;
            if Length(s) > 0 then
              break;
          end;
        end;
      end;
    end else if Element is TPasEnumValue then
      s := ResolveLinkID(Element.Parent.PathName)
    else  
      s := ResolveLinkID(Element.PathName);

    if Length(s) > 0 then
    begin
      Result := CreateLink(Parent, s);
      AppendText(Result, Element.Name);
    end else
    begin
      Result := nil;
      AppendText(Parent, Element.Name);
    end;
  end else
  begin
    Result := nil;
    AppendText(CreateWarning(Parent), '<NIL>');
  end;
end;


{ Returns the new CodeEl, which will be the old CodeEl in most cases }
function THTMLWriter.AppendType(CodeEl, TableEl: TDOMElement;
  Element: TPasType; Expanded: Boolean; NestingLevel: Integer): TDOMElement;

Var
  S : String;

begin
  Result := CodeEl;

  if not Assigned(Element) then
    AppendText(CreateWarning(CodeEl), '<NIL>')
  else if (not Expanded) and (Length(Element.Name) > 0) then
    AppendHyperlink(CodeEl, Element)
  else
  // Array
  if Element.ClassType = TPasArrayType then
  begin
    S:='array ';
    If (TPasArrayType(Element).IndexRange<>'') then
      S:=S+'[' + TPasArrayType(Element).IndexRange + '] ';
    S:=S+'of ';
    If (TPasArrayType(Element).ElType=Nil) then
      S:=S+'Const';
    AppendPasSHFragment(CodeEl,S,0);
    If (TPasArrayType(Element).ElType<>Nil) then
      Result := AppendType(CodeEl, TableEl, TPasArrayType(Element).ElType, False);
  end else
  // Procedure or funtion type
  if Element.InheritsFrom(TPasProcedureType) then
  begin
    AppendKw(CodeEl, TPasProcedureType(Element).TypeName);
    Result := AppendProcType(CodeEl, TableEl, TPasProcedureType(Element), 0)
  end else
  // Range type
  if Element.InheritsFrom(TPasRangeType) then
    AppendPasSHFragment(CodeEl, TPasRangeType(Element).RangeStart + '..' +
      TPasRangeType(Element).RangeEnd, 0)
  // Record type
  else if Element.ClassType = TPasRecordType then
    Result := AppendRecordType(CodeEl, TableEl, TPasRecordType(Element), NestingLevel)
  else if (Element.ClassType = TPasFileType) and (TPasFileType(Element).elType=Nil) then
    AppendPasSHFragment(CodeEl,'file',0)
  else
  // Other types
    AppendHyperlink(CodeEl, Element);
end;

function THTMLWriter.AppendProcType(CodeEl, TableEl: TDOMElement;
  Element: TPasProcedureType; Indent: Integer): TDOMElement;

  function CreateIndentedCodeEl(Indent: Integer): TDOMElement;
  begin
    Result := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
    AppendNbSp(Result, Indent);
  end;

var
  i: Integer;
  Arg: TPasArgument;
begin
  if Element.Args.Count > 0 then
  begin
    AppendSym(CodeEl, '(');

    for i := 0 to Element.Args.Count - 1 do
    begin
      Arg := TPasArgument(Element.Args[i]);
      CodeEl := CreateIndentedCodeEl(Indent + 2);

      case Arg.Access of
        argConst: AppendKw(CodeEl, 'const ');
        argVar: AppendKw(CodeEl, 'var ');
        argOut: AppendKw(CodeEl, 'out ');
      end;
      AppendText(CodeEl, Arg.Name);
      if Assigned(Arg.ArgType) then
      begin
        AppendSym(CodeEl, ': ');
        CodeEl := AppendType(CodeEl, TableEl, Arg.ArgType, False);
      end;
      if Length(Arg.Value) > 0 then
        AppendPasSHFragment(CodeEl, ' = ' + Arg.Value, 0);
      if i < Element.Args.Count - 1 then
        AppendSym(CodeEl, ';');
    end;

    if Element.InheritsFrom(TPasFunctionType) or Element.IsOfObject then
    begin
      CodeEl := CreateIndentedCodeEl(Indent);
      if Element.InheritsFrom(TPasFunctionType) then
      begin
        AppendSym(CodeEl, '):');
        AppendHyperlink(CodeEl, TPasFunctionType(Element).ResultEl.ResultType);
      end else
        AppendSym(CodeEl, ')');
      if Element.IsOfObject then
      begin
        AppendText(CodeEl, ' ');        // Don't remove
        AppendKw(CodeEl, 'of object');
      end;
    end else
      if Indent > 0 then
        AppendSym(CodeEl, ')')
      else
      begin
        CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
        AppendSym(CodeEl, ')');
      end;
  end else
  begin
    { Procedure or function without arguments }
    if Element.InheritsFrom(TPasFunctionType) then
    begin
      AppendSym(CodeEl, ': ');
      AppendHyperlink(CodeEl, TPasFunctionType(Element).ResultEl.ResultType);
    end;
    if Element.IsOfObject then
      AppendKw(CodeEl, ' of object');
  end;
  Result := CodeEl;
end;

procedure THTMLWriter.AppendProcExt(CodeEl: TDOMElement;
  Element: TPasProcedure);

  procedure AppendExt(const Ext: String);
  begin
    AppendKw(CodeEl, ' ' + Ext);
    AppendSym(CodeEl, ';');
  end;

begin
  if Element.IsVirtual then
    AppendExt('virtual');
  if Element.IsDynamic then
    AppendExt('dynamic');
  if Element.IsAbstract then
    AppendExt('abstract');
  if Element.IsOverride then
    AppendExt('override');
  if Element.IsOverload then
    AppendExt('overload');
  if Element.IsMessage then
    AppendExt('message');
end;


{ Used in two places:
  - Page for the method of a class
  - Page for a tandalone procedure or function. }

procedure THTMLWriter.AppendProcDecl(CodeEl, TableEl: TDOMElement;
  Element: TPasProcedureBase);

  procedure WriteVariant(AProc: TPasProcedure);
  begin
    AppendProcArgsSection(TableEl.ParentNode, AProc.ProcType);

    AppendKw(CodeEl, AProc.TypeName);
    if Element.Parent.ClassType = TPasClassType then
    begin
      AppendText(CodeEl, ' ');
      AppendHyperlink(CodeEl, Element.Parent);
      AppendSym(CodeEl, '.');
      AppendText(CodeEl, AProc.Name);
    end else
      AppendText(CodeEl, ' ' + AProc.FullName);
    CodeEl := AppendProcType(CodeEl, TableEl, AProc.ProcType, 0);
    AppendSym(CodeEl, ';');
    AppendProcExt(CodeEl, AProc);
  end;

var
  i: Integer;
begin
  if Element.ClassType = TPasOverloadedProc then
    for i := 0 to TPasOverloadedProc(Element).Overloads.Count - 1 do
    begin
      if i > 0 then
      begin
        CreateEl(CodeEl, 'br');
        CreateEl(CodeEl, 'br');
      end;
      WriteVariant(TPasProcedure(TPasOverloadedProc(Element).Overloads[i]));
    end
  else
    WriteVariant(TPasProcedure(Element));
end;

procedure THTMLWriter.AppendProcArgsSection(Parent: TDOMNode;
  Element: TPasProcedureType);
var
  HasFullDescr, IsFirst: Boolean;
  ResultEl: TPasResultElement;
  ArgTableEl, TREl: TDOMElement;
  DocNode: TDocNode;
  i: Integer;
  Arg: TPasArgument;
begin
  IsFirst := True;
  for i := 0 to Element.Args.Count - 1 do
  begin
    Arg := TPasArgument(Element.Args[i]);
    if IsDescrNodeEmpty(Engine.FindShortDescr(Arg)) then
      continue;
    if IsFirst then
    begin
      IsFirst := False;
      AppendText(CreateH2(Parent), SDocArguments);
      ArgTableEl := CreateTable(Parent);
    end;
    TREl := CreateTR(ArgTableEl);
    AppendText(CreateCode(CreatePara(CreateTD_vtop(TREl))), Arg.Name);
    AppendShortDescrCell(TREl, Arg);
  end;

  if Element.ClassType = TPasFunctionType then
  begin
    ResultEl := TPasFunctionType(Element).ResultEl;
    DocNode := Engine.FindDocNode(ResultEl);
    HasFullDescr := Assigned(DocNode) and not IsDescrNodeEmpty(DocNode.Descr);
    if HasFullDescr or
      (Assigned(DocNode) and not IsDescrNodeEmpty(DocNode.ShortDescr)) then
    begin
      AppendText(CreateH2(Parent), SDocFunctionResult);
      if HasFullDescr then
        AppendDescr(ResultEl, Parent, DocNode.Descr, True)
      else
        AppendDescr(ResultEl, CreatePara(Parent), DocNode.ShortDescr, False);
    end;
  end;
end;

function THTMLWriter.AppendRecordType(CodeEl, TableEl: TDOMElement;
  Element: TPasRecordType; NestingLevel: Integer): TDOMElement;
var
  i, j: Integer;
  Variable: TPasVariable;
  TREl, TDEl: TDOMElement;
  CurVariant: TPasVariant;
begin
  if not (Element.Parent is TPasVariant) then
    if Element.IsPacked then
      If Element.IsBitPacked then
        AppendKw(CodeEl, 'bitpacked record')
      else
        AppendKW(CodeEl, 'packed record')
    else
      AppendKw(CodeEl, 'record');

  for i := 0 to Element.Members.Count - 1 do
  begin
    Variable := TPasVariable(Element.Members[i]);
    TREl := CreateTR(TableEl);
    CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
    AppendShortDescrCell(TREl, Variable);
    AppendNbSp(CodeEl, NestingLevel * 2 + 2);
    AppendText(CodeEl, Variable.Name);
    AppendSym(CodeEl, ': ');
    CodeEl := AppendType(CodeEl, TableEl, Variable.VarType, False, NestingLevel + 1);
    AppendSym(CodeEl, ';');
  end;

  if Assigned(Element.VariantType) then
  begin
    TREl := CreateTR(TableEl);
    CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
    AppendNbSp(CodeEl, NestingLevel * 2 + 2);
    AppendKw(CodeEl, 'case ');
    if TPasRecordType(Element).VariantName <> '' then
    begin
      AppendText(CodeEl, TPasRecordType(Element).VariantName);
      AppendSym(CodeEl, ': ');
    end;
    CodeEl := AppendType(CodeEl, TableEl, TPasRecordType(Element).VariantType, True);
    AppendKw(CodeEl, ' of');
    for i := 0 to TPasRecordType(Element).Variants.Count - 1 do
    begin
      CurVariant := TPasVariant(Element.Variants[i]);
      TREl := CreateTR(TableEl);
      CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
      AppendNbSp(CodeEl, NestingLevel * 2 + 4);
      for j := 0 to CurVariant.Values.Count - 1 do
      begin
	if j > 0 then
	  AppendSym(CodeEl, ', ');
	AppendPasSHFragment(CodeEl, CurVariant.Values[j], 0);
      end;
      AppendSym(CodeEl, ': (');
      AppendType(CodeEl, TableEl, CurVariant.Members, True, NestingLevel + 3);
      CodeEl := CreateCode(CreatePara(CreateTD_vtop(CreateTR(TableEl))));
      AppendNbSp(CodeEl, NestingLevel * 2 + 6);
      AppendSym(CodeEl, ');');
    end;
  end;

  if not (Element.Parent is TPasVariant) then
  begin
    CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
    AppendText(CodeEl, ' '); // !!!: Dirty trick, necessary for current XML writer
    AppendNbSp(CodeEl, NestingLevel * 2);
    AppendKw(CodeEl, 'end');
  end;
  Result := CodeEl;
end;

procedure THTMLWriter.AppendTitle(const AText: DOMString);
begin
  AppendText(TitleElement, AText);
  AppendText(CreateH1(BodyElement), AText);
end;

procedure THTMLWriter.AppendTopicMenuBar(Topic : TTopicElement);

var
  TableEl, TREl, ParaEl, TitleEl: TDOMElement;

  procedure AddLink(El : TPasElement; const AName: String);
  begin
    AppendText(ParaEl, '[');
    AppendText(CreateLink(ParaEl, ResolveLinkWithinPackage(El,0)),AName);
    AppendText(ParaEl, ']');
  end;

begin
  TableEl := CreateEl(BodyElement, 'table');
  TableEl['cellpadding'] := '4';
  TableEl['cellspacing'] := '0';
  TableEl['border'] := '0';
  TableEl['width'] := '100%';
  TableEl['class'] := 'bar';
  TREl := CreateTR(TableEl);
  ParaEl := CreateEl(CreateTD(TREl), 'b');
  If Assigned(Topic.Previous) then
    AddLink(Topic.Previous,SDocPrevious);
  If Assigned(Topic.Parent) then
    AddLink(Topic.Parent,SDocUp);
  if Assigned(Topic.Next) then
    AddLink(Topic.Next,SDocNext);
  if Length(SearchPage) > 0 then
    begin
    AppendText(ParaEl, '[');
    AppendText(CreateLink(ParaEl, SearchPage), SDocSearch);
    AppendText(ParaEl, ']');
    end;
  ParaEl := CreateTD(TREl);
  ParaEl['align'] := 'right';
  TitleEl := CreateEl(ParaEl, 'span');
  TitleEl['class'] := 'bartitle';
  if Assigned(Module) then
    AppendText(TitleEl, Format(SDocUnitTitle, [Module.Name]));
  if Assigned(Package) then
  begin
    AppendText(TitleEl, ' (');
    AppendHyperlink(TitleEl, Package);
    AppendText(TitleEl, ')');
  end;
end;

procedure THTMLWriter.AppendMenuBar(ASubpageIndex: Integer);

var
  TableEl, TREl, ParaEl, TitleEl: TDOMElement;

  procedure AddLink(ALinkSubpageIndex: Integer; const AName: String);
  begin
    AppendText(ParaEl, '[');
    if ALinkSubpageIndex = ASubpageIndex then
      AppendText(ParaEl, AName)
    else
      AppendText(
        CreateLink(ParaEl, ResolveLinkWithinPackage(Module, ALinkSubpageIndex)),
        AName);
    AppendText(ParaEl, ']');
  end;

begin
  TableEl := CreateEl(BodyElement, 'table');
  TableEl['cellpadding'] := '4';
  TableEl['cellspacing'] := '0';
  TableEl['border'] := '0';
  TableEl['width'] := '100%';
  TableEl['class'] := 'bar';
  TREl := CreateTR(TableEl);
  ParaEl := CreateEl(CreateTD(TREl), 'b');

  if Assigned(Module) then
    begin
    AddLink(0, SDocOverview);
    if Module.InterfaceSection.ResStrings.Count > 0 then
      AddLink(ResstrSubindex, SDocResStrings);
    if Module.InterfaceSection.Consts.Count > 0 then
      AddLink(ConstsSubindex, SDocConstants);
    if Module.InterfaceSection.Types.Count > 0 then
      AddLink(TypesSubindex, SDocTypes);
    if Module.InterfaceSection.Classes.Count > 0 then
      AddLink(ClassesSubindex, SDocClasses);
    if Module.InterfaceSection.Functions.Count > 0 then
      AddLink(ProcsSubindex, SDocProceduresAndFunctions);
    if Module.InterfaceSection.Variables.Count > 0 then
      AddLink(VarsSubindex, SDocVariables);
    AddLink(IndexSubIndex,SDocIdentifierIndex);  
    end
  else
    begin
    // Manually add link for package page
    AppendText(ParaEl, '[');
    if (IndexSubIndex = ASubpageIndex) then
      AppendText(ParaEl, SDocIdentifierIndex)
    else
      AppendText(
        CreateLink(ParaEl, ResolveLinkWithinPackage(Package, IndexSubIndex)),
        SDocIdentifierIndex);
    AppendText(ParaEl, ']');
    end;

  if Length(SearchPage) > 0 then
  begin
    AppendText(ParaEl, '[');
    AppendText(CreateLink(ParaEl, SearchPage), SDocSearch);
    AppendText(ParaEl, ']');
  end;
  ParaEl := CreateTD(TREl);
  ParaEl['align'] := 'right';
  TitleEl := CreateEl(ParaEl, 'span');
  TitleEl['class'] := 'bartitle';
  if Assigned(Module) then
    AppendText(TitleEl, Format(SDocUnitTitle, [Module.Name]));
  if Assigned(Package) then
  begin
    AppendText(TitleEl, ' (');
    AppendHyperlink(TitleEl, Package);
    AppendText(TitleEl, ')');
  end;
end;

procedure THTMLWriter.AppendSourceRef(AElement: TPasElement);
begin
  AppendText(CreatePara(BodyElement), Format(SDocSourcePosition,
    [ExtractFileName(AElement.SourceFilename), AElement.SourceLinenumber]));
end;

Procedure THTMLWriter.AppendSeeAlsoSection(AElement : TPasElement;DocNode : TDocNode);

var
  Node: TDOMNode;
  TableEl, El, TREl, TDEl, ParaEl, NewEl, DescrEl: TDOMElement;
  l,s: String;
  f: Text;
  IsFirstSeeAlso : Boolean;

begin
  if Not (Assigned(DocNode) and Assigned(DocNode.SeeAlso)) then
    Exit;
  IsFirstSeeAlso := True;
  Node:=DocNode.SeeAlso.FirstChild;
  While Assigned(Node) do
    begin
    if (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='link') then
      begin
       if IsFirstSeeAlso then
         begin
         IsFirstSeeAlso := False;
         AppendText(CreateH2(BodyElement), SDocSeeAlso);
         TableEl := CreateTable(BodyElement);
         end;
       El:=TDOMElement(Node);
       TREl:=CreateTR(TableEl);
       ParaEl:=CreatePara(CreateTD_vtop(TREl));
       l:=El['id'];
       s:= ResolveLinkID(l);
       if Length(s)=0 then
         begin
         WriteLn(Format(SErrUnknownLinkID, [l]));
         NewEl := CreateEl(ParaEl,'b')
         end
       else
         NewEl := CreateLink(ParaEl,s);
        if Not IsDescrNodeEmpty(El) then
          begin
          PushOutputNode(NewEl);
          Try
            ConvertBaseShortList(AElement, El, True)
          Finally
            PopOutputNode;
          end;
          end
        else
          AppendText(NewEl,El['id']);
       l:=El['id'];
       DescrEl := Engine.FindShortDescr(AElement.GetModule,L);
       if Assigned(DescrEl) then
         begin
         AppendNbSp(CreatePara(CreateTD(TREl)), 2);
         ParaEl := CreatePara(CreateTD(TREl));
         ParaEl['class'] := 'cmt';
         PushOutputNode(ParaEl);
         try
           ConvertShort(AElement, DescrEl);
         finally
           PopOutputNode;
         end;
         end;
       end; // Link node
     Node := Node.NextSibling;
     end; // While
end;

Procedure THTMLWriter.AppendExampleSection(AElement : TPasElement;DocNode : TDocNode);

var
  Node: TDOMNode;
//  TableEl, El, TREl, TDEl, ParaEl, NewEl, DescrEl: TDOMElement;
  fn,s: String;
  f: Text;

begin
  if not (Assigned(DocNode) and Assigned(DocNode.FirstExample)) then
    Exit;
  Node := DocNode.FirstExample;
  while Assigned(Node) do
    begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'example') then
      begin
      fn:=Engine.GetExampleFilename(TDOMElement(Node));
      If (fn<>'') then
        begin
        AppendText(CreateH2(BodyElement), SDocExample);
        try
          Assign(f, FN);
          Reset(f);
          try
            PushOutputNode(BodyElement);
            DescrBeginCode(False, TDOMElement(Node)['highlighter']);
            while not EOF(f) do
              begin
              ReadLn(f, s);
              DescrWriteCodeLine(s);
              end;
            DescrEndCode;
            PopOutputNode;
          finally
            Close(f);
          end;
        except
          on e: Exception do
            begin
            e.Message := '[example] ' + e.Message;
            raise;
            end;
        end;
        end;
      end;
    Node := Node.NextSibling;
    end;
end;

procedure THTMLWriter.AppendFooter;

Var
  S : String;
  F : TDomElement;
begin
  if FooterFile<>'' then
    ReadXMLFragment(BodyElement, FooterFile)
  else if IncludeDateInFooter then
    begin
    CreateEl(BodyElement, 'hr');
    F:=CreateEl(BodyElement,'span');
    F['class']:='footer';
    If (FDateFormat='') then
      S:=DateToStr(Date)
    else
      S:=FormatDateTime(FDateFormat,Date);  
    AppendText(F,Format(SDocDateGenerated,[S]));
    end;
end;

procedure THTMLWriter.FinishElementPage(AElement: TPasElement);

var
  DocNode: TDocNode;

begin
  DocNode := Engine.FindDocNode(AElement);
  If Assigned(DocNode) then
    begin
    // Description
    if Assigned(DocNode.Descr) then
      AppendDescrSection(AElement, BodyElement, DocNode.Descr, SDocDescription);

    // Append "Errors" section
    if Assigned(DocNode.ErrorsDoc) then
      AppendDescrSection(AElement, BodyElement, DocNode.ErrorsDoc, SDocErrors);

    // Append "See also" section
    AppendSeeAlsoSection(AElement,DocNode);

    // Append examples, if present
    AppendExampleSection(AElement,DocNode);
    end;
end;

Procedure THTMLWriter.CreateTopicPageBody(AElement : TTopicElement);

var
  DocNode: TDocNode;
  TableEl, TREl: TDOMElement;
  I : Integer;
  S : String;

begin
  AppendTopicMenuBar(AElement);
  DocNode:=AElement.TopicNode;
  if Assigned(DocNode) then  // should always be true, but we're being careful.
    begin
    AppendShortDescr(AElement,TitleElement, DocNode);
    AppendShortDescr(AElement,CreateH2(BodyElement), DocNode);
    if Assigned(DocNode.Descr) then
       AppendDescrSection(AElement, BodyElement, DocNode.Descr, '');
    AppendSeeAlsoSection(AElement,DocNode);
    CreateTopicLinks(DocNode,AElement);
    AppendExampleSection(AElement,DocNode);
    end;
end;

procedure THTMLWriter.CreatePageBody(AElement: TPasElement;
  ASubpageIndex: Integer);
var
  i: Integer;
  Element: TPasElement;
begin
  CurDirectory := Allocator.GetFilename(AElement, ASubpageIndex);
  i := Length(CurDirectory);
  while (i > 0) and not (CurDirectory[i] in AllowDirectorySeparators) do
    Dec(i);
  CurDirectory := Copy(CurDirectory, 1, i);
  BaseDirectory := Allocator.GetRelativePathToTop(AElement);

  if AElement.ClassType = TPasPackage then
    begin
    Module:=Nil;
    If (ASubPageIndex=0) then
      CreatePackagePageBody
    else
      CreatePackageIndex  
    end
  else
    begin
    Element := AElement;
    while (Element<>Nil) and (Element.ClassType<>TPasModule) do
      Element := Element.Parent;
    Module := TPasModule(Element);

    if AElement.ClassType = TPasModule then
      CreateModulePageBody(TPasModule(AElement), ASubpageIndex)
    else if AElement.Parent.InheritsFrom(TPasClassType) then
      CreateClassMemberPageBody(AElement)
    else if AElement.ClassType = TPasConst then
      CreateConstPageBody(TPasConst(AElement))
    else if AElement.InheritsFrom(TPasClassType) then
      CreateClassPageBody(TPasClassType(AElement), ASubpageIndex)
    else if AElement.InheritsFrom(TPasType) then
      CreateTypePageBody(TPasType(AElement))
    else if AElement.ClassType = TPasVariable then
      CreateVarPageBody(TPasVariable(AElement))
    else if AElement.InheritsFrom(TPasProcedureBase) then
      CreateProcPageBody(TPasProcedure(AElement))
    else if AElement.ClassType = TTopicELement then
      CreateTopicPageBody(TTopicElement(AElement))
  end;
end;

procedure THTMLWriter.CreateIndexPage(L : TStringList);

Var
  Lists  : Array['A'..'Z'] of TStringList;
  CL : TStringList;
  TableEl, TREl, EL: TDOMElement;
  E : TPasElement;
  I,Rows,J,Index : Integer;
  S : String;
  C : Char;

begin
  For C:='A' to 'Z' do
    Lists[C]:=Nil;
  L.Sort;
  Cl:=Nil;
  // Divide over alphabet
  For I:=0 to L.Count-1 do
    begin
    S:=L[i];
    E:=TPasElement(L.Objects[i]);
    If not (E is TPasUnresolvedTypeRef) then
      begin
      If (S<>'') then 
        begin
        C:=Upcase(S[1]);
        If Lists[C]=Nil then
          begin
          CL:=TStringList.Create;
          Lists[C]:=CL;
          end;
        end;
      if assigned(cl) then  
        CL.AddObject(S,E);
      end;  
    end;  
  Try  
  // Create a quick jump table to all available letters.    
  TableEl := CreateTable(BodyElement);
  TableEl['border']:='1';
  TableEl['width']:='50%';
  TREl := CreateTR(TableEl);
  for C:='A' to 'Z' do
    If (Lists[C]<>Nil) then
      begin
      El:=CreateTD_vtop(TREl);
      AppendText(CreateLink(El,'#SECTION'+C),C);
      If C<>'Z' then
       AppendNBsp(El,1);
      end;
  // Now emit all identifiers.    
  TableEl:=Nil;
  For C:='A' to 'Z' do
    begin
    CL:=Lists[C];
    If CL<>Nil then
      begin
      El:=CreateH2(BodyElement);
      AppendText(El,C);
      CreateAnchor(El,'SECTION'+C);
      TableEl := CreateTable(BodyElement);
      TableEl['Width']:='80%';
      // Determine number of rows needed
      Rows:=(CL.Count div IndexColCount);
      If ((CL.Count Mod IndexColCount)<>0) then
        Inc(Rows);
      // Fill rows  
      For I:=0 to Rows-1 do
        begin
        TREl := CreateTR(TableEl);
        For J:=0 to IndexColCount-1 do 
          begin
          El:=CreateTD_vtop(TREl);
          Index:=(J*Rows)+I;
          If (Index<CL.Count) then
            begin
            S:=CL[Index];
            E:=TPasElement(CL.Objects[Index]);
            AppendHyperlink(El,E);
            end;
          end;  
        end;  
      end; // have List
    end;  // For C:=
  Finally
    for C:='A' to 'Z' do
      FreeAndNil(Lists[C]);
  end;  
end;

procedure THTMLWriter.AddModuleIdentifiers(AModule : TPasModule; L : TStrings);

  Procedure AddElementsFromList(L : TStrings; List : TList);
  
  Var
    I : Integer;
    El : TPasElement;
    
  begin
    For I:=0 to List.Count-1 do
      begin
      El:=TPasElement(List[I]);
      L.AddObject(El.Name,El);
      If el is TPasEnumType then
        AddElementsFromList(L,TPasEnumType(el).Values);
      end;
  end;
  
begin
  AddElementsFromList(L,AModule.InterfaceSection.Consts);
  AddElementsFromList(L,AModule.InterfaceSection.Types);
  AddElementsFromList(L,AModule.InterfaceSection.Functions);
  AddElementsFromList(L,AModule.InterfaceSection.Classes);
  AddElementsFromList(L,AModule.InterfaceSection.Variables);
  AddElementsFromList(L,AModule.InterfaceSection.ResStrings);
end;


procedure THTMLWriter.CreatePackageIndex;


Var
  L : TStringList;
  I : Integer;
  M : TPasModule;
  E : TPasElement;
  S : String;    
  
begin
  L:=TStringList.Create;
  try
    L.Capacity:=PageInfos.Count; // Too much, but that doesn't hurt.
    For I:=0 to Package.Modules.Count-1 do
      begin
      M:=TPasModule(Package.Modules[i]);
      L.AddObject(M.Name,M);
      AddModuleIdentifiers(M,L);
      end;
    AppendMenuBar(IndexSubIndex);
    S:=Package.Name;
    If Length(S)>0 then
      Delete(S,1,1);
    AppendTitle(Format(SDocPackageIndex, [S]));
    CreateIndexPage(L);
  Finally
    L.Free;
  end;
end;

procedure THTMLWriter.CreatePackagePageBody;
var
  DocNode: TDocNode;
  TableEl, TREl: TDOMElement;
  i: Integer;
  ThisModule: TPasModule;
  L : TStringList;

begin
  AppendMenuBar(0);
  AppendTitle(Format(SDocPackageTitle, [Copy(Package.Name, 2, 256)]));
  AppendShortDescr(CreatePara(BodyElement), Package);

  AppendText(CreateH2(BodyElement), SDocUnits);
  TableEl := CreateTable(BodyElement);
  L:=TStringList.Create;
  Try
    L.Sorted:=True;
    // Sort modules.
    For I:=0 to Package.Modules.Count-1 do
      L.AddObject(TPasModule(Package.Modules[i]).Name,TPasModule(Package.Modules[i]));
    // Now create table.
    for i:=0 to L.Count - 1 do
      begin
      ThisModule := TPasModule(L.Objects[i]);
      TREl := CreateTR(TableEl);
      AppendHyperlink(CreateCode(CreatePara(CreateTD_vtop(TREl))), ThisModule);
      AppendShortDescrCell(TREl, ThisModule);
      end;
  Finally
    L.Free;
  end;

  DocNode := Engine.FindDocNode(Package);
  if Assigned(DocNode) then
    begin
    if Assigned(DocNode.Descr) then
       AppendDescrSection(nil, BodyElement, DocNode.Descr, SDocDescription);
    CreateTopicLinks(DocNode,Package);
    end;
end;

Procedure THTMLWriter.CreateTopicLinks(Node : TDocNode; PasElement : TPasElement);

var
  DocNode: TDocNode;
  TableEl, TREl: TDOMElement;
  First : Boolean;
  ThisTopic: TPasElement;

begin
  DocNode:=Node.FirstChild;
  First:=True;
  While Assigned(DocNode) do
    begin
    If DocNode.TopicNode then
      begin
      if first then
        begin
        First:=False;
        AppendText(CreateH2(BodyElement), SDocRelatedTopics);
        TableEl := CreateTable(BodyElement);
        end;
      TREl := CreateTR(TableEl);
      ThisTopic:=FindTopicElement(DocNode);
      if Assigned(ThisTopic) then
        AppendHyperlink(CreateCode(CreatePara(CreateTD_vtop(TREl))), ThisTopic);
      AppendShortDescrCell(TREl, ThisTopic);
      end;
    DocNode:=DocNode.NextSibling;
    end;
end;

procedure THTMLWriter.CreateModuleIndexPage(AModule: TPasModule);

Var
  L : TStringList;

begin
  L:=TStringList.Create;
  try
    AddModuleIdentifiers(AModule,L);
    AppendMenuBar(IndexSubIndex);
    AppendTitle(Format(SDocModuleIndex, [AModule.Name]));
    CreateIndexPage(L);
  Finally
    L.Free;
  end;  
end;

procedure THTMLWriter.CreateModulePageBody(AModule: TPasModule;
  ASubpageIndex: Integer);

  procedure CreateMainPage;
  var
    TableEl, TREl, TDEl, CodeEl: TDOMElement;
    i: Integer;
    UnitRef: TPasType;
    DocNode: TDocNode;
  begin
    AppendMenuBar(0);
    AppendTitle(Format(SDocUnitTitle, [AModule.Name]));
    AppendShortDescr(CreatePara(BodyElement), AModule);

    if AModule.InterfaceSection.UsesList.Count > 0 then
    begin
      TableEl := CreateTable(BodyElement);
      AppendKw(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), 'uses');
      for i := 0 to AModule.InterfaceSection.UsesList.Count - 1 do
      begin
        UnitRef := TPasType(AModule.InterfaceSection.UsesList[i]);
        DocNode := Engine.FindDocNode(UnitRef);
        if Assigned(DocNode) and DocNode.IsSkipped then
          continue;
        TREl := CreateTR(TableEl);
        TDEl := CreateTD_vtop(TREl);
        CodeEl := CreateCode(CreatePara(TDEl));
        AppendNbSp(CodeEl, 2);
        AppendHyperlink(CodeEl, UnitRef);
        if i < AModule.InterfaceSection.UsesList.Count - 1 then
          AppendSym(CodeEl, ',')
        else
          AppendSym(CodeEl, ';');
        AppendText(CodeEl, '  ');               // Space for descriptions
        AppendShortDescrCell(TREl, UnitRef);
      end;
    end;

    DocNode := Engine.FindDocNode(AModule);
    if Assigned(DocNode) then
      begin
      if Assigned(DocNode.Descr) then
        AppendDescrSection(AModule, BodyElement, DocNode.Descr, SDocOverview);
      CreateTopicLinks(DocNode,AModule);
      end;
  end;

  procedure CreateSimpleSubpage(const ATitle: DOMString; AList: TList);
  var
    TableEl, TREl, TDEl, CodeEl: TDOMElement;
    i, j: Integer;
    Decl: TPasElement;
    SortedList: TList;
    DocNode: TDocNode;
    S : String;

  begin
    AppendMenuBar(ASubpageIndex);
    S:=ATitle;
    AppendTitle(Format(SDocUnitTitle + ': %s', [AModule.Name, S]));
    SortedList := TList.Create;
    try
      for i := 0 to AList.Count - 1 do
      begin
        Decl := TPasElement(AList[i]);
        DocNode := Engine.FindDocNode(Decl);
        if (not Assigned(DocNode)) or (not DocNode.IsSkipped) then
        begin
          j := 0;
          while (j < SortedList.Count) and (CompareText(
            TPasElement(SortedList[j]).PathName, Decl.PathName) < 0) do
            Inc(j);
          SortedList.Insert(j, Decl);
        end;
      end;

      TableEl := CreateTable(BodyElement);
      for i := 0 to SortedList.Count - 1 do
      begin
        Decl := TPasElement(SortedList[i]);
        TREl := CreateTR(TableEl);
        CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
        AppendHyperlink(CodeEl, Decl);
        AppendShortDescrCell(TREl, Decl);
      end;
    finally
      SortedList.Free;
    end;
  end;

  procedure CreateResStringsPage;
  var
    ParaEl: TDOMElement;
    i, j: Integer;
    Decl: TPasResString;
    DocNode: TDocNode;
  begin
    AppendMenuBar(ResstrSubindex);
    AppendTitle(Format(SDocUnitTitle + ': %s', [AModule.Name, SDocResStrings]));
    for i := 0 to AModule.InterfaceSection.ResStrings.Count - 1 do
    begin
      Decl := TPasResString(AModule.InterfaceSection.ResStrings[i]);
      CreateEl(BodyElement, 'a')['name'] := LowerCase(Decl.Name);
      ParaEl := CreatePara(BodyElement);
      AppendText(CreateCode(ParaEl), Decl.Name);
      CreateEl(ParaEl, 'br');
      AppendText(ParaEl, Decl.Value);
    end;
  end;
  

begin
  case ASubpageIndex of
    0:
      CreateMainPage;
    ResstrSubindex:
      CreateResStringsPage;
    ConstsSubindex:
      CreateSimpleSubpage(SDocConstants, AModule.InterfaceSection.Consts);
    TypesSubindex:
      CreateSimpleSubpage(SDocTypes, AModule.InterfaceSection.Types);
    ClassesSubindex:
      CreateSimpleSubpage(SDocClasses, AModule.InterfaceSection.Classes);
    ProcsSubindex:
      CreateSimpleSubpage(SDocProceduresAndFunctions, AModule.InterfaceSection.Functions);
    VarsSubindex:
      CreateSimpleSubpage(SDocVariables, AModule.InterfaceSection.Variables);
    IndexSubIndex: 
      CreateModuleIndexPage(AModule);
  end;
end;

procedure THTMLWriter.CreateConstPageBody(AConst: TPasConst);
var
  TableEl, CodeEl: TDOMElement;
begin
  AppendMenuBar(-1);
  AppendTitle(AConst.Name);
  AppendShortDescr(CreatePara(BodyElement), AConst);
  AppendText(CreateH2(BodyElement), SDocDeclaration);
  AppendSourceRef(AConst);

  TableEl := CreateTable(BodyElement);
  CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));

  AppendKw(CodeEl, 'const');
  AppendText(CodeEl, ' ' + AConst.Name);
  if Assigned(AConst.VarType) then
  begin
    AppendSym(CodeEl, ': ');
    AppendType(CodeEl, TableEl, AConst.VarType, False);
  end;
  AppendPasSHFragment(CodeEl, ' = ' + AConst.Value + ';', 0);

  FinishElementPage(AConst);
end;

procedure THTMLWriter.CreateTypePageBody(AType: TPasType);
var
  TableEl, TREl, TDEl, CodeEl: TDOMElement;
  DocNode: TDocNode;
  i: Integer;
  s: String;
  EnumType: TPasEnumType;
  EnumValue: TPasEnumValue;
  Variable: TPasVariable;
begin
  AppendMenuBar(-1);
  AppendTitle(AType.Name);
  AppendShortDescr(CreatePara(BodyElement), AType);
  AppendText(CreateH2(BodyElement), SDocDeclaration);
  AppendSourceRef(AType);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  TDEl := CreateTD(TREl);
  CodeEl := CreateCode(CreatePara(TDEl));

  DocNode := Engine.FindDocNode(AType);
  AppendKw(CodeEl, 'type ');
  AppendText(CodeEl, AType.Name);
  AppendSym(CodeEl, ' = ');

  If Assigned(DocNode) and
     Assigned(DocNode.Node) and
     (Docnode.Node['opaque']='1') then
    AppendText(CodeEl,SDocOpaque)
  else
    begin
    // Alias
    if AType.ClassType = TPasAliasType then
      begin
      if Assigned(TPasAliasType(AType).DestType) then
        AppendHyperlink(CodeEl, TPasAliasType(AType).DestType)
      else
        AppendText(CreateWarning(CodeEl), '<Destination type is NIL>');
      AppendSym(CodeEl, ';');
    end else
    // Class of
    if AType.ClassType = TPasClassOfType then
    begin
      AppendKw(CodeEl, 'class of ');
      AppendHyperlink(CodeEl, TPasClassOfType(AType).DestType);
      AppendSym(CodeEl, ';');
    end else
    // Enumeration
    if AType.ClassType = TPasEnumType then
    begin
      AppendSym(CodeEl, '(');
      for i := 0 to TPasEnumType(AType).Values.Count - 1 do
      begin
        EnumValue := TPasEnumValue(TPasEnumType(AType).Values[i]);
        TREl := CreateTR(TableEl);
        CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
        AppendShortDescrCell(TREl, EnumValue);
        AppendNbSp(CodeEl, 2);
        s := EnumValue.Name;
        if EnumValue.IsValueUsed then
          s := s + ' = ' + IntToStr(EnumValue.Value);
        if i < TPasEnumType(AType).Values.Count - 1 then
          s := s + ',';
        AppendPasSHFragment(CodeEl, s, 0);
      end;
      AppendSym(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), ');');
    end else
    // Pointer type
    if AType.ClassType = TPasPointerType then
    begin
      AppendSym(CodeEl, '^');
      if Assigned(TPasPointerType(AType).DestType) then
        AppendHyperlink(CodeEl, TPasPointerType(AType).DestType)
      else
        AppendText(CreateWarning(CodeEl), '<Destination type is NIL>');
      AppendSym(CodeEl, ';');
    end else
    if AType.InheritsFrom(TPasProcedureType) then
    begin
      AppendSym(AppendType(CodeEl, TableEl, TPasType(AType), True), ';');
      AppendProcArgsSection(BodyElement, TPasProcedureType(AType));
    end else
    // Record
    if AType.ClassType = TPasRecordType then
    begin
      CodeEl := AppendRecordType(CodeEl, TableEl, TPasRecordType(AType), 0);
      AppendSym(CodeEl, ';');
    end else
    // Set
    if AType.ClassType = TPasSetType then
    begin
      AppendKw(CodeEl, 'set of ');
      if TPasSetType(AType).EnumType.ClassType = TPasEnumType then
      begin
        AppendSym(CodeEl, '(');
        EnumType := TPasEnumType(TPasSetType(AType).EnumType);
        for i := 0 to EnumType.Values.Count - 1 do
        begin
          EnumValue := TPasEnumValue(EnumType.Values[i]);
          TREl := CreateTR(TableEl);
          CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
          AppendShortDescrCell(TREl, EnumValue);
          AppendNbSp(CodeEl, 2);
          s := EnumValue.Name;
          if EnumValue.IsValueUsed then
            s := s + ' = ' + IntToStr(EnumValue.Value);
          if i < EnumType.Values.Count - 1 then
            s := s + ',';
          AppendPasSHFragment(CodeEl, s, 0);
        end;
        AppendSym(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), ');');
      end else
      begin
        AppendHyperlink(CodeEl, TPasSetType(AType).EnumType);
        AppendSym(CodeEl, ';');
      end;
    end else
    // Type alias
    if AType.ClassType = TPasTypeAliasType then
    begin
      AppendKw(CodeEl, 'type ');
      AppendHyperlink(CodeEl, TPasTypeAliasType(AType).DestType);
      AppendSym(CodeEl, ';');
    end else
    // Probably one of the simple types, which allowed in other places as wel...
      AppendSym(AppendType(CodeEl, TableEl, TPasType(AType), True), ';');
    end;
  FinishElementPage(AType);
end;


function PropertyFilter(AMember: TPasElement): Boolean;
begin
  Result := (AMember.ClassType = TPasProperty) and
    (Copy(AMember.Name, 1, 2) <> 'On');
end;

function MethodFilter(AMember: TPasElement): Boolean;
begin
  Result := AMember.InheritsFrom(TPasProcedureBase);
end;

function EventFilter(AMember: TPasElement): Boolean;
begin
  Result := (AMember.ClassType = TPasProperty) and
    (Copy(AMember.Name, 1, 2) = 'On');
end;

procedure THTMLWriter.CreateClassPageBody(AClass: TPasClassType;
  ASubpageIndex: Integer);
type
  TMemberFilter = function(AMember: TPasElement): Boolean;
var
  ParaEl: TDOMElement;

  procedure AppendMemberListLink(AListSubpageIndex: Integer;
    const AText: DOMString);
  var
    LinkEl: TDOMElement;
  begin
    AppendText(ParaEl, '[');
    LinkEl := CreateEl(ParaEl, 'a');
    LinkEl['href'] :=
      FixHtmlPath(ResolveLinkWithinPackage(AClass, AListSubpageIndex));
    LinkEl['onClick'] := 'window.open(''' + LinkEl['href'] + ''', ''list'', ' +
     '''dependent=yes,resizable=yes,scrollbars=yes,height=400,width=300''); return false;';
    AppendText(LinkEl, AText);
    AppendText(ParaEl, ' (');
    LinkEl := CreateEl(ParaEl, 'a');
    LinkEl['href'] :=
      FixHtmlPath(ResolveLinkWithinPackage(AClass, AListSubpageIndex + 1));
    LinkEl['onClick'] := 'window.open(''' + LinkEl['href'] + ''', ''list'', ' +
     '''dependent=yes,resizable=yes,scrollbars=yes,height=400,width=300''); return false;';
    AppendText(LinkEl, SDocByName);
    AppendText(ParaEl, ')');
    AppendText(ParaEl, '] ');
  end;

  procedure CreateMainPage;
  var
    TableEl, TREl, TDEl, CodeEl: TDOMElement;
    DocNode: TDocNode;
    Member: TPasElement;
    CurVisibility: TPasMemberVisibility;
    i: Integer;
    s: String;
    ThisClass: TPasClassType;
    HaveSeenTObject: Boolean;
  begin
    AppendMenuBar(-1);
    AppendTitle(AClass.Name);

    ParaEl := CreatePara(BodyElement);
    AppendMemberListLink(PropertiesByInheritanceSubindex, SDocProperties);
    AppendMemberListLink(MethodsByInheritanceSubindex, SDocMethods);
    AppendMemberListLink(EventsByInheritanceSubindex, SDocEvents);

    AppendShortDescr(CreatePara(BodyElement), AClass);
    AppendText(CreateH2(BodyElement), SDocDeclaration);
    AppendSourceRef(AClass);

    TableEl := CreateTable(BodyElement);

    TREl := CreateTR(TableEl);
    TDEl := CreateTD(TREl);
    CodeEl := CreateCode(CreatePara(TDEl));
    AppendKw(CodeEl, 'type');
    AppendText(CodeEl, ' ' + AClass.Name + ' ');
    AppendSym(CodeEl, '=');
    AppendText(CodeEl, ' ');
    AppendKw(CodeEl, ObjKindNames[AClass.ObjKind]);

    if Assigned(AClass.AncestorType) then
    begin
      AppendSym(CodeEl, '(');
      AppendHyperlink(CodeEl, AClass.AncestorType);
      AppendSym(CodeEl, ')');
    end;

    if AClass.Members.Count > 0 then
    begin
      CurVisibility := visDefault;
      for i := 0 to AClass.Members.Count - 1 do
      begin
        Member := TPasElement(AClass.Members[i]);
        if CurVisibility <> Member.Visibility then
        begin
          CurVisibility := Member.Visibility;
          if ((CurVisibility = visPrivate) and Engine.HidePrivate) or
            ((CurVisibility = visProtected) and Engine.HideProtected) then
            continue;
          case CurVisibility of
            visPrivate: s := 'private';
            visProtected: s := 'protected';
            visPublic: s := 'public';
            visPublished: s := 'published';
            visAutomated: s := 'automated';
          end;
          AppendKw(CreateCode(CreatePara(CreateTD(CreateTR(TableEl)))), s);
        end else
          if ((CurVisibility = visPrivate) and Engine.HidePrivate) or
            ((CurVisibility = visProtected) and Engine.HideProtected) then
            continue;

        TREl := CreateTR(TableEl);
        CodeEl := CreateCode(CreatePara(CreateTD_vtop(TREl)));
        AppendNbSp(CodeEl, 2);
        AppendShortDescrCell(TREl, Member);

        if Member.InheritsFrom(TPasProcedureBase) then
        begin
          AppendKw(CodeEl, TPasProcedureBase(Member).TypeName + ' ');
          AppendHyperlink(CodeEl, Member);
          if (Member.ClassType = TPasOverloadedProc) or
            (TPasProcedure(Member).ProcType.Args.Count > 0) then
            AppendSym(CodeEl, '();')
          else
            AppendSym(CodeEl, ';');
          if Member.ClassType <> TPasOverloadedProc then
            AppendProcExt(CodeEl, TPasProcedure(Member));
        end else
        if Member.ClassType = TPasVariable then
        begin
          AppendHyperlink(CodeEl, Member);
          AppendSym(CodeEl, ': ');
          AppendHyperlink(CodeEl, TPasVariable(Member).VarType);
          AppendSym(CodeEl, ';');
        end else
        if Member.ClassType = TPasProperty then
        begin
          AppendKw(CodeEl, 'property ');
          AppendHyperlink(CodeEl, Member);
          if Assigned(TPasProperty(Member).VarType) then
          begin
            AppendSym(CodeEl, ': ');
            AppendHyperlink(CodeEl, TPasProperty(Member).VarType);
          end;
          AppendSym(CodeEl, ';');
          if TPasProperty(Member).IsDefault then
          begin
            AppendKw(CodeEl, ' default');
            AppendSym(CodeEl, ';');
          end;
          SetLength(s, 0);
          if Length(TPasProperty(Member).ReadAccessorName) > 0 then
            s := s + 'r';
          if Length(TPasProperty(Member).WriteAccessorName) > 0 then
            s := s + 'w';
          if Length(TPasProperty(Member).StoredAccessorName) > 0 then
            s := s + 's';
          if Length(s) > 0 then
            AppendText(CodeEl, '  [' + s + ']');
        end else
          AppendText(CreateWarning(CodeEl), '<' + Member.ClassName + '>');
      end;

      CodeEl := CreateCode(CreatePara(CreateTD(CreateTR(TableEl))));
    end;

    AppendText(CodeEl, ' '); // !!!: Dirty trick, necessary for current XML writer
    AppendKw(CodeEl, 'end');
    AppendSym(CodeEl, ';');


    AppendText(CreateH2(BodyElement), SDocInheritance);
    TableEl := CreateTable(BodyElement);
    HaveSeenTObject := AClass.ObjKind <> okClass;
    ThisClass := AClass;
    while True do
    begin
      TREl := CreateTR(TableEl);
      TDEl := CreateTD_vtop(TREl);
      TDEl['align'] := 'center';
      CodeEl := CreateCode(CreatePara(TDEl));
      AppendHyperlink(CodeEl, ThisClass);
      AppendShortDescrCell(TREl, ThisClass);
      if HaveSeenTObject or (CompareText(ThisClass.Name, 'TObject') = 0) then
        HaveSeenTObject := True
      else
      begin
        TDEl := CreateTD(CreateTR(TableEl));
        TDEl['align'] := 'center';
        AppendText(TDEl, '|');
      end;

      if Assigned(ThisClass.AncestorType) then
      begin
        if ThisClass.AncestorType.InheritsFrom(TPasClassType) then
          ThisClass := TPasClassType(ThisClass.AncestorType)
        else
        begin
          TDEl := CreateTD(CreateTR(TableEl));
          TDEl['align'] := 'center';
          AppendText(CreateCode(CreatePara(TDEl)), ThisClass.AncestorType.Name);
          if CompareText(ThisClass.AncestorType.Name, 'TObject') = 0 then
            HaveSeenTObject := True
          else
          begin
            TDEl := CreateTD(CreateTR(TableEl));
            TDEl['align'] := 'center';
            AppendText(TDEl, '?');
          end;
          break;
        end
      end else
        break;
    end;

    if not HaveSeenTObject then
    begin
      TDEl := CreateTD(CreateTR(TableEl));
      TDEl['align'] := 'center';
      AppendText(CreateCode(CreatePara(TDEl)), 'TObject');
    end;

    FinishElementPage(AClass);
  end;

  procedure CreateInheritanceSubpage(AFilter: TMemberFilter);
  var
    ThisClass: TPasClassType;
    i: Integer;
    Member: TPasElement;
    TableEl, TREl, TDEl, ParaEl, LinkEl: TDOMElement;
  begin
    TableEl := CreateTable(BodyElement);
    ThisClass := AClass;
    while True do
    begin
      TREl := CreateTR(TableEl);
      TDEl := CreateTD(TREl);
      TDEl['colspan'] := '3';
      CreateTD(TREl);
      LinkEl := AppendHyperlink(CreateEl(CreateCode(CreatePara(TDEl)), 'b'), ThisClass);
      if Assigned(LinkEl) then
        LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
          '''; return false;';
      for i := 0 to ThisClass.Members.Count - 1 do
      begin
        Member := TPasElement(ThisClass.Members[i]);
        if ((Member.Visibility = visPrivate) and Engine.HidePrivate) or
          ((Member.Visibility = visProtected) and Engine.HideProtected) or
          not AFilter(Member) then
          continue;
        TREl := CreateTR(TableEl);
        ParaEl := CreatePara(CreateTD(TREl));
        case Member.Visibility of
          visPrivate:
            AppendText(ParaEl, 'pv');
          visProtected:
            AppendText(ParaEl, 'pt');
          visPublished:
            AppendText(ParaEl, 'pl');
        end;
        AppendNbSp(ParaEl, 1);

        ParaEl := CreateTD(TREl);
        if (Member.ClassType = TPasProperty) and
          (Length(TPasProperty(Member).WriteAccessorName) = 0) then
        begin
          AppendText(ParaEl, 'ro');
          AppendNbSp(ParaEl, 1);
        end;

        LinkEl := AppendHyperlink(CreatePara(CreateTD(TREl)), Member);
        if Assigned(LinkEl) then
          LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
            '''; return false;';
      end;
      if (not Assigned(ThisClass.AncestorType)) or
        (not (ThisClass.AncestorType.ClassType = TPasClassType)) then
        break;
      ThisClass := TPasClassType(ThisClass.AncestorType);
      AppendNbSp(CreatePara(CreateTD(CreateTR(TableEl))), 1);
    end;
  end;

  procedure CreateSortedSubpage(AFilter: TMemberFilter);
  var
    List: TList;
    ThisClass: TPasClassType;
    i, j: Integer;
    Member: TPasElement;
    TableEl, TREl, TDEl, ParaEl, LinkEl: TDOMElement;
  begin
    List := TList.Create;
    try
      ThisClass := AClass;
      while True do
      begin
        for i := 0 to ThisClass.Members.Count - 1 do
        begin
          Member := TPasElement(ThisClass.Members[i]);
          if (not (((Member.Visibility = visPrivate) and Engine.HidePrivate) or
            ((Member.Visibility = visProtected) and Engine.HideProtected))) and
            AFilter(Member) then
          begin
            j := 0;
            while (j < List.Count) and
              (CompareText(TPasElement(List[j]).Name, Member.Name) < 0) do
              Inc(j);
            List.Insert(j, Member);
          end;
        end;
        if (not Assigned(ThisClass.AncestorType)) or
          (not (ThisClass.AncestorType.ClassType = TPasClassType)) then
          break;
        ThisClass := TPasClassType(ThisClass.AncestorType);
      end;

      TableEl := CreateTable(BodyElement);
      for i := 0 to List.Count - 1 do
      begin
        Member := TPasElement(List[i]);
        TREl := CreateTR(TableEl);
        ParaEl := CreatePara(CreateTD(TREl));
        case Member.Visibility of
          visPrivate:
            AppendText(ParaEl, 'pv');
          visProtected:
            AppendText(ParaEl, 'pt');
          visPublished:
            AppendText(ParaEl, 'pl');
        end;
        AppendNbSp(ParaEl, 1);

        ParaEl := CreatePara(CreateTD(TREl));
        if (Member.ClassType = TPasProperty) and
          (Length(TPasProperty(Member).WriteAccessorName) = 0) then
        begin
          AppendText(ParaEl, 'ro');
          AppendNbSp(ParaEl, 1);
        end;

        TDEl := CreateTD(TREl);
        TDEl['nowrap'] := 'nowrap';
        ParaEl := CreatePara(TDEl);
        LinkEl := AppendHyperlink(ParaEl, Member);
        if Assigned(LinkEl) then
          LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
            '''; return false;';
        AppendText(ParaEl, ' (');
        LinkEl := AppendHyperlink(ParaEl, Member.Parent);
        if Assigned(LinkEl) then
          LinkEl['onClick'] := 'opener.location.href = ''' + LinkEl['href'] +
            '''; return false;';
        AppendText(ParaEl, ')');
      end;
    finally
      List.Free;
    end;
  end;

begin
  case ASubpageIndex of
    0:
      CreateMainPage;
    PropertiesByInheritanceSubindex:
      CreateInheritanceSubpage(@PropertyFilter);
    PropertiesByNameSubindex:
      CreateSortedSubpage(@PropertyFilter);
    MethodsByInheritanceSubindex:
      CreateInheritanceSubpage(@MethodFilter);
    MethodsByNameSubindex:
      CreateSortedSubpage(@MethodFilter);
    EventsByInheritanceSubindex:
      CreateInheritanceSubpage(@EventFilter);
    EventsByNameSubindex:
      CreateSortedSubpage(@EventFilter);
  end;
end;

procedure THTMLWriter.CreateClassMemberPageBody(AElement: TPasElement);
var
  TableEl, TREl, CodeEl: TDOMElement;

  procedure CreateVarPage(Element: TPasVariable);
  begin
    AppendHyperlink(CodeEl, Element.Parent);
    AppendSym(CodeEl, '.');
    AppendText(CodeEl, Element.Name);
    if Assigned(Element.VarType) then
    begin
      AppendSym(CodeEl, ': ');
      AppendSym(AppendType(CodeEl, TableEl, Element.VarType, False), ';');
    end;
  end;

  procedure CreatePropertyPage(Element: TPasProperty);
  var
    NeedBreak: Boolean;
  begin
    AppendKw(CodeEl, 'property ');
    AppendHyperlink(CodeEl, Element.Parent);
    AppendSym(CodeEl, '.');
    AppendText(CodeEl, Element.Name);
    if Assigned(Element.VarType) then
    begin
      AppendSym(CodeEl, ': ');
      AppendType(CodeEl, TableEl, Element.VarType, False);
    end;

    NeedBreak := False;
    if Length(TPasProperty(Element).IndexValue) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'index ');
      AppendPasSHFragment(CodeEl, TPasProperty(Element).IndexValue, 0);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).ReadAccessorName) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'read ');
      AppendText(CodeEl, TPasProperty(Element).ReadAccessorName);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).WriteAccessorName) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'write ');
      AppendText(CodeEl, TPasProperty(Element).WriteAccessorName);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).StoredAccessorName) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'stored ');
      AppendText(CodeEl, TPasProperty(Element).StoredAccessorName);
      NeedBreak := True;
    end;
    if Length(TPasProperty(Element).DefaultValue) <> 0 then
    begin
      CreateEl(CodeEl, 'br');
      AppendNbsp(CodeEl, 2);
      AppendKw(CodeEl, 'default ');
      AppendPasSHFragment(CodeEl, TPasProperty(Element).DefaultValue, 0);
      NeedBreak := True;
    end;

    AppendSym(CodeEl, ';');

    if TPasProperty(Element).IsDefault or TPasProperty(Element).IsNodefault then
    begin
      if NeedBreak then
      begin
        CreateEl(CodeEl, 'br');
        AppendNbsp(CodeEl, 2);
      end;
      if TPasProperty(Element).IsDefault then
        AppendKw(CodeEl, 'default')
      else
        AppendKw(CodeEl, 'nodefault');
      AppendSym(CodeEl, ';');
    end;
  end;

var
  s: String;
  DocNode: TDocNode;
begin
  AppendMenuBar(-1);
  AppendTitle(AElement.FullName);
  AppendShortDescr(CreatePara(BodyElement), AElement);
  AppendText(CreateH2(BodyElement), SDocDeclaration);
  AppendSourceRef(AElement);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  CodeEl := CreateCode(CreatePara(CreateTD(TREl)));
  AppendText(CodeEl, ' ');      // !!!: Workaround for current HTML writer

  case AElement.Visibility of
    visPrivate: s := 'private';
    visProtected: s := 'protected';
    visPublic: s := 'public';
    visPublished: s := 'published';
    visAutomated: s := 'automated';
    else s := '';
  end;
  if Length(s) > 0 then
    AppendKw(CodeEl, s);
  AppendText(CodeEl, ' ');

  if AElement.ClassType = TPasVariable then
    CreateVarPage(TPasVariable(AElement))
  else if AElement.InheritsFrom(TPasProcedureBase) then
    AppendProcDecl(CodeEl, TableEl, TPasProcedureBase(AElement))
  else if AElement.ClassType = TPasProperty then
    CreatePropertyPage(TPasProperty(AElement))
  else
    AppendText(CreateWarning(BodyElement), '<' + AElement.ClassName + '>');

  FinishElementPage(AElement);
end;

procedure THTMLWriter.CreateVarPageBody(AVar: TPasVariable);
var
  TableEl, TREl, TDEl, CodeEl, El: TDOMElement;
  DocNode: TDocNode;
begin
  AppendMenuBar(-1);
  AppendTitle(AVar.FullName);
  AppendShortDescr(CreatePara(BodyElement), AVar);
  AppendText(CreateH2(BodyElement), SDocDeclaration);
  AppendSourceRef(AVar);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  TDEl := CreateTD(TREl);
  CodeEl := CreateCode(CreatePara(TDEl));

  AppendKw(CodeEl, 'var');
  AppendText(CodeEl, ' ' + AVar.Name);
  if Assigned(AVar.VarType) then
  begin
    AppendSym(CodeEl, ': ');
    El := AppendType(CodeEl, TableEl, AVar.VarType, False);
  end else
    El := CodeEl;
  if Length(AVar.Value) > 0 then
    AppendPasSHFragment(El, ' = ' + AVar.Value + ';', 0)
  else
    AppendSym(El, ';');

  FinishElementPage(AVar);
end;

procedure THTMLWriter.CreateProcPageBody(AProc: TPasProcedureBase);
var
  TableEl, TREl, TDEl, CodeEl: TDOMElement;
begin
  AppendMenuBar(-1);
  AppendTitle(AProc.Name);
  AppendShortDescr(CreatePara(BodyElement), AProc);
  AppendText(CreateH2(BodyElement), SDocDeclaration);
  AppendSourceRef(AProc);

  TableEl := CreateTable(BodyElement);
  TREl := CreateTR(TableEl);
  TDEl := CreateTD(TREl);
  CodeEl := CreateCode(CreatePara(TDEl));

  AppendProcDecl(CodeEl, TableEl, AProc);

  FinishElementPage(AProc);
end;

Function THTMLWriter.InterPretOption(Const Cmd,Arg : String) : boolean;

begin
  Result:=True;
  if Cmd = '--html-search' then
    SearchPage := Arg
  else if Cmd = '--footer' then
    FooterFile := Arg
  else if Cmd = '--charset' then
    CharSet := Arg
  else if Cmd = '--index-colcount' then
    IndexColCount := StrToIntDef(Arg,IndexColCount)
  else if Cmd = '--image-url' then
    FBaseImageURL  := Arg
  else if Cmd = '--footer-date' then
    begin
    FIDF:=True;
    FDateFormat:=Arg;
    end
  else
    Result:=False;
end;

procedure THTMLWriter.WriteDoc;
begin
   WriteLn(Format(SWritingPages, [PageCount]));
   WriteHTMLPages;
end;

class procedure THTMLWriter.Usage(List: TStrings);
begin
  List.add('--footer');
  List.Add(SHTMLUsageFooter);
  List.Add('--footer-date[=Fmt]');
  List.Add(SHTMLUsageFooterDate);
  List.Add('--charset=set');
  List.Add(SHTMLUsageCharset);
  List.Add('--html-search=pagename');
  List.Add(SHTMLHtmlSearch);
  List.Add('--index-colcount=N');
  List.Add(SHTMLIndexColcount);
  List.Add('--image-url=url');
  List.Add(SHTMLImageUrl);
end;

// private methods

function THTMLWriter.GetPageCount: Integer;
begin
  Result := PageInfos.Count;
end;

procedure THTMLWriter.SetOnTest(const AValue: TNotifyEvent);
begin
  if FOnTest=AValue then exit;
    FOnTest:=AValue;
end;

procedure THTMLWriter.CreateAllocator;
begin
  FAllocator:=TLongNameFileAllocator.Create('.html');
end;


procedure THTMWriter.CreateAllocator;
begin
  FAllocator:=TShortNameFileAllocator.Create('.htm');
end;

initialization
  // Do not localize.
  RegisterWriter(THTMLWriter,'html','HTML output using fpdoc.css stylesheet.');
  RegisterWriter(THTMWriter,'htm','HTM (8.3 filenames) output using fpdoc.css stylesheet.');
  RegisterWriter(TCHMHTMLWriter,'chm','Compressed HTML file output using fpdoc.css stylesheet.');

finalization
  UnRegisterWriter('html');
  UnRegisterWriter('htm');
  UnRegisterWriter('chm');
end.
