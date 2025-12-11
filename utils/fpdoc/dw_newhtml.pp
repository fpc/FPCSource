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

unit dw_newhtml;
{$WARN 5024 off : Parameter "$1" not used}
interface

uses Classes, DOM, DOM_HTML, dGlobals, PasTree, dWriter, dw_basehtml, PasWrite;


type
  { TNewHTMLWriter }

  TNewHTMLWriter = class(TBaseHTMLWriter)
  private
    FCreateSideMenu: Boolean;
    FHeadElement: TDomElement;
    FOnTest: TNotifyEvent;
    FCSSFile: String;
    FCharSet : String;
    FHeaderHTML,
    FNavigatorHTML,
    FFooterHTML: TStringStream;
    FTitleElement: TDOMElement;
    FIncludeDateInFooter : Boolean;
    FUseMenuBrackets: Boolean;
    FDateFormat: String;
    FIndexColCount : Integer;
    FSearchPage : String;
    function GetVarDef(aElement: TPasVariable; aPrefixParent: Boolean): string;
    procedure SetModuleInfo(aElement: TPasElement; ASubpageIndex: integer);
    procedure SetOnTest(const AValue: TNotifyEvent);
  protected
    function CreateAllocator : TFileAllocator; override;
    procedure WriteDocPage(const aFileName: String; aElement: TPasElement; aSubPageIndex: Integer);  override;
    // General HTML creation
    function CreateH1(Parent: TDOMNode): THTMLElement; override;
    function CreateH2(Parent: TDOMNode): THTMLElement; override;
    function CreateH3(Parent: TDOMNode): THTMLElement; override;

    function CreateListColumn1(aParent: THTMLElement): THTMLElement;
    function CreateListColumn2(aParent: THTMLElement): THTMLElement;
    function CreateListColumns(aParent: THTMLElement): THTMLElement;
    function CreateSection(aParent : THTMLElement) : THTMLElement; virtual;
    procedure DescrWriteFileEl(const AText: DOMString); override;
    procedure DescrWriteVarEl(const AText: DOMString); override;

    function  AppendPasSHFragment(Parent: TDOMNode; const AText: String; AShFlags: Byte): Byte; override;
    function  AppendPasSHFragment(Parent: TDOMNode; const AText: String; AShFlags: Byte; aLinkIdentifierMap : TLinkIdentifierMap): Byte; override;

    procedure CreateCSSFile; virtual;

    procedure AppendTitle(aParent: TDomElement; const AText: AnsiString; Hints : TPasMemberHints = []); virtual;
    procedure AppendTitle(aParent: TDomElement; const AText: DOMString; Hints: TPasMemberHints=[]); virtual;
    function AppendType(CodeEl: TDOMElement; Element: TPasType): TDOMElement; virtual;
    function AppendProcType(CodeEl : TDOMElement;  Element: TPasProcedureType; Indent: Integer): TDOMElement; virtual;
    procedure AppendProcExt(CodeEl: TDOMElement; Element: TPasProcedure); virtual;
    procedure AppendProcDecl(CodeEl: TDOMElement; Element: TPasProcedureBase); virtual;
    procedure AppendProcArgsSection(Parent: THTMLElement; Element: TPasProcedureType; SkipResult : Boolean = False); virtual;
    procedure AppendShortDescrCell(Parent: TDOMNode; Element: TPasElement); override;
    procedure AppendDescrSection(AContext: TPasElement; Parent: TDOMNode; DescrNode: TDOMElement; const ATitle: DOMString); override;
    Procedure AppendSeeAlsoSection(AElement: TPasElement; aParent : TDOMElement; DocNode: TDocNode); override;

    // Structural elements
    procedure AppendMenuBar(ASubpageIndex: Integer);virtual;
    procedure AppendTopicMenuBar(Topic : TTopicElement);virtual;
    procedure FinishElementPage(AElement: TPasElement; aDescription: Boolean=True); virtual;
    procedure AppendFooter;virtual;
    procedure AppendSideMenuScript(aHead : THTMLElement);
    procedure AppendSideMenu(aMenu: THTMLElement);

    // Class
    procedure CreateClassMainPage(aClass: TPasClassType);virtual;
    procedure CreateClassInheritedSubpage(AClass: TPasClassType; aType: TClassMemberType);
    procedure CreateClassSortedSubpage(AClass: TPasClassType; aType : TClassMemberType);virtual;
    procedure CreateClassMemberList(aParent: THTMLElement; AClass: TPasClassType; DeclaredOnly: Boolean; AFilter: TMemberFilter);
    procedure AppendMemberListSection(aParent: THTMLELement; aClass: TPasClassType; aMemberType : TClassMemberType; aDeclaredOnly: Boolean);
    procedure AppendInheritanceTree(aParent: THTMLELement; aClass: TPasClassType);

    // Package

    procedure CreatePageBody(AElement: TPasElement; ASubpageIndex: Integer); virtual;
    procedure CreatePackagePageBody;virtual;
    procedure CreatePackageIndex;
    procedure CreatePackageClassHierarchy;
    procedure CreateClassHierarchyPage(AddUnit : Boolean);
    procedure CreateIndexPage(aParent : THTMLElement; L : TStringList); virtual;
    // Topic
    Procedure CreateTopicPageBody(AElement : TTopicElement);
    // Module
    procedure CreateModuleMainPage(aModule: TPasModule);virtual;
    procedure CreateModuleSimpleSubpage(aModule: TPasModule; ASubpageIndex: Integer; const ATitle: DOMString; AList: TFPList);virtual;
    procedure CreateModuleResStringsPage(aModule: TPasModule);virtual;
    procedure CreateModulePageBody(AModule: TPasModule; ASubpageIndex: Integer);
    procedure CreateModuleIndexPage(AModule: TPasModule); virtual;
    // Identifiers
    procedure CreateConstPageBody(AConst: TPasConst);
    procedure CreateTypePageBody(AType: TPasType);
    procedure CreateClassPageBody(AClass: TPasClassType; ASubpageIndex: Integer);
    procedure CreateClassMemberPageBody(AElement: TPasElement);
    procedure CreateVarPageBody(AVar: TPasVariable);
    procedure CreateProcPageBody(AProc: TPasProcedureBase);
    Procedure CreateTopicLinks(aParent : THTMLElement; Node : TDocNode; PasElement : TPasElement);
    // Type declarations
    function GetElementCode(aElement: TPasElement; aSparse: boolean; aFlags: TElementFlags = []): String;
    function AppendHighlightedCode(aParent: TDOMNode; aCode: String; const aLanguage: String=''; aMap: TLinkIdentifierMap = Nil): THTMLElement;
    function CreateCodeLines(aLines: array of string): string;
    procedure AppendTypeDecl(AType: TPasType);
    procedure AppendAliasTypeDecl(aType: TPasAliasType);
    procedure AppendClassOfTypeDecl(aType: TPasClassOfType);
    function  AppendCodeBlock(aParent: TDOMNode; const aLanguage: String=''): THTMLElement;
    procedure AppendEnumTypeDecl(aType: TPasEnumType);
    procedure AppendPointerTypeDecl(aType: TPasPointerType);
    procedure AppendProcedureTypeDecl(aType: TPasProcedureType);
    procedure AppendRecordTypeDecl(aType: TPasRecordType);
    procedure AppendSetTypeDecl(aType: TPasSetType);
    procedure AppendTypeAliasTypeDecl(aType: TPasTypeAliasType);

    //  Main documentation process
    Procedure DoWriteDocumentation; override;

    Property HeaderHTML : TStringStream Read FHeaderHTML;
    Property NavigatorHTML : TStringStream read FNavigatorHTML;
    Property FooterHTML : TStringStream read FFooterHTML;
    Property CSSFile : String Read FCSSFile;
    Property HeadElement : TDomElement Read FHeadElement;
    Property TitleElement: TDOMElement Read FTitleElement;
  public
    // Creating all module hierarchy classes happens here !
    constructor Create(APackage: TPasPackage; AEngine: TFPDocEngine); override;
    // Overrides
    Class Function FileNameExtension : String; override;
    class procedure Usage(List: TStrings); override;
    Class procedure SplitImport(var AFilename, ALinkPrefix: String); override;

    Function InterPretOption(Const Cmd,Arg : String) : boolean; override;

    // Single-page generation
    function CreateHTMLPage(AElement: TPasElement; ASubpageIndex: Integer): TXMLDocument; virtual;

    Property SearchPage: String Read FSearchPage Write FSearchPage;
    Property IncludeDateInFooter : Boolean Read FIncludeDateInFooter Write FIncludeDateInFooter;
    Property DateFormat : String Read FDateFormat Write FDateFormat;
    property OnTest: TNotifyEvent read FOnTest write SetOnTest;
    Property CharSet : String Read FCharSet Write FCharSet;
    Property IndexColCount : Integer Read FIndexColCount write FIndexColCount;
    Property CreateSideMenu : Boolean Read FCreateSideMenu Write FCreateSideMenu;
  end;


implementation

uses fpdocstrs, SysUtils, HTMWrite, syntax.highlighter, syntax.pascal, fpdocclasstree;

{$i newcss.inc}

constructor TNewHTMLWriter.Create(APackage: TPasPackage; AEngine: TFPDocEngine);

begin
  inherited Create(APackage, AEngine);
  // should default to true since this is the old behavior
  CreateSideMenu:=True;
  IndexColCount:=3;
  Charset:='iso-8859-1';
  FCSSFile:='fpdocs.css';
end;

procedure TNewHTMLWriter.AppendSideMenuScript(aHead: THTMLElement);

Const
  SFunc =
    '  document.addEventListener("DOMContentLoaded", () => {'+sLinebreak+
    '       const toggleButton = document.getElementById("menu-toggle");'+sLinebreak+
    '       const sideMenu = document.getElementById("side-menu");'+sLinebreak+
    '       const mainContent = document.getElementById("main-content");'+sLinebreak+
    '       if (toggleButton && sideMenu && mainContent) {'+sLinebreak+
    '           toggleButton.addEventListener("click", () => {'+sLinebreak+
    '               sideMenu.classList.toggle("is-expanded");'+sLinebreak+
    '               mainContent.classList.toggle("is-shifted");'+sLinebreak+
    '           });'+sLinebreak+
    '       }'+sLinebreak+
    '   });';

Var
  SE : THTMLElement;

begin
  SE:=Doc.CreateElement('script');
  aHead.AppendChild(SE);
  AppendText(SE,SFunc);
end;


procedure TNewHTMLWriter.AppendSideMenu(aMenu: THTMLElement);

  function AddLink(aParent : THTMLElement; ALinkSubpageIndex: Integer; const AName: String) : THTMLElement;

  begin
    Result:=CreateLink(aParent, ResolveLinkWithinPackage(Module, ALinkSubpageIndex));
    AppendText(Result,aName);
  end;

  function AddPackageLink(aParent: THTMLElement; ALinkSubpageIndex: Integer; const AName: String) : THTMLElement;
  var
    lURL : String;
  begin
    lURL:=ResolveLinkWithinPackage(Package, ALinkSubpageIndex);
    Result:=CreateLink(aParent,lURL);
    AppendText(Result,aName);
  end;


var
  lPara,lList,lItem : THTMLElement;
  lModules : TStringList;
  lModule : TPasModule;
  I : Integer;

begin
  lPara:=CreateEl(aMenu,'p','menu-label');
  AppendText(lPara,SDocPackageLinkTitle);
  lList:=CreateEl(aMenu,'ul','menu-list');
  lItem:=CreateEl(lList,'li');
  if Assigned(Module) then
  AddPackageLink(lItem,0, SDocReference);
  //El:=AppendHyperlink(lItem, Package) as THTMLELement;
  lItem:=CreateEl(lList,'li');
  AddPackageLink(lItem,IndexSubIndex, SDocIdentifierIndex);
  lItem:=CreateEl(lList,'li');
  AddPackageLink(lItem,ClassHierarchySubIndex, SDocPackageClassHierarchy);
  lPara:=CreateEl(aMenu,'p','menu-label');
  AppendText(lPara,SDocUnits);
  lList:=CreateEl(aMenu,'ul','menu-list');
  lModules:=TStringList.Create;
  try
    For I:=0 to Package.Modules.Count-1 do
      begin
      lModule:=TPasModule(Package.Modules[I]);
      lModules.AddObject(lModule.Name,lModule);
      end;
    lModules.Sort;
    For I:=0 to lModules.Count-1 do
      begin
      lModule:=TPasModule(lModules.Objects[I]);
      lItem:=CreateEl(lList,'li');
      AppendHyperlink(lItem, lModule);
      end;
  finally
    LModules.Free;
  end;

end;

procedure TNewHTMLWriter.SetModuleInfo(aElement : TPasElement; ASubpageIndex : integer);

var
  i : integer;
  Element : TPasElement;
begin
  CurDirectory := Allocator.GetFilename(AElement, ASubpageIndex);
  i := Length(CurDirectory);
  while (i > 0) and not (CurDirectory[i] in AllowDirectorySeparators) do
    Dec(i);
  CurDirectory := Copy(CurDirectory, 1, i);
  BaseDirectory := Allocator.GetRelativePathToTop(AElement);
  if aElement is TPasPackage then
    Module:=Nil
  else
    begin
    Element := AElement;
    while (Element<>Nil) and (not (Element.ClassType.inheritsfrom(TPasModule))) do
      Element := Element.Parent;
    Module := TPasModule(Element);
    end;
end;

function TNewHTMLWriter.CreateHTMLPage(AElement: TPasElement;
  ASubpageIndex: Integer): TXMLDocument;
var
  HTMLEl: THTMLHtmlElement;
  HeadEl: THTMLHeadElement;
  LMain,lMenu,LContent,BodyElement : THTMLElement;
  El: TDOMElement;

begin

  Result := THTMLDocument.Create;
  SetHTMLDocument(THTMLDocument(Result));
  Doc.AppendChild(Doc.Impl.CreateDocumentType('html','',''));

  HTMLEl := Doc.CreateHtmlElement;
  Doc.AppendChild(HTMLEl);

  HeadEl := Doc.CreateHeadElement;
  FHeadElement:=HeadEl;
  HTMLEl.AppendChild(HeadEl);
  El := Doc.CreateElement('meta');
  HeadEl.AppendChild(El);
  El['http-equiv'] := 'Content-Type';
  El['content'] := 'text/html; charset=utf-8';

  El := Doc.CreateElement('meta');
  HeadEl.AppendChild(El);
  El['name'] := 'viewport';
  El['content'] := 'width=device-width, initial-scale=1';


  FTitleElement := Doc.CreateElement('title');
  HeadEl.AppendChild(TitleElement);

  BodyElement := Doc.CreateElement('body');
  BodyElement['class']:='has-navbar-fixed-top';
  ContentElement:=BodyElement;
  HTMLEl.AppendChild(BodyElement);
  SetModuleInfo(aElement,ASubpageIndex);
  AppendMenuBar(ASubpageIndex);
  if CreateSideMenu then
    begin
    AppendSideMenuScript(HeadEl);
    LMain:=CreateEl(ContentElement,'div');
    LMain['id']:='main-layout';
    lMenu:=CreateEl(lMain,'aside','is-expanded');
    LMenu['id']:='side-menu';
    AppendSideMenu(lMenu);
    LContent:=CreateEl(lMain,'div','is-shifted');
    LContent['id']:='main-content';
    ContentElement:=lContent;
    end;
  CreatePageBody(AElement, ASubpageIndex);

  AppendFooter;

  El := Doc.CreateElement('link');
  HeadEl.AppendChild(El);
  El['rel'] := 'stylesheet';
  El['type'] := 'text/css';
  El['href'] := UTF8Decode(FixHtmlPath(UTF8Encode(Allocator.GetCSSFilename(AElement,'bulma'))));

  El := Doc.CreateElement('link');
  HeadEl.AppendChild(El);
  El['rel'] := 'stylesheet';
  El['type'] := 'text/css';
  El['href'] := UTF8Decode(FixHtmlPath(UTF8Encode(Allocator.GetCSSFilename(AElement,'fpdocs'))));
end;


procedure TNewHTMLWriter.WriteDocPage(const aFileName: String; aElement: TPasElement; aSubPageIndex: Integer);

Var
  PageDoc: TXMLDocument;
  FN : String;
begin
  PageDoc := CreateHTMLPage(aElement, aSubpageIndex);
  try
    FN:=GetFileBaseDir(Engine.Output)+aFilename;
    //writeln('Element: ',Element.PathName, ' FileName: ', FN);
    WriteHTMLFile(PageDoc, FN);
  except
    on E: Exception do
      DoLog(SErrCouldNotCreateFile, [aFileName, e.Message]);
  end;
  PageDoc.Free;
end;


function TNewHTMLWriter.CreateH1(Parent: TDOMNode): THTMLElement;
begin
  Result:=inherited CreateH1(Parent);
  Result['class']:='subtitle is-2'
end;

function TNewHTMLWriter.CreateH2(Parent: TDOMNode): THTMLElement;
begin
  Result:=inherited CreateH2(Parent);
  Result['class']:='subtitle is-4'
end;

function TNewHTMLWriter.CreateH3(Parent: TDOMNode): THTMLElement;
begin
  Result:=inherited CreateH3(Parent);
  Result['class']:='subtitle is-6'
end;

procedure TNewHTMLWriter.DoWriteDocumentation;


begin
  Inherited;
  CreateCSSFile;
end;

procedure TNewHTMLWriter.CreateCSSFile;

Var
  TempStream: TMemoryStream;
  Data : PByte;

begin
  TempStream := TMemoryStream.Create;
  try
    if (FCSSFile<>'') then
      begin
      if not FileExists(FCSSFile) then
        begin
        DoLog('Can''t find CSS file "%s"',[FCSSFILE]);
        halt(1);
        end;
      TempStream.LoadFromFile(FCSSFile);
      end
    else
      begin
      DoLog('Using built-in CSS file',[]);
      Data:=@DefaultNewCSS;
      TempStream.WriteBuffer(Data^,SizeOf(DefaultNewCSS));
      end;
   TempStream.Position := 0;
   TempStream.SaveToFile(Engine.output+'fpdocs.css');
  finally
    TempStream.Free;
  end;
end;


{ Returns the new CodeEl, which will be the old CodeEl in most cases }
function TNewHTMLWriter.AppendType(CodeEl: TDOMElement;  Element: TPasType): TDOMElement;

Var
  S : String;

begin
  Result := CodeEl;
  S:=GetElementCode(Element, False);
//  Writeln('Default code for "',Element.ClassName,'"(',Element.FullName,'):',S);
  AppendHighlightedCode(ContentElement,S);
end;

function TNewHTMLWriter.AppendProcType(CodeEl: TDOMElement;
  Element: TPasProcedureType; Indent: Integer): TDOMElement;

var
  i: Integer;
  Arg: TPasArgument;
  S : String;

begin
  if Element.Args.Count > 0 then
  begin
    AppendSym(CodeEl, '(');

    for i := 0 to Element.Args.Count - 1 do
    begin
      Arg := TPasArgument(Element.Args[i]);
      S:=AccessNames[Arg.Access];
      if (S<>'') then
        AppendKw(CodeEl,S);
      AppendText(CodeEl, Arg.Name);
      if Assigned(Arg.ArgType) then
      begin
        AppendSym(CodeEl, ': ');
        CodeEl := AppendType(CodeEl, Arg.ArgType);
      end;
      if Length(Arg.Value) > 0 then
        AppendPasSHFragment(CodeEl, ' = ' + Arg.Value, 0);
      if i < Element.Args.Count - 1 then
        AppendSym(CodeEl, ';');
    end;

    if Element.InheritsFrom(TPasFunctionType) or Element.IsOfObject then
    begin
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
        AppendSym(CodeEl, ')');
      end;
  end
  else
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

procedure TNewHTMLWriter.AppendProcExt(CodeEl: TDOMElement;
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
  - Page for a standalone procedure or function. }

procedure TNewHTMLWriter.AppendProcDecl(CodeEl: TDOMElement;  Element: TPasProcedureBase);

  procedure WriteVariant(AProc: TPasProcedure; SkipResult : Boolean);
  begin
    AppendHighlightedCode(CodeEl,GetElementCode(aProc,False,[efParent]));
  end;

var
  i,fc: Integer;
  P : TPasProcedure;
begin
  fc:=0;
  if Element.ClassType = TPasOverloadedProc then
    for i := 0 to TPasOverloadedProc(Element).Overloads.Count - 1 do
    begin
      P:=TPasProcedure(TPasOverloadedProc(Element).Overloads[i]);
      if (P.ProcType is TPasFunctionType) then
        Inc(fc);
      WriteVariant(P,fc>1);
    end
  else
    WriteVariant(TPasProcedure(Element),False);
end;

procedure TNewHTMLWriter.AppendProcArgsSection(Parent: THTMLElement;
  Element: TPasProcedureType; SkipResult : Boolean = False);
var
  HasFullDescr, HaveArgDescr: Boolean;
  ResultEl: TPasResultElement;
  lColumns, lColumn: THTMLElement;
  DocNode: TDocNode;
  i: Integer;
  Arg: TPasArgument;
begin
  HaveArgDescr:=False;
  I:=0;
  While (I<Element.Args.Count) and not HaveArgDescr do
    begin
    Arg := TPasArgument(Element.Args[i]);
    HaveArgDescr:=Not IsDescrNodeEmpty(Engine.FindShortDescr(Arg));
    inc(i);
    end;
  if HaveArgDescr then
    begin
    AppendText(CreateH2(Parent), SDocArguments);
    for i := 0 to Element.Args.Count - 1 do
      begin
      lColumns := CreateListColumns(Parent);
      Arg := TPasArgument(Element.Args[i]);
      if IsDescrNodeEmpty(Engine.FindShortDescr(Arg)) then
        continue;
      lColumn:=CreateListColumn1(lColumns);
      AppendText(lColumn, Arg.Name);
      lColumn:=CreateListColumn2(lColumns);
      AppendShortDescrCell(lColumn, Arg);
      end;
    end;
  if (Element.ClassType = TPasFunctionType) and not SkipResult then
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

procedure TNewHTMLWriter.AppendTopicMenuBar(Topic : TTopicElement);

  function AddLink(aParent : THTMLElement; ALinkSubpageIndex: Integer; const AName: String) : THTMLElement;

  begin
    Result:=CreateLink(aParent, ResolveLinkWithinPackage(Module, ALinkSubpageIndex));
    Result['class']:='navbar-item';
    AppendText(Result,aName);
  end;

  function AddPackageLink(aParent: THTMLElement; ALinkSubpageIndex: Integer; const AName: String) : THTMLElement;
  begin
    Result:=CreateLink(aParent, ResolveLinkWithinPackage(Package, ALinkSubpageIndex));
    Result['class']:='navbar-item';
    AppendText(Result,aName);
  end;


var
  El,NavBrand, NavItem, NavEl, NavMenu, NavBar : THTMLElement;
begin
  NavEl := CreateEl(ContentElement, 'nav','navbar is-link is-fixed-top');
  NavEl['role']:='navigation';
  NavEl['aria-label']:='main navigation';
  NavBrand:=CreateEl(NavEl,'div');
  // We use the brand for the link to the overview
  NavBrand['class']:='navbar-brand';
  NavItem:=CreateEl(NavBrand,'a');
  NavItem['class']:='navbar-item';
  if Assigned(Module) then
    begin
    NavItem['href']:=UTF8Decode(ResolveLinkWithinPackage(Module, 0));
    AppendText(NavItem,UTF8Decode(Module.Name));
    end
  else
    begin
    NavItem['href']:=UTF8Decode(ResolveLinkWithinPackage(Package, IndexSubIndex));
    AppendText(NavItem,UTF8Decode(Package.Name));
    end;
  // Now the other items follow
  NavMenu:=CreateEl(NavEl,'div');
  NavMenu['class']:='navbar-menu';
  NavBar:=CreateEl(NavMenu,'div');
  NavBar['class']:='navbar-start';
  if Assigned(Module) then
    begin
    // AddLink(NavBar,0, 'Unit '+Module.Name);
    if Module.InterfaceSection.ResStrings.Count > 0 then
      AddLink(NavBar,ResstrSubindex, SDocResStrings);
    if Module.InterfaceSection.Consts.Count > 0 then
      AddLink(NavBar,ConstsSubindex, SDocConstants);
    if Module.InterfaceSection.Types.Count > 0 then
      AddLink(NavBar,TypesSubindex, SDocTypes);
    if Module.InterfaceSection.Classes.Count > 0 then
      AddLink(NavBar,ClassesSubindex, SDocClasses);
    if Module.InterfaceSection.Functions.Count > 0 then
      AddLink(NavBar,ProcsSubindex, SDocProceduresAndFunctions);
    if Module.InterfaceSection.Variables.Count > 0 then
      AddLink(NavBar,VarsSubindex, SDocVariables);
    AddLink(NavBar,IndexSubIndex,SDocIdentifierIndex);
    AppendFragment(NavBar, NavigatorHTML);
    end
  else
    begin
    // Index
    AddPackageLink(NavBar,IndexSubIndex, SDocIdentifierIndex);
    // Class TObject tree
    AddPackageLink(NavBar,ClassHierarchySubIndex, SDocPackageClassHierarchy);
    AppendFragment(NavBar, NavigatorHTML)
    end;
  NavBar:=CreateEl(NavMenu,'div');
  NavBar['class']:='navbar-end';

  if Length(SearchPage) > 0 then
    begin
    El:=CreateLink(NavBar, SearchPage);
    El['class']:='navbar-item';
    AppendText(El, SDocSearch);
    end;

  if Assigned(Module) and Assigned(Package) then // Displays a Package page
  begin
    El:=AppendHyperlink(NavBar, Package) as THTMLELement;
    El['class']:='navbar-item';
  end;
  AppendFragment(ContentElement,HeaderHTML);
end;


function TNewHTMLWriter.CreateAllocator: TFileAllocator;
begin
   Result:=TLongNameFileAllocator.Create('.html');
end;

procedure TNewHTMLWriter.AppendMenuBar(ASubpageIndex: Integer);

  function AddLink(aParent : THTMLElement; ALinkSubpageIndex: Integer; const AName: String) : THTMLElement;

  begin
    Result:=CreateLink(aParent, ResolveLinkWithinPackage(Module, ALinkSubpageIndex));
    Result['class']:='navbar-item';
    AppendText(Result,aName);
  end;

  function AddPackageLink(aParent: THTMLElement; ALinkSubpageIndex: Integer; const AName: String) : THTMLElement;
  begin
    Result:=CreateLink(aParent, ResolveLinkWithinPackage(Package, ALinkSubpageIndex));
    Result['class']:='navbar-item';
    AppendText(Result,aName);
  end;


var
  El,NavBrand, NavItem, NavEl, NavMenu, NavBar : THTMLElement;
begin

  NavEl := CreateEl(ContentElement, 'nav','navbar is-link is-fixed-top');
  NavEl['role']:='navigation';
  NavEl['aria-label']:='main navigation';
  NavBrand:=CreateEl(NavEl,'div');
  // We use the brand for the link to the overview
  NavBrand['class']:='navbar-brand';
  if CreateSideMenu then
    begin
    NavItem:=CreateEl(NavBrand,'a','navbar-item');
    NavItem['role']:='button';
    NavItem['id']:='menu-toggle';
    NavItem['aria-label']:='menu';
    NavItem['aria-expanded']:='false';
    El:=CreateEl(NavItem,'span','burger-icon');
    AppendText(El,#$2261); //
    end;
  NavItem:=CreateEl(NavBrand,'a');
  NavItem['class']:='navbar-item';
  if Assigned(Module) then
    begin
    NavItem['href']:=UTF8Decode(ResolveLinkWithinPackage(Module, 0));
    AppendText(NavItem, UTF8Decode(Format(SDocUnitMenuTitle,[Module.Name])));
    end
  else
    begin
    NavItem['href']:=UTF8Decode(ResolveLinkWithinPackage(Package, IndexSubIndex));
    AppendText(NavItem,UTF8Decode(Format(SDocPackageMenuTitle,[Package.Name])));
    end;
  // Now the other items follow
  NavMenu:=CreateEl(NavEl,'div');
  NavMenu['class']:='navbar-menu';
  NavBar:=CreateEl(NavMenu,'div');
  NavBar['class']:='navbar-start';
  if Assigned(Module) then
    begin
    // AddLink(NavBar,0, 'Unit '+Module.Name);
    if Module.InterfaceSection.ResStrings.Count > 0 then
      AddLink(NavBar,ResstrSubindex, SDocResStrings);
    if Module.InterfaceSection.Consts.Count > 0 then
      AddLink(NavBar,ConstsSubindex, SDocConstants);
    if Module.InterfaceSection.Types.Count > 0 then
      AddLink(NavBar,TypesSubindex, SDocTypes);
    if Module.InterfaceSection.Classes.Count > 0 then
      AddLink(NavBar,ClassesSubindex, SDocClasses);
    if Module.InterfaceSection.Functions.Count > 0 then
      AddLink(NavBar,ProcsSubindex, SDocProceduresAndFunctions);
    if Module.InterfaceSection.Variables.Count > 0 then
      AddLink(NavBar,VarsSubindex, SDocVariables);
    AddLink(NavBar,IndexSubIndex,SDocIdentifierIndex);
    AppendFragment(NavBar, NavigatorHTML);
    end
  else
    begin
    // Index
    AddPackageLink(NavBar,IndexSubIndex, SDocIdentifierIndex);
    // Class TObject tree
    AddPackageLink(NavBar,ClassHierarchySubIndex, SDocPackageClassHierarchy);
    AppendFragment(NavBar, NavigatorHTML)
    end;
  NavBar:=CreateEl(NavMenu,'div');
  NavBar['class']:='navbar-end';

  if Length(SearchPage) > 0 then
    begin
    El:=CreateLink(NavBar, SearchPage);
    El['class']:='navbar-item';
    AppendText(El, SDocSearch);
    end;

  if Assigned(Module) and Assigned(Package) then // Displays a Package page
  begin
    El:=AppendHyperlink(NavBar, Package) as THTMLELement;
    El['class']:='navbar-item';
  end;
  AppendFragment(ContentElement,HeaderHTML);
end;


procedure TNewHTMLWriter.AppendFooter;

Var
  S : String;
  lContent,lDateEl,lFooter : TDomElement;
begin
  if not (Assigned(FooterHTML) or IncludeDateInFooter) then
    exit;
  lFooter:=CreateEl(ContentElement, 'footer','footer');
  lContent:=CreateEl(lFooter, 'div','has-text-centered');
  if Assigned(FooterHTML) then
    AppendFragment(lContent, FooterHTML)
  else if IncludeDateInFooter then
    begin
    lDateEl:=CreateEl(lContent,'span','footertext');
    If (FDateFormat='') then
      S:=DateToStr(Date)
    else
      S:=FormatDateTime(FDateFormat,Date);
    AppendText(lDateEl,Format(SDocDateGenerated,[S]));
    end;
end;

function TNewHTMLWriter.CreateSection(aParent: THTMLElement): THTMLElement;
begin
  Result:=CreateEl(aParent,'section','section');
end;

procedure TNewHTMLWriter.DescrWriteFileEl(const AText: DOMString);
var
  NewEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'span');
  NewEl['class'] := 'fileref';
  AppendText(NewEl, AText);
end;

procedure TNewHTMLWriter.DescrWriteVarEl(const AText: DOMString);
var
  NewEl: TDOMElement;
begin
  NewEl := CreateEl(CurOutputNode, 'span');
  NewEl['class'] := 'identifier';
  AppendText(NewEl, AText);
end;

function TNewHTMLWriter.AppendPasSHFragment(Parent: TDOMNode; const AText: String; AShFlags: Byte): Byte;
begin
  Result:=AppendPasSHFragment(Parent, AText, AShFlags,Nil);
end;

function TNewHTMLWriter.AppendPasSHFragment(Parent: TDOMNode; const AText: String; AShFlags: Byte;
  aLinkIdentifierMap: TLinkIdentifierMap): Byte;

var
  El: TDOMElement;

  Procedure OutputToken(aToken : TSyntaxToken);

  Var
    CurParent: TDomNode;
    lLink : String;

  begin
    lLink:='';
    If (aToken.Text='') then
      exit;
    If (el<>Nil) then
      CurParent:=El
    else
      begin
      CurParent:=Parent;
      if (aToken.Kind=shDefault) and Assigned(aLinkIdentifierMap) then
        lLink:=aLinkIdentifierMap.GetLink(aToken.Text);
      end;
    if lLink<>'' then
      CurParent:=CreateLink(CurParent,lLink);
    AppendText(CurParent,aToken.Text);
    El:=Nil;
  end;

  Function NewEl(Const ElType,Attr,AttrVal : DOMString) : TDomElement;

  begin
    Result:=CreateEl(Parent,ElType);
    Result[Attr]:=AttrVal;
  end;

  Function NewSpan(Const AttrVal : DOMString) : TDomElement;

  begin
    Result:=CreateEl(Parent,'span');
    Result['class']:=AttrVal;
  end;
var
  HL : TPascalSyntaxHighlighter;
  Tokens : TSyntaxTokenArray;
  T : TSyntaxToken;
begin
  Result:=0;
  HL:=TPascalSyntaxHighlighter.Create;
  Try
    Tokens:=HL.Execute(aText);
    For T in Tokens do
      begin
      case T.Kind  of
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
      OutputToken(T);
      end;
  finally
    HL.Free;
  end;
end;

procedure TNewHTMLWriter.AppendDescrSection(AContext: TPasElement; Parent: TDOMNode; DescrNode: TDOMElement; const ATitle: DOMString);

var
  lContent,lSection : THTMLElement;
begin
  if not IsDescrNodeEmpty(DescrNode) then
  begin
    lSection:=CreateSection(Parent as THTMLElement);
    lContent:=CreateEl(lSection,'div','content');
    If (ATitle<>'') then // Can be empty for topic.
      AppendText(CreateH2(lContent), ATitle);
    AppendDescr(AContext, lContent, DescrNode, True);
  end;
end;

procedure TNewHTMLWriter.AppendSeeAlsoSection(AElement: TPasElement; aParent: TDOMElement; DocNode: TDocNode);

  procedure GetSeeAlsoNodes(aList : TFPList);
  var
    Node : TDOMNode;
  begin
    // Get all nodes.
    Node:=DocNode.SeeAlso.FirstChild;
    While Assigned(Node) do
      begin
      if (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='link') then
        aList.Add(Node);
      Node := Node.NextSibling;
      end;
  end;

  procedure AppendSeeAlsoName(aParent : THTMLElement; El : TDOmElement);
  var
    NewEl : THTMLElement;
    l,s,n : domstring;
  begin
    l:=El['id'];
    // Create parent element for link text/id
    if Assigned(Engine) and Engine.FalbackSeeAlsoLinks then
      s:= ResolveLinkIDUnStrict(UTF8ENcode(l))
    else
      s:= ResolveLinkID(UTF8ENcode(l));
    if Length(s)=0 then
      begin
      if assigned(module) then
        s:=UTF8Decode(module.name)
      else
        s:='?';
      if l='' then l:='<empty>';
      if Assigned(AElement) then
        N:=UTF8Decode(AElement.PathName)
      else
        N:='?';
      DoLog(SErrUnknownLinkID, [s,N,l]);
      LinkUnresolvedInc();
      NewEl := CreateEl(aParent,'b')
      end
    else
      NewEl := CreateLink(aParent,s);
    // Append link
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
  end;

  Procedure AppendLinkShortDescr(aParent : THTMLElement; aDocEl: TDomElement);
  var
     l : domstring;
     DescrEl : TDomElement;
  begin
     l:=aDocEl['id'];
     DescrEl := Engine.FindShortDescr(AElement.GetModule,UTF8Encode(L));
     if Not Assigned(DescrEl) then
       exit;
     aParent['class'] := aParent['class']+' cmt';
     PushOutputNode(aParent);
     try
       ConvertShort(aElement, DescrEl);
     finally
       PopOutputNode;
     end;
  end;

var
  DocEl : TDOMElement;
  lSection, lColumns, lColumn: THTMLElement;
  i : integer;
  List : TFPList;

begin
  if Not (Assigned(DocNode) and Assigned(DocNode.SeeAlso)) then
    Exit;

  List:=TFPList.Create;
  try
    GetSeeAlsoNodes(List);
    if List.Count=0 then
      exit;
    lSection:=CreateSection(aParent as THTMLElement);
    AppendText(CreateH2(lSection), SDocSeeAlso);
    For I:=0 to List.Count-1 do
      begin
      DocEl:=TDOMElement(List[i]);
      lColumns:=CreateListColumns(lSection);
      // Name
      lColumn:=CreateListColumn1(lColumns);
      AppendSeeAlsoName(lColumn,DocEl);
      lColumn:=CreateListColumn2(lColumns);
      AppendLinkShortDescr(lColumn,DocEl);
      end;
  finally
    List.Free;
  end;
end;

procedure TNewHTMLWriter.FinishElementPage(AElement: TPasElement; aDescription : Boolean = True);

var
  DocNode: TDocNode;
  lSection : THTMLElement;
begin
  DocNode := Engine.FindDocNode(AElement);
  If Not Assigned(DocNode) then
    exit;

  // Description
  if aDescription and Assigned(DocNode.Descr) then
    AppendDescrSection(AElement, ContentElement, DocNode.Descr, UTF8Decode(SDocDescription));

  // Append "Errors" section
  if Assigned(DocNode.ErrorsDoc) then
    AppendDescrSection(AElement, ContentElement, DocNode.ErrorsDoc, UTF8Decode(SDocErrors));

  // Append Version info
  if Assigned(DocNode.Version) then
    AppendDescrSection(AElement, ContentElement, DocNode.Version, UTF8Decode(SDocVersion));

  // Append "See also" section
  AppendSeeAlsoSection(AElement,ContentElement,DocNode);

  // Append examples, if present
  lSection:=CreateSection(ContentElement);
  AppendExampleSection(AElement,lSection, DocNode);
  // Append notes, if present
  ConvertNotes(AElement,DocNode.Notes);
end;

procedure TNewHTMLWriter.CreateTopicPageBody(AElement: TTopicElement);

var
  DocNode: TDocNode;
  lSection : THTMLElement;

begin
  AppendTopicMenuBar(AElement);
  DocNode:=AElement.TopicNode;
  if Assigned(DocNode) then  // should always be true, but we're being careful.
    begin
    lSection:=CreateSection(ContentElement);
    AppendShortDescr(AElement,CreateH2(lSection), DocNode);
    if Assigned(DocNode.Descr) then
       AppendDescrSection(AElement, lSection, DocNode.Descr, '');
    AppendSeeAlsoSection(AElement,ContentElement,DocNode);
    CreateTopicLinks(ContentElement,DocNode,AElement);
    AppendExampleSection(AElement,DocNode);
    ConvertNotes(AElement,DocNode.Notes);
    end;
end;

procedure TNewHTMLWriter.CreateClassHierarchyPage(AddUnit : Boolean);

type
  TypeEN = (NPackage, NModule, NName);

  function PushClassElement(aParent : THTMLElement; IsParent : Boolean) : THTMLElement;
  Var
    H : THTMLElement;
  begin
    H:=CreateEl(aParent, 'li');
    if IsParent then
      begin
      H['class']:='parent expanded';
      H['onclick']:='expandorcollapse(event)';
      end;
    Result:=H;
  end;

  Function PushClassList(aParent : THTMLELement) : THTMLElement;

  begin
    Result:=CreateEl(aParent, 'ul');
    Result['class']:='treeview';
  end;

  function ExtractName(APathName: String; Tp: TypeEN):String;
  var
  l:TStringList;
  begin
    Result:= Trim(APathName);
    if Result = '' then exit;
    l:=TStringList.Create;
    try
      l.AddDelimitedText(Result, '.', True);
      if l.Count=3 then
        Result:= l.Strings[Integer(Tp)]
      else
        Result:='';
    finally
      l.free;
    end;
  end;

  Procedure AppendClass(aParent : THTMLElement; EN : TPasElementNode);

  Var
    PE,PM : TPasElement;
    I : Integer;
    El, SubParent : THTMLELement;

  begin
    if not Assigned(EN) then exit;
    PE:=EN.Element;
    SubParent:=PushClassElement(aParent,EN.ChildCount>0);
    if (PE<>Nil) then
      begin
      El:=CreateEl(SubParent, 'span');
      AppendHyperLink(El,PE);
      PM:=PE.GetModule();
      if (PM<>Nil) then
        begin
        AppendText(El,' (');
        AppendHyperLink(El,PM);
        AppendText(el,')');
        end
      end
    else
      AppendText(El,EN.Element.Name);
    if EN.ChildCount>0 then
      begin
      El:=PushClassList(SubParent);
      For I:=0 to EN.ChildCount-1 do
        AppendClass(El,EN.Children[i] as TPasElementNode);
      end;
  end;

begin
  AppendClass(PushClassList(ContentElement),TreeClass.RootNode);
end;

procedure TNewHTMLWriter.CreatePackageClassHierarchy;

Const
  SFunc = 'function expandorcollapse (event) { '+sLineBreak+
          '  var el = event.target;'+sLineBreak+
          '  if (el) { '+sLineBreak+
          '    el.classList.toggle("expanded");'+sLineBreak+
          '    event.stopPropagation();'+sLineBreak+
          '  }'+sLineBreak+
          '}';

Var
  S : String;
  SE : THTMLElement;

begin
  SE := Doc.CreateElement('script');
  AppendText(SE,SFunc);
  HeadElement.AppendChild(SE);
  S:=Package.Name;
  If Length(S)>0 then
    Delete(S,1,1);
  AppendTitle(ContentElement,UTF8Decode(Format(SDocPackageClassHierarchy, [S])));
  CreateClassHierarchyPage(True);
end;

procedure TNewHTMLWriter.CreatePageBody(AElement: TPasElement; ASubpageIndex: Integer);

begin
  if Module=nil then
    begin
    If (ASubPageIndex=0) then
      CreatePackagePageBody
    else if ASubPageIndex=IndexSubIndex then
      CreatePackageIndex
    else if ASubPageIndex=ClassHierarchySubIndex then
      CreatePackageClassHierarchy
    end
  else
    begin
    if AElement.ClassType.inheritsfrom(TPasModule) then
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
      CreateProcPageBody(TPasProcedureBase(AElement))
    else if AElement.ClassType = TTopicELement then
      CreateTopicPageBody(TTopicElement(AElement))
    else if AElement.ClassType = TPasProperty then
      CreateClassMemberPageBody(TPasProperty(AElement))
    else
      writeln('Unknown classtype: ',AElement.classtype.classname);
  end;
end;

procedure TNewHTMLWriter.CreateIndexPage(aParent: THTMLElement; L: TStringList);
Var
  Lists  : Array['A'..'Z'] of TStringList;
  CL : TStringList;
  lColumns, lColumn,  EL, el2: TDOMElement;
  E : TPasElement;
  I : Integer;
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
        If C='_' then
          C:='A';
        If (C in ['A'..'Z']) and (Lists[C]=Nil) then
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
  lColumns := CreateEl(aParent,'div','columns is-multiline');
  for C:='A' to 'Z' do
    If (Lists[C]<>Nil) then
      begin
      lColumn:=CreateEl(lColumns,'div','column is-narrow');
      lColumn:=CreateLink(lColumn,UTF8Decode('#SECTION'+C));
      lColumn['class']:='button is-link';
      AppendText(lColumn,UTF8Decode(C));
      end;
  // Now emit all identifiers.
  For C:='A' to 'Z' do
    begin
    CL:=Lists[C];
    If CL<>Nil then
      begin
      El:=CreateH2(aParent);
      AppendText(El,UTF8Decode(C));
      CreateAnchor(El,UTF8Decode('SECTION'+C));
      El:=CreateEl(aParent,'div');
      EL['style']:='display: block; column-count: 3';
      for I:=0 to CL.Count-1 do
        begin
        E:=TPasElement(CL.Objects[I]);
        El2:=AppendHyperlink(El,E);
        if assigned(EL2) then
          EL2['style']:='display: block;';
        end;
      end; // have List
    end;  // For C:=
  Finally
    for C:='A' to 'Z' do
      FreeAndNil(Lists[C]);
  end;
end;

procedure TNewHTMLWriter.CreatePackageIndex;

Var
  L : TStringList;
  I : Integer;
  M : TPasModule;
  S : String;
  lSection : THTMLElement;
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
    S:=Package.Name;
    If Length(S)>0 then
      Delete(S,1,1);
    lSection:=CreateSection(ContentElement);
    AppendTitle(lSection,UTF8Decode(Format(SDocPackageIndex, [S])));
    CreateIndexPage(lSection,L);
  Finally
    L.Free;
  end;
end;

procedure TNewHTMLWriter.CreatePackagePageBody;

var
  DocNode: TDocNode;
  lSection, lColumns, lColumn : THTMLElement;
  TableEl, TREl: TDOMElement;
  i: Integer;
  ThisModule: TPasModule;
  L : TStringList;

begin
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,Format(SDocPackageTitle, [Copy(Package.Name, 2, 256)]));
  AppendShortDescr(CreatePara(lSection), Package);
  AppendText(CreateH2(lSection), UTF8Encode(SDocUnits));
  TableEl := CreateTable(lSection);
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
      lColumns:=CreateListColumns(lSection);
      lColumn:=CreateListColumn1(lColumns);
      AppendHyperlink(lColumn, ThisModule);
      lColumn:=CreateListColumn2(lColumns);
      AppendShortDescrCell(lColumn, ThisModule);
      end;
  Finally
    L.Free;
  end;
  DocNode := Engine.FindDocNode(Package);
  if Assigned(DocNode) then
    begin
    if Assigned(DocNode.Descr) then
       AppendDescrSection(nil, ContentElement, DocNode.Descr, UTF8Decode(SDocDescription));
    CreateTopicLinks(ContentElement, DocNode,Package);
    end;
end;

function TNewHTMLWriter.CreateListColumns(aParent : THTMLElement) : THTMLElement;
begin
  Result:=CreateEl(aParent,'div','columns list');
end;

function TNewHTMLWriter.CreateListColumn1(aParent : THTMLElement) : THTMLElement;
begin
  Result:=CreateEl(aParent,'div','column is-2 list');
  Result['style']:='overflow:hidden; text-overflow: ellipsis;';
end;

function TNewHTMLWriter.CreateListColumn2(aParent : THTMLElement) : THTMLElement;
begin
  Result:=CreateEl(aParent,'div','column is-10 list');
end;


procedure TNewHTMLWriter.CreateTopicLinks (aParent : THTMLElement; Node: TDocNode; PasElement: TPasElement) ;

var
  DocNode: TDocNode;
  lSection,lColumns,lColumn: THTMLElement;
  HaveTopics : Boolean;
  ThisTopic: TPasElement;

begin
  DocNode:=Node.FirstChild;
  HaveTopics:=False;
  While Assigned(DocNode) and not HaveTopics do
    begin
    HaveTopics:=DocNode.TopicNode;
    DocNode:=DocNode.NextSibling;
    end;
  if not HaveTopics then
    exit;
  lSection:=CreateSection(aParent);
  AppendText(CreateH2(lSection), UTF8Decode(SDocRelatedTopics));
  DocNode:=Node.FirstChild;
  While Assigned(DocNode) do
    begin
    if DocNode.TopicNode then
      begin
      lColumns:=CreateListColumns(lSection);
      lColumn:=CreateListColumn1(lColumns);
      ThisTopic:=FindTopicElement(DocNode);
      if Assigned(ThisTopic) then
        AppendHyperlink(lColumn, ThisTopic);
      lColumn:=CreateListColumn2(lColumns);
      if Assigned(ThisTopic) then
        AppendShortDescrCell(lColumn, ThisTopic);
      end;
    DocNode:=DocNode.NextSibling;
    end;
end;

function TNewHTMLWriter.GetElementCode(aElement: TPasElement; aSparse : boolean; aFlags : TElementFlags =  []): String;
var
  W : TPasWriter;
  S : TStringStream;
  Vis : TPasMemberVisibilities;
begin
  Vis:=[];
  if Engine.HidePrivate then
    Vis:=Vis+[visPrivate,visStrictPrivate];
  if Engine.HideProtected then
    Vis:=Vis+[visProtected,visStrictProtected];
  W:=nil;
  S:=TStringStream.Create('');
  try
    W:=TPasWriter.Create(S);
    if aSparse then
      W.Options:=W.Options+[woSparse];
    W.SkipVisibilities:=Vis;
    W.WriteElement(aElement,[efSkipSection]+aFlags);
    Result:=S.DataString;
  finally
    S.Free;
    W.Free;
  end;

end;

procedure TNewHTMLWriter.CreateModuleIndexPage(AModule: TPasModule);

Var
  L : TStringList;
  lSection: THTMLElement;
begin
  L:=TStringList.Create;
  try
    AddModuleIdentifiers(AModule,L);
    lSection:=CreateSection(ContentElement);
    AppendTitle(lSection,Format(SDocModuleIndex, [AModule.Name]));
    PushContentElement(lSection);
    CreateIndexPage(lSection,L);
    PopContentElement;
  Finally
    L.Free;
  end;
end;

procedure TNewHTMLWriter.CreateModuleMainPage(aModule : TPasModule);

var
  lContent,lSection,lColumns,lColumn: THTMLElement;
  i: Integer;
  UnitRef: TPasType;
  DocNode: TDocNode;


begin
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,Format(SDocUnitTitle, [AModule.Name]),AModule.Hints);
  lContent:=CreateEl(lSection,'div','content');
  AppendShortDescr(lContent, AModule);

  if AModule.InterfaceSection.UsesList.Count > 0 then
  begin
    AppendKw(CreateCode(CreatePara(lContent)), 'uses');
    for i := 0 to AModule.InterfaceSection.UsesList.Count - 1 do
    begin
      lColumns:=CreateListColumns(lSection);
      UnitRef := TPasType(AModule.InterfaceSection.UsesList[i]);
      DocNode := Engine.FindDocNode(UnitRef);
      if Assigned(DocNode) and DocNode.IsSkipped then
        continue;
      lColumn:=CreateListColumn1(lColumns);
      AppendHyperlink(lColumn, UnitRef);
      lColumn:=CreateListColumn2(lColumns);
      AppendShortDescrCell(lColumn, UnitRef);
    end;
  end;

  DocNode := Engine.FindDocNode(AModule);
  if Assigned(DocNode) then
    begin
    if Assigned(DocNode.Descr) then
      AppendDescrSection(AModule, ContentElement, DocNode.Descr, UTF8Decode(SDocOverview));
    ConvertNotes(AModule,DocNode.Notes);
    CreateTopicLinks(ContentElement, DocNode,AModule);
    end;
end;


procedure TNewHTMLWriter.CreateModuleSimpleSubpage(aModule: TPasModule; ASubpageIndex: Integer; const ATitle: DOMString; AList: TFPList);

var
  lSection, ColumnsEl, ColumnEl, CodeEl: TDOMElement;
  i, j: Integer;
  Decl: TPasElement;
  SortedList: TFPList;
  DocNode: TDocNode;
  S : String;

begin
  S:=UTF8Encode(ATitle);
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,Format(SDocUnitTitle + ': %s', [AModule.Name, S]));
  SortedList := TFPList.Create;
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

    for i := 0 to SortedList.Count - 1 do
    begin
      ColumnsEl := CreateEl(lSection,'div','columns');
      Decl := TPasElement(SortedList[i]);
      ColumnEl := CreateEl(ColumnsEl,'div','column is-one-fifth list');
      ColumnEl['style']:='overflow:hidden; text-overflow: ellipsis;';
      CodeEl := CreateCode(ColumnEl);
      AppendHyperlink(CodeEl, Decl);
      ColumnEl := CreateEl(ColumnsEl,'div','column is-four-fifths list');
      AppendShortDescrCell(ColumnEl, Decl);
    end;
  finally
    SortedList.Free;
  end;
end;

procedure TNewHTMLWriter.CreateModuleResStringsPage(aModule : TPasModule);
var
  lsection,lColumns,lColumn: THTMLElement;
  i: Integer;
  Decl: TPasResString;

begin
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,Format(SDocUnitTitle + ': %s', [AModule.Name, SDocResStrings]));

  for i := 0 to AModule.InterfaceSection.ResStrings.Count - 1 do
  begin
    lColumns:=CreateListColumns(lSection);
    Decl := TPasResString(AModule.InterfaceSection.ResStrings[i]);
    lColumn:=CreateListColumn1(lColumns);
    CreateEl(lColumn, 'a')['name'] := UTF8Decode(LowerCase(Decl.Name));
    AppendText(lColumn, UTF8Decode(Decl.Name));
    lColumn := CreateListColumn2(lColumns);
    AppendText(lColumn, UTF8Decode(Decl.Expr.getDeclaration(true)));
  end;
end;


procedure TNewHTMLWriter.CreateModulePageBody(AModule: TPasModule;
  ASubpageIndex: Integer);

begin
  case ASubpageIndex of
    0:
      CreateModuleMainPage(aModule);
    ResstrSubindex:
      CreateModuleResStringsPage(aModule);
    ConstsSubindex:
      CreateModuleSimpleSubpage(aModule, ConstsSubindex,UTF8Decode(SDocConstants), AModule.InterfaceSection.Consts);
    TypesSubindex:
      CreateModuleSimpleSubpage(aModule, TypesSubindex,UTF8Decode(SDocTypes), AModule.InterfaceSection.Types);
    ClassesSubindex:
      CreateModuleSimpleSubpage(aModule, ClassesSubindex,UTF8Decode(SDocClasses), AModule.InterfaceSection.Classes);
    ProcsSubindex:
      CreateModuleSimpleSubpage(aModule, ProcsSubindex, UTF8Decode(SDocProceduresAndFunctions), AModule.InterfaceSection.Functions);
    VarsSubindex:
      CreateModuleSimpleSubpage(aModule, VarsSubindex,UTF8Decode(SDocVariables), AModule.InterfaceSection.Variables);
    IndexSubIndex:
      CreateModuleIndexPage(AModule);
  end;
end;

procedure TNewHTMLWriter.CreateConstPageBody(AConst: TPasConst);
var
  Section,CodeEl: THTMLElement;

begin
  Section:=CreateSection(ContentElement);
  AppendTitle(Section,AConst.Name,AConst.Hints);
  AppendShortDescr(CreatePara(Section), AConst);
  Section:=CreateSection(ContentElement);
  AppendText(CreateH2(Section), UTF8Decode(SDocDeclaration));
  AppendSourceRef(Section,AConst);
  CodeEl := AppendCodeBlock(Section);
  AppendPasSHFragment(CodeEl, GetElementCode(aConst,False),0);
  FinishElementPage(AConst);
end;

procedure TNewHTMLWriter.AppendShortDescrCell(Parent: TDOMNode;  Element: TPasElement);

var
  ParaEl: TDOMElement absolute Parent;

begin
  if Assigned(Engine.FindShortDescr(Element)) then
  begin
    if Parent is TDOMElement then
      ParaEl['class'] := ParaEl['class'] +' cmt';
    AppendShortDescr(Parent, Element);
  end;

end;

function TNewHTMLWriter.AppendCodeBlock(aParent : TDOMNode; const aLanguage : String = ''): THTMLElement;
var
  lLanguage : String;

begin
  Result:=CreateEl(aParent,'pre','code');
  lLanguage:=aLanguage;
  if lLanguage='' then
    lLanguage:='pascal';
  lLanguage:='code-'+lLanguage;
  Result:=CreateEl(Result,UTF8Decode('code'),UTF8Decode(lLanguage));
end;

function TNewHTMLWriter.AppendHighlightedCode(aParent: TDOMNode; aCode: String; const aLanguage: String; aMap: TLinkIdentifierMap
  ): THTMLElement;
begin
//  Writeln('Code:',aCode);
  Result:=AppendCodeBlock(aParent,aLanguage);
  AppendPasSHFragment(Result, aCode, 0, aMap);
end;

function TNewHTMLWriter.CreateCodeLines(aLines: array of string): string;
var
  i : integer;
begin
  Result:='';
  if Length(aLines)=0 then
    exit;
  Result:=aLines[0];
  For I:=1 to Length(aLines)-1 do
    Result:=Result+sLineBreak+aLines[i];
end;

procedure TNewHTMLWriter.AppendAliasTypeDecl(aType: TPasAliasType);
begin
  AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+aType.GetDeclaration(True)+';']));
end;

procedure TNewHTMLWriter.AppendTypeAliasTypeDecl(aType: TPasTypeAliasType);

var
  CodeEl : THTMLElement;
begin
  CodeEl:=AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+aType.GetDeclaration(True)+';']));
  AppendHyperlink(CodeEl, TPasTypeAliasType(AType).DestType);
end;


procedure TNewHTMLWriter.AppendClassOfTypeDecl(aType: TPasClassOfType);
var
  CodeEl : THTMLElement;

begin
  CodeEl:=AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+aType.GetDeclaration(True)+';']));
  if Assigned(TPasClassOfType(AType).DestType) then
    AppendHyperlink(CodeEl, TPasClassOfType(AType).DestType);
end;

procedure TNewHTMLWriter.AppendEnumTypeDecl(aType: TPasEnumType);
var
  S : String;
  i : integer;
  lColumns,lColumn : THTMLElement;
  EnumValue: TPasEnumValue;
begin
  S:=aType.GetDeclaration(true);
  AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+S]));
  AppendText(CreateH3(ContentElement),'Values');
  for i := 0 to AType.Values.Count - 1 do
  begin
    lColumns := CreateListColumns(ContentElement);
    EnumValue := TPasEnumValue(AType.Values[i]);
    lColumn := CreateListColumn1(lColumns);
    AppendText(lColumn,EnumValue.Name);
    lColumn:= CreateListColumn2(lColumns);
    AppendShortDescrCell(lColumn, EnumValue);
  end;
end;

procedure TNewHTMLWriter.AppendPointerTypeDecl(aType: TPasPointerType);
var
  CodeEl : THTMLElement;
begin
  CodeEl:=AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+aType.GetDeclaration(True)+';']));
  if Assigned(AType.DestType) then
    AppendHyperlink(CodeEl, AType.DestType)
end;

procedure TNewHTMLWriter.AppendProcedureTypeDecl(aType: TPasProcedureType);

begin
  AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+aType.GetDeclaration(True)+';']));
  AppendProcArgsSection(ContentElement, AType);
end;


procedure TNewHTMLWriter.AppendRecordTypeDecl(aType: TPasRecordType);
var
  lMap: TLinkIdentifierMap;
  I : Integer;

begin
  lMap:=TLinkIdentifierMap.Create(Self);
  try
    for I:=0 to aType.Members.Count-1 do
      lMap.AddLink(TPasElement(aType.Members[i]));
    AppendHighlightedCode(ContentElement,GetElementCode(aType,True),'',lMap);
  finally
    lMap.Free;
  end;
end;


procedure TNewHTMLWriter.AppendSetTypeDecl(aType: TPasSetType);
var
  dlEl,ddEl,dtEl,CodeEl : THTMLElement;
  EnumType : TPasEnumType;
  EnumValue : TPasEnumValue;
  I : Integer;

begin
  CodeEl:=AppendHighlightedCode(ContentElement,CreateCodeLines(['type','  '+aType.GetDeclaration(True)+';']));
  if AType.EnumType.ClassType = TPasEnumType then
    begin
    EnumType:=TPasEnumType(AType.EnumType);
    AppendText(CreateH3(ContentElement),'Values');
    dlEl := CreateEl(ContentElement,'div','columns list');
    for i := 0 to EnumType.Values.Count - 1 do
    begin
      EnumValue := TPasEnumValue(EnumType.Values[i]);
      dtEl := CreateEl(dlEl,'div','column is-2 list');
      AppendText(dtEl,EnumValue.Name);
      ddEl := CreateEl(dlEl,'div','column is-10 list');
      AppendShortDescrCell(ddEl, EnumValue);
    end;
    end
  else
    begin
    AppendHyperlink(CodeEl, TPasSetType(AType).EnumType);
    AppendSym(CodeEl, ';');
    end;
end;

procedure TNewHTMLWriter.AppendTypeDecl(AType: TPasType);

begin
  // Alias
  if AType.ClassType = TPasAliasType then
    AppendAliasTypeDecl(TPasAliasType(aType))
  else if AType.ClassType = TPasClassOfType then
    AppendClassOfTypeDecl(TPasClassOfType(AType))
  else if AType.ClassType = TPasEnumType then
    AppendEnumTypeDecl(TPasEnumType(AType))
  else if AType.ClassType = TPasPointerType then
    AppendPointerTypeDecl(TPasPointerType(aType))
  else if AType.InheritsFrom(TPasProcedureType) then
    AppendProcedureTypeDecl(TPasProcedureType(aType))
  else if AType.ClassType = TPasRecordType then
    AppendRecordTypeDecl(TPasRecordType(aType))
 else if AType.ClassType = TPasSetType then
   AppendSetTypeDecl(TPasSetType(aType))
 else if AType.ClassType = TPasTypeAliasType then
    AppendTypeAliasTypeDecl(TPasTypeAliasType(aType))
 else
  // Probably one of the simple types, which allowed in other places as wel...
    AppendType(ContentElement, TPasType(AType));
end;

procedure TNewHTMLWriter.CreateTypePageBody(AType: TPasType);
var
  Section,CodeEl: THTMLElement;
  DocNode: TDocNode;
begin
  Section:=CreateSection(ContentElement);
  AppendTitle(Section,AType.Name,AType.Hints);
  AppendShortDescr(CreatePara(section), AType);
  Section:=CreateSection(ContentElement);
  AppendText(CreateH2(section), UTF8Decode(SDocDeclaration));
  AppendSourceRef(Section,AType);
  DocNode := Engine.FindDocNode(AType);
  If Assigned(DocNode) and
     Assigned(DocNode.Node) and
     (Docnode.Node['opaque']='1') then
    begin
    CodeEl := AppendCodeBlock(Section);
    AppendKw(CodeEl, 'type ');
    AppendText(CodeEl, UTF8Decode(AType.Name));
    AppendSym(CodeEl, ' = ');
    AppendText(CodeEl,UTF8Decode(SDocOpaque))
    end
  else
    begin
    PushContentElement(Section);
    try
      AppendTypeDecl(AType);
    finally
      PopContentElement;
    end;
    end;
  FinishElementPage(AType);
end;


procedure TNewHTMLWriter.AppendTitle(aParent: TDomElement; const AText: AnsiString; Hints: TPasMemberHints);

begin
  AppendTitle(AParent,UTF8Decode(aText),Hints);
end;

procedure TNewHTMLWriter.AppendTitle(aParent : TDomElement; const AText: DOMString; Hints : TPasMemberHints = []);

Var
  T : UnicodeString;

begin
  T:=AText;
  if (Hints<>[]) then
    T:=T+' ('+UTF8Decode(Engine.HintsToStr(Hints))+')';
  AppendText(TitleElement, AText);
  AppendText(CreateH1(aParent), T)
end;

procedure TNewHTMLWriter.AppendMemberListSection(aParent: THTMLELement; aClass: TPasClassType; aMemberType: TClassMemberType;
  aDeclaredOnly: Boolean);
var
   LinkEl,lSection: THTMLELement;
   LinkAll : Boolean;
begin
  if not HasMembersToShow(aClass,True,GetMemberFilter(aMemberType)) then
    exit;
  linkAll:=aDeclaredOnly and Assigned(aClass.AncestorType) and Not aClass.AncestorType.InheritsFrom(TPasUnresolvedTypeRef);
  lSection:=CreateSection(ContentElement);
  AppendText(CreateH2(lSection),UTF8Decode(GetMemberDocName(aMemberType)));
  CreateAnchor(lsection,UTF8Decode(GetAnchorName(aMemberType)));
  CreateClassMemberList(lSection,AClass,True,GetMemberFilter(aMemberType));
  if LinkAll then
    begin
    LinkEl:=CreateLink(lSection,FixHtmlPath(ResolveLinkWithinPackage(AClass,GetMemberSubIndex(aMemberType))));
    AppendText(LinkEl,SSeeAll+' '+GetMemberDocName(aMemberType));
    end;
end;

procedure TNewHTMLWriter.AppendInheritanceTree(aParent : THTMLELement; aClass : TPasClassType);

  procedure AppendInterfaceInfo(ACodeEl : TDomElement ; AThisClass: TPasClassType);
  var
    i:Integer;
    ThisInterface:TPasClassType;
  begin
  if Assigned(AThisClass) and (AThisClass.Interfaces.count>0) then
    begin
      for i:=0 to AThisClass.interfaces.count-1 do
        begin
          ThisInterface:=TPasClassType(AThisClass.Interfaces[i]);
          AppendText(ACodeEl,',');
          AppendHyperlink(ACodeEl, ThisInterface);
        end;
    end;
  end;

var
  TableEl, TREl, TDEl, CodeEl: TDOMElement;
  ThisClass, PrevClass: TPasType;
  ThisTreeNode: TPasElementNode;
begin
  TableEl := CreateTable(aParent);
  // Process tree class information
  // First tree class link is to This class
  PrevClass:= nil;
  ThisClass:=aClass;
  ThisClass := AClass; ThisTreeNode := Nil;
  if AClass.ObjKind = okInterface then
    ThisTreeNode := TreeInterface.GetPasElNode(AClass)
  else
    ThisTreeNode := TreeClass.GetPasElNode(AClass);
  Repeat
    TREl := CreateTR(TableEl);
    TDEl := CreateTD_vtop(TREl);
    TDEl['align'] := 'center';
    CodeEl := CreateCode(CreatePara(TDEl));


    // Show class item
    AppendHyperlink(CodeEl, ThisClass);
    if Assigned(PrevClass) and (PrevClass Is TPasClassType)  then // Interfaces from prevClass
      AppendInterfaceInfo(CodeEl, TPasClassType(PrevClass));
    TDEl := CreateTD_vtop(TREl);
    AppendShortDescrCell(TDEl, ThisClass);

    if Assigned(ThisTreeNode) then
      if Assigned(ThisTreeNode.ParentNode) then
        begin
        TDEl := CreateTD(CreateTR(TableEl));
        TDEl['align'] := 'center';
        AppendText(TDEl, '|');
        PrevClass:= ThisClass;
        ThisClass := ThisTreeNode.ParentNode.Element;
        ThisTreeNode := ThisTreeNode.ParentNode;
        end
      else
        begin
        ThisClass := nil;
        ThisTreeNode:= nil;
        PrevClass:= nil;
        end
   Until (ThisTreeNode=Nil);
end;

procedure TNewHTMLWriter.CreateClassMainPage(aClass : TPasClassType);

var
  lSection: THTMLElement;
  ParaEl: TDOMElement;
  lMap: TLinkIdentifierMap;
  I : Integer;
  DocNode: TDocNode;
begin
  // Menu bar
  // Title, short description & navs
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,AClass.Name,AClass.Hints);
  ParaEl := CreatePara(lSection);
  AppendShortDescr(CreatePara(lSection), AClass);
  ParaEl:=CreateEl(lSection,'div','tabs');;
  ParaEl:=CreateEl(ParaEl,'ul');
  if HasMembersToShow(aClass,True,@PropertyFilter) then
    AppendText(CreateLink(ParaEl,'#properties'),UTF8Decode(SDocProperties));
  if HasMembersToShow(aClass,True,@MethodFilter) then
    AppendText(CreateLink(ParaEl,'#methods'),UTF8Decode(SDocMethods));
  if HasMembersToShow(aClass,True,@EventFilter) then
    AppendText(CreateLink(ParaEl,'#events'),UTF8Decode(SDocEvents));
  // Declaration
  lSection:=CreateSection(ContentElement);
  AppendText(CreateH2(lSection), UTF8Decode(SDocDeclaration));
  AppendSourceRef(lSection,AClass);
  lMap:=TLinkIdentifierMap.Create(Self);
  try
    if assigned(aClass.AncestorType) then
      lMap.AddLink(aClass.AncestorType);
    if assigned(aClass.Interfaces) then
      for I:=0 to AClass.Interfaces.Count-1 do
        lMap.AddLink(TPasElement(aClass.Interfaces[i]));
    for I:=0 to AClass.Members.Count-1 do
      lMap.AddLink(TPasElement(AClass.Members[i]));
    AppendHighlightedCode(lSection,GetElementCode(aClass,True),'',lMap);
  finally
    lMap.Free;
  end;
  // Inheritance
  lSection:=CreateSection(ContentElement);
  AppendText(CreateH2(lSection), UTF8Decode(SDocInheritance));
  AppendInheritanceTree(lSection,aClass);
  // Description
  DocNode := Engine.FindDocNode(aClass);
  If Assigned(DocNode) and Assigned(DocNode.Descr) then
    begin
    lSection:=CreateSection(ContentElement);
    AppendDescrSection(aClass,lSection,DocNode.Descr,UTF8Decode(SDocDescription));
    end;
  // Properties, methods and events
  AppendMemberListSection(ContentElement,aClass,cmtProperty,True);
  AppendMemberListSection(ContentElement,aClass,cmtMethod,True);
  AppendMemberListSection(ContentElement,aClass,cmtEvent,True);
  AppendMemberListSection(ContentElement,aClass,cmtField,True);
  // The rest
  FinishElementPage(aClass,False);
end;


procedure TNewHTMLWriter.CreateClassMemberList(aParent : THTMLElement; AClass: TPasClassType; DeclaredOnly : Boolean; AFilter: TMemberFilter);

  Function GetMemberHints(aMember : TPasElement) : String;
  var
    S : String;

  begin
    S:='';
    case aMember.Visibility of
      visPrivate,
      visStrictPrivate:
        S:='pv';
      visProtected,
      visStrictProtected:
        S:='pt';
      visPublished:
        S:='pl';
    else
      //
    end;
    if (aMember.ClassType = TPasProperty) and
       ((TPasProperty(aMember).WriteAccessorName) = '') then
      begin
      if S<>'' then
        S:=S+',';
      S:=S+'ro';
      end;
    Result:=S;
  end;

var
  List: TFPList;
  ThisClass: TPasClassType;
  i, j: Integer;
  Member: TPasElement;
  DlEl,DtEl,DdEl: TDOMElement;
  S : String;

begin
  List := TFPList.Create;
  try
    ThisClass := AClass;
    while Assigned(ThisClass) do
    begin
      for i := 0 to ThisClass.Members.Count - 1 do
      begin
        Member := TPasElement(ThisClass.Members[i]);
        if Engine.ShowElement(Member) and AFilter(Member) then
        begin
          j := 0;
          while (j < List.Count) and
            (CompareText(TPasElement(List[j]).Name, Member.Name) < 0) do
            Inc(j);
          List.Insert(j, Member);
        end;
      end;
      if DeclaredOnly or (Assigned(ThisClass.AncestorType) and not (ThisClass.AncestorType.inheritsfrom(TPasClassType))) then
        ThisClass:=Nil
      else
        ThisClass := TPasClassType(ThisClass.AncestorType);
    end;

    for i := 0 to List.Count - 1 do
    begin
      dlEl := CreateEl(aParent,'div','columns list');
      Member := TPasElement(List[i]);
      dTEl:= CreateEl(dlEl,'div','column list is-2');
      AppendHyperlink(dtEl, Member);
      ddEl:= CreateEl(dlEl,'div','column is-10 list');
      S:=GetMemberHints(Member);
      if S<>'' then
        begin
        S:='{'+S+'}';
        AppendNbSp(dtEl,1);
        AppendText(CreateEl(dtEl,'span','cmt'),S);
        end;
      AppendShortDescr(DdEl,Member);
    end;
  finally
    List.Free;
  end;
end;

procedure TNewHTMLWriter.CreateClassSortedSubpage(AClass: TPasClassType; aType: TClassMemberType);

var
  lSection,TitleEl, linkEl: THTMLElement;

begin
  lSection:=CreateSection(ContentElement);
  TitleEl:=CreateH1(lSection);
  AppendText(TitleEl, GetMemberOverviewTitle(aType));
  linkEl:=CreateLink(TitleEl,ResolveLinkWithinPackage(aClass,0));
  AppendText(LinkEl, aClass.Name);
  AppendMemberListSection(lSection,aClass,aType,False);
end;

procedure TNewHTMLWriter.CreateClassInheritedSubpage(AClass: TPasClassType; aType: TClassMemberType);

var
  lSection,TitleEl, linkEl: THTMLElement;
begin
  lSection:=CreateSection(ContentElement);
  TitleEl:=CreateH1(lSection);
  AppendText(TitleEl, GetMemberOverviewTitle(aType));
  linkEl:=CreateLink(TitleEl,ResolveLinkWithinPackage(aClass,0));
  AppendText(LinkEl, aClass.Name);
  AppendMemberListSection(lSection,aClass,aType,False);
end;


procedure TNewHTMLWriter.CreateClassPageBody(AClass: TPasClassType; ASubpageIndex: Integer);

begin
  case ASubpageIndex of
    0:
      CreateClassMainPage(aClass);
    PropertiesByInheritanceSubindex:
      CreateClassInheritedSubpage(aClass,cmtProperty);
    PropertiesByNameSubindex:
      CreateClassSortedSubpage(aClass,cmtProperty);
    MethodsByInheritanceSubindex:
      CreateClassInheritedSubpage(aClass,cmtMethod);
    MethodsByNameSubindex:
      CreateClassSortedSubpage(aClass,cmtMethod);
    EventsByInheritanceSubindex:
      CreateClassInheritedSubpage(aClass,cmtEvent);
    EventsByNameSubindex:
      CreateClassSortedSubpage(aClass,cmtEvent);
    FieldsByNameSubindex:
      CreateClassSortedSubpage(aClass,cmtField);
  end;
end;


function TNewHTMLWriter.GetVarDef(aElement : TPasVariable; aPrefixParent : Boolean) : string;

begin
  Result:=GetElementCode(aElement,False);
end;

procedure TNewHTMLWriter.CreateClassMemberPageBody(AElement: TPasElement);
var
  CodeBlock : TDOMElement;

  procedure CreateVarPage(Element: TPasVariable);

  var
    S : String;

  begin
    S:=GetElementCode(Element,False);
    AppendCodeBlock(ContentElement,S);
  end;

  procedure CreateTypePage(Element: TPasType);
  var
    S : String;

  begin
    S:=GetElementCode(Element,False);
    AppendCodeBlock(ContentElement,S);
  end;

  procedure CreateConstPage(Element: TPasConst);
  var
    S : String;

  begin
    S:=GetElementCode(Element,False);
    AppendCodeBlock(ContentElement,S);
  end;

  procedure CreatePropertyPage(Element: TPasProperty);
  var
    S : String;
  begin
    S:=GetElementCode(Element,True);
    AppendCodeBlock(ContentElement,S);
  end;

var
  s: String;

begin
  AppendTitle(ContentElement,AElement.FullName,AElement.Hints);
  AppendShortDescr(CreatePara(ContentElement), AElement);

  AppendText(CreateH2(ContentElement), SDocDeclaration);
  AppendSourceRef(ContentElement,AElement);

  CodeBlock := AppendCodeBlock(ContentElement);
  if (Assigned(aElement.Parent) and aElement.Parent.InheritsFrom(TPasType)) and (AElement.Visibility<>visDefault) then
    begin
    s:=VisibilityNames[AElement.Visibility];
    AppendKw(CodeBlock, s);
    AppendText(CodeBlock, ' ');
    end;
  if AElement is TPasProperty then
    CreatePropertyPage(TPasProperty(AElement))
  else if AElement is TPasConst then
    CreateConstPage(TPasConst(AElement))
  else if (AElement is TPasVariable) then
    CreateVarPage(TPasVariable(AElement))
  else if AElement is TPasProcedureBase then
    AppendProcDecl(CodeBlock,TPasProcedureBase(AElement))
  else if AElement is TPasType then
    CreateTypePage(TPasType(AElement))
  else
    AppendText(CreateWarning(ContentElement), '<' + AElement.ClassName + '>');

  FinishElementPage(AElement);
end;

procedure TNewHTMLWriter.CreateVarPageBody(AVar: TPasVariable);
var
  lSection,CodeEl: TDOMElement;

begin
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,AVar.FullName,AVar.Hints);
  AppendShortDescr(CreatePara(lSection), AVar);
  AppendText(CreateH2(lSection), SDocDeclaration);
  AppendSourceRef(lSection,AVar);
  CodeEl := AppendCodeBlock(lSection);
  AppendPasSHFragment(CodeEl, GetElementCode(aVar,False),0);
  FinishElementPage(AVar);
end;

procedure TNewHTMLWriter.CreateProcPageBody(AProc: TPasProcedureBase);

var
  lSection,CodeEl: THTMLElement;

begin
  lSection:=CreateSection(ContentElement);
  AppendTitle(lSection,AProc.Name,AProc.Hints);
  AppendShortDescr(CreatePara(lSection), AProc);
  lSection:=CreateSection(ContentElement);
  AppendText(CreateH2(lSection), SDocDeclaration);
  AppendSourceRef(lSection,AProc);
  CodeEl := CreateCode(lSection);
  AppendProcDecl(CodeEl, AProc);
  if aProc is TPasProcedure then
    if Assigned(TPasProcedure(aProc).ProcType) then
      AppendProcArgsSection(lSection, TPasProcedure(aProc).ProcType);
  FinishElementPage(AProc);
end;

function TNewHTMLWriter.InterPretOption ( const Cmd, Arg: String ) : boolean;

  Function ReadFile(aFileName : string) : TstringStream;

  begin
    aFileName:= SetDirSeparators(aFileName);
    try
      if copy(aFileName,1,1)<>'@' then
        Result:=TStringStream.Create(aFileName)
      else
        begin
        Delete(aFileName,1,1);
        Result:=TStringStream.Create('');
        Result.LoadFromFile(aFileName);
        Result.Position:=0;
        end;
    except
      Result.Free;
      Raise;
    end;
  end;

begin
  Result:=True;
  if Cmd = '--html-search' then
    SearchPage := Arg
  else if Cmd = '--footer' then
    FFooterHTML := ReadFile(Arg)
  else if Cmd = '--header' then
    FHeaderHTML := ReadFile(Arg)
  else if Cmd = '--navigator' then
    FNavigatorHTML := ReadFile(Arg)
  else if Cmd = '--charset' then
    CharSet := Arg
  else if Cmd = '--index-colcount' then
    IndexColCount := StrToIntDef(Arg,IndexColCount)
  else if Cmd = '--image-url' then
    BaseImageURL  := Arg
  else if Cmd = '--css-file' then
    FCSSFile := arg
  else if Cmd = '--footer-date' then
    begin
    FIncludeDateInFooter:=True;
    FDateFormat:=Arg;
    end
  else if Cmd = '--disable-menu-brackets' then
    FUseMenuBrackets:=False
  else
    Result:=inherited InterPretOption(Cmd, Arg);
end;


class procedure TNewHTMLWriter.Usage(List: TStrings);
begin
  List.add('--header=file');
  List.Add(SHTMLUsageHeader);
  List.add('--footer=file');
  List.Add(SHTMLUsageFooter);
  List.add('--navigator=file');
  List.Add(SHTMLUsageNavigator);
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
  List.Add('--disable-menu-brackets');
  List.Add(SHTMLDisableMenuBrackets);
  inherited Usage(List);
end;

class procedure TNewHTMLWriter.SplitImport(var AFilename, ALinkPrefix: String);
var
  i: integer;
begin
  i := Pos(',', AFilename);
  if i > 0 then
    begin  //split into filename and prefix
    ALinkPrefix := Copy(AFilename,i+1,Length(AFilename));
    SetLength(AFilename, i-1);
    end
  else if ALinkPrefix = '' then
    begin  //synthesize outdir\pgk.xct, ..\pkg
    ALinkPrefix := '../' + ChangeFileExt(ExtractFileName(AFilename), '');
    AFilename := ChangeFileExt(AFilename, '.xct');
    end;
end;

class function TNewHTMLWriter.FileNameExtension: String;
begin
  result:='';
end;

// private methods


procedure TNewHTMLWriter.SetOnTest(const AValue: TNotifyEvent);
begin
  if FOnTest=AValue then exit;
    FOnTest:=AValue;
end;


initialization
  // Do not localize.
  RegisterWriter(TNewHTMLWriter,'newhtml','HTML output using fpdocs.css stylesheet.');

finalization
  UnRegisterWriter('newhtml');

end.
