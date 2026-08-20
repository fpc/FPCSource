{
    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2026 by Michael Van Canneyt

    * HTML generator producing the page layout of the mkdocs material theme

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$mode objfpc}
{$H+}

unit dw_material;

interface

uses Classes, DOM, DOM_HTML, dGlobals, PasTree, dWriter, dw_basehtml, dw_newhtml;

Const
  MaterialAssetDir       = 'assets';
  MaterialCSSFile        = 'fpdoc-material.css';
  MaterialJSFile         = 'fpdoc-material.js';
  MaterialNavFile        = 'nav.js';
  MaterialSearchFile     = 'search.js';
  MaterialSearchLimit    = 30;
  MaterialDescrMaxLength = 160;

Type
  TMaterialColorScheme = (mcsAuto,mcsLight,mcsDark);

  { TMaterialNavUnit }

  TMaterialNavUnit = Class(TObject)
  private
    FDescription: String;
    FName: String;
    FPages: TStringList;
  Public
    constructor Create(const aName : String);
    destructor Destroy; override;
    // Register a page of this unit. Pages are kept in the order they appear.
    procedure AddPage(aSubPageIndex : Integer; const aFileName : String);
    // Name of the unit, as it appears in the sources
    property Name : String Read FName;
    // Short description of the unit, used as a tooltip
    property Description : String Read FDescription Write FDescription;
    // Pages of the unit: subpage index = file name
    property Pages : TStringList Read FPages;
  end;

  { TMaterialWriter }

  TMaterialWriter = Class(TNewHTMLWriter)
  Private
    FAccentColor: String;
    FColorScheme: TMaterialColorScheme;
    FDumpAssetDir: String;
    FFavIcon: String;
    FJSFile: String;
    FLogo: String;
    FPrimaryColor: String;
    FSearchLimit: Integer;
    FSiteTitle: String;
    FUseNavScript: Boolean;
    FUseSearch: Boolean;
    FUseTOC: Boolean;
    // State of the page being written
    FAnchors: TStringList;
    FListHeaders: TStringList;
    FArticleElement: THTMLElement;
    FBodyElement: THTMLElement;
    FPageFileName: String;
    FTOCElement: THTMLElement;
    FTOCSideBar: THTMLElement;
    // Data collected for the navigation tree and the search index
    FNavPackagePages: TStringList;
    FNavUnits: TStringList;
    FSearchEntries: TStringList;
    function GetNavUnit(aModule: TPasModule): TMaterialNavUnit;
  Protected
    // Text of a DOM node and everything below it, whitespace collapsed
    function NodeText(aNode: TDOMNode): String;
    // Short description of an element as plain text
    function GetShortDescrText(aElement: TPasElement): String; virtual;
    // Kind of element, as shown in the search results
    function GetElementKind(aElement: TPasElement): String; virtual;
    // Unique anchor name for a title within the current page
    function MakeAnchor(const aTitle: String): String; virtual;
    // Path of an asset file, relative to the page being written
    function GetAssetPath(const aFileName: String): String; virtual;
    // Title shown in the header and as the first step of the breadcrumb
    function GetPackageTitle: String; override;
    // The header shows the full title of the package, not just its name
    function GetBreadcrumbRootTitle: String; override;
    // The breadcrumb sits in the header, not above the contents of the page
    function CreateBreadcrumb(aParent: THTMLElement): THTMLElement; override;
    function CreateBreadcrumbItem(aList: THTMLElement; const aTitle, aURL: String): THTMLElement; override;
    procedure AppendPageBreadcrumb(aElement: TPasElement; aSubPageIndex: Integer); override;

    // Write the style sheet, the script and the images to the assets directory
    procedure CreateCSSFile; override;
    // Write one asset file, from a file on disk or from the built-in data
    procedure WriteAsset(const aFileName: String; aData: PByte; aSize: Integer; const aSourceFile: String;
                         const aAppend: String = ''); virtual;
    // Copy a file to the assets directory, keeping its name
    procedure CopyAsset(const aSourceFile: String); virtual;
    // Style sheet lines that set the colours given on the command line
    function GetPaletteCSS: String; virtual;
    // Collect the navigation tree and the search entries from the page list
    procedure CollectPageData; virtual;
    // Write the navigation tree as a script file
    procedure WriteNavigationData; virtual;
    // Write the identifier list used by the search box
    procedure WriteSearchData; virtual;

    // Fill the head element of the page
    procedure AppendHead(aHead: TDOMElement; aElement: TPasElement); virtual;
    // The bar at the top of the page: breadcrumb, search box and colour scheme
    procedure AppendHeaderBar(aParent: THTMLElement; aElement: TPasElement; aSubPageIndex: Integer); virtual;
    // The search box in the header
    procedure AppendSearchBox(aParent: THTMLElement); virtual;
    // The navigation in the left sidebar, used when there is no javascript
    procedure AppendPrimaryNav(aParent: THTMLElement); virtual;
    procedure AppendMenuBar(ASubpageIndex: Integer); override;
    procedure AppendTopicMenuBar(Topic: TTopicElement); override;
    procedure AppendFooter; override;
    // The scripts and the back to top button at the end of the page
    procedure AppendScripts(aParent: THTMLElement); virtual;
    // The copy button of a code block
    procedure AppendClipboardButton(aParent: TDOMNode); virtual;
    // Give every title an anchor and list the titles in the right sidebar
    procedure BuildTableOfContents; virtual;
    // Add the name of the package to the title of the page
    procedure FinishPageTitle(aElement: TPasElement); virtual;

    // Titles of the columns of the next overview table
    procedure SetListHeaders(const aTitles: array of String); virtual;
    // The overview table being filled, a new one is started when needed
    function GetListTable(aParent: THTMLElement): THTMLElement; virtual;
    // Overview lists are tables with a header row
    function CreateListColumns(aParent: THTMLElement): THTMLElement; override;
    function CreateListColumn1(aParent: THTMLElement): THTMLElement; override;
    function CreateListColumn2(aParent: THTMLElement): THTMLElement; override;

    // Pages with an overview table, they only set the titles of the columns
    procedure CreatePackagePageBody; override;
    procedure CreateModuleMainPage(aModule: TPasModule); override;
    procedure CreateModuleSimpleSubpage(aModule: TPasModule; ASubpageIndex: Integer; const ATitle: DOMString;
                                        AList: TFPList); override;
    procedure CreateModuleResStringsPage(aModule: TPasModule); override;
    procedure CreateClassMemberList(aParent: THTMLElement; AClass: TPasClassType; DeclaredOnly: Boolean;
                                    AFilter: TMemberFilter); override;
    procedure CreateTopicLinks(aParent: THTMLElement; Node: TDocNode; PasElement: TPasElement); override;
    procedure AppendProcArgsSection(Parent: THTMLElement; Element: TPasProcedureType; SkipResult: Boolean = False); override;
    procedure AppendSeeAlsoSection(AElement: TPasElement; aParent: TDOMElement; DocNode: TDocNode); override;
    procedure AppendEnumTypeDecl(aType: TPasEnumType); override;
    procedure AppendSetTypeDecl(aType: TPasSetType); override;

    // Content elements in material style
    function CreateH1(Parent: TDOMNode): THTMLElement; override;
    function CreateH2(Parent: TDOMNode): THTMLElement; override;
    function CreateH3(Parent: TDOMNode): THTMLElement; override;
    function CreateSection(aParent: THTMLElement): THTMLElement; override;
    function AppendCodeBlock(aParent: TDOMNode; const aLanguage: String = ''): THTMLElement; override;
    procedure AppendInheritanceTree(aParent: THTMLELement; aClass: TPasClassType); override;
    procedure CreateIndexPage(aParent: THTMLElement; L: TStringList); override;
    procedure DescrBeginRemark; override;
    procedure DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String); override;

    // Documentation process
    procedure WriteDocPage(const aFileName: String; aElement: TPasElement; aSubPageIndex: Integer); override;
    procedure DoWriteDocumentation; override;
  Public
    constructor Create(APackage: TPasPackage; AEngine: TFPDocEngine); override;
    destructor Destroy; override;
    // Create the document for one page: frame, contents and scripts
    function CreateHTMLPage(AElement: TPasElement; ASubpageIndex: Integer): TXMLDocument; override;
    // Handle one command line option of this backend
    function InterPretOption(const Cmd, Arg: String): boolean; override;
    // List the command line options of this backend
    class procedure Usage(List: TStrings); override;
    // Colour scheme used when the reader did not choose one
    property ColorScheme : TMaterialColorScheme Read FColorScheme Write FColorScheme;
    // Primary colour of the header, a material colour name or a CSS colour
    property PrimaryColor : String Read FPrimaryColor Write FPrimaryColor;
    // Colour of links and other accents
    property AccentColor : String Read FAccentColor Write FAccentColor;
    // Title shown in the header and in the browser title bar
    property SiteTitle : String Read FSiteTitle Write FSiteTitle;
    // Image file shown left of the title
    property Logo : String Read FLogo Write FLogo;
    // Image file used as browser icon
    property FavIcon : String Read FFavIcon Write FFavIcon;
    // Replacement for the built-in script file
    property JSFile : String Read FJSFile Write FJSFile;
    // Write the identifier index used by the search box
    property UseSearch : Boolean Read FUseSearch Write FUseSearch;
    // Maximum number of search results shown at once
    property SearchLimit : Integer Read FSearchLimit Write FSearchLimit;
    // Write the navigation tree as a script file
    property UseNavScript : Boolean Read FUseNavScript Write FUseNavScript;
    // Show the page contents in the right sidebar
    property UseTOC : Boolean Read FUseTOC Write FUseTOC;
  end;

implementation

uses SysUtils, HTMWrite, fpdocstrs, fpdocclasstree;

{$i materialcss.inc}
{$i materialjs.inc}

Const
  // Material colour names and their values
  MaterialColorNames : Array[0..20,0..1] of String = (
    ('red','#ef5350'),
    ('pink','#e91e63'),
    ('purple','#ab47bc'),
    ('deep-purple','#7e57c2'),
    ('indigo','#4051b5'),
    ('blue','#2196f3'),
    ('light-blue','#03a9f4'),
    ('cyan','#00bcd4'),
    ('teal','#009688'),
    ('green','#4caf50'),
    ('light-green','#7cb342'),
    ('lime','#c0ca33'),
    ('yellow','#f9a825'),
    ('amber','#ffb300'),
    ('orange','#fb8c00'),
    ('deep-orange','#ff7043'),
    ('brown','#795548'),
    ('grey','#757575'),
    ('blue-grey','#546e7a'),
    ('black','#000000'),
    ('white','#ffffff')
  );

  SchemeNames : Array[TMaterialColorScheme] of String = ('auto','light','dark');

  // Paragraph sign, used as anchor next to a title
  PilcrowUTF8 = #$C2#$B6;

Function MaterialColorValue(const aName : String) : String;

var
  I : Integer;

begin
  Result:='';
  if aName='' then
    exit;
  if aName[1]='#' then
    Exit(aName);
  For I:=Low(MaterialColorNames) to High(MaterialColorNames) do
    if SameText(MaterialColorNames[I,0],aName) then
      Exit(MaterialColorNames[I,1]);
end;

// Quoted javascript string literal
Function JSString(const S : String) : String;

var
  I : Integer;
  C : Char;

begin
  Result:='"';
  For I:=1 to Length(S) do
    begin
    C:=S[I];
    case C of
      '"' : Result:=Result+'\"';
      '\' : Result:=Result+'\\';
      #8  : Result:=Result+'\b';
      #9  : Result:=Result+'\t';
      #10 : Result:=Result+'\n';
      #12 : Result:=Result+'\f';
      #13 : Result:=Result+'\r';
    else
      // A literal < could end the script element too early
      if (C<#32) or (C='<') then
        Result:=Result+'\u00'+HexStr(Ord(C),2)
      else
        Result:=Result+C;
    end;
    end;
  Result:=Result+'"';
end;

{ ---------------------------------------------------------------------
  TMaterialNavUnit
  ---------------------------------------------------------------------}

constructor TMaterialNavUnit.Create(const aName: String);

begin
  FName:=aName;
  FPages:=TStringList.Create;
end;


destructor TMaterialNavUnit.Destroy;

begin
  FreeAndNil(FPages);
  inherited Destroy;
end;


procedure TMaterialNavUnit.AddPage(aSubPageIndex: Integer; const aFileName: String);

begin
  FPages.Add(IntToStr(aSubPageIndex)+'='+aFileName);
end;


{ ---------------------------------------------------------------------
  TMaterialWriter
  ---------------------------------------------------------------------}

constructor TMaterialWriter.Create(APackage: TPasPackage; AEngine: TFPDocEngine);

begin
  inherited Create(APackage, AEngine);
  CreateSideMenu:=False;
  CSSFile:='';
  FColorScheme:=mcsAuto;
  FSearchLimit:=MaterialSearchLimit;
  FUseNavScript:=True;
  FUseSearch:=True;
  FUseTOC:=True;
  FAnchors:=TStringList.Create;
  FAnchors.Sorted:=True;
  FListHeaders:=TStringList.Create;
  FNavUnits:=TStringList.Create;
  FNavUnits.OwnsObjects:=True;
  FNavPackagePages:=TStringList.Create;
  FSearchEntries:=TStringList.Create;
end;


destructor TMaterialWriter.Destroy;

begin
  FreeAndNil(FSearchEntries);
  FreeAndNil(FNavPackagePages);
  FreeAndNil(FNavUnits);
  FreeAndNil(FListHeaders);
  FreeAndNil(FAnchors);
  inherited Destroy;
end;


function TMaterialWriter.GetPackageTitle: String;

begin
  Result:=FSiteTitle;
  if Result='' then
    Result:=inherited GetPackageTitle;
end;


function TMaterialWriter.GetBreadcrumbRootTitle: String;

begin
  Result:=GetPackageTitle;
end;


function TMaterialWriter.GetNavUnit(aModule: TPasModule): TMaterialNavUnit;

var
  lIndex : Integer;
  lName : String;

begin
  lName:=LowerCase(aModule.Name);
  lIndex:=FNavUnits.IndexOf(lName);
  if lIndex=-1 then
    begin
    Result:=TMaterialNavUnit.Create(aModule.Name);
    Result.Description:=GetShortDescrText(aModule);
    FNavUnits.AddObject(lName,Result);
    end
  else
    Result:=TMaterialNavUnit(FNavUnits.Objects[lIndex]);
end;


function TMaterialWriter.NodeText(aNode: TDOMNode): String;

var
  lChild : TDOMNode;

begin
  Result:='';
  if aNode=Nil then
    exit;
  if aNode.NodeType in [TEXT_NODE,CDATA_SECTION_NODE] then
    Result:=UTF8Encode(aNode.NodeValue)
  else
    begin
    lChild:=aNode.FirstChild;
    While lChild<>Nil do
      begin
      Result:=Result+NodeText(lChild);
      lChild:=lChild.NextSibling;
      end;
    end;
end;


function TMaterialWriter.GetShortDescrText(aElement: TPasElement): String;

var
  lDescr : TDOMElement;
  lText : String;
  I : Integer;
  lLast : Boolean;

begin
  Result:='';
  lDescr:=Engine.FindShortDescr(aElement);
  if lDescr=Nil then
    exit;
  lText:=NodeText(lDescr);
  // Collapse all whitespace, so the text fits on one line
  lLast:=True;
  For I:=1 to Length(lText) do
    if lText[I] in [' ',#9,#10,#13] then
      begin
      if not lLast then
        Result:=Result+' ';
      lLast:=True;
      end
    else
      begin
      Result:=Result+lText[I];
      lLast:=False;
      end;
  Result:=Trim(Result);
  if Length(Result)>MaterialDescrMaxLength then
    Result:=Copy(Result,1,MaterialDescrMaxLength-3)+'...';
end;


function TMaterialWriter.GetElementKind(aElement: TPasElement): String;

begin
  if aElement is TPasModule then
    Result:='unit'
  else if aElement is TTopicElement then
    Result:='topic'
  else if aElement is TPasProperty then
    begin
    if Copy(aElement.Name,1,2)='On' then
      Result:='event'
    else
      Result:='property';
    end
  else if aElement is TPasConst then
    Result:='const'
  else if aElement is TPasResString then
    Result:='string'
  else if aElement is TPasClassType then
    begin
    case TPasClassType(aElement).ObjKind of
      okInterface : Result:='interface';
      okObject : Result:='object';
      okRecordHelper,
      okClassHelper,
      okTypeHelper : Result:='helper';
    else
      Result:='class';
    end;
    end
  else if aElement is TPasRecordType then
    Result:='record'
  else if aElement is TPasEnumType then
    Result:='enum'
  else if aElement is TPasProcedureBase then
    begin
    if aElement.Parent is TPasType then
      Result:='method'
    else if aElement is TPasFunction then
      Result:='function'
    else
      Result:='procedure';
    end
  else if aElement is TPasType then
    Result:='type'
  else if aElement is TPasVariable then
    begin
    if aElement.Parent is TPasType then
      Result:='field'
    else
      Result:='var';
    end
  else
    Result:='identifier';
end;


function TMaterialWriter.MakeAnchor(const aTitle: String): String;

var
  I,lCount : Integer;
  C : Char;
  lBase : String;

begin
  Result:='';
  For I:=1 to Length(aTitle) do
    begin
    C:=aTitle[I];
    if C in ['A'..'Z'] then
      C:=Chr(Ord(C)+(Ord('a')-Ord('A')));
    if C in ['a'..'z','0'..'9'] then
      Result:=Result+C
    else if (Result<>'') and (Result[Length(Result)]<>'-') then
      Result:=Result+'-';
    end;
  While (Result<>'') and (Result[Length(Result)]='-') do
    Delete(Result,Length(Result),1);
  if Result='' then
    Result:='section';
  lBase:=Result;
  lCount:=1;
  While FAnchors.IndexOf(Result)<>-1 do
    begin
    Inc(lCount);
    Result:=lBase+'-'+IntToStr(lCount);
    end;
  FAnchors.Add(Result);
end;


function TMaterialWriter.GetAssetPath(const aFileName: String): String;

begin
  Result:=BaseDirectory+MaterialAssetDir+'/'+aFileName;
end;


{ ---------------------------------------------------------------------
  Asset files
  ---------------------------------------------------------------------}

function TMaterialWriter.GetPaletteCSS: String;

var
  lPrimary,lAccent : String;

begin
  Result:='';
  lPrimary:=MaterialColorValue(FPrimaryColor);
  if (lPrimary='') and (FPrimaryColor<>'') then
    DoLog('Unknown colour name "%s", using the default colour',[FPrimaryColor]);
  lAccent:=MaterialColorValue(FAccentColor);
  if (lAccent='') and (FAccentColor<>'') then
    DoLog('Unknown colour name "%s", using the default colour',[FAccentColor]);
  if (lPrimary='') and (lAccent='') then
    exit;
  Result:=sLineBreak+'/* Colours set on the command line */'+sLineBreak+':root {'+sLineBreak;
  if lPrimary<>'' then
    begin
    Result:=Result+'  --md-primary-fg-color: '+lPrimary+';'+sLineBreak;
    // Dark text on a light header
    if SameText(FPrimaryColor,'white') or SameText(lPrimary,'#ffffff') then
      Result:=Result+'  --md-primary-bg-color: rgba(0,0,0,.87);'+sLineBreak;
    end;
  if lAccent<>'' then
    Result:=Result+'  --md-accent-fg-color: '+lAccent+';'+sLineBreak;
  Result:=Result+'}'+sLineBreak;
end;


procedure TMaterialWriter.WriteAsset(const aFileName: String; aData: PByte; aSize: Integer; const aSourceFile: String;
  const aAppend: String);

var
  lStream : TMemoryStream;
  lDir : String;

begin
  lDir:=IncludeTrailingPathDelimiter(GetFileBaseDir(Engine.Output)+MaterialAssetDir);
  if not ForceDirectories(lDir) then
    FPDocError(Format('Could not create directory "%s"',[lDir]));
  lStream:=TMemoryStream.Create;
  try
    if aSourceFile<>'' then
      begin
      if not FileExists(aSourceFile) then
        FPDocError(Format('Can''t find file "%s"',[aSourceFile]));
      lStream.LoadFromFile(aSourceFile);
      end
    else
      lStream.WriteBuffer(aData^,aSize);
    if aAppend<>'' then
      begin
      lStream.Position:=lStream.Size;
      lStream.WriteBuffer(aAppend[1],Length(aAppend));
      end;
    lStream.Position:=0;
    lStream.SaveToFile(lDir+aFileName);
  finally
    lStream.Free;
  end;
end;


procedure TMaterialWriter.CopyAsset(const aSourceFile: String);

begin
  WriteAsset(ExtractFileName(aSourceFile),Nil,0,aSourceFile);
end;


procedure TMaterialWriter.CreateCSSFile;

begin
  if FDumpAssetDir<>'' then
    begin
    ForceDirectories(FDumpAssetDir);
    with TMemoryStream.Create do
      try
        WriteBuffer(DefaultMaterialCSS,SizeOf(DefaultMaterialCSS));
        SaveToFile(IncludeTrailingPathDelimiter(FDumpAssetDir)+MaterialCSSFile);
      finally
        Free;
      end;
    with TMemoryStream.Create do
      try
        WriteBuffer(DefaultMaterialJS,SizeOf(DefaultMaterialJS));
        SaveToFile(IncludeTrailingPathDelimiter(FDumpAssetDir)+MaterialJSFile);
      finally
        Free;
      end;
    DoLog('Wrote style sheet and script to directory "%s"',[FDumpAssetDir]);
    end;
  WriteAsset(MaterialCSSFile,@DefaultMaterialCSS,SizeOf(DefaultMaterialCSS),CSSFile,GetPaletteCSS);
  WriteAsset(MaterialJSFile,@DefaultMaterialJS,SizeOf(DefaultMaterialJS),FJSFile);
  if FLogo<>'' then
    CopyAsset(FLogo);
  if FFavIcon<>'' then
    CopyAsset(FFavIcon);
end;


procedure TMaterialWriter.CollectPageData;

var
  I : Integer;
  lInfo : TPageInfo;
  lFile,lName,lUnit : String;
  lElement : TPasElement;
  lModule : TPasModule;

begin
  For I:=0 to PageInfos.Count-1 do
    begin
    lInfo:=TPageInfo(PageInfos[I]);
    lElement:=lInfo.Element;
    lFile:=FixHtmlPath(Allocator.GetFilename(lElement,lInfo.SubpageIndex));
    if lElement is TPasPackage then
      FNavPackagePages.Add(IntToStr(lInfo.SubpageIndex)+'='+lFile)
    else if lElement is TPasModule then
      GetNavUnit(TPasModule(lElement)).AddPage(lInfo.SubpageIndex,lFile);
    if FUseSearch and (lInfo.SubpageIndex=IdentifierIndex) and not (lElement is TPasPackage) then
      begin
      if lElement is TPasModule then
        lName:=lElement.Name
      else
        lName:=lElement.FullName;
      lModule:=lElement.GetModule;
      if lModule<>Nil then
        lUnit:=lModule.Name
      else
        lUnit:='';
      FSearchEntries.Add('['+JSString(lName)+','+JSString(lUnit)+','+JSString(GetElementKind(lElement))+','
                         +JSString(lFile)+','+JSString(GetShortDescrText(lElement))+']');
      end;
    end;
end;


procedure TMaterialWriter.WriteNavigationData;

var
  lNav : TStringList;
  lUnit : TMaterialNavUnit;
  lLine,lPages : String;
  I,J,lIndex : Integer;

begin
  lNav:=TStringList.Create;
  try
    lNav.Add('/* Navigation tree of the documentation, written by fpdoc */');
    lNav.Add('window.fpdocNav={');
    lNav.Add('"title":'+JSString(GetPackageTitle)+',');
    lNav.Add('"pkgTitle":'+JSString(SDocPackageLinkTitle)+',');
    lNav.Add('"unitsTitle":'+JSString(SDocUnits)+',');
    // Labels of the unit subpages
    lLine:='';
    For I:=IdentifierIndex to InterfaceHierarchySubIndex do
      if GetSubPageTitle(I)<>'' then
        begin
        if lLine<>'' then
          lLine:=lLine+',';
        lLine:=lLine+JSString(IntToStr(I))+':'+JSString(GetSubPageTitle(I));
        end;
    lNav.Add('"labels":{'+lLine+'},');
    // Pages of the package itself
    FNavPackagePages.Sort;
    lLine:='';
    For I:=0 to FNavPackagePages.Count-1 do
      begin
      lIndex:=StrToIntDef(FNavPackagePages.Names[I],-1);
      if GetSubPageTitle(lIndex)='' then
        continue;
      if lLine<>'' then
        lLine:=lLine+',';
      lLine:=lLine+'['+JSString(GetSubPageTitle(lIndex))+','+JSString(FNavPackagePages.ValueFromIndex[I])+']';
      end;
    lNav.Add('"pkg":['+lLine+'],');
    // Pages are shown in the order of their subpage index
    // Units with their pages
    lNav.Add('"units":[');
    FNavUnits.Sort;
    For I:=0 to FNavUnits.Count-1 do
      begin
      lUnit:=TMaterialNavUnit(FNavUnits.Objects[I]);
      lUnit.Pages.Sort;
      lPages:='';
      For J:=0 to lUnit.Pages.Count-1 do
        begin
        if lPages<>'' then
          lPages:=lPages+',';
        lPages:=lPages+'['+lUnit.Pages.Names[J]+','+JSString(lUnit.Pages.ValueFromIndex[J])+']';
        end;
      lLine:='['+JSString(lUnit.Name)+','+JSString(lUnit.Description)+',['+lPages+']]';
      if I<FNavUnits.Count-1 then
        lLine:=lLine+',';
      lNav.Add(lLine);
      end;
    lNav.Add(']};');
    lNav.SaveToFile(IncludeTrailingPathDelimiter(GetFileBaseDir(Engine.Output)+MaterialAssetDir)+MaterialNavFile);
  finally
    lNav.Free;
  end;
end;


procedure TMaterialWriter.WriteSearchData;

var
  lSearch : TStringList;
  I : Integer;

begin
  lSearch:=TStringList.Create;
  try
    lSearch.Add('/* Identifier index used by the search box, written by fpdoc */');
    lSearch.Add('window.fpdocIndex=[');
    For I:=0 to FSearchEntries.Count-1 do
      if I<FSearchEntries.Count-1 then
        lSearch.Add(FSearchEntries[I]+',')
      else
        lSearch.Add(FSearchEntries[I]);
    lSearch.Add('];');
    lSearch.SaveToFile(IncludeTrailingPathDelimiter(GetFileBaseDir(Engine.Output)+MaterialAssetDir)+MaterialSearchFile);
    DoLog('Wrote %d identifiers to the search index',[FSearchEntries.Count]);
  finally
    lSearch.Free;
  end;
end;


{ ---------------------------------------------------------------------
  Page frame
  ---------------------------------------------------------------------}

procedure TMaterialWriter.AppendHead(aHead: TDOMElement; aElement: TPasElement);

Const
  SchemeScript =
    '(function(){var s=null;'+
    'try{s=localStorage.getItem("fpdoc-scheme");}catch(e){}'+
    'if(!s){s=%s;}'+
    'if(s==="auto"){s="light";'+
    'if(window.matchMedia){if(window.matchMedia("(prefers-color-scheme: dark)").matches){s="dark";}}}'+
    'document.documentElement.setAttribute("data-fpdoc-scheme",s);})();';

var
  lEl : TDOMElement;

begin
  lEl:=CreateEl(aHead,'meta');
  lEl['charset']:='utf-8';
  lEl:=CreateEl(aHead,'meta');
  lEl['name']:='viewport';
  lEl['content']:='width=device-width, initial-scale=1';
  lEl:=CreateEl(aHead,'meta');
  lEl['name']:='generator';
  lEl['content']:='fpdoc';
  TitleElement:=CreateEl(aHead,'title');
  lEl:=CreateEl(aHead,'link');
  lEl['rel']:='stylesheet';
  lEl['href']:=UTF8Decode(GetAssetPath(MaterialCSSFile));
  if FFavIcon<>'' then
    begin
    lEl:=CreateEl(aHead,'link');
    lEl['rel']:='icon';
    lEl['href']:=UTF8Decode(GetAssetPath(ExtractFileName(FFavIcon)));
    end;
  // Set the colour scheme before the page is painted
  lEl:=CreateEl(aHead,'script');
  AppendText(lEl,Format(SchemeScript,[JSString(SchemeNames[FColorScheme])]));
end;


procedure TMaterialWriter.AppendSearchBox(aParent: THTMLElement);

var
  lSearch,lForm,lOutput,lEl : THTMLElement;

begin
  lSearch:=CreateEl(aParent,'div','md-search');
  lForm:=CreateEl(lSearch,'div','md-search__form');
  CreateEl(lForm,'span','md-search__icon');
  lEl:=CreateEl(lForm,'input','md-search__input');
  lEl['type']:='text';
  lEl['id']:='fpdoc-search';
  lEl['placeholder']:=UTF8Decode(SDocSearch);
  lEl['aria-label']:=UTF8Decode(SDocSearch);
  lEl['autocomplete']:='off';
  lEl['spellcheck']:='false';
  lOutput:=CreateEl(lSearch,'div','md-search__output');
  lOutput['id']:='fpdoc-search-output';
  lEl:=CreateEl(lOutput,'div','md-search__meta');
  lEl['id']:='fpdoc-search-meta';
  lEl:=CreateEl(lOutput,'ol','md-search-result__list');
  lEl['id']:='fpdoc-search-result';
end;


function TMaterialWriter.CreateBreadcrumb(aParent: THTMLElement): THTMLElement;

var
  lNav : THTMLElement;

begin
  lNav:=CreateEl(aParent,'nav','md-header__title');
  lNav['aria-label']:='Breadcrumb';
  Result:=CreateEl(lNav,'ol','md-breadcrumb');
end;


function TMaterialWriter.CreateBreadcrumbItem(aList: THTMLElement; const aTitle, aURL: String): THTMLElement;

begin
  Result:=CreateEl(aList,'li','md-breadcrumb__item');
  if aURL='' then
    begin
    Result['class']:='md-breadcrumb__item md-breadcrumb__item--current';
    AppendText(Result,UTF8Decode(aTitle));
    end
  else
    AppendText(CreateLink(Result,FixHtmlPath(aURL)),UTF8Decode(aTitle));
end;


procedure TMaterialWriter.AppendPageBreadcrumb(aElement: TPasElement; aSubPageIndex: Integer);

begin
  // The header of the page already shows the breadcrumb
end;


procedure TMaterialWriter.AppendHeaderBar(aParent: THTMLElement; aElement: TPasElement; aSubPageIndex: Integer);

var
  lHeader,lInner,lEl : THTMLElement;

begin
  lHeader:=CreateEl(aParent,'header','md-header');
  lInner:=CreateEl(lHeader,'div','md-header__inner');
  lEl:=CreateEl(lInner,'label','md-header__button md-header__button--drawer');
  lEl['for']:='__drawer';
  lEl['title']:=UTF8Decode(SDocUnits);
  lEl['aria-label']:=UTF8Decode(SDocUnits);
  if FLogo<>'' then
    begin
    lEl:=CreateEl(lInner,'img','md-header__logo');
    lEl['src']:=UTF8Decode(GetAssetPath(ExtractFileName(FLogo)));
    lEl['alt']:='';
    end;
  AppendBreadcrumb(lInner,aElement,aSubPageIndex);
  if FUseSearch then
    AppendSearchBox(lInner);
  lEl:=CreateEl(lInner,'button','md-header__button md-header__button--scheme');
  lEl['id']:='fpdoc-scheme';
  lEl['type']:='button';
  AppendFragment(lInner,NavigatorHTML);
end;


procedure TMaterialWriter.AppendPrimaryNav(aParent: THTMLElement);

  function AddItem(aList : THTMLElement; const aURL,aTitle : String) : THTMLElement;

  var
    lItem : THTMLElement;

  begin
    lItem:=CreateEl(aList,'li','md-nav__item');
    Result:=CreateLink(lItem,FixHtmlPath(aURL));
    Result['class']:='md-nav__link';
    AppendText(Result,UTF8Decode(aTitle));
  end;

  procedure AddModuleItem(aList : THTMLElement; aSubPageIndex : Integer);

  begin
    AddItem(aList,ResolveLinkWithinPackage(Module,aSubPageIndex),GetSubPageTitle(aSubPageIndex));
  end;

var
  lNav,lList,lItem,lSub,lEl : THTMLElement;
  I : Integer;
  lHasClasses : Boolean;

begin
  lNav:=CreateEl(aParent,'nav','md-nav md-nav--primary');
  lNav['id']:='fpdoc-nav';
  lNav['aria-label']:='Navigation';
  AppendText(CreateEl(lNav,'label','md-nav__title'),UTF8Decode(SDocPackageLinkTitle));
  lList:=CreateEl(lNav,'ul','md-nav__list');
  AddItem(lList,ResolveLinkWithinPackage(Package,IdentifierIndex),SDocOverview);
  AddItem(lList,ResolveLinkWithinPackage(Package,IndexSubIndex),SDocIdentifierIndex);
  // The class hierarchy page only exists if there are classes
  lHasClasses:=False;
  I:=0;
  While (I<Package.Modules.Count) and not lHasClasses do
    begin
    lHasClasses:=ModuleHasClasses(TPasModule(Package.Modules[I]));
    Inc(I);
    end;
  if lHasClasses then
    AddItem(lList,ResolveLinkWithinPackage(Package,ClassHierarchySubIndex),SDocPackageClassHierarchy);
  if Module=Nil then
    exit;
  AppendText(CreateEl(lNav,'label','md-nav__title'),UTF8Decode(SDocUnits));
  lList:=CreateEl(lNav,'ul','md-nav__list');
  // Pages of the unit this page belongs to
  lItem:=CreateEl(lList,'li','md-nav__item md-nav__item--nested md-nav__item--expanded');
  lEl:=CreateEl(lItem,'span','md-nav__link md-nav__link--nested');
  AppendText(lEl,UTF8Decode(Module.Name));
  lSub:=CreateEl(lItem,'nav','md-nav');
  lList:=CreateEl(lSub,'ul','md-nav__list');
  AddModuleItem(lList,IdentifierIndex);
  if Module.InterfaceSection<>Nil then
    begin
    if Module.InterfaceSection.ResStrings.Count>0 then
      AddModuleItem(lList,ResstrSubindex);
    if Module.InterfaceSection.Consts.Count>0 then
      AddModuleItem(lList,ConstsSubindex);
    if Module.InterfaceSection.Types.Count>0 then
      AddModuleItem(lList,TypesSubindex);
    if Module.InterfaceSection.Classes.Count>0 then
      AddModuleItem(lList,ClassesSubindex);
    if Module.InterfaceSection.Functions.Count>0 then
      AddModuleItem(lList,ProcsSubindex);
    if Module.InterfaceSection.Variables.Count>0 then
      AddModuleItem(lList,VarsSubindex);
    end;
  AddModuleItem(lList,IndexSubIndex);
end;


procedure TMaterialWriter.AppendMenuBar(ASubpageIndex: Integer);

begin
  // The navigation sits in the header and in the sidebar
end;


procedure TMaterialWriter.AppendTopicMenuBar(Topic: TTopicElement);

begin
  // The navigation sits in the header and in the sidebar
end;


procedure TMaterialWriter.AppendFooter;

var
  lFooter,lInner,lEl : THTMLElement;
  S : String;

begin
  lFooter:=CreateEl(FBodyElement,'footer','md-footer');
  lInner:=CreateEl(lFooter,'div','md-footer__inner');
  AppendFragment(lInner,FooterHTML);
  if IncludeDateInFooter then
    begin
    if DateFormat='' then
      S:=DateToStr(Date)
    else
      S:=FormatDateTime(DateFormat,Date);
    lEl:=CreateEl(lInner,'span','md-footer__link');
    AppendText(lEl,UTF8Decode(Format(SDocDateGenerated,[S])));
    end;
  lEl:=CreateEl(lInner,'span','md-footer__generated');
  AppendText(lEl,UTF8Decode(SMaterialGeneratedBy));
end;


procedure TMaterialWriter.AppendScripts(aParent: THTMLElement);

var
  lEl : THTMLElement;

begin
  lEl:=CreateEl(aParent,'button','md-top');
  lEl['id']:='fpdoc-top';
  lEl['type']:='button';
  AppendText(lEl,UTF8Decode(SMaterialBackToTop));
  if FUseNavScript then
    begin
    lEl:=CreateEl(aParent,'script');
    lEl['src']:=UTF8Decode(GetAssetPath(MaterialNavFile));
    end;
  lEl:=CreateEl(aParent,'script');
  lEl['src']:=UTF8Decode(GetAssetPath(MaterialJSFile));
end;


procedure TMaterialWriter.AppendClipboardButton(aParent: TDOMNode);

var
  lEl : THTMLElement;

begin
  lEl:=CreateEl(aParent,'button','md-clipboard');
  lEl['type']:='button';
  lEl['title']:=UTF8Decode(SMaterialCopyToClipboard);
end;


procedure TMaterialWriter.BuildTableOfContents;

var
  lList,lItem,lSubNav,lSubList : THTMLElement;
  lCount : Integer;

  function AddEntry(aList : THTMLElement; const aID,aTitle : String) : THTMLElement;

  var
    lEntry,lLink : THTMLElement;

  begin
    lEntry:=CreateEl(aList,'li','md-nav__item');
    lLink:=CreateLink(lEntry,'#'+aID);
    lLink['class']:='md-nav__link';
    AppendText(lLink,UTF8Decode(aTitle));
    Result:=lEntry;
  end;

  procedure HandleHeading(aHeading : TDOMElement; aLevel : Integer);

  var
    lID,lTitle : String;
    lLink : THTMLElement;

  begin
    lTitle:=Trim(NodeText(aHeading));
    lID:=UTF8Encode(aHeading['id']);
    if lID='' then
      begin
      lID:=MakeAnchor(lTitle);
      aHeading['id']:=UTF8Decode(lID);
      end;
    // Anchor to link to this heading
    lLink:=CreateLink(aHeading,'#'+lID);
    lLink['class']:='headerlink';
    lLink['title']:=UTF8Decode(SMaterialPermanentLink);
    AppendText(lLink,UTF8Decode(PilcrowUTF8));
    if (aLevel=1) or (lTitle='') then
      exit;
    Inc(lCount);
    if aLevel=2 then
      begin
      lItem:=AddEntry(lList,lID,lTitle);
      lSubList:=Nil;
      end
    else if lItem<>Nil then
      begin
      if lSubList=Nil then
        begin
        lSubNav:=CreateEl(lItem,'nav','md-nav');
        lSubList:=CreateEl(lSubNav,'ul','md-nav__list');
        end;
      AddEntry(lSubList,lID,lTitle);
      end
    else
      AddEntry(lList,lID,lTitle);
  end;

  procedure Walk(aNode : TDOMNode);

  var
    lChild,lNext : TDOMNode;
    lName : DOMString;

  begin
    lChild:=aNode.FirstChild;
    While lChild<>Nil do
      begin
      // The anchor is added to the heading, so remember the next node first
      lNext:=lChild.NextSibling;
      if lChild.NodeType=ELEMENT_NODE then
        begin
        lName:=LowerCase(lChild.NodeName);
        if lName='h1' then
          HandleHeading(TDOMElement(lChild),1)
        else if lName='h2' then
          HandleHeading(TDOMElement(lChild),2)
        else if lName='h3' then
          HandleHeading(TDOMElement(lChild),3)
        else
          Walk(lChild);
        end;
      lChild:=lNext;
      end;
  end;

begin
  if (FTOCSideBar=Nil) or (FArticleElement=Nil) then
    exit;
  lCount:=0;
  lItem:=Nil;
  lSubList:=Nil;
  AppendText(CreateEl(FTOCElement,'label','md-nav__title'),UTF8Decode(SMaterialTableOfContents));
  lList:=CreateEl(FTOCElement,'ul','md-nav__list');
  Walk(FArticleElement);
  // A single entry is not worth a sidebar
  if lCount<2 then
    FTOCSideBar.ParentNode.RemoveChild(FTOCSideBar);
end;


procedure TMaterialWriter.FinishPageTitle(aElement: TPasElement);

var
  S : String;

begin
  if TitleElement=Nil then
    exit;
  if TitleElement.FirstChild=Nil then
    begin
    if aElement is TPasPackage then
      S:=''
    else if aElement is TPasModule then
      S:=aElement.Name
    else
      S:=aElement.FullName;
    if S<>'' then
      AppendText(TitleElement,UTF8Decode(S));
    end;
  if TitleElement.FirstChild=Nil then
    AppendText(TitleElement,UTF8Decode(GetPackageTitle))
  else
    AppendText(TitleElement,UTF8Decode(' - '+GetPackageTitle));
end;


function TMaterialWriter.CreateHTMLPage(AElement: TPasElement; ASubpageIndex: Integer): TXMLDocument;

var
  lHTML : THTMLHtmlElement;
  lHead : THTMLHeadElement;
  lEl,lMain,lInner,lSideBar,lContent : THTMLElement;

begin
  Result:=THTMLDocument.Create;
  SetHTMLDocument(THTMLDocument(Result));
  Doc.AppendChild(Doc.Impl.CreateDocumentType('html','',''));
  FAnchors.Clear;
  FListHeaders.Clear;

  // Paths and current module must be known before the page is built
  SetModuleInfo(aElement,ASubpageIndex);

  lHTML:=Doc.CreateHtmlElement;
  Doc.AppendChild(lHTML);
  lHTML['lang']:='en';
  lHTML['class']:='no-js';

  lHead:=Doc.CreateHeadElement;
  HeadElement:=lHead;
  lHTML.AppendChild(lHead);
  AppendHead(lHead,aElement);

  FBodyElement:=CreateEl(lHTML,'body');
  ContentElement:=FBodyElement;
  FBodyElement['data-fpdoc-top']:=UTF8Decode(FixHtmlPath(BaseDirectory));
  FBodyElement['data-fpdoc-page']:=UTF8Decode(FPageFileName);
  if Module<>Nil then
    FBodyElement['data-fpdoc-unit']:=UTF8Decode(LowerCase(Module.Name));
  FBodyElement['data-fpdoc-search-limit']:=UTF8Decode(IntToStr(FSearchLimit));

  // Checkbox and overlay drive the navigation drawer on small screens
  lEl:=CreateEl(FBodyElement,'input','md-toggle');
  lEl['type']:='checkbox';
  lEl['id']:='__drawer';
  lEl['autocomplete']:='off';
  lEl:=CreateEl(FBodyElement,'label','md-overlay');
  lEl['for']:='__drawer';
  lEl:=CreateLink(FBodyElement,'#fpdoc-article');
  lEl['class']:='md-skip';
  AppendText(lEl,UTF8Decode(SMaterialSkipToContent));

  AppendHeaderBar(FBodyElement,aElement,ASubpageIndex);

  lMain:=CreateEl(FBodyElement,'main','md-main');
  lInner:=CreateEl(lMain,'div','md-main__inner md-grid');

  lSideBar:=CreateEl(lInner,'div','md-sidebar md-sidebar--primary');
  AppendPrimaryNav(CreateEl(lSideBar,'div','md-sidebar__inner'));

  if FUseTOC then
    begin
    FTOCSideBar:=CreateEl(lInner,'div','md-sidebar md-sidebar--secondary');
    FTOCElement:=CreateEl(CreateEl(FTOCSideBar,'div','md-sidebar__inner'),'nav','md-nav md-nav--secondary');
    FTOCElement['id']:='fpdoc-toc';
    end
  else
    begin
    FTOCSideBar:=Nil;
    FTOCElement:=Nil;
    end;

  lContent:=CreateEl(lInner,'div','md-content');
  FArticleElement:=CreateEl(lContent,'article','md-content__inner md-typeset');
  FArticleElement['id']:='fpdoc-article';
  ContentElement:=FArticleElement;
  AppendFragment(FArticleElement,HeaderHTML);

  CreatePageBody(AElement,ASubpageIndex);

  BuildTableOfContents;
  FinishPageTitle(aElement);
  AppendFooter;
  AppendScripts(FBodyElement);
end;


{ ---------------------------------------------------------------------
  Overview tables
  ---------------------------------------------------------------------}

procedure TMaterialWriter.SetListHeaders(const aTitles: array of String);

var
  S : String;

begin
  FListHeaders.Clear;
  For S in aTitles do
    FListHeaders.Add(S);
end;


function TMaterialWriter.GetListTable(aParent: THTMLElement): THTMLElement;

var
  lLast : TDOMNode;
  lTable,lRow : THTMLElement;
  S : String;

begin
  // Rows are added one by one, so the table of the previous row is reused
  lLast:=aParent.LastChild;
  if (lLast<>Nil) and (lLast.NodeType=ELEMENT_NODE) and (TDOMElement(lLast)['class']='fpdoc-list') then
    Exit(THTMLElement(lLast.LastChild));
  if FListHeaders.Count=0 then
    SetListHeaders([SDocName,SDocDescription]);
  lTable:=CreateEl(aParent,'table','fpdoc-list');
  lRow:=CreateEl(CreateEl(lTable,'thead'),'tr');
  For S in FListHeaders do
    AppendText(CreateEl(lRow,'th'),UTF8Decode(S));
  Result:=CreateEl(lTable,'tbody');
end;


function TMaterialWriter.CreateListColumns(aParent: THTMLElement): THTMLElement;

begin
  Result:=CreateEl(GetListTable(aParent),'tr');
end;


function TMaterialWriter.CreateListColumn1(aParent: THTMLElement): THTMLElement;

begin
  Result:=CreateEl(aParent,'td','fpdoc-list-name');
end;


function TMaterialWriter.CreateListColumn2(aParent: THTMLElement): THTMLElement;

begin
  Result:=CreateEl(aParent,'td','fpdoc-list-descr');
end;


procedure TMaterialWriter.CreatePackagePageBody;

begin
  SetListHeaders([SDocUnits,SDocDescription]);
  inherited CreatePackagePageBody;
end;


procedure TMaterialWriter.CreateModuleMainPage(aModule: TPasModule);

begin
  SetListHeaders([SDocUsedUnits,SDocDescription]);
  inherited CreateModuleMainPage(aModule);
end;


procedure TMaterialWriter.CreateModuleSimpleSubpage(aModule: TPasModule; ASubpageIndex: Integer; const ATitle: DOMString;
  AList: TFPList);

var
  lItem : String;

begin
  case ASubpageIndex of
    ConstsSubindex : lItem:=SDocConstant;
    TypesSubindex : lItem:=SDocType;
    ClassesSubindex : lItem:=SDocClass;
    ProcsSubindex : lItem:=SDocProcedureOrFunction;
    VarsSubindex : lItem:=SDocVariable;
  else
    lItem:=SDocName;
  end;
  SetListHeaders([lItem,SDocDescription]);
  inherited CreateModuleSimpleSubpage(aModule,ASubpageIndex,ATitle,AList);
end;


procedure TMaterialWriter.CreateModuleResStringsPage(aModule: TPasModule);

begin
  SetListHeaders([SDocName,SDocValue]);
  inherited CreateModuleResStringsPage(aModule);
end;


procedure TMaterialWriter.CreateTopicLinks(aParent: THTMLElement; Node: TDocNode; PasElement: TPasElement);

begin
  SetListHeaders([SDocTopic,SDocDescription]);
  inherited CreateTopicLinks(aParent,Node,PasElement);
end;


procedure TMaterialWriter.AppendProcArgsSection(Parent: THTMLElement; Element: TPasProcedureType; SkipResult: Boolean);

begin
  SetListHeaders([SDocName,SDocDescription]);
  inherited AppendProcArgsSection(Parent,Element,SkipResult);
end;


procedure TMaterialWriter.AppendSeeAlsoSection(AElement: TPasElement; aParent: TDOMElement; DocNode: TDocNode);

begin
  SetListHeaders([SDocName,SDocDescription]);
  inherited AppendSeeAlsoSection(AElement,aParent,DocNode);
end;


procedure TMaterialWriter.AppendEnumTypeDecl(aType: TPasEnumType);

begin
  SetListHeaders([SDocValue,SDocDescription]);
  inherited AppendEnumTypeDecl(aType);
end;


procedure TMaterialWriter.AppendSetTypeDecl(aType: TPasSetType);

begin
  SetListHeaders([SDocValue,SDocDescription]);
  inherited AppendSetTypeDecl(aType);
end;


procedure TMaterialWriter.CreateClassMemberList(aParent: THTMLElement; AClass: TPasClassType; DeclaredOnly: Boolean;
  AFilter: TMemberFilter);

var
  lList : TFPList;
  lClass : TPasClassType;
  I,J : Integer;
  lMember : TPasElement;
  lRow,lCell : THTMLElement;

begin
  SetListHeaders([SDocMember,SDocVisibility,SDocDescription]);
  lList:=TFPList.Create;
  try
    // Collect the members, sorted by name, walking up the ancestors if asked
    lClass:=AClass;
    While Assigned(lClass) do
      begin
      For I:=0 to lClass.Members.Count-1 do
        begin
        lMember:=TPasElement(lClass.Members[I]);
        if Engine.ShowElement(lMember) and AFilter(lMember) then
          begin
          J:=0;
          While (J<lList.Count) and (CompareText(TPasElement(lList[J]).Name,lMember.Name)<0) do
            Inc(J);
          lList.Insert(J,lMember);
          end;
        end;
      if DeclaredOnly or (Assigned(lClass.AncestorType) and not lClass.AncestorType.InheritsFrom(TPasClassType)) then
        lClass:=Nil
      else
        lClass:=TPasClassType(lClass.AncestorType);
      end;
    For I:=0 to lList.Count-1 do
      begin
      lMember:=TPasElement(lList[I]);
      lRow:=CreateListColumns(aParent);
      lCell:=CreateListColumn1(lRow);
      AppendHyperlink(lCell,lMember);
      if (lMember.ClassType=TPasProperty) and (TPasProperty(lMember).WriteAccessorName='') then
        begin
        AppendNbSp(lCell,1);
        AppendText(CreateEl(lCell,'span','fpdoc-readonly'),UTF8Decode(SMaterialReadOnly));
        end;
      lCell:=CreateEl(lRow,'td','fpdoc-list-visibility');
      AppendText(lCell,UTF8Decode(VisibilityNames[lMember.Visibility]));
      lCell:=CreateListColumn2(lRow);
      AppendShortDescr(lCell,lMember);
      end;
  finally
    lList.Free;
  end;
end;


{ ---------------------------------------------------------------------
  Content elements
  ---------------------------------------------------------------------}

function TMaterialWriter.CreateH1(Parent: TDOMNode): THTMLElement;

begin
  Result:=CreateEl(Parent,'h1');
end;


function TMaterialWriter.CreateH2(Parent: TDOMNode): THTMLElement;

begin
  Result:=CreateEl(Parent,'h2');
end;


function TMaterialWriter.CreateH3(Parent: TDOMNode): THTMLElement;

begin
  Result:=CreateEl(Parent,'h3');
end;


function TMaterialWriter.CreateSection(aParent: THTMLElement): THTMLElement;

begin
  Result:=CreateEl(aParent,'div','fpdoc-section');
end;


function TMaterialWriter.AppendCodeBlock(aParent: TDOMNode; const aLanguage: String): THTMLElement;

var
  lDiv,lPre : THTMLElement;
  lLanguage : String;

begin
  // Some pages add a second declaration to a code block that already exists
  if (aParent.NodeType=ELEMENT_NODE) and (LowerCase(aParent.NodeName)='code') then
    begin
    if aParent.FirstChild<>Nil then
      AppendText(aParent,sLineBreak);
    Exit(THTMLElement(aParent));
    end;
  lLanguage:=aLanguage;
  if lLanguage='' then
    lLanguage:='pascal';
  lDiv:=CreateEl(aParent,'div','highlight');
  AppendClipboardButton(lDiv);
  lPre:=CreateEl(lDiv,'pre');
  Result:=CreateEl(lPre,'code',UTF8Decode('language-'+lLanguage));
end;


procedure TMaterialWriter.DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String);

var
  lPre : TDOMNode;
  lParent : TDOMNode;
  lDiv : THTMLElement;

begin
  inherited DescrBeginCode(HasBorder,AHighlighterName);
  // The inherited call created and pushed a pre element. Wrap it, so it gets a copy button.
  lPre:=CurOutputNode;
  lParent:=lPre.ParentNode;
  if lParent=Nil then
    exit;
  lDiv:=CreateEl(lParent,'div','highlight');
  AppendClipboardButton(lDiv);
  lParent.RemoveChild(lPre);
  lDiv.AppendChild(lPre);
end;


procedure TMaterialWriter.DescrBeginRemark;

var
  lDiv : THTMLElement;

begin
  lDiv:=CreateEl(CurOutputNode,'div','admonition note');
  AppendText(CreateEl(lDiv,'p','admonition-title'),UTF8Decode(Trim(SDocRemark)));
  PushOutputNode(CreateEl(lDiv,'div','admonition-body'));
end;


procedure TMaterialWriter.AppendInheritanceTree(aParent: THTMLELement; aClass: TPasClassType);

var
  lChain : TFPList;
  lNode : TPasElementNode;
  lList,lItem : THTMLElement;
  lElement : TPasElement;
  I : Integer;

begin
  lChain:=TFPList.Create;
  try
    if aClass.ObjKind=okInterface then
      lNode:=TreeInterface.GetPasElNode(aClass)
    else
      lNode:=TreeClass.GetPasElNode(aClass);
    lElement:=aClass;
    While lElement<>Nil do
      begin
      lChain.Add(lElement);
      if (lNode=Nil) or (lNode.ParentNode=Nil) then
        lElement:=Nil
      else
        begin
        lNode:=lNode.ParentNode;
        lElement:=lNode.Element;
        end;
      end;
    lList:=CreateEl(aParent,'ul','fpdoc-inheritance');
    For I:=lChain.Count-1 downto 0 do
      begin
      lItem:=CreateEl(lList,'li');
      if I=0 then
        lItem['class']:='fpdoc-inheritance-self';
      AppendHyperlink(lItem,TPasElement(lChain[I]));
      end;
  finally
    lChain.Free;
  end;
  // Implemented interfaces
  if Assigned(aClass.Interfaces) and (aClass.Interfaces.Count>0) then
    begin
    lList:=CreateEl(aParent,'p','fpdoc-implements');
    AppendText(lList,UTF8Decode(SMaterialImplements+' '));
    For I:=0 to aClass.Interfaces.Count-1 do
      begin
      if I>0 then
        AppendText(lList,', ');
      AppendHyperlink(lList,TPasElement(aClass.Interfaces[I]));
      end;
    end;
end;


procedure TMaterialWriter.CreateIndexPage(aParent: THTMLElement; L: TStringList);

var
  Lists : Array['A'..'Z'] of TStringList;
  lCurrent : TStringList;
  lLetters,lColumns,lItem,lLink,lHeader : THTMLElement;
  lElement : TPasElement;
  I : Integer;
  S : String;
  C : Char;

begin
  For C:='A' to 'Z' do
    Lists[C]:=Nil;
  L.Sort;
  lCurrent:=Nil;
  // Divide the identifiers over the alphabet
  For I:=0 to L.Count-1 do
    begin
    S:=L[I];
    lElement:=TPasElement(L.Objects[I]);
    if (lElement is TPasUnresolvedTypeRef) or (S='') then
      continue;
    C:=Upcase(S[1]);
    if C='_' then
      C:='A';
    if not (C in ['A'..'Z']) then
      begin
      if lCurrent=Nil then
        continue;
      end
    else
      begin
      if Lists[C]=Nil then
        Lists[C]:=TStringList.Create;
      lCurrent:=Lists[C];
      end;
    lCurrent.AddObject(S,lElement);
    end;
  try
    // Jump list of the available letters
    lLetters:=CreateEl(aParent,'ul','fpdoc-letters');
    For C:='A' to 'Z' do
      if Lists[C]<>Nil then
        begin
        lItem:=CreateEl(lLetters,'li');
        lLink:=CreateLink(lItem,'#section-'+LowerCase(C));
        AppendText(lLink,UTF8Decode(C));
        end;
    // The identifiers themselves
    For C:='A' to 'Z' do
      begin
      lCurrent:=Lists[C];
      if lCurrent=Nil then
        continue;
      lHeader:=CreateH2(aParent);
      lHeader['id']:=UTF8Decode('section-'+LowerCase(C));
      AppendText(lHeader,UTF8Decode(C));
      lColumns:=CreateEl(aParent,'div','fpdoc-index-column');
      if IndexColCount>0 then
        lColumns['style']:=UTF8Decode('column-count: '+IntToStr(IndexColCount));
      For I:=0 to lCurrent.Count-1 do
        AppendHyperlink(lColumns,TPasElement(lCurrent.Objects[I]));
      end;
  finally
    For C:='A' to 'Z' do
      FreeAndNil(Lists[C]);
  end;
end;


{ ---------------------------------------------------------------------
  Documentation process
  ---------------------------------------------------------------------}

procedure TMaterialWriter.WriteDocPage(const aFileName: String; aElement: TPasElement; aSubPageIndex: Integer);

begin
  FPageFileName:=FixHtmlPath(aFileName);
  inherited WriteDocPage(aFileName,aElement,aSubPageIndex);
end;


procedure TMaterialWriter.DoWriteDocumentation;

begin
  // Writes all pages and, through CreateCSSFile, the style sheet and the script
  inherited DoWriteDocumentation;
  CollectPageData;
  if FUseNavScript then
    WriteNavigationData;
  if FUseSearch then
    WriteSearchData;
end;


function TMaterialWriter.InterPretOption(const Cmd, Arg: String): boolean;

  // Fail before the pages are written, not after
  function CheckedFile(const aFileName : String) : String;

  begin
    Result:=SetDirSeparators(aFileName);
    if (Result<>'') and not FileExists(Result) then
      FPDocError(Format('Can''t find file "%s"',[Result]));
  end;

begin
  Result:=True;
  if Cmd='--material-scheme' then
    begin
    if SameText(Arg,'auto') then
      FColorScheme:=mcsAuto
    else if SameText(Arg,'light') then
      FColorScheme:=mcsLight
    else if SameText(Arg,'dark') then
      FColorScheme:=mcsDark
    else
      FPDocError(Format('Unknown colour scheme "%s", use auto, light or dark',[Arg]));
    end
  else if Cmd='--material-color' then
    FPrimaryColor:=Arg
  else if Cmd='--material-accent' then
    FAccentColor:=Arg
  else if Cmd='--material-title' then
    FSiteTitle:=Arg
  else if Cmd='--material-logo' then
    FLogo:=CheckedFile(Arg)
  else if Cmd='--material-favicon' then
    FFavIcon:=CheckedFile(Arg)
  else if Cmd='--js-file' then
    FJSFile:=CheckedFile(Arg)
  else if Cmd='--css-file' then
    CSSFile:=CheckedFile(Arg)
  else if Cmd='--no-search' then
    FUseSearch:=False
  else if Cmd='--search-limit' then
    FSearchLimit:=StrToIntDef(Arg,FSearchLimit)
  else if Cmd='--no-toc' then
    FUseTOC:=False
  else if Cmd='--no-nav-script' then
    FUseNavScript:=False
  else if Cmd='--dump-assets' then
    FDumpAssetDir:=Arg
  else
    Result:=inherited InterPretOption(Cmd,Arg);
end;


class procedure TMaterialWriter.Usage(List: TStrings);

begin
  List.Add('--material-scheme=SCHEME');
  List.Add(SMaterialUsageScheme);
  List.Add('--material-color=NAME');
  List.Add(SMaterialUsageColor);
  List.Add('--material-accent=NAME');
  List.Add(SMaterialUsageAccent);
  List.Add('--material-title=TEXT');
  List.Add(SMaterialUsageTitle);
  List.Add('--material-logo=FILE');
  List.Add(SMaterialUsageLogo);
  List.Add('--material-favicon=FILE');
  List.Add(SMaterialUsageFavIcon);
  List.Add('--js-file=FILE');
  List.Add(SMaterialUsageJSFile);
  List.Add('--no-search');
  List.Add(SMaterialUsageNoSearch);
  List.Add('--search-limit=N');
  List.Add(SMaterialUsageSearchLimit);
  List.Add('--no-toc');
  List.Add(SMaterialUsageNoTOC);
  List.Add('--no-nav-script');
  List.Add(SMaterialUsageNoNavScript);
  List.Add('--dump-assets=DIR');
  List.Add(SMaterialUsageDumpAssets);
  inherited Usage(List);
end;


initialization
  // Do not localize.
  RegisterWriter(TMaterialWriter,'material',SMaterialWriterDescr);

finalization
  UnRegisterWriter('material');
end.
