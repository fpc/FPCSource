{
    $Id$

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * Global declarations
    * Link list management
    * Document node tree
    * Main engine

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit dGlobals;

{$MODE objfpc}
{$H+}

interface

uses Classes, DOM, PasTree, PParser;

resourcestring
  // Output strings
  SDocPackageTitle = 'Reference for package ''%s''';
  SDocPrograms = 'Programs';
  SDocUnits = 'Units';
  SDocUnitTitle = 'Reference for unit ''%s''';
  SDocInterfaceSection = 'Interface section';
  SDocImplementationSection = 'Implementation section';
  SDocUsedUnits = 'Used units';
  SDocUsedUnitsByUnitXY = 'Used units by unit ''%s''';
  SDocConstsTypesVars = 'Constants, types and variables';
  SDocResStrings = 'Resource strings';
  SDocTypes = 'Types';
  SDocConstants = 'Constants';
  SDocClasses = 'Classes';
  SDocProceduresAndFunctions = 'Procedures and functions';
  SDocVariables = 'Variables';

  SDocUnitOverview = 'Overview of unit ''%s''';
  SDocOverview = 'Overview';
  SDocSearch = 'Search';
  SDocDeclaration = 'Declaration';
  SDocDescription = 'Description';
  SDocErrors = 'Errors';
  SDocSeeAlso = 'See also';
  SDocExample = 'Example';
  SDocArguments = 'Arguments';
  SDocFunctionResult = 'Function result';
  SDocRemark = 'Remark:   ';
  SDocMethodOverview = 'Method overview';
  SDocPropertyOverview = 'Property overview';
  SDocPage = 'Page';
  SDocMethod = 'Method';
  SDocProperty = 'Property';
  SDocAccess = 'Access';
  SDocInheritance = 'Inheritance';
  SDocProperties = 'Properties';
  SDocMethods = 'Methods';
  SDocEvents = 'Events';
  SDocByName = 'by Name';
  SDocValue = 'Value';
  SDocExplanation = 'Explanation';
  SDocValuesForEnum = 'Enumeration values for type %s';

type

  // Assumes a list of TObject instances and frees them on destruction

  TObjectList = class(TList)
  public
    destructor Destroy; override;
  end;


  { Link entry tree
    TFPDocEngine stores the root of the entry tree in its property
    "RootLinkNode". The root has one child node for each package, for which
    documentation links are available. The children of a package node
    are module nodes; and the children of a module node are the top-level
    declarations of this module; the next level in the tree stores e.g. record
    members, and so on...
  }

  TLinkNode = class
  private
    FFirstChild, FNextSibling: TLinkNode;
    FName: String;
    FLink: String;
  public
    constructor Create(const AName, ALink: String);
    destructor Destroy; override;
    function FindChild(const APathName: String): TLinkNode;
    function CreateChildren(const APathName, ALinkTo: String): TLinkNode;
    // Properties for tree structure
    property FirstChild: TLinkNode read FFirstChild;
    property NextSibling: TLinkNode read FNextSibling;
    // Link properties
    property Name: String read FName;
    property Link: String read FLink;
  end;


  { Documentation entry tree
    TFPDocEngine stores the root of the entry tree in its property
    "RootDocNode". The root has one child node for each package, for which
    documentation is being provided by the user. The children of a package node
    are module nodes; and the children of a module node are the top-level
    declarations of this module; the next level in the tree stores e.g. record
    members, and so on...
  }

  TDocNode = class
  private
    FFirstChild, FNextSibling: TDocNode;
    FName: String;
    FNode: TDOMElement;
    FIsSkipped: Boolean;
    FShortDescr: TDOMElement;
    FDescr: TDOMElement;
    FErrorsDoc: TDOMElement;
    FSeeAlso: TDOMElement;
    FFirstExample: TDOMElement;
    FLink: String;
  public
    constructor Create(const AName: String; ANode: TDOMElement);
    destructor Destroy; override;
    function FindChild(const APathName: String): TDocNode;
    function CreateChildren(const APathName: String): TDocNode;
    // Properties for tree structure
    property FirstChild: TDocNode read FFirstChild;
    property NextSibling: TDocNode read FNextSibling;
    // Basic properties
    property Name: String read FName;
    property Node: TDOMElement read FNode;
    // Data fetched from the XML document
    property IsSkipped: Boolean read FIsSkipped;
    property ShortDescr: TDOMElement read FShortDescr;
    property Descr: TDOMElement read FDescr;
    property ErrorsDoc: TDOMElement read FErrorsDoc;
    property SeeAlso: TDOMElement read FSeeAlso;
    property FirstExample: TDOMElement read FFirstExample;
    property Link: String read FLink;
  end;


  // The main FPDoc engine

  TFPDocEngine = class(TPasTreeContainer)
  protected
    DescrDocs: TObjectList;		// List of XML documents
    DescrDocNames: TStringList;		// Names of the XML documents
    FRootLinkNode: TLinkNode;
    FRootDocNode: TDocNode;
    FPackages: TList;			// List of TFPPackage objects
    CurModule: TPasModule;
    CurPackageDocNode: TDocNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPackageName(const APackageName: String);
    procedure ReadContentFile(const AFilename, ALinkPrefix: String);
    procedure WriteContentFile(const AFilename: String);

    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
    function FindModule(const AName: String): TPasModule; override;

    // Link tree support
    procedure AddLink(const APathName, ALinkTo: String);
    function FindAbsoluteLink(const AName: String): String;
    function ResolveLink(AModule: TPasModule; const ALinkDest: String): String;

    // Documentation file support
    procedure AddDocFile(const AFilename: String);

    // Documentation retrieval
    function FindDocNode(AElement: TPasElement): TDocNode;
    function FindDocNode(ARefModule: TPasModule; const AName: String): TDocNode;
    function FindShortDescr(AElement: TPasElement): TDOMElement;
    function FindShortDescr(ARefModule: TPasModule;
      const AName: String): TDOMElement;
    function GetExampleFilename(const ExElement: TDOMElement): String;

    property RootLinkNode: TLinkNode read FRootLinkNode;
    property RootDocNode: TDocNode read FRootDocNode;
    property Package: TPasPackage read FPackage;

    Output: String;
    HasContentFile: Boolean;
    HidePrivate: Boolean;	// Hide private class members in output?
    HideProtected: Boolean;	// Hide protected class members in output?
  end;


procedure TranslateDocStrings(const Lang: String);



implementation

uses SysUtils, Gettext, XMLRead;


{ TObjectList }

destructor TObjectList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TObject(Items[i]).Free;
  inherited Destroy;
end;


{ TLinkNode }

constructor TLinkNode.Create(const AName, ALink: String);
begin
  inherited Create;
  FName := AName;
  FLink := ALink;
end;

destructor TLinkNode.Destroy;
begin
  if Assigned(FirstChild) then
    FirstChild.Free;
  if Assigned(NextSibling) then
    NextSibling.Free;
  inherited Destroy;
end;

function TLinkNode.FindChild(const APathName: String): TLinkNode;
var
  DotPos: Integer;
  ChildName: String;
  Child: TLinkNode;
begin
  if Length(APathName) = 0 then
    Result := Self
  else
  begin
    DotPos := Pos('.', APathName);
    if DotPos = 0 then
      ChildName := APathName
    else
      ChildName := Copy(APathName, 1, DotPos - 1);
    Child := FirstChild;
    while Assigned(Child) do
    begin
      if CompareText(Child.Name, ChildName) = 0 then
      begin
        if DotPos = 0 then
	  Result := Child
	else
          Result := Child.FindChild(
	    Copy(APathName, DotPos + 1, Length(APathName)));
	exit;
      end;
      Child := Child.NextSibling;
    end;
    Result := nil;
  end;
end;

function TLinkNode.CreateChildren(const APathName, ALinkTo: String): TLinkNode;
var
  DotPos: Integer;
  ChildName: String;
  Child, LastChild: TLinkNode;
begin
  if Length(APathName) = 0 then
    Result := Self
  else
  begin
    DotPos := Pos('.', APathName);
    if DotPos = 0 then
      ChildName := APathName
    else
      ChildName := Copy(APathName, 1, DotPos - 1);
    Child := FirstChild;
    LastChild := nil;
    while Assigned(Child) do
    begin
      if CompareText(Child.Name, ChildName) = 0 then
      begin
        if DotPos = 0 then
	  Result := Child
	else
          Result := Child.CreateChildren(
	    Copy(APathName, DotPos + 1, Length(APathName)), ALinkTo);
	exit;
      end;
      LastChild := Child;
      Child := Child.NextSibling;
    end;
    { No child found, let's create one if we are at the end of the path }
    if DotPos > 0 then
      // !!!: better throw an exception
      WriteLn('Link path does not exist: ', APathName);
    Result := TLinkNode.Create(ChildName, ALinkTo);
    if Assigned(LastChild) then
      LastChild.FNextSibling := Result
    else
      FFirstChild := Result;
  end;
end;


{ TDocNode }

constructor TDocNode.Create(const AName: String; ANode: TDOMElement);
begin
  inherited Create;
  FName := AName;
  FNode := ANode;
end;

destructor TDocNode.Destroy;
begin
  if Assigned(FirstChild) then
    FirstChild.Free;
  if Assigned(NextSibling) then
    NextSibling.Free;
  inherited Destroy;
end;

function TDocNode.FindChild(const APathName: String): TDocNode;
var
  DotPos: Integer;
  ChildName: String;
  Child: TDocNode;
begin
  if Length(APathName) = 0 then
    Result := Self
  else
  begin
    DotPos := Pos('.', APathName);
    if DotPos = 0 then
      ChildName := APathName
    else
      ChildName := Copy(APathName, 1, DotPos - 1);
    Child := FirstChild;
    while Assigned(Child) do
    begin
      if CompareText(Child.Name, ChildName) = 0 then
      begin
        if DotPos = 0 then
	  Result := Child
	else
          Result := Child.FindChild(
	    Copy(APathName, DotPos + 1, Length(APathName)));
	exit;
      end;
      Child := Child.NextSibling;
    end;
    Result := nil;
  end;
end;

function TDocNode.CreateChildren(const APathName: String): TDocNode;
var
  DotPos: Integer;
  ChildName: String;
  Child: TDocNode;
begin
  if Length(APathName) = 0 then
    Result := Self
  else
  begin
    DotPos := Pos('.', APathName);
    if DotPos = 0 then
      ChildName := APathName
    else
      ChildName := Copy(APathName, 1, DotPos - 1);
    Child := FirstChild;
    while Assigned(Child) do
    begin
      if CompareText(Child.Name, ChildName) = 0 then
      begin
        if DotPos = 0 then
	  Result := Child
	else
          Result := Child.CreateChildren(
	    Copy(APathName, DotPos + 1, Length(APathName)));
	exit;
      end;
      Child := Child.NextSibling;
    end;
    // No child found, let's create one
    Result := TDocNode.Create(ChildName, nil);
    if Assigned(FirstChild) then
    begin
      Result.FNextSibling := FirstChild;
      FFirstChild := Result;
    end else
      FFirstChild := Result;

    if DotPos > 0 then
      Result := Result.CreateChildren(
        Copy(APathName, DotPos + 1, Length(APathName)));
  end;
end;


{ TFPDocEngine }

constructor TFPDocEngine.Create;
begin
  inherited Create;
  DescrDocs := TObjectList.Create;
  DescrDocNames := TStringList.Create;
  FRootLinkNode := TLinkNode.Create('', '');
  FRootDocNode := TDocNode.Create('', nil);
  HidePrivate := True;
  FPackages := TList.Create;
end;

destructor TFPDocEngine.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPackages.Count - 1 do
    TPasPackage(FPackages[i]).Release;
  FPackages.Free;
  FRootDocNode.Free;
  FRootLinkNode.Free;
  DescrDocNames.Free;
  DescrDocs.Free;
  inherited Destroy;
end;

procedure TFPDocEngine.SetPackageName(const APackageName: String);
begin
  ASSERT(not Assigned(Package));
  FPackage := TPasPackage(inherited CreateElement(TPasPackage, '#' + APackageName, nil));
  FPackages.Add(FPackage);
  CurPackageDocNode := RootDocNode.FindChild('#' + APackageName);
end;

procedure TFPDocEngine.ReadContentFile(const AFilename, ALinkPrefix: String);
var
  f: Text;

  procedure ReadLinkTree;
  var
    s: String;
    PrevSpaces, ThisSpaces, i, StackIndex: Integer;
    CurParent, PrevSibling, NewNode: TLinkNode;
    ParentStack, SiblingStack: array[0..7] of TLinkNode;
  begin
    PrevSpaces := 0;
    CurParent := RootLinkNode;
    PrevSibling := nil;
    StackIndex := 0;
    while True do
    begin
      ReadLn(f, s);
      if Length(s) = 0 then
        break;
      ThisSpaces := 0;
      while s[ThisSpaces + 1] = ' ' do
        Inc(ThisSpaces);
      if ThisSpaces <> PrevSpaces then
      begin
        if ThisSpaces > PrevSpaces then
	begin
	  { Dive down one level }
	  ParentStack[StackIndex] := CurParent;
	  SiblingStack[StackIndex] := PrevSibling;
	  Inc(StackIndex);
          CurParent := PrevSibling;
	  PrevSibling := nil;
	end else
	  while PrevSpaces > ThisSpaces do
	  begin
	    Dec(StackIndex);
	    CurParent := ParentStack[StackIndex];
	    PrevSibling := SiblingStack[StackIndex];
	    Dec(PrevSpaces);
	  end;
	PrevSpaces := ThisSpaces;
      end;

      i := ThisSpaces + 1;
      while s[i] <> ' ' do
        Inc(i);
      NewNode := TLinkNode.Create(Copy(s, ThisSpaces + 1, i - ThisSpaces - 1),
        ALinkPrefix + Copy(s, i + 1, Length(s)));
      if Assigned(PrevSibling) then
        PrevSibling.FNextSibling := NewNode
      else
        CurParent.FFirstChild := NewNode;
      PrevSibling := NewNode;
    end;
  end;

  procedure ReadClasses;

    function CreateClass(const AName: String): TPasClassType;
    var
      DotPos, DotPos2, i: Integer;
      s: String;
      Package: TPasPackage;
      Module: TPasModule;
    begin
      // Find or create package
      DotPos := Pos('.', AName);
      s := Copy(AName, 1, DotPos - 1);
      Package := nil;
      for i := 0 to FPackages.Count - 1 do
        if CompareText(TPasPackage(FPackages[i]).Name, s) = 0 then
	begin
	  Package := TPasPackage(FPackages[i]);
	  break;
	end;
      if not Assigned(Package) then
      begin
        Package := TPasPackage(inherited CreateElement(TPasPackage, s, nil));
        FPackages.Add(Package);
      end;

      // Find or create module
      DotPos2 := DotPos;
      repeat
        Inc(DotPos2);
      until AName[DotPos2] = '.';
      s := Copy(AName, DotPos + 1, DotPos2 - DotPos - 1);
      Module := nil;
      for i := 0 to Package.Modules.Count - 1 do
        if CompareText(TPasModule(Package.Modules[i]).Name, s) = 0 then
	begin
	  Module := TPasModule(Package.Modules[i]);
	  break;
	end;
      if not Assigned(Module) then
      begin
	Module := TPasModule.Create(s, Package);
	Module.InterfaceSection := TPasSection.Create('', Module);
	Package.Modules.Add(Module);
      end;

      // Create node for class
      Result := TPasClassType.Create(Copy(AName, DotPos2 + 1, Length(AName)),
        Module.InterfaceSection);
      Result.ObjKind := okClass;
      Module.InterfaceSection.Declarations.Add(Result);
      Module.InterfaceSection.Classes.Add(Result);
    end;

  var
    s, Name: String;
    CurClass: TPasClassType;
    i: Integer;
    Member: TPasElement;
  begin
    CurClass := nil;
    while True do
    begin
      ReadLn(f, s);
      if Length(s) = 0 then
        break;
      if s[1] = '#' then
      begin
        // New class
	i := Pos(' ', s);
	CurClass := CreateClass(Copy(s, 1, i - 1));
      end else
      begin
        i := Pos(' ', s);
	if i = 0 then
          Name := Copy(s, 3, Length(s))
	else
	  Name := Copy(s, 3, i - 3);

        case s[2] of
	  'M':
	    Member := TPasProcedure.Create(Name, CurClass);
	  'P':
	    begin
	      Member := TPasProperty.Create(Name, CurClass);
	      if i > 0 then
	        while i <= Length(s) do
		begin
		  case s[i] of
		    'r':
		      TPasProperty(Member).ReadAccessorName := '<dummy>';
		    'w':
		      TPasProperty(Member).WriteAccessorName := '<dummy>';
		    's':
		      TPasProperty(Member).StoredAccessorName := '<dummy>';
		  end;
		  Inc(i);
		end;
	    end;
	  'V':
	    Member := TPasVariable.Create(Name, CurClass);
	  else
	    raise Exception.Create('Invalid member type: ' + s[2]);
	end;
	CurClass.Members.Add(Member);
      end;
    end;
  end;

var
  s: String;
begin
  Assign(f, AFilename);
  Reset(f);
  while not EOF(f) do
  begin
    ReadLn(f, s);
    if (Length(s) = 0) or (s[1] = '#') then
      continue;
    if s = ':link tree' then
      ReadLinkTree
    else if s = ':classes' then
      ReadClasses
    else
      repeat
        ReadLn(f, s);
      until EOF(f) or (Length(s) = 0);
  end;
  Close(f);
end;

procedure TFPDocEngine.WriteContentFile(const AFilename: String);
var
  ContentFile: Text;

  procedure ProcessLinkNode(ALinkNode: TLinkNode; const AIdent: String);
  var
    ChildNode: TLinkNode;
  begin
    WriteLn(ContentFile, AIdent, ALinkNode.Name, ' ', ALinkNode.Link);
    ChildNode := ALinkNode.FirstChild;
    while Assigned(ChildNode) do
    begin
      ProcessLinkNode(ChildNode, AIdent + ' ');
      ChildNode := ChildNode.NextSibling;
    end;
  end;

var
  LinkNode: TLinkNode;
  i, j, k: Integer;
  Module: TPasModule;
  ClassDecl: TPasClassType;
  Member: TPasElement;
  s: String;
begin
  Assign(ContentFile, AFilename);
  Rewrite(ContentFile);
  try
    WriteLn(ContentFile, '# FPDoc Content File');
    WriteLn(ContentFile, ':link tree');
    LinkNode := RootLinkNode.FirstChild;
    while Assigned(LinkNode) do
    begin
      if LinkNode.Name = Package.Name then
      begin
        ProcessLinkNode(LinkNode, '');
      end;
      LinkNode := LinkNode.NextSibling;
    end;

  if Assigned(Package) then
  begin
    WriteLn(ContentFile);
    WriteLn(ContentFile, ':classes');
    for i := 0 to Package.Modules.Count - 1 do
    begin
      Module := TPasModule(Package.Modules[i]);
      for j := 0 to Module.InterfaceSection.Classes.Count - 1 do
      begin
        ClassDecl := TPasClassType(Module.InterfaceSection.Classes[j]);
        Write(ContentFile, ClassDecl.PathName, ' ');
        if Assigned(ClassDecl.AncestorType) then
          WriteLn(ContentFile, ClassDecl.AncestorType.PathName)
        else if ClassDecl.ObjKind = okClass then
          WriteLn(ContentFile, '.TObject');
	for k := 0 to ClassDecl.Members.Count - 1 do
	begin
	  Member := TPasElement(ClassDecl.Members[k]);
	  Write(ContentFile, Chr(Ord(Member.Visibility) + Ord('0')));
	  SetLength(s, 0);
	  if Member.ClassType = TPasVariable then
	    Write(ContentFile, 'V')
	  else if Member.ClassType = TPasProperty then
	  begin
	    Write(ContentFile, 'P');
	    if Length(TPasProperty(Member).ReadAccessorName) > 0 then
	      s := s + 'r';
	    if Length(TPasProperty(Member).WriteAccessorName) > 0 then
	      s := s + 'w';
	    if Length(TPasProperty(Member).StoredAccessorName) > 0 then
	      s := s + 's';
	  end else
	    Write(ContentFile, 'M');	// Member must be a method
	  Write(ContentFile, Member.Name);
	  if Length(s) > 0 then
	    WriteLn(ContentFile, ' ', s)
	  else
	    WriteLn(ContentFile);
	end;
      end;
    end;
  end;


  finally
    Close(ContentFile);
  end;
end;

function TFPDocEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result);
end;

function TFPDocEngine.FindElement(const AName: String): TPasElement;

  function FindInModule(AModule: TPasModule; const LocalName: String): TPasElement;
  var
    l: TList;
    i: Integer;
  begin
    l := AModule.InterfaceSection.Declarations;
    for i := 0 to l.Count - 1 do
    begin
      Result := TPasElement(l[i]);
      if CompareText(Result.Name, LocalName) = 0 then
        exit;
    end;
    Result := nil;
 end;

var
  i: Integer;
  //ModuleName, LocalName: String;
  Module: TPasElement;
begin
{!!!: Don't know if we ever will have to use the following:
  i := Pos('.', AName);
  if i <> 0 then
  begin
    WriteLn('Dot found in name: ', AName);
    Result := nil;
  end else
  begin}
    Result := FindInModule(CurModule, AName);
    if not Assigned(Result) then
      for i := CurModule.InterfaceSection.UsesList.Count - 1 downto 0 do
      begin
        Module := TPasElement(CurModule.InterfaceSection.UsesList[i]);
	if Module.ClassType = TPasModule then
	begin
          Result := FindInModule(TPasModule(Module), AName);
	  if Assigned(Result) then
	    exit;
	end;
      end;
  {end;}
end;

function TFPDocEngine.FindModule(const AName: String): TPasModule;

  function FindInPackage(APackage: TPasPackage): TPasModule;
  var
    i: Integer;
  begin
    for i := 0 to APackage.Modules.Count - 1 do
    begin
      Result := TPasModule(APackage.Modules[i]);
      if CompareText(Result.Name, AName) = 0 then
        exit;
    end;
    Result := nil;
  end;

var
  i: Integer;
begin
  Result := FindInPackage(Package);
  if not Assigned(Result) then
    for i := FPackages.Count - 1 downto 0 do
    begin
      if TPasPackage(FPackages[i]) = Package then
        continue;
      Result := FindInPackage(TPasPackage(FPackages[i]));
      if Assigned(Result) then
        exit;
    end;
end;

procedure TFPDocEngine.AddLink(const APathName, ALinkTo: String);
begin
  RootLinkNode.CreateChildren(APathName, ALinkTo);
end;

function TFPDocEngine.FindAbsoluteLink(const AName: String): String;
var
  LinkNode: TLinkNode;
begin
  LinkNode := RootLinkNode.FindChild(AName);
  if Assigned(LinkNode) then
    Result := LinkNode.Link
  else
    SetLength(Result, 0);
end;

function TFPDocEngine.ResolveLink(AModule: TPasModule;
  const ALinkDest: String): String;
var
  i: Integer;
  ThisPackage: TLinkNode;
  UnitList: TList;
begin
//WriteLn('ResolveLink(', ALinkDest, ')... ');
  if Length(ALinkDest) = 0 then
  begin
    SetLength(Result, 0);
    exit;
  end;

  if ALinkDest[1] = '#' then
    Result := FindAbsoluteLink(ALinkDest)
  else
  begin
    Result := ResolveLink(AModule, AModule.PathName + '.' + ALinkDest);
    if Length(Result) > 0 then
      exit;

    { Try all packages }
    SetLength(Result, 0);
    ThisPackage := RootLinkNode.FirstChild;
    while Assigned(ThisPackage) do
    begin
      Result := ResolveLink(AModule, ThisPackage.Name + '.' + ALinkDest);
      if Length(Result) > 0 then
	exit;
      ThisPackage := ThisPackage.NextSibling;
    end;

    if Length(Result) = 0 then
    begin
      { Okay, then we have to try all imported units of the current module }
      UnitList := AModule.InterfaceSection.UsesList;
      for i := UnitList.Count - 1 downto 0 do
      begin
	{ Try all packages }
	ThisPackage := RootLinkNode.FirstChild;
	while Assigned(ThisPackage) do
	begin
	  Result := ResolveLink(AModule, ThisPackage.Name + '.' +
	    TPasType(UnitList[i]).Name + '.' + ALinkDest);
	  if Length(Result) > 0 then
	    exit;
	  ThisPackage := ThisPackage.NextSibling;
	end;
      end;
    end;
  end;

  if Length(Result) = 0 then
    for i := Length(ALinkDest) downto 1 do
      if ALinkDest[i] = '.' then
      begin
	Result := ResolveLink(AModule, Copy(ALinkDest, 1, i - 1));
        exit;
      end;
end;

procedure TFPDocEngine.AddDocFile(const AFilename: String);

  function ReadNode(OwnerDocNode: TDocNode; Element: TDOMElement): TDocNode;
  var
    Subnode: TDOMNode;
  begin
    if OwnerDocNode = RootDocNode then
      Result := OwnerDocNode.CreateChildren('#' + Element['name'])
    else
      Result := OwnerDocNode.CreateChildren(Element['name']);
    Result.FNode := Element;
    Result.FLink := Element['link'];
    Result.FIsSkipped := Element['skip'] = '1';
    Subnode := Element.FirstChild;
    while Assigned(Subnode) do
    begin
      if Subnode.NodeType = ELEMENT_NODE then
      begin
	if Subnode.NodeName = 'short' then
	  Result.FShortDescr := TDOMElement(Subnode)
	else if Subnode.NodeName = 'descr' then
	  Result.FDescr := TDOMElement(Subnode)
	else if Subnode.NodeName = 'errors' then
	  Result.FErrorsDoc := TDOMElement(Subnode)
	else if Subnode.NodeName = 'seealso' then
	  Result.FSeeAlso := TDOMElement(Subnode)
	else if (Subnode.NodeName = 'example') and
	  not Assigned(Result.FirstExample) then
	  Result.FFirstExample := TDOMElement(Subnode);
      end;
      Subnode := Subnode.NextSibling;
    end;
  end;

var
  i: Integer;
  Node, Subnode, Subsubnode: TDOMNode;
  Element: TDOMElement;
  Doc: TXMLDocument;
  PackageDocNode, ModuleDocNode: TDocNode;
begin
  ReadXMLFile(Doc, AFilename);
  DescrDocs.Add(Doc);
  DescrDocNames.Add(AFilename);

  Node := Doc.DocumentElement.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'package') then
    begin
      PackageDocNode := ReadNode(RootDocNode, TDOMElement(Node));

      // Scan all 'module' elements within this package element
      Subnode := Node.FirstChild;
      while Assigned(Subnode) do
      begin
        if (Subnode.NodeType = ELEMENT_NODE) and
	  (Subnode.NodeName = 'module') then
	begin
	  ModuleDocNode := ReadNode(PackageDocNode, TDOMElement(Subnode));

	  // Scan all 'element' elements within this module element
	  Subsubnode := Subnode.FirstChild;
	  while Assigned(Subsubnode) do
	  begin
	    if (Subsubnode.NodeType = ELEMENT_NODE) and
	      (Subsubnode.NodeName = 'element') then
	      ReadNode(ModuleDocNode, TDOMElement(Subsubnode));
	    Subsubnode := Subsubnode.NextSibling;
	  end;
	end;
        Subnode := Subnode.NextSibling;
      end;
    end;
    Node := Node.NextSibling;
  end;
end;

function TFPDocEngine.FindDocNode(AElement: TPasElement): TDocNode;
begin
  if AElement.InheritsFrom(TPasUnresolvedTypeRef) then
    Result := FindDocNode(AElement.GetModule, AElement.Name)
  else
    Result := RootDocNode.FindChild(AElement.PathName);
end;

function TFPDocEngine.FindDocNode(ARefModule: TPasModule;
  const AName: String): TDocNode;
var
  CurPackage: TDocNode;
  UnitList: TList;
  i: Integer;
begin
  if Length(AName) = 0 then
    Result := nil
  else
  begin
    if AName[1] = '#' then
      Result := RootDocNode.FindChild(AName)
    else
      Result := RootDocNode.FindChild(Package.Name + '.' + AName);
    if (not Assigned(Result)) and Assigned(ARefModule) then
      Result := RootDocNode.FindChild(ARefModule.PathName + '.' + AName);

    if (not Assigned(Result)) and (AName[1] <> '#') then
    begin
      CurPackage := RootDocNode.FirstChild;
      while Assigned(CurPackage) do
      begin
	Result := RootDocNode.FindChild(CurPackage.Name + '.' + AName);
	if Assigned(Result) then
	  break;
	CurPackage := CurPackage.NextSibling;
      end;
      if not Assigned(Result) then
      begin
        { Okay, then we have to try all imported units of the current module }
        UnitList := CurModule.InterfaceSection.UsesList;
        for i := UnitList.Count - 1 downto 0 do
        begin
          { Try all packages }
	  CurPackage := RootDocNode.FirstChild;
	  while Assigned(CurPackage) do
	  begin
	    Result := RootDocNode.FindChild(CurPackage.Name + '.' +
	      TPasType(UnitList[i]).Name + '.' + AName);
	    if Assigned(Result) then
	      break;
	    CurPackage := CurPackage.NextSibling;
	  end;
	end;
      end;
    end;
  end;
end;

function TFPDocEngine.FindShortDescr(AElement: TPasElement): TDOMElement;
var
  DocNode: TDocNode;
begin
  DocNode := FindDocNode(AElement);
  if Assigned(DocNode) then
    Result := DocNode.ShortDescr
  else
    Result := nil;
end;

function TFPDocEngine.FindShortDescr(ARefModule: TPasModule;
  const AName: String): TDOMElement;
var
  DocNode: TDocNode;
begin
  DocNode := FindDocNode(ARefModule, AName);
  if Assigned(DocNode) then
    Result := DocNode.ShortDescr
  else
    Result := nil;
end;

function TFPDocEngine.GetExampleFilename(const ExElement: TDOMElement): String;
var
  i: Integer;
begin
  for i := 0 to DescrDocs.Count - 1 do
    if TDOMDocument(DescrDocs[i]) = ExElement.OwnerDocument then
    begin
      Result := ExtractFilePath(DescrDocNames[i]) + ExElement['file'];
      exit;
    end;
  SetLength(Result, 0);
end;


{ Global helpers }

procedure TranslateDocStrings(const Lang: String);
var
  mo: TMOFile;
begin
{$IFDEF Unix}
  mo := TMOFile.Create(Format('/usr/local/share/locale/%s/LC_MESSAGES/dglobals.mo', [Lang]));
{$ELSE}
  mo := TMOFile.Create(Format('intl/dglobals.%s.mo', [Lang]));
{$ENDIF}
  try
    TranslateResourceStrings(mo);
  finally
    mo.Free;
  end;
end;



end.


{
  $Log$
  Revision 1.1  2003-03-17 23:03:20  michael
  + Initial import in CVS

  Revision 1.13  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.12  2002/11/15 19:44:18  sg
  * Cosmetic changes

  Revision 1.11  2002/10/12 17:00:45  michael
  + Changes to be able to disable private/protected nodes in skeleton

  Revision 1.10  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.9  2002/03/25 23:16:24  sg
  * fixed missing storing of documenation data for the DocNode of a module
    (so e.g. the Unit Overview is working again)

  Revision 1.8  2002/03/12 10:58:35  sg
  * reworked linking engine and internal structure

  Revision 1.7  2002/01/20 11:19:55  michael
  + Added link attribute and property to TFPelement

  Revision 1.6  2001/12/17 22:16:02  sg
  * Added TFPDocEngine.HideProtected
}
