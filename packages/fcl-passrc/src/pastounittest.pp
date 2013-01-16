{
    This file is part of the Free Component Library
    Copyright (c) 2012 by the Free Pascal team

    Pascal source to FPC Unit test generator

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit pastounittest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PScanner, pparser, pastree;


Type


  TTestMemberType = (tmtMethods,   // Generate tests for methods
                     tmtFields,    // Generate tests for fields
                     tmtProperties // Generate tests for properties
                     );
  TTestMemberTypes = set of TTestmemberType;
  TTestPropertyOption = (tDefault,    // Generate default test for a property
                         tGetBounds,  // Generate Property GetBounds test (tiOPF)
                         tRequired,   // Generate Property Required test (tiOPF)
                         tNotify,     // Generate Property change notification test (tiOPF)
                         tMaxLen);    // Generate property MaxLen (tiOPF)
  TTestpropertyOptions = set of TTestpropertyOption;
  TTestCodeOption = (coCreateDeclaration, // Generate declaration of test cases. 
                     coImplementation,  // generate (empty) implementation of tests
                     coDefaultFail,     // Insert Fail() statement in tests
                     coSingleClass,     // Use a single test class for all tests
                     coCreateUnit,      // Generate complete unit source
                     coSetup,           // Generate Setup() method for all test classes
                     coTearDown,        // Generate TearDown() method for all test classes
                     coFunctions,       // Generate tests for functions
                     coClasses,         // Generate tests for classes
                     coRegisterTests);  // Register all generated test classes
  TTestCodeOptions = set of TTestCodeOption;

  { TFPTestCodeCreator }

  TFPTestCodeCreator = Class(TComponent)
  private
    FCO: TTestCodeOptions;
    FDCT: TStrings;
    FDestUnitName: string;
    FFailMessage: String;
    FLimits: TStrings;
    FMemberTypes: TTestmemberTypes;
    FPO: TTestpropertyOptions;
    FTCP: String;
    FTP: String;
    FUTC: String;
    FVisibilities: TPasMemberVisibilities;
    FTests : TStrings;
    FM : String;
    procedure SetDCT(AValue: TStrings);
    procedure SetFailMessage(Const AValue: String);
    procedure SetLimits(AValue: TStrings);
    procedure StartTestClassImpl(C: TStrings; Const AClassName: String);
  protected
    // Split test name S in class name and method name.
    procedure ExtractClassMethod(S: string; out CN, MN: String);virtual;
    // Return classname for testcase for a class.
    Function GetTestClassName(CT : TPasClassType) : String; virtual;
    // Should this identifier be tested ? Only called for global identifiers.
    function AllowIdentifier(S: TPasElement): boolean;
    // Should return true if a variable/property type is a string type.
    function IsStringType(T:  TPasType): Boolean;virtual;
    // Add a test to the list of tests.
    // If ATestClass is empty, test is added to the global unit test class.
    // If coSingleClass is in the options, all tests are added to this class
    // and ATestClass is prefixed to the test name.
    Procedure AddTest(Const ATestClass,ATestName : String); virtual;
    // Create implementation of test code. After 'Implementation' keyword was added
    procedure CreateImplementationCode(C: TStrings); virtual;
    // Add a test method body to the implementation. AddFail=True adds a Fail statement.
    procedure AddMethodImpl(C: TStrings; Const AClassName, AMethodName: String; AddFail: Boolean; AddInherited : Boolean = false);virtual;
    // Called when all the methods of a class have been emitted. Empty.
    procedure EndTestClassImpl(C: TStrings; Const AClassName: String);virtual;
    // Create interface test code. After uses clause of interface section.
    procedure CreateInterfaceCode(C: TStrings);virtual;
    // Called whenever a new test class declaration is started.
    procedure StartTestClassDecl(C: TStrings; AClassName: String); virtual;
    // Called whenever a test class declaration is finished (adds end;)
    procedure EndTestClassDecl(C: TStrings; AClassName: String); virtual;
    // Called to add default test methods for a class.
    procedure AddDefaultMethodDecl(C: TStrings; Const AClassName: String);virtual;
    // Create test code based on tests
    procedure CreateTestCode(Dest: TStream; const InputUnitName: string);virtual;
    // Calls DoCreateTests for the interface section of the module.
    procedure DoCreateTests(M: TPasModule);virtual;
    // Create tests for a modult. Creates tests for functions/procedures and classes.
    procedure DoCreateTests(S: TPasSection);virtual;
    // Called for each function/procedure in a section to create tests for it.
    procedure DoCreateTests(P: TPasProcedure);virtual;
    // Called for each overloaded function/procedure in a section to create tests for it.
    procedure DoCreateTests(P: TPasOverloadedProc);virtual;
    // Called for each class in a section to create tests for the class.
    procedure DoCreateTests(CT: TPasClasstype);virtual;
    // Called for each overloaded method in a class to create tests for it (within visibilities).
    procedure DoCreateTests(const TCN: String; CT: TPasClasstype; P: TPasOverloadedProc);virtual;
    // Called for each method in a class to create tests for it (within visibilities)
    procedure DoCreateTests(const TCN: String; CT: TPasClasstype; P: TPasprocedure);virtual;
    // Called for each field in a class to create tests for it (within visibilities).
    procedure DoCreateTests(const TCN: String; CT: TPasClasstype; P: TPasVariable);virtual;
    // Called for each property in a class to create tests for it(within visibilities).
    procedure DoCreateTests(const TCN: String; CT: TPasClasstype; P: TPasProperty);virtual;
    // Parse the actual source and return module.
    function ParseSource(const ASourceStream : TStream): TPasModule;
    // Main entry to create tests.
    procedure CreateTests(M: TPasModule; Dest : TStream);
    // List of test names in the form ClassName.MethodName. Setup and Teardown are not in the list.
    Property Tests : TStrings Read FTests;
  Public
    Constructor Create(AOwner :TComponent); override;
    Destructor Destroy; override;
    // Create test unit cases in dest (file/stream/tstrings) based on
    // Code in source
    Procedure Execute(Const ASourceFileName,ADestFileName : String);
    Procedure Execute(Const ASourceStream,ADestStream : TStream);
    Procedure Execute(Const ASourceCode,ADestCode : TStrings);
  Published
    // If not empty, tests will be generated only for the global identifiers in this list
    Property LimitIdentifiers : TStrings Read FLimits Write SetLimits;
    // List of names of tests which are always generated for each test.
    Property DefaultClassTests : TStrings Read FDCT Write SetDCT;
    // For class members, member visibilities for which to generate tests.
    Property Visibilities : TPasMemberVisibilities Read FVisibilities Write FVisibilities;
    // For which class members should tests be generated
    Property MemberTypes : TTestmemberTypes Read FMemberTypes Write FMemberTypes;
    // What default tests should be generated for properties/fields in a class
    Property PropertyOptions : TTestpropertyOptions Read FPO Write FPO;
    // Various options for the generated code
    Property CodeOptions : TTestCodeOptions Read FCO Write FCO;
    // Destination unit name. If empty, name will be taken from input file.
    Property DestUnitName : string Read FDestUnitName Write FDestUnitName;
    // Name for the global unit test case. If not set, it is 'Test'+the input unit name
    Property UnitTestClassName: String Read FUTC Write FUTC;
    // Prefix for names of all tests
    Property TestNamePrefix : String Read FTP Write FTP;
    // Name of parent of all test classes
    Property TestClassParent : String Read FTCP Write FTCP;
    // Text to put in Fail() statement.
    Property FailMessage : String Read FFailMessage Write SetFailMessage;
  end;

Const
  DefaultVisibilities    = [visDefault,visPublished,visPublic];
  DefaultPropertyOptions = [tDefault];
  DefaultCodeOptions     = [coCreateDeclaration,coImplementation,coDefaultFail,coCreateUnit,
                            coSetup,coTearDown, coFunctions, coClasses,
                            coRegisterTests];
  DefaultMembers         = [tmtMethods,tmtFields,tmtProperties];
  DefaultTestClassParent = 'TTestCase';

Resourcestring
  DefaultFailmessage     = 'This test is not yet implemented';

Procedure CreateUnitTests(Const InputFile,OutputFile : String; ACodeOptions : TTestCodeOptions = [] );

implementation

Type
  { TTestContainer }

  TTestContainer = Class(TPasTreeContainer)
  Public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;overload;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

procedure CreateUnitTests(const InputFile, OutputFile: String; ACodeOptions : TTestCodeOptions = [] );
begin
  with TFPTestCodeCreator.Create(Nil) do
    try
      if ACodeOptions<>[] then
        CodeOptions:=ACodeOptions;
      Execute(inputfile,outputfile);
    finally
      free;
    end;
end;

{ TFPTestCodeCreator }

procedure TFPTestCodeCreator.SetLimits(AValue: TStrings);
begin
  if FLimits=AValue then Exit;
  FLimits.Assign(AValue);
end;

function TFPTestCodeCreator.GetTestClassName(CT: TPasClassType): String;
begin
  Result:=CT.Name;
  if Not (coSingleClass in CodeOptions) then
    begin
    if Upcase(Result[1])='T' then
      Delete(Result,1,1);
    Result:='TTest'+Result;
    end;
end;

procedure TFPTestCodeCreator.EndTestClassDecl(C: TStrings; AClassName: String);
begin
  C.Add('  end;');
  C.Add('');
  C.Add('');
end;

procedure TFPTestCodeCreator.AddTest(const ATestClass, ATestName: String);

Var
  CN,TN : String;

begin
  TN:=ATestName;
  if ATestClass='' then
    CN:=UnitTestClassName
  else
    CN:=ATestClass;
  if (coSingleClass in CodeOptions) then
    begin
    TN:=ATestClass+TN;
    CN:=UnitTestClassName;
    end;
  FTests.Add(CN+'.'+TestNamePrefix+TN);
end;

procedure TFPTestCodeCreator.DoCreateTests(const TCN: String;
  CT: TPasClasstype; P: TPasOverloadedProc);
begin
  AddTest(TCN,P.Name);
end;

procedure TFPTestCodeCreator.DoCreateTests(P : TPasProcedure);

begin
  AddTest('',P.Name);
end;

procedure TFPTestCodeCreator.DoCreateTests(P: TPasOverloadedProc);
begin
  AddTest('',P.Name);
end;

procedure TFPTestCodeCreator.DoCreateTests(Const TCN: String; CT : TPasClasstype; P : TPasprocedure);

begin
  AddTest(TCN,P.Name);
end;

Function TFPTestCodeCreator.IsStringType(T : TPasType) : Boolean;

Var
  tn : string;
begin
  While t is TPasAliasType do
    T:=TPasAliasType(t).DestType;
  tn:=lowercase(t.name);
  Result:=(T is TPasStringType) or (tn='string') or (tn='ansistring') or (tn='widestring') or (tn='unicodestring') or (tn='shortstring');
end;

procedure TFPTestCodeCreator.DoCreateTests(Const TCN: String; CT : TPasClasstype; P : TPasVariable);

begin
  if (tDefault in PropertyOptions) then
    AddTest(TCN,P.Name);
  if (tRequired in PropertyOptions) then
    AddTest(TCN,P.Name+'Required');
  if (tGetBounds in PropertyOptions) then
    AddTest(TCN,P.Name+'GetBounds');
  If (tmaxLen in PropertyOptions) then
    if Assigned(P.VarType) and IsStringType(P.VarType) then
      AddTest(TCN,P.Name+'MaxLen');
end;

procedure TFPTestCodeCreator.DoCreateTests(const TCN: String;
  CT: TPasClasstype; P: TPasProperty);
begin
  if (tDefault in PropertyOptions) then
    AddTest(TCN,P.Name);
  if (tRequired in PropertyOptions) then
    AddTest(TCN,P.Name+'Required');
  if (tGetBounds in PropertyOptions) then
    AddTest(TCN,P.Name+'GetBounds');
  if (tNotify in PropertyOptions) then
    AddTest(TCN,P.Name+'Notify');
  If (tmaxLen in PropertyOptions) then
    if Assigned(P.VarType) and IsStringType(P.VarType) then
      AddTest(TCN,P.Name+'MaxLen');
end;

procedure TFPTestCodeCreator.DoCreateTests(CT : TPasClasstype);

Var
  E : TPasElement;
  I : Integer;
  TCN : String;

begin
  TCN:=GetTestClassName(CT);
  For I:=0 to DefaultClassTests.Count-1 do
    AddTest(TCN,DefaultClassTests[i]);
  if (tmtMethods in Membertypes) then
    For I:=0 to CT.Members.Count-1 do
      begin
      E:=TPasElement(CT.Members[i]);
      if (E is TPasProcedure) and (E.Visibility in Visibilities) then
        DoCreateTests(TCN,CT,TPasProcedure(E))
      else if (E is TPasoverloadedProc) and (E.Visibility in Visibilities) then
        DoCreateTests(TCN,CT,TPasoverloadedProc(E));
      end;
  if (tmtFields in Membertypes) then
    For I:=0 to CT.Members.Count-1 do
      begin
      E:=TPasElement(CT.Members[i]);
      if (E is TPasVariable) and (Not(E is TPasProperty)) and (E.Visibility in Visibilities) then
        DoCreateTests(TCN,CT,TPasVariable(E));
      end;
  if (tmtProperties in Membertypes) then
    For I:=0 to CT.Members.Count-1 do
      begin
      E:=TPasElement(CT.Members[i]);
      if (E is TPasProperty) and (E.Visibility in Visibilities) then
        DoCreateTests(TCN,CT,TPasProperty(E));
      end;
end;

function TFPTestCodeCreator.AllowIdentifier(S: TPasElement) : boolean;

begin
  Result:=(LimitIdentifiers.Count=0) or (LimitIdentifiers.IndexOf(S.Name)<>-1);
end;

procedure TFPTestCodeCreator.DoCreateTests(S: TPasSection);

Var
  I : integer;
  CT : TPasClasstype;
  FT : TPasProcedure;
  O : TPasOverloadedProc;

begin
  if coClasses in CodeOptions then
    For I:=0 to S.Classes.Count-1 do
      begin
      CT:=TPasClassType(S.Classes[i]);
      If Not CT.IsForward then
        if AllowIdentifier(CT) then
          DoCreateTests(CT);
      end;
  if coFunctions in CodeOptions then
    For I:=0 to S.Functions.Count-1 do
      begin
      if TPasElement(S.Functions[i]) is TPasProcedure then
        begin
        FT:=TPasElement(S.Functions[i]) as TPasProcedure;
        If Not FT.IsForward then
          if AllowIdentifier(FT) then
            DoCreateTests(FT);
        end
      else if TPasElement(S.Functions[i]) is TPasOverloadedProc then
        begin
        O:=TPasElement(S.Functions[i]) as TPasOverloadedProc;
        if AllowIdentifier(O) then
          DoCreateTests(O);
        end;
      end;
end;

procedure TFPTestCodeCreator.DoCreateTests(M: TPasModule);

begin
  If UnitTestClassName='' then
    UnitTestClassName:='Test'+M.Name;
  DoCreateTests(M.InterfaceSection);
end;

procedure TFPTestCodeCreator.SetDCT(AValue: TStrings);
begin
  if FDCT=AValue then Exit;
  FDCT.Assign(AValue);
end;

procedure TFPTestCodeCreator.SetFailMessage(Const AValue: String);
begin
  if FFailMessage=AValue then Exit;
  FFailMessage:=AValue;
  FM:=StringReplace(FailMessage,'''','''''',[rfReplaceAll]);
end;

constructor TFPTestCodeCreator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLimits:=TStringList.Create;
  TStringList(FLimits).Sorted:=True;
  FDCT:=TstringList.Create;
  FDCT.Add('Empty');
  FDCT.Add('IsValid');
  TestNamePrefix:='Test';
  Visibilities:=DefaultVisibilities;
  CodeOptions:=DefaultCodeOptions;
  PropertyOptions:=DefaultPropertyOptions;
  MemberTypes:=DefaultMembers;
  TestClassParent:=DefaultTestClassParent;
  FailMessage:=DefaultFailmessage;
end;

destructor TFPTestCodeCreator.Destroy;
begin
  FreeAndNil(FDCT);
  FreeAndNil(FLimits);
  inherited Destroy;
end;

procedure TFPTestCodeCreator.Execute(const ASourceFileName,
  ADestFileName: String);

Var
  Fi,Fo : TFileStream;

begin
  Fi:=TFileStream.Create(ASourceFileName,fmOpenRead);
  try
    Fo:=TFileStream.Create(ADestFileName,fmCreate);
    try
      if (DestunitName='') then
        DestUnitName:=ChangeFileExt(ExtractFileName(ADestFileName),'');
      Execute(Fi,Fo);
    finally
      FO.free;
    end;
  finally
    Fi.Free;
  end;
end;

procedure TFPTestCodeCreator.StartTestClassDecl(C : TStrings; AClassName : String);

begin
  C.Add('  { '+AClassName+' }');
  C.Add('');
  C.Add(Format('  %s = Class(%s)',[ACLassName,TestClassParent]));
  If (([coSetup,coTearDown] * CodeOptions)<>[]) then
    begin
    C.Add('  Protected');
    if coSetup in CodeOptions then
      C.Add('    procedure Setup; override;');
    if coSetup in CodeOptions then
      C.Add('    procedure TearDown; override;');
    end;
end;


procedure TFPTestCodeCreator.AddDefaultMethodDecl(C : TStrings; const AClassName : String);

begin
//
end;

Procedure TFPTestCodeCreator.ExtractClassMethod(S : string; Out CN,MN : String);

Var
  P : Integer;
begin
  P:=Pos('.',S);
  Cn:=Copy(S,1,P-1);
  MN:=S;
  Delete(MN,1,P);
end;

procedure TFPTestCodeCreator.CreateInterfaceCode(C : TStrings);

Var
  CCN,CN,MN : String;
  I : Integer;

begin
  CCN:='';
  For I:=0 to FTests.Count-1 do
    begin
    ExtractClassMethod(FTests[i],Cn,MN);
    If (CN<>CCN) then
      begin
      if (CCN<>'') then
        EndTestClassDecl(C,CN);
      StartTestClassDecl(C,CN);
      C.Add('  Published');
      AddDefaultMethodDecl(C,CN);
      CCN:=CN;
      end;
    C.Add('    Procedure '+MN+';');
    end;
  if (CCN<>'') then
    EndTestClassDecl(C,CN);
end;

procedure TFPTestCodeCreator.AddMethodImpl(C : TStrings; Const AClassName,AMethodName : String; AddFail : Boolean; AddInherited : Boolean = false);

begin
  C.Add('');
  C.Add(Format('Procedure %s.%s;',[AClassName,AMethodName]));
  C.Add('');
  C.Add('begin');
  if AddFail then
    C.Add(Format('  Fail(''%s'');',[FM]));
  if AddInherited then
    C.Add('  Inherited;');  
  C.Add('end;');
  C.Add('');
end;

procedure TFPTestCodeCreator.StartTestClassImpl(C : TStrings; Const AClassName : String);

begin
  C.Add('');
  C.Add('  { '+AClassName+' }');
  C.Add('');
  if coSetup in CodeOptions then
    AddMethodImpl(C,AClassName,'Setup',False,True);
  if coTearDown in CodeOptions then
    AddMethodImpl(C,AClassName,'TearDown',False,True);
end;

procedure TFPTestCodeCreator.EndTestClassImpl(C : TStrings; Const AClassName : String);

begin
end;

procedure TFPTestCodeCreator.CreateImplementationCode(C : TStrings);

Var
  CCN,CN,MN : String;
  I : Integer;
  F : Boolean;

begin
  CCN:='';
  F:=coDefaultFail in CodeOptions;
  For I:=0 to FTests.Count-1 do
    begin
    ExtractClassMethod(FTests[i],Cn,MN);
    If (CN<>CCN) then
      begin
      if (CCN<>'') then
        EndTestClassImpl(C,CN);
      StartTestClassImpl(C,CN);
      CCN:=CN;
      end;
    AddMethodImpl(C,CN,MN,F);
    end;
  if (CCN<>'') then
    EndTestClassImpl(C,CN);
end;

procedure TFPTestCodeCreator.CreateTestCode(Dest : TStream; Const InputUnitName : string);

  Function GetTestClassNames : String;

  Var
    L : TStringList;
    i : Integer;
    CN,MN : String;

  begin
    L:=TStringList.Create;
    try
      L.Sorted:=True;
      L.Duplicates:=dupIgnore;
      For I:=0 to Tests.Count-1 do
        begin
        Self.ExtractClassMethod(Tests[i],CN,MN);
        L.Add(CN);
        end;
      Result:=L.CommaText;
    finally
      L.free;
    end;
  end;

Var
  C : TStrings;
  S : String;

begin
  C:=TStringList.Create;
  try
    If (coCreateUnit in CodeOptions) then
      begin
      C.Add(Format('unit %s;',[DestUnitName]));
      C.Add('');
      C.Add('interface');
      C.Add('');
      C.Add(Format('Uses Classes, SysUtils, fpcunit, testutils, testregistry, %s;',[InputUnitName]));
      C.Add('');
      C.Add('Type');
      end;
    If (coCreateDeclaration in CodeOptions) then
      CreateInterfaceCode(C);
    if (coImplementation in CodeOptions) then
      begin
      If (coCreateUnit in CodeOptions) then
        begin
        C.Add('');
        C.Add('implementation');
        C.Add('');
        end;
      CreateImplementationCode(C);
      If (coCreateUnit in CodeOptions) then
        begin
        C.Add('');
        if coRegisterTests in CodeOptions then
          begin
          S:=GetTestClassNames;
          C.Add('Initialization');
          C.Add(Format('  RegisterTests([%s]);',[S]));
          end;
        C.Add('end.');
        end;
      end;
    C.SaveToStream(Dest);
  finally
    C.Free;
  end;
end;

procedure TFPTestCodeCreator.CreateTests(M: TPasModule; Dest: TStream);

begin
  FTests:=TStringList.Create;
  try
    DoCreateTests(M);
    CreateTestCode(Dest,M.Name);
  finally
    FTests.Free;
  end;
end;

Function TFPTestCodeCreator.ParseSource(const ASourceStream : TStream) : TPasModule;

Var
  R : TStreamResolver;
  S : TPascalScanner;
  P : TPasParser;
  M : TPasModule;
  C : TTestContainer;

begin
  R:=TStreamResolver.Create;
  try
    R.AddStream('file.pp',ASourceStream);
    S:=TPascalScanner.Create(R);
    try
      S.OpenFile('file.pp');
      C:=TTestContainer.Create;
      try
        C.InterfaceOnly:=True;
        P:=TPasParser.Create(S,R,C);
        try
          P.ParseMain(Result);
        finally
          P.Free;
        end;
      finally
        C.Free;
      end;
    finally
      S.Free;
    end;
  finally
    R.Free;
  end;
end;

procedure TFPTestCodeCreator.Execute(const ASourceStream, ADestStream: TStream);

Var
  M : TPasModule;

begin
  M:=ParseSource(ASourceStream);
  try
    if Assigned(M) then
      CreateTests(M,ADestStream);
  finally
    M.Free;
  end;
end;

procedure TFPTestCodeCreator.Execute(const ASourceCode, ADestCode: TStrings);

Var
  MIn,Mout : TStringStream;

begin
  Min:=TStringStream.Create(ASourceCode.Text);
  try
    Mout:=TStringstream.Create('');
    try
      Min.Position:=0;
      Execute(Min,Mout);
      Mout.Position:=0;
      ADestCode.Text:=Mout.DataString;
    finally
      Mout.free;
    end;
  finally
    Min.Free;
  end;
end;

{ TTestContainer }

function TTestContainer.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result:=AClass.Create(AName,AParent);
  Result.Visibility:=AVisibility;
  Result.SourceFilename:=ASourceFileName;
  Result.SourceLinenumber:=ASourceLineNumber;
end;

function TTestContainer.FindElement(const AName: String): TPasElement;
begin
  Result:=Nil;
end;

end.

