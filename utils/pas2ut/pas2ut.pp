{
    This file is part of the Free Pascal project
    Copyright (c) 2012 by the Free Pascal team

    Pascal source to FPC Unit test generator program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program pas2ut;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, pastounittest, pastree,CustApp;

Resourcestring
   SErrNoInput = 'Error: No input file specified';

   SHelp0   = 'Usage : pp2ut [options] inputfile [outputfile]';
   SHelp1   = 'Where options is one or more of';
   SHelp2   = '--help                   this help';
   SHelp10  = '--test-protected         also generate tests for protected class members' ;
   SHelp20  = '--skip-default           skip tests for default visibility members' ;
   SHelp30  = '--skip-published         skip tests for published members' ;
   SHelp40  = '--skip-public            skip tests for public members';
   SHelp50  = '--tiopf                  tiopf tests  (default,bounds,required,notify,maxlen)' ;
   SHelp60  = '--skip-property-default  generate a default test for each property' ;
   SHelp70  = '--test-property-bounds   generate a GetBounds test for each property' ;
   SHelp80  = '--test-property-required generate a Required test for each property' ;
   SHelp90  = '--test-property-notify   generate a notify test for each property' ;
   SHelp100 = '--test-property-maxlen   generate a maxlen test for each property' ;
   SHelp105 = '--skip-declaration       Do not generate declarations for the tests' ;
   SHelp110 = '--skip-implementation    Do not generate (empty) implementation for the tests' ;
   SHelp120 = '--skip-fail              Skip fail() statement in test implementations ' ;
   SHelp130 = '--skip-unit              Do not generate a unit' ;
   SHelp140 = '--skip-setup             Skip TestCase class Setup() method' ;
   SHelp150 = '--skip-teardown          Skip testcase class TearDown() method' ;
   SHelp160 = '--skip-functions         Skip tests for functions/procedures' ;
   SHelp170 = '--skip-classes           Skip tests for classes' ;
   SHelp180 = '--skip-register          Do not generate RegisterTests statement' ;
   SHelp190 = '--singletestclass        Use a single test class' ;
   SHelp200 = '--skip-methods           Skip tests for methods of classes' ;
   SHelp210 = '--skip-fields            Skip tests for fields of classes';
   SHelp220 = '--skip-properties        Skip tests for properties of classes ' ;
   SHelp230 = '--testparentname=name    Set the name of the parent class of test classes' ;
   SHelp240 = '--testunitname=name      Set the name of the generated unit (default is taken from output file name)' ;
   SHelp250 = '--failmessage=Msg        Set the message for the Fail() statement ' ;
   SHelp260 = '--unittestclassname=name Set the global unit test class name' ;
   SHelp270 = '--prefix=name            Set the prefix for the test names (default is "Test") ' ;
   SHelp280 = '--limit=list             Specify a comma-separated list of global identifiers for which to generate tests.' ;
   SHelp290 = '--defaultclasstest=list  Specify a comma-separated list of default tests for each class' ;
   SHelp400 = '--limit and --defaultclasstest may be specified multiple times.';


type
  { TPasToUnitTestApplication }

  TPasToUnitTestApplication = class(TCustomApplication)
  Private
    FCodeGen : TFPTestCodeCreator;
    FInputFile,FoutputFile : string;
    function CheckOptions : Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPasToUnitTestApplication }

function TPasToUnitTestApplication.CheckOptions : Boolean;

  Procedure ov(value : TPasMemberVisibility;incl: Boolean);

  begin
    if incl then
      FCodeGen.Visibilities:=FCodeGen.Visibilities+[value]
    else
      FCodeGen.Visibilities:=FCodeGen.Visibilities-[value]
  end;

  Procedure op(value : TTestPropertyOption;incl: Boolean);

  begin
    if incl then
      FCodeGen.PropertyOptions:=FCodeGen.PropertyOptions+[value]
    else
      FCodeGen.PropertyOptions:=FCodeGen.PropertyOptions-[value]
  end;

  Procedure oc(value : TTestCodeOption;incl: Boolean);

  begin
    if incl then
      FCodeGen.CodeOptions:=FCodeGen.CodeOptions+[value]
    else
      FCodeGen.CodeOptions:=FCodeGen.CodeOptions-[value]
  end;

  Procedure om(value : TTestMemberType;incl: Boolean);

  begin
    if incl then
      FCodeGen.MemberTypes:=FCodeGen.MemberTypes+[value]
    else
      FCodeGen.MemberTypes:=FCodeGen.MemberTypes-[value]
  end;

  Procedure AddValues(S : String; List : Tstrings);

  Var
    P : Integer;
    V : String;

  begin
    Repeat
      P:=Pos(',',S);
      If P=0 then
        P:=Length(S)+1;
      V:=Trim(Copy(S,1,P-1));
      If (V<>'') then
        List.Add(V);
      Delete(S,1,P);  
    until (S='');
  end;

Var
  S,O : string;
  I,p : Integer;

begin
  Result:=False;
  I:=1;
  While (I<=ParamCount) do
    begin
    S:=ParamStr(I);
    P:=pos('=',S);
    if (P>0) then
      begin
      O:=S;
      Delete(O,1,P);
      S:=lowercase(Copy(S,1,P-1));
      end
    else
      O:='';
    if s='--test-protected' then
      ov(visProtected,true)
    else  if s='--skip-default' then
      ov(visDefault,false)
    else  if s='--skip-published' then
      ov(visPublished,false)
    else  if s='--skip-public' then
      ov(visPublic,false)
    else if s='--tiopf' then
      begin
      FCodeGen.PropertyOptions:=[tDefault,tGetBounds,tRequired,tNotify,tMaxLen];
      end
    else if s='--skip-property-default' then
      op(tdefault,false)
    else if s='--test-property-bounds' then
      op(tgetBounds,true)
    else if s='--test-property-required' then
      op(trequired,true)
    else if s='--test-property-notify' then
      op(tNotify,true)
    else if s='--test-property-maxlen' then
      op(tMaxLen,true)
    else if s='--skip-declaration' then
      oc(coCreateDeclaration,false)
    else if s='--skip-implementation' then
      oc(coImplementation,false)
    else if s='--skip-fail' then
      oc(coDefaultFail,false)
    else if s='--skip-unit' then
      oc(coCreateUnit,false)
    else if s='--skip-setup' then
      oc(coSetup,false)
    else if s='--skip-teardown' then
      oc(coTeardown,false)
    else if s='--skip-functions' then
      oc(coFunctions,false)
    else if s='--skip-classes' then
      oc(coClasses,false)
    else if s='--skip-register' then
      oc(coRegisterTests,false)
    else if s='--singletestclass' then
      oc(coSingleClass,true)
    else if s='--skip-methods' then
      om(tmtMethods,false)
    else if s='--skip-fields' then
      om(tmtMethods,false)
    else if s='--skip-properties' then
      om(tmtMethods,false)
    else if (s='--testparentname') then
      FCodeGen.TestClassParent:=o
    else if (s='--testunitname') then
      FCodeGen.DestUnitname:=o
    else if (s='--failmessage') then
      FCodeGen.Failmessage:=o
    else if (s='--unittestclassname') then
      FCodeGen.UnitTestClassName:=O
    else if (s='--prefix') then
      FCodeGen.TestNamePrefix:=O
    else if (s='--limit') then
      AddValues(O,FCodeGen.LimitIdentifiers)
    else if (s='--defaultclasstest') then
      AddValues(O,FCodeGen.DefaultClassTests)
    else
      begin
      if (FInputFile='') then
        FInputFile:=s
      else if (FoutputFile<>'') then
        begin
        WriteHelp;
        Exit;
        end
      else
        FoutputFile:=s;
      end;
    Inc(I);
    end;
  Result:=FInputFile<>'';
  If Not Result then
    begin
    Writeln(SErrNoInput);
    WriteHelp;
    end;
  If (FOutputFile='') then
    FOutputFile:='tc'+FInputFile;
end;

procedure TPasToUnitTestApplication.DoRun;
var
  ErrorMsg: String;
begin
  Terminate;
  // parse parameters
  if HasOption('h','help') then
    begin
    WriteHelp;
    Exit;
    end;
  if CheckOptions then
    FCodeGen.Execute(FInputfile,FOutputFile);
end;

constructor TPasToUnitTestApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FCodeGen :=TFPTestCodeCreator.Create(Self)
end;

destructor TPasToUnitTestApplication.Destroy;
begin
  FreeAndNil(FCodeGen);
  inherited Destroy;
end;

procedure TPasToUnitTestApplication.WriteHelp;
begin
  Writeln(SHelp0);
  Writeln(SHelp1);
  Writeln(SHelp10 );
  Writeln(SHelp20 );
  Writeln(SHelp30 );
  Writeln(SHelp40 );
  Writeln(SHelp50 );
  Writeln(SHelp60 );
  Writeln(SHelp70 );
  Writeln(SHelp80 );
  Writeln(SHelp90 );
  Writeln(SHelp100);
  Writeln(SHelp105);
  Writeln(SHelp110);
  Writeln(SHelp120);
  Writeln(SHelp130);
  Writeln(SHelp140);
  Writeln(SHelp150);
  Writeln(SHelp160);
  Writeln(SHelp170);
  Writeln(SHelp180);
  Writeln(SHelp190);
  Writeln(SHelp200);
  Writeln(SHelp210);
  Writeln(SHelp220);
  Writeln(SHelp230);
  Writeln(SHelp240);
  Writeln(SHelp250);
  Writeln(SHelp260);
  Writeln(SHelp270);
  Writeln(SHelp280);
  Writeln(SHelp290);
  Writeln(SHelp400);
end;

var
  Application: TPasToUnitTestApplication;

begin
  Application:=TPasToUnitTestApplication.Create(nil);
  Application.Title:='Pascal code to Unit Tests';
  Application.Run;
  Application.Free;
end.

