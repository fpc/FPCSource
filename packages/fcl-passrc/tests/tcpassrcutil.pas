unit tcpassrcutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils,passrcutil, testregistry;

type

  { TPasSrcUtilTest }

  TPasSrcUtilTest= class(TTestCase)
  Protected
    FAnalyser : TPasSrcAnalysis;
    FSrc : TStrings;
    FList : TStrings;
    FStream: TmemoryStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure AddLine(Const ALine : String);
    Procedure AddUses(Const AUsesList : String);
    Procedure StartUnit;
    Procedure StartImplementation;
    Procedure EndSource;
    Procedure AssertList(Msg : String; Els : Array of string);
    Property Analyser : TPasSrcAnalysis Read FAnalyser;
    Property List : TStrings Read FList;
  published
    procedure TestGetInterfaceUses;
    procedure TestGetInterfaceUsesEmpty;
    procedure TestGetImplementationUses;
    procedure TestGetImplementationUsesEmpty;
    procedure TestGetAllUses;
    procedure TestGetInterfaceIdentifiers;
    procedure TestGetInterfaceVarIdentifiers;
    procedure TestGetInterface2VarIdentifiers;
    procedure TestGetInterfaceConstIdentifiers;
    procedure TestGetInterface2ConstsIdentifiers;
    procedure TestGetInterfaceTypeIdentifiers;
    procedure TestGetInterface2TypeIdentifiers;
    procedure TestGetInterfaceProcIdentifiers;
    procedure TestGetInterfaceResourcestringIdentifiers;
    procedure TestGetInterfaceEnumTypeIdentifiersNoRecurse;
    procedure TestGetInterfaceEnumTypeIdentifiersRecurse;
    procedure TestGetInterfaceRecordTypeIdentifiersNoRecurse;
    procedure TestGetInterfaceRecordTypeIdentifiersRecurse;
    procedure TestGetInterfaceRecordTypeIdentifiersRecurseVariant;
    procedure TestGetInterfaceClassTypeIdentifiersNoRecurse;
    procedure TestGetInterfaceClassTypeIdentifiersRecurse;
    procedure TestGetImplementationVarIdentifiers;
    procedure TestInterfaceHasResourceStrings;
    procedure TestInterfaceHasResourceStringsFalse;
    procedure TestImplementationHasResourceStrings;
    procedure TestHasResourceStrings;
    procedure TestHasResourceStrings2;
    procedure TestHasResourceStrings3;
    procedure TestHasResourceStrings4;
  end;

implementation

procedure TPasSrcUtilTest.TestGetInterfaceUses;
begin
  StartUnit;
  AddUses('a,b,c');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceUnits(List);
  AssertList('4 interface units',['System','a','b','c']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceUsesEmpty;
begin
  StartUnit;
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceUnits(List);
  AssertList('0 interface units',[]);
end;

procedure TPasSrcUtilTest.TestGetImplementationUses;
begin
  StartUnit;
  StartImplementation;
  AddUses('d,a,b,c');
  EndSource;
  Analyser.GetImplementationUnits(List);
  AssertList('4 implementation units',['d','a','b','c']);
end;

procedure TPasSrcUtilTest.TestGetImplementationUsesEmpty;
begin
  StartUnit;
  StartImplementation;
  EndSource;
  Analyser.GetImplementationUnits(List);
  AssertList('0 implementation units',[]);
end;

procedure TPasSrcUtilTest.TestGetAllUses;
begin
  StartUnit;
  AddUses('a,b,c');
  StartImplementation;
  AddUses('d,e');
  EndSource;
  Analyser.GetUsedUnits(List);
  AssertList('6 units',['System','a','b','c','d','e']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceIdentifiers;
begin
  StartUnit;
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('0 identifiers',[]);
end;

procedure TPasSrcUtilTest.TestGetInterfaceVarIdentifiers;
begin
  StartUnit;
  AddLine('Var a : integer;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('1 identifiers',['a']);
end;

procedure TPasSrcUtilTest.TestGetInterface2VarIdentifiers;
begin
  StartUnit;
  AddLine('Var a,b : integer;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('2 identifiers',['a','b']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceConstIdentifiers;
begin
  StartUnit;
  AddLine('Const a = 123;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('1 identifiers',['a']);
end;

procedure TPasSrcUtilTest.TestGetInterface2ConstsIdentifiers;
begin
  StartUnit;
  AddLine('Const a = 123;');
  AddLine(' b = 123;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('2 identifiers',['a','b']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceTypeIdentifiers;
begin
  StartUnit;
  AddLine('Type a = Integer;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('1 identifiers',['a']);
end;

procedure TPasSrcUtilTest.TestGetInterface2TypeIdentifiers;
begin
  StartUnit;
  AddLine('Type a = Integer;');
  AddLine(' b = Word;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('2 identifiers',['a','b']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceProcIdentifiers;
begin
  StartUnit;
  AddLine('Procedure a (b : Integer);');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('1 identifiers',['a']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceResourcestringIdentifiers;
begin
  StartUnit;
  AddLine('Resourcestring astring = ''Something'';');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('1 identifiers',['astring']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceEnumTypeIdentifiersNoRecurse;
begin
  StartUnit;
  AddLine('Type aenum = (one,two,three);');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List);
  AssertList('1 identifiers',['aenum']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceEnumTypeIdentifiersRecurse;
begin
  StartUnit;
  AddLine('Type aenum = (one,two,three);');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List,True);
  AssertList('4 identifiers',['aenum','aenum.one','aenum.two','aenum.three']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceRecordTypeIdentifiersNoRecurse;
begin
  StartUnit;
  AddLine('Type arec = record one,two,three : integer; end;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List,False);
  AssertList('1 identifier',['arec']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceRecordTypeIdentifiersRecurse;
begin
  StartUnit;
  AddLine('Type arec = record one,two,three : integer; end;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List,True);
  AssertList('4 identifiers',['arec','arec.one','arec.two','arec.three']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceRecordTypeIdentifiersRecurseVariant;
begin
  StartUnit;
  AddLine('Type arec = record one,two,three : integer; case integer of 1: (x : integer;); end;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List,True);
  AssertList('4 identifiers',['arec','arec.one','arec.two','arec.three','arec.x']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceClassTypeIdentifiersNoRecurse;
begin
  StartUnit;
  AddLine('Type TMyClass = Class');
  AddLine('   one,two,three : integer;');
  AddLine('end;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List,False);
  AssertList('4 identifiers',['TMyClass']);
end;

procedure TPasSrcUtilTest.TestGetInterfaceClassTypeIdentifiersRecurse;
begin
  StartUnit;
  AddLine('Type TMyClass = Class');
  AddLine('   one,two,three : integer;');
  AddLine('end;');
  StartImplementation;
  EndSource;
  Analyser.GetInterfaceIdentifiers(List,True);
  AssertList('4 identifiers',['TMyClass','TMyClass.one','TMyClass.two','TMyClass.three']);
end;

procedure TPasSrcUtilTest.TestGetImplementationVarIdentifiers;
begin
  StartUnit;
  StartImplementation;
  AddLine('Var a : integer;');
  EndSource;
  Analyser.GetImplementationIdentifiers(List);
  AssertList('1 identifiers',['a']);
end;

procedure TPasSrcUtilTest.TestInterfaceHasResourceStrings;
begin
  StartUnit;
  AddLine('Resourcestring astring = ''Something'';');
  StartImplementation;
  EndSource;
  AssertEquals('Have res. strings',True,Analyser.InterfaceHasResourcestrings)
end;

procedure TPasSrcUtilTest.TestInterfaceHasResourceStringsFalse;
begin
  StartUnit;
  StartImplementation;
  AddLine('Resourcestring astring = ''Something'';');
  EndSource;
  AssertEquals('Have no res. strings',False,Analyser.InterfaceHasResourcestrings)
end;

procedure TPasSrcUtilTest.TestImplementationHasResourceStrings;
begin
  StartUnit;
  StartImplementation;
  AddLine('Resourcestring astring = ''Something'';');
  EndSource;
  AssertEquals('Have res. strings',True,Analyser.ImplementationHasResourcestrings)
end;

procedure TPasSrcUtilTest.TestHasResourceStrings;
begin
  StartUnit;
  StartImplementation;
  EndSource;
  AssertEquals('No res. strings',False,Analyser.ImplementationHasResourcestrings)
end;

procedure TPasSrcUtilTest.TestHasResourceStrings2;
begin
  StartUnit;
  AddLine('Resourcestring astring = ''Something'';');
  StartImplementation;
  EndSource;
  AssertEquals('Have  res. strings',True,Analyser.HasResourcestrings)
end;

procedure TPasSrcUtilTest.TestHasResourceStrings3;
begin
  StartUnit;
  AddLine('Resourcestring astring = ''Something'';');
  StartImplementation;
  EndSource;
  AssertEquals('Have  res. strings',True,Analyser.HasResourcestrings)
end;

procedure TPasSrcUtilTest.TestHasResourceStrings4;
begin
  StartUnit;
  AddLine('Resourcestring astring = ''Something'';');
  StartImplementation;
  AddLine('Resourcestring astring2 = ''Something'';');
  EndSource;
  AssertEquals('Have  res. strings',True,Analyser.HasResourcestrings)
end;

procedure TPasSrcUtilTest.SetUp;
begin
  FAnalyser:=TPasSrcAnalysis.Create(Nil);
  FSrc:=TStringList.Create;
  FList:=TStringList.Create;
  FStream:=TMemoryStream.Create;
  FAnalyser.FileName:='atest.pp';
  FAnalyser.Stream:=FStream;
end;

procedure TPasSrcUtilTest.TearDown;
begin
  FreeAndNil(FAnalyser);
  FreeAndNil(FStream);
  FreeAndNil(FSrc);
  FreeAndNil(FList);
end;

procedure TPasSrcUtilTest.AddLine(const ALine: String);
begin
  FSrc.Add(ALine);
end;

procedure TPasSrcUtilTest.AddUses(const AUsesList: String);
begin
  AddLine('uses '+AUseslist+';');
  AddLine('');
end;

procedure TPasSrcUtilTest.StartUnit;
begin
  AddLine('unit atest;');
  AddLine('');
  AddLine('Interface');
  AddLine('');
end;

procedure TPasSrcUtilTest.StartImplementation;
begin
  AddLine('');
  AddLine('Implementation');
  AddLine('');
end;

procedure TPasSrcUtilTest.EndSource;
begin
  AddLine('');
  AddLine('end.');
  FSrc.SaveToStream(FStream);
  FStream.Position:=0;
  Writeln('// Test name : ',Self.TestName);
  Writeln(FSrc.Text);
end;

procedure TPasSrcUtilTest.AssertList(Msg: String; Els: array of string);

Var
  I : Integer;

begin
  AssertEquals(Msg+': number of elements',Length(Els),List.Count);
  For I:=Low(Els) to High(Els) do
    AssertEquals(Msg+': list element '+IntToStr(i)+' matches : ',Els[i],List[i]);
end;


initialization

  RegisterTest(TPasSrcUtilTest);
end.

