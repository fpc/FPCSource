unit tcinifile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, inifiles, testregistry;

type

  { TTestIniFile }

  TTestIniFile= class(TTestCase)
  private
    Fini: TCustomIniFile;
  protected
    Procedure CreateIni;
    procedure SetUp; override;
    procedure TearDown; override;
    Property Ini : TCustomIniFile Read Fini;
  published
    procedure TestWriteBoolean;
    procedure TestReadBoolean;
  end;

implementation

procedure TTestIniFile.CreateIni;

begin
  Fini:=TMemIniFIle.Create('tmp.ini');
end;

procedure TTestIniFile.TestWriteBoolean;

begin
  CreateIni;
  Ini.WriteBool('a','b',true);
  AssertEquals('Default true','1',Ini.ReadString('a','b',''));
  Ini.WriteBool('a','b',False);
  AssertEquals('Default false','0',Ini.ReadString('a','b',''));
  Ini.Options:=Ini.Options+[ifoWriteStringBoolean];
  Ini.WriteBool('a','b',true);
  AssertEquals('Default string true','true',Ini.ReadString('a','b',''));
  Ini.WriteBool('a','b',false);
  AssertEquals('Default string false','false',Ini.ReadString('a','b',''));
  Ini.SetBoolStringValues(true,['t','true']);
  Ini.WriteBool('a','b',true);
  AssertEquals('True from string array','t',Ini.ReadString('a','b',''));
  Ini.SetBoolStringValues(false,['f','false']);
  Ini.WriteBool('a','b',false);
  AssertEquals('True from string array','f',Ini.ReadString('a','b',''));
end;

procedure TTestIniFile.TestReadBoolean;
begin
  CreateIni;
  Ini.WriteString('a','b','1');
  AssertEquals('Default true',true,Ini.ReadBool('a','b',False));
  Ini.WriteString('a','b','0');
  AssertEquals('Default false',false,Ini.ReadBool('a','b',True));
  Ini.WriteString('a','b','');
  AssertEquals('Empty returns Default ',true,Ini.ReadBool('a','b',true));
  Ini.SetBoolStringValues(true,['t','true']);
  Ini.WriteString('a','b','t');
  AssertEquals('First string match',true,Ini.ReadBool('a','b',false));
  Ini.WriteString('a','b','true');
  AssertEquals('Second string match',true,Ini.ReadBool('a','b',false));
  Ini.WriteString('a','b','d');
  AssertEquals('No string match, default',true,Ini.ReadBool('a','b',true));
  Ini.SetBoolStringValues(true,[]);
  Ini.SetBoolStringValues(false,['f','false']);
  Ini.WriteString('a','b','f');
  AssertEquals('First string match',false,Ini.ReadBool('a','b',true));
  Ini.WriteString('a','b','false');
  AssertEquals('Second string match',false,Ini.ReadBool('a','b',true));
  Ini.WriteString('a','b','d');
  AssertEquals('No string match, default',false,Ini.ReadBool('a','b',false));
  Ini.SetBoolStringValues(true,['t','true']);
  AssertEquals('No string match, default',false,Ini.ReadBool('a','b',false));
end;

procedure TTestIniFile.SetUp;
begin
  DeleteFile('tmp.ini');
end;

procedure TTestIniFile.TearDown;
begin
  DeleteFile('tmp.ini');
end;

initialization

  RegisterTest(TTestIniFile);
end.

