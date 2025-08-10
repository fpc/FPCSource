unit utcIniFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, inifiles;

procedure RegisterTests;

implementation

var
  Fini: TCustomIniFile;

function Setup: TTestString;
begin
  Result := '';
  if Assigned(Fini) then
    Fini.Free;
  Fini := nil;
  if FileExists('tmp.ini') then
    DeleteFile('tmp.ini');
  try
    Fini := TMemIniFile.Create('tmp.ini');
  except
    on E: Exception do
      Result := 'Setup failed: ' + E.Message;
  end;
end;

function TearDown: TTestString;
begin
  Result := '';
  if Assigned(Fini) then
    Fini.Free;
  Fini := nil;
  if FileExists('tmp.ini') then
    DeleteFile('tmp.ini');
end;

function TIniFile_TestWriteBoolean: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  Fini.WriteBool('a','b',true);
  AssertEquals('Default true','1',Fini.ReadString('a','b',''));
  Fini.WriteBool('a','b',False);
  AssertEquals('Default false','0',Fini.ReadString('a','b',''));
  Fini.Options:=Fini.Options+[ifoWriteStringBoolean];
  Fini.WriteBool('a','b',true);
  AssertEquals('Default string true','true',Fini.ReadString('a','b',''));
  Fini.WriteBool('a','b',false);
  AssertEquals('Default string false','false',Fini.ReadString('a','b',''));
  Fini.SetBoolStringValues(true,['t','true']);
  Fini.WriteBool('a','b',true);
  AssertEquals('True from string array','t',Fini.ReadString('a','b',''));
  Fini.SetBoolStringValues(false,['f','false']);
  Fini.WriteBool('a','b',false);
  AssertEquals('False from string array','f',Fini.ReadString('a','b',''));
end;

function TIniFile_TestReadBoolean: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  Fini.WriteString('a','b','1');
  AssertEquals('Default true',true,Fini.ReadBool('a','b',False));
  Fini.WriteString('a','b','0');
  AssertEquals('Default false',false,Fini.ReadBool('a','b',True));
  Fini.WriteString('a','b','');
  AssertEquals('Empty returns Default ',true,Fini.ReadBool('a','b',true));
  Fini.SetBoolStringValues(true,['t','true']);
  Fini.WriteString('a','b','t');
  AssertEquals('First string match',true,Fini.ReadBool('a','b',false));
  Fini.WriteString('a','b','true');
  AssertEquals('Second string match',true,Fini.ReadBool('a','b',false));
  Fini.WriteString('a','b','d');
  AssertEquals('No string match, default',true,Fini.ReadBool('a','b',true));
  Fini.SetBoolStringValues(true,[]);
  Fini.SetBoolStringValues(false,['f','false']);
  Fini.WriteString('a','b','f');
  AssertEquals('First string match false',false,Fini.ReadBool('a','b',true));
  Fini.WriteString('a','b','false');
  AssertEquals('Second string match false',false,Fini.ReadBool('a','b',true));
  Fini.WriteString('a','b','d');
  AssertEquals('No string match, default false',false,Fini.ReadBool('a','b',false));
  Fini.SetBoolStringValues(true,['t','true']);
  AssertEquals('No string match, default false 2',false,Fini.ReadBool('a','b',false));
  Fini.SetBoolStringValues(true,[]);
  Fini.SetBoolStringValues(False,[]);
  Fini.Options:=Fini.Options+[ifoWriteStringBoolean];
  Fini.WriteString('a','b','true');
  AssertEquals('ifoWriteStringBoolean, true string ',True,Fini.ReadBool('a','b',false));
  Fini.WriteString('a','b','false');
  AssertEquals('ifoWriteStringBoolean, false string',false,Fini.ReadBool('a','b',true));
  Fini.WriteString('a','b','soso');
  AssertEquals('ifoWriteStringBoolean, No string match, default',True,Fini.ReadBool('a','b',true));
end;

procedure RegisterTests;
begin
  AddSuite('TIniFileTests', @Setup, @TearDown, Nil, True);
  AddTest('TestWriteBoolean', @TIniFile_TestWriteBoolean, 'TIniFileTests');
  AddTest('TestReadBoolean', @TIniFile_TestReadBoolean, 'TIniFileTests');
end;

end.
