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

// Writes aValue with WriteFloat and checks that ReadFloat returns it unchanged
procedure CheckFloatRoundTrip(const aName: string; aValue: Double);

var
  lBack: Double;

begin
  Fini.WriteFloat('a','f',aValue);
  lBack:=Fini.ReadFloat('a','f',0);
  AssertTrue(aName+' reads back unchanged from '+Fini.ReadString('a','f',''),lBack=aValue);
end;


// Writes the round-trip values with the active format settings
procedure CheckFloatRoundTrips;

var
  lValue: Double;

begin
  CheckFloatRoundTrip('0.1',0.1);
  lValue:=1;
  CheckFloatRoundTrip('1/3',lValue/3);
  CheckFloatRoundTrip('Pi',Pi);
  CheckFloatRoundTrip('-123456.789',-123456.789);
  CheckFloatRoundTrip('MaxValue',Double.MaxValue);
  CheckFloatRoundTrip('MinValue',Double.MinValue);
  CheckFloatRoundTrip('Epsilon',Double.Epsilon);
end;


function TIniFile_TestFloatRoundTrip: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  CheckFloatRoundTrips;
  Fini.Options:=Fini.Options+[ifoFormatSettingsActive];
  Fini.FormatSettings.DecimalSeparator:=',';
  Fini.FormatSettings.ThousandSeparator:='.';
  CheckFloatRoundTrips;
end;


function TIniFile_TestWriteFloatShortest: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  Fini.Options:=Fini.Options+[ifoFormatSettingsActive];
  Fini.FormatSettings.DecimalSeparator:='.';
  Fini.WriteFloat('a','f',0.1);
  AssertEquals('0.1 is written with 15 digits','0.1',Fini.ReadString('a','f',''));
  Fini.WriteFloat('a','f',1.5);
  AssertEquals('1.5 is written as before','1.5',Fini.ReadString('a','f',''));
end;


function TIniFile_TestReadFloatInvalid: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  Fini.Options:=Fini.Options+[ifoFormatSettingsActive];
  Fini.FormatSettings.DecimalSeparator:='.';
  Fini.WriteString('a','f','1.79769313486232E308');
  AssertTrue('Value above the Double range returns Default',Fini.ReadFloat('a','f',42)=42);
  Fini.WriteString('a','f','abc');
  AssertTrue('Invalid value returns Default',Fini.ReadFloat('a','f',42)=42);
  AssertTrue('Missing value returns Default',Fini.ReadFloat('a','missing',42)=42);
end;


{$ifdef FPC_HAS_TYPE_EXTENDED}
// Writes aValue with WriteExFloat and checks that ReadExFloat returns it unchanged
procedure CheckExFloatRoundTrip(const aName: string; aValue: Extended);

var
  lBack: Extended;

begin
  Fini.WriteExFloat('a','f',aValue);
  lBack:=Fini.ReadExFloat('a','f',0);
  AssertTrue(aName+' reads back unchanged from '+Fini.ReadString('a','f',''),lBack=aValue);
end;


// Writes the Extended round-trip values with the active format settings
procedure CheckExFloatRoundTrips;

var
  lValue: Extended;

begin
  CheckExFloatRoundTrip('0.1',0.1);
  lValue:=1;
  CheckExFloatRoundTrip('1/3',lValue/3);
  CheckExFloatRoundTrip('Pi',Pi);
  CheckExFloatRoundTrip('Double MaxValue',Double.MaxValue);
  CheckExFloatRoundTrip('MaxValue',Extended.MaxValue);
  CheckExFloatRoundTrip('MinValue',Extended.MinValue);
  CheckExFloatRoundTrip('Epsilon',Extended.Epsilon);
end;


function TIniFile_TestExFloatRoundTrip: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  CheckExFloatRoundTrips;
  Fini.Options:=Fini.Options+[ifoFormatSettingsActive];
  Fini.FormatSettings.DecimalSeparator:=',';
  Fini.FormatSettings.ThousandSeparator:='.';
  CheckExFloatRoundTrips;
end;


function TIniFile_TestReadExFloatInvalid: TTestString;
begin
  Result := '';
  AssertNotNull('Ini object should be created', Fini);
  if not Assigned(Fini) then Exit;

  Fini.WriteString('a','f','abc');
  AssertTrue('Invalid value returns Default',Fini.ReadExFloat('a','f',42)=42);
  AssertTrue('Missing value returns Default',Fini.ReadExFloat('a','missing',42)=42);
end;
{$endif FPC_HAS_TYPE_EXTENDED}


procedure RegisterTests;
begin
  AddSuite('TIniFileTests', @Setup, @TearDown, Nil, True);
  AddTest('TestWriteBoolean', @TIniFile_TestWriteBoolean, 'TIniFileTests');
  AddTest('TestReadBoolean', @TIniFile_TestReadBoolean, 'TIniFileTests');
  AddTest('TestFloatRoundTrip', @TIniFile_TestFloatRoundTrip, 'TIniFileTests');
  AddTest('TestWriteFloatShortest', @TIniFile_TestWriteFloatShortest, 'TIniFileTests');
  AddTest('TestReadFloatInvalid', @TIniFile_TestReadFloatInvalid, 'TIniFileTests');
{$ifdef FPC_HAS_TYPE_EXTENDED}
  AddTest('TestExFloatRoundTrip', @TIniFile_TestExFloatRoundTrip, 'TIniFileTests');
  AddTest('TestReadExFloatInvalid', @TIniFile_TestReadExFloatInvalid, 'TIniFileTests');
{$endif FPC_HAS_TYPE_EXTENDED}
end;

end.
