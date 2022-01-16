unit tcjsonini;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, inifiles, jsonini;

Type

  { TJSONIniTest }

  TJSONIniTest = Class(TTestCase)
  private
    FFileContent: TJSONData;
    Fini: TJSONIniFile;
    FStrings: TStrings;
    FTestFile: String;
    procedure AssertValue(const aSection, Akey, avalue: string);
    procedure CreateIni;
    function GetIni: TJSONIniFile;
    function GetO: TJSONObject;
  Protected
    procedure HaveFile;
    Procedure ReLoad;
    procedure NoFileYet;
    procedure RemoveFile;
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure ReadFile;
    Procedure WriteFile;
    Procedure SampleFile;
    Property TestFile : String Read FTestFile;
    Property FileContent : TJSONData Read FFileContent Write FFileContent;
    Property ObjFileContent : TJSONObject Read GetO;
    Property Ini : TJSONIniFile Read GetIni;
    Property Strings : TStrings Read FStrings;
  Published
    Procedure TestEmpty;
    Procedure TestReadEmpty;
    Procedure TestReadEmptyValue;
    Procedure TestReadEmptyObject;
    Procedure TestRead1EmptySection;
    Procedure TestReadSections;
    procedure TestReadSection;
    procedure TestReadSectionValues;
    Procedure TestReadString;
    Procedure TestReadInteger;
    Procedure TestReadInt64;
    Procedure TestReadFloat;
    Procedure TestReadBoolean;
    Procedure TestReadDate;
    Procedure TestReadTime;
    Procedure TestReadDateTime;
    Procedure TestEraseSection;
    Procedure TestEraseSectionCaseMismatch;
    Procedure TestDeleteKey;
    Procedure TestDeleteKeySectionCaseMismatch;
    Procedure TestDeleteKeyKeyCaseMismatch;
    Procedure TestWriteString;
    Procedure TestWriteInteger;
    Procedure TestWriteBoolean;
    Procedure TestWriteDate;
    Procedure TestWriteDateTime;
    Procedure TestWriteTime;
    Procedure TestConvertIni;
    Procedure TestConvertIniString;
  end;

implementation

{ TJSONIniTest }

function TJSONIniTest.GetIni: TJSONIniFile;
begin
  If FIni=Nil then
    begin
    Fini:=TJSONIniFile.Create(TestFile);
    end;
  Result:=FIni;
end;

function TJSONIniTest.GetO: TJSONObject;
begin
  Result:=FFileContent as TJSONObject;
end;

procedure TJSONIniTest.Setup;
begin
  Inherited;
  FTestFile:=TestName+'.json';
  If FileExists(FTestFile) then
    DeleteFile(FTestFile);
  FStrings:=TStringList.Create;
  // Do nothing
end;

procedure TJSONIniTest.TearDown;
begin
  If FileExists(FTestFile) then
    DeleteFile(FTestFile);
  FreeAndNil(FFileContent);
  FreeAndNil(FIni);
  FreeAndNil(FStrings);
  Inherited;
end;

procedure TJSONIniTest.ReadFile;

Var
  F : TFileStream;

begin
  FreeAndNil(FFileContent);
  AssertTrue('Test File '+TestFile+' exists.',FileExists(TestFile));
  F:=TFileStream.Create(TestFile,fmOpenRead or fmShareDenyWrite);
  try
    FileContent:=GetJSON(F);
  finally
    F.Free;
  end;
end;

procedure TJSONIniTest.WriteFile;

Var
  F : TFileStream;
  S : TJSONStringType;

begin
  F:=TFileStream.Create(TestFile,fmCreate);
  try
    S:=FFileContent.AsJSON;
    F.WriteBuffer(S[1],Length(S));
  finally
    F.Free;
  end;
end;

procedure TJSONIniTest.SampleFile;

begin
  FileContent:=TJSONObject.Create([
    'a',TJSONObject.Create([
      'i',1,
      'i6',TJSONInt64Number.Create(Maxint*2),
      'f',1.2,
      's','test',
      'si','1',
      'si6',IntToStr(int64(MaxInt*2)),
      'sf','1.2',
      'dt','2001-05-06T23:24:25.678',
      'id',Round(EncodeDate(2001,05,06)),
      'fd',EncodeDate(2001,05,06),
      't','0000-00-00T12:13:14.567',
      'ft',Frac(EncodeTime(12,13,14,567)),
      'fdt',EncodeDate(2001,05,06)+EncodeTime(23,24,25,678),
      'd','2001-05-06',
      'b',true,
      'n',Nil,
      'o',TJSONObject.Create
    ]),
    'B',TJSONObject.Create([
      'I',1,
      'F',1.2,
      'S','test',
      'SI','1',
      'SF','1.2',
      'DT','2001-05-06T23:24:25.678',
      'T','0000-00-00T12:13:14.567',
      'D','2001-05-06',
      'B',true,
      'N',Nil,
      'O',TJSONObject.Create
    ]),
    'NO','not'
  ]);
  WriteFile;
end;

procedure TJSONIniTest.TestEmpty;
begin
  AssertFalse('No test file',FileExists(testfile));
  AssertNull('No ini',Fini);
  AssertNull('No file content',FFileContent);
  AssertNotNull('Have strings',Strings);
  AssertEquals('Have empty strings',0,Strings.Count);
end;

procedure TJSONIniTest.TestReadEmpty;
begin
  Ini.ReadSections(Strings);
  AssertEquals('No sections',0,Strings.Count);
end;

procedure TJSONIniTest.TestReadEmptyValue;
begin
  FileContent:=TJSONString.Create('me');
  WriteFile;
  Ini.ReadSections(Strings);
  AssertEquals('No sections',0,Strings.Count);
end;

procedure TJSONIniTest.TestReadEmptyObject;
begin
  FileContent:=TJSONObject.Create();
  WriteFile;
  Ini.ReadSections(Strings);
  AssertEquals('No sections',0,Strings.Count);
end;

procedure TJSONIniTest.TestRead1EmptySection;
begin
  FileContent:=TJSONObject.Create(['empty',TJSONOBject.Create]);
  WriteFile;
  Ini.ReadSections(Strings);
  AssertEquals('1 sections',1,Strings.Count);
  AssertEquals('Section name','empty',Strings[0]);
end;

procedure TJSONIniTest.TestReadSections;
begin
  SampleFile;
  Ini.ReadSections(Strings);
  AssertEquals('2 sections',2,Strings.Count);
  AssertEquals('Section name 0','a',Strings[0]);
  AssertEquals('Section name 1','B',Strings[1]);
end;

procedure TJSONIniTest.TestReadSection;
begin
  SampleFile;
  Ini.ReadSection('a',Strings);
  // Only valid values are reported
  AssertEquals('value count',(FileContent as TJSONObject).Objects['a'].Count-2,Strings.Count);
  AssertEquals('value names','i,i6,f,s,si,si6,sf,dt,id,fd,t,ft,fdt,d,b',Strings.CommaText);
end;

procedure TJSONIniTest.TestReadSectionValues;

Var
  D : TJSONEnum;

begin
  SampleFile;
  Ini.ReadSectionValues('a',Strings,[]);
  // Only valid values are reported
  AssertEquals('value count',(FileContent as TJSONObject).Objects['a'].Count-2,Strings.Count);
  for D in (FileContent as TJSONObject).Objects['a'] do
    if D.Value.JSONType in ActualValueJSONTypes then
      AssertEquals('value '+D.key,D.Value.AsString,Strings.Values[D.Key]);
  Strings.Clear;
  Ini.ReadSectionValues('a',Strings);
  // All valid values are reported
  AssertEquals('value count',(FileContent as TJSONObject).Objects['a'].Count,Strings.Count);
end;

procedure TJSONIniTest.TestReadString;
begin
  SampleFile;
  AssertEquals('Value, case OK','test',Ini.ReadString('a','s','nono'));
  AssertEquals('Value, key case not OK','test',Ini.ReadString('a','S','nono'));
  AssertEquals('Value, section case not OK','test',Ini.ReadString('A','s','nono'));
  AssertEquals('Value, section not exist','nono',Ini.ReadString('C','s','nono'));
  AssertEquals('Value, key not exist','nono',Ini.ReadString('a','Z','nono'));
  AssertEquals('Value, key not string','1',Ini.ReadString('a','i','nono'));
  AssertEquals('Value, key not valid value','nono',Ini.ReadString('a','o','nono'));
end;

procedure TJSONIniTest.TestReadInteger;

begin
  SampleFile;
  AssertEquals('Value, case OK',1,Ini.ReadInteger('a','i',2));
  AssertEquals('Value, key case not OK',1,Ini.ReadInteger('a','I',2));
  AssertEquals('Value, section case not OK',1,Ini.ReadInteger('A','i',2));
  AssertEquals('Value, section not exist',2,Ini.ReadInteger('C','i',2));
  AssertEquals('Value, key not exist',2,Ini.ReadInteger('a','Z',2));
  AssertEquals('Value, key not integer',2,Ini.ReadInteger('a','s',2));
  AssertEquals('Value, key not integer, but convertable to integer',1,Ini.ReadInteger('a','si',2));
end;

procedure TJSONIniTest.TestReadInt64;
Var
  I6 : Int64;
begin
  I6:=MaxInt*2;
  SampleFile;
  AssertEquals('Value, case OK',i6,Ini.ReadInt64('a','i6',2));
  AssertEquals('Value, key case not OK',i6,Ini.ReadInt64('a','I6',2));
  AssertEquals('Value, section case not OK',i6,Ini.ReadInt64('A','i6',2));
  AssertEquals('Value, section not exist',2,Ini.ReadInt64('C','i',2));
  AssertEquals('Value, key not exist',2,Ini.ReadInt64('a','Z',2));
  AssertEquals('Value, key not integer',2,Ini.ReadInt64('a','s',2));
  AssertEquals('Value, key not integer, but convertable to int64',I6,Ini.ReadInt64('a','si6',2));
end;

procedure TJSONIniTest.TestReadFloat;
begin
  SampleFile;
  AssertEquals('Value, case OK',1.2,Ini.ReadFloat('a','f',2.3));
  AssertEquals('Value, key case not OK',1.2,Ini.ReadFloat('a','F',2.3));
  AssertEquals('Value, section case not OK',1.2,Ini.ReadFloat('A','f',2.3));
  AssertEquals('Value, section not exist',2.3,Ini.ReadFloat('C','f',2.3));
  AssertEquals('Value, key not exist',2.3,Ini.ReadFloat('a','Z',2.3));
  AssertEquals('Value, key not float',2.3,Ini.ReadFloat('a','s',2.3));
  AssertEquals('Value, key not float, but convertable to float',1.2,Ini.ReadFloat('a','sf',2.3));
end;

procedure TJSONIniTest.TestReadBoolean;
begin
  SampleFile;
  AssertEquals('Value, case OK',True,Ini.ReadBool('a','b',False));
  AssertEquals('Value, key case not OK',True,Ini.ReadBool('a','B',False));
  AssertEquals('Value, section case not OK',True,Ini.ReadBool('A','b',False));
  AssertEquals('Value, section not exist',True,Ini.ReadBool('C','b',True));
  AssertEquals('Value, key not exist',True,Ini.ReadBool('a','Z',True));
  AssertEquals('Value, key not bool but integer',True,Ini.ReadBool('a','i',false));
end;

procedure TJSONIniTest.TestReadDate;

Var
  D,DD : TDateTime;

begin
  D:=EncodeDate(2001,05,06);
  DD:=EncodeDate(1999,11,12);
  SampleFile;
  AssertEquals('Value, case OK',D,Ini.ReadDate('a','d',DD));
  AssertEquals('Value, key case not OK',D,Ini.ReadDate('a','D',DD));
  AssertEquals('Value, section case not OK',D,Ini.ReadDate('A','d',DD));
  AssertEquals('Value, section not exist',DD,Ini.ReadDate('C','d',DD));
  AssertEquals('Value, date as integer',D,Ini.ReadDate('a','id',DD));
  AssertEquals('Value, date as float',D,Ini.ReadDate('a','fd',DD));
end;

procedure TJSONIniTest.TestReadTime;

Var
  T,DT : TDateTime;

begin
  T:=EncodeTime(12,13,14,567);
  DT:=EncodeTime(1,2,3,4);
  SampleFile;
  AssertEquals('Value, case OK',T,Ini.ReadTime('a','t',DT));
  AssertEquals('Value, key case not OK',T,Ini.ReadTime('a','T',DT));
  AssertEquals('Value, section case not OK',T,Ini.ReadTime('A','t',DT));
  AssertEquals('Value, section not exist',DT,Ini.ReadTime('C','t',DT));
  AssertEquals('Value, key exist as float',T,Ini.ReadTime('a','ft',DT));
end;

procedure TJSONIniTest.TestReadDateTime;
Var
  DT,DDT : TDateTime;

begin
  DT:=EncodeDate(2001,05,06)+EncodeTime(23,24,25,678);
  DDT:=EncodeDate(1999,11,12)+EncodeTime(1,2,3,4);
  SampleFile;
  AssertEquals('Value, case OK',DT,Ini.ReadDateTime('a','dt',DDT));
  AssertEquals('Value, key case not OK',DT,Ini.ReadDateTime('a','DT',DDT));
  AssertEquals('Value, section case not OK',DT,Ini.ReadDateTime('A','dt',DDT));
  AssertEquals('Value, section not exist',DDT,Ini.ReadDateTime('C','dt',DDT));
  AssertEquals('Value, key exist as float',DT,Ini.ReadDateTime('a','fdt',DDT));
end;

procedure TJSONIniTest.TestEraseSection;
begin
  SampleFile;
  Ini.EraseSection('B');
  Ini.UpdateFile;
  ReadFile;
  AssertEquals('No more section',-1,ObjFileContent.IndexOfName('B'));
end;

procedure TJSONIniTest.TestEraseSectionCaseMismatch;
begin
  SampleFile;
  Ini.EraseSection('b');
  Ini.UpdateFile;
  ReadFile;
  AssertEquals('No more section',-1,ObjFileContent.IndexOfName('B'));
end;

procedure TJSONIniTest.TestDeleteKey;
begin
  SampleFile;
  Ini.DeleteKey('a','i');
  Ini.UpdateFile;
  ReadFile;
  AssertEquals('No more key',-1,ObjFileContent.Objects['a'].IndexOfName('i'));
end;

procedure TJSONIniTest.TestDeleteKeySectionCaseMismatch;
begin
  SampleFile;
  Ini.DeleteKey('A','i');
  Ini.UpdateFile;
  ReadFile;
  AssertEquals('No more key',-1,ObjFileContent.Objects['a'].IndexOfName('i'));
end;

procedure TJSONIniTest.TestDeleteKeyKeyCaseMismatch;
begin
  SampleFile;
  Ini.DeleteKey('a','I');
  Ini.UpdateFile;
  ReadFile;
  AssertEquals('No more key',-1,ObjFileContent.Objects['a'].IndexOfName('i'));
end;

procedure TJSONIniTest.AssertValue(const aSection,Akey,avalue : string);

Var
  D : TJSONData;

begin
  ini.UpdateFile;
  ReadFile;
  D:=ObjFileContent.FindPath(asection+'.'+akey);
  AssertNotNull('Have value at '+asection+'.'+akey,D);
  AssertEquals('Correct value at '+asection+'.'+akey,AValue,D.AsString);
end;

procedure TJSONIniTest.NoFileYet;

begin
  AssertFalse('File not exist yet',FileExists(TestFile));
end;

procedure TJSONIniTest.HaveFile;

begin
  AssertTrue('Test file exists',FileExists(TestFile));
end;

procedure TJSONIniTest.ReLoad;
begin
  FreeAndNil(Fini);
  AssertNotNull(Ini);
end;

procedure TJSONIniTest.RemoveFile;
begin
  if FileExists(TestFile) then
    AssertTrue('Deleted file',DeleteFile(TestFile));
end;

procedure TJSONIniTest.TestWriteString;
begin
  Ini.WriteString('a','i','string');
  NoFileYet;
  AssertValue('a','i','string');
  Ini.CacheUpdates:=False;
  Ini.WriteString('a','i','string2');
  HaveFile;
  AssertValue('a','i','string2');
  Reload;
  AssertEquals('Can read value','string2',Ini.ReadString('a','i',''));
end;

procedure TJSONIniTest.TestWriteInteger;
begin
  Ini.Writeinteger('a','i',2);
  NoFileYet;
  AssertValue('a','i','2');
  Ini.CacheUpdates:=False;
  Ini.WriteInteger('a','i',3);
  HaveFile;
  AssertValue('a','i','3');
  Reload;
  AssertEquals('Can read value',3,Ini.ReadInteger('a','i',0));
end;

procedure TJSONIniTest.TestWriteBoolean;
begin
  Ini.WriteBool('a','i',true);
  NoFileYet;
  AssertValue('a','i','True');
  Ini.CacheUpdates:=False;
  Ini.WriteBool('a','i2',true);
  HaveFile;
  AssertValue('a','i2','True');
  Reload;
  AssertEquals('Can read value',True,Ini.ReadBool('a','i2',false));
end;

procedure TJSONIniTest.TestWriteDate;
Var
  D : TDateTime;
begin
  D:=EncodeDate(2001,2,3);
  Ini.WriteDate('a','i',D);
  NoFileYet;
  AssertValue('a','i','2001-02-03T00:00:00.000');
  Ini.CacheUpdates:=False;
  Ini.WriteDate('a','i',D+1);
  HaveFile;
  AssertValue('a','i','2001-02-04T00:00:00.000');
  Reload;
  AssertEquals('Can read value',D+1,Ini.ReadDate('a','i',0));
end;

procedure TJSONIniTest.TestWriteDateTime;

Var
  D : TDateTime;

begin
  D:=EncodeDate(2001,2,3)+EncodeTime(12,13,14,567);
  Ini.WriteDateTime('a','i',D);
  NoFileYet;
  AssertValue('a','i','2001-02-03T12:13:14.567');
  Ini.CacheUpdates:=False;
  Ini.WriteDateTime('a','i',D+1);
  HaveFile;
  AssertValue('a','i','2001-02-04T12:13:14.567');
  Reload;
  AssertEquals('Can read value',D+1,Ini.ReadDateTime('a','i',0));
end;

procedure TJSONIniTest.TestWriteTime;

Var
  D,D2 : TDateTime;

begin
  D:=EncodeTime(12,13,14,567);
  D2:=EncodeTime(13,14,15,678);
  Ini.WriteTime('a','i',D);
  NoFileYet;
  AssertValue('a','i','0000-00-00T12:13:14.567');
  Ini.CacheUpdates:=False;
  Ini.WriteTime('a','i',D2);
  HaveFile;
  AssertValue('a','i','0000-00-00T13:14:15.678');
  Reload;
  AssertEquals('Can read value',D2,Ini.ReadTime('a','i',0));
end;

procedure TJSONIniTest.CreateIni;

Var
  M : TMemIniFile;
  D,DT,T : TDateTime;

begin
  D:=EncodeDate(2001,2,3);
  T:=EncodeTime(12,13,14,567);
  DT:=D+T;
  if FileExists(TestName+'.ini') then
    DeleteFile(TestName+'.ini');
  M:=TMemIniFile.Create(TestName+'.ini');
  try
    M.WriteString('a','s','c');
    M.WriteInteger('a','i',2);
    M.WriteBool('a','b',True);
    M.WriteInt64('a','i6',Maxint*2);
    M.WriteDate('a','d',D);
    M.WriteTime('a','t',T);
    M.WriteDateTime('a','dt',DT);
    M.WriteFloat('a','f',1.23);
    M.UpdateFile;
  finally
    M.Free;
  end;
end;

procedure TJSONIniTest.TestConvertIni;

Var
  D,DT,T : TDateTime;

begin
  D:=EncodeDate(2001,2,3);
  T:=EncodeTime(12,13,14,567);
  DT:=D+T;
  CreateIni;
  TJSONIniFile.ConvertIni(TestName+'.ini',TestFile,False);
  AssertEquals('String','c',Ini.ReadString('a','s',''));
  AssertEquals('Integer',2,Ini.ReadInteger('a','i',1));
  AssertEquals('Bool',True,Ini.ReadBool('a','b',False));
  AssertEquals('Int64',Int64(Maxint*2),Ini.ReadInt64('a','i6',Maxint*2));
  AssertEquals('Date',D, Ini.ReadDate('a','d',0));
  AssertEquals('Time',T,Ini.ReadTime('a','t',0));
  AssertEquals('DateTime',DT,Ini.ReadDateTime('a','dt',0));
  AssertEquals('Float',1.23,Ini.ReadFloat('a','f',0));
  if FileExists(TestName+'.ini') then
    DeleteFile(TestName+'.ini');
end;

procedure TJSONIniTest.TestConvertIniString;
Var
  D,DT,T : TDateTime;

begin
  D:=EncodeDate(2001,2,3);
  T:=EncodeTime(12,13,14,567);
  DT:=D+T;
  CreateIni;
  TJSONIniFile.ConvertIni(TestName+'.ini',TestFile,True);
  AssertEquals('String','c',Ini.ReadString('a','s',''));
  AssertEquals('Integer',2,Ini.ReadInteger('a','i',1));
  AssertEquals('Bool',True,Ini.ReadBool('a','b',False));
  AssertEquals('Int64',Int64(Maxint*2),Ini.ReadInt64('a','i6',Maxint*2));
  AssertEquals('Date',DateToStr(D), Ini.ReadString('a','d',''));
  AssertEquals('Time',TimeToStr(T),Ini.ReadString('a','t',''));
  AssertEquals('DateTime',DateTimeToStr(DT),Ini.ReadString('a','dt',''));
  AssertEquals('Float',1.23,Ini.ReadFloat('a','f',0));
  if FileExists(TestName+'.ini') then
    DeleteFile(TestName+'.ini');
end;

initialization
  RegisterTest(TJSONIniTest);
end.

