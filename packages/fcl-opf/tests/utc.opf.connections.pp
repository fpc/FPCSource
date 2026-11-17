unit utc.opf.connections;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry, sqldb,
{$ENDIF}
  Opf.Types, Opf.Connections;

type

  { TSQLDBConnectionDefTest }

  TSQLDBConnectionDefTest = class(TTestCase)
  private
    class procedure AssignDefValues(F: TSQLDBConnectionDef;aIndex : Integer);
    procedure Testprops(F: TSQLDBConnectionDef; aIndex : Integer);
  protected
    FDef : TSQLDBConnectionDef;
    procedure SetUp; override;
    procedure TearDown; override;
    Property Def : TSQLDBConnectionDef Read FDef;
  published
    procedure TestEmpty;
    procedure TestAssign;
    procedure TestDisplayName;
  end;

  { TSQLDBConnectionListTest }

  TSQLDBConnectionListTest = class(TTestCase)
  private
  protected
    FList : TSQLDBConnectionList;
    Procedure Fill;
    procedure SetUp; override;
    procedure TearDown; override;
    Property List : TSQLDBConnectionList Read FList;
  published
    procedure TestEmpty;
    Procedure TestAdd;
    Procedure TestFill;
    Procedure TestIndexOfConnection;
    Procedure TestFindConnection;
    Procedure TestMultiConnectionProperties;
    procedure TestSaveLoadStreamEmpty;
    procedure TestSaveLoadStreamSingle;
    procedure TestSaveLoadStreamMultiple;
    procedure TestSaveLoadStreamPreservesOrder;
    procedure TestSaveLoadFile;
    procedure TestSaveLoadIniFileSingle;
    procedure TestSaveLoadIniFileMultiple;
    procedure TestSaveLoadIniFileWithParams;
    procedure TestSaveLoadIniFileEmpty;
    procedure TestAutoDetectJsonExtension;
    procedure TestAutoDetectIniExtension;
    procedure TestNoCipherPassthrough;
    procedure TestXorCipherRoundTrip;
    procedure TestXorCipherProducesDifferentOutput;
    procedure TestXorCipherEmptyString;
    procedure TestSaveLoadJsonWithXorCipher;
    procedure TestSaveLoadIniWithXorCipher;
    procedure TestSavePreservesInMemoryPassword;
    procedure TestDoubleSaveDoesNotDoubleEncrypt;
  end;

implementation

{ TSQLDBConnectionListTest }

procedure TSQLDBConnectionListTest.Fill;
begin
  List.AddConnection('pgsql1','H1','DB1','U1','P1').Name:='C1';
  List.AddConnection('pgsql2','H2','DB2','U2','P2').Name:='C2';
  List.AddConnection('pgsql3','H3','DB3','U3','P3').Name:='C3';
end;

procedure TSQLDBConnectionListTest.SetUp;
begin
  inherited SetUp;
  FList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
end;

procedure TSQLDBConnectionListTest.TearDown;
begin
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TSQLDBConnectionListTest.TestEmpty;
begin
  AssertNotNull('Have list',List);
  AssertEquals('Empty',0,List.Count);
end;

procedure TSQLDBConnectionListTest.TestAdd;

Var
  F : TSQLDBConnectionDef;

begin
  F:=List.AddConnection('pgsql1','H1','DB1','U1','P1');
  AssertEquals('List count',1,List.Count);
  AssertSame('Field OK',F,List[0]);
  AssertEquals('Name','Connection1',F.Name);
  AssertEquals('Type','pgsql1',F.ConnectionType);
  AssertEquals('Hostname','H1',F.HostName);
  AssertEquals('DatabaseName','DB1',F.DatabaseName);
  AssertEquals('UserName','U1',F.UserName);
  AssertEquals('Password','P1',F.Password);
  AssertEquals('Port',0,F.Port);
  AssertEquals('Charset','',F.Charset);
  AssertEquals('Params',0,F.Params.Count);
end;

procedure TSQLDBConnectionListTest.TestFill;
begin
  Fill;
  AssertEquals('Count',3,List.Count);
  AssertEquals('1','DB1',List[0].DatabaseName);
  AssertEquals('2','DB2',List[1].DatabaseName);
  AssertEquals('3','DB3',List[2].DatabaseName);
end;

procedure TSQLDBConnectionListTest.TestIndexOfConnection;
begin
  Fill;
  AssertEquals('Nonexist',-1,List.IndexOfConnection('N1'));
  AssertEquals('First ',0,List.IndexOfConnection('C1'));
  AssertEquals('First CI',0,List.IndexOfConnection('C1'));
  AssertEquals('Last ',2,List.IndexOfConnection('C3'));
  AssertEquals('Last CI',2,List.IndexOfConnection('c3'));
end;

procedure TSQLDBConnectionListTest.TestFindConnection;
begin
  Fill;
  AssertSame('Nonexist',Nil,List.FindConnection('N1'));
  AssertSame('First ',List[0],List.FindConnection('C1'));
  AssertSame('First CI',List[0],List.FindConnection('c1'));
  AssertSame('Last ',List[2],List.FindConnection('C3'));
  AssertSame('Last CI',List[2],List.FindConnection('c3'));
  AssertSame('Middle, prop ',List[1],List.FindConnection('C2'));
  AssertSame('Middle, prop, CI',List[1],List.FindConnection('c2'));
end;

procedure TSQLDBConnectionListTest.TestMultiConnectionProperties;
var
  I: Integer;
  F: TSQLDBConnectionDef;
begin
  for I:=1 to 3 do
  begin
    F:=List.AddConnection('Type'+IntToStr(I), 'Host'+IntToStr(I),
      'DB'+IntToStr(I), 'User'+IntToStr(I), 'Pass'+IntToStr(I));
    F.Name:='Conn'+IntToStr(I);
    F.CharSet:='UTF'+IntToStr(I);
    F.Port:=5430+I;
    F.Params.Add('param'+IntToStr(I)+'=val'+IntToStr(I));
  end;
  AssertEquals('Count', 3, List.Count);
  for I:=0 to 2 do
  begin
    F:=List[I];
    AssertEquals('Name '+IntToStr(I), 'Conn'+IntToStr(I+1), F.Name);
    AssertEquals('Type '+IntToStr(I), 'Type'+IntToStr(I+1), F.ConnectionType);
    AssertEquals('Host '+IntToStr(I), 'Host'+IntToStr(I+1), F.HostName);
    AssertEquals('DB '+IntToStr(I), 'DB'+IntToStr(I+1), F.DatabaseName);
    AssertEquals('User '+IntToStr(I), 'User'+IntToStr(I+1), F.UserName);
    AssertEquals('Pass '+IntToStr(I), 'Pass'+IntToStr(I+1), F.Password);
    AssertEquals('Charset '+IntToStr(I), 'UTF'+IntToStr(I+1), F.CharSet);
    AssertEquals('Port '+IntToStr(I), 5431+I, F.Port);
    AssertEquals('Params '+IntToStr(I), 1, F.Params.Count);
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadStreamEmpty;
var
  DstList: TSQLDBConnectionList;
  Stream: TMemoryStream;
begin
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  Stream:=TMemoryStream.Create;
  try
    List.SaveToStream(Stream);
    Stream.Position:=0;
    DstList.LoadFromStream(Stream);
    AssertEquals('Empty count', 0, DstList.Count);
  finally
    Stream.Free;
    DstList.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadStreamSingle;
var
  DstList: TSQLDBConnectionList;
  Stream: TMemoryStream;
  F, D: TSQLDBConnectionDef;
begin
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  Stream:=TMemoryStream.Create;
  try
    F:=List.AddConnection('pgsql','H1','DB1','U1','P1');
    F.Name:='MyConn';
    F.CharSet:='UTF8';
    F.Port:=5432;
    F.Role:='admin';
    F.Params.Add('opt1=val1');
    F.Enabled:=False;
    List.SaveToStream(Stream);
    Stream.Position:=0;
    DstList.LoadFromStream(Stream);
    AssertEquals('Count', 1, DstList.Count);
    D:=DstList[0];
    AssertEquals('Name', 'MyConn', D.Name);
    AssertEquals('ConnectionType', 'pgsql', D.ConnectionType);
    AssertEquals('HostName', 'H1', D.HostName);
    AssertEquals('DatabaseName', 'DB1', D.DatabaseName);
    AssertEquals('UserName', 'U1', D.UserName);
    AssertEquals('Password', 'P1', D.Password);
    AssertEquals('CharSet', 'UTF8', D.CharSet);
    AssertEquals('Port', 5432, D.Port);
    AssertEquals('Role', 'admin', D.Role);
    AssertEquals('Params.Count', 1, D.Params.Count);
    AssertEquals('Params[0]', 'opt1=val1', D.Params[0]);
    AssertFalse('Enabled preserved', D.Enabled);
  finally
    Stream.Free;
    DstList.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadStreamMultiple;
var
  DstList: TSQLDBConnectionList;
  Stream: TMemoryStream;
  F: TSQLDBConnectionDef;
  I: Integer;
begin
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  Stream:=TMemoryStream.Create;
  try
    for I:=1 to 3 do
    begin
      F:=List.AddConnection('Type'+IntToStr(I), 'Host'+IntToStr(I),
        'DB'+IntToStr(I), 'User'+IntToStr(I), 'Pass'+IntToStr(I));
      F.Name:='Conn'+IntToStr(I);
      F.CharSet:='CS'+IntToStr(I);
      F.Port:=5430+I;
      F.Role:='Role'+IntToStr(I);
      F.Params.Add('p'+IntToStr(I)+'=v'+IntToStr(I));
    end;
    List.SaveToStream(Stream);
    Stream.Position:=0;
    DstList.LoadFromStream(Stream);
    AssertEquals('Count', 3, DstList.Count);
    for I:=0 to 2 do
    begin
      F:=DstList[I];
      AssertEquals('Name '+IntToStr(I), 'Conn'+IntToStr(I+1), F.Name);
      AssertEquals('Type '+IntToStr(I), 'Type'+IntToStr(I+1), F.ConnectionType);
      AssertEquals('Host '+IntToStr(I), 'Host'+IntToStr(I+1), F.HostName);
      AssertEquals('DB '+IntToStr(I), 'DB'+IntToStr(I+1), F.DatabaseName);
      AssertEquals('User '+IntToStr(I), 'User'+IntToStr(I+1), F.UserName);
      AssertEquals('Pass '+IntToStr(I), 'Pass'+IntToStr(I+1), F.Password);
      AssertEquals('CharSet '+IntToStr(I), 'CS'+IntToStr(I+1), F.CharSet);
      AssertEquals('Port '+IntToStr(I), 5431+I, F.Port);
      AssertEquals('Role '+IntToStr(I), 'Role'+IntToStr(I+1), F.Role);
      AssertEquals('Params '+IntToStr(I), 1, F.Params.Count);
      AssertEquals('Param val '+IntToStr(I), 'p'+IntToStr(I+1)+'=v'+IntToStr(I+1), F.Params[0]);
    end;
  finally
    Stream.Free;
    DstList.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadStreamPreservesOrder;
var
  DstList: TSQLDBConnectionList;
  Stream: TMemoryStream;
begin
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  Stream:=TMemoryStream.Create;
  try
    List.AddConnection('t1','','','','').Name:='First';
    List.AddConnection('t2','','','','').Name:='Second';
    List.AddConnection('t3','','','','').Name:='Third';
    List.SaveToStream(Stream);
    Stream.Position:=0;
    DstList.LoadFromStream(Stream);
    AssertEquals('Count', 3, DstList.Count);
    AssertEquals('Order 0', 'First', DstList[0].Name);
    AssertEquals('Order 1', 'Second', DstList[1].Name);
    AssertEquals('Order 2', 'Third', DstList[2].Name);
  finally
    Stream.Free;
    DstList.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadFile;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  F, D: TSQLDBConnectionDef;
begin
  FN:=GetTempDir + 'opf_conn_test.json';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    F:=List.AddConnection('sqlite','','testdb','','');
    F.Name:='FileConn';
    F.Port:=0;
    F.CharSet:='UTF8';
    F.Params.Add('journal=wal');
    List.SaveToFile(FN);
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    D:=DstList[0];
    AssertEquals('Name', 'FileConn', D.Name);
    AssertEquals('ConnectionType', 'sqlite', D.ConnectionType);
    AssertEquals('DatabaseName', 'testdb', D.DatabaseName);
    AssertEquals('CharSet', 'UTF8', D.CharSet);
    AssertEquals('Params.Count', 1, D.Params.Count);
    AssertEquals('Params[0]', 'journal=wal', D.Params[0]);
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadIniFileSingle;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  F, D: TSQLDBConnectionDef;
begin
  FN:=GetTempDir+'opf_conn_test.ini';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    F:=List.AddConnection('PostgreSQL','localhost','mydb','admin','secret');
    F.Name:='MyConn';
    F.CharSet:='UTF8';
    F.Port:=5432;
    F.Role:='dba';
    F.Enabled:=False;
    F.Params.Add('journal=wal');
    List.SaveToFile(FN);
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    D:=DstList[0];
    AssertEquals('Name', 'MyConn', D.Name);
    AssertEquals('ConnectionType', 'PostgreSQL', D.ConnectionType);
    AssertEquals('HostName', 'localhost', D.HostName);
    AssertEquals('DatabaseName', 'mydb', D.DatabaseName);
    AssertEquals('UserName', 'admin', D.UserName);
    AssertEquals('Password', 'secret', D.Password);
    AssertEquals('CharSet', 'UTF8', D.CharSet);
    AssertEquals('Port', 5432, D.Port);
    AssertEquals('Role', 'dba', D.Role);
    AssertFalse('Enabled', D.Enabled);
    AssertEquals('Params.Count', 1, D.Params.Count);
    AssertEquals('Params[0]', 'journal=wal', D.Params[0]);
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadIniFileMultiple;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  F, D: TSQLDBConnectionDef;
  I: Integer;
begin
  FN:=GetTempDir+'opf_conn_test_multi.ini';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    for I:=1 to 3 do
    begin
      F:=List.AddConnection('Type'+IntToStr(I), 'Host'+IntToStr(I),
        'DB'+IntToStr(I), 'User'+IntToStr(I), 'Pass'+IntToStr(I));
      F.Name:='Conn'+IntToStr(I);
      F.CharSet:='CS'+IntToStr(I);
      F.Port:=5430+I;
      F.Role:='Role'+IntToStr(I);
    end;
    List.SaveToFile(FN);
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 3, DstList.Count);
    for I:=0 to 2 do
    begin
      D:=DstList[I];
      AssertEquals('Name '+IntToStr(I), 'Conn'+IntToStr(I+1), D.Name);
      AssertEquals('Type '+IntToStr(I), 'Type'+IntToStr(I+1), D.ConnectionType);
      AssertEquals('Host '+IntToStr(I), 'Host'+IntToStr(I+1), D.HostName);
      AssertEquals('DB '+IntToStr(I), 'DB'+IntToStr(I+1), D.DatabaseName);
      AssertEquals('User '+IntToStr(I), 'User'+IntToStr(I+1), D.UserName);
      AssertEquals('Pass '+IntToStr(I), 'Pass'+IntToStr(I+1), D.Password);
      AssertEquals('CharSet '+IntToStr(I), 'CS'+IntToStr(I+1), D.CharSet);
      AssertEquals('Port '+IntToStr(I), 5431+I, D.Port);
      AssertEquals('Role '+IntToStr(I), 'Role'+IntToStr(I+1), D.Role);
      AssertTrue('Enabled '+IntToStr(I), D.Enabled);
    end;
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadIniFileWithParams;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  F, D: TSQLDBConnectionDef;
begin
  FN:=GetTempDir+'opf_conn_test_params.ini';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    F:=List.AddConnection('sqlite','','testdb','','');
    F.Name:='ParamConn';
    F.Params.Add('journal=wal');
    F.Params.Add('mode=ro');
    F.Params.Add('cache_size=1000');
    List.SaveToFile(FN);
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    D:=DstList[0];
    AssertEquals('Name', 'ParamConn', D.Name);
    AssertEquals('Params.Count', 3, D.Params.Count);
    AssertEquals('Params[0]', 'journal=wal', D.Params[0]);
    AssertEquals('Params[1]', 'mode=ro', D.Params[1]);
    AssertEquals('Params[2]', 'cache_size=1000', D.Params[2]);
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadIniFileEmpty;
var
  DstList: TSQLDBConnectionList;
  FN: String;
begin
  FN:=GetTempDir+'opf_conn_test_empty.ini';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    List.SaveToFile(FN);
    DstList.LoadFromFile(FN);
    AssertEquals('Empty count', 0, DstList.Count);
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestAutoDetectJsonExtension;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  F, D: TSQLDBConnectionDef;
  Content: String;
  FS: TFileStream;
begin
  FN:=GetTempDir+'opf_conn_autodetect.json';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    F:=List.AddConnection('pgsql','H1','DB1','U1','P1');
    F.Name:='JsonConn';
    List.SaveToFile(FN);
    FS:=TFileStream.Create(FN, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Content, FS.Size);
      if FS.Size>0 then
        FS.ReadBuffer(Content[1], FS.Size);
    finally
      FS.Free;
    end;
    AssertTrue('JSON starts with [', (Length(Content)>0) and (Content[1]='['));
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    D:=DstList[0];
    AssertEquals('Name', 'JsonConn', D.Name);
    AssertEquals('ConnectionType', 'pgsql', D.ConnectionType);
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestAutoDetectIniExtension;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  F, D: TSQLDBConnectionDef;
  Content: String;
  FS: TFileStream;
begin
  FN:=GetTempDir+'opf_conn_autodetect.ini';
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    F:=List.AddConnection('pgsql','H1','DB1','U1','P1');
    F.Name:='IniConn';
    List.SaveToFile(FN);
    FS:=TFileStream.Create(FN, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(Content, FS.Size);
      if FS.Size>0 then
        FS.ReadBuffer(Content[1], FS.Size);
    finally
      FS.Free;
    end;
    AssertTrue('INI contains section bracket', Pos('[IniConn]', Content)>0);
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    D:=DstList[0];
    AssertEquals('Name', 'IniConn', D.Name);
    AssertEquals('ConnectionType', 'pgsql', D.ConnectionType);
  finally
    DstList.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestNoCipherPassthrough;
var
  O: TOPNoCipher;
  C: IOPPasswordCipher;
begin
  O:=TOPNoCipher.Create;
  try
    C:=O;
    AssertEquals('Encrypt passthrough', 'secret', C.Encrypt('secret'));
    AssertEquals('Decrypt passthrough', 'secret', C.Decrypt('secret'));
    AssertEquals('Empty encrypt', '', C.Encrypt(''));
    AssertEquals('Empty decrypt', '', C.Decrypt(''));
  finally
    O.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestXorCipherRoundTrip;
var
  O: TOPXorCipher;
  C: IOPPasswordCipher;
begin
  O:=TOPXorCipher.Create('mykey');
  try
    C:=O;
    AssertEquals('Round-trip', 'secret', C.Decrypt(C.Encrypt('secret')));
    AssertEquals('Round-trip long', 'a longer password!', C.Decrypt(C.Encrypt('a longer password!')));
  finally
    O.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestXorCipherProducesDifferentOutput;
var
  O: TOPXorCipher;
  C: IOPPasswordCipher;
begin
  O:=TOPXorCipher.Create('mykey');
  try
    C:=O;
    AssertTrue('Encrypted differs from plain', C.Encrypt('secret') <> 'secret');
  finally
    O.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestXorCipherEmptyString;
var
  O: TOPXorCipher;
  C: IOPPasswordCipher;
begin
  O:=TOPXorCipher.Create('mykey');
  try
    C:=O;
    AssertEquals('Empty encrypt', '', C.Encrypt(''));
    AssertEquals('Empty decrypt', '', C.Decrypt(''));
  finally
    O.Free;
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadJsonWithXorCipher;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  CipherObj: TOPXorCipher;
begin
  FN:=GetTempDir+'opf_cipher_test.json';
  CipherObj:=TOPXorCipher.Create('testkey');
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    List.PasswordCipher:=CipherObj;
    List.AddConnection('pgsql','H1','DB1','U1','secret123').Name:='C1';
    List.SaveToFile(FN);
    DstList.PasswordCipher:=CipherObj;
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    AssertEquals('Password decrypted', 'secret123', DstList[0].Password);
  finally
    DstList.Free;
    CipherObj.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestSaveLoadIniWithXorCipher;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  CipherObj: TOPXorCipher;
begin
  FN:=GetTempDir+'opf_cipher_test.ini';
  CipherObj:=TOPXorCipher.Create('testkey');
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    List.PasswordCipher:=CipherObj;
    List.AddConnection('pgsql','H1','DB1','U1','secret123').Name:='C1';
    List.SaveToFile(FN);
    DstList.PasswordCipher:=CipherObj;
    DstList.LoadFromFile(FN);
    AssertEquals('Count', 1, DstList.Count);
    AssertEquals('Password decrypted', 'secret123', DstList[0].Password);
  finally
    DstList.Free;
    CipherObj.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestSavePreservesInMemoryPassword;
var
  FN: String;
  CipherObj: TOPXorCipher;
begin
  FN:=GetTempDir+'opf_cipher_mem_test.json';
  CipherObj:=TOPXorCipher.Create('testkey');
  try
    List.PasswordCipher:=CipherObj;
    List.AddConnection('pgsql','H1','DB1','U1','mypass').Name:='C1';
    List.SaveToFile(FN);
    AssertEquals('In-memory password unchanged after save', 'mypass', List[0].Password);
  finally
    CipherObj.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

procedure TSQLDBConnectionListTest.TestDoubleSaveDoesNotDoubleEncrypt;
var
  DstList: TSQLDBConnectionList;
  FN: String;
  CipherObj: TOPXorCipher;
begin
  FN:=GetTempDir+'opf_cipher_double_test.json';
  CipherObj:=TOPXorCipher.Create('testkey');
  DstList:=TSQLDBConnectionList.Create(TSQLDBConnectionDef);
  try
    List.PasswordCipher:=CipherObj;
    List.AddConnection('pgsql','H1','DB1','U1','mypass').Name:='C1';
    List.SaveToFile(FN);
    List.SaveToFile(FN);
    DstList.PasswordCipher:=CipherObj;
    DstList.LoadFromFile(FN);
    AssertEquals('Password after double save', 'mypass', DstList[0].Password);
  finally
    DstList.Free;
    CipherObj.Free;
    if FileExists(FN) then
      DeleteFile(FN);
  end;
end;

{ TSQLDBConnectionDefTest }

procedure TSQLDBConnectionDefTest.SetUp;

begin
  inherited SetUp;
  FDef:=TSQLDBConnectionDef.Create(Nil);
  AssignDefValues(Def,1);
end;

class procedure TSQLDBConnectionDefTest.AssignDefValues(F : TSQLDBConnectionDef;aIndex : Integer);

var
  N : String;

begin
  N:=IntToStr(aIndex);
  F.Name:='C'+N;
  F.ConnectionType:='CT'+N;
  F.HostName:='H'+N;
  F.DatabaseName:='DB'+N;
  F.UserName:='U'+N;
  F.Password:='P'+N;
  F.CharSet:='CS'+N;
  F.Port:=1000+aIndex;
  F.Params.Add('P'+N+'=V'+N);
end;

procedure TSQLDBConnectionDefTest.TearDown;
begin
  FreeAndNil(FDef);
  inherited TearDown;
end;

procedure TSQLDBConnectionDefTest.Testprops(F: TSQLDBConnectionDef; aIndex : Integer);

Var
  N : String;

begin
  N:=IntToStr(aIndex);
  AssertEquals('Name','C'+N,F.Name);
  AssertEquals('ConnectionType','CT'+N,F.ConnectionType);
  AssertEquals('HostName','H'+N,F.HostName);
  AssertEquals('DatabaseName','DB'+N,F.DatabaseName);
  AssertEquals('UserName','U'+N,F.UserName);
  AssertEquals('Password','P'+N,F.Password);
  AssertEquals('CharSet','CS'+N,F.CharSet);
  AssertEquals('Name',1000+aIndex,F.Port);
  AssertEquals('Params.Count',1,F.Params.Count);
  AssertEquals('Params[0]','P'+N+'=V'+N,F.Params[0]);
end;

procedure TSQLDBConnectionDefTest.TestEmpty;
begin
  AssertNotNull('Have field Field');
  TestProps(Def,1);
end;

procedure TSQLDBConnectionDefTest.TestAssign;

Var
  F : TSQLDBConnectionDef;

begin
  F:=TSQLDBConnectionDef.Create(Nil);
  try
    F.Assign(Def);
    TestProps(F,1);
  finally
    F.Free;
  end;
end;

procedure TSQLDBConnectionDefTest.TestDisplayName;
begin
  AssertEquals('Get',Def.Name,Def.DisplayName);
  Def.DisplayName:='A';
  AssertEquals('Set','A',Def.Name);
end;


initialization
  RegisterTests([TSQLDBConnectionDefTest,TSQLDBConnectionListTest]);
end.

