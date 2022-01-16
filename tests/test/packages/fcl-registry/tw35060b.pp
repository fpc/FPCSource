{ %TARGET=win32,win64,wince }

program tw35060b;

{$apptype console}
{$assertions on}
{$ifdef fpc}
{$codepage utf8}
{$mode objfpc}
{$h+}
{$endif fpc}

uses
  SysUtils, Classes, Windows, Registry;

{$ifndef fpc}
type
  UnicodeString = WideString;

function GetLastOSError: Integer;
begin
  Result := GetLastError;
end;
{$endif}

const
  ExpectedUtf8Hex = 'C3 A4 C3 AB C3 AF';
  ExpectedUnicodeHex = '00E4 00EB 00EF';
  BugID = 'FPCBug0035060';

function UnicodeToHex(const S: UnicodeString): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(S) do
    Result := Result + IntToHex(Word(S[i]),4) + #32;
  Result := Trim(Result);
end;

function Utf8ToHex(const S: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(S) do
    Result := Result + IntToHex(Byte(S[i]),2) + #32;
  Result := Trim(Result);
end;


procedure CreateKeyInHKCU(const Key: UnicodeString);
Var
  u: UnicodeString;
  Disposition: Dword;
  Handle: HKEY;
  SecurityAttributes: Pointer; //LPSECURITY_ATTRIBUTES;
  FLastError: LongInt;
begin
  SecurityAttributes := Nil;
  u:=Key;
  Handle := 0;
  FLastError:=RegCreateKeyExW(HKEY_CURRENT_USER,
                              PWideChar(u),
                              0,
                              '',
                              REG_OPTION_NON_VOLATILE,
                              KEY_ALL_ACCESS,
                              SecurityAttributes,
                              Handle,
                              @Disposition);
  RegCloseKey(Handle);
  Assert(FLastError=ERROR_SUCCESS,format('Creating key "%s" using plain Windows API failed: "%s"',
                                         [String(Key),Trim(SysErrorMessage(FLastError))]));
end;


procedure CreateTestKey;
const
  TestKey: UnicodeString = 'Software\'+ UniCodeString(BugID)+ '\äëï';
var
  Len: Integer;
begin
  Len := Length(TestKey);
  //Being a bit paranoid here?
  Assert((Len=26) and (Word(TestKey[Len])=$EF) and (Word(TestKey[Len-1])=$EB) and (Word(TestKey[Len-2])=$E4),'Wrong encoding of TestKey');
  CreateKeyInHKCU(TestKey);
end;

procedure RemoveTestKey;
const
  TestKeyFull: UnicodeString = 'Software\'+ UniCodeString(BugID)+ '\äëï';
  TestKeyBugID: UnicodeString = 'Software\'+ UniCodeString(BugID);
var
  Key: UnicodeString;
  FLastError: LongInt;
begin
  Key:=TestKeyFull;
  FLastError:=RegDeleteKeyW(HKEY_CURRENT_USER,PWideChar(Key));
  Assert(FLastError=ERROR_SUCCESS,format('Removing key "%s" using plain Windows API failed: "%s"',
                                         [String(Key),Trim(SysErrorMessage(FLastError))]));

  Key:=TestKeyBugID;
  FLastError:=RegDeleteKeyW(HKEY_CURRENT_USER,PWideChar(Key));
  Assert(FLastError=ERROR_SUCCESS,format('Removing key "%s" using plain Windows API failed: "%s"',
                                         [String(Key),Trim(SysErrorMessage(FLastError))]));
end;

//End Registry plain API functions

var
  R: TRegistry;
  Name, S: String;
  Key: Utf8String;
  U: UnicodeString;
  B: Boolean;
  Err: Integer;
  CP: TSystemCodePage;
begin
  CreateTestKey;
  try
    Name := 'äëï';
    U := UnicodeString(Name);
    S := Utf8ToHex(Name);
    Assert(S=ExpectedUtf8Hex,format('Name is wrongly encoded: expected: %s, found: %s',[ExpectedUtf8Hex,S]));
    S := UnicodeToHex(U);
    Assert(S=ExpectedUnicodeHex,format('Name is wrongly encoded: expected: %s, found: %s',[ExpectedUnicodeHex,S]));

    R := TRegistry.Create(KEY_ALL_ACCESS);
    try
      R.RootKey := HKEY_CURRENT_USER;
      Key := 'Software\'+BugId+'\'+Name;
      CP := System.StringCodePage(Key);
      Assert(CP = 65001,format('The string that contains the key does not have UTF-8 as dynamic code page, but has codepage %d',[CP]));
      B := R.OpenKeyReadOnly(Key);
      Err := GetLastOSError;
      Assert(B,format('OpenKey(''%s'') failed: "%s" [%d]',[Key,Trim(SysErrorMessage(Err)),Err]));
      writeln(format('OpenKeyReadOnly(''%s''): OK',[Key]));
    finally
      R.Free;
    end;

  finally
    RemoveTestKey;
  end;
end.

