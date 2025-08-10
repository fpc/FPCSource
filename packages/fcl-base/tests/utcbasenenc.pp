unit utcbasenenc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, basenenc;

procedure RegisterTests;

implementation

function BytesToStr(const aBytes: TBytes): String;
var
  I: Integer;
begin
  Result := '';
  for I := Low(aBytes) to High(aBytes) do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + IntToStr(aBytes[I]);
  end;
  Result := '[' + Result + ']';
end;

function AssertBytesEqual(const Msg: String; const aActual, aExpected: TBytes): Boolean;
var
  Cmp: Boolean;
begin
  Cmp := (Length(aActual) = Length(aExpected)) and CompareMem(Pointer(aActual), Pointer(aExpected), Length(aActual));
  Result := AssertTrue(Msg + '. Expected: ' + BytesToStr(aExpected) + ', Actual: ' + BytesToStr(aActual), Cmp);
end;

function TestBaseN_RFC3548: TTestString;
begin
  Result := '';
  AssertEquals('RFC3548 B64 1', 'FPucA9l+', Base64.Encode([$14,$fb,$9c,$03,$d9,$7e]));
  AssertBytesEqual('RFC3548 B64 1 Decode', [$14,$fb,$9c,$03,$d9,$7e], Base64.Decode('FPucA9l+'));
  AssertEquals('RFC3548 B64 2', 'FPucA9k=', Base64.Encode([$14,$fb,$9c,$03,$d9]));
  AssertBytesEqual('RFC3548 B64 2 Decode', [$14,$fb,$9c,$03,$d9], Base64.Decode('FPucA9k='));
  AssertEquals('RFC3548 B64 3', 'FPucAw==', Base64.Encode([$14,$fb,$9c,$03]));
  AssertBytesEqual('RFC3548 B64 3 Decode', [$14,$fb,$9c,$03], Base64.Decode('FPucAw=='));
  AssertEquals('RFC3548 B64 URL', 'FPucA9l-', Base64URL.Encode([$14,$fb,$9c,$03,$d9,$7e]));
  AssertBytesEqual('RFC3548 B64 URL Decode', [$14,$fb,$9c,$03,$d9,$7e], Base64URL.Decode('FPucA9l-'));
end;

function TestBaseN_RFC4648_Base64: TTestString;
begin
  Result := '';
  AssertEquals('RFC4648 B64 empty', '', Base64.Encode(''));
  AssertEquals('RFC4648 B64 "f"', 'Zg==', Base64.Encode('f'));
  AssertEquals('RFC4648 B64 "fo"', 'Zm8=', Base64.Encode('fo'));
  AssertEquals('RFC4648 B64 "foo"', 'Zm9v', Base64.Encode('foo'));
  AssertEquals('RFC4648 B64 "foob"', 'Zm9vYg==', Base64.Encode('foob'));
  AssertEquals('RFC4648 B64 "fooba"', 'Zm9vYmE=', Base64.Encode('fooba'));
  AssertEquals('RFC4648 B64 "foobar"', 'Zm9vYmFy', Base64.Encode('foobar'));
end;

function TestBaseN_RFC4648_Base32: TTestString;
begin
  Result := '';
  AssertEquals('RFC4648 B32 empty', '', Base32.Encode(''));
  AssertEquals('RFC4648 B32 "f"', 'MY======', Base32.Encode('f'));
  AssertEquals('RFC4648 B32 "fo"', 'MZXQ====', Base32.Encode('fo'));
  AssertEquals('RFC4648 B32 "foo"', 'MZXW6===', Base32.Encode('foo'));
  AssertEquals('RFC4648 B32 "foob"', 'MZXW6YQ=', Base32.Encode('foob'));
  AssertEquals('RFC4648 B32 "fooba"', 'MZXW6YTB', Base32.Encode('fooba'));
  AssertEquals('RFC4648 B32 "foobar"', 'MZXW6YTBOI======', Base32.Encode('foobar'));
end;

function TestBaseN_RFC4648_Base32Hex: TTestString;
begin
  Result := '';
  AssertEquals('RFC4648 B32Hex empty', '', Base32Hex.Encode(''));
  AssertEquals('RFC4648 B32Hex "f"', 'CO======', Base32Hex.Encode('f'));
  AssertEquals('RFC4648 B32Hex "fo"', 'CPNG====', Base32Hex.Encode('fo'));
  AssertEquals('RFC4648 B32Hex "foo"', 'CPNMU===', Base32Hex.Encode('foo'));
  AssertEquals('RFC4648 B32Hex "foob"', 'CPNMUOG=', Base32Hex.Encode('foob'));
  AssertEquals('RFC4648 B32Hex "fooba"', 'CPNMUOJ1', Base32Hex.Encode('fooba'));
  AssertEquals('RFC4648 B32Hex "foobar"', 'CPNMUOJ1E8======', Base32Hex.Encode('foobar'));
end;

function TestBaseN_RFC4648_Base16: TTestString;
begin
  Result := '';
  AssertEquals('RFC4648 B16 empty', '', Base16.Encode(''));
  AssertEquals('RFC4648 B16 "f"', '66', Base16.Encode('f'));
  AssertEquals('RFC4648 B16 "fo"', '666F', Base16.Encode('fo'));
  AssertEquals('RFC4648 B16 "foo"', '666F6F', Base16.Encode('foo'));
  AssertEquals('RFC4648 B16 "foob"', '666F6F62', Base16.Encode('foob'));
  AssertEquals('RFC4648 B16 "fooba"', '666F6F6261', Base16.Encode('fooba'));
  AssertEquals('RFC4648 B16 "foobar"', '666F6F626172', Base16.Encode('foobar'));
end;

procedure RegisterTests;
begin
  AddSuite('TBaseNTests');
  AddTest('TestRFC3548', @TestBaseN_RFC3548, 'TBaseNTests');
  AddTest('TestRFC4648_Base64', @TestBaseN_RFC4648_Base64, 'TBaseNTests');
  AddTest('TestRFC4648_Base32', @TestBaseN_RFC4648_Base32, 'TBaseNTests');
  AddTest('TestRFC4648_Base32Hex', @TestBaseN_RFC4648_Base32Hex, 'TBaseNTests');
  AddTest('TestRFC4648_Base16', @TestBaseN_RFC4648_Base16, 'TBaseNTests');
end;

end.
