{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by Michael Van Canneyt,
    member of the Free Pascal development team

    Test for Base 16,32,32hex,32-crockford, 64,64url encoding/decoding, with or without padding

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testbasenenc;

uses sysutils, basenenc;


Procedure AssertEquals(Const aActual,aExpected : TBytes; aMsg : String);

  function ToStr(aBytes : TBytes) : string;

  Var
    I : Integer;

  begin
    Result:='';
    For I:=0 to Length(aBytes) do
      begin
      if I>0 then
        Result:=Result+',';
      Result:=Result+IntToStr(aBytes[i]);
      end;
    Result:='['+Result+']';
  end;

begin
  if (Length(aActual)<>Length(aExpected))
     or Not CompareMem(PByte(aActual),PByte(aExpected),Length(aActual)) then
    begin
    Writeln(aMsg,': results differ, actual: "',ToStr(aActual),'" <> "',ToStr(aExpected),'" (expected)');
    Halt(1);
    end;
end;

Procedure AssertEquals(Const aActual,aExpected,aMsg : String);

begin
  if aActual<>aExpected then
    begin
    Writeln(aMsg,': results differ, actual: "',aActual,'" <> "',aExpected,'" (expected)');
    Halt(1);
    end;
end;

Procedure DoTest(B : Tbytes; aExpected : String; aURL : Boolean = False);

Var
  B2 : TBytes;
  S : Ansistring;

begin
  if aURL then
    S:=Base64URL.Encode(B)
  else
    S:=Base64.Encode(B);
  AssertEquals(S,aExpected,'DoTest Wrong encode');
  if aURL then
    B2:=Base64URL.Decode(S)
  else
    B2:=Base64.Decode(S);
  AssertEquals(B2,B,'DoTest Wrong decode');
end;

Procedure DoTest64(aValue, aExpected : String);

begin
  DoTest(TEncoding.UTF8.GetAnsiBytes(aValue),aExpected);
end;

Procedure DoTest32(aValue, aExpected : String);

Var
  B2 : TBytes;
  S : Ansistring;

begin
  S:=Base32.Encode(aValue);
  AssertEquals(S,aExpected,'base32 encode');
  B2:=Base32.Decode(S);
  AssertEquals(b2,TEncoding.UTF8.GetAnsiBytes(aValue),'Base32 Wrong encode for '+aValue);
end;

Procedure DoTest32Hex(aValue, aExpected : String);

Var
  B2 : TBytes;
  S : Ansistring;

begin
  S:=Base32Hex.Encode(aValue);
  AssertEquals(S,aExpected,'Base32-hex Wrong encode for '+aValue);
  B2:=Base32Hex.Decode(S);
  AssertEquals(B2,TEncoding.UTF8.GetAnsiBytes(aValue),'Base32Hex Wrong encode for '+aValue);
end;

Procedure DoTest16(aValue, aExpected : String);

Var
  B2 : TBytes;
  S : Ansistring;

begin
  S:=Base16.Encode(aValue);
  AssertEquals(S,aExpected,'Base16 Wrong encode for '+aValue);
  B2:=Base16.Decode(S);
  AssertEquals(B2,TEncoding.UTF8.GetAnsiBytes(aValue),'Base16 Wrong decode for '+aValue);
end;



begin
  // From RFC 3548

  DoTest([$14,$fb,$9c,$03,$d9,$7e],'FPucA9l+');
  DoTest([$14,$fb,$9c,$03,$d9],'FPucA9k=');
  DoTest([$14,$fb,$9c,$03],'FPucAw==');
  DoTest([$14,$fb,$9c,$03,$d9,$7e],'FPucA9l-',True);

  // From RFC 4648
  DoTest64('','');
  DoTest64('f','Zg==');
  DoTest64('fo','Zm8=');
  DoTest64('foo','Zm9v');
  DoTest64('foob','Zm9vYg==');
  DoTest64('fooba','Zm9vYmE=');
  DoTest64('foobar','Zm9vYmFy');

  DoTest32('','');
  DoTest32('f','MY======');
  DoTest32('fo','MZXQ====');
  DoTest32('foo','MZXW6===');
  DoTest32('foob','MZXW6YQ=');
  DoTest32('fooba','MZXW6YTB');
  DoTest32('foobar','MZXW6YTBOI======');

  DoTest32HEX('','');
  DoTest32HEX('f','CO======');
  DoTest32HEX('fo','CPNG====');
  DoTest32HEX('foo','CPNMU===');
  DoTest32HEX('foob','CPNMUOG=');
  DoTest32HEX('fooba','CPNMUOJ1');
  DoTest32HEX('foobar','CPNMUOJ1E8======');

  DoTest16('','');
  DoTest16('f','66');
  DoTest16('fo','666F');
  DoTest16('foo','666F6F');
  DoTest16('foob','666F6F62');
  DoTest16('fooba','666F6F6261');
  DoTest16('foobar','666F6F626172');
end.

