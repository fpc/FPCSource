unit utchash;

{$mode objfpc}
{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$macro on}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.hash, generics.hashes;

type

  { TTestHashBase }

  TTestHashBase = Class(TTestCase)
  Public
    class procedure AssertEquals(aMsg: String; aExpected, aActual: TBytes); overload;
    class function BytesToString(Bytes: TBytes): String;
  end;

  TTestHash = Class(TTestHashBase)
  Private
    FB : TBytes;
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
  Published
    Procedure TestDigestAsInteger;
    Procedure TestDigestAsString;
    Procedure TestDigestAsStringGUID;
    Procedure TestGetRandomString;
    Procedure TestToBigEndian;
  end;

  { TTestMD5 }

  TTestMD5 = class(TTestHashBase)
  private
    FMD5: THashMD5;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property MD5 : THashMD5 Read FMD5;
  published
    procedure TestHashAsBytes;
    procedure TestHashAsBytesTwice;
    // Class functions
    Procedure TestGetHashBytesString;
    Procedure TestGetHashStringString;
    Procedure TestGetHashBytesStream;
    Procedure TestGetHashStringStream;
    Procedure TestGetHashBytesFromFile;
    Procedure TestGetHashStringFromFile;
  end;

  { TTestSha1 }

  TTestSha1 = Class(TTestHashBase)
  private
    FSHA1: THashSha1;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property SHA1: THashSha1 Read FSHA1;
  published
    procedure TestHashAsBytes;
    procedure TestHashAsBytesTwice;
    // Class functions
    Procedure TestGetHashBytesString;
    Procedure TestGetHashStringString;
    Procedure TestGetHashBytesStream;
    Procedure TestGetHashStringStream;
    Procedure TestGetHashBytesFromFile;
    Procedure TestGetHashStringFromFile;
  end;

  { TTestSha2 }

  { Only tests SHA256, on the assumption that the other supported fphash mechanisms are also correct,
    so we basically test the wrapper layer around the FPC native routines... }
  TTestSha2 = Class(TTestHashBase)
  private
    FSHA2: THashSha2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property SHA2: THashSha2 Read FSHA2;
  published
    procedure TestHashAsBytes;
    procedure TestHashAsBytesTwice;
    // Class functions
    Procedure TestGetHashBytesString;
    Procedure TestGetHashStringString;
    Procedure TestGetHashBytesStream;
    Procedure TestGetHashStringStream;
    Procedure TestGetHashBytesFromFile;
    Procedure TestGetHashStringFromFile;
  end;


  { TTestFNV }

  TTestFNV = Class(TTestHashBase)
  private
    FFNV: THashFNV1a32;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property FNV: THashFNV1a32 Read FFNV;
  Published
    Procedure TestHashAsBytes;
    Procedure TestGetHashBytes;         // (const aData: UnicodeString): TBytes; static;
    Procedure TestGetHashStringUnicode; // (const aData: UnicodeString): UnicodeString; overload; static;
    Procedure TestGetHashStringRawByte; // (const aData: RawByteString): UnicodeString; overload; static;
    Procedure TestGetHashValueUnicode;  // (const aData: UnicodeString): Integer; overload; static; inline;
    Procedure TestGetHashValueRawByte;  // (const aData: RawByteString): Integer; overload; static; inline;
    Procedure TestGetHashValue;         // (const aData; aLength: Cardinal; aInitialValue: Cardinal = FNV_SEED): Integer; overload; static; inline;
  end;

  { TTestFNV }

  { TTestBobJenkins }

  TTestBobJenkins = Class(TTestHashBase)
  private
    FBJ: THashBobJenkins;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property BJ: THashBobJenkins Read FBJ;
  Published
    Procedure TestHashAsBytes;
    Procedure TestGetHashBytes;         // (const aData: UnicodeString): TBytes; static;
    Procedure TestGetHashStringUnicode; // (const aData: UnicodeString): UnicodeString; overload; static;
    Procedure TestGetHashStringRawByte; // (const aData: RawByteString): UnicodeString; overload; static;
    Procedure TestGetHashValueUnicode;  // (const aData: UnicodeString): Integer; overload; static; inline;
    Procedure TestGetHashValueRawByte;  // (const aData: RawByteString): Integer; overload; static; inline;
    Procedure TestGetHashValue;         // (const aData; aLength: Cardinal; aInitialValue: Cardinal = FNV_SEED): Integer; overload; static; inline;
  end;



implementation


Class Function TTestHashBase.BytesToString(Bytes : TBytes) : String;

var
  I : Integer;


begin
  Result:='[';
  For I:=0 to Length(Bytes)-1 do
    begin
    if I>0 then
      Result:=Result+', ';
    Result:=Result+'$'+Format('%x',[Bytes[i]]);
    end;
  Result:=Result+']';
end;

class procedure TTestHashBase.AssertEquals(aMsg: String; aExpected, aActual: TBytes);

var
  I : Integer;

begin
  AssertEquals(aMsg+': length',Length(aExpected),Length(aActual));
  For I:=0 to length(aExpected)-1 do
    AssertEquals(aMsg+': Byte['+IntTostr(i)+']','$'+IntToHex(aExpected[i]),'$'+IntToHex(aActual[i]));
end;

{ TTestHash }

procedure TTestHash.Setup;
begin
  inherited Setup;
  SetLength(FB,0);
end;

procedure TTestHash.TearDown;
begin
  SetLength(FB,0);
  inherited TearDown;
end;

procedure TTestHash.TestDigestAsInteger;

  Procedure DoDigestInt;

  begin
    THash.DigestAsInteger(FB);
  end;

begin
//  class function DigestAsInteger(const aDigest: TBytes): Integer; static;
  SetLength(FB,4);
  FB[0]:=$01;
  FB[1]:=$02;
  FB[2]:=$03;
  FB[3]:=$04;
  AssertEquals('Integer',NtoBE($01020304),THash.DigestAsInteger(FB));
  SetLength(FB,5);
  AssertException('Size must be 4',EHashException,
  procedure
  begin
    THash.DigestAsInteger(FB);
  end);
end;

procedure TTestHash.TestDigestAsString;
begin
//  class function DigestAsString(const aDigest: TBytes; UpperCase : Boolean = false): UnicodeString; static;
  SetLength(FB,4);
  FB[0]:=$01;
  FB[1]:=$0A;
  FB[2]:=$03;
  FB[3]:=$0D;
  AssertEquals('Lower','010a030d',THash.DigestAsString(FB));
  AssertEquals('Upper','010A030D',THash.DigestAsString(FB,True));
end;


Operator = (a,b : TGUID) : Boolean;
var
  i: integer;
begin
  Result:=(a.D1=b.D1)
          and (a.D2=b.D2)
          and (a.D3=b.D3)
          and (a.D3=b.D3);
  I:=0;
  While Result and (I<8) do
    begin
    Result:=(a.D4[i]=b.D4[i]);
    Inc(I);
    end;
end;

procedure TTestHash.TestDigestAsStringGUID;

var
  I : integer;

begin
  SetLength(FB,16);
  For I:=0 to 15 do
    FB[i]:=I;
  AssertEquals('THash.DigestAsStringGUID','{00010203-0405-0607-0809-0A0B0C0D0E0F}',THash.DigestAsStringGUID(FB));
end;

procedure TTestHash.TestGetRandomString;

var
  S : UnicodeString;
  C : UnicodeChar;

begin
  S:=THash.GetRandomString;
  AssertEquals('Default length',10,Length(S));
  For C in S do
   If not Pos(C,RandomStringChars)>0 then
     Fail(C+' not in allowed chars');
end;

procedure TTestHash.TestToBigEndian;

var
  C : Cardinal;
  Q : UInt64;

begin
  C:=$01020304;
  AssertEquals('Cardinal',NToBE(C),THash.ToBigEndian(C));
  Q:=UInt64($0102030405060708);
  AssertEquals('Cardinal',NToBE(Q),THash.ToBigEndian(Q));
end;

Const
  ExpectABCMD5 : TBytes = ($90, $1, $50, $98, $3C, $D2, $4F, $B0, $D6, $96, $3F, $7D, $28, $E1, $7F, $72);
  ExpectABCSha1 : TBytes = ($A9, $99, $3E, $36, $47, $6, $81, $6A, $BA, $3E, $25, $71, $78, $50, $C2, $6C, $9C, $D0, $D8, $9D);
  ExpectABCSha2 : TBytes = ($BA, $78, $16, $BF, $8F, $1, $CF, $EA, $41, $41, $40, $DE, $5D, $AE, $22, $23, $B0, $3, $61, $A3, $96, $17, $7A, $9C, $B4, $10, $FF, $61, $F2, $0, $15, $AD);
  ExpectABCFNV = Cardinal($1A47E90B);
  ExpectABCFNVUnicode = Cardinal($AE1E997D);
  ExpectABCBJ = 238646833;
  ExpectABCBJUnicode = Cardinal(4228388320);



procedure TTestMD5.TestHashAsBytes;


var
  Act : TBytes;

begin
  FMD5.Update('abc');
  Act:=FMD5.HashAsBytes;
  // Writeln(BytesToString(Act));
  AssertEquals('abc',ExpectAbcMD5,Act);
end;

procedure TTestMD5.TestHashAsBytesTwice;

var
  Act : TBytes;

begin
  FMD5.Update('abc');
  Act:=FMD5.HashAsBytes;
  AssertEquals('abc',ExpectAbcMD5,Act);
  Act:=FMD5.HashAsBytes;
  AssertEquals('abc 2',ExpectAbcMD5,Act);
end;

procedure TTestMD5.TestGetHashBytesString;
begin
  AssertEquals('abc',ExpectAbcMD5,THashMD5.GetHashBytes('abc'));
end;

procedure TTestMD5.TestGetHashStringString;
begin
  AssertEquals('abc',THash.DigestAsString(ExpectAbcMD5),THashMD5.GetHashString('abc'));
end;

procedure TTestMD5.TestGetHashBytesStream;

Var
  S : TStream;
  A : AnsiString;

begin
  A:='abc';
  S:=TStringStream.Create(A);
  try
    AssertEquals('GetHashBytes',ExpectAbcMD5,THashMD5.GetHashBytes(S));
  finally
    S.Free;
  end;
end;

procedure TTestMD5.TestGetHashStringStream;

Var
  S : TStream;
  A : AnsiString;

begin
  A:='abc';
  S:=TStringStream.Create(A);
  try
    AssertEquals('GetHashBytes',THash.DigestAsString(ExpectAbcMD5),THashMD5.GetHashString(S));
  finally
    S.Free;
  end;
end;

procedure TTestMD5.TestGetHashBytesFromFile;

Var
  F,S : TStream;
  Fn,A : AnsiString;

begin
  A:='abc';
  F:=nil;
  S:=TStringStream.Create(A);
  try
    FN:=GetTempFileName;
    F:=TFileStream.Create(FN,fmCreate);
    F.CopyFrom(S,0);
    FreeAndNil(F);
    AssertEquals('GetHashBytes',ExpectAbcMD5,THashMD5.GetHashBytesFromFile(FN));
  finally
    S.Free;
    FreeAndNil(F);
  end;

end;

procedure TTestMD5.TestGetHashStringFromFile;

Var
  F,S : TStream;
  Fn,A : AnsiString;

begin
  A:='abc';
  F:=nil;
  S:=TStringStream.Create(A);
  try
    FN:=GetTempFileName;
    F:=TFileStream.Create(FN,fmCreate);
    F.CopyFrom(S,0);
    FreeAndNil(F);
    AssertEquals('GetHashBytes',THash.DigestAsString(ExpectABCMD5),THashMD5.GetHashStringFromFile(FN));
  finally
    S.Free;
    FreeAndNil(F);
  end;
end;



procedure TTestMD5.SetUp;
begin
  FMD5:=THashMD5.Create;
end;

procedure TTestMD5.TearDown;
begin
  //
end;

{ TTestSha1 }

procedure TTestSha1.SetUp;
begin
  inherited SetUp;
  FSHA1:=THashSha1.Create;
end;

procedure TTestSha1.TearDown;
begin
  inherited TearDown;
end;

procedure TTestSha1.TestHashAsBytes;

var
  Act : TBytes;

begin
  FSHA1.Update('abc');
  Act:=FSHA1.HashAsBytes;
  // Writeln(BytesToString(Act));
  AssertEquals('abc',ExpectAbcSha1,Act);
end;

procedure TTestSha1.TestHashAsBytesTwice;
var
  Act : TBytes;

begin
  FSHA1.Update('abc');
  Act:=FSHA1.HashAsBytes;
  // Writeln(BytesToString(Act));
  AssertEquals('abc',ExpectAbcSha1,Act);
  Act:=FSHA1.HashAsBytes;
  // Writeln(BytesToString(Act));
  AssertEquals('abc',ExpectAbcSha1,Act);
end;

procedure TTestSha1.TestGetHashBytesString;
begin
  AssertEquals('abc',ExpectAbcSha1,THashSha1.GetHashBytes('abc'));
end;

procedure TTestSha1.TestGetHashStringString;
begin
  AssertEquals('abc',THash.DigestAsString(ExpectAbcSha1),THashSha1.GetHashString('abc'));
end;

procedure TTestSha1.TestGetHashBytesStream;

Var
  S : TStream;
  A : AnsiString;

begin
  A:='abc';
  S:=TBytesStream.Create(TEncoding.UTF8.GetAnsiBytes(A));
  try
    AssertEquals('GetHashBytes',ExpectAbcSha1,THashSha1.GetHashBytes(S));
  finally
    S.Free;
  end;

end;

procedure TTestSha1.TestGetHashStringStream;

Var
  S : TStream;
  A : AnsiString;

begin
  A:='abc';
  S:=TStringStream.Create(A);
  try
    AssertEquals('GetHashBytes',THash.DigestAsString(ExpectAbcSha1),THashSha1.GetHashString(S));
  finally
    S.Free;
  end;
end;

procedure TTestSha1.TestGetHashBytesFromFile;
Var
  F,S : TStream;
  Fn,A : AnsiString;

begin
  A:='abc';
  F:=nil;
  S:=TStringStream.Create(A);
  try
    FN:=GetTempFileName;
    F:=TFileStream.Create(FN,fmCreate);
    F.CopyFrom(S,0);
    FreeAndNil(F);
    AssertEquals('GetHashBytes',ExpectAbcSha1,THashSha1.GetHashBytesFromFile(FN));
  finally
    S.Free;
    FreeAndNil(F);
  end;
end;

procedure TTestSha1.TestGetHashStringFromFile;

Var
  F,S : TStream;
  Fn,A : AnsiString;

begin
  A:='abc';
  F:=nil;
  S:=TStringStream.Create(A);
  try
    FN:=GetTempFileName;
    F:=TFileStream.Create(FN,fmCreate);
    F.CopyFrom(S,0);
    FreeAndNil(F);
    AssertEquals('GetHashBytes',THash.DigestAsString(ExpectABCSha1),THashSha1.GetHashStringFromFile(FN));
  finally
    S.Free;
    FreeAndNil(F);
  end;
end;

{ TTestSha2 }

procedure TTestSha2.SetUp;
begin
  inherited SetUp;
  FSHa2:=THashSha2.Create();
end;

procedure TTestSha2.TearDown;
begin
  FSHa2:=Default(THashSha2);
  inherited TearDown;
end;

procedure TTestSha2.TestHashAsBytes;
var
  Act : TBytes;

begin
  FSHA2.Update(UNicodeString('abc'));
  Act:=FSHA2.HashAsBytes;
  AssertEquals('abc',ExpectAbcSha2,Act);
end;

procedure TTestSha2.TestHashAsBytesTwice;
var
  Act : TBytes;

begin
  FSHA2.Update(UNicodeString('abc'));
  Act:=FSHA2.HashAsBytes;
  AssertEquals('abc',ExpectAbcSha2,Act);
  Act:=FSHA2.HashAsBytes;
  AssertEquals('abc',ExpectAbcSha2,Act);
end;

procedure TTestSha2.TestGetHashBytesString;
begin
  AssertEquals('abc',ExpectAbcSha2,THashSha2.GetHashBytes('abc'));
end;

procedure TTestSha2.TestGetHashStringString;
begin
  AssertEquals('abc',THash.DigestAsString(ExpectAbcSha2),THashSha2.GetHashString('abc'));
end;

procedure TTestSha2.TestGetHashBytesStream;
Var
  S : TStream;
  A : AnsiString;

begin
  A:='abc';
  S:=TBytesStream.Create(TEncoding.UTF8.GetAnsiBytes(A));
  try
    AssertEquals('GetHashBytes',ExpectAbcSha2,THashSha2.GetHashBytes(S));
  finally
    S.Free;
  end;
end;

procedure TTestSha2.TestGetHashStringStream;
Var
  S : TStream;
  A : AnsiString;

begin
  A:='abc';
  S:=TStringStream.Create(A);
  try
    AssertEquals('GetHashBytes',THash.DigestAsString(ExpectAbcSha2),THashSha2.GetHashString(S));
  finally
    S.Free;
  end;

end;

procedure TTestSha2.TestGetHashBytesFromFile;
Var
  F,S : TStream;
  Fn,A : AnsiString;

begin
  A:='abc';
  F:=nil;
  S:=TStringStream.Create(A);
  try
    FN:=GetTempFileName;
    F:=TFileStream.Create(FN,fmCreate);
    F.CopyFrom(S,0);
    FreeAndNil(F);
    AssertEquals('GetHashBytes',ExpectAbcSha2,THashSha2.GetHashBytesFromFile(FN));
  finally
    S.Free;
    FreeAndNil(F);
  end;
end;

procedure TTestSha2.TestGetHashStringFromFile;
Var
  F,S : TStream;
  Fn,A : AnsiString;

begin
  A:='abc';
  F:=nil;
  S:=TStringStream.Create(A);
  try
    FN:=GetTempFileName;
    F:=TFileStream.Create(FN,fmCreate);
    F.CopyFrom(S,0);
    FreeAndNil(F);
    AssertEquals('GetHashBytes',THash.DigestAsString(ExpectABCSha2),THashSha2.GetHashStringFromFile(FN));
  finally
    S.Free;
    FreeAndNil(F);
  end;
end;

{ TTestFNV }

procedure TTestFNV.SetUp;
begin
  inherited SetUp;
  FFNV:=THashFNV1a32.Create;
end;

procedure TTestFNV.TearDown;
begin
  inherited TearDown;
end;

procedure TTestFNV.TestHashAsBytes;

var
  Act : TBytes;

begin
//  Writeln('HashAsBytes');
  FFNV.Update(RawByteString('abc'));
  Act:=FFNV.HashAsBytes;
  AssertEquals('abc',ExpectABCFNV,THash.DigestAsInteger(Act));
end;

procedure TTestFNV.TestGetHashBytes;

var
  Act : TBytes;

begin
//  Writeln('GetHashBytes');
  Act:=THashFNV1a32.GetHashBytes('abc');
//  Writeln(BytesToString(Act));
  AssertEquals('abc',ExpectABCFNVUnicode,Cardinal(THash.DigestAsInteger(Act)));
end;

procedure TTestFNV.TestGetHashStringUnicode;

var
  A : Unicodestring;

begin
  A:='abc';
  AssertEquals('Hash',HexStr(ExpectABCFNVUnicode,8),THashFNV1a32.GetHashString(A));
end;

procedure TTestFNV.TestGetHashStringRawByte;

var
  A : Ansistring;

begin
  A:='abc';
  AssertEquals('Hash',HexStr(ExpectABCFNV,8),THashFNV1a32.GetHashString(A));
end;

procedure TTestFNV.TestGetHashValueUnicode;

var
  A : Unicodestring;

begin
  A:='abc';
  AssertEquals('Hash',ExpectABCFNVUnicode,Cardinal(THashFNV1a32.GetHashValue(A)));
end;

procedure TTestFNV.TestGetHashValueRawByte;

var
  A : Ansistring;

begin
  A:='abc';
  AssertEquals('Hash',ExpectABCFNV,THashFNV1a32.GetHashValue(A));
end;

procedure TTestFNV.TestGetHashValue;
var
  A : Ansistring;

begin
  A:='abc';
  AssertEquals('Hash',ExpectABCFNV,THashFNV1a32.GetHashValue(PByte(A)^,3));
end;

{ TTestBobJenkins }

procedure TTestBobJenkins.SetUp;
begin
  inherited SetUp;
  FBJ:=THashBobJenkins.Create;
end;

procedure TTestBobJenkins.TearDown;
begin
  FBJ:=Default(THashBobJenkins);
  inherited TearDown;
end;

procedure TTestBobJenkins.TestHashAsBytes;

var
  Act : TBytes;

begin
  FBJ.Update(RawByteString('abc'));
  Act:=FBJ.HashAsBytes;
  AssertEquals('abc',ExpectABCBJ,THash.DigestAsInteger(Act));
end;

procedure TTestBobJenkins.TestGetHashBytes;

var
  Act : TBytes;

begin
  Act:=THashBobJenkins.GetHashBytes('abc');
  AssertEquals('abc',ExpectABCBJUnicode,Cardinal(THash.DigestAsInteger(Act)));
end;

procedure TTestBobJenkins.TestGetHashStringUnicode;

var
  A : Unicodestring;

begin
  A:='abc';
  AssertEquals('Hash',HexStr(ExpectABCBJUnicode,8),THashBobJenkins.GetHashString(A));
end;

procedure TTestBobJenkins.TestGetHashStringRawByte;

var
  A : Ansistring;

begin
  A:='abc';
  AssertEquals('Hash',HexStr(ExpectABCBJ,8),THashBobJenkins.GetHashString(A));
end;

procedure TTestBobJenkins.TestGetHashValueUnicode;
var
  A : UnicodeString;
  D : Integer;


begin
  A:='abc';
  D:=THashBobJenkins.GetHashValue(A);
  AssertEquals('abc',ExpectABCBJUnicode,Cardinal(D));
end;

procedure TTestBobJenkins.TestGetHashValueRawByte;

var
  A : AnsiString;
  D : Integer;


begin
  A:='abc';
  D:=Cardinal(THashBobJenkins.GetHashValue(A));
  AssertEquals('abc',ExpectABCBJ,D);
end;

procedure TTestBobJenkins.TestGetHashValue;
var
  A : AnsiString;
  D : Integer;


begin
  A:='abc';
  D:=Cardinal(THashBobJenkins.GetHashValue(PByte(A),3));
  AssertEquals('abc',ExpectABCBJ,D);
end;

initialization

  RegisterTests([TTestHash,TTestMD5,TTestSha1,TTestFNV,TTestBobJenkins,TTestSha2]);
end.

