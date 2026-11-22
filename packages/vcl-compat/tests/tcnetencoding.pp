unit tcnetencoding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, System.NetEncoding;

type

  { TTestBase64Encoding }

  TTestBase64Encoding = class(TTestCase)
  private
    FBytes: TBytes;
    FEnc: TBase64Encoding;
    FEnDefBytes: TBytes;
    procedure AssertBytes(aExpected, aActual: TBytes);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Enc : TBase64Encoding Read FEnc;
    Property DefBytes : TBytes read FBytes;
    Property EncDefBytes : TBytes read FEnDefBytes;
  published
    procedure TestHookUp;
    procedure TestBytesToString;
    procedure TestPointerLengthToString;
    procedure TestBytesToBytes;
    procedure TestStringToString;
  end;

  { TTestURLEncoding }

  TTestURLEncoding = class(TTestCase)
  private
    FEnc: TURLEncoding;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Enc : TURLEncoding Read FEnc;
  published
    procedure TestHookUp;
    Procedure TestSpace;
    procedure TestEqual;
    procedure TestAmpersand;
    procedure TestQuestion;
    procedure TestNonASCIIUnicodeString;
    procedure TestNonASCIIRawByteString;
    procedure TestNonASCIIString;
    procedure TestEncodeQueryUnsafe;
    procedure TestEncodeWithEncoding;
  end;


  { TTestHTMLEncoding }

  TTestHTMLEncoding = class(TTestCase)
  private
    FEnc: THTMLEncoding;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Enc : THTMLEncoding Read FEnc;
  published
    procedure TestHookUp;
    procedure TestLessThan;
    procedure TestGreaterThan;
    procedure TestAmpersand;
    procedure TestBeforeAfter;
  end;


implementation

{ TTestHTMLEncoding }

procedure TTestHTMLEncoding.SetUp;
begin
  inherited SetUp;
  FEnc:=THTMLEncoding.Create;
end;

procedure TTestHTMLEncoding.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FEnc);
end;

procedure TTestHTMLEncoding.TestHookUp;
begin
  AssertNotNull('Enc',Enc);
end;

procedure TTestHTMLEncoding.TestLessThan;
begin
  AssertEquals('from lessThan','&lt;',Enc.Encode('<'));
  AssertEquals('To lessthan','<',Enc.Decode('&lt;'));
end;

procedure TTestHTMLEncoding.TestGreaterThan;
begin
  AssertEquals('from greaterThan','&gt;',Enc.Encode('>'));
  AssertEquals('To greaterthan','>',Enc.Decode('&gt;'));
end;

procedure TTestHTMLEncoding.TestAmpersand;
begin
  AssertEquals('from ampersand','&amp;',Enc.Encode('&'));
  AssertEquals('To ampersand','&',Enc.Decode('&amp;'));
end;

procedure TTestHTMLEncoding.TestBeforeAfter;
begin
  AssertEquals('from ','A&amp;B',Enc.Encode('A&B'));
  AssertEquals('To ','A&B',Enc.Decode('A&amp;B'));
end;

{ TTestURLEncoding }

procedure TTestURLEncoding.SetUp;
begin
  inherited SetUp;
  FEnc:=TURLENcoding.Create;
end;

procedure TTestURLEncoding.TearDown;
begin
  FreeAndNil(FEnc);
  inherited TearDown;
end;

procedure TTestURLEncoding.TestHookUp;
begin
  AssertNotNull('Enc',Enc);
end;

procedure TTestURLEncoding.TestSpace;
begin
  AssertEquals('Space','+',Enc.Encode(' '));
end;

procedure TTestURLEncoding.TestEqual;
begin
  AssertEquals('from Equal','%'+hexStr(Ord('='),2),Enc.Encode('='));
  AssertEquals('To Equal','=',Enc.Decode('%'+hexStr(Ord('='),2)));
end;

procedure TTestURLEncoding.TestAmpersand;
begin
  AssertEquals('From Ampersand','%'+hexStr(Ord('&'),2),Enc.Encode('&'));
  AssertEquals('To Ampersand','&',Enc.Decode('%'+hexStr(Ord('&'),2)));
end;

procedure TTestURLEncoding.TestQuestion;
begin
  AssertEquals('From QuestionMark','%'+hexStr(Ord('?'),2),Enc.Encode('?'));
  AssertEquals('To questionmark','?',Enc.Decode('%'+hexStr(Ord('?'),2)));
end;

const
  NonASCIIEncoded = 'a%C2%A3b%E2%82%ACc';

// a, pound sign, b, euro sign, c
function NonASCIIText : UnicodeString;

begin
  Result:='a'+WideChar($00A3)+'b'+WideChar($20AC)+'c';
end;


procedure TTestURLEncoding.TestNonASCIIUnicodeString;
begin
  AssertEquals('Encode UnicodeString',UnicodeString(NonASCIIEncoded),Enc.Encode(NonASCIIText));
  AssertEquals('Decode UnicodeString',NonASCIIText,Enc.Decode(UnicodeString(NonASCIIEncoded)));
end;

procedure TTestURLEncoding.TestNonASCIIRawByteString;

var
  lUTF8 : UTF8String;
  lDecoded : RawByteString;

begin
  lUTF8:=UTF8Encode(NonASCIIText);
  AssertEquals('Encode RawByteString',UnicodeString(NonASCIIEncoded),UnicodeString(Enc.Encode(RawByteString(lUTF8))));
  lDecoded:=Enc.Decode(RawByteString(NonASCIIEncoded));
  AssertEquals('Decode RawByteString code page',CP_UTF8,StringCodePage(lDecoded));
  AssertEquals('Decode RawByteString',NonASCIIText,UTF8Decode(lDecoded));
end;

procedure TTestURLEncoding.TestNonASCIIString;

var
  lText : string;

begin
  lText:=NonASCIIText;
  AssertEquals('EncodeQuery',UnicodeString(NonASCIIEncoded),UnicodeString(Enc.EncodeQuery(lText,[])));
  AssertEquals('EncodePath',UnicodeString('/x/'+NonASCIIEncoded),UnicodeString(Enc.EncodePath('/x/'+lText,[])));
  AssertEquals('Encode with options',UnicodeString(NonASCIIEncoded),UnicodeString(Enc.Encode(lText,[],[])));
  AssertEquals('URIDecode',NonASCIIText,UnicodeString(TURLEncoding.URIDecode(NonASCIIEncoded,False)));
end;

procedure TTestURLEncoding.TestEncodeQueryUnsafe;
begin
  AssertEquals('Quote and percent encoded, letters kept','L%27%25',Enc.EncodeQuery('L''%',[]));
end;

procedure TTestURLEncoding.TestEncodeWithEncoding;
begin
  AssertEquals('UTF-16LE bytes','a%00b%00',Enc.Encode('ab',[],[],TEncoding.Unicode));
end;

{ TTestBase64Encoding }

Const
  // Sequence of 5 bytes: 0,1,2,3,4 base64 encoded
  SDefBytes = 'AQIDBAU=';
  // Sequence of 5 letters: ABCDE base64 encoded
  SDefLetters = 'QUJDREU=';

procedure TTestBase64Encoding.SetUp;

Var
  I : integer;

begin
  Inherited;
  FEnc:=TBase64Encoding.Create;
  SetLength(FBytes,5);
  For I:=0 to 4 do
    FBytes[I]:=I+1;
  SetLength(FEnDefBytes,Length(SDefBytes));
  For I:=0 to Length(SDefBytes)-1 do
    FEnDefBytes[I]:=Ord(SDefBytes[I+1]);

end;

procedure TTestBase64Encoding.TearDown;
begin
  FreeAndNil(FEnc);
  Inherited;
end;

procedure TTestBase64Encoding.TestHookUp;
begin
  AssertNotNull('Enc',Enc);
end;

procedure TTestBase64Encoding.AssertBytes(aExpected,aActual : TBytes);

Var
  I : Integer;

begin
  AssertEquals('Length same',Length(aExpected),Length(aActual));
  For I:=0 to Length(aExpected)-1 do
    AssertEquals(Format('Byte at pos %d same',[i]),aExpected[i],aActual[i]);
end;

procedure TTestBase64Encoding.TestBytesToString;
begin
  AssertEquals('Encoding', SDefBytes,Enc.EncodeBytesToString(DefBytes));
end;

procedure TTestBase64Encoding.TestBytesToBytes;
begin
  AssertBytes(EncDefBytes,Enc.Encode(DefBytes));
end;

procedure  TTestBase64Encoding.TestPointerLengthToString;

begin
 AssertEquals('Encoding',SDefBytes,Enc.EncodeBytesToString(Pointer(DefBytes),Length(DefBytes)));
end;

procedure TTestBase64Encoding.TestStringToString;
begin
  AssertEquals('Encoding', SDefLetters,Enc.Encode('ABCDE'));
end;

initialization

  RegisterTests([TTestBase64Encoding,TTestURLEncoding,TTestHTMLEncoding]);
end.

