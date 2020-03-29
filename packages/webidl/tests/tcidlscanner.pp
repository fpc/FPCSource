unit tcidlscanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, webidlscanner;

type

  { TTestScanner }

  TTestScanner= class(TTestCase)
  private
    FScanner: TWebIDLScanner;
    FVersion: TWEbIDLversion;
    procedure SetVersion(AValue: TWEbIDLversion);
  protected
    procedure Init(Const aSource : string);
    Class Procedure AssertEquals(Msg : String; AExpected,AActual : TIDLToken); overload;
    Procedure TestSingle(Const aSource : String; AToken : TIDLToken);
    Procedure TestMulti(Const aSource : String; AToken : Array of TIDLToken);
    Procedure TestSingle(Const aSource : String; AToken : TIDLToken; AValue : String);
    Procedure TestMulti(Const aSource : String; AToken : Array of TIDLToken; AValues : Array of String);
    procedure SetUp; override;
    procedure TearDown; override;
    Property Scanner : TWebIDLScanner Read FScanner;
    Property Version : TWEbIDLversion Read FVersion Write SetVersion;
  published
    procedure TestHookUp;
    Procedure TestComment;
    Procedure TestWhitespace;
    Procedure TestString;
    Procedure TestNumberInteger;
    Procedure TestNumberFloat;
    Procedure TestNumberHex;
    Procedure TestNumberHex2;
    // Simple (one-character) tokens
    Procedure TestComma;                 // ','
    Procedure TestColon;                 // ':'
    Procedure TestBracketOpen;           // '('
    Procedure TestBracketClose;          // ')'
    Procedure TestCurlyBraceOpen;        // '{'
    Procedure TestCurlyBraceClose;       // '}'
    Procedure TestSquaredBraceOpen;       // '['
    Procedure TestSquaredBraceClose;      // ']'
    Procedure TestIdentifier;            // Any  identifier
    Procedure TestDot;  //       '.',
    Procedure TestSemicolon;//  ';',
    Procedure TestLess;//  '<',
    Procedure TestEqual;//  '=',
    Procedure TestLarger;//  '=',
    Procedure TestQuestionMark;//  '?',
    Procedure TestMinus;//  '-',
    Procedure TestOther;// 'other',
    Procedure Testshort;// 'other',

    Procedure TestTrue;
    Procedure TestFalse;
    Procedure TestNull;
    Procedure TestAny;
    Procedure TestAttribute;
    Procedure TestCallback;
    Procedure TestConst;
    Procedure TestDeleter;
    Procedure TestDictionary;
    Procedure TestEllipsis;
    Procedure TestEnum;
    Procedure TestGetter;
    Procedure TestImplements;
    Procedure TestMapLike;
    Procedure TestSetLike;
    Procedure TestRecord;
    Procedure TestInfinity;
    Procedure TestInherit;
    Procedure TestInterface;
    Procedure TestIterable;
    Procedure TestLegacyCaller;
    Procedure TestNan;
    Procedure TestNegInfinity;
    Procedure TestOptional;
    Procedure TestOr;
    Procedure TestPartial;
    Procedure TestReadOnly;
    Procedure TestRequired;
    Procedure TestSetter;
    Procedure TestStatic;
    Procedure TestStringifier;
    Procedure TestTypedef;
    Procedure TestUnrestricted;
    Procedure TestPromise;
    Procedure TestByteString;
    Procedure TestDOMString;
    Procedure TestUSVString;
    Procedure Testboolean;
    Procedure Testbyte;
    Procedure Testdouble;
    Procedure Testfloat;
    Procedure Testlong;
    Procedure Testobject;
    Procedure Testoctet;
    Procedure Testunsigned;
    Procedure Testvoid;
  end;

implementation

uses typinfo;

procedure TTestScanner.TestHookUp;
begin
  Init('');
  AssertNotNull('Have scanner',Scanner);
end;

procedure TTestScanner.TestComment;
begin
  TestSingle('// me',tkComment);
end;

procedure TTestScanner.TestWhitespace;
begin
  TestSingle('',tkWhitespace);
end;

procedure TTestScanner.TestString;
begin
  TestSingle('"abcd"',webidlscanner.tkString,'abcd');
end;

procedure TTestScanner.TestNumberInteger;
begin
  TestSingle('123',tkNumberInteger,'123');
end;

procedure TTestScanner.TestNumberFloat;
begin
  TestSingle('1.23',tkNumberFloat,'1.23');
end;

procedure TTestScanner.TestNumberHex;
begin
  TestSingle('0xABCDEF',tkNumberInteger,'0xABCDEF');
end;

procedure TTestScanner.TestNumberHex2;
begin
  // E is special
  TestSingle('0xABCDE',tkNumberInteger,'0xABCDE');
end;

procedure TTestScanner.TestComma;
begin
  TestSingle(',',tkComma);
end;

procedure TTestScanner.TestColon;
begin
  TestSingle(':',tkColon);
end;

procedure TTestScanner.TestBracketOpen;
begin
  TestSingle('(',tkBracketOpen);
end;

procedure TTestScanner.TestBracketClose;
begin
  TestSingle(')',tkBracketClose);
end;

procedure TTestScanner.TestCurlyBraceOpen;
begin
  TestSingle('{',tkCurlyBraceOpen);
end;

procedure TTestScanner.TestCurlyBraceClose;
begin
  TestSingle('}',tkCurlyBraceClose);
end;

procedure TTestScanner.TestSquaredBraceOpen;
begin
  TestSingle('[',tkSquaredBraceOpen);
end;

procedure TTestScanner.TestSquaredBraceClose;
begin
  TestSingle(']',tkSquaredBraceClose);
end;

procedure TTestScanner.TestIdentifier;
begin
  TestSingle('A',tkIdentifier,'A');
end;

procedure TTestScanner.TestDot;
begin
  TestSingle('.',tkDot);
end;

procedure TTestScanner.TestSemicolon;
begin
  TestSingle(';',tkSemiColon);
end;

procedure TTestScanner.TestLess;
begin
  TestSingle('<',tkLess);
end;

procedure TTestScanner.TestEqual;
begin
  TestSingle('=',tkEqual);
end;

procedure TTestScanner.TestLarger;
begin
  TestSingle('>',tkLarger);
end;

procedure TTestScanner.TestQuestionMark;
begin
  TestSingle('?',tkQuestionMark);
end;

procedure TTestScanner.TestMinus;
begin
  TestSingle('-',tkMinus);
end;

procedure TTestScanner.TestOther;
begin
  TestSingle('other',tkOther);
end;

procedure TTestScanner.Testshort;
begin
  TestSingle('short',tkShort);
end;

procedure TTestScanner.TestTrue;
begin
  TestSingle('true',tkTrue);
end;

procedure TTestScanner.TestFalse;
begin
  TestSingle('false',tkFalse);
end;

procedure TTestScanner.TestNull;
begin
  TestSingle('null',tkNull);
end;

procedure TTestScanner.TestAny;
begin
  TestSingle('any',webidlscanner.tkAny);
end;

procedure TTestScanner.TestAttribute;
begin
  TestSingle('attribute',tkAttribute);
end;

procedure TTestScanner.TestCallback;
begin
  TestSingle('callback',tkCallBack);
end;

procedure TTestScanner.TestConst;
begin
  TestSingle('const',tkConst);
end;

procedure TTestScanner.TestDeleter;
begin
  TestSingle('deleter',tkDeleter);
end;

procedure TTestScanner.TestDictionary;
begin
  TestSingle('dictionary',tkDictionary);
end;

procedure TTestScanner.TestEllipsis;
begin
  TestSingle('ellipsis',tkellipsis);
end;

procedure TTestScanner.TestEnum;
begin
  TestSingle('enum',tkenum);
end;

procedure TTestScanner.TestGetter;
begin
  TestSingle('getter',tkgetter);
end;

procedure TTestScanner.TestImplements;
begin
  TestSingle('implements',tkimplements);
  Version:=v2;
  TestSingle('implements',tkIdentifier);
end;

procedure TTestScanner.TestMapLike;
begin
  Version:=v2;
  TestSingle('maplike',tkmaplike);
  Version:=v1;
  TestSingle('maplike',tkIdentifier);
end;

procedure TTestScanner.TestSetLike;
begin
  Version:=v2;
  TestSingle('setlike',tkSetlike);
  Version:=v1;
  TestSingle('setlike',tkIdentifier);
end;

procedure TTestScanner.TestRecord;
begin
  Version:=v2;
  TestSingle('record',webidlscanner.tkRecord);
  Version:=v1;
  TestSingle('record',tkIdentifier);
end;

procedure TTestScanner.TestInfinity;
begin
  TestSingle('Infinity',tkinfinity);
end;

procedure TTestScanner.TestInherit;
begin
  TestSingle('inherit',tkinherit);
end;

procedure TTestScanner.TestInterface;
begin
  TestSingle('interface',webidlscanner.tkinterface);
end;

procedure TTestScanner.TestIterable;
begin
  TestSingle('iterable',tkiterable);
end;

procedure TTestScanner.TestLegacyCaller;
begin
  TestSingle('legacycaller',tklegacycaller);
end;

procedure TTestScanner.TestNan;
begin
  TestSingle('NaN',tkNan);
end;

procedure TTestScanner.TestNegInfinity;
begin
  TestSingle('-Infinity',tkneginfinity);
end;

procedure TTestScanner.TestOptional;
begin
  TestSingle('optional',tkoptional);
end;

procedure TTestScanner.TestOr;
begin
  TestSingle('or',tkOR);
end;

procedure TTestScanner.TestPartial;
begin
  TestSingle('partial',tkPartial);
end;

procedure TTestScanner.TestReadOnly;
begin
  TestSingle('readonly',tkreadonly);
end;

procedure TTestScanner.TestRequired;
begin
  TestSingle('required',tkrequired);
end;

procedure TTestScanner.TestSetter;
begin
  TestSingle('setter',tksetter);
end;

procedure TTestScanner.TestStatic;
begin
  TestSingle('static',tkstatic);
end;

procedure TTestScanner.TestStringifier;
begin
  TestSingle('stringifier',tkstringifier);
end;

procedure TTestScanner.TestTypedef;
begin
  TestSingle('typedef',tktypeDef);
end;

procedure TTestScanner.TestUnrestricted;
begin
  TestSingle('unrestricted',tkunrestricted);
end;

procedure TTestScanner.TestPromise;
begin
  TestSingle('Promise',tkpromise);
end;

procedure TTestScanner.TestByteString;
begin
  TestSingle('ByteString',tkBytestring);
end;

procedure TTestScanner.TestDOMString;
begin
  TestSingle('DOMString',tkDOMstring);
end;

procedure TTestScanner.TestUSVString;
begin
  TestSingle('USVString',tkUSVString);
end;

procedure TTestScanner.Testboolean;
begin
  TestSingle('boolean',tkBoolean);
end;

procedure TTestScanner.Testbyte;
begin
  TestSingle('byte',tkByte);
end;

procedure TTestScanner.Testdouble;
begin
  TestSingle('double',webidlscanner.tkDouble);
end;

procedure TTestScanner.Testfloat;
begin
  TestSingle('float',webidlscanner.tkfloat);
end;

procedure TTestScanner.Testlong;
begin
  TestSingle('long',tklong);
end;

procedure TTestScanner.Testobject;
begin
  TestSingle('object',webidlscanner.tkObject);
end;

procedure TTestScanner.Testoctet;
begin
  TestSingle('octet',tkOctet);
end;

procedure TTestScanner.Testunsigned;
begin
  TestSingle('unsigned',tkUnsigned);
end;

procedure TTestScanner.Testvoid;
begin
  TestSingle('void',tkVoid);
end;

procedure TTestScanner.SetVersion(AValue: TWEbIDLversion);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
  if Assigned(FScanner) then
    FScanner.Version:=FVersion;
end;

procedure TTestScanner.Init(const aSource: string);
begin
  FreeAndNil(FScanner);
  FScanner:=TWebIDLScanner.Create(aSource);
  FScanner.Version:=FVersion;
end;

class procedure TTestScanner.AssertEquals(Msg: String; AExpected,AActual: TIDLToken);
begin
  AssertEQuals(Msg,GetEnumName(TypeInfo(TIDLToken),Ord(AExpected)),GetEnumName(TypeInfo(TIDLToken),Ord(AActual)));
end;

procedure TTestScanner.TestSingle(const aSource: String; AToken: TIDLToken);
begin
  TestMulti(aSource,[aToken]);
end;

procedure TTestScanner.TestMulti(const aSource: String;
  AToken: array of TIDLToken);

Var
  I : Integer;
  t : TIDLToken;

begin
  Init(ASource);
  I:=0;
  Repeat
    t:=Scanner.FetchToken;
    If T<>tkEOF then
      begin
      If I>High(AToken) then
        Fail(Format('"%s": Too many tokens in source (got: %d, expected: %d)',[aSource,I+1,High(aToken)+1]));
      AssertEquals('"'+ASource+'": token '+IntToStr(I),AToken[I],T);
      Inc(I);
      end
  Until (t=tkEOF);
  If I<High(AToken) then
    Fail('"'+ASource+'": Too little tokens in source');
end;

procedure TTestScanner.TestSingle(const aSource: String; AToken: TIDLToken;
  AValue: String);
begin
  TestMulti(aSource,[aToken],[aValue]);
end;

procedure TTestScanner.TestMulti(const aSource: String;
  AToken: array of TIDLToken; AValues: array of String);
Var
  I : Integer;
  t : TIDLToken;

begin
  Init(ASource);
  I:=0;
  Repeat
    t:=Scanner.FetchToken;
    If T<>tkEOF then
      begin
      If I>High(AToken) then
        Fail(Format('"%s": Too many tokens in source (got: %d, expected: %d)',[aSource,I+1,High(aToken)+1]));
      AssertEquals('"'+ASource+'": token '+IntToStr(I),AToken[I],T);
      AssertEquals('"'+ASource+'": String '+IntToStr(I),AValues[I],FScanner.CurTokenString);
      Inc(I);
      end
  Until (t=tkEOF);
  If I<High(AToken) then
    Fail('"'+ASource+'": Too little tokens in source');
end;

procedure TTestScanner.SetUp;
begin
  Version:=v1;
end;

procedure TTestScanner.TearDown;
begin
  FreeAndNil(FScanner);
end;

initialization

  RegisterTest(TTestScanner);
end.

