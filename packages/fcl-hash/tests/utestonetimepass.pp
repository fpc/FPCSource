unit utestonetimepass;

{$mode ObjFPC}{$H+}

interface

uses
  FPCUnit, TestRegistry, Classes, SysUtils, Types, basenenc, fphashutils, onetimepass ;

type

  { TTestOnetimePass }

  TTestOnetimePass = class(TTestCase)
  Published
    Procedure Test1Interval;
    Procedure Test2Intervals;
    Procedure TestValid1;
    Procedure TestInValid1;
    Procedure TestGen;
    Procedure TestGenDefaultLength;
    Procedure TestGenCustomLength;
    Procedure TestGenTooShort;
    Procedure TestGenDiffers;
    Procedure TestGenIndependentOfRandSeed;
    Procedure TestGenCallback;
  end;

implementation

Const
  Secret = 'MFRGGZDFMZTWQ2LK';

Procedure TTestOnetimePass.Test1Interval;

begin
  AssertEquals('1 interval', 765705, HOTPCalculateToken(Secret, 1));
end;

procedure TTestOnetimePass.Test2Intervals;
begin
  AssertEquals('2 interval', 816065, HOTPCalculateToken(Secret, 2));
end;

procedure TTestOnetimePass.TestValid1;

Var
  C,Tok : LongInt;

begin
  C:=1;
  Tok:=TOTPCalculateToken(Secret);
  AssertTrue('Valid',TOTPValidate(Secret,Tok,1,C));
end;

procedure TTestOnetimePass.TestInValid1;
Var
  C,Tok : LongInt;

begin
  C:=1;
  Tok:=TOTPCalculateToken(Secret);
  AssertFalse('Invalid',TOTPValidate(Secret,Tok+1,1,C));
end;

procedure TTestOnetimePass.TestGen;

var
  lSecret : String;
  C,Tok : LongInt;

begin
  c:=1;
  lSecret:=TOTPSharedSecret();
  AssertEquals('Length',32,Length(lSecret));
  Tok:=TOTPCalculateToken(lSecret);
  AssertTrue('Valid',TOTPValidate(lSecret,Tok,1,C));
end;

procedure TTestOnetimePass.TestGenDefaultLength;

begin
  AssertEquals('default secret decodes to TOTP_SecretLength bytes', TOTP_SecretLength,
    Length(Base32.Decode(TOTPSharedSecret())));
end;


procedure TTestOnetimePass.TestGenCustomLength;

begin
  AssertEquals('a 32 byte secret decodes to 32 bytes', 32, Length(Base32.Decode(TOTPSharedSecret(Nil, 32))));
end;


procedure TTestOnetimePass.TestGenTooShort;

var
  lRaised : Boolean;

begin
  lRaised:=False;
  try
    TOTPSharedSecret(Nil, TOTP_MinSecretLength-1);
  except
    on EOTPError do
      lRaised:=True;
  end;
  AssertTrue('a secret below TOTP_MinSecretLength raises EOTPError', lRaised);
  AssertEquals('TOTP_MinSecretLength is accepted', TOTP_MinSecretLength,
    Length(Base32.Decode(TOTPSharedSecret(Nil, TOTP_MinSecretLength))));
end;


procedure TTestOnetimePass.TestGenDiffers;

begin
  AssertTrue('two generated secrets differ', TOTPSharedSecret()<>TOTPSharedSecret());
end;


procedure TTestOnetimePass.TestGenIndependentOfRandSeed;

var
  lFirst, lSecond : AnsiString;

begin
  if not OSRandomBytesAvailable then
    Ignore('no system random source on this target');
  RandSeed:=42;
  lFirst:=TOTPSharedSecret();
  RandSeed:=42;
  lSecond:=TOTPSharedSecret();
  AssertTrue('the secret does not come from the RTL Random', lFirst<>lSecond);
end;


// Fill aBytes with 1, 2, 3, ...
procedure CountingBytes(aBytes : TByteDynArray);

var
  I : Integer;

begin
  for I:=0 to Length(aBytes)-1 do
    aBytes[I]:=I+1;
end;


procedure TTestOnetimePass.TestGenCallback;

var
  lExpected : TByteDynArray;

begin
  lExpected:=[];
  SetLength(lExpected, TOTP_SecretLength);
  CountingBytes(lExpected);
  AssertEquals('the secret is made from the callback bytes', Base32.Encode(lExpected),
    TOTPSharedSecret(@CountingBytes));
end;


initialization
  RegisterTest(TTestOnetimePass);
end.

