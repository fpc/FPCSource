unit utestonetimepass;

{$mode ObjFPC}{$H+}

interface

uses
  FPCUnit, TestRegistry, Classes, SysUtils, onetimepass ;

type

  { TTestOnetimePass }

  TTestOnetimePass = class(TTestCase)
  Published
    Procedure Test1Interval;
    Procedure Test2Intervals;
    Procedure TestValid1;
    Procedure TestInValid1;
    Procedure TestGen;
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
  AssertEquals('Length',16,Length(lSecret));
  Tok:=TOTPCalculateToken(lSecret);
  AssertTrue('Valid',TOTPValidate(lSecret,Tok,1,C));
end;

initialization
  RegisterTest(TTestOnetimePass);
end.

