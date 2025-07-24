unit tccookies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  httpdefs;

type

  TTestTCookies = class(TTestCase)
  published
    procedure TestParseWriteRead;
  end;


implementation

procedure TTestTCookies.TestParseWriteRead;
const
  Cookies: array of String = (
    'Set-Cookie: cookie1',
    'Set-Cookie: cookie2;',
    'Set-Cookie: cookie3=1507337',
    'Set-Cookie: cookie4=1507337;',
    'Set-Cookie: cookie5=1507337; path',
    'Set-Cookie: cookie6=1507337; path=',
    'Set-Cookie: cookie7=1507337; path=/',
    'Set-Cookie: cookie8=1507337; secure',
    'Set-Cookie: cookie9=1507337; secure=',
    'Set-Cookie: cookie10=1507337; secure=false',
    'Set-Cookie: cookie11=1507337; secure; httponly',
    'Set-Cookie: cookie12=1507337; SameSite=None',
    'Set-Cookie: cookie13=1507337; SameSite=none',
    'Set-Cookie: cookie14=1507337; path=/; secure; httponly; SameSite=None'
  );
var
  Cookie: String;
  C: TCookie;
begin
  with TCookies.Create(TCookie) do
    try
      for Cookie in Cookies do
        AddFromString(Cookie);

      AssertEquals(Count, Length(Cookies));

      C := FindCookie('cookie0');
      AssertNull(C);

      C := FindCookie('cookie1');
      AssertNotNull(C);
      AssertEquals(C.Value, '');

      C := FindCookie('cookie2');
      AssertNotNull(C);
      AssertEquals(C.Value, '');

      C := FindCookie('cookie3');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');

      C := FindCookie('cookie4');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');

      C := FindCookie('cookie5');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertEquals(C.Path, '');

      C := FindCookie('cookie6');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertEquals(C.Path, '');

      C := FindCookie('cookie7');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertEquals(C.Path, '/');

      C := FindCookie('cookie8');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertTrue(C.Secure);

      C := FindCookie('cookie9');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertTrue(C.Secure);

      C := FindCookie('cookie10');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertFalse(C.Secure);

      C := FindCookie('cookie11');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertTrue(C.Secure);
      AssertTrue(C.HttpOnly);

      C := FindCookie('cookie12');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertTrue(C.SameSite = ssNone);

      C := FindCookie('cookie13');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertTrue(C.SameSite = ssNone);

      C := FindCookie('cookie14');
      AssertNotNull(C);
      AssertEquals(C.Value, '1507337');
      AssertEquals(C.Path, '/');
      AssertTrue(C.Secure);
      AssertTrue(C.HttpOnly);
      AssertTrue(C.SameSite = ssNone);
    finally
      Free;
    end;
end;



initialization

  RegisterTest(TTestTCookies);
end.

