unit tchttpprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  httpprotocol;

type

  { TTestHTTPEncode }

  TTestHTTPEncode = class(TTestCase)
  published
    // Regression: with aSpacesAsPlus=False a space must become %20, not a literal space.
    procedure TestSpaceEncodedAsPercent20;
    // Backwards compatible: the default single-argument overload keeps space as +.
    procedure TestSpaceAsPlusDefault;
    // A character marked unsafe is percent-encoded even though it is normally allowed.
    procedure TestUnsafeCharEncoded;
    // Percent-encoded path round-trips through HTTPDecode with + kept literal.
    procedure TestRoundTripPath;
  end;

implementation

procedure TTestHTTPEncode.TestSpaceEncodedAsPercent20;
begin
  AssertEquals('space becomes %20','a%20b',HTTPEncode('a b',[],False));
end;


procedure TTestHTTPEncode.TestSpaceAsPlusDefault;
begin
  AssertEquals('space becomes + by default','a+b',HTTPEncode('a b'));
end;


procedure TTestHTTPEncode.TestUnsafeCharEncoded;
begin
  AssertEquals('percent marked unsafe is encoded','a%25b',HTTPEncode('a%b',[Ord('%')],False));
end;


procedure TTestHTTPEncode.TestRoundTripPath;
begin
  AssertEquals('encoded path decodes back','a b',HTTPDecode(HTTPEncode('a b',[Ord('%')],False),False));
end;


initialization
  RegisterTest(TTestHTTPEncode);
end.
