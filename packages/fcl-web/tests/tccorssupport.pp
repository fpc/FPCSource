{ Tests origin matching of MatchCORSOrigin and the headers TCORSSupport.HandleRequest sets. }
unit tccorssupport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, httpdefs, httpprotocol;

type

  { TCORSTestResponse }

  // Response that sends nothing.
  TCORSTestResponse = class(TResponse)
  protected
    procedure DoSendHeaders(aHeaders: TStrings); override;
    procedure DoSendContent; override;
  end;

  { TTestMatchCORSOrigin }

  TTestMatchCORSOrigin = class(TTestCase)
  published
    procedure TestExactOrigin;
    procedure TestExactOriginCaseInsensitive;
    procedure TestOriginInList;
    procedure TestOriginNotInList;
    procedure TestSpaceSeparatedList;
    procedure TestTrailingSlashInEntry;
    procedure TestHostEntryMatchesAnyPort;
    procedure TestHostEntryMatchesAnyScheme;
    procedure TestHostPortEntry;
    procedure TestHostPortEntryOtherPort;
    procedure TestHostEntryIsNotSuffixMatch;
    procedure TestHostEntryIPv6;
    procedure TestSchemeEntryOtherScheme;
    procedure TestStarEntry;
    procedure TestEmptyOrigin;
    procedure TestOriginWithoutScheme;
  end;

  { TTestCORSSupport }

  TTestCORSSupport = class(TTestCase)
  private
    FCORS: TCORSSupport;
    FRequest: TRequest;
    FResponse: TCORSTestResponse;
    function Handle(const aOrigin: String; aOptions: THandleCORSOptions = [hcDetect]): Boolean;
    function AllowOrigin: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDisabledSetsNothing;
    procedure TestDefaultIsStar;
    procedure TestDefaultSendsNoVary;
    procedure TestEmptyAllowedOriginsIsStar;
    procedure TestListedOriginEchoed;
    procedure TestListSendsVary;
    procedure TestVaryAppended;
    procedure TestVaryNotDuplicated;
    procedure TestVaryTokenContainingOrigin;
    procedure TestUnlistedOriginGetsNoHeader;
    procedure TestUnlistedOriginGetsNoCredentials;
    procedure TestUnlistedOriginGetsNoPreflightHeaders;
    procedure TestUnlistedOriginPreflightStillSent;
    procedure TestNoOriginGetsNoHeader;
    procedure TestCredentialsWithListedOrigin;
    procedure TestPreflightHeadersWithListedOrigin;
    procedure TestLocalhostWithPort;
  end;

implementation


{ TCORSTestResponse }

procedure TCORSTestResponse.DoSendHeaders(aHeaders: TStrings);

begin
end;


procedure TCORSTestResponse.DoSendContent;

begin
end;


{ TTestMatchCORSOrigin }

procedure TTestMatchCORSOrigin.TestExactOrigin;

begin
  AssertEquals('Listed origin is returned', 'https://a.example',
    MatchCORSOrigin('https://a.example','https://a.example'));
end;


procedure TTestMatchCORSOrigin.TestExactOriginCaseInsensitive;

begin
  AssertEquals('Origin matches regardless of case and is returned as sent', 'https://A.Example',
    MatchCORSOrigin('https://a.example','https://A.Example'));
end;


procedure TTestMatchCORSOrigin.TestOriginInList;

begin
  AssertEquals('Second origin of a comma-separated list matches', 'https://b.example',
    MatchCORSOrigin('https://a.example, https://b.example','https://b.example'));
end;


procedure TTestMatchCORSOrigin.TestOriginNotInList;

begin
  AssertEquals('Unlisted origin gives an empty result', '',
    MatchCORSOrigin('https://a.example, https://b.example','https://evil.example'));
end;


procedure TTestMatchCORSOrigin.TestSpaceSeparatedList;

begin
  AssertEquals('Space-separated list matches', 'https://b.example',
    MatchCORSOrigin('https://a.example https://b.example','https://b.example'));
end;


procedure TTestMatchCORSOrigin.TestTrailingSlashInEntry;

begin
  AssertEquals('Entry with a trailing slash matches the origin', 'https://a.example',
    MatchCORSOrigin('https://a.example/','https://a.example'));
end;


procedure TTestMatchCORSOrigin.TestHostEntryMatchesAnyPort;

begin
  AssertEquals('Host entry matches the origin on any port', 'http://localhost:8080',
    MatchCORSOrigin('localhost','http://localhost:8080'));
end;


procedure TTestMatchCORSOrigin.TestHostEntryMatchesAnyScheme;

begin
  AssertEquals('Host entry matches an https origin', 'https://localhost',
    MatchCORSOrigin('localhost','https://localhost'));
end;


procedure TTestMatchCORSOrigin.TestHostPortEntry;

begin
  AssertEquals('host:port entry matches that port', 'http://localhost:3000',
    MatchCORSOrigin('localhost:3000','http://localhost:3000'));
end;


procedure TTestMatchCORSOrigin.TestHostPortEntryOtherPort;

begin
  AssertEquals('host:port entry does not match another port', '',
    MatchCORSOrigin('localhost:3000','http://localhost:4000'));
end;


procedure TTestMatchCORSOrigin.TestHostEntryIsNotSuffixMatch;

begin
  AssertEquals('Host entry does not match a host that ends with it', '',
    MatchCORSOrigin('example.com','https://evilexample.com'));
  AssertEquals('Host entry does not match a host that starts with it', '',
    MatchCORSOrigin('example.com','https://example.com.evil.net'));
end;


procedure TTestMatchCORSOrigin.TestHostEntryIPv6;

begin
  AssertEquals('Bracketed IPv6 host entry matches with a port', 'http://[::1]:8080',
    MatchCORSOrigin('[::1]','http://[::1]:8080'));
end;


procedure TTestMatchCORSOrigin.TestSchemeEntryOtherScheme;

begin
  AssertEquals('Entry with a scheme does not match another scheme', '',
    MatchCORSOrigin('https://a.example','http://a.example'));
end;


procedure TTestMatchCORSOrigin.TestStarEntry;

begin
  AssertEquals('Star entry gives a star', '*',
    MatchCORSOrigin('https://a.example, *','https://evil.example'));
end;


procedure TTestMatchCORSOrigin.TestEmptyOrigin;

begin
  AssertEquals('Empty origin matches no entry', '',
    MatchCORSOrigin('https://a.example localhost',''));
end;


procedure TTestMatchCORSOrigin.TestOriginWithoutScheme;

begin
  AssertEquals('Origin without a scheme is not matched on its host', '',
    MatchCORSOrigin('localhost','localhost:8080'));
  AssertEquals('Origin without a scheme matches an identical entry', 'null',
    MatchCORSOrigin('null','null'));
end;


{ TTestCORSSupport }

function TTestCORSSupport.Handle(const aOrigin: String; aOptions: THandleCORSOptions): Boolean;

begin
  if aOrigin<>'' then
    FRequest.SetCustomHeader('Origin',aOrigin);
  Result:=FCORS.HandleRequest(FRequest,FResponse,aOptions);
end;


function TTestCORSSupport.AllowOrigin: String;

begin
  Result:=FResponse.GetCustomHeader('Access-Control-Allow-Origin');
end;


procedure TTestCORSSupport.SetUp;

begin
  FCORS:=TCORSSupport.Create;
  FCORS.Enabled:=True;
  FRequest:=TRequest.Create;
  FRequest.Method:='GET';
  FResponse:=TCORSTestResponse.Create(FRequest);
end;


procedure TTestCORSSupport.TearDown;

begin
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  FreeAndNil(FCORS);
end;


procedure TTestCORSSupport.TestDisabledSetsNothing;

begin
  FCORS.Enabled:=False;
  Handle('https://a.example');
  AssertEquals('Disabled CORS sets no Allow-Origin', '', AllowOrigin);
  AssertEquals('Disabled CORS sets no Vary', '', FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestDefaultIsStar;

begin
  Handle('https://a.example');
  AssertEquals('Default allowed origins send a star', '*', AllowOrigin);
end;


procedure TTestCORSSupport.TestDefaultSendsNoVary;

begin
  Handle('https://a.example');
  AssertEquals('A star does not depend on the origin', '', FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestEmptyAllowedOriginsIsStar;

begin
  FCORS.AllowedOrigins:='';
  Handle('https://a.example');
  AssertEquals('Empty allowed origins send a star', '*', AllowOrigin);
end;


procedure TTestCORSSupport.TestListedOriginEchoed;

begin
  FCORS.AllowedOrigins:='https://a.example,https://b.example';
  Handle('https://b.example');
  AssertEquals('Listed origin is sent back alone', 'https://b.example', AllowOrigin);
end;


procedure TTestCORSSupport.TestListSendsVary;

begin
  FCORS.AllowedOrigins:='https://a.example';
  Handle('https://a.example');
  AssertEquals('Matched origin sends Vary: Origin', 'Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestVaryAppended;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FResponse.SetHeader(hhVary,'Accept-Encoding');
  Handle('https://a.example');
  AssertEquals('Origin is appended to an existing Vary', 'Accept-Encoding, Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestVaryNotDuplicated;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FResponse.SetHeader(hhVary,'Origin');
  Handle('https://a.example');
  AssertEquals('Vary that names Origin is kept', 'Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestVaryTokenContainingOrigin;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FResponse.SetHeader(hhVary,'X-Origin-Id');
  Handle('https://a.example');
  AssertEquals('A Vary token that only contains Origin does not count', 'X-Origin-Id, Origin',
    FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestUnlistedOriginGetsNoHeader;

begin
  FCORS.AllowedOrigins:='https://a.example,https://b.example';
  Handle('https://evil.example');
  AssertEquals('Unlisted origin gets no Allow-Origin', '', AllowOrigin);
  AssertEquals('Unlisted origin still sends Vary: Origin', 'Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestCORSSupport.TestUnlistedOriginGetsNoCredentials;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FCORS.Options:=[coAllowCredentials];
  Handle('https://evil.example');
  AssertEquals('Unlisted origin gets no Allow-Credentials', '',
    FResponse.GetCustomHeader('Access-Control-Allow-Credentials'));
end;


procedure TTestCORSSupport.TestUnlistedOriginGetsNoPreflightHeaders;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FRequest.Method:='OPTIONS';
  Handle('https://evil.example');
  AssertEquals('Unlisted origin gets no Allow-Methods', '',
    FResponse.GetCustomHeader('Access-Control-Allow-Methods'));
  AssertEquals('Unlisted origin gets no Allow-Headers', '',
    FResponse.GetCustomHeader('Access-Control-Allow-Headers'));
end;


procedure TTestCORSSupport.TestUnlistedOriginPreflightStillSent;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FRequest.Method:='OPTIONS';
  AssertTrue('Preflight of an unlisted origin is still answered with hcSend',
    Handle('https://evil.example',[hcDetect,hcSend]));
end;


procedure TTestCORSSupport.TestNoOriginGetsNoHeader;

begin
  FCORS.AllowedOrigins:='https://a.example';
  Handle('');
  AssertEquals('Request without Origin gets no Allow-Origin', '', AllowOrigin);
end;


procedure TTestCORSSupport.TestCredentialsWithListedOrigin;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FCORS.Options:=[coAllowCredentials];
  Handle('https://a.example');
  AssertEquals('Listed origin gets Allow-Credentials', 'true',
    FResponse.GetCustomHeader('Access-Control-Allow-Credentials'));
end;


procedure TTestCORSSupport.TestPreflightHeadersWithListedOrigin;

begin
  FCORS.AllowedOrigins:='https://a.example';
  FCORS.MaxAge:=600;
  FRequest.Method:='OPTIONS';
  Handle('https://a.example');
  AssertEquals('Listed origin gets Allow-Methods', DefaultAllowedMethods,
    FResponse.GetCustomHeader('Access-Control-Allow-Methods'));
  AssertEquals('Listed origin gets Allow-Headers', DefaultAllowedHeaders,
    FResponse.GetCustomHeader('Access-Control-Allow-Headers'));
  AssertEquals('Listed origin gets Max-Age', '600',
    FResponse.GetCustomHeader('Access-Control-Max-Age'));
end;


procedure TTestCORSSupport.TestLocalhostWithPort;

begin
  FCORS.AllowedOrigins:='localhost';
  FCORS.Options:=[coAllowCredentials];
  Handle('http://localhost:8080');
  AssertEquals('localhost entry allows a localhost origin with a port', 'http://localhost:8080', AllowOrigin);
end;

initialization
  RegisterTests([TTestMatchCORSOrigin,TTestCORSSupport]);
end.
