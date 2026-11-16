{ Tests the authorization and the location checks of TFPWebFileLocationAPIModule. }
unit tcfpwebfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fpcunit, testregistry, httpdefs, fpwebfile;

type

  { TTestLocationAPIModule }

  TTestLocationAPIModule = class(TFPWebFileLocationAPIModule)
  public
    // IsRequestAuthenticated with aIsRead
    function Authenticated(aRequest: TRequest; aIsRead: Boolean): Boolean;
    // The deprecated IsRequestAuthenticated
    function AuthenticatedOld(aRequest: TRequest): Boolean;
    // IsLocationAllowed
    function Allowed(const aPath: String): Boolean;
  end;

  { TTestFPWebFile }

  TTestFPWebFile = class(TTestCase)
  private
    FModule: TTestLocationAPIModule;
    FRequest: TRequest;
    FBaseDir: String;
    procedure SetPassword(const aPassword: String);
    function Dir(const aName: String): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyPasswordRead;
    procedure TestEmptyPasswordWrite;
    procedure TestPasswordNoHeader;
    procedure TestPasswordBearer;
    procedure TestPasswordWrongBearer;
    procedure TestPasswordQueryField;
    procedure TestDeprecatedOverload;
    procedure TestNoListsAllowsAll;
    procedure TestAllowedSubDir;
    procedure TestAllowedParentEscape;
    procedure TestAllowedPrefixSibling;
    procedure TestExcludedDir;
    procedure TestExcludedParentEscape;
    procedure TestRelativePath;
  end;

implementation

var
  RouteCount: Integer = 0;

{ TTestLocationAPIModule }

function TTestLocationAPIModule.Authenticated(aRequest: TRequest; aIsRead: Boolean): Boolean;
begin
  Result := IsRequestAuthenticated(aRequest, aIsRead);
end;

{$WARN 5043 OFF}
function TTestLocationAPIModule.AuthenticatedOld(aRequest: TRequest): Boolean;
begin
  Result := IsRequestAuthenticated(aRequest);
end;
{$WARN 5043 ON}

function TTestLocationAPIModule.Allowed(const aPath: String): Boolean;
begin
  Result := IsLocationAllowed(aPath);
end;

{ TTestFPWebFile }

// Sets the API password through a registration on a route that is not used yet
procedure TTestFPWebFile.SetPassword(const aPassword: String);
begin
  Inc(RouteCount);
  TFPWebFileLocationAPIModule.RegisterFileLocationAPI('/tcfpwebfile' + IntToStr(RouteCount), aPassword);
end;

function TTestFPWebFile.Dir(const aName: String): String;
begin
  Result := FBaseDir + aName;
end;

procedure TTestFPWebFile.SetUp;
begin
  FModule := TTestLocationAPIModule.CreateNew(nil, 0);
  FRequest := TRequest.Create;
  FBaseDir := IncludeTrailingPathDelimiter(ExpandFileName(GetTempDir(False))) + 'tcfpwebfile' + PathDelim;
  TFPWebFileLocationAPIModule.AllowedBaseDirs := nil;
  TFPWebFileLocationAPIModule.ExcludedBaseDirs := nil;
  TFPWebFileLocationAPIModule.AllowKeyInQueryField := False;
end;

procedure TTestFPWebFile.TearDown;
begin
  TFPWebFileLocationAPIModule.AllowedBaseDirs := nil;
  TFPWebFileLocationAPIModule.ExcludedBaseDirs := nil;
  TFPWebFileLocationAPIModule.AllowKeyInQueryField := False;
  SetPassword('');
  FreeAndNil(FRequest);
  FreeAndNil(FModule);
end;

procedure TTestFPWebFile.TestEmptyPasswordRead;
begin
  SetPassword('');
  AssertTrue('Without password a read is allowed', FModule.Authenticated(FRequest, True));
end;

procedure TTestFPWebFile.TestEmptyPasswordWrite;
begin
  SetPassword('');
  AssertFalse('Without password a write is refused', FModule.Authenticated(FRequest, False));
  FRequest.Authorization := 'Bearer ';
  AssertFalse('Without password a write with an empty bearer is refused', FModule.Authenticated(FRequest, False));
end;

procedure TTestFPWebFile.TestPasswordNoHeader;
begin
  SetPassword('secret');
  AssertFalse('A read without authorization is refused', FModule.Authenticated(FRequest, True));
  AssertFalse('A write without authorization is refused', FModule.Authenticated(FRequest, False));
end;

procedure TTestFPWebFile.TestPasswordBearer;
begin
  SetPassword('secret');
  FRequest.Authorization := 'Bearer secret';
  AssertTrue('A read with the right bearer is allowed', FModule.Authenticated(FRequest, True));
  AssertTrue('A write with the right bearer is allowed', FModule.Authenticated(FRequest, False));
end;

procedure TTestFPWebFile.TestPasswordWrongBearer;
begin
  SetPassword('secret');
  FRequest.Authorization := 'Bearer wrong';
  AssertFalse('A write with a wrong bearer is refused', FModule.Authenticated(FRequest, False));
end;

procedure TTestFPWebFile.TestPasswordQueryField;
begin
  SetPassword('secret');
  FRequest.QueryFields.Values['APIKey'] := 'secret';
  AssertFalse('The query field is ignored by default', FModule.Authenticated(FRequest, False));
  TFPWebFileLocationAPIModule.AllowKeyInQueryField := True;
  AssertTrue('The query field is used when allowed', FModule.Authenticated(FRequest, False));
  FRequest.QueryFields.Values['APIKey'] := 'wrong';
  AssertFalse('A wrong query field is refused', FModule.Authenticated(FRequest, False));
end;

procedure TTestFPWebFile.TestDeprecatedOverload;
begin
  SetPassword('secret');
  AssertFalse('Old overload without authorization', FModule.AuthenticatedOld(FRequest));
  FRequest.Authorization := 'Bearer secret';
  AssertTrue('Old overload with the right bearer', FModule.AuthenticatedOld(FRequest));
  SetPassword('');
  FRequest.Authorization := '';
  AssertTrue('Old overload without password is a read', FModule.AuthenticatedOld(FRequest));
end;

procedure TTestFPWebFile.TestNoListsAllowsAll;
begin
  AssertTrue('Without lists every path is allowed', FModule.Allowed(Dir('any')));
end;

procedure TTestFPWebFile.TestAllowedSubDir;
begin
  TFPWebFileLocationAPIModule.AllowedBaseDirs := TStringDynArray.Create(Dir('allowed'));
  AssertTrue('The allowed directory', FModule.Allowed(Dir('allowed')));
  AssertTrue('A directory below the allowed one', FModule.Allowed(Dir('allowed/sub')));
  AssertFalse('Another directory', FModule.Allowed(Dir('other')));
end;

procedure TTestFPWebFile.TestAllowedParentEscape;
begin
  TFPWebFileLocationAPIModule.AllowedBaseDirs := TStringDynArray.Create(Dir('allowed'));
  AssertFalse('.. out of the allowed directory', FModule.Allowed(Dir('allowed/../private')));
  AssertTrue('.. that stays below the allowed directory', FModule.Allowed(Dir('allowed/sub/../other')));
end;

procedure TTestFPWebFile.TestAllowedPrefixSibling;
begin
  TFPWebFileLocationAPIModule.AllowedBaseDirs := TStringDynArray.Create(Dir('allowed'));
  AssertFalse('A sibling with the allowed name as prefix', FModule.Allowed(Dir('allowedX')));
end;

procedure TTestFPWebFile.TestExcludedDir;
begin
  TFPWebFileLocationAPIModule.ExcludedBaseDirs := TStringDynArray.Create(Dir('excluded'));
  AssertFalse('The excluded directory', FModule.Allowed(Dir('excluded')));
  AssertFalse('Below the excluded directory', FModule.Allowed(Dir('excluded/sub')));
  AssertTrue('Another directory', FModule.Allowed(Dir('other')));
end;

procedure TTestFPWebFile.TestExcludedParentEscape;
begin
  TFPWebFileLocationAPIModule.ExcludedBaseDirs := TStringDynArray.Create(Dir('excluded'));
  AssertFalse('.. into the excluded directory', FModule.Allowed(Dir('other/../excluded')));
  TFPWebFileLocationAPIModule.ExcludedBaseDirs := TStringDynArray.Create(Dir('other/../excluded'));
  AssertFalse('An excluded entry with ..', FModule.Allowed(Dir('excluded')));
end;

procedure TTestFPWebFile.TestRelativePath;
var
  lOld: String;
begin
  lOld := GetCurrentDir;
  ForceDirectories(Dir('allowed'));
  try
    SetCurrentDir(Dir('allowed'));
    TFPWebFileLocationAPIModule.AllowedBaseDirs := TStringDynArray.Create(Dir('allowed'));
    AssertTrue('A relative path below the current allowed directory', FModule.Allowed('sub'));
    AssertFalse('A relative path out of the allowed directory', FModule.Allowed('../private'));
  finally
    SetCurrentDir(lOld);
    RemoveDir(Dir('allowed'));
    RemoveDir(FBaseDir);
  end;
end;

initialization
  RegisterTest(TTestFPWebFile);
end.
