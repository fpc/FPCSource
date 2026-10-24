{ %target=linux,freebsd,netbsd,openbsd,dragonfly,solaris,haiku,darwin }
{ GetAppConfigDir and GetAppConfigFile on Unix, and UseDarwinAppSupportDir on Darwin }
program tappconfigdir;

{$mode objfpc}{$H+}

uses
  SysUtils;

{$if not defined(darwin) and declared(UseDarwinAppSupportDir)}
{$error UseDarwinAppSupportDir is declared outside Darwin}
{$endif}

var
  gErrors: Integer = 0;


procedure CheckEqual(const aName, aValue, aExpected: string);

begin
  if aValue=aExpected then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR: got "',aValue,'", expected "',aExpected,'"');
    Inc(gErrors);
    end;
end;


function GetTestApplicationName: string;

begin
  Result:='tapp';
end;


function GetTestVendorName: string;

begin
  Result:='tvendor';
end;


// The user directory: XDG_CONFIG_HOME if set, else aDefault below HOME.
function UserBaseDir(const aDefault: string): string;

begin
  Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if Result<>'' then
    Result:=IncludeTrailingPathDelimiter(Result)
  else
    Result:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+aDefault;
end;


procedure CheckBaseDirs(const aPrefix, aUserBase, aGlobalBase: string);

begin
  CheckEqual(aPrefix+'GetAppConfigDir(False)',GetAppConfigDir(False),aUserBase+'tvendor/tapp/');
  CheckEqual(aPrefix+'GetAppConfigDir(True)',GetAppConfigDir(True),aGlobalBase+'tvendor/tapp/');
  CheckEqual(aPrefix+'GetAppConfigFile(False,False)',GetAppConfigFile(False,False),aUserBase+'tapp.cfg');
  CheckEqual(aPrefix+'GetAppConfigFile(False,True)',GetAppConfigFile(False,True),aUserBase+'tvendor/tapp/tapp.cfg');
  CheckEqual(aPrefix+'GetAppConfigFile(True,False)',GetAppConfigFile(True,False),aGlobalBase+'tapp.cfg');
  CheckEqual(aPrefix+'GetAppConfigFile(True,True)',GetAppConfigFile(True,True),aGlobalBase+'tvendor/tapp/tapp.cfg');
end;


begin
  OnGetApplicationName:=@GetTestApplicationName;
  OnGetVendorName:=@GetTestVendorName;
{$ifdef darwin}
  CheckEqual('UseDarwinAppSupportDir default',BoolToStr(UseDarwinAppSupportDir,True),'False');
{$endif}
  CheckBaseDirs('',UserBaseDir('.config/'),'/etc/');
{$ifdef darwin}
  UseDarwinAppSupportDir:=True;
  CheckBaseDirs('Application Support: ',UserBaseDir('Library/Application Support/'),'/Library/Application Support/');
  UseDarwinAppSupportDir:=False;
  CheckBaseDirs('Reset: ',UserBaseDir('.config/'),'/etc/');
{$endif}
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
