{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

{$endif ALLPACKAGES}

procedure add_gitlab(const ADirectory: string);

Const
  TargetsWithfpWeb = [linux,beos,haiku,freebsd,netbsd,openbsd,darwin,iphonesim,ios,solaris,win32,win64,wince,aix,dragonfly];

Var
  P : TPackage;
begin
  With Installer do
    begin
    P:=AddPackage('gitlab');
    P.ShortName:='gitlab';
    P.Directory:=ADirectory;
    P.Version:='3.3.1';
    P.OSes := TargetsWithfpWeb;
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('openssl',AllUnixOSes+AllWindowsOSes);
    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Simple client for Gitlab v4 API';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');
    T:=P.Targets.addUnit('gitlabclient.pas');
    end;
end;
    
{$ifndef ALLPACKAGES}
begin
  add_gitlab('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
