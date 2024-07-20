{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('openssl');
    P.Dependencies.Add('fcl-net');
    P.ShortName:='ossl';
    P.Description := 'Interface units for OpenSSL libraries supporting SSL-encrypted network communication.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.SourcePath.Add('src');
    P.OSes := AllUnixOSes+AllWindowsOSes+[OS2,EMX]-[qnx];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-extra',[OS2,EMX]);

    T:=P.Targets.AddUnit('openssl.pas');
    T:=P.Targets.AddUnit('fpopenssl.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('opensslsockets.pp');
      T.ResourceStrings:=true;
      T.Dependencies.AddUnit('openssl');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('test1.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
