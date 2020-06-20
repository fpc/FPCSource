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

    P:=AddPackage('pcap');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.1';
    P.SourcePath.Add('src');
    P.OSes := AllUnixOSes+[win32]-[qnx];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-extra');

    T:=P.Targets.AddUnit('pcap.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
