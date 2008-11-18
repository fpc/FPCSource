{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-process');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-process';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.Author := 'Michael van Canneyt and Free Pascal Development team';
    P.License := 'LGPL with modification';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Process (execution) related parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes);
    P.IncludePath.Add('src/dummy');

    T:=P.Targets.AddUnit('pipes.pp');
      T.Dependencies.AddInclude('pipes.inc');
    T:=P.Targets.AddUnit('process.pp');
      T.Dependencies.AddInclude('process.inc');
//      T.ResourceStrings:=True;
    T:=P.Targets.AddUnit('simpleipc.pp');
      T.Dependencies.AddInclude('simpleipc.inc');
      T.ResourceStrings:=True;
    T:=P.Targets.AddUnit('dbugmsg.pp');
      T.ResourceStrings:=True;
    T:=P.Targets.AddUnit('dbugintf.pp');
      T.ResourceStrings:=True;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
