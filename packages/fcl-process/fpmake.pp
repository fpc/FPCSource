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
    P.Version:='2.2.4';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes);
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
