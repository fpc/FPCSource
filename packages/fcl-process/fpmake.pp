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

    P:=StartPackage('fcl-process');
{$ifdef ALLPACKAGES}
    Directory:='fcl-process';
{$endif ALLPACKAGES}
    Version:='2.0.0';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes);
    T:=Targets.AddUnit('pipes.pp');
      T.Dependencies.AddInclude('pipes.inc');
    T:=Targets.AddUnit('process.pp');
      T.Dependencies.AddInclude('process.inc');
//      T.ResourceStrings:=True;
    T:=Targets.AddUnit('simpleipc.pp');
      T.Dependencies.AddInclude('simpleipc.inc');
      T.ResourceStrings:=True;
    T:=Targets.AddUnit('dbugmsg.pp');
      T.ResourceStrings:=True;
    T:=Targets.AddUnit('dbugintf.pp');
      T.ResourceStrings:=True;
    EndPackage;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
