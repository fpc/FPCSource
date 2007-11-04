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
    P.IncludePath.Add('src/unix',AllUnixOSs);
    P.IncludePath.Add('src/win',AllWindowsOSs);
    P.IncludePath.Add('src/$OS',AllOSs-AllWindowsOSs-AllUnixOSs);
    T:=Targets.AddUnit('pipes.pp');
      T.Dependencies.AddInclude('pipes.inc');
    T:=Targets.AddUnit('process.pp');
      T.Dependencies.AddInclude('process.inc');
      T.ResourceStrings:=True;
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

{$mode objfpc}{$H+}
program fpmake;

 { Generated automatically by fppkg on 4-11-07 }

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do
    begin
    {
      fcl-process
    }
    Run;
    end;
end.

