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

    P:=AddPackage('cdrom');
{$ifdef ALLPACKAGES}
    P.Directory:='cdrom';
{$endif ALLPACKAGES}
    P.Version:='2.0.0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('cdrom.pp');
      with T.Dependencies do
        begin
          AddInclude('cdromlin.inc');
          AddUnit('lincd');
        end;
    T:=P.Targets.AddUnit('discid.pp');
      with T.Dependencies do
        begin
          AddUnit('cdrom');
        end;
    T:=P.Targets.AddUnit('lincd.pp');
      with T.Dependencies do
        begin
          AddUnit('major');
        end;
    T:=P.Targets.AddUnit('major.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
