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

    P:=AddPackage('cairo');
{$ifdef ALLPACKAGES}
    P.Directory:='cairo';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';
    P.SourcePath.Add('src');

    P.Dependencies.Add('x11',AllUnixOSes);
    P.Dependencies.Add('fcl-image');

    T:=P.Targets.AddUnit('cairo.pp');
    T:=P.Targets.AddUnit('cairoft.pp');
    with T.Dependencies do
      begin
        AddUnit('cairo');
        AddUnit('freetypeh');
      end;
   T:=P.Targets.AddUnit('cairoxlib.pp',AllUnixOSes);
    with T.Dependencies do
      begin
        AddUnit('cairo');
        AddUnit('xlib');
        AddUnit('xrender');
      end;
   T:=P.Targets.AddUnit('cairowin32.pp',AllWindowOses);
    with T.Dependencies do
      begin
        AddUnit('cairo');
        AddUnit('windows');
      end;
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
