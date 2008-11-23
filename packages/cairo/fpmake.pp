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
    P.Version:='2.2.2-0';
    P.Author :=  'Library:  University of Southern California + Red Hat Inc., header: Luiz AmXrico Pereira CXmara';
    P.License := 'Library: MPL 1.1 + LGPL-2.1, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'a vector graphics library with display and print output';
    P.NeedLibC:= true;

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
   T:=P.Targets.AddUnit('cairowin32.pp',AllWindowsOses);
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
