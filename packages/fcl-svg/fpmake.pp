{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-svg');
    P.ShortName:='svg';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-image');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-css');
    if Defaults.OS in AllUnixOSes then
      P.Dependencies.Add('libfontconfig');
    P.OSes := [darwin, win32, win64, linux, solaris] + AllBSDOses; // Darwin was tested!
    P.Author := 'Michael Van Canneyt of the Free Pascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'SVG parsing and rendering.';
    P.NeedLibC:= false;
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('fpsvg.strings.pp');
    T:=P.Targets.AddUnit('fpsvg.types.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
        end;
    T:=P.Targets.AddUnit('fpsvg.backend.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
        end;
    T:=P.Targets.AddUnit('fpsvg.trace.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.backend');
        end;
    T:=P.Targets.AddUnit('fpsvg.dom.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
        end;
    T:=P.Targets.AddUnit('fpsvg.path.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.types');
        end;
    T:=P.Targets.AddUnit('fpsvg.geom.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
        end;
    T:=P.Targets.AddUnit('fpsvg.raster.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.geom');
        end;
    T:=P.Targets.AddUnit('fpsvg.soft.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.backend');
          AddUnit('fpsvg.geom');
          AddUnit('fpsvg.raster');
        end;
    T:=P.Targets.AddUnit('fpsvg.style.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.dom');
          AddUnit('fpsvg.read');
        end;
    T:=P.Targets.AddUnit('fpsvg.read.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.dom');
          AddUnit('fpsvg.path');
        end;
    T:=P.Targets.AddUnit('fpsvg.anim.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.dom');
          AddUnit('fpsvg.path');
          AddUnit('fpsvg.read');
          AddUnit('fpsvg.geom');
        end;
    T:=P.Targets.AddUnit('fpsvg.render.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.dom');
          AddUnit('fpsvg.read');
          AddUnit('fpsvg.path');
          AddUnit('fpsvg.style');
          AddUnit('fpsvg.geom');
          AddUnit('fpsvg.text');
          AddUnit('fpsvg.backend');
        end;
    T:=P.Targets.AddUnit('fpsvg.text.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.dom');
          AddUnit('fpsvg.read');
          AddUnit('fpsvg.style');
        end;
    T:=P.Targets.AddUnit('fpsvg.freetype.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
        end;
    T:=P.Targets.AddUnit('fpsvg.pp');
      with T.Dependencies do
        begin
          AddUnit('fpsvg.strings');
          AddUnit('fpsvg.types');
          AddUnit('fpsvg.backend');
          AddUnit('fpsvg.trace');
          AddUnit('fpsvg.dom');
          AddUnit('fpsvg.path');
          AddUnit('fpsvg.read');
          AddUnit('fpsvg.anim');
          AddUnit('fpsvg.style');
          AddUnit('fpsvg.geom');
          AddUnit('fpsvg.text');
          AddUnit('fpsvg.render');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
