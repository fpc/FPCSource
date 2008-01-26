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

    P:=AddPackage('imlib');
{$ifdef ALLPACKAGES}
    P.Directory:='imlib';
{$endif ALLPACKAGES}
    P.Version:='2.0.0';
    P.SourcePath.Add('src');
    P.Dependencies.Add('gtk1');
    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('gdk_imlib.pp');
      with T.Dependencies do
        begin
          AddUnit('glib');
          AddUnit('gdk');
          AddUnit('gtk');
        end;
    T:=P.Targets.AddUnit('imlib.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;


    T:=P.Targets.AddUnit('gdk_imlib.pp');
      with T.Dependencies do
        begin
          AddUnit('glib');
          AddUnit('gdk');
          AddUnit('gtk');
        end;
    T:=P.Targets.AddUnit('imlib.pp');
      with T.Dependencies do
        begin
          AddUnit('xlib');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
