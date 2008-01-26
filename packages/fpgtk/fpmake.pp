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

    P:=AddPackage('fpgtk');
{$ifdef ALLPACKAGES}
    P.Directory:='fpgtk';
{$endif ALLPACKAGES}
    P.Version:='2.0.0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fpglib.pp');
      with T.Dependencies do
        begin
          AddUnit('glib');
        end;
    T:=P.Targets.AddUnit('fpgtkext.pp');
      with T.Dependencies do
        begin
          AddUnit('fpgtk');
          AddUnit('gtk');
          AddUnit('gdk');
          AddUnit('glib');
        end;
    T:=P.Targets.AddUnit('fpgtk.pp');
      with T.Dependencies do
        begin
          AddUnit('gtk');
          AddUnit('gdk');
          AddUnit('glib');
          AddUnit('fpglib');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
