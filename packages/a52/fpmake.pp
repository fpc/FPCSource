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
    P:=AddPackage('a52');
    P.Author := 'Library: Michel Lespinasse and Aaron Holtzman, header: Ivo Steimann';
    P.License := 'Library: GPL2 or later, header: LGPL with modification';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A free library for decoding ATSC A/52 streams.';
    P.NeedLibC:= true;

{$ifdef ALLPACKAGES}
    P.Directory:='a52';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('a52.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
