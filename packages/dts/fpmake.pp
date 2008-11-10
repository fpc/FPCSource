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

    P:=AddPackage('dts');
{$ifdef ALLPACKAGES}
    P.Directory:='dts';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';

    P.Author := 'Library: Gildas Bazin, header: Ivo Steinmann';
    P.License := 'Library: GPL2 or later, header: LGPL with modification, ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'a low-level interface to decoding audio frames encoded using DTS Coherent Acoustics';
    P.NeedLibC:= true;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('dts.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
