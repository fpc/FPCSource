{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin


  With Installer do
    begin
{$endif ALLPACKAGES}
    P:=AddPackage('psy-q-sdk');
    P.Author := 'Kirill Kranz';
    P.License := 'GPL2 or later';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Bindings for the PSY-Q-SDK from Sony';
    P.NeedLibC:= false;
    P.OSes := [ps1];

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('libapi.pas');
    T:=P.Targets.AddUnit('libcd.pas');
    T:=P.Targets.AddUnit('libcomb.pas');
    T:=P.Targets.AddUnit('libds.pas');
    T:=P.Targets.AddUnit('libetc.pas');
    T:=P.Targets.AddUnit('libgpu.pas');
    T:=P.Targets.AddUnit('libgs.pas');
    T:=P.Targets.AddUnit('libgte.pas');
    T:=P.Targets.AddUnit('libgun.pas');
    T:=P.Targets.AddUnit('libhmd.pas');
    T:=P.Targets.AddUnit('libmcgui.pas');
    T:=P.Targets.AddUnit('libmcrd.pas');
    T:=P.Targets.AddUnit('libmcx.pas');
    T:=P.Targets.AddUnit('libpad.pas');
    T:=P.Targets.AddUnit('libpress.pas');
    T:=P.Targets.AddUnit('libsn.pas');
    T:=P.Targets.AddUnit('libsnd.pas');
    T:=P.Targets.AddUnit('libspu.pas');
    T:=P.Targets.AddUnit('libstd.pas');
    T:=P.Targets.AddUnit('libtap.pas');


    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
