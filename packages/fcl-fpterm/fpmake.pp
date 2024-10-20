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

    P:=AddPackage('fcl-fpterm');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';

    P.Author := 'Nikolay Nikolov';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'https://sourceforge.net/projects/fpterm/';
    P.Description := 'Terminal emulator library, written in Free Pascal.';
    P.OSes := P.OSes - [embedded,nativent,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,ps1];
//    p.OSes:=[linux,win32,win64,go32v2,macosx,openbsd,freebsd];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('system.terminal.base.pas');

    T:=P.Targets.AddUnit('system.terminal.view.pas');
    with T.Dependencies do
      begin
        AddUnit('system.terminal.base');
      end;

    T:=P.Targets.AddUnit('system.terminal.model.pas');
    with T.Dependencies do
      begin
        AddUnit('system.terminal.base');
        AddUnit('system.terminal.view');
      end;

    T:=P.Targets.AddUnit('system.terminal.inputoutputconnection.pas');

    T:=P.Targets.AddUnit('system.terminal.pointingdeviceinput.pas');
    with T.Dependencies do
      begin
        AddUnit('system.terminal.base');
      end;

    T:=P.Targets.AddUnit('system.terminal.logger.pas');

    T:=P.Targets.AddUnit('system.terminal.controller.pas');
    with T.Dependencies do
      begin
        AddUnit('system.terminal.base');
        AddUnit('system.terminal.model');
        AddUnit('system.terminal.logger');
      end;

    //P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
