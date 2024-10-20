{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;
{$endif ALLPACKAGES}

procedure add_fcl_fpterm(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
  begin
    P:=AddPackage('fcl-fpterm');
    P.Directory:=ADirectory;
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

    T:=P.Targets.AddUnit('system.terminal.keyboardinput.pas');
    with T.Dependencies do
      begin
        AddUnit('system.terminal.base');
      end;

    T:=P.Targets.AddUnit('system.terminal.pas');
    with T.Dependencies do
      begin
        AddUnit('system.terminal.base');
        AddUnit('system.terminal.view');
        AddUnit('system.terminal.model');
        AddUnit('system.terminal.controller');
        AddUnit('system.terminal.inputoutputconnection');
        AddUnit('system.terminal.pointingdeviceinput');
        AddUnit('system.terminal.keyboardinput');
      end;

    //P.NamespaceMap:='namespaces.lst';
  end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fcl_fpterm('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
