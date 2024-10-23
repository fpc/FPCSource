{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;
{$endif ALLPACKAGES}

procedure add_fcl_fpterm(const ADirectory: string);

Const
{copied from ../rtl-console/fpmake.pp}
  // All Unices have full set of KVM+Crt in unix/ except QNX which is not
  // in workable state atm.
  UnixLikes = AllUnixOSes -[QNX];

  WinEventOSes = [win32,win64];
  KVMAll       = [emx,go32v2,msdos,netware,netwlibc,os2,win32,win64,win16]+UnixLikes+AllAmigaLikeOSes;

  // all full KVMers have crt too
  CrtOSes      = KVMALL+[WatCom];
  KbdOSes      = KVMALL;
  VideoOSes    = KVMALL;
  MouseOSes    = KVMALL;
  TerminfoOSes = UnixLikes-[beos,haiku];

  rtl_consoleOSes =KVMALL+CrtOSes+TermInfoOSes;
{end of copied code}

  KVMAny       = KbdOSes+VideoOSes+MouseOSes;
  PtcKvmOSes   = [linux,win32,win64,go32v2,macosx,openbsd,freebsd];

  // OSes that have unix98pty and termio units
  UnixPtyOSes  = [linux,openbsd];

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
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    p.Dependencies.Add('rtl-console', KVMAny);
    p.Dependencies.Add('ptckvm', PtcKvmOSes);

    T:=P.Targets.AddUnit('fpterm.base.pas');

    T:=P.Targets.AddUnit('fpterm.view.pas');
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
      end;

    T:=P.Targets.AddUnit('fpterm.model.pas');
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
        AddUnit('fpterm.view');
      end;

    T:=P.Targets.AddUnit('fpterm.inputoutputconnection.pas');

    T:=P.Targets.AddUnit('fpterm.pointingdeviceinput.pas');
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
      end;

    T:=P.Targets.AddUnit('fpterm.logger.pas');

    T:=P.Targets.AddUnit('fpterm.controller.pas');
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
        AddUnit('fpterm.model');
        AddUnit('fpterm.logger');
      end;

    T:=P.Targets.AddUnit('fpterm.keyboardinput.pas');
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
      end;

    T:=P.Targets.AddUnit('fpterm.pas');
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
        AddUnit('fpterm.view');
        AddUnit('fpterm.model');
        AddUnit('fpterm.controller');
        AddUnit('fpterm.inputoutputconnection');
        AddUnit('fpterm.pointingdeviceinput');
        AddUnit('fpterm.keyboardinput');
      end;

    T:=P.Targets.AddUnit('fpterm.keyboardinput.keyboard.pas', KbdOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
        AddUnit('fpterm.keyboardinput');
      end;

    T:=P.Targets.AddUnit('fpterm.pointingdeviceinput.mouse.pas', MouseOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
        AddUnit('fpterm.pointingdeviceinput');
      end;

    T:=P.Targets.AddUnit('fpterm.view.video.base.pas', VideoOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm.base');
        AddUnit('fpterm.view');
      end;

    T:=P.Targets.AddUnit('fpterm.view.video.pas', VideoOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm.view.video.base');
      end;

    T:=P.Targets.AddUnit('fpterm.kvm.pas', KVMALL);
    with T.Dependencies do
      begin
        AddUnit('fpterm');
        AddUnit('fpterm.view');
        AddUnit('fpterm.keyboardinput');
        AddUnit('fpterm.pointingdeviceinput');
        AddUnit('fpterm.view.video');
        AddUnit('fpterm.keyboardinput.keyboard');
        AddUnit('fpterm.pointingdeviceinput.mouse');
      end;

    T:=P.Targets.AddUnit('fpterm.view.video.ptc.kvm.pas', PtcKvmOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm.view.video.base');
      end;

    T:=P.Targets.AddUnit('fpterm.ptc.kvm.pas', PtcKvmOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm');
        AddUnit('fpterm.view');
        AddUnit('fpterm.keyboardinput');
        AddUnit('fpterm.pointingdeviceinput');
        AddUnit('fpterm.view.video.ptc.kvm');
        AddUnit('fpterm.keyboardinput.keyboard');
        AddUnit('fpterm.pointingdeviceinput.mouse');
      end;

    T:=P.Targets.AddUnit('fpterm.pseudoterminal.unix.pas', UnixPtyOSes);

    T:=P.Targets.AddUnit('fpterm.inputoutputconnection.unixpty.pas', UnixPtyOSes);
    with T.Dependencies do
      begin
        AddUnit('fpterm.inputoutputconnection');
        AddUnit('fpterm.pseudoterminal.unix');
      end;
  end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fcl_fpterm('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
