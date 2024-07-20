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

    P:=AddPackage('winceunits');
    P.ShortName := 'wice';

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Version:='3.2.4-rc1';
    P.OSes := [win32, wince];

    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';

    P.SourcePath.Add('src');
    P.SupportBuildModes := [bmOneByOne];
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('rtl-extra');
   
    P.Options.Add('-Ur');

    // These units are from the winunits-base package.
    T:=P.Targets.AddUnit('../winunits-base/src/comobj.pp', [wince]);
    T:=P.Targets.AddUnit('../winunits-base/src/activex.pp', [wince]);
    T:=P.Targets.AddUnit('../winunits-base/src/ole2.pp', [wince]);
    T:=P.Targets.AddUnit('tlhelp32.pas', [wince]);
    T:=P.Targets.AddUnit('../winunits-base/src/comconst.pp', [wince]);
    T.ResourceStrings:=True;
    T:=P.Targets.AddUnit('../winunits-base/src/eventsink.pp', [wince]);
    T:=P.Targets.AddUnit('../winunits-base/src/stdole2.pas', [wince]);
    T:=P.Targets.AddUnit('../winunits-base/src/oleserver.pp', [wince]);
 
    T:=P.Targets.AddUnit('buildwinceunits.pp', [wince]);
    T.Install:=False;

    T:=P.Targets.AddImplicitUnit('aygshell.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('commctrl.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('commdlg.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('iphlpapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('notify.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('oleauto.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('power.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('shellapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('simmgr.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('tapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('gpsapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('todaycmn.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('windbase.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('gx.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('winioctl.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('msgqueue.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('pm.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('service.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('htmlctrl.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('sipapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('cpl.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('bt_api.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('bt_sdp.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('bthapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('bthutil.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('pimstore.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('ril.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('sms.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('ws2bth.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('keybd.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('nled.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('phone.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('connmgr.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('devload.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('devmgmt.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('mmreg.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('mmsystem.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('msacm.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('wininet.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('ras.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('raserror.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('sip.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('projects.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('wap.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('tsp.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('extapi.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('imm.pp', [wince]);
    T:=P.Targets.AddImplicitUnit('pnp.pas', [wince]);
    T:=P.Targets.AddImplicitUnit('storemgr.pas', [wince]);

    T:=P.Targets.AddUnit('cesync.pp', [wince,win32]);
    T:=P.Targets.AddUnit('rapitypes.pp',[wince,win32]);
    T:=P.Targets.AddUnit('rapi.pp',[win32]);
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

