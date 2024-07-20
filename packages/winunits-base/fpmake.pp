{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('winunits-base');
    P.ShortName:='wib';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.OSes:=[win32,win64];
    P.Author := 'Florian Klaempfl, Marco van de Voort';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Base Delphi compatible windows headers units outside the RTL';
    P.NeedLibC:= true;

    P.Dependencies.Add('fcl-registry');
    P.Dependencies.Add('fcl-base');
    { for WinSock2 }
    P.Dependencies.Add('rtl-extra');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('buildwinutilsbase.pp');
      T.Install:=False;
      with T.Dependencies do
        begin
          AddUnit('flatsb');
          AddUnit('winver');
          AddUnit('mmsystem');
          AddUnit('comconst');
          AddUnit('commctrl');
          AddUnit('commdlg');
          AddUnit('comobj');
          AddUnit('ole2');
          AddUnit('activex');
          AddUnit('shellapi');
          AddUnit('shlwapi');
          AddUnit('shlobj');
          AddUnit('oleserver');
          AddUnit('shfolder');
          AddUnit('richedit');
          AddUnit('imagehlp');
          AddUnit('winhttp');
	  AddUnit('wininet');
          AddUnit('uxtheme');
          AddInclude('tmschema.inc');
          AddUnit('dwmapi');
          AddUnit('multimon');
          AddUnit('htmlhelp');
          AddUnit('winutils');
          AddUnit('comserv');
          AddUnit('winspool');
          AddUnit('imm');
          AddUnit('imm_dyn');
          AddUnit('nb30');
          AddUnit('stdole2');
          AddUnit('eventsink');
          AddUnit('typelib');
          AddUnit('libkinect10');
          AddUnit('urlmon');
          AddUnit('httpapi');
        end;
    T:=P.Targets.AddImplicitUnit('activex.pp');
    T:=P.Targets.AddImplicitUnit('urlmon.pp');
    T:=P.Targets.AddImplicitUnit('comconst.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddImplicitUnit('commctrl.pp');
    T:=P.Targets.AddImplicitUnit('commdlg.pp');
    T:=P.Targets.AddImplicitUnit('comobj.pp');
    T:=P.Targets.AddImplicitUnit('flatsb.pp');
    T:=P.Targets.AddImplicitUnit('mmsystem.pp');
    T:=P.Targets.AddImplicitUnit('ole2.pp');
    T:=P.Targets.AddImplicitUnit('oleserver.pp');
    T:=P.Targets.AddImplicitUnit('richedit.pp');
    T:=P.Targets.AddImplicitUnit('shellapi.pp');
    T:=P.Targets.AddImplicitUnit('shlwapi.pp');
    T:=P.Targets.AddImplicitUnit('shfolder.pp');
    T:=P.Targets.AddImplicitUnit('shlobj.pp');
    T:=P.Targets.AddImplicitUnit('winver.pp');
    T:=P.Targets.AddImplicitUnit('wininet.pp');
    T:=P.Targets.AddImplicitUnit('imagehlp.pp');
    T:=P.Targets.AddImplicitUnit('winhttp.pp');
    T:=P.Targets.AddImplicitUnit('commdlg.pp');
    T:=P.Targets.AddImplicitUnit('wininet.pp');
    T:=P.Targets.AddImplicitUnit('uxtheme.pp');
    T:=P.Targets.AddImplicitUnit('multimon.pp');
    T:=P.Targets.AddImplicitUnit('dwmapi.pp');
    T:=P.Targets.AddImplicitUnit('htmlhelp.pp');
    T:=P.Targets.AddImplicitUnit('winutils.pp');
    T:=P.Targets.AddImplicitUnit('comserv.pp');
    T:=P.Targets.AddImplicitUnit('winspool.pp');
    T:=P.Targets.AddImplicitUnit('imm.pas');
    T:=P.Targets.AddImplicitUnit('imm_dyn.pas');
    T:=P.Targets.AddImplicitUnit('nb30.pp');
    T:=P.Targets.AddImplicitUnit('stdole2.pas');
    T:=P.Targets.AddImplicitUnit('eventsink.pp');
    T:=P.Targets.AddImplicitUnit('typelib.pas');
    T:=P.Targets.AddImplicitUnit('libkinect10.pp');
    T:=P.Targets.AddImplicitUnit('httpapi.pp');
    T.Dependencies.AddInclude('tmschema.inc');
    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('testcom1.pp');
    P.Targets.AddExampleProgram('OOTest.pp');
    P.Targets.AddExampleProgram('OOHelper.pp');
    P.Targets.AddExampleProgram('testver.pp');
    P.Targets.AddExampleProgram('testcom2.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
