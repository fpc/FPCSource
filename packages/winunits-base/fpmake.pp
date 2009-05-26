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
{$ifdef ALLPACKAGES}
    P.Directory:='winunits-base';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.OSes:=[win32,win64];
    P.Author := 'Florian Klaempfl, Marco van de Voort';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Base Delphi compatible windows headers units outside the RTL';
    P.NeedLibC:= true;

    P.SourcePath.Add('src');

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
          AddUnit('shlobj');
          AddUnit('oleserver');
          AddUnit('shfolder');
          AddUnit('richedit');
	  AddUnit('wininet');
          AddUnix('uxtheme');  
        end;
    T:=P.Targets.AddImplicitUnit('activex.pp');
    T:=P.Targets.AddImplicitUnit('comconst.pp');
    T:=P.Targets.AddImplicitUnit('commctrl.pp');
    T:=P.Targets.AddImplicitUnit('commdlg.pp');
    T:=P.Targets.AddImplicitUnit('comobj.pp');
    T:=P.Targets.AddImplicitUnit('flatsb.pp');
    T:=P.Targets.AddImplicitUnit('mmsystem.pp');
    T:=P.Targets.AddImplicitUnit('ole2.pp');
    T:=P.Targets.AddImplicitUnit('oleserver.pp');
    T:=P.Targets.AddImplicitUnit('richedit.pp');
    T:=P.Targets.AddImplicitUnit('shellapi.pp');
    T:=P.Targets.AddImplicitUnit('shfolder.pp');
    T:=P.Targets.AddImplicitUnit('shlobj.pp');
    T:=P.Targets.AddImplicitUnit('winver.pp');
    T:=P.Targets.AddImplicitUnit('wininet.pp');
    T:=P.Targets.AddImplicitUnit('imagehlp.pp');
    T:=P.Targets.AddImplicitUnit('commdlg.pp');
    T:=P.Targets.AddImplicitUnit('wininet.pp');
    T:=P.Targets.AddImplicitUnit('uxtheme.pp');
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
