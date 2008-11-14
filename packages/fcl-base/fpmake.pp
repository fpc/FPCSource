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

    P:=AddPackage('fcl-base');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-base';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';

    P.Dependencies.Add('winunits-jedi',[Win32,Win64]);

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/unix',AllUnixOSes);
    P.SourcePath.Add('src/win',AllWindowsOSes);
    P.SourcePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes);
    P.IncludePath.Add('src');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes);

    T:=P.Targets.AddUnit('avl_tree.pp');
    T:=P.Targets.AddUnit('base64.pp');
    T:=P.Targets.AddUnit('blowfish.pp');
    T:=P.Targets.AddUnit('bufstream.pp');
    T:=P.Targets.AddUnit('cachecls.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('contnrs.pp');
    T:=P.Targets.AddUnit('custapp.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('daemonapp.pp',AllWindowsOSes+AllUnixOSes);
      with T.Dependencies do
        begin
          AddInclude('daemonapp.inc');
          AddUnit('custapp');
          AddUnit('eventlog');
        end;
    T:=P.Targets.AddUnit('eventlog.pp');
      T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddInclude('eventlog.inc');
        end;
    T:=P.Targets.AddUnit('fptimer.pp',AllWindowsOSes+AllUnixOSes);
    T:=P.Targets.AddUnit('gettext.pp');
    T:=P.Targets.AddUnit('idea.pp');
    T:=P.Targets.AddUnit('inicol.pp');
      T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('inifiles');
        end;
    T:=P.Targets.AddUnit('inifiles.pp');
      with T.Dependencies do
        begin
          AddUnit('contnrs');
        end;
    T:=P.Targets.AddUnit('iostream.pp');
    T:=P.Targets.AddUnit('libtar.pp');
    T:=P.Targets.AddUnit('maskutils.pp');
    T:=P.Targets.AddUnit('pooledmm.pp');
    T:=P.Targets.AddUnit('rtfpars.pp');
      with T.Dependencies do
        begin
          AddInclude('rtfdata.inc');
        end;
    T:=P.Targets.AddUnit('rttiutils.pp');
    T:=P.Targets.AddUnit('streamcoll.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('streamex.pp');
    T:=P.Targets.AddUnit('streamio.pp');
    T:=P.Targets.AddUnit('syncobjs.pp',AllOSes-[GO32v2,OS2,EMX]);
    T:=P.Targets.AddUnit('uriparser.pp');
    T:=P.Targets.AddUnit('wformat.pp');
    T:=P.Targets.AddUnit('whtml.pp');
      with T.Dependencies do
        begin
          AddUnit('wformat');
        end;
    T:=P.Targets.AddUnit('wtex.pp');
      with T.Dependencies do
        begin
          AddUnit('wformat');
        end;
    T:=P.Targets.AddUnit('fpexprpars.pp');

    // Windows units
    T:=P.Targets.AddUnit('ServiceManager.pas',[Win32,Win64]);
    T:=P.Targets.AddUnit('fileinfo',AllWindowsOSes);

    // Additional sources
    P.Sources.AddSrcFiles('src/win/fclel.*');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
