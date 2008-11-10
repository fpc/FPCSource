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

    P:=AddPackage('os2units');
{$ifdef ALLPACKAGES}
    P.Directory:='os2units';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src',[OS2]);
//    P.Dependencies.Add('x11');
   // P.Targets.AddUnit('buildall.pas',[OS2]);
    P.Targets.AddUnit('clkdll.pas',[OS2]);
    P.Targets.AddUnit('dive.pas',[OS2]);
    P.Targets.AddUnit('ftpapi.pas',[OS2]);
    P.Targets.AddUnit('hwvideo.pas',[OS2]);
    P.Targets.AddUnit('lvm.pas',[OS2]);
    P.Targets.AddUnit('mciapi.pas',[OS2]);
    P.Targets.AddUnit('mcidrv.pas',[OS2]);
    P.Targets.AddUnit('mci.pas',[OS2]);
    P.Targets.AddUnit('mmbase.pas',[OS2]);
    P.Targets.AddUnit('mmio.pas',[OS2]);
    P.Targets.AddUnit('som.pas',[OS2]);
    P.Targets.AddUnit('sw.pas',[OS2]);
    P.Targets.AddUnit('wpstk.pp',[OS2]);

    P.Sources.AddSrc('readme.txt');

   // not compilable defunct for now

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('mciapi1.pas');
    P.Targets.AddExampleProgram('mciapi2.pas');
    P.Targets.AddExampleProgram('clktest.pas');
    P.Targets.AddExampleProgram('ftptest.pas');
    P.Targets.AddExampleProgram('lvmtest.pas');



{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
