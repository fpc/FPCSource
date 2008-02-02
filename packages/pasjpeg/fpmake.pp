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

    P:=AddPackage('pasjpeg');
{$ifdef ALLPACKAGES}
    P.Directory:='pasjpeg';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('buildpasjpeg.pp');
      T.Install:=False;
      with T.Dependencies do
        begin
          AddInclude('jconfig.inc');
          AddUnit('jcapimin');
          AddUnit('jcapistd');
          AddUnit('jccoefct');
          AddUnit('jccolor');
          AddUnit('jcdctmgr');
          AddUnit('jchuff');
          AddUnit('jcinit');
          AddUnit('jcmainct');
          AddUnit('jcmarker');
          AddUnit('jcmaster');
          AddUnit('jcomapi');
          AddUnit('jcparam');
          AddUnit('jcphuff');
          AddUnit('jcprepct');
          AddUnit('jcsample');
          AddUnit('jdapimin');
          AddUnit('jdapistd');
          AddUnit('jdatadst');
          AddUnit('jdatasrc');
          AddUnit('jdcoefct');
          AddUnit('jdcolor');
          AddUnit('jdct');
          AddUnit('jddctmgr');
          AddUnit('jdeferr');
          AddUnit('jdhuff');
          AddUnit('jdinput');
          AddUnit('jdmainct');
          AddUnit('jdmarker');
          AddUnit('jdmaster');
          AddUnit('jdmerge');
          AddUnit('jdphuff');
          AddUnit('jdpostct');
          AddUnit('jdsample');
          AddUnit('jerror');
          AddUnit('jfdctflt');
          AddUnit('jfdctfst');
          AddUnit('jfdctint');
          AddUnit('jidctflt');
          AddUnit('jidctfst');
          AddUnit('jidctint');
          AddUnit('jidctred');
          AddUnit('jinclude');
          AddUnit('jmemmgr');
          AddUnit('jmemnobs');
          AddUnit('jmorecfg');
          AddUnit('jpeglib');
          AddUnit('jquant1');
          AddUnit('jquant2');
          AddUnit('jutils');
        end;

    T:=P.Targets.AddImplicitUnit('jcapimin.pas');
    T:=P.Targets.AddImplicitUnit('jcapistd.pas');
    T:=P.Targets.AddImplicitUnit('jccoefct.pas');
    T:=P.Targets.AddImplicitUnit('jccolor.pas');
    T:=P.Targets.AddImplicitUnit('jcdctmgr.pas');
    T:=P.Targets.AddImplicitUnit('jchuff.pas');
    T:=P.Targets.AddImplicitUnit('jcinit.pas');
    T:=P.Targets.AddImplicitUnit('jcmainct.pas');
    T:=P.Targets.AddImplicitUnit('jcmarker.pas');
    T:=P.Targets.AddImplicitUnit('jcmaster.pas');
    T:=P.Targets.AddImplicitUnit('jcomapi.pas');
    T:=P.Targets.AddImplicitUnit('jcparam.pas');
    T:=P.Targets.AddImplicitUnit('jcphuff.pas');
    T:=P.Targets.AddImplicitUnit('jcprepct.pas');
    T:=P.Targets.AddImplicitUnit('jcsample.pas');
    T:=P.Targets.AddImplicitUnit('jdapimin.pas');
    T:=P.Targets.AddImplicitUnit('jdapistd.pas');
    T:=P.Targets.AddImplicitUnit('jdatadst.pas');
    T:=P.Targets.AddImplicitUnit('jdatasrc.pas');
    T:=P.Targets.AddImplicitUnit('jdcoefct.pas');
    T:=P.Targets.AddImplicitUnit('jdcolor.pas');
    T:=P.Targets.AddImplicitUnit('jdct.pas');
    T:=P.Targets.AddImplicitUnit('jddctmgr.pas');
    T:=P.Targets.AddImplicitUnit('jdeferr.pas');
    T:=P.Targets.AddImplicitUnit('jdhuff.pas');
    T:=P.Targets.AddImplicitUnit('jdinput.pas');
    T:=P.Targets.AddImplicitUnit('jdmainct.pas');
    T:=P.Targets.AddImplicitUnit('jdmarker.pas');
    T:=P.Targets.AddImplicitUnit('jdmaster.pas');
    T:=P.Targets.AddImplicitUnit('jdmerge.pas');
    T:=P.Targets.AddImplicitUnit('jdphuff.pas');
    T:=P.Targets.AddImplicitUnit('jdpostct.pas');
    T:=P.Targets.AddImplicitUnit('jdsample.pas');
    T:=P.Targets.AddImplicitUnit('jerror.pas');
    T:=P.Targets.AddImplicitUnit('jfdctflt.pas');
    T:=P.Targets.AddImplicitUnit('jfdctfst.pas');
    T:=P.Targets.AddImplicitUnit('jfdctint.pas');
    T:=P.Targets.AddImplicitUnit('jidctflt.pas');
    T:=P.Targets.AddImplicitUnit('jidctfst.pas');
    T:=P.Targets.AddImplicitUnit('jidctint.pas');
    T:=P.Targets.AddImplicitUnit('jidctred.pas');
    T:=P.Targets.AddImplicitUnit('jinclude.pas');
    T:=P.Targets.AddImplicitUnit('jmemmgr.pas');
    T:=P.Targets.AddImplicitUnit('jmemnobs.pas');
    T:=P.Targets.AddImplicitUnit('jmorecfg.pas');
    T:=P.Targets.AddImplicitUnit('jpeglib.pas');
    T:=P.Targets.AddImplicitUnit('jquant1.pas');
    T:=P.Targets.AddImplicitUnit('jquant2.pas');
    T:=P.Targets.AddImplicitUnit('jutils.pas');

    P.ExamplePath.Add('examples');

    T:=P.Targets.AddExampleProgram('cjpeg.pas');
    T:=P.Targets.AddExampleProgram('demo.pas');
    T:=P.Targets.AddExampleProgram('djpeg.pas');
    T:=P.Targets.AddExampleProgram('jpegtran.pas');
    T:=P.Targets.AddExampleProgram('rdjpgcom.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
