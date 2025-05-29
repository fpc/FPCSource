{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;
{$endif ALLPACKAGES}

procedure add_rtl_unicode(const ADirectory: string);

Const
  CollationOSes = [aix,android,macosclassic,darwin,emx,freebsd,go32v2,linux,netbsd,openbsd,os2,solaris,win32,win64,dragonfly,haiku,freertos,watcom,wasip1,wasip1threads];
  CPUnits       = [aix,amiga,aros,android,beos,macosclassic,darwin,iphonesim,ios,emx,gba,nds,freebsd,go32v2,haiku,linux,morphos,netbsd,netware,netwlibc,openbsd,os2,solaris,watcom,wii,win32,win64,wince,dragonfly,freertos,wasip1,wasip1threads];
  utf8bidiOSes  = [netware,netwlibc];
  freebidiOSes  = [netware,netwlibc];
  UnicodeBaseOnlyOSes   = [msdos,win16];
  UnicodeAllOSes =   CollationOSes + utf8bidiOSes + freebidiOSes + CPUnits + UnicodeBaseOnlyOSes;

  GraphemeBreakPropertyOSes = UnicodeAllOSes;
  EastAsianWidthOSes        = UnicodeAllOSes;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('rtl-unicode');
    P.ShortName:='rtlu';
    P.Directory:=ADirectory;
    P.Version:='3.3.1';
    P.Author := 'FPC core team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes:=unicodeAllOSes;
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Email := '';
    P.Description := 'Rtl-unicode, misc Unicode units';
    P.NeedLibC:= false;
    
    P.Dependencies.Add('rtl-objpas');

    P.SourcePath.Add('src/inc');
    P.SourcePath.Add('src/collations');
    P.IncludePath.Add('src/inc');
    P.IncludePath.Add('src/collations');

    T:=P.Targets.AddUnit('utf8utils.pp');
    
    T:=P.Targets.AddUnit('unicodeducet.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('ucadata.inc');
        AddInclude('ucadata_le.inc');
        AddInclude('ucadata_be.inc');
      end;
    T:=P.Targets.AddUnit('buildcollations.pas',CollationOSes);
    T.Install:=False;
    with T.Dependencies do
      begin
        AddUnit('collation_ru');
        AddUnit('collation_de');
        AddUnit('collation_ja');
        AddUnit('collation_sv');
        AddUnit('collation_es');
        AddUnit('collation_ko');
        AddUnit('collation_fr_ca');
        AddUnit('collation_zh');
      end;

    T:=P.Targets.AddImplicitUnit('collation_ru.pas',CollationOSes);
    T:=P.Targets.AddImplicitUnit('collation_de.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('collation_de_le.inc');
      end;
    T:=P.Targets.AddImplicitUnit('collation_ja.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('collation_ja_le.inc');
        AddInclude('collation_ja_be.inc');
      end;
    T:=P.Targets.AddImplicitUnit('collation_sv.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('collation_sv_be.inc');
        AddInclude('collation_sv_le.inc');
      end;
    T:=P.Targets.AddImplicitUnit('collation_es.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('collation_es_le.inc');
        AddInclude('collation_es_be.inc');
      end;
    T:=P.Targets.AddImplicitUnit('collation_ko.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('collation_ko_be.inc');
        AddInclude('collation_ko_le.inc');
      end;
    T:=P.Targets.AddImplicitUnit('collation_fr_ca.pas',CollationOSes);
    T:=P.Targets.AddImplicitUnit('collation_zh.pas',CollationOSes);
    with T.Dependencies do
      begin
        AddInclude('collation_zh_be.inc');
        AddInclude('collation_zh_le.inc');
      end;

    T:=P.Targets.AddUnit('freebidi.pp',freebidiOSes);
    T:=P.Targets.AddUnit('utf8bidi.pp',utf8bidiOSes);

    with T.Dependencies do
      AddUnit('freebidi');

    T:=P.Targets.AddUnit('cpbuildu.pp',CPUnits);
    T.Install:=False;
    with T.Dependencies do
      begin
        AddUnit('cp895',CPUnits);
        AddUnit('cp932',CPUnits);
        AddUnit('cp936',CPUnits);
        AddUnit('cp949',CPUnits);
        AddUnit('cp950',CPUnits);
      end;
    T:=P.Targets.AddImplicitUnit('cp895.pas',CPUnits);
    T:=P.Targets.AddImplicitUnit('cp932.pas',CPUnits);
    T:=P.Targets.AddImplicitUnit('cp936.pas',CPUnits);
    T:=P.Targets.AddImplicitUnit('cp949.pas',CPUnits);
    T:=P.Targets.AddImplicitUnit('cp950.pas',CPUnits);

//    T:=P.Targets.AddUnit('character.pp',characterOSes);

    T:=P.Targets.AddUnit('graphemebreakproperty.pp',GraphemeBreakPropertyOSes);
    with T.Dependencies do
      begin
        AddInclude('graphemebreakproperty_code.inc');
      end;

    T:=P.Targets.AddUnit('eastasianwidth.pp',EastAsianWidthOSes);
    with T.Dependencies do
      begin
        AddInclude('eastasianwidth_code.inc');
      end;

    P.NamespaceMap:='namespaces.lst';
    
  end
end;

{$ifndef ALLPACKAGES}
begin
  add_rtl_unicode('');
  Installer.Run;
end.
{$endif ALLPACKAGES}

