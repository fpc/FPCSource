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
    P:=AddPackage('fcl-pdf');
    P.ShortName:='fcpd';

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Author := 'Michael Van Canneyt & Graeme Geldenhuys';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'PDF generating and TTF file info library';
    P.NeedLibC:= false;
    P.OSes:=P.OSes-[embedded,win16,wince,msdos,nativent,macosclassic,palmos];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-image');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('paszlib');
    P.Dependencies.add('winunits-base',AllWindowsOSes-[wince]);
    P.Version:='3.2.3';
    P.Dependencies.add('libfontconfig',[linux] + AllBSDOses);
    T:=P.Targets.AddUnit('src/fpttfencodings.pp');
    T:=P.Targets.AddUnit('src/fpparsettf.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('src/fpfonttextmapping.pp');
    With T do
      Dependencies.AddUnit('fpttfencodings');
    T:=P.Targets.AddUnit('src/fpttfsubsetter.pp');
      T.ResourceStrings:=true;
    With T do
      begin
      Dependencies.AddUnit('fpparsettf');
      Dependencies.AddUnit('fpfonttextmapping');
      end;
    T:=P.Targets.AddUnit('src/fpttf.pp');
      T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('src/fppdf.pp');
      T.ResourceStrings:=true;
    With T do
      begin
      Dependencies.AddUnit('fpparsettf');
      Dependencies.AddUnit('fpttfsubsetter');
      Dependencies.AddInclude('src/fontmetrics_stdpdf.inc');
      end;
    T:=P.Targets.AddUnit('src/fppdfconsts.pp');
    T:=P.Targets.AddUnit('src/fppdfpredict.pp');
    T:=P.Targets.AddUnit('src/fppdfsource.pp');
    
    T:=P.Targets.AddUnit('src/fppdfobjects.pp');
    T.Dependencies.AddUnit('fppdfconsts');

    T:=P.Targets.AddUnit('src/fppdfcommands.pp');
    T.Dependencies.AddUnit('fppdfobjects');

    T:=P.Targets.AddUnit('src/fppdfscanner.pp');
    T.ResourceStrings:=true;
    T.Dependencies.AddUnit('fppdfobjects');
    T.Dependencies.AddUnit('fppdfsource');
    
    T:=P.Targets.AddUnit('src/fppdfparser.pp');
    T.ResourceStrings:=true;
    T.Dependencies.AddUnit('fppdfobjects');
    T.Dependencies.AddUnit('fppdfsource');
    T.Dependencies.AddUnit('fppdfconsts');
    T.Dependencies.AddUnit('fppdfpredict');
     
    // md5.ref
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
