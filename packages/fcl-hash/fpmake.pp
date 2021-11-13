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
    P:=AddPackage('fcl-hash');

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Author := 'Free Pascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Several hash and cryptography algorithms requiring classes.';
    P.NeedLibC:= false;
    P.OSes:=P.OSes-[embedded,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,msdos];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('rtl-extra');

    P.Version:='3.3.1';
    T:=P.Targets.AddUnit('src/ecc.pp');
    T:=P.Targets.AddUnit('src/hashutils.pp');
    T.Dependencies.AddUnit('ecc');
    T:=P.Targets.AddUnit('src/sha256.pp');
    T.Dependencies.AddUnit('hashutils');
    T:=P.Targets.AddUnit('src/sha512.pp');
    T.Dependencies.AddUnit('hashutils');
    T:=P.Targets.AddUnit('src/asn.pp');
    T.Dependencies.AddUnit('hashutils');
    T:=P.Targets.AddUnit('src/pem.pp');
    T.Dependencies.AddUnit('hashutils');
    T.Dependencies.AddUnit('asn');
    T:=P.Targets.AddUnit('src/ecdsa.pp');
    T.Dependencies.AddUnit('hashutils');
    T.Dependencies.AddUnit('ecc');
    T.Dependencies.AddUnit('sha256');
    T:=P.Targets.AddUnit('src/onetimepass.pp');
    
    T:=P.Targets.AddExampleunit('examples/demosha256.pp');
    // md5.ref
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
