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
    P:=AddPackage('hash');

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Author := 'Free Pascal development team, Mark Adler, Jacques Nomssi Nzali, Silvio Clecio';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Several hash and cryptography algorithms (MD5,CRC,Linux crypt and NTLM1).';
    P.NeedLibC:= false;
    P.OSes:=P.OSes-[embedded,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-base');

    P.Version:='3.3.1';
    T:=P.Targets.AddUnit('src/md5.pp');
    T.Dependencies.AddInclude('src/md5i386.inc', [i386], AllOSes-[darwin]);
    T:=P.Targets.AddUnit('src/sha1.pp');
    T.Dependencies.AddInclude('src/sha1i386.inc', [i386], AllOSes-[darwin]);
    T:=P.Targets.AddUnit('src/ecc.pp');
    T:=P.Targets.AddUnit('src/hashutils.pp');
    T.Dependencies.AddUnit('ecc');
    T:=P.Targets.AddUnit('src/sha256.pp');
    T.Dependencies.AddUnit('hashutils');
    T:=P.Targets.AddUnit('src/sha512.pp');
    T.Dependencies.AddUnit('hashutils');
    T:=P.Targets.AddUnit('src/onetimepass.pp');
    T:=P.Targets.AddUnit('src/crc.pas');
    T:=P.Targets.AddUnit('src/ntlm.pas');
    T:=P.Targets.AddUnit('src/uuid.pas');
    T:=P.Targets.AddUnit('src/hmac.pp');
    T:=P.Targets.AddUnit('src/unixcrypt.pas');
    
    T.OSes:=[Linux];
    T:=P.Targets.AddExampleunit('examples/mdtest.pas');
    T:=P.Targets.AddExampleunit('examples/crctest.pas');
    T:=P.Targets.AddExampleunit('examples/sha1test.pp');
    T:=P.Targets.AddExampleunit('examples/demosha256.pp');
    T:=P.Targets.AddExampleunit('examples/hmd5.pp');
    T:=P.Targets.AddExampleunit('examples/hsha1.pp');
    T:=P.Targets.AddExampleunit('examples/md5performancetest.pas');
    T:=P.Targets.AddExampleunit('examples/sha1performancetest.pas');
    // md5.ref
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
