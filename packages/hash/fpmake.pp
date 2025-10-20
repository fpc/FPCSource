{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

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
    P.OSes:=P.OSes-[embedded,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,ps1,wasip2,oric];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('rtl-extra');

    P.Version:='3.3.1';
    T:=P.Targets.AddUnit('src/md5.pp');
    T.Dependencies.AddInclude('src/md5i386.inc', [i386], AllOSes);
    T:=P.Targets.AddUnit('src/sha1.pp');
    T.Dependencies.AddInclude('src/sha1i386.inc', [i386], AllOSes);
    T.Dependencies.AddInclude('src/sha1x86.inc', [i386,x86_64], AllOSes);
    T.Dependencies.AddInclude('src/sha1x64_sysv.inc', [x86_64], AllOSes);
    T.Dependencies.AddInclude('src/sha1x64_win.inc', [x86_64], AllOSes);
    T:=P.Targets.AddUnit('src/crc.pas');
    T:=P.Targets.AddUnit('src/ntlm.pas');
    T:=P.Targets.AddUnit('src/uuid.pas');
    T:=P.Targets.AddUnit('src/hmac.pp');
    T:=P.Targets.AddUnit('src/fnvhash.pp');
    T:=P.Targets.AddUnit('src/unixcrypt.pas');
    
    T.OSes:=[Linux];
    T:=P.Targets.AddExampleunit('examples/mdtest.pas');
    T:=P.Targets.AddExampleunit('examples/hmd5.pp');
    T:=P.Targets.AddExampleunit('examples/hsha1.pp');
    T:=P.Targets.AddExampleunit('examples/md5performancetest.pas');
    T:=P.Targets.AddExampleunit('examples/sha1performancetest.pas');
    // md5.ref

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
