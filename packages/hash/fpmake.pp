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
    P.Directory:='hash';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    T:=P.Targets.AddUnit('src/md5.pp');
    T:=P.Targets.AddUnit('src/crc.pas');
    T:=P.Targets.AddUnit('src/ntlm.pas');
    T:=P.Targets.AddUnit('src/uuid.pas');
    T:=P.Targets.AddUnit('src/unixcrypt.pas');
      T.OSes:=[Linux];
    T:=P.Targets.AddExampleunit('examples/mdtest.pas');
    // md5.ref
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
