{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    StartPackage('hash');
{$ifdef ALLPACKAGES}
    Directory:='hash';
{$endif ALLPACKAGES}
    Version:='2.0.0';
    T:=Targets.AddUnit('src/md5.pp');
    T:=Targets.AddUnit('src/crc.pas');
    T:=Targets.AddUnit('src/ntlm.pas');
    T:=Targets.AddUnit('src/uuid.pas');
    T:=Targets.AddUnit('src/unixcrypt.pas');
      T.OSes:=[Linux];
    T:=Targets.AddExampleunit('tests/mdtest.pas');
    EndPackage;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
