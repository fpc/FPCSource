{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_ihx2tzx(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-ihx2tzx');
    P.ShortName:='ihx2tzx';
    P.OSes:=AllOSes-[embedded,msdos,win16,macos,palmos];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Author := 'Nikolay Nikolov';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A tool to convert Intel HEX Format files to ZX Spectrum tape files in the TZX format.';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='3.3.1';

    P.Dependencies.Add('fcl-base');

    T:=P.Targets.AddProgram('ihx2tzx.lpr');
    T.Dependencies.AddUnit('ihxreader');
    T.Dependencies.AddUnit('tzxwriter');
    T.Dependencies.AddUnit('zxbasic');
    P.Targets.AddUnit('ihxreader.pas').Install := false;
    P.Targets.AddUnit('tzxwriter.pas').Install := false;
    P.Targets.AddUnit('zxbasic.pas').Install := false;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_ihx2tzx('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




