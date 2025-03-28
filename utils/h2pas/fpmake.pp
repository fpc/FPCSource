{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses 
{$ifdef unix}
  cthreads,
{$endif}
  fpmkunit;
{$endif ALLPACKAGES}

procedure add_h2pas(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-h2pas');
    P.ShortName:='h2pa';
    { java and jvm-android do not support 
      fpc_get_output used in these sources }
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];
    { palmos does not support command line parameters }
    P.OSes := P.OSes - [palmos];
    { Program does not fit in 16-bit memory constraints }
    P.OSes := P.OSes - [msdos,win16,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1];
    { avr-embedded and i8086-embedded do not support all needed features by default }
    if Defaults.CPU in [avr,i8086,z80] then
      P.OSes := P.OSes - [embedded];
    { wasm32 CPU does not support
      goto used in these sources }
    if Defaults.CPU=wasm32 then
      P.OSes := P.OSes - [wasip1,wasip1threads,wasip2,embedded];

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A utility to create Pascal header files from c header files.';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='3.3.1';

    P.Options.Add('-Sg');

    p.Commands.AddCommand(caBeforeCompile, AddProgramExtension('pyacc', Defaults.BuildOS), '$(SOURCE) $(DEST)','h2pparse.pp','h2pas.y');
    p.Commands.AddCommand(caBeforeCompile, AddProgramExtension('plex', Defaults.BuildOS), '$(SOURCE) $(DEST)','scan.pas','scan.l');

    T:=P.Targets.AddProgram('h2pas.pas');
    T.Dependencies.AddUnit('h2poptions');
    T.Dependencies.AddUnit('h2plexlib');
    T.Dependencies.AddUnit('scan');
    T.Dependencies.AddUnit('h2pyacclib');
    
    T:=P.Targets.AddUnit('scan.pas');
    T.Install:=false;
    T.Dependencies.AddUnit('h2pbase');
    T.Dependencies.AddUnit('scanbase');
    T.Dependencies.AddUnit('h2pconst');

    T:=P.Targets.AddUnit('h2ptypes.pas');
    T.install:=false;

    T:=P.Targets.AddUnit('h2pout.pp');
    T.install:=false;
    T.Dependencies.AddUnit('h2pconst');
    T.Dependencies.AddUnit('h2poptions');
    T.Dependencies.AddUnit('h2ptypes');
    T.Dependencies.AddUnit('h2plexlib');
    T.Dependencies.AddUnit('h2pyacclib');
    
    
    T:=P.Targets.AddUnit('scanbase.pp');
    T.install:=false;
    T.Dependencies.AddUnit('h2pconst');
    T.Dependencies.AddUnit('h2poptions');
    T.Dependencies.AddUnit('h2ptypes');
    T.Dependencies.AddUnit('h2plexlib');

    T:=P.Targets.AddUnit('h2pbase.pp');
    T.install:=false;
    T.Dependencies.AddUnit('h2pconst');
    T.Dependencies.AddUnit('h2poptions');
    T.Dependencies.AddUnit('h2ptypes');
    T.Dependencies.AddUnit('h2plexlib');
    T.Dependencies.AddUnit('h2pyacclib');
    T.Dependencies.AddUnit('scanbase');
    T.Dependencies.AddUnit('h2pout');

    T:=P.Targets.AddProgram('h2paspp.pas');

    T:=P.Targets.AddProgram('h2paschk.pas');

    P.Targets.AddUnit('h2poptions.pas').install:=false;
    P.Targets.AddUnit('h2plexlib.pas').install:=false;
    P.Targets.AddUnit('h2pyacclib.pas').install:=false;
    P.Targets.AddUnit('h2pconst.pas').install:=false;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_h2pas('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




