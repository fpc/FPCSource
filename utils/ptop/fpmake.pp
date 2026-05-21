{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses
    {$ifdef UNIX}
        {$ifdef FPC_DOTTEDUNITS}
        system.cthreads,
        {$else}
        cthreads,
        {$endif FPC_DOTTEDUNITS}
    {$endif}
        fpmkunit
        ;
{$endif ALLPACKAGES}

procedure add_ptop(const ADirectory: string);
var P: TPackage;
    T: TTarget;
begin
    With Installer do
        begin
        P := AddPackage('utils-ptop');
        P.ShortName := 'ptop';
        P.OSes := AllOSes - [embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,human68k,sinclairql];
        
        if Defaults.CPU = jvm then
            P.OSes := P.OSes - [java,android];
    
        P.Author := '<various>';
        P.License := 'LGPL with modification';
        P.HomepageURL := 'www.freepascal.org';
        P.Email := '';
        P.Description := 'A tool to pretty print pascal source code.';
        P.NeedLibC:= false;
    
        P.Directory:=ADirectory;
        P.Version:='3.3.1';

        P.Dependencies.Add('fcl-base');
    
        T := P.Targets.AddProgram('ptop.pp');
        T.ResourceStrings := true;
        end;
end;

{$ifndef ALLPACKAGES}
begin
    add_ptop('');
    Installer.Run;
end.
{$endif ALLPACKAGES}
