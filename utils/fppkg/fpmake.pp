{$mode objfpc}{$H+}
program fpmake;

 { Generated automatically by fppkg on 5-2-07 }

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do 
    begin
    { 
      fppkg
    } 
    StartPackage('fppkg');
    Version:='2.0.0';
    Options:='-Filnet/sys';
    Dependencies.Add('fcl');
    Dependencies.Add('libcurl');
    T:=Targets.AddProgram('fppkg');
    T.ResourceStrings:=True;
      T.Directory:='';
    T:=Targets.AddUnit('fpmktype');
      T.Directory:='';
    T:=Targets.AddUnit('fpmkunit');
      T.Directory:='';
    T:=Targets.AddUnit('fprepos');
    T.ResourceStrings:=True;
      T.Directory:='';
    T:=Targets.AddUnit('fpxmlrep');
    T.ResourceStrings:=True;
      T.Directory:='';
    T:=Targets.AddUnit('pkgropts');
      T.Directory:='';
    T:=Targets.AddUnit('pkghandler');
      T.Directory:='';
    T:=Targets.AddUnit('pkgmkconv');
      T.Directory:='';
    T:=Targets.AddUnit('pkgdownload');
      T.Directory:='';
    T:=Targets.AddUnit('pkgwget');
    T.OS:=[linux,freebsd,netbsd,openbsd,darwin,solaris,win32,win64,wince];
      T.Directory:='';
    T:=Targets.AddUnit('pkglnet');
    T.OS:=[linux,freebsd,netbsd,openbsd,darwin,solaris,win32,win64,wince];
      T.Directory:='';
    T:=Targets.AddUnit('pkglibcurl');
    T.OS:=[linux,freebsd,netbsd,openbsd,darwin,solaris];
      T.Directory:='';
    T:=Targets.AddExampleunit('rep2xml');
      T.Directory:='';
    EndPackage;
    Run;
    end;
end.

