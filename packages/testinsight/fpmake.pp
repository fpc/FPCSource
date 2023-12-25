{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

{$endif ALLPACKAGES}

procedure add_testinsight(const aDirectory: string);

Const
  // All oses that have TFPHTTPClient
  
  WebOses = [aix,beos,haiku,linux,freebsd,darwin,iphonesim,ios,netbsd,openbsd,solaris,win32,win64,wince,android,dragonfly];

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
    P:=AddPackage('testinsight');
    P.ShortName:='tinsight';
    P.Directory:=aDirectory;
    P.Version:='3.3.1';
    
    P.OSes := WebOSes;
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('fcl-fpcunit');
    P.Dependencies.Add('fcl-json');
    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Send FPCUnit test results to a webserver (e.g. embedded in Lazarus IDE).';
    P.NeedLibC:= false;
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('testinsightprotocol.pp');
    
    T:=P.Targets.AddUnit('testinsightclient.pp');
    T.Dependencies.AddUnit('testinsightprotocol');
    
    T:=P.Targets.AddUnit('fpcunittestinsight.pp');
    T.Dependencies.AddUnit('testinsightclient');
    T.Dependencies.AddUnit('testinsightprotocol');
    
    P.NamespaceMap:='namespaces.lst';
    end;
      
end;
    
{$ifndef ALLPACKAGES}
begin
  add_testinsight('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
