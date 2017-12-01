{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_fpindexer(const ADirectory: string);
Const
  SqldbConnectionOSes = [beos,haiku,linux,freebsd,win32,win64,wince,darwin,iphonesim,netbsd,openbsd,aix,dragonfly];
  SqliteOSes          = [beos,haiku,linux,freebsd,darwin,iphonesim,netbsd,openbsd,win32,wince,aix,dragonfly];
    
Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
      P:=AddPackage('fpindexer');
      P.Directory:=ADirectory;
      P.Version:='3.0.5';
      P.OSes := [beos,haiku,freebsd,darwin,solaris,netbsd,openbsd,linux,win32,win64,wince,aix,dragonfly];
      P.Dependencies.Add('fcl-base');
      P.Dependencies.Add('fcl-db');
      P.Dependencies.Add('chm'); // for fastreaderhtml
      P.Dependencies.Add('sqlite');
      P.Author := 'Free Pascal development team';
      P.License := 'LGPL with modification, ';
      P.HomepageURL := 'www.freepascal.org';
      P.Email := '';
      P.Description := 'Free Pascal text indexer and search engine.';
      P.NeedLibC:= false;
  
      P.SourcePath.Add('src');
  
      T:=P.Targets.AddUnit('fpmasks.pp');
     
      T:=P.Targets.AddUnit('fpindexer.pp');
      T.Dependencies.AddUnit('fpmasks');
      T.ResourceStrings:=true;
      
      T:=P.Targets.AddUnit('memindexdb.pp');
      T.ResourceStrings:=true;
      T.Dependencies.AddUnit('fpindexer');
      
      T:=P.Targets.AddUnit('ireaderhtml.pp');
      T.Dependencies.AddUnit('fpindexer');
      
      T:=P.Targets.AddUnit('ireaderpas.pp');
      T.Dependencies.AddUnit('fpindexer');
      
      T:=P.Targets.AddUnit('ireadertxt.pp');
      T.Dependencies.AddUnit('fpindexer');
  
      T:=P.Targets.AddUnit('sqldbindexdb.pp',SqldbConnectionOSes);
      T.Dependencies.AddUnit('fpindexer');
      
      T:=P.Targets.AddUnit('sqliteindexdb.pp',SqliteOSes);
      T.Dependencies.AddUnit('fpindexer');
      
      T:=P.Targets.AddUnit('fbindexdb.pp',SqliteOSes);
      T.Dependencies.AddUnit('fpindexer');
      
      T:=P.Targets.AddUnit('dbindexer.pp',SqldbConnectionOSes);
      T.Dependencies.AddUnit('fpindexer');
      T.Dependencies.AddUnit('ireadertxt');
  end;
end;
    
{$ifndef ALLPACKAGES}
begin
  add_fpindexer('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
