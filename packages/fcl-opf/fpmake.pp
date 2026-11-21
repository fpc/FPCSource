{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-opf');
    P.ShortName:='fclopf';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('rtl-generics');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-db');
    P.Dependencies.Add('fcl-json');
    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Object persistence framework on top of SQLDB, part of Free Component Libraries (FCL).';
    P.NeedLibC:= false;
    P.OSes:= [aix,beos,haiku,linux,freebsd,darwin,iphonesim,ios,netbsd,openbsd,solaris,win32,win64,wince,android,dragonfly];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('opf.consts.pp');
    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('opf.types.pp');
    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('opf.logging.pp');
    T.Dependencies.AddUnit('opf.types');

    T:=P.Targets.AddUnit('opf.nullable.pp');

    T:=P.Targets.AddUnit('opf.proxy.pp');

    T:=P.Targets.AddUnit('opf.factory.pp');
    T.Dependencies.AddUnit('opf.types');

    T:=P.Targets.AddUnit('opf.fields.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.json.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.variants.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.listreg.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.task.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.logging');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.transaction.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.consts');
      AddUnit('opf.logging');
      end;

    T:=P.Targets.AddUnit('opf.pqerrors.pp');
    T.Dependencies.AddUnit('opf.types');

    T:=P.Targets.AddUnit('opf.attributes.pp');
    T.Dependencies.AddUnit('opf.fields');

    T:=P.Targets.AddUnit('opf.changetracking.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.fields');
      end;

    T:=P.Targets.AddUnit('opf.fieldvalue.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.nullable');
      AddUnit('opf.json');
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.snapshothooks.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.fieldvalue');
      end;

    T:=P.Targets.AddUnit('opf.connections.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.json');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.criteria.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.types');
      AddUnit('opf.consts');
      AddUnit('opf.fieldvalue');
      end;

    T:=P.Targets.AddUnit('opf.registry.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.fields');
      AddUnit('opf.factory');
      AddUnit('opf.consts');
      AddUnit('opf.logging');
      end;

    T:=P.Targets.AddUnit('opf.relations.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.m2m.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.consts');
      AddUnit('opf.logging');
      end;

    T:=P.Targets.AddUnit('opf.schema.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.relations');
      AddUnit('opf.m2m');
      end;

    T:=P.Targets.AddUnit('opf.driver.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.schema');
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.sql.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.driver');
      AddUnit('opf.consts');
      AddUnit('opf.logging');
      end;

    T:=P.Targets.AddUnit('opf.ddl.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.driver');
      AddUnit('opf.sql');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.relations');
      AddUnit('opf.m2m');
      AddUnit('opf.schema');
      AddUnit('opf.consts');
      AddUnit('opf.logging');
      AddUnit('opf.transaction');
      AddUnit('opf.fieldvalue');
      end;

    T:=P.Targets.AddUnit('opf.migration.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.ddl');
      AddUnit('opf.driver');
      AddUnit('opf.registry');
      AddUnit('opf.relations');
      AddUnit('opf.m2m');
      AddUnit('opf.schema');
      AddUnit('opf.sql');
      AddUnit('opf.logging');
      AddUnit('opf.types');
      AddUnit('opf.consts');
      AddUnit('opf.transaction');
      end;

    T:=P.Targets.AddUnit('opf.executor.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.driver');
      AddUnit('opf.sql');
      AddUnit('opf.listreg');
      AddUnit('opf.factory');
      AddUnit('opf.consts');
      AddUnit('opf.logging');
      AddUnit('opf.fieldvalue');
      end;

    T:=P.Targets.AddUnit('opf.persister.intf.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.consts');
      AddUnit('opf.factory');
      end;

    T:=P.Targets.AddUnit('opf.persister.rtti.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.consts');
      AddUnit('opf.json');
      AddUnit('opf.variants');
      AddUnit('opf.nullable');
      AddUnit('opf.fieldvalue');
      AddUnit('opf.factory');
      end;

    T:=P.Targets.AddUnit('opf.persister.extrtti.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.consts');
      AddUnit('opf.persister.rtti');
      AddUnit('opf.nullable');
      AddUnit('opf.attributes');
      AddUnit('opf.factory');
      AddUnit('opf.fieldvalue');
      AddUnit('opf.json');
      AddUnit('opf.variants');
      end;

    T:=P.Targets.AddUnit('opf.manager.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.connections');
      AddUnit('opf.registry');
      AddUnit('opf.fields');
      AddUnit('opf.sql');
      AddUnit('opf.relations');
      AddUnit('opf.m2m');
      AddUnit('opf.transaction');
      AddUnit('opf.ddl');
      AddUnit('opf.migration');
      AddUnit('opf.schema');
      AddUnit('opf.criteria');
      AddUnit('opf.changetracking');
      AddUnit('opf.proxy');
      AddUnit('opf.listreg');
      AddUnit('opf.consts');
      AddUnit('opf.executor');
      AddUnit('opf.logging');
      AddUnit('opf.driver');
      AddUnit('opf.fieldvalue');
      end;

    T:=P.Targets.AddUnit('opf.mapping.json.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.fields');
      AddUnit('opf.registry');
      AddUnit('opf.relations');
      AddUnit('opf.m2m');
      end;

    T:=P.Targets.AddUnit('opf.linq.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.manager');
      AddUnit('opf.criteria');
      AddUnit('opf.types');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.memento.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.types');
      AddUnit('opf.fields');
      AddUnit('opf.changetracking');
      AddUnit('opf.consts');
      end;

    T:=P.Targets.AddUnit('opf.memento.extrtti.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.changetracking');
      AddUnit('opf.memento');
      AddUnit('opf.consts');
      AddUnit('opf.json');
      AddUnit('opf.fieldvalue');
      end;

    T:=P.Targets.AddUnit('opf.mementotracker.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.changetracking');
      AddUnit('opf.memento');
      AddUnit('opf.manager');
      end;

    T:=P.Targets.AddUnit('opf.snapshot.pp');
    with T.Dependencies do
      begin
      AddUnit('opf.fields');
      AddUnit('opf.snapshothooks');
      AddUnit('opf.changetracking');
      AddUnit('opf.manager');
      end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
