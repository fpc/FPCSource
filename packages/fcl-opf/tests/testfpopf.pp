program testfpopf;

{$mode objfpc}{$H+}

uses
  // Code page conversions as in a real application: sqldb converts string
  // parameters and fields through the system code page.
{$IFDEF UNIX}
{$IFDEF FPC_DOTTEDUNITS}
  UnixApi.CThreads, UnixApi.CWString,
{$ELSE}
  cthreads, cwstring,
{$ENDIF}
{$ENDIF}
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
{$IFDEF FPC_DOTTEDUNITS}
  FpcUnit.ConsoleTestRunner,
{$ELSE}
  consoletestrunner,
{$ENDIF}
  utc.opf.manager, utc.opf.fields, utc.opf.connections, utc.opf.registry,
  utc.opf.sql, utc.opf.ddl, utc.opf.task,
  utc.opf.json,
  utc.opf.mapping.json,
  utc.opf.listreg,
  utc.opf.executor,
  utc.opf.failconn,
  Opf.Transaction, utc.opf.transaction,
  utc.opf.persister.rtti,
  Opf.Variants, utc.opf.variants,
  utc.opf.persister.intf,
  Opf.Relations, utc.opf.relations,
  utc.opf.nullable,
  utc.opf.m2m,
  Opf.Driver, utc.opf.driver,
  Opf.Logging, utc.opf.logging,
  Opf.Factory, utc.opf.factory,
  Opf.Criteria, utc.opf.criteria,
  Opf.Linq, utc.opf.linq,
  utc.opf.querysafety,
  utc.opf.contracts,
  utc.opf.typefidelity,
  utc.opf.schemacontracts,
  utc.opf.deletecontracts,
  utc.opf.dialectcontracts,
  utc.opf.persistercontracts,
  utc.opf.managercontracts,
  utc.opf.registrycontracts,
  Opf.PQErrors,
{$IFNDEF VER3_2}
  Opf.Persister.ExtRTTI, utc.opf.persister.extrtti,
  Opf.Attributes, utc.opf.attributes,
  Opf.Memento.ExtRTTI, utc.opf.memento.extrtti,
  utc.opf.attrcontracts,
  utc.opf.lazycontracts,
{$ENDIF}
  utc.opf.integration.base,
  utc.opf.integration.common,
  utc.opf.integration.sqlite, utc.opf.integration.pgsql,
  utc.opf.integration.fb, utc.opf.integration.mysql,
  utc.opf.integration.oracle,
  utc.opf.integration.safety,
  Opf.Schema, utc.opf.schema,
  Opf.Migration, utc.opf.migration,
  utc.opf.migrationchain,
  Opf.SnapshotHooks, Opf.Snapshot, utc.opf.dirty,
  utc.opf.nulldirty,
  utc.opf.undo,
  utc.opf.dialectddl,
  Opf.ChangeTracking, utc.opf.changetracking,
  Opf.Memento, utc.opf.memento,
  Opf.MementoTracker, utc.opf.mementotracker,
  Opf.Proxy, utc.opf.proxy;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  end;

var
  Application: TMyTestRunner;


begin
  SetupLogging;
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=true;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='testfpopf';
  Application.Run;
  Application.Free;
end.
