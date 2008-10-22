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

    P:=AddPackage('fcl-db');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-db';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.SourcePath.Add('src/base');
    P.SourcePath.Add('src/paradox');
    P.SourcePath.Add('src/sqldb');
    P.SourcePath.Add('src/sqldb/postgres');
    P.SourcePath.Add('src/sqldb/sqlite');
    P.SourcePath.Add('src/sqldb/interbase');
    P.SourcePath.Add('src/sqldb/mysql');
    P.SourcePath.Add('src/sqldb/odbc');
    P.SourcePath.Add('src/sqldb/examples');
    P.SourcePath.Add('src/sqldb/oracle');
    P.SourcePath.Add('src/sdf');
    P.SourcePath.Add('src/datadict');
    P.SourcePath.Add('src/memds');
    P.SourcePath.Add('src/codegen');
    P.SourcePath.Add('src/export');
    P.SourcePath.Add('src/sqlite');
    P.SourcePath.Add('src/dbase');
    P.IncludePath.Add('src/base');
    P.IncludePath.Add('src/sqldb');
    P.IncludePath.Add('src/sqldb/postgres');
    P.IncludePath.Add('src/sqldb/mysql');
    P.IncludePath.Add('src/sdf');
    P.IncludePath.Add('src/memds');  
    P.IncludePath.Add('src/sqlite');
    P.IncludePath.Add('src/dbase');


    T:=P.Targets.AddUnit('bufdataset.pas');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('bufdataset_parser');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('bufdataset_parser.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('dbf_prscore');
          AddUnit('dbf_prsdef');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('customsqliteds.pas');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('db.pas');
      with T.Dependencies do
        begin
          AddInclude('dataset.inc');
          AddInclude('fields.inc');
          AddInclude('datasource.inc');
          AddInclude('database.inc');
          AddInclude('dsparams.inc');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('dbcoll.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
        end;
    T:=P.Targets.AddUnit('dbconst.pas');
    T:=P.Targets.AddUnit('dbf.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('db');
          AddUnit('dbf_common');
          AddUnit('dbf_dbffile');
          AddUnit('dbf_parser');
          AddUnit('dbf_prsdef');
          AddUnit('dbf_cursor');
          AddUnit('dbf_fields');
          AddUnit('dbf_pgfile');
          AddUnit('dbf_idxfile');
          AddUnit('dbf_wtil');
          AddUnit('dbf_idxcur');
          AddUnit('dbf_memo');
          AddUnit('dbf_str');
        end;
    T:=P.Targets.AddUnit('dbf_collate.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_lang');
        end;
    T:=P.Targets.AddUnit('dbf_common.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('db');
          AddUnit('dbf_wtil');
        end;
    T:=P.Targets.AddUnit('dbf_cursor.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_pgfile');
          AddUnit('dbf_common');
        end;
    T:=P.Targets.AddUnit('dbf_dbffile.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddInclude('dbf_struct.inc');
          AddUnit('dbf_wtil');
          AddUnit('db');
          AddUnit('dbf_common');
          AddUnit('dbf_cursor');
          AddUnit('dbf_pgfile');
          AddUnit('dbf_fields');
          AddUnit('dbf_memo');
          AddUnit('dbf_idxfile');
          AddUnit('dbf_str');
          AddUnit('dbf_lang');
          AddUnit('dbf_prssupp');
          AddUnit('dbf_prsdef');
        end;
    T:=P.Targets.AddUnit('dbf_fields.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddInclude('dbf_struct.inc');
          AddUnit('db');
          AddUnit('dbf_common');
          AddUnit('dbf_str');
          AddUnit('dbf_dbffile');
        end;
    T:=P.Targets.AddUnit('dbf_idxcur.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_cursor');
          AddUnit('dbf_idxfile');
          AddUnit('dbf_prsdef');
          AddUnit('dbf_wtil');
          AddUnit('dbf_common');
        end;
    T:=P.Targets.AddUnit('dbf_idxfile.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_wtil');
          AddUnit('db');
          AddUnit('dbf_pgfile');
          AddUnit('dbf_parser');
          AddUnit('dbf_prsdef');
          AddUnit('dbf_cursor');
          AddUnit('dbf_collate');
          AddUnit('dbf_common');
          AddUnit('dbf_dbffile');
          AddUnit('dbf_fields');
          AddUnit('dbf_str');
          AddUnit('dbf_prssupp');
          AddUnit('dbf_prscore');
          AddUnit('dbf_lang');
        end;
    T:=P.Targets.AddUnit('dbf_lang.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_wtil');
        end;
    T:=P.Targets.AddUnit('dbf_memo.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_pgfile');
          AddUnit('dbf_common');
          AddUnit('dbf_dbffile');
        end;
    T:=P.Targets.AddUnit('dbf_parser.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_wtil');
          AddUnit('db');
          AddUnit('dbf_prscore');
          AddUnit('dbf_common');
          AddUnit('dbf_fields');
          AddUnit('dbf_prsdef');
          AddUnit('dbf_prssupp');
          AddUnit('dbf');
          AddUnit('dbf_dbffile');
          AddUnit('dbf_str');
        end;
    T:=P.Targets.AddUnit('dbf_pgfile.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('dbf_common');
          AddUnit('dbf_wtil');
          AddUnit('dbf_str');
        end;
    T:=P.Targets.AddUnit('dbf_prscore.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('db');
          AddUnit('dbf_prssupp');
          AddUnit('dbf_prsdef');
        end;
    T:=P.Targets.AddUnit('dbf_prsdef.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddUnit('db');
          AddUnit('dbf_prssupp');
        end;
    T:=P.Targets.AddUnit('dbf_prssupp.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddInclude('getstrfromint.inc');
          AddInclude('getstrfromint.inc');
        end;
    T:=P.Targets.AddUnit('dbf_str.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
          AddInclude('dbf_str.inc');
        end;
    T:=P.Targets.AddUnit('dbf_wtil.pas');
      with T.Dependencies do
        begin
          AddInclude('dbf_common.inc');
        end;
    T:=P.Targets.AddUnit('dbwhtml.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('whtml');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('fpcgcreatedbf.pp');
      with T.Dependencies do
        begin
          AddUnit('fpddcodegen');
          AddUnit('db');
        end;
    T:=P.Targets.AddUnit('fpcgdbcoll.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpddcodegen');
        end;
    T:=P.Targets.AddUnit('fpcgsqlconst.pp');
      with T.Dependencies do
        begin
          AddUnit('fpddcodegen');
        end;
    T:=P.Targets.AddUnit('fpcgtiopf.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpddcodegen');
        end;
    T:=P.Targets.AddUnit('fpcsvexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fpdatadict.pp');
      with T.Dependencies do
        begin
          AddUnit('inicol');
          AddUnit('inifiles');
          AddUnit('contnrs');
          AddUnit('db');
        end;
    T:=P.Targets.AddUnit('fpdbexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('streamio');
        end;
    T:=P.Targets.AddUnit('fpdbfexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('dbf');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fpddcodegen.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdatadict');
        end;
    T:=P.Targets.AddUnit('fpdddbf.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('dbf');
          AddUnit('fpdatadict');
          AddUnit('dbf_idxfile');
        end;
    T:=P.Targets.AddUnit('fpddfb.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('ibconnection');
        end;
    T:=P.Targets.AddUnit('fpddmysql40.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('mysql40conn');
        end;
    T:=P.Targets.AddUnit('fpddmysql41.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('mysql41conn');
        end;
    T:=P.Targets.AddUnit('fpddmysql50.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('mysql50conn');
        end;
    T:=P.Targets.AddUnit('fpddodbc.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('odbcconn');
        end;
    T:=P.Targets.AddUnit('fpddoracle.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('oracleconnection');
        end;
    T:=P.Targets.AddUnit('fpddpq.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('pqconnection');
        end;
    T:=P.Targets.AddUnit('fpddregstd.pp');
      with T.Dependencies do
        begin
          AddUnit('fpdatadict');
          AddUnit('fpdddbf');
          AddUnit('fpddfb');
          AddUnit('fpddpq');
          AddUnit('fpddoracle');
          AddUnit('fpddsqlite3');
          AddUnit('fpddmysql40');
          AddUnit('fpddmysql41');
          AddUnit('fpddmysql50');
          AddUnit('fpddodbc');
        end;
    T:=P.Targets.AddUnit('fpddsqldb.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('sqldb');
          AddUnit('fpdatadict');
        end;
    T:=P.Targets.AddUnit('fpddsqlite3.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('fpdatadict');
          AddUnit('fpddsqldb');
          AddUnit('sqlite3conn');
        end;
    T:=P.Targets.AddUnit('fpfixedexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fprtfexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fpsimplejsonexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fpsimplexmlexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fpsqlexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('fpstdexports.pp');
      with T.Dependencies do
        begin
          AddUnit('fpdbexport');
          AddUnit('fpcsvexport');
          AddUnit('fpfixedexport');
          AddUnit('fpsimplexmlexport');
          AddUnit('fpsimplejsonexport');
          AddUnit('fpsqlexport');
          AddUnit('fptexexport');
          AddUnit('fprtfexport');
          AddUnit('fpdbfexport');
        end;
    T:=P.Targets.AddUnit('fptexexport.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('fpdbexport');
        end;
    T:=P.Targets.AddUnit('ibconnection.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('dbconst');
          AddUnit('bufdataset');
          AddUnit('ibase60dyn');
        end;
    T:=P.Targets.AddUnit('memds.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
        end;
    T:=P.Targets.AddUnit('mysql40conn.pas');
      with T.Dependencies do
        begin
          AddInclude('mysqlconn.inc');
          AddUnit('bufdataset');
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('mysql40dyn');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('mysql41conn.pas');
      with T.Dependencies do
        begin
          AddInclude('mysqlconn.inc');
          AddUnit('bufdataset');
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('mysql41dyn');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('mysql4conn.pas');
      with T.Dependencies do
        begin
          AddInclude('mysqlconn.inc');
          AddUnit('bufdataset');
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('mysql40dyn');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('mysql50conn.pas');
      with T.Dependencies do
        begin
          AddInclude('mysqlconn.inc');
          AddUnit('bufdataset');
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('mysql50dyn');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('odbcconn.pas');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('odbcsqldyn');
          AddUnit('bufdataset');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('oracleconnection.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('dbconst');
          AddUnit('ocidyn');
          AddUnit('oratypes');
        end;
    T:=P.Targets.AddUnit('paradox.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('pxlib');
          AddUnit('bufdataset_parser');
        end;
    T:=P.Targets.AddUnit('pqconnection.pp');
      with T.Dependencies do
        begin
          AddUnit('sqldb');
          AddUnit('db');
          AddUnit('dbconst');
          AddUnit('bufdataset');
          AddUnit('postgres3dyn');
        end;
    T:=P.Targets.AddUnit('sdfdata.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
        end;
    T:=P.Targets.AddUnit('sqldb.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('bufdataset');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('sqlite3conn.pp');
      with T.Dependencies do
        begin
          AddUnit('db');
          AddUnit('bufdataset');
          AddUnit('sqldb');
          AddUnit('sqlite3dyn');
          AddUnit('dbconst');
        end;
    T:=P.Targets.AddUnit('sqlite3ds.pas');
      with T.Dependencies do
        begin
          AddUnit('customsqliteds');
          AddUnit('sqlite3');
          AddUnit('db');
        end;
    T:=P.Targets.AddUnit('sqliteds.pas');
      with T.Dependencies do
        begin
          AddUnit('customsqliteds');
          AddUnit('sqlite');
          AddUnit('db');
        end;

    P.ExamplePath.Add('tests');
    T:=P.Targets.AddExampleProgram('dbftoolsunit.pas');
    T:=P.Targets.AddExampleProgram('dbtestframework.pas');
    T:=P.Targets.AddExampleProgram('memdstoolsunit.pas');
    T:=P.Targets.AddExampleProgram('sdfdstoolsunit.pas');
    T:=P.Targets.AddExampleProgram('sqldbtoolsunit.pas');
    T:=P.Targets.AddExampleProgram('testbasics.pas');
    T:=P.Targets.AddExampleProgram('testdatasources.pas');
    T:=P.Targets.AddExampleProgram('testdbbasics.pas');
    T:=P.Targets.AddExampleProgram('testdddiff.pp');
    T:=P.Targets.AddExampleProgram('testfieldtypes.pas');
    T:=P.Targets.AddExampleProgram('testsqlscript.pas');
    T:=P.Targets.AddExampleProgram('toolsunit.pas');
    // database.ini.txt
    // README.txt

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



