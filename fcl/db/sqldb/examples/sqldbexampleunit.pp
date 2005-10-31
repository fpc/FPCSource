(******************************************************************************
 *                                                                            *
 *  (c) 2005 CNOC v.o.f.                                                      *
 *                                                                            *
 *  File:        SqldbExampleUnit.pp                                          *
 *  Author:      Joost van der Sluis (joost@cnoc.nl)                          *
 *  Description: This unit is used by the SQLdb examples                      *
 *  License:     GPL                                                          *
 *                                                                            *
 ******************************************************************************)
unit SqldbExampleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  sqldb, pqconnection, mysql4conn, IBConnection, ODBCConn;

var dbtype,
    dbname,
    dbuser,
    dbpassword   : string;

    Fconnection  : tSQLConnection;
    Ftransaction : tSQLTransaction;
    Fquery       : tSQLQuery;

const
 FPdevNames : Array[1..8] of string = ('Florian Klämpfl', 'Carl Eric Codère',
                  'Daniël Mantione', 'Jonas Maebe', 'Michael Van Canneyt',
                  'Peter Vreman', 'Pierre Muller', 'Marco van de Voort'
                 );
 FPdevEmails : Array[1..8] of string = ('florian@freepascal.org', 'ccodere@spamidontlike.ieee.org',
                  'd.s.p.mantione@twi.tudelft.nl', 'jonas@SPAM.freepascal.ME.org.NOT', 'michael@freepascal.org',
                  'peter@freepascal.org', 'muller@cerbere.u-strasbg.fr', 'marcov@stack.nl'
                 );
// I know, this results in bogus-values...
 FPdevBirthDates : Array[1..8] of TDateTime = (1-1-1991,2-2-1992,
                   3-3-1993, 4-4-1994, 5-5-1995,
                   6-6-1996, 7-7-1997, 8-8-1998
                  );


procedure ExitWithError(s : string);
procedure ReadIniFile;

procedure CreateFConnection;
procedure CreateFTransaction;
procedure CreateFQuery;

implementation

uses
  inifiles;

procedure ExitWithError(s : string);

begin
  writeln(s);
  writeln('Execution aborted');
  halt;
end;

procedure ReadIniFile;

var IniFile : TIniFile;

begin
  IniFile := TIniFile.Create('database.ini');
  dbtype := IniFile.ReadString('Database','Type','');
  dbname := IniFile.ReadString('Database','Name','');
  dbuser := IniFile.ReadString('Database','User','');
  dbpassword := IniFile.ReadString('Database','Password','');
  IniFile.Free;
end;

procedure CreateFConnection;

begin
  if dbtype = 'mysql' then Fconnection := tMySQLConnection.Create(nil);
  if dbtype = 'postgresql' then Fconnection := tpqConnection.Create(nil);
  if dbtype = 'interbase' then Fconnection := tIBConnection.Create(nil);
  if dbtype = 'odbc' then Fconnection := tODBCConnection.Create(nil);

  if not assigned(Fconnection) then ExitWithError('Invalid database-type, check if a valid database-type was provided in the file ''database.ini''');

  with Fconnection do
    begin
    DatabaseName := dbname;
    UserName := dbuser;
    Password := dbpassword;
    open;
    end;
end;

procedure CreateFTransaction;

begin
  Ftransaction := tsqltransaction.create(nil);
  with Ftransaction do
    begin
    database := Fconnection;
    StartTransaction;
    end;
end;

procedure CreateFQuery;

begin
  Fquery := TSQLQuery.create(nil);
  with Fquery do
    begin
    database := Fconnection;
    transaction := Ftransaction;
    end;
end;


end.

