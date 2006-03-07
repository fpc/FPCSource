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
  Classes, sysutils,
  sqldb, pqconnection, IBConnection, ODBCConn,
  mysql40conn, mysql41conn, mysql50conn, oracleconnection;

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

var
 FPdevBirthDates : Array[1..8] of TDateTime;


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
  
  FPdevBirthDates[1] := StrToDate('1-1-1991');
  FPdevBirthDates[2] := StrToDate('2-2-1992');
  FPdevBirthDates[3] := StrToDate('3-3-1993');
  FPdevBirthDates[4] := StrToDate('4-4-1994');
  FPdevBirthDates[5] := StrToDate('5-5-1995');
  FPdevBirthDates[6] := StrToDate('6-6-1996');
  FPdevBirthDates[7] := StrToDate('7-7-1997');
  FPdevBirthDates[8] := StrToDate('8-8-1998');
end;

procedure CreateFConnection;

begin
  if dbtype = 'mysql40' then Fconnection := tMySQL40Connection.Create(nil);
  if dbtype = 'mysql41' then Fconnection := tMySQL41Connection.Create(nil);
  if dbtype = 'mysql50' then Fconnection := tMySQL50Connection.Create(nil);
  if dbtype = 'postgresql' then Fconnection := tpqConnection.Create(nil);
  if dbtype = 'interbase' then Fconnection := tIBConnection.Create(nil);
  if dbtype = 'odbc' then Fconnection := tODBCConnection.Create(nil);
  if dbtype = 'oracle' then Fconnection := TOracleConnection.Create(nil);

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

