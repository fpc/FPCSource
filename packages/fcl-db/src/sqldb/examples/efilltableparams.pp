(******************************************************************************
 *                                                                            *
 *  (c) 2005 CNOC v.o.f.                                                      *
 *                                                                            *
 *  File:        eFillTableParams.pp                                          *
 *  Author:      Joost van der Sluis (joost@cnoc.nl)                          *
 *  Description: SQLDB example and test program                               *
 *  License:     GPL                                                          *
 *                                                                            *
 ******************************************************************************)

program eFillTableParams;

{$mode objfpc}{$H+}

uses
  Classes, sysutils,
  sqldb,
  SqldbExampleUnit;
  
var i : integer;

begin
  ReadIniFile;

  CreateFConnection;
  CreateFTransaction;
  CreateFQuery;

  with Fquery do
    begin

    SQL.Clear;
    
    SQL.Add('insert into FPDEV ( id, Name, Email, BirthDate)       ');
    SQL.Add('           values ( :id, :name, :email, :birthdate )   ');

    for i := 2 to 4 do
      begin
      params.ParamByName('id').asinteger := i;
      params.ParamByName('name').asstring := FPdevNames[i];
      params.ParamByName('email').AsString := FPdevEmails[i];
      params.ParamByName('birthdate').AsDateTime := FPdevBirthDates[i];
      ExecSql;
      end;

    for i := 5 to 7 do
      begin
      if dbtype <> 'oracle' then
        sql[1] := 'values ('+inttostr(i)+ ', ' +
                        '''' +FPdevNames[i]+ ''', ' +
                        '''' +FPdevEmails[i]+ ''', ' +
                        '''' +FormatDateTime('MM-DD-YYYY',FPdevBirthDates[i])+ ''')'
      else
        sql[1] := 'values ('+inttostr(i)+ ', ' +
                        '''' +FPdevNames[i]+ ''', ' +
                        '''' +FPdevEmails[i]+ ''', ' +
                        '''' +FormatDateTime('DD-MMM-YYYY',FPdevBirthDates[i])+ ''')';
      ExecSql;
      end;

    end;
  Ftransaction.CommitRetaining;

  Fquery.Free;
  Ftransaction.Free;
  Fconnection.Free;
end.

