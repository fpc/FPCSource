(******************************************************************************
 *                                                                            *
 *  (c) 2005 CNOC v.o.f.                                                      *
 *                                                                            *
 *  File:        gFilterTable.pp                                              *
 *  Author:      Joost van der Sluis (joost@cnoc.nl)                          *
 *  Description: SQLDB example and test program                               *
 *  License:     GPL                                                          *
 *                                                                            *
 ******************************************************************************)

program gFilterTable;

{$mode objfpc}{$H+}

uses
  Classes,
  sqldb,
  SqldbExampleUnit;

begin
  ReadIniFile;

  CreateFConnection;
  CreateFTransaction;
  CreateFQuery;

  with Fquery do
    begin

    ReadOnly := True;

    SQL.Clear;
    SQL.Add('select * from FPDEV');

    Writeln('Id;Name;Email;birthdate');

    Filter := 'id > 4';
    Filtered := True;

    Open;

    while not eof do
      begin
      write(fieldbyname('ID').asstring+';');
      write(fieldbyname('Name').asstring+';');
      write(fieldbyname('Email').asstring+';');
      writeln(fieldbyname('Birthdate').asstring);
      next;
      end;

    Filter := 'id < 5';
    First;

    while not eof do
      begin
      write(fieldbyname('ID').asstring+';');
      write(fieldbyname('Name').asstring+';');
      write(fieldbyname('Email').asstring+';');
      writeln(fieldbyname('Birthdate').asstring);
      next;
      end;

    close;

    end;

  Fquery.Free;
  Ftransaction.Free;
  Fconnection.Free;
end.

