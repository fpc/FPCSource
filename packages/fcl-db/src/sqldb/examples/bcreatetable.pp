(******************************************************************************
 *                                                                            *
 *  (c) 2005 CNOC v.o.f.                                                      *
 *                                                                            *
 *  File:        bCreateTable.pp                                              *
 *  Author:      Joost van der Sluis (joost@cnoc.nl)                          *
 *  Description: SQLDB example and test program                               *
 *  License:     GPL                                                          *
 *                                                                            *
 ******************************************************************************)

program bCreateTable;

{$mode objfpc}{$H+}

uses
  Classes,
  sqldb,
  SqldbExampleUnit;

begin
  ReadIniFile;
  
  CreateFConnection;
  CreateFTransaction;

  Fconnection.Transaction := Ftransaction;
  
  Fconnection.ExecuteDirect('create table FPDEV (       ' +
                            '  id INT NOT NULL,           ' +
                            '  Name VARCHAR(50),          ' +
                            '  Email CHAR(50),            ' +
                            '  Birthdate Date,            ' +
                            '  PRIMARY KEY (id)           ' +
                            ')                            ');

  FTransaction.Commit;
  Ftransaction.Free;
  Fconnection.Free;
end.
