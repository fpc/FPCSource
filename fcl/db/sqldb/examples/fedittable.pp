(******************************************************************************
 *                                                                            *
 *  (c) 2005 CNOC v.o.f.                                                      *
 *                                                                            *
 *  File:        fEditTable.pp                                                *
 *  Author:      Joost van der Sluis (joost@cnoc.nl)                          *
 *  Description: SQLDB example and test program                               *
 *  License:     GPL                                                          *
 *                                                                            *
 ******************************************************************************)

program fEditTable;

{$mode objfpc}{$H+}

uses
  Classes, sysutils,
  sqldb,
  SqldbExampleUnit;
  

begin
  ReadIniFile;

  CreateFConnection;
  CreateFTransaction;
  CreateFQuery;

  Fconnection.Transaction := Ftransaction; //all updates are performed in this transaction

  with Fquery do
    begin

    SQL.Clear;
    
    SQL.Add('select * from FPDEV');

// With these lines commented out, TSQLQuery creates the update, delete and insert
// queries itself.
// For more complex queries, though, it could be nessecary to provide these queries
// here

//    UpdateSQL.add('update fpdev set name=:name, birthdate=:birthdate where id=:OLD_id');
//    DeleteSQL.add('delete from fpdev where id=:OLD_id');
//    InsertSQL.add('insert into fpdev(id,name,email,birthdate) values (:id,:name,:email,:birthdate)');}

    open;

    Edit;
    FieldByName('name').AsString := FPdevNames[1];
    FieldByName('birthdate').AsDateTime := FPdevBirthDates[1];
    Post;
    
    Append;
    FieldByName('id').AsInteger := 8;
    FieldByName('name').AsString := FPdevNames[8];
    FieldByName('email').AsString := FPdevEmails[8];
    FieldByName('birthdate').AsDateTime := FPdevBirthDates[8];
    post;
    
    ApplyUpdates;

    end;
  Ftransaction.Commit;

  Fquery.Free;
  Ftransaction.Free;
  Fconnection.Free;
end.

