// $Id$

// Test program for interbase.pp unit

program testib;

uses Interbase,SysUtils,db;

{$linklib dl}
{$linklib crypt}

const
  dbpath = 'testdb.gdb';
  
var
  DBS : TIBDatabase;
  DS : TIBDataset;
  x  : integer;
  S  : TSystemTime;

begin
  DBS := TIBDatabase.Create(nil);
  DS := TIBDataset.Create(nil);
  DS.Database := DBS;
  DBS.DatabaseName := dbpath;
  DBS.UserName := 'SYSDBA';
  DBS.Password := 'masterkey';
  WriteLn('Clearing ''John Doe'' entry from table');
  DS.SQL.Add('delete from fpdev where username = ''John Doe''');
  DS.Open;
  DS.Close;
  DS.sql.clear;
  WriteLn('Inserting ''John Doe'' developer to fpdev table');
  DS.SQL.Add('insert into fpdev values (9,''John Doe'',''jd@unknown.net'')');
  DS.Open;
  DS.Close;
  DS.sql.clear;
  WriteLn('Making list from fpdev table');
  DS.SQL.Add('select * from fpdev');
  DS.Open;
  while not DS.EOF do
  begin
    for x := 0 to DS.FieldCount - 2 do
      Write(DS.Fields[x].AsString,',');
    WriteLn(DS.Fields[DS.FieldCount-1].AsString);
    DS.Next;
  end;
  
  DS.Close;
  DS.SQL.Clear;
  DS.Free;

  WriteLn;
  WriteLn('Trying to perform test of datatypes interpretation...');
  WriteLn('Some problems with TDateTimeField, see source');
  DS := TIBDataset.Create(nil);
  DS.Database := DBS;
  DS.SQL.Add('select * from test');
  DS.Open;
  while not DS.EOF do
  begin
    { Warning - TDateTimeField.AsDateTime returns wrong values,
      but conversions in TIBDataset are OK! }
    for x := 0 to DS.FieldCount - 1 do
      if (DS.Fields[x].DataType = ftDateTime) then
        WriteLn(DS.Fields[x].FieldName, ' : "',
          FormatDateTime('DD.MM.YYYY HH:MM:SS',DS.Fields[x].AsDateTime),'"')
      else WriteLn(DS.Fields[x].FieldName, ' : "',DS.Fields[x].AsString,'"');
    DS.Next;
  end;
  DS.Free;
  DBS.EndTransaction;
  DBS.Close;
  DBS.Free;
end.

{
  $Log$
  Revision 1.2  2000-07-13 11:32:57  michael
  + removed logs
 
}
