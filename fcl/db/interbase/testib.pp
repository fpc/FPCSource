// $Id$

// Test program for interbase.pp unit

program testib;

uses Interbase,SysUtils,db;

{$linklib dl}
{$linklib crypt}

const
  dbpath = 'obelix.wisa.be:/home/interbase/helpdesk.gdb';
  
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
  DBS.Connected:=True;
  DS.SQL.Add('select * from scholen');
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
{
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
}
  DBS.EndTransaction;
  DBS.Close;
  DBS.Free;
end.

{
  $Log$
  Revision 1.1  2000-06-04 08:15:43  michael
  + Initial implementation in FCL

  Revision 1.1.1.1  2000/06/02 06:56:37  stingp1
  Initial release

}