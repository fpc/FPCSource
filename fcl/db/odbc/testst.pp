program testst;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;
  St : TODBCSQLStatement;
  FieldNames : TStringList;
  I : Integer;
  PT : TODBCParamType;

procedure DumpFielddef(F : TODBCField);

begin
  Writeln('Field ',F.Position,' : ',F.Name);
  Writeln('Type : ',F.DataType);
  Writeln('Size : ',F.Size);
  Writeln('Decimal digits : ',F.DecimalDigits);
  Writeln('Nullable : ',F.Nullable);
end;


begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='FPC';
    Conn.Active:=True;
    ST:=TODBCSQLStatement.Create(Conn);
    Try
      ST.SQL.Text:='Select * from fpdev';
      ST.Prepare;
      Try
        FieldNames:=TStringList.Create;
        Try
          st.GetFieldList(FieldNames);
          Writeln('Found ',FieldNames.Count,' Fields in result set :');
          For I:=0 to FieldNames.Count-1 do
            Writeln(i+1,': ',FieldNames[i]);
          Writeln('End of list');
          st.bindfields(nil);
          with st.fields do
            for I:=0 to COunt-1 do
              DumpFielddef(st.fields.items[i] as TODBCField);
        finally
          FieldNames.Free;
          Writeln('Freed list');
        end;
      Finally
        St.Unprepare;
        Writeln('Unprepared');
      end;
    Finally
      ST.Free;
      Writeln('Freed statement');
    end;
    Conn.Active:=False;
    Writeln('Disactivated connection');
  Finally
    Conn.free;
    Writeln('Freed Connection');
  end;
end.
