program testsql;
{$mode objfpc}
uses fpodbc,Classes,odbcsql;

var
  Conn : TODBCConnection;
  St : TODBCSQLStatement;
  FieldNames : TStringList;
  I,Count : Integer;

procedure DumpFielddef(F : TODBCField);

begin
  Writeln('Field ',F.Position,' : ',F.Name);
  Writeln('Type : ',F.DataType);
  Writeln('Size : ',F.Size);
  Writeln('Decimal digits : ',F.DecimalDigits);
  Writeln('Nullable : ',F.Nullable);
end;

procedure DumpField(F : TODBCField);

begin
  With F do
    begin
    Write(Name:12,BufType:5,'  ');
    If IsNull then
      Writeln('(Null)')
    else
      Case BufType of
        SQL_Smallint : Writeln(AsInteger);
        SQL_Integer  : Writeln(AsInteger);
        SQL_BIT      : Writeln(AsInteger);
        SQL_CHAR     : Writeln(AsString);
        SQL_DOUBLE   : Writeln(AsDouble);
        SQL_DATE,
        SQL_TIME,
        SQL_TIMESTAMP,
        SQL_TYPE_DATE,
        SQL_TYPE_TIMESTAMP,
        SQL_TYPE_TIME : Writeln(AsString);
      else
        Writeln('Unknown field type');
      end;
    end;
end;


begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='FPC';
    Conn.Active:=True;
    ST:=TODBCSQLStatement.Create(Conn);
    Try
      ST.SQL.Text:='Select * from fpdev order by id';
      Writeln('Opening');
      ST.Open;
      Writeln('Opened');
      Try
        FieldNames:=TStringList.Create;
        Try
        st.GetFieldList(FieldNames);
        Writeln('Found ',FieldNames.Count,' Fields in result set :');
        For I:=0 to FieldNames.Count-1 do
          Writeln(i+1,': ',FieldNames[i]);
        Writeln('End of list');
        Writeln('FieldDefs:');
        with st.fields do
          for I:=0 to COunt-1 do
            DumpFielddef(st.fields.items[i] as TODBCField);
        Writeln('Data dump:');
        Count:=0;
        While not st.eof do
          begin
          Inc(Count);
          Writeln('Record no ',Count,' : ');
          Writeln('Name':12,'Type':5,'  Value');
          for I:=0 to st.fields.COunt-1 do
            DumpField(st.fields.items[i] as TODBCField);
          st.fetch;
          end;
        Writeln('End of data');
        finally
          FieldNames.Free;
          Writeln('Freed list');
        end;
      Finally
        st.Close;
        Writeln('Closed');
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
