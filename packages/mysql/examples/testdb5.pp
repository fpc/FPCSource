program testdb5;

{$mode objfpc}{$H+}

uses
  mysql57dyn;

Const
  CHost     : string = 'localhost';
  CDataBase : string = 'test';
  CQuery1   : string = 'select * from FPDEV';
  CQuery2   : string = 'select * from FPDEV where ID between ? and ?';

var
  Database : string;
  MySQL : PMYSQL;
  i, real_length : integer;
  s : string;
  Res : PMYSQL_RES;
  Field: PMYSQL_FIELD;
  Row : MYSQL_ROW;

  Stmt : PMYSQL_STMT;
  BindParams : array[1..2] of MYSQL_BIND;
  param1: longint = 10;
  param2: longint = 20;
  BindRes : array of MYSQL_BIND;
  BindFields : array of record
    buffer: record case byte of
      0: (AsLong: longint);
      1: (AsLongLong: int64);
      2: (AsDouble: double);
      3: (AsBytes: array[0..10] of byte);
    end;
    length: dword;
    is_null: my_bool;
    error: my_bool;
  end;

procedure MySQLError(const Msg: string);
begin
  Writeln (stderr, Msg, ' (', mysql_errno(MySQL), ': ', mysql_error(MySQL), ')');
  halt(1);
end;

procedure MySQLStmtError(const Msg: string);
begin
  Writeln (stderr, Msg, ' (', mysql_stmt_errno(Stmt), ': ', mysql_stmt_error(Stmt), ')');
  halt(1);
end;

begin
  if ParamCount=1 then
    Database := ParamStr(1)
  else
    Database := CDatabase;

  // load client library
  InitialiseMySQL;

  Write ('Connecting to ', CHost, ' MySQL...');
  MySQL := mysql_init(MySQL);
  if mysql_real_connect(MySQL, PAnsiChar(CHost), 'root', 'root', Nil, 0, Nil, CLIENT_MULTI_RESULTS) = nil then
    MySQLError('Couldn''t connect to MySQL.');
  Writeln ('Done.');

  Writeln ('Connection data:');
  writeln ('Host info       : ',mysql_get_host_info(MySQL));
  writeln ('Server info     : ',mysql_stat(MySQL));
  writeln ('Client info     : ',mysql_get_client_info);

  Writeln ('Selecting Database ',DataBase,'...');
  if mysql_select_db(MySQL, PAnsiChar(DataBase)) <> 0 then
    MySQLError('Couldn''t select database '+Database);

  (*** Example using traditional API: ***)
  writeln ('Executing query : ',CQuery1,' ...');
  if mysql_query(MySQL, PAnsiChar(CQuery1)) <> 0 then
    MySQLError('Query failed');

  Res := mysql_store_result(MySQL);
  if Res=Nil then
    MySQLError('Query returned nil result.');
  Writeln ('Number of records returned  : ',mysql_num_rows  (Res));
  Writeln ('Number of fields per record : ',mysql_num_fields(Res));

  Row := mysql_fetch_row(Res);
  while (Row <> nil) do
    begin
    Write  ('(');
    for i:=0 to mysql_num_fields(Res)-1 do
      begin
      if i > 0 then Write(', ');
      Field := mysql_fetch_field_direct(Res, i);
      Write  (Field^.name, ': ', Row[0]);
      end;
    Writeln(')');
    Row := mysql_fetch_row(Res);
    end;
  Writeln ('Freeing memory occupied by result set...');
  mysql_free_result (Res);

  (*** Example using prepared statement API: ***)
  writeln ('Preparing query : ',CQuery2,' ...');
  Stmt := mysql_stmt_init(MySQL);
  if mysql_stmt_prepare(Stmt, PAnsiChar(CQuery2), length(CQuery2)) <> 0 then
    MySQLStmtError('Query preparation failed');
  Writeln ('Query has ', mysql_stmt_param_count(Stmt), ' parameters');

  // binding input parameters
  BindParams[1].buffer_type := MYSQL_TYPE_LONG;
  BindParams[1].buffer := @param1;
  BindParams[1].buffer_length := 0; // for integer type no need to specify
  BindParams[1].length := nil;
  BindParams[1].is_null := nil;

  BindParams[2].buffer_type := MYSQL_TYPE_LONG;
  BindParams[2].buffer := @param2;

  if mysql_stmt_bind_param(Stmt, @BindParams[1]) <> 0 then
    MySQLStmtError('Parameters binding failed');

  writeln ('Executing query : ',CQuery2,' ...');
  if mysql_stmt_execute(Stmt) <> 0 then
    MySQLStmtError('Query execution failed');

  //mysql_stmt_store_result(Stmt); // optional; but may be required when using later "mysql_stmt_num_rows()"
  Writeln ('Number of records returned  : ',mysql_stmt_num_rows   (Stmt));
  Writeln ('Number of fields per record : ',mysql_stmt_field_count(Stmt));

  // prepare structures for output field binding
  SetLength(BindRes   , mysql_stmt_field_count(Stmt));
  SetLength(BindFields, mysql_stmt_field_count(Stmt));

  Res := mysql_stmt_result_metadata(Stmt); // Fetch result set meta information
  Field := mysql_fetch_fields(Res);

  // for each field in result set prepare result buffer
  for i:=0 to mysql_stmt_field_count(Stmt)-1 do
    begin
    BindRes[i].buffer_type := Field^.ftype;
    BindRes[i].buffer_length := 9;
    BindRes[i].buffer := @BindFields[i].buffer;
    BindRes[i].length := @BindFields[i].length;
    BindRes[i].is_null := @BindFields[i].is_null;
    BindRes[i].error := @BindFields[i].error;
    Inc(Field);
    end;

  if mysql_stmt_bind_result(Stmt, @BindRes[0]) <> 0 then
    MySQLStmtError('Bind result failed');

  // if output buffer is smaller than length of character data MYSQL_DATA_TRUNCATED is returned
  // and "error" member of MYSQL_BIND structure is set
  while mysql_stmt_fetch(Stmt) in [0, MYSQL_DATA_TRUNCATED] do
    begin
    for i:=0 to mysql_stmt_field_count(Stmt)-1 do
      begin
      if i > 0 then Write(', ');
      // check real length and set up space in result buffer
      real_length := BindFields[i].length;
      if BindRes[i].buffer_type in [MYSQL_TYPE_STRING, MYSQL_TYPE_VAR_STRING] then
        if real_length > 9 then
          begin
          // prepare buffer with required length
          SetLength(s, real_length);
          BindRes[i].buffer := @s[1];
          BindRes[i].buffer_length := real_length;
          // fetch again
          mysql_stmt_fetch_column(Stmt, @BindRes[i], i, 0);
          Write(s);
          end
        else
          begin
          SetString(s, BindRes[i].buffer, real_length);
          Write(s);
          end
      else
        Write(BindFields[i].buffer.AsLong);
      end;
      Writeln;
    end;

  mysql_free_result(Res); // Free the prepared result metadata
  mysql_stmt_free_result(Stmt);
  mysql_stmt_close(Stmt);

  Writeln ('Closing connection with MySQL.');
  mysql_close(MySQL);
  // unload client library
  ReleaseMySQL;
  halt(0);
end.

