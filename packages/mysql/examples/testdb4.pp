program qtest;

uses
  mysql4;

Const
  DataBase : Pchar = 'testdb';
  Query    : Pchar = 'Select * from FPdev';

var
  count,num : longint;
  code : integer;
  sock : PMYSQL;
  qmysql : TMYSQL;
  qbuf : string [160];
  rowbuf : TMYSQL_ROW;
  dummy : string;
  recbuf : PMYSQL_RES;

begin
  if paramcount=1 then
    begin
    Dummy:=Paramstr(1)+#0;
    DataBase:=@Dummy[1];
    end;
  Write ('Connecting to MySQL...');
  mysql_init(PMySQL(@qmysql));
  sock :=  mysql_real_connect(PMysql(@qmysql),nil,'michael','geen',nil,0,nil,0);
  if sock=Nil then
    begin
    Writeln (stderr,'Couldn''t connect to MySQL.');
    Writeln (stderr,mysql_error(@qmysql));
    halt(1);
    end;
  Writeln ('Done.');
  Writeln ('Connection data:');
{$ifdef Unix}
  writeln ('Mysql_port      : ',mysql_port);
  writeln ('Mysql_unix_port : ',mysql_unix_port);
{$endif}
  writeln ('Host info       : ',mysql_get_host_info(sock));
  writeln ('Server info     : ',mysql_stat(sock));
  writeln ('Client info     : ',mysql_get_client_info);

  Writeln ('Selecting Database ',DataBase,'...');
  if mysql_select_db(sock,DataBase) < 0 then
    begin
    Writeln (stderr,'Couldn''t select database ',Database);
    Writeln (stderr,mysql_error(sock));
    halt (1);
    end;

  writeln ('Executing query : ',Query,'...');
    if (mysql_query(sock,Query) < 0) then
      begin
      Writeln (stderr,'Query failed ');
      writeln (stderr,mysql_error(sock));
      Halt(1);
      end;

  recbuf := mysql_store_result(sock);
  if RecBuf=Nil then
    begin
    Writeln ('Query returned nil result.');
    mysql_close(sock);
    halt (1);
    end;
  Writeln ('Number of records returned  : ',mysql_num_rows (recbuf));
  Writeln ('Number of fields per record : ',mysql_num_fields(recbuf));

  rowbuf := mysql_fetch_row(recbuf);
  while (rowbuf <>nil) do
       begin
       Write  ('(Id: ', rowbuf[0]);
       Write  (', Name: ', rowbuf[1]);
       Writeln(', Email : ', rowbuf[2],')');
       rowbuf := mysql_fetch_row(recbuf);
       end;
  Writeln ('Freeing memory occupied by result set...');
  mysql_free_result (recbuf);

  Writeln ('Closing connection with MySQL.');
  mysql_close(sock);
  halt(0);
end.
