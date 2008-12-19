Program testpg;

{ Demo program to test pascal connection with postgres database }
{ Translated from the testlibpq example program of PostGreSQL   }

Uses postgres,strings;


Procedure exit_nicely(Conn : PPGconn);

begin
  PQfinish(conn);
  halt(1);
end;


Var
  pghost,pgport,pgoptions,pgtty,dbname : Pchar;
  nFields,i : longint;
  conn : PPGConn;
  res : PPGresult;
  dummy : string;

begin
  pghost := NiL;        { host name of the backend server }
  pgport := NiL;        { port of the backend server }
  pgoptions := NiL;     { special options to start up the backend server }
  pgtty := NiL;         { debugging tty for the backend server }
  if paramcount=1 then
    begin
    dummy:=paramstr(1)+#0;
    dbname:=@dummy[1];
    end
  else
    dbName := 'testdb';

  { make a connection to the database }
  conn := PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);

  { check to see that the backend connection was successfully made }
  if (PQstatus(conn) = CONNECTION_BAD) then
    begin
    Writeln (stderr, 'Connection to database ',dbname,' failed.');
    Writeln (stderr, PQerrorMessage(conn));
    exit_nicely(conn);
    end;

  res := PQexec(conn, 'select * from email');
  if (PQresultStatus(res) <> PGRES_TUPLES_OK) then
    begin
    Writeln (stderr, 'select command failed.');
    PQclear(res);
    exit_nicely(conn);
    end;


  { first, print out the attribute names }
  nFields := PQnfields(res);
  Write ('|',PQfname(res, 0),space (4-strlen(PQfname(res, 0))) );
  Write ('|',PQfname(res, 1),space (20-strlen(PQfname(res, 1))) );
  Write ('|',PQfname(res, 2),space (40-strlen(PQfname(res, 2))) );
  writeln ('|');
  writeln ('+----+--------------------+----------------------------------------+');

  { next, print out the instances }
  for i := 0 to PQntuples(res)-1 do
    begin
    write('|',PQgetvalue(res, i, 0),space (4-strlen(PQgetvalue(res, i,0))));
    write('|',PQgetvalue(res, i, 1),space (20-strlen(PQgetvalue(res, i,1))));
    write('|',PQgetvalue(res, i, 2),space (40-strlen(PQgetvalue(res, i,2))));
    writeln ('|');
    end;

  PQclear(res);

  { close the connection to the database and cleanup }
  PQfinish(conn);
end.
