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
  nFields,i,j : longint;
  conn : PPGConn;
  res : PPGresult;

begin
  pghost := NiL;        { host name of the backend server }
  pgport := NiL;        { port of the backend server }
  pgoptions := NiL;     { special options to start up the backend server }
  pgtty := NiL;         { debugging tty for the backend server }
  dbName := 'template1';

  { make a connection to the database }
  conn := PQsetdb(pghost, pgport, pgoptions, pgtty, dbName);

  { check to see that the backend connection was successfully made }
  if (PQstatus(conn) = CONNECTION_BAD) then
    begin
    Writeln (stderr, 'Connection to database ',dbname,' failed.');
    Writeln (stderr, PQerrorMessage(conn));
    exit_nicely(conn);
    end;


  { start a transaction block }
  res := PQexec(conn, 'BEGIN');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    Writeln (stderr, 'BEGIN command failed');
    PQclear(res);
    exit_nicely(conn);
    end;

  {
   * should PQclear PGresult whenever it is no longer needed to avoid
   * memory leaks
   }
  PQclear(res);

  {
   * fetch instances from the pg_database, the system catalog of
   * databases
   }
  res := PQexec(conn, 'DECLARE myportal CURSOR FOR select * from pg_database');
  if (PQresultStatus(res) <> PGRES_COMMAND_OK) then
    begin
    Writeln (stderr, 'DECLARE CURSOR command failed');
    PQclear(res);
    exit_nicely(conn);
    end;
  PQclear(res);

  res := PQexec(conn, 'FETCH ALL in myportal');
  if (PQresultStatus(res) <> PGRES_TUPLES_OK) then
    begin
    Writeln (stderr, 'FETCH ALL command didn''t return tuples properly');
    PQclear(res);
    exit_nicely(conn);
    end;

  { first, print out the attribute names }
  nFields := PQnfields(res);
  for i := 0 to nFields-1 do
    Write (PQfname(res, i),space (15-strlen(PQfname(res, i))) );
  writeln;
  writeln;

  { next, print out the instances }
  for i := 0 to PQntuples(res)-1 do
    begin
    for j := 0 to nFields-1 do
      write(PQgetvalue(res, i, j),space (15-strlen(PQgetvalue(res, i,j))));
    writeln;
    end;

  PQclear(res);

  { close the portal }
  res := PQexec(conn, 'CLOSE myportal');
  PQclear(res);

  { end the transaction }
  res := PQexec(conn, 'END');
  PQclear(res);

  { close the connection to the database and cleanup }
  PQfinish(conn);
end.
