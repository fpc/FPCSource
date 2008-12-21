{
}
program testib;

uses Ibase40, strings;

{$h-}

Const
     { Change to YOUR database server }

     ServerDb : pchar =  'testdb.gdb';

     { CHange to YOUR username and password. These may be empty }

      username = '';
      PWD = '';

     { Don't edit after this }

      dbinfo : array [1..3] of byte
             = (isc_info_page_size,isc_info_num_buffers,isc_info_end);
      query : pchar = 'select * from FPDev;';
      flag : array[0..2] of shortint = (0,0,0);

Type
  TStatusArray = Array[0..19] of ISC_Status;

Var
  DB : Tisc_db_handle;
  TA : TISC_tr_handle;
  statement : TISC_stmt_handle;
  DPB : String;
  Status : TStatusArray;
  sqlda : PXSQLDA;
  name,email : String;
  i,id : longint;
  fs : longint;

Function CheckIBstatus (Const Status : TStatusArray) : Boolean;

begin
  CheckIBstatus:=Not ((Status[0]=1) and (status[1]<>0))
end;

Procedure DoError (Const status : TStatusArray);

begin
  Writeln ('Failed:');
  isc_print_status(@status);
  halt(1);
end;

begin
  db:=Nil;
  dpb:=chr(isc_dpb_version1);
  If UserName<>'' then
    begin
    dpb:=dpb+chr(isc_dpb_user_name)+chr(length(UserName))+username;
    If pwd<>'' then
      dpb:=dpb+chr(isc_dpb_password)+chr(length(pwd))+pwd;
    end;
  Write ('Connecting to ',serverdb,': ');
  isc_attach_database(@Status[0],strlen(serverdb),serverdb,@db,length(dpb),@dpb[1]);
  if Not CheckIBStatus(Status) then
    DoError(status)
  else
    Writeln ('OK.');
  Write ('Starting Transaction : ');
  If ISC_start_transaction (@status[0],@ta,1,@db,0,Nil)<>0 then
    DoError(Status)
  else
    Writeln ('OK.');
  getmem (sqlda,XSQLDA_Length(3));
  sqlda^.sqln:=3;
  sqlda^.sqld:=3;
  sqlda^.version:=1;
  Write('Allocating statement : ');
  If isc_dsql_allocate_statement(@status ,@db,@statement)<>0 then
    DoError(Status)
  else
    Writeln ('OK.');
  Write ('Preparing statement : ');
  if ISC_dsql_prepare(@status,@ta,@statement,0,query,1,sqlda)<>0 then
    DoError(Status)
  else
    Writeln ('OK.');
  I:=0;
  With sqlda^.sqlvar[i] do
    begin
    sqldata := @id;
    sqltype := sql_long;
    sqlind  := @flag[0];
    end;
  inc(i);
  With sqlda^.sqlvar[i] do
    begin
    sqldata := @name[1];
    sqltype := sql_text;
    sqlind  := @flag[1];
    end;
  inc(i);
  With sqlda^.sqlvar[i] do
    begin
    sqldata := @email[1];
    sqltype := sql_text;
    sqlind  := @flag[2];
    end;
  Write ('Executing statement : ');
  if isc_dsql_execute(@status,@ta,@statement,1,Nil)<>0 then
    DoError(Status)
  else
    Writeln ('OK.');

  Writeln ('Fetching rows :');
  Repeat
    FS:=isc_dsql_fetch(@status,@statement,1,sqlda);
    If FS=0 then
      begin
      I:=255;
      While Name[I]=' ' do Dec(i);
      setlength(Name,i);
      I:=255;
      While Email[I]=' ' do Dec(i);
      setlength(email,i);
      Writeln ('(',ID,',',name,',',email,')');
      end;
  until FS<>0;
  If FS<>100 then
    DoError(status)
  else
    Writeln ('At end.');
  Write ('Freeing statement : ');
  if isc_dsql_free_statement(@status,@statement,DSQL_Close)<>0 then
    DoError(Status)
  else
    Writeln ('OK.');
  Write ('Committing transaction : ');
  If ISC_Commit_transaction(@status,@ta)<>0 then
    doerror(status)
  else
    Writeln ('OK.');
  Write ('Disconnecting from database: ');
  isc_detach_database(@status,@db);
  If CheckIBStatus (Status) Then
    Writeln ('OK.')
  else
    doerror(status);
end.
