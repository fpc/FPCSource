unit postgres;

interface

uses dllist;

{$linklib pq}
{$linklib c}

{ Not always needed. If you have problems linking, try to add this  }
{ $linklib crypt}

{ $include "libpq/pqcomm.h"}


Type
   Oid = cardinal;
   MsgType = Cardinal;
   PLongint = ^Longint;
   TSockAddr = Array [1..112] of byte; { Testded using C version sizeof() }

Const
   NAMEDATALEN = 32;
   OIDNAMELEN = 36;

Type
   TFILE = Longint;
   PFIle = ^TFILE;

type
   TConnStatusType = (CONNECTION_OK,CONNECTION_BAD);
   PConnStatusType= ^TConnStatusType;

   TExecStatusType = (PGRES_EMPTY_QUERY,PGRES_COMMAND_OK,PGRES_TUPLES_OK,
     PGRES_COPY_OUT,
     PGRES_COPY_IN,
     PGRES_BAD_RESPONSE,
     PGRES_NONFATAL_ERROR,
     PGRES_FATAL_ERROR
     );
   PExecStatusType= ^TExecStatusType;
{
        extern const AnsiChar  pgresStatus[];
}

const
   ERROR_MSG_LENGTH = 4096;
   COMMAND_LENGTH = 20;
   REMARK_LENGTH = 80;
   PORTAL_NAME_LENGTH = 16;

type
   TPQArgBlock = record
        len : longint;
        isint : longint;
        u : record
            case longint of
               0 : ( ptr:Plongint );
               1 : ( integer:longint );
          end;
     end;
   PPQArgBlock= ^TPQArgBlock;

   TPGresAttDesc = record
        name : PAnsiChar;
        adtid : Oid;
        adtsize : integer;
     end;
   PPGresAttDesc= ^TPGresAttDesc;
   PPPGresAttDesc= ^PPGresAttDesc;

const
   NULL_LEN = -1;

type
   TPGresAttValue = record
        len : longint;
        value : PAnsiChar;
     end;
   PPGresAttValue= ^TPGresAttValue;
   PPPGresAttValue= ^PPGresAttValue;

   TPGnotify = record
        relname : array[0..(NAMEDATALEN)-1] of AnsiChar;
        be_pid : longint;
     end;
   PPGnotify= ^TPGnotify;

   TPGlobjfuncs = record
        fn_lo_open : Oid;
        fn_lo_close : Oid;
        fn_lo_creat : Oid;
        fn_lo_unlink : Oid;
        fn_lo_lseek : Oid;
        fn_lo_tell : Oid;
        fn_lo_read : Oid;
        fn_lo_write : Oid;
     end;
   PPGlobjfuncs= ^TPGlobjfuncs;

   TPGconn = record
        pghost : PAnsiChar;
        pgtty : PAnsiChar;
        pgport : PAnsiChar;
        pgoptions : PAnsiChar;
        dbName : PAnsiChar;
        status : TConnStatusType;
        errorMessage : array[0..(ERROR_MSG_LENGTH)-1] of AnsiChar;
        Pfin : PFILE;
        Pfout : PFILE;
        Pfdebug : PFILE;
        sock : longint;
        laddr : TSockAddr;
        raddr : TSockAddr;
        salt : array[0..(2)-1] of AnsiChar;
        asyncNotifyWaiting : longint;
        notifyList : PDllist;
        pguser : PAnsiChar;
        pgpass : PAnsiChar;
        lobjfuncs : PPGlobjfuncs;
     end;
   PPGconn= ^TPGconn;

const
   CMDSTATUS_LEN = 40;

type
   TPGresult = record
        ntups : longint;
        numAttributes : longint;
        attDescs : PPGresAttDesc;
        tuples : PPPGresAttValue;
        tupArrSize : longint;
        resultStatus : TExecStatusType;
        cmdStatus : array[0..(CMDSTATUS_LEN)-1] of AnsiChar;
        binary : longint;
        conn : PPGconn;
     end;
   PPGresult= ^TPGresult;

   pqbool = AnsiChar;

   TPQprintopt = record
        header : pqbool;
        align : pqbool;
        standard : pqbool;
        html3 : pqbool;
        expanded : pqbool;
        pager : pqbool;
        fieldSep : PAnsiChar;
        tableOpt : PAnsiChar;
        caption : PAnsiChar;
     end;
   PPQprintopt= ^TPQprintopt;


   TPQconninfoOption = Record
      keyword   : PAnsiChar;
      environ   : PAnsiChar;
      compiled  : PAnsiChar;
      val       : PAnsiChar;
      Thelabel  : PAnsiChar;
      dispchar  : PAnsiChar;
      dispsize  : longint;
   end;
   PPQconninfoOption = ^TPQconninfoOption;

const
   MAX_MESSAGE_LEN = 8193;
   BYTELEN = 8;
   MAX_FIELDS = 512;
   DefaultHost     : PAnsiChar = 'localhost';
   DefaultTty      : PAnsiChar = '';
   DefaultOption   : PAnsiChar = '';
   DefaultAuthtype : PAnsiChar = '';
   DefaultPassword : PAnsiChar = '';

type
   TTUPLE = pointer;
   PTUPLE = ^TTUPLE;


  function  PQconnectdb(conninfo:PAnsiChar):PPGconn;cdecl; external;
  function  PQconndefaults:PPQconninfoOption;cdecl; external;
  function  PQsetdbLogin(pghost,pgport,pgoptions,pgtty,dbName,login,pwd : PAnsiChar):PPGConn;cdecl;external;
  procedure PQfinish(conn:PPGconn);cdecl; external;
  procedure PQreset(conn:PPGconn);cdecl; external;
  function  PQdb(conn:PPGconn):PAnsiChar;cdecl; external;
  function  PQuser(conn:PPGconn):PAnsiChar;cdecl; external;
  function  PQhost(conn:PPGconn):PAnsiChar;cdecl; external;
  function  PQoptions(conn:PPGconn):PAnsiChar;cdecl; external;
  function  PQport(conn:PPGconn):PAnsiChar;cdecl; external;
  function  PQtty(conn:PPGconn):PAnsiChar;cdecl; external;
  function  PQstatus(conn:PPGconn):TConnStatusType;cdecl; external;
  function  PQerrorMessage(conn:PPGconn):PAnsiChar;cdecl; external;
  procedure PQtrace(conn:PPGconn; debug_port:PFILE);cdecl; external;
  procedure PQuntrace(conn:PPGconn);cdecl; external;
  function  PQexec(conn:PPGconn; query:PAnsiChar):PPGresult;cdecl; external;
  function  PQgetline(conn:PPGconn; str:PAnsiChar; len:longint):longint;cdecl; external;
  function  PQendcopy(conn:PPGconn):longint;cdecl; external;
  function PQputline(conn:PPGconn; str:PAnsiChar) : longint;cdecl; external;
  function  PQresultStatus(res:PPGresult):TExecStatusType;cdecl; external;
  function  PQntuples(res:PPGresult):longint;cdecl; external;
  function  PQnfields(res:PPGresult):longint;cdecl; external;
  function  PQfname(res:PPGresult; field_num:longint):PAnsiChar;cdecl; external;
  function  PQfnumber(res:PPGresult; field_name:PAnsiChar):longint;cdecl; external;
  function  PQftype(res:PPGresult; field_num:longint):Oid;cdecl; external;
  function  PQfsize(res:PPGresult; field_num:longint):integer;cdecl; external;
  function  PQcmdStatus(res:PPGresult):PAnsiChar;cdecl; external;
  function  PQgetvalue(res:PPGresult; tup_num:longint; field_num:longint):PAnsiChar;cdecl; external;
  function  PQgetlength(res:PPGresult; tup_num:longint; field_num:longint):longint;cdecl; external;
  function  PQgetisnull(res:PPGresult; tup_num:longint; field_num:longint):longint;cdecl; external;
  procedure PQclear(res:PPGresult);cdecl; external;
  procedure PQdisplayTuples(res:PPGresult; fp:PFILE; fillAlign:longint; fieldSep:PAnsiChar; printHeader:longint; quiet:longint);cdecl; external;
  procedure PQprintTuples(res:PPGresult; fout:PFILE; printAttName:longint; terseOutput:longint; width:longint);cdecl; external;
  procedure PQprint(fout:PFILE; res:PPGresult; ps:PPQprintOpt);cdecl; external;
  function  PQnotifies(conn:PPGconn):PPGnotify;cdecl; external;
  function  PQfn(conn:PPGconn; fnid:longint; result_buf:Plongint; result_len:Plongint; result_is_int:longint; args:PPQArgBlock; nargs:longint):PPGresult;cdecl; external;
  function  fe_getauthsvc(PQerrormsg:PAnsiChar):MsgType;cdecl; external;
  procedure fe_setauthsvc(name:PAnsiChar; PQerrormsg:PAnsiChar);cdecl; external;
  function  fe_getauthname(PQerrormsg:PAnsiChar):PAnsiChar;cdecl; external;
  function  pqGets(s:PAnsiChar; maxlen:longint; stream:PFILE; debug:PFILE):longint;cdecl; external;
  function  pqGetnchar(s:PAnsiChar; maxlen:longint; stream:PFILE; debug:PFILE):longint;cdecl; external;
  function  pqPutnchar(s:PAnsiChar; maxlen:longint; stream:PFILE; debug:PFILE):longint;cdecl; external;
  function  pqPuts(s:PAnsiChar; stream:PFILE; debug:PFILE):longint;cdecl; external;
  function  pqGetc(stream:PFILE; debug:PFILE):longint;cdecl; external;
  function  pqGetInt(result:Plongint; bytes:longint; stream:PFILE; debug:PFILE):longint;cdecl; external;
  function  pqPutInt(n:longint; bytes:longint; stream:PFILE; debug:PFILE):longint;cdecl; external;
  procedure pqFlush(stream:PFILE; debug:PFILE);cdecl; external;
  function  PQoidStatus(res : PPGresult) : PAnsiChar;cdecl;external;
  function  PQcmdTuples(res : PPGresult) : PAnsiChar;cdecl;external;
  function  lo_open(conn:PPGconn; lobjId:Oid; mode:longint):longint; cdecl; external;
  function  lo_close(conn:PPGconn; fd:longint):longint; cdecl; external;
  function  lo_read(conn:PPGconn; fd:longint; buf:PAnsiChar; len:longint):longint; cdecl; external;
  function  lo_write(conn:PPGconn; fd:longint; buf:PAnsiChar; len:longint):longint; cdecl; external;
  function  lo_lseek(conn:PPGconn; fd:longint; offset:longint; whence:longint):longint; cdecl; external;
  function  lo_creat(conn:PPGconn; mode:longint):Oid;cdecl;external;
  function  lo_tell(conn:PPGconn; fd:longint):longint; cdecl; external;
  function  lo_unlink(conn:PPGconn; lobjId:Oid):longint; cdecl; external;
  function  lo_import(conn:PPGconn; filename:PAnsiChar):Oid;cdecl;external;
  function  lo_export(conn:PPGconn; lobjId:Oid; filename:PAnsiChar):longint; cdecl; external;

{$ifdef PGSQL6_2_1}
  Function  PQsetdb(pghost,pgport,pgoptions,pgtty,dbName : PAnsiChar):PPGConn; cdecl;external;
{$else}
  function PQsetdb(pghost,pgport,pgoptions,pgtty,dbName : PAnsiChar):PPGConn;
{$endif}

implementation



{ Define helper functions }

{
  In version 6.2.xxx, PGsetdb is a function in libpq.
  in version 6.3.xxx, PGsetdb is a macro, pointing to setdblogin !!
}

{$ifndef PGSQL6_2_1}
function PQsetdb(pghost,pgport,pgoptions,pgtty,dbName : PAnsiChar):PPGConn;
begin
 PQsetdb:=PQsetdbLogin(pghost,pgport,pgoptions,pgtty,dbName,nil,nil);
end;
{$endif}

end.
