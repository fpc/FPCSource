{ This unit contains the definitions for structures and externs for
  functions used by frontend postgres applications. It is based on
  Postgresql's libpq-fe.h.

  It is for postgreSQL version 7.4 and higher with support for the v3.0
  connection-protocol
}
unit postgres3;

interface

uses dllist;

{$PACKRECORDS C}

const
   External_library='pq';

Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
  size_t    = sizeint;
  psize_t   = ^size_t;
  TFILE     = Longint;
  PFIle     = ^TFILE;
  POid      = ^Oid;
  Oid       = dword;

const
   ERROR_MSG_LENGTH = 4096;
   CMDSTATUS_LEN = 40;

Type
  TSockAddr = Array [1..112] of byte;
  TPGresAttDesc = record
       name : Pchar;
       adtid : Oid;
       adtsize : integer;
    end;
  PPGresAttDesc= ^TPGresAttDesc;
  PPPGresAttDesc= ^PPGresAttDesc;
  TPGresAttValue = record
       len : longint;
       value : Pchar;
    end;
  PPGresAttValue= ^TPGresAttValue;
  PPPGresAttValue= ^PPGresAttValue;

  PExecStatusType = ^TExecStatusType;
  TExecStatusType = (PGRES_EMPTY_QUERY := 0,PGRES_COMMAND_OK,
       PGRES_TUPLES_OK,PGRES_COPY_OUT,PGRES_COPY_IN,
       PGRES_BAD_RESPONSE,PGRES_NONFATAL_ERROR,
       PGRES_FATAL_ERROR);

  
  TPGlobjfuncs = record
        fn_lo_open   : Oid;
        fn_lo_close  : Oid;
        fn_lo_creat  : Oid;
        fn_lo_unlink : Oid;
        fn_lo_lseek  : Oid;
        fn_lo_tell   : Oid;
        fn_lo_read   : Oid;
        fn_lo_write  : Oid;
    end;
  PPGlobjfuncs= ^TPGlobjfuncs;

  PConnStatusType = ^TConnStatusType;
  TConnStatusType = (CONNECTION_OK,CONNECTION_BAD,CONNECTION_STARTED,
       CONNECTION_MADE,CONNECTION_AWAITING_RESPONSE,
       CONNECTION_AUTH_OK,CONNECTION_SETENV,
       CONNECTION_SSL_STARTUP,CONNECTION_NEEDED);

   TPGconn = record
        pghost : Pchar;
        pgtty : Pchar;
        pgport : Pchar;
        pgoptions : Pchar;
        dbName : Pchar;
        status : TConnStatusType;
        errorMessage : array[0..(ERROR_MSG_LENGTH)-1] of char;
        Pfin : PFILE;
        Pfout : PFILE;
        Pfdebug : PFILE;
        sock : longint;
        laddr : TSockAddr;
        raddr : TSockAddr;
        salt : array[0..(2)-1] of char;
        asyncNotifyWaiting : longint;
        notifyList : PDllist;
        pguser : Pchar;
        pgpass : Pchar;
        lobjfuncs : PPGlobjfuncs;
    end;
  PPGconn= ^TPGconn;
   
  TPGresult = record
        ntups : longint;
        numAttributes : longint;
        attDescs : PPGresAttDesc;
        tuples : PPPGresAttValue;
        tupArrSize : longint;
        resultStatus : TExecStatusType;
        cmdStatus : array[0..(CMDSTATUS_LEN)-1] of char;
        binary : longint;
        conn : PPGconn;
    end;
  PPGresult= ^TPGresult;





  PPostgresPollingStatusType = ^PostgresPollingStatusType;
  PostgresPollingStatusType = (PGRES_POLLING_FAILED := 0,PGRES_POLLING_READING,
       PGRES_POLLING_WRITING,PGRES_POLLING_OK,
       PGRES_POLLING_ACTIVE);


  PPGTransactionStatusType = ^PGTransactionStatusType;
  PGTransactionStatusType = (PQTRANS_IDLE,PQTRANS_ACTIVE,PQTRANS_INTRANS,
       PQTRANS_INERROR,PQTRANS_UNKNOWN);

  PPGVerbosity = ^PGVerbosity;
  PGVerbosity = (PQERRORS_TERSE,PQERRORS_DEFAULT,PQERRORS_VERBOSE);

  PpgNotify = ^pgNotify;
  pgNotify = record
          relname : Pchar;
          be_pid : longint;
          extra : Pchar;
    end;

{ Function types for notice-handling callbacks  }
  PQnoticeReceiver = procedure (arg:pointer; res:PPGresult);cdecl;
  PQnoticeProcessor = procedure (arg:pointer; message:Pchar);cdecl;
{ Print options for PQprint()  }
  Ppqbool = ^pqbool;
  pqbool = char;

  P_PQprintOpt = ^_PQprintOpt;
  _PQprintOpt = record
          header : pqbool;
          align : pqbool;
          standard : pqbool;
          html3 : pqbool;
          expanded : pqbool;
          pager : pqbool;
          fieldSep : Pchar;
          tableOpt : Pchar;
          caption : Pchar;
          fieldName : ^Pchar;
    end;
  PQprintOpt = _PQprintOpt;
  PPQprintOpt = ^PQprintOpt;

  { ----------------
   * Structure for the conninfo parameter definitions returned by PQconndefaults
   *
   * All fields except "val" point at static strings which must not be altered.
   * "val" is either NULL or a malloc'd current-value string.  PQconninfoFree()
   * will release both the val strings and the PQconninfoOption array itself.
   * ----------------
    }

     P_PQconninfoOption = ^_PQconninfoOption;
     _PQconninfoOption = record
          keyword : Pchar;
          envvar : Pchar;
          compiled : Pchar;
          val : Pchar;
          _label : Pchar;
          dispchar : Pchar;
          dispsize : longint;
       end;
     PQconninfoOption = _PQconninfoOption;
     PPQconninfoOption = ^PQconninfoOption;
  { ----------------
   * PQArgBlock -- structure for PQfn() arguments
   * ----------------
    }
  { can't use void (dec compiler barfs)	  }

     PPQArgBlock = ^PQArgBlock;
     PQArgBlock = record
          len : longint;
          isint : longint;
          u : record
              case longint of
                 0 : ( ptr : Plongint );
                 1 : ( integer : longint );
              end;
       end;
  { ----------------
   * Exported functions of libpq
   * ----------------
    }
  { ===	in fe-connect.c ===  }
  { make a new client connection to the backend  }
  { Asynchronous (non-blocking)  }
(* Const before type ignored *)

  function PQconnectStart(conninfo:Pchar):PPGconn;cdecl;external External_library name 'PQconnectStart';

  function PQconnectPoll(conn:PPGconn):PostgresPollingStatusType;cdecl;external External_library name 'PQconnectPoll';

  { Synchronous (blocking)  }
(* Const before type ignored *)
  function PQconnectdb(conninfo:Pchar):PPGconn;cdecl;external External_library name 'PQconnectdb';

  function PQsetdbLogin(pghost:Pchar; pgport:Pchar; pgoptions:Pchar; pgtty:Pchar; dbName:Pchar;
             login:Pchar; pwd:Pchar):PPGconn;cdecl;external External_library name 'PQsetdbLogin';

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function PQsetdb(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME : pchar) : ppgconn;

  { close the current connection and free the PGconn data structure  }
  procedure PQfinish(conn:PPGconn);cdecl;external External_library name 'PQfinish';

  { get info about connection options known to PQconnectdb  }
  function PQconndefaults:PPQconninfoOption;cdecl;external External_library name 'PQconndefaults';

  { free the data structure returned by PQconndefaults()  }
  procedure PQconninfoFree(connOptions:PPQconninfoOption);cdecl;external External_library name 'PQconninfoFree';

  {
   * close the current connection and restablish a new one with the same
   * parameters
    }
  { Asynchronous (non-blocking)  }
  function PQresetStart(conn:PPGconn):longint;cdecl;external External_library name 'PQresetStart';

  function PQresetPoll(conn:PPGconn):PostgresPollingStatusType;cdecl;external External_library name 'PQresetPoll';

  { Synchronous (blocking)  }
  procedure PQreset(conn:PPGconn);cdecl;external External_library name 'PQreset';

  { issue a cancel request  }
  function PQrequestCancel(conn:PPGconn):longint;cdecl;external External_library name 'PQrequestCancel';

  { Accessor functions for PGconn objects  }
  function PQdb(conn:PPGconn):Pchar;cdecl;external External_library name 'PQdb';

  function PQuser(conn:PPGconn):Pchar;cdecl;external External_library name 'PQuser';

  function PQpass(conn:PPGconn):Pchar;cdecl;external External_library name 'PQpass';

  function PQhost(conn:PPGconn):Pchar;cdecl;external External_library name 'PQhost';

  function PQport(conn:PPGconn):Pchar;cdecl;external External_library name 'PQport';

  function PQtty(conn:PPGconn):Pchar;cdecl;external External_library name 'PQtty';

  function PQoptions(conn:PPGconn):Pchar;cdecl;external External_library name 'PQoptions';

  function PQstatus(conn:PPGconn):TConnStatusType;cdecl;external External_library name 'PQstatus';

  function PQtransactionStatus(conn:PPGconn):PGTransactionStatusType;cdecl;external External_library name 'PQtransactionStatus';

  function PQparameterStatus(conn:PPGconn; paramName:Pchar):Pchar;cdecl;external External_library name 'PQparameterStatus';

  function PQprotocolVersion(conn:PPGconn):longint;cdecl;external External_library name 'PQprotocolVersion';

  function PQerrorMessage(conn:PPGconn):Pchar;cdecl;external External_library name 'PQerrorMessage';

  function PQsocket(conn:PPGconn):longint;cdecl;external External_library name 'PQsocket';

  function PQbackendPID(conn:PPGconn):longint;cdecl;external External_library name 'PQbackendPID';

  function PQclientEncoding(conn:PPGconn):longint;cdecl;external External_library name 'PQclientEncoding';

  function PQsetClientEncoding(conn:PPGconn; encoding:Pchar):longint;cdecl;external External_library name 'PQsetClientEncoding';

{$ifdef USE_SSL}
  { Get the SSL structure associated with a connection  }
  function PQgetssl(conn:PPGconn):PSSL;cdecl;external External_library name 'PQgetssl';
{$endif}

  { Set verbosity for PQerrorMessage and PQresultErrorMessage  }
  function PQsetErrorVerbosity(conn:PPGconn; verbosity:PGVerbosity):PGVerbosity;cdecl;external External_library name 'PQsetErrorVerbosity';
  { Enable/disable tracing  }
  procedure PQtrace(conn:PPGconn; debug_port:PFILE);cdecl;external External_library name 'PQtrace';
  procedure PQuntrace(conn:PPGconn);cdecl;external External_library name 'PQuntrace';
  { Override default notice handling routines  }
  function PQsetNoticeReceiver(conn:PPGconn; proc:PQnoticeReceiver; arg:pointer):PQnoticeReceiver;cdecl;external External_library name 'PQsetNoticeReceiver';
  function PQsetNoticeProcessor(conn:PPGconn; proc:PQnoticeProcessor; arg:pointer):PQnoticeProcessor;cdecl;external External_library name 'PQsetNoticeProcessor';

  { === in fe-exec.c ===  }
  { Simple synchronous query  }
  function PQexec(conn:PPGconn; query:Pchar):PPGresult;cdecl;external External_library name 'PQexec';
  function PQexecParams(conn:PPGconn; command:Pchar; nParams:longint; paramTypes:POid; paramValues:PPchar;
             paramLengths:Plongint; paramFormats:Plongint; resultFormat:longint):PPGresult;cdecl;external External_library name 'PQexecParams';

  function PQexecPrepared(conn:PPGconn; stmtName:Pchar; nParams:longint; paramValues:PPchar; paramLengths:Plongint;
             paramFormats:Plongint; resultFormat:longint):PPGresult;cdecl;external External_library name 'PQexecPrepared';

  { Interface for multiple-result or asynchronous queries  }
  function PQsendQuery(conn:PPGconn; query:Pchar):longint;cdecl;external External_library name 'PQsendQuery';

  function PQsendQueryParams(conn:PPGconn; command:Pchar; nParams:longint; paramTypes:POid; paramValues:PPchar;
             paramLengths:Plongint; paramFormats:Plongint; resultFormat:longint):longint;cdecl;external External_library name 'PQsendQueryParams';

  function PQsendQueryPrepared(conn:PPGconn; stmtName:Pchar; nParams:longint; paramValues:PPchar; paramLengths:Plongint;
             paramFormats:Plongint; resultFormat:longint):longint;cdecl;external External_library name 'PQsendQueryPrepared';

  function PQgetResult(conn:PPGconn):PPGresult;cdecl;external External_library name 'PQgetResult';

  { Routines for managing an asynchronous query  }
  function PQisBusy(conn:PPGconn):longint;cdecl;external External_library name 'PQisBusy';

  function PQconsumeInput(conn:PPGconn):longint;cdecl;external External_library name 'PQconsumeInput';

  { LISTEN/NOTIFY support  }
  function PQnotifies(conn:PPGconn):PPGnotify;cdecl;external External_library name 'PQnotifies';

  { Routines for copy in/out  }
  function PQputCopyData(conn:PPGconn; buffer:Pchar; nbytes:longint):longint;cdecl;external External_library name 'PQputCopyData';

  function PQputCopyEnd(conn:PPGconn; errormsg:Pchar):longint;cdecl;external External_library name 'PQputCopyEnd';

  function PQgetCopyData(conn:PPGconn; buffer:PPchar; async:longint):longint;cdecl;external External_library name 'PQgetCopyData';

  { Deprecated routines for copy in/out  }
  function PQgetline(conn:PPGconn; _string:Pchar; length:longint):longint;cdecl;external External_library name 'PQgetline';

  function PQputline(conn:PPGconn; _string:Pchar):longint;cdecl;external External_library name 'PQputline';

  function PQgetlineAsync(conn:PPGconn; buffer:Pchar; bufsize:longint):longint;cdecl;external External_library name 'PQgetlineAsync';

  function PQputnbytes(conn:PPGconn; buffer:Pchar; nbytes:longint):longint;cdecl;external External_library name 'PQputnbytes';

  function PQendcopy(conn:PPGconn):longint;cdecl;external External_library name 'PQendcopy';

  { Set blocking/nonblocking connection to the backend  }
  function PQsetnonblocking(conn:PPGconn; arg:longint):longint;cdecl;external External_library name 'PQsetnonblocking';

  function PQisnonblocking(conn:PPGconn):longint;cdecl;external External_library name 'PQisnonblocking';

  { Force the write buffer to be written (or at least try)  }
  function PQflush(conn:PPGconn):longint;cdecl;external External_library name 'PQflush';

  {
   * "Fast path" interface --- not really recommended for application
   * use
    }
  function PQfn(conn:PPGconn; fnid:longint; result_buf:Plongint; result_len:Plongint; result_is_int:longint;
             args:PPQArgBlock; nargs:longint):PPGresult;cdecl;external External_library name 'PQfn';

  { Accessor functions for PGresult objects  }
  function PQresultStatus(res:PPGresult):TExecStatusType;cdecl;external External_library name 'PQresultStatus';

  function PQresStatus(status:TExecStatusType):Pchar;cdecl;external External_library name 'PQresStatus';
  function PQresultErrorMessage(res:PPGresult):Pchar;cdecl;external External_library name 'PQresultErrorMessage';

  function PQresultErrorField(res:PPGresult; fieldcode:longint):Pchar;cdecl;external External_library name 'PQresultErrorField';

  function PQntuples(res:PPGresult):longint;cdecl;external External_library name 'PQntuples';

  function PQnfields(res:PPGresult):longint;cdecl;external External_library name 'PQnfields';

  function PQbinaryTuples(res:PPGresult):longint;cdecl;external External_library name 'PQbinaryTuples';

  function PQfname(res:PPGresult; field_num:longint):Pchar;cdecl;external External_library name 'PQfname';

  function PQfnumber(res:PPGresult; field_name:Pchar):longint;cdecl;external External_library name 'PQfnumber';

  function PQftable(res:PPGresult; field_num:longint):Oid;cdecl;external External_library name 'PQftable';

  function PQftablecol(res:PPGresult; field_num:longint):longint;cdecl;external External_library name 'PQftablecol';

  function PQfformat(res:PPGresult; field_num:longint):longint;cdecl;external External_library name 'PQfformat';

  function PQftype(res:PPGresult; field_num:longint):Oid;cdecl;external External_library name 'PQftype';

  function PQfsize(res:PPGresult; field_num:longint):longint;cdecl;external External_library name 'PQfsize';

  function PQfmod(res:PPGresult; field_num:longint):longint;cdecl;external External_library name 'PQfmod';

  function PQcmdStatus(res:PPGresult):Pchar;cdecl;external External_library name 'PQcmdStatus';

  function PQoidStatus(res:PPGresult):Pchar;cdecl;external External_library name 'PQoidStatus';

  { old and ugly  }
  function PQoidValue(res:PPGresult):Oid;cdecl;external External_library name 'PQoidValue';

  { new and improved  }
  function PQcmdTuples(res:PPGresult):Pchar;cdecl;external External_library name 'PQcmdTuples';
  function PQgetvalue(res:PPGresult; tup_num:longint; field_num:longint):Pchar;cdecl;external External_library name 'PQgetvalue';

  function PQgetlength(res:PPGresult; tup_num:longint; field_num:longint):longint;cdecl;external External_library name 'PQgetlength';

  function PQgetisnull(res:PPGresult; tup_num:longint; field_num:longint):longint;cdecl;external External_library name 'PQgetisnull';

  { Delete a PGresult  }
  procedure PQclear(res:PPGresult);cdecl;external External_library name 'PQclear';

  { For freeing other alloc'd results, such as PGnotify structs  }
  procedure PQfreemem(ptr:pointer);cdecl;external External_library name 'PQfreemem';

  { Exists for backward compatibility.  bjm 2003-03-24  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
//  function PQfreeNotify(ptr : longint) : longint;

  {
   * Make an empty PGresult with given status (some apps find this
   * useful). If conn is not NULL and status indicates an error, the
   * conn's errorMessage is copied.
    }
  function PQmakeEmptyPGresult(conn:PPGconn; status:TExecStatusType):PPGresult;cdecl;external External_library name 'PQmakeEmptyPGresult';

  { Quoting strings before inclusion in queries.  }
  function PQescapeString(till:Pchar; from:Pchar; length:size_t):size_t;cdecl;external External_library name 'PQescapeString';
  function PQescapeBytea(bintext:Pbyte; binlen:size_t; bytealen:Psize_t):Pbyte;cdecl;external External_library name 'PQescapeBytea';
  function PQunescapeBytea(strtext:Pbyte; retbuflen:Psize_t):Pbyte;cdecl;external External_library name 'PQunescapeBytea';

  { === in fe-print.c ===  }
  { output stream  }
  procedure PQprint(fout:PFILE; res:PPGresult; ps:PPQprintOpt);cdecl;external External_library name 'PQprint';

  { option structure  }
  {
   * really old printing routines
    }
  { where to send the output  }
  { pad the fields with spaces  }
  { field separator  }
  { display headers?  }
  procedure PQdisplayTuples(res:PPGresult; fp:PFILE; fillAlign:longint; fieldSep:Pchar; printHeader:longint; 
              quiet:longint);cdecl;external External_library name 'PQdisplayTuples';

(* Const before type ignored *)
  { output stream  }
  { print attribute names  }
  { delimiter bars  }
  procedure PQprintTuples(res:PPGresult; fout:PFILE; printAttName:longint; terseOutput:longint; width:longint);cdecl;external External_library name 'PQprintTuples';

  { width of column, if 0, use variable
  								 * width  }
  { === in fe-lobj.c ===  }
  { Large-object access routines  }
  function lo_open(conn:PPGconn; lobjId:Oid; mode:longint):longint;cdecl;external External_library name 'lo_open';

  function lo_close(conn:PPGconn; fd:longint):longint;cdecl;external External_library name 'lo_close';

  function lo_read(conn:PPGconn; fd:longint; buf:Pchar; len:size_t):longint;cdecl;external External_library name 'lo_read';

  function lo_write(conn:PPGconn; fd:longint; buf:Pchar; len:size_t):longint;cdecl;external External_library name 'lo_write';

  function lo_lseek(conn:PPGconn; fd:longint; offset:longint; whence:longint):longint;cdecl;external External_library name 'lo_lseek';

  function lo_creat(conn:PPGconn; mode:longint):Oid;cdecl;external External_library name 'lo_creat';

  function lo_tell(conn:PPGconn; fd:longint):longint;cdecl;external External_library name 'lo_tell';

  function lo_unlink(conn:PPGconn; lobjId:Oid):longint;cdecl;external External_library name 'lo_unlink';

  function lo_import(conn:PPGconn; filename:Pchar):Oid;cdecl;external External_library name 'lo_import';

  function lo_export(conn:PPGconn; lobjId:Oid; filename:Pchar):longint;cdecl;external External_library name 'lo_export';

  { === in fe-misc.c ===  }
  { Determine length of multibyte encoded char at *s  }
  function PQmblen(s:Pbyte; encoding:longint):longint;cdecl;external External_library name 'PQmblen';

  { Get encoding id from environment variable PGCLIENTENCODING  }
  function PQenv2encoding:longint;cdecl;external External_library name 'PQenv2encoding';

implementation

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function PQsetdb(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME : pchar) : ppgconn;
    begin
       PQsetdb:=PQsetdbLogin(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME,'','');
    end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
{  function PQfreeNotify(ptr : longint) : longint;
    begin
       PQfreeNotify:=PQfreemem(ptr);
    end;}


end.
