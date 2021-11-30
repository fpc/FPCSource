{
    Copyright (c) 1999-2000 by Pavel Stingl <stingp1.eti@mail.cez.cz>


    Oracle Call Interface Translation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit OraOCI;

{$MODE FPC}
{$linklib clntsh}
// Disabled - it is a DLL in the windows system dir, apparently unused.
{  $linklib common}
{$linklib core4}
{$linklib nlsrtl3}
{$ifndef BSD}
{$linklib dl}
{$ENDIF}
{$linklib c}

interface

  type
    sb1                 = shortint;
    ub1                 = byte;
    sb2                 = integer;
    ub2                 = word;
    sb4                 = longint;
    ub4                 = cardinal;
    eb1                 = byte;
    eb2                 = word;
    eb4                 = cardinal;
    sword               = integer;
    uword               = integer;
    eword               = integer;
    PDouble             = ^double;
    PLongint            = ^longint;
    PPointer            = ^pointer;
    PWord               = ^word;
    PBoolean            = ^boolean;
    PCardinal           = ^cardinal;
    PByte               = ^byte;


    POCITime = ^OCITime;
    OCITime = packed record
      OCITimeHH      : ub1;
      OCITimeMM      : ub1;
      OCITimeSS      : ub1;
    end;

    POCIDate = ^OCIDate;
    OCIDate = packed record
      OCIDateYYYY    : sb2;
      OCIDateMM      : ub1;
      OCIDateDD      : ub1;
      OCIDateTime    : OCITime;
    end;

    POCIDateTime = ^TOCIDate;
    TOCIDate = packed record
      Year           : sb2;
      Month          : ub1;
      Day            : ub1;
      Hour           : ub1;
      Min            : ub1;
      Sec            : ub1;
    end;

    PCDA_DEF = ^TCDA_DEF;
    TCDA_DEF = packed record
      v2_rc             : sb2;
      ft                : ub2;
      rpc               : ub4;
      peo               : ub2;
      fc                : ub1;
      rcs1              : ub1;
      rc                : ub2;
      wrn               : ub1;
      rcs2              : ub1;
      rcs3              : sword;
        rid             : record
          rd            : record
            rcs4        : ub4;
            rcs5        : ub2;
            rcs6        : ub1;
          end;
          rcs7          : ub4;
          rcs8          : ub2;
        end;
      ose               : sword;
      chk               : ub1;
      rcsp              : pointer;
      filler            : array [39..64] of byte;
    end;

    PLDA_DEF            = ^TCDA_DEF;
    TLDA_DEF            = TCDA_DEF;



  const
    EB4MAXVAL           = 2147483647;
    EB4MINVAL           = 0;
    UB4MAXVAL           = $FFFFFFFF;
    UB4MINVAL           = 0;
    SB4MAXVAL           = 2147483647;
    SB4MINVAL           = -2147483647;
    MINEB4MAXVAL        = 2147483647;
    MAXEB4MINVAL        = 0;
    MINUB4MAXVAL        = $FFFFFFFF;
    MAXUB4MINVAL        = 0;
    MINSB4MAXVAL        = 2147483647;
    MAXSB4MINVAL        = -2147483647;

  const
     VARCHAR2_TYPE = 1;
     NUMBER_TYPE = 2;
     INT_TYPE = 3;
     FLOAT_TYPE = 4;
     STRING_TYPE = 5;
     ROWID_TYPE = 11;
     DATE_TYPE = 12;

     { OCI Environment Modes for opinit call  }
     OCI_EV_DEF = 0;            { default single-threaded environment  }
     OCI_EV_TSF = 1;            { thread-safe environment  }

     { OCI Logon Modes for olog call  }
     OCI_LM_DEF = 0;            { default login  }
     OCI_LM_NBL = 1;            { non-blocking logon  }

     OCI_ONE_PIECE = 0;         { there or this is the only piece  }
     OCI_FIRST_PIECE = 1;       { the first of many pieces  }
     OCI_NEXT_PIECE = 2;        { the next of many pieces  }
     OCI_LAST_PIECE = 3;        { the last piece of this column  }

     { input data types  }
     SQLT_CHR = 1;              { (ORANET TYPE) character string  }
     SQLT_NUM = 2;              { (ORANET TYPE) oracle numeric  }
     SQLT_INT = 3;              { (ORANET TYPE) integer  }
     SQLT_FLT = 4;              { (ORANET TYPE) Floating point number  }
     SQLT_STR = 5;              { zero terminated string  }
     SQLT_VNU = 6;              { NUM with preceding length byte  }
     SQLT_PDN = 7;              { (ORANET TYPE) Packed Decimal Numeric  }
     SQLT_LNG = 8;              { long  }
     SQLT_VCS = 9;              { Variable character string  }
     SQLT_NON = 10;             { Null/empty PCC Descriptor entry  }
     SQLT_RID = 11;             { rowid  }
     SQLT_DAT = 12;             { date in oracle format  }
     SQLT_VBI = 15;             { binary in VCS format  }
     SQLT_BIN = 23;             { binary data(DTYBIN)  }
     SQLT_LBI = 24;             { long binary  }
     SQLT_UIN = 68;             { unsigned integer  }
     SQLT_SLS = 91;             { Display sign leading separate  }
     SQLT_LVC = 94;             { Longer longs (char)  }
     SQLT_LVB = 95;             { Longer long binary  }
     SQLT_AFC = 96;             { Ansi fixed char  }
     SQLT_AVC = 97;             { Ansi Var char  }
     SQLT_CUR = 102;            { cursor  type  }
     SQLT_RDD = 104;            { rowid descriptor  }
     SQLT_LAB = 105;            { label type  }
     SQLT_OSL = 106;            { oslabel type  }
     SQLT_NTY = 108;            { named object type  }
     SQLT_REF = 110;            { ref type  }
     SQLT_CLOB = 112;           { character lob  }
     SQLT_BLOB = 113;           { binary lob  }
     SQLT_BFILEE = 114;         { binary file lob  }
     SQLT_CFILEE = 115;         { character file lob  }
     SQLT_RSET = 116;           { result set type  }
     SQLT_NCO = 122;            { named collection type (varray or nested table)  }
     SQLT_VST = 155;            { OCIString type  }
     SQLT_ODT = 156;            { OCIDate type  }

     { binary file lob  }
     SQLT_FILE = SQLT_BFILEE;
     SQLT_CFILE = SQLT_CFILEE;
     SQLT_BFILE = SQLT_BFILEE;

     { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information  }
     SQLCS_IMPLICIT = 1;        { for CHAR, VARCHAR2, CLOB w/o a specified set  }
     SQLCS_NCHAR = 2;           { for NCHAR, NCHAR VARYING, NCLOB  }
     SQLCS_EXPLICIT = 3;        { for CHAR, etc, with "CHARACTER SET ..." syntax  }
     SQLCS_FLEXIBLE = 4;        { for PL/SQL "flexible" parameters  }
     SQLCS_LIT_NULL = 5;        { for typecheck of NULL and empty_clob() lits  }

     { OCI Modes }
     OCI_DEFAULT = $00;  { the default value for parameters and attributes  }
     OCI_THREADED = $01;  { the application is in threaded environment  }
     OCI_OBJECT = $02;  { the application is in object environment  }
     OCI_NON_BLOCKING = $04;  { non blocking mode of operation  }
     OCI_ENV_NO_MUTEX = $08;  { the environment handle will not be protected
                                 by a mutex internally  }

     { OCI Handle Types }
     { handle types range from 1 - 49  }
     OCI_HTYPE_FIRST = 1;  { start value of handle type  }
     OCI_HTYPE_ENV = 1;  { environment handle  }
     OCI_HTYPE_ERROR = 2;  { error handle  }
     OCI_HTYPE_SVCCTX = 3;  { service handle  }
     OCI_HTYPE_STMT = 4;  { statement handle  }
     OCI_HTYPE_BIND = 5;  { bind handle  }
     OCI_HTYPE_DEFINE = 6;  { define handle  }
     OCI_HTYPE_DESCRIBE = 7;  { describe handle  }
     OCI_HTYPE_SERVER = 8;  { server handle  }
     OCI_HTYPE_SESSION = 9;  { authentication handle  }
     OCI_HTYPE_TRANS = 10;  { transaction handle  }
     OCI_HTYPE_COMPLEXOBJECT = 11;  { complex object retrieval handle  }
     OCI_HTYPE_SECURITY = 12;  { security handle  }
     OCI_HTYPE_LAST = 12;  { last value of a handle type  }

     { Descriptor Types }
     { descriptor values range from 50 - 255  }
     OCI_DTYPE_FIRST = 50;  { start value of descriptor type  }
     OCI_DTYPE_LOB = 50;  { lob  locator  }
     OCI_DTYPE_SNAP = 51;  { snapshot descriptor  }
     OCI_DTYPE_RSET = 52;  { result set descriptor  }
     OCI_DTYPE_PARAM = 53;  { a parameter descriptor obtained from ocigparm  }
     OCI_DTYPE_ROWID = 54;  { rowid descriptor  }
     OCI_DTYPE_COMPLEXOBJECTCOMP = 55;  { complex object retrieval descriptor  }
     OCI_DTYPE_FILE = 56;  { File Lob locator  }
     OCI_DTYPE_AQENQ_OPTIONS = 57;  { enqueue options  }
     OCI_DTYPE_AQDEQ_OPTIONS = 58;  { dequeue options  }
     OCI_DTYPE_AQMSG_PROPERTIES = 59;  { message properties  }
     OCI_DTYPE_AQAGENT = 60;  { aq agent  }
     OCI_DTYPE_LAST = 60;  { last value of a descriptor type  }

     { Object Ptr Types }
     OCI_OTYPE_NAME = 1;  { object name  }
     OCI_OTYPE_REF = 2;  { REF to TDO  }
     OCI_OTYPE_PTR = 3;  { PTR to TDO  }

  { Attribute Types }
  { the OCI function code  }
     OCI_ATTR_FNCODE = 1;  { the OCI function code  }
     OCI_ATTR_OBJECT = 2;  { is the environment initialized in object mode  }
     OCI_ATTR_NONBLOCKING_MODE = 3;  { non blocking mode  }
     OCI_ATTR_SQLCODE = 4;  { the SQL verb  }
     OCI_ATTR_ENV = 5;  { the environment handle  }
     OCI_ATTR_SERVER = 6;  { the server handle  }
     OCI_ATTR_SESSION = 7;  { the user session handle  }
     OCI_ATTR_TRANS = 8;  { the transaction handle  }
     OCI_ATTR_ROW_COUNT = 9;  { the rows processed so far  }
     OCI_ATTR_SQLFNCODE = 10;  { the SQL verb of the statement  }
     OCI_ATTR_PREFETCH_ROWS = 11;  { sets the number of rows to prefetch  }
     OCI_ATTR_NESTED_PREFETCH_ROWS = 12;  { the prefetch rows of nested table }
     OCI_ATTR_PREFETCH_MEMORY = 13;  { memory limit for rows fetched  }
     OCI_ATTR_NESTED_PREFETCH_MEMORY = 14;  { memory limit for nested rows  }
     OCI_ATTR_CHAR_COUNT = 15;  { this specifies the bind and define size in characters  }
     OCI_ATTR_PDSCL = 16;  { packed decimal scale  }
     OCI_ATTR_PDFMT = 17;  { packed decimal format  }
     OCI_ATTR_PARAM_COUNT = 18;  { number of column in the select list  }
     OCI_ATTR_ROWID = 19;  { the rowid  }
     OCI_ATTR_CHARSET = 20;  { the character set value  }
     OCI_ATTR_NCHAR = 21;  { NCHAR type  }
     OCI_ATTR_USERNAME = 22;  { username attribute  }
     OCI_ATTR_PASSWORD = 23;  { password attribute  }
     OCI_ATTR_STMT_TYPE = 24;  { statement type  }
     OCI_ATTR_INTERNAL_NAME = 25;  { user friendly global name  }
     OCI_ATTR_EXTERNAL_NAME = 26;  { the internal name for global txn  }
     OCI_ATTR_XID = 27;  { XOPEN defined global transaction id  }
     OCI_ATTR_TRANS_LOCK = 28;  { transaction lock }
     OCI_ATTR_TRANS_NAME = 29;  { string to identify a global transaction  }
     OCI_ATTR_HEAPALLOC = 30;  { memory allocated on the heap  }
     OCI_ATTR_CHARSET_ID = 31;  { Character Set ID  }
     OCI_ATTR_CHARSET_FORM = 32;  { Character Set Form  }
     OCI_ATTR_MAXDATA_SIZE = 33;  { Maximumsize of data on the server   }
     OCI_ATTR_CACHE_OPT_SIZE = 34;  { object cache optimal size  }
     OCI_ATTR_CACHE_MAX_SIZE = 35;  { object cache maximum size percentage  }
     OCI_ATTR_PINOPTION = 36;  { object cache default pin option  }
     OCI_ATTR_ALLOC_DURATION = 37;  { object cache default allocation duration  }
     OCI_ATTR_PIN_DURATION = 38;  { object cache default pin duration  }
     OCI_ATTR_FDO = 39;  { Format Descriptor object attribute  }
     OCI_ATTR_POSTPROCESSING_CALLBACK = 40;  { Callback to process outbind data  }
     OCI_ATTR_POSTPROCESSING_CONTEXT = 41;  { Callback context to process outbind data  }
     OCI_ATTR_ROWS_RETURNED = 42;  { Number of rows returned in current iter - for Bind handles  }
     OCI_ATTR_FOCBK = 43;  { Failover Callback attribute  }
     OCI_ATTR_IN_V8_MODE = 44;  { is the server/service context in V8 mode  }
     OCI_ATTR_LOBEMPTY = 45;  { empty lob ?  }
     OCI_ATTR_SESSLANG = 46;  { session language handle  }

  { Enqueue Options }
     OCI_ATTR_VISIBILITY = 47;  { visibility  }
     OCI_ATTR_RELATIVE_MSGID = 48;  { relative message id  }
     OCI_ATTR_SEQUENCE_DEVIATION = 49;  { sequence deviation  }

  { Dequeue Options }
     OCI_ATTR_CONSUMER_NAME = 50;  { consumer name  }
     OCI_ATTR_DEQ_MODE = 51;  { dequeue mode  }
     OCI_ATTR_NAVIGATION = 52;  { navigation  }
     OCI_ATTR_WAIT = 53;  { wait  }
     OCI_ATTR_DEQ_MSGID = 54;  { dequeue message id  }

  { Message Properties }
     OCI_ATTR_PRIORITY = 55;  { priority  }
     OCI_ATTR_DELAY = 56;  { delay  }
     OCI_ATTR_EXPIRATION = 57;  { expiration  }
     OCI_ATTR_CORRELATION = 58;  { correlation id  }
     OCI_ATTR_ATTEMPTS = 59;  { # of attempts  }
     OCI_ATTR_RECIPIENT_LIST = 60;  { recipient list  }
     OCI_ATTR_EXCEPTION_QUEUE = 61;  { exception queue name  }
     OCI_ATTR_ENQ_TIME = 62;  { enqueue time (only OCIAttrGet)  }
     OCI_ATTR_MSG_STATE = 63;  { message state (only OCIAttrGet)  }

  { AQ Agent }
     OCI_ATTR_AGENT_NAME = 64;  { agent name  }
     OCI_ATTR_AGENT_ADDRESS = 65;  { agent address  }
     OCI_ATTR_AGENT_PROTOCOL = 66;  { agent protocol  }

  { Parameter Attribute Types }
     OCI_ATTR_UNK = 101;  { unknown attribute  }
     OCI_ATTR_NUM_COLS = 102;  { number of columns  }
     OCI_ATTR_LIST_COLUMNS = 103;  { parameter of the column list  }
     OCI_ATTR_RDBA = 104;  { DBA of the segment header  }
     OCI_ATTR_CLUSTERED = 105;  { whether the table is clustered  }
     OCI_ATTR_PARTITIONED = 106;  { whether the table is partitioned  }
     OCI_ATTR_INDEX_ONLY = 107;  { whether the table is index only  }
     OCI_ATTR_LIST_ARGUMENTS = 108;  { parameter of the argument list  }
     OCI_ATTR_LIST_SUBPROGRAMS = 109;  { parameter of the subprogram list  }
     OCI_ATTR_REF_TDO = 110;  { REF to the type descriptor  }
     OCI_ATTR_LINK = 111;  { the database link name  }
     OCI_ATTR_MIN = 112;  { minimum value  }
     OCI_ATTR_MAX = 113;  { maximum value  }
     OCI_ATTR_INCR = 114;  { increment value  }
     OCI_ATTR_CACHE = 115;  { number of sequence numbers cached  }
     OCI_ATTR_ORDER = 116;  { whether the sequence is ordered  }
     OCI_ATTR_HW_MARK = 117;  { high-water mark  }
     OCI_ATTR_TYPE_SCHEMA = 118;  { type's schema name  }
     OCI_ATTR_TIMESTAMP = 119;  { timestamp of the object  }
     OCI_ATTR_NUM_ATTRS = 120;  { number of attributes  }
     OCI_ATTR_NUM_PARAMS = 121;  { number of parameters  }
     OCI_ATTR_OBJID = 122;  { object id for a table or view  }
     OCI_ATTR_PTYPE = 123;  { type of info described by  }
     OCI_ATTR_PARAM = 124;  { parameter descriptor  }
     OCI_ATTR_OVERLOAD_ID = 125;  { overload ID for funcs and procs  }
     OCI_ATTR_TABLESPACE = 126;  { table name space  }
     OCI_ATTR_TDO = 127;  { TDO of a type  }
     OCI_ATTR_PARSE_ERROR_OFFSET = 128;  { Parse Error offset  }

  { Credential Types }
     OCI_CRED_RDBMS = 1;  { database username/password  }
     OCI_CRED_EXT = 2;  { externally provided credentials  }

  { Error Return Values }
     OCI_SUCCESS = 0;  { maps to SQL_SUCCESS of SAG CLI  }
     OCI_SUCCESS_WITH_INFO = 1;  { maps to SQL_SUCCESS_WITH_INFO  }
     OCI_NO_DATA = 100;  { maps to SQL_NO_DATA  }
     OCI_ERROR = -(1);  { maps to SQL_ERROR  }
     OCI_INVALID_HANDLE = -(2);  { maps to SQL_INVALID_HANDLE  }
     OCI_NEED_DATA = 99;  { maps to SQL_NEED_DATA  }
     OCI_STILL_EXECUTING = -(3123);  { OCI would block error  }
     OCI_CONTINUE = -(24200);  { Continue with the body of the OCI function  }

  { Parsing Syntax Types }
     OCI_V7_SYNTAX = 2;  { V7 language  }
     OCI_V8_SYNTAX = 3;  { V8 language  }
     OCI_NTV_SYNTAX = 1;
     { Use what so ever is the native lang of server  }
     { these values must match the values defined in kpul.h  }

  { Scrollable Cursor Options }
     OCI_FETCH_NEXT = $02;  { next row  }
     OCI_FETCH_FIRST = $04;  { first row of the result set  }
     OCI_FETCH_LAST = $08;  { the last row of the result set  }
     OCI_FETCH_PRIOR = $10;  { the previous row relative to current  }
     OCI_FETCH_ABSOLUTE = $20;  { absolute offset from first  }
     OCI_FETCH_RELATIVE = $40;  { offset relative to current  }

  { Bind and Define Options }
     OCI_SB2_IND_PTR = $01;  { unused  }
     OCI_DATA_AT_EXEC = $02;  { data at execute time  }
     OCI_DYNAMIC_FETCH = $02;  { fetch dynamically  }
     OCI_PIECEWISE = $04;  { piecewise DMLs or fetch  }

  { Execution Modes }
     OCI_BATCH_MODE = $01;  { batch the oci statement for execution  }
     OCI_EXACT_FETCH = $02;  { fetch the exact rows specified  }
     OCI_KEEP_FETCH_STATE = $04;  { unused  }
     OCI_SCROLLABLE_CURSOR = $08;  { cursor scrollable  }
     OCI_DESCRIBE_ONLY = $10;  { only describe the statement  }
     OCI_COMMIT_ON_SUCCESS = $20;  { commit, if successful execution  }

  { Authentication Modes }
     OCI_MIGRATE = $0001;  { migratable auth context  }
     OCI_SYSDBA = $0002;  { for SYSDBA authorization  }
     OCI_SYSOPER = $0004;  { for SYSOPER authorization  }
     OCI_PRELIM_AUTH = $0008;  { for preliminary authorization  }

  { Piece Information }
     OCI_PARAM_IN = $01;  { in parameter  }
     OCI_PARAM_OUT = $02;  { out parameter  }

  { Transaction Start Flags }
  { NOTE: OCI_TRANS_JOIN and OCI_TRANS_NOMIGRATE not supported in 8.0.X        }
  { starts a new transaction branch  }
     OCI_TRANS_NEW = $00000001;  { starts a new transaction branch  }
     OCI_TRANS_JOIN = $00000002;  { join an existing transaction  }
     OCI_TRANS_RESUME = $00000004;  { resume this transaction  }
     OCI_TRANS_STARTMASK = $000000ff;
     OCI_TRANS_READONLY = $00000100;  { starts a readonly transaction  }
     OCI_TRANS_READWRITE = $00000200;  { starts a read-write transaction  }
     OCI_TRANS_SERIALIZABLE = $00000400;  { starts a serializable transaction  }
     OCI_TRANS_ISOLMASK = $0000ff00;
     OCI_TRANS_LOOSE = $00010000;  { a loosely coupled branch  }
     OCI_TRANS_TIGHT = $00020000;  { a tightly coupled branch  }
     OCI_TRANS_TYPEMASK = $000f0000;
     OCI_TRANS_NOMIGRATE = $00100000;  { non migratable transaction  }

  { Transaction End Flags }
     OCI_TRANS_TWOPHASE = $01000000;  { use two phase commit  }

  { AQ Constants
     NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE
     The following constants must match the PL/SQL dbms_aq constants
     NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE
    }
  { Visibility flags }
     OCI_ENQ_IMMEDIATE = 1;  { enqueue is an independent transaction  }
     OCI_ENQ_ON_COMMIT = 2;  { enqueue is part of current transaction  }

     OCI_DEQ_BROWSE = 1;  { read message without acquiring a lock  }
     OCI_DEQ_LOCKED = 2;  { read and obtain write lock on message  }
     OCI_DEQ_REMOVE = 3;  { read the message and delete it  }

  { Dequeue navigation flags }
     OCI_DEQ_FIRST_MSG = 1;  { get first message at head of queue  }
     OCI_DEQ_NEXT_MSG = 3;  { next message that is available  }
     OCI_DEQ_NEXT_TRANSACTION = 2;  { get first message of next txn group  }

  { Message states }
     OCI_MSG_WAITING = 1;  { the message delay has not yet completed  }
     OCI_MSG_READY = 0;  { the message is ready to be processed  }
     OCI_MSG_PROCESSED = 2;  { the message has been processed  }
     OCI_MSG_EXPIRED = 3;  { message has moved to exception queue  }

  { Sequence deviation }
     OCI_ENQ_BEFORE = 2;  { enqueue message before another message  }
     OCI_ENQ_TOP = 3;  { enqueue message before all messages  }

  { Visibility flags }
     OCI_DEQ_IMMEDIATE = 1;  { dequeue is an independent transaction  }
     OCI_DEQ_ON_COMMIT = 2;  { dequeue is part of current transaction  }

  { Wait }
     OCI_DEQ_WAIT_FOREVER = -(1);  { wait forever if no message available  }
     OCI_DEQ_NO_WAIT = 0;  { do not wait if no message is available  }

  { Delay }
     OCI_MSG_NO_DELAY = 0;  { message is available immediately  }

  { Expiration }
     OCI_MSG_NO_EXPIRATION = -(1);  { message will never expire  }

  { -------------------------- END AQ Constants -----------------------------  }
  {-----------Object Types      Not to be Used      -------------------------- }
  { Deprecated  }
     OCI_OTYPE_UNK = 0;
     OCI_OTYPE_TABLE = 1;
     OCI_OTYPE_VIEW = 2;
     OCI_OTYPE_SYN = 3;
     OCI_OTYPE_PROC = 4;
     OCI_OTYPE_FUNC = 5;
     OCI_OTYPE_PKG = 6;
     OCI_OTYPE_STMT = 7;

  { Describe Handle Parameter Attributes }
  { Attributes common to Columns and Stored Procs  }
     OCI_ATTR_DATA_SIZE = 1;  { maximum size of the data  }
     OCI_ATTR_DATA_TYPE = 2;  { the SQL type of the column/argument  }
     OCI_ATTR_DISP_SIZE = 3;  { the display size  }
     OCI_ATTR_NAME = 4;  { the name of the column/argument  }
     OCI_ATTR_PRECISION = 5;  { precision if number type  }
     OCI_ATTR_SCALE = 6;  { scale if number type  }
     OCI_ATTR_IS_NULL = 7;  { is it null ?  }
     OCI_ATTR_TYPE_NAME = 8;  { name of the named data type or a package name for package private types  }
     OCI_ATTR_SCHEMA_NAME = 9;  { the schema name  }
     OCI_ATTR_SUB_NAME = 10;  { type name if package private type  }
     OCI_ATTR_POSITION = 11;  { relative position of col/arg in the list of cols/args  }
     OCI_ATTR_COMPLEXOBJECTCOMP_TYPE = 50;  { complex object retrieval parameter attributes  }
     OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL = 51;
     OCI_ATTR_COMPLEXOBJECT_LEVEL = 52;
     OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE = 53;

  { Only Columns  }
     OCI_ATTR_DISP_NAME = 100;  { the display name  }

  {Only Stored Procs  }
     OCI_ATTR_OVERLOAD = 210;  { is this position overloaded  }
     OCI_ATTR_LEVEL = 211;  { level for structured types  }
     OCI_ATTR_HAS_DEFAULT = 212;  { has a default value  }
     OCI_ATTR_IOMODE = 213;  { in, out inout  }
     OCI_ATTR_RADIX = 214;  { returns a radix  }
     OCI_ATTR_NUM_ARGS = 215;  { total number of arguments  }

  { only named type attributes  }
     OCI_ATTR_TYPECODE = 216;  { object or collection  }
     OCI_ATTR_COLLECTION_TYPECODE = 217;  { varray or nested table  }
     OCI_ATTR_VERSION = 218;  { user assigned version  }
     OCI_ATTR_IS_INCOMPLETE_TYPE = 219;  { is this an incomplete type  }
     OCI_ATTR_IS_SYSTEM_TYPE = 220;  { a system type  }
     OCI_ATTR_IS_PREDEFINED_TYPE = 221;  { a predefined type  }
     OCI_ATTR_IS_TRANSIENT_TYPE = 222;  { a transient type  }
     OCI_ATTR_IS_SYSTEM_GENERATED_TYPE = 223;  { system generated type  }
     OCI_ATTR_HAS_NESTED_TABLE = 224;  { contains nested table attr  }
     OCI_ATTR_HAS_LOB = 225;  { has a lob attribute  }
     OCI_ATTR_HAS_FILE = 226;  { has a file attribute  }
     OCI_ATTR_COLLECTION_ELEMENT = 227;  { has a collection attribute  }
     OCI_ATTR_NUM_TYPE_ATTRS = 228;  { number of attribute types  }
     OCI_ATTR_LIST_TYPE_ATTRS = 229;  { list of type attributes  }
     OCI_ATTR_NUM_TYPE_METHODS = 230;  { number of type methods  }
     OCI_ATTR_LIST_TYPE_METHODS = 231;  { list of type methods  }
     OCI_ATTR_MAP_METHOD = 232;  { map method of type  }
     OCI_ATTR_ORDER_METHOD = 233;  { order method of type  }

  { only collection element  }
     OCI_ATTR_NUM_ELEMS = 234;  { number of elements  }

  { only type methods  }
     OCI_ATTR_ENCAPSULATION = 235;  { encapsulation level  }
     OCI_ATTR_IS_SELFISH = 236;  { method selfish  }
     OCI_ATTR_IS_VIRTUAL = 237;  { virtual  }
     OCI_ATTR_IS_INLINE = 238;  { inline  }
     OCI_ATTR_IS_CONSTANT = 239;  { constant  }
     OCI_ATTR_HAS_RESULT = 240;  { has result  }
     OCI_ATTR_IS_CONSTRUCTOR = 241;  { constructor  }
     OCI_ATTR_IS_DESTRUCTOR = 242;  { destructor  }
     OCI_ATTR_IS_OPERATOR = 243;  { operator  }
     OCI_ATTR_IS_MAP = 244;  { a map method  }
     OCI_ATTR_IS_ORDER = 245;  { order method  }
     OCI_ATTR_IS_RNDS = 246;  { read no data state method  }
     OCI_ATTR_IS_RNPS = 247;  { read no process state  }
     OCI_ATTR_IS_WNDS = 248;  { write no data state method  }
     OCI_ATTR_IS_WNPS = 249;  { write no process state  }

  { describing public objects  }
     OCI_ATTR_DESC_PUBLIC = 250;  { public object  }

  {---------------------------OCIPasswordChange------------------------------- }
     OCI_AUTH = $08;  { Change the password but do not login  }

  {------------------------Other Constants------------------------------------ }
     OCI_MAX_FNS = 100;  { max number of OCI Functions  }
     OCI_SQLSTATE_SIZE = 5;
     OCI_ERROR_MAXMSG_SIZE = 1024;  { max size of an error message  }
     OCI_LOBMAXSIZE = MINUB4MAXVAL;  { maximum lob data size  }
     OCI_ROWID_LEN = 23;
  {------------------------ Fail Over Events --------------------------------- }
     OCI_FO_END = $00000001;
     OCI_FO_ABORT = $00000002;
     OCI_FO_REAUTH = $00000004;
     OCI_FO_BEGIN = $00000008;
     OCI_FO_ERROR = $00000010;
  {------------------------- Fail Over Types --------------------------------- }
     OCI_FO_NONE = $00000001;
     OCI_FO_SESSION = $00000002;
     OCI_FO_SELECT = $00000004;
     OCI_FO_TXNAL = $00000008;
  {-----------------------Function Codes-------------------------------------- }
  { OCIInitialize  }
     OCI_FNCODE_INITIALIZE = 1;
  { OCIHandleAlloc  }
     OCI_FNCODE_HANDLEALLOC = 2;
  { OCIHandleFree  }
     OCI_FNCODE_HANDLEFREE = 3;
  { OCIDescriptorAlloc  }
     OCI_FNCODE_DESCRIPTORALLOC = 4;
  { OCIDescriptorFree  }
     OCI_FNCODE_DESCRIPTORFREE = 5;
  { OCIEnvInit  }
     OCI_FNCODE_ENVINIT = 6;
  { OCIServerAttach  }
     OCI_FNCODE_SERVERATTACH = 7;
  { OCIServerDetach  }
     OCI_FNCODE_SERVERDETACH = 8;
  { unused         9  }
  { OCISessionBegin  }
     OCI_FNCODE_SESSIONBEGIN = 10;
  { OCISessionEnd  }
     OCI_FNCODE_SESSIONEND = 11;
  { OCIPasswordChange  }
     OCI_FNCODE_PASSWORDCHANGE = 12;
  { OCIStmtPrepare  }
     OCI_FNCODE_STMTPREPARE = 13;
  { unused       14- 16  }
  { OCIBindDynamic  }
     OCI_FNCODE_BINDDYNAMIC = 17;
  { OCIBindObject  }
     OCI_FNCODE_BINDOBJECT = 18;
  { 19 unused  }
  { OCIBindArrayOfStruct  }
     OCI_FNCODE_BINDARRAYOFSTRUCT = 20;
  { OCIStmtExecute  }
     OCI_FNCODE_STMTEXECUTE = 21;
  { unused 22-24  }
  { OCIDefineObject  }
     OCI_FNCODE_DEFINEOBJECT = 25;
  { OCIDefineDynamic  }
     OCI_FNCODE_DEFINEDYNAMIC = 26;
  { OCIDefineArrayOfStruct  }
     OCI_FNCODE_DEFINEARRAYOFSTRUCT = 27;
  { OCIStmtFetch  }
     OCI_FNCODE_STMTFETCH = 28;
  { OCIStmtGetBindInfo  }
     OCI_FNCODE_STMTGETBIND = 29;
  { 30, 31 unused  }
  { OCIDescribeAny  }
     OCI_FNCODE_DESCRIBEANY = 32;
  { OCITransStart  }
     OCI_FNCODE_TRANSSTART = 33;
  { OCITransDetach  }
     OCI_FNCODE_TRANSDETACH = 34;
  { OCITransCommit  }
     OCI_FNCODE_TRANSCOMMIT = 35;
  { 36 unused  }
  { OCIErrorGet  }
     OCI_FNCODE_ERRORGET = 37;
  { OCILobFileOpen  }
     OCI_FNCODE_LOBOPENFILE = 38;
  { OCILobFileClose  }
     OCI_FNCODE_LOBCLOSEFILE = 39;
  { 40 was LOBCREATEFILE, unused  }
  { 41 was OCILobFileDelete, unused   }
  { OCILobCopy  }
     OCI_FNCODE_LOBCOPY = 42;
  { OCILobAppend  }
     OCI_FNCODE_LOBAPPEND = 43;
  { OCILobErase  }
     OCI_FNCODE_LOBERASE = 44;
  { OCILobGetLength  }
     OCI_FNCODE_LOBLENGTH = 45;
  { OCILobTrim  }
     OCI_FNCODE_LOBTRIM = 46;
  { OCILobRead  }
     OCI_FNCODE_LOBREAD = 47;
  { OCILobWrite  }
     OCI_FNCODE_LOBWRITE = 48;
  { 49 unused  }
  { OCIBreak  }
     OCI_FNCODE_SVCCTXBREAK = 50;
  { OCIServerVersion  }
     OCI_FNCODE_SERVERVERSION = 51;
  { unused 52, 53  }
  { OCIAttrGet  }
     OCI_FNCODE_ATTRGET = 54;
  { OCIAttrSet  }
     OCI_FNCODE_ATTRSET = 55;
  { OCIParamSet  }
     OCI_FNCODE_PARAMSET = 56;
  { OCIParamGet  }
     OCI_FNCODE_PARAMGET = 57;
  { OCIStmtGetPieceInfo  }
     OCI_FNCODE_STMTGETPIECEINFO = 58;
  { OCILdaToSvcCtx  }
     OCI_FNCODE_LDATOSVCCTX = 59;
  { 60 unused  }
  { OCIStmtSetPieceInfo  }
     OCI_FNCODE_STMTSETPIECEINFO = 61;
  { OCITransForget  }
     OCI_FNCODE_TRANSFORGET = 62;
  { OCITransPrepare  }
     OCI_FNCODE_TRANSPREPARE = 63;
  { OCITransRollback  }
     OCI_FNCODE_TRANSROLLBACK = 64;
  { OCIDefineByPos  }
     OCI_FNCODE_DEFINEBYPOS = 65;
  { OCIBindByPos  }
     OCI_FNCODE_BINDBYPOS = 66;
  { OCIBindByName  }
     OCI_FNCODE_BINDBYNAME = 67;
  { OCILobAssign  }
     OCI_FNCODE_LOBASSIGN = 68;
  { OCILobIsEqual  }
     OCI_FNCODE_LOBISEQUAL = 69;
  { OCILobLocatorIsInit  }
     OCI_FNCODE_LOBISINIT = 70;
  { 71 was lob locator size in beta2  }
  { OCILobEnableBuffering  }
     OCI_FNCODE_LOBENABLEBUFFERING = 71;
  { OCILobCharSetID  }
     OCI_FNCODE_LOBCHARSETID = 72;
  { OCILobCharSetForm  }
     OCI_FNCODE_LOBCHARSETFORM = 73;
  { OCILobFileSetName  }
     OCI_FNCODE_LOBFILESETNAME = 74;
  { OCILobFileGetName  }
     OCI_FNCODE_LOBFILEGETNAME = 75;
  { OCILogon  }
     OCI_FNCODE_LOGON = 76;
  { OCILogoff  }
     OCI_FNCODE_LOGOFF = 77;
  { OCILobDisableBuffering  }
     OCI_FNCODE_LOBDISABLEBUFFERING = 78;
  { OCILobFlushBuffer  }
     OCI_FNCODE_LOBFLUSHBUFFER = 79;
  { OCILobLoadFromFile  }
     OCI_FNCODE_LOBLOADFROMFILE = 80;

  {--------------------------- FILE open modes ------------------------------- }
     OCI_FILE_READONLY = 1;  { readonly mode open for FILE types  }

  {----------------------- LOB Buffering Flush Flags ------------------------- }
     OCI_LOB_BUFFER_FREE = 1;
     OCI_LOB_BUFFER_NOFREE = 2;

  {--------------------------- OCI Statement Types --------------------------- }
     OCI_STMT_SELECT = 1;  { select statement  }
     OCI_STMT_UPDATE = 2;  { update statement  }
     OCI_STMT_DELETE = 3;  { delete statement  }
     OCI_STMT_INSERT = 4;  { Insert Statement  }
     OCI_STMT_CREATE = 5;  { create statement  }
     OCI_STMT_DROP = 6;  { drop statement  }
     OCI_STMT_ALTER = 7;  { alter statement  }
     OCI_STMT_BEGIN = 8;  { begin ... (pl/sql statement) }
     OCI_STMT_DECLARE = 9;  { declare .. (pl/sql statement )  }

  {--------------------------- OCI Parameter Types --------------------------- }
     OCI_PTYPE_UNK = 0;  { unknown    }
     OCI_PTYPE_TABLE = 1;  { table      }
     OCI_PTYPE_VIEW = 2;  { view       }
     OCI_PTYPE_PROC = 3;  { procedure  }
     OCI_PTYPE_FUNC = 4;  { function   }
     OCI_PTYPE_PKG = 5;  { package    }
     OCI_PTYPE_TYPE = 6;  { user-defined type  }
     OCI_PTYPE_SYN = 7;  { synonym    }
     OCI_PTYPE_SEQ = 8;  { sequence   }
     OCI_PTYPE_COL = 9;  { column     }
     OCI_PTYPE_ARG = 10;  { argument   }
     OCI_PTYPE_LIST = 11;  { list       }
     OCI_PTYPE_TYPE_ATTR = 12;  { user-defined type's attribute  }
     OCI_PTYPE_TYPE_COLL = 13;  { collection type's element  }
     OCI_PTYPE_TYPE_METHOD = 14;  { collection type's element  }
     OCI_PTYPE_TYPE_ARG = 15;  { user-defined type method's argument  }
     OCI_PTYPE_TYPE_RESULT = 16;  { user-defined type method's result  }

  {----------------------------- OCI List Types ------------------------------ }
     OCI_LTYPE_UNK = 0;  { unknown    }
     OCI_LTYPE_COLUMN = 1;  { column list  }
     OCI_LTYPE_ARG_PROC = 2;  { procedure argument list  }
     OCI_LTYPE_ARG_FUNC = 3;  { function argument list  }
     OCI_LTYPE_SUBPRG = 4;  { subprogram list  }
     OCI_LTYPE_TYPE_ATTR = 5;  { type attribute  }
     OCI_LTYPE_TYPE_METHOD = 6;  { type method  }
     OCI_LTYPE_TYPE_ARG_PROC = 7;  { type method w/o result argument list  }
     OCI_LTYPE_TYPE_ARG_FUNC = 8;  { type method w/result argument list  }

type { Handle Definitions
 }
  OCIEnv           = pointer;  { OCI environment handle
 }
  OCIError         = pointer;  { OCI error handle
 }
  OCISvcCtx        = pointer;  { OCI service handle }
  OCIStmt          = pointer;  { OCI statement handle }
  OCIBind          = pointer;  { OCI bind handle
 }
  OCIDefine        = pointer;  { OCI Define handle
 }
  OCIDescribe      = pointer;  { OCI Describe handle
 }
  OCIServer        = pointer;  { OCI Server handle
 }
  OCISession       = pointer;  { OCI Authentication handle
 }
  OCIComplexObject = pointer;  { OCI COR handle
 }
  OCITrans         = pointer;  { OCI Transaction handle
 }
  OCISecurity      = pointer;  { OCI Security handle
 }
  OCIDirPathCtx    = pointer;  { OCI Direct Path handle
 }

type
  OCILobLocator         = pointer;  { OCI LOB Locator }
  OCIType               = pointer;
  OCICallbackInBind     = pointer;
  OCICallbackOutBind    = pointer;
  OCISnapshot           = pointer;
  OCIResult             = pointer;
  OCICallbackDefine     = pointer;
  OCIParam              = pointer;


  function OCIInitialize(
          mode:cardinal;
          ctxp:pointer;
          malocfp:pointer;
          ralocfp:pointer;
          mfreefp:pointer):sword;cdecl;external;

  function OCIHandleAlloc(
          parenth:pointer;
          var hndlpp:pointer;
          AType:cardinal;
          xtramem_sz:cardinal;
          usrmempp:pointer):sword;cdecl;external;

  function OCIHandleFree(
          hndlp:pointer;
          AType:cardinal):sword;cdecl;external;

  function OCIEnvInit(
          var envp: OCIEnv;

          mode: ub4;
          xtramemsz: Integer;
          usrmempp: pointer): sword; cdecl;external;

  function OCIDescriptorAlloc(
          parenth:pointer;
          descpp:pointer;
          AType:cardinal;
          xtramem_sz:cardinal;
          usrmempp:pointer):sword;cdecl;external;

  function OCIDescriptorFree(
          descp:pointer;
          AType:cardinal):sword;cdecl;external;

  function OCIServerAttach(
          srvhp:OCIServer;
          errhp:OCIError;
          dblink:PChar;
          dblink_len:longint;
          mode:cardinal):sword;cdecl;external;

  function OCIServerDetach(
          srvhp:OCIServer;
          errhp:OCIError;
          mode:cardinal):sword;cdecl;external;

  function OCISessionBegin(
          svchp:OCISvcCtx;
          errhp:OCIError;
          usrhp:OCISession;
          credt:cardinal;
          mode:cardinal):sword;cdecl;external;

  function OCISessionEnd(
          svchp:OCISvcCtx;
          errhp:OCIError;
          usrhp:OCISession;
          mode:cardinal):sword;cdecl;external;

  function OCILogon(
          envhp:OCIEnv;
          errhp:OCIError;
          var svchp:OCISvcCtx;
          username:PChar;
          uname_len:cardinal;
          password:PChar;
          passwd_len:cardinal;
          dbname:PChar;
          dbname_len:cardinal):sword;cdecl;external;

  function OCILogoff(
          svchp:OCISvcCtx;
          errhp:OCIError):sword;cdecl;external;

  function OCIErrorGet(
          hndlp:pointer;
          recordno:cardinal;
          sqlstate:PChar;
          var errcodep:PLongint;
          bufp:PChar;
          bufsiz:cardinal;
          AType:cardinal):sword;cdecl;external;

  function OCIPasswordChange(
          svchp:OCISvcCtx;
          errhp:OCIError;
          user_name:PChar;
          usernm_len:cardinal;
          opasswd:PChar;
          opasswd_len:cardinal;
          npasswd:PChar;
          npasswd_len:cardinal;
          mode:cardinal):sword;cdecl;external;

  function OCIStmtPrepare(
          stmtp:OCIStmt;
          errhp:OCIError;
          stmt:PChar;
          stmt_len:cardinal;
          language:cardinal;
          mode:cardinal):sword;cdecl;external;

  function OCIBindByPos(
          stmtp:OCIStmt;
          bindp:OCIBind;
          errhp:OCIError;
          position:cardinal;
          valuep:pointer;
          value_sz:longint;
          dty:word;
          indp:pointer;
          alenp:pword; rcodep:pword;
          maxarr_len:cardinal;
          curelep:pcardinal;
          mode:cardinal):sword;cdecl;external;

  function OCIBindByName(
          stmtp:OCIStmt;
          bindp:OCIBind;
          errhp:OCIError;
          placeholder:PChar;
          placeh_len:longint;
          valuep:pointer;
          value_sz:longint;
          dty:word;
          indp:pointer;
          alenp:pword;
          rcodep:pword;
          maxarr_len:cardinal;
          curelep:pcardinal;
          mode:cardinal):sword;cdecl;external;

  function OCIBindObject(
          bindp:OCIBind;
          errhp:OCIError;
          AType:OCIType;
          pgvpp:pointer;
          pvszsp:pcardinal;
          indpp:pointer;
          indszp:pcardinal):sword;cdecl;external;

  function OCIBindDynamic(
          bindp:OCIBind;
          errhp:OCIError;
          ictxp:pointer;
          icbfp:OCICallbackInBind;
          octxp:pointer;
          ocbfp:OCICallbackOutBind):sword;cdecl;external;

  function OCIBindArrayOfStruct(
          bindp:OCIBind;
          errhp:OCIError;
          pvskip:cardinal;
          indskip:cardinal;
          alskip:cardinal;
          rcskip:cardinal):sword;cdecl;external;

  function OCIStmtGetPieceInfo(
          stmtp:OCIStmt;
          errhp:OCIError;
          hndlpp:pointer;
          typep:pcardinal;
          in_outp:pbyte;
          iterp:pcardinal;
          idxp:pcardinal;
          piecep:pbyte):sword;cdecl;external;

  function OCIStmtSetPieceInfo(
          hndlp:pointer;
          AType:cardinal;
          errhp:OCIError;
          bufp:pointer;
          alenp:pcardinal;
          piece:byte;
          indp:pointer;
          rcodep:pword):sword;cdecl;external;

  function OCIStmtExecute(
          svchp:OCISvcCtx;
          stmtp:OCIStmt;
          errhp:OCIError;
          iters:cardinal;
          rowoff:cardinal;
          snap_in:OCISnapshot;
          snap_out:OCISnapshot;
          mode:cardinal):sword;cdecl;external;

  function OCIDefineByPos(
          stmtp:OCIStmt;
          defnp:OCIDefine;
          errhp:OCIError;
          position:cardinal;
          valuep:pointer;
          value_sz:longint;
          dty:word;
          indp:pointer;
          rlenp:pword;
          rcodep:pword;
          mode:cardinal):sword;cdecl;external;

  function OCIDefineObject(
          defnp:OCIDefine;
          errhp:OCIError;
          AType:OCIType;
          pgvpp:pointer;
          pvszsp:pcardinal;
          indpp:pointer;
          indszp:pcardinal):sword;cdecl;external;

  function OCIDefineDynamic(
          defnp:OCIDefine;
          errhp:OCIError;
          octxp:pointer;
          ocbfp:OCICallbackDefine):sword;cdecl;external;

  function OCIDefineArrayOfStruct(
          defnp:OCIDefine;
          errhp:OCIError;
          pvskip:cardinal;
          indskip:cardinal;
          rlskip:cardinal;
          rcskip:cardinal):sword;cdecl;external;

  function OCIStmtFetch(
          stmtp:OCIStmt;
          errhp:OCIError;
          nrows:cardinal;
          orientation:word;
          mode:cardinal):sword;cdecl;external;

  function OCIStmtGetBindInfo(
          stmtp:OCIStmt;
          errhp:OCIError;
          size:cardinal;
          startloc:cardinal;
          found:plongint;
          bvnp:PChar;
          bvnl:byte;
          invp:PChar;
          inpl:byte;
          dupl:byte;
          hndl:OCIBind):sword;cdecl;external;

  function OCIDescribeAny(
          svchp:OCISvcCtx;
          errhp:OCIError;
          objptr:pointer;
          objnm_len:cardinal;
          objptr_typ:byte;
          info_level:byte;
          objtyp:byte;
          dschp:OCIDescribe):sword;cdecl;external;

  function OCIParamGet(
          hndlp:pointer;
          htype:cardinal;
          errhp:OCIError;
          parmdpp:pointer;
          pos:cardinal):sword;cdecl;external;

  function OCIParamSet(
          hdlp:pointer;
          htyp:cardinal;
          errhp:OCIError;
          dscp:pointer;
          dtyp:cardinal;
          pos:cardinal):sword;cdecl;external;

  function OCITransStart(
          svchp:OCISvcCtx;
          errhp:OCIError;
          timeout:uword;
          flags:cardinal):sword;cdecl;external;

  function OCITransDetach(
          svchp:OCISvcCtx;
          errhp:OCIError;
          flags:cardinal):sword;cdecl;external;

  function OCITransCommit(
          svchp:OCISvcCtx;
          errhp:OCIError;
          flags:cardinal):sword;cdecl;external;

  function OCITransRollback(
          svchp:OCISvcCtx;
          errhp:OCIError;
          flags:cardinal):sword;cdecl;external;

  function OCITransPrepare(
          svchp:OCISvcCtx;
          errhp:OCIError;
          flags:cardinal):sword;cdecl;external;

  function OCITransForget(
          svchp:OCISvcCtx;
          errhp:OCIError;
          flags:cardinal):sword;cdecl;external;

  function OCILobAppend(
          svchp:OCISvcCtx;
          errhp:OCIError;
          dst_locp:OCILobLocator;
          src_locp:OCILobLocator):sword;cdecl;external;

  function OCILobAssign(
          envhp:OCIEnv;
          errhp:OCIError;
          src_locp:OCILobLocator;
          dst_locpp:OCILobLocator):sword;cdecl;external;

  function OCILobCharSetForm(
          envhp:OCIEnv;
          errhp:OCIError;
          locp:OCILobLocator;
          csfrm:pbyte):sword;cdecl;external;

  function OCILobCharSetId(
          envhp:OCIEnv;
          errhp:OCIError;
          locp:OCILobLocator;
          csid:pword):sword;cdecl;external;

  function OCILobCopy(
          svchp:OCISvcCtx;
          errhp:OCIError;
          dst_locp:OCILobLocator;
          src_locp:OCILobLocator;
          amount:cardinal;
          dst_offset:cardinal;
          src_offset:cardinal):sword;cdecl;external;

  function OCILobDisableBuffering(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator):sword;cdecl;external;

  function OCILobEnableBuffering(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator):sword;cdecl;external;

  function OCILobErase(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator;
          amount:pcardinal;
          offset:cardinal):sword;cdecl;external;

  function OCILobFileClose(
          svchp:OCISvcCtx;
          errhp:OCIError;
          filep:OCILobLocator):sword;cdecl;external;

  function OCILobFileCloseAll(
          svchp:OCISvcCtx;
          errhp:OCIError):sword;cdecl;external;

  function OCILobFileExists(
          svchp:OCISvcCtx;
          errhp:OCIError;
          filep:OCILobLocator;
          flag:pboolean):sword;cdecl;external;

  function OCILobFileGetName(
          envhp:OCIEnv;
          errhp:OCIError;
          filep:OCILobLocator;
          dir_alias:PChar;
          d_length:pword;
          filename:PChar;
          f_length:pword):sword;cdecl;external;

  function OCILobFileIsOpen(
          svchp:OCISvcCtx;
          errhp:OCIError;
          filep:OCILobLocator;
          flag:pboolean):sword;cdecl;external;

  function OCILobFileOpen(
          svchp:OCISvcCtx;
          errhp:OCIError;
          filep:OCILobLocator;
          mode:byte):sword;cdecl;external;

  function OCILobFileSetName(
          envhp:OCIEnv;
          errhp:OCIError;
          filepp:OCILobLocator;
          dir_alias:PChar;
          d_length:word;
          filename:PChar;
          f_length:word):sword;cdecl;external;

  function OCILobFlushBuffer(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator;
          flag:cardinal):sword;cdecl;external;

  function OCILobGetLength(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator;
          lenp:pcardinal):sword;cdecl;external;

  function OCILobIsEqual(
          envhp:OCIEnv;
          x:OCILobLocator;
          y:OCILobLocator;
          is_equal:pboolean):sword;cdecl;external;

  function OCILobLoadFromFile(
          svchp:OCISvcCtx;
          errhp:OCIError;
          dst_locp:OCILobLocator;
          src_filep:OCILobLocator;
          amount:cardinal;
          dst_offset:cardinal;
          src_offset:cardinal):sword;cdecl;external;

  function OCILobLocatorIsInit(
          envhp:OCIEnv;
          errhp:OCIError;
          locp:OCILobLocator;
          is_initialized:pboolean):sword;cdecl;external;

  function OCILobRead(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator;
          amtp:pcardinal;
          offset:cardinal;
          bufp:pointer;
          bufl:cardinal;
          ctxp:pointer;
          cbfp:pointer;
          csid:word;
          csfrm:byte):sword;cdecl;external;

  function OCILobTrim(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator;
          newlen:cardinal):sword;cdecl;external;

  function OCILobWrite(
          svchp:OCISvcCtx;
          errhp:OCIError;
          locp:OCILobLocator;
          amtp:pcardinal;
          offset:cardinal;
          bufp:pointer;
          buflen:cardinal;
          piece:byte;
          ctxp:pointer;
          cbfp:pointer;
          csid:word;
          csfrm:byte):sword;cdecl;external;

  function OCIBreak(
          hndlp:pointer;
          errhp:OCIError):sword;cdecl;external;

  function OCIReset(
          hndlp:pointer;
          errhp:OCIError):sword;cdecl;external;

  function OCIServerVersion(
          hndlp:pointer;
          errhp:OCIError;
          bufp:PChar;
          bufsz:cardinal;
          hndltype:byte):sword;cdecl;external;

  function OCIAttrGet(
          trgthndlp:pointer;
          trghndltyp:cardinal;
          attributep:pointer;
          sizep:pcardinal;
          attrtype:cardinal;
          errhp:OCIError):sword;cdecl;external;

  function OCIAttrSet(
          trgthndlp:pointer;
          trghndltyp:cardinal;
          attributep:pointer;
          size:cardinal;
          attrtype:cardinal;
          errhp:OCIError):sword;cdecl;external;

  function OCISvcCtxToLda(
          svchp:OCISvcCtx;
          errhp:OCIError;
          ldap:pLda_Def):sword;cdecl;external;

  function OCILdaToSvcCtx(
          svchpp:OCISvcCtx;
          errhp:OCIError;
          ldap:pLda_Def):sword;cdecl;external;

  function OCIResultSetToStmt(
          rsetdp:OCIResult;
          errhp:OCIError):sword;cdecl;external;

  function OCIDateToText(
      err:OCIError;
          date:POCIDate;
          fmt:PChar;
          fmt_length:ub1;
          lang_name:PChar;
      lang_length:ub4;
          buf_size:PCardinal;
          buf:PChar):sword;cdecl;external;

  {-------------------------------------------------------------------------------------------}
  { Security Package                                                                          }
  {-------------------------------------------------------------------------------------------}
  {
  function OCISecurityInitialize(sechandle:pOCISecurity; error_handle:OCIError):sword;cdecl;

  function OCISecurityTerminate(sechandle:pOCISecurity; error_handle:OCIError):sword;cdecl;

  function OCISecurityOpenWallet(osshandle:pOCISecurity; error_handle:OCIError; wrllen:size_t; wallet_resource_locator:PChar; pwdlen:size_t;
             password:PChar; wallet:pnzttWallet):sword;cdecl;

  function OCISecurityCloseWallet(osshandle:pOCISecurity; error_handle:OCIError; wallet:pnzttWallet):sword;cdecl;

  function OCISecurityCreateWallet(osshandle:pOCISecurity; error_handle:OCIError; wrllen:size_t; wallet_resource_locator:PChar; pwdlen:size_t;
             password:PChar; wallet:pnzttWallet):sword;cdecl;

  function OCISecurityDestroyWallet(osshandle:pOCISecurity; error_handle:OCIError; wrllen:size_t; wallet_resource_locator:PChar; pwdlen:size_t;
             password:PChar):sword;cdecl;

  function OCISecurityStorePersona(osshandle:pOCISecurity; error_handle:OCIError; persona:ppnzttPersona; wallet:pnzttWallet):sword;cdecl;

  function OCISecurityOpenPersona(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona):sword;cdecl;

  function OCISecurityClosePersona(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona):sword;cdecl;

  function OCISecurityRemovePersona(osshandle:pOCISecurity; error_handle:OCIError; persona:ppnzttPersona):sword;cdecl;

  function OCISecurityCreatePersona(osshandle:pOCISecurity; error_handle:OCIError; identity_type:nzttIdentType; cipher_type:nzttCipherType; desc:pnzttPersonaDesc;
             persona:ppnzttPersona):sword;cdecl;

  function OCISecuritySetProtection(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; crypto_engine_function:nzttcef; data_unit_format:nztttdufmt;
             protection_info:pnzttProtInfo):sword;cdecl;

  function OCISecurityGetProtection(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; crypto_engine_function:nzttcef; data_unit_format_ptr:pnztttdufmt;
             protection_info:pnzttProtInfo):sword;cdecl;

  function OCISecurityRemoveIdentity(osshandle:pOCISecurity; error_handle:OCIError; identity_ptr:ppnzttIdentity):sword;cdecl;

  function OCISecurityCreateIdentity(osshandle:pOCISecurity; error_handle:OCIError; AType:nzttIdentType; desc:pnzttIdentityDesc; identity_ptr:ppnzttIdentity):sword;cdecl;

  function OCISecurityAbortIdentity(osshandle:pOCISecurity; error_handle:OCIError; identity_ptr:ppnzttIdentity):sword;cdecl;

  function OCISecurityFreeIdentity(osshandle:pOCISecurity; error_handle:OCIError; identity_ptr:ppnzttIdentity):sword;cdecl;

  function OCISecurityStoreTrustedIdentity(osshandle:pOCISecurity; error_handle:OCIError; identity_ptr:ppnzttIdentity; persona:pnzttPersona):sword;cdecl;

  function OCISecuritySign(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; signature_state:nzttces; input_length:size_t;
             input:pbyte; buffer_block:pnzttBufferBlock):sword;cdecl;

  function OCISecuritySignExpansion(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; inputlen:size_t; signature_length:psize_t):sword;cdecl;

  function OCISecurityVerify(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; signature_state:nzttces; siglen:size_t;
             signature:pbyte; extracted_message:pnzttBufferBlock; verified:pboolean; validated:pboolean; signing_party_identity:ppnzttIdentity):sword;cdecl;

  function OCISecurityValidate(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; identity:pnzttIdentity; validated:pboolean):sword;cdecl;

  function OCISecuritySignDetached(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; signature_state:nzttces; input_length:size_t;
             input:pbyte; signature:pnzttBufferBlock):sword;cdecl;

  function OCISecuritySignDetExpansion(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; input_length:size_t; required_buffer_length:psize_t):sword;cdecl;

  function OCISecurityVerifyDetached(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; signature_state:nzttces; data_length:size_t;
             data:pbyte; siglen:size_t; signature:pbyte; verified:pboolean; validated:pboolean;
             signing_party_identity:ppnzttIdentity):sword;cdecl;

  function OCISecurity_PKEncrypt(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; number_of_recipients:size_t; recipient_list:pnzttIdentity;
             encryption_state:nzttces; input_length:size_t; input:pbyte; encrypted_data:pnzttBufferBlock):sword;cdecl;

  function OCISecurityPKEncryptExpansion(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; number_recipients:size_t; input_length:size_t;
             buffer_length_required:psize_t):sword;cdecl;

  function OCISecurityPKDecrypt(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; encryption_state:nzttces; input_length:size_t;
             input:pbyte; encrypted_data:pnzttBufferBlock):sword;cdecl;

  function OCISecurityEncrypt(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; encryption_state:nzttces; input_length:size_t;
             input:pbyte; encrypted_data:pnzttBufferBlock):sword;cdecl;

  function OCISecurityEncryptExpansion(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; input_length:size_t; encrypted_data_length:psize_t):sword;cdecl;

  function OCISecurityDecrypt(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; decryption_state:nzttces; input_length:size_t;
             input:pbyte; decrypted_data:pnzttBufferBlock):sword;cdecl;

  function OCISecurityEnvelope(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; number_of_recipients:size_t; identity:pnzttIdentity;
             encryption_state:nzttces; input_length:size_t; input:pbyte; enveloped_data:pnzttBufferBlock):sword;cdecl;

  function OCISecurityDeEnvelope(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; decryption_state:nzttces; input_length:size_t;
             input:pbyte; output_message:pnzttBufferBlock; verified:pboolean; validated:pboolean; sender_identity:ppnzttIdentity):sword;cdecl;

  function OCISecurityKeyedHash(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; hash_state:nzttces; input_length:size_t;
             input:pbyte; keyed_hash:pnzttBufferBlock):sword;cdecl;

  function OCISecurityKeyedHashExpansion(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; input_length:size_t; required_buffer_length:psize_t):sword;cdecl;

  function OCISecurityHash(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; hash_state:nzttces; input:size_t;
             input_length:pbyte; hash:pnzttBufferBlock):sword;cdecl;

  function OCISecurityHashExpansion(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; input_length:size_t; required_buffer_length:psize_t):sword;cdecl;

  function OCISecuritySeedRandom(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; seed_length:size_t; seed:pbyte):sword;cdecl;

  function OCISecurityRandomBytes(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; number_of_bytes_desired:size_t; random_bytes:pnzttBufferBlock):sword;cdecl;

  function OCISecurityRandomNumber(osshandle:pOCISecurity; error_handle:OCIError; persona:pnzttPersona; random_number_ptr:puword):sword;cdecl;

  function OCISecurityInitBlock(osshandle:pOCISecurity; error_handle:OCIError; buffer_block:pnzttBufferBlock):sword;cdecl;

  function OCISecurityReuseBlock(osshandle:pOCISecurity; error_handle:OCIError; buffer_block:pnzttBufferBlock):sword;cdecl;

  function OCISecurityPurgeBlock(osshandle:pOCISecurity; error_handle:OCIError; buffer_block:pnzttBufferBlock):sword;cdecl;

  function OCISecuritySetBlock(osshandle:pOCISecurity; error_handle:OCIError; flags_to_set:uword; buffer_length:size_t; used_buffer_length:size_t;
             buffer:pbyte; buffer_block:pnzttBufferBlock):sword;cdecl;

  function OCISecurityGetIdentity(osshandle:pOCISecurity; error_handle:OCIError; namelen:size_t; distinguished_name:PChar; identity:ppnzttIdentity):sword;cdecl;

  function OCIAQEnq(svchp:OCISvcCtx; errhp:OCIError; queue_name:PChar; enqopt:pOCIAQEnqOptions; msgprop:pOCIAQMsgProperties;
             payload_tdo:pOCIType; payload:ppointer; payload_ind:ppointer; msgid:ppOCIRaw; flags:cardinal):sword;cdecl;

  function OCIAQDeq(svchp:OCISvcCtx; errhp:OCIError; queue_name:PChar; deqopt:pOCIAQDeqOptions; msgprop:pOCIAQMsgProperties;
             payload_tdo:pOCIType; payload:ppointer; payload_ind:ppointer; msgid:ppOCIRaw; flags:cardinal):sword;cdecl;
}

  {-------------------------------------------------------------------------------------------}
  { Datatype Mapping                                                                          }
  {-------------------------------------------------------------------------------------------}

implementation


end.
