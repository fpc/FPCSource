{
    This file is part of the Free Component Library (FCL)

    MS SQL Server connection using DB-Library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    The Original Code was created by (c) 2010 Ladislav Karrach (Windows)
    for the Free Pascal project.
 **********************************************************************

    MS SQL Server Client Library is required (ntwdblib.dll)
    - or -
    FreeTDS (dblib.dll)
      freetds.conf: (http://www.freetds.org/userguide/freetdsconf.htm)
        [global]
        tds version = 7.1
        client charset = UTF-8
        port = 1433 or instance = ...  (optional)
        dump file = freetds.log        (optional)
        text size = 2147483647         (optional)

    TMSSQLConnection properties:
      HostName - can be specified also as 'servername:port' or 'servername\instance'
      CharSet - if you use Microsoft DB-Lib and set to 'UTF-8' then char/varchar fields will be UTF8Encoded/Decoded
                if you use FreeTDS DB-Lib then you must compile with iconv support (requires libiconv2.dll) or cast char/varchar to nchar/nvarchar in SELECTs
      Params - "AutoCommit=true" - if you don't want explicitly commit/rollback transactions
               "TextSize=16777216 - set maximum size of text/image data returned
}
unit mssqlconn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset,
  dblib;

type

  TClientCharset = (ccNone, ccUTF8, ccISO88591, ccUnknown);

  { TMSSQLConnection }

  TMSSQLConnection = class(TSQLConnection)
  private
    FDBLogin: PLOGINREC;
    FDBProc : PDBPROCESS;
    Ftds    : integer;     // TDS protocol version
    Fstatus : STATUS;      // current result/rows fetch status
    function CheckError(const Ret: RETCODE): RETCODE;
    procedure DBExecute(const cmd: string);
    function TranslateFldType(SQLDataType: integer): TFieldType;
    function ClientCharset: TClientCharset;
    function AutoCommit: boolean;
    function IsSybase: boolean;
  protected
    // Overrides from TSQLConnection
    function GetHandle:pointer; override;
    function GetAsSQLText(Param : TParam) : string; overload; override;
    // - Connect/disconnect
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    // - Handle (de)allocation
    function AllocateCursorHandle:TSQLCursor; override;
    procedure DeAllocateCursorHandle(var cursor:TSQLCursor); override;
    function AllocateTransactionHandle:TSQLHandle; override;
    // - Statement handling
    function StrToStatementType(s : string) : TStatementType; override;
    procedure PrepareStatement(cursor:TSQLCursor; ATransaction:TSQLTransaction; buf:string; AParams:TParams); override;
    procedure UnPrepareStatement(cursor:TSQLCursor); override;
    // - Transaction handling
    function GetTransactionHandle(trans:TSQLHandle):pointer; override;
    function StartDBTransaction(trans:TSQLHandle; AParams:string):boolean; override;
    function Commit(trans:TSQLHandle):boolean; override;
    function Rollback(trans:TSQLHandle):boolean; override;
    procedure CommitRetaining(trans:TSQLHandle); override;
    procedure RollbackRetaining(trans:TSQLHandle); override;
    // - Statement execution
    procedure Execute(cursor:TSQLCursor; ATransaction:TSQLTransaction; AParams:TParams); override;
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    // - Result retrieving
    procedure AddFieldDefs(cursor:TSQLCursor; FieldDefs:TFieldDefs); override;
    function Fetch(cursor:TSQLCursor):boolean; override;
    function LoadField(cursor:TSQLCursor; FieldDef:TFieldDef; buffer:pointer; out CreateBlob : boolean):boolean; override;
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction : TSQLTransaction); override;
    procedure FreeFldBuffers(cursor:TSQLCursor); override;
    // - UpdateIndexDefs
    procedure UpdateIndexDefs(IndexDefs:TIndexDefs; TableName:string); override;
    // - Schema info
    function GetSchemaInfoSQL(SchemaType:TSchemaType; SchemaObjectName, SchemaObjectPattern:string):string; override;
  public
    constructor Create(AOwner : TComponent); override;
    //property TDS:integer read Ftds;
  published
    // Redeclare properties from TSQLConnection
    property Password;
    property Transaction;
    property UserName;
    property CharSet;
    property HostName;
    // Redeclare properties from TDatabase
    property Connected;
    property Role;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

  { TSybaseConnection }

  TSybaseConnection = class(TMSSQLConnection)
  public
    constructor Create(AOwner : TComponent); override;
  end;

  { EMSSQLDatabaseError }

  EMSSQLDatabaseError = class(EDatabaseError)
    public
      DBErrorCode : integer;
  end;

  { TMSSQLConnectionDef }

  TMSSQLConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;

  { TSybaseConnectionDef }

  TSybaseConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;


var
   DBLibLibraryName: string = DBLIBDLL;

implementation

uses DBConst, StrUtils, FmtBCD;

type

  { TDBLibCursor }

  TDBLibCursor = class(TSQLCursor)
  protected
    FQuery: string;         //:ParamNames converted to $1,$2,..,$n
    FCanOpen: boolean;      //can return rows?
    FRowsAffected: integer;
    FParamReplaceString: string;
    function ReplaceParams(AParams: TParams; ASQLConnection: TMSSQLConnection): string; //replaces parameters placeholders $1,$2,..$n in FQuery with supplied values in AParams
  end;


const
  SBeginTransaction = 'BEGIN TRANSACTION';
  SAutoCommit = 'AUTOCOMMIT';
  STextSize   = 'TEXTSIZE';


var
  DBErrorStr, DBMsgStr: string;
  DBErrorNo, DBMsgNo: integer;

function DBErrHandler(dbproc: PDBPROCESS; severity, dberr, oserr:INT; dberrstr, oserrstr:PChar):INT; cdecl;
begin
  DBErrorStr:=DBErrorStr+LineEnding+dberrstr;
  DBErrorNo :=dberr;
  Result    :=INT_CANCEL;
end;

function DBMsgHandler(dbproc: PDBPROCESS; msgno: DBINT; msgstate, severity:INT; msgtext, srvname, procname:PChar; line:DBUSMALLINT):INT; cdecl;
begin
  DBMsgStr:=DBMsgStr+LineEnding+msgtext;
  DBMsgNo :=msgno;
  Result  :=0;
end;


{ TDBLibCursor }

function TDBLibCursor.ReplaceParams(AParams: TParams; ASQLConnection: TMSSQLConnection): string;
var i:integer;
    ParamNames, ParamValues: array of string;
begin
  if Assigned(AParams) and (AParams.Count > 0) then //taken from mysqlconn, pqconnection
  begin
    setlength(ParamNames, AParams.Count);
    setlength(ParamValues, AParams.Count);
    for i := 0 to AParams.Count -1 do
    begin
      ParamNames[AParams.Count-i-1] := format('%s%d', [FParamReplaceString, AParams[i].Index+1]);
      ParamValues[AParams.Count-i-1] := ASQLConnection.GetAsSQLText(AParams[i]);
      //showmessage(ParamNames[AParams.Count-i-1] + '=' + ParamValues[AParams.Count-i-1]);
    end;
    Result := stringsreplace(FQuery, ParamNames, ParamValues, [rfReplaceAll]);
  end
  else
    Result := FQuery;
end;


{ TSybaseConnection }

constructor TSybaseConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Ftds := DBTDS_50;
end;


{ TMSSQLConnection }

function TMSSQLConnection.IsSybase: boolean;
begin
  Result := (Ftds=DBTDS_50) or (Ftds=DBTDS_42);
end;

function TMSSQLConnection.CheckError(const Ret: RETCODE): RETCODE;
var E: EMSSQLDatabaseError;
    CompName: string;
begin
  if Ret=FAIL then
  begin
    if DBErrorStr = '' then
      case DBErrorNo of
        SYBEFCON: DBErrorStr:='SQL Server connection failed!';
      end;
    if Self.Name = '' then CompName := Self.ClassName else CompName := Self.Name;
    E:=EMSSQLDatabaseError.CreateFmt('%s : Error %d : %s'+LineEnding+'%s', [CompName, DBErrorNo, DBErrorStr, DBMsgStr]);
    E.DBErrorCode:=DBErrorNo;
    DBErrorStr:='';
    DBMsgStr:='';
    raise E;
  end;
  Result:=Ret;
end;

constructor TMSSQLConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnOptions := FConnOptions + [sqEscapeRepeat];
  //FieldNameQuoteChars:=DoubleQuotes; //default
  Ftds := DBTDS_UNKNOWN;
end;

function TMSSQLConnection.GetHandle: pointer;
begin
  Result:=FDBProc;
end;

function TMSSQLConnection.GetAsSQLText(Param: TParam): string;
  function IsBinary(const s: string): boolean;
  var i: integer;
  begin
    for i:=1 to length(s) do if s[i] < #9 then Exit(true);
    Exit(false);
  end;
  function StrToHex(const s: string): string;
  begin
    setlength(Result, 2*length(s));
    BinToHex(PChar(s), PChar(Result), length(s));
  end;
begin
  if not Param.IsNull then
    case Param.DataType of
      ftBoolean:
        if Param.AsBoolean then
          Result:='1'
        else
          Result:='0';
      ftString, ftFixedChar, ftMemo:
        //if IsBinary(Param.AsString) then
        //  Result := '0x' + StrToHex(Param.AsString)
        //else
        begin
          Result := QuotedStr(Param.AsString);
          if (Ftds >= DBTDS_70) then
            Result := 'N' + Result
          else if (Ftds = 0) and (ClientCharset = ccUTF8) then //hack: Microsoft DB-Lib used
            Result := UTF8Decode(Result);
        end;
      ftBlob, ftBytes, ftVarBytes:
        Result := '0x' + StrToHex(Param.AsString);
      else
        Result := inherited GetAsSQLText(Param);
    end
  else
    Result:=inherited GetAsSQLText(Param);
end;

procedure TMSSQLConnection.DoInternalConnect;
const
  DBVERSION: array[boolean] of BYTE = (DBVER60, DBVERSION_100);
  IMPLICIT_TRANSACTIONS_OFF: array[boolean] of shortstring = ('SET IMPLICIT_TRANSACTIONS OFF', 'SET CHAINED OFF');
  ANSI_DEFAULTS_ON: array[boolean] of shortstring = ('SET ANSI_DEFAULTS ON', 'SET QUOTED_IDENTIFIER ON');
  CURSOR_CLOSE_ON_COMMIT_OFF: array[boolean] of shortstring = ('SET CURSOR_CLOSE_ON_COMMIT OFF', 'SET CLOSE ON ENDTRAN OFF');
begin
  inherited DoInternalConnect;

  InitialiseDBLib(DBLibLibraryName);

  if not DBLibInit then
  begin
    dbinit();
    dberrhandle(@DBErrHandler);
    dbmsghandle(@DBMsgHandler);
    DBLibInit:=true;
  end;

  FDBLogin:=dblogin();
  if FDBLogin=nil then DatabaseError('dblogin() failed!');

  // DBVERSION_100 is ATM not implemented by FreeTDS 0.91;
  // set environment variable TDSVER to 5.0: Windows: SET TDSVER=5.0, Unix/Linux: TDSVER=5.0
  // or in freetds.conf: include "tds version=5.0"
  dbsetlversion(FDBLogin, DBVERSION[IsSybase]);

  if UserName = '' then
    dbsetlsecure(FDBLogin)
  else
  begin
    dbsetlname(FDBLogin, PChar(UserName), DBSETUSER);
    dbsetlname(FDBLogin, PChar(Password), DBSETPWD);
  end;

  if CharSet = '' then
    dbsetlcharset(FDBLogin, 'UTF-8')
  else
    dbsetlcharset(FDBLogin, PChar(CharSet));

  //dbsetlname(FDBLogin, PChar(TIMEOUT_IGNORE), DBSET_LOGINTIME);
  dbsetlogintime(10);

  FDBProc := dbopen(FDBLogin, PChar(HostName));
  if FDBProc=nil then CheckError(FAIL);

  Ftds := dbtds(FDBProc);

  //CheckError( dbsetopt(FDBProc, DBQUOTEDIDENT, '') ); //in FreeTDS executes: "SET QUOTED_IDENTIFIER ON"
  //CheckError( dbsetopt(FDBProc, DBTEXTSIZE,  '2147483647') ); //in FreeTDS: unimplemented, returns FAIL
  //CheckError( dbsetopt(FDBProc, DBTEXTLIMIT, '2147483647') ); //in FreeTDS: unimplemented, returns FAIL, but required by ntwdblib.dll
  //CheckError( dbsqlexec(FDBProc) ); //after setting DBTEXTSIZE option
  //CheckError (dbresults(FDBProc));
  //while dbresults(FDBProc) = SUCCEED do ;

  // Also SQL Server ODBC driver and Microsoft OLE DB Provider for SQL Server set ANSI_DEFAULTS to ON when connecting
  //DBExecute(ANSI_DEFAULTS_ON[IsSybase]);
  DBExecute('SET QUOTED_IDENTIFIER ON');

  if Params.IndexOfName(STextSize) <> -1 then
    DBExecute('SET TEXTSIZE '+Params.Values[STextSize])
  else
    DBExecute('SET TEXTSIZE 16777216');

  if AutoCommit then DBExecute(IMPLICIT_TRANSACTIONS_OFF[IsSybase]); //set connection to autocommit mode - default

  CheckError( dbuse(FDBProc, PChar(DatabaseName)) );
end;

procedure TMSSQLConnection.DoInternalDisconnect;
begin
  inherited DoInternalDisconnect;

  dbclose(FDBProc);
  dbfreelogin(FDBLogin);

  ReleaseDBLib;
end;

function TMSSQLConnection.AllocateCursorHandle: TSQLCursor;
begin
   Result:=TDBLibCursor.Create;
end;

procedure TMSSQLConnection.DeAllocateCursorHandle(var cursor: TSQLCursor);
begin
  FreeAndNil(cursor);
end;

function TMSSQLConnection.StrToStatementType(s: string): TStatementType;
begin
  s:=LowerCase(s);
  if s = 'exec' then
    Result:=stExecProcedure
  else
    Result:=inherited StrToStatementType(s);
end;

procedure TMSSQLConnection.PrepareStatement(cursor: TSQLCursor;
   ATransaction: TSQLTransaction; buf: string; AParams: TParams);
var
  ParamBinding : TParamBinding;
begin
  with cursor as TDBLibCursor do
  begin
    if assigned(AParams) and (AParams.Count > 0) then
      FQuery:=AParams.ParseSQL(buf, false, sqEscapeSlash in ConnOptions, sqEscapeRepeat in ConnOptions, psSimulated, ParamBinding, FParamReplaceString)
    else
      FQuery:=buf;
  end;
end;

procedure TMSSQLConnection.UnPrepareStatement(cursor: TSQLCursor);
begin
  if assigned(FDBProc) and (Fstatus <> NO_MORE_ROWS) then
    dbcanquery(FDBProc);
end;

function TMSSQLConnection.AllocateTransactionHandle: TSQLHandle;
begin
  Result:=nil;
end;

function TMSSQLConnection.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
  Result:=nil;
end;

function TMSSQLConnection.StartDBTransaction(trans: TSQLHandle; AParams: string): boolean;
begin
  Result := not AutoCommit;
  if Result then
    DBExecute(SBeginTransaction);
end;

function TMSSQLConnection.Commit(trans: TSQLHandle): boolean;
begin
  DBExecute('COMMIT');
  Result:=true;
end;

function TMSSQLConnection.Rollback(trans: TSQLHandle): boolean;
begin
  DBExecute('ROLLBACK');
  Result:=true;
end;

procedure TMSSQLConnection.CommitRetaining(trans: TSQLHandle);
begin
  if Commit(trans) then
    DBExecute(SBeginTransaction);
end;

procedure TMSSQLConnection.RollbackRetaining(trans: TSQLHandle);
begin
  if Rollback(trans) then
    DBExecute(SBeginTransaction);
end;

function TMSSQLConnection.AutoCommit: boolean;
begin
  Result := StrToBoolDef(Params.Values[SAutoCommit], False);
end;

procedure TMSSQLConnection.DBExecute(const cmd: string);
begin
  DBErrorStr:='';
  DBMsgStr  :='';
  CheckError( dbcmd(FDBProc, PChar(cmd)) );
  CheckError( dbsqlexec(FDBProc) );
  CheckError( dbresults(FDBProc) );
end;

function TMSSQLConnection.ClientCharset: TClientCharset;
begin
{$IF (FPC_VERSION>=2) AND (FPC_RELEASE>4)}
  case CharSet of
    ''           : Result := ccNone;
    'UTF-8'      : Result := ccUTF8;
    'ISO-8859-1' : Result := ccISO88591;
    else           Result := ccUnknown;
  end;
{$ELSE}
  if CharSet = '' then
    Result := ccNone
  else if CharSet = 'UTF-8' then
    Result := ccUTF8
  else if CharSet = 'ISO-8859-1' then
    Result := ccISO88591
  else
    Result := ccUnknown;
{$ENDIF}
end;

procedure TMSSQLConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
var c: TDBLibCursor;
    cmd: string;
    res: RETCODE;
begin
  c:=cursor as TDBLibCursor;

  cmd := c.ReplaceParams(AParams, Self);
  DBExecute(cmd);

  res := SUCCEED;
  repeat
    c.FCanOpen := dbcmdrow(FDBProc)=SUCCEED;
    c.FRowsAffected := dbcount(FDBProc);
    if assigned(dbiscount) and not dbiscount(FDBProc) then
      c.FRowsAffected := -1;

    if not c.FCanOpen then  //Sybase stored proc.
    begin
      repeat until dbnextrow(FDBProc) = NO_MORE_ROWS;
      res := CheckError( dbresults(FDBProc) );
    end;
  until (res = NO_MORE_RESULTS) or c.FCanOpen;

  if res = NO_MORE_RESULTS then
    Fstatus := NO_MORE_ROWS
  else
    Fstatus := MORE_ROWS;
end;

function TMSSQLConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;
begin
  if assigned(cursor) then
    Result := (cursor as TDBLibCursor).FRowsAffected
  else
    Result := inherited RowsAffected(cursor);
end;

function TMSSQLConnection.TranslateFldType(SQLDataType: integer): TFieldType;
begin
  case SQLDataType of
    SQLCHAR:             Result:=ftFixedChar;
    SQLVARCHAR:          Result:=ftString;
    SQLINT1, SQLINT2:    Result:=ftSmallInt;
    SQLINT4, SQLINTN:    Result:=ftInteger;
    SYBINT8:             Result:=ftLargeInt;
    SQLFLT4, SQLFLT8,
    SQLFLTN:             Result:=ftFloat;
    SQLMONEY4, SQLMONEY,
    SQLMONEYN:           Result:=ftCurrency;
    SQLDATETIM4, SQLDATETIME,
    SQLDATETIMN:         Result:=ftDateTime;
    SQLIMAGE:            Result:=ftBlob;
    SQLTEXT:             Result:=ftMemo;
    SQLDECIMAL, SQLNUMERIC: Result:=ftBCD;
    SQLBIT:              Result:=ftBoolean;
    SQLBINARY:           Result:=ftBytes;
    SQLVARBINARY:        Result:=ftVarBytes;
    SYBUNIQUE:           Result:=ftGuid;
  else
    DatabaseErrorFmt('Unsupported SQL DataType %d "%s"', [SQLDataType, dbprtype(SQLDataType)]);
    Result:=ftUnknown;
  end;
end;

procedure TMSSQLConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);
var i, FieldSize: integer;
    FieldName: string;
    FieldType: TFieldType;
    col: DBCOL;
begin
  col.SizeOfStruct:=sizeof(col);
  for i:=1 to dbnumcols(FDBProc) do
  begin
    if dbtablecolinfo(FDBProc, i, @col) = FAIL then continue;
    FieldName := col.Name;
    FieldType := TranslateFldType(col.Typ);
    case FieldType of
      ftString, ftFixedChar:
        begin
        FieldSize := col.MaxLength;
        if FieldSize > dsMaxStringSize then FieldSize := dsMaxStringSize;
        end;
      ftMemo, ftBlob,
      ftBytes, ftVarBytes:
        FieldSize := col.MaxLength;
      ftBCD:
        begin
        FieldSize := col.Scale;
        if (FieldSize > MaxBCDScale) or (col.Precision-col.Scale > MaxBCDPrecision-MaxBCDScale) then
          FieldType := ftFmtBCD;
        end;
      ftGuid:
        FieldSize := 38;
    else
      FieldSize := 0;
      if col.Identity and (FieldType = ftInteger) then
        FieldType := ftAutoInc;
    end;

{   // dbcolinfo(), dbcoltype() maps VARCHAR->CHAR, VARBINARY->BINARY:
    if col.VarLength {true also when column is nullable} then
      case FieldType of
        ftFixedChar: FieldType := ftString;
        ftBytes    : FieldType := ftVarBytes;
      end;
}
    with TFieldDef.Create(FieldDefs, FieldDefs.MakeNameUnique(FieldName), FieldType, FieldSize, (col.Null=0) and (not col.Identity), i) do
    begin
      //if col.Updatable = 0 then Attributes := Attributes + [faReadonly];
      case FieldType of
        ftBCD,
        ftFmtBCD: Precision := col.Precision;
      end;
    end;
  end;
end;

function TMSSQLConnection.Fetch(cursor: TSQLCursor): boolean;
begin
  //Compute rows resulting from the COMPUTE clause are not processed
  repeat
    Fstatus := dbnextrow(FDBProc);
    Result  := Fstatus=REG_ROW;
  until Result or (Fstatus = NO_MORE_ROWS);

  if Fstatus = NO_MORE_ROWS then
    while dbresults(FDBProc) <> NO_MORE_RESULTS do //process remaining results if there are any
      repeat until dbnextrow(FDBProc) = NO_MORE_ROWS;
end;

function TMSSQLConnection.LoadField(cursor: TSQLCursor; FieldDef: TFieldDef;
   buffer: pointer; out CreateBlob: boolean): boolean;
var i: integer;
    data, dest: PByte;
    datalen, destlen: DBINT;
    srctype, desttype: INT;
    dbdt: DBDATETIME;
    dbdr: DBDATEREC;
    bcdstr: array[0..MaxFmtBCDFractionSize+2] of char;
    f: double;
begin
  CreateBlob:=false;
  i:=FieldDef.FieldNo;

  srctype:=dbcoltype(FDBProc,i);
  data:=dbdata(FDBProc,i);
  datalen:=dbdatlen(FDBProc,i);
  Result:=assigned(data) and (datalen<>0);
  if not Result then
    Exit;

  dest:=buffer;
  destlen:=FieldDef.Size;
  case FieldDef.DataType of
    ftString, ftFixedChar:
      desttype:=SQLCHAR;
    ftBytes:
      desttype:=SQLBINARY;
    ftVarBytes:
      begin
      PWord(dest)^:=datalen;
      inc(dest, sizeof(Word));
      desttype:=SQLBINARY;
      end;
    ftSmallInt:
      begin
      desttype:=SQLINT2;
      destlen:=sizeof(DBSMALLINT); //smallint
      end;
    ftAutoInc,
    ftInteger:
      begin
      desttype:=SQLINT4;
      destlen:=sizeof(DBINT); //integer
      end;
    ftLargeInt:
      begin
      desttype:=SYBINT8;
      destlen:=sizeof(int64);
      end;
    ftCurrency,
    ftFloat:
      begin
      desttype:=SQLFLT8;
      destlen:=sizeof(DBFLT8); //double
      end;
    ftDateTime:
      begin
      dest:=@dbdt;
      desttype:=SQLDATETIME;
      destlen:=sizeof(dbdt);
      end;
    ftBCD:
      begin
      dest:=@f;
      desttype:=SQLFLT8;
      destlen:=sizeof(DBFLT8); //double
      end;
    ftFmtBCD:
      begin
{
      dbnum.precision:=FieldDef.Precision;
      dbnum.scale    :=FieldDef.Size;
      dest:=@dbnum;
      desttype:=SQLNUMERIC;
      destlen:=sizeof(dbnum);
}
      dest:=@bcdstr[0];
      desttype:=SQLCHAR;
      destlen:=sizeof(bcdstr);
      fillchar(bcdstr, destlen, 0); //required when used ntwdblib.dll
      end;
    ftBoolean:
      begin
      desttype:=SQLBIT;
      destlen:=sizeof(WordBool);
      end;
    ftGuid:
      begin
      desttype:=SQLCHAR;
      end;
    ftMemo,
    ftBlob:
      begin
      CreateBlob:=true;
      Exit;
      end
  else
    //DatabaseErrorFmt('Tried to load field of unsupported field type %s',[FieldTypeNames[FieldDef.DataType]]);
    Result:=false;
  end;

  dbconvert(FDBProc, srctype, data , datalen, desttype, dest, destlen);

  case FieldDef.DataType of
    ftString, ftFixedChar:
      begin
        PChar(dest + datalen)^ := #0; //strings must be null-terminated
        if ((Ftds = 0) and (ClientCharset = ccUTF8)) {hack: MS DB-Lib used} or
            (ClientCharset = ccISO88591) {hack: FreeTDS} then
          StrPLCopy(PChar(dest), UTF8Encode(PChar(dest)), destlen);
      end;
    ftDateTime:
      begin
        //detect DBDATEREC version by pre-setting dbdr
        dbdr.millisecond := -1;
        if dbdatecrack(FDBProc, @dbdr, @dbdt) = SUCCEED then
        begin
          if dbdr.millisecond = -1 then
            PDateTime(buffer)^ := composedatetime(
                  encodedate(dbdr.oldyear, dbdr.oldmonth, dbdr.oldday),
                  encodetime(dbdr.oldhour, dbdr.oldminute, dbdr.oldsecond, dbdr.oldmillisecond))
          else
            PDateTime(buffer)^ := composedatetime(
                  encodedate(dbdr.year, dbdr.month, dbdr.day),
                  encodetime(dbdr.hour, dbdr.minute, dbdr.second, dbdr.millisecond));
        end;
      end;
    ftBCD:
      PCurrency(buffer)^:=FloatToCurr(f);
    ftFmtBCD:
      PBCD(buffer)^:=StrToBCD(bcdstr, FSQLFormatSettings); //PBCD(buffer)^:=dbnumerictobcd(dbnum);
  end;
end;

procedure TMSSQLConnection.LoadBlobIntoBuffer(FieldDef: TFieldDef;
   ABlobBuf: PBufBlobField; cursor: TSQLCursor; ATransaction: TSQLTransaction);
var data: PByte;
    datalen: DBINT;
    srctype: INT;
begin
  //see also LoadField
  srctype:=dbcoltype(FDBProc, FieldDef.FieldNo);
  data:=dbdata(FDBProc, FieldDef.FieldNo);
  datalen:=dbdatlen(FDBProc, FieldDef.FieldNo);

  ReAllocMem(ABlobBuf^.BlobBuffer^.Buffer, datalen);

  ABlobBuf^.BlobBuffer^.Size :=
    dbconvert(FDBProc, srctype, data , datalen, srctype, ABlobBuf^.BlobBuffer^.Buffer, datalen);
end;

procedure TMSSQLConnection.FreeFldBuffers(cursor: TSQLCursor);
begin
   inherited FreeFldBuffers(cursor);
end;

procedure TMSSQLConnection.UpdateIndexDefs(IndexDefs: TIndexDefs; TableName: string);
const INDEXES_QUERY: array[boolean] of string=(
      //MS SQL Server; TODO: we can use "execute dbo.sp_helpindex 'TableName'" when Open on Execute will fully work
      'select i.name, i.indid, c.name as col_name,'+
              'indexproperty(i.id, i.name, ''IsUnique''),'+
              'objectproperty(o.id, ''IsPrimaryKey'') '+
      'from sysindexes i '+
            ' join sysindexkeys k on i.id=k.id and i.indid=k.indid '+
            ' join syscolumns c on k.id=c.id and k.colid=c.colid '+
            ' left join sysobjects o on i.name=o.name and i.id=o.parent_obj '+
      'where i.id=object_id(''%s'') '+
      'order by k.indid, k.keyno'
      ,
      //Sybase; http://infocenter.sybase.com/help/index.jsp?topic=/com.sybase.infocenter.help.ase.15.7/title.htm
      'select i.name, i.indid,' +
              'index_col(object_name(i.id),i.indid,c.colid) as col_name,' +
              '(i.status & 2)/2 as IsUnique,' +
              '(i.status & 2048)/2048 as IsPrimaryKey ' +
      'from sysindexes i '+
        ' join syscolumns c on c.id=i.id and c.colid<=i.keycnt-case i.indid when 1 then 0 else 1 end ' +
      'where i.id=object_id(''%s'') '+
        ' and i.indid between 1 and 254 '+ // indid 0 is the table name, 255 is TEXT,IMAGE
      'order by i.indid, c.colid'
      );
var qry : TSQLQuery;
begin
  //if not assigned(Transaction) then
  //  DatabaseError(SErrConnTransactionnSet);

  qry := TSQLQuery.Create(nil);
  qry.Transaction := Transaction;
  qry.Database := Self;
  with qry do
  begin
    ReadOnly := True;
    SQL.Text := format(INDEXES_QUERY[IsSybase], [TableName]);
    Open;
  end;
  while not qry.Eof do with IndexDefs.AddIndexDef do
  begin
    Name := trim(qry.Fields[0].AsString);
    Fields := trim(qry.Fields[2].AsString);
    if qry.Fields[3].AsInteger=1 then Options := Options + [ixUnique];
    if qry.Fields[4].AsInteger=1 then Options := Options + [ixPrimary];
    qry.Next;
    while (Name = trim(qry.Fields[0].AsString)) and (not qry.Eof) do
    begin
      Fields := Fields + ';' + trim(qry.Fields[2].AsString);
      qry.Next;
    end;
  end;
  qry.Close;
  qry.Free;
end;

function TMSSQLConnection.GetSchemaInfoSQL(SchemaType: TSchemaType; SchemaObjectName, SchemaObjectPattern: string): string;
const SCHEMA_QUERY='select name as %s from sysobjects where type=''%s'' order by 1';
begin
  case SchemaType of
    stTables     : Result := format(SCHEMA_QUERY, ['table_name','U']);
    stSysTables  : Result := format(SCHEMA_QUERY, ['table_name','S']);
    stProcedures : Result := format(SCHEMA_QUERY, ['proc_name','P']);
    stColumns    : Result := 'select name as column_name from syscolumns where id=object_id(''' + SchemaObjectName + ''') order by colorder';
  else
    DatabaseError(SMetadataUnavailable)
  end;
end;


{ TMSSQLConnectionDef }

class function TMSSQLConnectionDef.TypeName: String;
begin
   Result:='MSSQLServer';
end;

class function TMSSQLConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
   Result:=TMSSQLConnection;
end;

class function TMSSQLConnectionDef.Description: String;
begin
   Result:='Connect to MS SQL Server via Microsoft client library or via FreeTDS db-lib';
end;


{ TSybaseConnectionDef }

class function TSybaseConnectionDef.TypeName: String;
begin
  Result:='Sybase';
end;

class function TSybaseConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TSybaseConnection;
end;

class function TSybaseConnectionDef.Description: String;
begin
  Result:='Connect to Sybase SQL Server via FreeTDS db-lib';;
end;


initialization
  RegisterConnection(TMSSQLConnectionDef);
  RegisterConnection(TSybaseConnectionDef);

finalization
  UnRegisterConnection(TMSSQLConnectionDef);
  UnRegisterConnection(TSybaseConnectionDef);

end.

