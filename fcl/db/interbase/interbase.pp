{
    $Id$
    Copyright (c) 2000 by Pavel Stingl


    Interbase database & dataset
    
    Roughly based on work of FPC development team,
    especially Michael Van Canneyt 

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit interbase;

{$H+}

interface 

uses SysUtils, Classes, ibase60, Db;

type

  PInteger = ^integer;

  TIBDatabase = class (TDatabase)
  private
    FIBDatabaseHandle    : pointer;
    FIBTransactionHandle : pointer;
    FPassword            : string;
    FStatus              : array [0..19] of ISC_STATUS;
    FUserName            : string;
    
    procedure CheckError(ProcName : string);
  protected
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
  public
    constructor Create(AOwner : TComponent); override;

    procedure CommitTransaction; virtual;
    procedure RollbackTransaction; virtual;
    procedure StartTransaction; override;
    procedure EndTransaction; override;

    property DatabaseHandle: pointer read FIBDatabaseHandle; 
    property TransactionHandle: pointer read FIBTransactionHandle;
  published
    property Password: string read FPassword write FPassword;
    property UserName: string read FUserName write FUserName;
    
    property Connected;
    property DatabaseName;
    property KeepConnection;
    property LoginPrompt;
    property Params;
    property OnLogin;
  end;

  PIBBookmark = ^TIBBookmark;
  TIBBookmark = record
    BookmarkData: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;
  
  // TStatementType indicates if SQL statement returns
  // result set.
  TStatementType = (stResult, stNoResult, stDDL);
  
  TIBDataset = class (TDataset)
  private
    FBufferSize          : longint;
    FCurrentRecord       : longint;
    FCurrStmtType        : TStatementType;
    FDatabase            : TIBDatabase;
    FFlag                : array [0..1024] of shortint;
    FIsEOF               : boolean;
    FLoadingFieldDefs    : boolean;
	FSQLPrepared		 : boolean;
    FRecordSize          : word;
    FRecordCount         : integer;
    FSQL                 : TStrings;
    FSQLDA               : PXSQLDA;
    FSQLDAAllocated      : longint;
    FStatementHandle     : pointer;
    FStatus              : array [0..19] of ISC_STATUS;
    
    FDBHandle            : pointer;
    FTRHandle            : pointer;
    
    procedure CheckError(ProcName : string);
    procedure DoAssignBuffers;
    procedure DoExecSQL;
    procedure DoFetch;
    procedure DoFreeBuffers;
    procedure DoParseSQL;
    procedure DoSQLDAAlloc(Count : longint);
    procedure DoStmtAlloc;
    procedure DoStmtDealloc;
    
    procedure SetBufExtended(Field : TField; CurrBuff,Buffer : pointer);
    procedure SetBufInteger(Field : TField; CurrBuff,Buffer : pointer);
    procedure SetBufDateTime(Field : TField; CurrBuff,Buffer : pointer; AType : integer);
    procedure SetBufString(Field : TField; CurrBuff,Buffer : pointer);
    
    function GetStmtType: TStatementType;
    
    function LoadBufferFromData(Buffer : PChar): TGetResult;
    procedure SetDatabase(Value : TIBDatabase);
    procedure SetSizes;
    procedure TranslateFieldType(AType, AScale: longint; 
      var XType: TFieldType; var XScale: word);
  protected
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
	function GetRecordCount: integer; override;
    procedure InternalAddRecord(Buffer: Pointer; AAppend: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property SQL : TStrings read FSQL write FSQL;
    property Database : TIBDatabase read FDatabase write SetDatabase;
  end;

implementation

type

  TTm = packed record
    tm_sec : longint;
    tm_min : longint;
    tm_hour : longint;
    tm_mday : longint;
    tm_mon : longint;
    tm_year : longint;
    tm_wday : longint;
    tm_yday : longint;
    tm_isdst : longint;
    __tm_gmtoff : longint;
    __tm_zone : Pchar;
  end;


///////////////////////////////////////////////////////////////////////
// TIBDatabase implementation
//

// PRIVATE PART of TIBDatabase

{---------------------------------------------------------------------}
{ CheckError                                                          }
{ This procedure checks IB status vector and, if found some error     }
{ condition, raises exception with IB error text                      }
{---------------------------------------------------------------------}

procedure TIBDatabase.CheckError(ProcName:string);
var
  buf : array [0..1024] of char;
  P : pointer;
  x : integer;
begin
  if ((FStatus[0] = 1) and (FStatus[1] <> 0)) then
  begin
    p := @FStatus;
    isc_interprete(Buf, @p);
    raise Exception.Create(ProcName + ': ' + StrPas(buf));
  end;
end;


// PROTECTED PART of TIBDatabase

procedure TIBDatabase.DoInternalConnect;
var
  DPB : string;
begin
  if Connected then
    Close;    
  DPB := chr(isc_dpb_version1);
  if (FUserName <> '') then
  begin
    DPB := DPB + chr(isc_dpb_user_name) + chr(Length(FUserName)) + FUserName;
    if (FPassword <> '') then
      DPB := DPB + chr(isc_dpb_password) + chr(Length(FPassword)) + FPassword;
  end;
  if (DatabaseName = '') then
    raise Exception.Create('TIBDatabase.Open: Database connect string not filled in!');
  FIBDatabaseHandle := nil;
  if isc_attach_database(@FStatus, Length(DatabaseName), @DatabaseName[1], @FIBDatabaseHandle, 
         Length(DPB), @DPB[1]) <> 0 then
    CheckError('TIBDatabase.Open');
end;

procedure TIBDatabase.DoInternalDisconnect;
begin
  if not Connected then
  begin
    FIBDatabaseHandle := nil;
    Exit;
  end;
  isc_detach_database(@FStatus[0], @FIBDatabaseHandle);
  CheckError('TIBDatabase.Close');
end;


// PUBLIC PART of TIBDatabase

constructor TIBDatabase.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIBDatabaseHandle := nil;
  FIBTransactionHandle := nil;
  FUserName := '';
  FPassword := '';
end;

procedure TIBDatabase.CommitTransaction;
begin
  if FIBTransactionHandle <> nil then
    if isc_commit_retaining(@FStatus, @FIBTransactionHandle) <> 0 then
      CheckError('TIBDatabase.CommitTransaction');
end;

procedure TIBDatabase.RollbackTransaction;
begin
  if FIBTransactionHandle <> nil then
    if isc_rollback_retaining(@FStatus, FIBTransactionHandle) <> 0 then
      CheckError('TIBDatabase.RollbackTransaction');
end;

procedure TIBDatabase.StartTransaction;
begin
  if FIBTransactionHandle = nil then
  begin
    if isc_start_transaction(@FStatus, @FIBTransactionHandle, 1, [@FIBDatabaseHandle, 0, nil]) <> 0 then
      CheckError('TIBDatabase.StartTransaction');
  end;
end;

procedure TIBDatabase.EndTransaction;
begin
  if FIBTransactionHandle <> nil then
  begin
    if isc_commit_transaction(@FStatus, @FIBTransactionHandle) <> 0 then
      CheckError('TIBDatabase.EndTransaction');
    FIBTransactionHandle := nil;
  end;
end;


///////////////////////////////////////////////////////////////////////
// TIBDataset implementation
//

// PRIVATE PART

procedure TIBDataset.CheckError(ProcName : string);
var
  buf : array [0..1024] of char;
  P : pointer;
  Msg : string;
  x : integer;
begin
  if ((FStatus[0] = 1) and (FStatus[1] <> 0)) then
  begin
    p := @FStatus;
    while isc_interprete(Buf, @p) > 0 do
      Msg := Msg + #10' -' + StrPas(Buf);
    raise Exception.Create(ProcName + ': ' + Msg);
  end;
end;

procedure TIBDataset.DoAssignBuffers;
var
  Buf : PChar;
  x   : longint;
begin
  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    Buf := AllocMem(FSQLDA^.SQLVar[x].SQLLen);
    FSQLDA^.SQLVar[x].SQLData := Buf;
    FSQLDA^.SQLVar[x].SQLInd  := @FFlag[x];
  end;
end;

procedure TIBDataset.DoExecSQL;
begin
  if isc_dsql_execute(@FStatus, @FTrHandle, @FStatementHandle, 1, nil) <> 0 then
    CheckError('TIBDataset.DoExecSQL');
end;

procedure TIBDataset.DoFetch;
var
  Res : longint;
begin
  if FCurrStmtType <> stResult then Exit;
  Res := isc_dsql_fetch(@FStatus, @FStatementHandle, 1, FSQLDA);
  if (Res <> 100) then
    CheckError('TIBDataset.DoFetch');
  FIsEOF := (Res = 100);
end;

procedure TIBDataset.DoFreeBuffers;
var
  x   : longint;
begin
  for x := 0 to FSQLDA^.SQLD - 1 do
    if (FSQLDA^.SQLVar[x].SQLData <> nil) then
      FreeMem(FSQLDA^.SQLVar[x].SQLData);
end;

procedure TIBDataset.DoParseSQL;
var
  Buf      : string;
  x        : longint;
begin
  if FSQL.Count < 1 then
    raise Exception.Create('TIBDataset.DoParseSQL: Empty SQL statement');

  Buf := '';
  for x := 0 to FSQL.Count - 1 do
    Buf := Buf + FSQL[x] + ' ';

  if isc_dsql_prepare(@FStatus, @FTrHandle, @FStatementHandle, 0, @Buf[1], 1, nil) <> 0 then    CheckError('TIBDataset.DoParseSQL - Prepare');
    
  if isc_dsql_describe(@FStatus, @FStatementHandle, 1, FSQLDA) <> 0 then
    CheckError('TIBDataset.DoParseSQL - Describe');

  if FSQLDA^.SQLN < FSQLDA^.SQLD then
  begin
    x := FSQLDA^.SQLD;
    DoSQLDAAlloc(x);
    if isc_dsql_describe(@FStatus, @FStatementHandle, 1, FSQLDA) <> 0 then
      CheckError('TIBDataset.DoParseSQL - Describe');
  end;
  
  FCurrStmtType := GetStmtType;
  FSQLPrepared := True;  
end;

procedure TIBDataset.DoSQLDAAlloc(Count : longint);
begin
  if FSQLDAAllocated > 0 then
    FreeMem(FSQLDA, XSQLDA_Length * FSQLDAAllocated);
  GetMem(FSQLDA, XSQLDA_Length * Count);
  FSQLDAAllocated := Count;
  FSQLDA^.Version := SQLDA_VERSION1;
  FSQLDA^.SQLN := Count;
end;

procedure TIBDataset.DoStmtAlloc;
begin
  if not FDatabase.Connected then
    FDatabase.Open;
  if FDatabase.TransactionHandle = nil then
    FDatabase.StartTransaction;
  FDBHandle := FDatabase.DatabaseHandle;
  FTRHandle := FDatabase.TransactionHandle;

  if isc_dsql_allocate_statement(@FStatus, @FDBHandle, @FStatementHandle) <> 0 then
    CheckError('TIBDataset.DoStmtAlloc');
end;

procedure TIBDataset.DoStmtDealloc;
begin
  if isc_dsql_free_statement(@FStatus, @FStatementHandle, DSQL_Drop) <> 0 then
    CheckError('TIBDataset.DoStmtDealloc');
  FStatementHandle := nil;
end;

function TIBDataset.GetStmtType: TStatementType;
var
  ResBuf : array [0..7] of char;
  x : integer;
  SType : integer;
begin
  x := isc_info_sql_stmt_type;
  isc_dsql_sql_info(@FStatus, @FStatementHandle, SizeOf(x),
    @x, SizeOf(ResBuf), @ResBuf);
  if Ord(ResBuf[0]) = isc_info_sql_stmt_type then
  begin
    x := isc_vax_integer(@ResBuf[1], 2);
    SType := isc_vax_integer(@ResBuf[3], x);
  end;
  case SType of
    isc_info_sql_stmt_select:
      Result := stResult;
    isc_info_sql_stmt_insert, isc_info_sql_stmt_update,
    isc_info_sql_stmt_delete:
      Result := stNoResult;
    else Result := stDDL;
  end;
end;

function TIBDataset.LoadBufferFromData(Buffer : PChar): TGetResult;
var
  x : integer;
  p : word;
  T : TISC_TIMESTAMP;
begin
  DoFetch;
  if FIsEOF then
    Result := grEOF
  else begin
    for x := 0 to FSQLDA^.SQLD - 1 do
    begin
      if (FSQLDA^.SQLVar[x].SQLType = SQL_VARYING) or
         (FSQLDA^.SQLVar[x].SQLType = SQL_VARYING + 1) then
      begin
        Move(FSQLDA^.SQLVar[x].SQLData^, P, 2);
    	Move((FSQLDA^.SQLVar[x].SQLData + 2)^, Buffer^, P);
        PChar(Buffer+P)^ := #0;
      end
	  else
    	Move(FSQLDA^.SQLVar[x].SQLData^, Buffer^, FSQLDA^.SQLVar[x].SQLLen);
      Inc(Buffer,FSQLDA^.SQLVar[x].SQLLen);
    end;
    Result := grOK;
  end;
end;

procedure TIBDataset.SetDatabase(Value : TIBDatabase);
begin
  CheckInactive;
  If Value<>FDatabase then
  begin
    if Value<>Nil Then
      FDatabase:=Value; 
  end;
end;

procedure TIBDataset.SetSizes;
var
  x : integer;
begin
  FRecordSize := 0;
  FBufferSize := 0;
  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    Inc(FRecordSize, FSQLDA^.SQLVar[x].SQLLen);
  end;
  FBufferSize := FRecordSize + SizeOf(TIBBookmark);
end;

procedure TIBDataset.TranslateFieldType(AType, AScale: longint; 
  var XType: TFieldType; var XScale: word);
begin
  case AType of
    SQL_TEXT, SQL_VARYING, SQL_TEXT+1, SQL_VARYING+1:
      begin
        XType := ftString;
        XScale := AScale;
      end;
    SQL_DOUBLE, SQL_DOUBLE+1: 
      begin
        XType := ftFloat;
        XScale := AScale;
      end;
    SQL_LONG, SQL_LONG+1, SQL_SHORT, SQL_SHORT+1: 
      begin
        XType := ftInteger;
        XScale := AScale;
      end;
{    SQL_DATE, SQL_DATE+1, SQL_TIME, SQL_TIME+1,}
    SQL_TYPE_TIME:
      begin
        XType := ftTime;
        XScale := AScale;
      end;
    SQL_TYPE_DATE:
      begin
        XType := ftDate;
        XScale := AScale;
      end;
    SQL_FLOAT,SQL_FLOAT+1:
      begin
        XType := ftFloat;
        XScale := AScale;
      end;
    SQL_TIMESTAMP, SQL_TIMESTAMP+1: 
      begin
        XType := ftDateTime;
        XScale := AScale;
      end;
  end;
end;


// PROTECTED PART

function TIBDataset.AllocRecordBuffer: PChar;
begin
  Result := AllocMem(FBufferSize);
end;

procedure TIBDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;

procedure TIBDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PIBBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TIBDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PIBBookmark(Buffer + FRecordSize)^.BookmarkFlag;
end;

procedure TIBDataset.SetBufExtended(Field : TField; CurrBuff,Buffer : pointer);
var
  E    : extended;
  D    : double;
  S    : single;
begin
  case Field.Size of
    4    : 
      begin
        Move(CurrBuff^,S,4);
        E := S;
      end;
    8    :
      begin
        Move(CurrBuff^,D,8);
        E := D;
      end;
    10   : Move(CurrBuff^,E,10);
  end;
  Move(E, Buffer^, 10);
end;

procedure TIBDataset.SetBufInteger(Field : TField; CurrBuff,Buffer : pointer);
var
  I    : integer;
begin
  I := 0;
  Move(I, Buffer^, SizeOf(Integer));
  Move(CurrBuff^, Buffer^, Field.Size);
end;

procedure TIBDataset.SetBufDateTime(Field : TField; CurrBuff,Buffer : pointer; AType : integer);
var
  D    : TDateTime;
  S    : TSystemTime;
  TM   : TTm;
  TT   : TIsc_timestamp;
begin
  case AType of
    SQL_TYPE_DATE: 
      isc_decode_sql_date(PISC_DATE(CurrBuff), @TM);
    SQL_TYPE_TIME:
      isc_decode_sql_time(PISC_TIME(CurrBuff), @TM);
    SQL_TIMESTAMP, SQL_TIMESTAMP+1:
      isc_decode_timestamp(PISC_TIMESTAMP(CurrBuff), @TM);
  end;
  S.Year := TM.tm_year + 1900;
  S.Month := TM.tm_mon + 1;
  S.Day := TM.tm_mday;
  S.Hour := TM.tm_hour;
  S.Minute := TM.tm_min;
  S.Second := TM.tm_sec;
  S.Millisecond := 0;
  D := SystemTimeToDateTime(S);
  {$warning !!! D is okay, but Field.AsDateTime returns wrong value !!! } 
//  WriteLn(DateTimeToStr(D));
  Move(D, Buffer^, SizeOf(D));
end;

procedure TIBDataset.SetBufString(Field : TField; CurrBuff,Buffer : pointer);
begin
  Move(CurrBuff^, Buffer^, Field.Size);
  PChar(Buffer + Field.Size)^ := #0;
end;

function TIBDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  x        : longint;
  CurrBuff : PChar;
begin
  Result := False;
  CurrBuff := ActiveBuffer;
  for x := 0 to FSQLDA^.SQLD - 1 do
  begin
    if (Field.FieldName = FSQLDA^.SQLVar[x].SQLName) then
    begin

      case Field.DataType of
        ftFloat:  
          SetBufExtended(Field, CurrBuff, Buffer);
        ftString: 
          SetBufString(Field, CurrBuff, Buffer);
        ftDate,ftTime,ftDateTime:
          SetBufDateTime(Field, CurrBuff, Buffer, FSQLDA^.SQLVar[x].SQLType);
        ftInteger:
          SetBufInteger(Field, CurrBuff, Buffer);
      end;

      Result := True;

      break; 
    end
    else Inc(CurrBuff, FSQLDA^.SQLVar[x].SQLLen);
  end;
end;

function TIBDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if FCurrStmtType <> stResult then Exit;
  if FIsEOF then 
    Result := grEOF
  else begin
	Result := grOk;
    case GetMode of
	  gmPrior: 
		if FCurrentRecord <= 0 then
		begin
		  Result := grBOF;
		  FCurrentRecord := -1;
		end
		else Dec(FCurrentRecord);
	  gmCurrent:
		if (FCurrentRecord < 0) or (FCurrentRecord >= RecordCount) then
		  Result := grError;
      gmNext: 
		if FCurrentRecord >= (RecordCount - 1) then
        begin
		  Result := LoadBufferFromData(Buffer);
          if Result = grOk then 
          begin
            Inc(FCurrentRecord);
            Inc(FRecordCount);
          end;
        end
		else Inc(FCurrentRecord);
    end;

    if Result = grOK then
    begin
      with PIBBookmark(Buffer + FRecordSize)^ do
      begin
        BookmarkData := FCurrentRecord;
        BookmarkFlag := bfCurrent;
      end;               
    end
    else if (Result = grError) {and (DoCheck)} then
      DatabaseError('No record');
  end;
end;

function TIBDataset.GetRecordCount: integer;
begin
  Result := FRecordCount;
end;

function TIBDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

procedure TIBDataset.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
end;

procedure TIBDataset.InternalClose;
begin
  DoFreeBuffers;
  DoStmtDealloc;
  if DefaultFields then
    DestroyFields;
  FIsEOF := False;
  FCurrentRecord := -1;
  FBufferSize := 0;
  FRecordSize := 0;
  FRecordCount := 0;
//  DoSQLDAAlloc(50);
end;

procedure TIBDataset.InternalDelete;
begin
end;

procedure TIBDataset.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TIBDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecord := PInteger(ABookmark)^;
end;

procedure TIBDataset.InternalHandleException;
begin
  // not implemented
end;

procedure TIBDataset.InternalInitFieldDefs;
var
  x       : longint;
  TransFt : TFieldType;
  TransSz : word;
begin
  if FLoadingFieldDefs then 
  begin
    WriteLn('Loading FieldDefs...');
    Exit;
  end;
  
  FLoadingFieldDefs := True;
  
  try
    try
      FieldDefs.Clear;
      for x := 0 to FSQLDA^.SQLD - 1 do
      begin
        TranslateFieldType(FSQLDA^.SQLVar[x].SQLType, FSQLDA^.SQLVar[x].SQLLen,
          TransFt, TransSz);
        TFieldDef.Create(FieldDefs,
          FSQLDA^.SQLVar[x].SQLName, 
          TransFt, TransSz, False, (x+1));
      end;
    finally
    end;
  finally
    FLoadingFieldDefs := False;
  end;
end;

procedure TIBDataset.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FBufferSize, #0);
end;

procedure TIBDataset.InternalLast;
begin
  FCurrentRecord := RecordCount;
end;

procedure TIBDataset.InternalOpen;
begin
  try
    DoStmtAlloc;
    DoParseSQL;
    if FCurrStmtType = stResult then
    begin
      DoAssignBuffers;
      DoExecSQL;
      InternalInitFieldDefs;
      if DefaultFields then
        CreateFields;
      SetSizes;
      BindFields(True);
    end
    else DoExecSQL;
  except
	raise;
  end;
  
end;

procedure TIBDataset.InternalPost;
begin
end;

procedure TIBDataset.InternalSetToRecord(Buffer: PChar);
begin
  FCurrentRecord := PIBBookmark(Buffer + FRecordSize)^.BookmarkData;
end;

function TIBDataset.IsCursorOpen: Boolean;
begin
  Result := FStatementHandle <> nil; //??
end;

procedure TIBDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PIBBookmark(Buffer + FRecordSize)^.BookmarkFlag := Value;
end;

procedure TIBDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PIBBookmark(Buffer + FRecordSize)^.BookmarkData := PInteger(Data)^;
end;

procedure TIBDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

// PUBLIC PART

constructor TIBDataset.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FIsEOF := False;
  FCurrentRecord := -1;
  FBufferSize := 0;
  FRecordSize := 0;
  FRecordCount := 0;
  DoSQLDAAlloc(50);
end;

destructor TIBDataset.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
  FreeMem(FSQLDA, XSQLDA_Length * FSQLDAAllocated);
end;

end.

{
  $Log$
  Revision 1.2  2000-07-13 11:32:57  michael
  + removed logs
 
}
