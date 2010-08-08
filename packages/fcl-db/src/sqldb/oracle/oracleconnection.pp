unit OracleConnection;

{$mode objfpc}{$H+}

{$Define LinkDynamically}

interface

uses
  Classes, SysUtils, sqldb,db,dbconst,
{$IfDef LinkDynamically}
  ocidyn,
{$ELSE}
  oci,
{$ENDIF}
  oratypes;

const
  DefaultTimeOut = 60;

type
  EOraDatabaseError = class(EDatabaseError)
    public
      ORAErrorCode : Longint;
  end;

  TOracleTrans = Class(TSQLHandle)
  protected
    FOciSvcCtx  : POCISvcCtx;
    FOciTrans   : POCITrans;
    FOciFlags   : ub4;
  public
    destructor Destroy(); override;
  end;
  
  TOraFieldBuf = record
    Buffer : pointer;
    Ind    : sb2;
  end;

  TOracleCursor = Class(TSQLCursor)
    protected
      FOciStmt     : POCIStmt;
      FieldBuffers : array of TOraFieldBuf;
      ParamBuffers : array of TOraFieldBuf;
    end;

  { TOracleConnection }

  TOracleConnection = class (TSQLConnection)
  private
    FOciEnvironment : POciEnv;
    FOciError       : POCIError;
    FOciServer      : POCIServer;
    FOciUserSession : POCISession;
    FUserMem        : pointer;
    procedure HandleError;
    procedure SetParameters(cursor : TSQLCursor;AParams : TParams);
  protected
    // - Connect/disconnect
    procedure DoInternalConnect; override;
    procedure DoInternalDisconnect; override;
    // - Handle (de)allocation
    function AllocateCursorHandle:TSQLCursor; override;
    procedure DeAllocateCursorHandle(var cursor:TSQLCursor); override;
    function AllocateTransactionHandle:TSQLHandle; override;
    // - Statement handling
    procedure PrepareStatement(cursor:TSQLCursor; ATransaction:TSQLTransaction; buf:string; AParams:TParams); override;
    procedure UnPrepareStatement(cursor:TSQLCursor); override;
    // - Transaction handling
    procedure InternalStartDBTransaction(trans:TOracleTrans);
    function GetTransactionHandle(trans:TSQLHandle):pointer; override;
    function StartDBTransaction(trans:TSQLHandle; AParams:string):boolean; override;
    function Commit(trans:TSQLHandle):boolean; override;
    function Rollback(trans:TSQLHandle):boolean; override;
    procedure CommitRetaining(trans:TSQLHandle); override;
    procedure RollbackRetaining(trans:TSQLHandle); override;
    // - Statement execution
    procedure Execute(cursor:TSQLCursor; ATransaction:TSQLTransaction; AParams:TParams); override;
    // - Result retrieving
    procedure AddFieldDefs(cursor:TSQLCursor; FieldDefs:TFieldDefs); override;
    function Fetch(cursor:TSQLCursor):boolean; override;
    function LoadField(cursor:TSQLCursor; FieldDef:TFieldDef; buffer:pointer; out CreateBlob : boolean):boolean; override;
//    function CreateBlobStream(Field:TField; Mode:TBlobStreamMode):TStream; override;
    procedure FreeFldBuffers(cursor:TSQLCursor); override;

  public
    constructor Create(AOwner : TComponent); override;
  end;

  TOracleConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
  end;

implementation

uses
  math, StrUtils;

ResourceString
  SErrEnvCreateFailed = 'The creation of an Oracle environment failed.';
  SErrHandleAllocFailed = 'The allocation of the error handle failed.';
  SErrOracle = 'Oracle returned error %s:';

procedure TOracleConnection.HandleError;

var errcode : sb4;
    buf     : array[0..1023] of char;
    E       : EOraDatabaseError;

begin
  OCIErrorGet(FOciError,1,nil,errcode,@buf[0],1024,OCI_HTYPE_ERROR);

  if (Self.Name <> '') then
    E := EOraDatabaseError.CreateFmt('%s : %s',[Self.Name,pchar(buf)])
  else
    E := EOraDatabaseError.Create(pchar(buf));

  E.ORAErrorCode := errcode;
  Raise E;
end;

procedure TOracleConnection.DoInternalConnect;

var
  ConnectString : string;
  TempServiceContext : POCISvcCtx;

begin
{$IfDef LinkDynamically}
  InitialiseOCI;
{$EndIf}

  inherited DoInternalConnect;
  //todo: get rid of FUserMem, as it isn't used
  FUserMem := nil;

  // Create environment handle
  if OCIEnvCreate(FOciEnvironment,oci_default,nil,nil,nil,nil,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrEnvCreateFailed,self);
  // Create error handle
  if OciHandleAlloc(FOciEnvironment,FOciError,OCI_HTYPE_ERROR,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);
  // Create Server handle
  if OciHandleAlloc(FOciEnvironment,FOciServer,OCI_HTYPE_SERVER,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);
  // Initialize Server handle
  if hostname='' then connectstring := databasename
  else connectstring := '//'+hostname+'/'+databasename;
  if OCIServerAttach(FOciServer,FOciError,@(ConnectString[1]),Length(ConnectString),OCI_DEFAULT) <> OCI_SUCCESS then
    HandleError();

  // Create temporary service-context handle for user-authentication
  if OciHandleAlloc(FOciEnvironment,TempServiceContext,OCI_HTYPE_SVCCTX,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);

  // Create user-session handle
  if OciHandleAlloc(FOciEnvironment,FOciUserSession,OCI_HTYPE_SESSION,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);
  // Set the server-handle in the service-context handle
  if OCIAttrSet(TempServiceContext,OCI_HTYPE_SVCCTX,FOciServer,0,OCI_ATTR_SERVER,FOciError) <> OCI_SUCCESS then
    HandleError();
  // Set username and password in the user-session handle
  if OCIAttrSet(FOciUserSession,OCI_HTYPE_SESSION,@(Self.UserName[1]),Length(Self.UserName),OCI_ATTR_USERNAME,FOciError) <> OCI_SUCCESS then
    HandleError();
  if OCIAttrSet(FOciUserSession,OCI_HTYPE_SESSION,@(Self.Password[1]),Length(Self.Password),OCI_ATTR_PASSWORD,FOciError) <> OCI_SUCCESS then
    HandleError();
  // Authenticate
  if OCISessionBegin(TempServiceContext,FOciError,FOcIUserSession,OCI_CRED_RDBMS,OCI_DEFAULT) <> OCI_SUCCESS then
    HandleError();
  // Free temporary service-context handle
  OCIHandleFree(TempServiceContext,OCI_HTYPE_SVCCTX);
end;

procedure TOracleConnection.DoInternalDisconnect;
var
  TempServiceContext : POCISvcCtx;
begin
  inherited DoInternalDisconnect;

  // Create temporary service-context handle for user-disconnect
  if OciHandleAlloc(FOciEnvironment,TempServiceContext,OCI_HTYPE_SVCCTX,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);

  // Set the server handle in the service-context handle
  if OCIAttrSet(TempServiceContext,OCI_HTYPE_SVCCTX,FOciServer,0,OCI_ATTR_SERVER,FOciError) <> OCI_SUCCESS then
    HandleError();
  // Set the user session handle in the service-context handle
  if OCIAttrSet(TempServiceContext,OCI_HTYPE_SVCCTX,FOciUserSession,0,OCI_ATTR_SESSION,FOciError) <> OCI_SUCCESS then
    HandleError();
  // Disconnect uses-session handle
  if OCISessionEnd(TempServiceContext,FOciError,FOcIUserSession,OCI_DEFAULT) <> OCI_SUCCESS then
    HandleError();
  // Free user-session handle
  OCIHandleFree(FOciUserSession,OCI_HTYPE_SESSION);
  // Free temporary service-context handle
  OCIHandleFree(TempServiceContext,OCI_HTYPE_SVCCTX);

  // Disconnect server handle
  if OCIServerDetach(FOciServer,FOciError,OCI_DEFAULT) <> OCI_SUCCESS then
    HandleError();

  // Free connection handles
  OCIHandleFree(FOciServer,OCI_HTYPE_SERVER);
  OCIHandleFree(FOciError,OCI_HTYPE_ERROR);
  OCIHandleFree(FOciEnvironment,OCI_HTYPE_ENV);
{$IfDef LinkDynamically}
  ReleaseOCI;
{$EndIf}

end;

function TOracleConnection.AllocateCursorHandle: TSQLCursor;

var Cursor : TOracleCursor;

begin
  Cursor:=TOracleCursor.Create;
  Result := cursor;
end;

procedure TOracleConnection.DeAllocateCursorHandle(var cursor: TSQLCursor);

var tel : word;

begin
  with cursor as TOracleCursor do
    begin
    if Length(FieldBuffers) > 0 then
      for tel := 0 to high(FieldBuffers) do freemem(FieldBuffers[tel].buffer);
    end;
  FreeAndNil(cursor);
end;

function TOracleConnection.AllocateTransactionHandle: TSQLHandle;
var
  locRes : TOracleTrans;
begin
  locRes := TOracleTrans.Create();
  try
    // Allocate service-context handle
    if OciHandleAlloc(FOciEnvironment,locRes.FOciSvcCtx,OCI_HTYPE_SVCCTX,0,FUserMem) <> OCI_SUCCESS then
      DatabaseError(SErrHandleAllocFailed,self);
    // Set the server-handle in the service-context handle
    if OCIAttrSet(locRes.FOciSvcCtx,OCI_HTYPE_SVCCTX,FOciServer,0,OCI_ATTR_SERVER,FOciError) <> OCI_SUCCESS then
      HandleError();
    // Set the user-session handle in the service-context handle
    if OCIAttrSet(locRes.FOciSvcCtx,OCI_HTYPE_SVCCTX,FOciUserSession,0,OCI_ATTR_SESSION,FOciError) <> OCI_SUCCESS then
      HandleError();

    // Allocate transaction handle
    if OciHandleAlloc(FOciEnvironment,locRes.FOciTrans,OCI_HTYPE_TRANS,0,FUserMem) <> OCI_SUCCESS then
      DatabaseError(SErrHandleAllocFailed,self);
    // Set the transaction handle in the service-context handle
    if OCIAttrSet(locRes.FOciSvcCtx,OCI_HTYPE_SVCCTX,locRes.FOciTrans,0,OCI_ATTR_TRANS,FOciError) <> OCI_SUCCESS then
      HandleError();
  except
    locRes.Free();
    raise;
  end;
  Result := locRes;
end;

procedure TOracleConnection.PrepareStatement(cursor: TSQLCursor;
  ATransaction: TSQLTransaction; buf: string; AParams: TParams);
  
var tel      : integer;
    FOcibind : POCIDefine;
    
    OFieldType   : ub2;
    OFieldSize   : sb4;

begin
  with cursor as TOracleCursor do
    begin
    OciHandleAlloc(FOciEnvironment,FOciStmt,OCI_HTYPE_STMT,0,FUserMem);
    if OCIStmtPrepare2(TOracleTrans(ATransaction.Handle).FOciSvcCtx,FOciStmt,FOciError,@buf[1],length(buf),nil,0,OCI_NTV_SYNTAX,OCI_DEFAULT) = OCI_ERROR then
      HandleError;
    if assigned(AParams) then
      begin
      setlength(ParamBuffers,AParams.Count);
      for tel := 0 to AParams.Count-1 do
        begin

        case AParams[tel].DataType of
          ftInteger : begin OFieldType := SQLT_INT; OFieldSize := sizeof(integer); end;
          ftFloat : begin OFieldType := SQLT_FLT; OFieldSize := sizeof(double); end;
          ftDate, ftDateTime : begin OFieldType := SQLT_DAT; OFieldSize := 7; end;
          ftString  : begin OFieldType := SQLT_STR; OFieldSize := 4000; end;

        end;
        parambuffers[tel].buffer := getmem(OFieldSize);


        FOciBind := nil;

        if OCIBindByName(FOciStmt,FOcibind,FOciError,pchar(AParams[tel].Name),length(AParams[tel].Name),ParamBuffers[tel].buffer,OFieldSize,OFieldType,@ParamBuffers[tel].ind,nil,nil,0,nil,OCI_DEFAULT )= OCI_ERROR then
          HandleError;

        end;
      end;
    FPrepared := True;
    end;
end;

procedure TOracleConnection.SetParameters(cursor : TSQLCursor;AParams : TParams);

var SQLVarNr       : integer;
    i              : integer;
    f              : double;
    year,month,day : word;
    db             : array[0..4] of byte;
    pb             : pbyte;
    s              : string;

begin
  with cursor as TOracleCursor do for SQLVarNr := 0 to High(ParamBuffers) do with AParams[SQLVarNr] do
    begin
    if IsNull then parambuffers[SQLVarNr].ind := -1 else
      parambuffers[SQLVarNr].ind := 0;

    case DataType of
      ftInteger         : begin
                          i := asInteger;
                          move(i,parambuffers[SQLVarNr].buffer^,sizeof(integer));
                          end;
      ftFloat           : begin
                          f := asFloat;
                          move(f,parambuffers[SQLVarNr].buffer^,sizeof(double));
                          end;
      ftString          : begin
                          s := asString+#0;
                          move(s[1],parambuffers[SQLVarNr].buffer^,length(s)+1);
                          end;
      ftDate, ftDateTime: begin
                          DecodeDate(asDateTime,year,month,day);
                          pb := parambuffers[SQLVarNr].buffer;
                          pb[0] := (year div 100)+100;
                          pb[1] := (year mod 100)+100;
                          pb[2] := month;
                          pb[3] := day;
                          pb[4] := 1;
                          pb[5] := 1;
                          pb[6] := 1;
                          end;
    end;

    end;

end;

procedure TOracleConnection.UnPrepareStatement(cursor: TSQLCursor);
begin
  OCIHandleFree(TOracleCursor(cursor).FOciStmt,OCI_HTYPE_STMT);
  cursor.FPrepared:=False;
end;

procedure TOracleConnection.InternalStartDBTransaction(trans : TOracleTrans);
begin
  if OCITransStart(trans.FOciSvcCtx,FOciError,DefaultTimeOut,trans.FOciFlags) <> OCI_SUCCESS then
    HandleError();
end;

function TOracleConnection.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
  Result := trans;
end;

function TOracleConnection.StartDBTransaction(trans: TSQLHandle; AParams: string): boolean;
var
  x_flags : ub4;
  i : Integer;
  s : string;
  locTrans : TOracleTrans;
begin
  locTrans := TOracleTrans(trans);
  if ( Length(AParams) = 0 ) then begin
    x_flags := OCI_TRANS_NEW or OCI_TRANS_READWRITE;
  end else begin
    x_flags := OCI_DEFAULT;
    i := 1;
    s := ExtractSubStr(AParams,i,StdWordDelims);
    while ( s <> '' ) do begin
      if ( s = 'readonly' ) then
        x_flags := x_flags and OCI_TRANS_READONLY
      else if ( s = 'serializable' ) then
        x_flags := x_flags and OCI_TRANS_SERIALIZABLE
      else if ( s = 'readwrite' ) then
        x_flags := x_flags and OCI_TRANS_READWRITE;
      s := ExtractSubStr(AParams,i,StdWordDelims);
    end;
    x_flags := x_flags and OCI_TRANS_NEW;
  end;
  locTrans.FOciFlags := x_flags;
  InternalStartDBTransaction(locTrans);
  Result := True;
end;

function TOracleConnection.Commit(trans: TSQLHandle): boolean;
begin
  if OCITransCommit(TOracleTrans(trans).FOciSvcCtx,FOciError,OCI_DEFAULT) <> OCI_SUCCESS then
    HandleError();
  Result := True;
end;

function TOracleConnection.Rollback(trans: TSQLHandle): boolean;
begin
  if OCITransRollback(TOracleTrans(trans).FOciSvcCtx,FOciError,OCI_DEFAULT) <> OCI_SUCCESS then
    HandleError();
  Result := True;
end;

procedure TOracleConnection.CommitRetaining(trans: TSQLHandle);
begin
  Commit(trans);
  InternalStartDBTransaction(TOracleTrans(trans));
end;

procedure TOracleConnection.RollbackRetaining(trans: TSQLHandle);
begin
  Rollback(trans);
  InternalStartDBTransaction(TOracleTrans(trans));
end;

procedure TOracleConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
begin
  if Assigned(APArams) and (AParams.count > 0) then SetParameters(cursor, AParams);
  if cursor.FStatementType = stSelect then
    begin
    if OCIStmtExecute(TOracleTrans(ATransaction.Handle).FOciSvcCtx,(cursor as TOracleCursor).FOciStmt,FOciError,0,0,nil,nil,OCI_DEFAULT) = OCI_ERROR then
      HandleError;
    end
  else
    begin
    if OCIStmtExecute(TOracleTrans(ATransaction.Handle).FOciSvcCtx,(cursor as TOracleCursor).FOciStmt,FOciError,1,0,nil,nil,OCI_DEFAULT) = OCI_ERROR then
      HandleError;
    end;
end;

procedure TOracleConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);

var Param      : POCIParam;
    tel        : ub4;

    FieldType  : TFieldType;
    FieldName  : string;
    FieldSize  : word;

    OFieldType   : ub2;
    OFieldName   : Pchar;
    OFieldSize   : sb4;
    OFNameLength : ub4;
    NumCols      : ub4;
    FOciDefine   : POCIDefine;
    OPrecision   : sb2;
    OScale       : sb1;

begin
  Param := nil;
  with cursor as TOracleCursor do
    begin
    if OCIAttrGet(FOciStmt,OCI_HTYPE_STMT,@numcols,nil,OCI_ATTR_PARAM_COUNT,FOciError) = OCI_ERROR then
      HandleError;

    // Let op, moet gewist worden. En in een keer gealloceerd
    Setlength(FieldBuffers,numcols);

    for tel := 1 to numcols do
      begin
      if OCIParamGet(FOciStmt,OCI_HTYPE_STMT,FOciError,Param,tel) = OCI_ERROR then
        HandleError;

      if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldType,nil,OCI_ATTR_DATA_TYPE,FOciError) = OCI_ERROR then
        HandleError;

      if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldSize,nil,OCI_ATTR_DATA_SIZE,FOciError) = OCI_ERROR then
        HandleError;

      FieldSize := 0;
      
      case OFieldType of
        OCI_TYPECODE_NUMBER   : begin
                                if OCIAttrGet(Param,OCI_DTYPE_PARAM,@Oprecision,nil,OCI_ATTR_PRECISION,FOciError) = OCI_ERROR then
                                  HandleError;
                                if OCIAttrGet(Param,OCI_DTYPE_PARAM,@Oscale,nil,OCI_ATTR_SCALE,FOciError) = OCI_ERROR then
                                  HandleError;

                                if Oscale = 0 then
                                  begin
                                  FieldType := ftInteger;
                                  OFieldType := SQLT_INT;
                                  OFieldSize:= sizeof(integer);
                                  end
                                else if (oscale = -127) {and (OPrecision=0)} then
                                  begin
                                  FieldType := ftFloat;
                                  OFieldType := SQLT_FLT;
                                  OFieldSize:=sizeof(double);
                                  end
                                else if (oscale <=4) and (OPrecision<=12) then
                                  begin
                                  FieldType := ftBCD;
                                  FieldSize := oscale;
                                  OFieldType := SQLT_VNU;
                                  OFieldSize:= 22;
                                  end
                                else FieldType := ftUnknown;
                                end;
        OCI_TYPECODE_CHAR,
        OCI_TYPECODE_VARCHAR,
        OCI_TYPECODE_VARCHAR2 : begin FieldType := ftString; FieldSize := OFieldSize; inc(OFieldsize) ;OFieldType:=SQLT_STR end;
        OCI_TYPECODE_DATE     : FieldType := ftDate;
        OCI_TYPECODE_TIMESTAMP,
        OCI_TYPECODE_TIMESTAMP_LTZ,
        OCI_TYPECODE_TIMESTAMP_TZ  : begin
                                     FieldType := ftDateTime;
                                     OFieldType := SQLT_ODT;
                                     end;
      else
        FieldType := ftUnknown;
      end;

      FieldBuffers[tel-1].buffer := getmem(OFieldSize);

      FOciDefine := nil;
      if OciDefineByPos(FOciStmt,FOciDefine,FOciError,tel,fieldbuffers[tel-1].buffer,OFieldSize,OFieldType,@(fieldbuffers[tel-1].ind),nil,nil,OCI_DEFAULT) = OCI_ERROR then
        HandleError;

      if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldName,@OFNameLength,OCI_ATTR_NAME,FOciError) <> OCI_SUCCESS then
        HandleError;

      setlength(Fieldname,OFNameLength);
      move(OFieldName^,Fieldname[1],OFNameLength);

      TFieldDef.Create(FieldDefs, FieldDefs.MakeNameUnique(FieldName), FieldType, FieldSize, False, tel);
      end;
  end;
end;

function TOracleConnection.Fetch(cursor: TSQLCursor): boolean;
begin
  case OCIStmtFetch2((cursor as TOracleCursor).FOciStmt,FOciError,1,OCI_FETCH_NEXT,1,OCI_DEFAULT) of
    OCI_ERROR   : begin
                  Result := False;
                  HandleError;
                  end;
    OCI_NO_DATA : Result := False;
    OCI_SUCCESS : Result := True;
    OCI_SUCCESS_WITH_INFO : Begin
                            Result := True;
                            HandleError;
                            end;
  end; {case}
end;

function TOracleConnection.LoadField(cursor: TSQLCursor; FieldDef: TFieldDef; buffer: pointer; out CreateBlob : boolean): boolean;

var dt        : TDateTime;
    b         : pbyte;
    size,i    :  byte;
    exp       : shortint;
    cur       : Currency;
    odt       : POCIdateTime;

begin
  CreateBlob := False;
  with cursor as TOracleCursor do if fieldbuffers[FieldDef.FieldNo-1].ind = -1 then
    Result := False
  else
    begin
    result := True;
    case FieldDef.DataType of
      ftString          : move(fieldbuffers[FieldDef.FieldNo-1].buffer^,buffer^,FieldDef.Size);
      ftBCD             :  begin
                           b := fieldbuffers[FieldDef.FieldNo-1].buffer;
                           size := b[0];
                           cur := 0;
                           if (b[1] and $80)=$80 then // then the number is positive
                             begin
                             exp := (b[1] and $7f)-65;
                             for i := 2 to size do
                               cur := cur + (b[i]-1) * intpower(100,-(i-2)+exp);
                             end
                           else
                             begin
                             exp := (not(b[1]) and $7f)-65;
                             for i := 2 to size-1 do
                               cur := cur + (101-b[i]) * intpower(100,-(i-2)+exp);
                             cur := -cur;
                             end;
                           move(cur,buffer^,SizeOf(Currency));
                           end;
      ftFloat           : move(fieldbuffers[FieldDef.FieldNo-1].buffer^,buffer^,sizeof(double));
      ftInteger         : move(fieldbuffers[FieldDef.FieldNo-1].buffer^,buffer^,sizeof(integer));
      ftDate  : begin
                b := fieldbuffers[FieldDef.FieldNo-1].buffer;
                dt := EncodeDate((b[0]-100)*100+(b[1]-100),b[2],b[3]);
                move(dt,buffer^,sizeof(dt));
                end;
      ftDateTime : begin
                   odt := fieldbuffers[FieldDef.FieldNo-1].buffer;
                   dt := ComposeDateTime(EncodeDate(odt^.year,odt^.month,odt^.day), EncodeTime(odt^.hour,odt^.min,odt^.sec,0));
                   move(dt,buffer^,sizeof(dt));
                   end;
    else
      Result := False;

    end;
    end;
end;

{function TOracleConnection.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
//  Result:=inherited CreateBlobStream(Field, Mode);
end;}

procedure TOracleConnection.FreeFldBuffers(cursor: TSQLCursor);
begin
//  inherited FreeFldBuffers(cursor);
end;

constructor TOracleConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnOptions := FConnOptions + [sqEscapeRepeat];
  FUserMem := nil;
end;

{ TOracleConnectionDef }

class function TOracleConnectionDef.TypeName: String;
begin
  Result:='Oracle';
end;

class function TOracleConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result:=TOracleConnection;
end;

class function TOracleConnectionDef.Description: String;
begin
  Result:='Connect to an Oracle database directly via the client library';
end;

{ TOracleTrans }

destructor TOracleTrans.Destroy();
begin
  OCIHandleFree(FOciTrans,OCI_HTYPE_TRANS);
  OCIHandleFree(FOciSvcCtx,OCI_HTYPE_SVCCTX);
  inherited Destroy();
end;

initialization
  RegisterConnection(TOracleConnectionDef);
finalization
  RegisterConnection(TOracleConnectionDef);
end.

