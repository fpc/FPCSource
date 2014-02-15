unit oracleconnection;

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
    Len    : ub4;
    Size   : ub4;
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
    procedure GetParameters(cursor : TSQLCursor;AParams : TParams);
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
    function RowsAffected(cursor: TSQLCursor): TRowsCount; override;
    // - Result retrieval
    procedure AddFieldDefs(cursor:TSQLCursor; FieldDefs:TFieldDefs); override;
    function Fetch(cursor:TSQLCursor):boolean; override;
    function LoadField(cursor:TSQLCursor; FieldDef:TFieldDef; buffer:pointer; out CreateBlob : boolean):boolean; override;
//    function CreateBlobStream(Field:TField; Mode:TBlobStreamMode):TStream; override;
    procedure FreeFldBuffers(cursor:TSQLCursor); override;
    procedure UpdateIndexDefs(IndexDefs : TIndexDefs;TableName : string); override;
    function GetSchemaInfoSQL(SchemaType : TSchemaType; SchemaObjectName, SchemaPattern : string) : string; override;

  public
    constructor Create(AOwner : TComponent); override;
  end;

  { TOracleConnectionDef }

  TOracleConnectionDef = Class(TConnectionDef)
    Class Function TypeName : String; override;
    Class Function ConnectionClass : TSQLConnectionClass; override;
    Class Function Description : String; override;
    Class Function DefaultLibraryName : String; override;
    Class Function LoadFunction : TLibraryLoadFunction; override;
    Class Function UnLoadFunction : TLibraryUnLoadFunction; override;
    Class Function LoadedLibraryName: string; override;
  end;

implementation

uses
  math, StrUtils, FmtBCD;

ResourceString
  SErrEnvCreateFailed = 'The creation of an Oracle environment failed.';
  SErrHandleAllocFailed = 'The allocation of the error handle failed.';
  SErrOracle = 'Oracle returned error %s:';

//callback functions

function cbf_no_data(ictxp:Pdvoid; bindp:POCIBind; iter:ub4; index:ub4; bufpp:PPdvoid;
             alenp:Pub4; piecep:Pub1; indp:PPdvoid):sb4;cdecl;

begin
  bufpp^ := nil;
  alenp^ := 0;
  indp^ := nil;
  piecep^ := OCI_ONE_PIECE;
  result:=OCI_CONTINUE;
end;


function cbf_get_data(octxp:Pdvoid; bindp:POCIBind; iter:ub4; index:ub4; bufpp:PPdvoid;
             alenp:PPub4; piecep:Pub1; indp:PPdvoid; rcodep:PPub2):sb4;cdecl;

begin
//only 1 row can be stored. No support for multiple rows. When multiple rows, only last is kept.
  bufpp^:=TOraFieldBuf(octxp^).Buffer;
  indp^ := @TOraFieldBuf(octxp^).Ind;
  TOraFieldBuf(octxp^).Len:=TOraFieldBuf(octxp^).Size;   //reset size to full buffer
  alenp^ := @TOraFieldBuf(octxp^).Len;
  rcodep^:=nil;
  piecep^ := OCI_ONE_PIECE;
  result:=OCI_CONTINUE;
end;

//conversions

Procedure FmtBCD2Nvu(bcd:tBCD;b:pByte);
var
  i,j,cnt   : integer;
  nibbles   : array [0..maxfmtbcdfractionsize-1] of byte;
  exp       : shortint;
  bb        : byte;
begin
  fillchar(b[0],22,#0);
  if BCDPrecision(bcd)=0 then // zero, special case
    begin
    b[0]:=1;
    b[1]:=$80;
    end
  else
    begin
    if (BCDPrecision(bcd)-BCDScale(bcd)) mod 2 <>0 then // odd number before decimal point
      begin
      nibbles[0]:=0;
      j:=1;
      end
    else
      j:=0;
    for i:=0 to bcd.Precision -1 do
      if i mod 2 =0 then
        nibbles[i+j]:=bcd.Fraction[i div 2] shr 4
      else
        nibbles[i+j]:=bcd.Fraction[i div 2] and $0f;
    nibbles[bcd.Precision+j]:=0; // make sure last nibble is also 0 in case we have odd scale
    exp:=(BCDPrecision(bcd)-BCDScale(bcd)+1) div 2;
    cnt:=exp+(BCDScale(bcd)+1) div 2;
    // to avoid "ora 01438: value larger than specified precision allowed for this column"
    // remove trailing zeros (scale < 0)
    while (nibbles[cnt*2-2]*10+nibbles[cnt*2-1])=0 do
      cnt:=cnt-1;
    // and remove leading zeros (scale > precision)
    j:=0;
    while (nibbles[j*2]*10+nibbles[j*2+1])=0 do
      begin
      j:=j+1;
      exp:=exp-1;
      end;
    if IsBCDNegative(bcd) then
      begin
      b[0]:=cnt-j+1;
      b[1]:=not(exp+64) and $7f ;
      for i:=j to cnt-1 do
        begin
        bb:=nibbles[i*2]*10+nibbles[i*2+1];
        b[2+i-j]:=101-bb;
        end;
      if 2+cnt-j<22 then  // add a 102 at the end of the number if place left.
        begin
        b[0]:=b[0]+1;
        b[2+cnt-j]:=102;
        end;
      end
    else
      begin
      b[0]:=cnt-j+1;
      b[1]:=(exp+64) or $80 ;
      for i:=j to cnt-1 do
        begin
        bb:=nibbles[i*2]*10+nibbles[i*2+1];
        b[2+i-j]:=1+bb;
        end;
      end;
    end;
end;

function Nvu2FmtBCE(b:pbyte):tBCD;
var
  i,j       : integer;
  bb,size   : byte;
  exp       : shortint;
  nibbles   : array [0..maxfmtbcdfractionsize-1] of byte;
  scale     : integer;
begin
  size := b[0];
  if (size=1) and (b[1]=$80) then // special representation for 0
    result:=IntegerToBCD(0)
  else
    begin
    result.SignSpecialPlaces:=0; //sign positive, non blank, scale 0
    result.Precision:=1;         //BCDNegate works only if Precision <>0
    if (b[1] and $80)=$80 then // then the number is positive
      begin
      exp := (b[1] and $7f)-65;
      for i := 0 to size-2 do
        begin
        bb := b[i+2]-1;
        nibbles[i*2]:=bb div 10;
        nibbles[i*2+1]:=(bb mod 10);
        end;
      end
    else
      begin
      BCDNegate(result);
      exp := (not(b[1]) and $7f)-65;
      if b[size]=102 then  // last byte doesn't count if = 102
        size:=size-1;
      for i := 0 to size-2 do
        begin
        bb := 101-b[i+2];
        nibbles[i*2]:=bb div 10;
        nibbles[i*2+1]:=(bb mod 10);
        end;
      end;
    nibbles[(size-1)*2]:=0;
    result.Precision:=(size-1)*2;
    scale:=result.Precision-(exp*2+2);
    if scale>=0 then
      begin
      if (scale>result.Precision) then  // need to add leading 0s
        begin
        for i:=0 to (scale-result.Precision+1) div 2 do
          result.Fraction[i]:=0;
        i:=scale-result.Precision;
        result.Precision:=scale;
        end
      else
        i:=0;
      j:=i;
      if (i=0) and (nibbles[0]=0) then // get rid of leading zero received from oci
        begin
        result.Precision:=result.Precision-1;
        j:=-1;
        end;
      while i<=result.Precision do // copy nibbles
        begin
        if i mod 2 =0 then
          result.Fraction[i div 2]:=nibbles[i-j] shl 4
        else
          result.Fraction[i div 2]:=result.Fraction[i div 2] or nibbles[i-j];
        i:=i+1;
        end;
      result.SignSpecialPlaces:=result.SignSpecialPlaces or scale;
      end
    else
      begin // add trailing zeroes, increase precision to take them into account
      i:=0;
      while i<=result.Precision do // copy nibbles
        begin
        if i mod 2 =0 then
          result.Fraction[i div 2]:=nibbles[i] shl 4
        else
          result.Fraction[i div 2]:=result.Fraction[i div 2] or nibbles[i];
        i:=i+1;
        end;
      result.Precision:=result.Precision-scale;
      for i := size -1 to High(result.Fraction) do
        result.Fraction[i] := 0;
      end;
    end;
end;



// TOracleConnection

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

procedure TOracleConnection.GetParameters(cursor: TSQLCursor; AParams: TParams
  );
var SQLVarNr       : integer;
    i              : integer;
    f              : double;
    year,month,day : word;
    db             : array[0..4] of byte;
    pb             : pbyte;
    s              : string;

begin
  with cursor as TOracleCursor do for SQLVarNr := 0 to High(ParamBuffers) do
    with AParams[SQLVarNr] do
      if ParamType=ptOutput then
      begin
      if parambuffers[SQLVarNr].ind = -1 then
        Value:=null;

      case DataType of
        ftInteger         : begin
                            move(parambuffers[SQLVarNr].buffer^,i,sizeof(integer));
                            asInteger := i;
                            end;
        ftFloat           : begin
                            move(parambuffers[SQLVarNr].buffer^,f,sizeof(double));
                            asFloat := f;
                            end;
        ftString          : begin
                            SetLength(s,parambuffers[SQLVarNr].Len);
                            move(parambuffers[SQLVarNr].buffer^,s[1],length(s)+1);
                            asString:=s;
                            end;
        ftDate, ftDateTime: begin
                            pb := parambuffers[SQLVarNr].buffer;
                            year:=(pb[0]-100)*100+pb[1]-100;
                            month:=pb[2];
                            day:=pb[3];
                            asDateTime:=EncodeDate(year,month,day);
                            end;
        ftFMTBcd          : begin
                            AsFMTBCD:=Nvu2FmtBCE(parambuffers[SQLVarNr].buffer);
                            end;
        end;

      end;

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

  // Create temporary service-context handle for user authentication
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

var counter : word;

begin
  with cursor as TOracleCursor do
    begin
    if Length(FieldBuffers) > 0 then
      for counter := 0 to high(FieldBuffers) do freemem(FieldBuffers[counter].buffer);
    if Length(ParamBuffers) > 0 then
      for counter := 0 to high(ParamBuffers) do freemem(ParamBuffers[counter].buffer);
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
  
var counter  : integer;
    FOcibind : POCIDefine;
    
    OFieldType   : ub2;
    OFieldSize   : sb4;

    stmttype     : ub2;

begin
  with cursor as TOracleCursor do
    begin
    if OCIStmtPrepare2(TOracleTrans(ATransaction.Handle).FOciSvcCtx,FOciStmt,FOciError,@buf[1],length(buf),nil,0,OCI_NTV_SYNTAX,OCI_DEFAULT) = OCI_ERROR then
      HandleError;
    // get statement type
    if OCIAttrGet(FOciStmt,OCI_HTYPE_STMT,@stmttype,nil,OCI_ATTR_STMT_TYPE,FOciError) = OCI_ERROR then
      HandleError;
    case stmttype of
      OCI_STMT_SELECT:FStatementType := stSelect;
      OCI_STMT_UPDATE:FStatementType := stUpdate;
      OCI_STMT_DELETE:FStatementType := stDelete;
      OCI_STMT_INSERT:FStatementType := stInsert;
      OCI_STMT_CREATE,
      OCI_STMT_DROP,
      OCI_STMT_DECLARE,
      OCI_STMT_ALTER:FStatementType := stDDL;
    else
      FStatementType := stUnknown;
    end;
    if FStatementType in [stUpdate,stDelete,stInsert,stDDL] then
      FSelectable:=false;
    if assigned(AParams) then
      begin
      setlength(ParamBuffers,AParams.Count);
      for counter := 0 to AParams.Count-1 do
        begin

        case AParams[counter].DataType of
          ftInteger : begin OFieldType := SQLT_INT; OFieldSize := sizeof(integer); end;
          ftFloat : begin OFieldType := SQLT_FLT; OFieldSize := sizeof(double); end;
          ftDate, ftDateTime : begin OFieldType := SQLT_DAT; OFieldSize := 7; end;
          ftString  : begin OFieldType := SQLT_STR; OFieldSize := 4000; end;
          ftFMTBcd,ftBCD : begin OFieldType := SQLT_VNU; OFieldSize := 22; end;
        else
          DatabaseErrorFmt(SUnsupportedParameter,[Fieldtypenames[AParams[counter].DataType]],self);
        end;
        parambuffers[counter].buffer := getmem(OFieldSize);
        parambuffers[counter].Len := OFieldSize;
        parambuffers[counter].Size := OFieldSize;


        FOciBind := nil;

        if AParams[counter].ParamType=ptInput then
          begin
          if OCIBindByName(FOciStmt,FOcibind,FOciError,pchar(AParams[counter].Name),length(AParams[counter].Name),ParamBuffers[counter].buffer,OFieldSize,OFieldType,@ParamBuffers[counter].ind,nil,nil,0,nil,OCI_DEFAULT )= OCI_ERROR then
            HandleError;
          end
        else if AParams[counter].ParamType=ptOutput then
          begin
          if OCIBindByName(FOciStmt,FOcibind,FOciError,pchar(AParams[counter].Name),length(AParams[counter].Name),nil,OFieldSize,OFieldType,nil,nil,nil,0,nil,OCI_DATA_AT_EXEC )= OCI_ERROR then
            HandleError;
          if OCIBindDynamic(FOcibind, FOciError, nil, @cbf_no_data, @parambuffers[counter], @cbf_get_data) <> OCI_SUCCESS then
            HandleError;
          end;
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
    if ParamType=ptInput then
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
        ftFmtBCD,ftBCD    : begin
                            FmtBCD2Nvu(asFmtBCD,parambuffers[SQLVarNr].buffer);
                            end;
        else
          DatabaseErrorFmt(SUnsupportedParameter,[DataType],self);
      end;

      end;

end;

procedure TOracleConnection.UnPrepareStatement(cursor: TSQLCursor);
begin
  if OCIStmtRelease(TOracleCursor(cursor).FOciStmt,FOciError,nil,0,OCI_DEFAULT)<> OCI_SUCCESS then
    HandleError();
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
    if Assigned(APArams) and (AParams.count > 0) then GetParameters(cursor, AParams);
    end;
end;

function TOracleConnection.RowsAffected(cursor: TSQLCursor): TRowsCount;
var rowcount: ub4;
begin
  if OCIAttrGet((cursor as TOracleCursor).FOciStmt, OCI_HTYPE_STMT, @rowcount, nil, OCI_ATTR_ROW_COUNT, FOciError) = OCI_SUCCESS then
    Result:=rowcount
  else
    Result:=inherited RowsAffected(cursor);
end;

procedure TOracleConnection.AddFieldDefs(cursor: TSQLCursor; FieldDefs: TFieldDefs);

var Param      : POCIParam;
    counter    : ub4;

    FieldType  : TFieldType;
    FieldName  : string;
    FieldSize  : cardinal;

    OFieldType   : ub2;
    OFieldName   : Pchar;
    OFieldSize   : ub4;
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

    // Note: needs to be cleared then allocated in one go.
    Setlength(FieldBuffers,numcols);

    for counter := 1 to numcols do
      begin
      // Clear OFieldSize. Oracle 9i, 10g doc says *ub4 but some clients use *ub2 leaving
      // high 16 bit untouched resulting in huge values and ORA-01062
      // WARNING: this is not working in big endian systems !!!!
      // To be tested if BE systems have this *ub2<->*ub4 problem
      OFieldSize:=0;

      if OCIParamGet(FOciStmt,OCI_HTYPE_STMT,FOciError,Param,counter) = OCI_ERROR then
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
                                if (Oscale = 0) and (Oprecision<9) then
                                  begin
                                  if Oprecision=0 then //Number(0,0) = number(32,4)
                                    begin
                                    FieldType := ftFMTBCD;
                                    FieldSize := 4;
                                    OFieldType := SQLT_VNU;
                                    OFieldSize:= 22;
                                    end
                                  else
                                    begin
                                    FieldType := ftInteger;
                                    OFieldType := SQLT_INT;
                                    OFieldSize:= sizeof(integer);
                                    end;
                                  end
                                else if (Oscale = -127) {and (OPrecision=0)} then
                                  begin
                                  FieldType := ftFloat;
                                  OFieldType := SQLT_FLT;
                                  OFieldSize:=sizeof(double);
                                  end
                                else if (Oscale >=0) and (Oscale <=4) and (OPrecision<=12) then
                                  begin
                                  FieldType := ftBCD;
                                  FieldSize := oscale;
                                  OFieldType := SQLT_VNU;
                                  OFieldSize:= 22;
                                  end
                                else if (OPrecision-Oscale<64) and (Oscale < 64) then // limited to 63 digits before or after decimal point
                                  begin
                                  FieldType := ftFMTBCD;
                                  FieldSize := oscale;
                                  OFieldType := SQLT_VNU;
                                  OFieldSize:= 22;
                                  end
                                else // approximation with double, best we can do
                                  begin
                                  FieldType := ftFloat;
                                  OFieldType := SQLT_FLT;
                                  OFieldSize:=sizeof(double);
                                  end;
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

      FieldBuffers[counter-1].buffer := getmem(OFieldSize);

      FOciDefine := nil;
      if OciDefineByPos(FOciStmt,FOciDefine,FOciError,counter,fieldbuffers[counter-1].buffer,OFieldSize,OFieldType,@(fieldbuffers[counter-1].ind),nil,nil,OCI_DEFAULT) = OCI_ERROR then
        HandleError;

      if OCIAttrGet(Param,OCI_DTYPE_PARAM,@OFieldName,@OFNameLength,OCI_ATTR_NAME,FOciError) <> OCI_SUCCESS then
        HandleError;

      setlength(Fieldname,OFNameLength);
      move(OFieldName^,Fieldname[1],OFNameLength);

      FieldDefs.Add(FieldDefs.MakeNameUnique(FieldName), FieldType, FieldSize, False, counter);
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
      ftFMTBCD             :  begin
                           pBCD(buffer)^:= Nvu2FmtBCE(fieldbuffers[FieldDef.FieldNo-1].buffer);
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

procedure TOracleConnection.UpdateIndexDefs(IndexDefs: TIndexDefs;
  TableName: string);
var qry : TSQLQuery;

begin
  if not assigned(Transaction) then
    DatabaseError(SErrConnTransactionnSet);

  qry := tsqlquery.Create(nil);
  qry.transaction := Transaction;
  qry.database := Self;
  with qry do
    begin
    ReadOnly := True;
    sql.clear;

    sql.add('SELECT '+
              'i.INDEX_NAME,  '+
              'c.COLUMN_NAME, '+
              'p.CONSTRAINT_TYPE '+
            'FROM ALL_INDEXES i, ALL_IND_COLUMNS c,ALL_CONSTRAINTS p  '+
            'WHERE '+
              'i.OWNER=c.INDEX_OWNER AND '+
              'i.INDEX_NAME=c.INDEX_NAME AND '+
              'p.INDEX_NAME(+)=i.INDEX_NAME AND '+
              'Upper(c.TABLE_NAME) = ''' +  UpperCase(TableName) +''' '+
            'ORDER by i.INDEX_NAME,c.COLUMN_POSITION');
    open;
    end;
  while not qry.eof do with IndexDefs.AddIndexDef do
    begin
    Name := trim(qry.fields[0].asstring);
    Fields := trim(qry.Fields[1].asstring);
    If UpperCase(qry.fields[2].asString)='P' then options := options + [ixPrimary];
    If UpperCase(qry.fields[2].asString)='U' then options := options + [ixUnique];
    qry.next;
    while (name = qry.fields[0].asstring) and (not qry.eof) do
      begin
      Fields := Fields + ';' + trim(qry.Fields[2].asstring);
      qry.next;
      end;
    end;
  qry.close;
  qry.free;
end;

function TOracleConnection.GetSchemaInfoSQL(SchemaType: TSchemaType;
  SchemaObjectName, SchemaPattern: string): string;
var s : string;

begin
  case SchemaType of
    stTables     : s := 'SELECT '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          'sys_context( ''userenv'', ''current_schema'' ) as schema_name, '+
                          'TABLE_NAME '+
                        'FROM USER_CATALOG ' +
                        'WHERE '+
                          'TABLE_TYPE<>''SEQUENCE'' '+
                        'ORDER BY TABLE_NAME';

    stSysTables  : s := 'SELECT '+
                          '''' + DatabaseName + ''' as catalog_name, '+
                          'OWNER as schema_name, '+
                          'TABLE_NAME '+
                        'FROM ALL_CATALOG ' +
                        'WHERE '+
                          'TABLE_TYPE<>''SEQUENCE'' '+
                        'ORDER BY TABLE_NAME';
    stColumns    : s := 'SELECT '+
                          'COLUMN_NAME, '+
                          'DATA_TYPE as column_datatype, '+
                          'CHARACTER_SET_NAME, '+
                          'NULLABLE as column_nullable, '+
                          'DATA_LENGTH as column_length, '+
                          'DATA_PRECISION as column_precision, '+
                          'DATA_SCALE as column_scale, '+
                          'DATA_DEFAULT '+
                        'FROM ALL_TAB_COLUMNS '+
                        'WHERE Upper(TABLE_NAME) = '''+UpperCase(SchemaObjectName)+''' '+
                        'ORDER BY COLUMN_NAME';
    stProcedures : s := 'SELECT '+
                          'case when PROCEDURE_NAME is null then OBJECT_NAME ELSE OBJECT_NAME || ''.'' || PROCEDURE_NAME end AS proc_name '+
                        'FROM USER_PROCEDURES ';
  else
    DatabaseError(SMetadataUnavailable)
  end; {case}
  result := s;
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

class function TOracleConnectionDef.DefaultLibraryName: String;
begin
  {$IfDef LinkDynamically}
  Result:=ocilib;
  {$else}
  Result:='';
  {$endif}
end;

class function TOracleConnectionDef.LoadFunction: TLibraryLoadFunction;
begin
  {$IfDef LinkDynamically}
  Result:=@InitialiseOCI;
  {$else}
  Result:=Nil;
  {$endif}
end;

class function TOracleConnectionDef.UnLoadFunction: TLibraryUnLoadFunction;
begin
  {$IfDef LinkDynamically}
  Result:=@ReleaseOCI;
  {$else}
  Result:=Nil;
  {$endif}
end;

class function TOracleConnectionDef.LoadedLibraryName: string;
begin
  {$IfDef LinkDynamically}
  Result:=OCILoadedLibrary;
  {$else}
  Result:='';
  {$endif}
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

