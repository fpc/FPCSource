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

type
  TOracleTrans = Class(TSQLHandle)
    protected
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
    FOciSvcCtx      : POCISvcCtx;
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

uses math;

ResourceString
  SErrEnvCreateFailed = 'The creation of an Oracle environment failed.';
  SErrHandleAllocFailed = 'The allocation of the error handle failed.';
  SErrOracle = 'Oracle returned error %s:';

procedure TOracleConnection.HandleError;

var errcode : sb4;
    buf     : array[0..1023] of char;

begin
  OCIErrorGet(FOciError,1,nil,errcode,@buf[1],1023,OCI_HTYPE_ERROR);
  DatabaseErrorFmt(SErrOracle+LineEnding+buf,[inttostr(errcode)],self);
end;

procedure TOracleConnection.DoInternalConnect;

var ConnectString : string;

begin
{$IfDef LinkDynamically}
  InitialiseOCI;
{$EndIf}

  inherited DoInternalConnect;
  FUserMem := nil;
  if OCIEnvCreate(FOciEnvironment,oci_default,nil,nil,nil,nil,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrEnvCreateFailed,self);

  if OciHandleAlloc(FOciEnvironment,FOciError,OCI_HTYPE_ERROR,0,FUserMem) <> OCI_SUCCESS then
    DatabaseError(SErrHandleAllocFailed,self);

  if hostname='' then connectstring := databasename
  else connectstring := '//'+hostname+'/'+databasename;

  if OCILogon2(FOciEnvironment,FOciError,FOciSvcCtx,@username[1],length(username),@password[1],length(password),@connectstring[1],length(connectstring),OCI_DEFAULT) = OCI_ERROR then
    HandleError;
end;

procedure TOracleConnection.DoInternalDisconnect;
begin
  inherited DoInternalDisconnect;

  if OCILogoff(FOciSvcCtx,FOciError)<> OCI_SUCCESS then
    HandleError;

  OCIHandleFree(FOciSvcCtx,OCI_HTYPE_SVCCTX);
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
  OciHandleAlloc(FOciEnvironment,Cursor.FOciStmt,OCI_HTYPE_STMT,0,FUserMem);
  Result := cursor;
end;

procedure TOracleConnection.DeAllocateCursorHandle(var cursor: TSQLCursor);

var tel : word;

begin
  with cursor as TOracleCursor do
    begin
    OCIHandleFree(FOciStmt,OCI_HTYPE_ERROR);
    if Length(FieldBuffers) > 0 then
      for tel := 0 to high(FieldBuffers) do freemem(FieldBuffers[tel].buffer);
    end;
  FreeAndNil(cursor);
end;

function TOracleConnection.AllocateTransactionHandle: TSQLHandle;
begin
  Result:=nil;
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
    if OCIStmtPrepare(FOciStmt,FOciError,@buf[1],length(buf),OCI_NTV_SYNTAX,OCI_DEFAULT) = OCI_ERROR then
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
//
end;

function TOracleConnection.GetTransactionHandle(trans: TSQLHandle): pointer;
begin
// Transactions not implemented yet
end;

function TOracleConnection.StartDBTransaction(trans: TSQLHandle; AParams: string): boolean;
begin
// Transactions not implemented yet
end;

function TOracleConnection.Commit(trans: TSQLHandle): boolean;
begin
// Transactions not implemented yet
end;

function TOracleConnection.Rollback(trans: TSQLHandle): boolean;
begin
// Transactions not implemented yet
end;

procedure TOracleConnection.CommitRetaining(trans: TSQLHandle);
begin
// Transactions not implemented yet
end;

procedure TOracleConnection.RollbackRetaining(trans: TSQLHandle);
begin
// Transactions not implemented yet
end;

procedure TOracleConnection.Execute(cursor: TSQLCursor; ATransaction: TSQLTransaction; AParams: TParams);
begin
  if Assigned(APArams) and (AParams.count > 0) then SetParameters(cursor, AParams);
  if cursor.FStatementType = stSelect then
    begin
    if OCIStmtExecute(FOciSvcCtx,(cursor as TOracleCursor).FOciStmt,FOciError,0,0,nil,nil,OCI_DEFAULT) = OCI_ERROR then
      HandleError;
    end
  else
    begin
    if OCIStmtExecute(FOciSvcCtx,(cursor as TOracleCursor).FOciStmt,FOciError,1,0,nil,nil,OCI_DEFAULT) = OCI_ERROR then
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
                                  FieldSize := sizeof(Currency);
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

      TFieldDef.Create(FieldDefs, FieldName, FieldType, FieldSize, False, tel);
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
                           move(cur,buffer^,FieldDef.Size);
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

initialization
  RegisterConnection(TOracleConnectionDef);
finalization
  RegisterConnection(TOracleConnectionDef);
end.

