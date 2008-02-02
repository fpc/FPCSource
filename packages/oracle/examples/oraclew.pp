{
    Copyright (c) 1999-2000 by Pavel Stingl <stingp1.eti@mail.cez.cz>


    OCI workaround

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit oraclew;

interface

{$H+}
{$mode objfpc}

uses OCI, oratypes,Classes, SysUtils;

{ all pos parameters are indexed from 1..x! }

    procedure OraInit;
    procedure OraFin;
    procedure OraLogin(name, pass, server: string);
    procedure OraLogout;
    procedure OraSQLExec(sql: string);
    function OraGetFieldAsString(pos : integer) : string;
    function OraGetFieldAsInteger(pos : integer) : longint;
    function OraNext: boolean;
    function OraGetFieldCount: integer;
    function OraGetFieldName(pos : integer) : string;
    function OraGetFieldType(pos : integer) : longint;
    function IsFieldDate(Pos : integer): boolean;
    procedure OraError(errcode: integer; err: POCIError; msg : string);

const
    cDescribeBuf = 1024;
    cPCharBufLen = 4097;
    cPrefetchCnt = 100;

type

  PDescribeRec = ^TDescribeRec;
  TDescribeRec = record
    dbsize      : sb4;
    dbtype      : sb2;
    buf         : array [0..cDescribeBuf] of char;
    buflen      : sb4;
    dsize       : sb4;
    precision   : sb2;
    scale       : sb2;
    nullok      : sb2;

    // Define part
      valbuf    : array [0..cDescribeBuf] of char;
      flt_buf   : double;
      int_buf   : cardinal;
      int64_buf : int64;
      indp      : sb2;
      col_retlen: ub2;
      col_retcode: ub2;
  end;

var
    Env : POCIEnv;
    Err : POCIError;
    Svc : POCISvcCtx;
    Stmt: POCIStmt;
    FieldList : TList;

    ecode : integer;

implementation

  function DecodeDataType(dtype : longint): string;
  begin
    case dtype of
        SQLT_CHR : DecodeDataType := '(ORANET TYPE) character string';
        SQLT_NUM : DecodeDataType := '(ORANET TYPE) oracle numeric';
        SQLT_INT : DecodeDataType := '(ORANET TYPE) integer';
        SQLT_FLT : DecodeDataType := '(ORANET TYPE) Floating point number';
        SQLT_STR : DecodeDataType := 'zero terminated string';
        SQLT_VNU : DecodeDataType := 'NUM with preceding length byte';
        SQLT_PDN : DecodeDataType := '(ORANET TYPE) Packed Decimal Numeric';
        SQLT_LNG : DecodeDataType := 'long';
        SQLT_VCS : DecodeDataType := 'Variable character string';
        SQLT_NON : DecodeDataType := 'Null/empty PCC Descriptor entry';
        SQLT_RID : DecodeDataType := 'rowid';
        SQLT_DAT : DecodeDataType := 'date in oracle format';
        SQLT_VBI : DecodeDataType := 'binary in VCS format';
        SQLT_BIN : DecodeDataType := 'binary data(DTYBIN)';
        SQLT_LBI : DecodeDataType := 'long binary';
        SQLT_UIN : DecodeDataType := 'unsigned integer';
        SQLT_SLS : DecodeDataType := 'Display sign leading separate';
        SQLT_LVC : DecodeDataType := 'Longer longs (char)';
        SQLT_LVB : DecodeDataType := 'Longer long binary';
        SQLT_AFC : DecodeDataType := 'Ansi fixed char';
        SQLT_AVC : DecodeDataType := 'Ansi Var char';
        SQLT_CUR : DecodeDataType := 'cursor  type';
        SQLT_RDD : DecodeDataType := 'rowid descriptor';
        SQLT_LAB : DecodeDataType := 'label type';
        SQLT_OSL : DecodeDataType := 'oslabel type';
        SQLT_NTY : DecodeDataType := 'named object type';
        SQLT_REF : DecodeDataType := 'ref type';
        SQLT_CLOB : DecodeDataType := 'character lob';
        SQLT_BLOB : DecodeDataType := 'binary lob';
        SQLT_BFILEE : DecodeDataType := 'binary file lob';
        SQLT_CFILEE : DecodeDataType := 'character file lob';
        SQLT_RSET : DecodeDataType := 'result set type';
        SQLT_NCO : DecodeDataType := 'named collection type (varray or nested table)';
        SQLT_VST : DecodeDataType := 'OCIString type';
        SQLT_ODT : DecodeDataType := 'OCIDate type';
    else DecodeDataType := 'Unknown';
    end;
  end;

  procedure FieldListClear;
  var
        x: longint;
        PDesc: PDescribeRec;
  begin
        if FieldList.Count = 0 then Exit;
        for x := 0 to FieldList.Count - 1 do
        begin
            PDesc := FieldList[x];
            Dispose(PDesc);
        end;
        FieldList.Clear;
  end;

       procedure Describe;
       var
        fldc    : longint;
        paramd  : POCIParam;
        colname : PChar;
        colsize : ub4;
        Rec     : PDescribeRec;
       begin
        fldc := 1;

        FieldListClear;
        ecode := OCIParamGet(Stmt, OCI_HTYPE_STMT, Err, paramd, fldc);
        if ecode <> OCI_SUCCESS then
            ORAError(ecode, Err, 'OCIParamGetError');
        while ecode = OCI_SUCCESS do
        begin
            New(Rec);
            FillChar(Rec^.buf, sizeof(Rec^.buf), #0);
            ecode := OCIAttrGet(paramd, OCI_DTYPE_PARAM, @Rec^.dbtype, nil,
                OCI_ATTR_DATA_TYPE, Err);
            if ecode <> 0 then
            begin
                ORAError(ecode, Err, 'Retrieving DTYPE_PARAM:');
            end;
            colsize := 0;
            colname := nil;
            ecode := OCIAttrGet(paramd, OCI_DTYPE_PARAM, @colname, @colsize,
                OCI_ATTR_NAME, Err);
            if ecode <> 0 then
            begin
                ORAError(ecode, Err, 'Retrieving DTYPE_PARAM:');
            end;
            Move(Colname^,Rec^.buf, colsize);
            Rec^.buflen := colsize;
//          WriteLn('Column: ',Rec^.buf:15,'    DataType: ',DecodeDataType(Rec^.dbtype));
            inc(fldc);

            FieldList.Add(Rec);
            ecode := OCIParamGet(Stmt, OCI_HTYPE_STMT, Err, paramd, fldc);
        end;
    end;

    procedure Define;
    var
        x : longint;
        def: POCIDefine;
        PDesc : PDescribeRec;
        defptr: pointer;
        deflen: sword;
        deftyp: sword;
    begin
        def := nil;
        for x := 0 to FieldList.Count - 1 do
        begin
            PDesc := FieldList[x];
            case PDesc^.dbtype of
                SQLT_NUM: begin
                    if PDesc^.scale <> 0 then
                    begin
                        defptr := @PDesc^.flt_buf;
                        deflen := SizeOf(PDesc^.flt_buf);
                        deftyp := SQLT_FLT;
                        PDesc^.dbtype := SQLT_FLT;
                    end
                    else begin
              if PDesc^.dbsize > 4 then
              begin
                // WriteLn('BIG FAT WARNING!!!! dbsize int > 4 (',PDesc^.dbsize,')');
                defptr := @PDesc^.int64_buf;
                deflen := SizeOf(PDesc^.int64_buf);
                deftyp := SQLT_INT;
                PDesc^.dbtype := SQLT_INT;
              end
              else begin
                defptr := @PDesc^.int_buf;
                            deflen := SizeOf(PDesc^.int_buf);
                            deftyp := SQLT_INT;
                            PDesc^.dbtype := SQLT_INT;
              end;
                    end;
                end;
                else begin
                    defptr := @PDesc^.valbuf;
                    deflen := cDescribeBuf;
                    deftyp := PDesc^.dbtype;
                end;
            end;
            ecode := OCIDefineByPos(Stmt, def, Err, x + 1, defptr,
                deflen, deftyp, @PDesc^.indp, @PDesc^.col_retlen,
                @PDesc^.col_retcode, OCI_DEFAULT);
            if ecode <> 0 then
            begin
                OraError(ecode, Err, 'OCIDefineByPos: ');
            end;
        end;
    end;

    procedure OraError( errcode : integer; err: POCIError; msg : string );
    var
        buff : array [0..1024] of char;

    begin
        if err <> nil then
        begin
            case errcode of
                OCI_INVALID_HANDLE: Msg := Msg + ' OCI_INVALID_HANDLE';
            end;
            OCIErrorGet( err, 1, nil, errcode, @buff[0], 1024, OCI_HTYPE_ERROR);
            writeln(stderr, msg, ' ', buff);
        end
        else begin
            WriteLn(stderr, msg);
            Halt(1);
        end;
    end;

    procedure OraInit;
    begin
        ecode := OCIInitialize({OCI_DEFAULT or }OCI_OBJECT, nil, nil, nil, nil);
        if ecode <> 0 then OraError( ecode, nil, 'Error initializing OCI');
        ecode := OCIEnvInit(Env, OCI_DEFAULT, 0, nil);
        if ecode <> 0 then OraError( ecode, nil, 'Error initializing OCI environment');
        ecode := OCIHandleAlloc(Env, Err, OCI_HTYPE_ERROR, 0, nil);
        if ecode <> 0 then OraError( ecode, nil, 'Error allocating error handle');
        ecode := OCIHandleAlloc(Env, Stmt, OCI_HTYPE_STMT, 0, nil);
        if ecode <> 0 then OraError( ecode, nil, 'Error allocating statement handle');
    end;

    procedure OraLogin(name, pass, server: string);
    begin
        ecode := OCILogon(Env, Err, Svc, @name[1], Length(name),
            @pass[1], Length(pass), @server[1], Length(server));
        if ecode <> 0 then OraError(ecode, Err, '');
    end;

    procedure OraLogout;
    begin
        ecode := OCILogoff(Svc, Err);
        if ecode <> 0 then
            OraError(ecode, Err, 'OCILogoff: ');
    end;

    procedure OraFin;
    begin
        OCIHandleFree(Stmt, OCI_HTYPE_STMT);
        OCIHandleFree(Err, OCI_HTYPE_ERROR);
    end;

    procedure OraSQLExec(sql: string);
    var
        dtype: longint;
    begin
//    writeLn(Length(sql));
        ecode := OCIStmtPrepare(Stmt, Err, @sql[1], Length(sql),
            OCI_NTV_SYNTAX, OCI_DEFAULT);
        if ecode <> 0 then
        begin
            OraError(ecode, Err, 'OCIStmtPrepare:');
            Exit;
        end;

    dtype := cPrefetchCnt;
    ecode := OCIAttrSet(Stmt, OCI_HTYPE_STMT, @dtype,
      SizeOf(dtype), OCI_ATTR_PREFETCH_ROWS, Err);
        if ecode <> 0 then
        begin
            OraError(ecode, Err, 'ociattrset:');
            Exit;
        end;

        dtype := 0;
        ecode := OCIAttrGet(Stmt, OCI_HTYPE_STMT, @dtype, nil,
            OCI_ATTR_STMT_TYPE, Err);
        if ecode <> 0 then
        begin
            OraError(ecode, Err, 'ociattrget:');
            Exit;
        end;

        ecode := 0;
        if dtype = OCI_STMT_SELECT then
            ecode := OCIStmtExecute(Svc, Stmt, Err, 0, 0, nil, nil, OCI_DEFAULT)
        else ecode := OCIStmtExecute(Svc, Stmt, Err, 1, 0, nil, nil, OCI_DEFAULT);
        if ecode <> 0 then
        begin
            OraError(ecode, Err, 'OCIStmtExecute:');
            Exit;
        end;

        if dtype = OCI_STMT_SELECT then
        begin
            Describe;
            Define;
        end;
    end;

    function OraGetFieldCount : integer;
    begin
        OraGetFieldCount := FieldList.Count;
    end;

    function IsFieldDate(Pos : integer): boolean;
    var
      Desc : TDescribeRec;
    begin
      Result := False;
      if (Pos > FieldList.Count) or (Pos < 1) then
        Exit;
      Desc := TDescribeRec(FieldList[Pos-1]^);
      Result := (Desc.dbtype = SQLT_DAT);
    end;

    function OraGetFieldAsString(pos : integer) : string;
    var
        Desc : TDescribeRec;
    Date : array [0..6] of byte;
    begin
        if (Pos > FieldList.Count) or (Pos < 1) then
            Exit;
        Desc := TDescribeRec(FieldList[pos-1]^);
    if Desc.indp < 0 then
    begin
      OraGetFieldAsString := 'null';
      Exit;
    end;
        if Desc.dbtype = SQLT_STR then
        begin
            Desc.valbuf[Desc.col_retlen] := #0;
            OraGetFieldAsString := strpas(Desc.valbuf);
        end
        else if Desc.dbtype = SQLT_CHR then
        begin
            Desc.valbuf[Desc.col_retlen] := #0;
            OraGetFieldAsString := strpas(Desc.valbuf);
        end
        else if Desc.dbtype = SQLT_INT then
    begin
            OraGetFieldAsString := IntToStr(Desc.int_buf);
    end
        else if Desc.dbtype = SQLT_FLT then
            OraGetFieldAsString := FloatToStr(Desc.flt_buf)
        else if Desc.dbtype = SQLT_DAT then
    begin
        Move(Desc.valbuf,Date,SizeOf(Date));
            OraGetFieldAsString :=
          Format('%0.2d.%0.2d.%0.4d %0.2d:%0.2d:%0.2d',
          [Date[3],Date[2],(((Date[0]-100)*100)+(Date[1] - 100)),
           Date[4]-1,
           Date[5]-1,
           Date[6]-1]);
    end
        else if Desc.dbtype = SQLT_AFC then
        begin
            Desc.valbuf[Desc.col_retlen] := #0;
            OraGetFieldAsString := strpas(Desc.valbuf);
        end
        else OraGetFieldAsString := 'dbtype not implemented ' + IntToStr(Desc.dbtype);
    end;

    function OraGetFieldAsInteger(pos : integer) : longint;
    begin
        OraGetFieldAsInteger := 0;
    end;

    function OraNext: boolean;
    begin
        ecode := OCIStmtFetch(Stmt, Err, 1, OCI_FETCH_NEXT, OCI_DEFAULT);
        if ecode = 0 then
            OraNext := true
        else if ecode = OCI_SUCCESS_WITH_INFO then
            OraNext := false
        else if ecode = OCI_NO_DATA then
            OraNext := false
        else begin
            OraNext := false;
            OraError(ecode, err, 'OCIStmtFetch:');
        end;
    end;

    function OraGetFieldType(pos : integer) : longint;
    begin
      if (Pos > FieldList.Count) or (pos < 1) then
        Exit;
      OraGetFieldType := TDescribeRec(FieldList[pos-1]^).dbtype;
    end;

    function OraGetFieldName(pos : integer) : string;
    begin
        if (Pos > FieldList.Count) or (Pos < 1) then
            Exit;
        OraGetFieldName := strpas(TDescribeRec(FieldList[pos-1]^).buf);
    end;

initialization

    FieldList := TList.Create;

finalization

    FieldListClear;
    FieldList.Free;

end.
