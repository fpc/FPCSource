unit wasmbinwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wasmmodule, wasmbin, lebutils, wasmbincode
  ,wasmlink;

type
  TSectionRec = record
    secpos    : int64;
    szpos     : int64;
    datapos   : int64;
    endofdata : int64;
  end;

  { TBinWriter }

  TBinWriter = class
  protected
    dst  : TStream;
    org  : TStream;
    strm : TList;

    // the list of relocations per module
    reloc      : array of TRelocationEntry;
    relocCount : integer;

    procedure AddReloc(relocType: byte; ofs: int64; index: UInt32);

    procedure WriteRelocU32(u: longword);
    procedure SectionBegin(secId: byte; out secRec: TSectionRec; secsize: longWord=0);
    function SectionEnd(var secRec: TSectionRec): Boolean;

    procedure WriteInstList(list: TWasmInstrList; ofsAddition: LongWord);

    procedure WriteFuncTypeSect(m: TWasmModule);
    procedure WriteFuncSect(m: TWasmModule);
    procedure WriteExportSect(m: TWasmModule);
    procedure WriteCodeSect(m: TWasmModule);

    procedure pushStream(st: TStream);
    function popStream: TStream;
  public
    keepLeb128 : Boolean; // keep leb128 at 4 offset relocatable
    writeReloc : Boolean; // writting relocation (linking) information
    constructor Create;
    destructor Destroy; override;
    function Write(m: TWasmModule; adst: TStream): Boolean;
  end;

function WriteModule(m: TWasmModule; dst: TStream): Boolean;

type
  TLocalsInfo = record
    count : Integer;
    tp    : byte;
  end;
  TLocalInfoArray = array of TLocalsInfo;

// returns the list of local arrays
procedure GetLocalInfo(func: TWasmFunc; out loc: TLocalInfoArray);

implementation

procedure GetLocalInfo(func: TWasmFunc; out loc: TLocalInfoArray);
var
  i   : integer;
  cnt : integer;
  tp  : byte;
  nt  : byte;
  j   : integer;

  procedure Push;
  begin
    if j=length(loc) then begin
      if j=0 then SetLength(loc, 1)
      else SetLength(loc, j*2);
    end;
    loc[j].tp:=tp;
    loc[j].count:=cnt;
    inc(j);
  end;

begin
  SetLength(Loc, 0);
  if func.LocalsCount = 0 then Exit;
  cnt:=1;
  tp:=func.GetLocal(0).tp;
  j:=0;
  for i:=1 to func.LocalsCount-1 do begin
    nt := func.GetLocal(i).tp;
    if nt<>tp then begin
      Push;
      tp:=nt;
      cnt:=1;
    end else
      inc(cnt);
  end;
  Push;
  SetLength(loc, j);
end;

function WriteModule(m: TWasmModule; dst: TStream): Boolean;
var
  bw : TBinWriter;
begin
  bw := TBinWriter.Create;
  try
    bw.keepLeb128:=true;
    bw.writeReloc:=true;
    Normalize(m);
    Result := bw.Write(m, dst);
  finally
    bw.Free;
  end;
end;

{ TBinWriter }

procedure TBinWriter.AddReloc(relocType: byte; ofs: int64; index: UInt32);
begin
  if relocCount=length(reloc) then begin
    if relocCount=0 then SetLength(reloc, 16)
    else SetLength(reloc, relocCount*2);
  end;
  reloc[relocCount].reltype:=relocType;
  reloc[relocCount].offset:=ofs;
  reloc[relocCount].index:=index;
  inc(relocType);
end;

procedure TBinWriter.WriteRelocU32(u: longword);
begin
  WriteU(dst, u, sizeof(u)*8, keepLeb128);
end;

function TBinWriter.Write(m: TWasmModule; adst: TStream): Boolean;
var
  l : Longword;
begin
  if not Assigned(m) or not Assigned(adst) then begin
    Result:=false;
    Exit;
  end;
  keepLeb128:=keepLeb128 or writeReloc; // use 128, if relocation has been requested

  dst:=adst;
  org:=adst;

  dst.Write(WasmId_Buf, length(WasmId_Buf));
  l:=NtoLE(Wasm_Version1);
  dst.Write(l, sizeof(l));

  // 01 function type section
  WriteFuncTypeSect(m);

  // 03 function section
  WriteFuncSect(m);

  // 07 export section
  WriteExportSect(m);

  // 10 code section
  WriteCodeSect(m);

  Result:=true;
end;

procedure TBinWriter.SectionBegin(secId: byte; out secRec: TSectionRec; secsize: longWord=0);
begin
  secRec.secpos:=dst.Position;
  dst.WriteByte(secId);
  secRec.szpos:=dst.Position;
  WriteRelocU32(secsize);
  secRec.datapos:=dst.Position;
  secRec.endofdata:=dst.Position+secsize;
end;

function TBinWriter.SectionEnd(var secRec: TSectionRec): Boolean;
var
  sz: LongWord;
begin
  secRec.endofdata:=dst.Position;
  dst.Position:=secRec.szpos;
  sz := secRec.endofdata - secRec.datapos;
  WriteRelocU32(sz);
  dst.Position:=secRec.endofdata;
  Result := true;
end;

procedure TBinWriter.WriteFuncTypeSect(m: TWasmModule);
var
  sc : TSectionRec;
  i  : integer;
  j  : integer;
  tp : TWasmFuncType;
begin
  SectionBegin(SECT_TYPE, sc);

  WriteU32(dst, m.TypesCount);
  for i:=0 to m.TypesCount-1 do begin
    tp:=m.GetType(i);
    dst.WriteByte(func_type);

    WriteU32(dst, tp.ParamCount);
    for j:=0 to tp.ParamCount-1 do
      dst.WriteByte(tp.GetParam(i).tp);

    WriteU32(dst, tp.ResultCount);
    for j:=0 to tp.ResultCount-1 do
      dst.WriteByte(tp.GetResult(i).tp);
  end;
  SectionEnd(sc);
end;

procedure TBinWriter.WriteFuncSect(m: TWasmModule);
var
  sc : TSectionRec;
  i  : integer;
begin
  SectionBegin(SECT_FUNCTION, sc);

  WriteU32(dst, m.FuncCount);
  for i:=0 to m.FuncCount-1 do
    WriteRelocU32(m.GetFunc(i).functype.typeNum);

  SectionEnd(sc);
end;

procedure TBinWriter.WriteExportSect(m: TWasmModule);
var
  sc : TSectionRec;
  i  : integer;
  x  : TWasmExport;
begin
  SectionBegin(SECT_EXPORT, sc);

  WriteU32(dst, m.ExportCount);
  for i:=0 to m.ExportCount-1 do begin
    x:=m.GetExport(i);
    WriteU32(dst, length(x.name));
    if length(x.name)>0 then
      dst.Write(x.name[1], length(x.name));
    dst.WriteByte(x.exportType);
    WriteRelocU32(x.exportNum);
  end;

  SectionEnd(sc);
end;


procedure TBinWriter.WriteCodeSect(m: TWasmModule);
var
  sc    : TSectionRec;
  i, j  : integer;
  sz    : int64;
  mem   : TMemoryStream;
  la    : TLocalInfoArray;
  f     : TWasmFunc;
  dofs  : Int64;
begin
  SectionBegin(SECT_CODE, sc);

  mem:=TMemoryStream.Create;
  try
    WriteU32(dst, m.FuncCount);
    for i :=0 to m.FuncCount-1 do begin
      f:=m.GetFunc(i);

      GetLocalInfo(f, la);

      mem.Position:=0;
      dofs:=dst.Position;
      pushStream(mem);

      WriteU32(dst, length(la));
      for j:=0 to length(la)-1 do begin
        WriteU32(dst, la[i].count);
        dst.WriteByte(la[i].tp);
      end;
      WriteInstList(f.instr, LongWord(dofs-sc.datapos));
      popStream;

      sz:=mem.Position;
      mem.Position:=0;

      WriteRelocU32(sz);
      dst.CopyFrom(mem, sz);
    end;
  finally
    mem.Free;
  end;
  SectionEnd(sc);
end;

procedure TBinWriter.WriteInstList(list: TWasmInstrList; ofsAddition: Longword);
var
  i  : integer;
  ci : TWasmInstr;
begin
  for i:=0 to list.Count-1 do begin
    ci :=list[i];
    dst.WriteByte(ci.code);
    case INST_FLAGS[ci.code].Param of
      ipLeb:
        if INST_RELOC_FLAGS[ci.code].doReloc then begin
          AddReloc(INST_RELOC_FLAGS[ci.code].relocType, dst.Position+ofsAddition, ci.operandNum);
          WriteRelocU32(ci.operandNum)
        end
        else
          WriteU32(dst, ci.operandNum);
    end;
  end;
end;

procedure TBinWriter.pushStream(st: TStream);
begin
  if st=nil then Exit;
  strm.Add(st);
  dst:=st;
end;

function TBinWriter.popStream: TStream;
begin
  if strm.Count=0 then
    Result:=nil
  else begin
    Result:=TStream(strm[strm.Count-1]);
    strm.Delete(strm.Count-1);
  end;
  if strm.Count=0 then dst:=org
  else dst:=TStream(strm[strm.Count-1]);
end;

constructor TBinWriter.Create;
begin
  inherited Create;
  strm:=TList.Create;
end;

destructor TBinWriter.Destroy;
begin
  strm.Free;
  inherited Destroy;
end;


end.

