unit wasmbinwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wasmmodule, wasmbin, lebutils;

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
    procedure WriteRelocU32(u: longword);
    procedure SectionBegin(secId: byte; out secRec: TSectionRec; secsize: longWord=0);
    function SectionEnd(var secRec: TSectionRec): Boolean;

    procedure WriteFuncTypeSect(m: TWasmModule);
    procedure WriteFuncSect(m: TWasmModule);
    procedure WriteExportSect(m: TWasmModule);
    procedure WriteCodeSect(m: TWasmModule);

    procedure pushStream(st: TStream);
    function popStream: TStream;
  public
    isWriteReloc: boolean;
    constructor Create;
    destructor Destroy; override;
    function Write(m: TWasmModule; adst: TStream): Boolean;
  end;

function WriteModule(m: TWasmModule; dst: TStream): Boolean;

implementation

function WriteModule(m: TWasmModule; dst: TStream): Boolean;
var
  bw : TBinWriter;
begin
  bw := TBinWriter.Create;
  try
    Normalize(m);
    Result := bw.Write(m, dst);
  finally
    bw.Free;
  end;
end;

{ TBinWriter }

procedure TBinWriter.WriteRelocU32(u: longword);
begin
  WriteU(dst, u, sizeof(u), isWriteReloc);
end;

function TBinWriter.Write(m: TWasmModule; adst: TStream): Boolean;
var
  l : Longword;
begin
  if not Assigned(m) or not Assigned(adst) then begin
    Result:=false;
    Exit;
  end;
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

  WriteRelocU32(m.TypesCount);
  for i:=0 to m.TypesCount-1 do begin
    tp:=m.GetType(i);
    dst.WriteByte(func_type);

    WriteRelocU32(tp.ParamCount);
    for j:=0 to tp.ParamCount-1 do
      dst.WriteByte(tp.GetParam(i).tp);

    WriteRelocU32(tp.ResultCount);
    for j:=0 to tp.ResultCount-1 do
      dst.WriteByte(tp.GetResult(i).tp);
  end;
  SectionEnd(sc);
end;

procedure TBinWriter.WriteFuncSect(m: TWasmModule);
var
  sc : TSectionRec;
  i  : integer;
  //j  : integer;
  //tp : TWasmFuncType;
begin
  SectionBegin(SECT_FUNCTION, sc);

  WriteRelocU32(m.FuncCount);
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
  WriteRelocU32(m.ExportCount);

  for i:=0 to m.ExportCount-1 do begin
    x:=m.GetExport(i);
    WriteRelocU32(length(x.name));
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
  i     : integer;
  sz    : int64;
  mem   : TMemoryStream;
begin
  SectionBegin(SECT_CODE, sc);

  mem:=TMemoryStream.Create;
  try
    for i :=0 to m.FuncCount-1 do begin
      pushStream(mem);
      // todo: locals
      // todo: instructions
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

