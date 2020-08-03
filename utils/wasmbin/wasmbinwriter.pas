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

  TSymbolObject = class(TObject)
    syminfo : TSymInfo;
    next    : TSymbolObject;
  end;

  { TBinWriter }

  TBinWriter = class
  protected
    dst  : TStream;
    org  : TStream;
    strm : TList;

    module : TWasmModule;

    // the list of relocations per module
    writeSec   : Byte;
    reloc      : array of TRelocationEntry;
    relocCount : integer;

    symHead    : TSymbolObject;
    symTail    : TSymbolObject;
    symCount   : Integer;
    function AddSymbolObject: TSymbolObject;
    procedure AddReloc(relocType: byte; ofs: int64; index: UInt32);

    procedure WriteRelocU32(u: longword);
    procedure WriteString(const s: string);
    procedure SectionBegin(secId: byte; out secRec: TSectionRec; secsize: longWord=0);
    function SectionEnd(var secRec: TSectionRec): Boolean;

    procedure WriteInstList(list: TWasmInstrList; ofsAddition: LongWord);

    procedure WriteImportSect;
    procedure WriteFuncTypeSect;
    procedure WriteTableSect;
    procedure WriteFuncSect;
    procedure WriteExportSect;
    procedure WriteCodeSect;

    procedure WriteLinkingSect;
    procedure WriteRelocSect;

    procedure pushStream(st: TStream);
    function popStream: TStream;

    procedure PrepareLinkSym(m: TWasmModule);
  public
    keepLeb128 : Boolean; // keep leb128 at 4 offset relocatable
    writeReloc : Boolean; // writting relocation (linking) information
    constructor Create;
    destructor Destroy; override;
    function Write(m: TWasmModule; adst: TStream): Boolean;
  end;

function WriteModule(m: TWasmModule; dst: TStream;
  awriteLEB128: Boolean = false;
  awriteReloc: Boolean = false): Boolean;

type
  TLocalsInfo = record
    count : Integer;
    tp    : byte;
  end;
  TLocalInfoArray = array of TLocalsInfo;

// returns the list of local arrays
procedure GetLocalInfo(func: TWasmFunc; out loc: TLocalInfoArray);

procedure WriteLimit(dst: TStream; amin, amax: LongWord);

implementation

procedure WriteLimit(dst: TStream; amin, amax: LongWord);
begin
  if not Assigned(dst) then Exit;
  if amax>0 then begin
    dst.WriteByte(1);
    WriteU32(dst, amin);
    WriteU32(dst, amax);
  end else begin
    dst.WriteByte(0);
    WriteU32(dst, amin);
  end;
end;

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

function WriteModule(m: TWasmModule; dst: TStream;
  awriteLEB128, awriteReloc: Boolean): Boolean;
var
  bw : TBinWriter;
begin
  bw := TBinWriter.Create;
  try
    bw.keepLeb128:=awriteLEB128;
    bw.writeReloc:=awriteReloc;
    Result := bw.Write(m, dst);
  finally
    bw.Free;
  end;
end;

{ TBinWriter }

function TBinWriter.AddSymbolObject: TSymbolObject;
var
  so : TSymbolObject;
begin
  so := TSymbolObject.Create;
  if not Assigned(symHead) then symHead:=so;
  if Assigned(symTail) then symTail.Next:=so;
  symTail:=so;
  inc(symCount);
  Result:=so;
end;

procedure TBinWriter.AddReloc(relocType: byte; ofs: int64; index: UInt32);
var
  i : integer;
  f : TWasmFunc;
  so : TSymbolObject;
begin
  if relocCount=length(reloc) then begin
    if relocCount=0 then SetLength(reloc, 16)
    else SetLength(reloc, relocCount*2);
  end;
  i:=relocCount;
  reloc[i].sec:=writeSec;
  reloc[i].reltype:=relocType;
  reloc[i].offset:=ofs;
  reloc[i].index:=index;
  inc(relocCount);

  case relocType of
    R_WASM_FUNCTION_INDEX_LEB:
    begin
      // ok, so this is function.
      // the function MUST HAVE a symbol information available
      // if it doesn't have a symbol then "wasm-objdump" would fail to read it
      f:=module.GetFunc(index);
      inc(f.codeRefCount);
    end;
  end;
end;

procedure TBinWriter.WriteRelocU32(u: longword);
begin
  WriteU(dst, u, sizeof(u)*8, keepLeb128);
end;

procedure TBinWriter.WriteString(const s: string);
begin
  WriteU32(dst, length(s));
  if length(s)>0 then
    dst.Write(s[1], length(s));
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

  module:=m;
  dst:=adst;
  org:=adst;

  dst.Write(WasmId_Buf, length(WasmId_Buf));
  l:=NtoLE(Wasm_Version1);
  dst.Write(l, sizeof(l));

  writeSec:=0;
  // 01 function type section
  if m.TypesCount>0 then begin
    WriteFuncTypeSect;
    inc(writeSec);
  end;

  // 02 import section
  if m.ImportCount>0 then begin
    WriteImportSect;
    inc(writeSec);
  end;

  // 03 function section
  if m.FuncCount>0 then begin
    WriteFuncSect;
    inc(writeSec);
  end;

  // 04 tables section
  if m.TableCount>0 then begin
    WriteTableSect;
    inc(writeSec);
  end;

  // 07 export section
  if m.ExportCount>0 then begin
    WriteExportSect;
    inc(writeSec);
  end;

  // 10 code section
  if m.FuncCount>0 then begin
    WriteCodeSect;
    inc(writeSec);
  end;

  if writeReloc then begin
    PrepareLinkSym(m);
    WriteLinkingSect;
    WriteRelocSect;
  end;

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

procedure TBinWriter.WriteFuncTypeSect;
var
  sc : TSectionRec;
  i  : integer;
  j  : integer;
  tp : TWasmFuncType;
begin
  SectionBegin(SECT_TYPE, sc);

  WriteU32(dst, module.TypesCount);
  for i:=0 to module.TypesCount-1 do begin
    tp:=module.GetType(i);
    dst.WriteByte(func_type);

    WriteU32(dst, tp.ParamCount);
    for j:=0 to tp.ParamCount-1 do
      dst.WriteByte(tp.GetParam(j).tp);

    WriteU32(dst, tp.ResultCount);
    for j:=0 to tp.ResultCount-1 do
      dst.WriteByte(tp.GetResult(j).tp);
  end;
  SectionEnd(sc);
end;

procedure TBinWriter.WriteTableSect;
var
  sc  : TSectionRec;
  i   : integer;
  t   : TWasmTable;
begin
  SectionBegin(SECT_TABLE, sc);
  WriteU32(dst, module.TableCount);
  for i:=0 to module.TableCount-1 do begin
    t:=module.GetTable(i);
    dst.WriteByte(t.elemsType);
    WriteLimit(dst, t.min, t.max);
  end;
  SectionEnd(sc);
end;

procedure TBinWriter.WriteFuncSect;
var
  sc : TSectionRec;
  i  : integer;
begin
  SectionBegin(SECT_FUNCTION, sc);

  WriteU32(dst, module.FuncCount);
  for i:=0 to module.FuncCount-1 do
    // wat2test doesn't write the function section as relocatable
    // WriteRelocU32(m.GetFunc(i).functype.typeNum);
    WriteU32(dst, module.GetFunc(i).functype.typeNum);

  SectionEnd(sc);
end;

procedure TBinWriter.WriteExportSect;
var
  sc : TSectionRec;
  i  : integer;
  x  : TWasmExport;
begin
  SectionBegin(SECT_EXPORT, sc);

  WriteU32(dst, module.ExportCount);
  for i:=0 to module.ExportCount-1 do begin
    x:=module.GetExport(i);
    WriteU32(dst, length(x.name));
    if length(x.name)>0 then
      dst.Write(x.name[1], length(x.name));
    dst.WriteByte(x.exportType);

    //wat2wasm doesn't write relocate the information
    //WriteRelocU32(x.exportNum);
    WriteU32(dst, x.exportNum);
  end;

  SectionEnd(sc);
end;


procedure TBinWriter.WriteCodeSect;
var
  sc    : TSectionRec;
  i, j  : integer;
  sz    : int64;
  mem   : TMemoryStream;
  la    : TLocalInfoArray;
  f     : TWasmFunc;
  dofs  : Int64;
  p     : Int64;
begin
  SectionBegin(SECT_CODE, sc);

  mem:=TMemoryStream.Create;
  try
    WriteU32(dst, module.FuncCount);
    for i :=0 to module.FuncCount-1 do begin
      f:=module.GetFunc(i);

      GetLocalInfo(f, la);

      mem.Position:=0;
      dofs:=dst.Position+5; // "la" will be written after, 5 is for the writeSize. +5 is for WriteRelocU32(sz)
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

procedure TBinWriter.WriteLinkingSect;
var
  sc : TSectionRec;
  mem : TMemoryStream;
  so  : TSymbolObject;
begin
  SectionBegin(SECT_CUSTOM, sc);
  WriteString(SectionName_Linking);

  WriteU32(dst, LINKING_VERSION);
  if symCount>0 then begin
    dst.WriteByte(WASM_SYMBOL_TABLE);
    mem := TMemoryStream.Create;
    try
      pushStream(mem);
      //WriteU32(dst, symCount);
      WriteRelocU32(symCount);
      so:=symHead;
      while Assigned(so) do begin
        dst.WriteByte(so.syminfo.kind);
        WriteU32(dst, so.syminfo.flags);
        WriteU32(dst, so.syminfo.symindex);
        //if ((so.syminfo.flags and WASM_SYM_EXPLICIT_NAME)>0) then begin
          WriteU32(dst, length(so.syminfo.symname));
          dst.Write(so.syminfo.symname[1], length(so.syminfo.symname));
        //end;
        so:=so.next;
      end;
      popStream;

      mem.Position:=0;
      WriteU32(dst, mem.Size);
      dst.CopyFrom(mem, mem.size);
    finally
      mem.Free;
    end;
  end;
  // todo: fill out subsections

  SectionEnd(sc);
end;

procedure TBinWriter.WriteRelocSect;
var
  i  : integer;
  j  : integer;
  si : Byte;
  cnt: integer;
  sc : TSectionRec;
begin
  si:=0;
  i:=0;

  while (si<writeSec) and (i<relocCount) do begin
    if reloc[i].sec=si then begin
      SectionBegin(SECT_CUSTOM, sc);
      WriteString(SectionNamePfx_Reloc+'Code');
      j:=i;
      cnt:=0;
      while (i<relocCount) and (reloc[i].sec=si) do begin
        inc(cnt);
        inc(i);
      end;
      WriteU32(dst, reloc[j].sec);
      WriteU32(dst, cnt);
      for j:=j to i-1 do begin
        dst.WriteByte(reloc[j].reltype);
        WriteU32(dst, reloc[j].offset);
        WriteU32(dst, reloc[j].index);
      end;
      SectionEnd(sc);
    end;
    inc(si);
  end;
end;

function WriteI32Operand(dst: TStream; const operand: string): Boolean;
var
  err : integer;
  i32 : integer;
  u32 : LongWord;
begin
  Val(operand, i32, err);
  if err = 0 then
    u32 := LongWord(i32)
  else
    Val(operand, u32, err);

  Result := (err = 0);
  if Result then WriteU32(dst, u32);
end;

function WriteI64Operand(dst: TStream; const operand: string): Boolean;
var
  err : integer;
  i64 : Int64;
  u64 : UInt64;
begin
  Val(operand, i64, err);
  if err = 0 then begin
    WriteS64(dst, i64);
    Result := true;
    //Exit;
    //u64 := UInt64(i64)
  end else begin
    Val(operand, u64, err);
    if Result then WriteS64(dst, Int64(u64));
  end;
  Result := (err = 0);
end;


procedure TBinWriter.WriteInstList(list: TWasmInstrList; ofsAddition: LongWord);
var
  i  : integer;
  ci : TWasmInstr;
  idx : integer;
begin
  for i:=0 to list.Count-1 do begin
    ci :=list[i];
    dst.WriteByte(ci.code);
    case INST_FLAGS[ci.code].Param of
      ipi32: begin
        WriteI32Operand(dst, ci.operandText);
      end;

      ipi64: begin     // signed Leb of maximum 8 bytes
        WriteI64Operand(dst, ci.operandText);
      end;

      ipi32OrFunc: begin
        if (ci.operandText<>'') and (ci.operandText[1]='$') then begin
          idx := FindFunc(module, ci.operandText);
          AddReloc(INST_RELOC_FLAGS[ci.code].relocType, dst.Position+ofsAddition, idx);
          //todo: there's no need
          WriteRelocU32(LongWord(idx));
        end else
          WriteI32Operand(dst, ci.operandText);
      end;
      //ipf32,     // float point single
      //ipf64,     // float point double


      ipLeb:
      begin
        if INST_RELOC_FLAGS[ci.code].doReloc then begin
          AddReloc(INST_RELOC_FLAGS[ci.code].relocType, dst.Position+ofsAddition, ci.operandNum);
          WriteRelocU32(ci.operandNum)
        end
        else
          WriteU32(dst, ci.operandNum);
      end;

      ipCallType:
      begin
        if Assigned(ci.insttype) then begin
          if INST_RELOC_FLAGS[ci.code].doReloc then begin
            AddReloc(INST_RELOC_FLAGS[ci.code].relocType, dst.Position+ofsAddition, ci.insttype.typeNum);
            WriteRelocU32(ci.insttype.typeNum);
          end
          else
            WriteU32(dst, ci.insttype.typeNum);
        end else
          WriteU32(dst, LongWord(-1)); // this is an error.

        // table index reference
        WriteU32(dst, ci.operandNum);
      end;
    end;
  end;
end;

procedure TBinWriter.WriteImportSect;
var
  sc : TSectionRec;
  i  : integer;
  im : TWasmImport;
begin
  SectionBegin(SECT_IMPORT, sc);

  WriteU32(dst, module.ImportCount);
  for i:=0 to module.ImportCount-1 do begin
    im:=module.GetImport(i);

    WriteString(im.module);
    WriteString(im.name);
    if Assigned(im.fn) then begin
      dst.WriteByte(IMPDESC_FUNC);
      WriteU32(dst, im.fn.functype.typeNum);
    end;
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

function isFuncLinkSym(const l: TLinkInfo): boolean;
begin
  Result:=(l.Binding<>lbUndefined)
    or l.isHidden
    or l.isUndefined
    or l.NoStrip;
end;

procedure LinkInfoToBin(const src: TLinkInfo; var dst: TSymInfo; ASymTab: byte; aofs: longword);
begin
  dst.kind := ASymTab;
  dst.flags := 0;
  case src.Binding of
    lbWeak: dst.flags := dst.flags or WASM_SYM_BINDING_WEAK;
    lbLocal: dst.flags := dst.flags or WASM_SYM_BINDING_LOCAL;
    lbForHost: dst.flags := dst.flags or WASM_SYM_EXPORTED;
  end;
  if src.isHidden then dst.flags := dst.flags or WASM_SYM_VISIBILITY_HIDDEN;
  if src.isUndefined then dst.flags := dst.flags or  WASM_SYM_UNDEFINED;
  if src.NoStrip then dst.flags := dst.flags or WASM_SYM_NO_STRIP;
  dst.symindex := aofs;
  dst.hasSymIndex := ASymTab<>SYMTAB_DATA;
  dst.hasSymName := src.Name<>'';
  if (dst.hasSymName) then begin
    dst.flags := dst.flags or WASM_SYM_EXPLICIT_NAME;
    dst.symname := src.Name;
  end;
end;

procedure TBinWriter.PrepareLinkSym(m: TWasmModule);
var
  i   : integer;
  f   : TWasmFunc;
  so  : TSymbolObject;
begin
  for i:=0 to m.FuncCount-1 do begin
    f := m.GetFunc(i);
    if isFuncLinkSym(f.LinkInfo) or (f.codeRefCount>0) then begin
      if f.LinkInfo.Name ='' then f.LinkInfo.Name := f.id;
      so:=AddSymbolObject;
      LinkInfoToBin(f.linkInfo, so.syminfo, SYMTAB_FUNCTION, f.idNum);
    end;
  end;
end;

end.

