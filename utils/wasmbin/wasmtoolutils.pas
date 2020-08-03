unit wasmtoolutils;

interface

uses
  Classes,SysUtils, wasmbin, lebutils,
  //wasmbindebug,
  wasmlink, wasmlinkchange;

function ChangeSymbolFlagStream(st: TStream; syms: TStrings): Boolean;
procedure ChangeSymbolFlag(const fn, symfn: string);

procedure MatchExportNameToSymName(const x: TExportSection; const l: TLinkingSection; dst: TStrings);
function ExportRenameSym(var x: TExportSection; syms: TStrings): Integer;
function ExportRenameProcess(st, dst: TStream; syms: TStrings; doVerbose: Boolean): Boolean;
function ExportNameGather(const wasmfile: string; syms: TStrings; doVerbose: Boolean = false): Boolean;
procedure ExportRename(const fn, symfn: string; doVerbose: Boolean);

implementation

function ChangeSymbolFlagStream(st: TStream; syms: TStrings): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  nm  : string;
begin
  dw := st.ReadDWord;
  Result := dw = WasmId_Int;
  if not Result then Exit;

  dw := st.ReadDWord;
  while st.Position<st.Size do begin
    ofs := st.Position;
    sc.id := st.ReadByte;
    sc.Size := ReadU(st);

    ps := st.Position+sc.size;
    if sc.id=0 then begin
      nm := GetName(st);
      if nm = SectionName_Linking then begin
        ProcessLinkingSection(st, syms);
        break;
      end;
        //DumpLinking(st, sc.size - (st.Position - ofs));
    end;
    //if sc.id= 1 then DumpTypes(st);

    if st.Position <> ps then
    begin
      //writeln('adjust stream targ=',ps,' actual: ', st.position);
      st.Position := ps;
    end;
  end;
end;

// Assumption is made, there's only 1 table in the file!
// if a function is a stub function (the only code is "unreachable"), the status given
//  "weak" (it's a reference function elsewhere)
// if a function is located in the function table, then the status given is
//  "hidden" (do not add to the final linked executable)
// if a function is not located in the function table, the status given is:
//  "hidden"+"local" (local means the function can be used only in this object file)
procedure MatchExportNameToSymFlag(
  x: TExportSection;
  l: TLinkingSection;
  e: TElementSection;
  syms : TStrings)
begin
end;

function PredictSymbolsFromLink(const wasmfn: string; syms: TStrings; doVerbose: Boolean = false): Boolean;
var
  st : TFileStream;
  dw : LongWord;
  foundExport  : Boolean;
  foundElement : Boolean;
  foundLink    : Boolean;
  ofs : Int64;
  ps  : Int64;
  sc  : TSection;
  x   : TExportSection;
  l   : TLinkingSection;
  e   : TElementSection;
  cnt : Integer;
  nm  : string;
begin
  st := TFileStream.Create(wasmfn, fmOpenRead or fmShareDenyNone);
  try
    dw := st.ReadDWord;
    Result := dw = WasmId_Int;
    if not Result then begin
      Exit;
    end;
    dw := st.ReadDWord;

    foundElement:=false;
    foundExport:=false;
    foundLink:=false;
    while st.Position<st.Size do begin
      ofs := st.Position;
      sc.id := st.ReadByte;
      sc.Size := ReadU(st);

      ps := st.Position+sc.size;

      if sc.id = SECT_EXPORT then begin
        if doVerbose then writeln(' export section found');
        ReadExport(st, x);
        cnt := ExportRenameSym(x, syms);
        foundExport:=true;
      end else if sc.id = SECT_CUSTOM then begin
        nm := ReadName(st);
        if nm = SectionName_Linking then begin
          foundLink := true;
          ReadLinkingSection(st, sc.size, l);
        end;
      end;

      if st.Position <> ps then
        st.Position := ps;
    end;

    Result := foundLink and foundExport;
    if Result then
      MatchExportNameToSymFlag(x, l, syms);
  finally
    st.Free;
  end;
end;

procedure ChangeSymbolFlag(const fn, symfn: string);
var
  fs :TFileStream;
  syms:  TStringList;
begin
  syms:=TStringList.Create;
  fs := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone);
  try
    if (symfn<>'') then begin
      if not isWasmFile(symfn) then
        ReadSymbolsConf(symfn, syms)
      else begin
        PredictSymbolsFromLink(symfn, syms);
      end;
    end;
    ChangeSymbolFlagStream(fs, syms);
  finally
    fs.Free;
    syms.Free;
  end;
end;

function ExportRenameSym(var x: TExportSection; syms: TStrings): integer;
var
  i : integer;
  v : string;
begin
  Result := 0;
  for i:=0 to length(x.entries)-1 do begin
    v := syms.Values[x.entries[i].name];
    if v <> '' then begin
      x.entries[i].name := v;
      inc(Result);
    end;
  end;
end;

function ExportRenameProcess(st, dst: TStream; syms: TStrings; doVerbose: Boolean): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  x   : TExportSection;
  mem : TMemoryStream;
  cnt : integer;
begin
  dw := st.ReadDWord;
  Result := dw = WasmId_Int;
  if not Result then begin
    Exit;
  end;
  dw := st.ReadDWord;
  while st.Position<st.Size do begin
    ofs := st.Position;
    sc.id := st.ReadByte;
    sc.Size := ReadU(st);

    ps := st.Position+sc.size;

    if sc.id = SECT_EXPORT then begin
      if doVerbose then writeln(' export section found');
      ReadExport(st, x);
      cnt := ExportRenameSym(x, syms);
      writeln(' renamings: ', cnt);

      st.Position:=0;
      dst.CopyFrom(st, ofs);
      st.Position:=ps;

      mem := TMemoryStream.Create;
      WriteExport(x, mem);
      mem.Position:=0;

      dst.WriteByte(SECT_EXPORT);
      WriteU32(dst, mem.Size);
      dst.CopyFrom(mem, mem.Size);

      dst.CopyFrom(st, st.Size-st.Position);
      break;
    end;

    if st.Position <> ps then
      st.Position := ps;
  end;
end;

// match between exported function names and symbol names
procedure MatchExportNameToSymName(const x: TExportSection; const l: TLinkingSection;  dst: TStrings);
var
  expname : string;
  i,j     : integer;
begin
  for i:=0 to length(x.entries)-1 do begin
    // gathering only function names for now
    if x.entries[i].desc <> EXPDESC_FUNC then continue;
    expname := x.entries[i].name;
    for j:=0 to length(l.symbols)-1 do begin
      if (l.symbols[j].kind = SYMTAB_FUNCTION)
        and (l.symbols[j].symindex = x.entries[i].index)
        and (l.symbols[j].hasSymName)
      then
        dst.Values[ l.symbols[j].symname ] := expname;
    end;
  end;

end;

function ExportNameGather(const wasmfile: string; syms: TStrings; doVerbose: Boolean = false): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  mem : TMemoryStream;
  cnt : integer;
  st  : TFileStream;
  nm  : string;
  x   : TExportSection;
  foundExport: Boolean;
  l   : TLinkingSection;
  foundLink: Boolean;
begin
  st := TFileStream.Create(wasmfile, fmOpenRead or fmShareDenyNone);
  try
    dw := st.ReadDWord;
    Result := dw = WasmId_Int;
    if not Result then begin
      Exit;
    end;
    dw := st.ReadDWord;

    foundExport:=false;
    foundLink:=false;
    while st.Position<st.Size do begin
      ofs := st.Position;
      sc.id := st.ReadByte;
      sc.Size := ReadU(st);

      ps := st.Position+sc.size;

      if sc.id = SECT_EXPORT then begin
        if doVerbose then writeln(' export section found');
        ReadExport(st, x);
        cnt := ExportRenameSym(x, syms);
        foundExport:=true;
      end else if sc.id = SECT_CUSTOM then begin
        nm := ReadName(st);
        if nm = SectionName_Linking then begin
          foundLink := true;
          ReadLinkingSection(st, sc.size, l);
        end;
      end;

      if st.Position <> ps then
        st.Position := ps;
    end;

    Result := foundLink and foundExport;
    if Result then
      MatchExportNameToSymName(x, l, syms);
  finally
    st.Free;
  end;
end;

procedure ExportRename(const fn, symfn: string; doVerbose: Boolean);
var
  fs    : TFileStream;
  syms  : TStringList;
  dst   : TMemoryStream;
begin
  if doVerbose then writeln('Export symbols renaming');
  syms:=TStringList.Create;
  fs := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone);
  dst := TMemoryStream.Create;
  try
    if (symfn <> '') and fileExists(symfn) then
    begin
      if doVerbose then writeln('reading symbols: ', symfn);

      if isWasmFile(symfn) then
        ExportNameGather(symfn, syms, doVerbose)
      else
        syms.LoadFromFile(symfn);
      if doVerbose then write(syms.Text);
    end;

    ExportRenameProcess(fs, dst, syms, doVerbose);

    fs.Position:=0;
    dst.Position:=0;
    fs.CopyFrom(dst, dst.Size);
    fs.Size:=dst.Size;

  finally
    dst.Free;
    fs.Free;
    syms.Free;
  end;
end;

end.
