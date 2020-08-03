unit wasmtoolutils;

interface

uses
  Classes,SysUtils, wasmbin, lebutils,
  //wasmbindebug,
  wasmlink, wasmlinkchange;

function ReadStream(st: TStream): Boolean;
procedure ReadWasmFile(const fn: string);
function WriteStream(st: TStream; syms: TStrings): Boolean;
procedure ProcessWasmFile(const fn, symfn: string);
procedure RenameExport(var x: TExportSection; syms: TStrings);
function ProcessSections(st, dst: TStream; syms: TStrings): Boolean;
procedure ProcessWasmSection(const fn, {%H-}symfn: string);

implementation

function ReadStream(st: TStream): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  nm  : string;
begin
  dw := st.ReadDWord;
  Result := dw = WasmId_Int;
  if not Result then begin
    writeln('not a wasm file');
    Exit;
  end;
  dw := st.ReadDWord;
  writeln('version: ', dw);
  while st.Position<st.Size do begin
    ofs := st.Position;
    sc.id := st.ReadByte;
    sc.Size := ReadU(st);
    writeln(ofs,': id=', sc.id,'(', SectionIdToStr(sc.id),') sz=', sc.size);

    ps := st.Position+sc.size;
    if sc.id=0 then begin
      nm := GetName(st);
      writeln(nm);
      if nm = SectionName_Linking then
        DumpLinking(st, sc.size - (st.Position - ofs));
    end;
    //if sc.id= 1 then DumpTypes(st);

    if st.Position <> ps then
    begin
      //writeln('adjust stream targ=',ps,' actual: ', st.position);
      st.Position := ps;
    end;
  end;
end;


procedure ReadWasmFile(const fn: string);
var
  fs :TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    ReadStream(fs);
  finally
    fs.Free;
  end;
end;

function WriteStream(st: TStream; syms: TStrings): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  nm  : string;
begin
  writeln('read: ');
  dw := st.ReadDWord;
  writeln('dw: ' ,dw);
  Result := dw = WasmId_Int;
  if not Result then begin
    writeln('not a wasm file');
    Exit;
  end;
  dw := st.ReadDWord;
  writeln('version: ', dw);
  while st.Position<st.Size do begin
    ofs := st.Position;
    sc.id := st.ReadByte;
    sc.Size := ReadU(st);
    writeln(ofs,': id=', sc.id,'(', SectionIdToStr(sc.id),') sz=', sc.size);

    ps := st.Position+sc.size;
    if sc.id=0 then begin
      nm := GetName(st);
      writeln(nm);
      if nm = SectionName_Linking then begin
        writeln('rewriting linking...');
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

procedure ProcessWasmFile(const fn, symfn: string);
var
  fs :TFileStream;
  syms:  TStringList;
begin
  writeln('proc: ', fn);
  syms:=TStringList.Create;
  fs := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone);
  try
    if (symfn<>'') then
      ReadSymbolsConf(symfn, syms);
    writeln('size: ', fs.size);
    WriteStream(fs, syms);
  finally
    fs.Free;
    syms.Free;
  end;
end;

procedure RenameExport(var x: TExportSection; syms: TStrings);
var
  i : integer;
  v : string;
begin
  for i:=0 to length(x.entries)-1 do begin
    v := syms.Values[x.entries[i].name];
    if v <> '' then
      x.entries[i].name := v;
  end;
end;

function ProcessSections(st, dst: TStream; syms: TStrings): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  x   : TExportSection;
  i   : integer;
  mem : TMemoryStream;
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
    writeln(ofs,': id=', sc.id,'(', SectionIdToStr(sc.id),') sz=', sc.size);

    ps := st.Position+sc.size;

    if sc.id = SECT_EXPORT then begin
      ReadExport(st, x);
      RenameExport(x, syms);

      st.Position:=0;
      dst.CopyFrom(st, ofs);
      st.Position:=ps;

      mem := TMemoryStream.Create;
      WriteExport(x, mem);
      mem.Position:=0;

      dst.WriteByte(SECT_EXPORT);
      WriteU32(dst, mem.Size);
      dst.CopyFrom(mem, mem.Size);

      writeln('entries = ', length(x.entries));
      for i:=0 to length(x.entries)-1 do begin
        writeln(x.entries[i].desc,' ', x.entries[i].name)
      end;

      dst.CopyFrom(st, st.Size-st.Position);
      break; // done
    end;
    {if sc.id=0 then begin
      nm := GetName(st);
      writeln(nm);
      if nm = SectionName_Linking then begin
        writeln('rewriting linking...');
        ProcessLinkingSection(st, syms);
        break;
      end;
        //DumpLinking(st, sc.size - (st.Position - ofs));
    end;}
    //if sc.id= 1 then DumpTypes(st);

    if st.Position <> ps then
    begin
      //writeln('adjust stream targ=',ps,' actual: ', st.position);
      st.Position := ps;
    end;
  end;
end;

procedure ProcessWasmSection(const fn, {%H-}symfn: string);
var
  fs    : TFileStream;
  syms  : TStringList;
  dst   : TMemoryStream;
begin
  writeln('proc: ', fn);
  syms:=TStringList.Create;
  fs := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone);
  dst := TMemoryStream.Create;
  try
    if (symfn <> '') and fileExists(symfn) then
      syms.LoadFromFile(symfn);

    ProcessSections(fs, dst, syms);
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
