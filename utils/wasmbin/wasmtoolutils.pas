unit wasmtoolutils;

interface

uses
  Classes,SysUtils, wasmbin, lebutils,
  //wasmbindebug,
  wasmlink, wasmlinkchange;

function ChangeSymbolFlagStream(st: TStream; syms: TStrings): Boolean;
procedure ChangeSymbolFlag(const fn, symfn: string);

procedure ExportRenameSym(var x: TExportSection; syms: TStrings);
function ExportRenameProcess(st, dst: TStream; syms: TStrings): Boolean;
procedure ExportRename(const fn, symfn: string);

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

procedure ChangeSymbolFlag(const fn, symfn: string);
var
  fs :TFileStream;
  syms:  TStringList;
begin
  syms:=TStringList.Create;
  fs := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone);
  try
    if (symfn<>'') then
      ReadSymbolsConf(symfn, syms);
    ChangeSymbolFlagStream(fs, syms);
  finally
    fs.Free;
    syms.Free;
  end;
end;

procedure ExportRenameSym(var x: TExportSection; syms: TStrings);
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

function ExportRenameProcess(st, dst: TStream; syms: TStrings): Boolean;
var
  dw  : LongWord;
  ofs : int64;
  sc  : TSection;
  ps  : int64;
  x   : TExportSection;
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

    ps := st.Position+sc.size;

    if sc.id = SECT_EXPORT then begin
      ReadExport(st, x);
      ExportRenameSym(x, syms);

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

procedure ExportRename(const fn, symfn: string);
var
  fs    : TFileStream;
  syms  : TStringList;
  dst   : TMemoryStream;
begin
  syms:=TStringList.Create;
  fs := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone);
  dst := TMemoryStream.Create;
  try
    if (symfn <> '') and fileExists(symfn) then
      syms.LoadFromFile(symfn);

    writeln('ExportRenameProcess');
    ExportRenameProcess(fs, dst, syms);

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
