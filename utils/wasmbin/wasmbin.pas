unit wasmbin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lebutils;

const
  valtype_i32   = $7f;
  valtype_i64   = $7e;
  valtype_f32   = $7d;
  valtype_f64   = $7C;

  block_type    = $40;
  func_type     = $60;
  elem_type     = $70;

  global_const  = $00;
  global_mut    = $01;

  limit_min_inf = $00; // minimum - to infinity
  limit_min_max = $01; // minimum - maximum

const
  WasmId = #0'asm';
  WasmId_Int = $6D736100;

const
  SECT_CUSTOM   = 0;	// custom section
  SECT_TYPE     = 1;	// type section
  SECT_IMPORT   = 2;	// import section
  SECT_FUNCTION = 3;	// function section
  SECT_TABLE    = 4;	// table section
  SECT_MEMORY   = 5;	// memory section
  SECT_GLOBAL   = 6;	// global section
  SECT_EXPORT   = 7;	// export section
  SECT_START    = 8;	// start section
  SECT_ELEMENT  = 9;	// element section
  SECT_CODE     = 10;	// code section
  SECT_DATA     = 11;	// data section

type
  TSection = packed record
    id   : byte;
    size : LongWord; // it is Leb128 encoded in the file, thus cannot be read directly
  end;

  TFuncType = record
    param  : array of byte;
    result : array of byte;
  end;

  TFuncTypeArray = record
    funTypes : array of TFuncType;
  end;

  TCodeLocalEntry = record
    count  : LongWord;
    valtyp : Byte;
  end;

  TCodeInstr = record
    inst : byte;
    idxArr: array of LongWord;
    case byte of
      0: (align, offset : LongWord);
      1: (index: LongWord);
      2: (i32: LongWord);
      3: (i64: UInt64);
      4: (f32: single);
      5: (f64: double);
      // for labels
      6: (idxCount: integer;
          idxDef  :LongWord);
      7: (returnType: byte);
  end;

  TCodeEntry = record
    locals    : array of TCodeLocalEntry;
    instCount : integer;
    instr     : array of TCodeInstr;
  end;

  TCodeSection = record
    entries : array of TCodeEntry;
  end;


  TExportEntry = record
    name    : string;
    desc    : byte;
    index   : UInt32;
  end;

  TExportSection = record
    entries : array of TExportEntry;
  end;

function SectionIdToStr(id: integer): string;
function ValTypeToStr(id: integer): string;

// reads the name from the input stream
// the name consists of
//    size - in butes Leb128
//    bytes - in utf8 format
function ReadName(st: TStream): string;
procedure WriteName(st: TStream; const str: string);
function GetName(sr: TStream): string;

// reads
function GetU32(sr: TStream): UInt32;

// reads the code entry into TCodeEntry structure
procedure ReadCodeEntry(src: TStream; var en: TCodeEntry);
// reads the code entry into TCodeEntry structure
procedure ReadCodeSection(src: TStream; var sc: TCodeSection);


procedure ReadExportEntry(src: TStream; var ex: TExportEntry);
// reads the export entry
procedure ReadExport(src: TStream; var ex: TExportSection);
procedure WriteExport(const ex: TExportSection; dst: TStream);

implementation

function ValTypeToStr(id: integer): string;
begin
  case id of
    valtype_i32 : Result := 'i32';
    valtype_i64 : Result := 'i64';
    valtype_f32 : Result := 'f32';
    valtype_f64 : Result := 'f64';
  else
    Str(id, Result);
    Result := 'iUnk'+Result;
  end
end;

function SectionIdToStr(id: integer): string;
begin
  case id of
    sect_custom   : Result := 'custom';	// custom section
    sect_type     : Result := 'type';	// type section
    sect_import   : Result := 'import';	// import section
    sect_function : Result := 'function';	// function section
    sect_table    : Result := 'table';	// table section
    sect_memory   : Result := 'memory';	// memory section
    sect_global   : Result := 'global';	// global section
    sect_export   : Result := 'export';	// export section
    sect_start    : Result := 'start';	// start section
    sect_element  : Result := 'element';	// element section
    sect_code     : Result := 'code';	// code section
    sect_data     : Result := 'data';	// data section
  else
    Str(id, Result);
    Result := 'sect_unknown'+Result;
  end;

end;

function ReadName(st: TStream): string;
var
  ln : LongWord;
begin
  ln := ReadU(st);
  SetLength(result, ln);
  if ln>0 then st.Read(result[1], ln);
end;

procedure WriteName(st: TStream; const str: string);
begin
  WriteU32(st, length(str));
  if length(str)>0 then
    st.Write(str[1], length(str));
end;

function GetName(sr: TStream): string;
begin
  Result := ReadName(sr);
end;

function GetU32(sr: TStream): UInt32;
begin
  Result := UInt32(ReadU(sr));
end;

procedure ReadCodeEntry(src: TStream; var en: TCodeEntry);
var
  sz  : integer; // size in bytes
  //pos : int64;
  cnt : Integer;
  i   : integer;
begin
  sz := ReadU(src);

  cnt := ReadU(src);
  SetLength(en.locals, cnt);
  for i:=0 to cnt-1 do begin
    en.locals[i].count := ReadU(src);
    en.locals[i].valtyp := src.ReadByte;
  end;


end;

procedure ReadCodeSection(src: TStream; var sc: TCodeSection);
var
  cnt : integer;
  i   : integer;
begin
  cnt := ReadU(src);
  SetLength(sc.entries, cnt);
  for i:= 0 to cnt-1 do
    ReadCodeEntry(src, sc.entries[i]);
end;

procedure ReadExportEntry(src: TStream; var ex: TExportEntry);
begin
  ex.name := ReadName(src);
  ex.desc := src.ReadByte;
  ex.index := ReadU(src);
end;

procedure ReadExport(src: TStream; var ex: TExportSection);
var
  cnt : integer;
  i   : integer;
begin
  cnt := ReadU(src);
  SetLength(ex.entries, cnt);
  for i:=0 to cnt-1 do
    ReadExportEntry(src, ex.entries[i]);
end;

procedure WriteExport(const ex: TExportSection; dst: TStream);
var
  i : integer;
begin
  WriteU32(dst, length(ex.entries));
  for i:=0 to length(ex.entries)-1 do begin
    WriteName(dst, ex.entries[i].name);
    dst.WriteByte(ex.entries[i].desc);
    WriteU32(dst, ex.entries[i].index);
  end;
end;

end.

