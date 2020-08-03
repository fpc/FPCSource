unit wasmbin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
  sect_custom   = 0;	// custom section
  sect_type     = 1;	// type section
  sect_import   = 2;	// import section
  sect_function = 3;	// function section
  sect_table    = 4;	// table section
  sect_memory   = 5;	// memory section
  sect_global   = 6;	// global section
  sect_export   = 7;	// export section
  sect_start    = 8;	// start section
  sect_element  = 9;	// element section
  sect_code     = 10;	// code section
  sect_data     = 11;	// data section

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

function SectionIdToStr(id: integer): string;
function ValTypeToStr(id: integer): string;

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

end.

