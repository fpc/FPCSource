{
    Copyright (c) 2021 by Nikolay Nikolov

    Contains WebAssembly binary module format definitions

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit wasmbase;

{$i fpcdefs.inc}

interface

const
  WasmModuleMagic: array [0..3] of byte = ($00,$61,$73,$6D);
  WasmVersion: array [0..3] of byte = ($01,$00,$00,$00);

type
  TWasmSectionID = (
    wsiCustom    = 0,
    wsiType      = 1,
    wsiImport    = 2,
    wsiFunction  = 3,
    wsiTable     = 4,
    wsiMemory    = 5,
    wsiGlobal    = 6,
    wsiExport    = 7,
    wsiStart     = 8,
    wsiElement   = 9,
    wsiCode      = 10,
    wsiData      = 11,
    wsiDataCount = 12,
    wsiTag       = 13);

  TWasmCustomSectionType = (
    wcstLinking,
    wcstRelocCode,
    wcstRelocData,
    wcstProducers,
    wcstTargetFeatures,

    wcstDebugFrame,
    wcstDebugInfo,
    wcstDebugLine,
    wcstDebugAbbrev,
    wcstDebugAranges,
    wcstDebugRanges,
    wcstDebugStr,

    wcstRelocDebugFrame,
    wcstRelocDebugInfo,
    wcstRelocDebugLine,
    wcstRelocDebugAbbrev,
    wcstRelocDebugAranges,
    wcstRelocDebugRanges,
    wcstRelocDebugStr);

const
  WasmCustomSectionName: array [TWasmCustomSectionType] of string =
    ('linking',
     'reloc.CODE',
     'reloc.DATA',
     'producers',
     'target_features',

     '.debug_frame',
     '.debug_info',
     '.debug_line',
     '.debug_abbrev',
     '.debug_aranges',
     '.debug_ranges',
     '.debug_str',

     'reloc..debug_frame',
     'reloc..debug_info',
     'reloc..debug_line',
     'reloc..debug_abbrev',
     'reloc..debug_aranges',
     'reloc..debug_ranges',
     'reloc..debug_str');

type
  TWasmRelocationType = (
    R_WASM_FUNCTION_INDEX_LEB  = 0,
    R_WASM_TABLE_INDEX_SLEB    = 1,
    R_WASM_TABLE_INDEX_I32     = 2,
    R_WASM_MEMORY_ADDR_LEB     = 3,
    R_WASM_MEMORY_ADDR_SLEB    = 4,
    R_WASM_MEMORY_ADDR_I32     = 5,
    R_WASM_TYPE_INDEX_LEB      = 6,
    R_WASM_GLOBAL_INDEX_LEB    = 7,
    R_WASM_FUNCTION_OFFSET_I32 = 8,
    R_WASM_SECTION_OFFSET_I32  = 9,
    R_WASM_TAG_INDEX_LEB       = 10,
    R_WASM_GLOBAL_INDEX_I32    = 13,
    R_WASM_MEMORY_ADDR_LEB64   = 14,
    R_WASM_MEMORY_ADDR_SLEB64  = 15,
    R_WASM_MEMORY_ADDR_I64     = 16,
    R_WASM_TABLE_INDEX_SLEB64  = 18,
    R_WASM_TABLE_INDEX_I64     = 19,
    R_WASM_TABLE_NUMBER_LEB    = 20);

  TWasmLinkingSubsectionType = (
    WASM_SEGMENT_INFO = 5,
    WASM_INIT_FUNCS   = 6,
    WASM_COMDAT_INFO  = 7,
    WASM_SYMBOL_TABLE = 8);

  TWasmSymbolType   = (
    SYMTAB_FUNCTION = 0,
    SYMTAB_DATA     = 1,
    SYMTAB_GLOBAL   = 2,
    SYMTAB_SECTION  = 3,
    SYMTAB_EVENT    = 4,
    SYMTAB_TABLE    = 5);

const
  { segment flags }
  WASM_SEG_FLAG_STRINGS = $01;
  WASM_SEG_FLAG_TLS     = $02;

  { symbol flags }
  WASM_SYM_BINDING_WEAK      = $01;
  WASM_SYM_BINDING_LOCAL     = $02;
  WASM_SYM_VISIBILITY_HIDDEN = $04;
  WASM_SYM_UNDEFINED         = $10;
  WASM_SYM_EXPORTED          = $20;
  WASM_SYM_EXPLICIT_NAME     = $40;
  WASM_SYM_NO_STRIP          = $80;
  WASM_SYM_TLS               = $100;

implementation

end.
