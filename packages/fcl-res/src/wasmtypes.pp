{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by Nikolay Nikolov

    Types used by the WebAssembly resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit wasmtypes;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC}

interface

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

  TWasmCustomDebugSectionType = wcstDebugFrame..wcstDebugStr;

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

  PWasmRelocationEntry = ^TWasmRelocationEntry;
  TWasmRelocationEntry = record
    Typ: TWasmRelocationType;
    Offset: UInt32;
    Index: UInt32;
    Addend: Int32;
  end;

  TWasmResourceDataSegment = (wrdsResources, wrdsResHandles);

implementation

end.
