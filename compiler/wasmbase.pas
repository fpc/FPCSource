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
    wsiDataCount = 12);

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
    R_WASM_EVENT_INDEX_LEB     = 10,
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

implementation

end.
