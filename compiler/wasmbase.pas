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

implementation

end.
