{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by Nikolay Nikolov

    Constants used by the WebAssembly resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit wasmconsts;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Resources.WebAssembly.Types;
{$ELSE FPC_DOTTEDUNITS}
uses
  wasmtypes;
{$ENDIF FPC_DOTTEDUNITS}

const
  WasmModuleMagic: array [0..3] of byte = ($00,$61,$73,$6D);
  WasmVersion: array [0..3] of byte = ($01,$00,$00,$00);

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

  WasmResourceDataSegmentNames: array [TWasmResourceDataSegment] of string = (
    'fpc.resources',
    'fpc.reshandles'
  );

implementation

end.
