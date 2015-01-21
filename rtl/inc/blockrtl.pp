 {
    This file is part of the Free Pascal run time library.
    Copyright (c) 20134 by Jonas Maebe,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit blockrtl;

{ for tmethod }
{$mode objfpc}

{$packrecords c}

interface

  uses
    initc,
    ctypes;

  { blocks helpers }
  function _Block_copy(const aBlock: pointer): pointer; cdecl; external;
  procedure _Block_release(const aBlock: pointer); cdecl; external;
  procedure _Block_object_assign(dst, src: pointer; const flags: cint); cdecl; external;
  procedure _Block_object_dispose(const aBlock: pointer; const flags: cint); cdecl; external;

  { blocks isa pointers }
  var
    _NSConcreteGlobalBlock: array[0..31] of pointer; cvar; external;
    _NSConcreteStackBlock: array[0..31] of pointer; cvar; external;

  { from Clang source, distributed under the University of Illinois Open Source
    License }

  { BlockByrefFlags }
  const
    BLOCK_BYREF_HAS_COPY_DISPOSE  =  1 shl 25;
    BLOCK_BYREF_LAYOUT_MASK       = $f shl 28;
    BLOCK_BYREF_LAYOUT_EXTENDED   =  1 shl 28;
    BLOCK_BYREF_LAYOUT_NON_OBJECT =  2 shl 28;
    BLOCK_BYREF_LAYOUT_STRONG     =  3 shl 28;
    BLOCK_BYREF_LAYOUT_WEAK       =  4 shl 28;
    BLOCK_BYREF_LAYOUT_UNRETAINED =  5 shl 28;

  { BlockLiteralFlags }
  const
    BLOCK_HAS_COPY_DISPOSE    = 1 shl 25;
    BLOCK_HAS_CXX_OBJ         = 1 shl 26;
    BLOCK_IS_GLOBAL           = 1 shl 28;
    BLOCK_USE_STRET           = 1 shl 29;
    BLOCK_HAS_SIGNATURE       = 1 shl 30;
    BLOCK_HAS_EXTENDED_LAYOUT = 1 shl 31;

  { BlockFieldFlag_t }
  const
    BLOCK_FIELD_IS_OBJECT = $03; { id, NSObject, __attribute__((NSObject)), block, ... }
    BLOCK_FIELD_IS_BLOCK  = $07; { a block variable }
    BLOCK_FIELD_IS_BYREF  = $08; { the on stack structure holding the __block variable }
    BLOCK_FIELD_IS_WEAK   = $10; { declared __weak, only used in byref copy helpers }
    BLOCK_FIELD_IS_ARC    = $40; { field has ARC-specific semantics }
    BLOCK_BYREF_CALLER    = 128; { called from __block (byref) copy/dispose support routines }
    BLOCK_BYREF_CURRENT_MAX = 256;

  type
    FPC_Block_literal_base = record
      isa: pointer; // initialized to &_NSConcreteStackBlock or &_NSConcreteGlobalBlock
      flags: cint;
      reserved: cint;
      { actually a function pointer, will be cast by the compiler }
      invoke: pointer;
      descriptor: pointer;
    end;


    { descriptor for a simple block (no copy/release) }
    FPC_Block_descriptor_simple = record
      reserved: culong;
      Block_size: culong;
      { signatures are only for the "ABI.2010.3.16" version, but that's all we support
        because otherwise the callback has to be a C-style variadic function, which
        we cannot (yet?) generate on the callee side}
      signature: pchar;
    end;

    { descriptor for a simple block (no copy/release) }
    FPC_Block_descriptor_complex = record
      reserved: culong;
      Block_size: culong;
      copy_helper: procedure(dst, src: pointer); cdecl;
      dispose_helper: procedure(block: pointer); cdecl;
      { signatures are only for the "ABI.2010.3.16" version, but that's all we support
        because otherwise the callback has to be a C-style variadic function, which
        we cannot (yet?) generate on the callee side}
      signature: pchar;
    end;

    { for global procedures }
    FPC_Block_literal_static = record
      base: FPC_Block_literal_base;
      { no local state }
    end;

    { for simple procedure variable (just an address) }
    FPC_Block_literal_simple_procvar = record
      base: FPC_Block_literal_base;
      { the procvar to call }
      pv: pointer;
    end;

    { for procedure of object, and in the future also nested procedures and
      maybe Objective-C methods }
    FPC_Block_literal_complex_procvar = record
      base: FPC_Block_literal_base;
      { the procvar to call }
      pv: tmethod;
    end;
    PFPC_Block_literal_complex_procvar = ^FPC_Block_literal_complex_procvar;

implementation

end.
