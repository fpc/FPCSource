{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Sven Barth
    member of the Free Pascal development team

    Contains various RTTI related, private declarations that are used inside
    the system unit before rtti.inc might be included.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  {$define USE_PACKED}
{$endif}
{$ifdef VER2_6}
  {$define USE_PACKED}
{$endif}

{$ifndef VER3_0}
{ 3.1.1 and newer also (re)stores $MinEnumSize and $PackSet upon $Push/$Pop }
{$push}
{$endif}

{$MINENUMSIZE 1   this saves a lot of memory }
{$ifdef FPC_RTTI_PACKSET1}
{ for Delphi compatibility }
{$packset 1}
{$endif}

type
  PTypeKind = ^TTypeKind;

const
  // all potentially managed types
  tkManagedTypes   = [tkAstring,tkWstring,tkUstring,tkArray,
                     tkObject,tkRecord,tkDynArray,tkInterface,tkVariant];

type
  PRecordElement=^TRecordElement;
  TRecordElement=
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
    {$ifdef VER3_0}
    TypeInfo: Pointer;
    {$else}
    TypeInfo: PPointer;
    {$endif}
    {$ifdef VER2_6}
    Offset: Longint;
    {$else}
    Offset: SizeInt;
    {$endif}
  end;

  PRecordInfoFull=^TRecordInfoFull;
  TRecordInfoFull=
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
{$ifndef VER3_0}
    InitTable: Pointer;
{$endif VER3_0}
    Size: Longint;
    Count: Longint;
    { Elements: array[count] of TRecordElement }
  end;

  PRecordInfoInit=^TRecordInfoInit;
{$ifndef VER3_0}
  TRTTIRecVarOp=procedure(ARec: Pointer);
  TRTTIRecCopyOp=procedure(ASrc, ADest: Pointer);
  TRTTIRecOpType=(rotAny, rotInitialize, rotFinalize, rotAddRef, rotCopy);
  PRTTIRecordOpVMT=^TRTTIRecordOpVMT;
  TRTTIRecordOpVMT=
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
    Initialize: TRTTIRecVarOp;
    Finalize: TRTTIRecVarOp;
    AddRef: TRTTIRecVarOp;
    Copy: TRTTIRecCopyOp;
  end;

  TRTTIRecordOpOffsetEntry =
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
    ManagmentOperator: CodePointer;
    FieldOffset: SizeUInt;
  end;

  TRTTIRecordOpOffsetTable =
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
    Count: LongWord;
    Entries: array[0..0] of TRTTIRecordOpOffsetEntry;
  end;
  PRTTIRecordOpOffsetTable = ^TRTTIRecordOpOffsetTable;

  TRecordInfoInit=
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
    Terminator: Pointer;
    Size: Longint;
{$ifndef VER3_0}
    InitRecordOpTable: PRTTIRecordOpOffsetTable;
    RecordOp: PRTTIRecordOpVMT;
{$endif VER3_0}
    Count: Longint;
    { Elements: array[count] of TRecordElement }
  end;
{$else VER3_0}
  TRecordInfoInit=TRecordInfoFull;
{$endif VER3_0}

  PArrayInfo=^TArrayInfo;
  TArrayInfo=
{$ifdef USE_PACKED}
  packed
{$endif USE_PACKED}
  record
    Size: SizeInt;
    ElCount: SizeInt;
{$ifdef VER3_0}
    ElInfo: Pointer;
{$else}
    ElInfo: PPointer;
{$endif}
    DimCount: Byte;
    Dims:array[0..255] of Pointer;
  end;


{$ifndef VER3_0}
function RTTIRecordMopInitTable(ti: Pointer): PRTTIRecordOpOffsetTable; forward;
{$endif VER3_0}

{$ifdef VER3_0}
{$MINENUMSIZE DEFAULT}
{$PACKSET DEFAULT}
{$else}
{$pop}
{$endif}
