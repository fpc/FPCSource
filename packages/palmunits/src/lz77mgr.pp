{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2001 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Lz77Mgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * History:
 *    11/01/99 Created by Michel Turcotte
 *             Initial revision based on InetLib
 *
 *****************************************************************************)

unit lz77mgr;

interface

uses palmos, libtraps, errorbase, systemresources;

//
// Common PalmOS and Windows section
//

const
  Lz77VerID              = 1;
  Lz77LastSupportedVerID = 1;

  lz77Compress           = True;
  lz77Expand             = False;

type
  Lz77ErrorType = Err;

(********************************************************************
 * Error codes
 ********************************************************************)

const
  lz77Success                          = $00;
  // Non Fatal Errors
  lz77ErrNonFatalFirstErr              = lz77ErrorClass or $00;
  lz77ErrNonFatalInputBufferIncomplete = lz77ErrorClass or $01;
  lz77ErrNonFatalOutputBufferFull      = lz77ErrorClass or $02;
  lz77ErrNonFatalLastErr               = lz77ErrorClass or $7F;
  // Fatal Errors
  lz77ErrFatalFirstErr                 = lz77ErrorClass or $80;
  lz77ErrFatalUnfinishedInputBuffer    = lz77ErrorClass or $80;
  lz77ErrFatalInputBufferIncomplete    = lz77ErrorClass or $81;
  lz77ErrFatalInputBufferInvalid       = lz77ErrorClass or $82;
  lz77ErrFatalMemAllocation            = lz77ErrorClass or $83;
  lz77ErrFatalHandleInvalid            = lz77ErrorClass or $84;
  lz77ErrFatalCantChangeToCompress     = lz77ErrorClass or $85;
  lz77ErrFatalUnknownVersion           = lz77ErrorClass or $86;
  lz77ErrFatalOutputBufferTooSmall     = lz77ErrorClass or $87;
  lz77ErrFatalInvalidArgument          = lz77ErrorClass or $88;
  lz77ErrFatalLastErr                  = lz77ErrorClass or $FF;

function lz77ErrIsFatal(err: Lz77ErrorType): Boolean;

//
// Specific PalmOS section
//

// Creator. Used for both the database that contains the LZ77 Library and
//  it's features for the feature manager.
const
  lz77Creator = sysFileCLz77Lib; // Lz77 Library creator
  lz77LibName = 'Lz77.lib';      // pass in to SysLibFind()

(********************************************************************
 * LZ77 Library functions.
 ********************************************************************)

const
  lz77LibTrapChunk         = sysLibTrapCustom;
  lz77LibTrapMaxBufferSize = sysLibTrapCustom + 1;
  lz77LibTrapBufferGetInfo = sysLibTrapCustom + 2;
  lz77LibTrapBufferSetInfo = sysLibTrapCustom + 3;

//--------------------------------------------------
// Library initialization, shutdown, sleep and wake
//--------------------------------------------------

function Lz77LibOpen(
  libRefnum:        UInt16;    // Palm OS reference calling number
  var lz77HandleP:  MemHandle; // <-  Pointer to returning LZ77 handle (NULL for error)
  compressFlag:     Boolean;   // ->  TRUE = Compress; FALSE = Expand
  sourceSize:       UInt32;    // ->  Source size in bytes
  var destHP:       MemHandle; // <-> If (*destHP != NULL) => use pre allocated memory
                               //     (*destHP and *destSizeP)
                               //     If (*destHP == NULL) => allocate memory in *destHP
  var destSizeP:    UInt32;    // <-> If (*destSizeP ==0) THEN *destP must be NULL
                               //     => Lz77Open will calculate maximum buffer size
                               //     based on compressFlag and sourceSize
                               //     If (*destSizeP !=0) THEN it indicate
                               //     the size in bytes of the destination buffer
  useVerNum:        UInt16;    // ->  if (useVerNum !=0) THEN Use Version numbering
                               //     (Compress will write the value useVerNum in the
                               //      output buffer Expand will verify if the Version
                               //      in the source buffer is compatible)
  var primerP:      UInt8;     // ->  if (compressFlag ==lz77Compress)
                               //          UncompressPrimer buffer pointer
                               //     else CompressPrimer buffer pointer
                               //          Must be valid compressed lz77 data
                               //          compressed without a primer.
                               //     NULL means no primer
  primerL:          UInt32;    // ->  Byte length of primer
  processedPrimerL: UInt32     // ->  Byte length of processed primer
  ): Err; syscall sysLibTrapOpen;
   // Note: The output buffer must be large enough to include the emtire processed primer.
   //       When Expanding, the compressed primer is passed to the Open routine and
   //       the output buffer must be large enough to contain the expanded primer.

function Lz77LibClose(
  libRefnum:          UInt16;    // Palm OS reference calling number
  lz77Handle:         MemHandle; // ->  Lz77 Handle
  var ResultingSizeP: UInt32     // <-  Size in bytes of output generated buffer
                                 // Output buffer will be resized to the resulting size
                                 // if Lz77Open have allocated the output buffer.
                                 // Output buffer must be free by the calling application
  ): Err; syscall sysLibTrapClose;

function Lz77LibSleep(libRefnum: UInt16): Err; syscall sysLibTrapSleep;

function Lz77LibWake(libRefnum: UInt16): Err; syscall sysLibTrapWake;

function Lz77LibChunk(
  libRefnum:               UInt16;    // Palm OS reference calling number
  lz77Handle:              MemHandle; // ->  Lz77 Handle
  var sourceP:             Int8;      // ->  Source buffer pointer
  sourceSize:              UInt32;    // ->  Source buffer Size (bytes)
  var sourceBitReadOffset: UInt32     // <-> Next bit to read from source
  ): Err; syscall lz77LibTrapChunk;

function Lz77LibMaxBufferSize(
  libRefnum:          UInt16;  // Palm OS reference calling number
  compressFlag:       Boolean; // -> TRUE = Compress; FALSE = Expand
  sourceSize:         UInt32;  // -> Size of Source buffer
  var maxBufferSizeP: UInt32   // <- result size pointer
  ): Err; syscall lz77LibTrapMaxBufferSize;

function Lz77LibBufferGetInfo(
  libRefnum:           UInt16;    // Palm OS reference calling number
  lz77Handle:          MemHandle; // ->  Lz77 Handle
  var compressFlagP:   Boolean;   // <-  Get compressFlag (true = compress mode; false = expand mode)
  var bufferHP:        MemHandle; // <-  Get the Pointer to the accumulated destination buffer
  var bufferByteSizeP: UInt32;    // <-  Get destination buffer size in bytes
  var destBitOffsetP:  UInt32     // <-  Get destination bit offset
  ): Err; syscall lz77LibTrapBufferGetInfo;

function Lz77LibBufferSetInfo(
  libRefnum:     UInt16;    // Palm OS reference calling number
  lz77Handle:    MemHandle; // ->  Lz77 Handle
  compressFlag:  Boolean;   // ->  Set compressFlag (true = compress mode; false = expand mode)
  destH:         MemHandle; // ->  Set a Pointer to the accumulated destination buffer
  destByteSize:  UInt32;    // ->  Set destination buffer size in bytes
  destBitOffset: UInt32     // ->  Set destination bit offset
  ): Err; syscall lz77LibTrapBufferSetInfo;

implementation

function lz77ErrIsFatal(err: Lz77ErrorType): Boolean;
begin
  lz77ErrIsFatal := (err <> lz77Success) and ((err < lz77ErrNonFatalFirstErr) or (err > lz77ErrNonFatalLastErr));
end;

end.
