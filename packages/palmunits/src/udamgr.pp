{$MACRO ON}
 (***********************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: UDAMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *              Unified Data Manager header file
 *          Define type and generic macro to access data
 *
 * History:
 *    Jun 08, 2000   Created by Alain BASTY
 *
 ************************************************************************)

unit udamgr;

interface

uses palmos, coretraps, errorbase, exgmgr;

 (***********************************************************************
 * Generic options flags
 ************************************************************************)

const
  kUDAEndOfReader = 1;
  kUDAMoreData    = 2;

 (***********************************************************************
 * Generic control
 ************************************************************************)

const
  kUDAReinitialize = 1;

 (***********************************************************************
 * Generic error codes
 ************************************************************************)

const
  udaErrControl = udaErrorClass or 1;

 (***********************************************************************
 * General types
 ************************************************************************)

type
  UDABufferSize = UInt16;

const
  kUDAZeroTerminatedBuffer = $FFFF;

 (***********************************************************************
 * Types of callback functions
 ************************************************************************)

type
  UDAObjectPtr = ^UDAObjectType;
  UDAReaderPtr = ^UDAReaderType;
  UDAWriterPtr = ^UDAWriterType;

  UDADeleteFunction = procedure (var ioObject: UDAObjectPtr);
//!!!  UDAControlFunction = function (ioObject: UDAObjectPtr; parameter: UInt16; args: va_list): Err;

  UDAReadFunction = function (ioReader: UDAReaderPtr; var buffer: UInt8; bufferSize: UDABufferSize; var error: Err): UDABufferSize;

  UDAWriteFunction = function (ioWriter: UDAWriterPtr): Err;
  UDAFlushFunction = function (ioWriter: UDAWriterPtr): Err;

  UDAObjectType = record
    optionFlags: UInt16;
    deleteF:     UDADeleteFunction;
    controlF:    Pointer; //!!!UDAControlFunction;
  end;
  UDAObjectTag = UDAObjectType;

  UDAReaderType = record
    // The Reader is a base object
    optionFlags: UInt16 ;
    deleteF:  UDADeleteFunction;
    controlF: Pointer; //!!!UDAControlFunction;

    // Specific Reader fields
    readF: UDAReadFunction;
  end;
  UDAReaderTag = UDAReaderType;

  UDAFilterType = record
    // The Filter is a base Object
    optionFlags: UInt16;
    deleteF: UDADeleteFunction;
    controlF: Pointer; //!!!UDAControlFunction;

    // The Filter is a Reader
    readF: UDAReadFunction;

    // Specific Filter fields
    upperReader: ^UDAReaderType;
  end;
  UDAFilterTag = UDAFilterType;

  UDAWriterType = record
    // The Writer is a base Object
    optionFlags: UInt16;
    deleteF: UDADeleteFunction;
    controlF: Pointer; //!!!UDAControlFunction;

    // Specific Writer fields
    initiateWriteF: UDAWriteFunction;
    flushF: UDAFlushFunction;
    upperReader: ^UDAReaderType;
  end;
  UDAWriterTag = UDAWriterType;

 (***********************************************************************
 * Generic macro to access generic functions
 ************************************************************************)

(*
#define UDADelete(ioObject) (( *(ioObject->deleteF))((UDAObjectType** )(&(ioObject))))

#define UDARead(ioReader, bufferToFillP, bufferSizeInBytes, error) (( *(ioReader->readF))((UDAReaderType* )(ioReader), (bufferToFillP), (bufferSizeInBytes), (error)))

#define UDAEndOfReader(ioReader) (((ioReader)->optionFlags & kUDAEndOfReader) != 0)

#define UDAMoreData(ioReader) (((ioReader)->optionFlags & kUDAMoreData) != 0)

#define UDAFilterJoin(ioFilter, ioReader) (((UDAFilterType* )(ioFilter))->upperReader = ioReader)

#define UDAWriterJoin(ioWriter, ioReader) (ioWriter->upperReader = ioReader)

#define UDAInitiateWrite(ioWriter) (( *(ioWriter)->initiateWriteF))(ioWriter)

#define UDAWriterFlush(ioWriter) (( *(ioWriter)->flushF))(ioWriter)
*)

(*****************************************************************
 * UDA API
 ****************************************************************)

// Public UDAMgr function selectors
const
  sysUdaControl           = 0;
  sysUdaMemoryReaderNew   = 1;
  sysUdaExchangeReaderNew = 11;
  sysUdaExchangeWriterNew = 12;

// UDAMgr function prototypes

//!!! function UDAControl(var ioObject: UDAObjectType; parameter: UInt16, ...): Err; syscall sysTrapUdaMgrDispatch, sysUdaControl;

function UDAExchangeReaderNew(var socket: ExgSocketType): UDAReaderPtr;

function UDAExchangeWriterNew(var socket: ExgSocketType; bufferSize: UDABufferSize): UDAWriterPtr;

 (***********************************************************************
 * Memory reader
 ************************************************************************)

function UDAMemoryReaderNew(var bufferP: UInt8; bufferSizeInBytes: UDABufferSize): UDAReaderPtr;

implementation

function __UDAExchangeReaderNew(var socket: ExgSocketType): UDAReaderPtr; syscall sysTrapUdaMgrDispatch;
function __UDAExchangeWriterNew(var socket: ExgSocketType; bufferSize: UDABufferSize): UDAWriterPtr; syscall sysTrapUdaMgrDispatch;
function __UDAMemoryReaderNew(var bufferP: UInt8; bufferSizeInBytes: UDABufferSize): UDAReaderPtr; syscall sysTrapUdaMgrDispatch;

function UDAExchangeReaderNew(var socket: ExgSocketType): UDAReaderPtr;
begin
 asm
  move.l #$sysUdaExchangeReaderNew, D2;
 end;
 UDAExchangeReaderNew := __UDAExchangeReaderNew(socket);
end;

function UDAExchangeWriterNew(var socket: ExgSocketType; bufferSize: UDABufferSize): UDAWriterPtr;
begin
 asm
  move.l #$sysUdaExchangeWriterNew, D2;
 end;
 UDAExchangeWriterNew := __UDAExchangeWriterNew(socket, bufferSize);
end;

function UDAMemoryReaderNew(var bufferP: UInt8; bufferSizeInBytes: UDABufferSize): UDAReaderPtr;
begin
 asm
  move.l #$sysUdaMemoryReaderNew, D2;
 end;
 UDAMemoryReaderNew := __UDAMemoryReaderNew(bufferP, bufferSizeInBytes);
end;

end.
