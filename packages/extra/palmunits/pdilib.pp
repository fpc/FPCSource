{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PdiLib.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *              Public API of versit lib
 *
 * History:
 *    Jan 19, 2000   Created by Alain BASTY
 *
 *****************************************************************************)

unit pdilib;

interface

uses palmos, libtraps, errorbase, textmgr, udamgr, pdiconst;

(*******************************************************************
 * Unified data access types and macros
 *******************************************************************)

(*******************************************************************
 * Pdi library built-in name constants (default dictionary)
 *******************************************************************)

// Constants for vObject Names id, (PR)operties (N)ames id
// for (PA)rameters (N)ames id and (PA)rameters (V)alues id

(*******************************************************************
 * Internal library name which can be passed to SysLibFind()
 *******************************************************************)

const
  kPdiLibName = 'Pdi.lib';

(*******************************************************************
 * Pdi Library function trap ID's
 *******************************************************************)

  PdiLibTrapReaderNew                = sysLibTrapCustom;
  PdiLibTrapReaderDelete             = sysLibTrapCustom + 1;
  PdiLibTrapWriterNew                = sysLibTrapCustom + 2;
  PdiLibTrapWriterDelete             = sysLibTrapCustom + 3;
  PdiLibTrapReadProperty             = sysLibTrapCustom + 4;
  PdiLibTrapReadPropertyField        = sysLibTrapCustom + 5;
  PdiLibTrapReadPropertyName         = sysLibTrapCustom + 6;
  PdiLibTrapReadParameter            = sysLibTrapCustom + 7;
  PdiLibTrapDefineResizing           = sysLibTrapCustom + 8;
  PdiLibTrapEnterObject              = sysLibTrapCustom + 9;
  PdiLibTrapWriteBeginObject         = sysLibTrapCustom + 10;
  PdiLibTrapWriteProperty            = sysLibTrapCustom + 11;
  PdiLibTrapWriteParameter           = sysLibTrapCustom + 12;
  PdiLibTrapWritePropertyValue       = sysLibTrapCustom + 13;
  PdiLibTrapWritePropertyFields      = sysLibTrapCustom + 14;
  PdiLibTrapWritePropertyBinaryValue = sysLibTrapCustom + 15;
  PdiLibTrapSetEncoding              = sysLibTrapCustom + 16;
  PdiLibTrapSetCharset               = sysLibTrapCustom + 17;
  PdiLibTrapWritePropertyStr         = sysLibTrapCustom + 18;
  PdiLibTrapWriteParameterStr        = sysLibTrapCustom + 19;
  PdiLibTrapDefineReaderDictionary   = sysLibTrapCustom + 20;
  PdiLibTrapDefineWriterDictionary   = sysLibTrapCustom + 21;

(*******************************************************************
 * Pdi Library result codes
 *******************************************************************)

  pdiErrRead            = pdiErrorClass or 1;
  pdiErrWrite           = pdiErrorClass or 2;
  pdiErrNoPropertyName  = pdiErrorClass or 3;
  pdiErrNoPropertyValue = pdiErrorClass or 4;
  pdiErrMoreChars       = pdiErrorClass or 5;
  pdiErrNoMoreFields    = pdiErrorClass or 6;
  pdiErrOpenFailed      = pdiErrorClass or 7;
  pdiErrCloseFailed     = pdiErrorClass or 8;

(*******************************************************************
 * Pdi library constants
 *******************************************************************)

  kPdiASCIIEncoding  = 0;                                 // consider ascii value
  kPdiQPEncoding     = kPdiPAV_ENCODING_QUOTED_PRINTABLE; // value must be QP encoded (write) or is QP encoded (read)
  kPdiB64Encoding    = kPdiPAV_ENCODING_BASE64;           // value must be B64 encoded (write) or is B64 encoded (read)
  kPdiBEncoding      = kPdiPAV_ENCODING_B;                // same as above but ENCODING=B in place of ENCODING=BASE64
  kPdiEscapeEncoding = UInt16($8000);                     // special encoding where newline are backslashed
  kPdiNoEncoding     = UInt16($8001);                     // value must not be encoded (write)

// Constants for structured property values
  kPdiNoFields         = UInt16(0);  // Consider property value has just one field
  kPdiCommaFields      = UInt16(1);  // Consider property value can have several fields comma separated
  kPdiSemicolonFields  = UInt16(2);  // Consider property value can have several fields semicolon separated
  kPdiDefaultFields    = UInt16(4);  // Accept default fields definition (dictionary information)
  kPdiConvertComma     = UInt16(8);  // Consider property value has just one field, commas are converted to '\n'
  kPdiConvertSemicolon = UInt16(16); // Consider property value has just one field, semicolons are converted to '\n'

// Constants to manage parser/generator behavior

// Generator behavior
  kPdiEnableFolding          = UInt16(1);
  kPdiEnableQuotedPrintable  = UInt16(2);
  kPdiEscapeMultiFieldValues = UInt16(4); // Earlier PalmOS compatiblity
  kPdiEnableB                = UInt16(8); // New B encoding type (in place of base64)

  kPdiPalmCompatibility      = kPdiEscapeMultiFieldValues or kPdiEnableQuotedPrintable;

// Parser behavior, currently the open parser is OK
// Maybe future evolution will declare new constants
  kPdiOpenParser             = UInt16(16);     // Generic parser

// Constants to manage writting of values
  kPdiWriteData      = UInt16(0);  // No charset computation (non text values)
  kPdiWriteText      = UInt16(8);  // charset computation
  kPdiWriteMultiline = UInt16(16); // if present: must encode else encoding is determinated by charset

// Constant to manage growing buffers
  kPdiResizableBuffer        = UInt16($FFFF); // Special value to indicate a resizable buffer (handle based)
  kPdiDefaultBufferMaxSize   = UInt16($3FFF); // Maximum size of a resizable buffer non including terminal 0
  kPdiDefaultBufferDeltaSize = UInt16($0010); // Delta (& minimum) size of resizable buffer

// event mask of automata
  kPdiEOFEventMask                    = UInt16(1);
  kPdiGroupNameEventMask              = UInt16(2);    // A group name is found
  kPdiPropertyNameEventMask           = UInt16(4);    // A property name is found
  kPdiParameterNameEventMask          = UInt16(8);    // A parameter name is found
  kPdiParameterValueEventMask         = UInt16(16);   // A parameter value is found
  kPdiPropertyDefinedEventMask        = UInt16(32);   // A property definition is found (the ':' separator is reached)
  kPdiPropertyValueEventMask          = UInt16(64);   // An entire property value is found
  kPdiPropertyValueFieldEventMask     = UInt16(128);  // A value field is found (';' separated)
  kPdiPropertyValueItemEventMask      = UInt16(256);  // A value item is found (',' separated)
  kPdiPropertyValueMoreCharsEventMask = UInt16(512);  // The application didn't provide a large enought buffer: more chars must be read
  kPdiBeginObjectEventMask            = UInt16(1024); // BEGIN reached
  kPdiEndObjectEventMask              = UInt16(2048); // END reached
  kPdiPropertyValueCRLFEventMask      = UInt16(4096); // A value item is found (',' separated)

(*******************************************************************
 * Public Data structures.
 *******************************************************************)

type
  PdiDictionary = UInt8;
  PdiDictionaryPtr = ^PdiDictionary;

type
  PdiReaderType = record
    error: Err;                             // last error
    encoding: UInt8;                        // Type of encoding of the property value
    fieldNum: UInt8;
    charset: CharEncodingType;              // Charset of property value
    written: UInt16;                        // Current number of chars already written in buffer
    property_: UInt16;                      // ID of the current property
    propertyValueType: UInt16;              // type of property value
    parameter: UInt16;                      // ID of the last parsed parameter name
    parameterPairs: array [0..7] of UInt32; // set of bits of parsed parameter values
    customFieldNumber: UInt16;              // Value of X-PALM-CUSTOM (cutom fields)
    appData: Pointer;                       // General usage app dependent field
    pdiRefNum: UInt16;                      // The refNum of the Pdi library
    events: UInt16;                         // Mask of events (see kPdiXXXXEventMask constants)
    groupName: PChar;
    propertyName: PChar;
    parameterName: PChar;
    parameterValue: PChar;
    propertyValue: PChar;
  end;
  PdiReaderTag = PdiReaderType;
  PdiReaderPtr = ^PdiReaderType;

type
  PdiWriterType = record
    error: Err;                // last error
    encoding: UInt16;          // Type of encoding of the property value
    charset: CharEncodingType; // Charset of property value
    appData: Pointer;          // General usage app dependent field
    pdiRefNum: UInt16;         // The refNum of the Pdi library
  end;
  _PdiWriter = PdiWriterType;
  PdiWriterPtr = ^PdiWriterType;

(*******************************************************************
 * Library Open & Close functions
 *******************************************************************)

function PdiLibOpen(libRefnum: UInt16): Err; syscall sysLibTrapOpen;

function PdiLibClose(libRefnum: UInt16): Err; syscall sysLibTrapClose;

(*******************************************************************
 * Reader / Writer initialization & finalization functions
 *******************************************************************)

function PdiReaderNew(libRefnum: UInt16; input: UDAReaderPtr; version: UInt16): PdiReaderPtr; syscall PdiLibTrapReaderNew;

procedure PdiReaderDelete(libRefnum: UInt16; ioReader: PdiReaderPtr); syscall PdiLibTrapReaderDelete;

function PdiWriterNew(libRefnum: UInt16; output: UDAWriterPtr; version: UInt16): PdiWriterPtr; syscall PdiLibTrapWriterNew;

procedure PdiWriterDelete(libRefnum: UInt16; ioWriter: PdiWriterPtr); syscall PdiLibTrapWriterDelete;

(*******************************************************************
 * Read functions group.
 *******************************************************************)

function PdiReadProperty(libRefnum: UInt16; ioReader: PdiReaderPtr): Err; syscall PdiLibTrapReadProperty;

function PdiReadPropertyField(libRefnum: UInt16; ioReader: PdiReaderPtr; bufferPP: PCharPtr; bufferSize, readMode: UInt16): Err; syscall PdiLibTrapReadPropertyField;

function PdiReadPropertyName(libRefnum: UInt16; ioReader: PdiReaderPtr): Err; syscall PdiLibTrapReadPropertyName;

function PdiReadParameter(libRefnum: UInt16; ioReader: PdiReaderPtr): Err; syscall PdiLibTrapReadParameter;

function PdiDefineResizing(libRefnum: UInt16; ioReader: PdiReaderPtr; deltaSize, maxSize: UInt16): Err; syscall PdiLibTrapDefineResizing;

//!!!#define PdiParameterPairTest(reader, pair) \
//!!!   ((reader->parameterPairs[(pair) & 7] & ((UInt32) (1) << ((UInt8) (pair) >> 3))) != 0)

(*******************************************************************
 * Recursive objects functions group.
 *******************************************************************)

function PdiEnterObject(libRefnum: UInt16; ioReader: PdiReaderPtr): Err; syscall PdiLibTrapEnterObject;

(*******************************************************************
 * Write functions group.
 *******************************************************************)

function PdiWriteBeginObject(libRefnum: UInt16; ioWriter: PdiWriterPtr; objectNameID: UInt16): Err; syscall PdiLibTrapWriteBeginObject;

function PdiWriteEndObject(libRefnum: UInt16; ioWriter: PdiWriterPtr; objectNameID: UInt16): Err; syscall PdiLibTrapWriteBeginObject;

function PdiWriteProperty(libRefnum: UInt16; ioWriter: PdiWriterPtr; propertyNameID: UInt16): Err; syscall PdiLibTrapWriteProperty;

function PdiWriteParameter(libRefnum: UInt16; ioWriter: PdiWriterPtr; parameter: UInt16; parameterName: Boolean): Err; syscall PdiLibTrapWriteParameter;

function PdiWritePropertyValue(libRefnum:UInt16; ioWriter: PdiWriterPtr; buffer: PChar; options: UInt16): Err; syscall PdiLibTrapWritePropertyValue;

function PdiWritePropertyFields(libRefnum: UInt16; ioWriter: PdiWriterPtr; fields: PCharPtr; fieldNumber, options: UInt16): Err; syscall PdiLibTrapWritePropertyFields;

function PdiWritePropertyBinaryValue(libRefnum: UInt16; ioWriter: PdiWriterPtr; const buffer: PChar; size, options: UInt16): Err; syscall PdiLibTrapWritePropertyBinaryValue;

function PdiSetEncoding(libRefnum: UInt16; ioWriter: PdiWriterPtr; encoding: UInt16): Err; syscall PdiLibTrapSetEncoding;

function PdiSetCharset(libRefnum: UInt16; ioWriter: PdiWriterPtr; charset: CharEncodingType): Err; syscall PdiLibTrapSetCharset;

function PdiWritePropertyStr(libRefnum: UInt16; ioWriter: PdiWriterPtr; const propertyName: PChar; writeMode, requiredFields: UInt8): Err; syscall PdiLibTrapWritePropertyStr;

function PdiWriteParameterStr(libRefnum: UInt16; ioWriter: PdiWriterPtr; const parameterName, parameterValue: PChar): Err; syscall PdiLibTrapWriteParameterStr;

(*******************************************************************
 * Customisation functions group
 *******************************************************************)

function PdiDefineReaderDictionary(libRefnum: UInt16; ioReader: PdiReaderPtr; var dictionary: PdiDictionary; disableMainDictionary: Boolean): PdiDictionaryPtr; syscall PdiLibTrapDefineReaderDictionary;

function PdiDefineWriterDictionary(libRefnum: UInt16; ioWriter: PdiWriterPtr; var dictionary: PdiDictionary; disableMainDictionary: Boolean): PdiDictionaryPtr; syscall PdiLibTrapDefineWriterDictionary;

implementation

end.
