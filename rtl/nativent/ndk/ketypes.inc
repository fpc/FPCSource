{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This unit contains types used by the Kernel.
    Copyright (c) 2010 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  //
  // Maximum WOW64 Entries in KUSER_SHARED_DATA
  //
  MAX_WOW64_SHARED_ENTRIES = 16;

  //
  // Maximum Processor Features supported in KUSER_SHARED_DATA
  //
  PROCESSOR_FEATURE_MAX = 64;


type
  //
  // NT Product and Architecture Types
  //
  _NT_PRODUCT_TYPE = (
    NtProductWinNt = 1,
    NtProductLanManNt,
    NtProductServer
  );
  NT_PRODUCT_TYPE = _NT_PRODUCT_TYPE;
  PNT_PRODUCT_TYPE = ^NT_PRODUCT_TYPE;

  _ALTERNATIVE_ARCHITECTURE_TYPE = (
    StandardDesign,
    NEC98x86,
    EndAlternatives
  );
  ALTERNATIVE_ARCHITECTURE_TYPE = _ALTERNATIVE_ARCHITECTURE_TYPE;
  PALTERNATIVE_ARCHITECTURE_TYPE = ^ALTERNATIVE_ARCHITECTURE_TYPE;

  //
  // System Time Structure
  //
  _KSYSTEM_TIME = packed record
    LowPart: ULONG;
    High1Time: LONG;
    High2Time: LONG;
  end;
  KSYSTEM_TIME = _KSYSTEM_TIME;
  PKSYSTEM_TIME = ^KSYSTEM_TIME;

  //
  // Shared Kernel User Data
  //
  _KUSER_SHARED_DATA = packed record
    TickCountLowDeprecated: ULONG;
    TickCountMultiplier: ULONG;
    InterruptTime: KSYSTEM_TIME; {volatile}
    SystemTime: KSYSTEM_TIME; {volatile}
    TimeZoneBias: KSYSTEM_TIME; {volatile }
    ImageNumberLow: USHORT;
    ImageNumberHigh: USHORT;
    NtSystemRoot: array[0..259] of WideChar;
    MaxStackTraceDepth: ULONG;
    CryptoExponent: ULONg;
    TimeZoneId: ULONG;
    LargePageMinimum: ULONG;
    Reserved2: array[0..6] of ULONG;
    NtProductType: NT_PRODUCT_TYPE;
    ProductTypeIsValid: BOOLEAN;
    NtMajorVersion: ULONG;
    NtMinorVersion: ULONG;
    ProcessorFeature: array[0..PROCESSOR_FEATURE_MAX - 1] of BOOLEAN;
    Reserved1: ULONG;
    Reserved3: ULONG;
    TimeSlip: ULONG; {volatile}
    AlternativeArchitecture: ALTERNATIVE_ARCHITECTURE_TYPE;
    SystemExpirationDate: LARGE_INTEGER;
    SuiteMask: ULONG;
    KdDebuggerEnabled: BOOLEAN;
    { from here on the structures differ by 1 Byte depending on the version -.- }
  end;
  KUSER_SHARED_DATA = _KUSER_SHARED_DATA;
  PKUSER_SHARED_DATA = ^KUSER_SHARED_DATA;

//
// Dereferencable pointer to KUSER_SHARED_DATA in User-Mode
//
function SharedUserData: PKUSER_SHARED_DATA; inline; register;

