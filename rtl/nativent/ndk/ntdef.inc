{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This units contains some basic type definitions used by NT
    Copyright (c) 2010 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  PVOID = Pointer;
  PPVOID = ^PVOID;

  HANDLE = THandle; // already defined in system unit
  PHANDLE = ^HANDLE;

  { Upper-Case Versions of Some Standard C Types }
  //CHAR = Char;
  SHORT = ShortInt;
  LONG = LongInt;
  //DOUBLE = Double;

  { Unsigned Types }
  UCHAR = Byte;
  PUCHAR = ^Byte;
  USHORT = Word;
  PUSHORT = ^Word;
  ULONG = LongWord;
  PULONG = PLongWord;
  PCUCHAR = ^UCHAR; { const }
  PCUSHORT = ^USHORT; { const }
  PCULONG = ^ULONG; { const }
  FCHAR = UCHAR;
  FSHORT = USHORT;
  FLONG = ULONG;
  { This type is originaly called BOOLEAN, but that might generate problems
    in SysUtils include files, so we prefix it with NT. Also it's originally
    defined as UCHAR, but ByteBool allows the use of True/False }
  NT_BOOLEAN = ByteBool;
  PNT_BOOLEAN = ^NT_BOOLEAN;
  LOGICAL = ULONG;
  PLOGICAL = ^ULONG;

  { Signed Types }
  PSHORT = ^SHORT;
  PLONG = ^LONG;
  NTSTATUS = LONG;
  PNTSTATUS = ^NTSTATUS;
  SCHAR = SmallInt;
  PSCHAR = ^SCHAR;

  { types from basetsd.h }
  ULONG_PTR = LongWord; // seems to really be a PtrUInt

  { Large Integer Unions }
  // using Int64 is an alternative (QWord might have unintended side effects)
  LARGE_INTEGER = packed record
    case Boolean of
      True:(u: record
              LowPart: LongWord;
              HighPart: LongInt;
            end);
      False:(QuadPart: Int64);
  end;
  PLARGE_INTEGER = ^LARGE_INTEGER;

  { Native API Return Value Macros }
function NT_SUCCESS(Status: NTSTATUS): Boolean; inline; register;
function NT_INFORMATION(Status: NTSTATUS): Boolean; inline; register;
function NT_WARNING(Status: NTSTATUS): Boolean; inline; register;
function NT_ERROR(Status: NTSTATUS): Boolean; inline; register;

type
  { String Types }
  _UNICODE_STRING = packed record
    Length: Word;        // used characters in buffer
    MaximumLength: Word; // maximum characters in buffer
    Buffer: PWideChar;
  end;
  UNICODE_STRING = _UNICODE_STRING;
  PUNICODE_STRING = ^UNICODE_STRING;
  // alias to differ from TUnicodeString
  TNtUnicodeString = UNICODE_STRING;
  PNtUnicodeString = ^TNtUnicodeString;

  { Object Attributes }
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;
  _OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: HANDLE;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: PVOID;       // PSECURITY_DESCRIPTOR
    SecurityQualityOfService: PVOID; // PSECURITY_QUALITY_OF_SERVICE
  end;
  OBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;

procedure InitializeObjectAttributes(var aObjectAttr: OBJECT_ATTRIBUTES;
    aName: PUNICODE_STRING; aAttributes: ULONG; aRootDir: HANDLE;
    aSecurity: Pointer {PSECURITY_DESCRIPTOR}); inline; register;

