{
    This file contains the OS independent declarations of the system unit

    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                             Needed switches
****************************************************************************}

{$I-,Q-,H-,R-,V-}
{$mode objfpc}

{ At least 2.4.0 is required }
{$if defined(VER1) or defined(VER2_0) or defined(VER2_2) }
  {$fatal You need at least FPC 2.4.0 to build this version of FPC}
{$endif}

{ Using inlining for small system functions/wrappers }
{$inline on}
{$define SYSTEMINLINE}

{ don't use FPU registervariables on the i386 and i8086 }
{$if defined(CPUI386) or defined(CPUI8086)}
  {$maxfpuregisters 0}
{$endif CPUI386 or CPUI8086}

{ the assembler helpers need this}
{$ifdef CPUPOWERPC}
  {$goto+}
{$endif CPUPOWERPC}

{$ifdef CPUAVR}
  {$goto+}
{$endif CPUAVR}


{ needed for insert,delete,readln }
{$P+}
{ stack checking always disabled
  for system unit. This is because
  the startup code might not
  have been called yet when we
  get a stack error, this will
  cause big crashes
}
{$S-}

{****************************************************************************
                         Global Types and Constants
****************************************************************************}

{ some values which are used in RTL for TSystemCodePage type }
const
  CP_ACP     = 0;     // default to ANSI code page
  CP_OEMCP   = 1;     // default to OEM (console) code page
  CP_UTF16   = 1200;  // utf-16
  CP_UTF16BE = 1201;  // unicodeFFFE
  CP_UTF7    = 65000; // utf-7
  CP_UTF8    = 65001; // utf-8
  CP_ASCII   = 20127; // us-ascii
  CP_NONE    = $FFFF; // rawbytestring encoding

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;

  { moved here from psystem.pas
    Delphi allows chose of overloaded procedure depending
    on Real <-> Double, so use type here, see also tw7425.pp (FK) }
{$ifndef FPUNONE}
  Real = type Double;
{$endif}

{ Can be individually defined/undefined on a per-platform basis }
{ define FLOAT_ASCII_FALLBACK}

{$ifdef CPUI386}
  {$define CPU32}

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  {$ifndef FPUNONE}
    ValReal = Extended;
  {$endif}

  {$ifndef VER2_6}
  FarPointer = NearFsPointer;
  {$endif}
{$endif CPUI386}

{$ifdef CPUI8086}
  {$define CPU16}

  {$define DEFAULT_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}

  {$ifndef FPUNONE}
    ValReal = Extended;
  {$endif}

  {$if defined(FPC_MM_TINY)}
    {$define FPC_X86_CODE_NEAR}
    {$define FPC_X86_DATA_NEAR}
  {$elseif defined(FPC_MM_SMALL)}
    {$define FPC_X86_CODE_NEAR}
    {$define FPC_X86_DATA_NEAR}
  {$elseif defined(FPC_MM_MEDIUM)}
    {$define FPC_X86_CODE_FAR}
    {$define FPC_X86_DATA_NEAR}
  {$elseif defined(FPC_MM_COMPACT)}
    {$define FPC_X86_CODE_NEAR}
    {$define FPC_X86_DATA_FAR}
  {$elseif defined(FPC_MM_LARGE)}
    {$define FPC_X86_CODE_FAR}
    {$define FPC_X86_DATA_FAR}
  {$elseif defined(FPC_MM_HUGE)}
    {$define FPC_X86_CODE_FAR}
    {$define FPC_X86_DATA_HUGE}
  {$else}
    {$fatal No memory model defined}
  {$endif}
{$endif CPUI8086}

{$ifdef CPUX86_64}
{$ifdef FPC_HAS_TYPE_EXTENDED}
  { win64 doesn't support the legacy fpu }
  {$define DEFAULT_EXTENDED}
  {$define SUPPORT_EXTENDED}
  {$define SUPPORT_COMP}
  {$ifndef FPUNONE}
    ValReal = Extended;
  {$endif}
{$else FPC_HAS_TYPE_EXTENDED}
  {$define DEFAULT_DOUBLE}
  {$ifndef FPUNONE}
    ValReal = Double;
  {$endif}

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;
{$endif FPC_HAS_TYPE_EXTENDED}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$ifndef VER2_6}
  FarPointer = Pointer;
  {$endif}
{$endif CPUX86_64}

{$ifdef CPUM68K}
  {$define DEFAULT_DOUBLE}

  {$ifdef FPUSOFT}
    {$define FPC_INCLUDE_SOFTWARE_MOD_DIV}
    {$define FPC_INCLUDE_SOFTWARE_MUL}
  {$endif}

  { m68k int64 shl/shr uses soft helper for non constaznt values }
  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$ifndef FPUNONE}
    ValReal = Real;
  {$endif}

  { Comp type does not exist on fpu }
  Comp    = int64;
  PComp = ^Comp;

  FarPointer = Pointer;
{$endif CPUM68K}

{$ifdef CPUPOWERPC}
  {$define DEFAULT_DOUBLE}

  {$ifndef FPUNONE}
    {$define SUPPORT_SINGLE}
    {$define SUPPORT_DOUBLE}

    {$define FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

    ValReal = Double;
  {$endif}

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;

  FarPointer = Pointer;
{$endif CPUPOWERPC}

{$ifdef CPUSPARC}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

  {$ifndef FPUNONE}
    ValReal = Double;
  {$endif}

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;

  FarPointer = Pointer;
{$endif CPUSPARC}

{$if defined(CPUMIPS32) or defined(CPUMIPSEL32)}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

  {$ifndef FPUNONE}
    {$define FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}
    ValReal = Double;
  {$endif}

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;

  FarPointer = Pointer;
{$endif CPUMIPS32}


{$ifdef CPUARM}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

  {$define FPC_INCLUDE_SOFTWARE_MOD_DIV}
  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}
  {$define FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

  {$ifndef FPUNONE}
    ValReal = Real;
  {$endif}

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;

  FarPointer = Pointer;
{$endif CPUARM}

{$ifdef CPUAVR}
  {$define DEFAULT_SINGLE}

  {$define FPC_INCLUDE_SOFTWARE_MOD_DIV}
  {$define FPC_INCLUDE_SOFTWARE_MUL}
  {$define FPC_INCLUDE_SOFTWARE_SHIFT_INT64}

  {$ifndef FPUNONE}
    {$define SUPPORT_SINGLE}
    {$define SUPPORT_DOUBLE}

    {$define FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

    ValReal = Real;
  {$endif}

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;

  FarPointer = Pointer;
{$endif CPUAVR}

{$ifdef CPUJVM}
  {$define DEFAULT_DOUBLE}

  {$define SUPPORT_SINGLE}
  {$define SUPPORT_DOUBLE}

    ValReal = Double;

  { map comp to int64, but this doesn't mean we compile the comp support in! }
  Comp = Int64;
  PComp = ^Comp;
{$endif CPUJVM}

{$ifdef CPU64}
  SizeInt = Int64;
  SizeUInt = QWord;
  PtrInt = Int64;
  PtrUInt = QWord;
  ValSInt = int64;
  ValUInt = qword;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
{$endif CPU64}

{$ifdef CPU32}
  SizeInt = Longint;
  SizeUInt = DWord;
  PtrInt = Longint;
  PtrUInt = DWord;
  ValSInt = Longint;
  ValUInt = Cardinal;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
{$endif CPU32}

{$ifdef CPU16}
  SizeInt = Integer;
  SizeUInt = Word;
  {$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
    PtrInt = Longint;
    PtrUInt = DWord;
  {$else}
    PtrInt = Integer;
    PtrUInt = Word;
  {$endif}
  {$if defined(FPC_X86_CODE_FAR)}
    CodePointer = FarPointer;
    CodePtrInt = Longint;
    CodePtrUInt = DWord;
  {$elseif defined(FPC_X86_CODE_NEAR)}
    CodePointer = NearPointer;
    CodePtrInt = Integer;
    CodePtrUInt = Word;
  {$else}
    CodePointer = Pointer;
    CodePtrInt = PtrInt;
    CodePtrUInt = PtrUInt;
  {$endif}
  ValSInt = Integer;
  ValUInt = Word;
{$endif CPU16}

  { NativeInt and NativeUInt are Delphi compatibility types. Even though Delphi
    has IntPtr and UIntPtr, the Delphi documentation for NativeInt states that
    'The size of NativeInt is equivalent to the size of the pointer on the
    current platform'. Because of the misleading names, these types shouldn't be
    used in the FPC RTL. Note that on i8086 their size changes between 16-bit
    and 32-bit according to the memory model, so they're not really a 'native
    int' type there at all. }
  NativeInt  = PtrInt;
  NativeUInt = PtrUInt;

  Int8    = ShortInt;
  Int16   = SmallInt;
  Int32   = Longint;
  IntPtr  = PtrInt;
  UInt8   = Byte;
  UInt16  = Word;
  UInt32  = Cardinal;
  UIntPtr = PtrUInt;

{ Zero - terminated strings }
  PChar               = ^Char;
  PPChar              = ^PChar;
  PPPChar             = ^PPChar;

  { AnsiChar is equivalent of Char, so we need
    to use type renamings }
  TAnsiChar           = Char;
  AnsiChar            = Char;
  PAnsiChar           = PChar;
  PPAnsiChar          = PPChar;

  UCS4Char            = type 0..$10ffff;
  PUCS4Char           = ^UCS4Char;
{$ifdef CPU16}
  TUCS4CharArray      = array[0..32767 div sizeof(UCS4Char)-1] of UCS4Char;
{$else CPU16}
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
{$endif CPU16}
  PUCS4CharArray      = ^TUCS4CharArray;
  UCS4String          = array of UCS4Char;

{$ifdef FPC_HAS_CPSTRING}
  UTF8String          = type AnsiString(CP_UTF8);
{$else FPC_HAS_CPSTRING}
  UTF8String          = type ansistring;
{$endif FPC_HAS_CPSTRING}
  PUTF8String         = ^UTF8String;

{$ifdef FPC_HAS_CPSTRING}
  RawByteString       = type AnsiString(CP_NONE);
{$else FPC_HAS_CPSTRING}
  RawByteString       = ansistring;
{$endif FPC_HAS_CPSTRING}

  HRESULT             = type Longint;
{$ifndef FPUNONE}
  TDateTime           = type Double;
  TDate               = type TDateTime;
  TTime               = type TDateTime;
{$endif}
  TError               = type Longint;

{$ifndef FPUNONE}
  PSingle             = ^Single;
  PDouble             = ^Double;
  PExtended           = ^Extended;

  PPDouble            = ^PDouble;
{$endif}
  PCurrency           = ^Currency;
{$ifdef SUPPORT_COMP}
  PComp               = ^Comp;
{$endif SUPPORT_COMP}

  PSmallInt           = ^Smallint;
  PShortInt           = ^Shortint;
  PInteger            = ^Integer;
  PByte               = ^Byte;
  PWord               = ^word;
  PDWord              = ^DWord;
  PLongWord           = ^LongWord;
  PLongint            = ^Longint;
  PCardinal           = ^Cardinal;
  PQWord              = ^QWord;
  PInt64              = ^Int64;
  PPtrInt             = ^PtrInt;
  PPtrUInt            = ^PtrUInt;
  PSizeInt            = ^SizeInt;

  PPByte              = ^PByte;
  PPLongint           = ^PLongint;

  PPointer            = ^Pointer;
  PPPointer           = ^PPointer;

  PCodePointer        = ^CodePointer;
  PPCodePointer       = ^PCodePointer;

  PBoolean            = ^Boolean;
  PWordBool           = ^WordBool;
  PLongBool           = ^LongBool;

  PNativeInt 	      = ^NativeInt;
  PNativeUInt	      = ^NativeUint;
  pInt8   	      = PShortInt;
  pInt16  	      = PSmallint;
  pInt32  	      = PLongint;
  PIntPtr 	      = PPtrInt;
  pUInt8  	      = PByte;
  pUInt16 	      = PWord;
  pUInt32 	      = PDWord;
  PUintPtr	      = PPtrUInt;

  PShortString        = ^ShortString;
  PAnsiString         = ^AnsiString;

{$ifndef FPUNONE}
  PDate               = ^TDateTime;
  PDateTime           = ^TDateTime;
{$endif}
  PError              = ^TError;
  PVariant            = ^Variant;
  POleVariant         = ^OleVariant;

  PWideChar           = ^WideChar;
  PPWideChar          = ^PWideChar;
  PPPWideChar         = ^PPWideChar;
  WChar               = Widechar;
  UCS2Char            = WideChar;
  PUCS2Char           = PWideChar;
  PWideString         = ^WideString;

  UnicodeChar         = WideChar;
  PUnicodeChar        = ^UnicodeChar;
  PUnicodeString      = ^UnicodeString;

  TSystemCodePage     = Word;

{$ifdef VER2_6}
  { the size of textrec/filerec is hardcoded in the 2.6 compiler binary }
  {$define FPC_ANSI_TEXTFILEREC}
{$endif}
  TFileTextRecChar    = {$ifdef FPC_ANSI_TEXTFILEREC}AnsiChar{$else}UnicodeChar{$endif};
  PFileTextRecChar    = ^TFileTextRecChar;

  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{ procedure type }
  TProcedure  = Procedure;

{ platform-dependent types }
{$i sysosh.inc}

{ platform-dependent defines }
{$i rtldefs.inc}
(*
{*****************************************************************************
                   TextRec/FileRec exported to allow compiler to take size
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_FILEIO}
{$i filerec.inc}
{$endif FPC_HAS_FEATURE_FILEIO}

{$i textrec.inc}


type
  { Needed for fpc_get_output }
  PText               = ^Text;

  TEntryInformation = record
    InitFinalTable : Pointer;
    ThreadvarTablesTable : Pointer;
    asm_exit : Procedure;stdcall;
    PascalMain : Procedure;stdcall;
    valgrind_used : boolean;
  end;
*)

const
{ Maximum value of the biggest signed and unsigned integer type available}
  MaxSIntValue = High(ValSInt);
  MaxUIntValue = High(ValUInt);

{ max. values for longint and int}
  maxLongint  = $7fffffff;
  maxSmallint = 32767;

  maxint   = maxsmallint;

type
{$ifdef CPU16}
  IntegerArray  = array[0..maxSmallint div sizeof(Integer)-1] of Integer;
{$else CPU16}
  IntegerArray  = array[0..$effffff] of Integer;
{$endif CPU16}
  PIntegerArray = ^IntegerArray;
{$ifdef CPU16}
  PointerArray = array [0..32767 div sizeof(Pointer)-1] of Pointer;
{$else CPU16}
  PointerArray = array [0..512*1024*1024-2] of Pointer;
{$endif CPU16}
(*
  PPointerArray = ^PointerArray;
*)

  TBoundArray = array of SizeInt;
(*
{$ifdef CPU16}
  TPCharArray = packed array[0..(MaxSmallint div SizeOf(PChar))-1] of PChar;
{$else CPU16}
  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
{$endif CPU16}
  PPCharArray = ^TPCharArray;
*)

(* CtrlBreak set to true signalizes Ctrl-Break signal, otherwise Ctrl-C. *)
(* Return value of true means that the signal has been processed, false  *)
(* means that default handling should be used. *)
(*
TCtrlBreakHandler = function (CtrlBreak: boolean): boolean;
*)

const
{$ifdef cpui386}
  { Always i386 or newer }
  Test8086 : byte = 2;
  { Always 387 or newer. Emulated if needed. }
  Test8087 : byte = 3;
  { will be detected at startup }
  has_sse_support : boolean = false;
  has_sse2_support : boolean = false;
  has_sse3_support : boolean = false;
  has_mmx_support : boolean = false;
{$endif cpui386}
{$ifdef cpui8086}
  { will be detected at startup }
  { 0=8086/8088/80186/80188/NEC V20/NEC V30, 1=80286, 2=80386 or newer }
  Test8086 : byte = 0; public name '__Test8086';
  { will be detected at startup }
  { 0=NO FPU, 1=8087, 2=80287, 3=80387 or newer }
  Test8087 : byte = 0;
  { will be detected at startup }
  has_sse_support : boolean = false;
  has_mmx_support : boolean = false;
{$endif cpui8086}
{$ifdef cpum68k}
  Test68000 : byte = 0;      { Must be determined at startup for both }
  Test68881 : byte = 0;
{$endif cpum68k}

{ max level in dumping on error }
  Max_Frame_Dump : Word = 8;
(*
{ Exit Procedure handling consts and types  }
  ExitProc : codepointer = nil;
  Erroraddr: codepointer = nil;
*)
  Errorcode: Word    = 0;

{ file input modes }
  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;
  fmAppend = $D7B4;
  Filemode : byte = 2;
(* Value should be changed during system initialization as appropriate. *)

  { assume that this program will not spawn other threads, when the
    first thread is started the following constants need to be filled }
  IsMultiThread : longbool = FALSE;
  { set to true, if a threading helper is used before a thread
    manager has been installed }
  ThreadingAlreadyUsed : boolean = FALSE;
  { Indicates if there was an error }
  StackError : boolean = FALSE;
(*
  InitProc : CodePointer = nil;
*)
  { compatibility }
  ModuleIsLib : Boolean = FALSE;
  ModuleIsPackage : Boolean = FALSE;
  ModuleIsCpp : Boolean = FALSE;

var
  ExitCode    : Longint; (* public name 'operatingsystem_result'; *)
  RandSeed    : Cardinal;
  { Delphi compatibility }

{$ifdef FPC_HAS_FEATURE_DYNLIBS}
  IsLibrary : boolean = false; public name 'operatingsystem_islibrary';
{$else FPC_HAS_FEATURE_DYNLIBS}
const
  IsLibrary = false;
var
{$endif FPC_HAS_FEATURE_DYNLIBS}
  IsConsole : boolean = false; public name 'operatingsystem_isconsole';
  NoErrMsg: Boolean platform = False; // For Delphi compatibility, not used in FPC.
  FirstDotAtFileNameStartIsExtension : Boolean = False;
  
  DefaultSystemCodePage,
  DefaultUnicodeCodePage,
  { the code page to use when sending paths/file names to OS file system API
    calls using single byte strings, and to interpret the results gotten back
    from such API calls }
  DefaultFileSystemCodePage,
  { the code page to use to return file names from single byte file system calls
    in the RTL that return ansistrings (by default, same as a above) }
  DefaultRTLFileSystemCodePage,
  UTF8CompareLocale : TSystemCodePage;


(*
{$ifndef HAS_CMDLINE}
{Value should be changed during system initialization as appropriate.}
var cmdline:Pchar=nil;
{$endif}
*)

(*
{$ifdef FPC_HAS_FEATURE_THREADING}
ThreadVar
{$else FPC_HAS_FEATURE_THREADING}
Var
{$endif FPC_HAS_FEATURE_THREADING}
  ThreadID    : TThreadID;
  { Standard In- and Output }
  ErrOutput,
  Output,
  Input,
  StdOut,
  StdErr      : Text;
  InOutRes    : Word;
  { Stack checking }
  StackBottom : Pointer;
  StackLength : SizeUInt;
*)

{ Numbers for routines that have compiler magic }
{$I innr.inc}

