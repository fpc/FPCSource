{**********[ SOURCE FILE OF FREE VISION ]***************}
{                                                       }
{   Parts Copyright (c) 1992,96 by Florian Klaempfl     }
{   fnklaemp@cip.ft.uni-erlangen.de                     }
{                                                       }
{   Parts Copyright (c) 1996 by Frank ZAGO              }
{   zago@ecoledoc.ipc.fr                                }
{                                                       }
{   Parts Copyright (c) 1995 by MH Spiegel              }
{                                                       }
{   Parts Copyright (c) 1996 by Leon de Boer            }
{   ldeboer@ibm.net                                     }
{                                                       }
{              THIS CODE IS FREEWARE                    }
{*******************************************************}

{***************[ SUPPORTED PLATFORMS ]*****************}
{  16 and 32 Bit compilers                              }
{     DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{              - FPK Pascal              (32 Bit)       }
{     DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{     WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{     OS2      - Virtual Pascal 0.3 +    (32 Bit)       }
{                SpeedPascal 1.5 G +     (32 Bit)       }
{                C'T patch to BP         (16 Bit)       }
{*******************************************************}

UNIT Objects;

{$I os.inc}

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{ ******************************* REMARK ****************************** }
{  FPK does not accept  $IFNDEF compiler defines and mishandles $IFDEF  }
{  with constants. Can we please get this error fixed!!!!!              }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{====Compiler conditional defines to sort platforms out =============}
{$DEFINE NotFPKPascal}                                { Predefine Not FPK }
{$DEFINE NotOS2}                                      { Predefine NOT OS2 }

{$IFDEF FPK}                                          { FPK PASCAL }
   {$DEFINE FPKPascal}                                { Set FPK definition }
   {$DEFINE DOS_OS}                                   { Define DOS_OS }
   {$DEFINE CODE_32_BIT}                              { 32 BIT CODE }
   {$UNDEF USE_BGI}                                   { Can't use BGI }
   {$UNDEF NotFPKPascal}                              { This is FPK pascal }
{$ENDIF}

{$IFDEF MSDOS}                                        { MSDOS PLATFORM }
   {$DEFINE DOS_OS}                                   { Define DOS_OS }
{$ENDIF}

{$IFDEF DPMI}                                         { DPMI PLATFORM }
   {$DEFINE DOS_OS}                                   { Define DOS_OS }
{$ENDIF}

{$IFDEF Windows}                                      { WINDOWS platform }
   {$DEFINE ADV_OS}                                   { Set as advanced }
   {$UNDEF USE_BGI}                                   { Can't use BGI }
{$ENDIF}

{$IFDEF OS2}                                          { OS2 platform }
   {$DEFINE ADV_OS}                                   { Set as advanced }
   {$IFNDEF FPK}
    {$DEFINE BPOS2}                                    { Define BPOS2 }
   {$ENDIF FPK}
   {$UNDEF NotOS2}                                    { This is OS2 compiler }
   {$UNDEF USE_BGI}                                   { Can't use BGI }
   {$UNDEF DOS_OS}
{$ENDIF}

{$IFDEF VirtualPascal}                                { VIRTUAL PASCAL }
   {$DEFINE CODE_32_BIT}                              { 32 BIT CODE }
   {$DEFINE ASM_32_BIT}                               { 32 BIT ASSSEMBLER }
   {$DEFINE API_32_BIT}                               { 32 BIT API CALLS }
   {$UNDEF BPOS2}                                     { Undefine BPOS2 }
{$ENDIF}

{$IFDEF Speed}                                        { SPEED PASCAL }
   {$DEFINE CODE_32_BIT}                              { 32 BIT CODE }
   {$DEFINE ASM_32_BIT}                               { 32 BIT ASSSEMBLER }
   {$DEFINE API_32_BIT}                               { 32 BIT API CALLS }
   {$UNDEF BPOS2}                                     { Undefine BPOS2 }
{$ENDIF}
{--------------------------------------------------------------------}

{ ******************************* REMARK ****************************** }
{ How about FPK accepting all the standard compiler directives even if  }
{ It just ignores them for now!!                                        }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{==== Compiler directives ===========================================}
{$IFDEF FPKPascal}                                    { FPK PASCAL }
   {$E-}
   {$DEFINE NoExceptions}
   {$DEFINE SString}

   CONST
      Sw_MaxData = 128*1024*1024;                     { Maximum data size }

   TYPE
      Sw_Word    = LongInt;                           { Long integer now }
      Sw_Integer = LongInt;                           { Long integer now }

   TYPE
      FuncPtr = FUNCTION (Item: Pointer; _EBP: Sw_Word): Boolean;
      ProcPtr = PROCEDURE (Item: Pointer; _EBP: Sw_Word);

{$ENDIF}
{$IFDEF NotFPKPascal}                                 { ALL OTHER COMPILERS }
   {$N-} {  No 80x87 code generation }
   {$O+} { This unit may be overlaid }
   {$X+} { Extended syntax is ok }
   {$F+} { Force far calls }
   {$A+} { Word Align Data }
   {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
   {$R-} { Disable range checking }
   {$S-} { Disable Stack Checking }
   {$I-} { Disable IO Checking }
   {$Q-} { Disable Overflow Checking }
   {$V-} { Turn off strict VAR strings }
   {$B-} { Allow short circuit boolean evaluations }

   {$IFNDEF CODE_32_BIT}                              { 16 BIT DEFINITIONS }
   CONST
      Sw_MaxData = 65520;                             { Maximum data size }

   TYPE
      Sw_Word    = Word;                              { Standard word }
      Sw_Integer = Integer;                           { Standard integer }
   {$ELSE}                                            { 32 BIT DEFINITIONS }
   CONST
      Sw_MaxData = 128*1024*1024;                     { Maximum data size }

   TYPE
      Sw_Word    = LongInt;                           { Long integer now }
      Sw_Integer = LongInt;                           { Long integer now }
   {$ENDIF}

   TYPE
   {$IFDEF VirtualPascal}                             { VP is different }
      FuncPtr = FUNCTION (Item: Pointer): Boolean;
   {$ELSE}                                            { All others }
      FuncPtr = FUNCTION (Item: Pointer; _EBP: Sw_Word): Boolean;
   {$ENDIF}

   TYPE
   {$IFDEF VirtualPascal}                             { VP is different }
      ProcPtr = PROCEDURE (Item: Pointer);
   {$ELSE}                                            { All others }
      ProcPtr = PROCEDURE (Item: Pointer; _EBP: Sw_Word);
   {$ENDIF}

{$ENDIF}
{---------------------------------------------------------------------}

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                         STREAM ERROR STATE MASKS                        ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   stOk         =  0;                                 { No stream error }
   stError      = -1;                                 { Access error }
   stInitError  = -2;                                 { Initialize error }
   stReadError  = -3;                                 { Stream read error }
   stWriteError = -4;                                 { Stream write error }
   stGetError   = -5;                                 { Get object error }
   stPutError   = -6;                                 { Put object error }
   stSeekError  = -7;                                 { Seek error in stream }
   stOpenError  = -8;                                 { Error opening stream }

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                       STREAM ACCESS MODE CONSTANTS                      ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   stCreate    = $3C00;                               { Create new file }
   stOpenRead  = $3D00;                               { Read access only }
   stOpenWrite = $3D01;                               { Write access only }
   stOpen      = $3D02;                               { Read/write access }

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                         TCollection ERROR CODES                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   coIndexError = -1;                                 { Index out of range }
   coOverflow   = -2;                                 { Overflow }

CONST
{ ******************************* REMARK ****************************** }
{   These are completely NEW FREE VISION ONLY constants that are used   }
{  in conjuction with CreateStream a NEW FREE VISION call. This call    }
{  tries creating a stream in the order of the Strategy Mask and will   }
{  return the successfully created stream or nil if it fails.           }
{ ****************************** END REMARK *** Leon de Boer, 15May96 * }

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                      STREAM CREATE STRATEGY MASKS                       ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   sa_XMSFirst   = $8000;                             { Use XMS memory 1st }
   sa_EMSFirst   = $4000;                             { Use EMS memory 1st }
   sa_RAMFirst   = $2000;                             { Use RAM memory 1st }
   sa_DISKFirst  = $1000;                             { Use DISK space 1st }
   sa_XMSSecond  = $0800;                             { Use XMS memory 2nd }
   sa_EMSSecond  = $0400;                             { Use EMS memory 2nd }
   sa_RAMSecond  = $0200;                             { Use RAM memory 2nd }
   sa_DISKSecond = $0100;                             { Use DISK space 2nd }
   sa_XMSThird   = $0080;                             { Use XMS memory 3rd }
   sa_EMSThird   = $0040;                             { Use EMS memory 3rd }
   sa_RAMThird   = $0020;                             { Use RAM memory 3rd }
   sa_DISKThird  = $0010;                             { Use DISK space 3rd }
   sa_XMSFourth  = $0008;                             { Use XMS memory 4th }
   sa_EMSFourth  = $0004;                             { Use EMS memory 4th }
   sa_RAMFourth  = $0002;                             { Use RAM memory 4th }
   sa_DISKFourth = $0001;                             { Use DISK space 4th }

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                          GENERAL USE CONSTANTS                          ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
{$IFDEF VirtualPascal}
   vmtHeaderSize = 12;                                { VMT header size }
{$ELSE}
   vmtHeaderSize = 8;                                 { VMT header size }
{$ENDIF}

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                   MAXIMUM COLLECTION SIZE CONSTANT                      ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   MaxCollectionSize = Sw_MaxData DIV SizeOf(Pointer);{ Max collection size }

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                              CHARACTER SET                              ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TCharSet = SET Of Char;                            { Character set }
   PCharSet = ^TCharSet;                              { Character set ptr }

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                              GENERAL ARRAYS                             ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TByteArray = ARRAY [0..Sw_MaxData-1] Of Byte;      { Byte array }
   PByteArray = ^TByteArray;                          { Byte array pointer }

   TWordArray = ARRAY [0..Sw_MaxData DIV 2-1] Of Word;{ Word array }
   PWordArray = ^TWordArray;                          { Word array pointer }

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                           DOS FILENAME STRING                           ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
{$IFDEF DOS_OS}                                       { DOS/DPMI DEFINE }
   FNameStr = String[79];                             { DOS filename }
{$ENDIF}
{$IFDEF Windows}                                      { WINDOWS DEFINE }
   FNameStr = PChar;                                  { Windows filename }
{$ENDIF}
{$IFDEF OS2}                                          { OS2 DEFINE }
   FNameStr = String;                                 { OS2 filename }
{$ENDIF}

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                           DOS ASCIIZ FILENAME                           ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   AsciiZ = Array [0..255] Of Char;                   { Filename array }

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                          GENERAL TYPE POINTERS                          ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   PByte    = ^Byte;                                  { Byte pointer }
   PWord    = ^Word;                                  { Word pointer }
   PLongInt = ^LongInt;                               { LongInt pointer }
   PString  = ^String;                                { String pointer }

{***************************************************************************}
{                            RECORD DEFINITIONS                             }
{***************************************************************************}
TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                         TYPE CONVERSION RECORDS                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   WordRec = RECORD
     Lo, Hi: Byte;                                    { Word to bytes }
   END;

   LongRec = RECORD
     Lo, Hi: Word;                                    { LongInt to words }
   END;

   PtrRec = RECORD
     Ofs, Seg: Word;                                  { Pointer to words }
   END;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                 TStreamRec RECORD - STREAM OBJECT RECORD                ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   PStreamRec = ^TStreamRec;                          { Stream record ptr }
   TStreamRec = RECORD
      ObjType: Sw_Word;                               { Object type id }
      VmtLink: Sw_Word;                               { VMT link }
      Load : Pointer;                                 { Object load code }
      Store: Pointer;                                 { Object store code }
      Next : Sw_Word;                                 { Bytes to next }
   END;

{***************************************************************************}
{                            OBJECT DEFINITIONS                             }
{***************************************************************************}

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                       TPoint RECORD - POINT RECORD                      ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TPoint = RECORD
      X, Y: Integer;                                  { Point co-ordinates }
   END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                     TRect OBJECT - RECTANGLE OBJECT                     ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TRect = OBJECT
         A, B: TPoint;                                { Corner points }
      FUNCTION Empty: Boolean;
      FUNCTION Equals (R: TRect): Boolean;
      FUNCTION Contains (P: TPoint): Boolean;
      PROCEDURE Copy (R: TRect);
      PROCEDURE Union (R: TRect);
      PROCEDURE Intersect (R: TRect);
      PROCEDURE Move (ADX, ADY: Integer);
      PROCEDURE Grow (ADX, ADY: Integer);
      PROCEDURE Assign (XA, YA, XB, YB: Integer);
   END;
   PRect = ^TRect;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                 TObject OBJECT - BASE ANCESTOR OBJECT                   ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TObject = OBJECT
      CONSTRUCTOR Init;
      PROCEDURE Free;
      DESTRUCTOR Done;                                               Virtual;
   END;
   PObject = ^TObject;

TYPE
{ ******************************* REMARK ****************************** }
{  Two new virtual methods have been added to the object in the form of }
{  Close and Open. The main use here is in the Disk Based Descendants   }
{  the calls open and close the given file so these objects can be      }
{  used like standard files. All existing code will compile and work    }
{  completely normally oblivious to these new methods.                  }
{ ****************************** END REMARK *** Leon de Boer, 15May96 * }
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                TStream OBJECT - STREAM ANCESTOR OBJECT                  ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TStream = OBJECT (TObject)
         Status   : Integer;                          { Stream status }
         ErrorInfo: Integer;                          { Stream error info }
      FUNCTION Get: PObject;
      FUNCTION StrRead: PChar;
      FUNCTION GetPos: LongInt;                                      Virtual;
      FUNCTION GetSize: LongInt;                                     Virtual;
      FUNCTION ReadStr: PString;
      PROCEDURE Close;                                               Virtual;
      PROCEDURE Reset;
      PROCEDURE Flush;                                               Virtual;
      PROCEDURE Truncate;                                            Virtual;
      PROCEDURE Put (P: PObject);
      PROCEDURE Seek (Pos: LongInt);                                 Virtual;
      PROCEDURE StrWrite (P: PChar);
      PROCEDURE WriteStr (P: PString);
      PROCEDURE Open (OpenMode: Word);                               Virtual;
      PROCEDURE Error (Code, Info: Integer);                         Virtual;
      PROCEDURE Read (Var Buf; Count: Sw_Word);                      Virtual;
      PROCEDURE Write (Var Buf; Count: Sw_Word);                     Virtual;
      PROCEDURE CopyFrom (Var S: TStream; Count: Longint);
   END;
   PStream = ^TStream;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ               TDosStream OBJECT - DOS FILE STREAM OBJECT                ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TDosStream = OBJECT (TStream)
         Handle: Integer;                             { DOS file handle }
         FName : AsciiZ;                              { AsciiZ filename }
      CONSTRUCTOR Init (FileName: FNameStr; Mode: Word);
      DESTRUCTOR Done;                                               Virtual;
      FUNCTION GetPos: Longint;                                      Virtual;
      FUNCTION GetSize: Longint;                                     Virtual;
      PROCEDURE Close;                                               Virtual;
      PROCEDURE Seek (Pos: LongInt);                                 Virtual;
      PROCEDURE Open (OpenMode: Word);                               Virtual;
      PROCEDURE Read (Var Buf; Count: Sw_Word);                      Virtual;
      PROCEDURE Write (Var Buf; Count: Sw_Word);                     Virtual;
   END;
   PDosStream = ^TDosStream;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ               TBufStream OBJECT - BUFFERED DOS FILE STREAM              ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TBufStream = OBJECT (TDosStream)
   END;
   PBufStream = ^TBufStream;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                  TEmsStream OBJECT - EMS STREAM OBJECT                  ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TEmsStream = OBJECT (TStream)
   END;
   PEmsStream = ^TEmsStream;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                  TXmsStream OBJECT - XMS STREAM OBJECT                  ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TXmsStream = OBJECT (TStream)
   END;
   PXmsStream = ^TXmsStream;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ              TMemoryStream OBJECT - MEMORY STREAM OBJECT                ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TMemoryStream = OBJECT (TStream)
   END;
   PMemoryStream = ^TMemoryStream;

TYPE
  TItemList = Array [0..MaxCollectionSize - 1] Of Pointer;
  PItemList = ^TItemList;

{ ******************************* REMARK ****************************** }
{    The changes here look worse than they are. The Sw_Integer simply   }
{  switches between Integers and LongInts if switched between 16 and 32 }
{  bit code. All existing code will compile without any changes.        }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ             TCollection OBJECT - COLLECTION ANCESTOR OBJECT             ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TCollection = OBJECT (TObject)
         Items: PItemList;                            { Item list pointer }
         Count: Sw_Integer;                           { Item count }
         Limit: Sw_Integer;                           { Item limit count }
         Delta: Sw_Integer;                           { Inc delta size }
      CONSTRUCTOR Init (ALimit, ADelta: Sw_Integer);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done;                                               Virtual;
      FUNCTION At (Index: Sw_Integer): Pointer;
      FUNCTION IndexOf (Item: Pointer): Sw_Integer;                  Virtual;
      FUNCTION GetItem (Var S: TStream): Pointer;                    Virtual;
      FUNCTION LastThat (Test: Pointer): Pointer;
      FUNCTION FirstThat (Test: Pointer): Pointer;
      PROCEDURE Pack;
      PROCEDURE FreeAll;
      PROCEDURE DeleteAll;
      PROCEDURE Free (Item: Pointer);
      PROCEDURE Insert (Item: Pointer);                              Virtual;
      PROCEDURE Delete (Item: Pointer);
      PROCEDURE AtFree (Index: Sw_Integer);
      PROCEDURE FreeItem (Item: Pointer);                            Virtual;
      PROCEDURE AtDelete (Index: Sw_Integer);
      PROCEDURE ForEach (Action: Pointer);
      PROCEDURE SetLimit (ALimit: Sw_Integer);                       Virtual;
      PROCEDURE Error (Code, Info: Integer);                         Virtual;
      PROCEDURE AtPut (Index: Sw_Integer; Item: Pointer);
      PROCEDURE AtInsert (Index: Sw_Integer; Item: Pointer);
      PROCEDURE Store (Var S: TStream);
      PROCEDURE PutItem (Var S: TStream; Item: Pointer);             Virtual;
   END;
   PCollection = ^TCollection;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ         TSortedCollection OBJECT - SORTED COLLECTION ANCESTOR           ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TSortedCollection = OBJECT (TCollection)
         Duplicates: Boolean;                         { Duplicates flag }
      CONSTRUCTOR Init (ALimit, ADelta: Sw_Integer);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION KeyOf (Item: Pointer): Pointer;                       Virtual;
      FUNCTION IndexOf (Item: Pointer): Sw_Integer;                  Virtual;
      FUNCTION Compare (Key1, Key2: Pointer): Sw_Integer;            Virtual;
      FUNCTION Search (Key: Pointer; Var Index: Sw_Integer): Boolean;Virtual;
      PROCEDURE Insert (Item: Pointer);                              Virtual;
      PROCEDURE Store (Var S: TStream);
   END;
   PSortedCollection = ^TSortedCollection;

TYPE
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ          TStringCollection OBJECT - STRING COLLECTION OBJECT            ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TStringCollection = OBJECT (TSortedCollection)
      FUNCTION GetItem (Var S: TStream): Pointer;                    Virtual;
      FUNCTION Compare (Key1, Key2: Pointer): Sw_Integer;            Virtual;
      PROCEDURE FreeItem (Item: Pointer);                            Virtual;
      PROCEDURE PutItem (Var S: TStream; Item: Pointer);             Virtual;
   END;
   PStringCollection = ^TStringCollection;

TYPE
{ ******************************* REMARK ****************************** }
{    This is a completely NEW FREE VISION ONLY object which holds a     }
{  collection of strings but does not alphabetically sort them. It is   }
{  a very useful object as you will find !!!!                           }
{ ****************************** END REMARK *** Leon de Boer, 15May96 * }
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ        TUnSortedStrCollection - UNSORTED STRING COLLECTION OBJECT       ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   TUnSortedStrCollection = OBJECT (TStringCollection)
      PROCEDURE Insert (Item: Pointer);                              Virtual;
   END;
   PUnSortedStrCollection = ^TUnSortedStrCollection;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                   DYNAMIC STRING INTERFACE ROUTINES                     ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
FUNCTION NewStr (Const S: String): PString;
PROCEDURE DisposeStr (P: PString);

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                       STREAM INTERFACE ROUTINES                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
PROCEDURE Abstract;
PROCEDURE RegisterError;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                     NEW FREE VISION STREAM ROUTINES                     ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

{ ******************************* REMARK ****************************** }
{   This NEW FREE VISION call tries creating a stream in the order of   }
{  the Strategy Mask and will return the successfully created stream    }
{  or nil if it fails using the strategy given.                         }
{ ****************************** END REMARK *** Leon de Boer, 15May96 * }
FUNCTION CreateStream (Strategy: Word; ReqSize: LongInt): PStream;

{ ******************************* REMARK ****************************** }
{   As we have to provide these NEW FREE VISION CALLS as part of our    }
{  stream support we might as well provide them on the interface! They  }
{  mimic the behaviour of the OS2 API calls in most cases.              }
{ ****************************** END REMARK *** Leon de Boer, 16May96 * }

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                    NEW FREE VISION DOS FILE ROUTINES                    ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

{=DosFileOpen=========================================================
Calls the operating system to try to open the file denoted by the given
AsciiZ filename in the requested file mode. Any error is held in
DosStreamError and the call will return zero. If successful and no error
occurs the call will return the file handle of the opened file.
-> Platforms DOS/DPMI/WIN - Checked 16May96 LdB
=====================================================================}
FUNCTION DosFileOpen (Var FileName: AsciiZ; Mode: Word): Word;

{=DosRead============================================================
Calls the operating system to read BufferLength bytes of data from
the file denoted by the handle to the bufferarea. Any error in attempting
to read from the file is held in DosStreamError and returned from call.
If the return is zero (ie no error) BytesMoved contains the number of
bytes read from the file.
-> Platforms DOS/DPMI/WIN - Checked 16May96 LdB
=====================================================================}
FUNCTION DosRead(Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;

{=DosWrite===========================================================
Calls the operating system to write to BufferLength bytes of data from
the bufferarea to the file denoted by the handle. Any error in attempting
to write to the file is held in DosStreamError and returned from call.
If the return is zero (ie no error) BytesMoved contains the number of
bytes written to the file.
-> Platforms DOS/DPMI/WIN - Checked 16May96 LdB
=====================================================================}
FUNCTION DosWrite(Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;

{=DosSetFilePtr======================================================
Calls the operating system to move the file denoted by the handle to
to the requested position. The move method can be: 0 = absolute offset;
1 = offset from present location; 2 = offset from end of file;
Any error is held in DosErrorStream and returned from the call.
If the return is zero (ie no error) NewPos contains the new absolute
file position.
-> Platforms DOS/DPMI/WIN - Checked 16May96 LdB
=====================================================================}
FUNCTION DosSetFilePtr (Handle: Word; Pos: LongInt; MoveType: Word;
Var NewPos: LongInt): Word;

{=DosClose===========================================================
Calls the operating system to close the file handle provided. Any error
in attempting to close file is held DosErrorStream.
-> Platforms DOS/DPMI/WIN - Checked 16May96 LdB
=====================================================================}
PROCEDURE DosClose (Handle: Word);

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                      INITIALIZED PUBLIC VARIABLES                       ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   StreamError: Pointer = Nil;                        { Stream error ptr }
{$IFDEF NotFPKPascal}
   DosStreamError: Sw_Word = $0;                      { Dos stream error }
{$ENDIF}

{ ******************************* REMARK ****************************** }
{  FPK does not accept local variables with it's assembler which means  }
{  these have to be global. Can we please get this error fixed!!!!!     }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
{$IFDEF FPKPascal}                                    { FPK Pascal compiler }
VAR HoldEBP: Sw_Word; TransferHandle: Sw_Word;
    DosStreamError: Sw_Word ;                         { Dos stream error }
{$ENDIF}


{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
{$IFDEF Windows}                                      { WINDOWS CODE }
USES WinTypes, WinProcs;                              { Standard units }
{$ENDIF}

{$IFDEF Speed}                                        { SPEED PASCAL CODE }
USES BseDos;                                          { Speed Pascal def }
{$ENDIF}

{$IFDEF VirtualPascal}                                { VIRTUAL PASCAL CODE }
USES OS2Base;                                         { Virtual Pascal base }
{$ENDIF}

{$IFDEF BPOS2}                                        { C'T PATCH TO BP CODE }

   FUNCTION DosClose (Handle: Word): Word; FAR;
     EXTERNAL 'DOSCALLS' Index 59;                    { Dos close function }

   FUNCTION DosOpen (FileName: PChar; Var Handle: Word;
     Var ActionTaken: Word; FileSize: LongInt;
     FileAttr: Word; OpenFlag, OpenMode: Word;
     Reserved: Pointer): Word; FAR;
     EXTERNAL 'DOSCALLS' Index 70;                    { Dos open function }

   FUNCTION DosRead(Handle: Word; Var BufferArea;
     BufferLength: Word; Var BytesRead : Word): Word; FAR;
     EXTERNAL 'DOSCALLS' Index 137;                   { Dos read procedure }

   FUNCTION DosWrite(Handle: Word; Var BufferArea;
     BufferLength: Word; Var BytesRead : Word): Word; FAR;
     EXTERNAL 'DOSCALLS' Index 138;                   { Dos write procedure }

   FUNCTION DosSetFilePtr (Handle: Word; ulOffset: LongInt;
     MoveType: Word; Var NewPointer: LongInt): LongInt; FAR;
     EXTERNAL 'DOSCALLS' Index 58;                    { Dos write procedure }
{$ENDIF}

{$IFDEF OS2}                                          { OS2 CODE }
CONST
{ Private Os2 File mode magic numbers }
   FmInput  = $20;                                    { Open file for input }
   FmOutput = $31;                                    { Open file for output }
   FmInout  = $42;                                    { Open file }
   FmClosed = $0;                                     { Close file }
{$ENDIF}

{$IFDEF DPMI}                                         { DPMI CODE }
  {$DEFINE NewExeFormat}                              { New format EXE }
{$ENDIF}

{$IFDEF ADV_OS}                                       { WINDOWS/OS2 CODE }
  {$DEFINE NewExeFormat}                              { New format EXE }
{$ENDIF}

CONST
{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                     INITIALIZED PRIVATE VARIABLES                       ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
   StreamTypes: Sw_Word = $0;                         { Stream types }

{***************************************************************************}
{                               OBJECT METHODS                              }
{***************************************************************************}

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                          TRect OBJECT METHODS                           ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

PROCEDURE CheckEmpty (Var Rect: TRect);
{ ******************************* REMARK ****************************** }
{  This is is my desired code but FPK does not like the with statement  }
{  Can we please get this error fixed!!!!!                              }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
{   With Rect Do Begin }
{     If (A.X >= B.X) OR (A.Y >= B.Y) Then Begin    }   { Zero of reversed }
{       A.X := 0;                                   }   { Clear a.x }
{       A.Y := 0;                                   }   { Clear a.y }
{       B.X := 0;                                   }   { Clear b.x }
{       B.Y := 0;                                   }   { Clear b.y }
{     End; }
{   End; }
BEGIN
   If (Rect.A.X >= Rect.B.X) OR
   (Rect.A.Y >= Rect.B.Y) Then Begin                  { Zero of reversed }
     Rect.A.X := 0;                                   { Clear a.x }
     Rect.A.Y := 0;                                   { Clear a.y }
     Rect.B.X := 0;                                   { Clear b.x }
     Rect.B.Y := 0;                                   { Clear b.y }
   End;
END;

{ ******************************* REMARK ****************************** }
{  This is a bug fix of EMPTY from the original code which was:         }
{  Empty := (A.X = B.X) AND (A.Y = B.Y)                                 }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{**TRect********************************************************************}
{  Empty -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
FUNCTION TRect.Empty: Boolean;
BEGIN
   Empty := (A.X >= B.X) OR (A.Y >= B.Y);             { Empty result }
END;

{**TRect********************************************************************}
{  Equals -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB               }
{***************************************************************************}
FUNCTION TRect.Equals (R: TRect): Boolean;
BEGIN
   Equals := (A.X = R.A.X) AND (A.Y = R.A.Y) AND
   (B.X = R.B.X) AND (B.Y = R.B.Y);                   { Equals result }
END;

{ ******************************* REMARK ****************************** }
{  This is a bug fix of Contains from the original code which was:      }
{   Contains := (P.X >= A.X) AND (P.X <= B.X) AND                       }
{     (P.Y >= A.Y) AND (P.Y <= B.Y)                                     }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{**TRect********************************************************************}
{  Contains -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB             }
{***************************************************************************}
FUNCTION TRect.Contains (P: TPoint): Boolean;
BEGIN
   Contains := (P.X >= A.X) AND (P.X < B.X) AND
     (P.Y >= A.Y) AND (P.Y < B.Y);                    { Contains result }
END;

{**TRect********************************************************************}
{  Copy -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TRect.Copy (R: TRect);
BEGIN
   A := R.A;                                          { Copy point a }
   B := R.B;                                          { Copy point b }
END;

{**TRect********************************************************************}
{  Union -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
PROCEDURE TRect.Union (R: TRect);
BEGIN
   If (R.A.X < A.X) Then A.X := R.A.X;                { Take if smaller }
   If (R.A.Y < A.Y) Then A.Y := R.A.Y;                { Take if smaller }
   If (R.B.X > B.X) Then B.X := R.B.X;                { Take if larger }
   If (R.B.Y > B.Y) Then B.Y := R.B.Y;                { Take if larger }
END;

{**TRect********************************************************************}
{  Intersect -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB            }
{***************************************************************************}
PROCEDURE TRect.Intersect (R: TRect);
BEGIN
   If (R.A.X > A.X) Then A.X := R.A.X;                { Take if larger }
   If (R.A.Y > A.Y) Then A.Y := R.A.Y;                { Take if larger }
   If (R.B.X < B.X) Then B.X := R.B.X;                { Take if smaller }
   If (R.B.Y < B.Y) Then B.Y := R.B.Y;                { Take if smaller }
   CheckEmpty(Self);                                  { Check if empty }
END;

{**TRect********************************************************************}
{  Move -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TRect.Move (ADX, ADY: Integer);
BEGIN
   Inc(A.X, ADX);                                     { Adjust A.X }
   Inc(A.Y, ADY);                                     { Adjust A.Y }
   Inc(B.X, ADX);                                     { Adjust B.X }
   Inc(B.Y, ADY);                                     { Adjust B.Y }
END;

{**TRect********************************************************************}
{  Grow -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TRect.Grow (ADX, ADY: Integer);
BEGIN
   Dec(A.X, ADX);                                     { Adjust A.X }
   Dec(A.Y, ADY);                                     { Adjust A.Y }
   Inc(B.X, ADX);                                     { Adjust B.X }
   Inc(B.Y, ADY);                                     { Adjust B.Y }
   CheckEmpty(Self);                                  { Check if empty }
END;

{**TRect********************************************************************}
{  Assign -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB               }
{***************************************************************************}
PROCEDURE TRect.Assign (XA, YA, XB, YB: Integer);
BEGIN
   A.X := XA;                                         { Hold A.X value }
   A.Y := YA;                                         { Hold A.Y value }
   B.X := XB;                                         { Hold B.X value }
   B.Y := YB;                                         { Hold B.Y value }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                          TObject OBJECT METHODS                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

TYPE
   DummyObject = OBJECT (TObject)                     { Internal object }
     Data: RECORD END;                                { Helps size VMT link }
   END;

{ ******************************* REMARK ****************************** }
{ I Prefer this code because it self sizes VMT link rather than using a }
{ fixed record structure thus it should work on all compilers without a }
{ specific record to match each compiler.                               }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
CONSTRUCTOR TObject.Init;
VAR LinkSize: LongInt; Dummy: DummyObject;
BEGIN
   LinkSize := LongInt(@Dummy.Data)-LongInt(@Dummy);  { Calc VMT link size }
   FillChar(Pointer(LongInt(@Self)+LinkSize)^,
     SizeOf(Self)-LinkSize, #0);                      { Clear data fields }
END;

{**TObject******************************************************************}
{  Free -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TObject.Free;
BEGIN
   Dispose(PObject(@Self), Done);                     { Dispose of self }
END;

{**TObject******************************************************************}
{  Done -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
DESTRUCTOR TObject.Done;
BEGIN                                                 { Abstract method }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                          TStream OBJECT METHODS                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

{ ******************************* REMARK ****************************** }
{  Bug fix of TStream.StrRead from the original code which was:         }
{  GetMem(P, L+1) can fail and return Nil which should be checked!      }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{**TStream******************************************************************}
{  StrRead -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB              }
{***************************************************************************}
FUNCTION TStream.StrRead: PChar;
VAR L: Word; P: PChar;
BEGIN
   Read(L, SizeOf(L));                                { Read length }
   If (L=0) Then StrRead := Nil Else Begin            { Check for empty }
     GetMem(P, L + 1);                                { Allocate memory }
     If (P<>Nil) Then Begin                           { Check allocate okay }
       Read(P[0], L);                                 { Read the data }
       P[L] := #0;                                    { Terminate with #0 }
     End;
     StrRead := P;                                    { Return PChar }
   End;
END;

{ ******************************* REMARK ****************************** }
{  Bug fix of TStream.ReadStr from the original code which was:         }
{  GetMem(P, L+1) can fail and return Nil which should be checked!      }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{**TStream******************************************************************}
{  ReadStr -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB              }
{***************************************************************************}
FUNCTION TStream.ReadStr: PString;
VAR L: Byte; P: PString;
BEGIN
   Read(L, 1);                                        { Read string length }
   If (L > 0) Then Begin
     GetMem(P, L + 1);                                { Allocate memory }
     If (P<>Nil) Then Begin                           { Check allocate okay }
       P^[0] := Char(L);                              { Hold length }
       Read(P^[1], L);                                { Read string data }
     End;
     ReadStr := P;                                    { Return string ptr }
   End Else ReadStr := Nil;
END;

{**TStream******************************************************************}
{  GetPos -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB               }
{***************************************************************************}
FUNCTION TStream.GetPos: LongInt;
BEGIN                                                 { Abstract method }
   Abstract;                                          { Abstract error }
END;

{**TStream******************************************************************}
{  GetSize -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB              }
{***************************************************************************}
FUNCTION TStream.GetSize: LongInt;
BEGIN                                                 { Abstract method }
   Abstract;                                          { Abstract error }
END;

{**TStream******************************************************************}
{  Close -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
PROCEDURE TStream.Close;
BEGIN                                                 { Abstract method }
END;

{**TStream******************************************************************}
{  Reset -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
PROCEDURE TStream.Reset;
BEGIN
   Status := 0;                                       { Clear status }
   ErrorInfo := 0;                                    { Clear error info }
END;

{**TStream******************************************************************}
{  Flush -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
PROCEDURE TStream.Flush;
BEGIN                                                 { Abstract method }
END;

{**TStream******************************************************************}
{  Truncate -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB             }
{***************************************************************************}
PROCEDURE TStream.Truncate;
BEGIN
   Abstract;                                          { Abstract error }
END;

{**TStream******************************************************************}
{  Seek -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TStream.Seek (Pos: LongInt);
BEGIN
   Abstract;                                          { Abstract error }
END;

{**TStream******************************************************************}
{  StrWrite -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB             }
{***************************************************************************}
PROCEDURE TStream.StrWrite (P: PChar);
VAR L: Word; Q: PByteArray;
BEGIN
   L := 0;                                            { Preset no size }
   Q := PByteArray(P);                                { Transfer type }
   If (Q<>Nil) Then While (Q^[L]<>0) Do Inc(L);       { Calc PChar length }
   Write(L, SizeOf(L));                               { Store PChar length }
   If (P<>Nil) Then Write(P[0], L);                   { Write data }
END;

{**TStream******************************************************************}
{  WriteStr -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB             }
{***************************************************************************}
PROCEDURE TStream.WriteStr (P: PString);
CONST Empty: String[1] = '';
BEGIN
   If (P<>Nil) Then Write(P^, Length(P^) + 1)         { Write string }
     Else Write(Empty, 1);                            { Write empty string }
END;

{**TStream******************************************************************}
{  Open -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TStream.Open (OpenMode: Word);
BEGIN                                                 { Abstract method }
END;

{**TStream******************************************************************}
{  Error -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
PROCEDURE TStream.Error (Code, Info: Integer);
TYPE TErrorProc = Procedure(Var S: TStream);
BEGIN
   Status := Code;                                    { Hold error code }
   ErrorInfo := Info;                                 { Hold error info }
   If (StreamError<>Nil) Then
     TErrorProc(StreamError)(Self);                   { Call error ptr }
END;

{**TStream******************************************************************}
{  Read -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                 }
{***************************************************************************}
PROCEDURE TStream.Read (Var Buf; Count: Sw_Word);
BEGIN
   Abstract;                                          { Abstract error }
END;

{**TStream******************************************************************}
{  Write -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB                }
{***************************************************************************}
PROCEDURE TStream.Write (Var Buf; Count: Sw_Word);
BEGIN
   Abstract;                                          { Abstract error }
END;

{**TStream******************************************************************}
{  CopyFrom -> Platforms DOS/DPMI/WIN/OS2 - Checked 10May96 LdB             }
{***************************************************************************}
PROCEDURE TStream.CopyFrom (Var S: TStream; Count: Longint);
VAR W: Word; Buffer: Array[0..1023] of Byte;
BEGIN
   While (Count > 0) Do Begin
     If (Count > SizeOf(Buffer)) Then                 { To much data }
       W := SizeOf(Buffer) Else W := Count;           { Size to transfer }
     S.Read(Buffer, W);                               { Read from stream }
     Write(Buffer, W);                                { Write to stream }
     Dec(Count, W);                                   { Dec write count }
   End;
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                        TDosStream OBJECT METHODS                        ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

{**TDosStream***************************************************************}
{  Init -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                 }
{***************************************************************************}
CONSTRUCTOR TDosStream.Init (FileName: FNameStr; Mode: Word);
BEGIN
   Inherited Init;                                    { Call ancestor }
   {$IFDEF Windows}
   AnsiToOem(FileName, FName);                        { Ansi to OEM }
   {$ELSE}
   FileName := FileName+#0;                           { Make asciiz }
   Move(FileName[1], FName, Length(FileName));        { Create asciiz name }
   {$ENDIF}
   Handle := DosFileOpen(FName, Mode);                { Open the file }
   If (Handle=0) Then Begin                           { Open failed }
     Error(stInitError, DosStreamError);              { Call error }
     Status := stInitError;                           { Set fail status }
     Handle := -1;                                    { Set invalid handle }
   End;
END;

{**TDosStream***************************************************************}
{  Done -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                 }
{***************************************************************************}
DESTRUCTOR TDosStream.Done;
BEGIN
   If (Handle <> -1) Then DosClose(Handle);           { Close the file }
   Inherited Done;                                    { Call ancestor }
END;

{**TDosStream***************************************************************}
{  GetPos -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB               }
{***************************************************************************}
FUNCTION TDosStream.GetPos: LongInt;
VAR NewPosition: LongInt;
BEGIN
   If (Status=stOk) Then Begin                        { Check status okay }
     If (Handle = -1) Then DosStreamError := 103      { File not open }
       Else DosStreamError := DosSetFilePtr(Handle,
        0, 1, NewPosition);                           { Get file position }
     If (DosStreamError<>0) Then Begin                { Check for error }
        Error(stError, DosStreamError);               { Identify error }
        NewPosition := -1;                            { Invalidate position }
     End;
     GetPos := NewPosition;                           { Return file position }
   End Else GetPos := -1;                             { Stream in error }
END;

{**TDosStream***************************************************************}
{  GetSize -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB              }
{***************************************************************************}
FUNCTION TDosStream.GetSize: LongInt;
VAR CurrentPos, FileEndPos: LongInt;
BEGIN
   If (Status=stOk) Then Begin                        { Check status okay }
     If (Handle = -1) Then DosStreamError := 103      { File not open }
       Else DosStreamError := DosSetFilePtr(Handle,
        0, 1, CurrentPos);                            { Current position }
     If (DosStreamError=0) Then Begin                 { Check no errors }
        DosStreamError := DosSetFilePtr(Handle, 0, 2,
          FileEndPos);                                { Locate end of file }
        If (DosStreamError=0) Then
          DosSetFilePtr(Handle, 0, 1, CurrentPos);    { Reset position }
     End;
     If (DosStreamError<>0) Then Begin                { Check for error }
        Error(stError, DosStreamError);               { Identify error }
        FileEndPos := -1;                             { Invalidate size }
     End;
     GetSize := FileEndPos;                           { Return file size }
   End Else GetSize := -1;                            { Stream in error }
END;

{**TDosStream***************************************************************}
{  Close -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                }
{***************************************************************************}
PROCEDURE TDosStream.Close;
BEGIN
   If (Handle <> -1) Then DosClose(Handle);           { Close the file }
   Handle := -1;                                      { Handle now invalid }
END;

{**TDosStream***************************************************************}
{  Seek -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                 }
{***************************************************************************}
PROCEDURE TDosStream.Seek (Pos: LongInt);
VAR NewPosition: LongInt;
BEGIN
   If (Status=stOk) Then Begin                        { Check status okay }
     If (Pos < 0) Then Pos := 0;                      { Negatives removed }
     If (Handle = -1) Then DosStreamError := 103      { File not open }
       Else DosStreamError := DosSetFilePtr(Handle,
         Pos, 0, NewPosition);                        { Set file position }
     If ((DosStreamError<>0) OR (NewPosition<>Pos))   { We have an error }
     Then Begin
       If (DosStreamError<>0) Then                    { Error was detected }
         Error(stError, DosStreamError)               { Specific seek error }
         Else Error(stSeekError, 0);                  { General seek error }
     End;
   End;
END;

{**TDosStream***************************************************************}
{  Open -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                 }
{***************************************************************************}
PROCEDURE TDosStream.Open (OpenMode: Word);
BEGIN
   If (Handle = -1) Then Begin                        { File not open }
     Handle := DosFileOpen(FName, OpenMode);          { Open the file }
     If (Handle=0) Then Begin                         { File open failed }
       Error(stOpenError, DosStreamError);            { Call error }
       Handle := -1;                                  { Set invalid handle }
     End;
   End;
END;

{**TDosStream***************************************************************}
{  Read -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                 }
{***************************************************************************}
PROCEDURE TDosStream.Read (Var Buf; Count: Sw_Word);
VAR BytesMoved: Sw_Word;
BEGIN
   If (Status=stOk) Then Begin                        { Check status }
     If (Handle = -1) Then BytesMoved := 0 Else       { File not open }
       DosStreamError := DosRead(Handle, Buf, Count,
         BytesMoved);                                 { Read from file }
     If ((DosStreamError<>0) OR (BytesMoved<>Count))  { We have an error }
     Then Begin
       If (DosStreamError<>0) Then                    { Error was detected }
         Error(stError, DosStreamError)               { Specific read error }
         Else Error(stReadError, 0);                  { General read error }
     End;
   End Else FillChar(Buf, Count, #0);                 { Error clear buffer }
END;

{**TDosStream***************************************************************}
{  Write -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB                }
{***************************************************************************}
PROCEDURE TDosStream.Write (Var Buf; Count: Sw_Word);
VAR BytesMoved: Sw_Word;
BEGIN
   If (Status=stOk) Then Begin
     If (Handle=-1) Then BytesMoved := 0 Else         { File not open }
       DosStreamError := DosWrite(Handle, Buf, Count,
         BytesMoved);                                 { Write to file }
     If ((DosStreamError<>0) OR (BytesMoved<>Count))  { We have an error }
     Then Begin
       If (DosStreamError<>0) Then                    { Error was detected }
         Error(stError, DosStreamError)               { Specific write error }
         Else Error(stWriteError, 0);                 { General write error }
     End;
   End;
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                      TCollection OBJECT METHODS                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

CONSTRUCTOR TCollection.Init (ALimit, ADelta: Sw_Integer);
BEGIN
   Inherited Init;                                    { Call ancestor }
   Delta := ADelta;                                   { Set increment }
   SetLimit(ALimit);                                  { Set limit }
END;

CONSTRUCTOR TCollection.Load (Var S: TStream);
VAR C, I: Sw_Integer;
BEGIN
   S.Read(Count, SizeOf(Count));                      { Read count }
   S.Read(Limit, SizeOf(Limit));                      { Read limit }
   S.Read(Delta, SizeOf(Delta));                      { Read delta }
   Items := Nil;                                      { Clear item pointer }
   C := Count;                                        { Hold count }
   I := Limit;                                        { Hold limit }
   Count := 0;                                        { Clear count }
   Limit := 0;                                        { Clear limit }
   SetLimit(I);                                       { Set requested limit }
   Count := C;                                        { Set count }
   For I := 0 To C-1 Do AtPut(I, GetItem(S));         { Get each item }
END;

DESTRUCTOR TCollection.Done;
BEGIN
   FreeAll;                                           { Free all items }
   SetLimit(0);                                       { Release all memory }
END;

FUNCTION TCollection.At (Index: Sw_Integer): Pointer;
BEGIN
   If (Index < 0) OR (Index >= Count) Then Begin      { Invalid index }
     Error(coIndexError, Index);                      { Call error }
     At := Nil;                                       { Return nil }
   End Else At := Items^[Index];                      { Return item }
END;

{ ******************************* REMARK ****************************** }
{  Bug fix of TCollection.IndexOf from the original code which was:     }
{  For I := 0 To Count-1 Do  <- What happens if count=0!!!!             }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
FUNCTION TCollection.IndexOf (Item: Pointer): Sw_Integer;
VAR I: Sw_Integer;
BEGIN
   If (Count>0) Then Begin                            { Count is positive }
     For I := 0 To Count-1 Do                         { For each item }
       If (Items^[I]=Item) Then Begin                 { Look for match }
         IndexOf := I;                                { Return index }
         Exit;                                        { Now exit }
       End;
   End;
   IndexOf := -1;                                     { Return index }
END;

FUNCTION TCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.Get;                                  { Item off stream }
END;

FUNCTION TCollection.LastThat (Test: Pointer): Pointer;
VAR I: LongInt; P: FuncPtr; {$IFDEF NotFPKPascal} Hold_EBP: Sw_Word; {$ENDIF}
BEGIN
   {$IFDEF FPKPascal}                                 { FPK pascal compiler }
   ASM
     MOVL (%EBP), %EAX;                               { Load EBP }
     MOVL %EAX, U_OBJECTS_HOLDEBP;                    { Store to global }
   END;
   {$ELSE}                                            { Other compilers }
   ASM
     {$IFNDEF CODE_32_BIT}                            { 16 BIT CODE }
       MOV AX, [BP];                                  { Load AX from BP }
       {$IFDEF Windows}
       AND AL, 0FEH;                                  { Windows make even }
       {$ENDIF}
       MOV Hold_EBP, AX;                              { Hold value }
     {$ELSE}                                          { 32 BIT CODE }
       MOV EAX, [EBP];                                { Load EAX from EBP }
       MOV Hold_EBP, EAX;                             { Hold value }
     {$ENDIF}
   END;
   {$ENDIF}
   P := FuncPtr(Test);                                { Set function ptr }
   For I := Count DownTo 1 Do Begin                   { Down from last item }
     {$IFDEF FPKPascal}
       {$$$$$ crahes the compiler
       If P(Items^[I-1], HoldEBP) Then
       } Begin          { Test each item }
     {$ELSE}
       {$IFDEF VirtualPascal}
         If P(Items^[I-1]) Then Begin                 { Test each item }
       {$ELSE}
         If P(Items^[I-1], Hold_EBP) Then Begin       { Test each item }
       {$ENDIF}
     {$ENDIF}
       LastThat := Items^[I-1];                       { Return item }
       Exit;                                          { Now exit }
     End;
   End;
   LastThat := Nil;                                   { None passed test }
END;

FUNCTION TCollection.FirstThat (Test: Pointer): Pointer;
VAR I: LongInt; P: FuncPtr; {$IFDEF NotFPKPascal} Hold_EBP: Sw_Word; {$ENDIF}
BEGIN
   {$IFDEF FPKPascal}                                 { FPK pascal compiler }
   ASM
     MOVL (%EBP), %EAX;                               { Load EBP }
     MOVL %EAX, U_OBJECTS_HOLDEBP;                    { Store to global }
   END;
   {$ELSE}                                            { Other compilers }
   ASM
     {$IFNDEF CODE_32_BIT}                            { 16 BIT CODE }
       MOV AX, [BP];                                  { Load AX from BP }
       {$IFDEF Windows}
       AND AL, 0FEH;                                  { Windows make even }
       {$ENDIF}
       MOV Hold_EBP, AX;                              { Hold value }
     {$ELSE}                                          { 32 BIT CODE }
       MOV EAX, [EBP];                                { Load EAX from EBP }
       MOV Hold_EBP, EAX;                             { Hold value }
     {$ENDIF}
   END;
   {$ENDIF}
   P := FuncPtr(Test);                                { Set function ptr }
   For I := 1 To Count Do Begin                       { Up from first item }
     {$IFDEF FPKPascal}
       {$$$$$$ crashes the compiler
       If P(Items^[I-1], HoldEBP) Then }
       Begin          { Test each item }
     {$ELSE}
       {$IFDEF VirtualPascal}
         If P(Items^[I-1]) Then Begin                 { Test each item }
       {$ELSE}
         If P(Items^[I-1], Hold_EBP) Then Begin       { Test each item }
       {$ENDIF}
     {$ENDIF}
       FirstThat := Items^[I-1];                      { Return item }
       Exit;                                          { Now exit }
     End;
   End;
   FirstThat := Nil;                                  { None passed test }
END;

{ ******************************* REMARK ****************************** }
{  Bug fix of TCollection.Pack from the original code which was:        }
{  While (I<Count) Do  -  Yes but who forget to initialize variable I   }
{  If count is equal to zero this was going to crash big time and you   }
{  must re-adjust the count value - Basically it was stuffed!!!         }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
PROCEDURE TCollection.Pack;
VAR I, J: Sw_Integer;
BEGIN
   If (Count>0) Then Begin                            { Count is positive }
     I := 0;                                          { Initialize dest }
     For J := 1 To Count Do Begin                     { For each item }
       If (Items^[J]<>Nil) Then Begin                 { Entry is non nil }
         Items^[I] := Items^[J];                      { Transfer item }
         Inc(I);                                      { Advance dest }
       End;
     End;
     Count := I;                                      { Adjust count }
   End;
END;

PROCEDURE TCollection.FreeAll;
VAR I: Sw_Integer;
BEGIN
   For I := 0 To Count-1 Do FreeItem(At(I));          { Release each item }
   Count := 0;                                        { Clear item count }
END;

PROCEDURE TCollection.DeleteAll;
BEGIN
   Count := 0;                                        { Clear item count }
END;

PROCEDURE TCollection.Free (Item: Pointer);
BEGIN
   Delete(Item);                                      { Delete from list }
   FreeItem(Item);                                    { Free the item }
END;

PROCEDURE TCollection.Insert (Item: Pointer);
BEGIN
   AtInsert(Count, Item);                             { Insert item }
END;

PROCEDURE TCollection.Delete (Item: Pointer);
BEGIN
   AtDelete(IndexOf(Item));                           { Delete from list }
END;

PROCEDURE TCollection.AtFree (Index: Sw_Integer);
VAR Item: Pointer;
BEGIN
   Item := At(Index);                                 { Retreive item ptr }
   AtDelete(Index);                                   { Delete item }
   FreeItem(Item);                                    { Free the item }
END;

PROCEDURE TCollection.FreeItem (Item: Pointer);
VAR P: PObject;
BEGIN
   P := PObject(Item);                                { Convert pointer }
   If (P<>Nil) Then Dispose(P, Done);                 { Dispose of object }
END;

PROCEDURE TCollection.AtDelete (Index: Sw_Integer);
BEGIN
   If (Index >= 0) AND (Index < Count) Then Begin     { Valid index }
     Dec(Count);                                      { One less item }
     If (Count>Index) Then Move(Items^[Index+1],
      Items^[Index], (Count-Index)*Sizeof(Pointer));  { Shuffle items down }
   End Else Error(coIndexError, Index);               { Index error }
END;

PROCEDURE TCollection.ForEach (Action: Pointer);
VAR I: LongInt; P: ProcPtr; {$IFDEF NotFPKPascal} Hold_EBP: Sw_Word; {$ENDIF}
BEGIN
   {$IFDEF FPKPascal}                                 { FPK pascal compiler }
   ASM
     MOVL (%EBP), %EAX;                               { Load EBP }
     MOVL %EAX, U_OBJECTS_HOLDEBP;                    { Store to global }
   END;
   {$ELSE}                                            { Other compilers }
   ASM
     {$IFNDEF CODE_32_BIT}                            { 16 BIT CODE }
       MOV AX, [BP];
       {$IFDEF WINDOWS}
       AND AL, 0FEH;                                  { Windows make even }
       {$ENDIF}
       MOV Hold_EBP, AX;                              { Hold value }
     {$ELSE}                                          { 32 BIT CODE }
       MOV EAX, [EBP];                                { Load EAX from EBP }
       MOV Hold_EBP, EAX;                             { Hold value }
     {$ENDIF}
   END;
   {$ENDIF}
   P := ProcPtr(Action);                              { Set procedure ptr }
   For I := 1 To Count Do                             { Up from first item }
     {$IFDEF FPKPascal}
       P(Items^[I-1], HoldEBP);                       { Call with each item }
     {$ELSE}
       {$IFDEF VirtualPascal}
         P(Items^[I-1]);                              { Call with each item }
       {$ELSE}
         P(Items^[I-1], Hold_EBP);                    { Call with each item }
       {$ENDIF}
    {$ENDIF}
END;

{ ******************************* REMARK ****************************** }
{  Bug fix of TCollection.SetLimit from the original code which was:    }
{  getmem(p,alimit*sizeof(pointer));  <- This can fail OR ALimit=0      }
{  move(items^,p^,count*sizeof(Pointer)); <- This would now crash!      }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
PROCEDURE TCollection.SetLimit (ALimit: Sw_Integer);
VAR AItems: PItemList;
BEGIN
   If (ALimit < Count) Then ALimit := Count;          { Stop underflow }
   If (ALimit > MaxCollectionSize) Then
     ALimit := MaxCollectionSize;                     { Stop overflow }
   If (ALimit <> Limit) Then Begin                    { Limits differ }
     If (ALimit = 0) Then AItems := Nil Else          { Alimit=0 nil entry }
       GetMem(AItems, ALimit * SizeOf(Pointer));      { Allocate memory }
     If (AItems<>Nil) OR (ALimit=0) Then Begin        { Check success }
       If (AItems <>Nil) AND (Items <> Nil) Then      { Check both valid }
         Move(Items^, AItems^, Count*SizeOf(Pointer));{ Move existing items }
       If (Limit <> 0) AND (Items <> Nil) Then        { Check old allocation }
         FreeMem(Items, Limit * SizeOf(Pointer));     { Release memory }
       Items := AItems;                               { Update items }
       Limit := ALimit;                               { Set limits }
     End;
   End;
END;

PROCEDURE TCollection.Error (Code, Info: Integer);
BEGIN
   RunError(212 - Code);                              { Run error }
END;

PROCEDURE TCollection.AtPut (Index: Sw_Integer; Item: Pointer);
BEGIN
   If (Index >= 0) AND (Index < Count) Then           { Index valid }
     Items^[Index] := Item                            { Put item in index }
     Else Error(coIndexError, Index);                 { Index error }
END;

{ ******************************* REMARK ****************************** }
{  Bug fix of TCollection.AtInsert from the original code which was:    }
{  original remark: copy old items, count is tested by move             }
{  Move(Items^[Index], Items^[Index+1],(Count-Index)*Sizeof(Pointer));  }
{  This does not work you must work from the back down!!!!              }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }
PROCEDURE TCollection.AtInsert (Index: Sw_Integer; Item: Pointer);
VAR I: Sw_Integer;
BEGIN
   If (Index >= 0) AND (Index <= Count) Then Begin    { Valid index }
     If (Count=Limit) Then  SetLimit(Limit+Delta);    { Expand size if able }
     If (Limit>Count) Then Begin
       If (Index < Count) Then Begin                  { Not last item }
         For I := Count DownTo Index Do               { Start from back }
           Items^[I] := Items^[I-1];                  { Move each item }
       End;
       Items^[Index] := Item;                         { Put item in list }
       Inc(Count);                                    { Inc count }
     End Else Error(coOverflow, Index);               { Expand failed }
   End Else Error(coIndexError, Index);               { Index error }
END;

PROCEDURE TCollection.Store (Var S: TStream);

   PROCEDURE DoPutItem (P: Pointer); FAR;
   BEGIN
     PutItem(S, P);                                   { Put item on stream }
   END;

BEGIN
   S.Write(Count, SizeOf(Count));                     { Write count }
   S.Write(Limit, SizeOf(Limit));                     { Write limit }
   S.Write(Delta, SizeOf(Delta));                     { Write delta }
   ForEach(@DoPutItem);                               { Each item to stream }
END;

PROCEDURE TCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.Put(Item);                                       { Put item on stream }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                      TSortedCollection OBJECT METHODS                   ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

CONSTRUCTOR TSortedCollection.Init (ALimit, ADelta: Sw_Integer);
BEGIN
   Inherited Init(ALimit, ADelta);                    { Call ancestor }
   Duplicates := False;                               { Clear flag }
END;

CONSTRUCTOR TSortedCollection.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(Duplicates, SizeOf(Duplicates));            { Read duplicate flag }
END;

FUNCTION TSortedCollection.KeyOf (Item: Pointer): Pointer;
BEGIN
   KeyOf := Item;                                     { Return item }
END;

FUNCTION TSortedCollection.IndexOf (Item: Pointer): Sw_Integer;
VAR I: Sw_Integer;
BEGIN
   IndexOf := -1;                                     { Preset result }
   If Search(KeyOf(Item), I) Then Begin               { Search for item }
     If Duplicates Then                               { Duplicates allowed }
       While (I < Count) AND (Item <> Items^[I]) Do
         Inc(I);                                      { Count duplicates }
     If (I < Count) Then IndexOf := I;                { Return result }
   End;
END;

FUNCTION TSortedCollection.Compare (Key1, Key2: Pointer): Sw_Integer;
BEGIN
   Abstract;                                          { Abstract method }
END;

FUNCTION TSortedCollection.Search (Key: Pointer; Var Index: Sw_Integer): Boolean;
VAR L, H, I, C: Sw_Integer;
BEGIN
   Search := False;                                   { Preset failure }
   L := 0;                                            { Start count }
   H := Count - 1;                                    { End count }
   While (L <= H) Do Begin
     I := (L + H) SHR 1;                              { Mid point }
     C := Compare(KeyOf(Items^[I]), Key);             { Compare with key }
     If (C < 0) Then L := I + 1 Else Begin            { Item to left }
       H := I - 1;                                    { Item to right }
       If C = 0 Then Begin                            { Item match found }
         Search := True;                              { Result true }
         If NOT Duplicates Then L := I;               { Force kick out }
       End;
     End;
   End;
   Index := L;                                        { Return result }
END;

PROCEDURE TSortedCollection.Insert (Item: Pointer);
VAR I: Sw_Integer;
BEGIN
   If NOT Search(KeyOf(Item), I) OR Duplicates Then   { Item valid }
     AtInsert(I, Item);                               { Insert the item }
END;

PROCEDURE TSortedCollection.Store (Var S: TStream);
BEGIN
   TCollection.Store(S);                              { Call ancestor }
   S.Write(Duplicates, SizeOf(Duplicates));           { Write duplicate flag }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                    TStringCollection OBJECT METHODS                     ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

FUNCTION TStringCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.ReadStr;                              { Get new item }
END;

FUNCTION TStringCollection.Compare (Key1, Key2: Pointer): Sw_Integer;
VAR I, J: Integer; P1, P2: PString;
BEGIN
   P1 := PString(Key1);                               { String 1 pointer }
   P2 := PString(Key2);                               { String 2 pointer }
   If (Length(P1^)<Length(P2^)) Then J := Length(P1^)
     Else J := Length(P2^);                           { Shortest length }
   I := 1;                                            { First character }
   While (I<J) AND (P1^[I]=P2^[I]) Do Inc(I);         { Scan till fail }
   If (P1^[I]=P2^[I]) Then Compare := 0 Else          { Strings matched }
     If (P1^[I]<P2^[I]) Then Compare := -1 Else       { String1 < String2 }
        Compare := 1;                                 { String1 > String2 }
END;

PROCEDURE TStringCollection.FreeItem (Item: Pointer);
BEGIN
   DisposeStr(Item);                                  { Dispose item }
END;

PROCEDURE TStringCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.WriteStr(Item);                                  { Write string }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                  TUnSortedStrCollection OBJECT METHODS                  ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
PROCEDURE TUnSortedStrCollection.Insert (Item: Pointer);
BEGIN
   AtInsert(Count, Item);                             { NO sorting insert }
END;




FUNCTION TStream.Get: PObject;
BEGIN
END;

PROCEDURE TStream.Put (P: PObject);
BEGIN
END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                   DYNAMIC STRING INTERFACE ROUTINES                     ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}
FUNCTION NewStr (Const S: String): PString;
VAR P: PString;
BEGIN
   If (S = '') Then P := Nil Else Begin               { Return nil }
     GetMem(P, Length(S) + 1);                        { Allocate memory }
     If (P<>Nil) Then P^ := S;                        { Hold string }
   End;
   NewStr := P;                                       { Return result }
END;

PROCEDURE DisposeStr (P: PString);
BEGIN
   If (P <> Nil) Then FreeMem(P, Length(P^) + 1);     { Release memory }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                       STREAM INTERFACE ROUTINES                         ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

PROCEDURE Abstract;
BEGIN
   RunError(211);                                     { Abstract error }
END;

PROCEDURE RegisterError;
BEGIN
   RunError(212);                                     { Register error }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                     NEW FREE VISION STREAM ROUTINES                     ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

FUNCTION CreateStream (Strategy: Word; ReqSize: LongInt): PStream;
VAR Stream: PStream;
BEGIN
   Stream := Nil;                                     { Preset failure }
   While (Strategy <> 0) AND (Stream = Nil) Do Begin
     If (Strategy AND sa_XMSFirst <> 0) Then Begin    { ** XMS STREAM ** }
     End Else
     If (Strategy AND sa_EMSFirst <> 0) Then Begin    { ** EMS STREAM ** }
     End Else
     If (Strategy AND sa_RamFirst <> 0) Then Begin    { ** RAM STREAM ** }
     End Else
     If (Strategy AND sa_DiskFirst <> 0) Then Begin   { ** DISK STREAM ** }
     End;
     If (Stream<>Nil) AND (Stream^.Status <> stOk)    { Stream in error }
     Then Begin
       Dispose(Stream, Done);                         { Dispose stream }
       Stream := Nil;                                 { Clear pointer }
     End;
     Strategy := Strategy SHL 4;                      { Next strategy mask }
   End;
   CreateStream := Stream;                            { Return stream result }
END;

{ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›}
{ﬁ                    NEW FREE VISION DOS FILE ROUTINES                    ›}
{ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›}

{***************************************************************************}
{  DosFileOpen -> Platforms DOS/DPMI/WIN/OS2 - Checked 16May96 LdB          }
{***************************************************************************}
FUNCTION DosFileOpen (Var FileName: AsciiZ; Mode: Word): Word;
{$IFDEF NotOS2}                                       { DOS/DPMI/WINDOWS }
   {$IFDEF FPKPascal}                                 { FPK Pascal compiler }
     {$IFDEF GO32V2}
var regs : trealregs;
BEGIN
         syscopytodos(longint(@FileName),256);
         regs.realedx:=tb mod 16;
         regs.realds:=tb div 16;
         regs.realeax := Mode;
         regs.realecx:=0;
         sysrealintr($21,regs);
         if (regs.realflags and 1) <> 0 then
           begin
           InOutRes:=lo(regs.realeax);
           DosFileOpen:=-1;
           exit;
           end else
           DosFileOpen:=regs.realeax;
END;
    {$ELSE not GO32V2}
BEGIN
   ASM
     XOR %AX, %AX;                                    { Clear error }
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;
     MOVL Filename, %EDX;                             { Filename to open }
     XOR %CX, %CX;
     MOVW Mode, %AX;                               { Mode to open file }
     PUSHL %EBP;
     INT $0x21;                                         { Open/create the file }
     POPL %EBP;
     JNC EXIT1;
     MOV %AX, U_OBJECTS_DOSSTREAMERROR;               { Hold error }
     XOR %AX, %AX;                                    { Open failed }
   EXIT1:
     MOV %AX, U_OBJECTS_TRANSFERHANDLE;               { Hold opened handle }
   END;
   DosFileOpen := TransferHandle;                     { Return handle }
END;
     {$ENDIF GO32V2}
   {$ELSE}                                            { Other compilers }
ASSEMBLER;
   ASM
     XOR AX, AX;                                      { Dos error cleared }
     MOV DosStreamError, AX;
     MOV AX, Mode;                                    { Mode to open file }
     PUSH DS;
     LDS DX, FileName;                                { Filename to open }
     XOR CX, CX;
     INT $21;                                         { Open/create file }
     POP DS;
     JNC @@Exit1;                                     { Check for error }
     MOV DosStreamError, AX;
     XOR AX, AX;                                      { Open fail return 0 }
   @@Exit1:
   END;
   {$ENDIF}
{$ELSE}                                               { OS2 CODE }
{$IFNDEF FPK}
VAR Attr, OpenFlags, OpenMode: Word; Success, Handle, ActionTaken: Sw_Word;
BEGIN
   Case Mode Of
     stCreate: Begin                                  { Create file }
         Attr := $20;                                 { Archive file }
         OpenFlags := 18;                             { Open flags }
         OpenMode := FmInOut;                         { Input/output file }
       End;
     stOpenRead: Begin                                { Open file for read }
         Attr := $0;                                  { Any attributes }
         OpenFlags := 1;                              { Open flags }
         OpenMode := FmInput;                         { Input file }
       End;
     stOpenWrite: Begin                               { Open file for write }
         Attr := $0;                                  { Any attributes }
         OpenFlags := 1;                              { Open flags }
         OpenMode := FmOutput;                        { Output file }
       End;
     stOpen: Begin                                    { Open file read/write }
         Attr := $0;                                  { Any attributes }
         OpenFlags := 1;                              { Open flags }
         OpenMode := FmInOut;                         { Input/output file }
       End;
   End;
   {$IFDEF Speed}                                     { Speed pascal differs }
   DosStreamError := DosOpen(CString(FileName), Handle,
   {$ELSE}                                            { Other OS2 compilers }
   DosStreamError := DosOpen(@FileName[0], Handle,
   {$ENDIF}
     ActionTaken, 0, Attr, OpenFlags, OpenMode, Nil); { Open the file }
   If (DosStreamError=0) Then DosFileOpen := Handle   { Successful open }
     Else DosFileOpen := 0;                           { Fail so return zero }
END;
{$ELSE FPK}
BEGIN
   ASM
     XOR %AX, %AX;                                    { Clear error }
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;
     MOVL Filename, %EDX;                             { Filename to open }
     XOR %CX, %CX;
     MOVW Mode, %AX;                                  { Mode to open file }
     CALL ___SYSCALL;                                 { Open/create the file }
     JNC EXIT1;
     MOV %AX, U_OBJECTS_DOSSTREAMERROR;               { Hold error }
     XOR %AX, %AX;                                    { Open failed }
   EXIT1:
     MOV %AX, U_OBJECTS_TRANSFERHANDLE;               { Hold opened handle }
   END;
   DosFileOpen := TransferHandle;                     { Return handle }
END;
{$ENDIF FPK}
{$ENDIF}

{***************************************************************************}
{  DosRead -> Platforms DOS/DPMI/WIN - Checked 16May96 LdB                  }
{***************************************************************************}
FUNCTION DosRead (Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
{$IFDEF FPKPascal}                                    { FPK pascal compiler }
{$IFDEF GO32V2}
BEGIN
BytesMoved:=system.dosread(Handle,longint(@BufferArea),BufferLength);
DosRead:=InOutRes;
End;
{$ELSE not GO32V2}
{$IFNDEF OS2}
BEGIN
   ASM
     MOVL BufferArea, %EDX;                             { Buffer for data }
     MOVL BufferLength, %CX;                              { Bytes to read }
     MOVB $0x3F, %AH;
     MOVW Handle, %BX;                              { Load file handle }
     PUSHL %EBP;
     INT $0x21;                                         { Read from file }
     POPL %EBP;
     JC EXIT2;                                        { Check for error }
     MOVL BytesMoved, %EDI;
     MOVZWL %AX, %EAX;
     MOVL %EAX, (%EDI);                               { Update bytes moved }
     XOR %EAX, %EAX;                                  { Clear register }
   EXIT2:
     MOV %AX, U_OBJECTS_DOSSTREAMERROR;               { DOS error returned }
   END;
   DosRead := DosStreamError;                         { Return any error }
END;
{$ELSE OS2}
BEGIN
   ASM
     MOVL BufferArea, %EDX;                             { Buffer for data }
     MOVL BufferLength, %CX;                              { Bytes to read }
     MOVB $0x3F, %AH;
     MOVW Handle, %BX;                                { Load file handle }
     CALL ___SYSCALL;                                 { Read from file }
     JC EXIT2;                                        { Check for error }
     MOVL BytesMoved, %EDI;
     MOVZWL %AX, %EAX;
     MOVL %EAX, (%EDI);                               { Update bytes moved }
     XOR %EAX, %EAX;                                  { Clear register }
   EXIT2:
     MOV %AX, U_OBJECTS_DOSSTREAMERROR;               { DOS error returned }
   END;
   DosRead := DosStreamError;                         { Return any error }
END;
{$ENDIF OS2}
{$EndIf GO32V2}
{$ELSE}                                               { Other compilers }
ASSEMBLER;
   ASM
     PUSH DS;
     LDS DX, BufferArea;                              { Data dest buffer }
     MOV CX, BufferLength;
     MOV BX, Handle;                                  { Load file handle }
     MOV AH, $0x3F;
     INT $0x21;                                         { Read from file }
     POP DS;
     JC @@Exit2;                                      { Check for error }
     LES DI, BytesMoved;
     MOV ES:[DI], AX;                                 { Update bytes moved }
     XOR AX, AX;
   @@Exit2:
     MOV DosStreamError, AX;                          { DOS error returned }
   END;
{$ENDIF}

{***************************************************************************}
{  DosWrite -> Platforms DOS/DPMI/WIN - Checked 16May96 LdB                 }
{***************************************************************************}
FUNCTION DosWrite (Handle: Word; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
{$IFDEF FPKPascal}                                    { FPK pascal compiler }
{$IFDEF GO32V2}
BEGIN
system.doswrite(Handle,longint(@BufferArea),BufferLength);
BytesMoved:=BufferLength;
DosWrite:=InOutRes;
End;
{$ELSE not GO32V2}
BEGIN
   ASM
     MOVL BufferArea, %EDX;                             { Buffer with data }
     MOVL BufferLength, %CX;                              { Bytes to write }
     MOVB $0x40, %AH;
     MOVW Handle, %BX;                              { Load file handle }
     PUSHL %EBP;
     INT $0x21;                                         { Write to file }
     POPL %EBP;
     JC EXIT3;                                        { Check for error }
     MOVL BytesMoved, %EDI;
     MOVZWL %AX, %EAX;
     MOVL %EAX, (%EDI);                               { Update bytes moved }
     XOR %EAX, %EAX;
   EXIT3:
     MOV %AX, U_OBJECTS_DOSSTREAMERROR;               { DOS error returned }
   END;
   DosWrite := DosStreamError;                        { Return any error }
END;
{$ENDIF GO32V2}
{$ELSE}                                               { Other compilers }
ASSEMBLER;
   ASM
     PUSH DS;
     LDS DX, BufferArea;                              { Data source buffer }
     MOV CX, BufferLength;
     MOV BX, Handle;                                  { Load file handle }
     MOV AH, $40;
     INT $21;                                         { Write to file }
     POP DS;
     JC @@Exit3;                                      { Check for error }
     LES DI, BytesMoved;
     MOV ES:[DI], AX;                                 { Update bytes moved }
     XOR AX, AX;
   @@Exit3:
     MOV DosStreamError, AX;                          { DOS error returned }
   END;
{$ENDIF}

{***************************************************************************}
{  DosSetFilePtr -> Platforms DOS/DPMI/WIN - Checked 16May96 LdB            }
{***************************************************************************}
FUNCTION DosSetFilePtr (Handle: Word; Pos: LongInt; MoveType: Word;
VAR NewPos: LongInt): Word;
{$IFDEF FPKPascal}                                    { FPK pascal compiler }
{$IFNDEF OS2}
BEGIN
   ASM
     MOVW MoveType, %AX;                              { Load move type }
     MOVB $0x42, %AH;
     MOVW POS, %DX;                              { Load file position }
     MOVL POS, %ECX;
     SHRL $16,%ECX;
     MOVW Handle, %BX;                              { Load file handle }
     PUSHL %EBP;
     INT $0x21;                                         { Position the file }
     POPL %EBP;
     JC EXIT4;
     MOVL NewPos, %EDI;                              { New position address }
     MOVW %AX, %BX;
     MOVW %DX, %AX;
     SHLL $0x10, %EAX;                                   { Roll to high part }
     MOVW %BX, %AX;
     MOVL %EAX, (%EDI);                               { Update new position }
     XOR %EAX, %EAX;
   EXIT4:
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;              { DOS error returned }
   END;
   DosSetFilePtr := DosStreamError;                   { Return any error }
END;
{$ELSE OS2}
BEGIN
   ASM
     MOVW MoveType, %AX;                              { Load move type }
     MOVB $0x42, %AH;
     MOVW POS, %DX;                                  { Load file position }
     MOVL POS, %ECX;
     SHRL $16,%ECX;
     MOVW Handle, %BX;                               { Load file handle }
     CALL ___SYSCALL;                                { Position the file }
     JC EXIT4;
     MOVL NewPos, %EDI;                              { New position address }
     MOVW %AX, %BX;
     MOVW %DX, %AX;
     SHLL $0x10, %EAX;                                   { Roll to high part }
     MOVW %BX, %AX;
     MOVL %EAX, (%EDI);                               { Update new position }
     XOR %EAX, %EAX;
   EXIT4:
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;              { DOS error returned }
   END;
   DosSetFilePtr := DosStreamError;                   { Return any error }
END;
{$ENDIF OS2}
{$ELSE}                                               { Other compilers }
ASSEMBLER;
   ASM
     MOV AX, MoveType;                                { Load move type }
     MOV AH, $42;
     MOV DX, Pos.Word[0];                             { Load file position }
     MOV CX, Pos.Word[2];
     MOV BX, Handle;                                  { Load file handle }
     INT $21;                                         { Position the file }
     JC @@Exit4;
     LES DI, NewPos;                                  { New position address }
     MOV ES:[DI], AX;
     MOV ES:[DI+2], DX;                               { Update new position }
     XOR AX, AX;
   @@Exit4:
     MOV DosStreamError, AX;                          { DOS error returned }
   END;
{$ENDIF}

{***************************************************************************}
{  DosClose -> Platforms DOS/DPMI/WIN - Checked 16May96 LdB                 }
{***************************************************************************}
PROCEDURE DosClose (Handle: Word);
{$IFDEF FPKPascal}                                    { FPK pascal compiler }
{$IFNDEF OS2}
BEGIN
   ASM
     XOR %AX, %AX;
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;              { DOS error cleared }
     MOVB $0x3E, %AH;
     MOVW Handle, %BX;                               { DOS file handle }
     PUSHL %EBP;
     INT $0x21;                                         { Close the file }
     POPL %EBP;
     JNC EXIT5;
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;              { DOS error returned }
   EXIT5:
   END;
END;
{$ELSE OS2}
BEGIN
   ASM
     XOR %AX, %AX;
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;              { DOS error cleared }
     MOVB $0x3E, %AH;
     MOVW Handle, %BX;                                { DOS file handle }
     CALL ___SYSCALL;                                 { Close the file }
     JNC EXIT5;
     MOVW %AX, U_OBJECTS_DOSSTREAMERROR;              { DOS error returned }
   EXIT5:
   END;
END;
{$ENDIF OS2}
{$ELSE}                                               { Other compilers }
ASSEMBLER;
   ASM
     XOR AX, AX;                                      { DOS error cleared }
     MOV DosStreamError, AX;
     MOV BX, Handle;                                  { DOS file handle }
     MOV AH, $3E;
     INT $21;                                         { Close the file }
     JNC @@Exit5;
     MOV DosStreamError, AX;                          { DOS error returned }
   @@Exit5:
   END;
{$ENDIF}


END.
