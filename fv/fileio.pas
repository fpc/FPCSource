{ $Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{          System independent FILE I/O control             }
{                                                          }
{   Copyright (c) 1996, 1997, 1998, 1999 by Leon de Boer   }
{   ldeboer@attglobal.net  - primary e-mail address        }
{   ldeboer@projectent.com.au - backup e-mail address      }
{                                                          }
{****************[ THIS CODE IS FREEWARE ]*****************}
{                                                          }
{     This sourcecode is released for the purpose to       }
{   promote the pascal language on all platforms. You may  }
{   redistribute it and/or modify with the following       }
{   DISCLAIMER.                                            }
{                                                          }
{     This SOURCE CODE is distributed "AS IS" WITHOUT      }
{   WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR     }
{   ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.     }
{                                                          }
{*****************[ SUPPORTED PLATFORMS ]******************}
{     16 and 32 Bit compilers                              }
{        DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{        DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - FPC 0.9912+ (GO32V2)    (32 Bit)       }
{        WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - Delphi 1.0+             (16 Bit)       }
{        WIN95/NT - Delphi 2.0+             (32 Bit)       }
{                 - Virtual Pascal 2.0+     (32 Bit)       }
{                 - Speedsoft Sybil 2.0+    (32 Bit)       }
{                 - FPC 0.9912+             (32 Bit)       }
{        OS2      - Virtual Pascal 1.0+     (32 Bit)       }
{                 - Speed Pascal 1.0+       (32 Bit)       }
{                 - C'T patch to BP         (16 Bit)       }
{        LINUX    - FPC 1.0.2+              (32 Bit)       }
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     12 Jun 96   First DOS/DPMI platform release    }
{  1.10     12 Mar 97   Windows conversion added.          }
{  1.20     29 Aug 97   Platform.inc sort added.           }
{  1.30     12 Jun 98   Virtual pascal 2.0 code added.     }
{  1.40     10 Sep 98   Checks run & commenting added.     }
{  1.50     28 Oct 98   Fixed for FPC version 0.998        }
{                       Only Go32v2 supported no Go32v1    }
{  1.60     14 Jun 99   References to Common.pas added.    }
{  1.61     07 Jul 99   Speedsoft SYBIL 2.0 code added.    }
{  1.62     03 Nov 99   FPC windows support added.         }
{  1.70     10 Nov 00   Revamp using changed common unit   }
{**********************************************************}

UNIT FileIO;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC} { FPC doesn't support these switches }
  {$F-} { Short calls are okay }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$P-} { Normal string variables }
  {$E+} {  Emulation is on }
  {$N-} {  No 80x87 code generation }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$IFNDEF OS_UNIX}
{$S-} { Disable Stack Checking }
{$ENDIF}
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

{$IFDEF OS_DOS}                                       { DOS/DPMI ONLY }
  {$IFDEF PPC_FPC}                                    { FPC COMPILER }
    {$IFNDEF GO32V2}                                  { MUST BE GO32V2 }
    This only works in GO32V2 mode in FPC!
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

USES
  {$IFDEF WIN16} WinTypes, WinProcs, {$ENDIF}         { Stardard BP units }
  FVCommon;                                           { Standard GFV unit }

{***************************************************************************}
{                             PUBLIC CONSTANTS                              }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                         FILE ACCESS MODE CONSTANTS                        }
{---------------------------------------------------------------------------}
CONST
   fa_Create    = $3C00;                              { Create new file }
   fa_OpenRead  = $3D00;                              { Read access only }
   fa_OpenWrite = $3D01;                              { Write access only }
   fa_Open      = $3D02;                              { Read/write access }

{---------------------------------------------------------------------------}
{                         FILE SHARE MODE CONSTANTS                         }
{---------------------------------------------------------------------------}
CONST
   fm_DenyAll   = $0010;                              { Exclusive file use }
   fm_DenyWrite = $0020;                              { Deny write access }
   fm_DenyRead  = $0030;                              { Deny read access }
   fm_DenyNone  = $0040;                              { Deny no access }

{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
CONST
   HFILE_ERROR = -1;                                  { File handle error }
{$ENDIF}

{***************************************************************************}
{                          PUBLIC TYPE DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                              ASCIIZ FILENAME                              }
{---------------------------------------------------------------------------}
TYPE
   AsciiZ = Array [0..255] Of Char;                   { Filename array }

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           FILE CONTROL ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-FileClose----------------------------------------------------------
The file opened by the handle is closed. If close action is successful
true is returned but if the handle is invalid or a file error occurs
false will be returned.
14Nov00 LdB
---------------------------------------------------------------------}
FUNCTION FileClose (Handle: THandle): Boolean;

{-FileOpen-----------------------------------------------------------
Given a valid filename to file that exists, is not locked with a valid
access mode the file is opened and the file handle returned. If the
name or mode is invalid or an error occurs the return will be zero.
27Oct98 LdB
---------------------------------------------------------------------}
FUNCTION FileOpen (Const FileName: AsciiZ; Mode: Word): THandle;

{-SetFileSize--------------------------------------------------------
The file opened by the handle is set the given size. If the action is
successful zero is returned but if the handle is invalid or a file error
occurs a standard file error value will be returned.
21Oct98 LdB
---------------------------------------------------------------------}
FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;

{-SetFilePos---------------------------------------------------------
The file opened by the handle is set the given position in the file.
If the action is successful zero is returned but if the handle is invalid
the position is beyond the file size or a file error occurs a standard
file error value will be returned.
21Oct98 LdB
---------------------------------------------------------------------}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;

{-FileRead-----------------------------------------------------------
The file opened by the handle has count bytes read from it an placed
into the given buffer. If the read action is successful the actual bytes
transfered is returned in actual and the function returns zero. If an
error occurs the function will return a file error constant and actual
will contain the bytes transfered before the error if any.
22Oct98 LdB
---------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;

{-FileWrite----------------------------------------------------------
The file opened by the handle has count bytes written to it from the
given buffer. If the write action is successful the actual bytes
transfered is returned in actual and the function returns zero. If an
error occurs the function will return a file error constant and actual
will contain the bytes transfered before the error if any.
22Oct98 LdB
---------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                               IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{$IFDEF OS_WINDOWS}                                   { WIN/NT UNITS }

  {$IFNDEF PPC_SPEED}                                 { NON SPEED COMPILER }
    {$IFDEF WIN32}                                    { WIN32 COMPILER }
    USES Windows;                                     { Standard unit }
    {$ENDIF}
  TYPE LongWord = LongInt;                            { Type fixup }
  {$ELSE}                                             { SPEEDSOFT COMPILER }
  USES WinNT, WinBase;                                { Standard units }
  {$ENDIF}

{$ENDIF}

{$IFDEF OS_OS2}                                       { OS2 COMPILERS }

  {$IFDEF PPC_VIRTUAL}                                { VIRTUAL PASCAL UNITS }
  USES OS2Base;                                       { Standard unit }
  {$ENDIF}

  {$IFDEF PPC_SPEED}                                  { SPEED PASCAL UNITS }
  USES BseDos, Os2Def;                                { Standard units }
  {$ENDIF}

  {$IFDEF PPC_BPOS2}                                  { C'T PATCH TO BP UNITS }
  USES DosTypes, DosProcs;                            { Standard units }
  {$ENDIF}

  {$IFDEF PPC_FPC}                                    { FPC UNITS }
  USES DosCalls, OS2Def;                              { Standard units }
  {$ENDIF}
  
{$ENDIF}

{$IFDEF OS_UNIX}                                     { LINUX COMPILER }
  USES
    {$ifdef VER1_0}
      linux;
    {$else}
      unix;
    {$endif}
{$ENDIF}

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{  FileClose -> Platforms DOS/DPMI/WIN/NT/OS2/LINUX - Updated 14Nov00 LdB   }
{---------------------------------------------------------------------------}
FUNCTION FileClose (Handle: THandle): Boolean;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOV BX, Handle;                                  { DOS file handle }
     MOV AX, $3E00;                                   { Close function }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Close the file }
     POP BP;                                          { Reload register }
     MOV AL, True;                                    { Preset true }
     JNC @@Exit1;                                     { Return success }
     MOV AL, False;                                   { Return failure }
   @@Exit1:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   VAR Regs: TRealRegs;
   BEGIN
     Regs.RealEBX := Handle;                          { Transfer handle }
     Regs.RealEAX := $3E00;                           { Close file function }
     SysRealIntr($21, Regs);                          { Call DOS interrupt }
     If (Regs.RealFlags AND $1 = 0) Then              { Check carry flag }
       FileClose := True Else FileClose := False;     { Return true/false }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
BEGIN
   {$IFDEF BIT_16}                                    { 16 BIT WINDOWS CODE }
   If (_lclose(Handle) = 0) Then FileClose := True    { Close the file }
     Else FileClose := False;                         { Closure failed }
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT WINDOWS CODE }
   FileClose := CloseHandle(Handle);                  { Close the file }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
BEGIN
   If (DosClose(Handle) = 0) Then FileClose := True   { Try to close file }
     Else FileClose := False;                         { Closure failed }
END;
{$ENDIF}
{$IFDEF OS_UNIX}                                     { LINUX CODE }
BEGIN
   fdClose(Handle);                                   { Close the file }
   FileClose := LinuxError <= 0
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  FileOpen -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION FileOpen (Const FileName: AsciiZ; Mode: Word): THandle;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOV AX, Mode;                                    { Mode to open file }
     XOR CX, CX;                                      { No attributes set }
     PUSH DS;                                         { Save segment }
     LDS DX, FileName;                                { Filename to open }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Open/create file }
     POP BP;                                          { Restore register }
     POP DS;                                          { Restore segment }
     JNC @@Exit2;                                     { Check for error }
     XOR AX, AX;                                      { Open fail return 0 }
   @@Exit2:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   VAR Regs: TRealRegs;
   BEGIN
     SysCopyToDos(LongInt(@FileName), 256);           { Transfer filename }
     Regs.RealEDX := Tb MOD 16;
     Regs.RealDS := Tb DIV 16;                        { Linear addr of Tb }
     Regs.RealEAX := Mode;                            { Mode to open with }
     Regs.RealECX := 0;                               { No attributes set }
     SysRealIntr($21, Regs);                          { Call DOS int 21 }
     If (Regs.RealFlags AND 1 <> 0) Then FileOpen := 0{ Error encountered }
       Else FileOpen := Regs.RealEAX AND $FFFF;       { Return file handle }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
VAR Hnd: Integer; OpenMode: Sw_Word;
  {$IFDEF BIT_16} Buf: TOfStruct; {$ENDIF}            { 16 BIT VARIABLES }
  {$IFDEF BIT_32} ShareMode, Flags: LongInt; {$ENDIF} { 32 BIT VARIABLES }
BEGIN
   {$IFDEF BIT_16}                                    { 16 BIT WINDOW CODE }
   If (Mode = fa_Create) Then OpenMode := of_Create   { Set create mask bit }
     Else OpenMode := Mode AND $00FF;                 { Set open mask bits }
   Hnd := OpenFile(FileName, Buf, OpenMode);          { Open the file }
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT WINDOWS CODE }
   If (Mode = fa_Create) Then Begin                   { Create file }
     OpenMode := Generic_Read OR Generic_Write;       { Set access mask bit }
     Flags := Create_Always;                          { Create always mask }
   End Else Begin                                     { Open the file }
     OpenMode := Generic_Read;                        { Read only access set }
     If (Mode AND $0001 <> 0) Then                    { Check write flag }
       OpenMode := OpenMode AND NOT Generic_Read;     { Write only access set }
     If (Mode AND $0002 <> 0) Then                    { Check read/write flag }
       OpenMode := OpenMode OR Generic_Write;         { Read/Write access }
     Flags := Open_Existing;                          { Open existing mask }
   End;
   ShareMode := file_Share_Read OR
     file_Share_Write;                                { Deny none flag set }
   Hnd := CreateFile(FileName, OpenMode, ShareMode,
    Nil, Flags, File_Attribute_Normal, 0);            { Open the file }
   {$ENDIF}
   If (Hnd <> -1) Then FileOpen := Hnd Else           { Return handle }
     FileOpen := 0;                                   { Return error }
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
VAR OpenFlags, OpenMode: Word; Handle, ActionTaken: Sw_Word;
BEGIN
   If (Mode = fa_Create) Then Begin                   { Create file }
     OpenMode := Open_Flags_NoInherit OR
       Open_Share_DenyNone OR
       Open_Access_ReadWrite;                         { Open mode }
     OpenFlags := OPEN_ACTION_CREATE_IF_NEW OR
        OPEN_ACTION_REPLACE_IF_EXISTS;                { Open flags }
   End Else Begin
     OpenMode := Mode AND $00FF OR
       Open_Share_DenyNone;                           { Set open mode bits }
     OpenFlags := OPEN_ACTION_OPEN_IF_EXISTS;         { Set open flags }
   End;
   {$IFDEF PPC_BPOS2}                                 { C'T patched COMPILER }
   If (DosOpen(@FileName, Handle, ActionTaken, 0, 0,
     OpenFlags, OpenMode, 0) = 0) Then
       FileOpen := Handle Else FileOpen := 0;         { Return handle/fail }
   {$ELSE}                                            { OTHER OS2 COMPILERS }
    {$IFDEF PPC_FPC}
    If (DosOpen(@FileName, Longint(Handle), ActionTaken, 0, 0,
      OpenFlags, OpenMode, Nil) = 0) Then
        FileOpen := Handle Else FileOpen := 0;         { Return handle/fail }
    {$ELSE}
    If (DosOpen(FileName, Handle, ActionTaken, 0, 0,
      OpenFlags, OpenMode, Nil) = 0) Then
        FileOpen := Handle Else FileOpen := 0;         { Return handle/fail }
    {$ENDIF}
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_UNIX}
BEGIN
   if mode = fa_Create    then mode := Open_Creat or Open_RdWr else
   if mode = fa_OpenRead  then mode := Open_RdOnly             else
   if mode = fa_OpenWrite then mode := Open_WrOnly             else
   if mode = fa_Open      then mode := Open_RdWr;
   FileOpen := fdOpen(FileName, mode);
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  SetFileSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 26Feb97 LdB       }
{---------------------------------------------------------------------------}
FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOV DX, FileSize.Word[0];                        { Load file position }
     MOV CX, FileSize.Word[2];
     MOV BX, Handle;                                  { Load file handle }
     MOV AX, $4200;                                   { Load function id }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Position the file }
     POP BP;                                          { Reload register }
     JC @@Exit3;                                      { Exit if error }
     XOR CX, CX;                                      { Force truncation }
     MOV BX, Handle;                                  { File handle }
     MOV AX, $4000;                                   { Load function id }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Truncate file }
     POP BP;                                          { Reload register }
     JC @@Exit3;                                      { Exit if error }
     XOR AX, AX;                                      { Return successful }
   @@Exit3:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   VAR Regs: TRealRegs;
   BEGIN
     Regs.RealEDX := FileSize AND $FFFF;              { Lo word of filesize }
     Regs.RealECX := (FileSize SHR 16) AND $FFFF;     { Hi word of filesize }
     Regs.RealEBX := LongInt(Handle);                 { Load file handle }
     Regs.RealEAX := $4000;                           { Load function id }
     SysRealIntr($21, Regs);                          { Call DOS int 21 }
     If (Regs.RealFlags AND 1 <> 0) Then
       SetFileSize := Regs.RealEAX AND $FFFF          { Error encountered }
       Else SetFileSize := 0;                         { Return successful }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
VAR {$IFDEF BIT_16} Buf, {$ENDIF} Actual: LongInt;
BEGIN
   {$IFDEF BIT_16}                                    { 16 BIT WINDOWS CODE }
   Actual := _llseek(Handle, FileSize, 0);            { Position file }
   If (Actual = FileSize) Then Begin                  { No position error }
     Actual := _lwrite(Handle, Pointer(@Buf), 0);     { Truncate the file }
     If (Actual <> -1) Then SetFileSize := 0 Else     { No truncate error }
       SetFileSize := 103;                            { File truncate error }
   End Else SetFileSize := 103;                       { File truncate error }
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT WINDOWS CODE }
   Actual := SetFilePointer(Handle, FileSize, Nil, 0);{ Position file }
   If (Actual = FileSize) Then Begin                  { No position error }
     If SetEndOfFile(Handle) Then SetFileSize := 0    { No truncate error }
       Else SetFileSize := 103;                       { File truncate error }
   End Else SetFileSize := 103;                       { File truncate error }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
BEGIN
   {$IFDEF PPC_BPOS2}                                 { C'T patched COMPILER }
   SetFileSize := DosNewSize(Handle, FileSize);       { Truncate the file }
   {$ELSE}                                            { OTHER OS2 COMPILERS }
   SetFileSize := DosSetFileSize(Handle, FileSize);   { Truncate the file }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_UNIX}
VAR
   Actual : LongInt;
BEGIN
   Actual := fdSeek(Handle, FileSize, 0);             { Position file }
   If (Actual = FileSize) Then Begin                  { No position error }
     if (fdTruncate(Handle, FileSize))                { Truncate the file }
        Then SetFileSize := 0                         { No truncate error }
        else SetFileSize := 103;                      { File truncate error }
   End Else SetFileSize := 103;                       { File truncate error }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  SetFilePos -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 25Feb97 LdB        }
{---------------------------------------------------------------------------}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     MOV AX, MoveType;                                { Load move type }
     MOV AH, $42;                                     { Load function id }
     MOV DX, Pos.Word[0];                             { Load file position }
     MOV CX, Pos.Word[2];
     MOV BX, Handle;                                  { Load file handle }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Position the file }
     POP BP;                                          { Reload register }
     JC @@Exit6;
     LES DI, Actual;                                  { Actual var addr }
     MOV ES:[DI], AX;
     MOV ES:[DI+2], DX;                               { Update actual }
     XOR AX, AX;                                      { Set was successful }
   @@Exit6:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   VAR Regs: TRealRegs;
   BEGIN
     Actual := 0;                                     { Zero actual count }
     Regs.RealEAX := ($42 SHL 8) + Byte(MoveType);    { Set function id }
     Regs.RealEBX := LongInt(Handle);                 { Fetch file handle }
     Regs.RealEDX := Pos AND $FFFF;                   { Keep low word }
     Regs.RealECX := Pos SHR 16;                      { Keep high word }
     SysRealIntr($21, Regs);                          { Call dos interrupt }
     If (Regs.RealFlags AND $1 = 0) Then Begin
       Actual := Lo(Regs.RealEDX) SHL 16 +
         Lo(Regs.RealEAX);                            { Current position }
       SetFilePos := 0;                               { Function successful }
     End Else SetFilePos := Lo(Regs.RealEAX);         { I/O error returned }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WINDOWS CODE }
BEGIN
   {$IFDEF BIT_16}                                    { 16 BIT WINDOWS CODE }
   Actual := _llseek(Handle, Pos, MoveType);          { Position file }
   If (Actual <> -1) Then SetFilePos := 0 Else        { No position error }
     SetFilePos := 107;                               { File position error }
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT WINDOWS CODE }
   Actual := SetFilePointer(Handle, Pos, Nil, MoveType);{ Position file }
   If (Actual <> -1) Then SetFilePos := 0 Else        { No position error }
     SetFilePos := 107;                               { File position error }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
BEGIN
   {$IFDEF PPC_BPOS2}
   If (DosChgFilePtr(Handle, Pos, MoveType, Actual)=0){ Set file position }
     Then SetFilePos := 0 Else SetFilePos := 107;     { File position error }
   {$ELSE}                                            { OTHER OS2 COMPILERS }
   If (DosSetFilePtr(Handle, Pos, MoveType, Actual)=0){ Set file position }
     Then SetFilePos := 0 Else SetFilePos := 107;     { File position error }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_UNIX}
BEGIN
   Actual := fdSeek(Handle, Pos, MoveType);
   If (Actual <> -1) Then SetFilePos := 0 Else        { No position error }
      SetFilePos := 107;                               { File position error }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  FileRead -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22Oct98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     XOR AX, AX;                                      { Zero register }
     LES DI, Actual;                                  { Actual var address }
     MOV ES:[DI], AX;                                 { Zero actual var }
     PUSH DS;                                         { Save segment }
     LDS DX, Buf;                                     { Data destination }
     MOV CX, Count;                                   { Amount to read }
     MOV BX, Handle;                                  { Load file handle }
     MOV AX, $3F00;                                   { Load function id }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Read from file }
     POP BP;                                          { Reload register }
     POP DS;                                          { Restore segment }
     JC @@Exit4;                                      { Check for error }
     LES DI, Actual;                                  { Actual var address }
     MOV ES:[DI], AX;                                 { Update bytes moved }
     XOR AX, AX;                                      { Return success }
   @@Exit4:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   BEGIN
     Actual := System.Do_Read(LongInt(Handle),
       LongInt(@Buf), Count);                         { Read data from file }
     FileRead := InOutRes;                            { I/O status returned }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
BEGIN
   {$IFDEF BIT_16}                                    { 16 BIT WINDOWS CODE }
   Actual := _lread(Handle, Pointer(@Buf), Count);    { Read from file }
   If (Actual = Count) Then FileRead := 0 Else        { No read error }
     FileRead := 104;                                 { File read error }
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT WINDOWS CODE }
   If ReadFile(Handle, Buf, Count, DWord(Actual),
     Nil) AND (Actual = Count) Then FileRead := 0     { No read error }
     Else FileRead := 104;                            { File read error }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
BEGIN
   If (DosRead(Handle, Buf, Count, Actual) = 0) AND   { Read from file }
   (Actual = Count) Then FileRead := 0 Else           { No read error }
     FileRead := 104;                                 { File read error }
END;
{$ENDIF}
{$IFDEF OS_UNIX}
BEGIN
   Actual := fdRead(Handle, Buf, Count);
   if (Actual = Count) Then FileRead := 0             { No read error }
     Else FileRead := 104;                            { File read error }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  FileWrite -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22Oct98 LdB         }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
{$IFDEF OS_DOS}                                       { DOS/DPMI CODE }
   {$IFDEF ASM_BP}                                    { BP COMPATABLE ASM }
   ASSEMBLER;
   ASM
     XOR AX, AX;                                      { Zero register }
     LES DI, Actual;                                  { Actual var address }
     MOV ES:[DI], AX;                                 { Zero actual var }
     PUSH DS;                                         { Save segment }
     LDS DX, Buf;                                     { Data source buffer }
     MOV CX, Count;                                   { Amount to write }
     MOV BX, Handle;                                  { Load file handle }
     MOV AX, $4000;                                   { Load function id }
     PUSH BP;                                         { Store register }
     INT $21;                                         { Write to file }
     POP BP;                                          { Reload register }
     POP DS;                                          { Restore segment }
     JC @@Exit5;                                      { Check for error }
     LES DI, Actual;                                  { Actual var address }
     MOV ES:[DI], AX;                                 { Update bytes moved }
     XOR AX, AX;                                      { Write successful }
   @@Exit5:
   END;
   {$ENDIF}
   {$IFDEF ASM_FPC}                                   { FPC COMPATABLE ASM }
   BEGIN
     Actual := System.Do_Write(LongInt(Handle),
       LongInt(@Buf), Count);                         { Write data to file }
     FileWrite := InOutRes;                           { I/O status returned }
   END;
   {$ENDIF}
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
BEGIN
   {$IFDEF BIT_16}                                    { 16 BIT WINDOWS CODE }
   Actual := _lwrite(Handle, Pointer(@Buf), Count);   { Write to file }
   If (Actual = Count) Then FileWrite := 0 Else       { No write error }
     FileWrite := 105;                                { File write error }
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT WINDOWS CODE }
   If WriteFile(Handle, Buf, Count, DWord(Actual),
     Nil) AND (Actual = Count) Then FileWrite := 0    { No write error }
     Else FileWrite := 105;                           { File write error }
   {$ENDIF}
END;
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 CODE }
BEGIN
   If (DosWrite(Handle, Buf, Count, Actual) = 0) AND  { Write to file }
   (Actual = Count) Then FileWrite := 0 Else          { No write error }
     FileWrite := 105;                                { File write error }
END;
{$ENDIF}
{$IFDEF OS_UNIX}
BEGIN
   Actual := fdWrite(Handle, Buf, Count);
   If (Actual = Count) Then FileWrite := 0 Else       { No write error }
     FileWrite := 105;                                { File write error }
END;
{$ENDIF}

END.
{
 $Log$
 Revision 1.10  2002-10-13 20:52:09  hajny
   * mistyping corrected

 Revision 1.9  2002/10/12 19:39:00  hajny
   * FPC/2 support

 Revision 1.8  2002/09/22 19:42:22  hajny
   + FPC/2 support added

 Revision 1.7  2002/09/07 15:06:36  peter
   * old logs removed and tabs fixed

 Revision 1.6  2002/06/04 11:12:41  marco
  * Renamefest

}
