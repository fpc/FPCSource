{ $Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{              DOS System XMS control unit                 }
{                                                          }
{       Extracted from my original OBJECTS.PAS unit.       }
{                                                          }
{        Copyright (c) 1998 by Leon de Boer                }
{       ldeboer@attglobal.net  - primary e-mail address    }
{       ldeboer@starwon.com.au - backup e-mail address     }
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
{                                                          }
{        DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     31 Aug 98   First release moved from original  }
{                       objects unit.                      }
{**********************************************************}

UNIT XMSUnit;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF FPC}{ FPC doesn't support these switches }
   {$F+} { Force far calls }
   {$A+} { Word Align Data }
   {$B-} { Allow short circuit boolean evaluations }
   {$O+} { This unit may be overlaid }
   {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
   {$E+} { Emulation is on }
   {$N-} { No 80x87 code generation }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

{$IFNDEF PROC_Real}
This UNIT can only compile under DOS REAL MODE!!!!
{$ENDIF}

{***************************************************************************}
{                             PUBLIC CONSTANTS                              }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                     STANDARD XMS ERROR STATE CONSTANTS                    }
{---------------------------------------------------------------------------}
CONST
   XMSInvalidFunc   = $80;                            { Invalid function }
   XMSVDiskDetect   = $81;                            { VDisk detected }
   XMSA20GateError  = $82;                            { A20 gating error }
   XMSGeneralError  = $8E;                            { General error }
   XMSNoHMA         = $90;                            { HMA does not exist }
   XMSHMAInUse      = $91;                            { HMA already in use }
   XMSHMAMinError   = $92;                            { HMA < /HMAMIN param }
   XMSHMANotAlloc   = $93;                            { HMA not allocated }
   XMSA20Enabled    = $94;                            { A20 still enabled }
   XMSNoXMSLeft     = $A0;                            { All XMS allocated }
   XMSNoXMSHandle   = $A1;                            { All handles used }
   XMSHandleInvalid = $A2;                            { Invalid XMS handle }
   XMSInvSrcHandle  = $A3;                            { Invalid Source Handle }
   XMSInvSrcOffset  = $A4;                            { Invalid Source Offset }
   XMSInvDestHandle = $A5;                            { Invalid Dest Handle }
   XMSInvDestOffset = $A6;                            { Invalid Dest Offset }
   XMSInvXferLength = $A7;                            { Invalid Length }
   XMSOverlapError  = $A8;                            { Move has overlap }
   XMSParityError   = $A9;                            { XMS parity error }
   XMSBlkNotLocked  = $AA;                            { Block not locked }
   XMSBlockLocked   = $AB;                            { Block is locked }
   XMSLockCountOver = $AC;                            { Lock count overflow }
   XMSLockFailed    = $AD;                            { Lock failed }
   XMSSmallerUMB    = $B0;                            { Smaller UMB avail }
   XMSNoUMBAvail    = $B1;                            { No UMB's available }
   XMSUMBSegInvalid = $B2;                            { Invalid UMB segment }
   XMSNotPresent    = $FF;                            { No XMS driver found }

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          XMS INTERFACE ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-IsXMSPresent-------------------------------------------------------
Returns true/false as to the availability of XMS support functions.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION IsXMSPresent: Boolean;

{-XMS_Version--------------------------------------------------------
If XMS functions are available returns the version of XMS support that
is supported. If no XMS support or error is encountered returns zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_Version: Word;

{-XMS_MaxAvail-------------------------------------------------------
If XMS functions are available returns the maximum XMS memory available
if none was in use. No XMS support or error will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_MaxAvail: LongInt;

{-XMS_MemAvail-------------------------------------------------------
If XMS functions are available returns the XMS memory that is currently
available. No XMS support or error will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_MemAvail: LongInt;

{-XMS_GetMem---------------------------------------------------------
If XMS functions are available and enough XMS memory is available the
requested KB of xms is allocated and the XMS handle returned. No XMS
support or error will return a zero handle.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_GetMem (KbSize: Word): Word;

{-XMS_FreeMem--------------------------------------------------------
If XMS functions are available and a valid XMS handle is given the XMS
memory belonging to the handle is release and true returned. No XMS
support, an invalid handle or an error will return a false result.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_FreeMem (Handle : Word): Boolean;

{-XMS_ResizeMem------------------------------------------------------
If XMS functions are available and enough XMS memory is available and
a valid XMS handle is given the new XMS size will be allocated and
all the data in the old memory will be moved to the new memory. No XMS
support, insufficient XMS memory or error will return an XMS error state
while successful operations will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_ResizeMem (OldSize, NewSize: Word; Var Handle: Word): Byte;

{-XMS_MoveMem--------------------------------------------------------
If XMS functions are available size amount of data held in FromAddress
is transfered to the ToAddress. The handles can be XMS handles if the
associated address is an XMS offset or zero if the address refers to
a real mode address. No XMS support or error will return an XMS error
state while successful operations will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION XMS_MoveMem (ToAddress: LongInt; ToHandle: Word; FromAddress: LongInt;
FromHandle: Word; Size: LongInt): Byte;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{***************************************************************************}
{                        PRIVATE INITIALIZED VARIABLES                      }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                    INITIALIZED XMS PRIVATE VARIABLES                      }
{---------------------------------------------------------------------------}
CONST
   XMSPresent  : Boolean = False;                     { XMS present state }
   XMSInit     : Boolean = False;                     { XMS ready flag }
   XMSReAlloc  : Boolean = False;                     { XMS reallocatable }
   XMSEntryAddr: Pointer = Nil;                       { XMS entry address }

{***************************************************************************}
{                          PRIVATE INTERNAL ROUTINES                        }
{***************************************************************************}

{---------------------------------------------------------------------------}
{  InitializeXMS -> Platforms DOS - Checked 27Jan97 LdB                     }
{---------------------------------------------------------------------------}
PROCEDURE InitializeXMS; ASSEMBLER;
ASM
   CMP [XMSInit], True;                               { XMS initialized }
   JZ @@Exit;
   XOR BX, BX;
   MOV ES, BX;                                        { Zero out registers }
   MOV AX, 4310H;
   INT 2FH;                                           { Driver entry point }
   MOV AX, ES;
   OR AX, BX;                                         { Entry point check }
   JZ @@Exit;
   MOV [XMSPresent], True;                            { XMS is present }
   MOV XMSEntryAddr.Word[0], BX;
   MOV XMSEntryAddr.Word[2], ES;                      { Hold entry address }
   MOV AH, 09H;
   MOV DX, 0001H;                                     { Allocate 1k block }
   CALL POINTER [XMSEntryAddr];
   PUSH DX;                                           { Hold handle 1 }
   MOV AH, 09H;
   MOV DX, 0001H;                                     { Allocate 1K block }
   CALL POINTER [XMSEntryAddr];
   MOV BX, DX;                                        { Hold handle 2 }
   POP DX;
   PUSH BX;                                           { Save handle 2 }
   PUSH DX;                                           { Save handle 1 }
   MOV AH, 0FH;
   MOV BX, 0020H;                                     { Realloc 32K block }
   CALL POINTER [XMSEntryADDR];
   OR AX, AX;                                         { Chk success 0=fail }
   JZ @@ReAllocateFail;
   MOV BYTE PTR [XMSReAlloc], True;                   { XMS reallocate - IBM }
@@ReAllocateFail:
   POP DX;                                            { Recover handle 1 }
   MOV AH, 0AH;
   CALL POINTER [XMSEntryAddr];                       { Release all blocks }
   POP DX;                                            { Recover handle 2 }
   MOV AH, 0AH;
   CALL POINTER [XMSEntryAddr];                       { Release all blocks }
@@Exit:
   MOV [XMSInit], True;                               { XMS initialized }
END;

{---------------------------------------------------------------------------}
{  XMS_EvenMoveMem -> Platforms DOS - Checked 27Jan97 LdB                   }
{---------------------------------------------------------------------------}
FUNCTION XMS_EvenMoveMem (ToAddress: LongInt; ToHandle: Word; FromAddress: LongInt;
FromHandle: Word; Size: LongInt): Byte; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   MOV BL, XMSNotPresent;                             { Preset error }
   CMP XMSPresent, True;                              { XMS present }
   JNZ @@XMSError;                                    { No XMS so exit }
   MOV AH, 0BH;                                       { Move function }
   LEA SI, Size;                                      { Address of size }
   PUSH DS;
   POP ES;                                            { Load data segment }
   PUSH SS;
   POP DS;
   CALL ES:[XMSEntryAddr];                            { Call XMS handler }
   PUSH ES;
   POP DS;
   CMP AX, 1;                                         { Check for AX=1 }
   JNZ @@XMSError;                                    { Jump on error }
   MOV BL, 0H;                                        { Clear error status }
@@XMSError:
   MOV AL, BL;                                        { Function failed }
@@Exit:
END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          XMS INTERFACE ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  IsXMSPresent -> Platforms DOS - Checked 27Jan97 LdB                      }
{---------------------------------------------------------------------------}
FUNCTION IsXMSPresent: Boolean; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   MOV AL, [XMSPresent];                              { Return result }
END;

{---------------------------------------------------------------------------}
{  XMS_Version -> Platforms DOS - Checked 27Jan97 LdB                       }
{---------------------------------------------------------------------------}
FUNCTION XMS_Version: Word; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   CMP XMSPresent, True;                              { Check XMS present }
   JNZ @@XMSError;                                    { Jump if no XMS }
   MOV AH, 0H;                                        { XMS version call id }
   CALL POINTER [XMSEntryAddr];                       { XMS handler call }
   JMP @@Exit;                                        { Now exit }
@@XMSError:
   XOR AX, AX;                                        { Return zero }
@@Exit:
END;

{---------------------------------------------------------------------------}
{  XMS_MaxAvail -> Platforms DOS - Checked 27Jan97 LdB                      }
{---------------------------------------------------------------------------}
FUNCTION XMS_MaxAvail: LongInt; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   CMP XMSPresent, True;                              { Check XMS present }
   JNZ @@XMSError;                                    { Jump if no XMS }
   MOV AH, 08H;                                       { Query memory call id }
   CALL POINTER [XMSEntryAddr];                       { XMS handler call }
   OR BL, BL;                                         { Check for error }
   JZ @@Exit;                                         { Exit on no error }
@@XMSError:
   XOR AX, AX;                                        { Return zero }
@@Exit:                                               { AX = Kilobytes }
   XOR DX, DX;                                        { Clear register }
   MOV CX, 000AH;                                     { 1 SHL 10 = 1K }
   DB $0F; DB $A5; DB $C2;                            { SHL DX:AX, CL}
   SHL AX, CL;                                        { Roll lower word }
END;

{---------------------------------------------------------------------------}
{  XMS_MemAvail -> Platforms DOS - Checked 27Jan97 LdB                      }
{---------------------------------------------------------------------------}
FUNCTION XMS_MemAvail: LongInt; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   CMP XMSPresent, True;                              { Check XMS present }
   JNZ @@XMSError;                                    { Jump if no XMS }
   MOV AH, 08H;                                       { Query memory call id }
   CALL POINTER [XMSEntryAddr];                       { XMS handler call }
   MOV AX, DX;                                        { Transfer register }
   OR BL, BL;                                         { Check for error }
   JZ @@Exit;                                         { Exit on no error }
@@XMSError:
   XOR AX, AX;                                        { Return zero }
@@Exit:                                               { AX = Kilobytes }
   XOR DX, DX;                                        { Clear register }
   MOV CX, 000AH;                                     { 1 SHL 10 = 1K }
   DB $0F; DB $A5; DB $C2;                            { SHL DX:AX, CL}
   SHL AX, CL;                                        { Roll lower word }
END;

{---------------------------------------------------------------------------}
{  XMS_GetMem -> Platforms DOS - Checked 27Jan97 LdB                        }
{---------------------------------------------------------------------------}
FUNCTION XMS_GetMem (KbSize: Word): Word; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   CMP XMSPresent, True;                              { Check XMS present }
   JNZ @@XMSError;                                    { Jump if no XMS }
   MOV AH, 09H;                                       { Allocate blocks id }
   MOV DX, [KbSize];                                  { Size to allocate }
   CALL [XMSEntryAddr];                               { Call XMS handler }
   CMP AX, 1;                                         { Check for AX=1 }
   JZ @@Exit;                                         { Jump on no error }
@@XMSError:
   XOR DX, DX;                                        { Clear handle }
@@Exit:
   MOV AX, DX;                                        { Transfer register }
END;

{---------------------------------------------------------------------------}
{  XMS_FreeMem -> Platforms DOS - Checked 27Jan97 LdB                       }
{---------------------------------------------------------------------------}
FUNCTION XMS_FreeMem (Handle : Word): Boolean; ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   CMP XMSPresent, True;                              { Check XMS present }
   JNZ @@XMSError;                                    { Jump if no XMS }
   MOV AH, 0AH;                                       { Deallocate blocks id }
   MOV DX, [Handle];                                  { Handle for blocks }
   CALL [XMSEntryAddr];                               { Call XMS handler }
   CMP AX, 1;                                         { Check for AX=1 }
   JNZ @@XMSError;                                    { Jump on error }
   MOV AX, True;                                      { Function success }
   JMP @@Exit;                                        { Now exit }
@@XMSError:
   MOV AX, False;                                     { Function failed }
@@Exit:
END;

{---------------------------------------------------------------------------}
{  XMS_ResizeMem -> Platforms DOS - Checked 28Feb97 LdB                     }
{---------------------------------------------------------------------------}
FUNCTION XMS_ResizeMem (OldSize, NewSize: Word; Var Handle: Word): Byte;
ASSEMBLER;
ASM
   CMP BYTE PTR [XMSInit], True;                      { Is XMS initialized }
   JZ @@XMSInitialized;                               { Jump if initialized }
   CALL InitializeXMS;                                { Initialize XMS }
@@XMSInitialized:
   MOV AX, XMSNotPresent;                             { Preset error state }
   CMP XMSPresent, True;                              { Check XMS present }
   JNZ @@Exit;                                        { Jump if no XMS }
   CMP BYTE PTR [XMSReAlloc], True;                   { Check Realloc flag }
   JZ @@DirectResize;                                 { Jump if flag is set }

   { * REMARK * - This is a bug fix for early versions of XMS drivers }
   {              in which the reallocate only worked for the last block }

   MOV AH, 09H;                                       { Allocate new handle }
   MOV DX, [NewSize];                                 { New XMS size }
   CALL [XMSEntryAddr];                               { Call XMS handler }
   CMP AX, 1;                                         { Check for fail }
   JNZ @@ErrorExit;                                   { Failed so exit }
   PUSH DX;                                           { Save new handle }
   XOR AX, AX;                                        { Clear register }
   PUSH AX;
   PUSH AX;                                           { To address is nil }
   PUSH DX;                                           { To handle }
   PUSH AX;
   PUSH AX;                                           { From address is nil }
   LES SI, [Handle];                                  { Load handle address }
   MOV DX, ES:[SI];                                   { Load handle }
   PUSH DX;                                           { From handle }
   MOV AX, [OldSize];                                 { Start with oldsize }
   CMP AX, [NewSize];                                 { Compare to new size }
   JLE @@NewBigger;
   MOV AX, [NewSize];                                 { Take smaller size }
@@NewBigger:
   XOR DX, DX;                                        { Clear register }
   MOV CX, 000AH;                                     { 1 SHL 10 = 1K }
   DB $0F; DB $A5; DB $C2;                            { SHL DX:AX, CL}
   SHL AX, CL;                                        { Roll lower word }
   PUSH DX;                                           { Push new size }
   PUSH AX;
   CALL FAR PTR XMS_MoveMem;                          { Move old to new }
   POP DX;                                            { Reload old handle }
   MOV BL, AL;                                        { Transfer result }
   CMP AX, 0;                                         { Check for success }
   JNZ @@ErrorExit;                                   { No error so exit }
   LES SI, [Handle];
   MOV BX, ES:[SI];                                   { Hold old handle }
   MOV ES:[SI], DX;                                   { Set new handle }
   MOV DX, BX;
   MOV AH, 0AH;                                       { Release old handle }
   CALL [XMSEntryAddr];                               { Call XMS handler }
   CMP AX, 1;                                         { Check for success }
   JNZ @@ErrorExit;                                   { No error so exit }
   XOR AX, AX;                                        { Clear the register }
   JMP @@Exit;                                        { Now exit }
   { * REMARK END * - Leon de Boer }

@@DirectResize:
   MOV AH, 0FH;                                       { Load function id }
   MOV BX, [NewSize];                                 { Load up new size }
   LES SI, [Handle];                                  { Address of handle }
   MOV DX, ES:[SI];                                   { Load handle }
   CALL [XMSEntryAddr];                               { Call XMS handler }
   CMP AX, 1;                                         { Check for success }
   JNZ @@ErrorExit;                                   { If error jump exit }
   MOV BL, 0;                                         { Clear the register }
@@ErrorExit:
   MOV AL, BL;                                        { Create return value }
@@Exit:
END;

{---------------------------------------------------------------------------}
{  XMS_MoveMem -> Platforms DOS - Checked 28Feb97 LdB                       }
{---------------------------------------------------------------------------}
FUNCTION XMS_MoveMem (ToAddress: LongInt; ToHandle: Word; FromAddress: LongInt;
FromHandle: Word; Size: LongInt): Byte;
VAR Success: Byte; Temp1, Temp2: Array [1..2] Of Byte;
BEGIN
   If Odd(Size) Then Begin                            { Size is odd }
     Success := XMS_EvenMoveMem(LongInt(@Temp1), 0,
       ToAddress, ToHandle, 2);                       { Dest fetch word }
     If (Success = 0) Then Begin
       Success := XMS_EvenMoveMem(LongInt(@Temp2), 0,
         FromAddress, FromHandle, 2);                 { Source fetch word }
       If (Success = 0) Then Begin
         Temp1[1] := Temp2[1];                        { Source to dest }
         Success := XMS_EvenMoveMem(ToAddress,
           ToHandle, LongInt(@Temp1), 0, 2);          { Update dest word }
         Inc(ToAddress);                              { Inc to address }
         Inc(FromAddress);                            { Inc from address }
         Dec(Size);                                   { One less byte }
       End;
     End;
   End Else Success := 0;                             { Even size to move }
   If (Success = 0) AND (Size > 0) Then               { Okay to move data }
     Success := XMS_EvenMoveMem(ToAddress, ToHandle,
       FromAddress, FromHandle, Size);                { Move even size }
   XMS_MoveMem := Success;                            { Return result }
END;

END.
{
 $Log$
 Revision 1.2  2000-08-24 12:00:23  marco
  * CVS log and ID tags


}