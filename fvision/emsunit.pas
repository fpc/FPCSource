{$Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{              DOS System EMS control unit                 }
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

UNIT EMSUnit;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF FPC} { FPC doesn't support these switches }
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
THis UNIT can only compile under DOS REAL MODE!!!!
{$ENDIF}

{***************************************************************************}
{                             PUBLIC CONSTANTS                              }
{***************************************************************************}
{---------------------------------------------------------------------------}
{                     STANDARD EMS ERROR STATE CONSTANTS                    }
{---------------------------------------------------------------------------}
CONST
   EMSInternalError = $80;                            { Internal error }
   EMSHardwareFail  = $81;                            { Hardware failure }
   EMSInvalidFunc   = $84;                            { Invalid EMS funtion }
   EMSNoEMSHandles  = $85;                            { No EMS handles left }
   EMSBeyondMax     = $87;                            { Req > EMS max size }
   EMSToManyPages   = $88;                            { Fewer pages free }
   EMSZeroPageReq   = $89;                            { Req zero page alloc }
   EMSNotPresent    = $FF;                            { No EMS driver found }

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          EMS INTERFACE ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-IsEMSPresent-------------------------------------------------------
Returns true/false as to the availability of EMS support functions.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION IsEMSPresent: Boolean;

{-EMS_Version--------------------------------------------------------
If EMS functions are available returns the version of EMS support that
is supported. If no EMS support or error is encountered returns zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_Version: Word;

{-EMS_MaxAvail-------------------------------------------------------
If EMS functions are available returns the maximum EMS memory available
if none was in use. No EMS support or error will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_MaxAvail: LongInt;

{-EMS_MemAvail-------------------------------------------------------
If EMS functions are available returns the EMS memory that is currently
available. No EMS support or error will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_MemAvail: LongInt;

{-EMS_GetMem---------------------------------------------------------
If EMS functions are available and enough EMS memory is available the
requested pages (16Kb = 1 Page) of ems is allocated and the EMS handle
returned. No EMS support or error will return a zero handle.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_GetMem (Pages: Word): Word;

{-EMS_FreeMem--------------------------------------------------------
If EMS functions are available and a valid EMS handle is given the EMS
memory belonging to the handle is release and true returned. No EMS
support, an invalid handle or an error will return a false result.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_FreeMem (Handle: Word): Boolean;

{-EMS_ResizeMem------------------------------------------------------
If EMS functions are available and enough EMS memory is available and
a valid EMS handle is given the new EMS size will be allocated and
all the data in the old memory will be moved to the new memory. No EMS
support, insufficient EMS memory or error will return an EMS error state
while successful operations will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_ResizeMem (NewSize, Handle: Word): Byte;

{-EMS_MoveMem--------------------------------------------------------
If EMS functions are available size amount of data held in FromAddress
is transfered to the ToAddress. The handles can be EMS handles if the
associated address is an EMS offset or zero if the address refers to
a real mode address. No EMS support or error will return an EMS error
state while successful operations will return zero.
31Aug98 LdB
---------------------------------------------------------------------}
FUNCTION EMS_MoveMem (ToAddr: LongInt; ToHandle: Word; FromAddr: LongInt;
FromHandle: Word; Size: LongInt): Byte;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{***************************************************************************}
{                        PRIVATE INITIALIZED VARIABLES                      }
{***************************************************************************}
{---------------------------------------------------------------------------}
{                    INITIALIZED EMS PRIVATE VARIABLES                      }
{---------------------------------------------------------------------------}
CONST
   EMSPresent  : Boolean = False;                     { EMS present state }
   EMSInit     : Boolean = False;                     { EMS ready flag }
   EMSFrame    : Word = $FFFF;                        { EMS page frame }

{***************************************************************************}
{                          PRIVATE INTERNAL ROUTINES                        }
{***************************************************************************}

{---------------------------------------------------------------------------}
{  InitializeEMS -> Platforms DOS - Checked 28Jan97 LdB                     }
{---------------------------------------------------------------------------}
PROCEDURE InitializeEMS; ASSEMBLER;
CONST EMSCheck: String[8] = 'EMMXXXX0';               { EMS check string }
ASM
   MOV AX, 3567H;
   INT 21H;                                           { Get EMS vector }
   CLD;
   MOV DI, 000AH;
   LEA SI, EMSCheck;                                  { Check string }
   INC SI;
   MOV CX, 0008;
   REPZ CMPSB;                                        { Loop on equal }
   OR CX, CX;
   JNZ @@NoEMS;                                       { Do strings equal }
   MOV AH, 41H;
   INT 67H;                                           { Get frame page }
   OR AH, AH;
   JNZ @@NoEMS;                                       { Check for error }
   MOV WORD PTR [EMSFrame], BX;
   MOV BYTE PTR [EMSPresent], True;                   { EMS present true }
@@NoEMS:
   MOV BYTE PTR [EMSInit], True;                      { EMS initialized }
END;

{---------------------------------------------------------------------------}
{  EMS_MapPage -> Platforms DOS - Checked 28Feb97 LdB                       }
{---------------------------------------------------------------------------}
FUNCTION EMS_MapPage (Handle, LogPage: Word; PhyPage: Byte): Byte; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   MOV AX, EMSNotPresent;                             { Preset return }
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@Exit;                                        { Exit if no EMS }
   MOV AH, 44H;                                       { Set EMS function id }
   MOV AL, [PhyPage];                                 { Physical EMS page }
   MOV BX, [LogPage];                                 { Logical EMS page }
   MOV DX, [Handle];                                  { Load handle }
   INT 67H;                                           { Remap memory call }
   XCHG AH, AL;                                       { Exchange registers }
@@Exit:
END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          EMS INTERFACE ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{---------------------------------------------------------------------------}
{  IsEMSPresent -> Platforms DOS - Checked 28Jan97 LdB                      }
{---------------------------------------------------------------------------}
FUNCTION IsEMSPresent: Boolean; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   MOV AL, [EMSPresent];                              { Return result }
END;

{---------------------------------------------------------------------------}
{  EMS_Version -> Platforms DOS - Checked 28Jan97 LdB                       }
{---------------------------------------------------------------------------}
FUNCTION EMS_Version: Word; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   XOR AX, AX;                                        { Preset zero return }
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@Exit;                                        { Exit if no EMS }
   MOV AH, 46H;                                       { Set EMS function id }
   INT 67H;                                           { Get EMS version }
   OR AH, AH;                                         { Check for error }
   JZ @@EMSVerOk;                                     { Jump if no error }
   XOR AX, AX;                                        { Return zero - error }
@@EMSVerOk:
   MOV CL, 4;                                         { Load shift count }
   SHL AX, CL;                                        { Shift to position }
@@Exit:
END;

{---------------------------------------------------------------------------}
{  EMS_MaxAvail -> Platforms DOS - Checked 28Jan97 LdB                      }
{---------------------------------------------------------------------------}
FUNCTION EMS_MaxAvail: LongInt; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   XOR AX, AX;                                        { Preset zero return }
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@Exit;                                        { Exit if no EMS }
   MOV AH, 42H;                                       { Set EMS function id }
   INT 67H;                                           { Get EMS usage }
   MOV BX, DX;                                        { Transfer register }
   XOR DX, DX;                                        { Clear register }
   OR AH, AH;                                         { Check for error }
   JZ @@Exit;                                         { Jump if no error }
@@EMSError:
   XOR BX, BX;                                        { Return zero }
   MOV DX, BX;
@@Exit:                                               { DX:BX = Total pages }
   MOV AX, BX;                                        { Transfer register }
   MOV CX, 000EH;                                     { 1 SHL 14 = 16K }
   DB $0F; DB $A5; DB $C2;                            { SHL DX:AX, CL}
   SHL AX, CL;                                        { Roll lower word }
END;

{---------------------------------------------------------------------------}
{  EMS_MemAvail -> Platforms DOS - Checked 28Jan97 LdB                      }
{---------------------------------------------------------------------------}
FUNCTION EMS_MemAvail: LongInt; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   XOR AX, AX;                                        { Preset zero return }
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@Exit;                                        { Exit if no EMS }
   MOV AH, 42H;                                       { Set EMS function id }
   INT 67H;                                           { Get EMS usage }
   XOR DX, DX;                                        { Clear register }
   OR AH, AH;                                         { Check for error }
   JZ @@Exit;                                         { Jump if no error }
@@EMSError:
   XOR BX, BX;                                        { Return zero }
   MOV DX, BX;
@@Exit:                                               { DX:BX = Avail pages }
   MOV AX, BX;                                        { Transfer register }
   MOV CX, 000EH;                                     { 1 SHL 14 = 16K }
   DB $0F; DB $A5; DB $C2;                            { SHL DX:AX, CL}
   SHL AX, CL;                                        { Roll lower word }
END;

{---------------------------------------------------------------------------}
{  EMS_GetMem -> Platforms DOS - Checked 28Feb97 LdB                        }
{---------------------------------------------------------------------------}
FUNCTION EMS_GetMem (Pages: Word): Word; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   XOR DX, DX;                                        { Preset no handle }
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@Exit;                                        { Exit if no EMS }
   MOV AH, 43H;                                       { Set EMS function id }
   MOV BX, [Pages];                                   { Pages to allocate }
   INT 67H;                                           { Allocate EMS memory }
   OR AH, AH;                                         { Check register AX }
   JZ @@Exit;                                         { Zero means no error }
   XOR DX, DX;                                        { Clr handle on error }
@@Exit:
   MOV AX, DX;                                        { Return result }
END;

{---------------------------------------------------------------------------}
{  EMS_FreeMem -> Platforms DOS - Checked 28Feb97 LdB                       }
{---------------------------------------------------------------------------}
FUNCTION EMS_FreeMem (Handle: Word): Boolean; ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@EMSError;                                    { Error if no EMS }
   MOV AH, 45H;                                       { Set EMS function id }
   MOV DX, [Handle];                                  { Load handle }
   INT 67H;                                           { Release handle call }
   OR AH, AH;                                         { Check for error }
   JNZ @@EMSError;                                    { Jump if error }
   MOV AX, True;                                      { Function success }
   JMP @@Exit;                                        { Now exit }
@@EMSError:
   MOV AX, False;                                     { Function failed }
@@Exit:
END;

{---------------------------------------------------------------------------}
{  EMS_ResizeMem -> Platforms DOS - Checked 28Feb97 LdB                     }
{---------------------------------------------------------------------------}
FUNCTION EMS_ResizeMem (NewSize, Handle: Word): Byte;
ASSEMBLER;
ASM
   CMP BYTE PTR [EMSInit], True;                      { Chk EMS initialized }
   JZ @@EMSInitialized;                               { Jump if initialized }
   CALL InitializeEMS;                                { Initialize EMS }
@@EMSInitialized:
   MOV AX, EMSNotPresent;                             { Preset return }
   CMP EMSPresent, True;                              { Check EMS present }
   JNZ @@Exit;                                        { Exit if no EMS }
   MOV AH, 51H;                                       { Set EMS function id }
   MOV BX, [NewSize];                                 { Load new size  }
   MOV DX, [Handle];                                  { Load handle }
   INT 67H;                                           { Reallocate memory }
   XCHG AH, AL;                                       { Exchange registers }
@@Exit:
END;

FUNCTION EMS_MoveMem (ToAddr: LongInt; ToHandle: Word; FromAddr: LongInt;
FromHandle: Word; Size: LongInt): Byte;
VAR Er: Byte; W, EMSPage, EMSPos, EMSPage1, EMSPos1: Word;
BEGIN
   Er := 0;                                           { Preset no error }
   If (Size > 0) Then Begin                           { Valid move size }
     Repeat
       If (Size > $FFFF) Then W := $FFFF
         Else W := Size;                              { Size to move }
       If (ToHandle = 0) AND (FromHandle = 0)
       Then Begin                                     { Standard memory }
         Move(Pointer(ToAddr)^, Pointer(FromAddr)^,
           W);                                        { Move the data }
       End Else If (ToHandle <> 0) AND (FromHandle <> 0)
       Then Begin                                     { EMS to EMS move }
         If (Size > $7FFF) Then W := $7FFF
           Else W := Size;                            { Size to move }
         EMSPage := (FromAddr AND $FFFFC000) SHR 14;  { Current from page }
         EMSPos := FromAddr AND $00003FFF;            { Current from position }
         Er := EMS_MapPage(FromHandle, EMSPage, 0);   { Map to page 0 }
         If (Er = 0) AND (W > $3FFF) Then
           Er := EMS_MapPage(FromHandle, EMSPage+1, 1);{ Map to page 1 }
         EMSPage1 := (ToAddr AND $FFFFC000) SHR 14;   { Current to page }
         EMSPos1 := ToAddr AND $00003FFF;             { Current to position }
         Er := EMS_MapPage(ToHandle, EMSPage+2, 2);   { Map to page 2 }
         If (Er = 0) AND (W > $3FFF) Then
           Er := EMS_MapPage(ToHandle, EMSPage+3, 3); { Map to page 3 }
         If (Er = 0) Then Move(Ptr(EMSFrame, EMSPos1
           + $8000)^, Ptr(EMSFrame, EMSPos)^, W);     { Move data EMS -> EMS }
       End Else If (ToHandle = 0) Then Begin          { Get data from EMS }
         EMSPage := (FromAddr AND $FFFFC000) SHR 14;  { Current from page }
         EMSPos := FromAddr AND $00003FFF;            { Current from position }
         Er := EMS_MapPage(FromHandle, EMSPage, 0);   { Map to page 0 }
         If (Er = 0) AND (W > $3FFF) Then
           Er := EMS_MapPage(FromHandle, EMSPage+1, 1);{ Map to page 1 }
         If (Er = 0) AND (W > $7FFF) Then
           Er := EMS_MapPage(FromHandle, EMSPage+2, 2);{ Map to page 2 }
         If (Er = 0) AND (W > $BFFF) Then
           Er := EMS_MapPage(FromHandle, EMSPage+3, 3);{ Map to page 3 }
         If (Er = 0) Then Move(Pointer(ToAddr)^,
           Ptr(EMSFrame, EMSPos)^, W);                { Move data from EMS }
       End Else If (FromHandle = 0) Then Begin        { Put data in EMS }
         EMSPage := (ToAddr AND $FFFFC000) SHR 14;    { Current to page }
         EMSPos := ToAddr AND $00003FFF;              { Current to position }
         Er := EMS_MapPage(ToHandle, EMSPage, 0);     { Map to page 0 }
         If (Er = 0) AND (W > $3FFF) Then
           Er := EMS_MapPage(ToHandle, EMSPage+1, 1); { Map to page 1 }
         If (Er = 0) AND (W > $7FFF) Then
           Er := EMS_MapPage(ToHandle, EMSPage+2, 2); { Map to page 2 }
         If (Er = 0) AND (W > $BFFF) Then
           Er := EMS_MapPage(ToHandle, EMSPage+3, 3); { Map to page 3 }
         If (Er = 0) Then Move(Ptr(EMSFrame, EMSPos)^,
          Pointer(FromAddr)^, W);                     { Move data to EMS }
       End;
       If (Er = 0) Then Begin
         Size := Size - W;                            { Subtract moved size }
         ToAddr := ToAddr + W;                        { Inc to address }
         FromAddr := FromAddr + W;                    { Inc from address }
       End;
     Until (Size = 0) OR (Er <> 0);                   { Until all moved/error }
   End Else Er := EMSInvalidFunc;                     { Invalid size }
   EMS_MoveMem := Er;                                 { Return any error }
END;

END.
{
 $Log$
 Revision 1.2  2000-08-24 12:00:21  marco
  * CVS log and ID tags


}