{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{          System independent clone of MEMORY.PAS          }
{                                                          }
{   Interface Copyright (c) 1992 Borland International     }
{                                                          }
{   Copyright (c) 1996, 1997, 1998, 1999 by Leon de Boer   }
{   ldeboer@attglobal.net  - primary e-mail address        }
{   ldeboer@starwon.com.au - backup e-mail address         }
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
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     19 feb 96   Initial DOS/DPMI code released.    }
{  1.10     18 Jul 97   Windows conversion added.          }
{  1.20     29 Aug 97   Platform.inc sort added.           }
{  1.30     05 May 98   Virtual pascal 2.0 code added.     }
{  1.40     01 Oct 99   Complete multiplatform rewrite     }
{  1.41     03 Nov 99   FPC Windows support added          }
{**********************************************************}

UNIT Memory;

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC}{ FPC doesn't support these switches }
  {$F+} { Force far calls }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$P-} { Normal string variables }
  {$N-} { No 80x87 code generation }
  {$E+} { Emulation is on }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

USES FVCommon;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           MEMORY ACCESS ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-MemAlloc-----------------------------------------------------------
Allocates the requested size of memory if this takes memory free below
the safety pool then a nil pointer is returned.
01Oct99 LdB
---------------------------------------------------------------------}
FUNCTION MemAlloc (Size: Word): Pointer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                 MEMORY MANAGER SYSTEM CONTROL ROUTINES                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-LowMemory----------------------------------------------------------
Returns if the free memory left is below the safety pool value.
01Oct99 LdB
---------------------------------------------------------------------}
FUNCTION LowMemory: Boolean;

{-InitMemory---------------------------------------------------------
Initializes the memory and safety pool manager. This should be called
prior to using any of the memory manager routines.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE InitMemory;

{-DoneMemory---------------------------------------------------------
Closes the memory and safety pool manager. This should be called after
using the memory manager routines so as to clean up.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE DoneMemory;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           CACHE MEMORY ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-NewCache-----------------------------------------------------------
Create a new cache of given size in pointer P failure will return nil.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE NewCache (Var P: Pointer; Size: Word);

{-DisposeCache-------------------------------------------------------
Dispose of a cache buffer given by pointer P.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE DisposeCache (P: Pointer);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          BUFFER MEMORY ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-GetBufferSize------------------------------------------------------
Returns the size of memory buffer given by pointer P.
01Oct99 LdB
---------------------------------------------------------------------}
FUNCTION GetBufferSize (P: Pointer): Word;

{-SetBufferSize------------------------------------------------------
Change the size of buffer given by pointer P to the size requested.
01Oct99 LdB
---------------------------------------------------------------------}
FUNCTION SetBufferSize (var P: Pointer; Size: Word): Boolean;

{-DisposeBuffer------------------------------------------------------
Dispose of buffer given by pointer P.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE DisposeBuffer (P: Pointer);

{-NewBuffer----------------------------------------------------------
Create a new buffer of given size in ptr P failure will return nil.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE NewBuffer (Var P: Pointer; Size: Word);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        DOS MEMORY CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-InitDosMem---------------------------------------------------------
Initialize memory manager routine for a shell to launch a DOS window.
Interface for compatability only under DPMI/WIN/NT/OS2 platforms.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE InitDosMem;

{-DoneDosMem---------------------------------------------------------
Finished shell to a DOS window so reset memory manager again.
Interface for compatability only under DPMI/WIN/NT/OS2 platforms.
01Oct99 LdB
---------------------------------------------------------------------}
PROCEDURE DoneDosMem;

{***************************************************************************}
{                         PUBLIC INITIALIZED VARIABLES                      }
{***************************************************************************}
CONST
   LowMemSize    : Word = 4096 DIV 16;                {   4K }
   SafetyPoolSize: Word = 8192;                       { Safety pool size }
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
   MaxHeapSize   : Word = 655360 DIV 16;              { 640K }
   MaxBufMem     : Word = 65536 DIV 16;               {  64K }
{$ENDIF}

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
{$IFDEF OS_WINDOWS}                                   { WIN/NT CODE }
   {$IFDEF PPC_FPC}                                   { FPC WINDOWS COMPILER }
   USES Windows;                                      { Standard unit }
   {$ELSE}                                            { OTHER COMPILERS }
   USES WinProcs, WinTypes;                           { Standard units }
   {$ENDIF}
{$ENDIF}

{$IFDEF OS_OS2}                                       { OS2 CODE }
  {$IFDEF PPC_FPC}
     USES DosCalls;                                        { Standard unit }
  {$ELSE}
     USES Os2Base;                                         { Standard unit }
  {$ENDIF}
{$ENDIF}

{***************************************************************************}
{                      PRIVATE RECORD TYPE DEFINITIONS                      }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                         TBuffer RECORD DEFINITION                         }
{---------------------------------------------------------------------------}
TYPE
   PBuffer = ^TBuffer;                                { Buffer pointer }
   TBuffer =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   PACKED
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   RECORD
     {$IFDEF PROC_REAL}                               { REAL MODE DOS CODE }
     Size  : Word;                                    { Buffer size }
     Master: ^Word;                                   { Master buffer }
     {$ELSE}                                          { DPMI/WIN/NT/OS2 CODE }
     Next: PBuffer;                                   { Next buffer }
     Size: Word;                                      { Buffer size }
     Data: RECORD END;                                { Buffer data }
     {$ENDIF}
   END;

{---------------------------------------------------------------------------}
{                     POINTER TYPE CONVERSION RECORDS                       }
{---------------------------------------------------------------------------}
TYPE
   PtrRec =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   PACKED
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   RECORD
     Ofs, Seg: Word;                                  { Pointer to words }
   END;

{---------------------------------------------------------------------------}
{                          TCache RECORD DEFINITION                         }
{---------------------------------------------------------------------------}
TYPE
   PCache = ^TCache;                                  { Cache pointer }
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
   TCache =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   PACKED
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   RECORD
      Size  : Word;                                   { Cache size }
      Master: ^Pointer;                               { Master cache }
      Data  : RECORD END;                             { Cache data }
   END;
{$ELSE}                                               { DPMI/WIN/NT/OS2 CODE }
   TCache =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   PACKED
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   RECORD
     Next  : PCache;                                  { Next cache }
     Master: ^Pointer;                                { Master cache }
     Size  : Word;                                    { Size of cache }
     Data  : RECORD END;                              { Cache data }
   End;
{$ENDIF}

{***************************************************************************}
{                       INITIALIZED PRIVATE VARIABLES                       }
{***************************************************************************}
CONST
   DisablePool: Boolean = False;                      { Disable safety pool }
   SafetyPool : Pointer = Nil;                        { Safety pool memory }
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
   HeapResult: Integer = 0;                           { Heap result }
   BufHeapPtr: Word = 0;                              { Heap position }
   BufHeapEnd: Word = 0;                              { Heap end }
   CachePtr  : Pointer = Nil;                         { Cache list }
{$ELSE}                                               { DPMI/WIN/NT/OS2 CODE }
   CacheList : PCache = Nil;                          { Cache list }
   BufferList: PBuffer = Nil;                         { Buffer list }
{$ENDIF}

{***************************************************************************}
{                          PRIVATE UNIT ROUTINES                            }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{             PRIVATE UNIT ROUTINES - REAL MODE DOS PLATFORMS               }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
{---------------------------------------------------------------------------}
{  GetBufSize -> Platforms DOS REAL MODE - Updated 01Oct99 LdB              }
{---------------------------------------------------------------------------}
FUNCTION GetBufSize (P: PBuffer): Word; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
BEGIN
   GetBufSize := (P^.Size + 15) SHR 4 + 1;            { Buffer paragraphs }
END;

{---------------------------------------------------------------------------}
{  FreeCacheMem -> Platforms DOS REAL MODE - Updated 01Oct99 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE FreeCacheMem; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
BEGIN
   While (CachePtr <> HeapEnd) Do
     DisposeCache(CachePtr);                          { Release blocks }
END;

{---------------------------------------------------------------------------}
{  SetMemTop -> Platforms DOS REAL MODE - Updated 01Oct99 LdB               }
{---------------------------------------------------------------------------}
PROCEDURE SetMemTop (MemTop: Pointer); ASSEMBLER;
ASM
   MOV BX, MemTop.Word[0];                            { Top of memory }
   ADD BX, 15;
   MOV CL, 4;
   SHR BX, CL;                                        { Size in paragraphs }
   ADD BX, MemTop.Word[2];
   MOV AX, PrefixSeg;                                 { Add prefix seg }
   SUB BX, AX;
   MOV ES, AX;
   MOV AH, 4AH;
   INT 21H;                                           { Call to DOS }
END;

{---------------------------------------------------------------------------}
{  MoveSeg -> Platforms DOS REAL MODE - Updated 01Oct99 LdB                 }
{---------------------------------------------------------------------------}
PROCEDURE MoveSeg (Source, Dest, Size: Word); NEAR; ASSEMBLER;
ASM
   PUSH DS;                                           { Save register }
   MOV AX, Source;
   MOV DX, Dest;                                      { Destination }
   MOV BX, Size;
   CMP AX, DX;                                        { Does Source=Dest? }
   JB @@3;
   CLD;                                               { Go forward }
@@1:
   MOV CX, 0FFFH;
   CMP CX, BX;
   JB @@2;
   MOV CX, BX;
@@2:
   MOV DS, AX;
   MOV ES, DX;
   ADD AX, CX;
   ADD DX, CX;
   SUB BX, CX;
   SHL CX, 3;                                         { Mult x8 }
   XOR SI, SI;
   XOR DI, DI;
   REP MOVSW;
   OR BX, BX;
   JNE @@1;
   JMP @@6;
@@3:                                                  { Source=Dest }
   ADD AX, BX;                                        { Hold register }
   ADD DX, BX;                                        { Must go backwards }
   STD;
@@4:
   MOV CX, 0FFFH;
   CMP CX, BX;
   JB @@5;
   MOV CX, BX;
@@5:
   SUB AX, CX;
   SUB DX, CX;
   SUB BX, CX;
   MOV DS, AX;
   MOV ES, DX;
   SHL CX, 3;                                         { Mult x8 }
   MOV SI, CX;
   DEC SI;
   SHL SI, 1;
   MOV DI, SI;
   REP MOVSW;                                         { Move data }
   OR BX, BX;
   JNE @@4;
@@6:
   POP DS;                                            { Recover register }
END;

{---------------------------------------------------------------------------}
{  SetBufSize -> Platforms DOS REAL MODE - Updated 01Oct99 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE SetBufSize (P: PBuffer; NewSize: Word); {$IFNDEF PPC_FPC}FAR;{$ENDIF}
VAR CurSize: Word;
BEGIN
   CurSize := GetBufSize(P);                          { Current size }
   MoveSeg(PtrRec(P).Seg + CurSize, PtrRec(P).Seg+
     NewSize, BufHeapPtr - PtrRec(P).Seg - CurSize);  { Move data }
   Inc(BufHeapPtr, NewSize - CurSize);                { Adjust heap space }
   Inc(PtrRec(P).Seg, NewSize);                       { Adjust pointer }
   While PtrRec(P).Seg < BufHeapPtr Do Begin
     Inc(P^.Master^, NewSize - CurSize);              { Adjust master }
     Inc(PtrRec(P).Seg, (P^.Size + 15) SHR 4 + 1);    { Adjust paragraphs }
   End;
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{            PRIVATE UNIT ROUTINES - DPMI/WIN/NT/OS2 PLATFORMS              }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{$IFNDEF PROC_REAL}                                   { DPMI/WIN/NT/OS2 CODE }
{---------------------------------------------------------------------------}
{  FreeCache -> Platforms DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB             }
{---------------------------------------------------------------------------}
FUNCTION FreeCache: Boolean; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
BEGIN
   FreeCache := False;                                { Preset fail }
   If (CacheList <> Nil) Then Begin
     DisposeCache(CacheList^.Next^.Master^);          { Dispose cache }
     FreeCache := True;                               { Return success }
   End;
END;

{---------------------------------------------------------------------------}
{  FreeCache -> Platforms DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB             }
{---------------------------------------------------------------------------}
FUNCTION FreeSafetyPool: Boolean; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
BEGIN
   FreeSafetyPool := False;                           { Preset fail }
   If (SafetyPool <> Nil) Then Begin                  { Pool exists }
     FreeMem(SafetyPool, SafetyPoolSize);             { Release memory }
     SafetyPool := Nil;                               { Clear pointer }
     FreeSafetyPool := True;                          { Return true }
   End;
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                 PRIVATE UNIT ROUTINES - ALL PLATFORMS                     }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  HeapNotify -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB        }
{---------------------------------------------------------------------------}
FUNCTION HeapNotify (Size: Word): Integer; {$IFNDEF PPC_FPC}FAR;{$ENDIF}
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
ASSEMBLER;
ASM
   CMP Size, 0;                                       { Check for zero size }
   JNE @@3;                                           { Exit if size = zero }
@@1:
   MOV AX, CachePtr.Word[2];
   CMP AX, HeapPtr.Word[2];                           { Compare segments }
   JA @@3;
   JB @@2;
   MOV AX, CachePtr.Word[0];
   CMP AX, HeapPtr.Word[0];                           { Compare offsets }
   JAE @@3;
@@2:
   XOR AX, AX;                                        { Clear register }
   PUSH AX;                                           { Push zero }
   PUSH AX;                                           { Push zero }
   CALL DisposeCache;                                 { Dispose cache }
   JMP @@1;
@@3:
   MOV AX, HeapResult;                                { Return result }
END;
{$ELSE}                                               { DPMI/WIN/NT/OS2 }
BEGIN
   If FreeCache Then HeapNotify := 2 Else             { Release cache }
     If DisablePool Then HeapNotify := 1 Else         { Safetypool disabled }
       If FreeSafetyPool Then HeapNotify := 2 Else    { Free safety pool }
         HeapNotify := 0;                             { Return success }
END;
{$ENDIF}


{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           MEMORY ACCESS ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  MemAlloc -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB          }
{---------------------------------------------------------------------------}
FUNCTION MemAlloc (Size: Word): Pointer;
VAR P: Pointer;
BEGIN
   {$IFDEF PROC_REAL}                                 { REAL MODE DOS CODE }
   HeapResult := 1;                                   { Stop error calls }
   GetMem(P, Size);                                   { Get memory }
   HeapResult := 0;                                   { Reset error calls }
   If (P <> Nil) AND LowMemory Then Begin             { Low memory }
     FreeMem(P, Size);                                { Release memory }
     P := Nil;                                        { Clear pointer }
   End;
   MemAlloc := P;                                     { Return result }
   {$ELSE}                                            { DPMI/WIN/NT/OS2 }
   DisablePool := True;                               { Disable safety }
   GetMem(P, Size);                                   { Allocate memory }
   DisablePool := False;                              { Enable safety }
   MemAlloc := P;                                     { Return result }
   {$ENDIF}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                 MEMORY MANAGER SYSTEM CONTROL ROUTINES                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  LowMemory -> Platforms DOS/DPMI/WIN/NT/OS2 - Checked 29Jun98 LdB         }
{---------------------------------------------------------------------------}
FUNCTION LowMemory: Boolean;
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
ASSEMBLER;
ASM
   MOV AX, HeapEnd.Word[2];                           { Get heap end }
   SUB AX, HeapPtr.Word[2];
   SUB AX, LowMemSize;                                { Subtract size }
   SBB AX, AX;
   NEG AX;                                            { Return result }
END;
{$ELSE}                                               { DPMI/WIN/NT/OS2 CODE }
BEGIN
   LowMemory := False;                                { Preset false }
   If (SafetyPool = Nil) Then Begin                   { Not initialized }
    SafetyPool := MemAlloc(SafetyPoolSize);           { Allocate safety pool }
    If (SafetyPool = Nil) Then LowMemory := True;     { Return if low memory }
   End;
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  InitMemory -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE InitMemory;
{$IFDEF PROC_REAL} VAR HeapSize: Word; {$ENDIF}
BEGIN
   {$IFDEF PROC_REAL}                                 { REAL MODE DOS CODE }
   HeapError := @HeapNotify;                          { Point to error proc }
   If (BufHeapPtr = 0) Then Begin
     HeapSize := PtrRec(HeapEnd).Seg
       - PtrRec(HeapOrg).Seg;                         { Calculate size }
     If (HeapSize > MaxHeapSize) Then
       HeapSize := MaxHeapSize;                       { Restrict max size }
     BufHeapEnd := PtrRec(HeapEnd).Seg;               { Set heap end }
     PtrRec(HeapEnd).Seg := PtrRec(HeapOrg).Seg
      + HeapSize;                                     { Add heapsize }
     BufHeapPtr := PtrRec(HeapEnd).Seg;               { Set heap pointer }
   End;
   CachePtr := HeapEnd;                               { Cache starts at end }
   {$ELSE}                                            { DPMI/WIN/NT/OS2 CODE }
   {$IFNDEF PPC_FPC}
   HeapError := @HeapNotify;                          { Set heap error proc }
   {$ENDIF}
   SafetyPoolSize := LowMemSize * 16;                 { Fix safety pool size }
   LowMemory;                                         { Check for low memory }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  DoneMemory -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE DoneMemory;
BEGIN
   {$IFDEF PROC_REAL}                                 { REAl MODE DOS CODE }
   FreeCacheMem;                                      { Release cache memory }
   {$ELSE}                                            { DPMI/WIN/NT/OS2 }
   While FreeCache Do;                                { Free cache memory }
   FreeSafetyPool;                                    { Release safety pool }
   {$ENDIF}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           CACHE MEMORY ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  NewCache -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE NewCache (Var P: Pointer; Size: Word);
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
ASSEMBLER;
ASM
   LES DI, P;                                         { Addres of var P }
   MOV AX, Size;
   ADD AX, (TYPE TCache)+15;                          { Add offset }
   MOV CL, 4;
   SHR AX, CL;
   MOV DX, CachePtr.Word[2];                          { Reteive cache ptr }
   SUB DX, AX;
   JC @@1;
   CMP DX, HeapPtr.Word[2];                           { Heap ptr end }
   JBE @@1;
   MOV CX, HeapEnd.Word[2];
   SUB CX, DX;
   CMP CX, MaxBufMem;                                 { Compare to maximum }
   JA @@1;
   MOV CachePtr.Word[2], DX;                          { Exchange ptr }
   PUSH DS;
   MOV DS, DX;
   XOR SI, SI;
   MOV DS:[SI].TCache.Size, AX;                       { Get cache size }
   MOV DS:[SI].TCache.Master.Word[0], DI;
   MOV DS:[SI].TCache.Master.Word[2], ES;             { Get master ptr }
   POP DS;
   MOV AX, OFFSET TCache.Data;
   JMP @@2;
@@1:
   XOR AX, AX;
   CWD;                                               { Make double word }
@@2:
   CLD;
   STOSW;                                             { Write low word }
   XCHG AX, DX;
   STOSW;                                             { Write high word }
END;
{$ELSE}                                               { DPMI/WIN/NT/OS2 CODE }
VAR Cache: PCache;
BEGIN
   Inc(Size, SizeOf(TCache));                         { Add cache size }
   If (MaxAvail >= Size) Then GetMem(Cache, Size)     { Allocate memory }
     Else Cache := Nil;                               { Not enough memory }
   If (Cache <> Nil) Then Begin                       { Cache is valid }
     If (CacheList = Nil) Then Cache^.Next := Cache
     Else Begin
       Cache^.Next := CacheList^.Next;                { Insert in list }
       CacheList^.Next := Cache;                      { Complete link }
     End;
     CacheList := Cache;                              { Hold cache ptr }
     Cache^.Size := Size;                             { Hold cache size }
     Cache^.Master := @P;                             { Hold master ptr }
{$ifdef fpc}
     Inc(Pointer(Cache), SizeOf(TCache));             { Set cache offset }
{$else fpc}
     Inc(PtrRec(Cache).Ofs, SizeOf(TCache));          { Set cache offset }
{$endif fpc}
   End;
   P := Cache;                                        { Return pointer }
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  DisposeCache -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE DisposeCache (P: Pointer);
{$IFDEF PROC_REAL}                                    { REAL MODE DOS CODE }
ASSEMBLER;
ASM
   MOV AX, CachePtr.Word[2];                          { Cache high word }
   XOR BX, BX;
   XOR CX, CX;
   MOV DX, P.Word[2];                                 { P high word }
@@1:
   MOV ES, AX;
   CMP AX, DX;                                        { Check for match }
   JE @@2;
   ADD AX, ES:[BX].TCache.Size;                       { Move to next cache }
   CMP AX, HeapEnd.Word[2];
   JE @@2;                                            { Are we at heap end }
   PUSH ES;
   INC CX;                                            { No so try next }
   JMP @@1;
@@2:
   PUSH ES;
   LES DI, ES:[BX].TCache.Master;                     { Pointe to master }
   XOR AX, AX;
   CLD;
   STOSW;                                             { Clear master ptr }
   STOSW;
   POP ES;
   MOV AX, ES:[BX].TCache.Size;                       { Next cache }
   JCXZ @@4;
@@3:
   POP DX;
   PUSH DS;
   PUSH CX;                                           { Hold registers }
   MOV DS, DX;
   ADD DX, AX;
   MOV ES, DX;
   MOV SI, DS:[BX].TCache.Size;                       { Get cache size }
   MOV CL, 3;
   SHL SI, CL;                                        { Multiply x8 }
   MOV CX, SI;
   SHL SI, 1;
   DEC SI;                                            { Adjust position }
   DEC SI;
   MOV DI, SI;
   STD;
   REP MOVSW;                                         { Move cache memory }
   LDS SI, ES:[BX].TCache.Master;
   MOV DS:[SI].Word[2], ES;                           { Store new master }
   POP CX;
   POP DS;                                            { Recover registers }
   LOOP @@3;
@@4:
   ADD CachePtr.Word[2], AX;                          { Add offset }
END;
{$ELSE}                                               { DPMI/WIN/NT/OS2 CODE }
VAR Cache, C: PCache;
BEGIN
{$ifdef fpc}
   Cache:=pointer(p)-SizeOf(TCache);
{$else fpc}
   PtrRec(Cache).Ofs := PtrRec(P).Ofs-SizeOf(TCache); { Previous cache }
   PtrRec(Cache).Seg := PtrRec(P).Seg;                { Segment }
{$endif fpc}
   C := CacheList;                                    { Start at 1st cache }
   While (C^.Next <> Cache) AND (C^.Next <> CacheList)
     Do C := C^.Next;                                 { Find previous }
   If (C^.Next = Cache) Then Begin                    { Cache found }
     If (C = Cache) Then CacheList := Nil Else Begin  { Only cache in list }
       If CacheList = Cache Then CacheList := C;      { First in list }
       C^.Next := Cache^.Next;                        { Remove from list }
     End;
     Cache^.Master^ := Nil;                           { Clear master }
     FreeMem(Cache, Cache^.Size);                     { Release memory }
   End;
END;
{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          BUFFER MEMORY ROUTINES                           }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  GetBufferSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB     }
{---------------------------------------------------------------------------}
FUNCTION GetBufferSize (P: Pointer): Word;
BEGIN
   {$IFDEF PROC_REAL}                                 { DOS CODE }
   Dec(PtrRec(P).Seg);                                { Segment prior }
   GetBufferSize := PBuffer(P)^.Size;                 { Size of this buffer }
   {$ELSE}                                            { DPMI/WIN/NT/OS2 CODE }
   If (P <> Nil) Then                                 { Check pointer }
     Begin
{$ifdef fpc}
       Dec(Pointer(P),SizeOf(TBuffer));                 { Correct to buffer }
{$else fpc}
       Dec(PtrRec(P).Ofs,SizeOf(TBuffer));              { Correct to buffer }
{$endif fpc}
       GetBufferSize := PBuffer(P)^.Size;               { Return buffer size }
     End
   Else
     GetBufferSize := 0;                       { Invalid pointer }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  SetBufferSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB     }
{---------------------------------------------------------------------------}
FUNCTION SetBufferSize (var P: Pointer; Size: Word): Boolean;
VAR NewSize: Word;
BEGIN
   SetBufferSize := False;                            { Preset failure }
   {$IFDEF PROC_REAL}                                 { REAL MODE DOS CODE }
   Dec(PtrRec(P).Seg);                                { Prior segment }
   NewSize := (Size + 15) SHR 4 + 1;                  { Paragraph size }
   If (BufHeapPtr+NewSize-GetBufSize(P)<=BufHeapEnd)  { Check enough heap }
   Then Begin
     SetBufSize(P, NewSize);                          { Set the buffer size }
     PBuffer(P)^.Size := Size;                        { Set the size }
     SetBufferSize := True;                           { Return success }
   End;
   {$ELSE}                                            { DPMI/WIN/NT/OS2 CODE }
 {$ifdef fpc}
   Dec(Pointer(P),SizeOf(TBuffer));                 { Correct to buffer }
   SetBufferSize := ReAllocMem(P, Size + SizeOf(TBuffer)) <> nil;
   if SetBufferSize then
      TBuffer(P^).Size := Size + SizeOf(TBuffer);
   Inc(Pointer(P), SizeOf(TBuffer));                 { Correct to buffer }
{$endif fpc}
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  DisposeBuffer -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE DisposeBuffer (P: Pointer);
{$IFNDEF PROC_REAL} VAR Buffer,PrevBuf: PBuffer; {$ENDIF}
BEGIN
   If (P <> Nil) Then Begin
     {$IFDEF PROC_REAL}                               { REAL MODE DOS CODE }
     Dec(PtrRec(P).Seg);                              { Prior segement }
     SetBufSize(P, 0);                                { Release memory }
     {$ELSE}                                          { DPMI/WIN/NT/OS2 CODE }
{$ifdef fpc}
     Dec(Pointer(P), SizeOf(TBuffer));                { Actual buffer pointer }
{$else fpc}
     Dec(PtrRec(P).Ofs, SizeOf(TBuffer));             { Actual buffer pointer }
{$endif fpc}
     Buffer := BufferList;                            { Start on first }
     PrevBuf := Nil;                                  { Preset prevbuf to nil }
     While (Buffer <> Nil) AND (P <> Buffer) Do Begin { Search for buffer }
       PrevBuf := Buffer;                             { Hold last buffer }
       Buffer := Buffer^.Next;                        { Move to next buffer }
     End;
     If (Buffer <> Nil) Then Begin                    { Buffer was found }
       If (PrevBuf = Nil) Then                        { We were first on list }
         BufferList := Buffer^.Next Else              { Set bufferlist entry }
         PrevBuf^.Next := Buffer^.Next;               { Remove us from chain }
       FreeMem(Buffer, Buffer^.Size);                 { Release buffer }
     End;
     {$ENDIF}
   End;
END;

{---------------------------------------------------------------------------}
{  NewBuffer -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE NewBuffer (Var P: Pointer; Size: Word);
VAR BufSize: Word; Buffer: PBuffer;
BEGIN
   {$IFDEF PROC_REAL}                                 { REAL MODE DOS CODE }
   BufSize := (Size + 15) SHR 4 + 1;                  { Paragraphs to alloc }
   If (BufHeapPtr+BufSize > BufHeapEnd) Then P := Nil { Exceeeds heap }
   Else Begin
     Buffer := Ptr(BufHeapPtr, 0);                    { Current position }
     Buffer^.Size := Size;                            { Set size }
     Buffer^.Master := @PtrRec(P).Seg;                { Set master }
     P := Ptr(BufHeapPtr + 1, 0);                     { Position ptr }
     Inc(BufHeapPtr, BufSize);                        { Allow space on heap }
   End;
   {$ELSE}                                            { DPMI/WIN/NT/OS2 CODE }
   BufSize := Size + SizeOf(TBuffer);                 { Size to allocate }
   Buffer := MemAlloc(BufSize);                       { Allocate the memory }
   If (Buffer <> Nil) Then Begin
     Buffer^.Next := BufferList;                      { First part of chain }
     BufferList := Buffer;                            { Complete the chain }
     Buffer^.Size := BufSize;                         { Hold the buffer size }
{$ifdef fpc}
     Inc(Pointer(Buffer), SizeOf(TBuffer));           { Buffer to data area }
{$else fpc}
     Inc(PtrRec(Buffer).Ofs, SizeOf(TBuffer));        { Buffer to data area }
{$endif fpc}
   End;
   P := Buffer;                                       { Return the buffer ptr }
   {$ENDIF}
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        DOS MEMORY CONTROL ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  InitDosMem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE InitDosMem;
BEGIN
   {$IFDEF PROC_REAL}                                 { REAl MODE DOS CODE }
   SetMemTop(Ptr(BufHeapEnd, 0));                     { Move heap to empty }
   {$ENDIF}
END;

{---------------------------------------------------------------------------}
{  DoneDosMem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 01Oct99 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE DoneDosMem;
{$IFDEF PROC_REAL} VAR MemTop: Pointer; {$ENDIF}
BEGIN
   {$IFDEF PROC_REAL}                                 { REAL MODE DOS CODE }
   MemTop := Ptr(BufHeapPtr, 0);                      { Top of memory }
   If (BufHeapPtr = PtrRec(HeapEnd).Seg) Then Begin   { Is memory empty }
     FreeCacheMem;                                    { Release memory }
     MemTop := HeapPtr;                               { Set pointer }
   End;
   SetMemTop(MemTop);                                 { Release memory }
   {$ENDIF}
END;

END.
