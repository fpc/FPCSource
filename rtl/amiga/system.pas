{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl Eric Codere
    Some parts taken from
       Marcel Timmermans - Modula 2 Compiler
       Nils Sjoholm - Amiga porter
       Matthew Dillon - Dice C (with his kind permission)
          dillon@backplane.com

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit {$ifdef VER1_0}sysamiga{$else}{$ifdef VER0_99}sysamiga{$ELSE}system{$endif}{$ENDIF};

{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o GetDir with different drive numbers                              }
{--------------------------------------------------------------------}

{$I os.inc}

{ AmigaOS uses character #10 as eoln only }
{$DEFINE SHORT_LINEBREAK}

  interface

    {$I systemh.inc}

    {$I heaph.inc}

{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;

 sLineBreak: string [1] = LineEnding;

    { used for single computations }
    const BIAS4 = $7f-1;

const
  UnusedHandle    : longint = -1;
  StdInputHandle  : longint = 0;
  StdOutputHandle : longint = 0;
  StdErrorHandle  : longint = 0;

 _ExecBase:longint = $4;
 _WorkbenchMsg : longint = 0;

 _IntuitionBase : pointer = nil;       { intuition library pointer }
 _DosBase       : pointer = nil;       { DOS library pointer       }
 _UtilityBase   : pointer = nil;       { utiity library pointer    }

 { Required for crt unit }
  function do_read(h,addr,len : longint) : longint;
  function do_write(h,addr,len : longint) : longint;





  implementation

 const

   intuitionname : pchar = 'intuition.library';
   dosname : pchar = 'dos.library';
   utilityname : pchar = 'utility.library';
   argc : longint = 0;
   { AmigaOS does not autoamtically deallocate memory on program termination }
   { therefore we have to handle this manually. This is a list of allocated  }
   { pointers from the OS, we cannot use a linked list, because the linked   }
   { list itself uses the HEAP!                                              }
   pointerlist : array[1..8] of longint =
    (0,0,0,0,0,0,0,0);


    {$I exec.inc}

  TYPE
    TDateStamp = packed record
        ds_Days         : Longint;      { Number of days since Jan. 1, 1978 }
        ds_Minute       : Longint;      { Number of minutes past midnight }
        ds_Tick         : Longint;      { Number of ticks past minute }
    end;
    PDateStamp = ^TDateStamp;


    PFileInfoBlock = ^TfileInfoBlock;
    TFileInfoBlock = packed record
        fib_DiskKey     : Longint;
        fib_DirEntryType : Longint;
                        { Type of Directory. If < 0, then a plain file.
                          If > 0 a directory }
        fib_FileName    : Array [0..107] of Char;
                        { Null terminated. Max 30 chars used for now }
        fib_Protection  : Longint;
                        { bit mask of protection, rwxd are 3-0. }
        fib_EntryType   : Longint;
        fib_Size        : Longint;      { Number of bytes in file }
        fib_NumBlocks   : Longint;      { Number of blocks in file }
        fib_Date        : TDateStamp; { Date file last changed }
        fib_Comment     : Array [0..79] of Char;
                        { Null terminated comment associated with file }
        fib_Reserved    : Array [0..35] of Char;
    end;


    TProcess = packed record
        pr_Task         : TTask;
        pr_MsgPort      : TMsgPort;      { This is BPTR address from DOS functions  }
{126}   pr_Pad          : Word;         { Remaining variables on 4 byte boundaries }
{128}   pr_SegList      : Pointer;      { Array of seg lists used by this process  }
{132}   pr_StackSize    : Longint;      { Size of process stack in bytes            }
{136}   pr_GlobVec      : Pointer;      { Global vector for this process (BCPL)    }
{140}   pr_TaskNum      : Longint;      { CLI task number of zero if not a CLI      }
{144}   pr_StackBase    : BPTR;         { Ptr to high memory end of process stack  }
{148}   pr_Result2      : Longint;      { Value of secondary result from last call }
{152}   pr_CurrentDir   : BPTR;         { Lock associated with current directory   }
{156}   pr_CIS          : BPTR;         { Current CLI Input Stream                  }
{160}   pr_COS          : BPTR;         { Current CLI Output Stream                 }
{164}   pr_ConsoleTask  : Pointer;      { Console handler process for current window}
{168}   pr_FileSystemTask : Pointer;    { File handler process for current drive   }
{172}   pr_CLI          : BPTR;         { pointer to ConsoleLineInterpreter         }
        pr_ReturnAddr   : Pointer;      { pointer to previous stack frame           }
        pr_PktWait      : Pointer;      { Function to be called when awaiting msg  }
        pr_WindowPtr    : Pointer;      { Window for error printing }
        { following definitions are new with 2.0 }
        pr_HomeDir      : BPTR;         { Home directory of executing program      }
        pr_Flags        : Longint;      { flags telling dos about process          }
        pr_ExitCode     : Pointer;      { code to call on exit of program OR NULL  }
        pr_ExitData     : Longint;      { Passed as an argument to pr_ExitCode.    }
        pr_Arguments    : PChar;        { Arguments passed to the process at start }
        pr_LocalVars    : TMinList;      { Local environment variables             }
        pr_ShellPrivate : Longint;      { for the use of the current shell         }
        pr_CES          : BPTR;         { Error stream - IF NULL, use pr_COS       }
    end;
    PProcess = ^TProcess;

  { AmigaOS does not automatically close opened files on exit back to  }
  { the operating system, therefore as a precuation we close all files }
  { manually on exit.                                                  }
  PFileList = ^TFileList;
  TFileList = record { no packed, must be correctly aligned }
   Handle: longint;      { Handle to file    }
   next: pfilelist;      { Next file in list }
   closed: boolean;      { TRUE=file already closed }
  end;




    Const
     CTRL_C               = 20;      { Error code on CTRL-C press }
     SIGBREAKF_CTRL_C     = $1000;   { CTRL-C signal flags }

    _LVOFindTask          = -294;
    _LVOWaitPort          = -384;
    _LVOGetMsg            = -372;
    _LVOOpenLibrary       = -552;
    _LVOCloseLibrary      = -414;
    _LVOClose             = -36;
    _LVOOpen              = -30;
    _LVOIoErr             = -132;
    _LVOSeek              = -66;
    _LVODeleteFile        = -72;
    _LVORename            = -78;
    _LVOWrite             = -48;
    _LVORead              = -42;
    _LVOCreateDir         = -120;
    _LVOSetCurrentDirName = -558;
    _LVOGetCurrentDirName = -564;
    _LVOInput             = -54;
    _LVOOutput            = -60;
    _LVOUnLock            = -90;
    _LVOLock              = -84;
    _LVOCurrentDir        = -126;

    _LVONameFromLock      = -402;
    _LVONameFromFH        = -408;
    _LVOGetProgramName    = -576;
    _LVOGetProgramDir     = -600;
    _LVODupLock           =  -96;
    _LVOExamine           = -102;
    _LVOParentDir         = -210;
    _LVOSetFileSize       = -456;
    _LVOSetSignal         = -306;
    _LVOAllocVec          = -684;
    _LVOFreeVec           = -690;


      { Errors from IoErr(), etc. }
      ERROR_NO_FREE_STORE              = 103;
      ERROR_TASK_TABLE_FULL            = 105;
      ERROR_BAD_TEMPLATE               = 114;
      ERROR_BAD_NUMBER                 = 115;
      ERROR_REQUIRED_ARG_MISSING       = 116;
      ERROR_KEY_NEEDS_ARG              = 117;
      ERROR_TOO_MANY_ARGS              = 118;
      ERROR_UNMATCHED_QUOTES           = 119;
      ERROR_LINE_TOO_LONG              = 120;
      ERROR_FILE_NOT_OBJECT            = 121;
      ERROR_INVALID_RESIDENT_LIBRARY   = 122;
      ERROR_NO_DEFAULT_DIR             = 201;
      ERROR_OBJECT_IN_USE              = 202;
      ERROR_OBJECT_EXISTS              = 203;
      ERROR_DIR_NOT_FOUND              = 204;
      ERROR_OBJECT_NOT_FOUND           = 205;
      ERROR_BAD_STREAM_NAME            = 206;
      ERROR_OBJECT_TOO_LARGE           = 207;
      ERROR_ACTION_NOT_KNOWN           = 209;
      ERROR_INVALID_COMPONENT_NAME     = 210;
      ERROR_INVALID_LOCK               = 211;
      ERROR_OBJECT_WRONG_TYPE          = 212;
      ERROR_DISK_NOT_VALIDATED         = 213;
      ERROR_DISK_WRITE_PROTECTED       = 214;
      ERROR_RENAME_ACROSS_DEVICES      = 215;
      ERROR_DIRECTORY_NOT_EMPTY        = 216;
      ERROR_TOO_MANY_LEVELS            = 217;
      ERROR_DEVICE_NOT_MOUNTED         = 218;
      ERROR_SEEK_ERROR                 = 219;
      ERROR_COMMENT_TOO_BIG            = 220;
      ERROR_DISK_FULL                  = 221;
      ERROR_DELETE_PROTECTED           = 222;
      ERROR_WRITE_PROTECTED            = 223;
      ERROR_READ_PROTECTED             = 224;
      ERROR_NOT_A_DOS_DISK             = 225;
      ERROR_NO_DISK                    = 226;
      ERROR_NO_MORE_ENTRIES            = 232;
      { added for 1.4 }
      ERROR_IS_SOFT_LINK               = 233;
      ERROR_OBJECT_LINKED              = 234;
      ERROR_BAD_HUNK                   = 235;
      ERROR_NOT_IMPLEMENTED            = 236;
      ERROR_RECORD_NOT_LOCKED          = 240;
      ERROR_LOCK_COLLISION             = 241;
      ERROR_LOCK_TIMEOUT               = 242;
      ERROR_UNLOCK_ERROR               = 243;



    var
      Initial: boolean;           { Have successfully opened Std I/O   }
      errno : word;               { AmigaOS IO Error number            }
      FileList : pFileList;       { Linked list of opened files        }
      {old_exit: Pointer; not needed anymore }
      FromHalt : boolean;
      OrigDir : Longint;   { Current lock on original startup directory }

    {$I system.inc}
    {$I lowmath.inc}




  { ************************ AMIGAOS STUB ROUTINES ************************* }

  procedure DateStamp(var ds : tDateStamp);
  begin
   asm
      MOVE.L  A6,-(A7)
      MOVE.L  ds,d1
      { LAST THING TO SETUP SHOULD BE A6, otherwise you can }
      { not accept local variable, nor any parameters! :)   }
      MOVE.L  _DOSBase,A6
      JSR -192(A6)
      MOVE.L  (A7)+,A6
  end;
 end;



  { UNLOCK the BPTR pointed to in L }
  Procedure Unlock(alock: longint);
  Begin
    asm
     move.l  alock,d1
     move.l  a6,d6           { save base pointer    }
     move.l   _DosBase,a6
     jsr     _LVOUnlock(a6)
     move.l  d6,a6           { restore base pointer }
    end;
  end;

  { Change to the directory pointed to in the lock }
  Function CurrentDir(alock : longint) : longint;
  Begin
    asm
      move.l  alock,d1
      move.l  a6,d6           { save base pointer    }
      move.l  _DosBase,a6
      jsr     _LVOCurrentDir(a6)
      move.l  d6,a6           { restore base pointer }
      move.l  d0,@Result
    end;
  end;

  { Duplicate a lock }
  Function DupLock(alock: longint): Longint;
   Begin
     asm
       move.l  alock,d1
       move.l  a6,d6           { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVODupLock(a6)
       move.l  d6,a6           { restore base pointer }
       move.l  d0,@Result
     end;
   end;

  { Returns a lock on the directory was loaded from }
  Function GetProgramLock: longint;
  Begin
   asm
       move.l  a6,d6           { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVOGetProgramDir(a6)
       move.l  d6,a6           { restore base pointer }
       move.l  d0,@Result
   end;
  end;



  Function Examine(alock :longint; var fib: TFileInfoBlock) : Boolean;
  Begin
    asm
       move.l  d2,-(sp)
       move.l  fib,d2         { pointer to FIB        }
       move.l  alock,d1
       move.l  a6,d6           { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVOExamine(a6)
       move.l  d6,a6           { restore base pointer }
       tst.l   d0
       bne     @success
       bra     @end
    @success:
       move.b  #1,d0
    @end:
       move.b  d0,@Result
       move.l  (sp)+,d2
    end;
  end;

  { Returns the parent directory of a lock }
  Function ParentDir(alock : longint): longint;
   Begin
     asm
       move.l  alock,d1
       move.l  a6,d6           { save base pointer    }
       move.l  _DosBase,a6
       jsr     _LVOParentDir(a6)
       move.l  d6,a6           { restore base pointer }
       move.l  d0,@Result
     end;
   end;


   Function FindTask(p : PChar): PProcess;
   Begin
    asm
         move.l  a6,d6              { Save base pointer    }
         move.l  p,d0
         move.l  d0,a1
         move.l  _ExecBase,a6
         jsr     _LVOFindTask(a6)
         move.l  d6,a6              { Restore base pointer }
         move.l  d0,@Result
    end;
   end;


{$S-}
    Procedure stack_check; assembler;
    { Check for local variable allocation }
    { On Entry -> d0 : size of local stack we are trying to allocate }
     asm
      XDEF STACKCHECK
        move.l  sp,d1            { get value of stack pointer            }

        { We must add some security, because Writing the RunError strings }
        { requires a LOT of stack space (at least 1030 bytes!)            }
        add.l   #2048,d0
        sub.l   d0,d1            {  sp - stack_size                      }

        move.l  _ExecBase,a0
        move.l  276(A0),A0       { ExecBase.thisTask }
        { if allocated stack_pointer - splower <= 0 then stack_ovf       }
        cmp.l   58(A0),D1        { Task.SpLower      }
        bgt     @Ok
        move.l  #202,d0
        jsr     HALT_ERROR       { stack overflow    }
    @Ok:
   end;


   { This routine from EXEC determines if the Ctrl-C key has }
   { been used since the last call to I/O routines.          }
   { Use to halt the program.                                }
   { Returns the state of the old signals.                   }
   Function SetSignal(newSignal: longint; SignalMask: longint): longint;
   Begin
     asm
       move.l  newSignal,d0
       move.l  SignalMask,d1
       move.l  a6,d6          { save Base pointer into scratch register }
       move.l  _ExecBase,a6
       jsr     _LVOSetSignal(a6)
       move.l  d6,a6
       move.l  d0,@Result
     end;
   end;


   Function AllocVec(bytesize: longint; attributes: longint):longint;
   Begin
     asm
       move.l  bytesize,d0
       move.l  attributes,d1
       move.l  a6,d6          { save Base pointer into scratch register }
       move.l  _ExecBase,a6
       jsr     _LVOAllocVec(a6)
       move.l  d6,a6
       move.l  d0,@Result
     end;
   end;


   Procedure FreeVec(p: longint);
   Begin
     asm
       move.l  p,a1
       move.l  a6,d6          { save Base pointer into scratch register }
       move.l  _ExecBase,a6
       jsr     _LVOFreeVec(a6)
       move.l  d6,a6
     end;
   end;


   { Converts an AMIGAOS error code to a TP compatible error code }
   Procedure Error2InOut;
   Begin
     case errno of
       ERROR_BAD_NUMBER,
       ERROR_ACTION_NOT_KNOWN,
       ERROR_NOT_IMPLEMENTED : InOutRes := 1;

       ERROR_OBJECT_NOT_FOUND : InOutRes := 2;
       ERROR_DIR_NOT_FOUND :  InOutRes := 3;

       ERROR_DISK_WRITE_PROTECTED : InOutRes := 150;

       ERROR_OBJECT_WRONG_TYPE : InOutRes := 151;

       ERROR_OBJECT_EXISTS,
       ERROR_DELETE_PROTECTED,
       ERROR_WRITE_PROTECTED,
       ERROR_READ_PROTECTED,
       ERROR_OBJECT_IN_USE,
       ERROR_DIRECTORY_NOT_EMPTY : InOutRes := 5;

       ERROR_NO_MORE_ENTRIES : InOutRes := 18;

       ERROR_RENAME_ACROSS_DEVICES : InOutRes := 17;

       ERROR_DISK_FULL : InOutRes := 101;

       ERROR_INVALID_RESIDENT_LIBRARY : InoutRes := 153;
       ERROR_BAD_HUNK : InOutRes := 153;

       ERROR_NOT_A_DOS_DISK : InOutRes := 157;

       ERROR_NO_DISK,
       ERROR_DISK_NOT_VALIDATED,
       ERROR_DEVICE_NOT_MOUNTED : InOutRes := 152;

       ERROR_SEEK_ERROR : InOutRes := 156;

       ERROR_LOCK_COLLISION,
       ERROR_LOCK_TIMEOUT,
       ERROR_UNLOCK_ERROR,
       ERROR_INVALID_LOCK,
       ERROR_INVALID_COMPONENT_NAME,
       ERROR_BAD_STREAM_NAME,
       ERROR_FILE_NOT_OBJECT : InOutRes := 6;
     else
       InOutres := errno;
     end;
     errno:=0;
   end;


    procedure CloseLibrary(lib : pointer);
    {  Close the library pointed to in lib }
    Begin
      asm
         MOVE.L  A6,-(A7)
         MOVE.L  lib,a1
         MOVE.L  _ExecBase,A6
         JSR     _LVOCloseLibrary(A6)
         MOVE.L  (A7)+,A6
      end;
    end;


   Function KickVersion: word; assembler;
   asm
     move.l  _ExecBase, a0       { Get Exec Base                           }
     move.w  20(a0), d0          { Return version - version at this offset }
   end;


  { ************************ AMIGAOS SUPP ROUTINES ************************* }

(*  Procedure CloseList(p: pFileList);*)
  (***********************************************************************)
  (* PROCEDURE CloseList                                                 *)
  (*  Description: This routine each time the program is about to        *)
  (*  terminate, it closes all opened file handles, as this is not       *)
  (*  handled by the operating system.                                   *)
  (*   p -> Start of linked list of opened files                         *)
  (***********************************************************************)
(*  var
   hp: pFileList;
   hp1: pFileList;
   h: longint;
  Begin
   hp:=p;
   while Assigned(hp) do
    Begin
      if NOT hp^.closed then
       Begin
        h:=hp^.handle;
        if (h <> StdInputHandle) and (h <> StdOutputHandle) and (h <> StdErrorHandle) then
        Begin
          { directly close file here, it is faster then doing }
          { it do_close.                                      }
          asm
            move.l  h,d1
            move.l  a6,d6              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVOClose(a6)
            move.l  d6,a6              { restore a6 }
          end;
        end;
       end;
      hp1:=hp;
      hp:=hp^.next;
      dispose(hp1);
    end;
  end;*)


(* Procedure AddToList(var p: pFileList; h: longint);*)
  (***********************************************************************)
  (* PROCEDURE AddToList                                                 *)
  (*  Description: Adds a node to the linked list of files.              *)
  (*                                                                     *)
  (*   p -> Start of File list linked list, if not allocated allocates   *)
  (*        it for you.                                                  *)
  (*   h -> handle of file to add                                        *)
  (***********************************************************************)
(*  var
   hp: pFileList;
   hp1: pFileList;
  Begin
    if p = nil then
     Begin
       new(p);
       p^.handle:=h;
       p^.closed := FALSE;
       p^.next := nil;
       exit;
     end;
     hp:=p;
    { Find last list in entry }
    while assigned(hp) do
     Begin
        if hp^.next = nil then break;
        hp:=hp^.next;
     end;
    { Found last list in entry then add it to the list }
    new(hp1);
    hp^.next:=hp1;
    hp1^.next:=nil;
    hp1^.handle:=h;
    hp1^.closed:=FALSE;
  end;


  Procedure SetClosedList(var p: pFileList; h: longint);
  { Set the file flag to closed if the file is being closed }
  var
   hp: pFileList;
  Begin
    hp:=p;
    while assigned(hp) do
     Begin
        if hp^.handle = h then
         Begin
           hp^.closed:=TRUE;
           break;
         end;
        hp:=hp^.next;
     end;
  end;*)


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
  Procedure system_exit;
    var
     i: byte;
    Begin
        { We must remove the CTRL-C FALG here because halt }
        { may call I/O routines, which in turn might call  }
        { halt, so a recursive stack crash                 }
        IF (SetSignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 THEN
           SetSignal(0,SIGBREAKF_CTRL_C);
         { Close remaining opened files }
{         CloseList(FileList); }
        if (OrigDir <> 0) then
         Begin
            Unlock(CurrentDir(OrigDir));
            OrigDir := 0;
         end;
         { Is this a normal exit - YES, close libs }
         IF NOT FromHalt then
           Begin
             { close the libraries }
             If _UtilityBase <> nil then
                 CloseLibrary(_UtilityBase);
             If _DosBase <> nil then
                 CloseLibrary(_DosBase);
             If _IntuitionBase <> nil then
                 CloseLibrary(_IntuitionBase);
             _UtilityBase := nil;
             _DosBase := nil;
             _IntuitionBase := nil;
           end;
         { Dispose of extraneous allocated pointers }
         for I:=1 to 8 do
           Begin
             if pointerlist[i] <> 0 then FreeVec(pointerlist[i]);
           end;
         { exitproc:=old_exit;obsolete }
    end;


    procedure halt(errnum : byte);
      begin
        { Indicate to the SYSTEM EXIT procedure that we are calling it }
        { from halt, and that its library will be closed HERE and not  }
        { in the exit procedure.                                       }
        FromHalt:=TRUE;
        { We must remove the CTRL-C FALG here because halt }
        { may call I/O routines, which in turn might call  }
        { halt, so a recursive stack crash                 }
        IF (SetSignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 THEN
           SetSignal(0,SIGBREAKF_CTRL_C);
        { WE can only FLUSH the stdio   }
        { if the handles have correctly }
        { been set.                     }
        { No exit procedures exist      }
        { if in initial state           }
        If NOT Initial then
        Begin
          do_exit;
          flush(stderr);
        end;
        { close the libraries }
        If _UtilityBase <> nil then
           CloseLibrary(_UtilityBase);
        If _DosBase <> nil then
           CloseLibrary(_DosBase);
        If _IntuitionBase <> nil then
           CloseLibrary(_IntuitionBase);
        _UtilityBase := nil;
        _DosBase := nil;
        _IntuitionBase := nil;
         asm
            clr.l   d0
            move.b  errnum,d0
            move.l  STKPTR,sp
            rts
         end;
      end;



  { ************************ PARAMCOUNT/PARAMSTR *************************** }

      function paramcount : longint;
      Begin
        paramcount := argc;
      end;


      function args : pointer; assembler;
      asm
         move.l __ARGS,d0
      end;

   Function GetParamCount(const p: pchar): longint;
   var
    i: word;
    count: word;
   Begin
    i:=0;
    count:=0;
    while p[count] <> #0 do
     Begin
       if (p[count] <> ' ') and (p[count] <> #9) and (p[count] <> #0) then
       Begin
          i:=i+1;
          while (p[count] <> ' ') and (p[count] <> #9) and (p[count] <> #0) do
           count:=count+1;
       end;
       if p[count] = #0 then break;
       count:=count+1;
     end;
     GetParamCount:=longint(i);
   end;


   Function GetParam(index: word; const p : pchar): string;
   { On Entry: index = string index to correct parameter  }
   { On exit:  = correct character index into pchar array }
   { Returns correct index to command line argument }
   var
    count: word;
    localindex: word;
    l: byte;
    temp: string;
   Begin
     temp:='';
     count := 0;
     { first index is one }
     localindex := 1;
     l:=0;
     While p[count] <> #0 do
       Begin
         if (p[count] <> ' ') and (p[count] <> #9) then
           Begin
             if localindex = index then
              Begin
               while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) and (l < 256) do
                Begin
                  temp:=temp+p[count];
                  l:=l+1;
                  count:=count+1;
                end;
                temp[0]:=char(l);
                GetParam:=temp;
                exit;
              end;
             { Point to next argument in list }
             while (p[count] <> #0) and (p[count] <> ' ') and (p[count] <> #9) do
               Begin
                 count:=count+1;
               end;
             localindex:=localindex+1;
           end;
         if p[count] = #0 then break;
         count:=count+1;
       end;
     GetParam:=temp;
   end;


    Function GetProgramDir : String;
    var
     s1: string;
     alock: longint;
     counter : byte;
    Begin
     FillChar(@s1,255,#0);
     { GetLock of program directory }
     asm
            move.l  a6,d6              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVOGetProgramDir(a6)
            move.l  d6,a6              { restore a6 }
            move.l  d0,alock           { save the lock }
     end;
     if alock <> 0 then
      Begin
        { Get the name from the lock! }
        asm
            movem.l d2/d3,-(sp)        { save used registers             }
            move.l  alock,d1
            lea     s1,a0              { Get pointer to string!          }
            move.l  a0,d2
            add.l   #1,d2              { let  us point past the length byte! }
            move.l  #255,d3
            move.l  a6,d6              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVONameFromLock(a6)
            move.l  d6,a6              { restore a6 }
            movem.l (sp)+,d2/d3
        end;
        { no check out the length of the string }
        counter := 1;
        while s1[counter] <> #0 do
           Inc(counter);
        s1[0] := char(counter-1);
        GetProgramDir := s1;
      end
     else
      GetProgramDir := '';
    end;


    Function GetProgramName : string;
    { Returns ONLY the program name }
    { There seems to be a bug in v39 since if the program is not }
    { called from its home directory the program name will also  }
    { contain the path!                                          }
    var
     s1: string;
     counter : byte;
    Begin
      FillChar(@s1,255,#0);
      asm
            move.l  d2,-(sp)           { Save used register      }
            lea     s1,a0              { Get pointer to string!  }
            move.l  a0,d1
            add.l   #1,d1              { point to correct offset }
            move.l  #255,d2
            move.l  a6,d6              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVOGetProgramName(a6)
            move.l  d6,a6              { restore a6 }
            move.l  (sp)+,d2           { restore saved register }
      end;
        { no check out and assign the length of the string }
        counter := 1;
        while s1[counter] <> #0 do
           Inc(counter);
        s1[0] := char(counter-1);
        { now remove any component path which should not be there }
        for counter:=length(s1) downto 1 do
          if (s1[counter] = '/') or (s1[counter] = ':') then break;
        { readjust counterv to point to character }
        if counter <> 1 then
          Inc(counter);
        GetProgramName:=copy(s1,counter,length(s1));
    end;


    function paramstr(l : longint) : string;
      var
       p : pchar;
       s1 : string;
      begin
         {   -> Call AmigaOS GetProgramName                             }
         if l = 0 then
         Begin
           s1 := GetProgramDir;
           { If this is a root, then simply don't add '/' }
           if s1[length(s1)] = ':' then
              paramstr:=s1+GetProgramName
           else
              { add backslash directory }
              paramstr:=s1+'/'+GetProgramName
         end
         else
         if (l>0) and (l<=paramcount) then
           begin
             p:=args;
             paramstr:=GetParam(word(l),p);
           end
         else paramstr:='';
      end;

  { ************************************************************************ }

    procedure randomize;

      var
         hl : longint;
         time : TDateStamp;
      begin
         DateStamp(time);
         randseed:=time.ds_tick;
      end;

function getheapstart:pointer;assembler;
asm
        lea.l   HEAP,a0
        move.l  a0,d0
end;


function getheapsize:longint;assembler;
asm
       move.l   HEAP_SIZE,d0
end ['D0'];

  { This routine is used to grow the heap.  }
  { But here we do a trick, we say that the }
  { heap cannot be regrown!                 }
  function sbrk( size: longint): longint;
  var
  { on exit -1 = if fails.               }
   p: longint;
   i: byte;
  Begin
    p:=0;
    { Is the pointer list full }
    if pointerlist[8] <> 0 then
    begin
     { yes, then don't allocate and simply exit }
     sbrk:=-1;
     exit;
    end;
    { Allocate best available memory }
    p:=AllocVec(size,0);
    if p = 0 then
     sbrk:=-1
    else
    Begin
       i:=1;
       { add it to the list of allocated pointers }
       { first find the last pointer in the list  }
       while (i < 8) and (pointerlist[i] <> 0) do
         i:=i+1;
       pointerlist[i]:=p;
       sbrk:=p;
    end;
  end;



{$I heap.inc}


{****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

procedure do_close(h : longint);
{ We cannot check for CTRL-C because this routine will be called }
{ on HALT to close all remaining opened files. Therefore no      }
{ CTRL-C checking otherwise a recursive call might result!       }
{$ifdef debug}
var
  buffer: array[0..255] of char;
{$endif}
begin
  { check if the file handle is in the list }
  { if so the put its field to closed       }
{  SetClosedList(FileList,h);}
{$ifdef debug}
  asm
     move.l  h,d1
     move.l  a6,d6
     move.l  d2,-(sp)
     move.l  d3,-(sp)
     lea     buffer,a0
     move.l  a0,d2
     move.l  #255,d3
     move.l  _DosBase,a6
     jsr     _LVONameFromFH(a6)
     move.l  d6,a6
     move.l  (sp)+,d3
     move.l  (sp)+,d2
  end;
  WriteLn(Buffer);
{$endif debug}
  asm
     move.l  h,d1
     move.l  a6,d6              { save a6 }
     move.l  _DOSBase,a6
     jsr     _LVOClose(a6)
     move.l  d6,a6              { restore a6 }
  end;
end;


function do_isdevice(handle:longint):boolean;
begin
  if (handle=stdoutputhandle) or (handle=stdinputhandle) or
  (handle=stderrorhandle) then
    do_isdevice:=TRUE
  else
    do_isdevice:=FALSE;
end;



procedure do_erase(p : pchar);
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  asm
           move.l  a6,d6               { save a6 }

           move.l  p,d1
           move.l  _DOSBase,a6
           jsr     _LVODeleteFile(a6)
           tst.l   d0                  { zero = failure }
           bne     @noerror

           jsr     _LVOIoErr(a6)
           move.w  d0,errno

         @noerror:
           move.l  d6,a6               { restore a6 }
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure do_rename(p1,p2 : pchar);
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  asm
           move.l  a6,d6                  { save a6 }
           move.l  d2,-(sp)               { save d2 }

           move.l  p1,d1
           move.l  p2,d2
           move.l  _DOSBase,a6
           jsr     _LVORename(a6)
           move.l  (sp)+,d2               { restore d2 }
           tst.l   d0
           bne     @dosreend              { if zero = error }
           jsr     _LVOIoErr(a6)
           move.w  d0,errno
         @dosreend:
           move.l  d6,a6                  { restore a6 }
  end;
  if errno <> 0 then
    Error2InOut;
end;


function do_write(h,addr,len : longint) : longint;
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  if len <= 0 then
   Begin
    do_write:=0;
    exit;
   end;
  asm
            move.l  a6,d6

            movem.l d2/d3,-(sp)
            move.l  h,d1             { we must of course set up the }
            move.l  addr,d2          { parameters BEFORE getting    }
            move.l  len,d3           { _DOSBase                     }
            move.l  _DOSBase,a6
            jsr     _LVOWrite(a6)
            movem.l (sp)+,d2/d3

            cmp.l   #-1,d0
            bne     @doswrend              { if -1 = error }
            jsr     _LVOIoErr(a6)
            move.w  d0,errno
            bra     @doswrend2
          @doswrend:
            { we must restore the base pointer before setting the result }
            move.l  d6,a6
            move.l  d0,@RESULT
            bra     @end
          @doswrend2:
            move.l  d6,a6
          @end:
  end;
  If errno <> 0 then
    Error2InOut;
end;


function do_read(h,addr,len : longint) : longint;
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  if len <= 0 then
  Begin
     do_read:=0;
     exit;
  end;
  asm
            move.l  a6,d6

            movem.l d2/d3,-(sp)
            move.l  h,d1         { we must set up aparamters BEFORE }
            move.l  addr,d2      { setting up a6 for the OS call    }
            move.l  len,d3
            move.l  _DOSBase,a6
            jsr     _LVORead(a6)
            movem.l (sp)+,d2/d3

            cmp.l   #-1,d0
            bne     @doswrend              { if -1 = error }
            jsr     _LVOIoErr(a6)
            move.w  d0,errno
            bra     @doswrend2
          @doswrend:
            { to store a result for the function  }
            { we must of course first get back the}
            { base pointer!                       }
            move.l  d6,a6
            move.l  d0,@RESULT
            bra     @end
          @doswrend2:
            move.l  d6,a6
          @end:
  end;
  If errno <> 0 then
    Error2InOut;
end;


function do_filepos(handle : longint) : longint;
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  asm
             move.l  a6,d6

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers              }

             clr.l   d2                    { offset 0 }
             move.l  #0,d3                 { OFFSET_CURRENT }
             move.l  _DOSBase,a6
             jsr    _LVOSeek(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
             move.w  d0,errno
             bra     @fposend
      @noerr:
             move.l  d6,a6                 { restore a6 }
             move.l  d0,@Result
             bra     @end
      @fposend:
             move.l  d6,a6                 { restore a6 }
      @end:
  end;
  If errno <> 0 then
    Error2InOut;
end;


procedure do_seek(handle,pos : longint);
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  asm
             move.l  a6,d6

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers              }

             move.l  pos,d2
             { -1 }
             move.l  #$ffffffff,d3          { OFFSET_BEGINNING }
             move.l  _DOSBase,a6
             jsr    _LVOSeek(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
             move.w  d0,errno
             bra     @seekend
      @noerr:
      @seekend:
             move.l  d6,a6                 { restore a6 }
  end;
  If errno <> 0 then
    Error2InOut;
end;


function do_seekend(handle:longint):longint;
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  asm
             { seek from end of file }
             move.l  a6,d6

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers              }

             clr.l   d2
             move.l  #1,d3                 { OFFSET_END }
             move.l  _DOSBase,a6
             jsr    _LVOSeek(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
             move.w  d0,errno
             bra     @seekend
      @noerr:
             move.l  d6,a6                 { restore a6 }
             move.l  d0,@Result
             bra     @end
      @seekend:
             move.l  d6,a6                 { restore a6 }
      @end:
  end;
  If Errno <> 0 then
    Error2InOut;
end;


function do_filesize(handle : longint) : longint;
var
  aktfilepos : longint;
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
    Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
    end;
   aktfilepos:=do_filepos(handle);
   { We have to do this two times, because seek returns the }
   { OLD position                                           }
   do_filesize:=do_seekend(handle);
   do_filesize:=do_seekend(handle);
   do_seek(handle,aktfilepos);
end;


procedure do_truncate (handle,pos:longint);
begin
      { Point to the end of the file }
      { with the new size            }
      asm
      @noerr_one:                          { Seek a second time            }
             move.l  a6,d6                 { Save base pointer             }

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers                }

             move.l  pos,d2
             move.l  #-1,d3                { Setup correct move type     }
             move.l  _DOSBase,a6           { from beginning of file      }
             jsr    _LVOSetFileSize(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
             move.w  d0,errno              { Global variable, so no need    }
      @noerr:                              { to restore base pointer now    }
             move.l  d6,a6                 { Restore base pointer           }
      end;
  If Errno <> 0 then
    Error2InOut;
end;


procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  i,j : longint;
  oflags: longint;
  path : string;
  buffer : array[0..255] of char;
  index : integer;
  s : string;
begin
 path:=strpas(p);
 for index:=1 to length(path) do
   if path[index]='\' then path[index]:='/';
 { remove any dot characters and replace by their current }
 { directory equivalent.                                  }
 if pos('../',path) = 1 then
 { look for parent directory }
    Begin
       delete(path,1,3);
       getdir(0,s);
       j:=length(s);
       while (s[j] <> '/') AND (s[j] <> ':') AND (j > 0 ) do
         dec(j);
       if j > 0 then
         s:=copy(s,1,j);
       path:=s+path;
    end
 else
 if pos('./',path) = 1 then
 { look for current directory }
    Begin
       delete(path,1,2);
       getdir(0,s);
       if (s[length(s)] <> '/') and (s[length(s)] <> ':') then
          s:=s+'/';
       path:=s+path;
    end;
  move(path[1],buffer,length(path));
  buffer[length(path)]:=#0;
 { close first if opened }
  if ((flags and $10000)=0) then
   begin
     case filerec(f).mode of
      fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file handle }
  filerec(f).handle:=UnusedHandle;
{ convert filemode to filerec modes }
  { READ/WRITE on existing file }
  { RESET/APPEND                }
  oflags := 1005;
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
       end;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  { READ/WRITE mode, create file in all cases }
  { REWRITE                                   }
  if (flags and $1000)<>0 then
   begin
     filerec(f).mode:=fmoutput;
     oflags := 1006;
   end
  else
  { READ/WRITE mode on existing file }
  { APPEND                           }
   if (flags and $100)<>0 then
    begin
      filerec(f).mode:=fmoutput;
      oflags := 1005;
    end;
{ empty name is special }
  if p[0]=#0 then
   begin
     case filerec(f).mode of
       fminput : filerec(f).handle:=StdInputHandle;
      fmappend,
      fmoutput : begin
                   filerec(f).handle:=StdOutputHandle;
                   filerec(f).mode:=fmoutput; {fool fmappend}
                 end;
     end;
     exit;
   end;
         asm
             move.l  a6,d6                  { save a6 }
             move.l  d2,-(sp)
             lea     buffer,a0
             move.l  a0,d1
             move.l  oflags,d2               { MODE_READWRITE }
             move.l  _DOSBase,a6
             jsr     _LVOOpen(a6)
             tst.l   d0
             bne     @noopenerror           { on zero an error occured }
             jsr     _LVOIoErr(a6)
             move.w  d0,errno
             bra     @openend
          @noopenerror:
             move.l  (sp)+,d2
             move.l  d6,a6                 { restore a6 }
             move.l  d0,i                  { we need the base pointer to access this variable }
             bra     @end
          @openend:
             move.l  d6,a6                 { restore a6 }
             move.l  (sp)+,d2
          @end:
         end;
(*    if Errno = 0 then*)
    { No error, add file handle to linked list }
    { this must be checked before the call to  }
    { Error2InIOut since it resets Errno to 0  }
(*      AddToList(FileList,i);*)
    If Errno <> 0 then
       Error2InOut;

    filerec(f).handle:=i;
    if (flags and $100)<>0 then
       do_seekend(filerec(f).handle);

end;

{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure mkdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  j: Integer;
  temp : string;
begin
  { We must check the Ctrl-C before IOChecking of course! }
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  If InOutRes <> 0 then exit;
  temp:=s;
  for j:=1 to length(temp) do
    if temp[j] = '\' then temp[j] := '/';
  move(temp[1],buffer,length(temp));
  buffer[length(temp)]:=#0;
  asm
        move.l  a6,d6
        { we must load the parameters BEFORE setting up the }
        { OS call with a6                                   }
        lea     buffer,a0
        move.l  a0,d1
        move.l  _DosBase,a6
        jsr     _LVOCreateDir(a6)
        tst.l   d0
        bne     @noerror
        jsr     _LVOIoErr(a6)
        move.w  d0,errno
        bra     @end
@noerror:
        { Now we must unlock the directory }
        { d0 = lock returned by create dir }
        move.l  d0,d1
        jsr     _LVOUnlock(a6)
@end:
        { restore base pointer }
        move.l  d6,a6
  end;
  If errno <> 0 then
    Error2InOut;
end;


procedure rmdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  j : Integer;
  temp : string;
begin
  { We must check the Ctrl-C before IOChecking of course! }
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  If InOutRes <> 0 then exit;
  temp:=s;
  for j:=1 to length(temp) do
    if temp[j] = '\' then temp[j] := '/';
  move(temp[1],buffer,length(temp));
  buffer[length(temp)]:=#0;
  do_erase(buffer);
end;



procedure chdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  alock : longint;
  FIB :pFileInfoBlock;
  j: integer;
  temp : string;
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
   Begin
     { Clear CTRL-C signal }
     SetSignal(0,SIGBREAKF_CTRL_C);
     Halt(CTRL_C);
   end;
  If InOutRes <> 0 then exit;
  temp:=s;
  for j:=1 to length(temp) do
    if temp[j] = '\' then temp[j] := '/';
  { Return parent directory }
  if s = '..' then
  Begin
       getdir(0,temp);
       j:=length(temp);
       { Look through the previous paths }
       while (temp[j] <> '/') AND (temp[j] <> ':') AND (j > 0 ) do
         dec(j);
       if j > 0 then
         temp:=copy(temp,1,j);
  end;
  alock := 0;
  fib:=nil;
  new(fib);

  move(temp[1],buffer,length(temp));
  buffer[length(temp)]:=#0;
  { Changing the directory is a pretty complicated affair }
  {   1) Obtain a lock on the directory                   }
  {   2) CurrentDir the lock                              }
  asm
    lea      buffer,a0
    move.l   a0,d1      { pointer to buffer in d1  }
    move.l   d2,-(sp)   { save d2 register         }
    move.l   #-2,d2     { ACCESS_READ lock         }
    move.l   a6,d6      { Save base pointer        }
    move.l   _DosBase,a6
    jsr      _LVOLock(a6){ Lock the directory      }
    move.l   (sp)+,d2   { Restore d2 register      }
    tst.l    d0         { zero = error!            }
    bne      @noerror
    jsr      _LVOIoErr(a6)
    move.w   d0,errno
    move.l   d6,a6       { reset base pointer       }
    bra      @End
  @noerror:
    move.l   d6,a6       { reset base pointer       }
    move.l   d0,alock    { save the lock            }
  @End:
  end;
  If errno <> 0 then
   Begin
     Error2InOut;
     exit;
   end;
  if (Examine(alock, fib^) = TRUE) AND (fib^.fib_DirEntryType > 0) then
    Begin
      alock := CurrentDir(alock);
      if OrigDir = 0 then
        Begin
          OrigDir := alock;
          alock := 0;
        end;
    end;
  if alock <> 0 then
    Unlock(alock);
  if assigned(fib) then dispose(fib);
end;




  Procedure GetCwd(var path: string);
   var
     lock: longint;
     fib: PfileInfoBlock;
     len : integer;
     newlock : longint;
     elen : integer;
     Process : PProcess;
    Begin
     len := 0;
     path := '';
     fib := nil;
     { By using a pointer instead of a local variable}
     { we are assured that the pointer is aligned on }
     { a dword boundary.                             }
     new(fib);
     Process := FindTask(nil);
     if (process^.pr_Task.tc_Node.ln_Type = NT_TASK) then
       Begin
         path:='';
         exit;
       end;
     lock := DupLock(process^.pr_CurrentDir);
     if (Lock = 0) then
       Begin
         path:='';
         exit;
       end;

    While (lock <> 0) and (Examine(lock,FIB^) = TRUE) do
    Begin
         elen := strlen(fib^.fib_FileName);
         if (len + elen + 2 > 255) then
            break;
         newlock := ParentDir(lock);
         if (len <> 0) then
          Begin
            if (newlock <> 0) then
               path:='/'+path
            else
               path:=':'+path;
            path:=strpas(fib^.fib_FileName)+path;
            Inc(len);
          end
         else
          Begin
            path:=strpas(fib^.fib_Filename);
            if (newlock = 0) then
             path:=path+':';
          end;

               len := len + elen;

               UnLock(lock);
               lock := newlock;
    end;
    if (lock <> 0) then
    Begin
            UnLock(lock);
            path := '';
    end;
    if assigned(fib) then dispose(fib);
 end;


procedure getdir(drivenr : byte;var dir : string);
begin
  if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then
    Begin
      { Clear CTRL-C signal }
      SetSignal(0,SIGBREAKF_CTRL_C);
      Halt(CTRL_C);
    end;
  GetCwd(dir);
  If errno <> 0 then
     Error2InOut;
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Procedure Startup; Assembler;
asm
    move.l  a6,d6         { save a6             }

    move.l  (4),a6        { get ExecBase pointer }
    move.l  a6,_ExecBase
    suba.l  a1,a1
    jsr     _LVOFindTask(a6)
    move.l  d0,a0
    { Check the stack value }

    {   are we running from a CLI?             }

    tst.l   172(a0)         { 172 = pr_CLI     }
    bne     @fromCLI

    { we do not support Workbench yet ..       }
    move.l  d6,a6           { restore a6       }
    move.l  #1,d0
    jsr     HALT_ERROR

@fromCLI:
    {  Open the following libraries:            }
    {   Intuition.library                       }
    {   dos.library                             }

    moveq.l  #0,d0
    move.l   intuitionname,a1      { directly since it is a pchar }
    jsr      _LVOOpenLibrary(a6)
    move.l   d0,_IntuitionBase
    beq      @exitprg

    moveq.l  #0,d0
    move.l   utilityname,a1        { directly since it is a pchar }
    jsr      _LVOOpenLibrary(a6)
    move.l   d0,_UtilityBase
    beq      @exitprg

    moveq.l  #0,d0
    move.l   dosname,a1            { directly since it is a pchar }
    jsr      _LVOOpenLibrary(a6)
    move.l   d0,_DOSBase
    beq      @exitprg

    { Find standard input and output               }
    { for CLI                                      }
@OpenFiles:
    move.l  _DOSBase,a6
    jsr     _LVOInput(a6)        { get standard in                   }
    move.l  d0, StdInputHandle   { save standard Input handle        }
{    move.l  d0,d1               }{ set up for next call              }
{   jsr     _LVOIsInteractive(a6)}{ is it interactive?             }
{   move.l  #_Input,a0          }{ get file record again             }
{   move.b  d0,INTERACTIVE(a0)  }{ set flag                          }
{   beq     StdInNotInteractive }{ skip this if not interactive    }
{   move.l  BUFFER(a0),a1       }{ get buffer address                }
{   add.l   #1,a1               }{ make end one byte further on      }
{   move.l  a1,MAX(a0)          }{ set buffer size                   }
{   move.l  a1,CURRENT(a0)      }{ will need a read                  }
    bra     @OpenStdOutput
@StdInNotInteractive
{    jsr _p%FillBuffer     }      { fill the buffer                   }
@OpenStdOutput
    jsr     _LVOOutput(a6)      { get ouput file handle             }
    move.l  d0,StdOutputHandle  { get file record                   }
    bra     @startupend
{    move.l  d0,d1             }  { set up for call                   }
{    jsr _LVOIsInteractive(a6) }  { is it interactive?                }
{    move.l  #_Output,a0       }  { get file record                   }
{    move.b  d0,INTERACTIVE(a0)}  { set flag                          }
@exitprg:
     move.l d6,a6                 { restore a6                        }
     move.l #219,d0
     jsr    HALT_ERROR
@startupend:
     move.l d6,a6                 { restore a6                        }
end;



begin
  errno:= 0;
  FromHalt := FALSE;
{  Initial state is on -- in case of RunErrors before the i/o handles are }
{  ok.                                                                    }
  Initial:=TRUE;
{ Initialize ExitProc }
  ExitProc:=Nil;
  Startup;
{ to test stack depth }
  loweststack:=maxlongint;
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  { The Amiga does not seem to have a StdError }
  { handle, therefore make the StdError handle }
  { equal to the StdOutputHandle.              }
  StdErrorHandle := StdOutputHandle;
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Now Handles and function handlers are setup }
{ correctly.                                  }
  Initial:=FALSE;
{ Reset IO Error }
  InOutRes:=0;
{ Startup }
  { Only AmigaOS v2.04 or greater is supported }
  If KickVersion < 36 then
   Begin
     WriteLn('v36 or greater of Kickstart required.');
     Halt(1);
   end;
   argc:=GetParamCount(args);
   OrigDir := 0;
   FileList := nil;
end.


{
  $Log$
  Revision 1.4  2002-09-07 16:01:16  peter
    * old logs removed and tabs fixed

}
