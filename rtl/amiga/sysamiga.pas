{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.
    Some parts taken from
       Marcel Timmermans - Modula 2 Compiler
       Nils Sjoholm - Amiga porter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysamiga;

{ Things left to do :                                          }
{   - Fix randomize                                            }
{   - Fix DOSError result variable to conform to IOResult of   }
{      Turbo Pascal                                            }

{$I os.inc}

  interface

    { used for single computations }
    const BIAS4 = $7f-1;

    {$I systemh.inc}

    {$I heaph.inc}

const
  UnusedHandle    : longint = -1; 
  StdInputHandle  : longint = 0;
  StdOutputHandle : longint = 0;
  StdErrorHandle  : longint = 0; 

 _ExecBase:longint = $4;
 _WorkbenchMsg : longint = 0;
 intuitionname : pchar = 'intuition.library';
 dosname : pchar = 'dos.library';
 utilityname : pchar = 'utility.library';

 _IntuitionBase : pointer = nil;       { intuition library pointer }
 _DosBase       : pointer = nil;       { DOS library pointer       }
 _UtilityBase   : pointer = nil;       { utiity library pointer    }

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


  implementation

    var
      Initial: boolean;

    {$I system.inc}
    {$I lowmath.inc}

    type
       plongint = ^longint;


{$S-}
    PROCEDURE St1(stack_size: longint);[public,alias: 'STACKCHECK'];
    begin
     asm
         { called when trying to get local stack }
         { if the compiler directive $S is set   }
         { it must preserve all registers !!     }
        move.l  stack_size, d0
        add.l   sp,d0     { stacksize + actual stackpointer  }
        move.l  _ExecBase,a0
        move.l  276(A0),A0       { ExecBase.thisTask }
        cmp.l   58(A0),D0        { Task.SpLower      }
        bgt     @Ok
        move.l  #202,d0
        jsr     HALT_ERROR       { stack overflow    }
    @Ok:
     end;
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
     move.l  20(a0), d0          { Return version - version at this offset }
   end;

    procedure halt(errnum : byte);
      begin
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
         Begin
           CloseLibrary(_UtilityBase);
         end;
         If _DosBase <> nil then
         Begin
           CloseLibrary(_DosBase);
         end;
         If _IntuitionBase <> nil then
         Begin
           CloseLibrary(_IntuitionBase);
         end;
         asm
            clr.l   d0
            move.b  errnum,d0
            move.l  STKPTR,sp
            rts
         end;
      end;

    function paramcount : longint; assembler;
    asm
            clr.l   d0
            move.w  __ARGC,d0
            sub.w   #1,d0
    end;

    function paramstr(l : longint) : string;

      function args : pointer; assembler;
      asm
         move.l __ARGS,d0
      end;

      var
         p : ^pchar;

      begin
         if (l>=0) and (l<=paramcount) then
           begin
              p:=args;
              paramstr:=strpas(p[l]);
           end
         else paramstr:='';
      end;

    procedure randomize;

      var
         hl : longint;

      begin
         asm
           { !!!!!!! }
         end;
         randseed:=hl;
      end;

  { This routine is used to grow the heap.  }
  { But here we do a trick, we say that the }
  { heap cannot be regrown!                 }
  function sbrk( size: longint): longint;
  { on exit -1 = if fails.               }
  Begin
   sbrk:=-1;
  end;



{$I heap.inc}


{****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

procedure do_close(h : longint);
begin
  asm
            move.l  h,d1
            move.l  a6,d6              { save a6 }
            move.l  _DOSBase,a6
            jsr     _LVOClose(a6)
            move.l  d6,a6              { restore a6 }
  end;
end;


procedure do_erase(p : pchar);
begin
  asm
           move.l  a6,d6               { save a6 }

           move.l  p,d1
           move.l  _DOSBase,a6
           jsr     _LVODeleteFile(a6)
           tst.l   d0                  { zero = failure }
           bne     @noerror

           jsr     _LVOIoErr(a6)
           move.l  d0,InOutRes

         @noerror:
           move.l  d6,a6               { restore a6 }
  end;
end;


procedure do_rename(p1,p2 : pchar);
begin
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
           move.l  d0,InOutRes
         @dosreend:
           move.l  d6,a6                  { restore a6 }
  end;
end;


function do_write(h,addr,len : longint) : longint;
begin
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

            tst.l   d0
            bne     @doswrend              { if zero = error }
            jsr     _LVOIoErr(a6)
            move.l  d0,InOutRes
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
end;


function do_read(h,addr,len : longint) : longint;
begin
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

            tst.l   d0
            bne     @doswrend              { if zero = error }
            jsr     _LVOIoErr(a6)
            move.l  d0,InOutRes
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
end;


function do_filepos(handle : longint) : longint;
begin
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
             move.l  d0,InOutRes
             bra     @fposend
      @noerr:
             move.l  d6,a6                 { restore a6 }
             move.l  d0,@Result
             bra     @end
      @fposend:
             move.l  d6,a6                 { restore a6 }
      @end:
  end;
end;


procedure do_seek(handle,pos : longint);
begin
  asm
             move.l  a6,d6

             move.l  handle,d1
             move.l  d2,-(sp)
             move.l  d3,-(sp)              { save registers              }

             move.l  pos,d2
             move.l  #-1,d3                 { OFFSET_BEGINNING }
             move.l  _DOSBase,a6
             jsr    _LVOSeek(a6)

             move.l  (sp)+,d3              { restore registers }
             move.l  (sp)+,d2
             cmp.l   #-1,d0                { is there a file access error? }
             bne     @noerr
             jsr     _LVOIoErr(a6)
             move.l  d0,InOutRes
             bra     @seekend
      @noerr:
      @seekend:
             move.l  d6,a6                 { restore a6 }
  end;
end;


function do_seekend(handle:longint):longint;
begin
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
             move.l  d0,InOutRes
             bra     @seekend
      @noerr:
             move.l  d6,a6                 { restore a6 }
             move.l  d0,@Result
             bra     @end
      @seekend:
             move.l  d6,a6                 { restore a6 }
      @end:
  end;
end;


function do_filesize(handle : longint) : longint;
var
  aktfilepos : longint;
begin
   aktfilepos:=do_filepos(handle);
   do_filesize:=do_seekend(handle);
   do_seek(handle,aktfilepos);
end;


procedure do_truncate (handle,pos:longint);
begin
  {!!!!!!!!!!!!}
end;


procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  i : longint;
  oflags: longint;
begin
 { close first if opened }
  if ((flags and $1000)=0) then
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
  oflags:=$04;
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
         oflags:=$01;
       end;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  if (flags and $100)<>0 then
   begin
     filerec(f).mode:=fmoutput;
     oflags:=$02;
   end
  else
   if (flags and $10)<>0 then
    begin
      filerec(f).mode:=fmoutput;
      oflags:=$04;
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
    { THE AMIGA AUTOMATICALLY OPENS IN READ-WRITE MODE }
    { FOR ALL CASES.                                   }
         asm
             move.l  a6,d6                  { save a6 }

             move.l  f,d1
             move.l  #1004,d0               { MODE_READWRITE }
             move.l  _DOSBase,a6
             jsr     _LVOOpen(a6)
             tst.l   d0
             bne     @noopenerror           { on zero an error occured }
             jsr     _LVOIoErr(a6)
             move.l  d0,InOutRes
             bra     @openend
          @noopenerror:
             move.l  d6,a6                 { restore a6 }
             move.l  d0,i                  { we need the base pointer to access this variable }
             bra     @end
          @openend:
             move.l  d6,a6                 { restore a6 }
          @end:
         end;
    filerec(f).handle:=i;
    if (flags and $10)<>0 then
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
      
procedure mkdir(const s : string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
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
        move.l  #1,InOutRes
@noerror:
        move.l  d6,a6
  end;
end;


procedure rmdir(const s : string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  do_erase(buffer);
end;
  

procedure chdir(const s : string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  asm
        move.l  a6,d6
        lea     buffer,a1
        move.l  a1,d1
        move.l  _DosBase,a6
        jsr     _LVOSetCurrentDirName(a6)
        bne     @noerror
        move.l  #1,InOutRes
@noerror:
        move.l  d6,a6
  end;
end;


procedure getdir(drivenr : byte;var dir : string);
var
  l : longint;
  p : pointer;
begin
  l:=length(dir);
  if drivenr <> 0 then
   begin
     dir:='';
     exit;
   end;
  p:=@dir[1];
  if l <> 0 then         { workaround for v36 bug }
   Begin
     asm
        move.l  a6,d6
        move.l  p,d1
        move.l  l,d2
        move.l  _DosBase,a6
        jsr     _LVOGetCurrentDirName(a6)
        bne     @noerror
        move.l  #1,InOutRes
      @noerror:
        move.l  d6,a6
     end;
   end
  else
   dir:='';
{ upcase the string (FPKPascal function) }
  dir:=upcase(dir);
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


procedure OpenStdIO(var f:text;mode:word;hdl:longint);
begin
  Assign(f,'');
  TextRec(f).Handle:=hdl;
  TextRec(f).Mode:=mode;
  TextRec(f).InOutFunc:=@FileInOutFunc;
  TextRec(f).FlushFunc:=@FileInOutFunc;
  TextRec(f).Closefunc:=@fileclosefunc;
end;


begin
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
end.


{
  $Log$
  Revision 1.2  1998-05-25 12:08:49  carl
     * Handles now proprely setup
     * Correct Exit code on init failure
     * Library pointer now ok (Thanks to Nils Sjoholm)
     * OpenStdError was never initialized
     * ;assembler; routines problems bugfixed
     * stackcheck routine fix

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.14  1998/03/21 04:20:09  carl
    * correct ExecBase pointer (from Nils Sjoholm)
    * correct OpenLibrary vector (from Nils Sjoholm)

  Revision 1.13  1998/03/14 21:34:32  carl
    * forgot to save a6 in Startup routine

  Revision 1.12  1998/02/24 21:19:42  carl
  *** empty log message ***

  Revision 1.11  1998/02/23 02:22:49  carl
    * bugfix if linking problems

  Revision 1.9  1998/02/06 16:34:32  carl
    + do_open is now standard with other platforms

  Revision 1.8  1998/02/02 15:01:45  carl
    * fixed bug with opening library versions (from Nils Sjoholm)

  Revision 1.7  1998/01/31 19:35:19  carl
    + added opening of utility.library

  Revision 1.6  1998/01/29 23:20:54  peter
    - Removed Backslash convert

  Revision 1.5  1998/01/27 10:55:04  peter
    * Amiga uses / not \, so change AllowSlash -> AllowBackSlash

  Revision 1.4  1998/01/25 21:53:20  peter
    + Universal Handles support for StdIn/StdOut/StdErr
    * Updated layout of sysamiga.pas

  Revision 1.3  1998/01/24 21:09:53  carl
    + added missing input/output function pointers

  Revision 1.2  1998/01/24 14:08:25  carl
    * RunError 217 --> RunError 219 (cannot open lib)
    + Standard Handle names implemented

  Revision 1.1  1998/01/24 05:12:15  carl
    + initial revision, some stuff still missing though.
      (and as you might imagine ... untested :))
}
