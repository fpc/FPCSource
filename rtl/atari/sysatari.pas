{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$define ATARI}
unit sysatari;


{ Left to do :                                                    }
{    - Fix DOSError codes to conform to those of DOS (TP)         }

{$I os.inc}

  interface

    { used for single computations }
    const BIAS4 = $7f-1;

    {$I systemh.inc}

    {$I heaph.inc}

const
  UnusedHandle    = $ffff; 
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = $ffff; 

  implementation

    {$I system.inc}
    {$I lowmath.inc}

    type
       plongint = ^longint;

{$S-}
    procedure Stack_Check; assembler;
    { Check for local variable allocation }
    { On Entry -> d0 : size of local stack we are trying to allocate }
         asm
          XDEF STACKCHECK
           move.l  sp,d1            { get value of stack pointer            }
           sub.l   d0,d1            {  sp - stack_size                      }
           cmp.l    __BREAK,d1
           bgt      @st1nosweat
           move.l   #202,d0
           jsr      HALT_ERROR
         @st1nosweat:
         end;


    procedure halt(errnum : byte);

      begin
         do_exit;
         flush(stderr);
         asm
            clr.l   d0
            move.b  errnum,d0
            move.w  d0,-(sp)
            move.w  #$4c,-(sp)
            trap    #1
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
           movem.l d2/d3/a2/a3, -(sp)     { save OS registers }
           move.w #17,-(sp)
           trap   #14         { call xbios - random number }
           add.l  #2,sp
           movem.l (sp)+,d2/d3/a2/a3
           move.l d0,hl       { result in d0 }
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

procedure AllowSlash(p:pchar);
var
  i : longint;
begin
{ allow slash as backslash }
  for i:=0 to strlen(p) do
   if p[i]='/' then p[i]:='\';
end;


procedure do_close(h : longint);
begin
  asm
        movem.l d2/d3/a2/a3,-(sp)
        move.l  h,-(sp)
        move.w  #$3e,-(sp)
        trap    #1
        add.l   #4,sp      { restore stack ... }
        movem.l (sp)+,d2/d3/a2/a3
  end;
end;


procedure do_erase(p : pchar);
begin
  AllowSlash(p);
  asm
        move.l  d2,d6            { save d2   }
        movem.l d3/a2/a3,-(sp)   { save regs }
        move.l  p,-(sp)
        move.w #$41,-(sp)
        trap   #1
        add.l  #6,sp
        move.l d6,d2       { restore d2 }
        movem.l (sp)+,d3/a2/a3
        tst.w  d0
        beq    @doserend
        move.w d0,InOutRes
        @doserend:
  end;
end;


procedure do_rename(p1,p2 : pchar);
begin
  AllowSlash(p1);
  AllowSlash(p2);
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.l  p1,-(sp)
            move.l  p2,-(sp)
            clr.w   -(sp)
            move.w  #$56,-(sp)
            trap    #1
            lea     12(sp),sp
            move.l  d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.w   d0
            beq     @dosreend
            move.w  d0,InOutRes    { error ... }
         @dosreend:
  end;
end;

function do_isdevice(handle:longint):boolean;
begin
  if (handle=stdoutputhandle) or (handle=stdinputhandle) or
  (handle=stderrorhandle) then
    do_isdevice:=FALSE;
  else
    do_isdevice:=TRUE;
end;


function do_write(h,addr,len : longint) : longint;
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.l  addr,-(sp)
            move.l  len,-(sp)
            move.w  h,-(sp)
            move.w  #$40,-(sp)
            trap    #1
            lea     12(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.l   d0
            bpl     @doswrend
            move.w  d0,InOutRes    { error ... }
          @doswrend:
            move.l  d0,@RESULT
  end;
end;


function do_read(h,addr,len : longint) : longint;
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.l addr,-(sp)
            move.l len,-(sp)
            move.w h,-(sp)
            move.w #$40,-(sp)
            trap   #1
            lea    12(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.l   d0
            bpl     @dosrdend
            move.w  d0,InOutRes    { error ... }
          @dosrdend:
            move.l  d0,@Result
  end;
end;


function do_filepos(handle : longint) : longint;
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.w #1,-(sp)     { seek from current position }
            move.w handle,-(sp)
            move.l #0,-(sp)     { with a seek offset of zero }
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            move.l d0,@Result
  end;
end;


procedure do_seek(handle,pos : longint);
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.w #0,-(sp)     { seek from start of file    }
            move.w handle,-(sp)
            move.l pos,-(sp)
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
  end;
end;


function do_seekend(handle:longint):longint;
var
 t: longint;
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.w #2,-(sp)     { seek from end of file        }
            move.w handle,-(sp)
            move.l #0,-(sp)     { with an offset of 0 from end }
            move.w #$42,-(sp)
            trap   #1
            lea    10(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            move.l d0,t
  end;
   do_seekend:=t;
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
  do_seek(handle,pos);
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
  AllowSlash(p);
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
   asm
      movem.l d2/d3/a2/a3,-(sp)    { save used registers }

      cmp.l   #4,oflags    { check if append mode ... }
      bne     @opencont2
      move.w  #2,d0        { append mode... r/w open   }
      bra     @opencont1
    @opencont2:
      move.l  oflags,d0    { use flag as source  ...    }
    @opencont1:
      move.w  d0,-(sp)
      pea     p
      move.w  #$3d,-(sp)
      trap    #1
      add.l   #8,sp       { restore stack of os call }

      movem.l (sp)+,d2/d3/a2/a3

      tst.l   d0
      bpl     @opennoerr
      move.w  d0,InOutRes
    @opennoerr:
      move.l  d0,i        { get handle ... }
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

procedure DosDir(func:byte;const s:string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  AllowSlash(pchar(@buffer));
  asm
        move.l  d2,d6      { save d2 }
        movem.l d3/a2/a3,-(sp)
        pea     buffer
        move.b  func,-(sp)
        trap    #1
        add.l   #6,sp
        move.l  d6,d2       { restore d2 }
        movem.l (sp)+,d3/a2/a3
        tst.w   d0
        beq     @dosdirend
        move.w  d0,InOutRes
     @dosdirend:
  end;
end;


procedure mkdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then exit;
  DosDir($39,s);
end;


procedure rmdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then exit;
  DosDir($3a,s);
end;


procedure chdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then exit;
  DosDir($3b,s);
end;


procedure getdir(drivenr : byte;var dir : string);
var
  temp : array[0..255] of char;
  sof  : pchar;
  i    : longint;
begin
  sof:=pchar(@dir[4]);
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)

            { Get dir from drivenr : 0=default, 1=A etc... }
            move.w drivenr,-(sp)

            { put (previously saved) offset in si }
            pea    dir

            { call attos function 47H : Get dir }
            move.w #$47,-(sp)

            { make the call }
            trap   #1
            add.l  #8,sp

            move.l d6,d2         { restore d2 }
            movem.l (sp)+,d3/a2/a3
  end;
{ Now Dir should be filled with directory in ASCIIZ, }
{ starting from dir[4]                               }
  dir[0]:=#3;
  dir[2]:=':';
  dir[3]:='\';
  i:=4;
{ conversation to Pascal string }
  while (dir[i]<>#0) do
   begin
   { convert path name to DOS }
     if dir[i]='/' then
      dir[i]:='\';
     dir[0]:=chr(i);
     inc(i);
   end;
{ upcase the string (FPKPascal function) }
  dir:=upcase(dir);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=chr(65+drivenr-1)
  else
   begin
      asm
        move.l  d2,d6      { save d2 }
        movem.l d3/a2/a3,-(sp)
        move.w #$19,-(sp)
        trap   #1
        add.l  #2,sp
        move.l d6,d2        { restore d2 }
        movem.l (sp)+,d3/a2/a3
     end;
     dir[1]:=chr(i);
   end;
end;

      
{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}
      

begin
{ Initialize ExitProc }
  ExitProc:=Nil;
{ to test stack depth }
  loweststack:=maxlongint;
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
end.

{
  $Log$
  Revision 1.4  1998-07-02 12:39:27  carl
    * IOCheck for mkdir,chdir and rmdir, just like in TP

  Revision 1.3  1998/07/01 14:40:20  carl
    + new stack checking implemented
    + IOCheck for chdir , getdir , mkdir and rmdir

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.8  1998/02/23 02:27:39  carl
    * make it link correctly

  Revision 1.7  1998/02/06 16:33:02  carl
    * oops... commited wrong file
    + do_open is now standard with other platforms

  Revision 1.5  1998/01/31 19:32:51  carl
    - removed incorrect $define

  Revision 1.4  1998/01/27 10:55:45  peter
    * Word Handles from -1 -> $ffff

  Revision 1.3  1998/01/25 22:44:14  peter
    * Using uniform layout

}
