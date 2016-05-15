{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level file functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode motorola}

{****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

procedure DoDirSeparators(p:pchar);
var
  i : longint;
begin
{ allow slash as backslash }
  for i:=0 to strlen(p) do
   if p[i] in AllowDirectorySeparators then p[i]:=DirectorySeparator;
end;


procedure do_close(h : longint);
begin
  asm
        movem.l d2/d3/a2/a3,-(sp)
        move.l  h,d0
        move.w  d0,-(sp)
        move.w  #$3e,-(sp)
        trap    #1
        add.l   #4,sp      { restore stack ... }
        movem.l (sp)+,d2/d3/a2/a3
  end;
end;


procedure do_erase(p : pchar);
begin
  DoDirSeparators(p);
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
        move.w d0,errno
        @doserend:
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure do_rename(p1,p2 : pchar);
begin
  DoDirSeparators(p1);
  DoDirSeparators(p2);
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
            move.w  d0,errno    { error ... }
         @dosreend:
  end;
  if errno <> 0 then
     Error2InOut;
end;

function do_isdevice(handle:word):boolean;
begin
  if (handle=stdoutputhandle) or (handle=stdinputhandle) or
  (handle=stderrorhandle) then
    do_isdevice:=FALSE
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
            move.l  h,d0
            move.w  d0,-(sp)
            move.w  #$40,-(sp)
            trap    #1
            lea     12(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.l   d0
            bpl     @doswrend
            move.w  d0,errno    { error ... }
          @doswrend:
            move.l  d0,@RESULT
  end;
  if errno <> 0 then
     Error2InOut;
end;


function do_read(h,addr,len : longint) : longint;
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.l addr,-(sp)
            move.l len,-(sp)
            move.l h,d0
            move.w d0,-(sp)
            move.w #$3f,-(sp)
            trap   #1
            lea    12(sp),sp
            move.l d6,d2       { restore d2 }
            movem.l (sp)+,d3/a2/a3
            tst.l   d0
            bpl     @dosrdend
            move.w  d0,errno    { error ... }
          @dosrdend:
            move.l  d0,@Result
  end;
  if errno <> 0 then
     Error2InOut;
end;


function do_filepos(handle : longint) : longint;
begin
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)
            move.w #1,-(sp)     { seek from current position }
            move.l handle,d0
            move.w d0,-(sp)
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
            move.l handle,d0
            move.w d0,-(sp)
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
            move.l handle,d0
            move.w d0,-(sp)
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
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  i : word;
  oflags: longint;
begin
  DoDirSeparators(p);
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
  oflags:=$02; { read/write mode }
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
         oflags:=$00; { read mode only }
       end;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  if (flags and $1000)<>0 then
   begin
     filerec(f).mode:=fmoutput;
     oflags:=$04;  { read/write with create }
   end
  else
   if (flags and $100)<>0 then
    begin
      filerec(f).mode:=fmoutput;
      oflags:=$02;  { read/write             }
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

      cmp.l   #4,oflags    { check if rewrite mode ... }
      bne     @opencont2
      { rewrite mode - create new file }
      move.w  #0,-(sp)
      move.l  p,-(sp)
      move.w  #$3c,-(sp)
      trap    #1
      add.l   #8,sp       { restore stack of os call }
      bra     @end
      { reset - open existing files     }
    @opencont2:
      move.l  oflags,d0    { use flag as source  ...    }
    @opencont1:
      move.w  d0,-(sp)
      move.l  p,-(sp)
      move.w  #$3d,-(sp)
      trap    #1
      add.l   #8,sp       { restore stack of os call }
   @end:
      movem.l (sp)+,d2/d3/a2/a3

      tst.w   d0
      bpl     @opennoerr  { if positive return values then ok }
      cmp.w   #-1,d0      { if handle is -1 CON:              }
      beq     @opennoerr
      cmp.w   #-2,d0      { if handle is -2 AUX:              }
      beq     @opennoerr
      cmp.w   #-3,d0      { if handle is -3 PRN:              }
      beq     @opennoerr
      move.w  d0,errno    { otherwise normal error            }
    @opennoerr:
      move.w  d0,i        { get handle as SIGNED VALUE...     }
    end;
  if errno <> 0 then
    begin
     Error2InOut;
     FileRec(f).mode:=fmclosed;
    end;
  filerec(f).handle:=i;
  if ((flags and $100) <> 0) and
       (FileRec (F).Handle <> UnusedHandle) then
   do_seekend(filerec(f).handle);
end;
