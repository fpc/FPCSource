{
    This file is part of the Free Pascal run time library.

    Main OS dependant body of the system unit, loosely modelled
    after POSIX.  *BSD version (Linux version is near identical)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Procedure Do_Close(Handle:thandle);
var
  res: cint;
Begin
  repeat
    res:=Fpclose(cint(Handle));
  until (res<>-1) or (geterrno<>ESysEINTR);
  if res<>0 then
    Errno2Inoutres;
End;

Procedure Do_Erase(p: pchar; pchangeable: boolean);
var
 fileinfo : stat;
Begin
  { verify if the filename is actually a directory }
  { if so return error and do nothing, as defined  }
  { by POSIX                                       }
  if Fpstat(p,fileinfo)<0 then
   begin
     Errno2Inoutres;
     exit;
   end;
  if FpS_ISDIR(fileinfo.st_mode) then
   begin
     InOutRes := 2;
     exit;
   end;
  if Fpunlink(p)<0 then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;

{ truncate at a given position }
procedure do_truncate (handle:thandle;fpos:longint);
begin
  { should be simulated in cases where it is not }
  { available.                                   }
  If Fpftruncate(handle,fpos)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
end;



Procedure Do_Rename(p1,p2:pchar; p1changeable, p2changeable: boolean);
Begin
  If Fprename(p1,p2)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Function Do_Write(Handle:thandle;Addr:Pointer;Len:Longint):longint;

var j : cint;
Begin
  repeat
    Do_Write:=Fpwrite(Handle,addr,len);
    j:=geterrno;
  until (do_write<>-1) or ((j<>ESysEINTR) and (j<>ESysEAgain));
  If Do_Write<0 Then
   Begin
    Errno2InOutRes;
    Do_Write:=0;
   End
  else
   InOutRes:=0;
End;


Function Do_Read(Handle:thandle;Addr:Pointer;Len:Longint):Longint;

var j:cint;

Begin
  repeat
    Do_Read:=Fpread(Handle,addr,len);
    j:=geterrno;
  until (do_read<>-1) or ((j<>ESysEINTR) and (j<>ESysEAgain));
  If Do_Read<0 Then
   Begin
    Errno2InOutRes;
    Do_Read:=0;
   End
  else
   InOutRes:=0;
End;

function Do_FilePos(Handle: thandle):Int64;
Begin
  do_FilePos:=Fplseek(Handle, 0, SEEK_CUR);
  If Do_FilePos<0 Then
    Errno2InOutRes
  else
   InOutRes:=0;
End;

Procedure Do_Seek(Handle:thandle;Pos:Int64);
Begin
  If Fplseek(Handle, pos, SEEK_SET)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;

Function Do_SeekEnd(Handle:thandle):Int64;
begin
  Do_SeekEnd:=Fplseek(Handle,0,SEEK_END);
  If Do_SeekEnd<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
end;

Function Do_FileSize(Handle:thandle):Int64;
var
  Info : Stat;
  Ret  : Longint;
Begin
  Ret:=Fpfstat(handle,info);
  If Ret=0 Then
   Do_FileSize:=Info.st_size
  else
   Do_FileSize:=0;
  If Ret<0 Then
   Errno2InOutRes
  Else
   InOutRes:=0;
End;

Procedure Do_Open(var f; p: pchar; flags: longint; pchangeable: boolean);
{
  FileRec and textrec have both Handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
const
  { read/write permission for everyone }
  MODE_OPEN = S_IWUSR OR S_IRUSR OR
              S_IWGRP OR S_IRGRP OR
              S_IWOTH OR S_IROTH;
var
  oflags : cint;
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;
{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=O_RDONLY;
         FileRec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=O_WRONLY;
         FileRec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=O_RDWR;
         FileRec(f).mode:=fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);
{ empty name is special }
  if p[0]=#0 then
   begin
     case FileRec(f).mode of
       fminput :
         FileRec(f).Handle:=StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle:=StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle:=StdOutputHandle;
           FileRec(f).mode:=fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
  repeat
    FileRec(f).Handle:=Fpopen(p,oflags,MODE_OPEN);
  until (FileRec(f).Handle<>-1) or (geterrno<>ESysEINTR);
  if (FileRec(f).Handle<0) and
    (getErrNo=ESysEROFS) and ((OFlags and O_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(O_RDWR);
     repeat
       FileRec(f).Handle:=Fpopen(p,oflags,MODE_OPEN);
     until (FileRec(f).Handle<>-1) or (geterrno<>ESysEINTR);
   end;
  If Filerec(f).Handle<0 Then
   Errno2Inoutres
  else
   InOutRes:=0;
End;


