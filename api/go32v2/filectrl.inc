{
  System independent filecontrol interface for go32v2

  $Id$
}
uses
  Go32;

function OpenFileStr(FName: PChar; Flags: Longint): TFileHandle;
Var
  regs : trealregs;
begin
  copytodos(FName^,256);
  if LFNSupport then
   regs.realeax:=$716c
  else
   regs.realeax:=$6c00;
  regs.realedx:=$1;
  regs.realds:=tb_segment;
  regs.realesi:=tb_offset;
  regs.realebx:=$2000;
  regs.realecx:=$20;
  realintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   begin
     ErrorCode:=lo(regs.realeax);
     exit(0);
   end
  else
   OpenFileStr:=regs.realeax and $ffff;
end;


function CreateFileStr(FName: PChar): TFileHandle;
Var
  regs : trealregs;
begin
  copytodos(FName^,256);
  if LFNSupport then
   regs.realeax:=$716c
  else
   regs.realeax:=$6c00;
  regs.realedx:=$12;
  regs.realds:=tb_segment;
  regs.realesi:=tb_offset;
  regs.realebx:=$2001;
  regs.realecx:=$20;
  realintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   begin
     ErrorCode:=lo(regs.realeax);
     exit(0);
   end
  else
   CreateFileStr:=regs.realeax and $ffff;
end;


procedure DeleteFileStr(FName: PChar);
var
  regs : trealregs;
begin
  copytodos(FName^,256);
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7141
  else
   regs.realeax:=$4100;
  regs.realesi:=0;
  regs.realecx:=0;
  realintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   ErrorCode:=lo(regs.realeax);
end;


procedure CloseFile(Handle: TFileHandle);
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realeax:=$3e00;
  RealIntr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   ErrorCode:=lo(regs.realeax);
end;


function SeekFile(Handle: TFileHandle; Pos: TFileInt; SeekType: Word): TFileInt;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realecx:=pos shr 16;
  regs.realedx:=pos and $ffff;
  regs.realeax:=$4200 or SeekType;
  RealIntr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   begin
     ErrorCode:=lo(regs.realeax);
     SeekFile:=-1;
   end
  else
   SeekFile:=lo(regs.realedx) shl 16+lo(regs.realeax);
end;


function ReadFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  regs     : trealregs;
  addr     : pchar;
  len,
  size,
  readsize : longint;
begin
  len:=count;
  addr:=@buff;
  readsize:=0;
  while len > 0 do
   begin
     if len>tb_size then
      size:=tb_size
     else
      size:=len;
     regs.realecx:=len;
     regs.realedx:=tb_offset;
     regs.realds:=tb_segment;
     regs.realebx:=handle;
     regs.realeax:=$3f00;
     RealIntr($21,regs);
     if (regs.realflags and carryflag) <> 0 then
      begin
        InOutRes:=lo(regs.realeax);
        exit(0);
      end
     else
      if regs.realeax<size then
       begin
         copyfromdos(addr^,regs.realeax);
         exit(readsize+regs.realeax);
       end;
     copyfromdos(addr^,regs.realeax);
     inc(readsize,regs.realeax);
     inc(addr,regs.realeax);
     dec(len,regs.realeax);
   end;
  readfile:=readsize;
end;


function WriteFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  regs      : trealregs;
  addr      : pchar;
  len,
  size,
  writesize : longint;
begin
  len:=count;
  addr:=@buff;
  writesize:=0;
  while len > 0 do
   begin
     if len>tb_size then
      size:=tb_size
     else
      size:=len;
     copytodos(addr^,size);
     regs.realecx:=size;
     regs.realedx:=tb_offset;
     regs.realds:=tb_segment;
     regs.realebx:=handle;
     regs.realeax:=$4000;
     RealIntr($21,regs);
     if (regs.realflags and carryflag) <> 0 then
      begin
        ErrorCode:=lo(regs.realeax);
        exit(writesize);
      end;
     dec(len,size);
     inc(writesize,size);
     inc(addr,size);
   end;
  WriteFile:=WriteSize;
end;


procedure FlushFile(Handle: TFileHandle);
var
  regs : trealregs;
begin
  regs.ebx:=handle;
  regs.ah:=$68;
  realintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   ErrorCode:=lo(regs.realeax);
end;


procedure TruncateFile(Handle: TFileHandle);
var
  regs : trealregs;
begin
  regs.realecx:=0;
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
  regs.realebx:=handle;
  regs.realeax:=$4000;
  RealIntr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   ErrorCode:=lo(regs.realeax);
end;

function EndOfFile(Handle: TFileHandle): Boolean;
begin
  EndOfFile := FilePos(Handle) >= FileSize(Handle);
end;

function FilePos(Handle: TFileHandle): TFileInt;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realecx:=0;
  regs.realedx:=0;
  regs.realeax:=$4201;
  RealIntr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   Begin
     InOutRes:=lo(regs.realeax);
     filepos:=-1;
   end
  else
   filepos:=lo(regs.realedx) shl 16+lo(regs.realeax);
end;

function FileSize(Handle: TFileHandle): TFileInt;
var
  aktfilepos : longint;
begin
  aktfilepos:=filepos(handle);
  filesize:=seekfile(handle,0,2);
  seekfile(handle,aktfilepos,0);
end;

{
  $Log$
  Revision 1.2  2000-07-13 11:32:24  michael
  + removed logs
 
}
