{
  System independent low-level video interface for go32v2

  $Id$
}

{$ASMMODE ATT}

uses
  mouse,
  go32;


procedure TargetEntry;
begin
end;

procedure TargetExit;
begin
end;

var
  OldVideoBuf : PVideoBuf;

  { used to know if LastCursorType is valid }
const
  InitVideoCalled : boolean = false;
  LastCursorType : word = crUnderline;

{ allways set blink state again }

procedure SetHighBitBlink;
var
  regs : trealregs;
begin
  regs.ax:=$1003;
  regs.bx:=$0001;
  realintr($10,regs);
end;

function BIOSGetScreenMode(var Cols,Rows: word; var Color: boolean): boolean;
var r: trealregs;
    L: longint;
    LSel,LSeg: word;
    B: array[0..63] of byte;
type TWord = word; PWord = ^TWord;
var Size: word;
    OK: boolean;
begin
  L:=global_dos_alloc(64);
  LSeg:=(L shr 16);
  LSel:=(L and $ffff);

  r.ah:=$1b; r.bx:=0;
  r.es:=LSeg; r.di:=0;
  realintr($10,r);
  OK:=(r.al=$1b);
  if OK then
  begin
    dpmi_dosmemget(LSeg,0,B,64);
    Cols:=PWord(@B[5])^; Rows:=B[$22];
    Color:=PWord(@B[$27])^<>0;
  end;
  global_dos_free(LSel);
  BIOSGetScreenMode:=OK;
end;

procedure InitVideo;
var
  regs : trealregs;
begin
  VideoSeg:=$b800;
  if (ScreenWidth=$ffff) or (ScreenHeight=$ffff) or
    (ScreenWidth=0) or (ScreenHeight=0) then
    begin
       ScreenColor:=true;
       regs.ah:=$0f;
       realintr($10,regs);
       if (regs.al and 1)=0 then
         ScreenColor:=false;
       if regs.al=7 then
         begin
            ScreenColor:=false;
            VideoSeg:=$b000;
         end
       else
         VideoSeg:=$b800;
       ScreenWidth:=regs.ah;
       regs.ax:=$1130;
       regs.bx:=0;
       realintr($10,regs);
       ScreenHeight:=regs.dl+1;
       BIOSGetScreenMode(ScreenWidth,ScreenHeight,ScreenColor);
    end;
  regs.ah:=$03;
  regs.bh:=0;
  realintr($10,regs);
  CursorLines:=regs.cl;
  CursorX:=regs.dl;
  CursorY:=regs.dh;
  If InitVideoCalled then
    Begin
      FreeMem(VideoBuf,VideoBufSize);
      FreeMem(OldVideoBuf,VideoBufSize);
    End;
{ allocate pmode memory buffer }
  VideoBufSize:=ScreenWidth*ScreenHeight*2;
  GetMem(VideoBuf,VideoBufSize);
  GetMem(OldVideoBuf,VideoBufSize);
  InitVideoCalled:=true;
  SetHighBitBlink;
  SetCursorType(LastCursorType);
  { ClearScreen; removed here
    to be able to catch the content of the monitor }
end;


procedure DoneVideo;
begin
  If InitVideoCalled then
    Begin
      LastCursorType:=GetCursorType;
      ClearScreen;
      SetCursorType(crUnderLine);
      SetCursorPos(0,0);
      FreeMem(VideoBuf,VideoBufSize);
      VideoBuf:=nil;
      FreeMem(OldVideoBuf,VideoBufSize);
      OldVideoBuf:=nil;
      InitVideoCalled:=false;
      VideoBufSize:=0;
    End;
end;


function GetCapabilities: Word;
begin
  GetCapabilities := $3F;
end;


procedure SetCursorPos(NewCursorX, NewCursorY: Word);
var
  regs : trealregs;
begin
  regs.ah:=$02;
  regs.bh:=0;
  regs.dh:=NewCursorY;
  regs.dl:=NewCursorX;
  realintr($10,regs);
  CursorY:=regs.dh;
  CursorX:=regs.dl;
end;

{ I don't know the maximum value for the scan line
  probably 7 or 15 depending on resolution !!
  }
function GetCursorType: Word;
var
  regs : trealregs;
begin
  regs.ah:=$03;
  regs.bh:=0;
  realintr($10,regs);
  GetCursorType:=crHidden;
  if (regs.ch and $60)=0 then
   begin
     GetCursorType:=crBlock;
     if (regs.ch and $1f)<>0 then
      begin
        GetCursorType:=crHalfBlock;
        if regs.cl+1=(regs.ch and $1F) then
         GetCursorType:=crUnderline;
      end;
   end;
end;


procedure SetCursorType(NewType: Word);
var
  regs : trealregs;
const
  MaxCursorLines = 7;
begin
  regs.ah:=$01;
  regs.bx:=NewType;
  case NewType of
   crHidden    : regs.cx:=$2000;
   crHalfBlock : begin
                   regs.ch:=MaxCursorLines shr 1;
                   regs.cl:=MaxCursorLines;
                 end;
   crBlock     : begin
                   regs.ch:=0;
                   regs.cl:=MaxCursorLines;
                 end;
   else          begin
                   regs.ch:=MaxCursorLines-1;
                   regs.cl:=MaxCursorLines;
                 end;
  end;
  realintr($10,regs);
end;


function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;
type
  wordrec=packed record
    lo,hi : word;
  end;
var
  regs : trealregs;
begin
  regs.ax:=wordrec(Params).lo;
  regs.bx:=wordrec(Params).hi;
  realintr($10,regs);
  defaultvideomodeselector:=true;
  DoCustomMouse(false);
end;

function VideoModeSelector8x8(const VideoMode: TVideoMode; Params: Longint): Boolean;
type
  wordrec=packed record
    lo,hi : word;
  end;
var
  regs : trealregs;
begin
  regs.ax:=3;
  regs.bx:=0;
  realintr($10,regs);
  regs.ax:=$1112;
  regs.bx:=$0;
  realintr($10,regs);
  videomodeselector8x8:=true;
  ScreenColor:=true;
  ScreenWidth:=80;
  ScreenHeight:=50;
  DoCustomMouse(false);
end;

procedure ClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
  UpdateScreen(true);
end;


procedure UpdateScreen(Force: Boolean);
begin
  if LockUpdateScreen<>0 then
   exit;
  if not force then
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        orl     %ecx,%ecx
        jz      .Lno_update
        movb    $1,force
.Lno_update:
     end;
   end;
  if Force then
   begin
{     dosmemput(videoseg,0,videobuf^,VideoBufSize);}
      asm
        pushw %es
        pushl %edi
        pushl %esi

        xor  %edi, %edi
        movw videoseg, %di
        shll $0x4, %edi
        movl videobuf, %esi
        movl videobufsize, %ecx
        movw %fs, %ax
        movw %ax, %es
        rep movsb

        popl  %esi
        popl  %edi
        popw  %es
      end ['EAX','ECX'];
     move(videobuf^,oldvideobuf^,VideoBufSize);
   end;
end;


procedure RegisterVideoModes;
begin
  RegisterVideoMode(40, 25, False,@DefaultVideoModeSelector, $00000000);
  RegisterVideoMode(40, 25, True, @DefaultVideoModeSelector, $00000001);
  RegisterVideoMode(80, 25, False,@DefaultVideoModeSelector, $00000002);
  RegisterVideoMode(80, 25, True, @DefaultVideoModeSelector, $00000003);
  RegisterVideoMode(80, 50, True, @VideoModeSelector8x8, 0);
end;

{
  $Log$
  Revision 1.4  2000-10-04 11:53:31  pierre
   Add TargetEntry and TargetExit (merged)

  Revision 1.3  2000/08/16 18:51:57  peter
    * Fixes from Gabor (merged)

  Revision 1.2  2000/07/13 11:32:24  michael
  + removed logs

}