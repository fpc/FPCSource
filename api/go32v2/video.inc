{
  System independent low-level video interface for go32v2

  $Id$
}

{$ASMMODE ATT}

uses
  mouse,
  go32;

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
  Revision 1.3  2000-02-07 22:54:44  florian
    * custommouse define removed, i.e. code is always active
    * the xor value for the mouse cursor must be $7f instead of $ff

  Revision 1.2  2000/02/06 14:29:45  florian
    * mouse support for vesa resolutions under go32v2, needs currently the define
      custommouse

  Revision 1.1  2000/01/06 01:20:30  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.14  1999/10/03 19:53:26  peter
    * changed screenheight detection

  Revision 1.13  1999/08/16 18:26:20  peter
    * asm updatescreen for speed reasons

  Revision 1.12  1999/06/02 11:22:10  pierre
   * @ needed for proc address

  Revision 1.11  1999/04/01 12:51:51  pierre
   * removed clearscreen in initvideo for capture

  Revision 1.10  1999/03/21 22:49:40  florian
    * correct screeneight in 80x50 mode

  Revision 1.9  1999/03/14 22:15:49  florian
    * my last changes doesn't work correctly, fixed more
      the screen height calculation works incorrect in 80x50 mode

  Revision 1.8  1999/03/14 17:43:03  florian
    + 80x50 mode support added
    * some bugs in VESA mode support removed

  Revision 1.7  1999/02/19 16:42:48  peter
    * fixed typo

  Revision 1.6  1999/02/19 12:29:52  pierre
    * several bugs related to Cursor fixed !
      I still don't know the maximum value for
      the scan line (depends on resolution used !)

  Revision 1.5  1999/02/08 17:53:17  pierre
   + added restoring of BlinkState in InitVideo, old mode not stored

  Revision 1.4  1998/12/15 17:17:17  peter
    + cursor at 1,1 at the end

  Revision 1.3  1998/12/12 19:13:01  peter
    * keyboard updates
    * make test target, make all only makes units

  Revision 1.2  1998/12/10 11:41:50  florian
    * cursor is properly restored in DoneVideo

  Revision 1.1  1998/12/04 12:48:27  peter
    * moved some dirs

  Revision 1.4  1998/11/01 20:29:11  peter
    + lockupdatescreen counter to not let updatescreen() update

  Revision 1.3  1998/10/28 21:18:26  peter
    * more fixes

  Revision 1.2  1998/10/28 00:02:08  peter
    + mouse
    + video.clearscreen, video.videobufsize

  Revision 1.1  1998/10/26 11:31:47  peter
    + inital include files

}