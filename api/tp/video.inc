{
  System independent low-level video interface for tp7

  $Id$
}

{ use a buffer, just like linux,go32v2 }
{$define use_buf}

var
  VideoSeg    : word;
  OldVideoBuf : PVideoBuf;

{ internal function, which is by default available under FPC }
procedure fillword(var buf;len,w:word);assembler;
asm
        les     di,buf
        mov     cx,len
        mov     ax,w
        rep     stosw
end;


procedure InitVideo;
begin
  asm
        mov     ah,0fh
        int     10h
        mov     [ScreenColor],1
        test    al,1            { even modes are colored }
        jne     @ColorOn
        mov     [ScreenColor],0
@ColorOn:
        cmp     al,7            { 7 mono mode }
        mov     dx,SegB800
        jne     @@1
        mov     [ScreenColor],0
        mov     dx,SegB000
@@1:
{$ifdef use_buf}
        mov     videoseg,dx
{$else}
        mov     [word ptr VideoBuf+0], 0
        mov     [word ptr VideoBuf+2], dx
{$endif}
        xchg    al,ah
        xor     ah,ah
        mov     [ScreenWidth],ax
        mov     bx,40h
        mov     cx,ax                   { cx:=ax, pipeline ok }
        mov     es,bx
        shl     cx,1
        mov     ax,[word ptr es:04ch] { Size of videobuf }
        xor     dx,dx
        div     cx
        mov     [ScreenHeight],ax
        mov     ah,03h
        xor     bh,bh
        int     10h
        mov     [CursorLines], cl
        xor     ax,ax
        mov     al,dl
        mov     [CursorX],ax
        mov     al,dh
        mov     [CursorY],ax
  end;
{$ifdef use_buf}
  VideoBufSize:=ScreenWidth*ScreenHeight*2;
  GetMem(VideoBuf,VideoBufSize);
  GetMem(OldVideoBuf,VideoBufSize);
{$endif}
  ClearScreen;
end;


procedure DoneVideo;
begin
  ClearScreen;
  SetCursorType(crUnderLine);
  SetCursorPos(0,0);
{$ifdef use_buf}
  FreeMem(VideoBuf,VideoBufSize);
  FreeMem(OldVideoBuf,VideoBufSize);
  VideoBufSize:=0;
{$endif}
end;


function GetCapabilities: Word;
begin
  GetCapabilities := $3F;
end;


procedure SetCursorPos(NewCursorX, NewCursorY: Word); assembler;
asm
        mov     ah,02h
        xor     bh,bh
        mov     dh,[byte ptr NewCursorY]
        mov     dl,[byte ptr NewCursorX]
        int     10h
        mov     [byte ptr CursorY],dh
        mov     [byte ptr CursorX],dl
end;


function GetCursorType: Word; assembler;
asm
        mov     ah,03h
        xor     bh,bh
        int     10h
        mov     ax,crHidden
        cmp     cx,2000h
        je      @@1
        mov     ax,crBlock
        cmp     ch,00h
        je      @@1
        mov     ax,crHalfBlock
        mov     bl,[CursorLines]
        shr     bl,1
        cmp     ch,bl
        jbe     @@1
        mov     ax,crUnderline
@@1:
end;


procedure SetCursorType(NewType: Word); assembler;
asm
        mov     ah,01h
        mov     bx,[NewType]
        mov     cx,2000h
        cmp     bx,crHidden
        je      @@1
        mov     ch,[CursorLines]
        mov     cl,ch
        shr     ch,1
        cmp     bx,crHalfBlock
        je      @@1
        mov     ch,0
        cmp     bx,crBlock
        je      @@1
        mov     cl,[CursorLines]
        mov     ch,cl
        dec     ch
@@1:
        int     10h
end;


procedure ClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
{$ifdef use_buf}
  UpdateScreen(true);
{$endif}
end;


procedure UpdateScreen(Force: Boolean);
{$ifdef use_buf}
var
  SwapPtr : PVideoBuf;
{$endif}
begin
  if LockUpdateScreen<>0 then
   exit;
{$ifdef use_buf}
  if not force then
   begin
     asm
        mov     cx,word ptr VideoBufSize
        shr     cx,1
        les     di,OldVideoBuf
        push    ds
        lds     si,VideoBuf
        repe    cmpsw
        pop     ds
        or      cx,cx
        jz      @@10
        mov     force,1
@@10:
     end;
   end;
  if force then
   begin
     move(videobuf^,ptr(videoseg,0)^,VideoBufSize);
     move(videobuf^,oldvideobuf^,VideoBufSize);
   end;
{$endif}
end;


function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean; assembler;
asm
        mov     ax,[word ptr Params+0]
        mov     bx,[word ptr Params+2]
        push    bp
        int     10h
        pop     bp
        mov     al,1
end;


procedure RegisterVideoModes;
begin
  RegisterVideoMode(40, 25, False, DefaultVideoModeSelector, $00000000);
  RegisterVideoMode(40, 25, True, DefaultVideoModeSelector, $00000001);
  RegisterVideoMode(80, 25, False, DefaultVideoModeSelector, $00000002);
  RegisterVideoMode(80, 25, True, DefaultVideoModeSelector, $00000003);
end;

{
  $Log$
  Revision 1.2  2000-07-13 11:32:27  michael
  + removed logs
 
}
