{
  System independent mouse interface for tp7

  $Id$
}

procedure MouseInt;far;assembler;
asm
        mov     si,seg @data
        mov     ds,si
        mov     si,cx
        mov     MouseButtons,bl
        mov     MouseWhereX,si
        mov     MouseWhereY,dx
        cmp     PendingMouseEvents,MouseEventBufSize
        je      @@20
        les     di,PendingMouseTail
        cld
        xchg    ax,bx
        stosw
        xchg    ax,cx
        shr     ax,3
        stosw
        xchg    ax,dx
        shr     ax,3
        stosw
        xor     ax,ax
        stosw
        mov     ax,offset PendingMouseEvent
        add     ax,MouseEventBufSize*8
        cmp     di,ax
        jne     @@10
        mov     di,offset PendingMouseEvent
@@10:   mov     word ptr PendingMouseTail,di
        inc     PendingMouseEvents
@@20:
end;


procedure InitMouse;
begin
  PendingMouseHead:=@PendingMouseEvent;
  PendingMouseTail:=@PendingMouseEvent;
  PendingMouseEvents:=0;
  FillChar(LastMouseEvent,sizeof(TMouseEvent),0);
  asm
        mov     ax,0ch
        mov     cx,0ffffh
        mov     dx,offset MouseInt
        push    cs
        pop     es
        push    bp
        int     33h
        pop     bp
  end;
  ShowMouse;
end;


procedure DoneMouse;
begin
  HideMouse;
  asm
        mov     ax,0ch
        xor     cx,cx
        xor     dx,dx
        mov     es,cx
        push    bp
        int     33h
        pop     bp
  end;
end;


function DetectMouse:byte;assembler;
asm
        mov     ax,3533h
        push    bp
        int     21h
        pop     bp
        mov     ax,es
        or      ax,bx
        jz      @@99
        xor     ax,ax
        push    bp
        int     33h
        pop     bp
        or      ax,ax
        jz      @@99
        mov     ax,bx
@@99:
end;


procedure ShowMouse;assembler;
asm
        mov     ax,1
        push    bp
        int     33h
        pop     bp
end;


procedure HideMouse;assembler;
asm
        mov     ax,2
        push    bp
        int     33h
        pop     bp
end;


function GetMouseX:word;assembler;
asm
        mov     ax,3
        push    bp
        int     33h
        pop     bp
        mov     ax,cx
        shr     ax,3
        inc     ax
end;


function GetMouseY:word;assembler;
asm
        mov     ax,3
        push    bp
        int     33h
        pop     bp
        mov     ax,dx
        shr     ax,3
        inc     ax
end;


function GetMouseButtons:word;assembler;
asm
        mov     ax,3
        push    bp
        int     33h
        pop     bp
        mov     ax,bx
end;


procedure SetMouseXY(x,y:word);assembler;
asm
        mov     ax,4
        mov     cx,x
        mov     dx,y
        push    bp
        int     33h
        pop     bp
end;


procedure GetMouseEvent(var MouseEvent: TMouseEvent);
begin
  repeat until PendingMouseEvents>0;
  MouseEvent:=PendingMouseHead^;
  inc(PendingMouseHead);
  if longint(PendingMouseHead)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
   PendingMouseHead:=@PendingMouseEvent;
  dec(PendingMouseEvents);
  if (LastMouseEvent.x<>MouseEvent.x) or (LastMouseEvent.y<>MouseEvent.y) then
   MouseEvent.Action:=MouseActionMove;
  if (LastMouseEvent.Buttons<>MouseEvent.Buttons) then
   begin
     if (LastMouseEvent.Buttons=0) then
      MouseEvent.Action:=MouseActionDown
     else
      MouseEvent.Action:=MouseActionUp;
   end;
  LastMouseEvent:=MouseEvent;
end;


function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;
begin
  if PendingMouseEvents>0 then
   begin
     MouseEvent:=PendingMouseHead^;
     PollMouseEvent:=true;
   end
  else
   PollMouseEvent:=false;
end;

{
  $Log$
  Revision 1.1  2000-07-13 06:29:41  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.2  1998/12/11 00:13:21  peter
    + SetMouseXY
    * use far for exitproc procedure

  Revision 1.1  1998/12/04 12:48:57  peter
    * moved some dirs

  Revision 1.1  1998/10/28 00:02:09  peter
    + mouse
    + video.clearscreen, video.videobufsize

}
