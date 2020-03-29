{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Mouse unit for MS-DOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Mouse;
interface

{$i mouseh.inc}

{ tells the mouse unit to draw the mouse cursor itself }
procedure DoCustomMouse(b : boolean);

implementation

uses
  video,dos;

{$i mouse.inc}


var
  CurrentMask : word;
  MouseCallback : CodePointer;                       { Mouse call back ptr }
const
  { indicates whether the mouse cursor is visible when the mouse cursor is
    drawn by this unit (i.e. drawmousecursor=true) }
  CustomMouse_MouseIsVisible: boolean = false;
  MousePresent : boolean = false;
  First_try    : boolean = true;
  drawmousecursor : boolean = false;

  { CustomMouse_HideCount holds the hide count for the custom drawn mouse
    cursor. Normally, when the mouse cursor is drawn by the int 33h mouse
    driver (and not by this unit), the driver internally maintains a 'hide
    counter', so that if you call HideMouse multiple times, you need to call
    ShowMouse the same number of times. When the mouse cursor is customly
    drawn by this unit, we use this variable in order to maintain the same
    behaviour. }
  CustomMouse_HideCount: smallint = 1;

  { position where the mouse was drawn the last time }
  oldmousex : smallint = -1;
  oldmousey : smallint = -1;
  mouselock : boolean = false;

{ if the cursor is drawn by this the unit, we must be careful }
{ when drawing while the interrupt handler is called          }
procedure lockmouse;assembler;

  asm
  @@trylockagain:
     mov     al,1
     xchg    al,mouselock
     or      al,al
     jne     @@trylockagain
  end;

procedure unlockmouse;

  begin
     mouselock:=false;
  end;


procedure MouseInt;assembler;
asm
        push    ds
        push    es
        push    di
        push    cx
        push    dx
{$ifdef FPC_MM_TINY}
        push    cs
        pop     ds
{$else}
        mov     di, SEG @DATA
        mov     ds, di
{$endif}
        mov     mousebuttons,bl
        mov     mousewherex,cx
        mov     mousewherey,dx
        shr     cx,1
        shr     cx,1
        shr     cx,1
        shr     dx,1
        shr     dx,1
        shr     dx,1
{$ifdef FPC_MM_HUGE}
        mov     di, SEG ScreenWidth
        mov     es, di
        cmp     es:[ScreenWidth], 40
{$else}
        cmp     ScreenWidth, 40
{$endif}
        jne     @@morethan40cols
        shr     cx,1
@@morethan40cols:
        { should we draw the mouse cursor? }
        cmp     drawmousecursor, 0
        je      @@mouse_nocursor
        cmp     CustomMouse_MouseIsVisible, 0
        je      @@mouse_nocursor
        push    ax
        push    bx
{$ifdef FPC_MM_HUGE}
        push    si
{$endif}
        { check lock }
        mov     al, 1
        xchg    al, mouselock
        or      al, al
        { don't update the cursor yet, because hide/showcursor is called }
        jne     @@dont_draw

        { calculate address of old mouse cursor }
        mov     ax, oldmousey
{$ifdef FPC_MM_HUGE}
        { ES still points to the data segment of unit 'video' }
        mov     si, es:[screenwidth]
        imul    si
{$else}
        imul    screenwidth
{$endif}
        add     ax, oldmousex
        shl     ax, 1
        xchg    ax, bx
        { load start of video buffer }
{$ifdef FPC_MM_HUGE}
        { ES still points to the data segment of unit 'video' }
        mov     di, es:[videoseg]
{$else}
        mov     di, videoseg
{$endif}
        mov     es, di
        { remove old cursor }
        xor     byte ptr es:[bx], 7fh

        { store position of old cursor }
        mov     oldmousex, cx
        mov     oldmousey, dx

        { calculate address of new cursor }
        mov     ax, dx
{$ifdef FPC_MM_HUGE}
        imul    si
{$else}
        imul    screenwidth
{$endif}
        add     ax, cx
        shl     ax, 1
        xchg    ax, bx
        { draw new cursor }
        xor     byte ptr es:[bx], 7fh

        { unlock mouse }
        mov     mouselock, 0

@@dont_draw:
{$ifdef FPC_MM_HUGE}
        pop     si
{$endif}
        pop     bx
        pop     ax
@@mouse_nocursor:
        cmp     PendingMouseEvents, MouseEventBufSize
        je      @@mouse_exit
{$if defined(FPC_MM_COMPACT) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
        les     di, [PendingMouseTail]
        mov     word ptr es:[di], bx
        mov     word ptr es:[di+2], cx
        mov     word ptr es:[di+4], dx
        mov     word ptr es:[di+6], 0
{$else}
        mov     di, PendingMouseTail
        mov     word ptr [di], bx
        mov     word ptr [di+2], cx
        mov     word ptr [di+4], dx
        mov     word ptr [di+6], 0
{$endif}
        add     di, 8
        lea     ax, PendingMouseEvent
        add     ax, MouseEventBufSize*8
        cmp     di, ax
        jne     @@mouse_nowrap
        lea     di, PendingMouseEvent
@@mouse_nowrap:
        mov     word ptr PendingMouseTail, di
        inc     PendingMouseEvents
@@mouse_exit:
        pop     dx
        pop     cx
        pop     di
        pop     es
        pop     ds
        retf
end;

PROCEDURE Mouse_Action (Mask : Word; P : CodePointer);
VAR
  Rg    : Registers;
BEGIN
  if (P <> MouseCallBack) or (Mask<>CurrentMask) then                        { Check func different }
    begin
      { Remove old calback }
      if (CurrentMask <> 0) then
      begin
        Rg.AX := 12;                                   { Function id }
        Rg.CX := 0;                                    { Zero mask register }
        Rg.ES := 0;                                    { Zero proc seg }
        Rg.DX := 0;                                    { Zero proc ofs }
        Intr($33, Rg);                                 { Stop INT 33 callback }
      end;
      if P = nil then
        Mask := 0;                                    { Zero mask register }
      MouseCallback := P;                            { Set call back addr }
      if Mask<>0 then
        begin
          Rg.AX := 12;                                   { Set function id }
          Rg.CX := Mask;                                 { Set mask register }
          If Mask<>0 then
            begin
              Rg.ES := Seg(P^);
              Rg.DX := Ofs(P^);
            end
          else
            begin
              Rg.ES:=0;
              Rg.DX:=0;
            end;
          Intr($33, Rg);                                 { Set interrupt 33 }
        end;
      CurrentMask:=Mask;
    end;
END;


{ We need to remove the mouse callback before exiting !! PM }

const StoredExit : CodePointer = Nil;
      FirstMouseInitDone : boolean = false;

procedure MouseSafeExit;
begin
  ExitProc:=StoredExit;
  if MouseCallBack<>Nil then
    Mouse_Action(0, Nil);
  if not FirstMouseInitDone then
    exit;
  FirstMouseInitDone:=false;
end;

procedure SysInitMouse;
begin
  if not MousePresent then
    begin
      if DetectMouse=0 then
        begin
          if First_try then
            begin
              Writeln('No mouse driver found ');
              First_try:=false;
            end;
          exit;
        end
      else
        MousePresent:=true;
    end;
  { don't do this twice !! PM }

  If not FirstMouseInitDone then
    begin
      StoredExit:=ExitProc;
      ExitProc:=@MouseSafeExit;
      FirstMouseInitDone:=true;
    end;
  If MouseCallBack=Nil then
    Mouse_Action($ffff, @MouseInt);                    { Set masks/interrupt }
  drawmousecursor:=false;
  CustomMouse_MouseIsVisible:=false;
  if (screenwidth>80) or (screenheight>50) then
    DoCustomMouse(true);
  ShowMouse;
end;


procedure SysDoneMouse;
begin
  HideMouse;
  If (MouseCallBack <> Nil) Then
    Mouse_Action(0, Nil);                            { Clear mask/interrupt }
end;


function SysDetectMouse:byte;assembler;
asm
        xor     ax, ax
        mov     es, ax
        mov     di, es:[4*33h]
        or      di, es:[4*33h+2]
        jz      @@no_mouse

        push    bp
        int     33h
        pop     bp
        or      ax, ax
        jz      @@no_mouse
        mov     ax, bx
@@no_mouse:
end;


procedure SysShowMouse;

begin
   if drawmousecursor then
     begin
        lockmouse;
        if CustomMouse_HideCount>0 then
          Dec(CustomMouse_HideCount);
        if (CustomMouse_HideCount=0) and not(CustomMouse_MouseIsVisible) then
          begin
             oldmousex:=getmousex-1;
             oldmousey:=getmousey-1;
             mem[videoseg:(((screenwidth*oldmousey)+oldmousex)*2)+1]:=
               mem[videoseg:(((screenwidth*oldmousey)+oldmousex)*2)+1] xor $7f;
             CustomMouse_MouseIsVisible:=true;
          end;
        unlockmouse;
     end
   else
     asm
             cmp     MousePresent, 1
             jne     @@ShowMouseExit
             mov     ax, 1
             push    bp
             int     33h
             pop     bp
     @@ShowMouseExit:
     end;
end;


procedure SysHideMouse;

begin
   if drawmousecursor then
     begin
        lockmouse;
        Inc(CustomMouse_HideCount);
        if CustomMouse_MouseIsVisible then
          begin
             CustomMouse_MouseIsVisible:=false;
             mem[videoseg:(((screenwidth*oldmousey)+oldmousex)*2)+1]:=
               mem[videoseg:(((screenwidth*oldmousey)+oldmousex)*2)+1] xor $7f;
             oldmousex:=-1;
             oldmousey:=-1;
          end;
        unlockmouse;
     end
   else
     asm
             cmp     MousePresent, 1
             jne     @@HideMouseExit
             mov     ax, 2
             push    bp
             int     33h
             pop     bp
     @@HideMouseExit:
     end;
end;


function SysGetMouseX:word;assembler;
asm
        cmp     MousePresent, 1
        jne     @@GetMouseXError
        mov     ax, 3
        push    bp
        int     33h
        pop     bp
        xchg    ax, cx
        shr     ax, 1
        shr     ax, 1
        shr     ax, 1
{$ifdef FPC_MM_HUGE}
        mov     bx, SEG ScreenWidth
        mov     es, bx
        cmp     es:[ScreenWidth], 40
{$else}
        cmp     ScreenWidth, 40
{$endif}
        jne     @@morethan40cols
        shr     ax, 1
@@morethan40cols:
        inc     ax
        jmp @@exit
@@GetMouseXError:
        xor     ax, ax
@@exit:
end;


function SysGetMouseY:word;assembler;
asm
        cmp     MousePresent, 1
        jne     @@GetMouseYError
        mov     ax, 3
        push    bp
        int     33h
        pop     bp
        xchg    ax, dx
        shr     ax, 1
        shr     ax, 1
        shr     ax, 1
        inc     ax
        jmp @@exit
@@GetMouseYError:
        xor     ax, ax
@@exit:
end;


function SysGetMouseButtons:word;assembler;
asm
        cmp     MousePresent, 1
        jne     @@GetMouseButtonsError
        mov     ax, 3
        push    bp
        int     33h
        pop     bp
        xchg    ax, bx
        jmp     @@exit
@@GetMouseButtonsError:
        xor     ax, ax
@@exit:
end;


procedure SysSetMouseXY(x,y:word);assembler;
asm
        cmp     MousePresent, 1
        jne     @@SetMouseXYExit
        mov     cx, x
        mov     dx, y
        mov     ax, 4
        push    bp
        int     33h
        pop     bp
@@SetMouseXYExit:
end;

Procedure SetMouseXRange (Min,Max:word);
begin
  If Not(MousePresent) Then Exit;
  asm
        mov     ax, 7
        mov     cx, min
        mov     dx, max
        push    bp
        int     33h
        pop     bp
  end;
end;

Procedure SetMouseYRange (min,max:word);
begin
  If Not(MousePresent) Then Exit;
  asm
        mov     ax, 8
        mov     cx, min
        mov     dx, max
        push    bp
        int     33h
        pop     bp
  end;
end;

procedure DoCustomMouse(b : boolean);

  begin
     lockmouse;
     CustomMouse_HideCount:=1;
     oldmousex:=-1;
     oldmousey:=-1;
     if ScreenWidth=40 then
       SetMouseXRange(0,(screenwidth-1)*16)
     else
       SetMouseXRange(0,(screenwidth-1)*8);
     SetMouseYRange(0,(screenheight-1)*8);
     if b then
       begin
          CustomMouse_MouseIsVisible:=false;
          drawmousecursor:=true;
       end
     else
       drawmousecursor:=false;
     unlockmouse;
  end;

procedure SysGetMouseEvent(var MouseEvent: TMouseEvent);
var
 RR: Registers;
begin
  if not MousePresent then
    begin
      Fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
    end;
  while PendingMouseEvents = 0 do
   begin
(* Give up time slices while waiting for mouse events. *)
    Intr ($28, RR);
   end;
  MouseEvent:=PendingMouseHead^;
  inc(PendingMouseHead);
  if PendingMouseHead=@PendingMouseEvent[0]+MouseEventBufsize then
   PendingMouseHead:=@PendingMouseEvent[0];
  dec(PendingMouseEvents);
  if (LastMouseEvent.x<>MouseEvent.x) or (LastMouseEvent.y<>MouseEvent.y) then
   MouseEvent.Action:=MouseActionMove;
  if (LastMouseEvent.Buttons<>MouseEvent.Buttons) then
   begin
     if (LastMouseEvent.Buttons and MouseEvent.buttons<>LastMouseEvent.Buttons) then
       MouseEvent.Action:=MouseActionUp
     else
       MouseEvent.Action:=MouseActionDown;
   end;
  LastMouseEvent:=MouseEvent;
end;


Const
  SysMouseDriver : TMouseDriver = (
    useDefaultQueue : true;
    InitDriver      : @SysInitMouse;
    DoneDriver      : @SysDoneMouse;
    DetectMouse     : @SysDetectMouse;
    ShowMouse       : @SysShowMouse;
    HideMouse       : @SysHideMouse;
    GetMouseX       : @SysGetMouseX;
    GetMouseY       : @SysGetMouseY;
    GetMouseButtons : @SysGetMouseButtons;
    SetMouseXY      : @SysSetMouseXY;
    GetMouseEvent   : @SysGetMouseEvent;
    PollMouseEvent  : Nil;
    PutMouseEvent  : Nil;
  );

Begin
  SetMouseDriver(SysMouseDriver);
end.
