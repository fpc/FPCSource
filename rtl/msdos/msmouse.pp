{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Mouse unit for microsoft mouse compatible drivers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
Unit MSMouse;
Interface

{
  Mouse support functions and procedures, with error checking: if mouse
  isn't present then the routine ends. If you want to remove error checking,
  remove the next define.
}


{initializes the mouse with the default values for the current screen mode}
  Function InitMouse:Boolean;

{shows mouse pointer,text+graphics screen support}
  Procedure ShowMouse;

{hides mouse pointer}
  Procedure HideMouse;

{reads mouse position in pixels (divide by 8 to get text position in standard
 text mode) and reads the buttons state:
    bit 1 set -> left button pressed
    bit 2 set -> right button pressed
    bit 3 set -> middle button pressed
 Have a look at the example program in the manual to see how you can use this}
  Procedure GetMouseState(var x,y, buttons :LongInt);

{returns true if the left button is pressed}
  Function LPressed:Boolean;

{returns true if the right button is pressed}
  Function RPressed:Boolean;

{returns true if the middle button is pressed}
  Function MPressed:Boolean;

{positions the mouse pointer}
  Procedure SetMousePos(x,y:LongInt);

{returns at which position "button" was last pressed in x,y and returns the
 number of times this button has been pressed since the last time this
 function was called with "button" as parameter. For button you can use the
 LButton, RButton and MButton constants for resp. the left, right and middle
 button}
  Function GetLastButtonPress(button:LongInt;var x,y:LongInt): LongInt;

{returns at which position "button" was last released in x,y and returns the
 number of times this button has been re since the last time. For button
 you can use the LButton, RButton and MButton constants for resp. the left,
 right and middle button}
Function GetLastButtonRelease (button : LongInt; var x,y:LongInt): LongInt;

{sets mouse's x range, with Min and Max resp. the higest and the lowest
 column (in pixels) in between which the mouse cursor can move}
  Procedure SetMouseXRange (Min,Max:LongInt);

{sets mouse's y range, with Min and Max resp. the higest and the lowest
 row (in pixels) in between which the mouse cursor can move}
  Procedure SetMouseYRange (Min,Max:LongInt);

{set the window coordinates in which the mouse cursor can move}
  Procedure SetMouseWindow(x1,y1,x2,y2:LongInt);

{sets the mouse shape in text mode: background and foreground color and the
 Ascii value with which the character on screen is XOR'ed when the cursor
 moves over it. Set to 0 for a "transparent" cursor}
  Procedure SetMouseShape(ForeColor,BackColor,Ascii:Byte);

{sets the mouse ascii in text mode. The difference between this one and
 SetMouseShape, is that the foreground and background colors stay the same
 and that the Ascii code you enter is the character that you will get on
 screen; there's no XOR'ing}
  Procedure SetMouseAscii(Ascii:Byte);

{set mouse speed in mickey's/pixel; default: horizontal: 8; vertical: 16}
  Procedure SetMouseSpeed(Horizontal ,Vertical:LongInt);

{set a rectangle on screen that mouse will disappear if it is moved into}
  Procedure SetMouseHideWindow(x1,y1,x2,y2:LongInt);

Const LButton = 1; {left button}
      RButton = 2; {right button}
      MButton = 4; {middle button}

Var
  MouseFound: Boolean;

Implementation


Function InitMouse: Boolean;
begin
  if MemL[0:$33*4] = 0 then
    exit(False);
  asm
        xor     ax,ax
        push    bp
        int     $33
        pop     bp
        cmp     ax, $FFFF
        mov     al, 0
        jne     @@1
        inc     al
@@1:
        mov     @Result, al
  end;
end;


Procedure ShowMouse;
begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 1
        push    bp
        int     $33
        pop     bp
  end;
end;

Procedure HideMouse;
begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 2
        push    bp
        int     $33
        pop     bp
  end;
end;

Procedure GetMouseState(var x,y,buttons:LongInt);
begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 3
        push    bp
        int     $33
        pop     bp
{$if defined(FPC_MM_TINY) or defined(FPC_MM_SMALL) or defined(FPC_MM_MEDIUM)}
        mov     di, x
        mov     [di], cx
        mov     word [di + 2], 0
        mov     di, y
        mov     [di], dx
        mov     word [di + 2], 0
        mov     di, buttons
        mov     [di], bx
        mov     word [di + 2], 0
{$else}
        mov     ax, ds
        lds     di, x
        mov     [di], cx
        mov     word [di + 2], 0
        lds     di, y
        mov     [di], dx
        mov     word [di + 2], 0
        lds     di, buttons
        mov     [di], bx
        mov     word [di + 2], 0
        mov     ds, ax
{$endif}
  end;
end;

Function LPressed:Boolean;
Begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 3
        push    bp
        int     $33
        pop     bp
        mov     ax,bx
        and     ax, 1
        mov     @Result, al
  end;
end;

Function RPressed:Boolean;
Begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 3
        push    bp
        int     $33
        pop     bp
        mov     ax,bx
        shr     ax,1
        and     ax,1
        mov     @Result, al
  end;
end;

Function MPressed:Boolean;
Begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 3
        push    bp
        int     $33
        pop     bp
        mov     ax, bx
        shr     ax, 1
        shr     ax, 1
        and     ax, 1
        mov     @Result, al
  end;
end;

Procedure SetMousePos(x,y:LongInt);
Begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 4
        mov     cx, x
        mov     dx, y
        push    bp
        int     $33
        pop     bp
  End;
End;

Function GetLastButtonPress(Button: LongInt;var x,y:LongInt):LongInt;
Begin
  If (Not MouseFound) Then Exit;
  GetLastButtonPress := 0;
  asm
        mov     ax, 5
        mov     bx, button
        shr     bx, 1        {0 = left, 1 = right, 2 = middle}
        push    bp
        int     $33
        pop     bp
        mov     @Result, bx
{$if defined(FPC_MM_TINY) or defined(FPC_MM_SMALL) or defined(FPC_MM_MEDIUM)}
        mov     di, x
        mov     [di], cx
        mov     word [di + 2], 0
        mov     di, y
        mov     [di], dx
        mov     word [di + 2], 0
{$else}
        mov     si, ds
        lds     di, x
        mov     [di], cx
        mov     word [di + 2], 0
        lds     di, y
        mov     [di], dx
        mov     word [di + 2], 0
        mov     ds, si
{$endif}
  end;
end;

Function GetLastButtonRelease (button : LongInt; var x,y:LongInt): LongInt;
begin
  If (Not MouseFound) Then Exit;
  GetLastButtonRelease := 0;
  asm
        mov     ax, 6
        mov     bx, button
        shr     bx, 1        {0 = left, 1 = right, 2 = middle}
        push    bp
        int     $33
        pop     bp
        mov     @Result, bx
{$if defined(FPC_MM_TINY) or defined(FPC_MM_SMALL) or defined(FPC_MM_MEDIUM)}
        mov     di, x
        mov     [di], cx
        mov     word [di + 2], 0
        mov     di, y
        mov     [di], dx
        mov     word [di + 2], 0
{$else}
        mov     si, ds
        lds     di, x
        mov     [di], cx
        mov     word [di + 2], 0
        lds     di, y
        mov     [di], dx
        mov     word [di + 2], 0
        mov     ds, si
{$endif}
  end;
end;

Procedure SetMouseXRange (Min,Max:LongInt);
begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 7
        mov     cx, min
        mov     dx, max
        push    bp
        int     $33
        pop     bp
  end;
end;

Procedure SetMouseYRange (min,max:LongInt);
begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, 8
        mov     cx, min
        mov     dx, max
        push    bp
        int     $33
        pop     bp
  end;
end;

Procedure SetMouseWindow(x1,y1,x2,y2:LongInt);
Begin
  If (Not MouseFound) Then Exit;
  SetMouseXRange(x1,x2);
  SetMouseYRange(y1,y2);
End;

Procedure SetMouseShape(ForeColor,BackColor,Ascii:Byte);
Begin
  If (Not MouseFound) Then Exit;
  asm
        xor     bx, bx
        mov     ax, 10
        xor     dx, dx
        mov     dh, BackColor
        mov     cl, 4
        shl     dh, cl
        add     dh, ForeColor
        mov     dl, Ascii
        mov     cx, $ffff
        push    bp
        int     $33
        pop     bp
  End;
End;

Procedure SetMouseAscii(Ascii:byte);
Begin
  If (Not MouseFound) Then Exit;
  asm
        xor     bx, bx
        mov     ax, 10
        mov     cx, $ff00
        xor     dx,dx
        mov     dl, Ascii
        push    bp
        int     $33
        pop     bp
  End;
End;

Procedure SetMouseHideWindow(x1,y1,x2,y2:LongInt);
Begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, $0010
        mov     cx, x1
        mov     dx, y1
        mov     si, x2
        mov     di, y2
        push    bp
        int     $33
        pop     bp
  end;
End;

Procedure SetMouseSpeed(Horizontal,Vertical:LongInt);
Begin
  If (Not MouseFound) Then Exit;
  asm
        mov     ax, $0f
        mov     cx, Horizontal
        mov     dx, Vertical
        push    bp
        int     $33
        pop     bp
  end;
End;

Begin
  MouseFound := InitMouse;
End.
