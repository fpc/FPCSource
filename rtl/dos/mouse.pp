{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
Unit Mouse;
Interface

{
  Mouse support functions and procedures,with error checking if mouse
  isn't present then the routine ends,if you want to remove error checking
  remove the next define.
}

{$DEFINE MOUSECHECK}

{check if mouse is present and sets the mouse variable}
  Function Check_Mouse:Boolean;
{shows mouse pointer,text+graphics screen support}
  Procedure Show_Mouse;
{hides mouse pointer}
  Procedure Hide_Mouse;
{reads mouse position in pixels,divide by 8 to get text position,and reads
  buttons state(1-left button,2=right button,7=middle button)}
  Procedure read_mouse (var x,y:Longint;var buttons:Longint);
{sets mouse pointer in text mode}
  Procedure Mouse_Cur(X,Y:Longint);
{sets the mouse shape in text mode}
  Procedure Mouse_Shape(BackColor,ForColor,Ascii:LongInt);
{sets the mouse ascii in text mode}
  Procedure Mouse_Ascii(Ascii:LongInt);
{returns which button was pressed after last call to function}
  Function mouse_press(var x,y:Longint;button:Longint):Longint;
{returns which button was realeased after last call to function}
  Function mouse_release (var x,y:Longint;button:Longint):integer;
{set's mouse y range}
  Procedure mouse_yrange (min,max:Longint);
{set's mouse y range}
  Procedure mouse_xrange (min,max:Longint);
{set mouse speed}
  Procedure Micky(Horizontal ,Vertical:Longint);
{set rectangle on screen that mouse will disappear if will point on it}
  Procedure Unseen_Mouse(x1,y1,x2,y2:Longint);
{return if right button pressed}
  Function IsRPressed:Boolean;
{return if left button pressed}
  Function IsLPressed:Boolean;
{return if middle button pressed}
  Function IsMPressed:Boolean;
{return mouse X coordinate in textmode}
  Function MouseX:longint;
{return mouse Y coordinate in textmode}
  Function MouseY:longint;
{return which mouse buttons are pressed, only in bit 0-2}
  Function MouseButtons:longint;
{set window for mouse}
  Procedure MWindow(x1,y1,x2,y2:Longint);

Var
  MouseFound:Boolean;

Implementation

{$I386_ATT}

Function Check_Mouse:Boolean;
begin
  asm
        xorl    %eax,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        cmpw    $0xffff,%ax
        setz    %al
        movb    %al,MouseFound
        movb    %al,__RESULT
  end;
end;


procedure show_mouse;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $1,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
  end;
end;

procedure hide_mouse;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $2,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
  end;
end;

procedure read_mouse (var x,y,buttons:Longint);
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        andl    $0xffff,%ecx
        andl    $0xffff,%edx
        movl    x,%eax
        movl    %ecx,(%eax)
        movl    y,%eax
        movl    %edx,(%eax)
        movl    buttons,%eax
        movw    %bx,(%eax)
  end;
end;

function mouse_press(var x,y:Longint;button:Longint):Longint;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $5,%eax
        movl    button,%ebx
        pushl   %ebp
        int     $0x33
        popl    %ebp
        andl    $0xffff,%ecx
        andl    $0xffff,%edx
        movl    x,%ebx
        movl    %ecx,(%ebx)
        movl    y,%ebx
        movl    %edx,(%ebx)
        movl    %eax,__RESULT
  end;
end;

function mouse_release (var x,y:Longint;button : Longint):integer;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $6,%eax
        movl    button,%ebx
        pushl   %ebp
        int     $0x33
        popl    %ebp
        andl    $0xffff,%ecx
        andl    $0xffff,%edx
        movl    x,%ebx
        movl    %ecx,(%ebx)
        movl    y,%ebx
        movl    %edx,(%ebx)
        movl    %eax,__RESULT
  end;
end;

procedure mouse_yrange (min,max:Longint);
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $8,%eax
        movl    min,%ecx
        movl    max,%edx
        pushl   %ebp
        int     $0x33
        popl    %ebp
  end;
end;

procedure mouse_xrange (min,max:Longint);
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $7,%eax
        movl    min,%ecx
        movl    max,%edx
        pushl   %ebp
        int     $0x33
        popl    %ebp
  end;
end;

Procedure Mouse_Cur(X,Y:Longint);
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $4,%eax
        movl    X,%ecx
        movl    Y,%edx
        shll    $3,%ecx
        shll    $3,%edx
        pushl   %ebp
        int     $0x33
        popl    %ebp
  End;
End;

Procedure Mouse_Shape(BackColor,ForColor,Ascii:LongInt);
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        xorl    %ebx,%ebx
        movl    $0xa,%eax
        movl    $0xff,%ecx
        xorl    %edx,%edx
        movb    8(%ebp),%dh
        shlb    $4,%dh
        addb    ForColor,%dh
        pushl   %ebp
        int     $0x33
        popl    %ebp
  End;
End;

Procedure Mouse_Ascii(Ascii:LongInt);
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        xorl    %ebx,%ebx
        movl    $0xa,%eax
        movl    $0xff00,%ecx
        xorl    %edx,%edx
        movb    8(%ebp),%dl
        pushl   %ebp
        int     $0x33
        popl    %ebp
  End;
End;

Procedure Unseen_Mouse(x1,y1,x2,y2:Longint);
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $0x0010,%eax
        movl    x1,%ecx
        movl    y1,%edx
        movl    x2,%esi
        movl    y2,%edi
        pushl   %ebp
        int     $0x33
        popl    %ebp
  end;
End;

Procedure Micky(Horizontal ,Vertical:Longint);
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $0x0f,%eax
        movl    Horizontal,%ecx
        movl    Vertical,%edx
        pushl   %ebp
        int     $0x33
        popl    %ebp
  end;
End;

Function IsRPressed:Boolean;
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        movl    %ebx,%eax
        shrl    $1,%eax
        andl    $1,%eax
        movb    %al,__RESULT
  end;
end;

Function IsLPressed:Boolean;
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        movl    %ebx,%eax
        andl    $1,%eax
        movb    %al,__RESULT
  end;
end;

Function IsMPressed:Boolean;
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        movl    %ebx,%eax
        shrl    $2,%eax
        andl    $1,%eax
        movb    %al,__RESULT
  end;
end;

function MouseX:longint;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        movzwl  %cx,%eax
        shrl    $3,%eax
        incl    %eax
        movl    %eax,__RESULT
  end;
end;

function MouseY:longint;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        movzwl  %dx,%eax
        shrl    $3,%eax
        incl    %eax
        movl    %eax,__RESULT
  end;
end;

function MouseButtons:longint;
begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  asm
        movl    $3,%eax
        pushl   %ebp
        int     $0x33
        popl    %ebp
        movl    %ebx,%eax
        andl    $7,%eax
        movl    %eax,__RESULT
  end;
end;

Procedure MWindow(x1,y1,x2,y2:Longint);
Begin
{$IFDEF MOUSECHECK}
  If (Not MouseFound) Then Exit;
{$ENDIF}
  mouse_xrange(x1,x2);
  mouse_yrange(y1,y2);
End;

Begin
  Check_Mouse;
End.
{
  $Log$
  Revision 1.3  1998-04-05 13:56:54  peter
    - fixed mouse to compile with $i386_att
    + linux crt supports redirecting (not Esc-codes anymore)

  Revision 1.2  1998/03/26 12:25:22  peter
    * integrated both mouse units

  Revision 1.1.1.1  1998/03/25 11:18:41  root
  * Restored version

  Revision 1.4  1998/03/24 15:53:12  peter
    * cleanup and doesn't give warnings when compiling

  Revision 1.3  1998/01/26 11:56:24  michael
  + Added log at the end

  Revision 1.2
  date: 1997/12/01 12:15:45;  author: michael;  state: Exp;  lines: +14 -12
  + added copyright reference in header.

  Revision 1.1
  date: 1997/11/27 08:33:49;  author: michael;  state: Exp;
  Initial revision

  Revision 1.1.1.1
  date: 1997/11/27 08:33:49;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
}
