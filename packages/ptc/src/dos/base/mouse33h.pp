{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit mouse33h;

{$MODE objfpc}

interface

procedure InitMouse;
procedure ShowCursor;
procedure HideCursor;
procedure ReadMouse(Out X, Y: Integer; Out Left, Right, Middle: Boolean);
procedure MoveMouseTo(const X, Y: Integer);
procedure SetHCursorRange(const MinX, MaxX: Integer);
procedure SetVCursorRange(const MinY, MaxY: Integer);
procedure SetCursorRange(const MinX, MinY, MaxX, MaxY: Integer);

var
  MousePresent: Boolean = False;

implementation

uses
  go32fix;

procedure InitMouse;

var
  RealRegs: TRealRegs;

begin
  { todo: check if INT 33 vector is neither 0000h:0000h nor points at an IRET instruction (BYTE CFh) }
  RealRegs.AX := $0000;
  realintr($33, RealRegs);
  MousePresent := RealRegs.AX = $FFFF;
  if MousePresent then
  begin
    { RealRegs.BX is number of buttons:
      according to ralf brown's interrupts list
      0000h other than two
      0002h two buttons (many drivers)
      0003h Mouse Systems/Logitech three-button mouse
      FFFFh two buttons }

    { ... }
  end;
end;

procedure ShowCursor;

var
  RealRegs: TRealRegs;

begin
  Assert(MousePresent);

  RealRegs.AX := $0001;
  realintr($33, RealRegs);
end;

procedure HideCursor;

var
  RealRegs: TRealRegs;

begin
  Assert(MousePresent);

  RealRegs.AX := $0002;
  realintr($33, RealRegs);
end;

procedure ReadMouse(Out X, Y: Integer; Out Left, Right, Middle: Boolean);

var
  RealRegs: TRealRegs;

begin
  Assert(MousePresent);

  RealRegs.AX := $0003;
  realintr($33, RealRegs);
  X := RealRegs.CX;
  Y := RealRegs.DX;
  Left := RealRegs.BX and 1 <> 0;
  Right := RealRegs.BX and 2 <> 0;
  Middle := RealRegs.BX and 4 <> 0;
end;

procedure MoveMouseTo(const X, Y: Integer);

var
  RealRegs: TRealRegs;

begin
  Assert(MousePresent);

  RealRegs.AX := $0004;
  RealRegs.CX := X;
  RealRegs.DX := Y;
  realintr($33, RealRegs);
end;

procedure SetHCursorRange(const MinX, MaxX: Integer);

var
  RealRegs: TRealRegs;

begin
  Assert(MousePresent);
  Assert(MinX <= MaxX);

  RealRegs.AX := $0007;
  RealRegs.CX := MinX;
  RealRegs.DX := MaxX;
  realintr($33, RealRegs);
end;

procedure SetVCursorRange(const MinY, MaxY: Integer);

var
  RealRegs: TRealRegs;

begin
  Assert(MousePresent);
  Assert(MinY <= MaxY);

  RealRegs.AX := $0008;
  RealRegs.CX := MinY;
  RealRegs.DX := MaxY;
  realintr($33, RealRegs);
end;

procedure SetCursorRange(const MinX, MinY, MaxX, MaxY: Integer);

begin
  SetHCursorRange(MinX, MaxX);
  SetVCursorRange(MinY, MaxY);
end;

end.
