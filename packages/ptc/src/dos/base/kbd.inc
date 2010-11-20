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

{Constructor TDosKeyboard.Create;

begin
end;

destructor TDosKeyboard.Destroy;

begin
  inherited Destroy;
End;}

procedure TDosKeyboard.GetPendingEvents(queue: TEventQueue);

var
  c: Integer;
  Ch, Ex: Char;

begin
  while KeyPressed do
  begin
    Ch := ReadKey;
    if Ch = #0 then
      Ex := ReadKey
    else
      Ex := #0;
    if Ch <> #0 then
    begin
      Ch := UpCase(Ch);
      c := Ord(Ch);
    end
    else
    begin
      case Ord(Ex) of
        59: c := PTCKEY_F1;
        60: c := PTCKEY_F2;
        61: c := PTCKEY_F3;
        62: c := PTCKEY_F4;
        63: c := PTCKEY_F5;
        64: c := PTCKEY_F6;
        65: c := PTCKEY_F7;
        66: c := PTCKEY_F8;
        67: c := PTCKEY_F9;
        68: c := PTCKEY_F10;
        71: c := PTCKEY_HOME;
        72: c := PTCKEY_UP;
        73: c := PTCKEY_PAGEUP;
        75: c := PTCKEY_LEFT;
        76: c := PTCKEY_NUMPAD5;
        77: c := PTCKEY_RIGHT;
        79: c := PTCKEY_END;
        80: c := PTCKEY_DOWN;
        81: c := PTCKEY_PAGEDOWN;
        82: c := PTCKEY_INSERT;
        83: c := PTCKEY_DELETE;
        133: c := PTCKEY_F11;
        134: c := PTCKEY_F12;
      end;
    end;
    queue.AddEvent(TPTCKeyEvent.Create(c, False, False, False, True));
    queue.AddEvent(TPTCKeyEvent.Create(c, False, False, False, False));
  end;
end;
