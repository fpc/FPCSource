{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2010 by Nikolay Nikolov (nickysn@users.sourceforge.net)

    This file implements keyboard input support for ptcgraph

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ptccrt;

{$MODE objfpc}
{$DEFINE HasCRT}

{$IFDEF WinCE}
  {$UNDEF HasCRT}
{$ENDIF WinCE}

interface

{$IFDEF HasCRT}
uses
  crt;
{$ENDIF HasCRT}

type
{$IFDEF HasCRT}
  tcrtcoord = crt.tcrtcoord;
{$ELSE HasCRT}
  tcrtcoord = 1..255;
{$ENDIF HasCRT}

var
  DirectVideo: Boolean {$IFDEF HasCRT}absolute crt.DirectVideo{$ENDIF HasCRT};
  TextAttr: Byte {$IFDEF HasCRT}absolute crt.TextAttr{$ENDIF HasCRT};

function KeyPressed: Boolean;
function ReadKey: Char;
procedure ClrScr;
procedure ClrEol;
procedure GotoXY(X, Y: tcrtcoord);
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure Delay(MS: Word);
procedure Sound(HZ: Word);
procedure NoSound;

implementation

uses
  ptcgraph, ptc, ptcwrapper
  {$IFDEF UNIX}
  , baseunix
  {$ENDIF UNIX}
  {$IF defined(Win32) or defined(Win64) or defined(WinCE)}
  , windows
  {$ENDIF defined(Win32) or defined(Win64) or defined(WinCE)}
  ;

function InGraphMode: Boolean;
begin
  Result := (PTCWrapperObject <> nil) and (PTCWrapperObject.IsOpen);
end;

var
  KeyBuffer: array[0..64] of Char;
  KeyBufHead, KeyBufTail: Integer;

function KeyBufEmpty: Boolean;
begin
  Result := KeyBufHead = KeyBufTail;
end;

procedure KeyBufAdd(Ch: Char);
begin
  { do nothing, if the buffer is full }
  if ((KeyBufTail + 1) = KeyBufHead) or
     ((KeyBufTail = High(KeyBuffer)) and (KeyBufHead = Low(KeyBuffer))) then
    exit;
  KeyBuffer[KeyBufTail] := Ch;
  Inc(KeyBufTail);
  if KeyBufTail > High(KeyBuffer) then
    KeyBufTail := Low(KeyBuffer);
end;

procedure KeyBufAdd(S: String);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    KeyBufAdd(S[I]);
end;

function KeyBufGet: Char;
begin
  if KeyBufHead <> KeyBufTail then
  begin
    Result := KeyBuffer[KeyBufHead];
    Inc(KeyBufHead);
    if KeyBufHead > High(KeyBuffer) then
      KeyBufHead := Low(KeyBuffer);
  end;
end;

procedure GetKeyEvents;
var
  ev: IPTCEvent;
  KeyEv: IPTCKeyEvent;
begin
  repeat
    PTCWrapperObject.NextEvent(ev, False, [PTCKeyEvent]);
    if ev <> nil then
    begin
      KeyEv := ev as IPTCKeyEvent;
      if KeyEv.Press then
      begin
        case KeyEv.Code of
          PTCKEY_BACKSPACE:
            if KeyEv.Control then
              KeyBufAdd(#127)
            else
              KeyBufAdd(#8);
          PTCKEY_ENTER:  KeyBufAdd(#13);
          PTCKEY_ESCAPE: KeyBufAdd(#27);
          PTCKEY_INSERT: KeyBufAdd(#0#82);
          PTCKEY_DELETE: KeyBufAdd(#0#83);
          PTCKEY_LEFT:   KeyBufAdd(#0#75);
          PTCKEY_UP:     KeyBufAdd(#0#72);
          PTCKEY_RIGHT:  KeyBufAdd(#0#77);
          PTCKEY_DOWN:   KeyBufAdd(#0#80);
          PTCKEY_HOME:     KeyBufAdd(#0#71);
          PTCKEY_END:      KeyBufAdd(#0#79);
          PTCKEY_PAGEUP:   KeyBufAdd(#0#73);
          PTCKEY_PAGEDOWN: KeyBufAdd(#0#81);
          PTCKEY_F1:     KeyBufAdd(#0#59);
          PTCKEY_F2:     KeyBufAdd(#0#60);
          PTCKEY_F3:     KeyBufAdd(#0#61);
          PTCKEY_F4:     KeyBufAdd(#0#62);
          PTCKEY_F5:     KeyBufAdd(#0#63);
          PTCKEY_F6:     KeyBufAdd(#0#64);
          PTCKEY_F7:     KeyBufAdd(#0#65);
          PTCKEY_F8:     KeyBufAdd(#0#66);
          PTCKEY_F9:     KeyBufAdd(#0#67);
          PTCKEY_F10:    KeyBufAdd(#0#68);
          else
            if (KeyEv.Unicode >= 32) and (KeyEv.Unicode <= 127) then
              KeyBufAdd(Chr(KeyEv.Unicode));
        end;
      end;
    end;
  until ev = nil;
end;

function KeyPressed: Boolean;
begin
  if not InGraphMode then
  begin
{$IFDEF HasCRT}
    Result := crt.KeyPressed
{$ELSE HasCRT}
    Result := False;
{$ENDIF HasCRT}
  end
  else
  begin
    GetKeyEvents;
    Result := not KeyBufEmpty;
  end;
end;

function ReadKey: Char;
{$IFDEF UNIX}
var
  req, rem: TTimeSpec;
{$ENDIF UNIX}
begin
  if not InGraphMode then
  begin
{$IFDEF HasCRT}
    Result := crt.ReadKey;
{$ELSE HasCRT}
    Result := #0;
{$ENDIF HasCRT}
  end
  else
  begin
    while not KeyPressed do
    begin
{$IFDEF UNIX}
      req.tv_sec := 0;
      req.tv_nsec := 1000000;
      fpnanosleep(@req, @rem);
{$ENDIF UNIX}
{$IF defined(Win32) or defined(Win64) or defined(WinCE)}
      Sleep(1);
{$ENDIF defined(Win32) or defined(Win64) or defined(WinCE)}
    end;
    Result := KeyBufGet;
  end;
end;

procedure ClrScr;
begin
{$IFDEF HasCRT}
  crt.ClrScr;
{$ENDIF HasCRT}
end;

procedure ClrEol;
begin
{$IFDEF HasCRT}
  crt.ClrEol;
{$ENDIF HasCRT}
end;

procedure GotoXY(X, Y: tcrtcoord);
begin
{$IFDEF HasCRT}
  crt.GotoXY(X, Y);
{$ENDIF HasCRT}
end;

procedure TextColor(Color: Byte);
begin
{$IFDEF HasCRT}
  crt.TextColor(Color);
{$ENDIF HasCRT}
end;

procedure TextBackground(Color: Byte);
begin
{$IFDEF HasCRT}
  crt.TextBackground(Color);
{$ENDIF HasCRT}
end;

procedure Delay(MS: Word);
begin
{$IFDEF HasCRT}
  crt.Delay(MS);
{$ENDIF HasCRT}
end;

procedure Sound(HZ: Word);
begin
{$IFDEF HasCRT}
  crt.Sound(HZ);
{$ENDIF HasCRT}
end;

procedure NoSound;
begin
{$IFDEF HasCRT}
  crt.NoSound;
{$ENDIF HasCRT}
end;

end.
