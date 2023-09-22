{
    Free Pascal PTCPas framebuffer library single threaded wrapper
    Copyright (C) 2010, 2011, 2012, 2013, 2019, 2021 Nikolay Nikolov (nickysn@users.sourceforge.net)

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$IFNDEF FPC_DOTTEDUNITS}
unit ptcwrapper;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, Ptc, PTC.EventQueue;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, ptc, ptceventqueue;
{$ENDIF FPC_DOTTEDUNITS}

type
  TPTCWrapperThread = class
  private
    FTimer: IPTCTimer;
    FLastUpdateTime: Real;
    FConsole: IPTCConsole;
    FSurface: array of IPTCSurface;
    FPalette: IPTCPalette;
    FNeedsUpdate: Boolean;
    FPaletteNeedsUpdate: Boolean;
    FCurrentVisualPage: Integer;

    FEventQueue: TEventQueue;

    FPixels: array of Pointer;
    FPaletteData: Pointer;

    FOpen: Boolean;
    FLocked: Boolean;
    FPaletteLocked: Boolean;
    {FError?}

    procedure GetEvents;
    procedure MaybeUpdate;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate;
    procedure WaitFor;

    procedure Open(const ATitle: AnsiString; AVirtualPages: Integer; APages: Integer = 0);
    procedure Open(const ATitle: AnsiString; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
    procedure Open(const ATitle: AnsiString; ASurfaceWidth, ASurfaceHeight, AWidth, AHeight: Integer; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
    procedure Open(const ATitle: AnsiString; ASurfaceWidth, ASurfaceHeight: Integer; AMode: IPTCMode; AVirtualPages: Integer; APages: Integer = 0);
    procedure Close;

    function Option(const AOption: AnsiString): Boolean;

    function Modes: TPTCModeList;

    procedure SetVisualPage(AVisualPage: Integer);

    function Lock(AVirtualPage: Integer): Pointer;
    procedure Unlock;

    function PaletteLock: Pointer;
    procedure PaletteUnlock;

    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;

    function MoveMouseTo(AX, AY: Integer): Boolean;

    property IsOpen: Boolean read FOpen;
  end;

implementation

constructor TPTCWrapperThread.Create;
begin
  FOpen := False;
  FNeedsUpdate := False;

  FTimer := TPTCTimerFactory.CreateNew;
  FTimer.Start;
  FLastUpdateTime := -1000;

  FConsole := TPTCConsoleFactory.CreateNew;
  FConsole.Option('intercept window close');

  FEventQueue := TEventQueue.Create;
  FPalette := TPTCPaletteFactory.CreateNew;
  FPaletteData := FPalette.Data;

  FOpen := False;

  inherited Create;
end;

destructor TPTCWrapperThread.Destroy;
begin
  FOpen := False;

  FreeAndNil(FEventQueue);

  if Assigned(FConsole) then
    FConsole.Close;

  SetLength(FSurface, 0);
  FConsole := nil;

  inherited;
end;

procedure TPTCWrapperThread.GetEvents;
var
  Event: IPTCEvent;
  NextEventAvailable: Boolean;
begin
  repeat
    NextEventAvailable := FConsole.NextEvent(Event, False, PTCAnyEvent);
    if NextEventAvailable then
      FEventQueue.AddEvent(Event);
  until not NextEventAvailable;
end;

procedure TPTCWrapperThread.MaybeUpdate;
const
  MinFrameTime = 1/60;
begin
  if FOpen then
  begin
    GetEvents;

    if (not FLocked) and (not FPaletteLocked) and (FNeedsUpdate or FPaletteNeedsUpdate) and
       ((FTimer.Time - FLastUpdateTime) > MinFrameTime) then
    begin
      if FPaletteNeedsUpdate then
        FSurface[FCurrentVisualPage].Palette(FPalette);
      FSurface[FCurrentVisualPage].Copy(FConsole);
      if FPaletteNeedsUpdate then
        FConsole.Palette(FPalette);
      FConsole.Update;

      FNeedsUpdate := False;
      FPaletteNeedsUpdate := False;

      FLastUpdateTime := FTimer.Time;
    end;
  end;
end;

procedure TPTCWrapperThread.Open(const ATitle: AnsiString; AVirtualPages: Integer; APages: Integer = 0);
var
  I: Integer;
begin
  SetLength(FSurface, AVirtualPages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := nil;
  FConsole.Open(ATitle, APages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := TPTCSurfaceFactory.CreateNew(FConsole.Width, FConsole.Height, FConsole.Format);
  SetLength(FPixels, Length(FSurface));
  for I := Low(FSurface) to High(FSurface) do
  begin
    FPixels[I] := FSurface[I].Lock;
    FSurface[I].Unlock;
  end;
  FOpen := True;
end;

procedure TPTCWrapperThread.Open(const ATitle: AnsiString; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
var
  I: Integer;
begin
  SetLength(FSurface, AVirtualPages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := nil;
  FConsole.Open(ATitle, AFormat, APages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := TPTCSurfaceFactory.CreateNew(FConsole.Width, FConsole.Height, AFormat);
  SetLength(FPixels, Length(FSurface));
  for I := Low(FSurface) to High(FSurface) do
  begin
    FPixels[I] := FSurface[I].Lock;
    FSurface[I].Unlock;
  end;
  FOpen := True;
end;

procedure TPTCWrapperThread.Open(const ATitle: AnsiString; ASurfaceWidth, ASurfaceHeight, AWidth, AHeight: Integer; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
var
  I: Integer;
begin
  SetLength(FSurface, AVirtualPages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := nil;
  FConsole.Open(ATitle, AWidth, AHeight, AFormat, APages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := TPTCSurfaceFactory.CreateNew(ASurfaceWidth, ASurfaceHeight, AFormat);
  SetLength(FPixels, Length(FSurface));
  for I := Low(FSurface) to High(FSurface) do
  begin
    FPixels[I] := FSurface[I].Lock;
    FSurface[I].Unlock;
  end;
  FOpen := True;
end;

procedure TPTCWrapperThread.Open(const ATitle: AnsiString; ASurfaceWidth, ASurfaceHeight: Integer; AMode: IPTCMode; AVirtualPages: Integer; APages: Integer = 0);
var
  I: Integer;
begin
  SetLength(FSurface, AVirtualPages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := nil;
  FConsole.Open(ATitle, AMode, APages);
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := TPTCSurfaceFactory.CreateNew(ASurfaceWidth, ASurfaceHeight, AMode.Format);
  SetLength(FPixels, Length(FSurface));
  for I := Low(FSurface) to High(FSurface) do
  begin
    FPixels[I] := FSurface[I].Lock;
    FSurface[I].Unlock;
  end;
  FOpen := True;
end;

procedure TPTCWrapperThread.Close;
var
  I: Integer;
begin
  FConsole.Close;
  for I := Low(FSurface) to High(FSurface) do
    FSurface[I] := nil;
  SetLength(FSurface, 0);
  SetLength(FPixels, 0);
  FOpen := False;
end;

function TPTCWrapperThread.Option(const AOption: AnsiString): Boolean;
begin
  Result := FConsole.Option(AOption);
end;

function TPTCWrapperThread.Modes: TPTCModeList;
begin
  Result := FConsole.Modes;
end;

procedure TPTCWrapperThread.SetVisualPage(AVisualPage: Integer);
begin
  if FCurrentVisualPage <> AVisualPage then
  begin
    FCurrentVisualPage := AVisualPage;
    FNeedsUpdate := True;
    FPaletteNeedsUpdate := True;  { todo: no need to set this always }
  end;
  MaybeUpdate;
end;

function TPTCWrapperThread.Lock(AVirtualPage: Integer): Pointer;
begin
  FLocked := True;
  if AVirtualPage = FCurrentVisualPage then
    FNeedsUpdate := True;
  Result := FPixels[AVirtualPage];
end;

procedure TPTCWrapperThread.Unlock;
begin
  FLocked := False;
  MaybeUpdate;
end;

function TPTCWrapperThread.PaletteLock: Pointer;
begin
  FPaletteLocked := True;
  FPaletteNeedsUpdate := True;
  Result := FPaletteData;
end;

procedure TPTCWrapperThread.PaletteUnlock;
begin
  FPaletteLocked := False;
  MaybeUpdate;
end;

function TPTCWrapperThread.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  repeat
    MaybeUpdate;

    AEvent := FEventQueue.NextEvent(AEventMask);
  until (not AWait) or (AEvent <> nil);
  Result := AEvent <> nil;
end;

function TPTCWrapperThread.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
begin
  repeat
    MaybeUpdate;

    Result := FEventQueue.PeekEvent(AEventMask);
  until (not AWait) or (Result <> nil);
end;

function TPTCWrapperThread.MoveMouseTo(AX, AY: Integer): Boolean;
begin
  Result := FConsole.MoveMouseTo(AX, AY);
end;

procedure TPTCWrapperThread.Terminate;
begin
end;

procedure TPTCWrapperThread.WaitFor;
begin
end;

end.
