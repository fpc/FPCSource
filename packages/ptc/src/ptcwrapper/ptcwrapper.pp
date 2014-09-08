{
    Free Pascal PTCPas framebuffer library threaded wrapper
    Copyright (C) 2010, 2011, 2012, 2013 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

unit ptcwrapper;

{$MODE objfpc}{$H+}

{$IFDEF GO32V2}
{ this unit does not work under go32v2, because DOS does not support threads,
  but this hack at least makes it compile, so we don't need to exclude it from
  the makefiles for now. }
{$DEFINE NoSyncObjsHack}
{$ENDIF GO32V2}

interface

uses
  {$IFDEF unix}cthreads,{$ENDIF}
  SysUtils, Classes{$IFNDEF NoSyncObjsHack}, syncobjs{$ENDIF}, ptc, ptceventqueue;

type
{$IFDEF NoSyncObjsHack}
  TCriticalSection = class
  public
    procedure Acquire;
    procedure Release;
  end;
{$ENDIF NoSyncObjsHack}
  TPTCWrapperOpenType = (pwotDefault, pwotFormat, pwotWidthHeightFormat, pwotMode);
  TPTCWrapperOpenRequest = record
    OpenType: TPTCWrapperOpenType;
    Title: string;
    SurfaceWidth, SurfaceHeight: Integer;
    Width, Height: Integer;
    Format: IPTCFormat;
    Mode: IPTCMode;
    VirtualPages: Integer;
    Pages: Integer;

    Processed: Boolean;
    Success: Boolean;
  end;

  TPTCWrapperCloseRequest = record
    Processed: Boolean;
    Success: Boolean;
  end;

  TPTCWrapperOptionRequest = record
    Option: string;

    Processed: Boolean;
    Result: Boolean;
  end;

  TPTCWrapperGetModesRequest = record
    Processed: Boolean;

    Result: TPTCModeList;
  end;

  TPTCWrapperThread = class(TThread)
  private
    FConsole: IPTCConsole;
    FSurface: array of IPTCSurface;
    FPalette: IPTCPalette;
    FSurfaceCriticalSection: TCriticalSection;
    FNeedsUpdate: Boolean;
    FPaletteNeedsUpdate: Boolean;
    FCurrentVisualPage: Integer;

    FEventQueue: TEventQueue;

    FPixels: array of Pointer;
    FPaletteData: Pointer;

    FOpen: Boolean;
    {FError?}

    FOpenRequest: TPTCWrapperOpenRequest;
    FCloseRequest: TPTCWrapperCloseRequest;
    FOptionRequest: TPTCWrapperOptionRequest;
    FGetModesRequest: TPTCWrapperGetModesRequest;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open(const ATitle: string; AVirtualPages: Integer; APages: Integer = 0);
    procedure Open(const ATitle: string; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
    procedure Open(const ATitle: string; ASurfaceWidth, ASurfaceHeight, AWidth, AHeight: Integer; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
    procedure Open(const ATitle: string; ASurfaceWidth, ASurfaceHeight: Integer; AMode: IPTCMode; AVirtualPages: Integer; APages: Integer = 0);
    procedure Close;

    function Option(const AOption: string): Boolean;

    function Modes: TPTCModeList;

    procedure SetVisualPage(AVisualPage: Integer);

    function Lock(AVirtualPage: Integer): Pointer;
    procedure Unlock;

    function PaletteLock: Pointer;
    procedure PaletteUnlock;

    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;

    property IsOpen: Boolean read FOpen;
  end;

implementation

{$IFDEF NoSyncObjsHack}
procedure TCriticalSection.Acquire;
begin
end;

procedure TCriticalSection.Release;
begin
end;
{$ENDIF NoSyncObjsHack}

constructor TPTCWrapperThread.Create;
begin
  FOpen := False;
  FNeedsUpdate := False;

  FOpenRequest.Processed := True;
  FCloseRequest.Processed := True;
  FOptionRequest.Processed := True;
  FGetModesRequest.Processed := True;

  FSurfaceCriticalSection := TCriticalSection.Create;

  inherited Create(False);
end;

destructor TPTCWrapperThread.Destroy;
begin
  inherited;
end;

procedure TPTCWrapperThread.Execute;
  procedure GetEvents;
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

  procedure ProcessRequests;
  var
    I: Integer;
  begin
    if not FOpenRequest.Processed then
    begin
      for I := Low(FSurface) to High(FSurface) do
        FSurface[I] := nil;
      with FOpenRequest do
      begin
        SetLength(FSurface, VirtualPages);
        case OpenType of
          pwotDefault:
            begin
              FConsole.Open(Title, Pages);
              for I := Low(FSurface) to High(FSurface) do
                FSurface[I] := TPTCSurfaceFactory.CreateNew(FConsole.Width, FConsole.Height, FConsole.Format);
            end;
          pwotFormat:
            begin
              FConsole.Open(Title, Format, Pages);
              for I := Low(FSurface) to High(FSurface) do
                FSurface[I] := TPTCSurfaceFactory.CreateNew(FConsole.Width, FConsole.Height, Format);
            end;
          pwotWidthHeightFormat:
            begin
              FConsole.Open(Title, Width, Height, Format, Pages);
              for I := Low(FSurface) to High(FSurface) do
                FSurface[I] := TPTCSurfaceFactory.CreateNew(SurfaceWidth, SurfaceHeight, Format);
            end;
          pwotMode:
            begin
              FConsole.Open(Title, Mode, Pages);
              for I := Low(FSurface) to High(FSurface) do
                FSurface[I] := TPTCSurfaceFactory.CreateNew(SurfaceWidth, SurfaceHeight, Mode.Format);
            end;
        end;
      end;

      SetLength(FPixels, Length(FSurface));
      for I := Low(FSurface) to High(FSurface) do
      begin
        FPixels[I] := FSurface[I].Lock;
        FSurface[I].Unlock;
      end;
      FOpen := True;
      FOpenRequest.Success := True;
      FOpenRequest.Processed := True;
    end;

    if not FCloseRequest.Processed then
    begin
      FConsole.Close;
      for I := Low(FSurface) to High(FSurface) do
        FSurface[I] := nil;
      SetLength(FSurface, 0);
      SetLength(FPixels, 0);
      FOpen := False;
      FCloseRequest.Success := True;
      FCloseRequest.Processed := True;
    end;

    if not FOptionRequest.Processed then
    begin
      FOptionRequest.Result := FConsole.Option(FOptionRequest.Option);
      FOptionRequest.Processed := True;
    end;

    if not FGetModesRequest.Processed then
    begin
      FGetModesRequest.Result := FConsole.Modes;
      FGetModesRequest.Processed := True;
    end;
  end;

begin
  try
    FConsole := TPTCConsoleFactory.CreateNew;
    FConsole.Option('intercept window close');

    FEventQueue := TEventQueue.Create;
    FPalette := TPTCPaletteFactory.CreateNew;
    FPaletteData := FPalette.Data;

    FOpen := False;
    while not Terminated do
    begin
      ThreadSwitch;
      Sleep(10);

      FSurfaceCriticalSection.Acquire;
      try
        ProcessRequests;

        if FOpen then
        begin
          GetEvents;

          if FNeedsUpdate or FPaletteNeedsUpdate then
          begin
            if FPaletteNeedsUpdate then
              FSurface[FCurrentVisualPage].Palette(FPalette);
            FSurface[FCurrentVisualPage].Copy(FConsole);
            if FPaletteNeedsUpdate then
              FConsole.Palette(FPalette);
            FConsole.Update;

            FNeedsUpdate := False;
            FPaletteNeedsUpdate := False;
          end;
        end;
      finally
        FSurfaceCriticalSection.Release;
      end;
    end;

  finally
    FOpen := False;

    FreeAndNil(FEventQueue);

    if Assigned(FConsole) then
      FConsole.Close;

    SetLength(FSurface, 0);
    FConsole := nil;
  end;
end;

procedure TPTCWrapperThread.Open(const ATitle: string; AVirtualPages: Integer; APages: Integer = 0);
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FOpenRequest do
    begin
      OpenType := pwotDefault;
      Title := ATitle;
      VirtualPages := AVirtualPages;
      Pages := APages;
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FOpenRequest.Processed;
end;

procedure TPTCWrapperThread.Open(const ATitle: string; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FOpenRequest do
    begin
      OpenType := pwotFormat;
      Title := ATitle;
      Format := AFormat;
      VirtualPages := AVirtualPages;
      Pages := APages;
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FOpenRequest.Processed;
end;

procedure TPTCWrapperThread.Open(const ATitle: string; ASurfaceWidth, ASurfaceHeight, AWidth, AHeight: Integer; AFormat: IPTCFormat; AVirtualPages: Integer; APages: Integer = 0);
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FOpenRequest do
    begin
      OpenType := pwotWidthHeightFormat;
      Title := ATitle;
      SurfaceWidth := ASurfaceWidth;
      SurfaceHeight := ASurfaceHeight;
      Width := AWidth;
      Height := AHeight;
      Format := AFormat;
      VirtualPages := AVirtualPages;
      Pages := APages;
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FOpenRequest.Processed;
end;

procedure TPTCWrapperThread.Open(const ATitle: string; ASurfaceWidth, ASurfaceHeight: Integer; AMode: IPTCMode; AVirtualPages: Integer; APages: Integer = 0);
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FOpenRequest do
    begin
      OpenType := pwotMode;
      Title := ATitle;
      SurfaceWidth := ASurfaceWidth;
      SurfaceHeight := ASurfaceHeight;
      Mode := AMode;
      VirtualPages := AVirtualPages;
      Pages := APages;
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FOpenRequest.Processed;
end;

procedure TPTCWrapperThread.Close;
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FCloseRequest do
    begin
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FCloseRequest.Processed;
end;

function TPTCWrapperThread.Option(const AOption: string): Boolean;
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FOptionRequest do
    begin
      Option := AOption;
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FOptionRequest.Processed;
  Result := FOptionRequest.Result;
end;

function TPTCWrapperThread.Modes: TPTCModeList;
begin
  FSurfaceCriticalSection.Acquire;
  try
    with FGetModesRequest do
    begin
      Processed := False;
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;

  repeat
    ThreadSwitch;
  until FGetModesRequest.Processed;
  Result := FGetModesRequest.Result;
end;

procedure TPTCWrapperThread.SetVisualPage(AVisualPage: Integer);
begin
  FSurfaceCriticalSection.Acquire;
  try
    if FCurrentVisualPage <> AVisualPage then
    begin
      FCurrentVisualPage := AVisualPage;
      FNeedsUpdate := True;
      FPaletteNeedsUpdate := True;  { todo: no need to set this always }
    end;
  finally
    FSurfaceCriticalSection.Release;
  end;
end;

function TPTCWrapperThread.Lock(AVirtualPage: Integer): Pointer;
begin
  FSurfaceCriticalSection.Acquire;
  if AVirtualPage = FCurrentVisualPage then
    FNeedsUpdate := True;
  Result := FPixels[AVirtualPage];
end;

procedure TPTCWrapperThread.Unlock;
begin
  FSurfaceCriticalSection.Release;
end;

function TPTCWrapperThread.PaletteLock: Pointer;
begin
  FSurfaceCriticalSection.Acquire;
  FPaletteNeedsUpdate := True;
  Result := FPaletteData;
end;

procedure TPTCWrapperThread.PaletteUnlock;
begin
  FSurfaceCriticalSection.Release;
end;

function TPTCWrapperThread.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  repeat
    ThreadSwitch;

    FSurfaceCriticalSection.Acquire;
    AEvent := FEventQueue.NextEvent(AEventMask);
    FSurfaceCriticalSection.Release;
  until (not AWait) or (AEvent <> nil);
  Result := AEvent <> nil;
end;

function TPTCWrapperThread.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
begin
  repeat
    ThreadSwitch;

    FSurfaceCriticalSection.Acquire;
    Result := FEventQueue.PeekEvent(AEventMask);
    FSurfaceCriticalSection.Release;
  until (not AWait) or (Result <> nil);
end;

end.
