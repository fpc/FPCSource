{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2011  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

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

constructor TDirectXDisplay.Create;
begin
  FInformation := '';
  FCursorsaved := False;
  FOpen := False;
  FFullscreen := False;
  FDDraw := nil;
  FWindow := 0;
//  FForeground := 0;
//  FillChar(FModes, SizeOf(FModes), 0);
//  FillChar(FResolutions, SizeOf(FResolutions), 0);
end;

destructor TDirectXDisplay.Destroy;
begin
  close;
  internal_dispose_modes;
  internal_dispose_resolutions;
  inherited Destroy;
end;

procedure TDirectXDisplay.internal_dispose_modes;
begin
  SetLength(FModes, 0);
  FModesCount := 0;
end;

procedure TDirectXDisplay.internal_dispose_resolutions;
begin
  SetLength(FResolutions, 0);
end;

function TDirectXDisplay_callback(descriptor: PDDSurfaceDesc {TODO: TDDSurfaceDesc vs. TDDSurfaceDesc2???}; Context: Pointer): HRESULT; stdcall;
var
  display: TDirectXDisplay;
  format: IPTCFormat;
begin
  if (descriptor = nil) or (Context = nil) then
  begin
    Result := DDENUMRET_CANCEL;
    exit;
  end;
  display := TDirectXDisplay(Context);
  if ((descriptor^.dwFlags and DDSD_WIDTH) <> 0) and
     ((descriptor^.dwFlags and DDSD_HEIGHT) <> 0) and
     ((descriptor^.dwFlags and DDSD_PIXELFORMAT) <> 0) then
  begin
    format := DirectXTranslate(descriptor^.ddpfPixelFormat);
    Inc(display.FModesCount);
    SetLength(display.FModes, display.FModesCount);
    display.FModes[display.FModesCount - 1] :=
      TPTCMode.Create(descriptor^.dwWidth, descriptor^.dwHeight, format);
  end
  else
  begin
    LOG('EnumDisplayModesCallback was not given enough mode information');
  end;
  Result := DDENUMRET_OK;
end;

procedure TDirectXDisplay.Setup(const ADDraw: IDirectDraw2);
var
  version: TOSVERSIONINFO;
  match, found: Boolean;
  i, j: Integer;
  tempMode: IPTCMode;
  format8: IPTCFormat;
begin
  LOG('setting up display lpDD2');
  FDDraw := ADDraw;
  FInformation := 'windows version x.xx.x' + #13 + #10 + 'ddraw version x.xx' + #13 + #10 + 'display driver name xxxxx' +
    #13 + #10 + 'display driver vendor xxxxx' + #13 + #10 + 'certified driver? x' + #13 + #10;
  internal_dispose_modes;
  DirectXCheck(FDDraw.EnumDisplayModes(0, nil, Self, {LPDDENUMMODESCALLBACK(}@TDirectXDisplay_callback{)}));
  version.dwOSVersionInfoSize := SizeOf(version);
  if GetVersionEx(version) then
  begin
    if version.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    begin
      LOG('detected windows 95/98');
      format8 := TPTCFormat.Create(8);
      found := False;
      for i := 0 to FModesCount - 1 do
        if (FModes[i].Width = 320) and (FModes[i].Height = 200) and
            FModes[i].Format.Equals(format8) then
          found := True;
      if not found then
      begin
        LOG('adding 320x200x8 to mode list');
	
	Inc(FModesCount);
        SetLength(FModes, FModesCount);
        FModes[FModesCount - 1] := TPTCMode.Create(320, 200, format8);
      end;
      found := False;
      for i := 0 to FModesCount - 1 do
        if (FModes[i].Width = 320) and (FModes[i].Height = 240) and
            FModes[i].Format.Equals(format8) then
          found := True;
      if not found then
      begin
        LOG('adding 320x240x8 to mode list');

	Inc(FModesCount);
        SetLength(FModes, FModesCount);
        FModes[FModesCount - 1] := TPTCMode.Create(320, 240, format8);
      end;
    end
    else
      if version.dwPlatformId = VER_PLATFORM_WIN32_NT then
      begin
        LOG('detected windows nt');
      end;
  end;
  LOG('number of display modes', FModesCount);

  internal_dispose_resolutions;
  for i := 0 to FModesCount - 1 do
  begin
    match := False;
    for j := 0 to FResolutionsCount - 1 do
      if (FModes[i].Width = FResolutions[j].Width) and
         (FModes[i].Height = FResolutions[j].Height) then
      begin
        match := True;
        Break;
      end;
    if not match then
    begin
      Inc(FResolutionsCount);
      SetLength(FResolutions, FResolutionsCount);
      FResolutions[FResolutionsCount - 1] := FModes[i];
    end;
  end;

  { kludge sort... :) }
  for i := 0 to FResolutionsCount - 1 do
    for j := i + 1 to FResolutionsCount - 1 do
      if (FResolutions[i].Width > FResolutions[j].Width) or
         (FResolutions[i].Height > FResolutions[j].Height) then
      begin
        tempMode := FResolutions[i];
        FResolutions[i] := FResolutions[j];
        FResolutions[j] := tempMode;
      end;
  LOG('number of unique resolutions', FResolutionsCount);
  for i := 0 to FResolutionsCount - 1 do
  begin
    LOG(IntToStr(FResolutions[i].Width) + ' x ' + IntToStr(FResolutions[i].Height));
  end;
end;

function TDirectXDisplay.GetModes: TPTCModeList;
begin
  Result := FModes;
end;

function TDirectXDisplay.Test(const AMode: IPTCMode; AExact: Boolean): Boolean;
var
  i: Integer;
begin
  if FModesCount = 0 then
  begin
    LOG('mode test success with empty mode list');
    exit(True);
  end;
  if AExact then
  begin
    for i := 0 to FModesCount - 1 do
      if FModes[i].Equals(AMode) then
      begin
        LOG('test exact mode success');
        exit(True);
      end;
    LOG('test exact mode failure');
    Result := False;
  end
  else
  begin
    for i := 0 to FResolutionsCount - 1 do
      if (AMode.Width <= FResolutions[i].Width) and
         (AMode.Height <= FResolutions[i].Height) then
      begin
        LOG('test nearest mode success');
        exit(True);
      end;
    LOG('test nearest mode failure');
    Result := False;
  end;
end;

procedure TDirectXDisplay.Cooperative(AWindow: HWND; AFullscreen: Boolean);
begin
  if AFullscreen then
  begin
    LOG('entering exclusive mode');
    DirectXCheck(FDDraw.SetCooperativeLevel(AWindow, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN or DDSCL_ALLOWMODEX));
  end
  else
  begin
    LOG('entering normal cooperative mode');
    DirectXCheck(FDDraw.SetCooperativeLevel(AWindow, DDSCL_NORMAL));
  end;
  FWindow := AWindow;
  FFullscreen := AFullscreen;
end;

procedure TDirectXDisplay.Open;
begin
  FreeAndNil(FMode);
  FMode := TPTCMode.Create;
  FOpen := True;
  LOG('opening windowed display');
end;

procedure TDirectXDisplay.Open(const AMode: IPTCMode; AExact: Boolean; AFrequency: Integer);
begin
  if AExact then
  begin
    LOG('opening exact fullscreen display mode');
    internal_open(AMode, True, AFrequency);
  end
  else
  begin
    LOG('opening nearest fullscreen mode');
    internal_open_nearest(AMode, False, AFrequency);
  end;
  LOG('successfully opened fullscreen display mode');
end;

procedure TDirectXDisplay.Close;
begin
  if FOpen and (FDDraw <> Nil) then
  begin
    LOG('closing display');
    if FFullscreen then
    begin
      LOG('restoring display mode');
      FDDraw.RestoreDisplayMode;
      LOG('leaving exclusive mode');
      FDDraw.SetCooperativeLevel(FWindow, DDSCL_NORMAL);
    end;
  end;
  FOpen := False;
  FFullscreen := False;
end;

procedure TDirectXDisplay.Save;
var
  p: TPOINT;
begin
  LOG('saving desktop');

  FCursorSaved := GetCursorPos(p);
  FCursorX := p.x;
  FCursorY := p.y;

{  m_foreground := GetForegroundWindow;
  GetWindowRect(m_foreground, m_foreground_rect);
  m_foreground_placement.length := SizeOf(WINDOWPLACEMENT);
  GetWindowPlacement(m_foreground, m_foreground_placement);}
end;

procedure TDirectXDisplay.restore;
begin
{  if (m_foreground <> 0) and IsWindow(m_foreground) and (m_foreground <> m_window) then
  Begin}
  LOG('restoring desktop');
  if IsWindow(FWindow) and FFullscreen then
    SetWindowPos(FWindow, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
  if FCursorSaved then
  begin
    SetCursorPos(FCursorX, FCursorY);
    FCursorSaved := False;
  end;
{    SetForegroundWindow(m_foreground);
    SetWindowPlacement(m_foreground, m_foreground_placement);
    SetWindowPos(m_foreground, HWND_TOP, m_foreground_rect.left, m_foreground_rect.top, m_foreground_rect.right - m_foreground_rect.left, m_foreground_rect.bottom - m_foreground_rect.top, SWP_FRAMECHANGED or SWP_NOCOPYBITS);
    m_foreground := 0;
  End;}
end;

procedure TDirectXDisplay.internal_open(const AMode: IPTCMode; AExact: Boolean; AFrequency: Integer);
begin
  LOG('internal display open');
  LOG('mode width', AMode.Width);
  LOG('mode height', AMode.Height);
  LOG('mode format', AMode.Format);
  LOG('mode frequency', AFrequency);
  if AExact then
  begin
    LOG('setting exact mode');
    DirectXCheck(FDDraw.SetDisplayMode(AMode.Width, AMode.Height, AMode.Format.Bits, AFrequency, 0));
  end
  else
    case AMode.Format.Bits of
      32: begin
        LOG('setting virtual 32 mode');
        if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 32, AFrequency, 0) <> DD_OK then
          if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 24, AFrequency, 0) <> DD_OK then
            DirectXCheck(FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 16, AFrequency, 0));
      end;
      24: begin
        LOG('setting virtual 24 mode');
        if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 24, AFrequency, 0) <> DD_OK then
          if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 32, AFrequency, 0) <> DD_OK then
            DirectXCheck(FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 16, AFrequency, 0));
      end;
      16: begin
        LOG('setting virtual 16 mode');
        if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 16, AFrequency, 0) <> DD_OK then
          if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 24, AFrequency, 0) <> DD_OK then
            DirectXCheck(FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 32, AFrequency, 0));
      end;
      8: begin
        LOG('setting virtual 8 mode');
        if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 8, AFrequency, 0) <> DD_OK then
          if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 24, AFrequency, 0) <> DD_OK then {yes, 24bit is now faster than 32bit!}
            if FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 32, AFrequency, 0) <> DD_OK then
              DirectXCheck(FDDraw.SetDisplayMode(AMode.Width, AMode.Height, 16, AFrequency, 0));
      end;
      else
        raise TPTCError.Create('unsupported pixel format');
    end;
  LOG('internal display open success');
  FMode := TPTCMode.Create(AMode);
  FOpen := True;
end;

procedure TDirectXDisplay.internal_open_nearest(const AMode: IPTCMode; AExact: Boolean; AFrequency: Integer);

  function Pass2TryMode(TryWidth, TryHeight: Integer): Boolean;
  var
    tmpMode: IPTCMode;
  begin
    if (AMode.Width <= TryWidth) and (AMode.Height <= TryHeight) then
    try
      tmpMode := TPTCMode.Create(TryWidth, TryHeight, AMode.Format);
      internal_open(tmpMode, AExact, AFrequency);
      exit(true);
    except
      on TPTCError do;
    end;
    Result := false;
  end;

var
  index: Integer;
  match, match_exact: IPTCMode;
  i: Integer;
  width, height: Integer;
  dx1, dy1, dx2, dy2: Integer;
begin
  if FResolutionsCount <> 0 then
  begin
    LOG('searching for nearest mode in resolutions list');
    index := 0;
    match_exact := nil;
    match := nil;
    for i := 0 to FResolutionsCount - 1 do
    begin
      width := FResolutions[i].Width;
      height := FResolutions[i].Height;
      if (AMode.Width <= width) and (AMode.Height <= height) then
      begin
        if (width = AMode.Width) and (height = AMode.Height) then
        begin
          LOG('found an exact match');
          match_exact := TPTCMode.Create(width, height, AMode.Format);
        end;
        if match <> nil then
        begin
          dx1 := match.Width - AMode.Width;
          dy1 := match.Height - AMode.Height;
          dx2 := width - AMode.Width;
          dy2 := height - AMode.Height;
          if (dx2 <= dx1) and (dy2 <= dy1) then
          begin
            LOG('found a better nearest match');
            match := TPTCMode.Create(width, height, AMode.Format);
          end;
        end
        else
        begin
          LOG('found first nearest match');
          match := TPTCMode.Create(width, height, AMode.Format);
        end;
      end
      else
      begin
{        LOG('stopping nearest mode search');
        Break;}
      end;
    end;

    if match_exact <> nil then
    try
      LOG('trying an exact match');
      internal_open(match_exact, AExact, AFrequency);
      exit;
    except
      on TPTCError do;
    end;
    if match <> nil then
    try
      LOG('trying nearest match');
      internal_open(match, AExact, AFrequency);
      exit;
    except
      on TPTCError do;
    end;
  end
  else
  begin
    LOG('no resolutions list for nearest mode search');
  end;

  LOG('could not find a nearest match in first pass');
  try
    LOG('trying requested mode first');
    internal_open(AMode, AExact, AFrequency);
    exit;
  except
    on TPTCError do
    begin
      LOG('falling back to nearest standard mode');
      if Pass2TryMode(320, 200) then exit;
      if Pass2TryMode(320, 240) then exit;
      if Pass2TryMode(512, 384) then exit;
      if Pass2TryMode(640, 400) then exit;
      if Pass2TryMode(640, 480) then exit;
      if Pass2TryMode(800, 600) then exit;
      if Pass2TryMode(1024, 768) then exit;
      if Pass2TryMode(1280, 1024) then exit;
      if Pass2TryMode(1600, 1200) then exit;
    end;
  end;
  raise TPTCError.Create('could not set display mode');
end;
