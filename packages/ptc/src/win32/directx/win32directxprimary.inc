{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2013  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

constructor TDirectXPrimary.Create;
begin
  FArea := TPTCArea.Create;
  FClip := TPTCArea.Create;
  FFormat := TPTCFormat.Create;
  FClear := TPTCClear.Create;
  FPalette := TPTCPalette.Create;

  FActive := True;
  FBlocking := True;
  FCentering := True;
  FSynchronize := True;
  FFullscreen := False;
end;

destructor TDirectXPrimary.Destroy;
begin
  { close }
  Close;
  FClear.Free;
  inherited Destroy;
end;

procedure TDirectXPrimary.Initialize(AWindow: TWin32Window; const ADDraw2: IDirectDraw2);
begin
  LOG('initializing primary surface');
  Close;
  FWindow := AWindow;
  FDD2 := ADDraw2;
end;

procedure TDirectXPrimary.Primary(APages: Integer; AVideo, AFullscreen, APalette, AComplex: Boolean);
var
  attach_primary_pages: Boolean;
  descriptor: TDDSURFACEDESC;
  ddpf: TDDPIXELFORMAT;
  capabilities: TDDSCAPS;
  I: Integer;
  rectangle: TRECT;
begin
  try
    LOG('creating primary surface');
    LOG('pages', APages);
    LOG('video', AVideo);
    LOG('fullscreen', AFullscreen);
    LOG('palette', APalette);
    LOG('complex', AComplex);
    if APages <= 0 then
      raise TPTCError.Create('invalid number of pages');
    FFullscreen := AFullscreen;
    attach_primary_pages := False;
    if AComplex or (not APalette) or (APages = 1) then
    begin
      LOG('creating a complex primary flipping surface');
      FillChar(descriptor, SizeOf(descriptor), 0);
      descriptor.dwSize := SizeOf(descriptor);
      descriptor.dwFlags := DDSD_CAPS;
      if APages > 1 then
        descriptor.dwFlags := descriptor.dwFlags or DDSD_BACKBUFFERCOUNT;
      descriptor.dwBackBufferCount := APages - 1;
      descriptor.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;
      if AVideo then
        descriptor.ddsCaps.dwCaps := descriptor.ddsCaps.dwCaps or DDSCAPS_VIDEOMEMORY;
      if APages > 1 then
        descriptor.ddsCaps.dwCaps := descriptor.ddsCaps.dwCaps or DDSCAPS_COMPLEX or DDSCAPS_FLIP;
      DirectXCheck(FDD2.CreateSurface(@descriptor, FDDSPrimary, nil), 'FDD2.CreateSurface failed in TDirectXPrimary.Primary');
    end
    else
    begin
      LOG('creating a simple primary surface');
      FillChar(descriptor, SizeOf(descriptor), 0);
      descriptor.dwSize := SizeOf(descriptor);
      descriptor.dwFlags := DDSD_CAPS;
      descriptor.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;
      if AVideo then
        descriptor.ddsCaps.dwCaps := descriptor.ddsCaps.dwCaps or DDSCAPS_VIDEOMEMORY;
      DirectXCheck(FDD2.CreateSurface(@descriptor, FDDSPrimary, nil), 'FDD2.CreateSurface failed in TDirectXPrimary.Primary (palette)');
      attach_primary_pages := True;
    end;
    FillChar(descriptor, SizeOf(descriptor), 0);
    descriptor.dwSize := SizeOf(descriptor);
    DirectXCheck(FDDSPrimary.GetSurfaceDesc(@descriptor), 'FDDSPrimary.GetSurfaceDesc failed in TDirectXPrimary.Primary');
    if (descriptor.ddsCaps.dwCaps and DDSCAPS_VIDEOMEMORY) <> 0 then
    begin
      LOG('primary surface is in video memory');
    end
    else
    begin
      LOG('primary surface is in system memory');
    end;
    FillChar(ddpf, SizeOf(ddpf), 0);
    ddpf.dwSize := SizeOf(ddpf);
    DirectXCheck(FDDSPrimary.GetPixelFormat(@ddpf), 'FDDSPrimary.GetPixelFormat failed in TDirectXPrimary.Primary');
    FFront := FDDSPrimary;
    FPages := APages;
    FWidth := descriptor.dwWidth;
    FHeight := descriptor.dwHeight;
    FFormat := DirectXTranslate(ddpf);
    LOG('primary width', FWidth);
    LOG('primary height', FHeight);
    LOG('primary pages', FPages);
    LOG('primary format', FFormat);
    if APalette then
    begin
      LOG('clearing primary palette');
      palette(TPTCPalette.Create);
    end;
    if attach_primary_pages then
    begin
      if (APages - 1) > High(FDDSPrimaryPage) then
        raise TPTCError.Create('too many primary pages');
      for I := 0 to APages - 2 do
      begin
        LOG('creating primary page surface');
        FillChar(descriptor, SizeOf(descriptor), 0);
        descriptor.dwSize := SizeOf(descriptor);
        descriptor.dwFlags := DDSD_CAPS or DDSD_WIDTH or DDSD_HEIGHT;
        descriptor.dwWidth := FWidth;
        descriptor.dwHeight := FHeight;
        descriptor.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;
        if AVideo then
          descriptor.ddsCaps.dwCaps := descriptor.ddsCaps.dwCaps or DDSCAPS_VIDEOMEMORY;
        DirectXCheck(FDD2.CreateSurface(@descriptor, FDDSPrimaryPage[I], nil), 'FDD2.CreateSurface failed in TDirectXPrimary.Primary (primary page ' + IntToStr(I) + ')');

        FillChar(descriptor, SizeOf(descriptor), 0);
        descriptor.dwSize := SizeOf(descriptor);
        DirectXCheck(FDDSPrimaryPage[I].GetSurfaceDesc(@descriptor), 'FDDSPrimaryPage[' + IntToStr(I) + '].GetSurfaceDesc failed in TDirectXPrimary.Primary');

        if (descriptor.ddsCaps.dwCaps and DDSCAPS_VIDEOMEMORY) <> 0 then
        begin
          LOG('primary surface page is in video memory');
        end
        else
        begin
          LOG('primary surface page is in system memory');
        end;
        LOG('attaching page to primary surface');
        DirectXCheck(FDDSPrimary.AddAttachedSurface(FDDSPrimaryPage[I]), 'FDDSPrimary.AddAttachedSurface failed in TDirectXPrimary.Primary (primary page ' + IntToStr(I) + ')');
      end;
    end;
    FPrimaryWidth := FWidth;
    FPrimaryHeight := FHeight;
    if not AFullscreen then
    begin
      GetClientRect(FWindow.handle, rectangle);
      FWidth := rectangle.right;
      FHeight := rectangle.bottom;
    end;
    FArea := TPTCArea.Create(0, 0, FWidth, FHeight);
    FClip := TPTCArea.Create(FArea);
    if APages > 1 then
    begin
      capabilities.dwCaps := DDSCAPS_BACKBUFFER;
      DirectXCheck(FFront.GetAttachedSurface(@capabilities, FDDSPrimaryBack), 'FFront.GetAttachedSurface failed in TDirectXPrimary.Primary');

      FillChar(descriptor, SizeOf(descriptor), 0);
      descriptor.dwSize := SizeOf(descriptor);
      DirectXCheck(FDDSPrimaryBack.GetSurfaceDesc(@descriptor), 'FDDSPrimaryBack.GetSurfaceDesc failed in TDirectXPrimary.Primary');

      if (descriptor.ddsCaps.dwCaps and DDSCAPS_VIDEOMEMORY) <> 0 then
      begin
        LOG('primary back surface is in video memory');
      end
      else
      begin
        LOG('primary back surface is in system memory');
      end;
    end
    else
      FDDSPrimaryBack := FFront;
    FBack := FDDSPrimaryBack;
    if AFullscreen then
      while APages > 0 do
      begin
        Dec(APages);
        LOG('clearing primary page');
        Clear;
        Update;
      end;
  except
    on error: TPTCError do
    begin
      if FDDSPrimary <> nil then
      begin
        FDDSPrimary := nil;
      end;
      raise TPTCError.Create('could not create primary surface', error);
    end;
  end;
end;

procedure TDirectXPrimary.Secondary(AWidth, AHeight: Integer);
var
  descriptor: TDDSURFACEDESC;
  hel: TDDCAPS;
  driver: TDDCAPS;
  capabilities: TDDSCAPS;
begin
  LOG('creating secondary surface');
  LOG('width', AWidth);
  LOG('height', AHeight);

  if FDDC <> nil then
  begin
    LOG('releasing clipper');
    FDDC := nil;
  end;
  if FDDSSecondary <> nil then
  begin
    LOG('releasing secondary surface');
    FDDSSecondary := nil;
  end;

  FillChar(descriptor, SizeOf(descriptor), 0);
  descriptor.dwSize := SizeOf(descriptor);
  descriptor.dwFlags := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH;
  descriptor.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;
  descriptor.dwHeight := AHeight;
  descriptor.dwWidth := AWidth;
  DirectXCheck(FDD2.CreateSurface(@descriptor, FDDSSecondary, Nil), 'FDD2.CreateSurface failed in TDirectXPrimary.Secondary');

  FillChar(descriptor, SizeOf(descriptor), 0);
  descriptor.dwSize := SizeOf(descriptor);
  DirectXCheck(FDDSSecondary.GetSurfaceDesc(@descriptor), 'FDDSSecondary.GetSurfaceDesc failed in TDirectXPrimary.Secondary');

  if (descriptor.ddsCaps.dwCaps and DDSCAPS_VIDEOMEMORY) <> 0 then
  begin
    LOG('secondary surface is in video memory');
  end
  else
  begin
    LOG('secondary surface is in system memory');
  end;

  if not FFullscreen then
  begin
    LOG('attaching clipper to primary surface');
    DirectXCheck(FDD2.CreateClipper(0, FDDC, nil), 'FDD2.CreateClipper failed in TDirectXPrimary.Secondary');
    DirectXCheck(FDDC.SetHWnd(0, FWindow.Handle), 'FDDC.SetHWnd failed in TDirectXPrimary.Secondary');
    DirectXCheck(FDDSPrimary.SetClipper(FDDC), 'FDDSPrimary.SetClipper failed in TDirectXPrimary.Secondary');
  end;
  FWidth := AWidth;
  FHeight := AHeight;
  FArea := TPTCArea.Create(0, 0, FWidth, FHeight);
  FClip := TPTCArea.Create(FArea);
  FSecondaryWidth := FWidth;
  FSecondaryHeight := FHeight;
  FBack := FDDSSecondary;

{  hel.dwSize := SizeOf(hel);
  driver.dwSize := SizeOf(driver);
  DirectXCheck(FDD2^.GetCaps(@driver, @hel));}
  {
    auto stretching support is disabled below because in almost 100% of cases
    centering is faster and we must choose the fastest option by default!
  }
  {todo: DDCAPS!!!!!!!!!!!}
{  if ((driver.dwCaps and DDCAPS_BLTSTRETCH) <> 0) and
     ((driver.dwFXCaps and DDFXCAPS_BLTSTRETCHY) <> 0) then
  begin
    LOG('found hardware stretching support');
  end
  else
  begin
    LOG('no hardware stretching support');
  End;}

  FDDSSecondary.GetCaps(@capabilities);
  if (capabilities.dwCaps and DDSCAPS_SYSTEMMEMORY) <> 0 then
  begin
    LOG('secondary surface is in system memory');
  end;

  Centering(True);

  LOG('clearing secondary page');

  Clear;

  Update;
end;

procedure TDirectXPrimary.Synchronize(AUpdate: Boolean);
begin
  FSynchronize := AUpdate;
  if FPages > 1 then
    FSynchronize := False;
  LOG('primary synchronize', AUpdate);
end;

procedure TDirectXPrimary.Centering(ACenter: Boolean);
begin
  FCentering := ACenter;
  LOG('primary centering', FCentering);
end;

procedure TDirectXPrimary.Close;
var
  i: Integer;
  lost: Boolean;
begin
  try
    LOG('closing primary surface');
    lost := False;
    if (FDDSPrimary <> nil) and (FDDSPrimary.IsLost <> DD_OK) then
      lost := True;
    if (FDDSSecondary <> nil) and (FDDSSecondary.IsLost <> DD_OK) then
      lost := True;
    if (FBack <> nil) and (FDDSPrimary <> nil) and FFullscreen and (not lost) then
    begin
      palette(TPTCPalette.Create);
      LOG('clearing primary pages');
      for i := 0 to FPages - 1 do
      begin
        clear;
        update;
      end;
    end;
  except
    on TPTCError do
    begin
      LOG('primary close clearing failed');
    end;
  end;

  FBack := nil;
  FFront := nil;
  FDDSPrimaryBack := nil;

  if FDDC <> nil then
  begin
    LOG('releasing clipper');
    FDDC := nil;
  end;
  if FDDSSecondary <> nil then
  begin
    LOG('releasing secondary surface');
    FDDSSecondary := nil;
  end;
  i := 0;
  while FDDSPrimaryPage[i] <> nil do
  begin
    LOG('releasing attached primary surface page');
    FDDSPrimaryPage[i] := nil;
    Inc(i);
  end;
  if FDDSPrimary <> nil then
  begin
    LOG('releasing primary surface');
    FDDSPrimary := nil;
  end;
end;

procedure TDirectXPrimary.Update;
begin
  Block;
  Paint;
  if FPages > 1 then
    DirectXCheck(FFront.Flip(nil, DDFLIP_WAIT), 'FFront.Flip failed in TDirectXPrimary.Update');
end;

function TDirectXPrimary.Lock: Pointer;
var
  descriptor: TDDSURFACEDESC;
  pnt: POINT;
  rct: RECT;
begin
  Block;
  descriptor.dwSize := SizeOf(descriptor);
  if FFullscreen or (FBack = FDDSSecondary) then
  begin
    DirectXCheck(FBack.Lock(nil, @descriptor, DDLOCK_WAIT, 0), 'FBack.Lock failed in TDirectXPrimary.Lock');
    FLocked := descriptor.lpSurface;
  end
  else
  begin
    pnt.x := 0;
    pnt.y := 0;
    ClientToScreen(FWindow.handle, pnt);
    rct.left := pnt.x;
    rct.top := pnt.y;
    rct.right := pnt.x + FWidth;
    rct.bottom := pnt.y + FHeight;
    DirectXCheck(FBack.Lock(@rct, @descriptor, DDLOCK_WAIT, 0), 'FBack.Lock(rect) failed in TDirectXPrimary.Lock');
    FLocked := descriptor.lpSurface;
  end;
  Result := FLocked;
end;

procedure TDirectXPrimary.Unlock;
begin
  Block;
  DirectXCheck(FBack.Unlock(FLocked), 'FBack.Unlock failed in TDirectXPrimary.Unlock');
end;

procedure TDirectXPrimary.Clear;
var
  fx: TDDBLTFX;
begin
  Block;
  if FFullscreen or (FBack = FDDSSecondary) then
  begin
    fx.dwSize := SizeOf(fx);
    fx.dwFillColor := 0;
    DirectXCheck(FBack.Blt(nil, nil, nil, DDBLT_COLORFILL or DDBLT_WAIT, @fx), 'FBack.Blt failed in TDirectXPrimary.Clear');
  end
  else
  begin
    { todo: replace with hardware clear! }
    if Format.Direct then
      Clear(TPTCColor.Create(0, 0, 0, 0), FArea)
    else
      Clear(TPTCColor.Create(0), FArea);
  end;
end;

procedure TDirectXPrimary.Clear(AColor: IPTCColor; const AArea: IPTCArea);
var
  clipped, clipped_area: IPTCArea;
  clear_color: DWord;
  rct: RECT;
  fx: TDDBLTFX;
  pixels: Pointer;
begin
  Block;
  if FFullscreen or (FBack = FDDSSecondary) then
  begin
    clipped := TPTCClipper.clip(AArea, FClip);
    clear_color := Pack(AColor, FFormat);
    with rct do
    begin
      left := clipped.left;
      top := clipped.top;
      right := clipped.right;
      bottom := clipped.bottom;
    end;
    fx.dwSize := SizeOf(fx);
    fx.dwFillColor := clear_color;
    DirectXCheck(FBack.Blt(@rct, nil, nil, DDBLT_COLORFILL or DDBLT_WAIT, @fx), 'FBack.Blt(rect) failed in TDirectXPrimary.Clear');
  end
  else
  begin
    { todo: replace with accelerated clearing code! }
    pixels := Lock;
    try
      clipped_area := TPTCClipper.clip(AArea, Clip);
      FClear.Request(Format);
      FClear.Clear(pixels, clipped_area.left, clipped_area.right, clipped_area.width, clipped_area.height, Pitch, AColor);
      Unlock;
    except
      on error: TPTCError do
      begin
        Unlock;
        raise TPTCError.Create('failed to clear console area', error);
      end;
    end;
  end;
end;

procedure TDirectXPrimary.Palette(APalette: IPTCPalette);
begin
  Block;

  FPalette.Load(APalette.Data);
  ResetPalette;
end;

procedure TDirectXPrimary.ResetPalette;
var
  data: PUint32;
  temp: array [0..255] of PALETTEENTRY;
  I: Integer;
  DDP: IDirectDrawPalette;
begin
  if not FFormat.Indexed then
  begin
    LOG('palette set in direct color');
  end
  else
  begin
    data := FPalette.Data;
    for I := 0 to 255 do
    begin
      temp[I].peRed := (data[I] and $00FF0000) shr 16;
      temp[I].peGreen := (data[I] and $0000FF00) shr 8;
      temp[I].peBlue := data[I] and $000000FF;
      temp[I].peFlags := 0;
    end;
    if FDDSPrimary.GetPalette(DDP) <> DD_OK then
    begin
      DirectXCheck(FDD2.CreatePalette(DDPCAPS_8BIT or DDPCAPS_ALLOW256 or DDPCAPS_INITIALIZE, @temp, DDP, nil), 'FDD2.CreatePalette failed in TDirectXPrimary.Palette');
      DirectXCheck(FDDSPrimary.SetPalette(DDP), 'FDDSPrimary.SetPalette failed in TDirectXPrimary.Palette');
    end
    else
      DirectXCheck(DDP.SetEntries(0, 0, 256, @temp), 'DDP.SetEntries failed in TDirectXPrimary.Palette');
  end;
end;

procedure TDirectXPrimary.Clip(const AArea: IPTCArea);
begin
  FClip := TPTCClipper.clip(AArea, FArea);
end;

function TDirectXPrimary.GetPitch: Integer;
var
  descriptor: TDDSURFACEDESC;
begin
  Block;
  descriptor.dwSize := SizeOf(descriptor);
  DirectXCheck(FBack.GetSurfaceDesc(@descriptor), 'FBack.GetSurfaceDesc failed in TDirectXPrimary.GetPitch');
  Result := descriptor.lPitch;
end;

function TDirectXPrimary.GetDDS: IDirectDrawSurface;
begin
  if FDDSSecondary <> nil then
    Result := FDDSSecondary
  else
    Result := FDDSPrimaryBack;
end;

procedure TDirectXPrimary.Activate;
begin
  LOG('primary activated');
  FActive := True;
end;

procedure TDirectXPrimary.Deactivate;
begin
  LOG('primary deactivated');
  if FBlocking then
    FActive := False
  else
    {no deactivation when not blocking};
end;

procedure TDirectXPrimary.Block;
var
  restored: Boolean;
begin
  if not FBlocking then
    exit;
  if not active then
  begin
    restored := False;
    while not restored do
    begin
      LOG('blocking until activated');
      while not active do
      begin
        FWindow.update(True);
        Sleep(10);
      end;
      LOG('primary is active');
      FWindow.update(True);
      try
        restore;
        restored := True;
        LOG('successful restore');
      except
        on TPTCError do
        begin
          LOG('application is active but cannot restore');
        end;
      end;
      Sleep(10);
    end;
  end;
  if FDDSPrimary.IsLost <> DD_OK then
    raise TPTCError.Create('primary surface lost unexpectedly!');
  if (FDDSSecondary <> nil) and (FDDSSecondary.IsLost <> DD_OK) then
    raise TPTCError.Create('secondary surface lost unexpectedly!');
end;

procedure TDirectXPrimary.Save;
begin
  if FDDSPrimary.IsLost = DD_OK then
  begin
    LOG('saving contents of primary surface');

    { todo: save contents of primary surface }
  end
  else
  begin
    LOG('could not save primary surface');
  end;

  if (FDDSSecondary <> nil) and (FDDSSecondary.IsLost = DD_OK) then
  begin
    LOG('saving contents of secondary surface');

    { todo: save contents of secondary surface }
  end
  else
    if FDDSSecondary <> nil then
    begin
      LOG('could not save secondary surface');
    end;
end;

procedure TDirectXPrimary.Restore;
var
  I: Integer;
  rct: RECT;
  fx: TDDBLTFX;
begin
  DirectXCheck(FDDSPrimary.Restore, 'FDDSPrimary.Restore failed in TDirectXConsole.Restore');
  if FDDSSecondary <> nil then
    DirectXCheck(FDDSSecondary.Restore, 'FDDSSecondary.Restore failed in TDirectXConsole.Restore');
  LOG('restoring contents of primary surface');
  { todo: restore palette object on primary surface ? }
  { todo: restore contents of primary surface }
  if FDDSPrimaryPage[0] <> nil then
  begin
    LOG('restoring attached pages');
    for I := 0 to FPages - 2 do
      DirectXCheck(FDDSPrimaryPage[I].Restore, 'FDDSPrimaryPage[' + IntToStr(I) + '].Restore failed in TDirectXConsole.Restore');
  end;

  if FDDSSecondary <> nil then
  begin
    if FFullscreen then
    begin
      LOG('temporary primary surface clear');

      { temporary: clear primary surface }
      with rct do
      begin
        left := 0;
        top := 0;
        right := FPrimaryWidth;
        bottom := FPrimaryHeight;
      end;
      fx.dwSize := SizeOf(fx);
      fx.dwFillColor := 0;
      DirectXCheck(FDDSPrimary.Blt(@rct, nil, nil, DDBLT_COLORFILL or DDBLT_WAIT, @fx), 'FDDSPrimary.Blt failed in TDirectXPrimary.Restore');
    end;
    LOG('restoring contents of secondary surface');
    { todo: restore contents of secondary surface }
  end;
end;

procedure TDirectXPrimary.Paint;
var
  source, destination: RECT;
  pnt: POINT;
  x, y: Integer;
  fx: TDDBLTFX;
begin
  if not Active then
  begin
    LOG('paint when not active');
    exit;
  end;
  if FDDSSecondary <> nil then
  begin
    if (FDDSPrimary.IsLost <> DD_OK) or
       (FDDSSecondary.IsLost <> DD_OK) then
    begin
      LOG('paint when surfaces are lost');
      exit;
    end;
    source.left := 0;
    source.top := 0;
    source.right := FSecondaryWidth;
    source.bottom := FSecondaryHeight;
    destination.left := 0;
    destination.top := 0;
    destination.right := FPrimaryWidth;
    destination.bottom := FPrimaryHeight;

    { note: code below assumes secondary is smaller than primary }
    if FCentering and FFullscreen then
    begin
      x := (destination.right - source.right) div 2;
      y := (destination.bottom - source.bottom) div 2;

      destination.left := x;
      destination.top := y;
      destination.right := x + source.right;
      destination.bottom := y + source.bottom;
    end;
    if not FFullscreen then
    begin
      pnt.x := 0;
      pnt.y := 0;
      ClientToScreen(FWindow.handle, pnt);

      GetClientRect(FWindow.handle, destination);
      Inc(destination.left, pnt.x);
      Inc(destination.top, pnt.y);
      Inc(destination.right, pnt.x);
      Inc(destination.bottom, pnt.y);
    end;

    if ((source.right - source.left) = 0) or
       ((source.bottom - source.top) = 0) or
       ((destination.right - destination.left) = 0) or
       ((destination.bottom - destination.top) = 0) then
    begin
      LOG('zero area in primary paint');
      exit;
    end;

    if FSynchronize then
    begin
      fx.dwSize := SizeOf(fx);
      fx.dwDDFX := DDBLTFX_NOTEARING;
      try
        DirectXCheck(FDDSPrimaryBack.Blt(@destination, FDDSSecondary, @source, DDBLT_WAIT or DDBLT_DDFX, @fx), 'FDDSPrimary.Blt (synchronized) failed in TDirectXPrimary.Paint');
      except
        on TPTCError do
        begin
          LOG('falling back to unsynchronized blt');
          FSynchronize := False;
        end;
      end;
    end;
    if not FSynchronize then
      DirectXCheck(FDDSPrimaryBack.Blt(@destination, FDDSSecondary, @source, DDBLT_WAIT, nil), 'FDDSPrimaryBack.Blt (unsynchronized) failed in TDirectXPrimary.Paint');
  end;
end;

procedure TDirectXPrimary.Blocking(ABlocking: Boolean);
begin
  FBlocking := ABlocking;
end;

function TDirectXPrimary.Pack(const AColor: IPTCColor; const AFormat: IPTCFormat): Uint32;
var
  r_base, g_base, b_base, a_base: Integer;
  r_size, g_size, b_size, a_size: Integer;
  r_scale, g_scale, b_scale, a_scale: Single;
begin
  if AColor.direct and AFormat.direct then
  begin
    Analyse(AFormat.r, r_base, r_size);
    Analyse(AFormat.g, g_base, g_size);
    Analyse(AFormat.b, b_base, b_size);
    Analyse(AFormat.a, a_base, a_size);
    r_scale := 1 shl r_size;
    g_scale := 1 shl g_size;
    b_scale := 1 shl b_size;
    a_scale := 1 shl a_size;
    Result := (Trunc(AColor.r * r_scale) shl r_base) or
              (Trunc(AColor.g * g_scale) shl g_base) or
              (Trunc(AColor.b * b_scale) shl b_base) or
              (Trunc(AColor.a * a_scale) shl a_base);
  end
  else
    if AColor.indexed and AFormat.indexed then
      Result := AColor.index
    else
      raise TPTCError.Create('color format type mismatch');
end;

procedure TDirectXPrimary.Analyse(AMask: Uint32; out ABase, ASize: Integer);
begin
  ABase := 0;
  ASize := 0;
  if AMask = 0 then
    exit;
  while (AMask and 1) = 0 do
  begin
    AMask := AMask shr 1;
    Inc(ABase);
  end;
  while (AMask and 1) <> 0 do
  begin
    AMask := AMask shr 1;
    Inc(ASize);
  end;
end;

function TDirectXPrimary.Palette: IPTCPalette;
begin
  Result := FPalette;
end;

function TDirectXPrimary.Clip: IPTCArea;
begin
  Result := FClip;
end;

procedure TDirectXPrimary.InternalResize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FArea := TPTCArea.Create(0, 0, FWidth, FHeight);
  FClip := TPTCArea.Create(FArea);
end;
