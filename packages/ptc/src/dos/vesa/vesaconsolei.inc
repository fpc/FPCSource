{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2011 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

{$MACRO ON}

{$DEFINE DEFAULT_WIDTH:=320}
{$DEFINE DEFAULT_HEIGHT:=200}
{$DEFINE DEFAULT_FORMAT:=TPTCFormat.Create(32, $00FF0000, $0000FF00, $000000FF)}
{ $DEFINE DEFAULT_FORMAT:=TPTCFormat.Create(24, $00FF0000, $0000FF00, $000000FF)}

procedure VESALogProcedure(const S: string);
begin
  LOG(s);
end;

constructor TVESAConsole.Create;
begin
  inherited Create;
  
  FTryLFB := true;
  FTryWindowed := true;
  
  vesa.LogProcedure := @VESALogProcedure;

  FOpen := False;
  FLocked := False;
  FTitle := '';
  FInformation := '';
  FDefaultWidth := DEFAULT_WIDTH;
  FDefaultHeight := DEFAULT_HEIGHT;
  FDefaultFormat := DEFAULT_FORMAT;

  FPrimary := nil;
  InitVESA;
  if not VBEPresent then
    raise TPTCError.Create('the system does not support the VESA BIOS extensions');

  UpdateModeList;

  FClip := TPTCArea.Create;
  FArea := TPTCArea.Create;
  FCopy := TPTCCopy.Create;
  FPalette := TPTCPalette.Create;
  Configure('ptcpas.cfg');
end;

destructor TVESAConsole.Destroy;
begin
  Close;
  
  FKeyboard.Free;
  FMouse.Free;
  FEventQueue.Free;
  FCopy.Free;
  inherited Destroy;
end;

procedure TVESAConsole.UpdateModeList;
var
  I, J: Integer;
  r, g, b, a: DWord;
  tmpbpp: Integer;
begin
  SetLength(FModes, 0);
  SetLength(FModesN, 0);

(*  FModesLast := -1;
  for I := Low(VBEModes) to High(VBEModes) do
    with VBEModes[I] do
      if (MemoryModel = vmmmDirectColor) and
         ((BitsPerPixel = 8) or
          (BitsPerPixel = 15) or
          (BitsPerPixel = 16) or
          (BitsPerPixel = 24) or
          (BitsPerPixel = 32)) then
        Inc(FModesLast)
      else
        if (MemoryModel = vmmmPackedPixel) and (BitsPerPixel = 8) then
          Inc(FModesLast, 2);
  SetLength(FModes, FModesLast + 2);
  SetLength(FModesN, FModesLast + 1);
//  Writeln(FModesLast, ' ', NrOfModes);
  FModes[FModesLast + 1] := TPTCMode.Create; {mark end of list!} *)
  J := -1;
  for I := Low(VBEModes) to High(VBEModes) do
    with VBEModes[I] do
      if (FTryWindowed and SupportsWindowed) or (FTryLFB and SupportsLFB) then
        if (MemoryModel = vmmmDirectColor) and
           ((BitsPerPixel = 8) or
            (BitsPerPixel = 15) or
            (BitsPerPixel = 16) or
            (BitsPerPixel = 24) or
            (BitsPerPixel = 32)) then
        begin
          if FTryWindowed and SupportsWindowed then
          begin
            Inc(J);
            r := MakeMask(WindowedRedMaskSize, WindowedRedFieldPosition);
            g := MakeMask(WindowedGreenMaskSize, WindowedGreenFieldPosition);
            b := MakeMask(WindowedBlueMaskSize, WindowedBlueFieldPosition);
            {a := MakeMask(RsvdMaskSize, RsvdFieldPosition);}
            a := 0;
            if BitsPerPixel = 15 then
              tmpbpp := 16
            else
              tmpbpp := BitsPerPixel;
            SetLength(FModes, J + 1);
            SetLength(FModesN, J + 1);
            FModes[J] := TPTCMode.Create(XResolution, YResolution, TPTCFormat.Create(tmpbpp, r, g, b, a));
            FModesN[J].Index := I;
            FModesN[J].SupportsWindowed := true;
          end;
	  
	  if FTryLFB and SupportsLFB then
	  begin
	    if (FTryWindowed and SupportsWindowed) and
	       (WindowedRedMaskSize        = LFBRedMaskSize) and
	       (WindowedRedFieldPosition   = LFBRedFieldPosition) and
	       (WindowedGreenMaskSize      = LFBGreenMaskSize) and
	       (WindowedGreenFieldPosition = LFBGreenFieldPosition) and
	       (WindowedBlueMaskSize       = LFBBlueMaskSize) and
	       (WindowedBlueFieldPosition  = LFBBlueFieldPosition) then
	    begin
	      { the same as the windowed mode => do not add a new mode, just
	        set the LFB flag of the last one }
	      FModesN[J].SupportsLFB := true;
	    end
	    else
	    begin
              Inc(J);
              r := MakeMask(LFBRedMaskSize, LFBRedFieldPosition);
              g := MakeMask(LFBGreenMaskSize, LFBGreenFieldPosition);
              b := MakeMask(LFBBlueMaskSize, LFBBlueFieldPosition);
              {a := MakeMask(RsvdMaskSize, RsvdFieldPosition);}
              a := 0;
              if BitsPerPixel = 15 then
                tmpbpp := 16
              else
                tmpbpp := BitsPerPixel;
              SetLength(FModes, J + 1);
              SetLength(FModesN, J + 1);
              FModes[J] := TPTCMode.Create(XResolution, YResolution, TPTCFormat.Create(tmpbpp, r, g, b, a));
              FModesN[J].Index := I;
              FModesN[J].SupportsLFB := true;
	    end;
	  end;
{          Inc(FModesLast)}
        end
        else
          if (MemoryModel = vmmmPackedPixel) and (BitsPerPixel = 8) then
          begin
            Inc(J);
            SetLength(FModes, J + 1);
            SetLength(FModesN, J + 1);
            FModes[J] := TPTCMode.Create(XResolution, YResolution, TPTCFormat.Create(8));
            FModesN[J].Index := I;
            FModesN[J].SupportsWindowed := FTryWindowed and SupportsWindowed;
            FModesN[J].SupportsLFB := FTryLFB and SupportsLFB;
            Inc(J);
			
            {RGB 332}
            SetLength(FModes, J + 1);
            SetLength(FModesN, J + 1);
            FModes[J] := TPTCMode.Create(XResolution, YResolution, TPTCFormat.Create(8, $E0, $1C, $03));
            FModesN[J].Index := I;
            FModesN[J].SupportsWindowed := FTryWindowed and SupportsWindowed;
            FModesN[J].SupportsLFB := FTryLFB and SupportsLFB;
{           Inc(FModesLast, 2);}
          end;

  FModesLast := J;
end;

procedure TVESAConsole.Configure(const AFileName: string);
var
  F: TextFile;
  S: string;
begin
  AssignFile(F, AFileName);
  {$push}{$I-}
  Reset(F);
  {$pop}
  if IOResult <> 0 then
    exit;
  while not EoF(F) do
  begin
    {$push}{$I-}
    Readln(F, S);
    {$pop}
    if IOResult <> 0 then
      Break;
    Option(S);
  end;
  CloseFile(F);
end;

procedure TVESAConsole.EnableLFB;
begin
  if FTryLFB <> true then
  begin
    FTryLFB := true;
    UpdateModeList;
  end;
end;

procedure TVESAConsole.DisableLFB;
begin
  if FTryLFB <> false then
  begin
    FTryLFB := false;
    UpdateModeList;
  end;
end;

procedure TVESAConsole.EnableWindowed;
begin
  if FTryWindowed <> true then
  begin
    FTryWindowed := true;
    UpdateModeList;
  end;
end;

procedure TVESAConsole.DisableWindowed;
begin
  if FTryWindowed <> false then
  begin
    FTryWindowed := false;
    UpdateModeList;
  end;
end;

function TVESAConsole.Option(const AOption: string): Boolean;
begin
  {...}
  if AOption = 'enable lfb' then
  begin
    EnableLFB;
    Result := true;
    exit;
  end;
  if AOption = 'disable lfb' then
  begin
    DisableLFB;
    Result := true;
    exit;
  end;
  if AOption = 'default lfb' then
  begin
    EnableLFB;
    Result := true;
    exit;
  end;
  
  if AOption = 'enable windowed framebuffer' then
  begin
    EnableWindowed;
    Result := true;
    exit;
  end;
  if AOption = 'disable windowed framebuffer' then
  begin
    DisableWindowed;
    Result := true;
    exit;
  end;
  if AOption = 'default windowed framebuffer' then
  begin
    EnableWindowed;
    Result := true;
    exit;
  end;
  
  if AOption = 'enable dpmi508h' then
  begin
    VESA.TryDPMI508h := true;
    Result := true;
    exit;
  end;
  if AOption = 'disable dpmi508h' then
  begin
    VESA.TryDPMI508h := false;
    Result := true;
    exit;
  end;
  if AOption = 'default dpmi508h' then
  begin
    VESA.TryDPMI508h := VESA.TryDPMI508hDefault;
    Result := true;
    exit;
  end;
  
  if AOption = 'enable nearptr' then
  begin
    VESA.TryNearPtr := true;
    Result := true;
    exit;
  end;
  if AOption = 'disable nearptr' then
  begin
    VESA.TryNearPtr := false;
    Result := true;
    exit;
  end;
  if AOption = 'default nearptr' then
  begin
    VESA.TryNearPtr := VESA.TryNearPtrDefault;
    Result := true;
    exit;
  end;
  
  if AOption = 'no8bitdac' then
  begin
    VESA.EightBitDACEnabled := false;
    Result := true;
    exit;
  end;
  
  if AOption = 'enable logging' then
  begin
    LOG_enabled := true;
    Result := true;
    exit;
  end;
  if AOption = 'disable logging' then
  begin
    LOG_enabled := false;
    Result := true;
    exit;
  end;

  Result := FCopy.Option(AOption);
end;

function TVESAConsole.Modes: TPTCModeList;
begin
  Result := FModes;
end;

function TVESAConsole.FindBestMode(const AMode: IPTCMode): Integer;
var
  I: Integer;
  modefound, bestmodefound: Integer;
  x, y, bpp: Integer;
begin
  if not AMode.Valid then
    raise TPTCError.Create('invalid mode');

  for I := 0 to FModesLast do
    if FModes[I].Equals(AMode) then
    begin
      Result := I;
      exit;
    end;

  modefound := -1;
  bestmodefound := -1;
  if (modefound = -1) and (AMode.Format.Direct) then
  begin
    x := 100000000;
    y := x;
    bpp := -1;
    for I := 0 to FModesLast do
      if (FModes[i].Width >= AMode.Width) and
         (FModes[i].Height >= AMode.Height) and
         (FModes[i].Width <= x) and
         (FModes[i].Height <= y) and
         (((FModes[i].Format.Bits >= bpp) and
           (bpp < AMode.Format.Bits)) or
          ((FModes[i].Format.Bits < bpp) and
           (FModes[i].Format.Bits >= AMode.Format.Bits) and
           (bpp > AMode.Format.Bits))) then
      begin
        bestmodefound := I;
        x := FModes[i].Width;
        y := FModes[i].Height;
        bpp := FModes[i].Format.Bits;
      end;
{      if FModes[I].bpp >=  then
      begin
        modefound := I;
        Break;
      End;}
  end;
  if (modefound = -1) and (AMode.format.indexed) then
  begin
    x := 100000000;
    y := x;
    bpp := -1;
    for I := 0 to FModesLast do
      if (FModes[i].Width >= AMode.Width) and
         (FModes[i].Height >= AMode.Height) and
         (FModes[i].Width <= x) and
         (FModes[i].Height <= y) { and
         (((FModes[i].format.bits >= bpp) and
           (bpp < _mode.format.bits)) or
          ((FModes[i].format.bits < bpp) and
           (FModes[i].format.bits >= _mode.format.bits) and
           (bpp > _mode.format.bits)))} then
      begin
        if (FModes[i].Width <> x) or (FModes[i].Height <> y) then
          bpp := -1;
        if FModes[i].Format.Indexed or
           (FModes[i].Format.Bits > bpp) then
        begin
          bestmodefound := I;
          x := FModes[i].Width;
          y := FModes[i].Height;
          bpp := FModes[i].Format.Bits;
          if FModes[i].Format.Indexed then
            bpp := 1000;
        end;
      end;
{      if FModes[I].bpp >=  then
      begin
        modefound := I;
        Break;
      End;}
  end;

  if bestmodefound <> -1 then
    modefound := bestmodefound;

  Result := modefound;
end;

procedure TVESAConsole.Open(const ATitle: string; APages: Integer); overload;
begin
  Open(ATitle, FDefaultFormat, APages);
end;

procedure TVESAConsole.Open(const ATitle: string; AFormat: IPTCFormat;
                            APages: Integer); overload;
begin
  Open(ATitle, FDefaultWidth, FDefaultHeight, AFormat, APages);
end;

procedure TVESAConsole.Open(const ATitle: string; AWidth, AHeight: Integer;
                            AFormat: IPTCFormat; APages: Integer); overload;
begin
  Open(ATitle, TPTCMode.Create(AWidth, AHeight, AFormat), APages);
end;

procedure TVESAConsole.Open(const ATitle: string; AMode: IPTCMode;
                            APages: Integer); overload;

  procedure SetRGB332Palette;
  var
    I: Integer;
    plt: array [0..255] of packed record
      B, G, R, A: Byte;
    end;
  begin
    for I := 0 to 255 do
      with plt[I] do
      begin
        case I shr 5 of
          0: R := 0;
          1: R := 36;
          2: R := 73;
          3: R := 109;
          4: R := 146;
          5: R := 182;
          6: R := 219;
          7: R := 255;
        end;
        case (I shr 2) and 7 of
          0: G := 0;
          1: G := 36;
          2: G := 73;
          3: G := 109;
          4: G := 146;
          5: G := 182;
          6: G := 219;
          7: G := 255;
        end;
        case I and 3 of
          0: B := 0;
          1: B := 85;
          2: B := 170;
          3: B := 255;
        end;
        A := 0;
      end;
    SetPalette(@plt, 0, 256);
  end;

var
  ModeFound: Integer;
begin
  ModeFound := FindBestMode(AMode);

  if ModeFound = -1 then
    raise TPTCError.Create('mode not found >:)');

  internal_close;
  FTitle := ATitle;

  FCurrentMode := ModeFound;

  if FModesN[ModeFound].SupportsLFB then
  begin
    if not SetVESAMode(FModesN[ModeFound].Index, true) then
    begin
      if FTryWindowed then
      begin
        { LFB -> Windowed fallback (useful for buggy DPMI hosts like NTVDM) }
	LOG('Setting LFB mode failed; will try without LFB...');
        DisableLFB;
	Open(ATitle, AMode, APages);
	exit;
      end
      else
        raise TPTCError.Create('error setting VBE mode');
    end;
  end
  else
  begin
    if not SetVESAMode(FModesN[ModeFound].Index, false) then
      raise TPTCError.Create('error setting VBE mode');
  end;
  
  FVESACurrentMode := FModesN[ModeFound].Index;

  with FModes[FCurrentMode].Format do
    if (Bits = 8) and Direct and (R = $E0) and (G = $1C) and (B = $03) then
      SetRGB332Palette;

  with VBEModes[FVESACurrentMode] do
  begin
    FWidth := XResolution;
    FHeight := YResolution;
    if LFBUsed then
    begin
      FPitch := LFBBytesPerScanLine;
      FVideoPagesCount := LFBNumberOfImagePages + 1;
    end
    else
    begin
      FPitch := WindowedBytesPerScanline;
      FVideoPagesCount := WindowedNumberOfImagePages + 1;
    end;
    if FVideoPagesCount < 1 then
      FVideoPagesCount := 1;
    if (APages > 0) and (FVideoPagesCount > APages) then
      FVideoPagesCount := APages;
    FVideoPageSize := YResolution * FPitch;
    FVideoPageHeight := YResolution;
    FCurrentVideoPage := 0;
    if FVideoPagesCount > 1 then
      FNextVideoPage := 1
    else
      FNextVideoPage := 0;
  end;
  FArea := TPTCArea.Create(0, 0, FWidth, FHeight);
  FClip := FArea;

  FLFBNearPtrAccessAvailable := LFBNearPtrAccessAvailable;
  if not FLFBNearPtrAccessAvailable then
  begin
    FPrimary := GetMem(FHeight * FPitch);
    FillChar(FPrimary^, FHeight * FPitch, 0);
  end;

  if FLFBNearPtrAccessAvailable and (FVideoPagesCount > 1) then
  begin
    { clear video memory pages numbers 1..FVideoPagesCount-1 }
    FillChar((PByte(LFBNearPtrAccessPtr) + FVideoPageSize)^, FVideoPageSize * (FVideoPagesCount - 1), 0);
  end;

  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  FKeyboard := TDosKeyboard.Create;
  FMouse := TDosMouse.Create(FWidth, FHeight);
  FEventQueue := TEventQueue.Create;

  { temporary platform dependent information fudge }
  FInformation := 'dos version x.xx.x'+#13+#10+'vesa version x.xx'+#13+#10+'vesa driver name xxxxx'+#13+#10+'display driver vendor xxxxx'+#13+#10+'certified driver? x'+#13+#10;

  { set open flag }
  FOpen := True;
end;

procedure TVESAConsole.Close;
begin
  if FOpen then
  begin
    if FLocked then
      raise TPTCError.Create('console is still locked');
    {flush all key presses}
    while KeyPressed do ReadKey;
    internal_close;
    FOpen := False;
  end;
end;

procedure TVESAConsole.Flush;
begin
  check_open;
  check_unlocked;
end;

procedure TVESAConsole.Finish;
begin
  check_open;
  check_unlocked;
end;

procedure TVESAConsole.Update;
var
  framebuffer: PInteger;
begin
  check_open;
  check_unlocked;
  if FVideoPagesCount = 1 then
  begin
    WaitRetraceSinglePage;
    if not FLFBNearPtrAccessAvailable then
    begin
      WriteToVideoMemory(FPrimary, 0, FPitch * FHeight);
    end;
  end
  else
  begin
    if not FLFBNearPtrAccessAvailable then
    begin
      WriteToVideoMemory(FPrimary, FNextVideoPage * FVideoPageSize, FPitch * FHeight);
    end;
    SetDisplayStart(0, FNextVideoPage * FVideoPageHeight, true);
    FCurrentVideoPage := FNextVideoPage;
    FNextVideoPage := FCurrentVideoPage + 1;
    if FNextVideoPage >= FVideoPagesCount then
      FNextVideoPage := 0;
  end;
end;

procedure TVESAConsole.Update(AArea: IPTCArea);
begin
  Update;
end;

procedure TVESAConsole.Copy(ASurface: IPTCSurface);
var
  Pixels: Pointer;
begin
  check_open;
  check_unlocked;
  Pixels := Lock;
  try
    ASurface.Load(Pixels, Width, Height, Pitch, Format, Palette);
    Unlock;
  except
    on error: TPTCError do
    begin
      Unlock;
      raise TPTCError.Create('failed to copy console to surface', error);
    end;
  end;
end;

procedure TVESAConsole.Copy(ASurface: IPTCSurface;
                            ASource, ADestination: IPTCArea);
var
  pixels: Pointer;
begin
  check_open;
  check_unlocked;
  Pixels := Lock;
  try
    ASurface.Load(Pixels, Width, Height, Pitch, Format, Palette, ASource, ADestination);
    Unlock;
  except
    on error: TPTCError do
    begin
      Unlock;
      raise TPTCError.Create('failed to copy console to surface', error);
    end;
  end;
end;

function TVESAConsole.Lock: Pointer;
var
  pixels: Pointer;
begin
  check_open;
  if FLocked then
    raise TPTCError.Create('console is already locked');

  FLocked := True;
  if FLFBNearPtrAccessAvailable then
    Result := PByte(LFBNearPtrAccessPtr) + (FNextVideoPage * FVideoPageSize)
  else
    Result := FPrimary;
end;

procedure TVESAConsole.Unlock;
begin
  check_open;
  if not FLocked then
    raise TPTCError.Create('console is not locked');

  FLocked := False;
end;

procedure TVESAConsole.Load(const APixels: Pointer;
                            AWidth, AHeight, APitch: Integer;
                            AFormat: IPTCFormat;
                            APalette: IPTCPalette);
var
  ConsolePixels: Pointer;
begin
  check_open;
  check_unlocked;
  if Clip.Equals(Area) then
  begin
    ConsolePixels := Lock;
    try
      try
        FCopy.Request(AFormat, Format);
        FCopy.Palette(APalette, Palette);
        FCopy.Copy(APixels, 0, 0, AWidth, AHeight, APitch, ConsolePixels, 0, 0,
                    Width, Height, Pitch);
      except
        on error: TPTCError do
        begin
          raise TPTCError.Create('failed to load pixels to console', error);
        end;
      end;
    finally
      Unlock;
    end;
  end
  else
    Load(APixels, AWidth, AHeight, APitch, AFormat, APalette, TPTCArea.Create(0, 0, Width, Height), Area);
end;

procedure TVESAConsole.Load(const APixels: Pointer;
                            AWidth, AHeight, APitch: Integer;
                            AFormat: IPTCFormat;
                            APalette: IPTCPalette;
                            ASource, ADestination: IPTCArea);
var
  ConsolePixels: Pointer;
  ClippedSource, ClippedDestination: IPTCArea;
begin
  check_open;
  check_unlocked;
  ConsolePixels := Lock;
  try
    try
      TPTCClipper.Clip(ASource, TPTCArea.Create(0, 0, AWidth, AHeight), ClippedSource, ADestination, Clip, ClippedDestination);
      FCopy.Request(AFormat, Format);
      FCopy.Palette(APalette, Palette);
      FCopy.Copy(APixels, ClippedSource.Left, ClippedSource.Top, ClippedSource.Width, ClippedSource.Height, APitch,
                 ConsolePixels, ClippedDestination.Left, ClippedDestination.Top, ClippedDestination.Width, ClippedDestination.Height, Pitch);
    except
      on error:TPTCError do
      begin
        raise TPTCError.Create('failed to load pixels to console area', error);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TVESAConsole.Save(APixels: Pointer;
                            AWidth, AHeight, APitch: Integer;
                            AFormat: IPTCFormat;
                            APalette: IPTCPalette);
var
  ConsolePixels: Pointer;
begin
  check_open;
  check_unlocked;
  if Clip.Equals(Area) then
  begin
    ConsolePixels := Lock;
    try
      try
        FCopy.Request(Format, AFormat);
        FCopy.Palette(Palette, APalette);
        FCopy.Copy(ConsolePixels, 0, 0, Width, Height, Pitch, APixels, 0, 0,
                    AWidth, AHeight, APitch);
      except
        on error: TPTCError do
        begin
          raise TPTCError.Create('failed to save console pixels', error);
        end;
      end;
    finally
      Unlock;
    end;
  end
  else
    Save(APixels, AWidth, AHeight, APitch, AFormat, APalette, Area, TPTCArea.Create(0, 0, Width, Height));
end;

procedure TVESAConsole.Save(APixels: Pointer;
                            AWidth, AHeight, APitch: Integer;
                            AFormat: IPTCFormat;
                            APalette: IPTCPalette;
                            ASource, ADestination: IPTCArea);
var
  ConsolePixels: Pointer;
  ClippedSource, ClippedDestination: IPTCArea;
begin
  check_open;
  check_unlocked;
  ConsolePixels := Lock;
  try
    try
      TPTCClipper.Clip(ASource, Clip, ClippedSource, ADestination, TPTCArea.Create(0, 0, AWidth, AHeight), ClippedDestination);
      FCopy.Request(Format, AFormat);
      FCopy.Palette(Palette, APalette);
      FCopy.Copy(ConsolePixels, ClippedSource.Left, ClippedSource.Top, ClippedSource.Width, ClippedSource.Height, Pitch,
                 APixels, ClippedDestination.Left, ClippedDestination.Top, ClippedDestination.Width, ClippedDestination.Height, APitch);
    except
      on error:TPTCError do
      begin
        raise TPTCError.Create('failed to save console area pixels', error);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TVESAConsole.Clear;
var
  Color: IPTCColor;
begin
  check_open;
  check_unlocked;
  if Format.Direct then
    Color := TPTCColor.Create(0, 0, 0, 0)
  else
    Color := TPTCColor.Create(0);
  Clear(Color);
end;

procedure TVESAConsole.Clear(AColor: IPTCColor);
var
  tmp: TPTCArea;
begin
  check_open;
  check_unlocked;
  Clear(AColor, TPTCArea.Create);
end;

procedure TVESAConsole.Clear(AColor: IPTCColor;
                             AArea: IPTCArea);
begin
  check_open;
  check_unlocked;
  {...}
end;

procedure TVESAConsole.Palette(APalette: IPTCPalette);
begin
  check_open;

  if Format.Indexed then
  begin
    FPalette.Load(APalette.Data);
    SetPalette(APalette.Data, 0, 256);
  end;
end;

function TVESAConsole.Palette: IPTCPalette;
begin
  check_open;
  Result := FPalette;
end;

procedure TVESAConsole.Clip(AArea: IPTCArea);
var
  tmp: TPTCArea;
begin
  check_open;
  FClip := TPTCClipper.Clip(AArea, FArea);
end;

function TVESAConsole.GetWidth: Integer;
begin
  check_open;
  Result := FWidth;
end;

function TVESAConsole.GetHeight: Integer;
begin
  check_open;
  Result := FHeight;
end;

function TVESAConsole.GetPitch: Integer;
begin
  check_open;
  Result := FPitch;
end;

function TVESAConsole.GetPages: Integer;
begin
  check_open;
  Result := 2;{FPrimary.pages;}
end;

function TVESAConsole.GetArea: IPTCArea;
begin
  check_open;
  Result := FArea;
end;

function TVESAConsole.Clip: IPTCArea;
begin
  check_open;
  Result := FClip;
end;

function TVESAConsole.GetFormat: IPTCFormat;
begin
  check_open;
  Result := FModes[FCurrentMode].Format;
end;

function TVESAConsole.GetName: string;
begin
  Result := 'VESA';
end;

function TVESAConsole.GetTitle: string;
begin
  Result := FTitle;
end;

function TVESAConsole.GetInformation: string;
begin
  Result := FInformation;
end;

procedure TVESAConsole.internal_close;
begin
  FreeMemAndNil(FPrimary);
  FreeAndNil(FKeyboard);
  FreeAndNil(FMouse);
  FreeAndNil(FEventQueue);
  RestoreTextMode;
end;

procedure TVESAConsole.HandleEvents;
begin
  FKeyboard.GetPendingEvents(FEventQueue);
  FMouse.GetPendingEvents(FEventQueue);
end;

function TVESAConsole.NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  check_open;

  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    AEvent := FEventQueue.NextEvent(AEventMask);
  until (not AWait) or (AEvent <> nil);
  Result := AEvent <> nil;
end;

function TVESAConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;
begin
  check_open;

  repeat
    { get events }
    HandleEvents;

    { try to find an event that matches the EventMask }
    Result := FEventQueue.PeekEvent(AEventMask);
  until (not AWait) or (Result <> nil);
end;

procedure TVESAConsole.check_open;
begin
  if not FOpen then
    raise TPTCError.Create('console is not open');
end;

procedure TVESAConsole.check_unlocked;
begin
  if FLocked then
    raise TPTCError.Create('console is not unlocked');
end;
