{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2012 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

{$WARNING add these to the windows unit}
const
  PFD_DIRECT3D_ACCELERATED = $00004000;
  PFD_SUPPORT_COMPOSITION = $00008000;

constructor TWin32OpenGLWindow.Create(const AWndClass, ATitle: string; AExtra, AStyle, AClassStyle: DWord;
              AShow, AX, AY, AWidth, AHeight: Integer; ACenter, AMultithreaded, ACursor, AInterceptClose: Boolean;
              const AOpenGLAttributes: IPTCOpenGLAttributes);
begin
  SetOpenGLAttributes(AOpenGLAttributes);
  inherited Create(AWndClass, ATitle, AExtra, AStyle, AClassStyle,
                   AShow, AX, AY, AWidth, AHeight, ACenter, AMultithreaded, ACursor, AInterceptClose);
end;

function TWin32OpenGLWindow.WMCreate(
                              hWnd: HWND;
                              uMsg: UINT;
                              wParam: WPARAM;
                              lParam: LPARAM): LRESULT;
var
  dc: HDC;
  Context: HGLRC;
begin
  LOG('inside OpenGL WM_CREATE handler');
  LOG('getting device context');
  dc := GetDC(hWnd);
  if dc = 0 then
  begin
    LOG('GetDC returned an error, failing WM_CREATE');
    exit(-1);
  end;

{$IFDEF DEBUG}
  LOG('enumerating all pixel formats available on this device context');
  if not EnumerateAllPixelFormats(dc) then
  begin
    LOG('error enumerating pixel formats, failing WM_CREATE');
    LOG('ReleaseDC');
    if ReleaseDC(hWnd, dc) = 0 then
      LOG('ReleaseDC failed');
    exit(-1);
  end;
{$ENDIF DEBUG}

  LOG('setting up OpenGL pixel format');
  if not Self.SetupOpenGLPixelFormat(dc) then
  begin
    LOG('error setting up OpenGL pixel format, failing WM_CREATE');
    LOG('ReleaseDC');
    if ReleaseDC(hWnd, dc) = 0 then
      LOG('ReleaseDC failed');
    exit(-1);
  end;

  LOG('creating OpenGL rendering context');
  Context := wglCreateContext(dc);
  if Context = 0 then
  begin
    LOG('error creating OpenGL rendering context, failing WM_CREATE');
    LOG('ReleaseDC');
    if ReleaseDC(hWnd, dc) = 0 then
      LOG('ReleaseDC failed');
    exit(-1);
  end;

  LOG('making it current');
  if not wglMakeCurrent(dc, Context) then
  begin
    LOG('error making the OpenGL rendering context current, failing WM_CREATE');
    LOG('ReleaseDC');
    if ReleaseDC(hWnd, dc) = 0 then
      LOG('ReleaseDC failed');
    LOG('wglDeleteContext');
    if not wglDeleteContext(Context) then
      LOG('wglDeleteContext failed');
    exit(-1);
  end;

  LOG('WM_CREATE success');
  Result := 0;
end;

function TWin32OpenGLWindow.WMDestroy(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  dc: HDC;
  Context: HGLRC;
begin
  LOG('inside OpenGL WM_DESTROY handler');

  Context := wglGetCurrentContext;
  if Context <> 0 then
  begin
    dc := wglGetCurrentDC;

    LOG('wglMakeCurrent(0, 0)');
    if not wglMakeCurrent(0, 0) then
      LOG('wglMakeCurrent(0, 0) failed');

    if dc <> 0 then
    begin
      LOG('ReleaseDC');
      if ReleaseDC(hWnd, dc) = 0 then
        LOG('ReleaseDC failed');
    end
    else
      LOG('no WGL device context to release');

    LOG('wglDeleteContext');
    if not wglDeleteContext(Context) then
      LOG('wglDeleteContext failed');
  end
  else
    LOG('no current context to cleanup');

  LOG('WM_DESTROY done');

  inherited;
end;

procedure TWin32OpenGLWindow.SetOpenGLAttributes(const AOpenGLAttributes: IPTCOpenGLAttributes);
var
  Flags: DWORD;
begin
  Flags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;

  if AOpenGLAttributes.DoubleBufferDontCare then
    Flags := Flags or PFD_DOUBLEBUFFER_DONTCARE
  else
    if AOpenGLAttributes.DoubleBuffer then
	  Flags := Flags or PFD_DOUBLEBUFFER;

  if AOpenGLAttributes.StereoDontCare then
    Flags := Flags or PFD_STEREO_DONTCARE
  else
    if AOpenGLAttributes.Stereo then
	  Flags := Flags or PFD_STEREO;

  FillChar(FPixelFormatDescriptor, SizeOf(FPixelFormatDescriptor), 0);
  FPixelFormatDescriptor.nSize := SizeOf(FPixelFormatDescriptor);
  FPixelFormatDescriptor.nVersion := 1;
  FPixelFormatDescriptor.dwFlags := Flags;
  FPixelFormatDescriptor.iPixelType := PFD_TYPE_RGBA;
  FPixelFormatDescriptor.cColorBits := AOpenGLAttributes.BufferSize;
  FPixelFormatDescriptor.cRedBits := 0;
  FPixelFormatDescriptor.cRedShift := 0;
  FPixelFormatDescriptor.cGreenBits := 0;
  FPixelFormatDescriptor.cGreenShift := 0;
  FPixelFormatDescriptor.cBlueBits := 0;
  FPixelFormatDescriptor.cBlueShift := 0;
  FPixelFormatDescriptor.cAlphaBits := 0;
  FPixelFormatDescriptor.cAlphaShift := 0;
  FPixelFormatDescriptor.cAccumBits := 0;
  FPixelFormatDescriptor.cAccumRedBits := 0;
  FPixelFormatDescriptor.cAccumGreenBits := 0;
  FPixelFormatDescriptor.cAccumBlueBits := 0;
  FPixelFormatDescriptor.cAccumAlphaBits := 0;
  FPixelFormatDescriptor.cDepthBits := AOpenGLAttributes.DepthSize;
  FPixelFormatDescriptor.cStencilBits := AOpenGLAttributes.StencilSize;
  FPixelFormatDescriptor.cAuxBuffers := 0;
  FPixelFormatDescriptor.iLayerType := PFD_MAIN_PLANE;
  FPixelFormatDescriptor.bReserved := 0;
  FPixelFormatDescriptor.dwLayerMask := 0;
  FPixelFormatDescriptor.dwVisibleMask := 0;
  FPixelFormatDescriptor.dwDamageMask := 0;
end;

function TWin32OpenGLWindow.EnumerateAllPixelFormats(hdc: HDC): Boolean;
var
  pfd: PIXELFORMATDESCRIPTOR;
  pf_index, pf_count: Integer;
begin
  pf_count := DescribePixelFormat(hdc, 1, 0, nil);
  if pf_count = 0 then
  begin
    LOG('DescribePixelFormat failed');
    exit(False);
  end;
  LOG('pixel formats count', pf_count);

  for pf_index := 1 to pf_count do
  begin
    FillChar(pfd, SizeOf(pfd), 0);
    pfd.nSize := SizeOf(pfd);

    LOG('describing pixel format ' + IntToStr(pf_index));
    if DescribePixelFormat(hdc, pf_index, SizeOf(pfd), @pfd) = 0 then
    begin
      LOG('DescribePixelFormat failed');
      exit(False);
    end;
    LogPixelFormatDescriptor(pfd);
  end;
end;

function TWin32OpenGLWindow.SetupOpenGLPixelFormat(hdc: HDC): Boolean;
var
  pf_index: Integer;
begin
  LOG('calling ChoosePixelFormat with:');
  LogPixelFormatDescriptor(FPixelFormatDescriptor);

  pf_index := ChoosePixelFormat(hdc, FPixelFormatDescriptor);
  if pf_index = 0 then
  begin
    LOG('ChoosePixelFormat failed');
    exit(False);
  end;
  LOG('ChoosePixelFormat result', pf_index);

  LOG('getting description');
  if DescribePixelFormat(hdc, pf_index, SizeOf(FChosenPixelFormatDescriptor), @FChosenPixelFormatDescriptor) = 0 then
  begin
    LOG('DescribePixelFormat failed');
    exit(False);
  end;
  LogPixelFormatDescriptor(FChosenPixelFormatDescriptor);

  LOG('setting pixel format');
  if not SetPixelFormat(hdc, pf_index, @FPixelFormatDescriptor) then
  begin
    LOG('SetPixelFormat failed');
    exit(False);
  end;

  Result := True;
end;

procedure TWin32OpenGLWindow.LogPixelFormatDescriptor(const pfd: PIXELFORMATDESCRIPTOR);

  function dwFlags2String(dwFlags: DWORD): string;
  begin
    Result := IntToStr(dwFlags) + ' (';
    if (dwFlags and PFD_DOUBLEBUFFER) <> 0 then
      Result := Result + 'PFD_DOUBLEBUFFER + ';
    if (dwFlags and PFD_STEREO) <> 0 then
      Result := Result + 'PFD_STEREO + ';
    if (dwFlags and PFD_DRAW_TO_WINDOW) <> 0 then
      Result := Result + 'PFD_DRAW_TO_WINDOW + ';
    if (dwFlags and PFD_DRAW_TO_BITMAP) <> 0 then
      Result := Result + 'PFD_DRAW_TO_BITMAP + ';
    if (dwFlags and PFD_SUPPORT_GDI) <> 0 then
      Result := Result + 'PFD_SUPPORT_GDI + ';
    if (dwFlags and PFD_SUPPORT_OPENGL) <> 0 then
      Result := Result + 'PFD_SUPPORT_OPENGL + ';
    if (dwFlags and PFD_GENERIC_FORMAT) <> 0 then
      Result := Result + 'PFD_GENERIC_FORMAT + ';
    if (dwFlags and PFD_NEED_PALETTE) <> 0 then
      Result := Result + 'PFD_NEED_PALETTE + ';
    if (dwFlags and PFD_NEED_SYSTEM_PALETTE) <> 0 then
      Result := Result + 'PFD_NEED_SYSTEM_PALETTE + ';
    if (dwFlags and PFD_SWAP_EXCHANGE) <> 0 then
      Result := Result + 'PFD_SWAP_EXCHANGE + ';
    if (dwFlags and PFD_SWAP_COPY) <> 0 then
      Result := Result + 'PFD_SWAP_COPY + ';
    if (dwFlags and PFD_SWAP_LAYER_BUFFERS) <> 0 then
      Result := Result + 'PFD_SWAP_LAYER_BUFFERS + ';
    if (dwFlags and PFD_GENERIC_ACCELERATED) <> 0 then
      Result := Result + 'PFD_GENERIC_ACCELERATED + ';
    if (dwFlags and PFD_SUPPORT_DIRECTDRAW) <> 0 then
      Result := Result + 'PFD_SUPPORT_DIRECTDRAW + ';
    if (dwFlags and PFD_DIRECT3D_ACCELERATED) <> 0 then
      Result := Result + 'PFD_DIRECT3D_ACCELERATED + ';
    if (dwFlags and PFD_SUPPORT_COMPOSITION) <> 0 then
      Result := Result + 'PFD_SUPPORT_COMPOSITION + ';
    if (dwFlags and PFD_DEPTH_DONTCARE) <> 0 then
      Result := Result + 'PFD_DEPTH_DONTCARE + ';
    if (dwFlags and PFD_DOUBLEBUFFER_DONTCARE) <> 0 then
      Result := Result + 'PFD_DOUBLEBUFFER_DONTCARE + ';
    if (dwFlags and PFD_STEREO_DONTCARE) <> 0 then
      Result := Result + 'PFD_STEREO_DONTCARE + ';
    if Copy(Result, Length(Result) - 2, 3) = ' + ' then
      Result := Copy(Result, 1, Length(Result) - 3);
    Result := Result + ')';
  end;

  function iPixelType2String(iPixelType: Byte): string;
  begin
    case iPixelType of
      PFD_TYPE_RGBA: Result := 'PFD_TYPE_RGBA';
      PFD_TYPE_COLORINDEX: Result := 'PFD_TYPE_COLORINDEX';
      else
        Result := 'Unknown';
    end;
    Result := IntToStr(iPixelType) + ' (' + Result + ')';
  end;

  function iLayerType2String(iLayerType: Byte): string;
  begin
    case iLayerType of
      PFD_MAIN_PLANE: Result := 'PFD_MAIN_PLANE';
      PFD_OVERLAY_PLANE: Result := 'PFD_OVERLAY_PLANE';
      Byte(PFD_UNDERLAY_PLANE): Result := 'PFD_UNDERLAY_PLANE';
      else
        Result := 'Unknown';
    end;
    Result := IntToStr(iLayerType) + ' (' + Result + ')';
  end;

begin
  LOG('PIXELFORMATDESCRIPTOR:');
  LOG('nSize          ', pfd.nSize);
  LOG('nVersion       ', pfd.nVersion);
  LOG('dwFlags        ', dwFlags2String(pfd.dwFlags));
  LOG('iPixelType     ', iPixelType2String(pfd.iPixelType));
  LOG('cColorBits     ', pfd.cColorBits);
  LOG('cRedBits       ', pfd.cRedBits);
  LOG('cRedShift      ', pfd.cRedShift);
  LOG('cGreenBits     ', pfd.cGreenBits);
  LOG('cGreenShift    ', pfd.cGreenShift);
  LOG('cBlueBits      ', pfd.cBlueBits);
  LOG('cBlueShift     ', pfd.cBlueShift);
  LOG('cAlphaBits     ', pfd.cAlphaBits);
  LOG('cAlphaShift    ', pfd.cAlphaShift);
  LOG('cAccumBits     ', pfd.cAccumBits);
  LOG('cAccumRedBits  ', pfd.cAccumRedBits);
  LOG('cAccumGreenBits', pfd.cAccumGreenBits);
  LOG('cAccumBlueBits ', pfd.cAccumBlueBits);
  LOG('cAccumAlphaBits', pfd.cAccumAlphaBits);
  LOG('cDepthBits     ', pfd.cDepthBits);
  LOG('cStencilBits   ', pfd.cStencilBits);
  LOG('cAuxBuffers    ', pfd.cAuxBuffers);
  LOG('iLayerType     ', iLayerType2String(pfd.iLayerType));
  LOG('bReserved      ', pfd.bReserved);
  LOG('dwLayerMask    ', pfd.dwLayerMask);
  LOG('dwVisibleMask  ', pfd.dwVisibleMask);
  LOG('dwDamageMask   ', pfd.dwDamageMask);
end;
