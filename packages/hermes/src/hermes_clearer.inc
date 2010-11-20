{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

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

type
  PClearerInstance = ^TClearerInstance;
  TClearerInstance = record
    format: PHermesFormat;
    func: THermesClearPtr;
  end;

function Hermes_ClearerInstance: THermesClearerHandle;
var
  newinstance: PClearerInstance;
begin
  New(newinstance);
  newinstance^.func := nil;
  newinstance^.format := Hermes_FormatNewEmpty;
  Result := THermesClearerHandle(newinstance);
end;

procedure Hermes_ClearerReturn(handle: THermesClearerHandle);
begin
  if handle <> nil then
  begin
    Hermes_FormatFree(PClearerInstance(handle)^.format);
    Dispose(PClearerInstance(handle));
  end;
end;

function Hermes_ClearerRequest(handle: THermesClearerHandle; format: PHermesFormat): Boolean;
var
  clr: PClearerInstance;
  i: Integer;
begin
  if handle = nil then
  begin
    Result := False;
    exit;
  end;
  clr := PClearerInstance(handle);

  { if the clearer is the same, return 1 }
  if Hermes_FormatEquals(clr^.format, format) then
  begin
    Result := True;
    exit;
  end;

  { Otherwise look for a new clearer }
  clr^.func := nil;
  for i := 0 to numClearers - 1 do
  begin
    if Clearers[i]^.bits = format^.bits then
    begin
      clr^.func := Clearers[i]^.func;
      Hermes_FormatCopy(format, clr^.format);
      Result := True;
      exit;
    end;
  end;
  Result := False;
end;

function Hermes_ClearerClear(handle: THermesClearerHandle; pixels: Pointer;
                             x1, y1, width, height, pitch: Integer;
                             r, g, b: Uint32; index: Uint8): Boolean;
var
  info: THermesGenericInfo;
  clr: PClearerInstance;
  pixelval, d_r, d_g, d_b, d_a: Uint32;
  iface: THermesClearInterface;
begin
  if (height <= 0) or (width <= 0) then
  begin
    Result := True;
    exit;
  end;

  if handle = nil then
  begin
    Result := False;
    exit;
  end;
  clr := PClearerInstance(handle);

  { No conversion function assigned }
  if clr^.func = nil then
  begin
    Result := False;
    exit;
  end;

  if clr^.format^.indexed then
    pixelval := index
  else
  begin
    Hermes_Calculate_Generic_Info(24, 16, 8, 32,
                                  Hermes_Topbit(clr^.format^.r),
                                  Hermes_Topbit(clr^.format^.g),
                                  Hermes_Topbit(clr^.format^.b),
                                  Hermes_Topbit(clr^.format^.a), @info);
    pixelval := (index shl 24) or (r shl 16) or (g shl 8) or b;
    d_r := ((pixelval shr info.r_right) shl info.r_left) and clr^.format^.r;
    d_g := ((pixelval shr info.g_right) shl info.g_left) and clr^.format^.g;
    d_b := ((pixelval shr info.b_right) shl info.b_left) and clr^.format^.b;
    d_a := ((pixelval shr info.a_right) shl info.a_left) and clr^.format^.a;
    pixelval := d_r or d_g or d_b or d_a;
  end;
  iface.dest := pixels;
  Inc(iface.dest, y1*pitch + x1*(clr^.format^.bits shr 3));

  iface.width := width;
  iface.height := height;
  iface.add := pitch - width * (clr^.format^.bits shr 3);

  iface.value := pixelval;

  { Optimization }
  if iface.add = 0 then
  begin
    iface.width := iface.width * iface.height;
    iface.height := 1;
  end;

  clr^.func(@iface);
  Result := True;
end;
