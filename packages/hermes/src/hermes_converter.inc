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

function Hermes_ConverterInstance(flags: DWord): THermesConverterHandle;
var
  newinstance: PHermesConverter;
begin
  { Create a HermesConverter }
  New(newinstance);

  { Zero it out }
  newinstance^.loopnormal := nil;
  newinstance^.loopstretch := nil;
  newinstance^.normal := nil;
  newinstance^.stretch := nil;
  newinstance^.dither := nil;
  newinstance^.ditherstretch := nil;
  newinstance^.flags := flags;
  FillChar(newinstance^.source, 0, SizeOf(THermesFormat));
  FillChar(newinstance^.dest, 0, SizeOf(THermesFormat));

  Result := THermesConverterHandle(newinstance);
end;

procedure Hermes_ConverterReturn(handle: THermesConverterHandle);
begin
  if handle = nil then
    exit;

  Dispose(PHermesConverter(handle));
end;

function Hermes_ConverterRequest(handle: THermesConverterHandle;
                                 source, dest: PHermesFormat): Boolean;
var
  searchlist: Integer;
  i: Integer;
  found: Boolean;
  cnv: PHermesConverter;
begin
{  DebugMSG('Hermes_ConverterRequest(' + C2Str(source^.bits)
   + ',' + C2Str(source^.r) + ',' + C2Str(source^.g) + ',' +
   C2Str(source^.b) + ';' + C2Str(dest^.bits)
   + ',' + C2Str(dest^.r) + ',' + C2Str(dest^.g) + ',' +
   C2Str(dest^.b) + ')');}

  Result := False;
  searchlist := 0;
  i := 0;
  found := False;
  { Check array ranges }
  if handle = nil then
    exit;
  cnv := PHermesConverter(handle);

  { Cache repeated requests of the same conversion }
  if Hermes_FormatEquals(source, @cnv^.source) and
     Hermes_FormatEquals(dest, @cnv^.dest) then
  begin
    Result := True;
    exit;
  end;

  { Clear the generic converter flag }
  cnv^.flags := cnv^.flags and (not HERMES_CONVERT_GENERIC);

  { if the source and destination are equal, use copy routines }
  if Hermes_FormatEquals(source, dest) then
  begin
{    DebugMSG('format equals!');}
    if ((source^.bits and 7) <> 0) or (source^.bits > 32) or
       (source^.bits = 0) then
      exit;
    i := (source^.bits shr 3) - 1;
    if equalConverters[i] = nil then
      exit;
    Hermes_FormatCopy(source, @cnv^.source);
    Hermes_FormatCopy(dest, @cnv^.dest);
    cnv^.loopnormal := equalConverters[i]^.loopnormal;
    cnv^.loopstretch := equalConverters[i]^.loopstretch;
    cnv^.normal := equalConverters[i]^.normal;
    cnv^.stretch := equalConverters[i]^.stretch;
    Result := True;
    exit;
  end;

  { Start looking for specialised converters }
  searchlist := $ff;
  case source^.bits of
    32: if (source^.r = $ff0000) and (source^.g = $ff00) and (source^.b = $ff) then
           searchlist := 0
         else
           if (source^.r = ($ff shl 20)) and
              (source^.g = ($ff shl 10)) and
              (source^.b = $ff) then
             searchlist := 3;
    24: if (source^.r = $ff0000) and (source^.g = $ff00) and (source^.b = $ff) then
           searchlist := 1;
    16: if (source^.r = $f800) and (source^.g = $7e0) and (source^.b = $1f) then
           searchlist := 2;
     8: if source^.indexed then
           searchlist := 4;
  end;

  { We can use a quicker loop for 8 bit }
  if searchlist <> $ff then
    if source^.bits = 8 then
    begin
      for i := 0 to numConverters[searchlist] - 1 do
        if standardConverters[searchlist][i] <> nil then
          if dest^.bits = standardConverters[searchlist][i]^.dest.bits then
          begin
            Hermes_FormatCopy(source, @cnv^.source);
            Hermes_FormatCopy(dest, @cnv^.dest);

            cnv^.loopnormal := standardConverters[searchlist][i]^.loopnormal;
            cnv^.loopstretch := standardConverters[searchlist][i]^.loopstretch;

            cnv^.normal := standardConverters[searchlist][i]^.normal;
            cnv^.stretch := standardConverters[searchlist][i]^.stretch;

            cnv^.dither := standardConverters[searchlist][i]^.dither;
            cnv^.ditherstretch := standardConverters[searchlist][i]^.ditherstretch;

            Result := True;
            exit;
          end;
    end
    else
      for i := 0 to numConverters[searchlist] - 1 do
        if standardConverters[searchlist][i] <> nil then
          if Hermes_FormatEquals(@standardConverters[searchlist][i]^.source, source) and
             Hermes_FormatEquals(@standardConverters[searchlist][i]^.dest, dest) then
          begin
            Hermes_FormatCopy(source, @cnv^.source);
            Hermes_FormatCopy(dest, @cnv^.dest);

            cnv^.loopnormal := standardConverters[searchlist][i]^.loopnormal;
            cnv^.loopstretch := standardConverters[searchlist][i]^.loopstretch;

            cnv^.normal := standardConverters[searchlist][i]^.normal;
            cnv^.stretch := standardConverters[searchlist][i]^.stretch;

            cnv^.dither := standardConverters[searchlist][i]^.dither;
            cnv^.ditherstretch := standardConverters[searchlist][i]^.ditherstretch;

            Result := True;
            exit;
          end;


  { Otherwise find a generic converter }
{  DebugMSG('looking for a generic converter!');}


  cnv^.loopnormal := nil;
  cnv^.loopstretch := nil;
  cnv^.dither := nil;
  cnv^.ditherstretch := nil;
  cnv^.flags := cnv^.flags or HERMES_CONVERT_GENERIC;


  { Generic routines implement whole converters not scanline converters,
    assign placeholders }

  cnv^.normal := @NotApplicable;
  cnv^.stretch := @NotApplicable;

  found := False;

{
        Converting rules:

        C -> C
        C -> A

        A -> O, A -> A
        A -> C

        O -> O , A, C are the same
}

  if source^.has_colorkey and dest^.has_colorkey then { Ck -> Ck }
    case source^.bits of
      32: case dest^.bits of
        32: begin
          cnv^.loopnormal := @ConvertP_Generic32_C_Generic32_C;
          cnv^.loopstretch := @ConvertP_Generic32_C_Generic32_C_S;
          found := True;
        end;
        24: begin
          cnv^.loopnormal := @ConvertP_Generic32_C_Generic24_C;
          found := True;
        end;
        16: begin
          cnv^.loopnormal := @ConvertP_Generic32_C_Generic16_C;
          cnv^.loopstretch := @ConvertP_Generic32_C_Generic16_C_S;
          found := True;
        end;
        8: begin
          cnv^.loopnormal := @ConvertP_Generic32_C_Generic8_C;
          found := True;
        end;
      end;
      24: case dest^.bits of
        32: begin
          cnv^.loopnormal := @ConvertP_Generic24_C_Generic32_C;
          found := True;
        end;
        24: begin
          cnv^.loopnormal := @ConvertP_Generic24_C_Generic24_C;
          found := True;
        end;
        16: begin
          cnv^.loopnormal := @ConvertP_Generic24_C_Generic16_C;
          found := True;
        end;
        8: begin
          cnv^.loopnormal := @ConvertP_Generic24_C_Generic8_C;
          found := True;
        end;
      end;
      16: case dest^.bits of
        32: begin
          cnv^.loopnormal := @ConvertP_Generic16_C_Generic32_C;
          found := True;
        end;
        24: begin
          cnv^.loopnormal := @ConvertP_Generic16_C_Generic24_C;
          found := True;
        end;
        16: begin
          cnv^.loopnormal := @ConvertP_Generic16_C_Generic16_C;
          found := True;
        end;
        8: begin
          cnv^.loopnormal := @ConvertP_Generic16_C_Generic8_C;
          found := True;
        end;
      end;
    end
  else
    if source^.has_colorkey and (dest^.a <> 0) then { Ck -> A }
      case source^.bits of
        32: case dest^.bits of
          32: begin
            cnv^.loopnormal := @ConvertP_Generic32_C_Generic32_A;
            cnv^.loopstretch := @ConvertP_Generic32_C_Generic32_A_S;
            found := True;
          end;
          24: begin
            cnv^.loopnormal := @ConvertP_Generic32_C_Generic24_A;
            found := True;
          end;
          16: begin
            cnv^.loopnormal := @ConvertP_Generic32_C_Generic16_A;
            cnv^.loopstretch := @ConvertP_Generic32_C_Generic16_A_S;
            found := True;
          end;
          8: begin
            cnv^.loopnormal := @ConvertP_Generic32_C_Generic8_A;
            found := True;
          end;
        end;
        24: case dest^.bits of
          32: begin
            cnv^.loopnormal := @ConvertP_Generic24_C_Generic32_A;
            found := True;
          end;
          24: begin
            cnv^.loopnormal := @ConvertP_Generic24_C_Generic24_A;
            found := True;
          end;
          16: begin
            cnv^.loopnormal := @ConvertP_Generic24_C_Generic16_A;
            found := True;
          end;
          8: begin
            cnv^.loopnormal := @ConvertP_Generic24_C_Generic8_A;
            found := True;
          end;
        end;
        16: case dest^.bits of
          32: begin
            cnv^.loopnormal := @ConvertP_Generic16_C_Generic32_A;
            found := True;
          end;
          24: begin
            cnv^.loopnormal := @ConvertP_Generic16_C_Generic24_A;
            found := True;
          end;
          16: begin
            cnv^.loopnormal := @ConvertP_Generic16_C_Generic16_A;
            found := True;
          end;
          8: begin
            cnv^.loopnormal := @ConvertP_Generic16_C_Generic8_A;
            found := True;
          end;
        end;
      end
    else
      if (source^.a <> 0) and dest^.has_colorkey then { A -> Ck }
        case source^.bits of
          32: case dest^.bits of
            32: begin
              cnv^.loopnormal := @ConvertP_Generic32_A_Generic32_C;
              cnv^.loopstretch := @ConvertP_Generic32_A_Generic32_C_S;
              found := True;
            end;
            24: begin
              cnv^.loopnormal := @ConvertP_Generic32_A_Generic24_C;
              found := True;
            end;
            16: begin
              cnv^.loopnormal := @ConvertP_Generic32_A_Generic16_C;
              cnv^.loopnormal := @ConvertP_Generic32_A_Generic16_C_S;
              found := True;
            end;
            8: begin
              cnv^.loopnormal := @ConvertP_Generic32_A_Generic8_C;
              found := True;
            end;
          end;
          24: case dest^.bits of
            32: begin
              cnv^.loopnormal := @ConvertP_Generic24_A_Generic32_C;
              found := True;
            end;
            24: begin
              cnv^.loopnormal := @ConvertP_Generic24_A_Generic24_C;
              found := True;
            end;
            16: begin
              cnv^.loopnormal := @ConvertP_Generic24_A_Generic16_C;
              found := True;
            end;
            8: begin
              cnv^.loopnormal := @ConvertP_Generic24_A_Generic8_C;
              found := True;
            end;
          end;
          16: case dest^.bits of
            32: begin
              cnv^.loopnormal := @ConvertP_Generic16_A_Generic32_C;
              found := True;
            end;
            24: begin
              cnv^.loopnormal := @ConvertP_Generic16_A_Generic24_C;
              found := True;
            end;
            16: begin
              cnv^.loopnormal := @ConvertP_Generic16_A_Generic16_C;
              found := True;
            end;
            8: begin
              cnv^.loopnormal := @ConvertP_Generic16_A_Generic8_C;
              found := True;
            end;
          end;
        end
      else
        if (source^.a <> 0) and (dest^.a <> 0) then { A -> A }
          case source^.bits of
            32: case dest^.bits of
              32: begin
                cnv^.loopnormal := @ConvertP_Generic32_A_Generic32_A;
                cnv^.loopstretch := @ConvertP_Generic32_A_Generic32_A_S;
                found := True;
              end;
              24: begin
                cnv^.loopnormal := @ConvertP_Generic32_A_Generic24_A;
                found := True;
              end;
              16: begin
                cnv^.loopnormal := @ConvertP_Generic32_A_Generic16_A;
                cnv^.loopstretch := @ConvertP_Generic32_A_Generic16_A_S;
                found := True;
              end;
              8: begin
                cnv^.loopnormal := @ConvertP_Generic32_A_Generic8_A;
                found := True;
              end;
            end;
            24: case dest^.bits of
              32: begin
                cnv^.loopnormal := @ConvertP_Generic24_A_Generic32_A;
                found := True;
              end;
              24: begin
                cnv^.loopnormal := @ConvertP_Generic24_A_Generic24_A;
                found := True;
              end;
              16: begin
                cnv^.loopnormal := @ConvertP_Generic24_A_Generic16_A;
                found := True;
              end;
              8: begin
                cnv^.loopnormal := @ConvertP_Generic24_A_Generic8_A;
                found := True;
              end;
            end;
            16: case dest^.bits of
              32: begin
                cnv^.loopnormal := @ConvertP_Generic16_A_Generic32_A;
                found := True;
              end;
              24: begin
                cnv^.loopnormal := @ConvertP_Generic16_A_Generic24_A;
                found := True;
              end;
              16: begin
                cnv^.loopnormal := @ConvertP_Generic16_A_Generic16_A;
                found := True;
              end;
              8: begin
                cnv^.loopnormal := @ConvertP_Generic16_A_Generic8_A;
                found := True;
              end;
            end;
          end
        else { O->O, O->A, A->O, Ck->O, O->Ck }
          case source^.bits of
            32: case dest^.bits of
              32: begin
                cnv^.loopnormal := @ConvertP_Generic32_Generic32;
                cnv^.loopstretch := @ConvertP_Generic32_Generic32_S;
                found := True;
              end;
              24: begin
                cnv^.loopnormal := @ConvertP_Generic32_Generic24;
                cnv^.loopstretch := @ConvertP_Generic32_Generic24_S;
                found := True;
              end;
              16: begin
                cnv^.loopnormal := @ConvertP_Generic32_Generic16;
                cnv^.loopstretch := @ConvertP_Generic32_Generic16_S;
                found := True;
              end;
              8: begin
                cnv^.loopnormal := @ConvertP_Generic32_Generic8;
                cnv^.loopstretch := @ConvertP_Generic32_Generic8_S;
                found := True;
              end;
            end;
            24: case dest^.bits of
              32: begin
                cnv^.loopnormal := @ConvertP_Generic24_Generic32;
                cnv^.loopstretch := @ConvertP_Generic24_Generic32_S;
                found := True;
              end;
              24: begin
                cnv^.loopnormal := @ConvertP_Generic24_Generic24;
                cnv^.loopstretch := @ConvertP_Generic24_Generic24_S;
                found := True;
              end;
              16: begin
                cnv^.loopnormal := @ConvertP_Generic24_Generic16;
                cnv^.loopstretch := @ConvertP_Generic24_Generic16_S;
                found := True;
              end;
              8: begin
                cnv^.loopnormal := @ConvertP_Generic24_Generic8;
                cnv^.loopstretch := @ConvertP_Generic24_Generic8_S;
                found := True;
              end;
            end;
            16: case dest^.bits of
              32: begin
                cnv^.loopnormal := @ConvertP_Generic16_Generic32;
                cnv^.loopstretch := @ConvertP_Generic16_Generic32_S;
                found := True;
              end;
              24: begin
                cnv^.loopnormal := @ConvertP_Generic16_Generic24;
                cnv^.loopstretch := @ConvertP_Generic16_Generic24_S;
                found := True;
              end;
              16: begin
                cnv^.loopnormal := @ConvertP_Generic16_Generic16;
                cnv^.loopstretch := @ConvertP_Generic16_Generic16_S;
                found := True;
              end;
              8: begin
                cnv^.loopnormal := @ConvertP_Generic16_Generic8;
                cnv^.loopstretch := @ConvertP_Generic16_Generic8_S;
                found := True;
              end;
            end;
          end;

  if found then
  begin
    Hermes_FormatCopy(source, @cnv^.source);
    Hermes_FormatCopy(dest, @cnv^.dest);

    Result := True;
    exit;
  end;

  DebugMSG('no converter found!!!');
  { No converter found, fail }
  Result := False;
end;

function Hermes_ConverterPalette(handle: THermesConverterHandle; sourcepal, destpal: THermesPaletteHandle): Boolean;
var
  cnv: PHermesConverter;
begin
{  DebugMSG('Hermes_ConverterPalette('+C2Str(sourcepal)+','+C2Str(destpal)+')');}
  Result := False;
  if handle = nil then
    exit;

  cnv := PHermesConverter(handle);

  { Fail silently if not indexed colour format }
  if not cnv^.source.indexed then
  begin
    cnv^.lookup := nil;
    Result := True;
    exit;
  end;

  cnv^.lookup := Hermes_PaletteGetTable(sourcepal, @cnv^.dest);

  if cnv^.lookup = nil then
    exit;

  Result := True;
end;

function Hermes_ConverterCopy(handle: THermesConverterHandle; s_pixels: Pointer;
                              s_x, s_y, s_width, s_height, s_pitch: Integer;
                              d_pixels: Pointer; d_x, d_y, d_width,
                              d_height, d_pitch: Integer): Boolean;
var
  cnv: PHermesConverter;
  iface: THermesConverterInterface;
begin
  Result := False;
  if handle = nil then
    exit;
//  cnv := ConverterList[handle];
  cnv := PHermesConverter(handle);
//  if cnv = nil then
//    exit;

  { Returns success if height or width is zero. This is debatable.. ! }
  if (s_width <= 0) or (s_height <= 0) or (d_width <= 0) or (d_height <= 0) then
  begin
    Result := True;
    exit;
  end;

  iface.s_pixels := s_pixels;
  iface.s_width := s_width;
  iface.s_height := s_height;
  iface.s_add := s_pitch - s_width * (cnv^.source.bits shr 3);
  iface.s_pitch := s_pitch;

  iface.d_pixels := d_pixels;
  iface.d_width := d_width;
  iface.d_height := d_height;
  iface.d_add := d_pitch - d_width*(cnv^.dest.bits shr 3);
  iface.d_pitch := d_pitch;

  Inc(iface.s_pixels, s_y * s_pitch + s_x * (cnv^.source.bits shr 3));
  Inc(iface.d_pixels, d_y * d_pitch + d_x * (cnv^.dest.bits shr 3));

  iface.s_has_colorkey := cnv^.source.has_colorkey;
  iface.d_has_colorkey := cnv^.dest.has_colorkey;
  iface.s_colorkey := cnv^.source.colorkey;
  iface.d_colorkey := cnv^.dest.colorkey;

  iface.lookup := cnv^.lookup;


  { for generic converters, do some extra setup (find shifts, etc.)
    TODO: Move that out of here and in the request routine ! }
  if (cnv^.flags and HERMES_CONVERT_GENERIC) <> 0 then
  begin
    Hermes_Calculate_Generic_Info(Hermes_Topbit(cnv^.source.r),
                                  Hermes_Topbit(cnv^.source.g),
                                  Hermes_Topbit(cnv^.source.b),
                                  Hermes_Topbit(cnv^.source.a),
                                  Hermes_Topbit(cnv^.dest.r),
                                  Hermes_Topbit(cnv^.dest.g),
                                  Hermes_Topbit(cnv^.dest.b),
                                  Hermes_Topbit(cnv^.dest.a),
                                  @iface.info);
    iface.mask_r := cnv^.dest.r;
    iface.mask_g := cnv^.dest.g;
    iface.mask_b := cnv^.dest.b;
    iface.mask_a := cnv^.dest.a;
  end;

  { Check for dithering. This should not be in here but in request as well }
  if (cnv^.flags and HERMES_CONVERT_DITHER) <> 0 then
  begin
    { if there is a ditherer, use it else fall back to normal }
    if cnv^.dither <> nil then
      cnv^.loopnormal := cnv^.dither;
  end;

  { Normal conversion }
  if (s_width = d_width) and (s_height = d_height) then
  begin
    if (cnv^.normal = nil) or (cnv^.loopnormal = nil) then
      exit;
    { Optimization
    if (iface.s_add = 0) and (iface.d_add = 0) then
    begin
      iface.s_width := iface.s_width * s_height;
      iface.d_width := iface.d_width * d_height;
      iface.s_height := 1;
      iface.d_height := 1;
    end;}
    iface.func := cnv^.normal;
    cnv^.loopnormal(@iface);
    Result := True;
    exit;
  end
  { Stretch conversion }
  else
  begin
    if (cnv^.stretch = nil) or (cnv^.loopstretch = nil) then
      exit;
    iface.func := cnv^.stretch;
    cnv^.loopstretch(@iface);
  end;
  Result := True;
end;
