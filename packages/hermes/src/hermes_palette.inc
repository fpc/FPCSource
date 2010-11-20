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
  PHermesLookupTable = ^THermesLookupTable;
  THermesLookupTable = record
    data: ^Uint32;             { Actual lookup table }
    valid: Boolean;           { Is this table up to date? }
    format: THermesFormat;    { Format of lookup table }
  end;
  PHermesPalette = ^THermesPalette;
  THermesPalette = record
    data: ^Uint32;             { Palette data }
    tables: PHermesList;      { Linked list of HermesLookupTables }
  end;

function Hermes_PaletteInstance: THermesPaletteHandle;
var
  newinstance: PHermesPalette;
begin
  { Create a new palette structure }
  New(newinstance);
  { Create palette data }
  newinstance^.data := GetMem(256*SizeOf(Uint32));
  { Create lookup table list }
  newinstance^.tables := Hermes_ListNew;

  Result := THermesPaletteHandle(newinstance);
end;

procedure HermesLookupTable_FreeCallback(q: Pointer);
begin
  FreeMem(PHermesLookupTable(q)^.data);
  Dispose(PHermesLookupTable(q));
end;

procedure Hermes_PaletteReturn(handle: THermesPaletteHandle);
begin
  if handle <> nil then
  begin
    { Free palette data and lookup tables }
    FreeMem(PHermesPalette(handle)^.data);
    Hermes_ListDestroy(PHermesPalette(handle)^.tables, @HermesLookupTable_FreeCallback);
    Dispose(PHermesPalette(handle));
  end;
end;

procedure Hermes_PaletteSet(handle: THermesPaletteHandle; palette: Pointer);
var
  element: PHermesListElement;
  pal: PHermesPalette;
begin
{  DebugMSG('Hermes_PaletteSet('+C2Str(handle)+','+C2Str(DWord(palette))+')');}
  if handle = nil then
    exit;

  pal := PHermesPalette(handle);
  element := pal^.tables^.first;

  { Invalidate all lookup tables }
  while element <> nil do
  begin
    (PHermesLookupTable(element^.data))^.valid := False;
    element := element^.next;
  end;

{  FillChar(palette^, 256*4, $7f);}
  Move(palette^, pal^.data^, 256*4);
end;

function Hermes_PaletteGet(handle: THermesPaletteHandle): Pointer;
begin
  if handle = nil then
  begin
    Result := nil;
    exit;
  end;

  Result := PHermesPalette(handle)^.data;
end;

procedure Hermes_PaletteMakeLookup(lookup, palette: PUint32;
                                   format: PHermesFormat);
var
  info: THermesGenericInfo;
  I: Integer;
  r, g, b: Uint32;
begin
{  DebugMSG('Yo! Hermes_PaletteMakeLookup');}
  r := 0;
  g := 0;
  b := 0;
  if format^.indexed then
    exit;

  Hermes_Calculate_Generic_Info(24,16,8,32,
                                Hermes_Topbit(format^.r),
                                Hermes_Topbit(format^.g),
                                Hermes_Topbit(format^.b),
                                Hermes_Topbit(format^.a),
                                @info);

  { Optimised loop if there are no left shifts }
  if (info.r_left = 0) and (info.g_left = 0) and (info.b_left = 0) then
    for I := 0 to 255 do
    begin
      r := (palette[i] shr info.r_right) and format^.r;
      g := (palette[i] shr info.g_right) and format^.g;
      b := (palette[i] shr info.b_right) and format^.b;
      lookup[i] := r or g or b;
    end
  else
    for I := 0 to 255 do
    begin
      r := ((palette[i] shr info.r_right) shl info.r_left) and format^.r;
      g := ((palette[i] shr info.g_right) shl info.g_left) and format^.g;
      b := ((palette[i] shr info.b_right) shl info.b_left) and format^.b;
      lookup[i] := r or g or b;
    end;
end;

function Hermes_PaletteGetTable(palette: THermesPaletteHandle; format: PHermesFormat): Pointer;
var
  element: PHermesListElement;
  pal: PHermesPalette;
  table: PHermesLookupTable;
begin
  if palette = nil then
  begin
    Result := nil;
    exit;
  end;

  pal := PHermesPalette(palette);

  { Go to the first table in the list }
  element := pal^.tables^.first;

  { Search for correct table using format }
  while element <> nil do
  begin
    table := element^.data;

    if Hermes_FormatEquals(@table^.format, format) then
    begin
      if table^.valid then
      begin
        Result := table^.data;
        exit;
      end;

      { Have to recreate the lookup table }
      Hermes_PaletteMakeLookup(table^.data, pal^.data, format);
      table^.valid := True;

      Result := table^.data;
      exit;
    end;

    element := element^.next;
  end;

  { Format not found, have to create a new table (need no handle) }
  New(table);
  table^.data := GetMem(1024);

  { Create lookup table }
  Hermes_PaletteMakeLookup(table^.data, pal^.data, format);
  Hermes_FormatCopy(format, @table^.format);

  table^.valid := True;

  { Create a new list element }
  element := Hermes_ListElementNew(0);
  element^.data := table;

  { Add to the front of the list }
  Hermes_ListAddFront(pal^.tables, element);

  { Return lookup data }
  Result := table^.data;
end;

procedure Hermes_PaletteInvalidateCache(handle: THermesPaletteHandle);
var
  element: PHermesListElement;
  pal: PHermesPalette;
begin
  if handle = nil then
    exit;

  pal := PHermesPalette(handle);

  element := pal^.tables^.first;

  { Invalidate all lookup tables }

  while element <> nil do
  begin
    (PHermesLookupTable(element^.data))^.valid := False;
    element := element^.next;
  end;
end;
