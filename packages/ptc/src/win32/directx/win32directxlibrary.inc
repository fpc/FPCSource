{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2009, 2010  Nikolay Nikolov (nickysn@users.sourceforge.net)
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

constructor TDirectXLibrary.Create;
begin
  try
    LOG('loading ddraw.dll');
    FLibrary := LoadLibrary('ddraw.dll');
    if FLibrary = 0 then
      raise TPTCError.Create('could not load ddraw.dll');

    LOG('importing DirectDrawCreate');
    Pointer(DirectDrawCreate) := GetProcAddress(FLibrary, 'DirectDrawCreate');
    if DirectDrawCreate = nil then
      raise TPTCError.Create('could not get address of DirectDrawCreate');

    LOG('creating lpDD');
    DirectXCheck(DirectDrawCreate(nil, FlpDD, nil));

    LOG('querying lpDD2');
    FlpDD2 := FlpDD as IDirectDraw2;
  except
    on error: TPTCError do
    begin
      { close }
      Close;

      { rethrow }
      raise TPTCError.Create('could not initialize ddraw', error);
    end;
  end;
end;

destructor TDirectXLibrary.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TDirectXLibrary.Close;
begin
  if FlpDD2 <> nil then
  begin
    LOG('releasing lpDD2');
    FlpDD2 := nil;
  end;

  if FlpDD <> nil then
  begin
    LOG('releasing lpDD');
    FlpDD := nil;
  end;

  if FLibrary <> 0 then
  begin
    LOG('closing ddraw.dll');
    FreeLibrary(FLibrary);
    FLibrary := 0;
  end;
end;
