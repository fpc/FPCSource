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

{function Hermes_FormatNewEmpty: PHermesFormat;
function Hermes_FormatNew(bits: Integer; r, g, b, a: Uint32;
                          indexed: Boolean): PHermesFormat;
procedure Hermes_FormatFree(fmt: PHermesFormat);
function Hermes_FormatNewEx(bits: Integer; r, g, b, a: Uint32;
                            indexed, has_colorkey: Boolean;
                            colorkey: Uint32): PHermesFormat;
function Hermes_FormatEquals(op1, op2: PHermesFormat): Boolean;
procedure Hermes_FormatCopy(source, dest: PHermesFormat);}

function Hermes_FormatNewEmpty: PHermesFormat;
begin
  New(Result);
  Result^.bits := 0;
  Result^.indexed := False;
  Result^.r := 0;
  Result^.g := 0;
  Result^.b := 0;
  Result^.a := 0;
  Result^.has_colorkey := False;
  Result^.colorkey := 0;
end;

function Hermes_FormatNew(bits: Integer; r, g, b, a: Uint32;
                          indexed: Boolean): PHermesFormat;
begin
  if indexed and (bits <> 8) then
  begin
    Result := nil;
    exit;
  end;
  New(Result);
  Result^.bits := bits;
  Result^.r := r;
  Result^.g := g;
  Result^.b := b;
  Result^.a := a;
  Result^.indexed := indexed;
  Result^.has_colorkey := False;
  Result^.colorkey := 0;
end;

procedure Hermes_FormatFree(fmt: PHermesFormat);
begin
  if fmt <> nil then
    Dispose(fmt);
end;

function Hermes_FormatNewEx(bits: Integer; r, g, b, a: Uint32;
                            indexed, has_colorkey: Boolean;
                            colorkey: Uint32): PHermesFormat;
begin
  if indexed and (bits <> 8) then
  begin
    Result := nil;
    exit;
  end;
  New(Result);
  Result^.bits := bits;
  Result^.r := r;
  Result^.g := g;
  Result^.b := b;
  Result^.a := a;
  Result^.indexed := indexed;
  Result^.has_colorkey := has_colorkey;
  Result^.colorkey := colorkey;
end;

function Hermes_FormatEquals(op1, op2: PHermesFormat): Boolean;
begin
  Result := ((op1^.indexed = op2^.indexed) and
             (op1^.bits = op2^.bits) and
             (op1^.r = op2^.r) and
             (op1^.g = op2^.g) and
             (op1^.b = op2^.b) and
             (op1^.a = op2^.a) and
             (op1^.has_colorkey = op2^.has_colorkey) and
            ((op1^.has_colorkey = False) or
             (op1^.colorkey = op2^.colorkey)));
end;

procedure Hermes_FormatCopy(source, dest: PHermesFormat);
begin
  Move(source^, dest^, SizeOf(THermesFormat));
end;
