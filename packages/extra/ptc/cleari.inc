{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

Constructor TPTCClear.Create;

Begin
  Fformat := Nil;
  { initialize hermes }
  If Not Hermes_Init Then
    Raise TPTCError.Create('could not initialize hermes');

  { default current format }
  Fformat := TPTCFormat.Create;
  { create hermes clearer instance }
  Fhandle := Hermes_ClearerInstance;
  { check hermes clearer instance }
  If Fhandle = 0 Then
    Raise TPTCError.Create('could not create hermes clearer instance');
End;

Destructor TPTCClear.Destroy;

Begin
  { return the clearer instance }
  Hermes_ClearerReturn(Fhandle);
  Fformat.Free;
  { free hermes }
  Hermes_Done;
  
  Inherited Destroy;
End;

Procedure TPTCClear.request(Const format : TPTCFormat);

Var
  hermes_format : PHermesFormat;

Begin
  hermes_format := @format.Fformat;
  { request surface clear for this format }
  If Not Hermes_ClearerRequest(Fhandle, hermes_format) Then
    Raise TPTCError.Create('unsupported clear format');

  { update current format }
  Fformat.Assign(format);
End;

Procedure TPTCClear.clear(pixels : Pointer; x, y, width, height, pitch : Integer; Const color : TPTCColor);

Var
  r, g, b, a : LongInt;
  index : LongInt;

Begin
{$IFDEF DEBUG}
  {
  This checking is performed only when DEBUG is defined,
  and can be used to track down errors early caused by passing
  nil pointers to the clear function.

  Even though technically clear should never receive a nil
  pointer, we provide a check here to assist in debugging
  just in case it ever does!
  }
  If pixels = Nil Then
    Raise TPTCError.Create('nil pixels pointer in clear');
{$ELSE}
  { In release build no checking is performed for the sake of efficiency. }
{$ENDIF}

  { check format type }
  If Fformat.direct Then
  Begin
    { check color type }
    If Not color.direct Then
      Raise TPTCError.Create('direct pixel formats can only be cleared with direct color');

    { setup clear color }
    r := Trunc(color.r * 255);
    g := Trunc(color.g * 255);
    b := Trunc(color.b * 255);
    a := Trunc(color.a * 255);

    { clamp red }
    If r > 255 Then
      r := 255
    Else
      If r < 0 Then
        r := 0;

    { clamp green }
    If g > 255 Then
      g := 255
    Else
      If g < 0 Then
        g := 0;

    { clamp blue }
    If b > 255 Then
      b := 255
    Else
      If b < 0 Then
        b := 0;

    { clamp alpha }
    If a > 255 Then
      a := 255
    Else
      If a < 0 Then
        a := 0;

    { perform the clearing }
    Hermes_ClearerClear(Fhandle,pixels,x,y,width,height,pitch,r,g,b,a);
  End
  Else
  Begin
    { check color type }
    If Not color.indexed Then
      Raise TPTCError.Create('indexed pixel formats can only be cleared with indexed color');

    { setup clear index }
    index := color.index;

    { clamp color index }
    If index > 255 Then
      index := 255
    Else
      If index < 0 Then
        index := 0;

    { perform the clearing }
    Hermes_ClearerClear(Fhandle,pixels,x,y,width,height,pitch,0,0,0,index);
  End;
End;
