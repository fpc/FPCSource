{
    $Id$
    Copyright (c) 1998-2000 by Daniel Mantione

    Handles the overlay initialisation for a TP7 compiled version

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit ppovin;

interface

var
  ovrminsize:longint;

procedure InitOverlay;

implementation
uses overlay;


function _heaperror(size:word):integer;far;
type
  heaprecord=record
    next:pointer;
    values:longint;
  end;
var
  l,m:longint;
begin
  l:=ovrgetbuf-ovrminsize;
  if (size>maxavail) and (l>=size) then
   begin
     m:=((longint(size)+$3fff) and $ffffc000);
     {Clear the overlay buffer.}
     ovrclearbuf;
     {Shrink it.}
     ovrheapend:=ovrheapend-m shr 4;
     heaprecord(ptr(ovrheapend,0)^).next:=freelist;
     heaprecord(ptr(ovrheapend,0)^).values:=m shl 12;
     heaporg:=ptr(ovrheapend,0);
     freelist:=heaporg;
     Writeln('Warning: Overlay buffer was shrunk because of memory shortage');
     _heaperror:=2;
   end
  else
   _heaperror:=0;
end;

procedure InitOverlay;
begin
  heaperror:=@_heaperror;
end;


var
  s:string;
begin
  s:=paramstr(0);
  ovrinit(copy(s,1,length(s)-3)+'ovr');
  if ovrresult=ovrok then
   begin
     {May fail if no EMS memory is available. No need for error
      checking, though, as the overlay manager happily runs without
      EMS.}
     ovrinitEMS;
     ovrminsize:=ovrgetbuf;
     ovrsetbuf(ovrminsize+$20000);
   end
  else
  { only for real mode TP : runerror ok here PM }
   runerror($da);
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:29:54  michael
  + Initial import

  Revision 1.6  2000/02/09 13:22:59  peter
    * log truncated

  Revision 1.5  2000/01/07 01:14:30  peter
    * updated copyright to 2000

  Revision 1.4  1999/09/16 11:34:58  pierre
   * typo correction

}


