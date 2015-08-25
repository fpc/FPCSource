{
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Unit to read a CDROM disc TOC and get a list of CD Rom devices

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit cdrom;

{$mode objfpc}
{$h+}


interface

Type
  // Frames are 1/75th of a second.
  // To get the seconds of a track divide the frames by 75.
  // TrackLen: Double; ...
  // TrackLen := Frames / 75.
  TTocEntry = Record
    min, sec, frame : Integer;
  end;
  PTocEntry = ^TTocEntry;

// Returns the High value to use in a loop. Each entry is the position of the end
// of a track. For audio cd's the zero'th entry is not audio data. If an audio cd
// has 10 songs then ReadCDToc will return 10 but there are 11 entries: 0..10.
// You still need to use the zero'th entry to get the first track length:
// Track1Length := TOC[1].frames = TOC[0].frames.
Function ReadCDTOC(Device : String; Var CDTOC : Array of TTocEntry) : Integer;

// Returns the number of devices placed in 'Devices'
Function GetCDRomDevices(Var Devices : Array of string) : Integer;

Implementation

{$ifdef linux}
{$i cdromlin.inc}
{$else}
{$i cdromw32.inc}
{$endif}

end.
