unit gtk2ext;

{$mode objfpc}{$H+}

interface

uses
  glib2, gdk2, gdk2pixbuf, gtk2;
  
const
  { This is equired when people don't have -dev/-devel packages on linux.
    I'm not sure how Darwin is handled tho }
  {$ifdef windows}
  GtkLibNames: array[1..1] of string = (gtklib);
  {$else}
    {$ifdef darwin} // Mac/Darwin
    GtkLibNames: array[1..1] of string = (gtklib); // TODO: I don't know this one!
    {$else} // BSD, Solaris, Linux
    GtkLibNames: array[1..2] of string = (gtklib, gtklib + '.0');
    {$endif}
  {$endif}
  
{$i gtkstatusiconh.inc}
{$i gtkscalebuttonh.inc}
{$i gtkvolumebuttonh.inc}
{$i gtktextmarkh.inc}
{$i gtktextiterh.inc}

implementation

uses
  SysUtils, dynlibs;

var
  gtkhandle : tlibhandle;

{$i gtkstatusicon.inc}
{$i gtkscalebutton.inc}
{$i gtkvolumebutton.inc}
{$i gtktextmark.inc}
{$i gtktextiter.inc}

var
  libIter: Integer;

initialization
  for libIter := High(GtkLibNames) downto Low(GtkLibNames) do begin
    gtkhandle := LoadLibrary(GtkLibNames[libIter]);
    if gtkhandle <> 0 then begin
      Loadgtkstatusicon;
      Loadgtkscalebutton;
      Loadgtkvolumebutton;
      Loadgtktextmark;
      Loadgtkiter;
      // add all specific component load functions here
      Break;
    end;
  end;
finalization
  Freegtkstatusicon;
  Freegtkscalebutton;
  Freegtkvolumebutton;
  Freegtktextmark;
  Freegtkiter;
  // add all specific component free functions here
  if gtkhandle <> 0 then
    FreeLibrary(gtkhandle);

end.

