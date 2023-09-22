{$IFNDEF FPC_DOTTEDUNITS}
unit gtk2ext;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.Glib2, Api.Gdk2, Api.Gtk2.Gdk2pixbuf, Api.Gtk2.Gtk2;
{$ELSE FPC_DOTTEDUNITS}
uses
  glib2, gdk2, gdk2pixbuf, gtk2;
{$ENDIF FPC_DOTTEDUNITS}
  
const
  { This is equired when people don't have -dev/-devel packages on linux.
    I'm not sure how Darwin is handled tho }
  {$ifdef windows}
  GtkLibNames: array[1..1] of ansistring = (gtklib);
  {$else}
    {$ifdef darwin} // Mac/Darwin
    GtkLibNames: array[1..1] of ansistring = (gtklib); // TODO: I don't know this one!
    {$else} // BSD, Solaris, Linux
    GtkLibNames: array[1..2] of ansistring = (gtklib, gtklib + '.0');
    {$endif}
  {$endif}
  
{$i gtkstatusiconh.inc}
{$i gtkscalebuttonh.inc}
{$i gtkvolumebuttonh.inc}
{$i gtktextmarkh.inc}
{$i gtktextiterh.inc}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.DynLibs;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, dynlibs;
{$ENDIF FPC_DOTTEDUNITS}

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

