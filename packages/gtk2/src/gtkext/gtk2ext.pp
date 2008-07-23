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

implementation

uses
  SysUtils, dynlibs;

var
  gtkhandle : tlibhandle;

{$i gtkstatusicon.inc}

var
  libIter: Integer;

initialization
  for libIter := High(GtkLibNames) downto Low(GtkLibNames) do begin
    gtkhandle := LoadLibrary(GtkLibNames[libIter]);
    if gtkhandle <> 0 then begin
      // add all specific component load functions here
      Loadgtkstatusicon;
      Break;
    end;
  end;
finalization
  // add all specific component free functions here
  Freegtkstatusicon;
  if gtkhandle <> 0 then
    FreeLibrary(gtkhandle);

end.

