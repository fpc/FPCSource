unit xinerama;
interface
uses
{$IFDEF UNIX}
 baseunix,
{$ENDIF UNIX}
 xlib;
{ Converted from X11/Xinerama.h }

{$PACKRECORDS C}


type
{$IFNDEF UNIX}
   cint = longint;
   pcint = plongint;
   cshort = smallint;
{$ENDIF UNIX}

   PXineramaScreenInfo = ^TXineramaScreenInfo;
   TXineramaScreenInfo = record
        screen_number : cint;
        x_org 	      : cshort;
        y_org 	      : cshort;
        width 	      : cshort;
        height 	      : cshort;
     end;

function XineramaQueryExtension(dpy:PDisplay; event_base:Pcint; error_base:Pcint):TBool;cdecl;external 'Xinerama';
function XineramaQueryVersion(dpy:PDisplay; major:Pcint; minor:Pcint):TStatus;cdecl;external 'Xinerama';
function XineramaIsActive(dpy:PDisplay):TBool;cdecl;external 'Xinerama';
function XineramaQueryScreens(dpy:PDisplay; number:Pcint):PXineramaScreenInfo;cdecl;external 'Xinerama';

implementation

end.
