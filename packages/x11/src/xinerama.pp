{$IFNDEF FPC_DOTTEDUNITS}
unit xinerama;
{$ENDIF FPC_DOTTEDUNITS}
interface
{$IFDEF FPC_DOTTEDUNITS}
uses
 System.CTypes, Api.X11.Xlib;
{$ELSE FPC_DOTTEDUNITS}
uses
 ctypes, xlib;
{$ENDIF FPC_DOTTEDUNITS}
{ Converted from X11/Xinerama.h }

{$PACKRECORDS C}


type

   PXineramaScreenInfo = ^TXineramaScreenInfo;
   TXineramaScreenInfo = record
        screen_number : cint;
        x_org         : cshort;
        y_org         : cshort;
        width         : cshort;
        height        : cshort;
     end;

function XineramaQueryExtension(dpy:PDisplay; event_base:Pcint; error_base:Pcint):TBoolResult;cdecl;external 'Xinerama';
function XineramaQueryVersion(dpy:PDisplay; major:Pcint; minor:Pcint):TStatus;cdecl;external 'Xinerama';
function XineramaIsActive(dpy:PDisplay):TBoolResult;cdecl;external 'Xinerama';
function XineramaQueryScreens(dpy:PDisplay; number:Pcint):PXineramaScreenInfo;cdecl;external 'Xinerama';

implementation

end.
