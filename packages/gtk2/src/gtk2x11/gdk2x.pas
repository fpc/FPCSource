{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

}
{$IFNDEF FPC_DOTTEDUNITS}
unit gdk2x;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF FPC}
  {$mode objfpc}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE KYLIX}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, UnixApi.Types, UnixApi.Unix, UnixApi.Base, Api.Glib2, Api.Gdk2, Api.X11.Xlib, Api.X11.X, Api.X11.Xrender;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, UnixType, Unix, BaseUnix, glib2, gdk2, XLib, X, XRender;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef FREEBSD}
  {$linklib pthread}
{$endif}

{$IFNDEF KYLIX}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN 4}
  {$WEAKPACKAGEUNIT}
  {$WARNINGS OFF}
{$ENDIF}

{$DEFINE read_forward_definitions}
type
{$I include/gdk2x11includes.inc}
{$UNDEF read_forward_definitions}

{$DEFINE read_interface_rest}
{$I include/gdk2x11includes.inc}
{$UNDEF read_interface_rest}

implementation

{*****************************************************************************
 * macro functions
 *
 *****************************************************************************}

// call implementation parts of header files
{$DEFINE read_implementation}
{$I include/gdk2x11includes.inc}
{$UNDEF read_implementation}

end.

