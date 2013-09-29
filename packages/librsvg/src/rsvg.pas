{
  Translation of the librsvg headers for FreePascal
  Copyright (C) 2009 by Ivo Steinmann
}

(* 
   rsvg.h: SAX-based renderer for SVG files into a GdkPixbuf.
 
   Copyright (C) 2000 Eazel, Inc.
  
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
  
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.
  
   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
  
   Author: Raph Levien <raph@artofcode.com>
*)

unit rsvg;

{$mode objfpc}
{$MINENUMSIZE 4}

interface

uses
  ctypes,
  glib2,
  gdk2pixbuf;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}
{.$DEFINE DYNLINK}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  proj4lib = 'librsvg-2.dll';
{$ELSEIF Defined(UNIX)}
  proj4lib = 'librsvg-2.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB rsvg-2}
{$ENDIF}


(***********************************************************************)
(* Header : rsvg.h                                                     *)
(***********************************************************************)

//#include <gdk-pixbuf/gdk-pixbuf.h>

{G_BEGIN_DECLS

#define RSVG_TYPE_HANDLE                  (rsvg_handle_get_type ())
#define RSVG_HANDLE(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), RSVG_TYPE_HANDLE, RsvgHandle))
#define RSVG_HANDLE_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), RSVG_TYPE_HANDLE, RsvgHandleClass))
#define RSVG_IS_HANDLE(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), RSVG_TYPE_HANDLE))
#define RSVG_IS_HANDLE_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), RSVG_TYPE_HANDLE))
#define RSVG_HANDLE_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), RSVG_TYPE_HANDLE, RsvgHandleClass))

GType rsvg_handle_get_type (void);}

(**
 * An enumeration representing possible error domains
 *)
type
  RsvgError = (
    RSVG_ERROR_FAILED
  );

(**
 * 
 *)
{#define RSVG_ERROR (rsvg_error_quark ())
GQuark rsvg_error_quark (void) G_GNUC_CONST;}

(**
 * The RsvgHandle is an object representing the parsed form of a SVG
 *)
type
  PRsvgHandle = ^RsvgHandle;
  RsvgHandle = record
    parent: TGObjectClass;
    _abi_padding: array[0..14] of gpointer;
  end;

  PRsvgHandlePrivate = ^RsvgHandlePrivate;
  RsvgHandlePrivate = record
    parent: TGObject;
    priv: PRsvgHandlePrivate;
    _abi_padding: array[0..14] of gpointer;
  end;


(* RsvgDimensionData
 *)
  PRsvgDimensionData = ^RsvgDimensionData;
  RsvgDimensionData = record
        (**
   * SVG's width, in pixels
   *)
    width: cint;

        (**
   * SVG's height, in pixels
   *)
    height: cint;

        (**
   * em
   *)
    em: gdouble;

        (**
   * ex
   *)
    ex: gdouble;
  end;

procedure rsvg_init(); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
procedure rsvg_term(); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

procedure rsvg_set_default_dpi(dpi: double); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
procedure rsvg_set_default_dpi_x_y(dpi_x: double; dpi_y: double); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

procedure rsvg_handle_set_dpi(handle: PRsvgHandle; dpi: double); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
procedure rsvg_handle_set_dpi_x_y(handle: PRsvgHandle; dpi_x: double; dpi_y: double); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

function rsvg_handle_new(): PRsvgHandle; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_write(handle: PRsvgHandle; buf: pguchar; count: gsize; var error: PGError): gboolean; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_close(handle: PRsvgHandle; var error: PGError): gboolean; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_get_pixbuf(handle: PRsvgHandle): PGdkPixbuf; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_get_pixbuf_sub(handle: PRsvgHandle; id: pchar): PGdkPixbuf; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

function rsvg_handle_get_base_uri(handle: PRsvgHandle): pchar; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
procedure rsvg_handle_set_base_uri(handle: PRsvgHandle; base_uri: pchar); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

procedure rsvg_handle_get_dimensions(handle: PRsvgHandle; dimension_data: PRsvgDimensionData); cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

(* Accessibility API *)

function rsvg_handle_get_title(handle: PRsvgHandle): pchar; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_get_desc(handle: PRsvgHandle): pchar; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_get_metadata(handle: PRsvgHandle): pchar; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

function rsvg_handle_new_from_data(data: Pguint8; data_len: gsize; var error: PGError): PRsvgHandle; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};
function rsvg_handle_new_from_file(file_name: pgchar; var error: PGError): PRsvgHandle; cdecl; external {$IFDEF DYNLIB}proj4lib{$ENDIF};

implementation

end.
