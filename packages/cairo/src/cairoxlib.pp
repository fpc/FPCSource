unit CairoXlib;
{
    This file is part of the Free Pascal libraries.
    Copyright (c) 2003-2008 by the Free Pascal development team

    Translation of cairo-ft.h 

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Translation of cairo-xlib.h version 1.4
 by Jeffrey Pohlmeyer 
 updated to version 1.4 by Luiz Américo Pereira Câmara 2007

 As per original authors wish, this file is dual licensed LGPL-MPL see the original file
  cairo.pp for the full license.
}

{$mode ObjFpc}

interface

uses
  Cairo, x, xlib, xrender;
  
function  cairo_xlib_surface_create(dpy: PDisplay; drawable: TDrawable; visual: PVisual; width, height: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_for_bitmap(dpy: PDisplay; bitmap: TPixmap; screen: PScreen; width, height: LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_with_xrender_format(dpy: PDisplay; drawable: TDrawable; screen: PScreen; format: PXRenderPictFormat; width, height:LongInt): Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_depth(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_display(surface: Pcairo_surface_t): PDisplay; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_drawable(surface: Pcairo_surface_t): TDrawable; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_height(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_screen(surface: Pcairo_surface_t): PScreen; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_visual(surface: Pcairo_surface_t): PVisual; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_get_width(surface: Pcairo_surface_t): LongInt; cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_size(surface: Pcairo_surface_t; width, height: LongInt); cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_drawable(surface: Pcairo_surface_t; drawable: TDrawable; width, height: LongInt); cdecl; external LIB_CAIRO;

implementation

end.
