{

  Translation of the gtkglarea 4 headers for Free Pascal
  Copyright (C) 2000 Sebastian Guenther

  Copyright notice of gtkglarea:

  * Copyright (C) 1997-1998 Janne Löf <jlof@mail.student.oulu.fi>
  *
  * This library is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Library General Public
  * License as published by the Free Software Foundation; either
  * version 2 of the License, or (at your option) any later version.
  *
  * This library is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Library General Public License for more details.
  *
  * You should have received a copy of the GNU Library General Public
  * License along with this library; if not, write to the Free
  * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit gtkglarea;

{$MODE objfpc}
{$PACKRECORDS C}

interface
{$ifndef os2}

uses
  GDK, GTK, GL;

const
  libgtkgl = 'gtkgl';

// ===================================================================
//   gdkgl
// ===================================================================

{
  These definitions are duplicated from GL/glx.h that comes with Mesa.
  I don't want every program to include GL/glx.h, that might become
  problem if GtkGLArea is ever ported to non X environments like
  (horror!) Windows.
}

// enum _GDK_GL_CONFIGS
  GDK_GL_NONE                           = 0;
  GDK_GL_USE_GL                         = 1;
  GDK_GL_BUFFER_SIZE                    = 2;
  GDK_GL_LEVEL                          = 3;
  GDK_GL_RGBA                           = 4;
  GDK_GL_DOUBLEBUFFER                   = 5;
  GDK_GL_STEREO                         = 6;
  GDK_GL_AUX_BUFFERS                    = 7;
  GDK_GL_RED_SIZE                       = 8;
  GDK_GL_GREEN_SIZE                     = 9;
  GDK_GL_BLUE_SIZE                      = 10;
  GDK_GL_ALPHA_SIZE                     = 11;
  GDK_GL_DEPTH_SIZE                     = 12;
  GDK_GL_STENCIL_SIZE                   = 13;
  GDK_GL_ACCUM_RED_SIZE                 = 14;
  GDK_GL_ACCUM_GREEN_SIZE               = 15;
  GDK_GL_ACCUM_BLUE_SIZE                = 16;
  GDK_GL_ACCUM_ALPHA_SIZE               = 17;

  // GLX_EXT_visual_info extension
  GDK_GL_X_VISUAL_TYPE_EXT              = $22;
  GDK_GL_TRANSPARENT_TYPE_EXT           = $23;
  GDK_GL_TRANSPARENT_INDEX_VALUE_EXT    = $24;
  GDK_GL_TRANSPARENT_RED_VALUE_EXT      = $25;
  GDK_GL_TRANSPARENT_GREEN_VALUE_EXT    = $26;
  GDK_GL_TRANSPARENT_BLUE_VALUE_EXT     = $27;
  GDK_GL_TRANSPARENT_ALPHA_VALUE_EXT    = $28;


type

  TGdkGLContext = record end;
  PGdkGLContext = ^TGdkGLContext;


function  gdk_gl_query: Integer; cdecl; external libgtkgl;
function  gdk_gl_choose_visual(attrList: PInteger): PGdkVisual; cdecl; external libgtkgl;
{$ifndef win32}
function  gdk_gl_get_config(visual: PGdkVisual; attrib: Integer): Integer; cdecl; external libgtkgl;
function  gdk_gl_context_new(visual: PGdkVisual): PGdkGLContext; cdecl; external libgtkgl;
function  gdk_gl_context_share_new(visual: PGdkVisual; sharelist: PGdkGLContext; direct: Integer): PGdkGLContext; cdecl; external libgtkgl;
function  gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext; cdecl; external libgtkgl;
procedure gdk_gl_context_unref(context: PGdkGLContext); cdecl; external libgtkgl;
{$endif}
function  gdk_gl_make_current(drawable: PGdkDrawable; context: PGdkGLContext): Integer; cdecl; external libgtkgl;
procedure gdk_gl_swap_buffers(drawable: PGdkDrawable); cdecl; external libgtkgl;
procedure gdk_gl_wait_gdk; cdecl; external libgtkgl;
procedure gdk_gl_wait_gl; cdecl; external libgtkgl;


// glpixmap stuff

type

  TGdkGLPixmap = record end;
  PGdkGLPixmap = ^TGdkGLPixmap;

{$ifndef win32}
function  gdk_gl_pixmap_new(visual: PGdkVisual; pixmap: PGdkPixmap): PGdkGLPixmap; cdecl; external libgtkgl;
function  gdk_gl_pixmap_ref(glpixmap: PGdkGLPixmap): PGdkGLPixmap; cdecl; external libgtkgl;
procedure gdk_gl_pixmap_unref(glpixmap: PGdkGLPixmap); cdecl; external libgtkgl;
function  gdk_gl_pixmap_make_current(glpixmap: PGdkGLPixmap; context: PGdkGLContext): Integer; cdecl; external libgtkgl;
{$endif}


// fonts
{$ifndef win32}
procedure gdk_gl_use_gdk_font(font: PGdkFont; first, count, list_base: Integer); cdecl; external libgtkgl;
{$endif}


// ===================================================================
//   gtkglarea
// ===================================================================

type

  PGtkGLArea = ^TGtkGLArea;

  TGtkGLArea = record
    darea: TGtkDrawingArea;
    glcontext: PGdkGLContext;
  end;


  PGtkGLAreaClass = ^TGtkGLAreaClass;
  TGtkGLAreaClass = record
    parent_class: TGtkDrawingAreaClass;
  end;


function  GTK_TYPE_GL_AREA: TGtkType; cdecl; external libgtkgl name 'gtk_gl_area_get_type';
function  GTK_IS_GL_AREA(obj: Pointer): Boolean;
function  GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;

function  gtk_gl_area_get_type: TGtkType; cdecl; external libgtkgl;
function  gtk_gl_area_new(attrList: PInteger): PGtkWidget; cdecl; external libgtkgl;
function  gtk_gl_area_share_new(attrList: PInteger; share: PGtkGLArea): PGtkWidget; cdecl; external libgtkgl;
function  gtk_gl_area_new_vargs(share: PGtkGLArea; args: array of const): PGtkWidget; cdecl; external libgtkgl;

function  gtk_gl_area_make_current(glarea: PGtkGLArea): Integer; cdecl; external libgtkgl;
procedure gtk_gl_area_swapbuffers(glarea: PGtkGLArea); cdecl; external libgtkgl;

{$endif os2}
implementation
{$ifndef os2}


function GTK_IS_GL_AREA(obj: Pointer): Boolean;
begin
  Result := Assigned(obj) and GTK_IS_GL_AREA_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;
begin
  Result := Assigned(klass) and (PGtkTypeClass(klass)^.thetype = GTK_TYPE_GL_AREA);
end;

{$endif os2}
end.
