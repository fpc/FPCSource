{ Original file is /usr/include/gtkglext-1.0/gdk/gdkgltokens.h }

{$ifdef read_interface_types}

  TGdkGLConfigAttrib = LongInt;
  TGdkGLConfigCaveat = LongInt;
  TGdkGLVisualType = LongInt;
  TGdkGLTransparentType = LongInt;
  TGdkGLDrawableTypeMask = LongInt;
  TGdkGLRenderTypeMask = LongInt;
  TGdkGLBufferMask = LongInt;
  TGdkGLConfigError = LongInt;
  TGdkGLRenderType = LongInt;
  TGdkGLDrawableAttrib = LongInt;
  TGdkGLPbufferAttrib = LongInt;
  TGdkGLEventMask = LongInt;
  TGdkGLEventType = LongInt;
  TGdkGLDrawableType = LongInt;

{$endif read_interface_types}

{$ifdef read_interface_rest}

{*
 * Success return value
 *}
const
  GDK_GL_SUCCESS =  0;  {* same as 'Success' of X11 *}

{*
 * Attribute list terminator
 *}
  GDK_GL_ATTRIB_LIST_NONE = 0;  {* same as 'None' of X11 *}

{*
 * This source is based on the OpenGL(R) Sample Implementation by SGI.
 *}

{*
** License Applicability. Except to the extent portions of this file are
** made subject to an alternative license as permitted in the SGI Free
** Software License B, Version 1.1 (the "License"), the contents of this
** file are subject only to the provisions of the License. You may not use
** this file except in compliance with the License. You may obtain a copy
** of the License at Silicon Graphics, Inc., attn: Legal Services, 1600
** Amphitheatre Parkway, Mountain View, CA 94043-1351, or at:
**
** http://oss.sgi.com/projects/FreeB
**
** Note that, as provided in the License, the Software is distributed on an
** "AS IS" basis, with ALL EXPRESS AND IMPLIED WARRANTIES AND CONDITIONS
** DISCLAIMED, INCLUDING, WITHOUT LIMITATION, ANY IMPLIED WARRANTIES AND
** CONDITIONS OF MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A
** PARTICULAR PURPOSE, AND NON-INFRINGEMENT.
**
** Original Code. The Original Code is: OpenGL Sample Implementation,
** Version 1.2.1, released January 26, 2000, developed by Silicon Graphics,
** Inc. The Original Code is Copyright (c) 1991-2000 Silicon Graphics, Inc.
** Copyright in any portions created by third parties is as indicated
** elsewhere herein. All Rights Reserved.
**
** Additional Notice Provisions: The application programming interfaces
** established by SGI in conjunction with the Original Code are The
** OpenGL(R) Graphics System: A Specification (Version 1.2.1), released
** April 1, 1999; The OpenGL(R) Graphics System Utility Library (Version
** 1.3), released November 4, 1998; and OpenGL(R) Graphics with the X
** Window System(R) (Version 1.3), released October 19, 1998. This software
** was created using the OpenGL(R) version 1.2.1 Sample Implementation
** published by SGI, but has not been independently verified as being
** compliant with the OpenGL(R) version 1.2.1 Specification.
*}

{*
 * Visual Config Attributes.
 *}

  GDK_GL_USE_GL                     = 1;  {* support GLX rendering *}
  GDK_GL_BUFFER_SIZE                = 2;  {* depth of the color buffer *}
  GDK_GL_LEVEL                      = 3;  {* level in plane stacking *}
  GDK_GL_RGBA                       = 4;  {* true if RGBA mode *}
  GDK_GL_DOUBLEBUFFER               = 5;  {* double buffering supported *}
  GDK_GL_STEREO                     = 6;  {* stereo buffering supported *}
  GDK_GL_AUX_BUFFERS                = 7;  {* number of aux buffers *}
  GDK_GL_RED_SIZE                   = 8;  {* number of red component bits *}
  GDK_GL_GREEN_SIZE                 = 9;  {* number of green component bits *}
  GDK_GL_BLUE_SIZE                  = 10; {* number of blue component bits *}
  GDK_GL_ALPHA_SIZE                 = 11; {* number of alpha component bits *}
  GDK_GL_DEPTH_SIZE                 = 12; {* number of depth bits *}
  GDK_GL_STENCIL_SIZE               = 13; {* number of stencil bits *}
  GDK_GL_ACCUM_RED_SIZE             = 14; {* number of red accum bits *}
  GDK_GL_ACCUM_GREEN_SIZE           = 15; {* number of green accum bits *}
  GDK_GL_ACCUM_BLUE_SIZE            = 16; {* number of blue accum bits *}
  GDK_GL_ACCUM_ALPHA_SIZE           = 17; {* number of alpha accum bits *}

  {*
   * FBConfig-specific attributes.
   * [ GLX 1.3 and later ]
   *}
  GDK_GL_CONFIG_CAVEAT              = $20;
  GDK_GL_X_VISUAL_TYPE              = $22;
  GDK_GL_TRANSPARENT_TYPE           = $23;
  GDK_GL_TRANSPARENT_INDEX_VALUE    = $24;
  GDK_GL_TRANSPARENT_RED_VALUE      = $25;
  GDK_GL_TRANSPARENT_GREEN_VALUE    = $26;
  GDK_GL_TRANSPARENT_BLUE_VALUE     = $27;
  GDK_GL_TRANSPARENT_ALPHA_VALUE    = $28;
  GDK_GL_DRAWABLE_TYPE              = $8010;
  GDK_GL_RENDER_TYPE                = $8011;
  GDK_GL_X_RENDERABLE               = $8012;
  GDK_GL_FBCONFIG_ID                = $8013;
  GDK_GL_MAX_PBUFFER_WIDTH          = $8016;
  GDK_GL_MAX_PBUFFER_HEIGHT         = $8017;
  GDK_GL_MAX_PBUFFER_PIXELS         = $8018;
  GDK_GL_VISUAL_ID                  = $800B;
  GDK_GL_SCREEN                     = $800C;

  {*
   * Multisampling configuration attributes.
   * [ GLX 1.4 and later ]
   *}
  GDK_GL_SAMPLE_BUFFERS             = 100000;
  GDK_GL_SAMPLES                    = 100001;
  { Those were values for TGdkGLConfigAttrib }

{*
 * Generic "don't care" value.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_DONT_CARE = $FFFFFFFF;

{*
 * "none" value.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_NONE = $8000;

{*
 * GLX_CONFIG_CAVEAT attribute values.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_CONFIG_CAVEAT_DONT_CARE    = $FFFFFFFF; {* GDK_GL_DONT_CARE *}
  GDK_GL_CONFIG_CAVEAT_NONE         = $8000;     {* GDK_GL_NONE *}
  GDK_GL_SLOW_CONFIG                = $8001;
  GDK_GL_NON_CONFORMANT_CONFIG      = $800D;
  { Those were values for TGdkGLConfigCaveat }

{*
 * GLX_X_VISUAL_TYPE attribute values.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_VISUAL_TYPE_DONT_CARE      = $FFFFFFFF; {* GDK_GL_DONT_CARE *}
  GDK_GL_TRUE_COLOR                 = $8002;
  GDK_GL_DIRECT_COLOR               = $8003;
  GDK_GL_PSEUDO_COLOR               = $8004;
  GDK_GL_STATIC_COLOR               = $8005;
  GDK_GL_GRAY_SCALE                 = $8006;
  GDK_GL_STATIC_GRAY                = $8007;
  { Those were values for TGdkGLVisualType }

{*
 * GLX_TRANSPARENT_TYPE attribute values.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_TRANSPARENT_NONE           = $8000; {* GDK_GL_NONE *}
  GDK_GL_TRANSPARENT_RGB            = $8008;
  GDK_GL_TRANSPARENT_INDEX          = $8009;
  { Those were values for TGdkGLTransparentType }

{*
 * GLX_DRAWABLE_TYPE bits.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_WINDOW_BIT                 = 1 shl 0; {* $00000001 *}
  GDK_GL_PIXMAP_BIT                 = 1 shl 1; {* $00000002 *}
  GDK_GL_PBUFFER_BIT                = 1 shl 2; {* $00000004 *}
  { Those were values for TGdkGLDrawableTypeMask }

{*
 * GLX_RENDER_TYPE bits.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_RGBA_BIT                   = 1 shl 0; {* $00000001 *}
  GDK_GL_COLOR_INDEX_BIT            = 1 shl 1; {* $00000002 *}
  { Those were values for TGdkGLRenderTypeMask }

{*
 * Buffer mask bits.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_FRONT_LEFT_BUFFER_BIT      = 1 shl 0; {* $00000001 *}
  GDK_GL_FRONT_RIGHT_BUFFER_BIT     = 1 shl 1; {* $00000002 *}
  GDK_GL_BACK_LEFT_BUFFER_BIT       = 1 shl 2; {* $00000004 *}
  GDK_GL_BACK_RIGHT_BUFFER_BIT      = 1 shl 3; {* $00000008 *}
  GDK_GL_AUX_BUFFERS_BIT            = 1 shl 4; {* $00000010 *}
  GDK_GL_DEPTH_BUFFER_BIT           = 1 shl 5; {* $00000020 *}
  GDK_GL_STENCIL_BUFFER_BIT         = 1 shl 6; {* $00000040 *}
  GDK_GL_ACCUM_BUFFER_BIT           = 1 shl 7; {* $00000080 *}
  { Those were values for TGdkGLBufferMask }

{*
 * Error return values from glXGetConfig.  Success is indicated by
 * a value of 0.
 *}
  GDK_GL_BAD_SCREEN                 = 1; {* screen # is bad *}
  GDK_GL_BAD_ATTRIBUTE              = 2; {* attribute to get is bad *}
  GDK_GL_NO_EXTENSION               = 3; {* no glx extension on server *}
  GDK_GL_BAD_VISUAL                 = 4; {* visual # not known by GLX *}
  GDK_GL_BAD_CONTEXT                = 5; {* returned only by import_context EXT? *}
  GDK_GL_BAD_VALUE                  = 6; {* returned only by glXSwapIntervalSGI? *}
  GDK_GL_BAD_ENUM                   = 7; {* unused? *}
  { Those were values for TGdkGLConfigError }

{*
 * glXCreateNewContext render_type attribute values.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_RGBA_TYPE                  = $8014;
  GDK_GL_COLOR_INDEX_TYPE           = $8015;
  { Those were values for TGdkGLRenderType }

{*
 * glXQueryDrawable attributes.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_PRESERVED_CONTENTS         = $801B;
  GDK_GL_LARGEST_PBUFFER            = $801C;
  GDK_GL_WIDTH                      = $801D;
  GDK_GL_HEIGHT                     = $801E;
  GDK_GL_EVENT_MASK                 = $801F;
  { Those were values for TGdkGLDrawableAttrib }

{*
 * glXCreatePbuffer attributes.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_PBUFFER_PRESERVED_CONTENTS = $801B; {* GDK_GL_PRESERVED_CONTENTS *}
  GDK_GL_PBUFFER_LARGEST_PBUFFER    = $801C; {* GDK_GL_LARGEST_PBUFFER *}
  GDK_GL_PBUFFER_HEIGHT             = $8040;
  GDK_GL_PBUFFER_WIDTH              = $8041;
  { Those were values for TGdkGLPbufferAttrib }

{*
 * glXSelectEvent event mask bits.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_PBUFFER_CLOBBER_MASK       = 1 shl 27;  {* $08000000 *}
  { Those were values for TGdkGLEventMask }

{*
 * GLXPbufferClobberEvent event_type values.
 * [ GLX 1.3 and later ]
 *}
  GDK_GL_DAMAGED                    = $8020;
  GDK_GL_SAVED                      = $8021;
  { Those were values for TGdkGLEventType }

{*
 * GLXPbufferClobberEvent draw_type values.
 * [ GLX 1.3 and later ]
 *}
  { Pascal translation comments:
    Name GDK_GL_WINDOW conflicts (even in case-sensitve C,
    but there macros conflicts are not errors)
    with function GDK_GL_WINDOW in gdkglwindow.inc.
    Renamed to GDK_GL_WINDOW_VALUE. }
  GDK_GL_WINDOW_VALUE               = $8022;
  GDK_GL_PBUFFER                    = $8023;
  { Those were values for TGdkGLDrawableType }

{$endif read_interface_rest}
