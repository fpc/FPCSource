{

  Adaption of the delphi3d.net OpenGL units to FreePascal
  Sebastian Guenther (sg@freepascal.org) in 2002
  These units are free to use
}

(*++ BUILD Version: 0004    // Increment this if a change has global effects

Copyright (c) 1985-96, Microsoft Corporation

Module Name:

    gl.h

Abstract:

    Procedure declarations, constant definitions and macros for the OpenGL
    component.

--*)

(*
** Copyright 1996 Silicon Graphics, Inc.
** All Rights Reserved.
**
** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
** the contents of this file may not be disclosed to third parties, copied or
** duplicated in any form, in whole or in part, without the prior written
** permission of Silicon Graphics, Inc.
**
** RESTRICTED RIGHTS LEGEND:
** Use, duplication or disclosure by the Government is subject to restrictions
** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
** rights reserved under the Copyright Laws of the United States.
*)

{******************************************************************************}
{ Converted to Delphi by Tom Nuydens (tom@delphi3d.net)                        }
{ For the latest updates, visit Delphi3D: http://www.delphi3d.net              }
{******************************************************************************}

{$MACRO ON}
{$MODE Delphi}
{$IFDEF Win32}
  {$DEFINE extdecl := stdcall}
{$ELSE}
  {$DEFINE extdecl := cdecl}
  {$LINKLIB c}
{$ENDIF}

unit GL;

interface

uses
  SysUtils,
  {$IFDEF Win32}
  Windows
  {$ELSE Win32}
  DLLFuncs
  {$ENDIF Win32};

var
  LibGL: THandle;

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;

{******************************************************************************}

const
  // Version
  GL_VERSION_1_1                    = 1;

  // AccumOp
  GL_ACCUM                          = $0100;
  GL_LOAD                           = $0101;
  GL_RETURN                         = $0102;
  GL_MULT                           = $0103;
  GL_ADD                            = $0104;

  // AlphaFunction
  GL_NEVER                          = $0200;
  GL_LESS                           = $0201;
  GL_EQUAL                          = $0202;
  GL_LEQUAL                         = $0203;
  GL_GREATER                        = $0204;
  GL_NOTEQUAL                       = $0205;
  GL_GEQUAL                         = $0206;
  GL_ALWAYS                         = $0207;

  // AttribMask
  GL_CURRENT_BIT                    = $00000001;
  GL_POINT_BIT                      = $00000002;
  GL_LINE_BIT                       = $00000004;
  GL_POLYGON_BIT                    = $00000008;
  GL_POLYGON_STIPPLE_BIT            = $00000010;
  GL_PIXEL_MODE_BIT                 = $00000020;
  GL_LIGHTING_BIT                   = $00000040;
  GL_FOG_BIT                        = $00000080;
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_ACCUM_BUFFER_BIT               = $00000200;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_VIEWPORT_BIT                   = $00000800;
  GL_TRANSFORM_BIT                  = $00001000;
  GL_ENABLE_BIT                     = $00002000;
  GL_COLOR_BUFFER_BIT               = $00004000;
  GL_HINT_BIT                       = $00008000;
  GL_EVAL_BIT                       = $00010000;
  GL_LIST_BIT                       = $00020000;
  GL_TEXTURE_BIT                    = $00040000;
  GL_SCISSOR_BIT                    = $00080000;
  GL_ALL_ATTRIB_BITS                = $000FFFFF;

  // BeginMode
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_LINE_LOOP                      = $0002;
  GL_LINE_STRIP                     = $0003;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007;
  GL_QUAD_STRIP                     = $0008;
  GL_POLYGON                        = $0009;

  // BlendingFactorDest
  GL_ZERO                           = 0;
  GL_ONE                            = 1;
  GL_SRC_COLOR                      = $0300;
  GL_ONE_MINUS_SRC_COLOR            = $0301;
  GL_SRC_ALPHA                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA            = $0303;
  GL_DST_ALPHA                      = $0304;
  GL_ONE_MINUS_DST_ALPHA            = $0305;

  // BlendingFactorSrc
  //      GL_ZERO
  //      GL_ONE
  GL_DST_COLOR                      = $0306;
  GL_ONE_MINUS_DST_COLOR            = $0307;
  GL_SRC_ALPHA_SATURATE             = $0308;
  //      GL_SRC_ALPHA
  //      GL_ONE_MINUS_SRC_ALPHA
  //      GL_DST_ALPHA
  //      GL_ONE_MINUS_DST_ALPHA

  // Boolean
  GL_TRUE                           = 1;
  GL_FALSE                          = 0;

  // ClearBufferMask
  //      GL_COLOR_BUFFER_BIT
  //      GL_ACCUM_BUFFER_BIT
  //      GL_STENCIL_BUFFER_BIT
  //      GL_DEPTH_BUFFER_BIT

  // ClientArrayType
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY

  // ClipPlaneName
  GL_CLIP_PLANE0                    = $3000;
  GL_CLIP_PLANE1                    = $3001;
  GL_CLIP_PLANE2                    = $3002;
  GL_CLIP_PLANE3                    = $3003;
  GL_CLIP_PLANE4                    = $3004;
  GL_CLIP_PLANE5                    = $3005;

  // ColorMaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // ColorMaterialParameter
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR
  //      GL_EMISSION
  //      GL_AMBIENT_AND_DIFFUSE

  // ColorPointerType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // CullFaceMode
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // DataType
  GL_BYTE                           = $1400;
  GL_UNSIGNED_BYTE                  = $1401;
  GL_SHORT                          = $1402;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_INT                            = $1404;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_2_BYTES                        = $1407;
  GL_3_BYTES                        = $1408;
  GL_4_BYTES                        = $1409;
  GL_DOUBLE                         = $140A;

  // DepthFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // DrawBufferMode
  GL_NONE                           = 0;
  GL_FRONT_LEFT                     = $0400;
  GL_FRONT_RIGHT                    = $0401;
  GL_BACK_LEFT                      = $0402;
  GL_BACK_RIGHT                     = $0403;
  GL_FRONT                          = $0404;
  GL_BACK                           = $0405;
  GL_LEFT                           = $0406;
  GL_RIGHT                          = $0407;
  GL_FRONT_AND_BACK                 = $0408;
  GL_AUX0                           = $0409;
  GL_AUX1                           = $040A;
  GL_AUX2                           = $040B;
  GL_AUX3                           = $040C;

  // Enable
  //      GL_FOG
  //      GL_LIGHTING
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_LINE_STIPPLE
  //      GL_POLYGON_STIPPLE
  //      GL_CULL_FACE
  //      GL_ALPHA_TEST
  //      GL_BLEND
  //      GL_INDEX_LOGIC_OP
  //      GL_COLOR_LOGIC_OP
  //      GL_DITHER
  //      GL_STENCIL_TEST
  //      GL_DEPTH_TEST
  //      GL_CLIP_PLANE0
  //      GL_CLIP_PLANE1
  //      GL_CLIP_PLANE2
  //      GL_CLIP_PLANE3
  //      GL_CLIP_PLANE4
  //      GL_CLIP_PLANE5
  //      GL_LIGHT0
  //      GL_LIGHT1
  //      GL_LIGHT2
  //      GL_LIGHT3
  //      GL_LIGHT4
  //      GL_LIGHT5
  //      GL_LIGHT6
  //      GL_LIGHT7
  //      GL_TEXTURE_GEN_S
  //      GL_TEXTURE_GEN_T
  //      GL_TEXTURE_GEN_R
  //      GL_TEXTURE_GEN_Q
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_POINT_SMOOTH
  //      GL_LINE_SMOOTH
  //      GL_POLYGON_SMOOTH
  //      GL_SCISSOR_TEST
  //      GL_COLOR_MATERIAL
  //      GL_NORMALIZE
  //      GL_AUTO_NORMAL
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_POLYGON_OFFSET_POINT
  //      GL_POLYGON_OFFSET_LINE
  //      GL_POLYGON_OFFSET_FILL

  // ErrorCode
  GL_NO_ERROR                       = 0;
  GL_INVALID_ENUM                   = $0500;
  GL_INVALID_VALUE                  = $0501;
  GL_INVALID_OPERATION              = $0502;
  GL_STACK_OVERFLOW                 = $0503;
  GL_STACK_UNDERFLOW                = $0504;
  GL_OUT_OF_MEMORY                  = $0505;

  // FeedBackMode
  GL_2D                             = $0600;
  GL_3D                             = $0601;
  GL_3D_COLOR                       = $0602;
  GL_3D_COLOR_TEXTURE               = $0603;
  GL_4D_COLOR_TEXTURE               = $0604;

  // FeedBackToken
  GL_PASS_THROUGH_TOKEN             = $0700;
  GL_POINT_TOKEN                    = $0701;
  GL_LINE_TOKEN                     = $0702;
  GL_POLYGON_TOKEN                  = $0703;
  GL_BITMAP_TOKEN                   = $0704;
  GL_DRAW_PIXEL_TOKEN               = $0705;
  GL_COPY_PIXEL_TOKEN               = $0706;
  GL_LINE_RESET_TOKEN               = $0707;

  // FogMode
  //      GL_LINEAR
  GL_EXP                            = $0800;
  GL_EXP2                           = $0801;

  // FogParameter
  //      GL_FOG_COLOR
  //      GL_FOG_DENSITY
  //      GL_FOG_END
  //      GL_FOG_INDEX
  //      GL_FOG_MODE
  //      GL_FOG_START

  // FrontFaceDirection
  GL_CW                             = $0900;
  GL_CCW                            = $0901;

  // GetMapTarget
  GL_COEFF                          = $0A00;
  GL_ORDER                          = $0A01;
  GL_DOMAIN                         = $0A02;

  // GetPixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // GetPointerTarget
  //      GL_VERTEX_ARRAY_POINTER
  //      GL_NORMAL_ARRAY_POINTER
  //      GL_COLOR_ARRAY_POINTER
  //      GL_INDEX_ARRAY_POINTER
  //      GL_TEXTURE_COORD_ARRAY_POINTER
  //      GL_EDGE_FLAG_ARRAY_POINTER

  // GetTarget
  GL_CURRENT_COLOR                  = $0B00;
  GL_CURRENT_INDEX                  = $0B01;
  GL_CURRENT_NORMAL                 = $0B02;
  GL_CURRENT_TEXTURE_COORDS         = $0B03;
  GL_CURRENT_RASTER_COLOR           = $0B04;
  GL_CURRENT_RASTER_INDEX           = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS  = $0B06;
  GL_CURRENT_RASTER_POSITION        = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID  = $0B08;
  GL_CURRENT_RASTER_DISTANCE        = $0B09;
  GL_POINT_SMOOTH                   = $0B10;
  GL_POINT_SIZE                     = $0B11;
  GL_POINT_SIZE_RANGE               = $0B12;
  GL_POINT_SIZE_GRANULARITY         = $0B13;
  GL_LINE_SMOOTH                    = $0B20;
  GL_LINE_WIDTH                     = $0B21;
  GL_LINE_WIDTH_RANGE               = $0B22;
  GL_LINE_WIDTH_GRANULARITY         = $0B23;
  GL_LINE_STIPPLE                   = $0B24;
  GL_LINE_STIPPLE_PATTERN           = $0B25;
  GL_LINE_STIPPLE_REPEAT            = $0B26;
  GL_LIST_MODE                      = $0B30;
  GL_MAX_LIST_NESTING               = $0B31;
  GL_LIST_BASE                      = $0B32;
  GL_LIST_INDEX                     = $0B33;
  GL_POLYGON_MODE                   = $0B40;
  GL_POLYGON_SMOOTH                 = $0B41;
  GL_POLYGON_STIPPLE                = $0B42;
  GL_EDGE_FLAG                      = $0B43;
  GL_CULL_FACE                      = $0B44;
  GL_CULL_FACE_MODE                 = $0B45;
  GL_FRONT_FACE                     = $0B46;
  GL_LIGHTING                       = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER       = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE           = $0B52;
  GL_LIGHT_MODEL_AMBIENT            = $0B53;
  GL_SHADE_MODEL                    = $0B54;
  GL_COLOR_MATERIAL_FACE            = $0B55;
  GL_COLOR_MATERIAL_PARAMETER       = $0B56;
  GL_COLOR_MATERIAL                 = $0B57;
  GL_FOG                            = $0B60;
  GL_FOG_INDEX                      = $0B61;
  GL_FOG_DENSITY                    = $0B62;
  GL_FOG_START                      = $0B63;
  GL_FOG_END                        = $0B64;
  GL_FOG_MODE                       = $0B65;
  GL_FOG_COLOR                      = $0B66;
  GL_DEPTH_RANGE                    = $0B70;
  GL_DEPTH_TEST                     = $0B71;
  GL_DEPTH_WRITEMASK                = $0B72;
  GL_DEPTH_CLEAR_VALUE              = $0B73;
  GL_DEPTH_FUNC                     = $0B74;
  GL_ACCUM_CLEAR_VALUE              = $0B80;
  GL_STENCIL_TEST                   = $0B90;
  GL_STENCIL_CLEAR_VALUE            = $0B91;
  GL_STENCIL_FUNC                   = $0B92;
  GL_STENCIL_VALUE_MASK             = $0B93;
  GL_STENCIL_FAIL                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS        = $0B96;
  GL_STENCIL_REF                    = $0B97;
  GL_STENCIL_WRITEMASK              = $0B98;
  GL_MATRIX_MODE                    = $0BA0;
  GL_NORMALIZE                      = $0BA1;
  GL_VIEWPORT                       = $0BA2;
  GL_MODELVIEW_STACK_DEPTH          = $0BA3;
  GL_PROJECTION_STACK_DEPTH         = $0BA4;
  GL_TEXTURE_STACK_DEPTH            = $0BA5;
  GL_MODELVIEW_MATRIX               = $0BA6;
  GL_PROJECTION_MATRIX              = $0BA7;
  GL_TEXTURE_MATRIX                 = $0BA8;
  GL_ATTRIB_STACK_DEPTH             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH      = $0BB1;
  GL_ALPHA_TEST                     = $0BC0;
  GL_ALPHA_TEST_FUNC                = $0BC1;
  GL_ALPHA_TEST_REF                 = $0BC2;
  GL_DITHER                         = $0BD0;
  GL_BLEND_DST                      = $0BE0;
  GL_BLEND_SRC                      = $0BE1;
  GL_BLEND                          = $0BE2;
  GL_LOGIC_OP_MODE                  = $0BF0;
  GL_INDEX_LOGIC_OP                 = $0BF1;
  GL_COLOR_LOGIC_OP                 = $0BF2;
  GL_AUX_BUFFERS                    = $0C00;
  GL_DRAW_BUFFER                    = $0C01;
  GL_READ_BUFFER                    = $0C02;
  GL_SCISSOR_BOX                    = $0C10;
  GL_SCISSOR_TEST                   = $0C11;
  GL_INDEX_CLEAR_VALUE              = $0C20;
  GL_INDEX_WRITEMASK                = $0C21;
  GL_COLOR_CLEAR_VALUE              = $0C22;
  GL_COLOR_WRITEMASK                = $0C23;
  GL_INDEX_MODE                     = $0C30;
  GL_RGBA_MODE                      = $0C31;
  GL_DOUBLEBUFFER                   = $0C32;
  GL_STEREO                         = $0C33;
  GL_RENDER_MODE                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_POINT_SMOOTH_HINT              = $0C51;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_POLYGON_SMOOTH_HINT            = $0C53;
  GL_FOG_HINT                       = $0C54;
  GL_TEXTURE_GEN_S                  = $0C60;
  GL_TEXTURE_GEN_T                  = $0C61;
  GL_TEXTURE_GEN_R                  = $0C62;
  GL_TEXTURE_GEN_Q                  = $0C63;
  GL_PIXEL_MAP_I_TO_I               = $0C70;
  GL_PIXEL_MAP_S_TO_S               = $0C71;
  GL_PIXEL_MAP_I_TO_R               = $0C72;
  GL_PIXEL_MAP_I_TO_G               = $0C73;
  GL_PIXEL_MAP_I_TO_B               = $0C74;
  GL_PIXEL_MAP_I_TO_A               = $0C75;
  GL_PIXEL_MAP_R_TO_R               = $0C76;
  GL_PIXEL_MAP_G_TO_G               = $0C77;
  GL_PIXEL_MAP_B_TO_B               = $0C78;
  GL_PIXEL_MAP_A_TO_A               = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE          = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE          = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE          = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE          = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE          = $0CB9;
  GL_UNPACK_SWAP_BYTES              = $0CF0;
  GL_UNPACK_LSB_FIRST               = $0CF1;
  GL_UNPACK_ROW_LENGTH              = $0CF2;
  GL_UNPACK_SKIP_ROWS               = $0CF3;
  GL_UNPACK_SKIP_PIXELS             = $0CF4;
  GL_UNPACK_ALIGNMENT               = $0CF5;
  GL_PACK_SWAP_BYTES                = $0D00;
  GL_PACK_LSB_FIRST                 = $0D01;
  GL_PACK_ROW_LENGTH                = $0D02;
  GL_PACK_SKIP_ROWS                 = $0D03;
  GL_PACK_SKIP_PIXELS               = $0D04;
  GL_PACK_ALIGNMENT                 = $0D05;
  GL_MAP_COLOR                      = $0D10;
  GL_MAP_STENCIL                    = $0D11;
  GL_INDEX_SHIFT                    = $0D12;
  GL_INDEX_OFFSET                   = $0D13;
  GL_RED_SCALE                      = $0D14;
  GL_RED_BIAS                       = $0D15;
  GL_ZOOM_X                         = $0D16;
  GL_ZOOM_Y                         = $0D17;
  GL_GREEN_SCALE                    = $0D18;
  GL_GREEN_BIAS                     = $0D19;
  GL_BLUE_SCALE                     = $0D1A;
  GL_BLUE_BIAS                      = $0D1B;
  GL_ALPHA_SCALE                    = $0D1C;
  GL_ALPHA_BIAS                     = $0D1D;
  GL_DEPTH_SCALE                    = $0D1E;
  GL_DEPTH_BIAS                     = $0D1F;
  GL_MAX_EVAL_ORDER                 = $0D30;
  GL_MAX_LIGHTS                     = $0D31;
  GL_MAX_CLIP_PLANES                = $0D32;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_PIXEL_MAP_TABLE            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH      = $0D36;
  GL_MAX_NAME_STACK_DEPTH           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH        = $0D39;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = $0D3B;
  GL_SUBPIXEL_BITS                  = $0D50;
  GL_INDEX_BITS                     = $0D51;
  GL_RED_BITS                       = $0D52;
  GL_GREEN_BITS                     = $0D53;
  GL_BLUE_BITS                      = $0D54;
  GL_ALPHA_BITS                     = $0D55;
  GL_DEPTH_BITS                     = $0D56;
  GL_STENCIL_BITS                   = $0D57;
  GL_ACCUM_RED_BITS                 = $0D58;
  GL_ACCUM_GREEN_BITS               = $0D59;
  GL_ACCUM_BLUE_BITS                = $0D5A;
  GL_ACCUM_ALPHA_BITS               = $0D5B;
  GL_NAME_STACK_DEPTH               = $0D70;
  GL_AUTO_NORMAL                    = $0D80;
  GL_MAP1_COLOR_4                   = $0D90;
  GL_MAP1_INDEX                     = $0D91;
  GL_MAP1_NORMAL                    = $0D92;
  GL_MAP1_TEXTURE_COORD_1           = $0D93;
  GL_MAP1_TEXTURE_COORD_2           = $0D94;
  GL_MAP1_TEXTURE_COORD_3           = $0D95;
  GL_MAP1_TEXTURE_COORD_4           = $0D96;
  GL_MAP1_VERTEX_3                  = $0D97;
  GL_MAP1_VERTEX_4                  = $0D98;
  GL_MAP2_COLOR_4                   = $0DB0;
  GL_MAP2_INDEX                     = $0DB1;
  GL_MAP2_NORMAL                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1           = $0DB3;
  GL_MAP2_TEXTURE_COORD_2           = $0DB4;
  GL_MAP2_TEXTURE_COORD_3           = $0DB5;
  GL_MAP2_TEXTURE_COORD_4           = $0DB6;
  GL_MAP2_VERTEX_3                  = $0DB7;
  GL_MAP2_VERTEX_4                  = $0DB8;
  GL_MAP1_GRID_DOMAIN               = $0DD0;
  GL_MAP1_GRID_SEGMENTS             = $0DD1;
  GL_MAP2_GRID_DOMAIN               = $0DD2;
  GL_MAP2_GRID_SEGMENTS             = $0DD3;
  GL_TEXTURE_1D                     = $0DE0;
  GL_TEXTURE_2D                     = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER        = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE           = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE           = $0DF2;
  GL_SELECTION_BUFFER_POINTER       = $0DF3;
  GL_SELECTION_BUFFER_SIZE          = $0DF4;
  //      GL_TEXTURE_BINDING_1D
  //      GL_TEXTURE_BINDING_2D
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_VERTEX_ARRAY_SIZE
  //      GL_VERTEX_ARRAY_TYPE
  //      GL_VERTEX_ARRAY_STRIDE
  //      GL_NORMAL_ARRAY_TYPE
  //      GL_NORMAL_ARRAY_STRIDE
  //      GL_COLOR_ARRAY_SIZE
  //      GL_COLOR_ARRAY_TYPE
  //      GL_COLOR_ARRAY_STRIDE
  //      GL_INDEX_ARRAY_TYPE
  //      GL_INDEX_ARRAY_STRIDE
  //      GL_TEXTURE_COORD_ARRAY_SIZE
  //      GL_TEXTURE_COORD_ARRAY_TYPE
  //      GL_TEXTURE_COORD_ARRAY_STRIDE
  //      GL_EDGE_FLAG_ARRAY_STRIDE
  //      GL_POLYGON_OFFSET_FACTOR
  //      GL_POLYGON_OFFSET_UNITS

  // GetTextureParameter
  //      GL_TEXTURE_MAG_FILTER
  //      GL_TEXTURE_MIN_FILTER
  //      GL_TEXTURE_WRAP_S
  //      GL_TEXTURE_WRAP_T
  GL_TEXTURE_WIDTH                  = $1000;
  GL_TEXTURE_HEIGHT                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT        = $1003;
  GL_TEXTURE_BORDER_COLOR           = $1004;
  GL_TEXTURE_BORDER                 = $1005;
  //      GL_TEXTURE_RED_SIZE
  //      GL_TEXTURE_GREEN_SIZE
  //      GL_TEXTURE_BLUE_SIZE
  //      GL_TEXTURE_ALPHA_SIZE
  //      GL_TEXTURE_LUMINANCE_SIZE
  //      GL_TEXTURE_INTENSITY_SIZE
  //      GL_TEXTURE_PRIORITY
  //      GL_TEXTURE_RESIDENT

  // HintMode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // HintTarget
  //      GL_PERSPECTIVE_CORRECTION_HINT
  //      GL_POINT_SMOOTH_HINT
  //      GL_LINE_SMOOTH_HINT
  //      GL_POLYGON_SMOOTH_HINT
  //      GL_FOG_HINT

  // IndexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // LightModelParameter
  //      GL_LIGHT_MODEL_AMBIENT
  //      GL_LIGHT_MODEL_LOCAL_VIEWER
  //      GL_LIGHT_MODEL_TWO_SIDE

  // LightName
  GL_LIGHT0                         = $4000;
  GL_LIGHT1                         = $4001;
  GL_LIGHT2                         = $4002;
  GL_LIGHT3                         = $4003;
  GL_LIGHT4                         = $4004;
  GL_LIGHT5                         = $4005;
  GL_LIGHT6                         = $4006;
  GL_LIGHT7                         = $4007;

  // LightParameter
  GL_AMBIENT                        = $1200;
  GL_DIFFUSE                        = $1201;
  GL_SPECULAR                       = $1202;
  GL_POSITION                       = $1203;
  GL_SPOT_DIRECTION                 = $1204;
  GL_SPOT_EXPONENT                  = $1205;
  GL_SPOT_CUTOFF                    = $1206;
  GL_CONSTANT_ATTENUATION           = $1207;
  GL_LINEAR_ATTENUATION             = $1208;
  GL_QUADRATIC_ATTENUATION          = $1209;

  // InterleavedArrays
  //      GL_V2F
  //      GL_V3F
  //      GL_C4UB_V2F
  //      GL_C4UB_V3F
  //      GL_C3F_V3F
  //      GL_N3F_V3F
  //      GL_C4F_N3F_V3F
  //      GL_T2F_V3F
  //      GL_T4F_V4F
  //      GL_T2F_C4UB_V3F
  //      GL_T2F_C3F_V3F
  //      GL_T2F_N3F_V3F
  //      GL_T2F_C4F_N3F_V3F
  //      GL_T4F_C4F_N3F_V4F

  // ListMode
  GL_COMPILE                        = $1300;
  GL_COMPILE_AND_EXECUTE            = $1301;

  // ListNameType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_2_BYTES
  //      GL_3_BYTES
  //      GL_4_BYTES

  // LogicOp
  GL_CLEAR                          = $1500;
  GL_AND                            = $1501;
  GL_AND_REVERSE                    = $1502;
  GL_COPY                           = $1503;
  GL_AND_INVERTED                   = $1504;
  GL_NOOP                           = $1505;
  GL_XOR                            = $1506;
  GL_OR                             = $1507;
  GL_NOR                            = $1508;
  GL_EQUIV                          = $1509;
  GL_INVERT                         = $150A;
  GL_OR_REVERSE                     = $150B;
  GL_COPY_INVERTED                  = $150C;
  GL_OR_INVERTED                    = $150D;
  GL_NAND                           = $150E;
  GL_SET                            = $150F;

  // MapTarget
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4

  // MaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // MaterialParameter
  GL_EMISSION                       = $1600;
  GL_SHININESS                      = $1601;
  GL_AMBIENT_AND_DIFFUSE            = $1602;
  GL_COLOR_INDEXES                  = $1603;
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR

  // MatrixMode
  GL_MODELVIEW                      = $1700;
  GL_PROJECTION                     = $1701;
  GL_TEXTURE                        = $1702;

  // MeshMode1
  //      GL_POINT
  //      GL_LINE

  // MeshMode2
  //      GL_POINT
  //      GL_LINE
  //      GL_FILL

  // NormalPointerType
  //      GL_BYTE
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // PixelCopyType
  GL_COLOR                          = $1800;
  GL_DEPTH                          = $1801;
  GL_STENCIL                        = $1802;

  // PixelFormat
  GL_COLOR_INDEX                    = $1900;
  GL_STENCIL_INDEX                  = $1901;
  GL_DEPTH_COMPONENT                = $1902;
  GL_RED                            = $1903;
  GL_GREEN                          = $1904;
  GL_BLUE                           = $1905;
  GL_ALPHA                          = $1906;
  GL_RGB                            = $1907;
  GL_RGBA                           = $1908;
  GL_LUMINANCE                      = $1909;
  GL_LUMINANCE_ALPHA                = $190A;

  // PixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // PixelStore
  //      GL_UNPACK_SWAP_BYTES
  //      GL_UNPACK_LSB_FIRST
  //      GL_UNPACK_ROW_LENGTH
  //      GL_UNPACK_SKIP_ROWS
  //      GL_UNPACK_SKIP_PIXELS
  //      GL_UNPACK_ALIGNMENT
  //      GL_PACK_SWAP_BYTES
  //      GL_PACK_LSB_FIRST
  //      GL_PACK_ROW_LENGTH
  //      GL_PACK_SKIP_ROWS
  //      GL_PACK_SKIP_PIXELS
  //      GL_PACK_ALIGNMENT

  // PixelTransfer
  //      GL_MAP_COLOR
  //      GL_MAP_STENCIL
  //      GL_INDEX_SHIFT
  //      GL_INDEX_OFFSET
  //      GL_RED_SCALE
  //      GL_RED_BIAS
  //      GL_GREEN_SCALE
  //      GL_GREEN_BIAS
  //      GL_BLUE_SCALE
  //      GL_BLUE_BIAS
  //      GL_ALPHA_SCALE
  //      GL_ALPHA_BIAS
  //      GL_DEPTH_SCALE
  //      GL_DEPTH_BIAS

  // PixelType
  GL_BITMAP                         = $1A00;
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT

  // PolygonMode
  GL_POINT                          = $1B00;
  GL_LINE                           = $1B01;
  GL_FILL                           = $1B02;

  // ReadBufferMode
  //      GL_FRONT_LEFT
  //      GL_FRONT_RIGHT
  //      GL_BACK_LEFT
  //      GL_BACK_RIGHT
  //      GL_FRONT
  //      GL_BACK
  //      GL_LEFT
  //      GL_RIGHT
  //      GL_AUX0
  //      GL_AUX1
  //      GL_AUX2
  //      GL_AUX3

  // RenderingMode
  GL_RENDER                         = $1C00;
  GL_FEEDBACK                       = $1C01;
  GL_SELECT                         = $1C02;

  // ShadingModel
  GL_FLAT                           = $1D00;
  GL_SMOOTH                         = $1D01;

  // StencilFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // StencilOp
  //      GL_ZERO
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;
  //      GL_INVERT

  // StringName
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

  // TextureCoordName
  GL_S                              = $2000;
  GL_T                              = $2001;
  GL_R                              = $2002;
  GL_Q                              = $2003;

  // TexCoordPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // TextureEnvMode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  //      GL_BLEND
  //      GL_REPLACE

  // TextureEnvParameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;

  // TextureEnvTarget
  GL_TEXTURE_ENV                    = $2300;

  // TextureGenMode
  GL_EYE_LINEAR                     = $2400;
  GL_OBJECT_LINEAR                  = $2401;
  GL_SPHERE_MAP                     = $2402;

  // TextureGenParameter
  GL_TEXTURE_GEN_MODE               = $2500;
  GL_OBJECT_PLANE                   = $2501;
  GL_EYE_PLANE                      = $2502;

  // TextureMagFilter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;

  // TextureMinFilter
  //      GL_NEAREST
  //      GL_LINEAR
  GL_NEAREST_MIPMAP_NEAREST         = $2700;
  GL_LINEAR_MIPMAP_NEAREST          = $2701;
  GL_NEAREST_MIPMAP_LINEAR          = $2702;
  GL_LINEAR_MIPMAP_LINEAR           = $2703;

  // TextureParameterName
  GL_TEXTURE_MAG_FILTER             = $2800;
  GL_TEXTURE_MIN_FILTER             = $2801;
  GL_TEXTURE_WRAP_S                 = $2802;
  GL_TEXTURE_WRAP_T                 = $2803;
  //      GL_TEXTURE_BORDER_COLOR
  //      GL_TEXTURE_PRIORITY

  // TextureTarget
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_PROXY_TEXTURE_1D
  //      GL_PROXY_TEXTURE_2D

  // TextureWrapMode
  GL_CLAMP                          = $2900;
  GL_REPEAT                         = $2901;

  // VertexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // ClientAttribMask
  GL_CLIENT_PIXEL_STORE_BIT         = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT        = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS         = $FFFFFFFF;

  // polygon_offset
  GL_POLYGON_OFFSET_FACTOR          = $8038;
  GL_POLYGON_OFFSET_UNITS           = $2A00;
  GL_POLYGON_OFFSET_POINT           = $2A01;
  GL_POLYGON_OFFSET_LINE            = $2A02;
  GL_POLYGON_OFFSET_FILL            = $8037;

  // texture
  GL_ALPHA4                         = $803B;
  GL_ALPHA8                         = $803C;
  GL_ALPHA12                        = $803D;
  GL_ALPHA16                        = $803E;
  GL_LUMINANCE4                     = $803F;
  GL_LUMINANCE8                     = $8040;
  GL_LUMINANCE12                    = $8041;
  GL_LUMINANCE16                    = $8042;
  GL_LUMINANCE4_ALPHA4              = $8043;
  GL_LUMINANCE6_ALPHA2              = $8044;
  GL_LUMINANCE8_ALPHA8              = $8045;
  GL_LUMINANCE12_ALPHA4             = $8046;
  GL_LUMINANCE12_ALPHA12            = $8047;
  GL_LUMINANCE16_ALPHA16            = $8048;
  GL_INTENSITY                      = $8049;
  GL_INTENSITY4                     = $804A;
  GL_INTENSITY8                     = $804B;
  GL_INTENSITY12                    = $804C;
  GL_INTENSITY16                    = $804D;
  GL_R3_G3_B2                       = $2A10;
  GL_RGB4                           = $804F;
  GL_RGB5                           = $8050;
  GL_RGB8                           = $8051;
  GL_RGB10                          = $8052;
  GL_RGB12                          = $8053;
  GL_RGB16                          = $8054;
  GL_RGBA2                          = $8055;
  GL_RGBA4                          = $8056;
  GL_RGB5_A1                        = $8057;
  GL_RGBA8                          = $8058;
  GL_RGB10_A2                       = $8059;
  GL_RGBA12                         = $805A;
  GL_RGBA16                         = $805B;
  GL_TEXTURE_RED_SIZE               = $805C;
  GL_TEXTURE_GREEN_SIZE             = $805D;
  GL_TEXTURE_BLUE_SIZE              = $805E;
  GL_TEXTURE_ALPHA_SIZE             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE         = $8060;
  GL_TEXTURE_INTENSITY_SIZE         = $8061;
  GL_PROXY_TEXTURE_1D               = $8063;
  GL_PROXY_TEXTURE_2D               = $8064;

  // texture_object
  GL_TEXTURE_PRIORITY               = $8066;
  GL_TEXTURE_RESIDENT               = $8067;
  GL_TEXTURE_BINDING_1D             = $8068;
  GL_TEXTURE_BINDING_2D             = $8069;

  // vertex_array
  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_INDEX_ARRAY                    = $8077;
  GL_TEXTURE_COORD_ARRAY            = $8078;
  GL_EDGE_FLAG_ARRAY                = $8079;
  GL_VERTEX_ARRAY_SIZE              = $807A;
  GL_VERTEX_ARRAY_TYPE              = $807B;
  GL_VERTEX_ARRAY_STRIDE            = $807C;
  GL_NORMAL_ARRAY_TYPE              = $807E;
  GL_NORMAL_ARRAY_STRIDE            = $807F;
  GL_COLOR_ARRAY_SIZE               = $8081;
  GL_COLOR_ARRAY_TYPE               = $8082;
  GL_COLOR_ARRAY_STRIDE             = $8083;
  GL_INDEX_ARRAY_TYPE               = $8085;
  GL_INDEX_ARRAY_STRIDE             = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE         = $808C;
  GL_VERTEX_ARRAY_POINTER           = $808E;
  GL_NORMAL_ARRAY_POINTER           = $808F;
  GL_COLOR_ARRAY_POINTER            = $8090;
  GL_INDEX_ARRAY_POINTER            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER        = $8093;
  GL_V2F                            = $2A20;
  GL_V3F                            = $2A21;
  GL_C4UB_V2F                       = $2A22;
  GL_C4UB_V3F                       = $2A23;
  GL_C3F_V3F                        = $2A24;
  GL_N3F_V3F                        = $2A25;
  GL_C4F_N3F_V3F                    = $2A26;
  GL_T2F_V3F                        = $2A27;
  GL_T4F_V4F                        = $2A28;
  GL_T2F_C4UB_V3F                   = $2A29;
  GL_T2F_C3F_V3F                    = $2A2A;
  GL_T2F_N3F_V3F                    = $2A2B;
  GL_T2F_C4F_N3F_V3F                = $2A2C;
  GL_T4F_C4F_N3F_V4F                = $2A2D;

  // Extensions
  GL_EXT_vertex_array               = 1;
  GL_WIN_swap_hint                  = 1;
  GL_EXT_bgra                       = 1;
  GL_EXT_paletted_texture           = 1;

  // EXT_vertex_array
  GL_VERTEX_ARRAY_EXT               = $8074;
  GL_NORMAL_ARRAY_EXT               = $8075;
  GL_COLOR_ARRAY_EXT                = $8076;
  GL_INDEX_ARRAY_EXT                = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT        = $8078;
  GL_EDGE_FLAG_ARRAY_EXT            = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT          = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT          = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT        = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT         = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT          = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT        = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT         = $8080;
  GL_COLOR_ARRAY_SIZE_EXT           = $8081;
  GL_COLOR_ARRAY_TYPE_EXT           = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT         = $8083;
  GL_COLOR_ARRAY_COUNT_EXT          = $8084;
  GL_INDEX_ARRAY_TYPE_EXT           = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT         = $8086;
  GL_INDEX_ARRAY_COUNT_EXT          = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT   = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT   = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT  = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT     = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT      = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT       = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT       = $808F;
  GL_COLOR_ARRAY_POINTER_EXT        = $8090;
  GL_INDEX_ARRAY_POINTER_EXT        = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT    = $8093;
  GL_DOUBLE_EXT                     = GL_DOUBLE;

  // EXT_bgra
  GL_BGR_EXT                        = $80E0;
  GL_BGRA_EXT                       = $80E1;

  // EXT_paletted_texture

  // These must match the GL_COLOR_TABLE_*_SGI enumerants
  GL_COLOR_TABLE_FORMAT_EXT         = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT          = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT       = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT     = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT      = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT     = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT = $80DF;

  GL_COLOR_INDEX1_EXT               = $80E2;
  GL_COLOR_INDEX2_EXT               = $80E3;
  GL_COLOR_INDEX4_EXT               = $80E4;
  GL_COLOR_INDEX8_EXT               = $80E5;
  GL_COLOR_INDEX12_EXT              = $80E6;
  GL_COLOR_INDEX16_EXT              = $80E7;

  // For compatibility with OpenGL v1.0
  GL_LOGIC_OP                       = GL_INDEX_LOGIC_OP;
  GL_TEXTURE_COMPONENTS             = GL_TEXTURE_INTERNAL_FORMAT;

{******************************************************************************}

var
  glAccum: procedure(op: GLenum; value: GLfloat); extdecl;
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); extdecl;
  glAreTexturesResident: function (n: GLsizei; const textures: PGLuint; residences: PGLboolean): GLboolean; extdecl;
  glArrayElement: procedure(i: GLint); extdecl;
  glBegin: procedure(mode: GLenum); extdecl;
  glBindTexture: procedure(target: GLenum; texture: GLuint); extdecl;
  glBitmap: procedure (width, height: GLsizei; xorig, yorig: GLfloat; xmove, ymove: GLfloat; const bitmap: PGLubyte); extdecl;
  glBlendFunc: procedure(sfactor, dfactor: GLenum); extdecl;
  glCallList: procedure(list: GLuint); extdecl;
  glCallLists: procedure(n: GLsizei; atype: GLenum; const lists: Pointer); extdecl;
  glClear: procedure(mask: GLbitfield); extdecl;
  glClearAccum: procedure(red, green, blue, alpha: GLfloat); extdecl;
  glClearColor: procedure(red, green, blue, alpha: GLclampf); extdecl;
  glClearDepth: procedure(depth: GLclampd); extdecl;
  glClearIndex: procedure(c: GLfloat); extdecl;
  glClearStencil: procedure(s: GLint); extdecl;
  glClipPlane: procedure(plane: GLenum; const equation: PGLdouble); extdecl;
  glColor3b: procedure(red, green, blue: GLbyte); extdecl;
  glColor3bv: procedure(const v: PGLbyte); extdecl;
  glColor3d: procedure(red, green, blue: GLdouble); extdecl;
  glColor3dv: procedure(const v: PGLdouble); extdecl;
  glColor3f: procedure(red, green, blue: GLfloat); extdecl;
  glColor3fv: procedure(const v: PGLfloat); extdecl;
  glColor3i: procedure(red, green, blue: GLint); extdecl;
  glColor3iv: procedure(const v: PGLint); extdecl;
  glColor3s: procedure(red, green, blue: GLshort); extdecl;
  glColor3sv: procedure(const v: PGLshort); extdecl;
  glColor3ub: procedure(red, green, blue: GLubyte); extdecl;
  glColor3ubv: procedure(const v: PGLubyte); extdecl;
  glColor3ui: procedure(red, green, blue: GLuint); extdecl;
  glColor3uiv: procedure(const v: PGLuint); extdecl;
  glColor3us: procedure(red, green, blue: GLushort); extdecl;
  glColor3usv: procedure(const v: PGLushort); extdecl;
  glColor4b: procedure(red, green, blue, alpha: GLbyte); extdecl;
  glColor4bv: procedure(const v: PGLbyte); extdecl;
  glColor4d: procedure(red, green, blue, alpha: GLdouble); extdecl;
  glColor4dv: procedure(const v: PGLdouble); extdecl;
  glColor4f: procedure(red, green, blue, alpha: GLfloat); extdecl;
  glColor4fv: procedure(const v: PGLfloat); extdecl;
  glColor4i: procedure(red, green, blue, alpha: GLint); extdecl;
  glColor4iv: procedure(const v: PGLint); extdecl;
  glColor4s: procedure(red, green, blue, alpha: GLshort); extdecl;
  glColor4sv: procedure(const v: PGLshort); extdecl;
  glColor4ub: procedure(red, green, blue, alpha: GLubyte); extdecl;
  glColor4ubv: procedure(const v: PGLubyte); extdecl;
  glColor4ui: procedure(red, green, blue, alpha: GLuint); extdecl;
  glColor4uiv: procedure(const v: PGLuint); extdecl;
  glColor4us: procedure(red, green, blue, alpha: GLushort); extdecl;
  glColor4usv: procedure(const v: PGLushort); extdecl;
  glColorMask: procedure(red, green, blue, alpha: GLboolean); extdecl;
  glColorMaterial: procedure(face, mode: GLenum); extdecl;
  glColorPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glCopyPixels: procedure(x, y: GLint; width, height: GLsizei; atype: GLenum); extdecl;
  glCopyTexImage1D: procedure (target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width: GLsizei; border: GLint); extdecl;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); extdecl;
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y: GLint; width: GLsizei); extdecl;
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); extdecl;
  glCullFace: procedure(mode: GLenum); extdecl;
  glDeleteLists: procedure(list: GLuint; range: GLsizei); extdecl;
  glDeleteTextures: procedure(n: GLsizei; const textures: PGLuint); extdecl;
  glDepthFunc: procedure(func: GLenum); extdecl;
  glDepthMask: procedure(flag: GLboolean); extdecl;
  glDepthRange: procedure(zNear, zFar: GLclampd); extdecl;
  glDisable: procedure(cap: GLenum); extdecl;
  glDisableClientState: procedure(aarray: GLenum); extdecl;
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  glDrawBuffer: procedure(mode: GLenum); extdecl;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); extdecl;
  glDrawPixels: procedure(width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glEdgeFlag: procedure(flag: GLboolean); extdecl;
  glEdgeFlagPointer: procedure(stride: GLsizei; const pointer: Pointer); extdecl;
  glEdgeFlagv: procedure(const flag: PGLboolean); extdecl;
  glEnable: procedure(cap: GLenum); extdecl;
  glEnableClientState: procedure(aarray: GLenum); extdecl;
  glEnd: procedure; extdecl;
  glEndList: procedure; extdecl;
  glEvalCoord1d: procedure(u: GLdouble); extdecl;
  glEvalCoord1dv: procedure(const u: PGLdouble); extdecl;
  glEvalCoord1f: procedure(u: GLfloat); extdecl;
  glEvalCoord1fv: procedure(const u: PGLfloat); extdecl;
  glEvalCoord2d: procedure(u, v: GLdouble); extdecl;
  glEvalCoord2dv: procedure(const u: PGLdouble); extdecl;
  glEvalCoord2f: procedure(u, v: GLfloat); extdecl;
  glEvalCoord2fv: procedure(const u: PGLfloat); extdecl;
  glEvalMesh1: procedure(mode: GLenum; i1, i2: GLint); extdecl;
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: GLint); extdecl;
  glEvalPoint1: procedure(i: GLint); extdecl;
  glEvalPoint2: procedure(i, j: GLint); extdecl;
  glFeedbackBuffer: procedure(size: GLsizei; atype: GLenum; buffer: PGLfloat); extdecl;
  glFinish: procedure; extdecl;
  glFlush: procedure; extdecl;
  glFogf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glFogfv: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glFogi: procedure(pname: GLenum; param: GLint); extdecl;
  glFogiv: procedure(pname: GLenum; const params: PGLint); extdecl;
  glFrontFace: procedure(mode: GLenum); extdecl;
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: GLdouble); extdecl;
  glGenLists: function(range: GLsizei): GLuint; extdecl;
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); extdecl;
  glGetBooleanv: procedure(pname: GLenum; params: PGLboolean); extdecl;
  glGetClipPlane: procedure(plane: GLenum; equation: PGLdouble); extdecl;
  glGetDoublev: procedure(pname: GLenum; params: PGLdouble); extdecl;
  glGetError: function: GLenum; extdecl;
  glGetFloatv: procedure(pname: GLenum; params: PGLfloat); extdecl;
  glGetIntegerv: procedure(pname: GLenum; params: PGLint); extdecl;
  glGetLightfv: procedure(light, pname: GLenum; params: PGLfloat); extdecl;
  glGetLightiv: procedure(light, pname: GLenum; params: PGLint); extdecl;
  glGetMapdv: procedure(target, query: GLenum; v: PGLdouble); extdecl;
  glGetMapfv: procedure(target, query: GLenum; v: PGLfloat); extdecl;
  glGetMapiv: procedure(target, query: GLenum; v: PGLint); extdecl;
  glGetMaterialfv: procedure(face, pname: GLenum; params: PGLfloat); extdecl;
  glGetMaterialiv: procedure(face, pname: GLenum; params: PGLint); extdecl;
  glGetPixelMapfv: procedure(map: GLenum; values: PGLfloat); extdecl;
  glGetPixelMapuiv: procedure(map: GLenum; values: PGLuint); extdecl;
  glGetPixelMapusv: procedure(map: GLenum; values: PGLushort); extdecl;
  glGetPointerv: procedure(pname: GLenum; params: Pointer); extdecl;
  glGetPolygonStipple: procedure(mask: PGLubyte); extdecl;
  glGetString: function(name: GLenum): PChar; extdecl;
  glGetTexEnvfv: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexEnviv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glGetTexGendv: procedure(coord, pname: GLenum; params: PGLdouble); extdecl;
  glGetTexGenfv: procedure(coord, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexGeniv: procedure(coord, pname: GLenum; params: PGLint); extdecl;
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); extdecl;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: Pointer); extdecl;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); extdecl;
  glGetTexParameterfv: procedure(target, pname: GLenum; params: PGLfloat); extdecl;
  glGetTexParameteriv: procedure(target, pname: GLenum; params: PGLint); extdecl;
  glHint: procedure(target, mode: GLenum); extdecl;
  glIndexMask: procedure(mask: GLuint); extdecl;
  glIndexPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glIndexd: procedure(c: GLdouble); extdecl;
  glIndexdv: procedure(const c: PGLdouble); extdecl;
  glIndexf: procedure(c: GLfloat); extdecl;
  glIndexfv: procedure(const c: PGLfloat); extdecl;
  glIndexi: procedure(c: GLint); extdecl;
  glIndexiv: procedure(const c: PGLint); extdecl;
  glIndexs: procedure(c: GLshort); extdecl;
  glIndexsv: procedure(const c: PGLshort); extdecl;
  glIndexub: procedure(c: GLubyte); extdecl;
  glIndexubv: procedure(const c: PGLubyte); extdecl;
  glInitNames: procedure; extdecl;
  glInterleavedArrays: procedure(format: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glIsEnabled: function(cap: GLenum): GLboolean; extdecl;
  glIsList: function(list: GLuint): GLboolean; extdecl;
  glIsTexture: function(texture: GLuint): GLboolean; extdecl;
  glLightModelf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glLightModelfv: procedure(pname: GLenum; const params: PGLfloat); extdecl;
  glLightModeli: procedure(pname: GLenum; param: GLint); extdecl;
  glLightModeliv: procedure(pname: GLenum; const params: PGLint); extdecl;
  glLightf: procedure(light, pname: GLenum; param: GLfloat); extdecl;
  glLightfv: procedure(light, pname: GLenum; const params: PGLfloat); extdecl;
  glLighti: procedure(light, pname: GLenum; param: GLint); extdecl;
  glLightiv: procedure(light, pname: GLenum; const params: PGLint); extdecl;
  glLineStipple: procedure(factor: GLint; pattern: GLushort); extdecl;
  glLineWidth: procedure(width: GLfloat); extdecl;
  glListBase: procedure(base: GLuint); extdecl;
  glLoadIdentity: procedure; extdecl;
  glLoadMatrixd: procedure(const m: PGLdouble); extdecl;
  glLoadMatrixf: procedure(const m: PGLfloat); extdecl;
  glLoadName: procedure(name: GLuint); extdecl;
  glLogicOp: procedure(opcode: GLenum); extdecl;
  glMap1d: procedure(target: GLenum; u1, u2: GLdouble; stride, order: GLint; const points: PGLdouble); extdecl;
  glMap1f: procedure(target: GLenum; u1, u2: GLfloat; stride, order: GLint; const points: PGLfloat); extdecl;
  glMap2d: procedure(target: GLenum; u1, u2: GLdouble; ustride, uorder: GLint; v1, v2: GLdouble; vstride, vorder: GLint; const points: PGLdouble); extdecl;
  glMap2f: procedure(target: GLenum; u1, u2: GLfloat; ustride, uorder: GLint; v1, v2: GLfloat; vstride, vorder: GLint; const points: PGLfloat); extdecl;
  glMapGrid1d: procedure(un: GLint; u1, u2: GLdouble); extdecl;
  glMapGrid1f: procedure(un: GLint; u1, u2: GLfloat); extdecl;
  glMapGrid2d: procedure(un: GLint; u1, u2: GLdouble; vn: GLint; v1, v2: GLdouble); extdecl;
  glMapGrid2f: procedure(un: GLint; u1, u2: GLfloat; vn: GLint; v1, v2: GLfloat); extdecl;
  glMaterialf: procedure(face, pname: GLenum; param: GLfloat); extdecl;
  glMaterialfv: procedure(face, pname: GLenum; const params: PGLfloat); extdecl;
  glMateriali: procedure(face, pname: GLenum; param: GLint); extdecl;
  glMaterialiv: procedure(face, pname: GLenum; const params: PGLint); extdecl;
  glMatrixMode: procedure(mode: GLenum); extdecl;
  glMultMatrixd: procedure(const m: PGLdouble); extdecl;
  glMultMatrixf: procedure(const m: PGLfloat); extdecl;
  glNewList: procedure(list: GLuint; mode: GLenum); extdecl;
  glNormal3b: procedure(nx, ny, nz: GLbyte); extdecl;
  glNormal3bv: procedure(const v: PGLbyte); extdecl;
  glNormal3d: procedure(nx, ny, nz: GLdouble); extdecl;
  glNormal3dv: procedure(const v: PGLdouble); extdecl;
  glNormal3f: procedure(nx, ny, nz: GLfloat); extdecl;
  glNormal3fv: procedure(const v: PGLfloat); extdecl;
  glNormal3i: procedure(nx, ny, nz: GLint); extdecl;
  glNormal3iv: procedure(const v: PGLint); extdecl;
  glNormal3s: procedure(nx, ny, nz: GLshort); extdecl;
  glNormal3sv: procedure(const v: PGLshort); extdecl;
  glNormalPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glOrtho: procedure(left, right, bottom, top, zNear, zFar: GLdouble); extdecl;
  glPassThrough: procedure(token: GLfloat); extdecl;
  glPixelMapfv: procedure(map: GLenum; mapsize: GLsizei; const values: PGLfloat); extdecl;
  glPixelMapuiv: procedure(map: GLenum; mapsize: GLsizei; const values: PGLuint); extdecl;
  glPixelMapusv: procedure(map: GLenum; mapsize: GLsizei; const values: PGLushort); extdecl;
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelStorei: procedure(pname: GLenum; param: GLint); extdecl;
  glPixelTransferf: procedure(pname: GLenum; param: GLfloat); extdecl;
  glPixelTransferi: procedure(pname: GLenum; param: GLint); extdecl;
  glPixelZoom: procedure(xfactor, yfactor: GLfloat); extdecl;
  glPointSize: procedure(size: GLfloat); extdecl;
  glPolygonMode: procedure(face, mode: GLenum); extdecl;
  glPolygonOffset: procedure(factor, units: GLfloat); extdecl;
  glPolygonStipple: procedure(const mask: PGLubyte); extdecl;
  glPopAttrib: procedure; extdecl;
  glPopClientAttrib: procedure; extdecl;
  glPopMatrix: procedure; extdecl;
  glPopName: procedure; extdecl;
  glPrioritizeTextures: procedure(n: GLsizei; const textures: PGLuint; const priorities: PGLclampf); extdecl;
  glPushAttrib: procedure(mask: GLbitfield); extdecl;
  glPushClientAttrib: procedure(mask: GLbitfield); extdecl;
  glPushMatrix: procedure; extdecl;
  glPushName: procedure(name: GLuint); extdecl;
  glRasterPos2d: procedure(x, y: GLdouble); extdecl;
  glRasterPos2dv: procedure(const v: PGLdouble); extdecl;
  glRasterPos2f: procedure(x, y: GLfloat); extdecl;
  glRasterPos2fv: procedure(const v: PGLfloat); extdecl;
  glRasterPos2i: procedure(x, y: GLint); extdecl;
  glRasterPos2iv: procedure(const v: PGLint); extdecl;
  glRasterPos2s: procedure(x, y: GLshort); extdecl;
  glRasterPos2sv: procedure(const v: PGLshort); extdecl;
  glRasterPos3d: procedure(x, y, z: GLdouble); extdecl;
  glRasterPos3dv: procedure(const v: PGLdouble); extdecl;
  glRasterPos3f: procedure(x, y, z: GLfloat); extdecl;
  glRasterPos3fv: procedure(const v: PGLfloat); extdecl;
  glRasterPos3i: procedure(x, y, z: GLint); extdecl;
  glRasterPos3iv: procedure(const v: PGLint); extdecl;
  glRasterPos3s: procedure(x, y, z: GLshort); extdecl;
  glRasterPos3sv: procedure(const v: PGLshort); extdecl;
  glRasterPos4d: procedure(x, y, z, w: GLdouble); extdecl;
  glRasterPos4dv: procedure(const v: PGLdouble); extdecl;
  glRasterPos4f: procedure(x, y, z, w: GLfloat); extdecl;
  glRasterPos4fv: procedure(const v: PGLfloat); extdecl;
  glRasterPos4i: procedure(x, y, z, w: GLint); extdecl;
  glRasterPos4iv: procedure(const v: PGLint); extdecl;
  glRasterPos4s: procedure(x, y, z, w: GLshort); extdecl;
  glRasterPos4sv: procedure(const v: PGLshort); extdecl;
  glReadBuffer: procedure(mode: GLenum); extdecl;
  glReadPixels: procedure(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); extdecl;
  glRectd: procedure(x1, y1, x2, y2: GLdouble); extdecl;
  glRectdv: procedure(const v1: PGLdouble; const v2: PGLdouble); extdecl;
  glRectf: procedure(x1, y1, x2, y2: GLfloat); extdecl;
  glRectfv: procedure(const v1: PGLfloat; const v2: PGLfloat); extdecl;
  glRecti: procedure(x1, y1, x2, y2: GLint); extdecl;
  glRectiv: procedure(const v1: PGLint; const v2: PGLint); extdecl;
  glRects: procedure(x1, y1, x2, y2: GLshort); extdecl;
  glRectsv: procedure(const v1: PGLshort; const v2: PGLshort); extdecl;
  glRenderMode: function(mode: GLint): GLint; extdecl;
  glRotated: procedure(angle, x, y, z: GLdouble); extdecl;
  glRotatef: procedure(angle, x, y, z: GLfloat); extdecl;
  glScaled: procedure(x, y, z: GLdouble); extdecl;
  glScalef: procedure(x, y, z: GLfloat); extdecl;
  glScissor: procedure(x, y: GLint; width, height: GLsizei); extdecl;
  glSelectBuffer: procedure(size: GLsizei; buffer: PGLuint); extdecl;
  glShadeModel: procedure(mode: GLenum); extdecl;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); extdecl;
  glStencilMask: procedure(mask: GLuint); extdecl;
  glStencilOp: procedure(fail, zfail, zpass: GLenum); extdecl;
  glTexCoord1d: procedure(s: GLdouble); extdecl;
  glTexCoord1dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord1f: procedure(s: GLfloat); extdecl;
  glTexCoord1fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord1i: procedure(s: GLint); extdecl;
  glTexCoord1iv: procedure(const v: PGLint); extdecl;
  glTexCoord1s: procedure(s: GLshort); extdecl;
  glTexCoord1sv: procedure(const v: PGLshort); extdecl;
  glTexCoord2d: procedure(s, t: GLdouble); extdecl;
  glTexCoord2dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord2f: procedure(s, t: GLfloat); extdecl;
  glTexCoord2fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord2i: procedure(s, t: GLint); extdecl;
  glTexCoord2iv: procedure(const v: PGLint); extdecl;
  glTexCoord2s: procedure(s, t: GLshort); extdecl;
  glTexCoord2sv: procedure(const v: PGLshort); extdecl;
  glTexCoord3d: procedure(s, t, r: GLdouble); extdecl;
  glTexCoord3dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord3f: procedure(s, t, r: GLfloat); extdecl;
  glTexCoord3fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord3i: procedure(s, t, r: GLint); extdecl;
  glTexCoord3iv: procedure(const v: PGLint); extdecl;
  glTexCoord3s: procedure(s, t, r: GLshort); extdecl;
  glTexCoord3sv: procedure(const v: PGLshort); extdecl;
  glTexCoord4d: procedure(s, t, r, q: GLdouble); extdecl;
  glTexCoord4dv: procedure(const v: PGLdouble); extdecl;
  glTexCoord4f: procedure(s, t, r, q: GLfloat); extdecl;
  glTexCoord4fv: procedure(const v: PGLfloat); extdecl;
  glTexCoord4i: procedure(s, t, r, q: GLint); extdecl;
  glTexCoord4iv: procedure(const v: PGLint); extdecl;
  glTexCoord4s: procedure(s, t, r, q: GLshort); extdecl;
  glTexCoord4sv: procedure(const v: PGLshort); extdecl;
  glTexCoordPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glTexEnvf: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexEnvfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexEnvi: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexEnviv: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexGend: procedure(coord: GLenum; pname: GLenum; param: GLdouble); extdecl;
  glTexGendv: procedure(coord: GLenum; pname: GLenum; const params: PGLdouble); extdecl;
  glTexGenf: procedure(coord: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexGenfv: procedure(coord: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexGeni: procedure(coord: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexGeniv: procedure(coord: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexImage1D: procedure(target: GLenum; level, internalformat: GLint; width: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexImage2D: procedure(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); extdecl;
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); extdecl;
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); extdecl;
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); extdecl;
  glTexSubImage1D: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); extdecl;
  glTranslated: procedure(x, y, z: GLdouble); extdecl;
  glTranslatef: procedure(x, y, z: GLfloat); extdecl;
  glVertex2d: procedure(x, y: GLdouble); extdecl;
  glVertex2dv: procedure(const v: PGLdouble); extdecl;
  glVertex2f: procedure(x, y: GLfloat); extdecl;
  glVertex2fv: procedure(const v: PGLfloat); extdecl;
  glVertex2i: procedure(x, y: GLint); extdecl;
  glVertex2iv: procedure(const v: PGLint); extdecl;
  glVertex2s: procedure(x, y: GLshort); extdecl;
  glVertex2sv: procedure(const v: PGLshort); extdecl;
  glVertex3d: procedure(x, y, z: GLdouble); extdecl;
  glVertex3dv: procedure(const v: PGLdouble); extdecl;
  glVertex3f: procedure(x, y, z: GLfloat); extdecl;
  glVertex3fv: procedure(const v: PGLfloat); extdecl;
  glVertex3i: procedure(x, y, z: GLint); extdecl;
  glVertex3iv: procedure(const v: PGLint); extdecl;
  glVertex3s: procedure(x, y, z: GLshort); extdecl;
  glVertex3sv: procedure(const v: PGLshort); extdecl;
  glVertex4d: procedure(x, y, z, w: GLdouble); extdecl;
  glVertex4dv: procedure(const v: PGLdouble); extdecl;
  glVertex4f: procedure(x, y, z, w: GLfloat); extdecl;
  glVertex4fv: procedure(const v: PGLfloat); extdecl;
  glVertex4i: procedure(x, y, z, w: GLint); extdecl;
  glVertex4iv: procedure(const v: PGLint); extdecl;
  glVertex4s: procedure(x, y, z, w: GLshort); extdecl;
  glVertex4sv: procedure(const v: PGLshort); extdecl;
  glVertexPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glViewport: procedure(x, y: GLint; width, height: GLsizei); extdecl;
  {$IFDEF Win32}
  ChoosePixelFormat: function(DC: HDC; p2: PPixelFormatDescriptor): Integer; extdecl;
  {$ENDIF}

type
  // EXT_vertex_array
  PFNGLARRAYELEMENTEXTPROC = procedure(i: GLint); extdecl;
  PFNGLDRAWARRAYSEXTPROC = procedure(mode: GLenum; first: GLint; count: GLsizei); extdecl;
  PFNGLVERTEXPOINTEREXTPROC = procedure(size: GLint; atype: GLenum;
                                        stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLNORMALPOINTEREXTPROC = procedure(atype: GLenum; stride, count: GLsizei;
                                        const pointer: Pointer); extdecl;
  PFNGLCOLORPOINTEREXTPROC = procedure(size: GLint; atype: GLenum; stride, count: GLsizei;
                                       const pointer: Pointer); extdecl;
  PFNGLINDEXPOINTEREXTPROC = procedure(atype: GLenum; stride, count: GLsizei;
                                       const pointer: Pointer); extdecl;
  PFNGLTEXCOORDPOINTEREXTPROC = procedure(size: GLint; atype: GLenum;
                                          stride, count: GLsizei; const pointer: Pointer); extdecl;
  PFNGLEDGEFLAGPOINTEREXTPROC = procedure(stride, count: GLsizei;
                                          const pointer: PGLboolean); extdecl;
  PFNGLGETPOINTERVEXTPROC = procedure(pname: GLenum; params: Pointer); extdecl;
  PFNGLARRAYELEMENTARRAYEXTPROC = procedure(mode: GLenum; count: GLsizei;
                                            const pi: Pointer); extdecl;

  // WIN_swap_hint
  PFNGLADDSWAPHINTRECTWINPROC = procedure(x, y: GLint; width, height: GLsizei); extdecl;

  // EXT_paletted_texture
  PFNGLCOLORTABLEEXTPROC = procedure(target, internalFormat: GLenum; width: GLsizei;
                                     format, atype: GLenum; const data: Pointer); extdecl;
  PFNGLCOLORSUBTABLEEXTPROC = procedure(target: GLenum; start, count: GLsizei;
                                        format, atype: GLenum; const data: Pointer); extdecl;
  PFNGLGETCOLORTABLEEXTPROC = procedure(target, format, atype: GLenum; data: Pointer); extdecl;
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: GLenum; params: PGLint); extdecl;
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: GLenum; params: PGLfloat); extdecl;

procedure LoadOpenGL(const dll: String);
procedure FreeOpenGL;

implementation

{$ifdef win32}
function WinChoosePixelFormat(DC: HDC; p2: PPixelFormatDescriptor): Integer; extdecl; external 'gdi32' name 'ChoosePixelFormat';
{$endif}

procedure FreeOpenGL;
begin

  @glAccum := nil;
  @glAlphaFunc := nil;
  @glAreTexturesResident := nil;
  @glArrayElement := nil;
  @glBegin := nil;
  @glBindTexture := nil;
  @glBitmap := nil;
  @glBlendFunc := nil;
  @glCallList := nil;
  @glCallLists := nil;
  @glClear := nil;
  @glClearAccum := nil;
  @glClearColor := nil;
  @glClearDepth := nil;
  @glClearIndex := nil;
  @glClearStencil := nil;
  @glClipPlane := nil;
  @glColor3b := nil;
  @glColor3bv := nil;
  @glColor3d := nil;
  @glColor3dv := nil;
  @glColor3f := nil;
  @glColor3fv := nil;
  @glColor3i := nil;
  @glColor3iv := nil;
  @glColor3s := nil;
  @glColor3sv := nil;
  @glColor3ub := nil;
  @glColor3ubv := nil;
  @glColor3ui := nil;
  @glColor3uiv := nil;
  @glColor3us := nil;
  @glColor3usv := nil;
  @glColor4b := nil;
  @glColor4bv := nil;
  @glColor4d := nil;
  @glColor4dv := nil;
  @glColor4f := nil;
  @glColor4fv := nil;
  @glColor4i := nil;
  @glColor4iv := nil;
  @glColor4s := nil;
  @glColor4sv := nil;
  @glColor4ub := nil;
  @glColor4ubv := nil;
  @glColor4ui := nil;
  @glColor4uiv := nil;
  @glColor4us := nil;
  @glColor4usv := nil;
  @glColorMask := nil;
  @glColorMaterial := nil;
  @glColorPointer := nil;
  @glCopyPixels := nil;
  @glCopyTexImage1D := nil;
  @glCopyTexImage2D := nil;
  @glCopyTexSubImage1D := nil;
  @glCopyTexSubImage2D := nil;
  @glCullFace := nil;
  @glDeleteLists := nil;
  @glDeleteTextures := nil;
  @glDepthFunc := nil;
  @glDepthMask := nil;
  @glDepthRange := nil;
  @glDisable := nil;
  @glDisableClientState := nil;
  @glDrawArrays := nil;
  @glDrawBuffer := nil;
  @glDrawElements := nil;
  @glDrawPixels := nil;
  @glEdgeFlag := nil;
  @glEdgeFlagPointer := nil;
  @glEdgeFlagv := nil;
  @glEnable := nil;
  @glEnableClientState := nil;
  @glEnd := nil;
  @glEndList := nil;
  @glEvalCoord1d := nil;
  @glEvalCoord1dv := nil;
  @glEvalCoord1f := nil;
  @glEvalCoord1fv := nil;
  @glEvalCoord2d := nil;
  @glEvalCoord2dv := nil;
  @glEvalCoord2f := nil;
  @glEvalCoord2fv := nil;
  @glEvalMesh1 := nil;
  @glEvalMesh2 := nil;
  @glEvalPoint1 := nil;
  @glEvalPoint2 := nil;
  @glFeedbackBuffer := nil;
  @glFinish := nil;
  @glFlush := nil;
  @glFogf := nil;
  @glFogfv := nil;
  @glFogi := nil;
  @glFogiv := nil;
  @glFrontFace := nil;
  @glFrustum := nil;
  @glGenLists := nil;
  @glGenTextures := nil;
  @glGetBooleanv := nil;
  @glGetClipPlane := nil;
  @glGetDoublev := nil;
  @glGetError := nil;
  @glGetFloatv := nil;
  @glGetIntegerv := nil;
  @glGetLightfv := nil;
  @glGetLightiv := nil;
  @glGetMapdv := nil;
  @glGetMapfv := nil;
  @glGetMapiv := nil;
  @glGetMaterialfv := nil;
  @glGetMaterialiv := nil;
  @glGetPixelMapfv := nil;
  @glGetPixelMapuiv := nil;
  @glGetPixelMapusv := nil;
  @glGetPointerv := nil;
  @glGetPolygonStipple := nil;
  @glGetString := nil;
  @glGetTexEnvfv := nil;
  @glGetTexEnviv := nil;
  @glGetTexGendv := nil;
  @glGetTexGenfv := nil;
  @glGetTexGeniv := nil;
  @glGetTexImage := nil;
  @glGetTexLevelParameterfv := nil;
  @glGetTexLevelParameteriv := nil;
  @glGetTexParameterfv := nil;
  @glGetTexParameteriv := nil;
  @glHint := nil;
  @glIndexMask := nil;
  @glIndexPointer := nil;
  @glIndexd := nil;
  @glIndexdv := nil;
  @glIndexf := nil;
  @glIndexfv := nil;
  @glIndexi := nil;
  @glIndexiv := nil;
  @glIndexs := nil;
  @glIndexsv := nil;
  @glIndexub := nil;
  @glIndexubv := nil;
  @glInitNames := nil;
  @glInterleavedArrays := nil;
  @glIsEnabled := nil;
  @glIsList := nil;
  @glIsTexture := nil;
  @glLightModelf := nil;
  @glLightModelfv := nil;
  @glLightModeli := nil;
  @glLightModeliv := nil;
  @glLightf := nil;
  @glLightfv := nil;
  @glLighti := nil;
  @glLightiv := nil;
  @glLineStipple := nil;
  @glLineWidth := nil;
  @glListBase := nil;
  @glLoadIdentity := nil;
  @glLoadMatrixd := nil;
  @glLoadMatrixf := nil;
  @glLoadName := nil;
  @glLogicOp := nil;
  @glMap1d := nil;
  @glMap1f := nil;
  @glMap2d := nil;
  @glMap2f := nil;
  @glMapGrid1d := nil;
  @glMapGrid1f := nil;
  @glMapGrid2d := nil;
  @glMapGrid2f := nil;
  @glMaterialf := nil;
  @glMaterialfv := nil;
  @glMateriali := nil;
  @glMaterialiv := nil;
  @glMatrixMode := nil;
  @glMultMatrixd := nil;
  @glMultMatrixf := nil;
  @glNewList := nil;
  @glNormal3b := nil;
  @glNormal3bv := nil;
  @glNormal3d := nil;
  @glNormal3dv := nil;
  @glNormal3f := nil;
  @glNormal3fv := nil;
  @glNormal3i := nil;
  @glNormal3iv := nil;
  @glNormal3s := nil;
  @glNormal3sv := nil;
  @glNormalPointer := nil;
  @glOrtho := nil;
  @glPassThrough := nil;
  @glPixelMapfv := nil;
  @glPixelMapuiv := nil;
  @glPixelMapusv := nil;
  @glPixelStoref := nil;
  @glPixelStorei := nil;
  @glPixelTransferf := nil;
  @glPixelTransferi := nil;
  @glPixelZoom := nil;
  @glPointSize := nil;
  @glPolygonMode := nil;
  @glPolygonOffset := nil;
  @glPolygonStipple := nil;
  @glPopAttrib := nil;
  @glPopClientAttrib := nil;
  @glPopMatrix := nil;
  @glPopName := nil;
  @glPrioritizeTextures := nil;
  @glPushAttrib := nil;
  @glPushClientAttrib := nil;
  @glPushMatrix := nil;
  @glPushName := nil;
  @glRasterPos2d := nil;
  @glRasterPos2dv := nil;
  @glRasterPos2f := nil;
  @glRasterPos2fv := nil;
  @glRasterPos2i := nil;
  @glRasterPos2iv := nil;
  @glRasterPos2s := nil;
  @glRasterPos2sv := nil;
  @glRasterPos3d := nil;
  @glRasterPos3dv := nil;
  @glRasterPos3f := nil;
  @glRasterPos3fv := nil;
  @glRasterPos3i := nil;
  @glRasterPos3iv := nil;
  @glRasterPos3s := nil;
  @glRasterPos3sv := nil;
  @glRasterPos4d := nil;
  @glRasterPos4dv := nil;
  @glRasterPos4f := nil;
  @glRasterPos4fv := nil;
  @glRasterPos4i := nil;
  @glRasterPos4iv := nil;
  @glRasterPos4s := nil;
  @glRasterPos4sv := nil;
  @glReadBuffer := nil;
  @glReadPixels := nil;
  @glRectd := nil;
  @glRectdv := nil;
  @glRectf := nil;
  @glRectfv := nil;
  @glRecti := nil;
  @glRectiv := nil;
  @glRects := nil;
  @glRectsv := nil;
  @glRenderMode := nil;
  @glRotated := nil;
  @glRotatef := nil;
  @glScaled := nil;
  @glScalef := nil;
  @glScissor := nil;
  @glSelectBuffer := nil;
  @glShadeModel := nil;
  @glStencilFunc := nil;
  @glStencilMask := nil;
  @glStencilOp := nil;
  @glTexCoord1d := nil;
  @glTexCoord1dv := nil;
  @glTexCoord1f := nil;
  @glTexCoord1fv := nil;
  @glTexCoord1i := nil;
  @glTexCoord1iv := nil;
  @glTexCoord1s := nil;
  @glTexCoord1sv := nil;
  @glTexCoord2d := nil;
  @glTexCoord2dv := nil;
  @glTexCoord2f := nil;
  @glTexCoord2fv := nil;
  @glTexCoord2i := nil;
  @glTexCoord2iv := nil;
  @glTexCoord2s := nil;
  @glTexCoord2sv := nil;
  @glTexCoord3d := nil;
  @glTexCoord3dv := nil;
  @glTexCoord3f := nil;
  @glTexCoord3fv := nil;
  @glTexCoord3i := nil;
  @glTexCoord3iv := nil;
  @glTexCoord3s := nil;
  @glTexCoord3sv := nil;
  @glTexCoord4d := nil;
  @glTexCoord4dv := nil;
  @glTexCoord4f := nil;
  @glTexCoord4fv := nil;
  @glTexCoord4i := nil;
  @glTexCoord4iv := nil;
  @glTexCoord4s := nil;
  @glTexCoord4sv := nil;
  @glTexCoordPointer := nil;
  @glTexEnvf := nil;
  @glTexEnvfv := nil;
  @glTexEnvi := nil;
  @glTexEnviv := nil;
  @glTexGend := nil;
  @glTexGendv := nil;
  @glTexGenf := nil;
  @glTexGenfv := nil;
  @glTexGeni := nil;
  @glTexGeniv := nil;
  @glTexImage1D := nil;
  @glTexImage2D := nil;
  @glTexParameterf := nil;
  @glTexParameterfv := nil;
  @glTexParameteri := nil;
  @glTexParameteriv := nil;
  @glTexSubImage1D := nil;
  @glTexSubImage2D := nil;
  @glTranslated := nil;
  @glTranslatef := nil;
  @glVertex2d := nil;
  @glVertex2dv := nil;
  @glVertex2f := nil;
  @glVertex2fv := nil;
  @glVertex2i := nil;
  @glVertex2iv := nil;
  @glVertex2s := nil;
  @glVertex2sv := nil;
  @glVertex3d := nil;
  @glVertex3dv := nil;
  @glVertex3f := nil;
  @glVertex3fv := nil;
  @glVertex3i := nil;
  @glVertex3iv := nil;
  @glVertex3s := nil;
  @glVertex3sv := nil;
  @glVertex4d := nil;
  @glVertex4dv := nil;
  @glVertex4f := nil;
  @glVertex4fv := nil;
  @glVertex4i := nil;
  @glVertex4iv := nil;
  @glVertex4s := nil;
  @glVertex4sv := nil;
  @glVertexPointer := nil;
  @glViewport := nil;
  {$IFDEF Win32}
  @ChoosePixelFormat := nil;
  {$ENDIF}

  FreeLibrary(LibGL);

end;

procedure LoadOpenGL(const dll: String);
begin

  FreeOpenGL;

  LibGL := LoadLibrary(PChar(dll));
  if LibGL = 0 then raise Exception.Create('Could not load OpenGL from ' + dll);

  @glAccum := GetProcAddress(LibGL, 'glAccum');
  @glAlphaFunc := GetProcAddress(LibGL, 'glAlphaFunc');
  @glAreTexturesResident := GetProcAddress(LibGL, 'glAreTexturesResident');
  @glArrayElement := GetProcAddress(LibGL, 'glArrayElement');
  @glBegin := GetProcAddress(LibGL, 'glBegin');
  @glBindTexture := GetProcAddress(LibGL, 'glBindTexture');
  @glBitmap := GetProcAddress(LibGL, 'glBitmap');
  @glBlendFunc := GetProcAddress(LibGL, 'glBlendFunc');
  @glCallList := GetProcAddress(LibGL, 'glCallList');
  @glCallLists := GetProcAddress(LibGL, 'glCallLists');
  @glClear := GetProcAddress(LibGL, 'glClear');
  @glClearAccum := GetProcAddress(LibGL, 'glClearAccum');
  @glClearColor := GetProcAddress(LibGL, 'glClearColor');
  @glClearDepth := GetProcAddress(LibGL, 'glClearDepth');
  @glClearIndex := GetProcAddress(LibGL, 'glClearIndex');
  @glClearStencil := GetProcAddress(LibGL, 'glClearStencil');
  @glClipPlane := GetProcAddress(LibGL, 'glClipPlane');
  @glColor3b := GetProcAddress(LibGL, 'glColor3b');
  @glColor3bv := GetProcAddress(LibGL, 'glColor3bv');
  @glColor3d := GetProcAddress(LibGL, 'glColor3d');
  @glColor3dv := GetProcAddress(LibGL, 'glColor3dv');
  @glColor3f := GetProcAddress(LibGL, 'glColor3f');
  @glColor3fv := GetProcAddress(LibGL, 'glColor3fv');
  @glColor3i := GetProcAddress(LibGL, 'glColor3i');
  @glColor3iv := GetProcAddress(LibGL, 'glColor3iv');
  @glColor3s := GetProcAddress(LibGL, 'glColor3s');
  @glColor3sv := GetProcAddress(LibGL, 'glColor3sv');
  @glColor3ub := GetProcAddress(LibGL, 'glColor3ub');
  @glColor3ubv := GetProcAddress(LibGL, 'glColor3ubv');
  @glColor3ui := GetProcAddress(LibGL, 'glColor3ui');
  @glColor3uiv := GetProcAddress(LibGL, 'glColor3uiv');
  @glColor3us := GetProcAddress(LibGL, 'glColor3us');
  @glColor3usv := GetProcAddress(LibGL, 'glColor3usv');
  @glColor4b := GetProcAddress(LibGL, 'glColor4b');
  @glColor4bv := GetProcAddress(LibGL, 'glColor4bv');
  @glColor4d := GetProcAddress(LibGL, 'glColor4d');
  @glColor4dv := GetProcAddress(LibGL, 'glColor4dv');
  @glColor4f := GetProcAddress(LibGL, 'glColor4f');
  @glColor4fv := GetProcAddress(LibGL, 'glColor4fv');
  @glColor4i := GetProcAddress(LibGL, 'glColor4i');
  @glColor4iv := GetProcAddress(LibGL, 'glColor4iv');
  @glColor4s := GetProcAddress(LibGL, 'glColor4s');
  @glColor4sv := GetProcAddress(LibGL, 'glColor4sv');
  @glColor4ub := GetProcAddress(LibGL, 'glColor4ub');
  @glColor4ubv := GetProcAddress(LibGL, 'glColor4ubv');
  @glColor4ui := GetProcAddress(LibGL, 'glColor4ui');
  @glColor4uiv := GetProcAddress(LibGL, 'glColor4uiv');
  @glColor4us := GetProcAddress(LibGL, 'glColor4us');
  @glColor4usv := GetProcAddress(LibGL, 'glColor4usv');
  @glColorMask := GetProcAddress(LibGL, 'glColorMask');
  @glColorMaterial := GetProcAddress(LibGL, 'glColorMaterial');
  @glColorPointer := GetProcAddress(LibGL, 'glColorPointer');
  @glCopyPixels := GetProcAddress(LibGL, 'glCopyPixels');
  @glCopyTexImage1D := GetProcAddress(LibGL, 'glCopyTexImage1D');
  @glCopyTexImage2D := GetProcAddress(LibGL, 'glCopyTexImage2D');
  @glCopyTexSubImage1D := GetProcAddress(LibGL, 'glCopyTexSubImage1D');
  @glCopyTexSubImage2D := GetProcAddress(LibGL, 'glCopyTexSubImage2D');
  @glCullFace := GetProcAddress(LibGL, 'glCullFace');
  @glDeleteLists := GetProcAddress(LibGL, 'glDeleteLists');
  @glDeleteTextures := GetProcAddress(LibGL, 'glDeleteTextures');
  @glDepthFunc := GetProcAddress(LibGL, 'glDepthFunc');
  @glDepthMask := GetProcAddress(LibGL, 'glDepthMask');
  @glDepthRange := GetProcAddress(LibGL, 'glDepthRange');
  @glDisable := GetProcAddress(LibGL, 'glDisable');
  @glDisableClientState := GetProcAddress(LibGL, 'glDisableClientState');
  @glDrawArrays := GetProcAddress(LibGL, 'glDrawArrays');
  @glDrawBuffer := GetProcAddress(LibGL, 'glDrawBuffer');
  @glDrawElements := GetProcAddress(LibGL, 'glDrawElements');
  @glDrawPixels := GetProcAddress(LibGL, 'glDrawPixels');
  @glEdgeFlag := GetProcAddress(LibGL, 'glEdgeFlag');
  @glEdgeFlagPointer := GetProcAddress(LibGL, 'glEdgeFlagPointer');
  @glEdgeFlagv := GetProcAddress(LibGL, 'glEdgeFlagv');
  @glEnable := GetProcAddress(LibGL, 'glEnable');
  @glEnableClientState := GetProcAddress(LibGL, 'glEnableClientState');
  @glEnd := GetProcAddress(LibGL, 'glEnd');
  @glEndList := GetProcAddress(LibGL, 'glEndList');
  @glEvalCoord1d := GetProcAddress(LibGL, 'glEvalCoord1d');
  @glEvalCoord1dv := GetProcAddress(LibGL, 'glEvalCoord1dv');
  @glEvalCoord1f := GetProcAddress(LibGL, 'glEvalCoord1f');
  @glEvalCoord1fv := GetProcAddress(LibGL, 'glEvalCoord1fv');
  @glEvalCoord2d := GetProcAddress(LibGL, 'glEvalCoord2d');
  @glEvalCoord2dv := GetProcAddress(LibGL, 'glEvalCoord2dv');
  @glEvalCoord2f := GetProcAddress(LibGL, 'glEvalCoord2f');
  @glEvalCoord2fv := GetProcAddress(LibGL, 'glEvalCoord2fv');
  @glEvalMesh1 := GetProcAddress(LibGL, 'glEvalMesh1');
  @glEvalMesh2 := GetProcAddress(LibGL, 'glEvalMesh2');
  @glEvalPoint1 := GetProcAddress(LibGL, 'glEvalPoint1');
  @glEvalPoint2 := GetProcAddress(LibGL, 'glEvalPoint2');
  @glFeedbackBuffer := GetProcAddress(LibGL, 'glFeedbackBuffer');
  @glFinish := GetProcAddress(LibGL, 'glFinish');
  @glFlush := GetProcAddress(LibGL, 'glFlush');
  @glFogf := GetProcAddress(LibGL, 'glFogf');
  @glFogfv := GetProcAddress(LibGL, 'glFogfv');
  @glFogi := GetProcAddress(LibGL, 'glFogi');
  @glFogiv := GetProcAddress(LibGL, 'glFogiv');
  @glFrontFace := GetProcAddress(LibGL, 'glFrontFace');
  @glFrustum := GetProcAddress(LibGL, 'glFrustum');
  @glGenLists := GetProcAddress(LibGL, 'glGenLists');
  @glGenTextures := GetProcAddress(LibGL, 'glGenTextures');
  @glGetBooleanv := GetProcAddress(LibGL, 'glGetBooleanv');
  @glGetClipPlane := GetProcAddress(LibGL, 'glGetClipPlane');
  @glGetDoublev := GetProcAddress(LibGL, 'glGetDoublev');
  @glGetError := GetProcAddress(LibGL, 'glGetError');
  @glGetFloatv := GetProcAddress(LibGL, 'glGetFloatv');
  @glGetIntegerv := GetProcAddress(LibGL, 'glGetIntegerv');
  @glGetLightfv := GetProcAddress(LibGL, 'glGetLightfv');
  @glGetLightiv := GetProcAddress(LibGL, 'glGetLightiv');
  @glGetMapdv := GetProcAddress(LibGL, 'glGetMapdv');
  @glGetMapfv := GetProcAddress(LibGL, 'glGetMapfv');
  @glGetMapiv := GetProcAddress(LibGL, 'glGetMapiv');
  @glGetMaterialfv := GetProcAddress(LibGL, 'glGetMaterialfv');
  @glGetMaterialiv := GetProcAddress(LibGL, 'glGetMaterialiv');
  @glGetPixelMapfv := GetProcAddress(LibGL, 'glGetPixelMapfv');
  @glGetPixelMapuiv := GetProcAddress(LibGL, 'glGetPixelMapuiv');
  @glGetPixelMapusv := GetProcAddress(LibGL, 'glGetPixelMapusv');
  @glGetPointerv := GetProcAddress(LibGL, 'glGetPointerv');
  @glGetPolygonStipple := GetProcAddress(LibGL, 'glGetPolygonStipple');
  @glGetString := GetProcAddress(LibGL, 'glGetString');
  @glGetTexEnvfv := GetProcAddress(LibGL, 'glGetTexEnvfv');
  @glGetTexEnviv := GetProcAddress(LibGL, 'glGetTexEnviv');
  @glGetTexGendv := GetProcAddress(LibGL, 'glGetTexGendv');
  @glGetTexGenfv := GetProcAddress(LibGL, 'glGetTexGenfv');
  @glGetTexGeniv := GetProcAddress(LibGL, 'glGetTexGeniv');
  @glGetTexImage := GetProcAddress(LibGL, 'glGetTexImage');
  @glGetTexLevelParameterfv := GetProcAddress(LibGL, 'glGetTexLevelParameterfv');
  @glGetTexLevelParameteriv := GetProcAddress(LibGL, 'glGetTexLevelParameteriv');
  @glGetTexParameterfv := GetProcAddress(LibGL, 'glGetTexParameterfv');
  @glGetTexParameteriv := GetProcAddress(LibGL, 'glGetTexParameteriv');
  @glHint := GetProcAddress(LibGL, 'glHint');
  @glIndexMask := GetProcAddress(LibGL, 'glIndexMask');
  @glIndexPointer := GetProcAddress(LibGL, 'glIndexPointer');
  @glIndexd := GetProcAddress(LibGL, 'glIndexd');
  @glIndexdv := GetProcAddress(LibGL, 'glIndexdv');
  @glIndexf := GetProcAddress(LibGL, 'glIndexf');
  @glIndexfv := GetProcAddress(LibGL, 'glIndexfv');
  @glIndexi := GetProcAddress(LibGL, 'glIndexi');
  @glIndexiv := GetProcAddress(LibGL, 'glIndexiv');
  @glIndexs := GetProcAddress(LibGL, 'glIndexs');
  @glIndexsv := GetProcAddress(LibGL, 'glIndexsv');
  @glIndexub := GetProcAddress(LibGL, 'glIndexub');
  @glIndexubv := GetProcAddress(LibGL, 'glIndexubv');
  @glInitNames := GetProcAddress(LibGL, 'glInitNames');
  @glInterleavedArrays := GetProcAddress(LibGL, 'glInterleavedArrays');
  @glIsEnabled := GetProcAddress(LibGL, 'glIsEnabled');
  @glIsList := GetProcAddress(LibGL, 'glIsList');
  @glIsTexture := GetProcAddress(LibGL, 'glIsTexture');
  @glLightModelf := GetProcAddress(LibGL, 'glLightModelf');
  @glLightModelfv := GetProcAddress(LibGL, 'glLightModelfv');
  @glLightModeli := GetProcAddress(LibGL, 'glLightModeli');
  @glLightModeliv := GetProcAddress(LibGL, 'glLightModeliv');
  @glLightf := GetProcAddress(LibGL, 'glLightf');
  @glLightfv := GetProcAddress(LibGL, 'glLightfv');
  @glLighti := GetProcAddress(LibGL, 'glLighti');
  @glLightiv := GetProcAddress(LibGL, 'glLightiv');
  @glLineStipple := GetProcAddress(LibGL, 'glLineStipple');
  @glLineWidth := GetProcAddress(LibGL, 'glLineWidth');
  @glListBase := GetProcAddress(LibGL, 'glListBase');
  @glLoadIdentity := GetProcAddress(LibGL, 'glLoadIdentity');
  @glLoadMatrixd := GetProcAddress(LibGL, 'glLoadMatrixd');
  @glLoadMatrixf := GetProcAddress(LibGL, 'glLoadMatrixf');
  @glLoadName := GetProcAddress(LibGL, 'glLoadName');
  @glLogicOp := GetProcAddress(LibGL, 'glLogicOp');
  @glMap1d := GetProcAddress(LibGL, 'glMap1d');
  @glMap1f := GetProcAddress(LibGL, 'glMap1f');
  @glMap2d := GetProcAddress(LibGL, 'glMap2d');
  @glMap2f := GetProcAddress(LibGL, 'glMap2f');
  @glMapGrid1d := GetProcAddress(LibGL, 'glMapGrid1d');
  @glMapGrid1f := GetProcAddress(LibGL, 'glMapGrid1f');
  @glMapGrid2d := GetProcAddress(LibGL, 'glMapGrid2d');
  @glMapGrid2f := GetProcAddress(LibGL, 'glMapGrid2f');
  @glMaterialf := GetProcAddress(LibGL, 'glMaterialf');
  @glMaterialfv := GetProcAddress(LibGL, 'glMaterialfv');
  @glMateriali := GetProcAddress(LibGL, 'glMateriali');
  @glMaterialiv := GetProcAddress(LibGL, 'glMaterialiv');
  @glMatrixMode := GetProcAddress(LibGL, 'glMatrixMode');
  @glMultMatrixd := GetProcAddress(LibGL, 'glMultMatrixd');
  @glMultMatrixf := GetProcAddress(LibGL, 'glMultMatrixf');
  @glNewList := GetProcAddress(LibGL, 'glNewList');
  @glNormal3b := GetProcAddress(LibGL, 'glNormal3b');
  @glNormal3bv := GetProcAddress(LibGL, 'glNormal3bv');
  @glNormal3d := GetProcAddress(LibGL, 'glNormal3d');
  @glNormal3dv := GetProcAddress(LibGL, 'glNormal3dv');
  @glNormal3f := GetProcAddress(LibGL, 'glNormal3f');
  @glNormal3fv := GetProcAddress(LibGL, 'glNormal3fv');
  @glNormal3i := GetProcAddress(LibGL, 'glNormal3i');
  @glNormal3iv := GetProcAddress(LibGL, 'glNormal3iv');
  @glNormal3s := GetProcAddress(LibGL, 'glNormal3s');
  @glNormal3sv := GetProcAddress(LibGL, 'glNormal3sv');
  @glNormalPointer := GetProcAddress(LibGL, 'glNormalPointer');
  @glOrtho := GetProcAddress(LibGL, 'glOrtho');
  @glPassThrough := GetProcAddress(LibGL, 'glPassThrough');
  @glPixelMapfv := GetProcAddress(LibGL, 'glPixelMapfv');
  @glPixelMapuiv := GetProcAddress(LibGL, 'glPixelMapuiv');
  @glPixelMapusv := GetProcAddress(LibGL, 'glPixelMapusv');
  @glPixelStoref := GetProcAddress(LibGL, 'glPixelStoref');
  @glPixelStorei := GetProcAddress(LibGL, 'glPixelStorei');
  @glPixelTransferf := GetProcAddress(LibGL, 'glPixelTransferf');
  @glPixelTransferi := GetProcAddress(LibGL, 'glPixelTransferi');
  @glPixelZoom := GetProcAddress(LibGL, 'glPixelZoom');
  @glPointSize := GetProcAddress(LibGL, 'glPointSize');
  @glPolygonMode := GetProcAddress(LibGL, 'glPolygonMode');
  @glPolygonOffset := GetProcAddress(LibGL, 'glPolygonOffset');
  @glPolygonStipple := GetProcAddress(LibGL, 'glPolygonStipple');
  @glPopAttrib := GetProcAddress(LibGL, 'glPopAttrib');
  @glPopClientAttrib := GetProcAddress(LibGL, 'glPopClientAttrib');
  @glPopMatrix := GetProcAddress(LibGL, 'glPopMatrix');
  @glPopName := GetProcAddress(LibGL, 'glPopName');
  @glPrioritizeTextures := GetProcAddress(LibGL, 'glPrioritizeTextures');
  @glPushAttrib := GetProcAddress(LibGL, 'glPushAttrib');
  @glPushClientAttrib := GetProcAddress(LibGL, 'glPushClientAttrib');
  @glPushMatrix := GetProcAddress(LibGL, 'glPushMatrix');
  @glPushName := GetProcAddress(LibGL, 'glPushName');
  @glRasterPos2d := GetProcAddress(LibGL, 'glRasterPos2d');
  @glRasterPos2dv := GetProcAddress(LibGL, 'glRasterPos2dv');
  @glRasterPos2f := GetProcAddress(LibGL, 'glRasterPos2f');
  @glRasterPos2fv := GetProcAddress(LibGL, 'glRasterPos2fv');
  @glRasterPos2i := GetProcAddress(LibGL, 'glRasterPos2i');
  @glRasterPos2iv := GetProcAddress(LibGL, 'glRasterPos2iv');
  @glRasterPos2s := GetProcAddress(LibGL, 'glRasterPos2s');
  @glRasterPos2sv := GetProcAddress(LibGL, 'glRasterPos2sv');
  @glRasterPos3d := GetProcAddress(LibGL, 'glRasterPos3d');
  @glRasterPos3dv := GetProcAddress(LibGL, 'glRasterPos3dv');
  @glRasterPos3f := GetProcAddress(LibGL, 'glRasterPos3f');
  @glRasterPos3fv := GetProcAddress(LibGL, 'glRasterPos3fv');
  @glRasterPos3i := GetProcAddress(LibGL, 'glRasterPos3i');
  @glRasterPos3iv := GetProcAddress(LibGL, 'glRasterPos3iv');
  @glRasterPos3s := GetProcAddress(LibGL, 'glRasterPos3s');
  @glRasterPos3sv := GetProcAddress(LibGL, 'glRasterPos3sv');
  @glRasterPos4d := GetProcAddress(LibGL, 'glRasterPos4d');
  @glRasterPos4dv := GetProcAddress(LibGL, 'glRasterPos4dv');
  @glRasterPos4f := GetProcAddress(LibGL, 'glRasterPos4f');
  @glRasterPos4fv := GetProcAddress(LibGL, 'glRasterPos4fv');
  @glRasterPos4i := GetProcAddress(LibGL, 'glRasterPos4i');
  @glRasterPos4iv := GetProcAddress(LibGL, 'glRasterPos4iv');
  @glRasterPos4s := GetProcAddress(LibGL, 'glRasterPos4s');
  @glRasterPos4sv := GetProcAddress(LibGL, 'glRasterPos4sv');
  @glReadBuffer := GetProcAddress(LibGL, 'glReadBuffer');
  @glReadPixels := GetProcAddress(LibGL, 'glReadPixels');
  @glRectd := GetProcAddress(LibGL, 'glRectd');
  @glRectdv := GetProcAddress(LibGL, 'glRectdv');
  @glRectf := GetProcAddress(LibGL, 'glRectf');
  @glRectfv := GetProcAddress(LibGL, 'glRectfv');
  @glRecti := GetProcAddress(LibGL, 'glRecti');
  @glRectiv := GetProcAddress(LibGL, 'glRectiv');
  @glRects := GetProcAddress(LibGL, 'glRects');
  @glRectsv := GetProcAddress(LibGL, 'glRectsv');
  @glRenderMode := GetProcAddress(LibGL, 'glRenderMode');
  @glRotated := GetProcAddress(LibGL, 'glRotated');
  @glRotatef := GetProcAddress(LibGL, 'glRotatef');
  @glScaled := GetProcAddress(LibGL, 'glScaled');
  @glScalef := GetProcAddress(LibGL, 'glScalef');
  @glScissor := GetProcAddress(LibGL, 'glScissor');
  @glSelectBuffer := GetProcAddress(LibGL, 'glSelectBuffer');
  @glShadeModel := GetProcAddress(LibGL, 'glShadeModel');
  @glStencilFunc := GetProcAddress(LibGL, 'glStencilFunc');
  @glStencilMask := GetProcAddress(LibGL, 'glStencilMask');
  @glStencilOp := GetProcAddress(LibGL, 'glStencilOp');
  @glTexCoord1d := GetProcAddress(LibGL, 'glTexCoord1d');
  @glTexCoord1dv := GetProcAddress(LibGL, 'glTexCoord1dv');
  @glTexCoord1f := GetProcAddress(LibGL, 'glTexCoord1f');
  @glTexCoord1fv := GetProcAddress(LibGL, 'glTexCoord1fv');
  @glTexCoord1i := GetProcAddress(LibGL, 'glTexCoord1i');
  @glTexCoord1iv := GetProcAddress(LibGL, 'glTexCoord1iv');
  @glTexCoord1s := GetProcAddress(LibGL, 'glTexCoord1s');
  @glTexCoord1sv := GetProcAddress(LibGL, 'glTexCoord1sv');
  @glTexCoord2d := GetProcAddress(LibGL, 'glTexCoord2d');
  @glTexCoord2dv := GetProcAddress(LibGL, 'glTexCoord2dv');
  @glTexCoord2f := GetProcAddress(LibGL, 'glTexCoord2f');
  @glTexCoord2fv := GetProcAddress(LibGL, 'glTexCoord2fv');
  @glTexCoord2i := GetProcAddress(LibGL, 'glTexCoord2i');
  @glTexCoord2iv := GetProcAddress(LibGL, 'glTexCoord2iv');
  @glTexCoord2s := GetProcAddress(LibGL, 'glTexCoord2s');
  @glTexCoord2sv := GetProcAddress(LibGL, 'glTexCoord2sv');
  @glTexCoord3d := GetProcAddress(LibGL, 'glTexCoord3d');
  @glTexCoord3dv := GetProcAddress(LibGL, 'glTexCoord3dv');
  @glTexCoord3f := GetProcAddress(LibGL, 'glTexCoord3f');
  @glTexCoord3fv := GetProcAddress(LibGL, 'glTexCoord3fv');
  @glTexCoord3i := GetProcAddress(LibGL, 'glTexCoord3i');
  @glTexCoord3iv := GetProcAddress(LibGL, 'glTexCoord3iv');
  @glTexCoord3s := GetProcAddress(LibGL, 'glTexCoord3s');
  @glTexCoord3sv := GetProcAddress(LibGL, 'glTexCoord3sv');
  @glTexCoord4d := GetProcAddress(LibGL, 'glTexCoord4d');
  @glTexCoord4dv := GetProcAddress(LibGL, 'glTexCoord4dv');
  @glTexCoord4f := GetProcAddress(LibGL, 'glTexCoord4f');
  @glTexCoord4fv := GetProcAddress(LibGL, 'glTexCoord4fv');
  @glTexCoord4i := GetProcAddress(LibGL, 'glTexCoord4i');
  @glTexCoord4iv := GetProcAddress(LibGL, 'glTexCoord4iv');
  @glTexCoord4s := GetProcAddress(LibGL, 'glTexCoord4s');
  @glTexCoord4sv := GetProcAddress(LibGL, 'glTexCoord4sv');
  @glTexCoordPointer := GetProcAddress(LibGL, 'glTexCoordPointer');
  @glTexEnvf := GetProcAddress(LibGL, 'glTexEnvf');
  @glTexEnvfv := GetProcAddress(LibGL, 'glTexEnvfv');
  @glTexEnvi := GetProcAddress(LibGL, 'glTexEnvi');
  @glTexEnviv := GetProcAddress(LibGL, 'glTexEnviv');
  @glTexGend := GetProcAddress(LibGL, 'glTexGend');
  @glTexGendv := GetProcAddress(LibGL, 'glTexGendv');
  @glTexGenf := GetProcAddress(LibGL, 'glTexGenf');
  @glTexGenfv := GetProcAddress(LibGL, 'glTexGenfv');
  @glTexGeni := GetProcAddress(LibGL, 'glTexGeni');
  @glTexGeniv := GetProcAddress(LibGL, 'glTexGeniv');
  @glTexImage1D := GetProcAddress(LibGL, 'glTexImage1D');
  @glTexImage2D := GetProcAddress(LibGL, 'glTexImage2D');
  @glTexParameterf := GetProcAddress(LibGL, 'glTexParameterf');
  @glTexParameterfv := GetProcAddress(LibGL, 'glTexParameterfv');
  @glTexParameteri := GetProcAddress(LibGL, 'glTexParameteri');
  @glTexParameteriv := GetProcAddress(LibGL, 'glTexParameteriv');
  @glTexSubImage1D := GetProcAddress(LibGL, 'glTexSubImage1D');
  @glTexSubImage2D := GetProcAddress(LibGL, 'glTexSubImage2D');
  @glTranslated := GetProcAddress(LibGL, 'glTranslated');
  @glTranslatef := GetProcAddress(LibGL, 'glTranslatef');
  @glVertex2d := GetProcAddress(LibGL, 'glVertex2d');
  @glVertex2dv := GetProcAddress(LibGL, 'glVertex2dv');
  @glVertex2f := GetProcAddress(LibGL, 'glVertex2f');
  @glVertex2fv := GetProcAddress(LibGL, 'glVertex2fv');
  @glVertex2i := GetProcAddress(LibGL, 'glVertex2i');
  @glVertex2iv := GetProcAddress(LibGL, 'glVertex2iv');
  @glVertex2s := GetProcAddress(LibGL, 'glVertex2s');
  @glVertex2sv := GetProcAddress(LibGL, 'glVertex2sv');
  @glVertex3d := GetProcAddress(LibGL, 'glVertex3d');
  @glVertex3dv := GetProcAddress(LibGL, 'glVertex3dv');
  @glVertex3f := GetProcAddress(LibGL, 'glVertex3f');
  @glVertex3fv := GetProcAddress(LibGL, 'glVertex3fv');
  @glVertex3i := GetProcAddress(LibGL, 'glVertex3i');
  @glVertex3iv := GetProcAddress(LibGL, 'glVertex3iv');
  @glVertex3s := GetProcAddress(LibGL, 'glVertex3s');
  @glVertex3sv := GetProcAddress(LibGL, 'glVertex3sv');
  @glVertex4d := GetProcAddress(LibGL, 'glVertex4d');
  @glVertex4dv := GetProcAddress(LibGL, 'glVertex4dv');
  @glVertex4f := GetProcAddress(LibGL, 'glVertex4f');
  @glVertex4fv := GetProcAddress(LibGL, 'glVertex4fv');
  @glVertex4i := GetProcAddress(LibGL, 'glVertex4i');
  @glVertex4iv := GetProcAddress(LibGL, 'glVertex4iv');
  @glVertex4s := GetProcAddress(LibGL, 'glVertex4s');
  @glVertex4sv := GetProcAddress(LibGL, 'glVertex4sv');
  @glVertexPointer := GetProcAddress(LibGL, 'glVertexPointer');
  @glViewport := GetProcAddress(LibGL, 'glViewport');

  {$IFDEF Win32}
  @ChoosePixelFormat := GetProcAddress(LibGL, 'ChoosePixelFormat');
  if not Assigned(ChoosePixelFormat) then
    @ChoosePixelFormat := @WinChoosePixelFormat;
  {$ENDIF}

end;

initialization

  {$IFNDEF FPC}
  Set8087CW($133F);
  {$ENDIF}

  try
    {$IFDEF Win32}
    LoadOpenGL('opengl32.dll');
    {$ELSE}
    {$ifdef darwin}
    LoadOpenGL('/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib');
    {$ELSE}
    LoadOpenGL('libGL.so.1');
    {$endif}
    {$ENDIF}
  except
    writeln('Error opening OpenGL library');
    halt(1);
  end;

finalization

  FreeOpenGL;

end.
