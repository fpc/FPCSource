{
  $Id$

  Translation of the Mesa GL headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther


  Mesa 3-D graphics library
  Version:  3.0
  Copyright (C) 1995-1998  Brian Paul

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$MODE delphi}  // objfpc would not work because of direct proc var assignments

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

unit GL;

interface

{$DEFINE GL1_0}
{x$DEFINE GL1_1}
{x$DEFINE GL1_2}
{x$DEFINE MESA}                 {enable if you want to use some special mesa extensions}
{x$DEFINE EXTENSIONS}           {enable if you need one/all of extensions}
{x$DEFINE SGI_EXTENSIONS}       {enable if you need one/all of extensions}

{$MACRO ON}

{$IFDEF Win32}
  {$DEFINE ogl_dll := }
  uses Windows;
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}


// =======================================================
//   Unit specific extensions
// =======================================================

function InitGLFromLibrary(libname: PChar): Boolean;

// determines automatically which libraries to use:
function InitGL: Boolean;


var
  GLDumpUnresolvedFunctions,
  GLInitialized: Boolean;


// =======================================================
//   GL consts, types and functions
// =======================================================


// -------------------------------------------------------
//   GL types
// -------------------------------------------------------

type
  PSingle   = ^Single;
  PDouble   = ^Double;

  GLvoid    = Pointer;
  GLboolean = Byte;

  GLbyte    = ShortInt; // 1-byte signed
  GLshort   = Integer;  // 2-byte signed
  GLint     = LongInt;  // 4-byte signed

  GLubyte   = Byte;     // 1-byte unsigned
  GLushort  = Word;     // 2-byte unsigned
  GLuint    = DWord;    // 4-byte signed

  GLsizei   = LongInt;  // 4-byte signed

  GLfloat   = Single;   // single precision float
  GLclampf  = Single;   // single precision float in [0,1]
  GLdouble  = Double;   // double precision float
  GLclampd  = Double;   // double precision float in [0,1]

  GLenum    = DWord;

  PGLBoolean = ^GLBoolean;
  PGLFloat   = ^GLfloat;
  PGLDouble  = ^GLDouble;

type
  GLbitfield = DWord;  { was an enum - no corresponding thing in pascal }
const
  GL_CURRENT_BIT        = $00000001;
  GL_POINT_BIT          = $00000002;
  GL_LINE_BIT           = $00000004;
  GL_POLYGON_BIT        = $00000008;
  GL_POLYGON_STIPPLE_BIT= $00000010;
  GL_PIXEL_MODE_BIT     = $00000020;
  GL_LIGHTING_BIT       = $00000040;
  GL_FOG_BIT            = $00000080;
  GL_DEPTH_BUFFER_BIT   = $00000100;
  GL_ACCUM_BUFFER_BIT   = $00000200;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_VIEWPORT_BIT       = $00000800;
  GL_TRANSFORM_BIT      = $00001000;
  GL_ENABLE_BIT         = $00002000;
  GL_COLOR_BUFFER_BIT   = $00004000;
  GL_HINT_BIT           = $00008000;
  GL_EVAL_BIT           = $00010000;
  GL_LIST_BIT           = $00020000;
  GL_TEXTURE_BIT        = $00040000;
  GL_SCISSOR_BIT        = $00080000;
  GL_ALL_ATTRIB_BITS    = $000fffff;


// -------------------------------------------------------
//   GL constants
// -------------------------------------------------------

{$IFDEF GL1_0}

const
  GL_NO_ERROR                           = 0;

  // Boolean values
  GL_FALSE                              = 0;
  GL_TRUE                               = 1;

  // Data types
  GL_BYTE                               = $1400;
  GL_UNSIGNED_BYTE                      = $1401;
  GL_SHORT                              = $1402;
  GL_UNSIGNED_SHORT                     = $1403;
  GL_INT                                = $1404;
  GL_UNSIGNED_INT                       = $1405;
  GL_FLOAT                              = $1406;
  GL_DOUBLE                             = $140A;
  GL_2_BYTES                            = $1407;
  GL_3_BYTES                            = $1408;
  GL_4_BYTES                            = $1409;

  // Primitives
  GL_LINES                              = $0001;
  GL_POINTS                             = $0000;
  GL_LINE_STRIP                         = $0003;
  GL_LINE_LOOP                          = $0002;
  GL_TRIANGLES                          = $0004;
  GL_TRIANGLE_STRIP                     = $0005;
  GL_TRIANGLE_FAN                       = $0006;
  GL_QUADS                              = $0007;
  GL_QUAD_STRIP                         = $0008;
  GL_POLYGON                            = $0009;
  GL_EDGE_FLAG                          = $0B43;

  // Vertex arrays
  GL_VERTEX_ARRAY                       = $8074;
  GL_NORMAL_ARRAY                       = $8075;
  GL_COLOR_ARRAY                        = $8076;
  GL_INDEX_ARRAY                        = $8077;
  GL_TEXTURE_COORD_ARRAY                = $8078;
  GL_EDGE_FLAG_ARRAY                    = $8079;
  GL_VERTEX_ARRAY_SIZE                  = $807A;
  GL_VERTEX_ARRAY_TYPE                  = $807B;
  GL_VERTEX_ARRAY_STRIDE                = $807C;
  GL_NORMAL_ARRAY_TYPE                  = $807E;
  GL_NORMAL_ARRAY_STRIDE                = $807F;
  GL_COLOR_ARRAY_SIZE                   = $8081;
  GL_COLOR_ARRAY_TYPE                   = $8082;
  GL_COLOR_ARRAY_STRIDE                 = $8083;
  GL_INDEX_ARRAY_TYPE                   = $8085;
  GL_INDEX_ARRAY_STRIDE                 = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE           = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE           = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE         = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE             = $808C;
  GL_VERTEX_ARRAY_POINTER               = $808E;
  GL_NORMAL_ARRAY_POINTER               = $808F;
  GL_COLOR_ARRAY_POINTER                = $8090;
  GL_INDEX_ARRAY_POINTER                = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER        = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER            = $8093;
  GL_V2F                                = $2A20;
  GL_V3F                                = $2A21;
  GL_C4UB_V2F                           = $2A22;
  GL_C4UB_V3F                           = $2A23;
  GL_C3F_V3F                            = $2A24;
  GL_N3F_V3F                            = $2A25;
  GL_C4F_N3F_V3F                        = $2A26;
  GL_T2F_V3F                            = $2A27;
  GL_T4F_V4F                            = $2A28;
  GL_T2F_C4UB_V3F                       = $2A29;
  GL_T2F_C3F_V3F                        = $2A2A;
  GL_T2F_N3F_V3F                        = $2A2B;
  GL_T2F_C4F_N3F_V3F                    = $2A2C;
  GL_T4F_C4F_N3F_V4F                    = $2A2D;

  // Matrix Mode
  GL_MATRIX_MODE                        = $0BA0;
  GL_MODELVIEW                          = $1700;
  GL_PROJECTION                         = $1701;
  GL_TEXTURE                            = $1702;

  // Points
  GL_POINT_SMOOTH                       = $0B10;
  GL_POINT_SIZE                         = $0B11;
  GL_POINT_SIZE_GRANULARITY             = $0B13;
  GL_POINT_SIZE_RANGE                   = $0B12;

  // Lines
  GL_LINE_SMOOTH                        = $0B20;
  GL_LINE_STIPPLE                       = $0B24;
  GL_LINE_STIPPLE_PATTERN               = $0B25;
  GL_LINE_STIPPLE_REPEAT                = $0B26;
  GL_LINE_WIDTH                         = $0B21;
  GL_LINE_WIDTH_GRANULARITY             = $0B23;
  GL_LINE_WIDTH_RANGE                   = $0B22;

  // Polygons
  GL_POINT                              = $1B00;
  GL_LINE                               = $1B01;
  GL_FILL                               = $1B02;
  GL_CCW                                = $0901;
  GL_CW                                 = $0900;
  GL_FRONT                              = $0404;
  GL_BACK                               = $0405;
  GL_CULL_FACE                          = $0B44;
  GL_CULL_FACE_MODE                     = $0B45;
  GL_POLYGON_SMOOTH                     = $0B41;
  GL_POLYGON_STIPPLE                    = $0B42;
  GL_FRONT_FACE                         = $0B46;
  GL_POLYGON_MODE                       = $0B40;
  GL_POLYGON_OFFSET_FACTOR              = $8038;
  GL_POLYGON_OFFSET_UNITS               = $2A00;
  GL_POLYGON_OFFSET_POINT               = $2A01;
  GL_POLYGON_OFFSET_LINE                = $2A02;
  GL_POLYGON_OFFSET_FILL                = $8037;

  // Display lists
  GL_COMPILE                            = $1300;
  GL_COMPILE_AND_EXECUTE                = $1301;
  GL_LIST_BASE                          = $0B32;
  GL_LIST_INDEX                         = $0B33;
  GL_LIST_MODE                          = $0B30;

  // Depth buffer
  GL_NEVER                              = $0200;
  GL_LESS                               = $0201;
  GL_GEQUAL                             = $0206;
  GL_LEQUAL                             = $0203;
  GL_GREATER                            = $0204;
  GL_NOTEQUAL                           = $0205;
  GL_EQUAL                              = $0202;
  GL_ALWAYS                             = $0207;
  GL_DEPTH_TEST                         = $0B71;
  GL_DEPTH_BITS                         = $0D56;
  GL_DEPTH_CLEAR_VALUE                  = $0B73;
  GL_DEPTH_FUNC                         = $0B74;
  GL_DEPTH_RANGE                        = $0B70;
  GL_DEPTH_WRITEMASK                    = $0B72;
  GL_DEPTH_COMPONENT                    = $1902;

  // Lighting
  GL_LIGHTING                           = $0B50;
  GL_LIGHT0                             = $4000;
  GL_LIGHT1                             = $4001;
  GL_LIGHT2                             = $4002;
  GL_LIGHT3                             = $4003;
  GL_LIGHT4                             = $4004;
  GL_LIGHT5                             = $4005;
  GL_LIGHT6                             = $4006;
  GL_LIGHT7                             = $4007;
  GL_SPOT_EXPONENT                      = $1205;
  GL_SPOT_CUTOFF                        = $1206;
  GL_CONSTANT_ATTENUATION               = $1207;
  GL_LINEAR_ATTENUATION                 = $1208;
  GL_QUADRATIC_ATTENUATION              = $1209;
  GL_AMBIENT                            = $1200;
  GL_DIFFUSE                            = $1201;
  GL_SPECULAR                           = $1202;
  GL_SHININESS                          = $1601;
  GL_EMISSION                           = $1600;
  GL_POSITION                           = $1203;
  GL_SPOT_DIRECTION                     = $1204;
  GL_AMBIENT_AND_DIFFUSE                = $1602;
  GL_COLOR_INDEXES                      = $1603;
  GL_LIGHT_MODEL_TWO_SIDE               = $0B52;
  GL_LIGHT_MODEL_LOCAL_VIEWER           = $0B51;
  GL_LIGHT_MODEL_AMBIENT                = $0B53;
  GL_FRONT_AND_BACK                     = $0408;
  GL_SHADE_MODEL                        = $0B54;
  GL_FLAT                               = $1D00;
  GL_SMOOTH                             = $1D01;
  GL_COLOR_MATERIAL                     = $0B57;
  GL_COLOR_MATERIAL_FACE                = $0B55;
  GL_COLOR_MATERIAL_PARAMETER           = $0B56;
  GL_NORMALIZE                          = $0BA1;

  // User clipping planes
  GL_CLIP_PLANE0                        = $3000;
  GL_CLIP_PLANE1                        = $3001;
  GL_CLIP_PLANE2                        = $3002;
  GL_CLIP_PLANE3                        = $3003;
  GL_CLIP_PLANE4                        = $3004;
  GL_CLIP_PLANE5                        = $3005;

  // Accumulation buffer
  GL_ACCUM_RED_BITS                     = $0D58;
  GL_ACCUM_GREEN_BITS                   = $0D59;
  GL_ACCUM_BLUE_BITS                    = $0D5A;
  GL_ACCUM_ALPHA_BITS                   = $0D5B;
  GL_ACCUM_CLEAR_VALUE                  = $0B80;
  GL_ACCUM                              = $0100;
  GL_ADD                                = $0104;
  GL_LOAD                               = $0101;
  GL_MULT                               = $0103;
  GL_RETURN                             = $0102;

  // Alpha testing
  GL_ALPHA_TEST                         = $0BC0;
  GL_ALPHA_TEST_REF                     = $0BC2;
  GL_ALPHA_TEST_FUNC                    = $0BC1;

  // Blending
  GL_BLEND                              = $0BE2;
  GL_BLEND_SRC                          = $0BE1;
  GL_BLEND_DST                          = $0BE0;
  GL_ZERO                               = 0;
  GL_ONE                                = 1;
  GL_SRC_COLOR                          = $0300;
  GL_ONE_MINUS_SRC_COLOR                = $0301;
  GL_DST_COLOR                          = $0306;
  GL_ONE_MINUS_DST_COLOR                = $0307;
  GL_SRC_ALPHA                          = $0302;
  GL_ONE_MINUS_SRC_ALPHA                = $0303;
  GL_DST_ALPHA                          = $0304;
  GL_ONE_MINUS_DST_ALPHA                = $0305;
  GL_SRC_ALPHA_SATURATE                 = $0308;
  GL_CONSTANT_COLOR                     = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR           = $8002;
  GL_CONSTANT_ALPHA                     = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA           = $8004;

  // Render mode
  GL_FEEDBACK                           = $1C01;
  GL_RENDER                             = $1C00;
  GL_SELECT                             = $1C02;

  // Feedback
  GL_2D                                 = $0600;
  GL_3D                                 = $0601;
  GL_3D_COLOR                           = $0602;
  GL_3D_COLOR_TEXTURE                   = $0603;
  GL_4D_COLOR_TEXTURE                   = $0604;
  GL_POINT_TOKEN                        = $0701;
  GL_LINE_TOKEN                         = $0702;
  GL_LINE_RESET_TOKEN                   = $0707;
  GL_POLYGON_TOKEN                      = $0703;
  GL_BITMAP_TOKEN                       = $0704;
  GL_DRAW_PIXEL_TOKEN                   = $0705;
  GL_COPY_PIXEL_TOKEN                   = $0706;
  GL_PASS_THROUGH_TOKEN                 = $0700;
  GL_FEEDBACK_BUFFER_POINTER            = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE               = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE               = $0DF2;

  // Selection
  GL_SELECTION_BUFFER_POINTER           = $0DF3;
  GL_SELECTION_BUFFER_SIZE              = $0DF4;

  // Fog
  GL_FOG                                = $0B60;
  GL_FOG_MODE                           = $0B65;
  GL_FOG_DENSITY                        = $0B62;
  GL_FOG_COLOR                          = $0B66;
  GL_FOG_INDEX                          = $0B61;
  GL_FOG_START                          = $0B63;
  GL_FOG_END                            = $0B64;
  GL_LINEAR                             = $2601;
  GL_EXP                                = $0800;
  GL_EXP2                               = $0801;

  // Logic ops
  GL_LOGIC_OP                           = $0BF1;
  GL_INDEX_LOGIC_OP                     = $0BF1;
  GL_COLOR_LOGIC_OP                     = $0BF2;
  GL_LOGIC_OP_MODE                      = $0BF0;
  GL_CLEAR                              = $1500;
  GL_SET                                = $150F;
  GL_COPY                               = $1503;
  GL_COPY_INVERTED                      = $150C;
  GL_NOOP                               = $1505;
  GL_INVERT                             = $150A;
  GL_AND                                = $1501;
  GL_NAND                               = $150E;
  GL_OR                                 = $1507;
  GL_NOR                                = $1508;
  GL_XOR                                = $1506;
  GL_EQUIV                              = $1509;
  GL_AND_REVERSE                        = $1502;
  GL_AND_INVERTED                       = $1504;
  GL_OR_REVERSE                         = $150B;
  GL_OR_INVERTED                        = $150D;

  // Stencil
  GL_STENCIL_TEST                       = $0B90;
  GL_STENCIL_WRITEMASK                  = $0B98;
  GL_STENCIL_BITS                       = $0D57;
  GL_STENCIL_FUNC                       = $0B92;
  GL_STENCIL_VALUE_MASK                 = $0B93;
  GL_STENCIL_REF                        = $0B97;
  GL_STENCIL_FAIL                       = $0B94;
  GL_STENCIL_PASS_DEPTH_PASS            = $0B96;
  GL_STENCIL_PASS_DEPTH_FAIL            = $0B95;
  GL_STENCIL_CLEAR_VALUE                = $0B91;
  GL_STENCIL_INDEX                      = $1901;
  GL_KEEP                               = $1E00;
  GL_REPLACE                            = $1E01;
  GL_INCR                               = $1E02;
  GL_DECR                               = $1E03;

  // Buffers, Pixel Drawing/Reading
  GL_NONE                               = 0;
  GL_LEFT                               = $0406;
  GL_RIGHT                              = $0407;
  //GL_FRONT                            = $0404;
  //GL_BACK                             = $0405;
  //GL_FRONT_AND_BACK                   = $0408;
  GL_FRONT_LEFT                         = $0400;
  GL_FRONT_RIGHT                        = $0401;
  GL_BACK_LEFT                          = $0402;
  GL_BACK_RIGHT                         = $0403;
  GL_AUX0                               = $0409;
  GL_AUX1                               = $040A;
  GL_AUX2                               = $040B;
  GL_AUX3                               = $040C;
  GL_COLOR_INDEX                        = $1900;
  GL_RED                                = $1903;
  GL_GREEN                              = $1904;
  GL_BLUE                               = $1905;
  GL_ALPHA                              = $1906;
  GL_LUMINANCE                          = $1909;
  GL_LUMINANCE_ALPHA                    = $190A;
  GL_ALPHA_BITS                         = $0D55;
  GL_RED_BITS                           = $0D52;
  GL_GREEN_BITS                         = $0D53;
  GL_BLUE_BITS                          = $0D54;
  GL_INDEX_BITS                         = $0D51;
  GL_SUBPIXEL_BITS                      = $0D50;
  GL_AUX_BUFFERS                        = $0C00;
  GL_READ_BUFFER                        = $0C02;
  GL_DRAW_BUFFER                        = $0C01;
  GL_DOUBLEBUFFER                       = $0C32;
  GL_STEREO                             = $0C33;
  GL_BITMAP                             = $1A00;
  GL_COLOR                              = $1800;
  GL_DEPTH                              = $1801;
  GL_STENCIL                            = $1802;
  GL_DITHER                             = $0BD0;
  GL_RGB                                = $1907;
  GL_RGBA                               = $1908;

  // Implementation limits
  GL_MAX_LIST_NESTING                   = $0B31;
  GL_MAX_ATTRIB_STACK_DEPTH             = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH          = $0D36;
  GL_MAX_NAME_STACK_DEPTH               = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH         = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH            = $0D39;
  GL_MAX_EVAL_ORDER                     = $0D30;
  GL_MAX_LIGHTS                         = $0D31;
  GL_MAX_CLIP_PLANES                    = $0D32;
  GL_MAX_TEXTURE_SIZE                   = $0D33;
  GL_MAX_PIXEL_MAP_TABLE                = $0D34;
  GL_MAX_VIEWPORT_DIMS                  = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH      = $0D3B;

  // Gets
  GL_ATTRIB_STACK_DEPTH                 = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH          = $0BB1;
  GL_COLOR_CLEAR_VALUE                  = $0C22;
  GL_COLOR_WRITEMASK                    = $0C23;
  GL_CURRENT_INDEX                      = $0B01;
  GL_CURRENT_COLOR                      = $0B00;
  GL_CURRENT_NORMAL                     = $0B02;
  GL_CURRENT_RASTER_COLOR               = $0B04;
  GL_CURRENT_RASTER_DISTANCE            = $0B09;
  GL_CURRENT_RASTER_INDEX               = $0B05;
  GL_CURRENT_RASTER_POSITION            = $0B07;
  GL_CURRENT_RASTER_TEXTURE_COORDS      = $0B06;
  GL_CURRENT_RASTER_POSITION_VALID      = $0B08;
  GL_CURRENT_TEXTURE_COORDS             = $0B03;
  GL_INDEX_CLEAR_VALUE                  = $0C20;
  GL_INDEX_MODE                         = $0C30;
  GL_INDEX_WRITEMASK                    = $0C21;
  GL_MODELVIEW_MATRIX                   = $0BA6;
  GL_MODELVIEW_STACK_DEPTH              = $0BA3;
  GL_NAME_STACK_DEPTH                   = $0D70;
  GL_PROJECTION_MATRIX                  = $0BA7;
  GL_PROJECTION_STACK_DEPTH             = $0BA4;
  GL_RENDER_MODE                        = $0C40;
  GL_RGBA_MODE                          = $0C31;
  GL_TEXTURE_MATRIX                     = $0BA8;
  GL_TEXTURE_STACK_DEPTH                = $0BA5;
  GL_VIEWPORT                           = $0BA2;

  // Evaluators
  GL_AUTO_NORMAL                        = $0D80;
  GL_MAP1_COLOR_4                       = $0D90;
  GL_MAP1_GRID_DOMAIN                   = $0DD0;
  GL_MAP1_GRID_SEGMENTS                 = $0DD1;
  GL_MAP1_INDEX                         = $0D91;
  GL_MAP1_NORMAL                        = $0D92;
  GL_MAP1_TEXTURE_COORD_1               = $0D93;
  GL_MAP1_TEXTURE_COORD_2               = $0D94;
  GL_MAP1_TEXTURE_COORD_3               = $0D95;
  GL_MAP1_TEXTURE_COORD_4               = $0D96;
  GL_MAP1_VERTEX_3                      = $0D97;
  GL_MAP1_VERTEX_4                      = $0D98;
  GL_MAP2_COLOR_4                       = $0DB0;
  GL_MAP2_GRID_DOMAIN                   = $0DD2;
  GL_MAP2_GRID_SEGMENTS                 = $0DD3;
  GL_MAP2_INDEX                         = $0DB1;
  GL_MAP2_NORMAL                        = $0DB2;
  GL_MAP2_TEXTURE_COORD_1               = $0DB3;
  GL_MAP2_TEXTURE_COORD_2               = $0DB4;
  GL_MAP2_TEXTURE_COORD_3               = $0DB5;
  GL_MAP2_TEXTURE_COORD_4               = $0DB6;
  GL_MAP2_VERTEX_3                      = $0DB7;
  GL_MAP2_VERTEX_4                      = $0DB8;
  GL_COEFF                              = $0A00;
  GL_DOMAIN                             = $0A02;
  GL_ORDER                              = $0A01;

  // Hints
  GL_FOG_HINT                           = $0C54;
  GL_LINE_SMOOTH_HINT                   = $0C52;
  GL_PERSPECTIVE_CORRECTION_HINT        = $0C50;
  GL_POINT_SMOOTH_HINT                  = $0C51;
  GL_POLYGON_SMOOTH_HINT                = $0C53;
  GL_DONT_CARE                          = $1100;
  GL_FASTEST                            = $1101;
  GL_NICEST                             = $1102;

  // Scissor box
  GL_SCISSOR_TEST                       = $0C11;
  GL_SCISSOR_BOX                        = $0C10;

  // Pixel Mode / Transfer
  GL_MAP_COLOR                          = $0D10;
  GL_MAP_STENCIL                        = $0D11;
  GL_INDEX_SHIFT                        = $0D12;
  GL_INDEX_OFFSET                       = $0D13;
  GL_RED_SCALE                          = $0D14;
  GL_RED_BIAS                           = $0D15;
  GL_GREEN_SCALE                        = $0D18;
  GL_GREEN_BIAS                         = $0D19;
  GL_BLUE_SCALE                         = $0D1A;
  GL_BLUE_BIAS                          = $0D1B;
  GL_ALPHA_SCALE                        = $0D1C;
  GL_ALPHA_BIAS                         = $0D1D;
  GL_DEPTH_SCALE                        = $0D1E;
  GL_DEPTH_BIAS                         = $0D1F;
  GL_PIXEL_MAP_S_TO_S_SIZE              = $0CB1;
  GL_PIXEL_MAP_I_TO_I_SIZE              = $0CB0;
  GL_PIXEL_MAP_I_TO_R_SIZE              = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE              = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE              = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE              = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE              = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE              = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE              = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE              = $0CB9;
  GL_PIXEL_MAP_S_TO_S                   = $0C71;
  GL_PIXEL_MAP_I_TO_I                   = $0C70;
  GL_PIXEL_MAP_I_TO_R                   = $0C72;
  GL_PIXEL_MAP_I_TO_G                   = $0C73;
  GL_PIXEL_MAP_I_TO_B                   = $0C74;
  GL_PIXEL_MAP_I_TO_A                   = $0C75;
  GL_PIXEL_MAP_R_TO_R                   = $0C76;
  GL_PIXEL_MAP_G_TO_G                   = $0C77;
  GL_PIXEL_MAP_B_TO_B                   = $0C78;
  GL_PIXEL_MAP_A_TO_A                   = $0C79;
  GL_PACK_ALIGNMENT                     = $0D05;
  GL_PACK_LSB_FIRST                     = $0D01;
  GL_PACK_ROW_LENGTH                    = $0D02;
  GL_PACK_SKIP_PIXELS                   = $0D04;
  GL_PACK_SKIP_ROWS                     = $0D03;
  GL_PACK_SWAP_BYTES                    = $0D00;
  GL_UNPACK_ALIGNMENT                   = $0CF5;
  GL_UNPACK_LSB_FIRST                   = $0CF1;
  GL_UNPACK_ROW_LENGTH                  = $0CF2;
  GL_UNPACK_SKIP_PIXELS                 = $0CF4;
  GL_UNPACK_SKIP_ROWS                   = $0CF3;
  GL_UNPACK_SWAP_BYTES                  = $0CF0;
  GL_ZOOM_X                             = $0D16;
  GL_ZOOM_Y                             = $0D17;

  // Texture mapping
  GL_TEXTURE_ENV                        = $2300;
  GL_TEXTURE_ENV_MODE                   = $2200;
  GL_TEXTURE_1D                         = $0DE0;
  GL_TEXTURE_2D                         = $0DE1;
  GL_TEXTURE_WRAP_S                     = $2802;
  GL_TEXTURE_WRAP_T                     = $2803;
  GL_TEXTURE_MAG_FILTER                 = $2800;
  GL_TEXTURE_MIN_FILTER                 = $2801;
  GL_TEXTURE_ENV_COLOR                  = $2201;
  GL_TEXTURE_GEN_S                      = $0C60;
  GL_TEXTURE_GEN_T                      = $0C61;
  GL_TEXTURE_GEN_MODE                   = $2500;
  GL_TEXTURE_BORDER_COLOR               = $1004;
  GL_TEXTURE_WIDTH                      = $1000;
  GL_TEXTURE_HEIGHT                     = $1001;
  GL_TEXTURE_BORDER                     = $1005;
  GL_TEXTURE_COMPONENTS                 = $1003;
  GL_TEXTURE_RED_SIZE                   = $805C;
  GL_TEXTURE_GREEN_SIZE                 = $805D;
  GL_TEXTURE_BLUE_SIZE                  = $805E;
  GL_TEXTURE_ALPHA_SIZE                 = $805F;
  GL_TEXTURE_LUMINANCE_SIZE             = $8060;
  GL_TEXTURE_INTENSITY_SIZE             = $8061;
  GL_NEAREST_MIPMAP_NEAREST             = $2700;
  GL_NEAREST_MIPMAP_LINEAR              = $2702;
  GL_LINEAR_MIPMAP_NEAREST              = $2701;
  GL_LINEAR_MIPMAP_LINEAR               = $2703;
  GL_OBJECT_LINEAR                      = $2401;
  GL_OBJECT_PLANE                       = $2501;
  GL_EYE_LINEAR                         = $2400;
  GL_EYE_PLANE                          = $2502;
  GL_SPHERE_MAP                         = $2402;
  GL_DECAL                              = $2101;
  GL_MODULATE                           = $2100;
  GL_NEAREST                            = $2600;
  GL_REPEAT                             = $2901;
  GL_CLAMP                              = $2900;
  GL_S                                  = $2000;
  GL_T                                  = $2001;
  GL_R                                  = $2002;
  GL_Q                                  = $2003;
  GL_TEXTURE_GEN_R                      = $0C62;
  GL_TEXTURE_GEN_Q                      = $0C63;
{$ENDIF GL1_0}

{$IFDEF GL1_1}

const
  // GL 1.1 texturing
  GL_PROXY_TEXTURE_1D                   = $8063;
  GL_PROXY_TEXTURE_2D                   = $8064;
  GL_TEXTURE_PRIORITY                   = $8066;
  GL_TEXTURE_RESIDENT                   = $8067;
  GL_TEXTURE_BINDING_1D                 = $8068;
  GL_TEXTURE_BINDING_2D                 = $8069;
  GL_TEXTURE_INTERNAL_FORMAT            = $1003;

  // Internal texture formats (GL 1.1)
  GL_ALPHA4                             = $803B;
  GL_ALPHA8                             = $803C;
  GL_ALPHA12                            = $803D;
  GL_ALPHA16                            = $803E;
  GL_LUMINANCE4                         = $803F;
  GL_LUMINANCE8                         = $8040;
  GL_LUMINANCE12                        = $8041;
  GL_LUMINANCE16                        = $8042;
  GL_LUMINANCE4_ALPHA4                  = $8043;
  GL_LUMINANCE6_ALPHA2                  = $8044;
  GL_LUMINANCE8_ALPHA8                  = $8045;
  GL_LUMINANCE12_ALPHA4                 = $8046;
  GL_LUMINANCE12_ALPHA12                = $8047;
  GL_LUMINANCE16_ALPHA16                = $8048;
  GL_INTENSITY                          = $8049;
  GL_INTENSITY4                         = $804A;
  GL_INTENSITY8                         = $804B;
  GL_INTENSITY12                        = $804C;
  GL_INTENSITY16                        = $804D;
  GL_R3_G3_B2                           = $2A10;
  GL_RGB4                               = $804F;
  GL_RGB5                               = $8050;
  GL_RGB8                               = $8051;
  GL_RGB10                              = $8052;
  GL_RGB12                              = $8053;
  GL_RGB16                              = $8054;
  GL_RGBA2                              = $8055;
  GL_RGBA4                              = $8056;
  GL_RGB5_A1                            = $8057;
  GL_RGBA8                              = $8058;
  GL_RGB10_A2                           = $8059;
  GL_RGBA12                             = $805A;
  GL_RGBA16                             = $805B;

{$ENDIF GL1_1}

{$IFDEF GL1_2}

const
  // GL 1.2 texturing
  GL_PACK_SKIP_IMAGES                   = $806B;
  GL_PACK_IMAGE_HEIGHT                  = $806C;
  GL_UNPACK_SKIP_IMAGES                 = $806D;
  GL_UNPACK_IMAGE_HEIGHT                = $806E;
  GL_TEXTURE_3D                         = $806F;
  GL_PROXY_TEXTURE_3D                   = $8070;
  GL_TEXTURE_DEPTH                      = $8071;
  GL_TEXTURE_WRAP_R                     = $8072;
  GL_MAX_3D_TEXTURE_SIZE                = $8073;
  GL_TEXTURE_BINDING_3D                 = $806A;

const
  GL_RESCALE_NORMAL                     = $803A;
  GL_CLAMP_TO_EDGE                      = $812F;
  GL_MAX_ELEMENTS_VERTICES              = $F0E8;
  GL_MAX_ELEMENTS_INDICES               = $F0E9;
  GL_BGR                                = $80E0;
  GL_BGRA                               = $80E1;
  GL_UNSIGNED_BYTE_3_3_2                = $8032;
  GL_UNSIGNED_BYTE_2_3_3_REV            = $8362;
  GL_UNSIGNED_SHORT_5_6_5               = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV           = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4             = $8033;
  GL_UNSIGNED_SHORT_4_4_4_4_REV         = $8365;
  GL_UNSIGNED_SHORT_5_5_5_1             = $8034;
  GL_UNSIGNED_SHORT_1_5_5_5_REV         = $8366;
  GL_UNSIGNED_INT_8_8_8_8               = $8035;
  GL_UNSIGNED_INT_8_8_8_8_REV           = $8367;
  GL_UNSIGNED_INT_10_10_10_2            = $8036;
  GL_UNSIGNED_INT_2_10_10_10_REV        = $8368;
  GL_LIGHT_MODEL_COLOR_CONTROL          = $81F8;
  GL_SINGLE_COLOR                       = $81F9;
  GL_SEPARATE_SPECULAR_COLOR            = $81FA;
  GL_TEXTURE_MIN_LOD                    = $813A;
  GL_TEXTURE_MAX_LOD                    = $813B;
  GL_TEXTURE_BASE_LEVEL                 = $813C;
  GL_TEXTURE_MAX_LEVEL                  = $813D;

{$ENDIF GL1_2}

const
  // Utility
  GL_VENDOR                             = $1F00;
  GL_RENDERER                           = $1F01;
  GL_VERSION                            = $1F02;
  GL_EXTENSIONS                         = $1F03;

  // Errors
  GL_INVALID_VALUE                      = $0501;
  GL_INVALID_ENUM                       = $0500;
  GL_INVALID_OPERATION                  = $0502;
  GL_STACK_OVERFLOW                     = $0503;
  GL_STACK_UNDERFLOW                    = $0504;
  GL_OUT_OF_MEMORY                      = $0505;


// -------------------------------------------------------
//   GL extensions constants
// -------------------------------------------------------

{$IFDEF EXTENSIONS}

const
  // GL_EXT_blend_minmax and GL_EXT_blend_color
  GL_CONSTANT_COLOR_EXT                 = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT       = $8002;
  GL_CONSTANT_ALPHA_EXT                 = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT       = $8004;
  GL_BLEND_EQUATION_EXT                 = $8009;
  GL_MIN_EXT                            = $8007;
  GL_MAX_EXT                            = $8008;
  GL_FUNC_ADD_EXT                       = $8006;
  GL_FUNC_SUBTRACT_EXT                  = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT          = $800B;
  GL_BLEND_COLOR_EXT                    = $8005;

  // GL_EXT_polygon_offset
  GL_POLYGON_OFFSET_EXT                 = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT          = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT            = $8039;

  // GL_EXT_vertex_array
  GL_VERTEX_ARRAY_EXT                   = $8074;
  GL_NORMAL_ARRAY_EXT                   = $8075;
  GL_COLOR_ARRAY_EXT                    = $8076;
  GL_INDEX_ARRAY_EXT                    = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT            = $8078;
  GL_EDGE_FLAG_ARRAY_EXT                = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT              = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT              = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT            = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT             = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT              = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT            = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT             = $8080;
  GL_COLOR_ARRAY_SIZE_EXT               = $8081;
  GL_COLOR_ARRAY_TYPE_EXT               = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT             = $8083;
  GL_COLOR_ARRAY_COUNT_EXT              = $8084;
  GL_INDEX_ARRAY_TYPE_EXT               = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT             = $8086;
  GL_INDEX_ARRAY_COUNT_EXT              = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT     = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT      = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT         = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT          = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT           = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT           = $808F;
  GL_COLOR_ARRAY_POINTER_EXT            = $8090;
  GL_INDEX_ARRAY_POINTER_EXT            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT        = $8093;

  // GL_EXT_texture_object
  GL_TEXTURE_PRIORITY_EXT               = $8066;
  GL_TEXTURE_RESIDENT_EXT               = $8067;
  GL_TEXTURE_1D_BINDING_EXT             = $8068;
  GL_TEXTURE_2D_BINDING_EXT             = $8069;

  // GL_EXT_texture3D
  GL_PACK_SKIP_IMAGES_EXT               = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT              = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT             = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT            = $806E;
  GL_TEXTURE_3D_EXT                     = $806F;
  GL_PROXY_TEXTURE_3D_EXT               = $8070;
  GL_TEXTURE_DEPTH_EXT                  = $8071;
  GL_TEXTURE_WRAP_R_EXT                 = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT            = $8073;
  GL_TEXTURE_3D_BINDING_EXT             = $806A;

  // GL_EXT_paletted_texture
  GL_TABLE_TOO_LARGE_EXT                = $8031;
  GL_COLOR_TABLE_FORMAT_EXT             = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT              = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT           = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT         = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT          = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT         = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT     = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT     = $80DF;
  GL_TEXTURE_INDEX_SIZE_EXT             = $80ED;
  GL_COLOR_INDEX1_EXT                   = $80E2;
  GL_COLOR_INDEX2_EXT                   = $80E3;
  GL_COLOR_INDEX4_EXT                   = $80E4;
  GL_COLOR_INDEX8_EXT                   = $80E5;
  GL_COLOR_INDEX12_EXT                  = $80E6;
  GL_COLOR_INDEX16_EXT                  = $80E7;

  // GL_EXT_shared_texture_palette
  GL_SHARED_TEXTURE_PALETTE_EXT         = $81FB;

  // GL_EXT_point_parameters
  GL_POINT_SIZE_MIN_EXT                 = $8126;
  GL_POINT_SIZE_MAX_EXT                 = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT      = $8128;
  GL_DISTANCE_ATTENUATION_EXT           = $8129;

  // GL_EXT_rescale_normal
  GL_RESCALE_NORMAL_EXT                 = $803A;

  // GL_EXT_abgr
  GL_ABGR_EXT                           = $8000;

const
  // GL_EXT_multitexture
  GL_SELECTED_TEXTURE_EXT               = $83C0;
  GL_SELECTED_TEXTURE_COORD_SET_EXT     = $83C1;
  GL_SELECTED_TEXTURE_TRANSFORM_EXT     = $83C2;
  GL_MAX_TEXTURES_EXT                   = $83C3;
  GL_MAX_TEXTURE_COORD_SETS_EXT         = $83C4;
  GL_TEXTURE_ENV_COORD_SET_EXT          = $83C5;
  GL_TEXTURE0_EXT                       = $83C6;
  GL_TEXTURE1_EXT                       = $83C7;
  GL_TEXTURE2_EXT                       = $83C8;
  GL_TEXTURE3_EXT                       = $83C9;

{$ENDIF EXTENSIONS}

{$IFDEF SGI_EXTENSIONS}

const
  // GL_SGIS_multitexture
  GL_SELECTED_TEXTURE_SGIS              = $835C;
  GL_SELECTED_TEXTURE_COORD_SET_SGIS    = $835D;
  GL_MAX_TEXTURES_SGIS                  = $835E;
  GL_TEXTURE0_SGIS                      = $835F;
  GL_TEXTURE1_SGIS                      = $8360;
  GL_TEXTURE2_SGIS                      = $8361;
  GL_TEXTURE3_SGIS                      = $8362;
  GL_TEXTURE_COORD_SET_SOURCE_SGIS      = $8363;

const
  // GL_SGIS_texture_edge_clamp
  GL_CLAMP_TO_EDGE_SGIS                 = $812F;

{$ENDIF SGI_EXTENSIONS}

{$IFDEF MESA}
{$ENDIF MESA}


// -------------------------------------------------------
//   GL procedures and functions
// -------------------------------------------------------

{$IFDEF GL1_0}
var
// Miscellaneous
glClearIndex: procedure(c: Single); ogl_dll
glClearColor: procedure(red, green, blue, alpha: GLclampf); ogl_dll
glClear: procedure(mask: GLbitfield); ogl_dll
glIndexMask: procedure(mask: LongWord); ogl_dll
glColorMask: procedure(red, green, blue, alpha: GLboolean); ogl_dll
glAlphaFunc: procedure(func: GLenum; ref: GLclampf); ogl_dll
glBlendFunc: procedure(sfactor, dfactor: GLenum); ogl_dll
glLogicOp: procedure(opcode: GLenum); ogl_dll
glCullFace: procedure(mode: GLenum); ogl_dll
glFrontFace: procedure(mode: GLenum); ogl_dll
glPointSize: procedure(size: Single); ogl_dll
glLineWidth: procedure(width: Single); ogl_dll
glLineStipple: procedure(factor: LongInt; pattern: Word); ogl_dll
glPolygonMode: procedure(face, mode: GLenum); ogl_dll
glPolygonOffset: procedure(factor, units: Single); ogl_dll
glPolygonStipple: procedure(var mask: Byte); ogl_dll
glGetPolygonStipple: procedure(var mask: Byte); ogl_dll
glEdgeFlag: procedure(flag: GLBoolean); ogl_dll
glEdgeFlagv: procedure(var flag: GLBoolean); ogl_dll
glScissor: procedure(x, y, width, height: LongInt); ogl_dll
glClipPlane: procedure(plane: GLenum; var equation: Double); ogl_dll
glGetClipPlane: procedure(plane: GLenum; var equation: Double); ogl_dll
glDrawBuffer: procedure(mode: GLenum); ogl_dll
glReadBuffer: procedure(mode: GLenum); ogl_dll
glEnable: procedure(cap: LongInt); ogl_dll
glDisable: procedure(cap: LongInt); ogl_dll
glIsEnabled: function(cap: GLenum): GLBoolean; ogl_dll
glGetBooleanv: procedure(pname: GLenum; params : PGLBoolean); ogl_dll
glGetDoublev: procedure(pname: GLenum; params : PDouble); ogl_dll
glGetFloatv: procedure(pname: GLenum; params : PSingle); ogl_dll
glGetIntegerv: procedure(pname: GLenum; params : PLongInt); ogl_dll
glPushAttrib: procedure(mask: GLbitfield); ogl_dll
glPopAttrib: procedure; ogl_dll
glRenderMode: function(mode: GLenum): LongInt; ogl_dll
glGetError: function: GLenum; ogl_dll
glGetString: function(name: GLenum): PChar; ogl_dll
glFinish: procedure; ogl_dll
glFlush: procedure; ogl_dll
glHint: procedure(target, mode: GLenum); ogl_dll

// Depth Buffer
glClearDepth: procedure(depth: GLclampd); ogl_dll
glDepthFunc: procedure(func: LongInt); ogl_dll
glDepthMask: procedure(flag: GLBoolean); ogl_dll
glDepthRange: procedure(near_val, far_val: GLclampd); ogl_dll

// Accumulation Buffer
glClearAccum: procedure(red, green, blue, alpha: Single); ogl_dll
glAccum: procedure(op: GLenum; value: Single); ogl_dll

// Tranformation
glMatrixMode: procedure(mode: GLenum); ogl_dll
glOrtho: procedure(left, right, bottom, top, near_val, far_val: Double); ogl_dll
glFrustum: procedure(left, right, bottom, top, near_val, far_val: Double); ogl_dll
glViewport: procedure(x, y, width, height: LongInt); ogl_dll
glPushMatrix: procedure; ogl_dll
glPopMatrix: procedure; ogl_dll
glLoadIdentity: procedure; ogl_dll
glLoadMatrixd: procedure(var m: Double); ogl_dll
glLoadMatrixf: procedure(var m: PSingle); ogl_dll
glMultMatrixd: procedure(var m: Double); ogl_dll
glMultMatrixf: procedure(var m: Single); ogl_dll
glRotated: procedure(angle, x, y, z: Double); ogl_dll
glRotatef: procedure(angle, x, y, z: Single); ogl_dll
glScaled: procedure(x, y, z: Double); ogl_dll
glScalef: procedure(x, y, z: Single); ogl_dll
glTranslated: procedure(x, y, z: Double); ogl_dll
glTranslatef: procedure(x, y, z: Single); ogl_dll

// Display Lists
glIsList: function(list: LongWord): GLBoolean; ogl_dll
glDeleteLists: procedure(list: LongWord; range: LongInt); ogl_dll
glGenLists: function(range: LongInt): LongWord; ogl_dll
glNewList: procedure(list: LongWord; mode: GLenum); ogl_dll
glEndList: procedure; ogl_dll
glCallList: procedure(list: LongWord); ogl_dll
glCallLists: procedure(n: LongInt; AType: GLenum; var lists); ogl_dll
glListBase: procedure(base: LongWord); ogl_dll

// Drawing Functions
glBegin: procedure(mode: GLenum); ogl_dll
glEnd: procedure; ogl_dll
glVertex2d: procedure(x, y: Double); ogl_dll
glVertex2f: procedure(x, y: Single); ogl_dll
glVertex2i: procedure(x, y: LongInt); ogl_dll
glVertex2s: procedure(x, y: SmallInt); ogl_dll
glVertex3d: procedure(x, y, z: Double); ogl_dll
glVertex3f: procedure(x, y, z: Single); ogl_dll
glVertex3i: procedure(x, y, z: LongInt); ogl_dll
glVertex3s: procedure(x, y, z: SmallInt); ogl_dll
glVertex4d: procedure(x, y, z, w: Double); ogl_dll
glVertex4f: procedure(x, y, z, w: Single); ogl_dll
glVertex4i: procedure(x, y, z, w: LongInt); ogl_dll
glVertex4s: procedure(x, y, z, w: SmallInt); ogl_dll
glVertex2dv: procedure(var v: Double); ogl_dll
glVertex2fv: procedure(var v: Single); ogl_dll
glVertex2iv: procedure(var v: LongInt); ogl_dll
glVertex2sv: procedure(var v: SmallInt); ogl_dll
glVertex3dv: procedure(var v: Double); ogl_dll
glVertex3fv: procedure(var v: Single); ogl_dll
glVertex3iv: procedure(var v: LongInt); ogl_dll
glVertex3sv: procedure(var v: SmallInt); ogl_dll
glVertex4dv: procedure(var v: Double); ogl_dll
glVertex4fv: procedure(var v: Single); ogl_dll
glVertex4iv: procedure(var v: LongInt); ogl_dll
glVertex4sv: procedure(var v: SmallInt); ogl_dll
glNormal3b: procedure(nx, ny, nz: Byte); ogl_dll
glNormal3d: procedure(nx, ny, nz: Double); ogl_dll
glNormal3f: procedure(nx, ny, nz: Single); ogl_dll
glNormal3i: procedure(nx, ny, nz: LongInt); ogl_dll
glNormal3s: procedure(nx, ny, nz: SmallInt); ogl_dll
glNormal3bv: procedure(var v: ShortInt); ogl_dll
glNormal3dv: procedure(var v: Double); ogl_dll
glNormal3fv: procedure(var v: Single); ogl_dll
glNormal3iv: procedure(var v: LongInt); ogl_dll
glNormal3sv: procedure(var v: SmallInt); ogl_dll
glIndexd: procedure(c: Double); ogl_dll
glIndexf: procedure(c: Single); ogl_dll
glIndexi: procedure(c: LongInt); ogl_dll
glIndexs: procedure(c: SmallInt); ogl_dll
glIndexdv: procedure(var c: Double); ogl_dll
glIndexfv: procedure(var c: Single); ogl_dll
glIndexiv: procedure(var c: LongInt); ogl_dll
glIndexsv: procedure(var c: SmallInt); ogl_dll
glColor3b: procedure (red, green, blue: ShortInt); ogl_dll
glColor3d: procedure (red, green, blue: Double); ogl_dll
glColor3f: procedure (red, green, blue: Single); ogl_dll
glColor3i: procedure (red, green, blue: LongInt); ogl_dll
glColor3s: procedure (red, green, blue: SmallInt); ogl_dll
glColor3ub: procedure(red, green, blue: Byte); ogl_dll
glColor3ui: procedure(red, green, blue: LongWord); ogl_dll
glColor3us: procedure(red, green, blue: Word); ogl_dll
glColor4b: procedure (red, green, blue, alpha: ShortInt); ogl_dll
glColor4d: procedure (red, green, blue, alpha: Double); ogl_dll
glColor4f: procedure (red, green, blue, alpha: Single); ogl_dll
glColor4i: procedure (red, green, blue, alpha: LongInt); ogl_dll
glColor4s: procedure (red, green, blue, alpha: SmallInt); ogl_dll
glColor4ub: procedure(red, green, blue, alpha: Byte); ogl_dll
glColor4ui: procedure(red, green, blue, alpha: LongWord); ogl_dll
glColor4us: procedure(red, green, blue, alpha: Word); ogl_dll
glColor3bv: procedure (var v: ShortInt); ogl_dll
glColor3dv: procedure (var v: Double); ogl_dll
glColor3fv: procedure (var v: Single); ogl_dll
glColor3iv: procedure (var v: LongInt); ogl_dll
glColor3sv: procedure (var v: SmallInt); ogl_dll
glColor3ubv: procedure(var v: Byte); ogl_dll
glColor3uiv: procedure(var v: LongWord); ogl_dll
glColor3usv: procedure(var v: Word); ogl_dll
glColor4bv: procedure (var v: ShortInt); ogl_dll
glColor4dv: procedure (var v: Double); ogl_dll
glColor4fv: procedure (var v: Single); ogl_dll
glColor4iv: procedure (var v: LongInt); ogl_dll
glColor4sv: procedure (var v: SmallInt); ogl_dll
glColor4ubv: procedure(var v: Byte); ogl_dll
glColor4uiv: procedure(var v: LongWord); ogl_dll
glColor4usv: procedure(var v: Word); ogl_dll
glTexCoord1d: procedure(s: Double); ogl_dll
glTexCoord1f: procedure(s: Single); ogl_dll
glTexCoord1i: procedure(s: LongInt); ogl_dll
glTexCoord1s: procedure(s: SmallInt); ogl_dll
glTexCoord2d: procedure(s, t: Double); ogl_dll
glTexCoord2f: procedure(s, t: Single); ogl_dll
glTexCoord2i: procedure(s, t: LongInt); ogl_dll
glTexCoord2s: procedure(s, t: SmallInt); ogl_dll
glTexCoord3d: procedure(s, t, r: Double); ogl_dll
glTexCoord3f: procedure(s, t, r: Single); ogl_dll
glTexCoord3i: procedure(s, t, r: LongInt); ogl_dll
glTexCoord3s: procedure(s, t, r: SmallInt); ogl_dll
glTexCoord4d: procedure(s, t, r, q: Double); ogl_dll
glTexCoord4f: procedure(s, t, r, q: Single); ogl_dll
glTexCoord4i: procedure(s, t, r, q: LongInt); ogl_dll
glTexCoord4s: procedure(s, t, r, q: SmallInt); ogl_dll
glTexCoord1dv: procedure(var v: Double); ogl_dll
glTexCoord1fv: procedure(var v: Single); ogl_dll
glTexCoord1iv: procedure(var v: LongInt); ogl_dll
glTexCoord1sv: procedure(var v: SmallInt); ogl_dll
glTexCoord2dv: procedure(var v: Double); ogl_dll
glTexCoord2fv: procedure(var v: Single); ogl_dll
glTexCoord2iv: procedure(var v: LongInt); ogl_dll
glTexCoord2sv: procedure(var v: SmallInt); ogl_dll
glTexCoord3dv: procedure(var v: Double); ogl_dll
glTexCoord3fv: procedure(var v: Single); ogl_dll
glTexCoord3iv: procedure(var v: LongInt); ogl_dll
glTexCoord3sv: procedure(var v: SmallInt); ogl_dll
glTexCoord4dv: procedure(var v: Double); ogl_dll
glTexCoord4fv: procedure(var v: Single); ogl_dll
glTexCoord4iv: procedure(var v: LongInt); ogl_dll
glTexCoord4sv: procedure(var v: SmallInt); ogl_dll
glRasterPos2d: procedure(x, y: Double); ogl_dll
glRasterPos2f: procedure(x, y: Single); ogl_dll
glRasterPos2i: procedure(x, y: LongInt); ogl_dll
glRasterPos2s: procedure(x, y: SmallInt); ogl_dll
glRasterPos3d: procedure(x, y, z: Double); ogl_dll
glRasterPos3f: procedure(x, y, z: Single); ogl_dll
glRasterPos3i: procedure(x, y, z: LongInt); ogl_dll
glRasterPos3s: procedure(x, y, z: SmallInt); ogl_dll
glRasterPos4d: procedure(x, y, z, w: Double); ogl_dll
glRasterPos4f: procedure(x, y, z, w: Single); ogl_dll
glRasterPos4i: procedure(x, y, z, w: LongInt); ogl_dll
glRasterPos4s: procedure(x, y, z, w: SmallInt); ogl_dll
glRasterPos2dv: procedure(var v: Double); ogl_dll
glRasterPos2fv: procedure(var v: Single); ogl_dll
glRasterPos2iv: procedure(var v: LongInt); ogl_dll
glRasterPos2sv: procedure(var v: SmallInt); ogl_dll
glRasterPos3dv: procedure(var v: Double); ogl_dll
glRasterPos3fv: procedure(var v: Single); ogl_dll
glRasterPos3iv: procedure(var v: LongInt); ogl_dll
glRasterPos3sv: procedure(var v: SmallInt); ogl_dll
glRasterPos4dv: procedure(var v: Double); ogl_dll
glRasterPos4fv: procedure(var v: Single); ogl_dll
glRasterPos4iv: procedure(var v: LongInt); ogl_dll
glRasterPos4sv: procedure(var v: SmallInt); ogl_dll
glRectd: procedure(x1, y1, x2, y2: Double); ogl_dll
glRectf: procedure(x1, y1, x2, y2: Single); ogl_dll
glRecti: procedure(x1, y1, x2, y2: LongInt); ogl_dll
glRects: procedure(x1, y1, x2, y2: SmallInt); ogl_dll
glRectdv: procedure(var v1, v2: Double); ogl_dll
glRectfv: procedure(var v1, v2: Single); ogl_dll
glRectiv: procedure(var v1, v2: LongInt); ogl_dll
glRectsv: procedure(var v1, v2: SmallInt); ogl_dll

// Lighting
glShadeModel: procedure(mode: GLenum); ogl_dll
glLightf: procedure(light, pname: GLenum; param: Single); ogl_dll
glLighti: procedure(light, pname: GLenum; param: LongInt); ogl_dll
glLightfv: procedure(light, pname: GLenum; params : PSingle); ogl_dll
glLightiv: procedure(light, pname: GLenum; params : PLongInt); ogl_dll
glGetLightfv: procedure(light, pname: GLenum; params : PSingle); ogl_dll
glGetLightiv: procedure(light, pname: GLenum; params : PLongInt); ogl_dll
glLightModelf: procedure(pname: GLenum; param: Single); ogl_dll
glLightModeli: procedure(pname: GLenum; param: LongInt); ogl_dll
glLightModelfv: procedure(pname: GLenum; params : PSingle); ogl_dll
glLightModeliv: procedure(pname: GLenum; params : PLongInt); ogl_dll
glMaterialf: procedure(face, pname: GLenum; param: Single); ogl_dll
glMateriali: procedure(face, pname: GLenum; param: LongInt); ogl_dll
glMaterialfv: procedure(face, pname: GLenum; params : PSingle); ogl_dll
glMaterialiv: procedure(face, pname: GLenum; params : PLongInt); ogl_dll
glGetMaterialfv: procedure(face, pname: GLenum; params : PSingle); ogl_dll
glGetMaterialiv: procedure(face, pname: GLenum; params : PLongInt); ogl_dll
glColorMaterial: procedure(face, mode: GLenum); ogl_dll

// Raster Functions
glPixelZoom: procedure(xfactor, yfactor: Single); ogl_dll
glPixelStoref: procedure(pname: GLenum; param: Single); ogl_dll
glPixelStorei: procedure(pname: GLenum; param: LongInt); ogl_dll
glPixelTransferf: procedure(pname: GLenum; param: Single); ogl_dll
glPixelTransferi: procedure(pname: GLenum; param: LongInt); ogl_dll
glPixelMapfv: procedure(map: GLenum; mapsize: LongInt; var values: Single); ogl_dll
glPixelMapuiv: procedure(map: GLenum; mapsize: LongInt; var values: LongWord); ogl_dll
glPixelMapusv: procedure(map: GLenum; mapsize: LongInt; var values: Word); ogl_dll
glGetPixelMapfv: procedure(map: GLenum; var values: Single); ogl_dll
glGetPixelMapuiv: procedure(map: GLenum; var values: LongWord); ogl_dll
glGetPixelMapusv: procedure(map: GLenum; var values: Word); ogl_dll
glBitmap: procedure(width, height: LongInt; xorig, yorig, xmove, ymove: Single; var bitmap); ogl_dll
glReadPixels: procedure(x, y, width, height: LongInt; format, AType: GLenum; var pixels); ogl_dll
glDrawPixels: procedure(width, height: LongInt; format, AType: GLenum; var pixels); ogl_dll
glCopyPixels: procedure(x, y, width, height: LongInt; AType: GLenum); ogl_dll

// Stenciling
glStencilFunc: procedure(func: GLenum; ref: LongInt; mask: LongWord); ogl_dll
glStencilMask: procedure(mask: LongWord); ogl_dll
glStencilOp: procedure(fail, zfail, zpass: GLenum); ogl_dll
glClearStencil: procedure(s: LongInt); ogl_dll

// Texture Mapping
glTexGend: procedure(cord, pname: GLenum; param: Double); ogl_dll
glTexGenf: procedure(cord, pname: GLenum; param: Single); ogl_dll
glTexGeni: procedure(cord, pname: GLenum; param: LongInt); ogl_dll
glTexGendv: procedure(cord, pname: GLenum; params : PDouble); ogl_dll
glTexGenfv: procedure(cord, pname: GLenum; params : PSingle); ogl_dll
glTexGeniv: procedure(cord, pname: GLenum; params : PLongInt); ogl_dll
glGetTexGendv: procedure(cord, pname: GLenum; params : PDouble); ogl_dll
glGetTexGenfv: procedure(cord, pname: GLenum; params : PSingle); ogl_dll
glGetTexGeniv: procedure(cord, pname: GLenum; params : PLongInt); ogl_dll
glTexEnvf: procedure(target, pname: GLenum; param: Single); ogl_dll
glTexEnvi: procedure(target, pname: GLenum; param: LongInt); ogl_dll
glTexEnvfv: procedure(target, pname: GLenum; params : PSingle); ogl_dll
glTexEnviv: procedure(target, pname: GLenum; params : PLongInt); ogl_dll
glGetTexEnvfv: procedure(target, pname: GLenum; params : PSingle); ogl_dll
glGetTexEnviv: procedure(target, pname: GLenum; params : PLongInt); ogl_dll
glTexParameterf: procedure(target, pname: GLenum; param: Single); ogl_dll
glTexParameteri: procedure(target, pname: GLenum; param: LongInt); ogl_dll
glTexParameterfv: procedure(target, pname: GLenum; params : PSingle); ogl_dll
glTexParameteriv: procedure(target, pname: GLenum; params : PLongInt); ogl_dll
glGetTexParameterfv: procedure(target, pname: GLenum; params : PSingle); ogl_dll
glGetTexParameteriv: procedure(target, pname: GLenum; params : PLongInt); ogl_dll
glGetTexLevelParameterfv: procedure(target: GLenum; level: LongInt; pname: GLenum; params : PSingle); ogl_dll
glGetTexLevelParameteriv: procedure(target: GLenum; level: LongInt; pname: GLenum; params : PLongInt); ogl_dll
glTexImage1D: procedure(target: GLenum; level, internalFormat, width, border: LongInt; format, AType: GLenum; var pixels); ogl_dll
glTexImage2D: procedure(target: GLenum; level, internalFormat, width, height, border: LongInt; format, AType: GLenum; var pixels); ogl_dll
glGetTexImage: procedure(target: GLenum; level: LongInt; format, AType: GLenum; var pixels); ogl_dll

// Evaluators
glMap1d: procedure(target: GLenum; u1, u2: Double; stride, order: LongInt; var points: Double); ogl_dll
glMap1f: procedure(target: GLenum; u1, u2: Single; stride, order: LongInt; var points: Single); ogl_dll
glMap2d: procedure(target: GLenum; u1, u2: Double; ustride, uorder: LongInt; v1, v2: Double; vstride, vorder: LongInt; var points: Double); ogl_dll
glMap2f: procedure(target: GLenum; u1, u2: Single; ustride, uorder: LongInt; v1, v2: Single; vstride, vorder: LongInt; var points: Single); ogl_dll
glGetMapdv: procedure(target, query: GLenum; var v: Double); ogl_dll
glGetMapfv: procedure(target, query: GLenum; var v: Single); ogl_dll
glGetMapiv: procedure(target, query: GLenum; var v: LongInt); ogl_dll
glEvalCoord1d: procedure(u: Double); ogl_dll
glEvalCoord1f: procedure(u: Single); ogl_dll
glEvalCoord1dv: procedure(var u: Double); ogl_dll
glEvalCoord1fv: procedure(var u: Single); ogl_dll
glEvalCoord2d: procedure(u, v: Double); ogl_dll
glEvalCoord2f: procedure(u, v: Single); ogl_dll
glEvalCoord2dv: procedure(var u, v: Double); ogl_dll
glEvalCoord2fv: procedure(var u, v: Single); ogl_dll
glMapGrid1d: procedure(un: LongInt; u1, u2: Double); ogl_dll
glMapGrid1f: procedure(un: LongInt; u1, u2: Single); ogl_dll
glMapGrid2d: procedure(un: LongInt; u1, u2: Double; vn: LongInt; v1, v2: Double); ogl_dll
glMapGrid2f: procedure(un: LongInt; u1, u2: Single; vn: LongInt; v1, v2: Single); ogl_dll
glEvalPoint1: procedure(i: LongInt); ogl_dll
glEvalPoint2: procedure(i, j: LongInt); ogl_dll
glEvalMesh1: procedure(mode: GLenum; i1, i2: LongInt); ogl_dll
glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: LongInt); ogl_dll

// Fog
glFogf: procedure(pname: GLenum; param: Single); ogl_dll
glFogi: procedure(pname: GLenum; param: LongInt); ogl_dll
glFogfv: procedure(pname: GLenum; params : PSingle); ogl_dll
glFogiv: procedure(pname: GLenum; params : PLongInt); ogl_dll

// Selection and Feedback
glFeedbackBuffer: procedure(size: LongInt; AType: GLenum; var buffer: Single); ogl_dll
glPassThrough: procedure(token: Single); ogl_dll
glSelectBuffer: procedure(size: LongInt; var buffer: LongWord); ogl_dll
glInitNames: procedure; ogl_dll
glLoadName: procedure(name: LongWord); ogl_dll
glPushName: procedure(name: LongWord); ogl_dll
glPopName: procedure; ogl_dll

{$ENDIF GL1_0}

{$IFDEF GL1_1}
var

// Miscellaneous
glEnableClientState: procedure(cap: GLenum); ogl_dll
glDisableClientState: procedure(cap: GLenum); ogl_dll
glPushClientAttrib: procedure(mask: GLbitfield); ogl_dll
glPopClientAttrib: procedure; ogl_dll

// Drawing Functions
glIndexub: procedure(c: Byte); ogl_dll
glIndexubv: procedure(var c: Byte); ogl_dll

// Vertex Arrays
glVertexPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); ogl_dll
glNormalPointer: procedure(AType: GLenum; stride: LongInt; var ptr); ogl_dll
glColorPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); ogl_dll
glIndexPointer: procedure(AType: GLenum; stride: LongInt; var ptr); ogl_dll
glTexCoordPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); ogl_dll
glEdgeFlagPointer: procedure(stride: LongInt; var ptr); ogl_dll
glGetPointerv: procedure(pname: GLenum; var params: Pointer); ogl_dll
glArrayElement: procedure(i: LongInt); ogl_dll
glDrawArrays: procedure(mode: GLenum; first, count: LongInt); ogl_dll
glDrawElements: procedure(mode: GLenum; count: Integer; AType: GLenum; var indices); ogl_dll
glInterleavedArrays: procedure(format: GLenum; stride: LongInt; var pointer); ogl_dll

// Texture Mapping
glGenTextures: procedure(n: LongInt; var textures: LongWord); ogl_dll
glDeleteTextures: procedure(n: LongInt; var textures: LongWord); ogl_dll
glBindTexture: procedure(target: GLenum; texture: LongWord); ogl_dll
glPrioritizeTextures: procedure(n: LongInt; var textures: LongWord; var priorities: GLclampf); ogl_dll
glAreTexturesResident: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; ogl_dll
glIsTexture: function(texture: LongWord): Boolean; ogl_dll
glTexSubImage1D: procedure(target: GLenum; level, xoffset, width: LongInt; format, AType: GLenum; var pixels); ogl_dll
glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, width, height: LongInt; format, AType: GLenum; var pixels); ogl_dll
glCopyTexImage1D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, border: LongInt); ogl_dll
glCopyTexImage2D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, height, border: LongInt); ogl_dll
glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y, width: LongInt); ogl_dll
glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y, width, height: LongInt); ogl_dll

{$ENDIF GL1_1}

{$IFDEF GL1_2}
var
glDrawRangeElements: procedure(mode: GLenum; AStart, AEnd: LongWord; count: LongInt; AType: GLenum; var indices); ogl_dll
glTexImage3D: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLEnum; var pixels); ogl_dll
glTexSubImage3D: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLEnum; var pixels); ogl_dll
glCopyTexSubImage3D: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, x, y, width, height: LongInt); ogl_dll
{$ENDIF GL1_2}


// -------------------------------------------------------
//   GL Extensions
// -------------------------------------------------------

{$IFDEF EXTENSIONS}
var

// === 1.0 Extensions ===

// GL_EXT_blend_minmax
glBlendEquationEXT: procedure(mode: GLenum); ogl_dll

// GL_EXT_blend_color
glBlendColorEXT: procedure(red, green, blue, alpha: GLclampf); ogl_dll

// GL_EXT_polygon_offset
glPolygonOffsetEXT: procedure(factor, bias: Single); ogl_dll

// GL_EXT_vertex_array
glVertexPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); ogl_dll
glNormalPointerEXT: procedure(AType: GLenum; stride, count: LongInt; var ptr); ogl_dll
glColorPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); ogl_dll
glIndexPointerEXT: procedure(AType: GLenum; stride, count: LongInt; var ptr); ogl_dll
glTexCoordPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); ogl_dll
glEdgeFlagPointerEXT: procedure(stride, count: LongInt; var ptr: Boolean); ogl_dll
glGetPointervEXT: procedure(pname: GLenum; var params: Pointer); ogl_dll
glArrayElementEXT: procedure(i: LongInt); ogl_dll
glDrawArraysEXT: procedure(mode: GLEnum; first, count: LongInt); ogl_dll

// GL_EXT_texture_object
glGenTexturesEXT: procedure(n: LongInt; var textures: LongWord); ogl_dll
glDeleteTexturesEXT: procedure(n: LongInt; var textures: LongWord); ogl_dll
glBindTextureEXT: procedure(target: GLenum; texture: LongWord); ogl_dll
glPrioritizeTexturesEXT: procedure(n: LongInt; var textures: LongWord; var priorities: GLClampf); ogl_dll
glAreTexturesResidentEXT: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; ogl_dll
glIsTextureEXT: function(texture: LongWord): Boolean; ogl_dll

// GL_EXT_texture3D
glTexImage3DEXT: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLenum; var pixels); ogl_dll
glTexSubImage3DEXT: procedure(target: GLenum; level, xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLenum; var pixels); ogl_dll
glCopyTexSubImage3DEXT: procedure(target: GLenum; level, xoffset, yoffset, zoffset, x, y, width, height: LongInt); ogl_dll

// GL_EXT_color_table
glColorTableEXT: procedure(target, internalformat: GLenum; width: LongInt; format, AType: GLenum; var table); ogl_dll
glColorSubTableEXT: procedure(target: GLenum; start, count: LongInt; format, AType: GLEnum; var data); ogl_dll
glGetColorTableEXT: procedure(target, format, AType: GLenum; var table); ogl_dll
glGetColorTableParameterfvEXT: procedure(target, pname: GLenum; var params: Single); ogl_dll
glGetColorTableParameterivEXT: procedure(target, pname: GLenum; var params: LongInt); ogl_dll

{$ENDIF EXTENSIONS}

// library dependent extensions

{$IFDEF SGI_EXTENSIONS}
var

// GL_SGIS_multitexture
glMultiTexCoord1dSGIS: procedure(target: GLenum; s: Double); ogl_dll
glMultiTexCoord1dvSGIS: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord1fSGIS: procedure(target: GLenum; s: Single); ogl_dll
glMultiTexCoord1fvSGIS: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord1iSGIS: procedure(target: GLenum; s: LongInt); ogl_dll
glMultiTexCoord1ivSGIS: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord1sSGIS: procedure(target: GLenum; s: ShortInt); ogl_dll
glMultiTexCoord1svSGIS: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoord2dSGIS: procedure(target: GLenum; s, t: Double); ogl_dll
glMultiTexCoord2dvSGIS: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord2fSGIS: procedure(target: GLenum; s, t: Single); ogl_dll
glMultiTexCoord2fvSGIS: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord2iSGIS: procedure(target: GLenum; s, t: LongInt); ogl_dll
glMultiTexCoord2ivSGIS: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord2sSGIS: procedure(target: GLenum; s, t: ShortInt); ogl_dll
glMultiTexCoord2svSGIS: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoord3dSGIS: procedure(target: GLenum; s, t, r: Double); ogl_dll
glMultiTexCoord3dvSGIS: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord3fSGIS: procedure(target: GLenum; s, t, r: Single); ogl_dll
glMultiTexCoord3fvSGIS: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord3iSGIS: procedure(target: GLenum; s, t, r: LongInt); ogl_dll
glMultiTexCoord3ivSGIS: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord3sSGIS: procedure(target: GLenum; s, t, r: ShortInt); ogl_dll
glMultiTexCoord3svSGIS: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoord4dSGIS: procedure(target: GLenum; s, t, r, q: Double); ogl_dll
glMultiTexCoord4dvSGIS: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord4fSGIS: procedure(target: GLenum; s, t, r, q: Single); ogl_dll
glMultiTexCoord4fvSGIS: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord4iSGIS: procedure(target: GLenum; s, t, r, q: LongInt); ogl_dll
glMultiTexCoord4ivSGIS: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord4sSGIS: procedure(target: GLenum; s, t, r, q: ShortInt); ogl_dll
glMultiTexCoord4svSGIS: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoordPointerSGIS: procedure(target: GLenum; size: LongInt; AType: GLEnum; stride: LongInt; var APointer); ogl_dll
glSelectTextureSGIS: procedure(target: GLenum); ogl_dll
glSelectTextureCoordSetSGIS: procedure(target: GLenum); ogl_dll

// GL_EXT_multitexture
glMultiTexCoord1dEXT: procedure(target: GLenum; s: Double); ogl_dll
glMultiTexCoord1dvEXT: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord1fEXT: procedure(target: GLenum; s: Single); ogl_dll
glMultiTexCoord1fvEXT: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord1iEXT: procedure(target: GLenum; s: LongInt); ogl_dll
glMultiTexCoord1ivEXT: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord1sEXT: procedure(target: GLenum; s: ShortInt); ogl_dll
glMultiTexCoord1svEXT: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoord2dEXT: procedure(target: GLenum; s, t: Double); ogl_dll
glMultiTexCoord2dvEXT: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord2fEXT: procedure(target: GLenum; s, t: Single); ogl_dll
glMultiTexCoord2fvEXT: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord2iEXT: procedure(target: GLenum; s, t: LongInt); ogl_dll
glMultiTexCoord2ivEXT: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord2sEXT: procedure(target: GLenum; s, t: ShortInt); ogl_dll
glMultiTexCoord2svEXT: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoord3dEXT: procedure(target: GLenum; s, t, r: Double); ogl_dll
glMultiTexCoord3dvEXT: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord3fEXT: procedure(target: GLenum; s, t, r: Single); ogl_dll
glMultiTexCoord3fvEXT: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord3iEXT: procedure(target: GLenum; s, t, r: LongInt); ogl_dll
glMultiTexCoord3ivEXT: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord3sEXT: procedure(target: GLenum; s, t, r: ShortInt); ogl_dll
glMultiTexCoord3svEXT: procedure(target: GLenum; var v: ShortInt); ogl_dll
glMultiTexCoord4dEXT: procedure(target: GLenum; s, t, r, q: Double); ogl_dll
glMultiTexCoord4dvEXT: procedure(target: GLenum; var v: Double); ogl_dll
glMultiTexCoord4fEXT: procedure(target: GLenum; s, t, r, q: Single); ogl_dll
glMultiTexCoord4fvEXT: procedure(target: GLenum; var v: Single); ogl_dll
glMultiTexCoord4iEXT: procedure(target: GLenum; s, t, r, q: LongInt); ogl_dll
glMultiTexCoord4ivEXT: procedure(target: GLenum; var v: LongInt); ogl_dll
glMultiTexCoord4sEXT: procedure(target: GLenum; s, t, r, q: ShortInt); ogl_dll
glMultiTexCoord4svEXT: procedure(target: GLenum; var v: ShortInt); ogl_dll
glInterleavedTextureCoordSetsEXT: procedure(factor: LongInt); ogl_dll
glSelectTextureEXT: procedure(target: GLenum); ogl_dll
glSelectTextureCoordSetEXT: procedure(target: GLenum); ogl_dll
glSelectTextureTransformEXT: procedure(target: GLenum); ogl_dll

// GL_EXT_point_parameters
glPointParameterfEXT: procedure(pname: GLenum; param: Single); ogl_dll
glPointParameterfvEXT: procedure(pname: GLenum; var params: Single); ogl_dll

{$ENDIF SGI_EXTENSIONS}

{$ifdef MESA}
var
// GL_MESA_window_pos
glWindowPos2iMESA: procedure(x, y: LongInt); ogl_dll
glWindowPos2sMESA: procedure(x, y: ShortInt); ogl_dll
glWindowPos2fMESA: procedure(x, y: Single); ogl_dll
glWindowPos2dMESA: procedure(x, y: Double); ogl_dll
glWindowPos2ivMESA: procedure(var p: LongInt); ogl_dll
glWindowPos2svMESA: procedure(var p: ShortInt); ogl_dll
glWindowPos2fvMESA: procedure(var p: Single); ogl_dll
glWindowPos2dvMESA: procedure(var p: Double); ogl_dll
glWindowPos3iMESA: procedure(x, y, z: LongInt); ogl_dll
glWindowPos3sMESA: procedure(x, y, z: ShortInt); ogl_dll
glWindowPos3fMESA: procedure(x, y, z: Single); ogl_dll
glWindowPos3dMESA: procedure(x, y, z: Double); ogl_dll
glWindowPos3ivMESA: procedure(var p: LongInt); ogl_dll
glWindowPos3svMESA: procedure(var p: ShortInt); ogl_dll
glWindowPos3fvMESA: procedure(var p: Single); ogl_dll
glWindowPos3dvMESA: procedure(var p: Double); ogl_dll
glWindowPos4iMESA: procedure(x, y, z, w: LongInt); ogl_dll
glWindowPos4sMESA: procedure(x, y, z, w: ShortInt); ogl_dll
glWindowPos4fMESA: procedure(x, y, z, w: Single); ogl_dll
glWindowPos4dMESA: procedure(x, y, z, w: Double); ogl_dll
glWindowPos4ivMESA: procedure(var p: LongInt); ogl_dll
glWindowPos4svMESA: procedure(var p: ShortInt); ogl_dll
glWindowPos4fvMESA: procedure(var p: Single); ogl_dll
glWindowPos4dvMESA: procedure(var p: Double); ogl_dll

// GL_MESA_resize_buffers
glResizeBuffersMESA: procedure; ogl_dll
{$endif MESA}


// =======================================================
// =======================================================

implementation

type
  HInstance = LongWord;

var
  libGL : HInstance;

function GetProc(handle: HInstance; name: PChar): Pointer;
begin
  Result := GetProcAddress(handle, name);
  if (Result = nil) and GLDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

function InitGLFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGL := LoadLibrary(libname);
  if libGL = 0 then exit;

{$ifdef GL1_0}
// Miscellaneous
  glClearIndex := GetProc(libGL, 'glClearIndex');
  glClearColor := GetProc(libGL, 'glClearColor');
  glClear := GetProc(libGL, 'glClear');
  glIndexMask := GetProc(libGL, 'glIndexMask');
  glColorMask := GetProc(libGL, 'glColorMask');
  glAlphaFunc := GetProc(libGL, 'glAlphaFunc');
  glBlendFunc := GetProc(libGL, 'glBlendFunc');
  glLogicOp := GetProc(libGL, 'glLogicOp');
  glCullFace := GetProc(libGL, 'glCullFace');
  glFrontFace := GetProc(libGL, 'glFrontFace');
  glPointSize := GetProc(libGL, 'glPointSize');
  glLineWidth := GetProc(libGL, 'glLineWidth');
  glLineStipple := GetProc(libGL, 'glLineStipple');
  glPolygonMode := GetProc(libGL, 'glPolygonMode');
  glPolygonOffset := GetProc(libGL, 'glPolygonOffset');
  glPolygonStipple := GetProc(libGL, 'glPolygonStipple');
  glGetPolygonStipple := GetProc(libGL, 'glGetPolygonStipple');
  glEdgeFlag := GetProc(libGL, 'glEdgeFlag');
  glEdgeFlagv := GetProc(libGL, 'glEdgeFlagv');
  glScissor := GetProc(libGL, 'glScissor');
  glClipPlane := GetProc(libGL, 'glClipPlane');
  glGetClipPlane := GetProc(libGL, 'glGetClipPlane');
  glDrawBuffer := GetProc(libGL, 'glDrawBuffer');
  glReadBuffer := GetProc(libGL, 'glReadBuffer');
  glEnable := GetProc(libGL, 'glEnable');
  glDisable := GetProc(libGL, 'glDisable');
  glIsEnabled := GetProc(libGL, 'glIsEnabled');
  glGetBooleanv := GetProc(libGL, 'glGetBooleanv');
  glGetDoublev := GetProc(libGL, 'glGetDoublev');
  glGetFloatv := GetProc(libGL, 'glGetFloatv');
  glGetIntegerv := GetProc(libGL, 'glGetIntegerv');
  glPushAttrib := GetProc(libGL, 'glPushAttrib');
  glPopAttrib := GetProc(libGL, 'glPopAttrib');
  glRenderMode := GetProc(libGL, 'glRenderMode');
  glGetError := GetProc(libGL, 'glGetError');
  glGetString := GetProc(libGL, 'glGetString');
  glFinish := GetProc(libGL, 'glFinish');
  glFlush := GetProc(libGL, 'glFlush');
  glHint := GetProc(libGL, 'glHint');
// Depth Buffer
  glClearDepth := GetProc(libGL, 'glClearDepth');
  glDepthFunc := GetProc(libGL, 'glDepthFunc');
  glDepthMask := GetProc(libGL, 'glDepthMask');
  glDepthRange := GetProc(libGL, 'glDepthRange');
// Accumulation Buffer
  glClearAccum := GetProc(libGL, 'glClearAccum');
  glAccum := GetProc(libGL, 'glAccum');
// Tranformation
  glMatrixMode := GetProc(libGL, 'glMatrixMode');
  glOrtho := GetProc(libGL, 'glOrtho');
  glFrustum := GetProc(libGL, 'glFrustum');
  glViewport := GetProc(libGL, 'glViewport');
  glPushMatrix := GetProc(libGL, 'glPushMatrix');
  glPopMatrix := GetProc(libGL, 'glPopMatrix');
  glLoadIdentity := GetProc(libGL, 'glLoadIdentity');
  glLoadMatrixd := GetProc(libGL, 'glLoadMatrixd');
  glLoadMatrixf := GetProc(libGL, 'glLoadMatrixf');
  glMultMatrixd := GetProc(libGL, 'glMultMatrixd');
  glMultMatrixf := GetProc(libGL, 'glMultMatrixf');
  glRotated := GetProc(libGL, 'glRotated');
  glRotatef := GetProc(libGL, 'glRotatef');
  glScaled := GetProc(libGL, 'glScaled');
  glScalef := GetProc(libGL, 'glScalef');
  glTranslated := GetProc(libGL, 'glTranslated');
  glTranslatef := GetProc(libGL, 'glTranslatef');
// Display Lists
  glIsList := GetProc(libGL, 'glIsList');
  glDeleteLists := GetProc(libGL, 'glDeleteLists');
  glGenLists := GetProc(libGL, 'glGenLists');
  glNewList := GetProc(libGL, 'glNewList');
  glEndList := GetProc(libGL, 'glEndList');
  glCallList := GetProc(libGL, 'glCallList');
  glCallLists := GetProc(libGL, 'glCallLists');
  glListBase := GetProc(libGL, 'glListBase');
// Drawing Functions
  glBegin := GetProc(libGL, 'glBegin');
  glEnd := GetProc(libGL, 'glEnd');
  glVertex2d := GetProc(libGL, 'glVertex2d');
  glVertex2f := GetProc(libGL, 'glVertex2f');
  glVertex2i := GetProc(libGL, 'glVertex2i');
  glVertex2s := GetProc(libGL, 'glVertex2s');
  glVertex3d := GetProc(libGL, 'glVertex3d');
  glVertex3f := GetProc(libGL, 'glVertex3f');
  glVertex3i := GetProc(libGL, 'glVertex3i');
  glVertex3s := GetProc(libGL, 'glVertex3s');
  glVertex4d := GetProc(libGL, 'glVertex4d');
  glVertex4f := GetProc(libGL, 'glVertex4f');
  glVertex4i := GetProc(libGL, 'glVertex4i');
  glVertex4s := GetProc(libGL, 'glVertex4s');
  glVertex2dv := GetProc(libGL, 'glVertex2dv');
  glVertex2fv := GetProc(libGL, 'glVertex2fv');
  glVertex2iv := GetProc(libGL, 'glVertex2iv');
  glVertex2sv := GetProc(libGL, 'glVertex2sv');
  glVertex3dv := GetProc(libGL, 'glVertex3dv');
  glVertex3fv := GetProc(libGL, 'glVertex3fv');
  glVertex3iv := GetProc(libGL, 'glVertex3iv');
  glVertex3sv := GetProc(libGL, 'glVertex3sv');
  glVertex4dv := GetProc(libGL, 'glVertex4dv');
  glVertex4fv := GetProc(libGL, 'glVertex4fv');
  glVertex4iv := GetProc(libGL, 'glVertex4iv');
  glVertex4sv := GetProc(libGL, 'glVertex4sv');
  glNormal3b := GetProc(libGL, 'glNormal3b');
  glNormal3d := GetProc(libGL, 'glNormal3d');
  glNormal3f := GetProc(libGL, 'glNormal3f');
  glNormal3i := GetProc(libGL, 'glNormal3i');
  glNormal3s := GetProc(libGL, 'glNormal3s');
  glNormal3bv := GetProc(libGL, 'glNormal3bv');
  glNormal3dv := GetProc(libGL, 'glNormal3dv');
  glNormal3fv := GetProc(libGL, 'glNormal3fv');
  glNormal3iv := GetProc(libGL, 'glNormal3iv');
  glNormal3sv := GetProc(libGL, 'glNormal3sv');
  glIndexd := GetProc(libGL, 'glIndexd');
  glIndexf := GetProc(libGL, 'glIndexf');
  glIndexi := GetProc(libGL, 'glIndexi');
  glIndexs := GetProc(libGL, 'glIndexs');
  glIndexdv := GetProc(libGL, 'glIndexdv');
  glIndexfv := GetProc(libGL, 'glIndexfv');
  glIndexiv := GetProc(libGL, 'glIndexiv');
  glIndexsv := GetProc(libGL, 'glIndexsv');
  glColor3b := GetProc(libGL, 'glColor3b');
  glColor3d := GetProc(libGL, 'glColor3d');
  glColor3f := GetProc(libGL, 'glColor3f');
  glColor3i := GetProc(libGL, 'glColor3i');
  glColor3s := GetProc(libGL, 'glColor3s');
  glColor3ub := GetProc(libGL, 'glColor3ub');
  glColor3ui := GetProc(libGL, 'glColor3ui');
  glColor3us := GetProc(libGL, 'glColor3us');
  glColor4b := GetProc(libGL, 'glColor4b');
  glColor4d := GetProc(libGL, 'glColor4d');
  glColor4f := GetProc(libGL, 'glColor4f');
  glColor4i := GetProc(libGL, 'glColor4i');
  glColor4s := GetProc(libGL, 'glColor4s');
  glColor4ub := GetProc(libGL, 'glColor4ub');
  glColor4ui := GetProc(libGL, 'glColor4ui');
  glColor4us := GetProc(libGL, 'glColor4us');
  glColor3bv := GetProc(libGL, 'glColor3bv');
  glColor3dv := GetProc(libGL, 'glColor3dv');
  glColor3fv := GetProc(libGL, 'glColor3fv');
  glColor3iv := GetProc(libGL, 'glColor3iv');
  glColor3sv := GetProc(libGL, 'glColor3sv');
  glColor3ubv := GetProc(libGL, 'glColor3ubv');
  glColor3uiv := GetProc(libGL, 'glColor3uiv');
  glColor3usv := GetProc(libGL, 'glColor3usv');
  glColor4bv := GetProc(libGL, 'glColor4bv');
  glColor4dv := GetProc(libGL, 'glColor4dv');
  glColor4fv := GetProc(libGL, 'glColor4fv');
  glColor4iv := GetProc(libGL, 'glColor4iv');
  glColor4sv := GetProc(libGL, 'glColor4sv');
  glColor4ubv := GetProc(libGL, 'glColor4ubv');
  glColor4uiv := GetProc(libGL, 'glColor4uiv');
  glColor4usv := GetProc(libGL, 'glColor4usv');
  glTexCoord1d := GetProc(libGL, 'glTexCoord1d');
  glTexCoord1f := GetProc(libGL, 'glTexCoord1f');
  glTexCoord1i := GetProc(libGL, 'glTexCoord1i');
  glTexCoord1s := GetProc(libGL, 'glTexCoord1s');
  glTexCoord2d := GetProc(libGL, 'glTexCoord2d');
  glTexCoord2f := GetProc(libGL, 'glTexCoord2f');
  glTexCoord2i := GetProc(libGL, 'glTexCoord2i');
  glTexCoord2s := GetProc(libGL, 'glTexCoord2s');
  glTexCoord3d := GetProc(libGL, 'glTexCoord3d');
  glTexCoord3f := GetProc(libGL, 'glTexCoord3f');
  glTexCoord3i := GetProc(libGL, 'glTexCoord3i');
  glTexCoord3s := GetProc(libGL, 'glTexCoord3s');
  glTexCoord4d := GetProc(libGL, 'glTexCoord4d');
  glTexCoord4f := GetProc(libGL, 'glTexCoord4f');
  glTexCoord4i := GetProc(libGL, 'glTexCoord4i');
  glTexCoord4s := GetProc(libGL, 'glTexCoord4s');
  glTexCoord1dv := GetProc(libGL, 'glTexCoord1dv');
  glTexCoord1fv := GetProc(libGL, 'glTexCoord1fv');
  glTexCoord1iv := GetProc(libGL, 'glTexCoord1iv');
  glTexCoord1sv := GetProc(libGL, 'glTexCoord1sv');
  glTexCoord2dv := GetProc(libGL, 'glTexCoord2dv');
  glTexCoord2fv := GetProc(libGL, 'glTexCoord2fv');
  glTexCoord2iv := GetProc(libGL, 'glTexCoord2iv');
  glTexCoord2sv := GetProc(libGL, 'glTexCoord2sv');
  glTexCoord3dv := GetProc(libGL, 'glTexCoord3dv');
  glTexCoord3fv := GetProc(libGL, 'glTexCoord3fv');
  glTexCoord3iv := GetProc(libGL, 'glTexCoord3iv');
  glTexCoord3sv := GetProc(libGL, 'glTexCoord3sv');
  glTexCoord4dv := GetProc(libGL, 'glTexCoord4dv');
  glTexCoord4fv := GetProc(libGL, 'glTexCoord4fv');
  glTexCoord4iv := GetProc(libGL, 'glTexCoord4iv');
  glTexCoord4sv := GetProc(libGL, 'glTexCoord4sv');
  glRasterPos2d := GetProc(libGL, 'glRasterPos2d');
  glRasterPos2f := GetProc(libGL, 'glRasterPos2f');
  glRasterPos2i := GetProc(libGL, 'glRasterPos2i');
  glRasterPos2s := GetProc(libGL, 'glRasterPos2s');
  glRasterPos3d := GetProc(libGL, 'glRasterPos3d');
  glRasterPos3f := GetProc(libGL, 'glRasterPos3f');
  glRasterPos3i := GetProc(libGL, 'glRasterPos3i');
  glRasterPos3s := GetProc(libGL, 'glRasterPos3s');
  glRasterPos4d := GetProc(libGL, 'glRasterPos4d');
  glRasterPos4f := GetProc(libGL, 'glRasterPos4f');
  glRasterPos4i := GetProc(libGL, 'glRasterPos4i');
  glRasterPos4s := GetProc(libGL, 'glRasterPos4s');
  glRasterPos2dv := GetProc(libGL, 'glRasterPos2dv');
  glRasterPos2fv := GetProc(libGL, 'glRasterPos2fv');
  glRasterPos2iv := GetProc(libGL, 'glRasterPos2iv');
  glRasterPos2sv := GetProc(libGL, 'glRasterPos2sv');
  glRasterPos3dv := GetProc(libGL, 'glRasterPos3dv');
  glRasterPos3fv := GetProc(libGL, 'glRasterPos3fv');
  glRasterPos3iv := GetProc(libGL, 'glRasterPos3iv');
  glRasterPos3sv := GetProc(libGL, 'glRasterPos3sv');
  glRasterPos4dv := GetProc(libGL, 'glRasterPos4dv');
  glRasterPos4fv := GetProc(libGL, 'glRasterPos4fv');
  glRasterPos4iv := GetProc(libGL, 'glRasterPos4iv');
  glRasterPos4sv := GetProc(libGL, 'glRasterPos4sv');
  glRectd := GetProc(libGL, 'glRectd');
  glRectf := GetProc(libGL, 'glRectf');
  glRecti := GetProc(libGL, 'glRecti');
  glRects := GetProc(libGL, 'glRects');
  glRectdv := GetProc(libGL, 'glRectdv');
  glRectfv := GetProc(libGL, 'glRectfv');
  glRectiv := GetProc(libGL, 'glRectiv');
  glRectsv := GetProc(libGL, 'glRectsv');
// Lighting
  glShadeModel := GetProc(libGL, 'glShadeModel');
  glLightf := GetProc(libGL, 'glLightf');
  glLighti := GetProc(libGL, 'glLighti');
  glLightfv := GetProc(libGL, 'glLightfv');
  glLightiv := GetProc(libGL, 'glLightiv');
  glGetLightfv := GetProc(libGL, 'glGetLightfv');
  glGetLightiv := GetProc(libGL, 'glGetLightiv');
  glLightModelf := GetProc(libGL, 'glLightModelf');
  glLightModeli := GetProc(libGL, 'glLightModeli');
  glLightModelfv := GetProc(libGL, 'glLightModelfv');
  glLightModeliv := GetProc(libGL, 'glLightModeliv');
  glMaterialf := GetProc(libGL, 'glMaterialf');
  glMateriali := GetProc(libGL, 'glMateriali');
  glMaterialfv := GetProc(libGL, 'glMaterialfv');
  glMaterialiv := GetProc(libGL, 'glMaterialiv');
  glGetMaterialfv := GetProc(libGL, 'glGetMaterialfv');
  glGetMaterialiv := GetProc(libGL, 'glGetMaterialiv');
  glColorMaterial := GetProc(libGL, 'glColorMaterial');
// Raster Functions
  glPixelZoom := GetProc(libGL, 'glPixelZoom');
  glPixelStoref := GetProc(libGL, 'glPixelStoref');
  glPixelStorei := GetProc(libGL, 'glPixelStorei');
  glPixelTransferf := GetProc(libGL, 'glPixelTransferf');
  glPixelTransferi := GetProc(libGL, 'glPixelTransferi');
  glPixelMapfv := GetProc(libGL, 'glPixelMapfv');
  glPixelMapuiv := GetProc(libGL, 'glPixelMapuiv');
  glPixelMapusv := GetProc(libGL, 'glPixelMapusv');
  glGetPixelMapfv := GetProc(libGL, 'glGetPixelMapfv');
  glGetPixelMapuiv := GetProc(libGL, 'glGetPixelMapuiv');
  glGetPixelMapusv := GetProc(libGL, 'glGetPixelMapusv');
  glBitmap := GetProc(libGL, 'glBitmap');
  glReadPixels := GetProc(libGL, 'glReadPixels');
  glDrawPixels := GetProc(libGL, 'glDrawPixels');
  glCopyPixels := GetProc(libGL, 'glCopyPixels');
// Stenciling
  glStencilFunc := GetProc(libGL, 'glStencilFunc');
  glStencilMask := GetProc(libGL, 'glStencilMask');
  glStencilOp := GetProc(libGL, 'glStencilOp');
  glClearStencil := GetProc(libGL, 'glClearStencil');
// Texture Mapping
  glTexGend := GetProc(libGL, 'glTexGend');
  glTexGenf := GetProc(libGL, 'glTexGenf');
  glTexGeni := GetProc(libGL, 'glTexGeni');
  glTexGendv := GetProc(libGL, 'glTexGendv');
  glTexGenfv := GetProc(libGL, 'glTexGenfv');
  glTexGeniv := GetProc(libGL, 'glTexGeniv');
  glGetTexGendv := GetProc(libGL, 'glGetTexGendv');
  glGetTexGenfv := GetProc(libGL, 'glGetTexGenfv');
  glGetTexGeniv := GetProc(libGL, 'glGetTexGeniv');
  glTexEnvf := GetProc(libGL, 'glTexEnvf');
  glTexEnvi := GetProc(libGL, 'glTexEnvi');
  glTexEnvfv := GetProc(libGL, 'glTexEnvfv');
  glTexEnviv := GetProc(libGL, 'glTexEnviv');
  glGetTexEnvfv := GetProc(libGL, 'glGetTexEnvfv');
  glGetTexEnviv := GetProc(libGL, 'glGetTexEnviv');
  glTexParameterf := GetProc(libGL, 'glTexParameterf');
  glTexParameteri := GetProc(libGL, 'glTexParameteri');
  glTexParameterfv := GetProc(libGL, 'glTexParameterfv');
  glTexParameteriv := GetProc(libGL, 'glTexParameteriv');
  glGetTexParameterfv := GetProc(libGL, 'glGetTexParameterfv');
  glGetTexParameteriv := GetProc(libGL, 'glGetTexParameteriv');
  glGetTexLevelParameterfv := GetProc(libGL, 'glGetTexLevelParameterfv');
  glGetTexLevelParameteriv := GetProc(libGL, 'glGetTexLevelParameteriv');
  glTexImage1D := GetProc(libGL, 'glTexImage1D');
  glTexImage2D := GetProc(libGL, 'glTexImage2D');
  glGetTexImage := GetProc(libGL, 'glGetTexImage');
// Evaluators
  glMap1d := GetProc(libGL, 'glMap1d');
  glMap1f := GetProc(libGL, 'glMap1f');
  glMap2d := GetProc(libGL, 'glMap2d');
  glMap2f := GetProc(libGL, 'glMap2f');
  glGetMapdv := GetProc(libGL, 'glGetMapdv');
  glGetMapfv := GetProc(libGL, 'glGetMapfv');
  glGetMapiv := GetProc(libGL, 'glGetMapiv');
  glEvalCoord1d := GetProc(libGL, 'glEvalCoord1d');
  glEvalCoord1f := GetProc(libGL, 'glEvalCoord1f');
  glEvalCoord1dv := GetProc(libGL, 'glEvalCoord1dv');
  glEvalCoord1fv := GetProc(libGL, 'glEvalCoord1fv');
  glEvalCoord2d := GetProc(libGL, 'glEvalCoord2d');
  glEvalCoord2f := GetProc(libGL, 'glEvalCoord2f');
  glEvalCoord2dv := GetProc(libGL, 'glEvalCoord2dv');
  glEvalCoord2fv := GetProc(libGL, 'glEvalCoord2fv');
  glMapGrid1d := GetProc(libGL, 'glMapGrid1d');
  glMapGrid1f := GetProc(libGL, 'glMapGrid1f');
  glMapGrid2d := GetProc(libGL, 'glMapGrid2d');
  glMapGrid2f := GetProc(libGL, 'glMapGrid2f');
  glEvalPoint1 := GetProc(libGL, 'glEvalPoint1');
  glEvalPoint2 := GetProc(libGL, 'glEvalPoint2');
  glEvalMesh1 := GetProc(libGL, 'glEvalMesh1');
  glEvalMesh2 := GetProc(libGL, 'glEvalMesh2');
// Fog
  glFogf := GetProc(libGL, 'glFogf');
  glFogi := GetProc(libGL, 'glFogi');
  glFogfv := GetProc(libGL, 'glFogfv');
  glFogiv := GetProc(libGL, 'glFogiv');
// Selection and Feedback
  glFeedbackBuffer := GetProc(libGL, 'glFeedbackBuffer');
  glPassThrough := GetProc(libGL, 'glPassThrough');
  glSelectBuffer := GetProc(libGL, 'glSelectBuffer');
  glInitNames := GetProc(libGL, 'glInitNames');
  glLoadName := GetProc(libGL, 'glLoadName');
  glPushName := GetProc(libGL, 'glPushName');
  glPopName := GetProc(libGL, 'glPopName');
{$endif GL1_0}

{$ifdef GL1_1}
// Miscellaneous
  glEnableClientState := GetProc(libGL, 'glEnableClientState');
  glDisableClientState := GetProc(libGL, 'glDisableClientState');
  glPushClientAttrib := GetProc(libGL, 'glPushClientAttrib');
  glPopClientAttrib := GetProc(libGL, 'glPopClientAttrib');
// Drawing Functions
  glIndexub := GetProc(libGL, 'glIndexub');
  glIndexubv := GetProc(libGL, 'glIndexubv');
// Vertex Arrays
  glVertexPointer := GetProc(libGL, 'glVertexPointer');
  glNormalPointer := GetProc(libGL, 'glNormalPointer');
  glColorPointer := GetProc(libGL, 'glColorPointer');
  glIndexPointer := GetProc(libGL, 'glIndexPointer');
  glTexCoordPointer := GetProc(libGL, 'glTexCoordPointer');
  glEdgeFlagPointer := GetProc(libGL, 'glEdgeFlagPointer');
  glGetPointerv := GetProc(libGL, 'glGetPointerv');
  glArrayElement := GetProc(libGL, 'glArrayElement');
  glDrawArrays := GetProc(libGL, 'glDrawArrays');
  glDrawElements := GetProc(libGL, 'glDrawElements');
  glInterleavedArrays := GetProc(libGL, 'glInterleavedArrays');
// Texture Mapping
  glGenTextures := GetProc(libGL, 'glGenTextures');
  glDeleteTextures := GetProc(libGL, 'glDeleteTextures');
  glBindTexture := GetProc(libGL, 'glBindTexture');
  glPrioritizeTextures := GetProc(libGL, 'glPrioritizeTextures');
  glAreTexturesResident := GetProc(libGL, 'glAreTexturesResident');
  glIsTexture := GetProc(libGL, 'glIsTexture');
  glTexSubImage1D := GetProc(libGL, 'glTexSubImage1D');
  glTexSubImage2D := GetProc(libGL, 'glTexSubImage2D');
  glCopyTexImage1D := GetProc(libGL, 'glCopyTexImage1D');
  glCopyTexImage2D := GetProc(libGL, 'glCopyTexImage2D');
  glCopyTexSubImage1D := GetProc(libGL, 'glCopyTexSubImage1D');
  glCopyTexSubImage2D := GetProc(libGL, 'glCopyTexSubImage2D');
{$endif GL1_1}

{$ifdef GL1_2}
  glDrawRangeElements := GetProc(libGL, 'glDrawRangeElements');
  glTexImage3D := GetProc(libGL, 'glTexImage3D');
  glTexSubImage3D := GetProc(libGL, 'glTexSubImage3D');
  glCopyTexSubImage3D := GetProc(libGL, 'glCopyTexSubImage3D');
{$endif GL1_2}

{$ifdef EXTENSIONS}
// === 1.0 Extensions ===
// GL_EXT_blend_minmax
  glBlendEquationEXT := GetProc(libGL, 'glBlendEquationEXT');
// GL_EXT_blend_color
  glBlendColorEXT := GetProc(libGL, 'glBlendColorEXT');
// GL_EXT_polygon_offset
  glPolygonOffsetEXT := GetProc(libGL, 'glPolygonOffsetEXT');
// GL_EXT_vertex_array
  glVertexPointerEXT := GetProc(libGL, 'glVertexPointerEXT');
  glNormalPointerEXT := GetProc(libGL, 'glNormalPointerEXT');
  glColorPointerEXT := GetProc(libGL, 'glColorPointerEXT');
  glIndexPointerEXT := GetProc(libGL, 'glIndexPointerEXT');
  glTexCoordPointerEXT := GetProc(libGL, 'glTexCoordPointerEXT');
  glEdgeFlagPointerEXT := GetProc(libGL, 'glEdgeFlagPointerEXT');
  glGetPointervEXT := GetProc(libGL, 'glGetPointervEXT');
  glArrayElementEXT := GetProc(libGL, 'glArrayElementEXT');
  glDrawArraysEXT := GetProc(libGL, 'glDrawArraysEXT');
// GL_EXT_texture_object
  glGenTexturesEXT := GetProc(libGL, 'glGenTexturesEXT');
  glDeleteTexturesEXT := GetProc(libGL, 'glDeleteTexturesEXT');
  glBindTextureEXT := GetProc(libGL, 'glBindTextureEXT');
  glPrioritizeTexturesEXT := GetProc(libGL, 'glPrioritizeTexturesEXT');
  glAreTexturesResidentEXT := GetProc(libGL, 'glAreTexturesResidentEXT');
  glIsTextureEXT := GetProc(libGL, 'glIsTextureEXT');
// GL_EXT_texture3D
  glTexImage3DEXT := GetProc(libGL, 'glTexImage3DEXT');
  glTexSubImage3DEXT := GetProc(libGL, 'glTexSubImage3DEXT');
  glCopyTexSubImage3DEXT := GetProc(libGL, 'glCopyTexSubImage3DEXT');
// GL_EXT_color_table
  glColorTableEXT := GetProc(libGL, 'glColorTableEXT');
  glColorSubTableEXT := GetProc(libGL, 'glColorSubTableEXT');
  glGetColorTableEXT := GetProc(libGL, 'glGetColorTableEXT');
  glGetColorTableParameterfvEXT := GetProc(libGL, 'glGetColorTableParameterfvEXT');
  glGetColorTableParameterivEXT := GetProc(libGL, 'glGetColorTableParameterivEXT');
{$endif EXTENSIONS}

{$ifdef SGI_EXTENSIONS}
// GL_SGIS_multitexture
  glMultiTexCoord1dSGIS := GetProc(libGL, 'glMultiTexCoord1dSGIS');
  glMultiTexCoord1dvSGIS := GetProc(libGL, 'glMultiTexCoord1dvSGIS');
  glMultiTexCoord1fSGIS := GetProc(libGL, 'glMultiTexCoord1fSGIS');
  glMultiTexCoord1fvSGIS := GetProc(libGL, 'glMultiTexCoord1fvSGIS');
  glMultiTexCoord1iSGIS := GetProc(libGL, 'glMultiTexCoord1iSGIS');
  glMultiTexCoord1ivSGIS := GetProc(libGL, 'glMultiTexCoord1ivSGIS');
  glMultiTexCoord1sSGIS := GetProc(libGL, 'glMultiTexCoord1sSGIS');
  glMultiTexCoord1svSGIS := GetProc(libGL, 'glMultiTexCoord1svSGIS');
  glMultiTexCoord2dSGIS := GetProc(libGL, 'glMultiTexCoord2dSGIS');
  glMultiTexCoord2dvSGIS := GetProc(libGL, 'glMultiTexCoord2dvSGIS');
  glMultiTexCoord2fSGIS := GetProc(libGL, 'glMultiTexCoord2fSGIS');
  glMultiTexCoord2fvSGIS := GetProc(libGL, 'glMultiTexCoord2fvSGIS');
  glMultiTexCoord2iSGIS := GetProc(libGL, 'glMultiTexCoord2iSGIS');
  glMultiTexCoord2ivSGIS := GetProc(libGL, 'glMultiTexCoord2ivSGIS');
  glMultiTexCoord2sSGIS := GetProc(libGL, 'glMultiTexCoord2sSGIS');
  glMultiTexCoord2svSGIS := GetProc(libGL, 'glMultiTexCoord2svSGIS');
  glMultiTexCoord3dSGIS := GetProc(libGL, 'glMultiTexCoord3dSGIS');
  glMultiTexCoord3dvSGIS := GetProc(libGL, 'glMultiTexCoord3dvSGIS');
  glMultiTexCoord3fSGIS := GetProc(libGL, 'glMultiTexCoord3fSGIS');
  glMultiTexCoord3fvSGIS := GetProc(libGL, 'glMultiTexCoord3fvSGIS');
  glMultiTexCoord3iSGIS := GetProc(libGL, 'glMultiTexCoord3iSGIS');
  glMultiTexCoord3ivSGIS := GetProc(libGL, 'glMultiTexCoord3ivSGIS');
  glMultiTexCoord3sSGIS := GetProc(libGL, 'glMultiTexCoord3sSGIS');
  glMultiTexCoord3svSGIS := GetProc(libGL, 'glMultiTexCoord3svSGIS');
  glMultiTexCoord4dSGIS := GetProc(libGL, 'glMultiTexCoord4dSGIS');
  glMultiTexCoord4dvSGIS := GetProc(libGL, 'glMultiTexCoord4dvSGIS');
  glMultiTexCoord4fSGIS := GetProc(libGL, 'glMultiTexCoord4fSGIS');
  glMultiTexCoord4fvSGIS := GetProc(libGL, 'glMultiTexCoord4fvSGIS');
  glMultiTexCoord4iSGIS := GetProc(libGL, 'glMultiTexCoord4iSGIS');
  glMultiTexCoord4ivSGIS := GetProc(libGL, 'glMultiTexCoord4ivSGIS');
  glMultiTexCoord4sSGIS := GetProc(libGL, 'glMultiTexCoord4sSGIS');
  glMultiTexCoord4svSGIS := GetProc(libGL, 'glMultiTexCoord4svSGIS');
  glMultiTexCoordPointerSGIS := GetProc(libGL, 'glMultiTexCoordPointerSGIS');
  glSelectTextureSGIS := GetProc(libGL, 'glSelectTextureSGIS');
  glSelectTextureCoordSetSGIS := GetProc(libGL, 'glSelectTextureCoordSetSGIS');
// GL_EXT_multitexture
  glMultiTexCoord1dEXT := GetProc(libGL, 'glMultiTexCoord1dEXT');
  glMultiTexCoord1dvEXT := GetProc(libGL, 'glMultiTexCoord1dvEXT');
  glMultiTexCoord1fEXT := GetProc(libGL, 'glMultiTexCoord1fEXT');
  glMultiTexCoord1fvEXT := GetProc(libGL, 'glMultiTexCoord1fvEXT');
  glMultiTexCoord1iEXT := GetProc(libGL, 'glMultiTexCoord1iEXT');
  glMultiTexCoord1ivEXT := GetProc(libGL, 'glMultiTexCoord1ivEXT');
  glMultiTexCoord1sEXT := GetProc(libGL, 'glMultiTexCoord1sEXT');
  glMultiTexCoord1svEXT := GetProc(libGL, 'glMultiTexCoord1svEXT');
  glMultiTexCoord2dEXT := GetProc(libGL, 'glMultiTexCoord2dEXT');
  glMultiTexCoord2dvEXT := GetProc(libGL, 'glMultiTexCoord2dvEXT');
  glMultiTexCoord2fEXT := GetProc(libGL, 'glMultiTexCoord2fEXT');
  glMultiTexCoord2fvEXT := GetProc(libGL, 'glMultiTexCoord2fvEXT');
  glMultiTexCoord2iEXT := GetProc(libGL, 'glMultiTexCoord2iEXT');
  glMultiTexCoord2ivEXT := GetProc(libGL, 'glMultiTexCoord2ivEXT');
  glMultiTexCoord2sEXT := GetProc(libGL, 'glMultiTexCoord2sEXT');
  glMultiTexCoord2svEXT := GetProc(libGL, 'glMultiTexCoord2svEXT');
  glMultiTexCoord3dEXT := GetProc(libGL, 'glMultiTexCoord3dEXT');
  glMultiTexCoord3dvEXT := GetProc(libGL, 'glMultiTexCoord3dvEXT');
  glMultiTexCoord3fEXT := GetProc(libGL, 'glMultiTexCoord3fEXT');
  glMultiTexCoord3fvEXT := GetProc(libGL, 'glMultiTexCoord3fvEXT');
  glMultiTexCoord3iEXT := GetProc(libGL, 'glMultiTexCoord3iEXT');
  glMultiTexCoord3ivEXT := GetProc(libGL, 'glMultiTexCoord3ivEXT');
  glMultiTexCoord3sEXT := GetProc(libGL, 'glMultiTexCoord3sEXT');
  glMultiTexCoord3svEXT := GetProc(libGL, 'glMultiTexCoord3svEXT');
  glMultiTexCoord4dEXT := GetProc(libGL, 'glMultiTexCoord4dEXT');
  glMultiTexCoord4dvEXT := GetProc(libGL, 'glMultiTexCoord4dvEXT');
  glMultiTexCoord4fEXT := GetProc(libGL, 'glMultiTexCoord4fEXT');
  glMultiTexCoord4fvEXT := GetProc(libGL, 'glMultiTexCoord4fvEXT');
  glMultiTexCoord4iEXT := GetProc(libGL, 'glMultiTexCoord4iEXT');
  glMultiTexCoord4ivEXT := GetProc(libGL, 'glMultiTexCoord4ivEXT');
  glMultiTexCoord4sEXT := GetProc(libGL, 'glMultiTexCoord4sEXT');
  glMultiTexCoord4svEXT := GetProc(libGL, 'glMultiTexCoord4svEXT');
  glInterleavedTextureCoordSetsEXT := GetProc(libGL, 'glInterleavedTextureCoordSetsEXT');
  glSelectTextureEXT := GetProc(libGL, 'glSelectTextureEXT');
  glSelectTextureCoordSetEXT := GetProc(libGL, 'glSelectTextureCoordSetEXT');
  glSelectTextureTransformEXT := GetProc(libGL, 'glSelectTextureTransformEXT');
// GL_EXT_point_parameters
  glPointParameterfEXT := GetProc(libGL, 'glPointParameterfEXT');
  glPointParameterfvEXT := GetProc(libGL, 'glPointParameterfvEXT');
{$endif SGI_EXTENSIONS}

{$ifdef MESA}
// GL_MESA_window_pos
  glWindowPos2iMESA := GetProc(libGL, 'glWindowPos2iMESA');
  glWindowPos2sMESA := GetProc(libGL, 'glWindowPos2sMESA');
  glWindowPos2fMESA := GetProc(libGL, 'glWindowPos2fMESA');
  glWindowPos2dMESA := GetProc(libGL, 'glWindowPos2dMESA');
  glWindowPos2ivMESA := GetProc(libGL, 'glWindowPos2ivMESA');
  glWindowPos2svMESA := GetProc(libGL, 'glWindowPos2svMESA');
  glWindowPos2fvMESA := GetProc(libGL, 'glWindowPos2fvMESA');
  glWindowPos2dvMESA := GetProc(libGL, 'glWindowPos2dvMESA');
  glWindowPos3iMESA := GetProc(libGL, 'glWindowPos3iMESA');
  glWindowPos3sMESA := GetProc(libGL, 'glWindowPos3sMESA');
  glWindowPos3fMESA := GetProc(libGL, 'glWindowPos3fMESA');
  glWindowPos3dMESA := GetProc(libGL, 'glWindowPos3dMESA');
  glWindowPos3ivMESA := GetProc(libGL, 'glWindowPos3ivMESA');
  glWindowPos3svMESA := GetProc(libGL, 'glWindowPos3svMESA');
  glWindowPos3fvMESA := GetProc(libGL, 'glWindowPos3fvMESA');
  glWindowPos3dvMESA := GetProc(libGL, 'glWindowPos3dvMESA');
  glWindowPos4iMESA := GetProc(libGL, 'glWindowPos4iMESA');
  glWindowPos4sMESA := GetProc(libGL, 'glWindowPos4sMESA');
  glWindowPos4fMESA := GetProc(libGL, 'glWindowPos4fMESA');
  glWindowPos4dMESA := GetProc(libGL, 'glWindowPos4dMESA');
  glWindowPos4ivMESA := GetProc(libGL, 'glWindowPos4ivMESA');
  glWindowPos4svMESA := GetProc(libGL, 'glWindowPos4svMESA');
  glWindowPos4fvMESA := GetProc(libGL, 'glWindowPos4fvMESA');
  glWindowPos4dvMESA := GetProc(libGL, 'glWindowPos4dvMESA');
// GL_MESA_resize_buffers
  glResizeBuffersMESA := GetProc(libGL, 'glResizeBuffersMESA');
{$endif MESA}

  GLInitialized := True;
  Result := True;
end;


function InitGL: Boolean;
begin
  Result := InitGLFromLibrary('opengl32.dll');
end;


initialization
  InitGL;
finalization
  if libGL <> 0 then FreeLibrary(libGL);
end.


{
  $Log$
  Revision 1.1.2.1  2000-09-03 22:14:40  peter
    * regenerated

  Revision 1.1  2000/09/03 21:25:45  peter
    * new updated version
    * gtkglarea unit and demo
    * win32 opengl headers
    * morph3d demo

  Revision 1.1  2000/07/13 06:34:17  michael
  + Initial import

  Revision 1.2  2000/05/31 00:34:28  alex
  made templates work

}


{
  $Log$
  Revision 1.1.2.1  2000-09-03 22:14:40  peter
    * regenerated

}
