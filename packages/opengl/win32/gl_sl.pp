{
  Translation of the Mesa GL headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther
  Template for static linking in Win32 environment by Alexander Stohr.

  Original copyright notice:

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

{
  You have to enable Macros (compiler switch "-Sm") for compiling this unit!
  This is necessary for supporting different platforms with different calling
  conventions via a single unit.
}

unit GL_SL;


interface

{$DEFINE GL1_0}
{x$DEFINE GL1_1}
{x$DEFINE GL1_2}
{x$DEFINE MESA}                 {enable if you want to use some special mesa extensions}
{x$DEFINE EXTENSIONS}           {enable if you need one/all of extensions}
{x$DEFINE SGI_EXTENSIONS}       {enable if you need one/all of extensions}

{
  *** Note: ***
  There is currently one importants side effect when doing static linking.

  If you include a function statically than its required to be present
  in the supplied DLLs of your OS and Hardware.
  This means if the DLL is not present your program will not run.
  In Win95 it appears that you will be informined you about
  the name of the first missing symbol while loading the executable.
}

{$MACRO ON}

{$IFDEF Win32}
  {$DEFINE ogl_dll := external 'opengl32.dll'}
  uses Windows;
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}


// =======================================================
//   Unit specific extensions
// =======================================================

// none - no special init required


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
// Miscellaneous
procedure glClearIndex(c: Single); ogl_dll;
procedure glClearColor(red, green, blue, alpha: GLclampf); ogl_dll;
procedure glClear(mask: GLbitfield); ogl_dll;
procedure glIndexMask(mask: LongWord); ogl_dll;
procedure glColorMask(red, green, blue, alpha: GLboolean); ogl_dll;
procedure glAlphaFunc(func: GLenum; ref: GLclampf); ogl_dll;
procedure glBlendFunc(sfactor, dfactor: GLenum); ogl_dll;
procedure glLogicOp(opcode: GLenum); ogl_dll;
procedure glCullFace(mode: GLenum); ogl_dll;
procedure glFrontFace(mode: GLenum); ogl_dll;
procedure glPointSize(size: Single); ogl_dll;
procedure glLineWidth(width: Single); ogl_dll;
procedure glLineStipple(factor: LongInt; pattern: Word); ogl_dll;
procedure glPolygonMode(face, mode: GLenum); ogl_dll;
procedure glPolygonOffset(factor, units: Single); ogl_dll;
procedure glPolygonStipple(var mask: Byte); ogl_dll;
procedure glGetPolygonStipple(var mask: Byte); ogl_dll;
procedure glEdgeFlag(flag: GLBoolean); ogl_dll;
procedure glEdgeFlagv(var flag: GLBoolean); ogl_dll;
procedure glScissor(x, y, width, height: LongInt); ogl_dll;
procedure glClipPlane(plane: GLenum; var equation: Double); ogl_dll;
procedure glGetClipPlane(plane: GLenum; var equation: Double); ogl_dll;
procedure glDrawBuffer(mode: GLenum); ogl_dll;
procedure glReadBuffer(mode: GLenum); ogl_dll;
procedure glEnable(cap: LongInt); ogl_dll;
procedure glDisable(cap: LongInt); ogl_dll;
function glIsEnabled(cap: GLenum): GLBoolean; ogl_dll;
procedure glGetBooleanv(pname: GLenum; params : PGLBoolean); ogl_dll;
procedure glGetDoublev(pname: GLenum; params : PDouble); ogl_dll;
procedure glGetFloatv(pname: GLenum; params : PSingle); ogl_dll;
procedure glGetIntegerv(pname: GLenum; params : PLongInt); ogl_dll;
procedure glPushAttrib(mask: GLbitfield); ogl_dll;
procedure glPopAttrib; ogl_dll;
function glRenderMode(mode: GLenum): LongInt; ogl_dll;
function glGetError: GLenum; ogl_dll;
function glGetString(name: GLenum): PChar; ogl_dll;
procedure glFinish; ogl_dll;
procedure glFlush; ogl_dll;
procedure glHint(target, mode: GLenum); ogl_dll;

// Depth Buffer
procedure glClearDepth(depth: GLclampd); ogl_dll;
procedure glDepthFunc(func: LongInt); ogl_dll;
procedure glDepthMask(flag: GLBoolean); ogl_dll;
procedure glDepthRange(near_val, far_val: GLclampd); ogl_dll;

// Accumulation Buffer
procedure glClearAccum(red, green, blue, alpha: Single); ogl_dll;
procedure glAccum(op: GLenum; value: Single); ogl_dll;

// Tranformation
procedure glMatrixMode(mode: GLenum); ogl_dll;
procedure glOrtho(left, right, bottom, top, near_val, far_val: Double); ogl_dll;
procedure glFrustum(left, right, bottom, top, near_val, far_val: Double); ogl_dll;
procedure glViewport(x, y, width, height: LongInt); ogl_dll;
procedure glPushMatrix; ogl_dll;
procedure glPopMatrix; ogl_dll;
procedure glLoadIdentity; ogl_dll;
procedure glLoadMatrixd(var m: Double); ogl_dll;
procedure glLoadMatrixf(var m: PSingle); ogl_dll;
procedure glMultMatrixd(var m: Double); ogl_dll;
procedure glMultMatrixf(var m: Single); ogl_dll;
procedure glRotated(angle, x, y, z: Double); ogl_dll;
procedure glRotatef(angle, x, y, z: Single); ogl_dll;
procedure glScaled(x, y, z: Double); ogl_dll;
procedure glScalef(x, y, z: Single); ogl_dll;
procedure glTranslated(x, y, z: Double); ogl_dll;
procedure glTranslatef(x, y, z: Single); ogl_dll;

// Display Lists
function glIsList(list: LongWord): GLBoolean; ogl_dll;
procedure glDeleteLists(list: LongWord; range: LongInt); ogl_dll;
function glGenLists(range: LongInt): LongWord; ogl_dll;
procedure glNewList(list: LongWord; mode: GLenum); ogl_dll;
procedure glEndList; ogl_dll;
procedure glCallList(list: LongWord); ogl_dll;
procedure glCallLists(n: LongInt; AType: GLenum; var lists); ogl_dll;
procedure glListBase(base: LongWord); ogl_dll;

// Drawing Functions
procedure glBegin(mode: GLenum); ogl_dll;
procedure glEnd; ogl_dll;
procedure glVertex2d(x, y: Double); ogl_dll;
procedure glVertex2f(x, y: Single); ogl_dll;
procedure glVertex2i(x, y: LongInt); ogl_dll;
procedure glVertex2s(x, y: SmallInt); ogl_dll;
procedure glVertex3d(x, y, z: Double); ogl_dll;
procedure glVertex3f(x, y, z: Single); ogl_dll;
procedure glVertex3i(x, y, z: LongInt); ogl_dll;
procedure glVertex3s(x, y, z: SmallInt); ogl_dll;
procedure glVertex4d(x, y, z, w: Double); ogl_dll;
procedure glVertex4f(x, y, z, w: Single); ogl_dll;
procedure glVertex4i(x, y, z, w: LongInt); ogl_dll;
procedure glVertex4s(x, y, z, w: SmallInt); ogl_dll;
procedure glVertex2dv(var v: Double); ogl_dll;
procedure glVertex2fv(var v: Single); ogl_dll;
procedure glVertex2iv(var v: LongInt); ogl_dll;
procedure glVertex2sv(var v: SmallInt); ogl_dll;
procedure glVertex3dv(var v: Double); ogl_dll;
procedure glVertex3fv(var v: Single); ogl_dll;
procedure glVertex3iv(var v: LongInt); ogl_dll;
procedure glVertex3sv(var v: SmallInt); ogl_dll;
procedure glVertex4dv(var v: Double); ogl_dll;
procedure glVertex4fv(var v: Single); ogl_dll;
procedure glVertex4iv(var v: LongInt); ogl_dll;
procedure glVertex4sv(var v: SmallInt); ogl_dll;
procedure glNormal3b(nx, ny, nz: Byte); ogl_dll;
procedure glNormal3d(nx, ny, nz: Double); ogl_dll;
procedure glNormal3f(nx, ny, nz: Single); ogl_dll;
procedure glNormal3i(nx, ny, nz: LongInt); ogl_dll;
procedure glNormal3s(nx, ny, nz: SmallInt); ogl_dll;
procedure glNormal3bv(var v: ShortInt); ogl_dll;
procedure glNormal3dv(var v: Double); ogl_dll;
procedure glNormal3fv(var v: Single); ogl_dll;
procedure glNormal3iv(var v: LongInt); ogl_dll;
procedure glNormal3sv(var v: SmallInt); ogl_dll;
procedure glIndexd(c: Double); ogl_dll;
procedure glIndexf(c: Single); ogl_dll;
procedure glIndexi(c: LongInt); ogl_dll;
procedure glIndexs(c: SmallInt); ogl_dll;
procedure glIndexdv(var c: Double); ogl_dll;
procedure glIndexfv(var c: Single); ogl_dll;
procedure glIndexiv(var c: LongInt); ogl_dll;
procedure glIndexsv(var c: SmallInt); ogl_dll;
procedure  glColor3b(red, green, blue: ShortInt); ogl_dll;
procedure  glColor3d(red, green, blue: Double); ogl_dll;
procedure  glColor3f(red, green, blue: Single); ogl_dll;
procedure  glColor3i(red, green, blue: LongInt); ogl_dll;
procedure  glColor3s(red, green, blue: SmallInt); ogl_dll;
procedure glColor3ub(red, green, blue: Byte); ogl_dll;
procedure glColor3ui(red, green, blue: LongWord); ogl_dll;
procedure glColor3us(red, green, blue: Word); ogl_dll;
procedure  glColor4b(red, green, blue, alpha: ShortInt); ogl_dll;
procedure  glColor4d(red, green, blue, alpha: Double); ogl_dll;
procedure  glColor4f(red, green, blue, alpha: Single); ogl_dll;
procedure  glColor4i(red, green, blue, alpha: LongInt); ogl_dll;
procedure  glColor4s(red, green, blue, alpha: SmallInt); ogl_dll;
procedure glColor4ub(red, green, blue, alpha: Byte); ogl_dll;
procedure glColor4ui(red, green, blue, alpha: LongWord); ogl_dll;
procedure glColor4us(red, green, blue, alpha: Word); ogl_dll;
procedure  glColor3bv(var v: ShortInt); ogl_dll;
procedure  glColor3dv(var v: Double); ogl_dll;
procedure  glColor3fv(var v: Single); ogl_dll;
procedure  glColor3iv(var v: LongInt); ogl_dll;
procedure  glColor3sv(var v: SmallInt); ogl_dll;
procedure glColor3ubv(var v: Byte); ogl_dll;
procedure glColor3uiv(var v: LongWord); ogl_dll;
procedure glColor3usv(var v: Word); ogl_dll;
procedure  glColor4bv(var v: ShortInt); ogl_dll;
procedure  glColor4dv(var v: Double); ogl_dll;
procedure  glColor4fv(var v: Single); ogl_dll;
procedure  glColor4iv(var v: LongInt); ogl_dll;
procedure  glColor4sv(var v: SmallInt); ogl_dll;
procedure glColor4ubv(var v: Byte); ogl_dll;
procedure glColor4uiv(var v: LongWord); ogl_dll;
procedure glColor4usv(var v: Word); ogl_dll;
procedure glTexCoord1d(s: Double); ogl_dll;
procedure glTexCoord1f(s: Single); ogl_dll;
procedure glTexCoord1i(s: LongInt); ogl_dll;
procedure glTexCoord1s(s: SmallInt); ogl_dll;
procedure glTexCoord2d(s, t: Double); ogl_dll;
procedure glTexCoord2f(s, t: Single); ogl_dll;
procedure glTexCoord2i(s, t: LongInt); ogl_dll;
procedure glTexCoord2s(s, t: SmallInt); ogl_dll;
procedure glTexCoord3d(s, t, r: Double); ogl_dll;
procedure glTexCoord3f(s, t, r: Single); ogl_dll;
procedure glTexCoord3i(s, t, r: LongInt); ogl_dll;
procedure glTexCoord3s(s, t, r: SmallInt); ogl_dll;
procedure glTexCoord4d(s, t, r, q: Double); ogl_dll;
procedure glTexCoord4f(s, t, r, q: Single); ogl_dll;
procedure glTexCoord4i(s, t, r, q: LongInt); ogl_dll;
procedure glTexCoord4s(s, t, r, q: SmallInt); ogl_dll;
procedure glTexCoord1dv(var v: Double); ogl_dll;
procedure glTexCoord1fv(var v: Single); ogl_dll;
procedure glTexCoord1iv(var v: LongInt); ogl_dll;
procedure glTexCoord1sv(var v: SmallInt); ogl_dll;
procedure glTexCoord2dv(var v: Double); ogl_dll;
procedure glTexCoord2fv(var v: Single); ogl_dll;
procedure glTexCoord2iv(var v: LongInt); ogl_dll;
procedure glTexCoord2sv(var v: SmallInt); ogl_dll;
procedure glTexCoord3dv(var v: Double); ogl_dll;
procedure glTexCoord3fv(var v: Single); ogl_dll;
procedure glTexCoord3iv(var v: LongInt); ogl_dll;
procedure glTexCoord3sv(var v: SmallInt); ogl_dll;
procedure glTexCoord4dv(var v: Double); ogl_dll;
procedure glTexCoord4fv(var v: Single); ogl_dll;
procedure glTexCoord4iv(var v: LongInt); ogl_dll;
procedure glTexCoord4sv(var v: SmallInt); ogl_dll;
procedure glRasterPos2d(x, y: Double); ogl_dll;
procedure glRasterPos2f(x, y: Single); ogl_dll;
procedure glRasterPos2i(x, y: LongInt); ogl_dll;
procedure glRasterPos2s(x, y: SmallInt); ogl_dll;
procedure glRasterPos3d(x, y, z: Double); ogl_dll;
procedure glRasterPos3f(x, y, z: Single); ogl_dll;
procedure glRasterPos3i(x, y, z: LongInt); ogl_dll;
procedure glRasterPos3s(x, y, z: SmallInt); ogl_dll;
procedure glRasterPos4d(x, y, z, w: Double); ogl_dll;
procedure glRasterPos4f(x, y, z, w: Single); ogl_dll;
procedure glRasterPos4i(x, y, z, w: LongInt); ogl_dll;
procedure glRasterPos4s(x, y, z, w: SmallInt); ogl_dll;
procedure glRasterPos2dv(var v: Double); ogl_dll;
procedure glRasterPos2fv(var v: Single); ogl_dll;
procedure glRasterPos2iv(var v: LongInt); ogl_dll;
procedure glRasterPos2sv(var v: SmallInt); ogl_dll;
procedure glRasterPos3dv(var v: Double); ogl_dll;
procedure glRasterPos3fv(var v: Single); ogl_dll;
procedure glRasterPos3iv(var v: LongInt); ogl_dll;
procedure glRasterPos3sv(var v: SmallInt); ogl_dll;
procedure glRasterPos4dv(var v: Double); ogl_dll;
procedure glRasterPos4fv(var v: Single); ogl_dll;
procedure glRasterPos4iv(var v: LongInt); ogl_dll;
procedure glRasterPos4sv(var v: SmallInt); ogl_dll;
procedure glRectd(x1, y1, x2, y2: Double); ogl_dll;
procedure glRectf(x1, y1, x2, y2: Single); ogl_dll;
procedure glRecti(x1, y1, x2, y2: LongInt); ogl_dll;
procedure glRects(x1, y1, x2, y2: SmallInt); ogl_dll;
procedure glRectdv(var v1, v2: Double); ogl_dll;
procedure glRectfv(var v1, v2: Single); ogl_dll;
procedure glRectiv(var v1, v2: LongInt); ogl_dll;
procedure glRectsv(var v1, v2: SmallInt); ogl_dll;

// Lighting
procedure glShadeModel(mode: GLenum); ogl_dll;
procedure glLightf(light, pname: GLenum; param: Single); ogl_dll;
procedure glLighti(light, pname: GLenum; param: LongInt); ogl_dll;
procedure glLightfv(light, pname: GLenum; params : PSingle); ogl_dll;
procedure glLightiv(light, pname: GLenum; params : PLongInt); ogl_dll;
procedure glGetLightfv(light, pname: GLenum; params : PSingle); ogl_dll;
procedure glGetLightiv(light, pname: GLenum; params : PLongInt); ogl_dll;
procedure glLightModelf(pname: GLenum; param: Single); ogl_dll;
procedure glLightModeli(pname: GLenum; param: LongInt); ogl_dll;
procedure glLightModelfv(pname: GLenum; params : PSingle); ogl_dll;
procedure glLightModeliv(pname: GLenum; params : PLongInt); ogl_dll;
procedure glMaterialf(face, pname: GLenum; param: Single); ogl_dll;
procedure glMateriali(face, pname: GLenum; param: LongInt); ogl_dll;
procedure glMaterialfv(face, pname: GLenum; params : PSingle); ogl_dll;
procedure glMaterialiv(face, pname: GLenum; params : PLongInt); ogl_dll;
procedure glGetMaterialfv(face, pname: GLenum; params : PSingle); ogl_dll;
procedure glGetMaterialiv(face, pname: GLenum; params : PLongInt); ogl_dll;
procedure glColorMaterial(face, mode: GLenum); ogl_dll;

// Raster Functions
procedure glPixelZoom(xfactor, yfactor: Single); ogl_dll;
procedure glPixelStoref(pname: GLenum; param: Single); ogl_dll;
procedure glPixelStorei(pname: GLenum; param: LongInt); ogl_dll;
procedure glPixelTransferf(pname: GLenum; param: Single); ogl_dll;
procedure glPixelTransferi(pname: GLenum; param: LongInt); ogl_dll;
procedure glPixelMapfv(map: GLenum; mapsize: LongInt; var values: Single); ogl_dll;
procedure glPixelMapuiv(map: GLenum; mapsize: LongInt; var values: LongWord); ogl_dll;
procedure glPixelMapusv(map: GLenum; mapsize: LongInt; var values: Word); ogl_dll;
procedure glGetPixelMapfv(map: GLenum; var values: Single); ogl_dll;
procedure glGetPixelMapuiv(map: GLenum; var values: LongWord); ogl_dll;
procedure glGetPixelMapusv(map: GLenum; var values: Word); ogl_dll;
procedure glBitmap(width, height: LongInt; xorig, yorig, xmove, ymove: Single; var bitmap); ogl_dll;
procedure glReadPixels(x, y, width, height: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glDrawPixels(width, height: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glCopyPixels(x, y, width, height: LongInt; AType: GLenum); ogl_dll;

// Stenciling
procedure glStencilFunc(func: GLenum; ref: LongInt; mask: LongWord); ogl_dll;
procedure glStencilMask(mask: LongWord); ogl_dll;
procedure glStencilOp(fail, zfail, zpass: GLenum); ogl_dll;
procedure glClearStencil(s: LongInt); ogl_dll;

// Texture Mapping
procedure glTexGend(cord, pname: GLenum; param: Double); ogl_dll;
procedure glTexGenf(cord, pname: GLenum; param: Single); ogl_dll;
procedure glTexGeni(cord, pname: GLenum; param: LongInt); ogl_dll;
procedure glTexGendv(cord, pname: GLenum; params : PDouble); ogl_dll;
procedure glTexGenfv(cord, pname: GLenum; params : PSingle); ogl_dll;
procedure glTexGeniv(cord, pname: GLenum; params : PLongInt); ogl_dll;
procedure glGetTexGendv(cord, pname: GLenum; params : PDouble); ogl_dll;
procedure glGetTexGenfv(cord, pname: GLenum; params : PSingle); ogl_dll;
procedure glGetTexGeniv(cord, pname: GLenum; params : PLongInt); ogl_dll;
procedure glTexEnvf(target, pname: GLenum; param: Single); ogl_dll;
procedure glTexEnvi(target, pname: GLenum; param: LongInt); ogl_dll;
procedure glTexEnvfv(target, pname: GLenum; params : PSingle); ogl_dll;
procedure glTexEnviv(target, pname: GLenum; params : PLongInt); ogl_dll;
procedure glGetTexEnvfv(target, pname: GLenum; params : PSingle); ogl_dll;
procedure glGetTexEnviv(target, pname: GLenum; params : PLongInt); ogl_dll;
procedure glTexParameterf(target, pname: GLenum; param: Single); ogl_dll;
procedure glTexParameteri(target, pname: GLenum; param: LongInt); ogl_dll;
procedure glTexParameterfv(target, pname: GLenum; params : PSingle); ogl_dll;
procedure glTexParameteriv(target, pname: GLenum; params : PLongInt); ogl_dll;
procedure glGetTexParameterfv(target, pname: GLenum; params : PSingle); ogl_dll;
procedure glGetTexParameteriv(target, pname: GLenum; params : PLongInt); ogl_dll;
procedure glGetTexLevelParameterfv(target: GLenum; level: LongInt; pname: GLenum; params : PSingle); ogl_dll;
procedure glGetTexLevelParameteriv(target: GLenum; level: LongInt; pname: GLenum; params : PLongInt); ogl_dll;
procedure glTexImage1D(target: GLenum; level, internalFormat, width, border: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glTexImage2D(target: GLenum; level, internalFormat, width, height, border: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glGetTexImage(target: GLenum; level: LongInt; format, AType: GLenum; var pixels); ogl_dll;

// Evaluators
procedure glMap1d(target: GLenum; u1, u2: Double; stride, order: LongInt; var points: Double); ogl_dll;
procedure glMap1f(target: GLenum; u1, u2: Single; stride, order: LongInt; var points: Single); ogl_dll;
procedure glMap2d(target: GLenum; u1, u2: Double; ustride, uorder: LongInt; v1, v2: Double; vstride, vorder: LongInt; var points: Double); ogl_dll;
procedure glMap2f(target: GLenum; u1, u2: Single; ustride, uorder: LongInt; v1, v2: Single; vstride, vorder: LongInt; var points: Single); ogl_dll;
procedure glGetMapdv(target, query: GLenum; var v: Double); ogl_dll;
procedure glGetMapfv(target, query: GLenum; var v: Single); ogl_dll;
procedure glGetMapiv(target, query: GLenum; var v: LongInt); ogl_dll;
procedure glEvalCoord1d(u: Double); ogl_dll;
procedure glEvalCoord1f(u: Single); ogl_dll;
procedure glEvalCoord1dv(var u: Double); ogl_dll;
procedure glEvalCoord1fv(var u: Single); ogl_dll;
procedure glEvalCoord2d(u, v: Double); ogl_dll;
procedure glEvalCoord2f(u, v: Single); ogl_dll;
procedure glEvalCoord2dv(var u, v: Double); ogl_dll;
procedure glEvalCoord2fv(var u, v: Single); ogl_dll;
procedure glMapGrid1d(un: LongInt; u1, u2: Double); ogl_dll;
procedure glMapGrid1f(un: LongInt; u1, u2: Single); ogl_dll;
procedure glMapGrid2d(un: LongInt; u1, u2: Double; vn: LongInt; v1, v2: Double); ogl_dll;
procedure glMapGrid2f(un: LongInt; u1, u2: Single; vn: LongInt; v1, v2: Single); ogl_dll;
procedure glEvalPoint1(i: LongInt); ogl_dll;
procedure glEvalPoint2(i, j: LongInt); ogl_dll;
procedure glEvalMesh1(mode: GLenum; i1, i2: LongInt); ogl_dll;
procedure glEvalMesh2(mode: GLenum; i1, i2, j1, j2: LongInt); ogl_dll;

// Fog
procedure glFogf(pname: GLenum; param: Single); ogl_dll;
procedure glFogi(pname: GLenum; param: LongInt); ogl_dll;
procedure glFogfv(pname: GLenum; params : PSingle); ogl_dll;
procedure glFogiv(pname: GLenum; params : PLongInt); ogl_dll;

// Selection and Feedback
procedure glFeedbackBuffer(size: LongInt; AType: GLenum; var buffer: Single); ogl_dll;
procedure glPassThrough(token: Single); ogl_dll;
procedure glSelectBuffer(size: LongInt; var buffer: LongWord); ogl_dll;
procedure glInitNames; ogl_dll;
procedure glLoadName(name: LongWord); ogl_dll;
procedure glPushName(name: LongWord); ogl_dll;
procedure glPopName; ogl_dll;

{$ENDIF GL1_0}

{$IFDEF GL1_1}

// Miscellaneous
procedure glEnableClientState(cap: GLenum); ogl_dll;
procedure glDisableClientState(cap: GLenum); ogl_dll;
procedure glPushClientAttrib(mask: GLbitfield); ogl_dll;
procedure glPopClientAttrib; ogl_dll;

// Drawing Functions
procedure glIndexub(c: Byte); ogl_dll;
procedure glIndexubv(var c: Byte); ogl_dll;

// Vertex Arrays
procedure glVertexPointer(size: LongInt; AType: GLenum; stride: LongInt; var ptr); ogl_dll;
procedure glNormalPointer(AType: GLenum; stride: LongInt; var ptr); ogl_dll;
procedure glColorPointer(size: LongInt; AType: GLenum; stride: LongInt; var ptr); ogl_dll;
procedure glIndexPointer(AType: GLenum; stride: LongInt; var ptr); ogl_dll;
procedure glTexCoordPointer(size: LongInt; AType: GLenum; stride: LongInt; var ptr); ogl_dll;
procedure glEdgeFlagPointer(stride: LongInt; var ptr); ogl_dll;
procedure glGetPointerv(pname: GLenum; var params: Pointer); ogl_dll;
procedure glArrayElement(i: LongInt); ogl_dll;
procedure glDrawArrays(mode: GLenum; first, count: LongInt); ogl_dll;
procedure glDrawElements(mode: GLenum; count: Integer; AType: GLenum; var indices); ogl_dll;
procedure glInterleavedArrays(format: GLenum; stride: LongInt; var pointer); ogl_dll;

// Texture Mapping
procedure glGenTextures(n: LongInt; var textures: LongWord); ogl_dll;
procedure glDeleteTextures(n: LongInt; var textures: LongWord); ogl_dll;
procedure glBindTexture(target: GLenum; texture: LongWord); ogl_dll;
procedure glPrioritizeTextures(n: LongInt; var textures: LongWord; var priorities: GLclampf); ogl_dll;
function glAreTexturesResident(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; ogl_dll;
function glIsTexture(texture: LongWord): Boolean; ogl_dll;
procedure glTexSubImage1D(target: GLenum; level, xoffset, width: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset, width, height: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glCopyTexImage1D(target: GLenum; level: LongInt; format: GLenum; x, y, width, border: LongInt); ogl_dll;
procedure glCopyTexImage2D(target: GLenum; level: LongInt; format: GLenum; x, y, width, height, border: LongInt); ogl_dll;
procedure glCopyTexSubImage1D(target: GLenum; level, xoffset, x, y, width: LongInt); ogl_dll;
procedure glCopyTexSubImage2D(target: GLenum; level, xoffset, yoffset, x, y, width, height: LongInt); ogl_dll;

{$ENDIF GL1_1}

{$IFDEF GL1_2}
procedure glDrawRangeElements(mode: GLenum; AStart, AEnd: LongWord; count: LongInt; AType: GLenum; var indices); ogl_dll;
procedure glTexImage3D(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLEnum; var pixels); ogl_dll;
procedure glTexSubImage3D(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLEnum; var pixels); ogl_dll;
procedure glCopyTexSubImage3D(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, x, y, width, height: LongInt); ogl_dll;
{$ENDIF GL1_2}


// -------------------------------------------------------
//   GL Extensions
// -------------------------------------------------------

{$IFDEF EXTENSIONS}

// === 1.0 Extensions ===

// GL_EXT_blend_minmax
procedure glBlendEquationEXT(mode: GLenum); ogl_dll;

// GL_EXT_blend_color
procedure glBlendColorEXT(red, green, blue, alpha: GLclampf); ogl_dll;

// GL_EXT_polygon_offset
procedure glPolygonOffsetEXT(factor, bias: Single); ogl_dll;

// GL_EXT_vertex_array
procedure glVertexPointerEXT(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); ogl_dll;
procedure glNormalPointerEXT(AType: GLenum; stride, count: LongInt; var ptr); ogl_dll;
procedure glColorPointerEXT(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); ogl_dll;
procedure glIndexPointerEXT(AType: GLenum; stride, count: LongInt; var ptr); ogl_dll;
procedure glTexCoordPointerEXT(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); ogl_dll;
procedure glEdgeFlagPointerEXT(stride, count: LongInt; var ptr: Boolean); ogl_dll;
procedure glGetPointervEXT(pname: GLenum; var params: Pointer); ogl_dll;
procedure glArrayElementEXT(i: LongInt); ogl_dll;
procedure glDrawArraysEXT(mode: GLEnum; first, count: LongInt); ogl_dll;

// GL_EXT_texture_object
procedure glGenTexturesEXT(n: LongInt; var textures: LongWord); ogl_dll;
procedure glDeleteTexturesEXT(n: LongInt; var textures: LongWord); ogl_dll;
procedure glBindTextureEXT(target: GLenum; texture: LongWord); ogl_dll;
procedure glPrioritizeTexturesEXT(n: LongInt; var textures: LongWord; var priorities: GLClampf); ogl_dll;
function glAreTexturesResidentEXT(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; ogl_dll;
function glIsTextureEXT(texture: LongWord): Boolean; ogl_dll;

// GL_EXT_texture3D
procedure glTexImage3DEXT(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glTexSubImage3DEXT(target: GLenum; level, xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLenum; var pixels); ogl_dll;
procedure glCopyTexSubImage3DEXT(target: GLenum; level, xoffset, yoffset, zoffset, x, y, width, height: LongInt); ogl_dll;

// GL_EXT_color_table
procedure glColorTableEXT(target, internalformat: GLenum; width: LongInt; format, AType: GLenum; var table); ogl_dll;
procedure glColorSubTableEXT(target: GLenum; start, count: LongInt; format, AType: GLEnum; var data); ogl_dll;
procedure glGetColorTableEXT(target, format, AType: GLenum; var table); ogl_dll;
procedure glGetColorTableParameterfvEXT(target, pname: GLenum; var params: Single); ogl_dll;
procedure glGetColorTableParameterivEXT(target, pname: GLenum; var params: LongInt); ogl_dll;

{$ENDIF EXTENSIONS}

// library dependent extensions

{$IFDEF SGI_EXTENSIONS}

// GL_SGIS_multitexture
procedure glMultiTexCoord1dSGIS(target: GLenum; s: Double); ogl_dll;
procedure glMultiTexCoord1dvSGIS(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord1fSGIS(target: GLenum; s: Single); ogl_dll;
procedure glMultiTexCoord1fvSGIS(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord1iSGIS(target: GLenum; s: LongInt); ogl_dll;
procedure glMultiTexCoord1ivSGIS(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord1sSGIS(target: GLenum; s: ShortInt); ogl_dll;
procedure glMultiTexCoord1svSGIS(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoord2dSGIS(target: GLenum; s, t: Double); ogl_dll;
procedure glMultiTexCoord2dvSGIS(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord2fSGIS(target: GLenum; s, t: Single); ogl_dll;
procedure glMultiTexCoord2fvSGIS(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord2iSGIS(target: GLenum; s, t: LongInt); ogl_dll;
procedure glMultiTexCoord2ivSGIS(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord2sSGIS(target: GLenum; s, t: ShortInt); ogl_dll;
procedure glMultiTexCoord2svSGIS(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoord3dSGIS(target: GLenum; s, t, r: Double); ogl_dll;
procedure glMultiTexCoord3dvSGIS(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord3fSGIS(target: GLenum; s, t, r: Single); ogl_dll;
procedure glMultiTexCoord3fvSGIS(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord3iSGIS(target: GLenum; s, t, r: LongInt); ogl_dll;
procedure glMultiTexCoord3ivSGIS(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord3sSGIS(target: GLenum; s, t, r: ShortInt); ogl_dll;
procedure glMultiTexCoord3svSGIS(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoord4dSGIS(target: GLenum; s, t, r, q: Double); ogl_dll;
procedure glMultiTexCoord4dvSGIS(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord4fSGIS(target: GLenum; s, t, r, q: Single); ogl_dll;
procedure glMultiTexCoord4fvSGIS(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord4iSGIS(target: GLenum; s, t, r, q: LongInt); ogl_dll;
procedure glMultiTexCoord4ivSGIS(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord4sSGIS(target: GLenum; s, t, r, q: ShortInt); ogl_dll;
procedure glMultiTexCoord4svSGIS(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoordPointerSGIS(target: GLenum; size: LongInt; AType: GLEnum; stride: LongInt; var APointer); ogl_dll;
procedure glSelectTextureSGIS(target: GLenum); ogl_dll;
procedure glSelectTextureCoordSetSGIS(target: GLenum); ogl_dll;

// GL_EXT_multitexture
procedure glMultiTexCoord1dEXT(target: GLenum; s: Double); ogl_dll;
procedure glMultiTexCoord1dvEXT(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord1fEXT(target: GLenum; s: Single); ogl_dll;
procedure glMultiTexCoord1fvEXT(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord1iEXT(target: GLenum; s: LongInt); ogl_dll;
procedure glMultiTexCoord1ivEXT(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord1sEXT(target: GLenum; s: ShortInt); ogl_dll;
procedure glMultiTexCoord1svEXT(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoord2dEXT(target: GLenum; s, t: Double); ogl_dll;
procedure glMultiTexCoord2dvEXT(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord2fEXT(target: GLenum; s, t: Single); ogl_dll;
procedure glMultiTexCoord2fvEXT(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord2iEXT(target: GLenum; s, t: LongInt); ogl_dll;
procedure glMultiTexCoord2ivEXT(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord2sEXT(target: GLenum; s, t: ShortInt); ogl_dll;
procedure glMultiTexCoord2svEXT(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoord3dEXT(target: GLenum; s, t, r: Double); ogl_dll;
procedure glMultiTexCoord3dvEXT(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord3fEXT(target: GLenum; s, t, r: Single); ogl_dll;
procedure glMultiTexCoord3fvEXT(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord3iEXT(target: GLenum; s, t, r: LongInt); ogl_dll;
procedure glMultiTexCoord3ivEXT(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord3sEXT(target: GLenum; s, t, r: ShortInt); ogl_dll;
procedure glMultiTexCoord3svEXT(target: GLenum; var v: ShortInt); ogl_dll;
procedure glMultiTexCoord4dEXT(target: GLenum; s, t, r, q: Double); ogl_dll;
procedure glMultiTexCoord4dvEXT(target: GLenum; var v: Double); ogl_dll;
procedure glMultiTexCoord4fEXT(target: GLenum; s, t, r, q: Single); ogl_dll;
procedure glMultiTexCoord4fvEXT(target: GLenum; var v: Single); ogl_dll;
procedure glMultiTexCoord4iEXT(target: GLenum; s, t, r, q: LongInt); ogl_dll;
procedure glMultiTexCoord4ivEXT(target: GLenum; var v: LongInt); ogl_dll;
procedure glMultiTexCoord4sEXT(target: GLenum; s, t, r, q: ShortInt); ogl_dll;
procedure glMultiTexCoord4svEXT(target: GLenum; var v: ShortInt); ogl_dll;
procedure glInterleavedTextureCoordSetsEXT(factor: LongInt); ogl_dll;
procedure glSelectTextureEXT(target: GLenum); ogl_dll;
procedure glSelectTextureCoordSetEXT(target: GLenum); ogl_dll;
procedure glSelectTextureTransformEXT(target: GLenum); ogl_dll;

// GL_EXT_point_parameters
procedure glPointParameterfEXT(pname: GLenum; param: Single); ogl_dll;
procedure glPointParameterfvEXT(pname: GLenum; var params: Single); ogl_dll;

{$ENDIF SGI_EXTENSIONS}

{$ifdef MESA}
// GL_MESA_window_pos
procedure glWindowPos2iMESA(x, y: LongInt); ogl_dll;
procedure glWindowPos2sMESA(x, y: ShortInt); ogl_dll;
procedure glWindowPos2fMESA(x, y: Single); ogl_dll;
procedure glWindowPos2dMESA(x, y: Double); ogl_dll;
procedure glWindowPos2ivMESA(var p: LongInt); ogl_dll;
procedure glWindowPos2svMESA(var p: ShortInt); ogl_dll;
procedure glWindowPos2fvMESA(var p: Single); ogl_dll;
procedure glWindowPos2dvMESA(var p: Double); ogl_dll;
procedure glWindowPos3iMESA(x, y, z: LongInt); ogl_dll;
procedure glWindowPos3sMESA(x, y, z: ShortInt); ogl_dll;
procedure glWindowPos3fMESA(x, y, z: Single); ogl_dll;
procedure glWindowPos3dMESA(x, y, z: Double); ogl_dll;
procedure glWindowPos3ivMESA(var p: LongInt); ogl_dll;
procedure glWindowPos3svMESA(var p: ShortInt); ogl_dll;
procedure glWindowPos3fvMESA(var p: Single); ogl_dll;
procedure glWindowPos3dvMESA(var p: Double); ogl_dll;
procedure glWindowPos4iMESA(x, y, z, w: LongInt); ogl_dll;
procedure glWindowPos4sMESA(x, y, z, w: ShortInt); ogl_dll;
procedure glWindowPos4fMESA(x, y, z, w: Single); ogl_dll;
procedure glWindowPos4dMESA(x, y, z, w: Double); ogl_dll;
procedure glWindowPos4ivMESA(var p: LongInt); ogl_dll;
procedure glWindowPos4svMESA(var p: ShortInt); ogl_dll;
procedure glWindowPos4fvMESA(var p: Single); ogl_dll;
procedure glWindowPos4dvMESA(var p: Double); ogl_dll;

// GL_MESA_resize_buffers
procedure glResizeBuffersMESA; ogl_dll;
{$endif MESA}


// =======================================================
// =======================================================

implementation

{BEGIN}{OF INIT}
END.


{
  $Log$
  Revision 1.1.2.1  2000-09-03 22:14:40  peter
    * regenerated

}
