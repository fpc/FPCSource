{
  $Id$
  Translation of the Mesa GL, GLU and GLX headers for Free Pascal
  Linux Version, Copyright (C) 1999 Sebastian Guenther


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

unit GL;

interface
uses XLib;

// ===================================================================
//   Unit specific extensions
// ===================================================================

function InitGLFromLibrary(libname: PChar): Boolean;
function InitGLUFromLibrary(libname: PChar): Boolean;
// Requires that the GL library has already been initialized:
function InitGLX: Boolean;

// determines automatically which libraries to use:
function InitGL: Boolean;
function InitGLU: Boolean;


var
  GLInitialized, GLUInitialized, GLXInitialized: Boolean;

// ===================================================================
//   GL consts, types and functions
// ===================================================================


// -------------------------------------------------------------------
//   GL types
// -------------------------------------------------------------------

type

  PSingle = ^Single;
  PDouble = ^Double;

  GLclampf = Single;    // single precision float in [0,1]
  GLclampd = Double;    // double precision float in [0,1]

  GLenum = Integer;

type
  GLbitfield = set of (GL_CURRENT_BIT, GL_POINT_BIT, GL_LINE_BIT,
    GL_POLYGON_BIT, GL_POLYGON_STIPPLE_BIT, GL_PIXEL_MODE_BIT,
    GL_LIGHTING_BIT, GL_FOG_BIT, GL_DEPTH_BUFFER_BIT, GL_ACCUM_BUFFER_BIT,
    GL_STENCIL_BUFFER_BIT, GL_VIEWPORT_BIT, GL_TRANSFORM_BIT, GL_ENABLE_BIT,
    GL_COLOR_BUFFER_BIT, GL_HINT_BIT, GL_EVAL_BIT, GL_LIST_BIT, GL_TEXTURE_BIT,
    GL_SCISSOR_BIT);

const
  GL_ALL_ATTRIB_BITS = [Low(GLbitfield)..High(GLbitfield)];


// -------------------------------------------------------------------
//   GL constants
// -------------------------------------------------------------------

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

  // GL 1.1 texturing
  GL_PROXY_TEXTURE_1D                   = $8063;
  GL_PROXY_TEXTURE_2D                   = $8064;
  GL_TEXTURE_PRIORITY                   = $8066;
  GL_TEXTURE_RESIDENT                   = $8067;
  GL_TEXTURE_BINDING_1D                 = $8068;
  GL_TEXTURE_BINDING_2D                 = $8069;
  GL_TEXTURE_INTERNAL_FORMAT            = $1003;


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

  // OpenGL 1.2
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


// -------------------------------------------------------------------
//   GL procedures and functions
// -------------------------------------------------------------------

var

  // Miscellaneous
  glClearIndex: procedure(c: Single); cdecl;
  glClearColor: procedure(red, green, blue, alpha: GLclampf); cdecl;
  glClear: procedure(mask: GLbitfield); cdecl;
  glIndexMask: procedure(mask: LongWord); cdecl;
  glColorMask: procedure(red, green, blue, alpha: Boolean); cdecl;
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); cdecl;
  glBlendFunc: procedure(sfactor, dfactor: GLenum); cdecl;
  glLogicOp: procedure(opcode: GLenum); cdecl;
  glCullFace: procedure(mode: GLenum); cdecl;
  glFrontFace: procedure(mode: GLenum); cdecl;
  glPointSize: procedure(size: Single); cdecl;
  glLineWidth: procedure(width: Single); cdecl;
  glLineStipple: procedure(factor: LongInt; pattern: Word); cdecl;
  glPolygonMode: procedure(face, mode: GLenum); cdecl;
  glPolygonOffset: procedure(factor, units: Single); cdecl;
  glPolygonStipple: procedure(var mask: Byte); cdecl;
  glGetPolygonStipple: procedure(var mask: Byte); cdecl;
  glEdgeFlag: procedure(flag: Boolean); cdecl;
  glEdgeFlagv: procedure(var flag: Boolean); cdecl;
  glScissor: procedure(x, y, width, height: LongInt); cdecl;
  glClipPlane: procedure(plane: GLenum; var equation: Double); cdecl;
  glGetClipPlane: procedure(plane: GLenum; var equation: Double); cdecl;
  glDrawBuffer: procedure(mode: GLenum); cdecl;
  glReadBuffer: procedure(mode: GLenum); cdecl;
  glEnable: procedure(cap: LongInt); cdecl;
  glDisable: procedure(cap: LongInt); cdecl;
  glIsEnabled: function(cap: GLenum): Boolean; cdecl;
  glEnableClientState: procedure(cap: GLenum); cdecl; // 1.1
  glDisableClientState: procedure(cap: GLenum); cdecl; // 1.1
  glGetBooleanv: procedure(pname: GLenum; var params: Boolean); cdecl;
  glGetDoublev: procedure(pname: GLenum; var params: Double); cdecl;
  glGetFloatv: procedure(pname: GLenum; var params: Single); cdecl;
  glGetIntegerv: procedure(pname: GLenum; var params: LongInt); cdecl;
  glPushAttrib: procedure(mask: GLbitfield); cdecl;
  glPopAttrib: procedure; cdecl;
  glPushClientAttrib: procedure(mask: GLbitfield); cdecl; // 1.1
  glPopClientAttrib: procedure; cdecl; // 1.1
  glRenderMode: function(mode: GLenum): LongInt; cdecl;
  glGetError: function: GLenum; cdecl;
  glGetString: function(name: GLenum): PChar; cdecl;
  glFinish: procedure; cdecl;
  glFlush: procedure; cdecl;
  glHint: procedure(target, mode: GLenum); cdecl;


  // Depth Buffer
  glClearDepth: procedure(depth: GLclampd); cdecl;
  glDepthFunc: procedure(func: LongInt); cdecl;
  glDepthMask: procedure(flag: Boolean); cdecl;
  glDepthRange: procedure(near_val, far_val: GLclampd); cdecl;

  // Accumulation Buffer
  glClearAccum: procedure(red, green, blue, alpha: Single); cdecl;
  glAccum: procedure(op: GLenum; value: Single); cdecl;

  // Tranformation
  glMatrixMode: procedure(mode: GLenum); cdecl;
  glOrtho: procedure(left, right, bottom, top, near_val, far_val: Double); cdecl;
  glFrustum: procedure(left, right, bottom, top, near_val, far_val: Double); cdecl;
  glViewport: procedure(x, y, width, height: LongInt); cdecl;
  glPushMatrix: procedure; cdecl;
  glPopMatrix: procedure; cdecl;
  glLoadIdentity: procedure; cdecl;
  glLoadMatrixd: procedure(var m: Double); cdecl;
  glLoadMatrixf: procedure(var m: PSingle); cdecl;
  glMultMatrixd: procedure(var m: Double); cdecl;
  glMultMatrixf: procedure(var m: Single); cdecl;
  glRotated: procedure(angle, x, y, z: Double); cdecl;
  glRotatef: procedure(angle, x, y, z: Single); cdecl;
  glScaled: procedure(x, y, z: Double); cdecl;
  glScalef: procedure(x, y, z: Single); cdecl;
  glTranslated: procedure(x, y, z: Double); cdecl;
  glTranslatef: procedure(x, y, z: Single); cdecl;

  // Display Lists
  glIsList: function(list: LongWord): Boolean; cdecl;
  glDeleteLists: procedure(list: LongWord; range: LongInt); cdecl;
  glGenLists: function(range: LongInt): LongWord; cdecl;
  glNewList: procedure(list: LongWord; mode: GLenum); cdecl;
  glEndList: procedure; cdecl;
  glCallList: procedure(list: LongWord); cdecl;
  glCallLists: procedure(n: LongInt; AType: GLenum; var lists); cdecl;
  glListBase: procedure(base: LongWord); cdecl;

  // Drawing Functions
  glBegin: procedure(mode: GLenum); cdecl;
  glEnd: procedure; cdecl;
  glVertex2d: procedure(x, y: Double); cdecl;
  glVertex2f: procedure(x, y: Single); cdecl;
  glVertex2i: procedure(x, y: LongInt); cdecl;
  glVertex2s: procedure(x, y: SmallInt); cdecl;
  glVertex3d: procedure(x, y, z: Double); cdecl;
  glVertex3f: procedure(x, y, z: Single); cdecl;
  glVertex3i: procedure(x, y, z: LongInt); cdecl;
  glVertex3s: procedure(x, y, z: SmallInt); cdecl;
  glVertex4d: procedure(x, y, z, w: Double); cdecl;
  glVertex4f: procedure(x, y, z, w: Single); cdecl;
  glVertex4i: procedure(x, y, z, w: LongInt); cdecl;
  glVertex4s: procedure(x, y, z, w: SmallInt); cdecl;
  glVertex2dv: procedure(var v: Double); cdecl;
  glVertex2fv: procedure(var v: Single); cdecl;
  glVertex2iv: procedure(var v: LongInt); cdecl;
  glVertex2sv: procedure(var v: SmallInt); cdecl;
  glVertex3dv: procedure(var v: Double); cdecl;
  glVertex3fv: procedure(var v: Single); cdecl;
  glVertex3iv: procedure(var v: LongInt); cdecl;
  glVertex3sv: procedure(var v: SmallInt); cdecl;
  glVertex4dv: procedure(var v: Double); cdecl;
  glVertex4fv: procedure(var v: Single); cdecl;
  glVertex4iv: procedure(var v: LongInt); cdecl;
  glVertex4sv: procedure(var v: SmallInt); cdecl;
  glNormal3b: procedure(nx, ny, nz: Byte); cdecl;
  glNormal3d: procedure(nx, ny, nz: Double); cdecl;
  glNormal3f: procedure(nx, ny, nz: Single); cdecl;
  glNormal3i: procedure(nx, ny, nz: LongInt); cdecl;
  glNormal3s: procedure(nx, ny, nz: SmallInt); cdecl;
  glNormal3bv: procedure(var v: ShortInt); cdecl;
  glNormal3dv: procedure(var v: Double); cdecl;
  glNormal3fv: procedure(var v: Single); cdecl;
  glNormal3iv: procedure(var v: LongInt); cdecl;
  glNormal3sv: procedure(var v: SmallInt); cdecl;
  glIndexd: procedure(c: Double); cdecl;
  glIndexf: procedure(c: Single); cdecl;
  glIndexi: procedure(c: LongInt); cdecl;
  glIndexs: procedure(c: SmallInt); cdecl;
  glIndexub: procedure(c: Byte); cdecl; // 1.1
  glIndexdv: procedure(var c: Double); cdecl;
  glIndexfv: procedure(var c: Single); cdecl;
  glIndexiv: procedure(var c: LongInt); cdecl;
  glIndexsv: procedure(var c: SmallInt); cdecl;
  glIndexubv: procedure(var c: Byte); cdecl; // 1.1
  glColor3b : procedure(red, green, blue: ShortInt); cdecl;
  glColor3d : procedure(red, green, blue: Double); cdecl;
  glColor3f : procedure(red, green, blue: Single); cdecl;
  glColor3i : procedure(red, green, blue: LongInt); cdecl;
  glColor3s : procedure(red, green, blue: SmallInt); cdecl;
  glColor3ub: procedure(red, green, blue: Byte); cdecl;
  glColor3ui: procedure(red, green, blue: LongWord); cdecl;
  glColor3us: procedure(red, green, blue: Word); cdecl;
  glColor4b : procedure(red, green, blue, alpha: ShortInt); cdecl;
  glColor4d : procedure(red, green, blue, alpha: Double); cdecl;
  glColor4f : procedure(red, green, blue, alpha: Single); cdecl;
  glColor4i : procedure(red, green, blue, alpha: LongInt); cdecl;
  glColor4s : procedure(red, green, blue, alpha: SmallInt); cdecl;
  glColor4ub: procedure(red, green, blue, alpha: Byte); cdecl;
  glColor4ui: procedure(red, green, blue, alpha: LongWord); cdecl;
  glColor4us: procedure(red, green, blue, alpha: Word); cdecl;
  glColor3bv : procedure(var v: ShortInt); cdecl;
  glColor3dv : procedure(var v: Double); cdecl;
  glColor3fv : procedure(var v: Single); cdecl;
  glColor3iv : procedure(var v: LongInt); cdecl;
  glColor3sv : procedure(var v: SmallInt); cdecl;
  glColor3ubv: procedure(var v: Byte); cdecl;
  glColor3uiv: procedure(var v: LongWord); cdecl;
  glColor3usv: procedure(var v: Word); cdecl;
  glColor4bv : procedure(var v: ShortInt); cdecl;
  glColor4dv : procedure(var v: Double); cdecl;
  glColor4fv : procedure(var v: Single); cdecl;
  glColor4iv : procedure(var v: LongInt); cdecl;
  glColor4sv : procedure(var v: SmallInt); cdecl;
  glColor4ubv: procedure(var v: Byte); cdecl;
  glColor4uiv: procedure(var v: LongWord); cdecl;
  glColor4usv: procedure(var v: Word); cdecl;
  glTexCoord1d: procedure(s: Double); cdecl;
  glTexCoord1f: procedure(s: Single); cdecl;
  glTexCoord1i: procedure(s: LongInt); cdecl;
  glTexCoord1s: procedure(s: SmallInt); cdecl;
  glTexCoord2d: procedure(s, t: Double); cdecl;
  glTexCoord2f: procedure(s, t: Single); cdecl;
  glTexCoord2i: procedure(s, t: LongInt); cdecl;
  glTexCoord2s: procedure(s, t: SmallInt); cdecl;
  glTexCoord3d: procedure(s, t, r: Double); cdecl;
  glTexCoord3f: procedure(s, t, r: Single); cdecl;
  glTexCoord3i: procedure(s, t, r: LongInt); cdecl;
  glTexCoord3s: procedure(s, t, r: SmallInt); cdecl;
  glTexCoord4d: procedure(s, t, r, q: Double); cdecl;
  glTexCoord4f: procedure(s, t, r, q: Single); cdecl;
  glTexCoord4i: procedure(s, t, r, q: LongInt); cdecl;
  glTexCoord4s: procedure(s, t, r, q: SmallInt); cdecl;
  glTexCoord1dv: procedure(var v: Double); cdecl;
  glTexCoord1fv: procedure(var v: Single); cdecl;
  glTexCoord1iv: procedure(var v: LongInt); cdecl;
  glTexCoord1sv: procedure(var v: SmallInt); cdecl;
  glTexCoord2dv: procedure(var v: Double); cdecl;
  glTexCoord2fv: procedure(var v: Single); cdecl;
  glTexCoord2iv: procedure(var v: LongInt); cdecl;
  glTexCoord2sv: procedure(var v: SmallInt); cdecl;
  glTexCoord3dv: procedure(var v: Double); cdecl;
  glTexCoord3fv: procedure(var v: Single); cdecl;
  glTexCoord3iv: procedure(var v: LongInt); cdecl;
  glTexCoord3sv: procedure(var v: SmallInt); cdecl;
  glTexCoord4dv: procedure(var v: Double); cdecl;
  glTexCoord4fv: procedure(var v: Single); cdecl;
  glTexCoord4iv: procedure(var v: LongInt); cdecl;
  glTexCoord4sv: procedure(var v: SmallInt); cdecl;
  glRasterPos2d: procedure(x, y: Double); cdecl;
  glRasterPos2f: procedure(x, y: Single); cdecl;
  glRasterPos2i: procedure(x, y: LongInt); cdecl;
  glRasterPos2s: procedure(x, y: SmallInt); cdecl;
  glRasterPos3d: procedure(x, y, z: Double); cdecl;
  glRasterPos3f: procedure(x, y, z: Single); cdecl;
  glRasterPos3i: procedure(x, y, z: LongInt); cdecl;
  glRasterPos3s: procedure(x, y, z: SmallInt); cdecl;
  glRasterPos4d: procedure(x, y, z, w: Double); cdecl;
  glRasterPos4f: procedure(x, y, z, w: Single); cdecl;
  glRasterPos4i: procedure(x, y, z, w: LongInt); cdecl;
  glRasterPos4s: procedure(x, y, z, w: SmallInt); cdecl;
  glRasterPos2dv: procedure(var v: Double); cdecl;
  glRasterPos2fv: procedure(var v: Single); cdecl;
  glRasterPos2iv: procedure(var v: LongInt); cdecl;
  glRasterPos2sv: procedure(var v: SmallInt); cdecl;
  glRasterPos3dv: procedure(var v: Double); cdecl;
  glRasterPos3fv: procedure(var v: Single); cdecl;
  glRasterPos3iv: procedure(var v: LongInt); cdecl;
  glRasterPos3sv: procedure(var v: SmallInt); cdecl;
  glRasterPos4dv: procedure(var v: Double); cdecl;
  glRasterPos4fv: procedure(var v: Single); cdecl;
  glRasterPos4iv: procedure(var v: LongInt); cdecl;
  glRasterPos4sv: procedure(var v: SmallInt); cdecl;
  glRectd: procedure(x1, y1, x2, y2: Double); cdecl;
  glRectf: procedure(x1, y1, x2, y2: Single); cdecl;
  glRecti: procedure(x1, y1, x2, y2: LongInt); cdecl;
  glRects: procedure(x1, y1, x2, y2: SmallInt); cdecl;
  glRectdv: procedure(var v1, v2: Double); cdecl;
  glRectfv: procedure(var v1, v2: Single); cdecl;
  glRectiv: procedure(var v1, v2: LongInt); cdecl;
  glRectsv: procedure(var v1, v2: SmallInt); cdecl;

  // Vertex Arrays (1.1)
  glVertexPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); cdecl;
  glNormalPointer: procedure(AType: GLenum; stride: LongInt; var ptr); cdecl;
  glColorPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); cdecl;
  glIndexPointer: procedure(AType: GLenum; stride: LongInt; var ptr); cdecl;
  glTexCoordPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); cdecl;
  glEdgeFlagPointer: procedure(stride: LongInt; var ptr); cdecl;
  glGetPointerv: procedure(pname: GLenum; var params: Pointer); cdecl;
  glArrayElement: procedure(i: LongInt); cdecl;
  glDrawArrays: procedure(mode: GLenum; first, count: LongInt); cdecl;
  glDrawElements: procedure(mode: GLenum; count: Integer; AType: GLenum; var indices); cdecl;
  glInterleavedArrays: procedure(format: GLenum; stride: LongInt; var pointer); cdecl;

  // Lighting
  glShadeModel: procedure(mode: GLenum); cdecl;
  glLightf: procedure(light, pname: GLenum; param: Single); cdecl;
  glLighti: procedure(light, pname: GLenum; param: LongInt); cdecl;
  glLightfv: procedure(light, pname: GLenum; var params: Single); cdecl;
  glLightiv: procedure(light, pname: GLenum; var params: LongInt); cdecl;
  glGetLightfv: procedure(light, pname: GLenum; var params: Single); cdecl;
  glGetLightiv: procedure(light, pname: GLenum; var params: LongInt); cdecl;
  glLightModelf: procedure(pname: GLenum; param: Single); cdecl;
  glLightModeli: procedure(pname: GLenum; param: LongInt); cdecl;
  glLightModelfv: procedure(pname: GLenum; var params: Single); cdecl;
  glLightModeliv: procedure(pname: GLenum; var param: LongInt); cdecl;
  glMaterialf: procedure(face, pname: GLenum; param: Single); cdecl;
  glMateriali: procedure(face, pname: GLenum; param: LongInt); cdecl;
  glMaterialfv: procedure(face, pname: GLenum; var params: Single); cdecl;
  glMaterialiv: procedure(face, pname: GLenum; var params: LongInt); cdecl;
  glGetMaterialfv: procedure(face, pname: GLenum; var params: Single); cdecl;
  glGetMaterialiv: procedure(face, pname: GLenum; var params: LongInt); cdecl;
  glColorMaterial: procedure(face, mode: GLenum); cdecl;

  // Raster Functions
  glPixelZoom: procedure(xfactor, yfactor: Single); cdecl;
  glPixelStoref: procedure(pname: GLenum; param: Single); cdecl;
  glPixelStorei: procedure(pname: GLenum; param: LongInt); cdecl;
  glPixelTransferf: procedure(pname: GLenum; param: Single); cdecl;
  glPixelTransferi: procedure(pname: GLenum; param: LongInt); cdecl;
  glPixelMapfv: procedure(map: GLenum; mapsize: LongInt; var values: Single); cdecl;
  glPixelMapuiv: procedure(map: GLenum; mapsize: LongInt; var values: LongWord); cdecl;
  glPixelMapusv: procedure(map: GLenum; mapsize: LongInt; var values: Word); cdecl;
  glGetPixelMapfv: procedure(map: GLenum; var values: Single); cdecl;
  glGetPixelMapuiv: procedure(map: GLenum; var values: LongWord); cdecl;
  glGetPixelMapusv: procedure(map: GLenum; var values: Word); cdecl;
  glBitmap: procedure(width, height: LongInt; xorig, yorig, xmove, ymove: Single; var bitmap); cdecl;
  glReadPixels: procedure(x, y, width, height: LongInt; format, AType: GLenum; var pixels); cdecl;
  glDrawPixels: procedure(width, height: LongInt; format, AType: GLenum; var pixels); cdecl;
  glCopyPixels: procedure(x, y, width, height: LongInt; AType: GLenum); cdecl;

  // Stenciling
  glStencilFunc: procedure(func: GLenum; ref: LongInt; mask: LongWord); cdecl;
  glStencilMask: procedure(mask: LongWord); cdecl;
  glStencilOp: procedure(fail, zfail, zpass: GLenum); cdecl;
  glClearStencil: procedure(s: LongInt); cdecl;

  // Texture Mapping
  glTexGend: procedure(cord, pname: GLenum; param: Double); cdecl;
  glTexGenf: procedure(cord, pname: GLenum; param: Single); cdecl;
  glTexGeni: procedure(cord, pname: GLenum; param: LongInt); cdecl;
  glTexGendv: procedure(cord, pname: GLenum; var params: Double); cdecl;
  glTexGenfv: procedure(cord, pname: GLenum; var params: Single); cdecl;
  glTexGeniv: procedure(cord, pname: GLenum; var params: LongInt); cdecl;
  glGetTexGendv: procedure(cord, pname: GLenum; var params: Double); cdecl;
  glGetTexGenfv: procedure(cord, pname: GLenum; var params: Single); cdecl;
  glGetTexGeniv: procedure(cord, pname: GLenum; var params: LongInt); cdecl;
  glTexEnvf: procedure(target, pname: GLenum; param: Single); cdecl;
  glTexEnvi: procedure(target, pname: GLenum; param: LongInt); cdecl;
  glTexEnvfv: procedure(target, pname: GLenum; var params: Single); cdecl;
  glTexEnviv: procedure(target, pname: GLenum; var params: LongInt); cdecl;
  glGetTexEnvfv: procedure(target, pname: GLenum; var params: Single); cdecl;
  glGetTexEnviv: procedure(target, pname: GLenum; var params: LongInt); cdecl;
  glTexParameterf: procedure(target, pname: GLenum; param: Single); cdecl;
  glTexParameteri: procedure(target, pname: GLenum; param: LongInt); cdecl;
  glTexParameterfv: procedure(target, pname: GLenum; var params: Single); cdecl;
  glTexParameteriv: procedure(target, pname: GLenum; var params: LongInt); cdecl;
  glGetTexParameterfv: procedure(target, pname: GLenum; var params: Single); cdecl;
  glGetTexParameteriv: procedure(target, pname: GLenum; var params: LongInt); cdecl;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: LongInt; pname: GLenum; var params: Single); cdecl;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: LongInt; pname: GLenum; var params: LongInt); cdecl;
  glTexImage1D: procedure(target: GLenum; level, internalFormat, width, border: LongInt; format, AType: GLenum; var pixels); cdecl;
  glTexImage2D: procedure(target: GLenum; level, internalFormat, width, height, border: LongInt; format, AType: GLenum; var pixels); cdecl;
  glGetTexImage: procedure(target: GLenum; level: LongInt; format, AType: GLenum; var pixels); cdecl;
  // 1.1 functions:
  glGenTextures: procedure(n: LongInt; var textures: LongWord); cdecl;
  glDeleteTextures: procedure(n: LongInt; var textures: LongWord); cdecl;
  glBindTexture: procedure(target: GLenum; texture: LongWord); cdecl;
  glPrioritizeTextures: procedure(n: LongInt; var textures: LongWord; var priorities: GLclampf); cdecl;
  glAreTexturesResident: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; cdecl;
  glIsTexture: function(texture: LongWord): Boolean; cdecl;
  glTexSubImage1D: procedure(target: GLenum; level, xoffset, width: LongInt; format, AType: GLenum; var pixels); cdecl;
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, width, height: LongInt; format, AType: GLenum; var pixels); cdecl;
  glCopyTexImage1D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, border: LongInt); cdecl;
  glCopyTexImage2D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, height, border: LongInt); cdecl;
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y, width: LongInt); cdecl;
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y, width, height: LongInt); cdecl;

  // Evaluators
  glMap1d: procedure(target: GLenum; u1, u2: Double; stride, order: LongInt; var points: Double); cdecl;
  glMap1f: procedure(target: GLenum; u1, u2: Single; stride, order: LongInt; var points: Single); cdecl;
  glMap2d: procedure(target: GLenum; u1, u2: Double; ustride, uorder: LongInt; v1, v2: Double; vstride, vorder: LongInt; var points: Double); cdecl;
  glMap2f: procedure(target: GLenum; u1, u2: Single; ustride, uorder: LongInt; v1, v2: Single; vstride, vorder: LongInt; var points: Single); cdecl;
  glGetMapdv: procedure(target, query: GLenum; var v: Double); cdecl;
  glGetMapfv: procedure(target, query: GLenum; var v: Single); cdecl;
  glGetMapiv: procedure(target, query: GLenum; var v: LongInt); cdecl;
  glEvalCoord1d: procedure(u: Double); cdecl;
  glEvalCoord1f: procedure(u: Single); cdecl;
  glEvalCoord1dv: procedure(var u: Double); cdecl;
  glEvalCoord1fv: procedure(var u: Single); cdecl;
  glEvalCoord2d: procedure(u, v: Double); cdecl;
  glEvalCoord2f: procedure(u, v: Single); cdecl;
  glEvalCoord2dv: procedure(var u, v: Double); cdecl;
  glEvalCoord2fv: procedure(var u, v: Single); cdecl;
  glMapGrid1d: procedure(un: LongInt; u1, u2: Double); cdecl;
  glMapGrid1f: procedure(un: LongInt; u1, u2: Single); cdecl;
  glMapGrid2d: procedure(un: LongInt; u1, u2: Double; vn: LongInt; v1, v2: Double); cdecl;
  glMapGrid2f: procedure(un: LongInt; u1, u2: Single; vn: LongInt; v1, v2: Single); cdecl;
  glEvalPoint1: procedure(i: LongInt); cdecl;
  glEvalPoint2: procedure(i, j: LongInt); cdecl;
  glEvalMesh1: procedure(mode: GLenum; i1, i2: LongInt); cdecl;
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: LongInt); cdecl;

  // Fog
  glFogf: procedure(pname: GLenum; param: Single); cdecl;
  glFogi: procedure(pname: GLenum; param: LongInt); cdecl;
  glFogfv: procedure(pname: GLenum; var params: Single); cdecl;
  glFogiv: procedure(pname: GLenum; var params: LongInt); cdecl;

  // Selection and Feedback
  glFeedbackBuffer: procedure(size: LongInt; AType: GLenum; var buffer: Single); cdecl;
  glPassThrough: procedure(token: Single); cdecl;
  glSelectBuffer: procedure(size: LongInt; var buffer: LongWord); cdecl;
  glInitNames: procedure; cdecl;
  glLoadName: procedure(name: LongWord); cdecl;
  glPushName: procedure(name: LongWord); cdecl;
  glPopName: procedure; cdecl;

const
// -------------------------------------------------------
//   GL extensions constants
// -------------------------------------------------------

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

  // GL_SGIS_multitexture
  GL_SELECTED_TEXTURE_SGIS              = $835C;
  GL_SELECTED_TEXTURE_COORD_SET_SGIS    = $835D;
  GL_MAX_TEXTURES_SGIS                  = $835E;
  GL_TEXTURE0_SGIS                      = $835F;
  GL_TEXTURE1_SGIS                      = $8360;
  GL_TEXTURE2_SGIS                      = $8361;
  GL_TEXTURE3_SGIS                      = $8362;
  GL_TEXTURE_COORD_SET_SOURCE_SGIS      = $8363;

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

  // GL_SGIS_texture_edge_clamp
  GL_CLAMP_TO_EDGE_SGIS                 = $812F;


// -------------------------------------------------------
//   GL Extension Procedures
// -------------------------------------------------------

var

  // === 1.0 Extensions ===

  // GL_EXT_blend_minmax
  glBlendEquationEXT: procedure(mode: GLenum); cdecl;

  // GL_EXT_blend_color
  glBlendColorEXT: procedure(red, green, blue, alpha: GLclampf); cdecl;

  // GL_EXT_polygon_offset
  glPolygonOffsetEXT: procedure(factor, bias: Single); cdecl;

  // GL_EXT_vertex_array
  glVertexPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); cdecl;
  glNormalPointerEXT: procedure(AType: GLenum; stride, count: LongInt; var ptr); cdecl;
  glColorPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); cdecl;
  glIndexPointerEXT: procedure(AType: GLenum; stride, count: LongInt; var ptr); cdecl;
  glTexCoordPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); cdecl;
  glEdgeFlagPointerEXT: procedure(stride, count: LongInt; var ptr: Boolean); cdecl;
  glGetPointervEXT: procedure(pname: GLenum; var params: Pointer); cdecl;
  glArrayElementEXT: procedure(i: LongInt); cdecl;
  glDrawArraysEXT: procedure(mode: GLEnum; first, count: LongInt); cdecl;

  // GL_EXT_texture_object
  glGenTexturesEXT: procedure(n: LongInt; var textures: LongWord); cdecl;
  glDeleteTexturesEXT: procedure(n: LongInt; var textures: LongWord); cdecl;
  glBindTextureEXT: procedure(target: GLenum; texture: LongWord); cdecl;
  glPrioritizeTexturesEXT: procedure(n: LongInt; var textures: LongWord; var priorities: GLClampf); cdecl;
  glAreTexturesResidentEXT: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; cdecl;
  glIsTextureEXT: function(texture: LongWord): Boolean; cdecl;

  // GL_EXT_texture3D
  glTexImage3DEXT: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLenum; var pixels); cdecl;
  glTexSubImage3DEXT: procedure(target: GLenum; level, xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLenum; var pixels); cdecl;
  glCopyTexSubImage3DEXT: procedure(target: GLenum; level, xoffset, yoffset, zoffset, x, y, width, height: LongInt); cdecl;

  // GL_EXT_color_table
  glColorTableEXT: procedure(target, internalformat: GLenum; width: LongInt; format, AType: GLenum; var table); cdecl;
  glColorSubTableEXT: procedure(target: GLenum; start, count: LongInt; format, AType: GLEnum; var data); cdecl;
  glGetColorTableEXT: procedure(target, format, AType: GLenum; var table); cdecl;
  glGetColorTableParameterfvEXT: procedure(target, pname: GLenum; var params: Single); cdecl;
  glGetColorTableParameterivEXT: procedure(target, pname: GLenum; var params: LongInt); cdecl;

  // GL_SGIS_multitexture
  glMultiTexCoord1dSGIS: procedure(target: GLenum; s: Double); cdecl;
  glMultiTexCoord1dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord1fSGIS: procedure(target: GLenum; s: Single); cdecl;
  glMultiTexCoord1fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord1iSGIS: procedure(target: GLenum; s: LongInt); cdecl;
  glMultiTexCoord1ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord1sSGIS: procedure(target: GLenum; s: ShortInt); cdecl;
  glMultiTexCoord1svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoord2dSGIS: procedure(target: GLenum; s, t: Double); cdecl;
  glMultiTexCoord2dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord2fSGIS: procedure(target: GLenum; s, t: Single); cdecl;
  glMultiTexCoord2fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord2iSGIS: procedure(target: GLenum; s, t: LongInt); cdecl;
  glMultiTexCoord2ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord2sSGIS: procedure(target: GLenum; s, t: ShortInt); cdecl;
  glMultiTexCoord2svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoord3dSGIS: procedure(target: GLenum; s, t, r: Double); cdecl;
  glMultiTexCoord3dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord3fSGIS: procedure(target: GLenum; s, t, r: Single); cdecl;
  glMultiTexCoord3fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord3iSGIS: procedure(target: GLenum; s, t, r: LongInt); cdecl;
  glMultiTexCoord3ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord3sSGIS: procedure(target: GLenum; s, t, r: ShortInt); cdecl;
  glMultiTexCoord3svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoord4dSGIS: procedure(target: GLenum; s, t, r, q: Double); cdecl;
  glMultiTexCoord4dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord4fSGIS: procedure(target: GLenum; s, t, r, q: Single); cdecl;
  glMultiTexCoord4fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord4iSGIS: procedure(target: GLenum; s, t, r, q: LongInt); cdecl;
  glMultiTexCoord4ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord4sSGIS: procedure(target: GLenum; s, t, r, q: ShortInt); cdecl;
  glMultiTexCoord4svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoordPointerSGIS: procedure(target: GLenum; size: LongInt; AType: GLEnum; stride: LongInt; var APointer); cdecl;
  glSelectTextureSGIS: procedure(target: GLenum); cdecl;
  glSelectTextureCoordSetSGIS: procedure(target: GLenum); cdecl;

  // GL_EXT_multitexture
  glMultiTexCoord1dEXT: procedure(target: GLenum; s: Double); cdecl;
  glMultiTexCoord1dvEXT: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord1fEXT: procedure(target: GLenum; s: Single); cdecl;
  glMultiTexCoord1fvEXT: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord1iEXT: procedure(target: GLenum; s: LongInt); cdecl;
  glMultiTexCoord1ivEXT: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord1sEXT: procedure(target: GLenum; s: ShortInt); cdecl;
  glMultiTexCoord1svEXT: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoord2dEXT: procedure(target: GLenum; s, t: Double); cdecl;
  glMultiTexCoord2dvEXT: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord2fEXT: procedure(target: GLenum; s, t: Single); cdecl;
  glMultiTexCoord2fvEXT: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord2iEXT: procedure(target: GLenum; s, t: LongInt); cdecl;
  glMultiTexCoord2ivEXT: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord2sEXT: procedure(target: GLenum; s, t: ShortInt); cdecl;
  glMultiTexCoord2svEXT: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoord3dEXT: procedure(target: GLenum; s, t, r: Double); cdecl;
  glMultiTexCoord3dvEXT: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord3fEXT: procedure(target: GLenum; s, t, r: Single); cdecl;
  glMultiTexCoord3fvEXT: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord3iEXT: procedure(target: GLenum; s, t, r: LongInt); cdecl;
  glMultiTexCoord3ivEXT: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord3sEXT: procedure(target: GLenum; s, t, r: ShortInt); cdecl;
  glMultiTexCoord3svEXT: procedure(target: GLenum; var v: ShortInt); cdecl;
  glMultiTexCoord4dEXT: procedure(target: GLenum; s, t, r, q: Double); cdecl;
  glMultiTexCoord4dvEXT: procedure(target: GLenum; var v: Double); cdecl;
  glMultiTexCoord4fEXT: procedure(target: GLenum; s, t, r, q: Single); cdecl;
  glMultiTexCoord4fvEXT: procedure(target: GLenum; var v: Single); cdecl;
  glMultiTexCoord4iEXT: procedure(target: GLenum; s, t, r, q: LongInt); cdecl;
  glMultiTexCoord4ivEXT: procedure(target: GLenum; var v: LongInt); cdecl;
  glMultiTexCoord4sEXT: procedure(target: GLenum; s, t, r, q: ShortInt); cdecl;
  glMultiTexCoord4svEXT: procedure(target: GLenum; var v: ShortInt); cdecl;
  glInterleavedTextureCoordSetsEXT: procedure(factor: LongInt); cdecl;
  glSelectTextureEXT: procedure(target: GLenum); cdecl;
  glSelectTextureCoordSetEXT: procedure(target: GLenum); cdecl;
  glSelectTextureTransformEXT: procedure(target: GLenum); cdecl;

  // GL_EXT_point_parameters
  glPointParameterfEXT: procedure(pname: GLenum; param: Single); cdecl;
  glPointParameterfvEXT: procedure(pname: GLenum; var params: Single); cdecl;

  // GL_MESA_window_pos
  glWindowPos2iMESA: procedure(x, y: LongInt); cdecl;
  glWindowPos2sMESA: procedure(x, y: ShortInt); cdecl;
  glWindowPos2fMESA: procedure(x, y: Single); cdecl;
  glWindowPos2dMESA: procedure(x, y: Double); cdecl;
  glWindowPos2ivMESA: procedure(var p: LongInt); cdecl;
  glWindowPos2svMESA: procedure(var p: ShortInt); cdecl;
  glWindowPos2fvMESA: procedure(var p: Single); cdecl;
  glWindowPos2dvMESA: procedure(var p: Double); cdecl;
  glWindowPos3iMESA: procedure(x, y, z: LongInt); cdecl;
  glWindowPos3sMESA: procedure(x, y, z: ShortInt); cdecl;
  glWindowPos3fMESA: procedure(x, y, z: Single); cdecl;
  glWindowPos3dMESA: procedure(x, y, z: Double); cdecl;
  glWindowPos3ivMESA: procedure(var p: LongInt); cdecl;
  glWindowPos3svMESA: procedure(var p: ShortInt); cdecl;
  glWindowPos3fvMESA: procedure(var p: Single); cdecl;
  glWindowPos3dvMESA: procedure(var p: Double); cdecl;
  glWindowPos4iMESA: procedure(x, y, z, w: LongInt); cdecl;
  glWindowPos4sMESA: procedure(x, y, z, w: ShortInt); cdecl;
  glWindowPos4fMESA: procedure(x, y, z, w: Single); cdecl;
  glWindowPos4dMESA: procedure(x, y, z, w: Double); cdecl;
  glWindowPos4ivMESA: procedure(var p: LongInt); cdecl;
  glWindowPos4svMESA: procedure(var p: ShortInt); cdecl;
  glWindowPos4fvMESA: procedure(var p: Single); cdecl;
  glWindowPos4dvMESA: procedure(var p: Double); cdecl;

  // GL_MESA_resize_buffers
  glResizeBuffersMESA: procedure; cdecl;


// ===================================================================
//   GLU consts, types and functions
// ===================================================================

const
  GLU_TRUE                              = GL_TRUE;
  GLU_FALSE                             = GL_FALSE;

  // Normal vectors
  GLU_SMOOTH                            = 100000;
  GLU_FLAT                              = 100001;
  GLU_NONE                              = 100002;

  // Quadric draw styles
  GLU_POINT                             = 100010;
  GLU_LINE                              = 100011;
  GLU_FILL                              = 100012;
  GLU_SILHOUETTE                        = 100013;

  // Quadric orientation
  GLU_OUTSIDE                           = 100020;
  GLU_INSIDE                            = 100021;

  // Tesselator
  GLU_BEGIN                             = 100100;
  GLU_VERTEX                            = 100101;
  GLU_END                               = 100102;
  GLU_ERROR                             = 100103;
  GLU_EDGE_FLAG                         = 100104;

  // Contour types
  GLU_CW                                = 100120;
  GLU_CCW                               = 100121;
  GLU_INTERIOR                          = 100122;
  GLU_EXTERIOR                          = 100123;
  GLU_UNKNOWN                           = 100124;

  // Tesselation errors
  GLU_TESS_ERROR1                       = 100151;  // missing gluEndPolygon
  GLU_TESS_ERROR2                       = 100152;  // missing gluBeginPolygon
  GLU_TESS_ERROR3                       = 100153;  // misoriented contour
  GLU_TESS_ERROR4                       = 100154;  // vertex/edge intersection
  GLU_TESS_ERROR5                       = 100155;  // misoriented or self-intersecting loops
  GLU_TESS_ERROR6                       = 100156;  // coincident vertices
  GLU_TESS_ERROR7                       = 100157;  // all vertices collinear
  GLU_TESS_ERROR8                       = 100158;  // intersecting edges
  GLU_TESS_ERROR9                       = 100159;  // not coplanar contours

  // NURBS
  GLU_AUTO_LOAD_MATRIX                  = 100200;
  GLU_CULLING                           = 100201;
  GLU_PARAMETRIC_TOLERANCE              = 100202;
  GLU_SAMPLING_TOLERANCE                = 100203;
  GLU_DISPLAY_MODE                      = 100204;
  GLU_SAMPLING_METHOD                   = 100205;
  GLU_U_STEP                            = 100206;
  GLU_V_STEP                            = 100207;

  GLU_PATH_LENGTH                       = 100215;
  GLU_PARAMETRIC_ERROR                  = 100216;
  GLU_DOMAIN_DISTANCE                   = 100217;

  GLU_MAP1_TRIM_2                       = 100210;
  GLU_MAP1_TRIM_3                       = 100211;

  GLU_OUTLINE_POLYGON                   = 100240;
  GLU_OUTLINE_PATCH                     = 100241;

  GLU_NURBS_ERROR1                      = 100251;   // spline order un-supported
  GLU_NURBS_ERROR2                      = 100252;   // too few knots
  GLU_NURBS_ERROR3                      = 100253;   // valid knot range is empty
  GLU_NURBS_ERROR4                      = 100254;   // decreasing knot sequence
  GLU_NURBS_ERROR5                      = 100255;   // knot multiplicity > spline order
  GLU_NURBS_ERROR6                      = 100256;   // endcurve() must follow bgncurve()
  GLU_NURBS_ERROR7                      = 100257;   // bgncurve() must precede endcurve()
  GLU_NURBS_ERROR8                      = 100258;   // ctrlarray or knot vector is NULL
  GLU_NURBS_ERROR9                      = 100259;   // cannot draw pwlcurves
  GLU_NURBS_ERROR10                     = 100260;   // missing gluNurbsCurve()
  GLU_NURBS_ERROR11                     = 100261;   // missing gluNurbsSurface()
  GLU_NURBS_ERROR12                     = 100262;   // endtrim() must precede endsurface()
  GLU_NURBS_ERROR13                     = 100263;   // bgnsurface() must precede endsurface()
  GLU_NURBS_ERROR14                     = 100264;   // curve of improper type passed as trim curve
  GLU_NURBS_ERROR15                     = 100265;   // bgnsurface() must precede bgntrim()
  GLU_NURBS_ERROR16                     = 100266;   // endtrim() must follow bgntrim()
  GLU_NURBS_ERROR17                     = 100267;   // bgntrim() must precede endtrim()*/
  GLU_NURBS_ERROR18                     = 100268;   // invalid or missing trim curve*/
  GLU_NURBS_ERROR19                     = 100269;   // bgntrim() must precede pwlcurve()
  GLU_NURBS_ERROR20                     = 100270;   // pwlcurve referenced twice*/
  GLU_NURBS_ERROR21                     = 100271;   // pwlcurve and nurbscurve mixed
  GLU_NURBS_ERROR22                     = 100272;   // improper usage of trim data type
  GLU_NURBS_ERROR23                     = 100273;   // nurbscurve referenced twice
  GLU_NURBS_ERROR24                     = 100274;   // nurbscurve and pwlcurve mixed
  GLU_NURBS_ERROR25                     = 100275;   // nurbssurface referenced twice
  GLU_NURBS_ERROR26                     = 100276;   // invalid property
  GLU_NURBS_ERROR27                     = 100277;   // endsurface() must follow bgnsurface()
  GLU_NURBS_ERROR28                     = 100278;   // intersecting or misoriented trim curves
  GLU_NURBS_ERROR29                     = 100279;   // intersecting trim curves
  GLU_NURBS_ERROR30                     = 100280;   // UNUSED
  GLU_NURBS_ERROR31                     = 100281;   // unconnected trim curves
  GLU_NURBS_ERROR32                     = 100282;   // unknown knot error
  GLU_NURBS_ERROR33                     = 100283;   // negative vertex count encountered
  GLU_NURBS_ERROR34                     = 100284;   // negative byte-stride
  GLU_NURBS_ERROR35                     = 100285;   // unknown type descriptor
  GLU_NURBS_ERROR36                     = 100286;   // null control point reference
  GLU_NURBS_ERROR37                     = 100287;   // duplicate point on pwlcurve

  // Errors
  GLU_INVALID_ENUM                      = 100900;
  GLU_INVALID_VALUE                     = 100901;
  GLU_OUT_OF_MEMORY                     = 100902;
  GLU_INCOMPATIBLE_GL_VERSION           = 100903;

  // New in GLU 1.1
  GLU_VERSION                           = 100800;
  GLU_EXTENSIONS                        = 100801;

type
  PGLUquadricObj = ^TGLUquadricObj;
  TGLUquadricObj = record end;
  PGLUtriangulatorObj = ^TGLUtriangulatorObj;
  TGLUtriangulatorObj = record end;
  PGLUnurbsObj = ^TGLUnurbsObj;
  TGLUnurbsObj = record end;

  TGLUQuadricCallback = procedure; cdecl;
  TGLUNurbsCallback = procedure; cdecl;
  TGLUTessCallback = procedure; cdecl;

  TGLUViewport = array[0..3] of LongInt;
  TGLUMatrixd = array[0..15] of Double;
  TGLUMatrixf = array[0..15] of Single;
  TGLUVectord = array[0..2] of Double;
var
  // Miscellaneous functions
  gluLookAt: procedure(eye, eyey, eyez, centerx, centery, centerz, upx, upy, upz: Double); cdecl;
  gluOrtho2D: procedure(left, right, bottom, top: Double); cdecl;
  gluPerspective: procedure(fovy, aspect, zNear, zFar: Double); cdecl;
  gluPickMatrix: procedure(x, y, width, height: Double; const viewport: TGLUViewport); cdecl;
  gluProject: procedure(objx, objy, objz: Double; const modelMatrix, projMatrix: TGLUMatrixd; const viewport: TGLUViewport; winx, winy, winz: Double); cdecl;
  gluUnProject: procedure(winx, winy, winz: Double; const modelMatrix, projMatrix: TGLUMatrixd; const viewport: TGLUViewport; objx, objy, objz: Double); cdecl;
  gluErrorString: procedure(errorCode: GLenum); cdecl;

  // Mipmapping and image scaling
  gluScaleImage: procedure(format: GLenum; within, heightin: LongInt; typein: GLenum; var datain; widthout, heightout: LongInt; typeout: GLenum; var dataout); cdecl;
  gluBuild1DMipmaps: procedure(target: GLenum; components, width: LongInt; format, AType: GLEnum; var data); cdecl;
  gluBuild2DMipmaps: procedure(target: GLenum; components, width, height: LongInt; format, AType: GLEnum; var data); cdecl;

  // Quadrics
  gluNewQuadric: function: PGLUquadricObj; cdecl;
  gluDeleteQuadric: procedure(state: PGLUquadricObj); cdecl;
  gluQuadricDrawStyle: procedure(quadObject: PGLUquadricObj; drawStyle: GLenum); cdecl;
  gluQuadricOrientation: procedure(quadObject: PGLUquadricObj; orientation: GLenum); cdecl;
  gluQuadricNormals: procedure(quadObject: PGLUquadricObj; normals: GLenum); cdecl;
  gluQuadricTexture: procedure(quadObject: PGLUquadricObj; textureCoords: Boolean); cdecl;
  gluQuadricCallback: procedure(quadObject: PGLUquadricObj; which: GLenum; fn: TGLUQuadricCallback); cdecl;
  gluCylinder: procedure(qobj: PGLUquadricObj; baseRadius, topRadius, height: Double; slices, stacks: LongInt); cdecl;
  gluSphere: procedure(qobj: PGLUquadricObj; radius: Double; slices, stacks: LongInt); cdecl;
  gluDisk: procedure(qobj: PGLUquadricObj; innerRadius, outerRadius: Double; slices, loops: LongInt); cdecl;
  gluPartialDisk: procedure(qobj: PGLUquadricObj; innerRadius, outerRadius: Double; slices, loops: LongInt; startAngle, sweepAngle: Double); cdecl;

  // Nurbs
  gluNewNurbsRenderer: function: PGLUnurbsObj; cdecl;
  gluDeleteNurbsRenderer: procedure(nobj: PGLUnurbsObj); cdecl;
  gluLoadSamplingMatrices: procedure(nobj: PGLUnurbsObj; const modelMatrix, projMatrix: TGLUMatrixf; const viewport: TGLUViewport); cdecl;
  gluNurbsProperty: procedure(nobj: PGLUnurbsObj; AProperty: GLenum; value: Single); cdecl;
  gluGetNurbsProperty: procedure(nobj: PGLUnurbsObj; AProperty: GLEnum; var value: Single); cdecl;
  gluBeginCurve: procedure(nobj: PGLUnurbsObj); cdecl;
  gluEndCurve: procedure(nobj: PGLUnurbsObj); cdecl;
  gluNurbsCurve: procedure(nobj: PGLUnurbsObj; nknots: LongInt; var know: Single; stride: LongInt; var ctlarray: Single; order: LongInt; AType: GLenum); cdecl;
  gluBeginSurface: procedure(nobj: PGLUnurbsObj); cdecl;
  gluEndSurface: procedure(nobj: PGLUnurbsObj); cdecl;
  gluNurbsSurface: procedure(nobj: PGLUnurbsObj; sknot_count: LongInt; var sknot: Single; tknot_count: LongInt; var tknot: Single; s_stride, t_stride: LongInt; var ctlarray: Single; sorder, torder: LongInt; AType: GLenum); cdecl;
  gluBeginTrim: procedure(nobj: PGLUnurbsObj); cdecl;
  gluEndTrim: procedure(nobj: PGLUnurbsObj); cdecl;
  gluPwlCurve: procedure(nobj: PGLUnurbsObj; count: LongInt; var AArray: Single; stride: LongInt; AType: GLenum); cdecl;
  gluNurbsCallback: procedure(nobj: PGLUnurbsObj; which: GLenum; fn: TGLUNurbsCallback); cdecl;

  // Polygon tesselation
  gluNewTess: function: PGLUtriangulatorObj; cdecl;
  gluTessCallback: procedure(tobj: PGLUtriangulatorObj; which: GLenum; fn: TGLUTessCallback); cdecl;

  gluDeleteTess: procedure(tobj: PGLUtriangulatorObj); cdecl;
  gluBeginPolygon: procedure(tobj: PGLUtriangulatorObj); cdecl;
  gluEndPolygon: procedure(tobj: PGLUtriangulatorObj); cdecl;
  gluNextContour: procedure(tobj: PGLUtriangulatorObj; AType: GLenum); cdecl;
  gluTessVertex: procedure(tobj: PGLUtriangulatorObj; v: TGLUVectord; var data); cdecl;

  // New functions in GLU 1.1
  gluGetString: function(name: GLenum): PChar; cdecl;


// ===================================================================
//   GLX consts, types and functions
// ===================================================================

// Tokens for glXChooseVisual and glXGetConfig:
const
  GLX_USE_GL                            = 1;
  GLX_BUFFER_SIZE                       = 2;
  GLX_LEVEL                             = 3;
  GLX_RGBA                              = 4;
  GLX_DOUBLEBUFFER                      = 5;
  GLX_STEREO                            = 6;
  GLX_AUX_BUFFERS                       = 7;
  GLX_RED_SIZE                          = 8;
  GLX_GREEN_SIZE                        = 9;
  GLX_BLUE_SIZE                         = 10;
  GLX_ALPHA_SIZE                        = 11;
  GLX_DEPTH_SIZE                        = 12;
  GLX_STENCIL_SIZE                      = 13;
  GLX_ACCUM_RED_SIZE                    = 14;
  GLX_ACCUM_GREEN_SIZE                  = 15;
  GLX_ACCUM_BLUE_SIZE                   = 16;
  GLX_ACCUM_ALPHA_SIZE                  = 17;

  // GLX_EXT_visual_info extension
  GLX_X_VISUAL_TYPE_EXT                 = $22;
  GLX_TRANSPARENT_TYPE_EXT              = $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT       = $24;
  GLX_TRANSPARENT_RED_VALUE_EXT         = $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT       = $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT        = $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT       = $28;


  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN                        = 1;
  GLX_BAD_ATTRIBUTE                     = 2;
  GLX_NO_EXTENSION                      = 3;
  GLX_BAD_VISUAL                        = 4;
  GLX_BAD_CONTEXT                       = 5;
  GLX_BAD_VALUE                         = 6;
  GLX_BAD_ENUM                          = 7;

  // GLX 1.1 and later:
  GLX_VENDOR                            = 1;
  GLX_VERSION                           = 2;
  GLX_EXTENSIONS                        = 3;

  // GLX_visual_info extension
  GLX_TRUE_COLOR_EXT                    = $8002;
  GLX_DIRECT_COLOR_EXT                  = $8003;
  GLX_PSEUDO_COLOR_EXT                  = $8004;
  GLX_STATIC_COLOR_EXT                  = $8005;
  GLX_GRAY_SCALE_EXT                    = $8006;
  GLX_STATIC_GRAY_EXT                   = $8007;
  GLX_NONE_EXT                          = $8000;
  GLX_TRANSPARENT_RGB_EXT               = $8008;
  GLX_TRANSPARENT_INDEX_EXT             = $8009;

type
  // From XLib:
  XPixmap = XID;
  XFont = XID;
  XColormap = XID;

  GLXContext = Pointer;
  GLXPixmap = XID;
  GLXDrawable = XID;
  GLXContextID = XID;

var
  glXChooseVisual: function(dpy: PDisplay; screen: Integer; var attribList: Integer): PXVisualInfo; cdecl; cdecl;
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl; cdecl;
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl; cdecl;
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl; cdecl;
  glXCopyContext: procedure(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl; cdecl;
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl; cdecl;
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl; cdecl;
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl; cdecl;
  glXQueryExtension: function(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl; cdecl;
  glXQueryVersion: function(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl; cdecl;
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl; cdecl;
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl; cdecl;
  glXGetCurrentContext: function: GLXContext; cdecl; cdecl;
  glXGetCurrentDrawable: function: GLXDrawable; cdecl; cdecl;
  glXWaitGL: procedure; cdecl; cdecl;
  glXWaitX: procedure; cdecl; cdecl;
  glXUseXFont: procedure(font: XFont; first, count, list: Integer); cdecl; cdecl;

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: Integer): PChar; cdecl; cdecl;
  glXQueryServerString: function(dpy: PDisplay; screen, name: Integer): PChar; cdecl; cdecl;
  glXGetClientString: function(dpy: PDisplay; name: Integer): PChar; cdecl; cdecl;

  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl; cdecl;
  glXReleaseBufferMESA: function(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl; cdecl;
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl; cdecl;
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl; cdecl;
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl; cdecl;


// ===================================================================
// ===================================================================

implementation

{$LINKLIB m}

function dlopen(AFile: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; name: PChar): Pointer; external 'dl';

function LoadLibrary(name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

function GetProc(handle: Pointer; name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if Result = nil then WriteLn('Unresolved: ', name);
end;

var
  libGL, libGLU, libGLX: Pointer;

function InitGLFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGL := LoadLibrary(libname);
  if not Assigned(libGL) then exit;

  glClearIndex := GetProc(libgl, 'glClearIndex');
  glClearColor := GetProc(libgl, 'glClearColor');
  glClear := GetProc(libgl, 'glClear');
  glIndexMask := GetProc(libgl, 'glIndexMask');
  glColorMask := GetProc(libgl, 'glColorMask');
  glAlphaFunc := GetProc(libgl, 'glAlphaFunc');
  glBlendFunc := GetProc(libgl, 'glBlendFunc');
  glLogicOp := GetProc(libgl, 'glLogicOp');
  glCullFace := GetProc(libgl, 'glCullFace');
  glFrontFace := GetProc(libgl, 'glFrontFace');
  glPointSize := GetProc(libgl, 'glPointSize');
  glLineWidth := GetProc(libgl, 'glLineWidth');
  glLineStipple := GetProc(libgl, 'glLineStipple');
  glPolygonMode := GetProc(libgl, 'glPolygonMode');
  glPolygonOffset := GetProc(libgl, 'glPolygonOffset');
  glPolygonStipple := GetProc(libgl, 'glPolygonStipple');
  glGetPolygonStipple := GetProc(libgl, 'glGetPolygonStipple');
  glEdgeFlag := GetProc(libgl, 'glEdgeFlag');
  glEdgeFlagv := GetProc(libgl, 'glEdgeFlagv');
  glScissor := GetProc(libgl, 'glScissor');
  glClipPlane := GetProc(libgl, 'glClipPlane');
  glGetClipPlane := GetProc(libgl, 'glGetClipPlane');
  glDrawBuffer := GetProc(libgl, 'glDrawBuffer');
  glReadBuffer := GetProc(libgl, 'glReadBuffer');
  glEnable := GetProc(libgl, 'glEnable');
  glDisable := GetProc(libgl, 'glDisable');
  glIsEnabled := GetProc(libgl, 'glIsEnabled');
  glEnableClientState := GetProc(libgl, 'glEnableClientState');
  glDisableClientState := GetProc(libgl, 'glDisableClientState');
  glGetBooleanv := GetProc(libgl, 'glGetBooleanv');
  glGetDoublev := GetProc(libgl, 'glGetDoublev');
  glGetFloatv := GetProc(libgl, 'glGetFloatv');
  glGetIntegerv := GetProc(libgl, 'glGetIntegerv');
  glPushAttrib := GetProc(libgl, 'glPushAttrib');
  glPopAttrib := GetProc(libgl, 'glPopAttrib');
  glPushClientAttrib := GetProc(libgl, 'glPushClientAttrib');
  glPopClientAttrib := GetProc(libgl, 'glPopClientAttrib');
  glRenderMode := GetProc(libgl, 'glRenderMode');
  glGetError := GetProc(libgl, 'glGetError');
  glGetString := GetProc(libgl, 'glGetString');
  glFinish := GetProc(libgl, 'glFinish');
  glFlush := GetProc(libgl, 'glFlush');
  glHint := GetProc(libgl, 'glHint');
  glClearDepth := GetProc(libgl, 'glClearDepth');
  glDepthFunc := GetProc(libgl, 'glDepthFunc');
  glDepthMask := GetProc(libgl, 'glDepthMask');
  glDepthRange := GetProc(libgl, 'glDepthRange');
  glClearAccum := GetProc(libgl, 'glClearAccum');
  glAccum := GetProc(libgl, 'glAccum');
  glMatrixMode := GetProc(libgl, 'glMatrixMode');
  glOrtho := GetProc(libgl, 'glOrtho');
  glFrustum := GetProc(libgl, 'glFrustum');
  glViewport := GetProc(libgl, 'glViewport');
  glPushMatrix := GetProc(libgl, 'glPushMatrix');
  glPopMatrix := GetProc(libgl, 'glPopMatrix');
  glLoadIdentity := GetProc(libgl, 'glLoadIdentity');
  glLoadMatrixd := GetProc(libgl, 'glLoadMatrixd');
  glLoadMatrixf := GetProc(libgl, 'glLoadMatrixf');
  glMultMatrixd := GetProc(libgl, 'glMultMatrixd');
  glMultMatrixf := GetProc(libgl, 'glMultMatrixf');
  glRotated := GetProc(libgl, 'glRotated');
  glRotatef := GetProc(libgl, 'glRotatef');
  glScaled := GetProc(libgl, 'glScaled');
  glScalef := GetProc(libgl, 'glScalef');
  glTranslated := GetProc(libgl, 'glTranslated');
  glTranslatef := GetProc(libgl, 'glTranslatef');
  glIsList := GetProc(libgl, 'glIsList');
  glDeleteLists := GetProc(libgl, 'glDeleteLists');
  glGenLists := GetProc(libgl, 'glGenLists');
  glNewList := GetProc(libgl, 'glNewList');
  glEndList := GetProc(libgl, 'glEndList');
  glCallList := GetProc(libgl, 'glCallList');
  glCallLists := GetProc(libgl, 'glCallLists');
  glListBase := GetProc(libgl, 'glListBase');
  glBegin := GetProc(libgl, 'glBegin');
  glEnd := GetProc(libgl, 'glEnd');
  glVertex2d := GetProc(libgl, 'glVertex2d');
  glVertex2f := GetProc(libgl, 'glVertex2f');
  glVertex2i := GetProc(libgl, 'glVertex2i');
  glVertex2s := GetProc(libgl, 'glVertex2s');
  glVertex3d := GetProc(libgl, 'glVertex3d');
  glVertex3f := GetProc(libgl, 'glVertex3f');
  glVertex3i := GetProc(libgl, 'glVertex3i');
  glVertex3s := GetProc(libgl, 'glVertex3s');
  glVertex4d := GetProc(libgl, 'glVertex4d');
  glVertex4f := GetProc(libgl, 'glVertex4f');
  glVertex4i := GetProc(libgl, 'glVertex4i');
  glVertex4s := GetProc(libgl, 'glVertex4s');
  glVertex2dv := GetProc(libgl, 'glVertex2dv');
  glVertex2fv := GetProc(libgl, 'glVertex2fv');
  glVertex2iv := GetProc(libgl, 'glVertex2iv');
  glVertex2sv := GetProc(libgl, 'glVertex2sv');
  glVertex3dv := GetProc(libgl, 'glVertex3dv');
  glVertex3fv := GetProc(libgl, 'glVertex3fv');
  glVertex3iv := GetProc(libgl, 'glVertex3iv');
  glVertex3sv := GetProc(libgl, 'glVertex3sv');
  glVertex4dv := GetProc(libgl, 'glVertex4dv');
  glVertex4fv := GetProc(libgl, 'glVertex4fv');
  glVertex4iv := GetProc(libgl, 'glVertex4iv');
  glVertex4sv := GetProc(libgl, 'glVertex4sv');
  glNormal3b := GetProc(libgl, 'glNormal3b');
  glNormal3d := GetProc(libgl, 'glNormal3d');
  glNormal3f := GetProc(libgl, 'glNormal3f');
  glNormal3i := GetProc(libgl, 'glNormal3i');
  glNormal3s := GetProc(libgl, 'glNormal3s');
  glNormal3bv := GetProc(libgl, 'glNormal3bv');
  glNormal3dv := GetProc(libgl, 'glNormal3dv');
  glNormal3fv := GetProc(libgl, 'glNormal3fv');
  glNormal3iv := GetProc(libgl, 'glNormal3iv');
  glNormal3sv := GetProc(libgl, 'glNormal3sv');
  glIndexd := GetProc(libgl, 'glIndexd');
  glIndexf := GetProc(libgl, 'glIndexf');
  glIndexi := GetProc(libgl, 'glIndexi');
  glIndexs := GetProc(libgl, 'glIndexs');
  glIndexub := GetProc(libgl, 'glIndexub');
  glIndexdv := GetProc(libgl, 'glIndexdv');
  glIndexfv := GetProc(libgl, 'glIndexfv');
  glIndexiv := GetProc(libgl, 'glIndexiv');
  glIndexsv := GetProc(libgl, 'glIndexsv');
  glIndexubv := GetProc(libgl, 'glIndexubv');
  glColor3b := GetProc(libgl, 'glColor3b');
  glColor3d := GetProc(libgl, 'glColor3d');
  glColor3f := GetProc(libgl, 'glColor3f');
  glColor3i := GetProc(libgl, 'glColor3i');
  glColor3s := GetProc(libgl, 'glColor3s');
  glColor3ub := GetProc(libgl, 'glColor3ub');
  glColor3ui := GetProc(libgl, 'glColor3ui');
  glColor3us := GetProc(libgl, 'glColor3us');
  glColor4b := GetProc(libgl, 'glColor4b');
  glColor4d := GetProc(libgl, 'glColor4d');
  glColor4f := GetProc(libgl, 'glColor4f');
  glColor4i := GetProc(libgl, 'glColor4i');
  glColor4s := GetProc(libgl, 'glColor4s');
  glColor4ub := GetProc(libgl, 'glColor4ub');
  glColor4ui := GetProc(libgl, 'glColor4ui');
  glColor4us := GetProc(libgl, 'glColor4us');
  glColor3bv := GetProc(libgl, 'glColor3bv');
  glColor3dv := GetProc(libgl, 'glColor3dv');
  glColor3fv := GetProc(libgl, 'glColor3fv');
  glColor3iv := GetProc(libgl, 'glColor3iv');
  glColor3sv := GetProc(libgl, 'glColor3sv');
  glColor3ubv := GetProc(libgl, 'glColor3ubv');
  glColor3uiv := GetProc(libgl, 'glColor3uiv');
  glColor3usv := GetProc(libgl, 'glColor3usv');
  glColor4bv := GetProc(libgl, 'glColor4bv');
  glColor4dv := GetProc(libgl, 'glColor4dv');
  glColor4fv := GetProc(libgl, 'glColor4fv');
  glColor4iv := GetProc(libgl, 'glColor4iv');
  glColor4sv := GetProc(libgl, 'glColor4sv');
  glColor4ubv := GetProc(libgl, 'glColor4ubv');
  glColor4uiv := GetProc(libgl, 'glColor4uiv');
  glColor4usv := GetProc(libgl, 'glColor4usv');
  glTexCoord1d := GetProc(libgl, 'glTexCoord1d');
  glTexCoord1f := GetProc(libgl, 'glTexCoord1f');
  glTexCoord1i := GetProc(libgl, 'glTexCoord1i');
  glTexCoord1s := GetProc(libgl, 'glTexCoord1s');
  glTexCoord2d := GetProc(libgl, 'glTexCoord2d');
  glTexCoord2f := GetProc(libgl, 'glTexCoord2f');
  glTexCoord2i := GetProc(libgl, 'glTexCoord2i');
  glTexCoord2s := GetProc(libgl, 'glTexCoord2s');
  glTexCoord3d := GetProc(libgl, 'glTexCoord3d');
  glTexCoord3f := GetProc(libgl, 'glTexCoord3f');
  glTexCoord3i := GetProc(libgl, 'glTexCoord3i');
  glTexCoord3s := GetProc(libgl, 'glTexCoord3s');
  glTexCoord4d := GetProc(libgl, 'glTexCoord4d');
  glTexCoord4f := GetProc(libgl, 'glTexCoord4f');
  glTexCoord4i := GetProc(libgl, 'glTexCoord4i');
  glTexCoord4s := GetProc(libgl, 'glTexCoord4s');
  glTexCoord1dv := GetProc(libgl, 'glTexCoord1dv');
  glTexCoord1fv := GetProc(libgl, 'glTexCoord1fv');
  glTexCoord1iv := GetProc(libgl, 'glTexCoord1iv');
  glTexCoord1sv := GetProc(libgl, 'glTexCoord1sv');
  glTexCoord2dv := GetProc(libgl, 'glTexCoord2dv');
  glTexCoord2fv := GetProc(libgl, 'glTexCoord2fv');
  glTexCoord2iv := GetProc(libgl, 'glTexCoord2iv');
  glTexCoord2sv := GetProc(libgl, 'glTexCoord2sv');
  glTexCoord3dv := GetProc(libgl, 'glTexCoord3dv');
  glTexCoord3fv := GetProc(libgl, 'glTexCoord3fv');
  glTexCoord3iv := GetProc(libgl, 'glTexCoord3iv');
  glTexCoord3sv := GetProc(libgl, 'glTexCoord3sv');
  glTexCoord4dv := GetProc(libgl, 'glTexCoord4dv');
  glTexCoord4fv := GetProc(libgl, 'glTexCoord4fv');
  glTexCoord4iv := GetProc(libgl, 'glTexCoord4iv');
  glTexCoord4sv := GetProc(libgl, 'glTexCoord4sv');
  glRasterPos2d := GetProc(libgl, 'glRasterPos2d');
  glRasterPos2f := GetProc(libgl, 'glRasterPos2f');
  glRasterPos2i := GetProc(libgl, 'glRasterPos2i');
  glRasterPos2s := GetProc(libgl, 'glRasterPos2s');
  glRasterPos3d := GetProc(libgl, 'glRasterPos3d');
  glRasterPos3f := GetProc(libgl, 'glRasterPos3f');
  glRasterPos3i := GetProc(libgl, 'glRasterPos3i');
  glRasterPos3s := GetProc(libgl, 'glRasterPos3s');
  glRasterPos4d := GetProc(libgl, 'glRasterPos4d');
  glRasterPos4f := GetProc(libgl, 'glRasterPos4f');
  glRasterPos4i := GetProc(libgl, 'glRasterPos4i');
  glRasterPos4s := GetProc(libgl, 'glRasterPos4s');
  glRasterPos2dv := GetProc(libgl, 'glRasterPos2dv');
  glRasterPos2fv := GetProc(libgl, 'glRasterPos2fv');
  glRasterPos2iv := GetProc(libgl, 'glRasterPos2iv');
  glRasterPos2sv := GetProc(libgl, 'glRasterPos2sv');
  glRasterPos3dv := GetProc(libgl, 'glRasterPos3dv');
  glRasterPos3fv := GetProc(libgl, 'glRasterPos3fv');
  glRasterPos3iv := GetProc(libgl, 'glRasterPos3iv');
  glRasterPos3sv := GetProc(libgl, 'glRasterPos3sv');
  glRasterPos4dv := GetProc(libgl, 'glRasterPos4dv');
  glRasterPos4fv := GetProc(libgl, 'glRasterPos4fv');
  glRasterPos4iv := GetProc(libgl, 'glRasterPos4iv');
  glRasterPos4sv := GetProc(libgl, 'glRasterPos4sv');
  glRectd := GetProc(libgl, 'glRectd');
  glRectf := GetProc(libgl, 'glRectf');
  glRecti := GetProc(libgl, 'glRecti');
  glRects := GetProc(libgl, 'glRects');
  glRectdv := GetProc(libgl, 'glRectdv');
  glRectfv := GetProc(libgl, 'glRectfv');
  glRectiv := GetProc(libgl, 'glRectiv');
  glRectsv := GetProc(libgl, 'glRectsv');
  glVertexPointer := GetProc(libgl, 'glVertexPointer');
  glNormalPointer := GetProc(libgl, 'glNormalPointer');
  glColorPointer := GetProc(libgl, 'glColorPointer');
  glIndexPointer := GetProc(libgl, 'glIndexPointer');
  glTexCoordPointer := GetProc(libgl, 'glTexCoordPointer');
  glEdgeFlagPointer := GetProc(libgl, 'glEdgeFlagPointer');
  glGetPointerv := GetProc(libgl, 'glGetPointerv');
  glArrayElement := GetProc(libgl, 'glArrayElement');
  glDrawArrays := GetProc(libgl, 'glDrawArrays');
  glDrawElements := GetProc(libgl, 'glDrawElements');
  glInterleavedArrays := GetProc(libgl, 'glInterleavedArrays');
  glShadeModel := GetProc(libgl, 'glShadeModel');
  glLightf := GetProc(libgl, 'glLightf');
  glLighti := GetProc(libgl, 'glLighti');
  glLightfv := GetProc(libgl, 'glLightfv');
  glLightiv := GetProc(libgl, 'glLightiv');
  glGetLightfv := GetProc(libgl, 'glGetLightfv');
  glGetLightiv := GetProc(libgl, 'glGetLightiv');
  glLightModelf := GetProc(libgl, 'glLightModelf');
  glLightModeli := GetProc(libgl, 'glLightModeli');
  glLightModelfv := GetProc(libgl, 'glLightModelfv');
  glLightModeliv := GetProc(libgl, 'glLightModeliv');
  glMaterialf := GetProc(libgl, 'glMaterialf');
  glMateriali := GetProc(libgl, 'glMateriali');
  glMaterialfv := GetProc(libgl, 'glMaterialfv');
  glMaterialiv := GetProc(libgl, 'glMaterialiv');
  glGetMaterialfv := GetProc(libgl, 'glGetMaterialfv');
  glGetMaterialiv := GetProc(libgl, 'glGetMaterialiv');
  glColorMaterial := GetProc(libgl, 'glColorMaterial');
  glPixelZoom := GetProc(libgl, 'glPixelZoom');
  glPixelStoref := GetProc(libgl, 'glPixelStoref');
  glPixelStorei := GetProc(libgl, 'glPixelStorei');
  glPixelTransferf := GetProc(libgl, 'glPixelTransferf');
  glPixelTransferi := GetProc(libgl, 'glPixelTransferi');
  glPixelMapfv := GetProc(libgl, 'glPixelMapfv');
  glPixelMapuiv := GetProc(libgl, 'glPixelMapuiv');
  glPixelMapusv := GetProc(libgl, 'glPixelMapusv');
  glGetPixelMapfv := GetProc(libgl, 'glGetPixelMapfv');
  glGetPixelMapuiv := GetProc(libgl, 'glGetPixelMapuiv');
  glGetPixelMapusv := GetProc(libgl, 'glGetPixelMapusv');
  glBitmap := GetProc(libgl, 'glBitmap');
  glReadPixels := GetProc(libgl, 'glReadPixels');
  glDrawPixels := GetProc(libgl, 'glDrawPixels');
  glCopyPixels := GetProc(libgl, 'glCopyPixels');
  glStencilFunc := GetProc(libgl, 'glStencilFunc');
  glStencilMask := GetProc(libgl, 'glStencilMask');
  glStencilOp := GetProc(libgl, 'glStencilOp');
  glClearStencil := GetProc(libgl, 'glClearStencil');
  glTexGend := GetProc(libgl, 'glTexGend');
  glTexGenf := GetProc(libgl, 'glTexGenf');
  glTexGeni := GetProc(libgl, 'glTexGeni');
  glTexGendv := GetProc(libgl, 'glTexGendv');
  glTexGenfv := GetProc(libgl, 'glTexGenfv');
  glTexGeniv := GetProc(libgl, 'glTexGeniv');
  glGetTexGendv := GetProc(libgl, 'glGetTexGendv');
  glGetTexGenfv := GetProc(libgl, 'glGetTexGenfv');
  glGetTexGeniv := GetProc(libgl, 'glGetTexGeniv');
  glTexEnvf := GetProc(libgl, 'glTexEnvf');
  glTexEnvi := GetProc(libgl, 'glTexEnvi');
  glTexEnvfv := GetProc(libgl, 'glTexEnvfv');
  glTexEnviv := GetProc(libgl, 'glTexEnviv');
  glGetTexEnvfv := GetProc(libgl, 'glGetTexEnvfv');
  glGetTexEnviv := GetProc(libgl, 'glGetTexEnviv');
  glTexParameterf := GetProc(libgl, 'glTexParameterf');
  glTexParameteri := GetProc(libgl, 'glTexParameteri');
  glTexParameterfv := GetProc(libgl, 'glTexParameterfv');
  glTexParameteriv := GetProc(libgl, 'glTexParameteriv');
  glGetTexParameterfv := GetProc(libgl, 'glGetTexParameterfv');
  glGetTexParameteriv := GetProc(libgl, 'glGetTexParameteriv');
  glGetTexLevelParameterfv := GetProc(libgl, 'glGetTexLevelParameterfv');
  glGetTexLevelParameteriv := GetProc(libgl, 'glGetTexLevelParameteriv');
  glTexImage1D := GetProc(libgl, 'glTexImage1D');
  glTexImage2D := GetProc(libgl, 'glTexImage2D');
  glGetTexImage := GetProc(libgl, 'glGetTexImage');
  glGenTextures := GetProc(libgl, 'glGenTextures');
  glDeleteTextures := GetProc(libgl, 'glDeleteTextures');
  glBindTexture := GetProc(libgl, 'glBindTexture');
  glPrioritizeTextures := GetProc(libgl, 'glPrioritizeTextures');
  glAreTexturesResident := GetProc(libgl, 'glAreTexturesResident');
  glIsTexture := GetProc(libgl, 'glIsTexture');
  glTexSubImage1D := GetProc(libgl, 'glTexSubImage1D');
  glTexSubImage2D := GetProc(libgl, 'glTexSubImage2D');
  glCopyTexImage1D := GetProc(libgl, 'glCopyTexImage1D');
  glCopyTexImage2D := GetProc(libgl, 'glCopyTexImage2D');
  glCopyTexSubImage1D := GetProc(libgl, 'glCopyTexSubImage1D');
  glCopyTexSubImage2D := GetProc(libgl, 'glCopyTexSubImage2D');
  glMap1d := GetProc(libgl, 'glMap1d');
  glMap1f := GetProc(libgl, 'glMap1f');
  glMap2d := GetProc(libgl, 'glMap2d');
  glMap2f := GetProc(libgl, 'glMap2f');
  glGetMapdv := GetProc(libgl, 'glGetMapdv');
  glGetMapfv := GetProc(libgl, 'glGetMapfv');
  glGetMapiv := GetProc(libgl, 'glGetMapiv');
  glEvalCoord1d := GetProc(libgl, 'glEvalCoord1d');
  glEvalCoord1f := GetProc(libgl, 'glEvalCoord1f');
  glEvalCoord1dv := GetProc(libgl, 'glEvalCoord1dv');
  glEvalCoord1fv := GetProc(libgl, 'glEvalCoord1fv');
  glEvalCoord2d := GetProc(libgl, 'glEvalCoord2d');
  glEvalCoord2f := GetProc(libgl, 'glEvalCoord2f');
  glEvalCoord2dv := GetProc(libgl, 'glEvalCoord2dv');
  glEvalCoord2fv := GetProc(libgl, 'glEvalCoord2fv');
  glMapGrid1d := GetProc(libgl, 'glMapGrid1d');
  glMapGrid1f := GetProc(libgl, 'glMapGrid1f');
  glMapGrid2d := GetProc(libgl, 'glMapGrid2d');
  glMapGrid2f := GetProc(libgl, 'glMapGrid2f');
  glEvalPoint1 := GetProc(libgl, 'glEvalPoint1');
  glEvalPoint2 := GetProc(libgl, 'glEvalPoint2');
  glEvalMesh1 := GetProc(libgl, 'glEvalMesh1');
  glEvalMesh2 := GetProc(libgl, 'glEvalMesh2');
  glFogf := GetProc(libgl, 'glFogf');
  glFogi := GetProc(libgl, 'glFogi');
  glFogfv := GetProc(libgl, 'glFogfv');
  glFogiv := GetProc(libgl, 'glFogiv');
  glFeedbackBuffer := GetProc(libgl, 'glFeedbackBuffer');
  glPassThrough := GetProc(libgl, 'glPassThrough');
  glSelectBuffer := GetProc(libgl, 'glSelectBuffer');
  glInitNames := GetProc(libgl, 'glInitNames');
  glLoadName := GetProc(libgl, 'glLoadName');
  glPushName := GetProc(libgl, 'glPushName');
  glPopName := GetProc(libgl, 'glPopName');

  GLInitialized := True;
  Result := True;
end;

function InitGLUFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGLU := LoadLibrary(libname);
  if not Assigned(libGLU) then exit;

  gluLookAt := GetProc(libglu, 'gluLookAt');
  gluOrtho2D := GetProc(libglu, 'gluOrtho2D');
  gluPerspective := GetProc(libglu, 'gluPerspective');
  gluPickMatrix := GetProc(libglu, 'gluPickMatrix');
  gluProject := GetProc(libglu, 'gluProject');
  gluUnProject := GetProc(libglu, 'gluUnProject');
  gluErrorString := GetProc(libglu, 'gluErrorString');
  gluScaleImage := GetProc(libglu, 'gluScaleImage');
  gluBuild1DMipmaps := GetProc(libglu, 'gluBuild1DMipmaps');
  gluBuild2DMipmaps := GetProc(libglu, 'gluBuild2DMipmaps');
  gluNewQuadric := GetProc(libglu, 'gluNewQuadric');
  gluDeleteQuadric := GetProc(libglu, 'gluDeleteQuadric');
  gluQuadricDrawStyle := GetProc(libglu, 'gluQuadricDrawStyle');
  gluQuadricOrientation := GetProc(libglu, 'gluQuadricOrientation');
  gluQuadricNormals := GetProc(libglu, 'gluQuadricNormals');
  gluQuadricTexture := GetProc(libglu, 'gluQuadricTexture');
  gluQuadricCallback := GetProc(libglu, 'gluQuadricCallback');
  gluCylinder := GetProc(libglu, 'gluCylinder');
  gluSphere := GetProc(libglu, 'gluSphere');
  gluDisk := GetProc(libglu, 'gluDisk');
  gluPartialDisk := GetProc(libglu, 'gluPartialDisk');
  gluNewNurbsRenderer := GetProc(libglu, 'gluNewNurbsRenderer');
  gluDeleteNurbsRenderer := GetProc(libglu, 'gluDeleteNurbsRenderer');
  gluLoadSamplingMatrices := GetProc(libglu, 'gluLoadSamplingMatrices');
  gluNurbsProperty := GetProc(libglu, 'gluNurbsProperty');
  gluGetNurbsProperty := GetProc(libglu, 'gluGetNurbsProperty');
  gluBeginCurve := GetProc(libglu, 'gluBeginCurve');
  gluEndCurve := GetProc(libglu, 'gluEndCurve');
  gluNurbsCurve := GetProc(libglu, 'gluNurbsCurve');
  gluBeginSurface := GetProc(libglu, 'gluBeginSurface');
  gluEndSurface := GetProc(libglu, 'gluEndSurface');
  gluNurbsSurface := GetProc(libglu, 'gluNurbsSurface');
  gluBeginTrim := GetProc(libglu, 'gluBeginTrim');
  gluEndTrim := GetProc(libglu, 'gluEndTrim');
  gluPwlCurve := GetProc(libglu, 'gluPwlCurve');
  gluNurbsCallback := GetProc(libglu, 'gluNurbsCallback');
  gluNewTess := GetProc(libglu, 'gluNewTess');
  gluTessCallback := GetProc(libglu, 'gluTessCallback');
  gluDeleteTess := GetProc(libglu, 'gluDeleteTess');
  gluBeginPolygon := GetProc(libglu, 'gluBeginPolygon');
  gluEndPolygon := GetProc(libglu, 'gluEndPolygon');
  gluNextContour := GetProc(libglu, 'gluNextContour');
  gluTessVertex := GetProc(libglu, 'gluTessVertex');
  gluGetString := GetProc(libglu, 'gluGetString');

  GLUInitialized := True;
  Result := True;
end;

function InitGLX: Boolean;
begin
  Result := False;
  if not Assigned(libGL) then exit;

  glXChooseVisual := GetProc(libglx, 'glXChooseVisual');
  glXCreateContext := GetProc(libglx, 'glXCreateContext');
  glXDestroyContext := GetProc(libglx, 'glXDestroyContext');
  glXMakeCurrent := GetProc(libglx, 'glXMakeCurrent');
  glXCopyContext := GetProc(libglx, 'glXCopyContext');
  glXSwapBuffers := GetProc(libglx, 'glXSwapBuffers');
  glXCreateGLXPixmap := GetProc(libglx, 'glXCreateGLXPixmap');
  glXDestroyGLXPixmap := GetProc(libglx, 'glXDestroyGLXPixmap');
  glXQueryExtension := GetProc(libglx, 'glXQueryExtension');
  glXQueryVersion := GetProc(libglx, 'glXQueryVersion');
  glXIsDirect := GetProc(libglx, 'glXIsDirect');
  glXGetConfig := GetProc(libglx, 'glXGetConfig');
  glXGetCurrentContext := GetProc(libglx, 'glXGetCurrentContext');
  glXGetCurrentDrawable := GetProc(libglx, 'glXGetCurrentDrawable');
  glXWaitGL := GetProc(libglx, 'glXWaitGL');
  glXWaitX := GetProc(libglx, 'glXWaitX');
  glXUseXFont := GetProc(libglx, 'glXUseXFont');
  glXQueryExtensionsString := GetProc(libglx, 'glXQueryExtensionsString');
  glXQueryServerString := GetProc(libglx, 'glXQueryServerString');
  glXGetClientString := GetProc(libglx, 'glXGetClientString');
  glXCreateGLXPixmapMESA := GetProc(libglx, 'glXCreateGLXPixmapMESA');
  glXReleaseBufferMESA := GetProc(libglx, 'glXReleaseBufferMESA');
  glXCopySubBufferMESA := GetProc(libglx, 'glXCopySubBufferMESA');
  glXGetVideoSyncSGI := GetProc(libglx, 'glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := GetProc(libglx, 'glXWaitVideoSyncSGI');

  GLXInitialized := True;
  Result := True;
end;

function InitGL: Boolean;
begin
  Result := InitGLFromLibrary('libGL.so') or InitGLFromLibrary('libMesaGL.so');
end;

function InitGLU: Boolean;
begin
  Result := InitGLUFromLibrary('libGLU.so') or InitGLUFromLibrary('libMesaGLU.so');
end;



finalization
  if Assigned(libGL)  then dlclose(libGL);
  if Assigned(libGLU) then dlclose(libGLU);
  if Assigned(libGLX) then dlclose(libGLX);
end.


{
  $Log$
  Revision 1.1  1999-12-23 13:51:50  peter
    * reorganized, it now doesn't depend on fcl anymore by default

}
