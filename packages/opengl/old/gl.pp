{
  $Id$

  Translation of the Mesa GL, GLU and GLX headers for FreePascal
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

{$MODE delphi}	// objfpc would not work because of direct proc var assignments

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

unit GL;

interface

{$IFDEF Linux}
  {$DEFINE gldecl := cdecl}
  {$DEFINE HasGLX}  // Activate GLX stuff
  uses XLib;
{$ELSE}
  {$IFDEF Win32}
    {$DEFINE gldecl := stdcall}
    uses Windows;
  {$ENDIF}
{$ENDIF}


// =======================================================
//   Unit specific extensions
// =======================================================

function InitGLFromLibrary(libname: PChar): Boolean;
function InitGLUFromLibrary(libname: PChar): Boolean;

{$IFDEF HasGLX}
// Requires that the GL library has already been initialized
function InitGLX: Boolean;
{$ENDIF}

// determines automatically which libraries to use:
function InitGL: Boolean;
function InitGLU: Boolean;


var
  GLInitialized, GLUInitialized: Boolean;
  {$IFDEF HasGLX} GLXInitialized: Boolean; {$ENDIF}


// =======================================================
//   GL consts, types and functions
// =======================================================


// -------------------------------------------------------
//   GL types
// -------------------------------------------------------

type

  PSingle = ^Single;
  PDouble = ^Double;

  GLclampf = Single;	// single precision float in [0,1]
  GLclampd = Double;	// double precision float in [0,1]

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


// -------------------------------------------------------
//   GL constants
// -------------------------------------------------------

const
  GL_NO_ERROR				= 0;

  // Boolean values
  GL_FALSE 				= 0;
  GL_TRUE  				= 1;

  // Data types 
  GL_BYTE           			= $1400;
  GL_UNSIGNED_BYTE  			= $1401;
  GL_SHORT          			= $1402;
  GL_UNSIGNED_SHORT 			= $1403;
  GL_INT            			= $1404;
  GL_UNSIGNED_INT  			= $1405;
  GL_FLOAT          			= $1406;
  GL_DOUBLE         			= $140A;
  GL_2_BYTES       			= $1407;
  GL_3_BYTES        			= $1408;
  GL_4_BYTES        			= $1409;

  // Primitives
  GL_LINES          			= $0001;
  GL_POINTS         			= $0000;
  GL_LINE_STRIP     			= $0003;
  GL_LINE_LOOP      			= $0002;
  GL_TRIANGLES      			= $0004;
  GL_TRIANGLE_STRIP 			= $0005;
  GL_TRIANGLE_FAN   			= $0006;
  GL_QUADS          			= $0007;
  GL_QUAD_STRIP     			= $0008;
  GL_POLYGON        			= $0009;
  GL_EDGE_FLAG      			= $0B43;

  // Vertex arrays
  GL_VERTEX_ARRAY			= $8074;
  GL_NORMAL_ARRAY			= $8075;
  GL_COLOR_ARRAY			= $8076;
  GL_INDEX_ARRAY			= $8077;
  GL_TEXTURE_COORD_ARRAY		= $8078;
  GL_EDGE_FLAG_ARRAY			= $8079;
  GL_VERTEX_ARRAY_SIZE			= $807A;
  GL_VERTEX_ARRAY_TYPE			= $807B;
  GL_VERTEX_ARRAY_STRIDE		= $807C;
  GL_NORMAL_ARRAY_TYPE			= $807E;
  GL_NORMAL_ARRAY_STRIDE		= $807F;
  GL_COLOR_ARRAY_SIZE			= $8081;
  GL_COLOR_ARRAY_TYPE			= $8082;
  GL_COLOR_ARRAY_STRIDE			= $8083;
  GL_INDEX_ARRAY_TYPE			= $8085;
  GL_INDEX_ARRAY_STRIDE			= $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE		= $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE		= $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE		= $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE		= $808C;
  GL_VERTEX_ARRAY_POINTER		= $808E;
  GL_NORMAL_ARRAY_POINTER		= $808F;
  GL_COLOR_ARRAY_POINTER		= $8090;
  GL_INDEX_ARRAY_POINTER		= $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER	= $8092;
  GL_EDGE_FLAG_ARRAY_POINTER		= $8093;
  GL_V2F				= $2A20;
  GL_V3F				= $2A21;
  GL_C4UB_V2F				= $2A22;
  GL_C4UB_V3F				= $2A23;
  GL_C3F_V3F				= $2A24;
  GL_N3F_V3F				= $2A25;
  GL_C4F_N3F_V3F			= $2A26;
  GL_T2F_V3F				= $2A27;
  GL_T4F_V4F				= $2A28;
  GL_T2F_C4UB_V3F			= $2A29;
  GL_T2F_C3F_V3F			= $2A2A;
  GL_T2F_N3F_V3F			= $2A2B;
  GL_T2F_C4F_N3F_V3F			= $2A2C;
  GL_T4F_C4F_N3F_V4F			= $2A2D;

  // Matrix Mode
  GL_MATRIX_MODE			= $0BA0;
  GL_MODELVIEW  			= $1700;
  GL_PROJECTION 			= $1701;
  GL_TEXTURE    			= $1702;

  // Points
  GL_POINT_SMOOTH			= $0B10;
  GL_POINT_SIZE				= $0B11;
  GL_POINT_SIZE_GRANULARITY 		= $0B13;
  GL_POINT_SIZE_RANGE			= $0B12;

  // Lines
  GL_LINE_SMOOTH			= $0B20;
  GL_LINE_STIPPLE			= $0B24;
  GL_LINE_STIPPLE_PATTERN		= $0B25;
  GL_LINE_STIPPLE_REPEAT		= $0B26;
  GL_LINE_WIDTH				= $0B21;
  GL_LINE_WIDTH_GRANULARITY		= $0B23;
  GL_LINE_WIDTH_RANGE			= $0B22;

  // Polygons
  GL_POINT                 		= $1B00;
  GL_LINE                  		= $1B01;
  GL_FILL                  		= $1B02;
  GL_CCW                   		= $0901;
  GL_CW                    		= $0900;
  GL_FRONT                 		= $0404;
  GL_BACK                  		= $0405;
  GL_CULL_FACE             		= $0B44;
  GL_CULL_FACE_MODE        		= $0B45;
  GL_POLYGON_SMOOTH        		= $0B41;
  GL_POLYGON_STIPPLE       		= $0B42;
  GL_FRONT_FACE            		= $0B46;
  GL_POLYGON_MODE          		= $0B40;
  GL_POLYGON_OFFSET_FACTOR 		= $8038;
  GL_POLYGON_OFFSET_UNITS  		= $2A00;
  GL_POLYGON_OFFSET_POINT  		= $2A01;
  GL_POLYGON_OFFSET_LINE   		= $2A02;
  GL_POLYGON_OFFSET_FILL   		= $8037;

  // Display lists
  GL_COMPILE				= $1300;
  GL_COMPILE_AND_EXECUTE		= $1301;
  GL_LIST_BASE				= $0B32;
  GL_LIST_INDEX				= $0B33;
  GL_LIST_MODE				= $0B30;

  // Depth buffer
  GL_NEVER             			= $0200;
  GL_LESS              			= $0201;
  GL_GEQUAL            			= $0206;
  GL_LEQUAL            			= $0203;
  GL_GREATER           			= $0204;
  GL_NOTEQUAL          			= $0205;
  GL_EQUAL             			= $0202;
  GL_ALWAYS            			= $0207;
  GL_DEPTH_TEST        			= $0B71;
  GL_DEPTH_BITS        			= $0D56;
  GL_DEPTH_CLEAR_VALUE 			= $0B73;
  GL_DEPTH_FUNC        			= $0B74;
  GL_DEPTH_RANGE       			= $0B70;
  GL_DEPTH_WRITEMASK   			= $0B72;
  GL_DEPTH_COMPONENT   			= $1902;

  // Lighting
  GL_LIGHTING				= $0B50;
  GL_LIGHT0				= $4000;
  GL_LIGHT1				= $4001;
  GL_LIGHT2				= $4002;
  GL_LIGHT3				= $4003;
  GL_LIGHT4				= $4004;
  GL_LIGHT5				= $4005;
  GL_LIGHT6				= $4006;
  GL_LIGHT7				= $4007;
  GL_SPOT_EXPONENT			= $1205;
  GL_SPOT_CUTOFF			= $1206;
  GL_CONSTANT_ATTENUATION		= $1207;
  GL_LINEAR_ATTENUATION			= $1208;
  GL_QUADRATIC_ATTENUATION		= $1209;
  GL_AMBIENT				= $1200;
  GL_DIFFUSE				= $1201;
  GL_SPECULAR				= $1202;
  GL_SHININESS				= $1601;
  GL_EMISSION				= $1600;
  GL_POSITION				= $1203;
  GL_SPOT_DIRECTION			= $1204;
  GL_AMBIENT_AND_DIFFUSE		= $1602;
  GL_COLOR_INDEXES			= $1603;
  GL_LIGHT_MODEL_TWO_SIDE		= $0B52;
  GL_LIGHT_MODEL_LOCAL_VIEWER		= $0B51;
  GL_LIGHT_MODEL_AMBIENT		= $0B53;
  GL_FRONT_AND_BACK			= $0408;
  GL_SHADE_MODEL			= $0B54;
  GL_FLAT				= $1D00;
  GL_SMOOTH				= $1D01;
  GL_COLOR_MATERIAL			= $0B57;
  GL_COLOR_MATERIAL_FACE		= $0B55;
  GL_COLOR_MATERIAL_PARAMETER		= $0B56;
  GL_NORMALIZE				= $0BA1;

  // User clipping planes
  GL_CLIP_PLANE0			= $3000;
  GL_CLIP_PLANE1			= $3001;
  GL_CLIP_PLANE2			= $3002;
  GL_CLIP_PLANE3			= $3003;
  GL_CLIP_PLANE4			= $3004;
  GL_CLIP_PLANE5			= $3005;

  // Accumulation buffer
  GL_ACCUM_RED_BITS			= $0D58;
  GL_ACCUM_GREEN_BITS			= $0D59;
  GL_ACCUM_BLUE_BITS			= $0D5A;
  GL_ACCUM_ALPHA_BITS			= $0D5B;
  GL_ACCUM_CLEAR_VALUE			= $0B80;
  GL_ACCUM				= $0100;
  GL_ADD				= $0104;
  GL_LOAD				= $0101;
  GL_MULT				= $0103;
  GL_RETURN				= $0102;

  // Alpha testing
  GL_ALPHA_TEST				= $0BC0;
  GL_ALPHA_TEST_REF			= $0BC2;
  GL_ALPHA_TEST_FUNC			= $0BC1;

  // Blending
  GL_BLEND				= $0BE2;
  GL_BLEND_SRC				= $0BE1;
  GL_BLEND_DST				= $0BE0;
  GL_ZERO				= 0;
  GL_ONE				= 1;
  GL_SRC_COLOR				= $0300;
  GL_ONE_MINUS_SRC_COLOR		= $0301;
  GL_DST_COLOR				= $0306;
  GL_ONE_MINUS_DST_COLOR		= $0307;
  GL_SRC_ALPHA				= $0302;
  GL_ONE_MINUS_SRC_ALPHA		= $0303;
  GL_DST_ALPHA				= $0304;
  GL_ONE_MINUS_DST_ALPHA		= $0305;
  GL_SRC_ALPHA_SATURATE			= $0308;
  GL_CONSTANT_COLOR			= $8001;
  GL_ONE_MINUS_CONSTANT_COLOR		= $8002;
  GL_CONSTANT_ALPHA			= $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA		= $8004;

  // Render mode
  GL_FEEDBACK				= $1C01;
  GL_RENDER				= $1C00;
  GL_SELECT				= $1C02;

  // Feedback
  GL_2D					= $0600;
  GL_3D					= $0601;
  GL_3D_COLOR				= $0602;
  GL_3D_COLOR_TEXTURE			= $0603;
  GL_4D_COLOR_TEXTURE			= $0604;
  GL_POINT_TOKEN			= $0701;
  GL_LINE_TOKEN				= $0702;
  GL_LINE_RESET_TOKEN			= $0707;
  GL_POLYGON_TOKEN			= $0703;
  GL_BITMAP_TOKEN			= $0704;
  GL_DRAW_PIXEL_TOKEN			= $0705;
  GL_COPY_PIXEL_TOKEN			= $0706;
  GL_PASS_THROUGH_TOKEN			= $0700;
  GL_FEEDBACK_BUFFER_POINTER		= $0DF0;
  GL_FEEDBACK_BUFFER_SIZE		= $0DF1;
  GL_FEEDBACK_BUFFER_TYPE		= $0DF2;

  // Selection
  GL_SELECTION_BUFFER_POINTER		= $0DF3;
  GL_SELECTION_BUFFER_SIZE		= $0DF4;

  // Fog
  GL_FOG				= $0B60;
  GL_FOG_MODE				= $0B65;
  GL_FOG_DENSITY			= $0B62;
  GL_FOG_COLOR				= $0B66;
  GL_FOG_INDEX				= $0B61;
  GL_FOG_START				= $0B63;
  GL_FOG_END				= $0B64;
  GL_LINEAR				= $2601;
  GL_EXP				= $0800;
  GL_EXP2				= $0801;

  // Logic ops
  GL_LOGIC_OP				= $0BF1;
  GL_INDEX_LOGIC_OP			= $0BF1;
  GL_COLOR_LOGIC_OP			= $0BF2;
  GL_LOGIC_OP_MODE			= $0BF0;
  GL_CLEAR				= $1500;
  GL_SET				= $150F;
  GL_COPY				= $1503;
  GL_COPY_INVERTED			= $150C;
  GL_NOOP				= $1505;
  GL_INVERT				= $150A;
  GL_AND				= $1501;
  GL_NAND				= $150E;
  GL_OR					= $1507;
  GL_NOR				= $1508;
  GL_XOR				= $1506;
  GL_EQUIV				= $1509;
  GL_AND_REVERSE			= $1502;
  GL_AND_INVERTED			= $1504;
  GL_OR_REVERSE				= $150B;
  GL_OR_INVERTED			= $150D;

  // Stencil
  GL_STENCIL_TEST			= $0B90;
  GL_STENCIL_WRITEMASK			= $0B98;
  GL_STENCIL_BITS			= $0D57;
  GL_STENCIL_FUNC			= $0B92;
  GL_STENCIL_VALUE_MASK			= $0B93;
  GL_STENCIL_REF			= $0B97;
  GL_STENCIL_FAIL			= $0B94;
  GL_STENCIL_PASS_DEPTH_PASS		= $0B96;
  GL_STENCIL_PASS_DEPTH_FAIL		= $0B95;
  GL_STENCIL_CLEAR_VALUE		= $0B91;
  GL_STENCIL_INDEX			= $1901;
  GL_KEEP				= $1E00;
  GL_REPLACE				= $1E01;
  GL_INCR				= $1E02;
  GL_DECR				= $1E03;

  // Buffers, Pixel Drawing/Reading
  GL_NONE				= 0;
  GL_LEFT				= $0406;
  GL_RIGHT				= $0407;
  //GL_FRONT				= $0404;
  //GL_BACK				= $0405;
  //GL_FRONT_AND_BACK			= $0408;
  GL_FRONT_LEFT				= $0400;
  GL_FRONT_RIGHT			= $0401;
  GL_BACK_LEFT				= $0402;
  GL_BACK_RIGHT				= $0403;
  GL_AUX0				= $0409;
  GL_AUX1				= $040A;
  GL_AUX2				= $040B;
  GL_AUX3				= $040C;
  GL_COLOR_INDEX			= $1900;
  GL_RED				= $1903;
  GL_GREEN				= $1904;
  GL_BLUE				= $1905;
  GL_ALPHA				= $1906;
  GL_LUMINANCE				= $1909;
  GL_LUMINANCE_ALPHA			= $190A;
  GL_ALPHA_BITS				= $0D55;
  GL_RED_BITS				= $0D52;
  GL_GREEN_BITS				= $0D53;
  GL_BLUE_BITS				= $0D54;
  GL_INDEX_BITS				= $0D51;
  GL_SUBPIXEL_BITS			= $0D50;
  GL_AUX_BUFFERS			= $0C00;
  GL_READ_BUFFER			= $0C02;
  GL_DRAW_BUFFER			= $0C01;
  GL_DOUBLEBUFFER			= $0C32;
  GL_STEREO				= $0C33;
  GL_BITMAP				= $1A00;
  GL_COLOR				= $1800;
  GL_DEPTH				= $1801;
  GL_STENCIL				= $1802;
  GL_DITHER				= $0BD0;
  GL_RGB				= $1907;
  GL_RGBA				= $1908;

  // Implementation limits
  GL_MAX_LIST_NESTING			= $0B31;
  GL_MAX_ATTRIB_STACK_DEPTH		= $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH		= $0D36;
  GL_MAX_NAME_STACK_DEPTH		= $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH		= $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH		= $0D39;
  GL_MAX_EVAL_ORDER			= $0D30;
  GL_MAX_LIGHTS				= $0D31;
  GL_MAX_CLIP_PLANES			= $0D32;
  GL_MAX_TEXTURE_SIZE			= $0D33;
  GL_MAX_PIXEL_MAP_TABLE		= $0D34;
  GL_MAX_VIEWPORT_DIMS			= $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH	= $0D3B;

  // Gets
  GL_ATTRIB_STACK_DEPTH			= $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH		= $0BB1;
  GL_COLOR_CLEAR_VALUE			= $0C22;
  GL_COLOR_WRITEMASK			= $0C23;
  GL_CURRENT_INDEX			= $0B01;
  GL_CURRENT_COLOR			= $0B00;
  GL_CURRENT_NORMAL			= $0B02;
  GL_CURRENT_RASTER_COLOR		= $0B04;
  GL_CURRENT_RASTER_DISTANCE		= $0B09;
  GL_CURRENT_RASTER_INDEX		= $0B05;
  GL_CURRENT_RASTER_POSITION		= $0B07;
  GL_CURRENT_RASTER_TEXTURE_COORDS 	= $0B06;
  GL_CURRENT_RASTER_POSITION_VALID 	= $0B08;
  GL_CURRENT_TEXTURE_COORDS		= $0B03;
  GL_INDEX_CLEAR_VALUE			= $0C20;
  GL_INDEX_MODE				= $0C30;
  GL_INDEX_WRITEMASK			= $0C21;
  GL_MODELVIEW_MATRIX			= $0BA6;
  GL_MODELVIEW_STACK_DEPTH		= $0BA3;
  GL_NAME_STACK_DEPTH			= $0D70;
  GL_PROJECTION_MATRIX			= $0BA7;
  GL_PROJECTION_STACK_DEPTH		= $0BA4;
  GL_RENDER_MODE			= $0C40;
  GL_RGBA_MODE				= $0C31;
  GL_TEXTURE_MATRIX			= $0BA8;
  GL_TEXTURE_STACK_DEPTH		= $0BA5;
  GL_VIEWPORT				= $0BA2;

  // Evaluators
  GL_AUTO_NORMAL			= $0D80;
  GL_MAP1_COLOR_4			= $0D90;
  GL_MAP1_GRID_DOMAIN			= $0DD0;
  GL_MAP1_GRID_SEGMENTS			= $0DD1;
  GL_MAP1_INDEX				= $0D91;
  GL_MAP1_NORMAL			= $0D92;
  GL_MAP1_TEXTURE_COORD_1		= $0D93;
  GL_MAP1_TEXTURE_COORD_2		= $0D94;
  GL_MAP1_TEXTURE_COORD_3		= $0D95;
  GL_MAP1_TEXTURE_COORD_4		= $0D96;
  GL_MAP1_VERTEX_3			= $0D97;
  GL_MAP1_VERTEX_4			= $0D98;
  GL_MAP2_COLOR_4			= $0DB0;
  GL_MAP2_GRID_DOMAIN			= $0DD2;
  GL_MAP2_GRID_SEGMENTS			= $0DD3;
  GL_MAP2_INDEX				= $0DB1;
  GL_MAP2_NORMAL			= $0DB2;
  GL_MAP2_TEXTURE_COORD_1		= $0DB3;
  GL_MAP2_TEXTURE_COORD_2		= $0DB4;
  GL_MAP2_TEXTURE_COORD_3		= $0DB5;
  GL_MAP2_TEXTURE_COORD_4		= $0DB6;
  GL_MAP2_VERTEX_3			= $0DB7;
  GL_MAP2_VERTEX_4			= $0DB8;
  GL_COEFF				= $0A00;
  GL_DOMAIN				= $0A02;
  GL_ORDER				= $0A01;

  // Hints
  GL_FOG_HINT				= $0C54;
  GL_LINE_SMOOTH_HINT			= $0C52;
  GL_PERSPECTIVE_CORRECTION_HINT	= $0C50;
  GL_POINT_SMOOTH_HINT			= $0C51;
  GL_POLYGON_SMOOTH_HINT		= $0C53;
  GL_DONT_CARE				= $1100;
  GL_FASTEST				= $1101;
  GL_NICEST				= $1102;

  // Scissor box
  GL_SCISSOR_TEST			= $0C11;
  GL_SCISSOR_BOX			= $0C10;

  // Pixel Mode / Transfer
  GL_MAP_COLOR				= $0D10;
  GL_MAP_STENCIL			= $0D11;
  GL_INDEX_SHIFT			= $0D12;
  GL_INDEX_OFFSET			= $0D13;
  GL_RED_SCALE				= $0D14;
  GL_RED_BIAS				= $0D15;
  GL_GREEN_SCALE			= $0D18;
  GL_GREEN_BIAS				= $0D19;
  GL_BLUE_SCALE				= $0D1A;
  GL_BLUE_BIAS				= $0D1B;
  GL_ALPHA_SCALE			= $0D1C;
  GL_ALPHA_BIAS				= $0D1D;
  GL_DEPTH_SCALE			= $0D1E;
  GL_DEPTH_BIAS				= $0D1F;
  GL_PIXEL_MAP_S_TO_S_SIZE		= $0CB1;
  GL_PIXEL_MAP_I_TO_I_SIZE		= $0CB0;
  GL_PIXEL_MAP_I_TO_R_SIZE		= $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE		= $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE		= $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE		= $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE		= $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE		= $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE		= $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE		= $0CB9;
  GL_PIXEL_MAP_S_TO_S			= $0C71;
  GL_PIXEL_MAP_I_TO_I			= $0C70;
  GL_PIXEL_MAP_I_TO_R			= $0C72;
  GL_PIXEL_MAP_I_TO_G			= $0C73;
  GL_PIXEL_MAP_I_TO_B			= $0C74;
  GL_PIXEL_MAP_I_TO_A			= $0C75;
  GL_PIXEL_MAP_R_TO_R			= $0C76;
  GL_PIXEL_MAP_G_TO_G			= $0C77;
  GL_PIXEL_MAP_B_TO_B			= $0C78;
  GL_PIXEL_MAP_A_TO_A			= $0C79;
  GL_PACK_ALIGNMENT			= $0D05;
  GL_PACK_LSB_FIRST			= $0D01;
  GL_PACK_ROW_LENGTH			= $0D02;
  GL_PACK_SKIP_PIXELS			= $0D04;
  GL_PACK_SKIP_ROWS			= $0D03;
  GL_PACK_SWAP_BYTES			= $0D00;
  GL_UNPACK_ALIGNMENT			= $0CF5;
  GL_UNPACK_LSB_FIRST			= $0CF1;
  GL_UNPACK_ROW_LENGTH			= $0CF2;
  GL_UNPACK_SKIP_PIXELS			= $0CF4;
  GL_UNPACK_SKIP_ROWS			= $0CF3;
  GL_UNPACK_SWAP_BYTES			= $0CF0;
  GL_ZOOM_X				= $0D16;
  GL_ZOOM_Y				= $0D17;

  // Texture mapping
  GL_TEXTURE_ENV                  	= $2300;
  GL_TEXTURE_ENV_MODE             	= $2200;
  GL_TEXTURE_1D                   	= $0DE0;
  GL_TEXTURE_2D                   	= $0DE1;
  GL_TEXTURE_WRAP_S			= $2802;
  GL_TEXTURE_WRAP_T			= $2803;
  GL_TEXTURE_MAG_FILTER			= $2800;
  GL_TEXTURE_MIN_FILTER			= $2801;
  GL_TEXTURE_ENV_COLOR			= $2201;
  GL_TEXTURE_GEN_S			= $0C60;
  GL_TEXTURE_GEN_T			= $0C61;
  GL_TEXTURE_GEN_MODE			= $2500;
  GL_TEXTURE_BORDER_COLOR		= $1004;
  GL_TEXTURE_WIDTH			= $1000;
  GL_TEXTURE_HEIGHT			= $1001;
  GL_TEXTURE_BORDER			= $1005;
  GL_TEXTURE_COMPONENTS			= $1003;
  GL_TEXTURE_RED_SIZE			= $805C;
  GL_TEXTURE_GREEN_SIZE			= $805D;
  GL_TEXTURE_BLUE_SIZE			= $805E;
  GL_TEXTURE_ALPHA_SIZE			= $805F;
  GL_TEXTURE_LUMINANCE_SIZE		= $8060;
  GL_TEXTURE_INTENSITY_SIZE		= $8061;
  GL_NEAREST_MIPMAP_NEAREST		= $2700;
  GL_NEAREST_MIPMAP_LINEAR		= $2702;
  GL_LINEAR_MIPMAP_NEAREST		= $2701;
  GL_LINEAR_MIPMAP_LINEAR		= $2703;
  GL_OBJECT_LINEAR			= $2401;
  GL_OBJECT_PLANE			= $2501;
  GL_EYE_LINEAR				= $2400;
  GL_EYE_PLANE				= $2502;
  GL_SPHERE_MAP				= $2402;
  GL_DECAL				= $2101;
  GL_MODULATE				= $2100;
  GL_NEAREST				= $2600;
  GL_REPEAT				= $2901;
  GL_CLAMP				= $2900;
  GL_S					= $2000;
  GL_T					= $2001;
  GL_R					= $2002;
  GL_Q					= $2003;
  GL_TEXTURE_GEN_R			= $0C62;
  GL_TEXTURE_GEN_Q			= $0C63;

  // GL 1.1 texturing
  GL_PROXY_TEXTURE_1D			= $8063;
  GL_PROXY_TEXTURE_2D			= $8064;
  GL_TEXTURE_PRIORITY			= $8066;
  GL_TEXTURE_RESIDENT			= $8067;
  GL_TEXTURE_BINDING_1D			= $8068;
  GL_TEXTURE_BINDING_2D			= $8069;
  GL_TEXTURE_INTERNAL_FORMAT		= $1003;


  // GL 1.2 texturing
  GL_PACK_SKIP_IMAGES			= $806B;
  GL_PACK_IMAGE_HEIGHT			= $806C;
  GL_UNPACK_SKIP_IMAGES			= $806D;
  GL_UNPACK_IMAGE_HEIGHT		= $806E;
  GL_TEXTURE_3D				= $806F;
  GL_PROXY_TEXTURE_3D			= $8070;
  GL_TEXTURE_DEPTH			= $8071;
  GL_TEXTURE_WRAP_R			= $8072;
  GL_MAX_3D_TEXTURE_SIZE		= $8073;
  GL_TEXTURE_BINDING_3D			= $806A;

  // Internal texture formats (GL 1.1)
  GL_ALPHA4				= $803B;
  GL_ALPHA8				= $803C;
  GL_ALPHA12				= $803D;
  GL_ALPHA16				= $803E;
  GL_LUMINANCE4				= $803F;
  GL_LUMINANCE8				= $8040;
  GL_LUMINANCE12			= $8041;
  GL_LUMINANCE16			= $8042;
  GL_LUMINANCE4_ALPHA4			= $8043;
  GL_LUMINANCE6_ALPHA2			= $8044;
  GL_LUMINANCE8_ALPHA8			= $8045;
  GL_LUMINANCE12_ALPHA4			= $8046;
  GL_LUMINANCE12_ALPHA12		= $8047;
  GL_LUMINANCE16_ALPHA16		= $8048;
  GL_INTENSITY				= $8049;
  GL_INTENSITY4				= $804A;
  GL_INTENSITY8				= $804B;
  GL_INTENSITY12			= $804C;
  GL_INTENSITY16			= $804D;
  GL_R3_G3_B2				= $2A10;
  GL_RGB4				= $804F;
  GL_RGB5				= $8050;
  GL_RGB8				= $8051;
  GL_RGB10				= $8052;
  GL_RGB12				= $8053;
  GL_RGB16				= $8054;
  GL_RGBA2				= $8055;
  GL_RGBA4				= $8056;
  GL_RGB5_A1				= $8057;
  GL_RGBA8				= $8058;
  GL_RGB10_A2				= $8059;
  GL_RGBA12				= $805A;
  GL_RGBA16				= $805B;

  // Utility
  GL_VENDOR				= $1F00;
  GL_RENDERER				= $1F01;
  GL_VERSION				= $1F02;
  GL_EXTENSIONS				= $1F03;

  // Errors
  GL_INVALID_VALUE			= $0501;
  GL_INVALID_ENUM			= $0500;
  GL_INVALID_OPERATION			= $0502;
  GL_STACK_OVERFLOW			= $0503;
  GL_STACK_UNDERFLOW			= $0504;
  GL_OUT_OF_MEMORY			= $0505;


// -------------------------------------------------------
//   GL extensions constants
// -------------------------------------------------------

  // GL_EXT_blend_minmax and GL_EXT_blend_color
  GL_CONSTANT_COLOR_EXT			= $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT	= $8002;
  GL_CONSTANT_ALPHA_EXT			= $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT	= $8004;
  GL_BLEND_EQUATION_EXT			= $8009;
  GL_MIN_EXT				= $8007;
  GL_MAX_EXT				= $8008;
  GL_FUNC_ADD_EXT			= $8006;
  GL_FUNC_SUBTRACT_EXT			= $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT		= $800B;
  GL_BLEND_COLOR_EXT			= $8005;

  // GL_EXT_polygon_offset
  GL_POLYGON_OFFSET_EXT			= $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT		= $8038;
  GL_POLYGON_OFFSET_BIAS_EXT		= $8039;

  // GL_EXT_vertex_array
  GL_VERTEX_ARRAY_EXT			= $8074;
  GL_NORMAL_ARRAY_EXT			= $8075;
  GL_COLOR_ARRAY_EXT			= $8076;
  GL_INDEX_ARRAY_EXT			= $8077;
  GL_TEXTURE_COORD_ARRAY_EXT		= $8078;
  GL_EDGE_FLAG_ARRAY_EXT		= $8079;
  GL_VERTEX_ARRAY_SIZE_EXT		= $807A;
  GL_VERTEX_ARRAY_TYPE_EXT		= $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT		= $807C;
  GL_VERTEX_ARRAY_COUNT_EXT		= $807D;
  GL_NORMAL_ARRAY_TYPE_EXT		= $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT		= $807F;
  GL_NORMAL_ARRAY_COUNT_EXT		= $8080;
  GL_COLOR_ARRAY_SIZE_EXT		= $8081;
  GL_COLOR_ARRAY_TYPE_EXT		= $8082;
  GL_COLOR_ARRAY_STRIDE_EXT		= $8083;
  GL_COLOR_ARRAY_COUNT_EXT		= $8084;
  GL_INDEX_ARRAY_TYPE_EXT		= $8085;
  GL_INDEX_ARRAY_STRIDE_EXT		= $8086;
  GL_INDEX_ARRAY_COUNT_EXT		= $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT	= $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT	= $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT	= $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT	= $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT		= $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT		= $808D;
  GL_VERTEX_ARRAY_POINTER_EXT		= $808E;
  GL_NORMAL_ARRAY_POINTER_EXT		= $808F;
  GL_COLOR_ARRAY_POINTER_EXT		= $8090;
  GL_INDEX_ARRAY_POINTER_EXT		= $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT	= $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT	= $8093;

  // GL_EXT_texture_object
  GL_TEXTURE_PRIORITY_EXT		= $8066;
  GL_TEXTURE_RESIDENT_EXT		= $8067;
  GL_TEXTURE_1D_BINDING_EXT		= $8068;
  GL_TEXTURE_2D_BINDING_EXT		= $8069;

  // GL_EXT_texture3D
  GL_PACK_SKIP_IMAGES_EXT		= $806B;
  GL_PACK_IMAGE_HEIGHT_EXT		= $806C;
  GL_UNPACK_SKIP_IMAGES_EXT		= $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT		= $806E;
  GL_TEXTURE_3D_EXT			= $806F;
  GL_PROXY_TEXTURE_3D_EXT		= $8070;
  GL_TEXTURE_DEPTH_EXT			= $8071;
  GL_TEXTURE_WRAP_R_EXT			= $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT		= $8073;
  GL_TEXTURE_3D_BINDING_EXT		= $806A;

  // GL_EXT_paletted_texture
  GL_TABLE_TOO_LARGE_EXT		= $8031;
  GL_COLOR_TABLE_FORMAT_EXT		= $80D8;
  GL_COLOR_TABLE_WIDTH_EXT		= $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT		= $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT		= $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT		= $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT	 	= $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT	= $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT	= $80DF;
  GL_TEXTURE_INDEX_SIZE_EXT		= $80ED;
  GL_COLOR_INDEX1_EXT			= $80E2;
  GL_COLOR_INDEX2_EXT			= $80E3;
  GL_COLOR_INDEX4_EXT			= $80E4;
  GL_COLOR_INDEX8_EXT			= $80E5;
  GL_COLOR_INDEX12_EXT			= $80E6;
  GL_COLOR_INDEX16_EXT			= $80E7;

  // GL_EXT_shared_texture_palette
  GL_SHARED_TEXTURE_PALETTE_EXT		= $81FB;

  // GL_EXT_point_parameters
  GL_POINT_SIZE_MIN_EXT			= $8126;
  GL_POINT_SIZE_MAX_EXT			= $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT	= $8128;
  GL_DISTANCE_ATTENUATION_EXT		= $8129;

  // GL_EXT_rescale_normal
  GL_RESCALE_NORMAL_EXT			= $803A;

  // GL_EXT_abgr
  GL_ABGR_EXT				= $8000;

  // GL_SGIS_multitexture
  GL_SELECTED_TEXTURE_SGIS		= $835C;
  GL_SELECTED_TEXTURE_COORD_SET_SGIS	= $835D;
  GL_MAX_TEXTURES_SGIS			= $835E;
  GL_TEXTURE0_SGIS			= $835F;
  GL_TEXTURE1_SGIS			= $8360;
  GL_TEXTURE2_SGIS			= $8361;
  GL_TEXTURE3_SGIS			= $8362;
  GL_TEXTURE_COORD_SET_SOURCE_SGIS	= $8363;

  // GL_EXT_multitexture
  GL_SELECTED_TEXTURE_EXT		= $83C0;
  GL_SELECTED_TEXTURE_COORD_SET_EXT	= $83C1;
  GL_SELECTED_TEXTURE_TRANSFORM_EXT	= $83C2;
  GL_MAX_TEXTURES_EXT			= $83C3;
  GL_MAX_TEXTURE_COORD_SETS_EXT		= $83C4;
  GL_TEXTURE_ENV_COORD_SET_EXT		= $83C5;
  GL_TEXTURE0_EXT			= $83C6;
  GL_TEXTURE1_EXT			= $83C7;
  GL_TEXTURE2_EXT			= $83C8;
  GL_TEXTURE3_EXT			= $83C9;

  // GL_SGIS_texture_edge_clamp
  GL_CLAMP_TO_EDGE_SGIS			= $812F;

  // OpenGL 1.2
  GL_RESCALE_NORMAL			= $803A;
  GL_CLAMP_TO_EDGE			= $812F;
  GL_MAX_ELEMENTS_VERTICES		= $F0E8;
  GL_MAX_ELEMENTS_INDICES		= $F0E9;
  GL_BGR				= $80E0;
  GL_BGRA				= $80E1;
  GL_UNSIGNED_BYTE_3_3_2		= $8032;
  GL_UNSIGNED_BYTE_2_3_3_REV		= $8362;
  GL_UNSIGNED_SHORT_5_6_5		= $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV		= $8364;
  GL_UNSIGNED_SHORT_4_4_4_4		= $8033;
  GL_UNSIGNED_SHORT_4_4_4_4_REV		= $8365;
  GL_UNSIGNED_SHORT_5_5_5_1		= $8034;
  GL_UNSIGNED_SHORT_1_5_5_5_REV		= $8366;
  GL_UNSIGNED_INT_8_8_8_8		= $8035;
  GL_UNSIGNED_INT_8_8_8_8_REV		= $8367;
  GL_UNSIGNED_INT_10_10_10_2		= $8036;
  GL_UNSIGNED_INT_2_10_10_10_REV	= $8368;
  GL_LIGHT_MODEL_COLOR_CONTROL		= $81F8;
  GL_SINGLE_COLOR			= $81F9;
  GL_SEPARATE_SPECULAR_COLOR		= $81FA;
  GL_TEXTURE_MIN_LOD			= $813A;
  GL_TEXTURE_MAX_LOD			= $813B;
  GL_TEXTURE_BASE_LEVEL			= $813C;
  GL_TEXTURE_MAX_LEVEL			= $813D;


// -------------------------------------------------------
//   GL procedures and functions
// -------------------------------------------------------

var
  // Miscellaneous
  glClearIndex: procedure(c: Single); gldecl;
  glClearColor: procedure(red, green, blue, alpha: GLclampf); gldecl;
  glClear: procedure(mask: GLbitfield); gldecl;
  glIndexMask: procedure(mask: LongWord); gldecl;
  glColorMask: procedure(red, green, blue, alpha: Boolean); gldecl;
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); gldecl;
  glBlendFunc: procedure(sfactor, dfactor: GLenum); gldecl;
  glLogicOp: procedure(opcode: GLenum); gldecl;
  glCullFace: procedure(mode: GLenum); gldecl;
  glFrontFace: procedure(mode: GLenum); gldecl;
  glPointSize: procedure(size: Single); gldecl;
  glLineWidth: procedure(width: Single); gldecl;
  glLineStipple: procedure(factor: LongInt; pattern: Word); gldecl;
  glPolygonMode: procedure(face, mode: GLenum); gldecl;
  glPolygonOffset: procedure(factor, units: Single); gldecl;
  glPolygonStipple: procedure(var mask: Byte); gldecl;
  glGetPolygonStipple: procedure(var mask: Byte); gldecl;
  glEdgeFlag: procedure(flag: Boolean); gldecl;
  glEdgeFlagv: procedure(var flag: Boolean); gldecl;
  glScissor: procedure(x, y, width, height: LongInt); gldecl;
  glClipPlane: procedure(plane: GLenum; var equation: Double); gldecl;
  glGetClipPlane: procedure(plane: GLenum; var equation: Double); gldecl;
  glDrawBuffer: procedure(mode: GLenum); gldecl;
  glReadBuffer: procedure(mode: GLenum); gldecl;
  glEnable: procedure(cap: LongInt); gldecl;
  glDisable: procedure(cap: LongInt); gldecl;
  glIsEnabled: function(cap: GLenum): Boolean; gldecl;
  glEnableClientState: procedure(cap: GLenum); gldecl;  // 1.1
  glDisableClientState: procedure(cap: GLenum); gldecl;  // 1.1
  glGetBooleanv: procedure(pname: GLenum; var params: Boolean); gldecl;
  glGetDoublev: procedure(pname: GLenum; var params: Double); gldecl;
  glGetFloatv: procedure(pname: GLenum; var params: Single); gldecl;
  glGetIntegerv: procedure(pname: GLenum; var params: LongInt); gldecl;
  glPushAttrib: procedure(mask: GLbitfield); gldecl;
  glPopAttrib: procedure; gldecl;
  glPushClientAttrib: procedure(mask: GLbitfield); gldecl;  // 1.1
  glPopClientAttrib: procedure; gldecl;  // 1.1
  glRenderMode: function(mode: GLenum): LongInt; gldecl;
  glGetError: function: GLenum; gldecl;
  glGetString: function(name: GLenum): PChar; gldecl;
  glFinish: procedure; gldecl;
  glFlush: procedure; gldecl;
  glHint: procedure(target, mode: GLenum); gldecl;


  // Depth Buffer
  glClearDepth: procedure(depth: GLclampd); gldecl;
  glDepthFunc: procedure(func: LongInt); gldecl;
  glDepthMask: procedure(flag: Boolean); gldecl;
  glDepthRange: procedure(near_val, far_val: GLclampd); gldecl;

  // Accumulation Buffer
  glClearAccum: procedure(red, green, blue, alpha: Single); gldecl;
  glAccum: procedure(op: GLenum; value: Single); gldecl;

  // Tranformation
  glMatrixMode: procedure(mode: GLenum); gldecl;
  glOrtho: procedure(left, right, bottom, top, near_val, far_val: Double); gldecl;
  glFrustum: procedure(left, right, bottom, top, near_val, far_val: Double); gldecl;
  glViewport: procedure(x, y, width, height: LongInt); gldecl;
  glPushMatrix: procedure; gldecl;
  glPopMatrix: procedure; gldecl;
  glLoadIdentity: procedure; gldecl;
  glLoadMatrixd: procedure(var m: Double); gldecl;
  glLoadMatrixf: procedure(var m: PSingle); gldecl;
  glMultMatrixd: procedure(var m: Double); gldecl;
  glMultMatrixf: procedure(var m: Single); gldecl;
  glRotated: procedure(angle, x, y, z: Double); gldecl;
  glRotatef: procedure(angle, x, y, z: Single); gldecl;
  glScaled: procedure(x, y, z: Double); gldecl;
  glScalef: procedure(x, y, z: Single); gldecl;
  glTranslated: procedure(x, y, z: Double); gldecl;
  glTranslatef: procedure(x, y, z: Single); gldecl;

  // Display Lists
  glIsList: function(list: LongWord): Boolean; gldecl;
  glDeleteLists: procedure(list: LongWord; range: LongInt); gldecl;
  glGenLists: function(range: LongInt): LongWord; gldecl;
  glNewList: procedure(list: LongWord; mode: GLenum); gldecl;
  glEndList: procedure; gldecl;
  glCallList: procedure(list: LongWord); gldecl;
  glCallLists: procedure(n: LongInt; AType: GLenum; var lists); gldecl;
  glListBase: procedure(base: LongWord); gldecl;

  // Drawing Functions
  glBegin: procedure(mode: GLenum); gldecl;
  glEnd: procedure; gldecl;
  glVertex2d: procedure(x, y: Double); gldecl;
  glVertex2f: procedure(x, y: Single); gldecl;
  glVertex2i: procedure(x, y: LongInt); gldecl;
  glVertex2s: procedure(x, y: SmallInt); gldecl;
  glVertex3d: procedure(x, y, z: Double); gldecl;
  glVertex3f: procedure(x, y, z: Single); gldecl;
  glVertex3i: procedure(x, y, z: LongInt); gldecl;
  glVertex3s: procedure(x, y, z: SmallInt); gldecl;
  glVertex4d: procedure(x, y, z, w: Double); gldecl;
  glVertex4f: procedure(x, y, z, w: Single); gldecl;
  glVertex4i: procedure(x, y, z, w: LongInt); gldecl;
  glVertex4s: procedure(x, y, z, w: SmallInt); gldecl;
  glVertex2dv: procedure(var v: Double); gldecl;
  glVertex2fv: procedure(var v: Single); gldecl;
  glVertex2iv: procedure(var v: LongInt); gldecl;
  glVertex2sv: procedure(var v: SmallInt); gldecl;
  glVertex3dv: procedure(var v: Double); gldecl;
  glVertex3fv: procedure(var v: Single); gldecl;
  glVertex3iv: procedure(var v: LongInt); gldecl;
  glVertex3sv: procedure(var v: SmallInt); gldecl;
  glVertex4dv: procedure(var v: Double); gldecl;
  glVertex4fv: procedure(var v: Single); gldecl;
  glVertex4iv: procedure(var v: LongInt); gldecl;
  glVertex4sv: procedure(var v: SmallInt); gldecl;
  glNormal3b: procedure(nx, ny, nz: Byte); gldecl;
  glNormal3d: procedure(nx, ny, nz: Double); gldecl;
  glNormal3f: procedure(nx, ny, nz: Single); gldecl;
  glNormal3i: procedure(nx, ny, nz: LongInt); gldecl;
  glNormal3s: procedure(nx, ny, nz: SmallInt); gldecl;
  glNormal3bv: procedure(var v: ShortInt); gldecl;
  glNormal3dv: procedure(var v: Double); gldecl;
  glNormal3fv: procedure(var v: Single); gldecl;
  glNormal3iv: procedure(var v: LongInt); gldecl;
  glNormal3sv: procedure(var v: SmallInt); gldecl;
  glIndexd: procedure(c: Double); gldecl;
  glIndexf: procedure(c: Single); gldecl;
  glIndexi: procedure(c: LongInt); gldecl;
  glIndexs: procedure(c: SmallInt); gldecl;
  glIndexub: procedure(c: Byte); gldecl;  // 1.1
  glIndexdv: procedure(var c: Double); gldecl;
  glIndexfv: procedure(var c: Single); gldecl;
  glIndexiv: procedure(var c: LongInt); gldecl;
  glIndexsv: procedure(var c: SmallInt); gldecl;
  glIndexubv: procedure(var c: Byte); gldecl;  // 1.1
  glColor3b : procedure(red, green, blue: ShortInt); gldecl;
  glColor3d : procedure(red, green, blue: Double); gldecl;
  glColor3f : procedure(red, green, blue: Single); gldecl;
  glColor3i : procedure(red, green, blue: LongInt); gldecl;
  glColor3s : procedure(red, green, blue: SmallInt); gldecl;
  glColor3ub: procedure(red, green, blue: Byte); gldecl;
  glColor3ui: procedure(red, green, blue: LongWord); gldecl;
  glColor3us: procedure(red, green, blue: Word); gldecl;
  glColor4b : procedure(red, green, blue, alpha: ShortInt); gldecl;
  glColor4d : procedure(red, green, blue, alpha: Double); gldecl;
  glColor4f : procedure(red, green, blue, alpha: Single); gldecl;
  glColor4i : procedure(red, green, blue, alpha: LongInt); gldecl;
  glColor4s : procedure(red, green, blue, alpha: SmallInt); gldecl;
  glColor4ub: procedure(red, green, blue, alpha: Byte); gldecl;
  glColor4ui: procedure(red, green, blue, alpha: LongWord); gldecl;
  glColor4us: procedure(red, green, blue, alpha: Word); gldecl;
  glColor3bv : procedure(var v: ShortInt); gldecl;
  glColor3dv : procedure(var v: Double); gldecl;
  glColor3fv : procedure(var v: Single); gldecl;
  glColor3iv : procedure(var v: LongInt); gldecl;
  glColor3sv : procedure(var v: SmallInt); gldecl;
  glColor3ubv: procedure(var v: Byte); gldecl;
  glColor3uiv: procedure(var v: LongWord); gldecl;
  glColor3usv: procedure(var v: Word); gldecl;
  glColor4bv : procedure(var v: ShortInt); gldecl;
  glColor4dv : procedure(var v: Double); gldecl;
  glColor4fv : procedure(var v: Single); gldecl;
  glColor4iv : procedure(var v: LongInt); gldecl;
  glColor4sv : procedure(var v: SmallInt); gldecl;
  glColor4ubv: procedure(var v: Byte); gldecl;
  glColor4uiv: procedure(var v: LongWord); gldecl;
  glColor4usv: procedure(var v: Word); gldecl;
  glTexCoord1d: procedure(s: Double); gldecl;
  glTexCoord1f: procedure(s: Single); gldecl;
  glTexCoord1i: procedure(s: LongInt); gldecl;
  glTexCoord1s: procedure(s: SmallInt); gldecl;
  glTexCoord2d: procedure(s, t: Double); gldecl;
  glTexCoord2f: procedure(s, t: Single); gldecl;
  glTexCoord2i: procedure(s, t: LongInt); gldecl;
  glTexCoord2s: procedure(s, t: SmallInt); gldecl;
  glTexCoord3d: procedure(s, t, r: Double); gldecl;
  glTexCoord3f: procedure(s, t, r: Single); gldecl;
  glTexCoord3i: procedure(s, t, r: LongInt); gldecl;
  glTexCoord3s: procedure(s, t, r: SmallInt); gldecl;
  glTexCoord4d: procedure(s, t, r, q: Double); gldecl;
  glTexCoord4f: procedure(s, t, r, q: Single); gldecl;
  glTexCoord4i: procedure(s, t, r, q: LongInt); gldecl;
  glTexCoord4s: procedure(s, t, r, q: SmallInt); gldecl;
  glTexCoord1dv: procedure(var v: Double); gldecl;
  glTexCoord1fv: procedure(var v: Single); gldecl;
  glTexCoord1iv: procedure(var v: LongInt); gldecl;
  glTexCoord1sv: procedure(var v: SmallInt); gldecl;
  glTexCoord2dv: procedure(var v: Double); gldecl;
  glTexCoord2fv: procedure(var v: Single); gldecl;
  glTexCoord2iv: procedure(var v: LongInt); gldecl;
  glTexCoord2sv: procedure(var v: SmallInt); gldecl;
  glTexCoord3dv: procedure(var v: Double); gldecl;
  glTexCoord3fv: procedure(var v: Single); gldecl;
  glTexCoord3iv: procedure(var v: LongInt); gldecl;
  glTexCoord3sv: procedure(var v: SmallInt); gldecl;
  glTexCoord4dv: procedure(var v: Double); gldecl;
  glTexCoord4fv: procedure(var v: Single); gldecl;
  glTexCoord4iv: procedure(var v: LongInt); gldecl;
  glTexCoord4sv: procedure(var v: SmallInt); gldecl;
  glRasterPos2d: procedure(x, y: Double); gldecl;
  glRasterPos2f: procedure(x, y: Single); gldecl;
  glRasterPos2i: procedure(x, y: LongInt); gldecl;
  glRasterPos2s: procedure(x, y: SmallInt); gldecl;
  glRasterPos3d: procedure(x, y, z: Double); gldecl;
  glRasterPos3f: procedure(x, y, z: Single); gldecl;
  glRasterPos3i: procedure(x, y, z: LongInt); gldecl;
  glRasterPos3s: procedure(x, y, z: SmallInt); gldecl;
  glRasterPos4d: procedure(x, y, z, w: Double); gldecl;
  glRasterPos4f: procedure(x, y, z, w: Single); gldecl;
  glRasterPos4i: procedure(x, y, z, w: LongInt); gldecl;
  glRasterPos4s: procedure(x, y, z, w: SmallInt); gldecl;
  glRasterPos2dv: procedure(var v: Double); gldecl;
  glRasterPos2fv: procedure(var v: Single); gldecl;
  glRasterPos2iv: procedure(var v: LongInt); gldecl;
  glRasterPos2sv: procedure(var v: SmallInt); gldecl;
  glRasterPos3dv: procedure(var v: Double); gldecl;
  glRasterPos3fv: procedure(var v: Single); gldecl;
  glRasterPos3iv: procedure(var v: LongInt); gldecl;
  glRasterPos3sv: procedure(var v: SmallInt); gldecl;
  glRasterPos4dv: procedure(var v: Double); gldecl;
  glRasterPos4fv: procedure(var v: Single); gldecl;
  glRasterPos4iv: procedure(var v: LongInt); gldecl;
  glRasterPos4sv: procedure(var v: SmallInt); gldecl;
  glRectd: procedure(x1, y1, x2, y2: Double); gldecl;
  glRectf: procedure(x1, y1, x2, y2: Single); gldecl;
  glRecti: procedure(x1, y1, x2, y2: LongInt); gldecl;
  glRects: procedure(x1, y1, x2, y2: SmallInt); gldecl;
  glRectdv: procedure(var v1, v2: Double); gldecl;
  glRectfv: procedure(var v1, v2: Single); gldecl;
  glRectiv: procedure(var v1, v2: LongInt); gldecl;
  glRectsv: procedure(var v1, v2: SmallInt); gldecl;

  // Vertex Arrays (1.1)
  glVertexPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); gldecl;
  glNormalPointer: procedure(AType: GLenum; stride: LongInt; var ptr); gldecl;
  glColorPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); gldecl;
  glIndexPointer: procedure(AType: GLenum; stride: LongInt; var ptr); gldecl;
  glTexCoordPointer: procedure(size: LongInt; AType: GLenum; stride: LongInt; var ptr); gldecl;
  glEdgeFlagPointer: procedure(stride: LongInt; var ptr); gldecl;
  glGetPointerv: procedure(pname: GLenum; var params: Pointer); gldecl;
  glArrayElement: procedure(i: LongInt); gldecl;
  glDrawArrays: procedure(mode: GLenum; first, count: LongInt); gldecl;
  glDrawElements: procedure(mode: GLenum; count: Integer; AType: GLenum; var indices); gldecl;
  glInterleavedArrays: procedure(format: GLenum; stride: LongInt; var pointer); gldecl;

  // Lighting
  glShadeModel: procedure(mode: GLenum); gldecl;
  glLightf: procedure(light, pname: GLenum; param: Single); gldecl;
  glLighti: procedure(light, pname: GLenum; param: LongInt); gldecl;
  glLightfv: procedure(light, pname: GLenum; var params: Single); gldecl;
  glLightiv: procedure(light, pname: GLenum; var params: LongInt); gldecl;
  glGetLightfv: procedure(light, pname: GLenum; var params: Single); gldecl;
  glGetLightiv: procedure(light, pname: GLenum; var params: LongInt); gldecl;
  glLightModelf: procedure(pname: GLenum; param: Single); gldecl;
  glLightModeli: procedure(pname: GLenum; param: LongInt); gldecl;
  glLightModelfv: procedure(pname: GLenum; var params: Single); gldecl;
  glLightModeliv: procedure(pname: GLenum; var param: LongInt); gldecl;
  glMaterialf: procedure(face, pname: GLenum; param: Single); gldecl;
  glMateriali: procedure(face, pname: GLenum; param: LongInt); gldecl;
  glMaterialfv: procedure(face, pname: GLenum; var params: Single); gldecl;
  glMaterialiv: procedure(face, pname: GLenum; var params: LongInt); gldecl;
  glGetMaterialfv: procedure(face, pname: GLenum; var params: Single); gldecl;
  glGetMaterialiv: procedure(face, pname: GLenum; var params: LongInt); gldecl;
  glColorMaterial: procedure(face, mode: GLenum); gldecl;

  // Raster Functions
  glPixelZoom: procedure(xfactor, yfactor: Single); gldecl;
  glPixelStoref: procedure(pname: GLenum; param: Single); gldecl;
  glPixelStorei: procedure(pname: GLenum; param: LongInt); gldecl;
  glPixelTransferf: procedure(pname: GLenum; param: Single); gldecl;
  glPixelTransferi: procedure(pname: GLenum; param: LongInt); gldecl;
  glPixelMapfv: procedure(map: GLenum; mapsize: LongInt; var values: Single); gldecl;
  glPixelMapuiv: procedure(map: GLenum; mapsize: LongInt; var values: LongWord); gldecl;
  glPixelMapusv: procedure(map: GLenum; mapsize: LongInt; var values: Word); gldecl;
  glGetPixelMapfv: procedure(map: GLenum; var values: Single); gldecl;
  glGetPixelMapuiv: procedure(map: GLenum; var values: LongWord); gldecl;
  glGetPixelMapusv: procedure(map: GLenum; var values: Word); gldecl;
  glBitmap: procedure(width, height: LongInt; xorig, yorig, xmove, ymove: Single; var bitmap); gldecl;
  glReadPixels: procedure(x, y, width, height: LongInt; format, AType: GLenum; var pixels); gldecl;
  glDrawPixels: procedure(width, height: LongInt; format, AType: GLenum; var pixels); gldecl;
  glCopyPixels: procedure(x, y, width, height: LongInt; AType: GLenum); gldecl;

  // Stenciling
  glStencilFunc: procedure(func: GLenum; ref: LongInt; mask: LongWord); gldecl;
  glStencilMask: procedure(mask: LongWord); gldecl;
  glStencilOp: procedure(fail, zfail, zpass: GLenum); gldecl;
  glClearStencil: procedure(s: LongInt); gldecl;

  // Texture Mapping
  glTexGend: procedure(cord, pname: GLenum; param: Double); gldecl;
  glTexGenf: procedure(cord, pname: GLenum; param: Single); gldecl;
  glTexGeni: procedure(cord, pname: GLenum; param: LongInt); gldecl;
  glTexGendv: procedure(cord, pname: GLenum; var params: Double); gldecl;
  glTexGenfv: procedure(cord, pname: GLenum; var params: Single); gldecl;
  glTexGeniv: procedure(cord, pname: GLenum; var params: LongInt); gldecl;
  glGetTexGendv: procedure(cord, pname: GLenum; var params: Double); gldecl;
  glGetTexGenfv: procedure(cord, pname: GLenum; var params: Single); gldecl;
  glGetTexGeniv: procedure(cord, pname: GLenum; var params: LongInt); gldecl;
  glTexEnvf: procedure(target, pname: GLenum; param: Single); gldecl;
  glTexEnvi: procedure(target, pname: GLenum; param: LongInt); gldecl;
  glTexEnvfv: procedure(target, pname: GLenum; var params: Single); gldecl;
  glTexEnviv: procedure(target, pname: GLenum; var params: LongInt); gldecl;
  glGetTexEnvfv: procedure(target, pname: GLenum; var params: Single); gldecl;
  glGetTexEnviv: procedure(target, pname: GLenum; var params: LongInt); gldecl;
  glTexParameterf: procedure(target, pname: GLenum; param: Single); gldecl;
  glTexParameteri: procedure(target, pname: GLenum; param: LongInt); gldecl;
  glTexParameterfv: procedure(target, pname: GLenum; var params: Single); gldecl;
  glTexParameteriv: procedure(target, pname: GLenum; var params: LongInt); gldecl;
  glGetTexParameterfv: procedure(target, pname: GLenum; var params: Single); gldecl;
  glGetTexParameteriv: procedure(target, pname: GLenum; var params: LongInt); gldecl;
  glGetTexLevelParameterfv: procedure(target: GLenum; level: LongInt; pname: GLenum; var params: Single); gldecl;
  glGetTexLevelParameteriv: procedure(target: GLenum; level: LongInt; pname: GLenum; var params: LongInt); gldecl;
  glTexImage1D: procedure(target: GLenum; level, internalFormat, width, border: LongInt; format, AType: GLenum; var pixels); gldecl;
  glTexImage2D: procedure(target: GLenum; level, internalFormat, width, height, border: LongInt; format, AType: GLenum; var pixels); gldecl;
  glGetTexImage: procedure(target: GLenum; level: LongInt; format, AType: GLenum; var pixels); gldecl;
  // 1.1 functions:
  glGenTextures: procedure(n: LongInt; var textures: LongWord); gldecl;
  glDeleteTextures: procedure(n: LongInt; var textures: LongWord); gldecl;
  glBindTexture: procedure(target: GLenum; texture: LongWord); gldecl;
  glPrioritizeTextures: procedure(n: LongInt; var textures: LongWord; var priorities: GLclampf); gldecl;
  glAreTexturesResident: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; gldecl;
  glIsTexture: function(texture: LongWord): Boolean; gldecl;
  glTexSubImage1D: procedure(target: GLenum; level, xoffset, width: LongInt; format, AType: GLenum; var pixels); gldecl;
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, width, height: LongInt; format, AType: GLenum; var pixels); gldecl;
  glCopyTexImage1D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, border: LongInt); gldecl;
  glCopyTexImage2D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, height, border: LongInt); gldecl;
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y, width: LongInt); gldecl;
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y, width, height: LongInt); gldecl;

  // Evaluators
  glMap1d: procedure(target: GLenum; u1, u2: Double; stride, order: LongInt; var points: Double); gldecl;
  glMap1f: procedure(target: GLenum; u1, u2: Single; stride, order: LongInt; var points: Single); gldecl;
  glMap2d: procedure(target: GLenum; u1, u2: Double; ustride, uorder: LongInt; v1, v2: Double; vstride, vorder: LongInt; var points: Double); gldecl;
  glMap2f: procedure(target: GLenum; u1, u2: Single; ustride, uorder: LongInt; v1, v2: Single; vstride, vorder: LongInt; var points: Single); gldecl;
  glGetMapdv: procedure(target, query: GLenum; var v: Double); gldecl;
  glGetMapfv: procedure(target, query: GLenum; var v: Single); gldecl;
  glGetMapiv: procedure(target, query: GLenum; var v: LongInt); gldecl;
  glEvalCoord1d: procedure(u: Double); gldecl;
  glEvalCoord1f: procedure(u: Single); gldecl;
  glEvalCoord1dv: procedure(var u: Double); gldecl;
  glEvalCoord1fv: procedure(var u: Single); gldecl;
  glEvalCoord2d: procedure(u, v: Double); gldecl;
  glEvalCoord2f: procedure(u, v: Single); gldecl;
  glEvalCoord2dv: procedure(var u, v: Double); gldecl;
  glEvalCoord2fv: procedure(var u, v: Single); gldecl;
  glMapGrid1d: procedure(un: LongInt; u1, u2: Double); gldecl;
  glMapGrid1f: procedure(un: LongInt; u1, u2: Single); gldecl;
  glMapGrid2d: procedure(un: LongInt; u1, u2: Double; vn: LongInt; v1, v2: Double); gldecl;
  glMapGrid2f: procedure(un: LongInt; u1, u2: Single; vn: LongInt; v1, v2: Single); gldecl;
  glEvalPoint1: procedure(i: LongInt); gldecl;
  glEvalPoint2: procedure(i, j: LongInt); gldecl;
  glEvalMesh1: procedure(mode: GLenum; i1, i2: LongInt); gldecl;
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: LongInt); gldecl;

  // Fog
  glFogf: procedure(pname: GLenum; param: Single); gldecl;
  glFogi: procedure(pname: GLenum; param: LongInt); gldecl;
  glFogfv: procedure(pname: GLenum; var params: Single); gldecl;
  glFogiv: procedure(pname: GLenum; var params: LongInt); gldecl;

  // Selection and Feedback
  glFeedbackBuffer: procedure(size: LongInt; AType: GLenum; var buffer: Single); gldecl;
  glPassThrough: procedure(token: Single); gldecl;
  glSelectBuffer: procedure(size: LongInt; var buffer: LongWord); gldecl;
  glInitNames: procedure; gldecl;
  glLoadName: procedure(name: LongWord); gldecl;
  glPushName: procedure(name: LongWord); gldecl;
  glPopName: procedure; gldecl;


// -------------------------------------------------------
//   GL Extensions
// -------------------------------------------------------

var
  // === 1.0 Extensions ===

  // GL_EXT_blend_minmax
  glBlendEquationEXT: procedure(mode: GLenum); gldecl;

  // GL_EXT_blend_color
  glBlendColorEXT: procedure(red, green, blue, alpha: GLclampf); gldecl;

  // GL_EXT_polygon_offset
  glPolygonOffsetEXT: procedure(factor, bias: Single); gldecl;

  // GL_EXT_vertex_array
  glVertexPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); gldecl;
  glNormalPointerEXT: procedure(AType: GLenum; stride, count: LongInt; var ptr); gldecl;
  glColorPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); gldecl;
  glIndexPointerEXT: procedure(AType: GLenum; stride, count: LongInt; var ptr); gldecl;
  glTexCoordPointerEXT: procedure(size: LongInt; AType: GLenum; stride, count: LongInt; var ptr); gldecl;
  glEdgeFlagPointerEXT: procedure(stride, count: LongInt; var ptr: Boolean); gldecl;
  glGetPointervEXT: procedure(pname: GLenum; var params: Pointer); gldecl;
  glArrayElementEXT: procedure(i: LongInt); gldecl;
  glDrawArraysEXT: procedure(mode: GLEnum; first, count: LongInt); gldecl;

  // GL_EXT_texture_object
  glGenTexturesEXT: procedure(n: LongInt; var textures: LongWord); gldecl;
  glDeleteTexturesEXT: procedure(n: LongInt; var textures: LongWord); gldecl;
  glBindTextureEXT: procedure(target: GLenum; texture: LongWord); gldecl;
  glPrioritizeTexturesEXT: procedure(n: LongInt; var textures: LongWord; var priorities: GLClampf); gldecl;
  glAreTexturesResidentEXT: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; gldecl;
  glIsTextureEXT: function(texture: LongWord): Boolean; gldecl;

  // GL_EXT_texture3D
  glTexImage3DEXT: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLenum; var pixels); gldecl;
  glTexSubImage3DEXT: procedure(target: GLenum; level, xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLenum; var pixels); gldecl;
  glCopyTexSubImage3DEXT: procedure(target: GLenum; level, xoffset, yoffset, zoffset, x, y, width, height: LongInt); gldecl;

  // GL_EXT_color_table
  glColorTableEXT: procedure(target, internalformat: GLenum; width: LongInt; format, AType: GLenum; var table); gldecl;
  glColorSubTableEXT: procedure(target: GLenum; start, count: LongInt; format, AType: GLEnum; var data); gldecl;
  glGetColorTableEXT: procedure(target, format, AType: GLenum; var table); gldecl;
  glGetColorTableParameterfvEXT: procedure(target, pname: GLenum; var params: Single); gldecl;
  glGetColorTableParameterivEXT: procedure(target, pname: GLenum; var params: LongInt); gldecl;

  // GL_SGIS_multitexture
  glMultiTexCoord1dSGIS: procedure(target: GLenum; s: Double); gldecl;
  glMultiTexCoord1dvSGIS: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord1fSGIS: procedure(target: GLenum; s: Single); gldecl;
  glMultiTexCoord1fvSGIS: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord1iSGIS: procedure(target: GLenum; s: LongInt); gldecl;
  glMultiTexCoord1ivSGIS: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord1sSGIS: procedure(target: GLenum; s: ShortInt); gldecl;
  glMultiTexCoord1svSGIS: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoord2dSGIS: procedure(target: GLenum; s, t: Double); gldecl;
  glMultiTexCoord2dvSGIS: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord2fSGIS: procedure(target: GLenum; s, t: Single); gldecl;
  glMultiTexCoord2fvSGIS: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord2iSGIS: procedure(target: GLenum; s, t: LongInt); gldecl;
  glMultiTexCoord2ivSGIS: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord2sSGIS: procedure(target: GLenum; s, t: ShortInt); gldecl;
  glMultiTexCoord2svSGIS: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoord3dSGIS: procedure(target: GLenum; s, t, r: Double); gldecl;
  glMultiTexCoord3dvSGIS: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord3fSGIS: procedure(target: GLenum; s, t, r: Single); gldecl;
  glMultiTexCoord3fvSGIS: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord3iSGIS: procedure(target: GLenum; s, t, r: LongInt); gldecl;
  glMultiTexCoord3ivSGIS: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord3sSGIS: procedure(target: GLenum; s, t, r: ShortInt); gldecl;
  glMultiTexCoord3svSGIS: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoord4dSGIS: procedure(target: GLenum; s, t, r, q: Double); gldecl;
  glMultiTexCoord4dvSGIS: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord4fSGIS: procedure(target: GLenum; s, t, r, q: Single); gldecl;
  glMultiTexCoord4fvSGIS: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord4iSGIS: procedure(target: GLenum; s, t, r, q: LongInt); gldecl;
  glMultiTexCoord4ivSGIS: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord4sSGIS: procedure(target: GLenum; s, t, r, q: ShortInt); gldecl;
  glMultiTexCoord4svSGIS: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoordPointerSGIS: procedure(target: GLenum; size: LongInt; AType: GLEnum; stride: LongInt; var APointer); gldecl;
  glSelectTextureSGIS: procedure(target: GLenum); gldecl;
  glSelectTextureCoordSetSGIS: procedure(target: GLenum); gldecl;

  // GL_EXT_multitexture
  glMultiTexCoord1dEXT: procedure(target: GLenum; s: Double); gldecl;
  glMultiTexCoord1dvEXT: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord1fEXT: procedure(target: GLenum; s: Single); gldecl;
  glMultiTexCoord1fvEXT: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord1iEXT: procedure(target: GLenum; s: LongInt); gldecl;
  glMultiTexCoord1ivEXT: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord1sEXT: procedure(target: GLenum; s: ShortInt); gldecl;
  glMultiTexCoord1svEXT: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoord2dEXT: procedure(target: GLenum; s, t: Double); gldecl;
  glMultiTexCoord2dvEXT: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord2fEXT: procedure(target: GLenum; s, t: Single); gldecl;
  glMultiTexCoord2fvEXT: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord2iEXT: procedure(target: GLenum; s, t: LongInt); gldecl;
  glMultiTexCoord2ivEXT: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord2sEXT: procedure(target: GLenum; s, t: ShortInt); gldecl;
  glMultiTexCoord2svEXT: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoord3dEXT: procedure(target: GLenum; s, t, r: Double); gldecl;
  glMultiTexCoord3dvEXT: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord3fEXT: procedure(target: GLenum; s, t, r: Single); gldecl;
  glMultiTexCoord3fvEXT: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord3iEXT: procedure(target: GLenum; s, t, r: LongInt); gldecl;
  glMultiTexCoord3ivEXT: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord3sEXT: procedure(target: GLenum; s, t, r: ShortInt); gldecl;
  glMultiTexCoord3svEXT: procedure(target: GLenum; var v: ShortInt); gldecl;
  glMultiTexCoord4dEXT: procedure(target: GLenum; s, t, r, q: Double); gldecl;
  glMultiTexCoord4dvEXT: procedure(target: GLenum; var v: Double); gldecl;
  glMultiTexCoord4fEXT: procedure(target: GLenum; s, t, r, q: Single); gldecl;
  glMultiTexCoord4fvEXT: procedure(target: GLenum; var v: Single); gldecl;
  glMultiTexCoord4iEXT: procedure(target: GLenum; s, t, r, q: LongInt); gldecl;
  glMultiTexCoord4ivEXT: procedure(target: GLenum; var v: LongInt); gldecl;
  glMultiTexCoord4sEXT: procedure(target: GLenum; s, t, r, q: ShortInt); gldecl;
  glMultiTexCoord4svEXT: procedure(target: GLenum; var v: ShortInt); gldecl;
  glInterleavedTextureCoordSetsEXT: procedure(factor: LongInt); gldecl;
  glSelectTextureEXT: procedure(target: GLenum); gldecl;
  glSelectTextureCoordSetEXT: procedure(target: GLenum); gldecl;
  glSelectTextureTransformEXT: procedure(target: GLenum); gldecl;

  // GL_EXT_point_parameters
  glPointParameterfEXT: procedure(pname: GLenum; param: Single); gldecl;
  glPointParameterfvEXT: procedure(pname: GLenum; var params: Single); gldecl;

  // GL_MESA_window_pos
  glWindowPos2iMESA: procedure(x, y: LongInt); gldecl;
  glWindowPos2sMESA: procedure(x, y: ShortInt); gldecl;
  glWindowPos2fMESA: procedure(x, y: Single); gldecl;
  glWindowPos2dMESA: procedure(x, y: Double); gldecl;
  glWindowPos2ivMESA: procedure(var p: LongInt); gldecl;
  glWindowPos2svMESA: procedure(var p: ShortInt); gldecl;
  glWindowPos2fvMESA: procedure(var p: Single); gldecl;
  glWindowPos2dvMESA: procedure(var p: Double); gldecl;
  glWindowPos3iMESA: procedure(x, y, z: LongInt); gldecl;
  glWindowPos3sMESA: procedure(x, y, z: ShortInt); gldecl;
  glWindowPos3fMESA: procedure(x, y, z: Single); gldecl;
  glWindowPos3dMESA: procedure(x, y, z: Double); gldecl;
  glWindowPos3ivMESA: procedure(var p: LongInt); gldecl;
  glWindowPos3svMESA: procedure(var p: ShortInt); gldecl;
  glWindowPos3fvMESA: procedure(var p: Single); gldecl;
  glWindowPos3dvMESA: procedure(var p: Double); gldecl;
  glWindowPos4iMESA: procedure(x, y, z, w: LongInt); gldecl;
  glWindowPos4sMESA: procedure(x, y, z, w: ShortInt); gldecl;
  glWindowPos4fMESA: procedure(x, y, z, w: Single); gldecl;
  glWindowPos4dMESA: procedure(x, y, z, w: Double); gldecl;
  glWindowPos4ivMESA: procedure(var p: LongInt); gldecl;
  glWindowPos4svMESA: procedure(var p: ShortInt); gldecl;
  glWindowPos4fvMESA: procedure(var p: Single); gldecl;
  glWindowPos4dvMESA: procedure(var p: Double); gldecl;

  // GL_MESA_resize_buffers
  glResizeBuffersMESA: procedure; gldecl;


// -------------------------------------------------------
//   GL 1.2 Functions
// -------------------------------------------------------

  glDrawRangeElements: procedure(mode: GLenum; AStart, AEnd: LongWord; count: LongInt; AType: GLenum; var indices); gldecl;
  glTexImage3D: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, AType: GLEnum; var pixels); gldecl;
  glTexSubImage3D: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, width, height, depth: LongInt; format, AType: GLEnum; var pixels); gldecl;
  glCopyTexSubImage3D: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, x, y, width, height: LongInt); gldecl;


// =======================================================
//   GLU consts, types and functions
// =======================================================

const
  GLU_TRUE 				= GL_TRUE;
  GLU_FALSE 				= GL_FALSE;

  // Normal vectors
  GLU_SMOOTH				= 100000;
  GLU_FLAT				= 100001;
  GLU_NONE				= 100002;

  // Quadric draw styles
  GLU_POINT				= 100010;
  GLU_LINE				= 100011;
  GLU_FILL				= 100012;
  GLU_SILHOUETTE			= 100013;

  // Quadric orientation
  GLU_OUTSIDE				= 100020;
  GLU_INSIDE				= 100021;

  // Tesselator
  GLU_BEGIN				= 100100;
  GLU_VERTEX				= 100101;
  GLU_END				= 100102;
  GLU_ERROR				= 100103;
  GLU_EDGE_FLAG				= 100104;

  // Contour types
  GLU_CW				= 100120;
  GLU_CCW				= 100121;
  GLU_INTERIOR				= 100122;
  GLU_EXTERIOR				= 100123;
  GLU_UNKNOWN				= 100124;

  // Tesselation errors
  GLU_TESS_ERROR1			= 100151;  // missing gluEndPolygon
  GLU_TESS_ERROR2 			= 100152;  // missing gluBeginPolygon
  GLU_TESS_ERROR3 			= 100153;  // misoriented contour
  GLU_TESS_ERROR4 			= 100154;  // vertex/edge intersection
  GLU_TESS_ERROR5 			= 100155;  // misoriented or self-intersecting loops
  GLU_TESS_ERROR6 			= 100156;  // coincident vertices
  GLU_TESS_ERROR7 			= 100157;  // all vertices collinear
  GLU_TESS_ERROR8 			= 100158;  // intersecting edges
  GLU_TESS_ERROR9 			= 100159;  // not coplanar contours

  // NURBS
  GLU_AUTO_LOAD_MATRIX			= 100200;
  GLU_CULLING				= 100201;
  GLU_PARAMETRIC_TOLERANCE		= 100202;
  GLU_SAMPLING_TOLERANCE		= 100203;
  GLU_DISPLAY_MODE			= 100204;
  GLU_SAMPLING_METHOD			= 100205;
  GLU_U_STEP				= 100206;
  GLU_V_STEP				= 100207;

  GLU_PATH_LENGTH			= 100215;
  GLU_PARAMETRIC_ERROR			= 100216;
  GLU_DOMAIN_DISTANCE			= 100217;

  GLU_MAP1_TRIM_2			= 100210;
  GLU_MAP1_TRIM_3			= 100211;

  GLU_OUTLINE_POLYGON			= 100240;
  GLU_OUTLINE_PATCH			= 100241;

  GLU_NURBS_ERROR1  			= 100251;   // spline order un-supported
  GLU_NURBS_ERROR2  			= 100252;   // too few knots
  GLU_NURBS_ERROR3  			= 100253;   // valid knot range is empty
  GLU_NURBS_ERROR4  			= 100254;   // decreasing knot sequence
  GLU_NURBS_ERROR5  			= 100255;   // knot multiplicity > spline order
  GLU_NURBS_ERROR6  			= 100256;   // endcurve() must follow bgncurve()
  GLU_NURBS_ERROR7  			= 100257;   // bgncurve() must precede endcurve()
  GLU_NURBS_ERROR8  			= 100258;   // ctrlarray or knot vector is NULL
  GLU_NURBS_ERROR9  			= 100259;   // cannot draw pwlcurves
  GLU_NURBS_ERROR10 			= 100260;   // missing gluNurbsCurve()
  GLU_NURBS_ERROR11 			= 100261;   // missing gluNurbsSurface()
  GLU_NURBS_ERROR12 			= 100262;   // endtrim() must precede endsurface()
  GLU_NURBS_ERROR13 			= 100263;   // bgnsurface() must precede endsurface()
  GLU_NURBS_ERROR14 			= 100264;   // curve of improper type passed as trim curve
  GLU_NURBS_ERROR15 			= 100265;   // bgnsurface() must precede bgntrim()
  GLU_NURBS_ERROR16 			= 100266;   // endtrim() must follow bgntrim()
  GLU_NURBS_ERROR17 			= 100267;   // bgntrim() must precede endtrim()*/
  GLU_NURBS_ERROR18 			= 100268;   // invalid or missing trim curve*/
  GLU_NURBS_ERROR19 			= 100269;   // bgntrim() must precede pwlcurve()
  GLU_NURBS_ERROR20 			= 100270;   // pwlcurve referenced twice*/
  GLU_NURBS_ERROR21 			= 100271;   // pwlcurve and nurbscurve mixed
  GLU_NURBS_ERROR22 			= 100272;   // improper usage of trim data type
  GLU_NURBS_ERROR23 			= 100273;   // nurbscurve referenced twice
  GLU_NURBS_ERROR24 			= 100274;   // nurbscurve and pwlcurve mixed
  GLU_NURBS_ERROR25 			= 100275;   // nurbssurface referenced twice
  GLU_NURBS_ERROR26 			= 100276;   // invalid property
  GLU_NURBS_ERROR27 			= 100277;   // endsurface() must follow bgnsurface()
  GLU_NURBS_ERROR28 			= 100278;   // intersecting or misoriented trim curves
  GLU_NURBS_ERROR29 			= 100279;   // intersecting trim curves
  GLU_NURBS_ERROR30 			= 100280;   // UNUSED
  GLU_NURBS_ERROR31 			= 100281;   // unconnected trim curves
  GLU_NURBS_ERROR32 			= 100282;   // unknown knot error
  GLU_NURBS_ERROR33 			= 100283;   // negative vertex count encountered
  GLU_NURBS_ERROR34 			= 100284;   // negative byte-stride
  GLU_NURBS_ERROR35 			= 100285;   // unknown type descriptor
  GLU_NURBS_ERROR36 			= 100286;   // null control point reference
  GLU_NURBS_ERROR37 			= 100287;   // duplicate point on pwlcurve

  // Errors
  GLU_INVALID_ENUM			= 100900;
  GLU_INVALID_VALUE			= 100901;
  GLU_OUT_OF_MEMORY			= 100902;
  GLU_INCOMPATIBLE_GL_VERSION		= 100903;

  // New in GLU 1.1
  GLU_VERSION				= 100800;
  GLU_EXTENSIONS			= 100801;

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
  gluLookAt: procedure(eye, eyey, eyez, centerx, centery, centerz, upx, upy, upz: Double); gldecl;
  gluOrtho2D: procedure(left, right, bottom, top: Double); gldecl;
  gluPerspective: procedure(fovy, aspect, zNear, zFar: Double); gldecl;
  gluPickMatrix: procedure(x, y, width, height: Double; const viewport: TGLUViewport); gldecl;
  gluProject: procedure(objx, objy, objz: Double; const modelMatrix, projMatrix: TGLUMatrixd; const viewport: TGLUViewport; winx, winy, winz: Double); gldecl;
  gluUnProject: procedure(winx, winy, winz: Double; const modelMatrix, projMatrix: TGLUMatrixd; const viewport: TGLUViewport; objx, objy, objz: Double); gldecl;
  gluErrorString: procedure(errorCode: GLenum); gldecl;

  // Mipmapping and image scaling
  gluScaleImage: procedure(format: GLenum; within, heightin: LongInt; typein: GLenum; var datain; widthout, heightout: LongInt; typeout: GLenum; var dataout); gldecl;
  gluBuild1DMipmaps: procedure(target: GLenum; components, width: LongInt; format, AType: GLEnum; var data); gldecl;
  gluBuild2DMipmaps: procedure(target: GLenum; components, width, height: LongInt; format, AType: GLEnum; var data); gldecl;

  // Quadrics
  gluNewQuadric: function: PGLUquadricObj; gldecl;
  gluDeleteQuadric: procedure(state: PGLUquadricObj); gldecl;
  gluQuadricDrawStyle: procedure(quadObject: PGLUquadricObj; drawStyle: GLenum); gldecl;
  gluQuadricOrientation: procedure(quadObject: PGLUquadricObj; orientation: GLenum); gldecl;
  gluQuadricNormals: procedure(quadObject: PGLUquadricObj; normals: GLenum); gldecl;
  gluQuadricTexture: procedure(quadObject: PGLUquadricObj; textureCoords: Boolean); gldecl;
  gluQuadricCallback: procedure(quadObject: PGLUquadricObj; which: GLenum; fn: TGLUQuadricCallback); gldecl;
  gluCylinder: procedure(qobj: PGLUquadricObj; baseRadius, topRadius, height: Double; slices, stacks: LongInt); gldecl;
  gluSphere: procedure(qobj: PGLUquadricObj; radius: Double; slices, stacks: LongInt); gldecl;
  gluDisk: procedure(qobj: PGLUquadricObj; innerRadius, outerRadius: Double; slices, loops: LongInt); gldecl;
  gluPartialDisk: procedure(qobj: PGLUquadricObj; innerRadius, outerRadius: Double; slices, loops: LongInt; startAngle, sweepAngle: Double); gldecl;

  // Nurbs
  gluNewNurbsRenderer: function: PGLUnurbsObj; gldecl;
  gluDeleteNurbsRenderer: procedure(nobj: PGLUnurbsObj); gldecl;
  gluLoadSamplingMatrices: procedure(nobj: PGLUnurbsObj; const modelMatrix, projMatrix: TGLUMatrixf; const viewport: TGLUViewport); gldecl;
  gluNurbsProperty: procedure(nobj: PGLUnurbsObj; AProperty: GLenum; value: Single); gldecl;
  gluGetNurbsProperty: procedure(nobj: PGLUnurbsObj; AProperty: GLEnum; var value: Single); gldecl;
  gluBeginCurve: procedure(nobj: PGLUnurbsObj); gldecl;
  gluEndCurve: procedure(nobj: PGLUnurbsObj); gldecl;
  gluNurbsCurve: procedure(nobj: PGLUnurbsObj; nknots: LongInt; var know: Single; stride: LongInt; var ctlarray: Single; order: LongInt; AType: GLenum); gldecl;
  gluBeginSurface: procedure(nobj: PGLUnurbsObj); gldecl;
  gluEndSurface: procedure(nobj: PGLUnurbsObj); gldecl;
  gluNurbsSurface: procedure(nobj: PGLUnurbsObj; sknot_count: LongInt; var sknot: Single; tknot_count: LongInt; var tknot: Single; s_stride, t_stride: LongInt; var ctlarray: Single; sorder, torder: LongInt; AType: GLenum); gldecl;
  gluBeginTrim: procedure(nobj: PGLUnurbsObj); gldecl;
  gluEndTrim: procedure(nobj: PGLUnurbsObj); gldecl;
  gluPwlCurve: procedure(nobj: PGLUnurbsObj; count: LongInt; var AArray: Single; stride: LongInt; AType: GLenum); gldecl;
  gluNurbsCallback: procedure(nobj: PGLUnurbsObj; which: GLenum; fn: TGLUNurbsCallback); gldecl;

  // Polygon tesselation
  gluNewTess: function: PGLUtriangulatorObj; gldecl;
  gluTessCallback: procedure(tobj: PGLUtriangulatorObj; which: GLenum; fn: TGLUTessCallback); gldecl;

  gluDeleteTess: procedure(tobj: PGLUtriangulatorObj); gldecl;
  gluBeginPolygon: procedure(tobj: PGLUtriangulatorObj); gldecl;
  gluEndPolygon: procedure(tobj: PGLUtriangulatorObj); gldecl;
  gluNextContour: procedure(tobj: PGLUtriangulatorObj; AType: GLenum); gldecl;
  gluTessVertex: procedure(tobj: PGLUtriangulatorObj; v: TGLUVectord; var data); gldecl;

  // New functions in GLU 1.1
  gluGetString: function(name: GLenum): PChar; gldecl;


// =======================================================
//   GLX consts, types and functions
// =======================================================

{$IFDEF HasGLX}

// Tokens for glXChooseVisual and glXGetConfig:
const
  GLX_USE_GL				= 1;
  GLX_BUFFER_SIZE			= 2;
  GLX_LEVEL				= 3;
  GLX_RGBA				= 4;
  GLX_DOUBLEBUFFER			= 5; 
  GLX_STEREO				= 6;
  GLX_AUX_BUFFERS			= 7;
  GLX_RED_SIZE				= 8;
  GLX_GREEN_SIZE			= 9;
  GLX_BLUE_SIZE				= 10;
  GLX_ALPHA_SIZE			= 11;
  GLX_DEPTH_SIZE			= 12;
  GLX_STENCIL_SIZE			= 13;
  GLX_ACCUM_RED_SIZE			= 14;
  GLX_ACCUM_GREEN_SIZE			= 15;
  GLX_ACCUM_BLUE_SIZE			= 16;
  GLX_ACCUM_ALPHA_SIZE			= 17;

  // GLX_EXT_visual_info extension
  GLX_X_VISUAL_TYPE_EXT			= $22;
  GLX_TRANSPARENT_TYPE_EXT		= $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT 	= $24;
  GLX_TRANSPARENT_RED_VALUE_EXT		= $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT	= $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT	= $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT	= $28;


  // Error codes returned by glXGetConfig:
  GLX_BAD_SCREEN			= 1;
  GLX_BAD_ATTRIBUTE			= 2;
  GLX_NO_EXTENSION			= 3;
  GLX_BAD_VISUAL			= 4;
  GLX_BAD_CONTEXT			= 5;
  GLX_BAD_VALUE       			= 6;
  GLX_BAD_ENUM				= 7;

  // GLX 1.1 and later:
  GLX_VENDOR				= 1;
  GLX_VERSION				= 2;
  GLX_EXTENSIONS 			= 3;

  // GLX_visual_info extension
  GLX_TRUE_COLOR_EXT			= $8002;
  GLX_DIRECT_COLOR_EXT			= $8003;
  GLX_PSEUDO_COLOR_EXT			= $8004;
  GLX_STATIC_COLOR_EXT			= $8005;
  GLX_GRAY_SCALE_EXT			= $8006;
  GLX_STATIC_GRAY_EXT			= $8007;
  GLX_NONE_EXT				= $8000;
  GLX_TRANSPARENT_RGB_EXT		= $8008;
  GLX_TRANSPARENT_INDEX_EXT		= $8009;

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
  glXChooseVisual: function(dpy: PDisplay; screen: Integer; var attribList: Integer): PXVisualInfo; cdecl;
  glXCreateContext: function(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl;
  glXDestroyContext: procedure(dpy: PDisplay; ctx: GLXContext); cdecl;
  glXMakeCurrent: function(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl;
  glXCopyContext: procedure(dpy: PDisplay; src, dst: GLXContext; mask: LongWord); cdecl;
  glXSwapBuffers: procedure(dpy: PDisplay; drawable: GLXDrawable); cdecl;
  glXCreateGLXPixmap: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap): GLXPixmap; cdecl;
  glXDestroyGLXPixmap: procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  glXQueryExtension: function(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl;
  glXQueryVersion: function(dpy: PDisplay; var maj, min: Integer): Boolean; cdecl;
  glXIsDirect: function(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl;
  glXGetConfig: function(dpy: PDisplay; visual: PXVisualInfo; attrib: Integer; var value: Integer): Integer; cdecl;
  glXGetCurrentContext: function: GLXContext; cdecl;
  glXGetCurrentDrawable: function: GLXDrawable; cdecl;
  glXWaitGL: procedure; cdecl;
  glXWaitX: procedure; cdecl;
  glXUseXFont: procedure(font: XFont; first, count, list: Integer); cdecl;

  // GLX 1.1 and later
  glXQueryExtensionsString: function(dpy: PDisplay; screen: Integer): PChar; cdecl;
  glXQueryServerString: function(dpy: PDisplay; screen, name: Integer): PChar; cdecl;
  glXGetClientString: function(dpy: PDisplay; name: Integer): PChar; cdecl;

  // Mesa GLX Extensions
  glXCreateGLXPixmapMESA: function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl;
  glXReleaseBufferMESA: function(dpy: PDisplay; d: GLXDrawable): Boolean; cdecl;
  glXCopySubBufferMESA: procedure(dpy: PDisplay; drawbale: GLXDrawable; x, y, width, height: Integer); cdecl;
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl;
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl;

{$ENDIF  IFDEF HasGLX}



// =======================================================
// =======================================================

implementation


{$IFDEF Linux}
{$LINKLIB m}
type
  HInstance = LongWord;

function dlopen(AFile: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; name: PChar): Pointer; external 'dl';

function LoadLibrary(name: PChar): HInstance;
begin
  Result := LongWord(dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY}));
end;

procedure FreeLibrary(handle: HInstance);
begin
  dlclose(Pointer(handle));
end;

function GetProcAddress(handle: HInstance; name: PChar): Pointer;
begin
  Result := dlsym(Pointer(handle), name);
  // if Result = nil then WriteLn('Unresolved: ', name);
end;

{$ENDIF}

var
  libGL, libGLU: HInstance;

function InitGLFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGL := LoadLibrary(libname);
  if libGL = 0 then exit;

  glClearIndex := GetProcAddress(libGL, 'glClearIndex');
  glClearColor := GetProcAddress(libGL, 'glClearColor');
  glClear := GetProcAddress(libGL, 'glClear');
  glIndexMask := GetProcAddress(libGL, 'glIndexMask');
  glColorMask := GetProcAddress(libGL, 'glColorMask');
  glAlphaFunc := GetProcAddress(libGL, 'glAlphaFunc');
  glBlendFunc := GetProcAddress(libGL, 'glBlendFunc');
  glLogicOp := GetProcAddress(libGL, 'glLogicOp');
  glCullFace := GetProcAddress(libGL, 'glCullFace');
  glFrontFace := GetProcAddress(libGL, 'glFrontFace');
  glPointSize := GetProcAddress(libGL, 'glPointSize');
  glLineWidth := GetProcAddress(libGL, 'glLineWidth');
  glLineStipple := GetProcAddress(libGL, 'glLineStipple');
  glPolygonMode := GetProcAddress(libGL, 'glPolygonMode');
  glPolygonOffset := GetProcAddress(libGL, 'glPolygonOffset');
  glPolygonStipple := GetProcAddress(libGL, 'glPolygonStipple');
  glGetPolygonStipple := GetProcAddress(libGL, 'glGetPolygonStipple');
  glEdgeFlag := GetProcAddress(libGL, 'glEdgeFlag');
  glEdgeFlagv := GetProcAddress(libGL, 'glEdgeFlagv');
  glScissor := GetProcAddress(libGL, 'glScissor');
  glClipPlane := GetProcAddress(libGL, 'glClipPlane');
  glGetClipPlane := GetProcAddress(libGL, 'glGetClipPlane');
  glDrawBuffer := GetProcAddress(libGL, 'glDrawBuffer');
  glReadBuffer := GetProcAddress(libGL, 'glReadBuffer');
  glEnable := GetProcAddress(libGL, 'glEnable');
  glDisable := GetProcAddress(libGL, 'glDisable');
  glIsEnabled := GetProcAddress(libGL, 'glIsEnabled');
  glEnableClientState := GetProcAddress(libGL, 'glEnableClientState');
  glDisableClientState := GetProcAddress(libGL, 'glDisableClientState');
  glGetBooleanv := GetProcAddress(libGL, 'glGetBooleanv');
  glGetDoublev := GetProcAddress(libGL, 'glGetDoublev');
  glGetFloatv := GetProcAddress(libGL, 'glGetFloatv');
  glGetIntegerv := GetProcAddress(libGL, 'glGetIntegerv');
  glPushAttrib := GetProcAddress(libGL, 'glPushAttrib');
  glPopAttrib := GetProcAddress(libGL, 'glPopAttrib');
  glPushClientAttrib := GetProcAddress(libGL, 'glPushClientAttrib');
  glPopClientAttrib := GetProcAddress(libGL, 'glPopClientAttrib');
  glRenderMode := GetProcAddress(libGL, 'glRenderMode');
  glGetError := GetProcAddress(libGL, 'glGetError');
  glGetString := GetProcAddress(libGL, 'glGetString');
  glFinish := GetProcAddress(libGL, 'glFinish');
  glFlush := GetProcAddress(libGL, 'glFlush');
  glHint := GetProcAddress(libGL, 'glHint');
  glClearDepth := GetProcAddress(libGL, 'glClearDepth');
  glDepthFunc := GetProcAddress(libGL, 'glDepthFunc');
  glDepthMask := GetProcAddress(libGL, 'glDepthMask');
  glDepthRange := GetProcAddress(libGL, 'glDepthRange');
  glClearAccum := GetProcAddress(libGL, 'glClearAccum');
  glAccum := GetProcAddress(libGL, 'glAccum');
  glMatrixMode := GetProcAddress(libGL, 'glMatrixMode');
  glOrtho := GetProcAddress(libGL, 'glOrtho');
  glFrustum := GetProcAddress(libGL, 'glFrustum');
  glViewport := GetProcAddress(libGL, 'glViewport');
  glPushMatrix := GetProcAddress(libGL, 'glPushMatrix');
  glPopMatrix := GetProcAddress(libGL, 'glPopMatrix');
  glLoadIdentity := GetProcAddress(libGL, 'glLoadIdentity');
  glLoadMatrixd := GetProcAddress(libGL, 'glLoadMatrixd');
  glLoadMatrixf := GetProcAddress(libGL, 'glLoadMatrixf');
  glMultMatrixd := GetProcAddress(libGL, 'glMultMatrixd');
  glMultMatrixf := GetProcAddress(libGL, 'glMultMatrixf');
  glRotated := GetProcAddress(libGL, 'glRotated');
  glRotatef := GetProcAddress(libGL, 'glRotatef');
  glScaled := GetProcAddress(libGL, 'glScaled');
  glScalef := GetProcAddress(libGL, 'glScalef');
  glTranslated := GetProcAddress(libGL, 'glTranslated');
  glTranslatef := GetProcAddress(libGL, 'glTranslatef');
  glIsList := GetProcAddress(libGL, 'glIsList');
  glDeleteLists := GetProcAddress(libGL, 'glDeleteLists');
  glGenLists := GetProcAddress(libGL, 'glGenLists');
  glNewList := GetProcAddress(libGL, 'glNewList');
  glEndList := GetProcAddress(libGL, 'glEndList');
  glCallList := GetProcAddress(libGL, 'glCallList');
  glCallLists := GetProcAddress(libGL, 'glCallLists');
  glListBase := GetProcAddress(libGL, 'glListBase');
  glBegin := GetProcAddress(libGL, 'glBegin');
  glEnd := GetProcAddress(libGL, 'glEnd');
  glVertex2d := GetProcAddress(libGL, 'glVertex2d');
  glVertex2f := GetProcAddress(libGL, 'glVertex2f');
  glVertex2i := GetProcAddress(libGL, 'glVertex2i');
  glVertex2s := GetProcAddress(libGL, 'glVertex2s');
  glVertex3d := GetProcAddress(libGL, 'glVertex3d');
  glVertex3f := GetProcAddress(libGL, 'glVertex3f');
  glVertex3i := GetProcAddress(libGL, 'glVertex3i');
  glVertex3s := GetProcAddress(libGL, 'glVertex3s');
  glVertex4d := GetProcAddress(libGL, 'glVertex4d');
  glVertex4f := GetProcAddress(libGL, 'glVertex4f');
  glVertex4i := GetProcAddress(libGL, 'glVertex4i');
  glVertex4s := GetProcAddress(libGL, 'glVertex4s');
  glVertex2dv := GetProcAddress(libGL, 'glVertex2dv');
  glVertex2fv := GetProcAddress(libGL, 'glVertex2fv');
  glVertex2iv := GetProcAddress(libGL, 'glVertex2iv');
  glVertex2sv := GetProcAddress(libGL, 'glVertex2sv');
  glVertex3dv := GetProcAddress(libGL, 'glVertex3dv');
  glVertex3fv := GetProcAddress(libGL, 'glVertex3fv');
  glVertex3iv := GetProcAddress(libGL, 'glVertex3iv');
  glVertex3sv := GetProcAddress(libGL, 'glVertex3sv');
  glVertex4dv := GetProcAddress(libGL, 'glVertex4dv');
  glVertex4fv := GetProcAddress(libGL, 'glVertex4fv');
  glVertex4iv := GetProcAddress(libGL, 'glVertex4iv');
  glVertex4sv := GetProcAddress(libGL, 'glVertex4sv');
  glNormal3b := GetProcAddress(libGL, 'glNormal3b');
  glNormal3d := GetProcAddress(libGL, 'glNormal3d');
  glNormal3f := GetProcAddress(libGL, 'glNormal3f');
  glNormal3i := GetProcAddress(libGL, 'glNormal3i');
  glNormal3s := GetProcAddress(libGL, 'glNormal3s');
  glNormal3bv := GetProcAddress(libGL, 'glNormal3bv');
  glNormal3dv := GetProcAddress(libGL, 'glNormal3dv');
  glNormal3fv := GetProcAddress(libGL, 'glNormal3fv');
  glNormal3iv := GetProcAddress(libGL, 'glNormal3iv');
  glNormal3sv := GetProcAddress(libGL, 'glNormal3sv');
  glIndexd := GetProcAddress(libGL, 'glIndexd');
  glIndexf := GetProcAddress(libGL, 'glIndexf');
  glIndexi := GetProcAddress(libGL, 'glIndexi');
  glIndexs := GetProcAddress(libGL, 'glIndexs');
  glIndexub := GetProcAddress(libGL, 'glIndexub');
  glIndexdv := GetProcAddress(libGL, 'glIndexdv');
  glIndexfv := GetProcAddress(libGL, 'glIndexfv');
  glIndexiv := GetProcAddress(libGL, 'glIndexiv');
  glIndexsv := GetProcAddress(libGL, 'glIndexsv');
  glIndexubv := GetProcAddress(libGL, 'glIndexubv');
  glColor3b := GetProcAddress(libGL, 'glColor3b');
  glColor3d := GetProcAddress(libGL, 'glColor3d');
  glColor3f := GetProcAddress(libGL, 'glColor3f');
  glColor3i := GetProcAddress(libGL, 'glColor3i');
  glColor3s := GetProcAddress(libGL, 'glColor3s');
  glColor3ub := GetProcAddress(libGL, 'glColor3ub');
  glColor3ui := GetProcAddress(libGL, 'glColor3ui');
  glColor3us := GetProcAddress(libGL, 'glColor3us');
  glColor4b := GetProcAddress(libGL, 'glColor4b');
  glColor4d := GetProcAddress(libGL, 'glColor4d');
  glColor4f := GetProcAddress(libGL, 'glColor4f');
  glColor4i := GetProcAddress(libGL, 'glColor4i');
  glColor4s := GetProcAddress(libGL, 'glColor4s');
  glColor4ub := GetProcAddress(libGL, 'glColor4ub');
  glColor4ui := GetProcAddress(libGL, 'glColor4ui');
  glColor4us := GetProcAddress(libGL, 'glColor4us');
  glColor3bv := GetProcAddress(libGL, 'glColor3bv');
  glColor3dv := GetProcAddress(libGL, 'glColor3dv');
  glColor3fv := GetProcAddress(libGL, 'glColor3fv');
  glColor3iv := GetProcAddress(libGL, 'glColor3iv');
  glColor3sv := GetProcAddress(libGL, 'glColor3sv');
  glColor3ubv := GetProcAddress(libGL, 'glColor3ubv');
  glColor3uiv := GetProcAddress(libGL, 'glColor3uiv');
  glColor3usv := GetProcAddress(libGL, 'glColor3usv');
  glColor4bv := GetProcAddress(libGL, 'glColor4bv');
  glColor4dv := GetProcAddress(libGL, 'glColor4dv');
  glColor4fv := GetProcAddress(libGL, 'glColor4fv');
  glColor4iv := GetProcAddress(libGL, 'glColor4iv');
  glColor4sv := GetProcAddress(libGL, 'glColor4sv');
  glColor4ubv := GetProcAddress(libGL, 'glColor4ubv');
  glColor4uiv := GetProcAddress(libGL, 'glColor4uiv');
  glColor4usv := GetProcAddress(libGL, 'glColor4usv');
  glTexCoord1d := GetProcAddress(libGL, 'glTexCoord1d');
  glTexCoord1f := GetProcAddress(libGL, 'glTexCoord1f');
  glTexCoord1i := GetProcAddress(libGL, 'glTexCoord1i');
  glTexCoord1s := GetProcAddress(libGL, 'glTexCoord1s');
  glTexCoord2d := GetProcAddress(libGL, 'glTexCoord2d');
  glTexCoord2f := GetProcAddress(libGL, 'glTexCoord2f');
  glTexCoord2i := GetProcAddress(libGL, 'glTexCoord2i');
  glTexCoord2s := GetProcAddress(libGL, 'glTexCoord2s');
  glTexCoord3d := GetProcAddress(libGL, 'glTexCoord3d');
  glTexCoord3f := GetProcAddress(libGL, 'glTexCoord3f');
  glTexCoord3i := GetProcAddress(libGL, 'glTexCoord3i');
  glTexCoord3s := GetProcAddress(libGL, 'glTexCoord3s');
  glTexCoord4d := GetProcAddress(libGL, 'glTexCoord4d');
  glTexCoord4f := GetProcAddress(libGL, 'glTexCoord4f');
  glTexCoord4i := GetProcAddress(libGL, 'glTexCoord4i');
  glTexCoord4s := GetProcAddress(libGL, 'glTexCoord4s');
  glTexCoord1dv := GetProcAddress(libGL, 'glTexCoord1dv');
  glTexCoord1fv := GetProcAddress(libGL, 'glTexCoord1fv');
  glTexCoord1iv := GetProcAddress(libGL, 'glTexCoord1iv');
  glTexCoord1sv := GetProcAddress(libGL, 'glTexCoord1sv');
  glTexCoord2dv := GetProcAddress(libGL, 'glTexCoord2dv');
  glTexCoord2fv := GetProcAddress(libGL, 'glTexCoord2fv');
  glTexCoord2iv := GetProcAddress(libGL, 'glTexCoord2iv');
  glTexCoord2sv := GetProcAddress(libGL, 'glTexCoord2sv');
  glTexCoord3dv := GetProcAddress(libGL, 'glTexCoord3dv');
  glTexCoord3fv := GetProcAddress(libGL, 'glTexCoord3fv');
  glTexCoord3iv := GetProcAddress(libGL, 'glTexCoord3iv');
  glTexCoord3sv := GetProcAddress(libGL, 'glTexCoord3sv');
  glTexCoord4dv := GetProcAddress(libGL, 'glTexCoord4dv');
  glTexCoord4fv := GetProcAddress(libGL, 'glTexCoord4fv');
  glTexCoord4iv := GetProcAddress(libGL, 'glTexCoord4iv');
  glTexCoord4sv := GetProcAddress(libGL, 'glTexCoord4sv');
  glRasterPos2d := GetProcAddress(libGL, 'glRasterPos2d');
  glRasterPos2f := GetProcAddress(libGL, 'glRasterPos2f');
  glRasterPos2i := GetProcAddress(libGL, 'glRasterPos2i');
  glRasterPos2s := GetProcAddress(libGL, 'glRasterPos2s');
  glRasterPos3d := GetProcAddress(libGL, 'glRasterPos3d');
  glRasterPos3f := GetProcAddress(libGL, 'glRasterPos3f');
  glRasterPos3i := GetProcAddress(libGL, 'glRasterPos3i');
  glRasterPos3s := GetProcAddress(libGL, 'glRasterPos3s');
  glRasterPos4d := GetProcAddress(libGL, 'glRasterPos4d');
  glRasterPos4f := GetProcAddress(libGL, 'glRasterPos4f');
  glRasterPos4i := GetProcAddress(libGL, 'glRasterPos4i');
  glRasterPos4s := GetProcAddress(libGL, 'glRasterPos4s');
  glRasterPos2dv := GetProcAddress(libGL, 'glRasterPos2dv');
  glRasterPos2fv := GetProcAddress(libGL, 'glRasterPos2fv');
  glRasterPos2iv := GetProcAddress(libGL, 'glRasterPos2iv');
  glRasterPos2sv := GetProcAddress(libGL, 'glRasterPos2sv');
  glRasterPos3dv := GetProcAddress(libGL, 'glRasterPos3dv');
  glRasterPos3fv := GetProcAddress(libGL, 'glRasterPos3fv');
  glRasterPos3iv := GetProcAddress(libGL, 'glRasterPos3iv');
  glRasterPos3sv := GetProcAddress(libGL, 'glRasterPos3sv');
  glRasterPos4dv := GetProcAddress(libGL, 'glRasterPos4dv');
  glRasterPos4fv := GetProcAddress(libGL, 'glRasterPos4fv');
  glRasterPos4iv := GetProcAddress(libGL, 'glRasterPos4iv');
  glRasterPos4sv := GetProcAddress(libGL, 'glRasterPos4sv');
  glRectd := GetProcAddress(libGL, 'glRectd');
  glRectf := GetProcAddress(libGL, 'glRectf');
  glRecti := GetProcAddress(libGL, 'glRecti');
  glRects := GetProcAddress(libGL, 'glRects');
  glRectdv := GetProcAddress(libGL, 'glRectdv');
  glRectfv := GetProcAddress(libGL, 'glRectfv');
  glRectiv := GetProcAddress(libGL, 'glRectiv');
  glRectsv := GetProcAddress(libGL, 'glRectsv');
  glVertexPointer := GetProcAddress(libGL, 'glVertexPointer');
  glNormalPointer := GetProcAddress(libGL, 'glNormalPointer');
  glColorPointer := GetProcAddress(libGL, 'glColorPointer');
  glIndexPointer := GetProcAddress(libGL, 'glIndexPointer');
  glTexCoordPointer := GetProcAddress(libGL, 'glTexCoordPointer');
  glEdgeFlagPointer := GetProcAddress(libGL, 'glEdgeFlagPointer');
  glGetPointerv := GetProcAddress(libGL, 'glGetPointerv');
  glArrayElement := GetProcAddress(libGL, 'glArrayElement');
  glDrawArrays := GetProcAddress(libGL, 'glDrawArrays');
  glDrawElements := GetProcAddress(libGL, 'glDrawElements');
  glInterleavedArrays := GetProcAddress(libGL, 'glInterleavedArrays');
  glShadeModel := GetProcAddress(libGL, 'glShadeModel');
  glLightf := GetProcAddress(libGL, 'glLightf');
  glLighti := GetProcAddress(libGL, 'glLighti');
  glLightfv := GetProcAddress(libGL, 'glLightfv');
  glLightiv := GetProcAddress(libGL, 'glLightiv');
  glGetLightfv := GetProcAddress(libGL, 'glGetLightfv');
  glGetLightiv := GetProcAddress(libGL, 'glGetLightiv');
  glLightModelf := GetProcAddress(libGL, 'glLightModelf');
  glLightModeli := GetProcAddress(libGL, 'glLightModeli');
  glLightModelfv := GetProcAddress(libGL, 'glLightModelfv');
  glLightModeliv := GetProcAddress(libGL, 'glLightModeliv');
  glMaterialf := GetProcAddress(libGL, 'glMaterialf');
  glMateriali := GetProcAddress(libGL, 'glMateriali');
  glMaterialfv := GetProcAddress(libGL, 'glMaterialfv');
  glMaterialiv := GetProcAddress(libGL, 'glMaterialiv');
  glGetMaterialfv := GetProcAddress(libGL, 'glGetMaterialfv');
  glGetMaterialiv := GetProcAddress(libGL, 'glGetMaterialiv');
  glColorMaterial := GetProcAddress(libGL, 'glColorMaterial');
  glPixelZoom := GetProcAddress(libGL, 'glPixelZoom');
  glPixelStoref := GetProcAddress(libGL, 'glPixelStoref');
  glPixelStorei := GetProcAddress(libGL, 'glPixelStorei');
  glPixelTransferf := GetProcAddress(libGL, 'glPixelTransferf');
  glPixelTransferi := GetProcAddress(libGL, 'glPixelTransferi');
  glPixelMapfv := GetProcAddress(libGL, 'glPixelMapfv');
  glPixelMapuiv := GetProcAddress(libGL, 'glPixelMapuiv');
  glPixelMapusv := GetProcAddress(libGL, 'glPixelMapusv');
  glGetPixelMapfv := GetProcAddress(libGL, 'glGetPixelMapfv');
  glGetPixelMapuiv := GetProcAddress(libGL, 'glGetPixelMapuiv');
  glGetPixelMapusv := GetProcAddress(libGL, 'glGetPixelMapusv');
  glBitmap := GetProcAddress(libGL, 'glBitmap');
  glReadPixels := GetProcAddress(libGL, 'glReadPixels');
  glDrawPixels := GetProcAddress(libGL, 'glDrawPixels');
  glCopyPixels := GetProcAddress(libGL, 'glCopyPixels');
  glStencilFunc := GetProcAddress(libGL, 'glStencilFunc');
  glStencilMask := GetProcAddress(libGL, 'glStencilMask');
  glStencilOp := GetProcAddress(libGL, 'glStencilOp');
  glClearStencil := GetProcAddress(libGL, 'glClearStencil');
  glTexGend := GetProcAddress(libGL, 'glTexGend');
  glTexGenf := GetProcAddress(libGL, 'glTexGenf');
  glTexGeni := GetProcAddress(libGL, 'glTexGeni');
  glTexGendv := GetProcAddress(libGL, 'glTexGendv');
  glTexGenfv := GetProcAddress(libGL, 'glTexGenfv');
  glTexGeniv := GetProcAddress(libGL, 'glTexGeniv');
  glGetTexGendv := GetProcAddress(libGL, 'glGetTexGendv');
  glGetTexGenfv := GetProcAddress(libGL, 'glGetTexGenfv');
  glGetTexGeniv := GetProcAddress(libGL, 'glGetTexGeniv');
  glTexEnvf := GetProcAddress(libGL, 'glTexEnvf');
  glTexEnvi := GetProcAddress(libGL, 'glTexEnvi');
  glTexEnvfv := GetProcAddress(libGL, 'glTexEnvfv');
  glTexEnviv := GetProcAddress(libGL, 'glTexEnviv');
  glGetTexEnvfv := GetProcAddress(libGL, 'glGetTexEnvfv');
  glGetTexEnviv := GetProcAddress(libGL, 'glGetTexEnviv');
  glTexParameterf := GetProcAddress(libGL, 'glTexParameterf');
  glTexParameteri := GetProcAddress(libGL, 'glTexParameteri');
  glTexParameterfv := GetProcAddress(libGL, 'glTexParameterfv');
  glTexParameteriv := GetProcAddress(libGL, 'glTexParameteriv');
  glGetTexParameterfv := GetProcAddress(libGL, 'glGetTexParameterfv');
  glGetTexParameteriv := GetProcAddress(libGL, 'glGetTexParameteriv');
  glGetTexLevelParameterfv := GetProcAddress(libGL, 'glGetTexLevelParameterfv');
  glGetTexLevelParameteriv := GetProcAddress(libGL, 'glGetTexLevelParameteriv');
  glTexImage1D := GetProcAddress(libGL, 'glTexImage1D');
  glTexImage2D := GetProcAddress(libGL, 'glTexImage2D');
  glGetTexImage := GetProcAddress(libGL, 'glGetTexImage');
  glGenTextures := GetProcAddress(libGL, 'glGenTextures');
  glDeleteTextures := GetProcAddress(libGL, 'glDeleteTextures');
  glBindTexture := GetProcAddress(libGL, 'glBindTexture');
  glPrioritizeTextures := GetProcAddress(libGL, 'glPrioritizeTextures');
  glAreTexturesResident := GetProcAddress(libGL, 'glAreTexturesResident');
  glIsTexture := GetProcAddress(libGL, 'glIsTexture');
  glTexSubImage1D := GetProcAddress(libGL, 'glTexSubImage1D');
  glTexSubImage2D := GetProcAddress(libGL, 'glTexSubImage2D');
  glCopyTexImage1D := GetProcAddress(libGL, 'glCopyTexImage1D');
  glCopyTexImage2D := GetProcAddress(libGL, 'glCopyTexImage2D');
  glCopyTexSubImage1D := GetProcAddress(libGL, 'glCopyTexSubImage1D');
  glCopyTexSubImage2D := GetProcAddress(libGL, 'glCopyTexSubImage2D');
  glMap1d := GetProcAddress(libGL, 'glMap1d');
  glMap1f := GetProcAddress(libGL, 'glMap1f');
  glMap2d := GetProcAddress(libGL, 'glMap2d');
  glMap2f := GetProcAddress(libGL, 'glMap2f');
  glGetMapdv := GetProcAddress(libGL, 'glGetMapdv');
  glGetMapfv := GetProcAddress(libGL, 'glGetMapfv');
  glGetMapiv := GetProcAddress(libGL, 'glGetMapiv');
  glEvalCoord1d := GetProcAddress(libGL, 'glEvalCoord1d');
  glEvalCoord1f := GetProcAddress(libGL, 'glEvalCoord1f');
  glEvalCoord1dv := GetProcAddress(libGL, 'glEvalCoord1dv');
  glEvalCoord1fv := GetProcAddress(libGL, 'glEvalCoord1fv');
  glEvalCoord2d := GetProcAddress(libGL, 'glEvalCoord2d');
  glEvalCoord2f := GetProcAddress(libGL, 'glEvalCoord2f');
  glEvalCoord2dv := GetProcAddress(libGL, 'glEvalCoord2dv');
  glEvalCoord2fv := GetProcAddress(libGL, 'glEvalCoord2fv');
  glMapGrid1d := GetProcAddress(libGL, 'glMapGrid1d');
  glMapGrid1f := GetProcAddress(libGL, 'glMapGrid1f');
  glMapGrid2d := GetProcAddress(libGL, 'glMapGrid2d');
  glMapGrid2f := GetProcAddress(libGL, 'glMapGrid2f');
  glEvalPoint1 := GetProcAddress(libGL, 'glEvalPoint1');
  glEvalPoint2 := GetProcAddress(libGL, 'glEvalPoint2');
  glEvalMesh1 := GetProcAddress(libGL, 'glEvalMesh1');
  glEvalMesh2 := GetProcAddress(libGL, 'glEvalMesh2');
  glFogf := GetProcAddress(libGL, 'glFogf');
  glFogi := GetProcAddress(libGL, 'glFogi');
  glFogfv := GetProcAddress(libGL, 'glFogfv');
  glFogiv := GetProcAddress(libGL, 'glFogiv');
  glFeedbackBuffer := GetProcAddress(libGL, 'glFeedbackBuffer');
  glPassThrough := GetProcAddress(libGL, 'glPassThrough');
  glSelectBuffer := GetProcAddress(libGL, 'glSelectBuffer');
  glInitNames := GetProcAddress(libGL, 'glInitNames');
  glLoadName := GetProcAddress(libGL, 'glLoadName');
  glPushName := GetProcAddress(libGL, 'glPushName');
  glPopName := GetProcAddress(libGL, 'glPopName');

  GLInitialized := True;
  Result := True;
end;

function InitGLUFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGLU := LoadLibrary(libname);
  if libGLU = 0 then exit;

  gluLookAt := GetProcAddress(libGLU, 'gluLookAt');
  gluOrtho2D := GetProcAddress(libGLU, 'gluOrtho2D');
  gluPerspective := GetProcAddress(libGLU, 'gluPerspective');
  gluPickMatrix := GetProcAddress(libGLU, 'gluPickMatrix');
  gluProject := GetProcAddress(libGLU, 'gluProject');
  gluUnProject := GetProcAddress(libGLU, 'gluUnProject');
  gluErrorString := GetProcAddress(libGLU, 'gluErrorString');
  gluScaleImage := GetProcAddress(libGLU, 'gluScaleImage');
  gluBuild1DMipmaps := GetProcAddress(libGLU, 'gluBuild1DMipmaps');
  gluBuild2DMipmaps := GetProcAddress(libGLU, 'gluBuild2DMipmaps');
  gluNewQuadric := GetProcAddress(libGLU, 'gluNewQuadric');
  gluDeleteQuadric := GetProcAddress(libGLU, 'gluDeleteQuadric');
  gluQuadricDrawStyle := GetProcAddress(libGLU, 'gluQuadricDrawStyle');
  gluQuadricOrientation := GetProcAddress(libGLU, 'gluQuadricOrientation');
  gluQuadricNormals := GetProcAddress(libGLU, 'gluQuadricNormals');
  gluQuadricTexture := GetProcAddress(libGLU, 'gluQuadricTexture');
  gluQuadricCallback := GetProcAddress(libGLU, 'gluQuadricCallback');
  gluCylinder := GetProcAddress(libGLU, 'gluCylinder');
  gluSphere := GetProcAddress(libGLU, 'gluSphere');
  gluDisk := GetProcAddress(libGLU, 'gluDisk');
  gluPartialDisk := GetProcAddress(libGLU, 'gluPartialDisk');
  gluNewNurbsRenderer := GetProcAddress(libGLU, 'gluNewNurbsRenderer');
  gluDeleteNurbsRenderer := GetProcAddress(libGLU, 'gluDeleteNurbsRenderer');
  gluLoadSamplingMatrices := GetProcAddress(libGLU, 'gluLoadSamplingMatrices');
  gluNurbsProperty := GetProcAddress(libGLU, 'gluNurbsProperty');
  gluGetNurbsProperty := GetProcAddress(libGLU, 'gluGetNurbsProperty');
  gluBeginCurve := GetProcAddress(libGLU, 'gluBeginCurve');
  gluEndCurve := GetProcAddress(libGLU, 'gluEndCurve');
  gluNurbsCurve := GetProcAddress(libGLU, 'gluNurbsCurve');
  gluBeginSurface := GetProcAddress(libGLU, 'gluBeginSurface');
  gluEndSurface := GetProcAddress(libGLU, 'gluEndSurface');
  gluNurbsSurface := GetProcAddress(libGLU, 'gluNurbsSurface');
  gluBeginTrim := GetProcAddress(libGLU, 'gluBeginTrim');
  gluEndTrim := GetProcAddress(libGLU, 'gluEndTrim');
  gluPwlCurve := GetProcAddress(libGLU, 'gluPwlCurve');
  gluNurbsCallback := GetProcAddress(libGLU, 'gluNurbsCallback');
  gluNewTess := GetProcAddress(libGLU, 'gluNewTess');
  gluTessCallback := GetProcAddress(libGLU, 'gluTessCallback');
  gluDeleteTess := GetProcAddress(libGLU, 'gluDeleteTess');
  gluBeginPolygon := GetProcAddress(libGLU, 'gluBeginPolygon');
  gluEndPolygon := GetProcAddress(libGLU, 'gluEndPolygon');
  gluNextContour := GetProcAddress(libGLU, 'gluNextContour');
  gluTessVertex := GetProcAddress(libGLU, 'gluTessVertex');
  gluGetString := GetProcAddress(libGLU, 'gluGetString');

  GLUInitialized := True;
  Result := True;
end;


{$IFDEF HasGLX}

function InitGLX: Boolean;
begin
  Result := False;
  if libGL = 0 then exit;
  
  glXQueryVersion := GetProcAddress(libGL, 'glXQueryVersion');
  if @glXQueryVersion = nil then exit;

  glXChooseVisual := GetProcAddress(libGL, 'glXChooseVisual');
  glXCreateContext := GetProcAddress(libGL, 'glXCreateContext');
  glXDestroyContext := GetProcAddress(libGL, 'glXDestroyContext');
  glXMakeCurrent := GetProcAddress(libGL, 'glXMakeCurrent');
  glXCopyContext := GetProcAddress(libGL, 'glXCopyContext');
  glXSwapBuffers := GetProcAddress(libGL, 'glXSwapBuffers');
  glXCreateGLXPixmap := GetProcAddress(libGL, 'glXCreateGLXPixmap');
  glXDestroyGLXPixmap := GetProcAddress(libGL, 'glXDestroyGLXPixmap');
  glXQueryExtension := GetProcAddress(libGL, 'glXQueryExtension');

  glXIsDirect := GetProcAddress(libGL, 'glXIsDirect');
  glXGetConfig := GetProcAddress(libGL, 'glXGetConfig');
  glXGetCurrentContext := GetProcAddress(libGL, 'glXGetCurrentContext');
  glXGetCurrentDrawable := GetProcAddress(libGL, 'glXGetCurrentDrawable');
  glXWaitGL := GetProcAddress(libGL, 'glXWaitGL');
  glXWaitX := GetProcAddress(libGL, 'glXWaitX');
  glXUseXFont := GetProcAddress(libGL, 'glXUseXFont');
  glXQueryExtensionsString := GetProcAddress(libGL, 'glXQueryExtensionsString');
  glXQueryServerString := GetProcAddress(libGL, 'glXQueryServerString');
  glXGetClientString := GetProcAddress(libGL, 'glXGetClientString');
  glXCreateGLXPixmapMESA := GetProcAddress(libGL, 'glXCreateGLXPixmapMESA');
  glXReleaseBufferMESA := GetProcAddress(libGL, 'glXReleaseBufferMESA');
  glXCopySubBufferMESA := GetProcAddress(libGL, 'glXCopySubBufferMESA');
  glXGetVideoSyncSGI := GetProcAddress(libGL, 'glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := GetProcAddress(libGL, 'glXWaitVideoSyncSGI');

  GLXInitialized := True;
  Result := True;
end;

{$ENDIF  IFDEF HasGLX}


function InitGL: Boolean;
begin
{$IFDEF Win32}
  Result := InitGLFromLibrary('opengl32.dll');
{$ELSE}
  {$IFDEF Linux}
  Result := InitGLFromLibrary('libGL.so') or InitGLFromLibrary('libMesaGL.so');
  {$ELSE}
    {$ERROR Unsupported platform}
  {$ENDIF}
{$ENDIF}
end;

function InitGLU: Boolean;
begin
{$IFDEF Win32}
  Result := InitGLUFromLibrary('glu32.dll');
{$ELSE}
  {$IFDEF Linux}
  Result := InitGLUFromLibrary('libGLU.so') or InitGLUFromLibrary('libMesaGLU.so');
  {$ELSE}
    {$ERROR Unsupported platform}
  {$ENDIF}
{$ENDIF}
end;



finalization
  if libGL <> 0 then FreeLibrary(libGL);
  if libGLU <> 0 then FreeLibrary(libGLU);
end.


{
  $Log$
  Revision 1.1  1999-11-28 17:55:23  sg
  * Added new unit generation tools and auto-generated GL units for Linux

  Revision 1.1  1999/11/10 14:15:33  sg
  * Added to CVS

}
