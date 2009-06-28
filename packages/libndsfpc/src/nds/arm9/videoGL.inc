(*
  $Id: videoGL.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)


{$ifndef ARM9}
  {$error 3D hardware is only available from the ARM9}
{$endif}


{$ifdef NDS_INTERFACE}
(*---------------------------------------------------------------------------------
	lut resolution for trig functions
	(must be power of two and must be the same as LUT resolution)
	in other words dont change unless you also change your LUTs
---------------------------------------------------------------------------------*)
const
  LUT_SIZE = (1 shl 15);
  LUT_MASK = ((1 shl 15) - 1);

type
  GLuint = cuint32;
  GLfloat = cfloat;

const
  MAX_TEXTURES = 2048;  //this should be enough ! but feel free to change

//////////////////////////////////////////////////////////////////////
// Fixed point / floating point / integer conversion macros
//////////////////////////////////////////////////////////////////////
type
  fixed12d3 = cuint16 ;

function int_to_12d3(n: cint): fixed12d3; inline;
function float_to_12d3(n: cfloat): fixed12d3; inline;

const
  GL_MAX_DEPTH     = $7FFF;

function inttof32(n: cint): cint32; inline;
function f32toint(n: cint32): cint; inline;
function floattof32(n: cfloat): cint32; inline;
function f32tofloat(n: cint32): cfloat; inline;

type
  t16 = cshort;//cint16?       // text coordinate 12.4 fixed point (or cint?)

function f32tot16(n: cint32): t16; inline;
function inttot16(n: cint): cint32; inline;    //???
function t16toint(n: t16): cint; inline;
function floattot16(n: cfloat): t16; inline;
function TEXTURE_PACK(u, v: cint): cint; inline;
//function TEXTURE_PACK(u, v: cshort): cuint; inline;

type
  v16 = cuint16; //cshort?       // vertex 4.12 fixed format

function inttov16(n: cint): cint{v16}; inline;
function f32tov16(n: cint32): v16; inline; 
function v16toint(n: v16): cint; inline;
function floattov16(n: cfloat): v16; inline;
function VERTEX_PACK(x,y: cint): cint; inline;

type
  v10 = cint16;

function inttov10(n: cint): cint; inline;
function f32tov10(n: cint32): cint; inline;
function v10toint(n: v10): cint; inline;
function floattov10(n: cfloat): v10; inline;
function NORMAL_PACK(x,y,z: cint): cint; inline;

type
  rgb = cushort;

  m3x3 = record
    m: array [0..8] of cint32;
  end;
  pm3x3 = ^m3x3;

  m4x4 = record
    m: array [0..15] of cint32;
  end; 
  pm4x4 = ^m4x4;

  m4x3 = record
    m: array [0..11] of cint32;
  end;
  pm4x3 = ^m4x3;

  GLvector = record
    x,y,z: cint32;
  end;
  PGLvector= ^GLvector;

const
  GL_FALSE    = 0;
  GL_TRUE     = 1;

//////////////////////////////////////////////////////////////////////
// glBegin constants
//////////////////////////////////////////////////////////////////////
type
  GL_GLBEGIN_ENUM = cint32;
const
  GL_TRIANGLES      : GL_GLBEGIN_ENUM = 0;
  GL_QUADS          : GL_GLBEGIN_ENUM = 1;
  GL_TRIANGLE_STRIP : GL_GLBEGIN_ENUM = 2;
  GL_QUAD_STRIP     : GL_GLBEGIN_ENUM = 3;
  GL_TRIANGLE       : GL_GLBEGIN_ENUM = 0;
  GL_QUAD           : GL_GLBEGIN_ENUM = 1;

//////////////////////////////////////////////////////////////////////
// glMatrixMode constants
//////////////////////////////////////////////////////////////////////
type
  GL_MATRIX_MODE_ENUM = cint32;
const
  GL_PROJECTION      : GL_MATRIX_MODE_ENUM = 0;
  GL_POSITION        : GL_MATRIX_MODE_ENUM = 1;
  GL_MODELVIEW       : GL_MATRIX_MODE_ENUM = 2;
  GL_TEXTURE         : GL_MATRIX_MODE_ENUM = 3;

//////////////////////////////////////////////////////////////////////
// glMaterialf constants
//////////////////////////////////////////////////////////////////////
type
  GL_MATERIALS_ENUM = cint32;
const
  GL_AMBIENT              : GL_MATERIALS_ENUM = $01;
  GL_DIFFUSE              : GL_MATERIALS_ENUM = $02;
  GL_AMBIENT_AND_DIFFUSE  : GL_MATERIALS_ENUM = $03;
  GL_SPECULAR             : GL_MATERIALS_ENUM = $04;
  GL_SHININESS            : GL_MATERIALS_ENUM = $08;
  GL_EMISSION             : GL_MATERIALS_ENUM = $10;

////////////////////////////////////////////////////////////
// glPolyFmt constants
////////////////////////////////////////////////////////////
type
  GL_POLY_FORMAT_ENUM = cint32;
const
  POLY_FORMAT_LIGHT0   : GL_POLY_FORMAT_ENUM = (1 shl 0);
  POLY_FORMAT_LIGHT1   : GL_POLY_FORMAT_ENUM = (1 shl 1);
  POLY_FORMAT_LIGHT2   : GL_POLY_FORMAT_ENUM = (1 shl 2);
  POLY_FORMAT_LIGHT3   : GL_POLY_FORMAT_ENUM = (1 shl 3);
  POLY_MODULATION      : GL_POLY_FORMAT_ENUM = (0 shl 4);
  POLY_DECAL           : GL_POLY_FORMAT_ENUM = (1 shl 4);
  POLY_TOON_HIGHLIGHT  : GL_POLY_FORMAT_ENUM = (2 shl 4);
  POLY_SHADOW          : GL_POLY_FORMAT_ENUM = (3 shl 4);
  POLY_CULL_FRONT      : GL_POLY_FORMAT_ENUM = (1 shl 6);
  POLY_CULL_BACK       : GL_POLY_FORMAT_ENUM = (2 shl 6);
  POLY_CULL_NONE       : GL_POLY_FORMAT_ENUM = (3 shl 6);
  POLY_FOG             : GL_POLY_FORMAT_ENUM = (1 shl 15);




////////////////////////////////////////////////////////////
// glTexImage2d constants
////////////////////////////////////////////////////////////
type 
  GL_TEXTURE_SIZE_ENUM = cint32;
const
	TEXTURE_SIZE_8    : GL_TEXTURE_SIZE_ENUM = 0;
	TEXTURE_SIZE_16   : GL_TEXTURE_SIZE_ENUM = 1;
	TEXTURE_SIZE_32   : GL_TEXTURE_SIZE_ENUM = 2;
	TEXTURE_SIZE_64   : GL_TEXTURE_SIZE_ENUM = 3;
	TEXTURE_SIZE_128  : GL_TEXTURE_SIZE_ENUM = 4;
	TEXTURE_SIZE_256  : GL_TEXTURE_SIZE_ENUM = 5;
	TEXTURE_SIZE_512  : GL_TEXTURE_SIZE_ENUM = 6;
	TEXTURE_SIZE_1024 : GL_TEXTURE_SIZE_ENUM = 7;

type
  GL_TEXTURE_PARAM_ENUM = cuint32;
const
	GL_TEXTURE_WRAP_S : GL_TEXTURE_PARAM_ENUM = (1 shl 16);
	GL_TEXTURE_WRAP_T : GL_TEXTURE_PARAM_ENUM = (1 shl 17);
	GL_TEXTURE_FLIP_S : GL_TEXTURE_PARAM_ENUM = (1 shl 18);
	GL_TEXTURE_FLIP_T : GL_TEXTURE_PARAM_ENUM = (1 shl 19);
	GL_TEXTURE_COLOR0_TRANSPARENT : GL_TEXTURE_PARAM_ENUM = (1 shl 29);
	GL_TEXTURE_ALPHA_MASK : GL_TEXTURE_PARAM_ENUM = (1 shl 29);
	TEXGEN_OFF      : GL_TEXTURE_PARAM_ENUM = (0 shl 30);
	TEXGEN_TEXCOORD : GL_TEXTURE_PARAM_ENUM = (1 shl 30);
	TEXGEN_NORMAL   : GL_TEXTURE_PARAM_ENUM = (2 shl 30);
	TEXGEN_POSITION : GL_TEXTURE_PARAM_ENUM = (3 shl 30);


type
  GL_TEXTURE_TYPE_ENUM = cint32;
const
	GL_RGB32_A3   : GL_TEXTURE_TYPE_ENUM = 1;
	GL_RGB4       : GL_TEXTURE_TYPE_ENUM = 2;
	GL_RGB16      : GL_TEXTURE_TYPE_ENUM = 3;
	GL_RGB256     : GL_TEXTURE_TYPE_ENUM = 4;
	GL_COMPRESSED : GL_TEXTURE_TYPE_ENUM = 5;
	GL_RGB8_A5    : GL_TEXTURE_TYPE_ENUM = 6;
	GL_RGBA       : GL_TEXTURE_TYPE_ENUM = 7;
	GL_RGB        : GL_TEXTURE_TYPE_ENUM = 8;



type
  DISP3DCNT_ENUM = cint32;
const
	GL_TEXTURE_2D      : DISP3DCNT_ENUM = (1 shl 0);
	GL_TOON_HIGHLIGHT  : DISP3DCNT_ENUM = (1 shl 1);
	GL_ALPHA_TEST      : DISP3DCNT_ENUM = (1 shl 2);
	GL_BLEND           : DISP3DCNT_ENUM = (1 shl 3);
	GL_ANTIALIAS       : DISP3DCNT_ENUM = (1 shl 4);
	GL_OUTLINE         : DISP3DCNT_ENUM = (1 shl 5);
  GL_FOG_ONLY_ALPHA  : DISP3DCNT_ENUM = (1 shl 6);
  GL_FOG             : DISP3DCNT_ENUM = (1 shl 7);
	GL_COLOR_UNDERFLOW : DISP3DCNT_ENUM = (1 shl 12);
	GL_POLY_OVERFLOW   : DISP3DCNT_ENUM = (1 shl 13);
	GL_CLEAR_BMP       : DISP3DCNT_ENUM = (1 shl 14);

type
  GL_GET_ENUM = (
    GL_GET_VERTEX_RAM_COUNT   = 0,
    GL_GET_POLYGON_RAM_COUNT  = 1,
    GL_GET_MATRIX_VECTOR      = 2,
    GL_GET_MATRIX_POSITION    = 3,
    GL_GET_MATRIX_PROJECTION  = 4,
    GL_GET_MATRIX_CLIP        = 5,
    GL_GET_TEXTURE_WIDTH      = 6,
    GL_GET_TEXTURE_HEIGHT     = 7
  );

type
	GLFLUSH_ENUM = cint32;
const
	GL_TRANS_MANUALSORT: GLFLUSH_ENUM = (1 shl 0);
	GL_WBUFFERING      : GLFLUSH_ENUM = (1 shl 1); 

type
  gl_hidden_globals = record
    matrixMode: GL_MATRIX_MODE_ENUM; // holds the current Matrix Mode
    // holds the current state of the clear color register
    clearColor: cuint32; // state of clear color register
    // texture globals
    textures: array [0..MAX_TEXTURES-1] of cuint32;
    texturePtrs: DynamicArray;
    activeTexture: cuint32;
    nextBlock: pcuint32;
    nextPBlock: cuint32;
    nameCount: cint;
  end;
  TGLHiddenGlobals = gl_hidden_globals;
  PGLHiddenGlobals = ^gl_hidden_globals;

var
  glGlobalData: gl_hidden_globals; cvar; external;
  // Pointer to global data for videoGL
  glGlob: PGLHiddenGlobals = @glGlobalData;

type 
  TArr4ofInt = array [0..3] of cint32;

function FIFO_COMMAND_PACK(c1,c2,c3,c4: cint): cint; inline;
function REG2ID(r: pcuint32): cuint8; inline;
function FIFO_NOP(): cuint8; inline;
function FIFO_STATUS(): cuint8; inline;
function FIFO_COLOR(): cuint8; inline;
function FIFO_VERTEX16(): cuint8; inline;
function FIFO_TEX_COORD(): cuint8; inline;
function FIFO_TEX_FORMAT(): cuint8; inline;
function FIFO_PAL_FORMAT(): cuint8; inline;
function FIFO_CLEAR_COLOR(): cuint8; inline;
function FIFO_CLEAR_DEPTH(): cuint8; inline;
function FIFO_LIGHT_VECTOR(): cuint8; inline;
function FIFO_LIGHT_COLOR(): cuint8; inline;
function FIFO_NORMAL(): cuint8; inline;
function FIFO_DIFFUSE_AMBIENT(): cuint8; inline;
function FIFO_SPECULAR_EMISSION(): cuint8; inline;
function FIFO_SHININESS(): cuint8; inline;
function FIFO_POLY_FORMAT(): cuint8; inline;
function FIFO_BEGIN(): cuint8; inline;
function FIFO_END(): cuint8; inline;
function FIFO_FLUSH(): cuint8; inline;
function FIFO_VIEWPORT(): cuint8; inline;

procedure glRotatef32i(angle: cint; x, y, z: cint32); cdecl; external;
function glTexImage2D(target, empty1: cint; _type: GL_TEXTURE_TYPE_ENUM; sizeX, sizeY, empty2, param: cint; texture: pcuint8): cint; cdecl; external;
procedure glTexLoadPal(const pal: pcuint16; count: cuint16; addr: cuint32); cdecl; external;
function gluTexLoadPal(const pal: pcuint16; count: cuint16; format: cuint8): cint; cdecl; external;
procedure glTexParameter(sizeX, sizeY: cuint8; const addr: pcuint32; mode: GL_TEXTURE_TYPE_ENUM; param: GL_TEXTURE_PARAM_ENUM); cdecl; external;
function glGetTexParameter(): cuint32; cdecl; external;
function glGetTexturePointer(name: cint): pointer; cdecl; external;
procedure glBindTexture(target, name: cint); cdecl; external;
procedure glColorTable(format: cuint8; addr: cuint32); cdecl; external;
function glGenTextures(n: cint; names: pcint): cint; cdecl; external;
procedure glResetTextures(); cdecl; external;
procedure glTexCoord2f32(u, v: cint32); cdecl; external;
procedure glMaterialf(mode: GL_MATERIALS_ENUM; color: rgb); cdecl; external;
procedure glInit_C(); cdecl; external;
function glGetGlobals(): PGLHiddenGlobals; cdecl; external;

function POLY_ALPHA(n: cint): cuint32; inline;
function POLY_ID(n: cint): cuint32; inline;	
procedure glBegin(mode: GL_GLBEGIN_ENUM); inline;
procedure glEnd(); inline;
procedure glClearDepth(depth: fixed12d3); inline;
procedure glColor3b(red, green, blue: cuint8); inline;
procedure glColor(color: rgb); inline;
procedure glVertex3v16(x, y, z: v16); inline;
procedure glTexCoord2t16(u, v: t16); inline;
procedure glPushMatrix(); inline;
procedure glPopMatrix(num: cint32); inline;
procedure glRestoreMatrix(index: cint32); inline;
procedure glStoreMatrix(index: cint32); inline;
procedure glScalev(const v: PGLvector); inline;
procedure glTranslatev(const v: PGLvector) ; inline;
procedure glTranslate3f32(x, y, z: cint32); inline;
procedure glScalef32(factor: cint32); inline;
procedure glLight(id: cint; color: rgb; x, y, z: v10); inline;
procedure glNormal(normal: cuint32); inline;
procedure glLoadIdentity(); inline;
procedure glMatrixMode(mode: GL_MATRIX_MODE_ENUM); inline;
procedure glViewport(x1, y1, x2, y2: cuint8); inline;
procedure glFlush(mode: cuint32); inline;
procedure glMaterialShinyness(); inline;
procedure glCallList(list: pcuint32); inline;
procedure glPolyFmt(params: cuint32); inline;
procedure glEnable(bits: cint); inline;
procedure glDisable(bits: cint); inline;
procedure glLoadMatrix4x4(const m: pm4x4); inline;
procedure glLoadMatrix4x3(const m: pm4x3); inline;
procedure glMultMatrix4x4(const m: pm4x4); inline;
procedure glMultMatrix4x3(const m: pm4x3); inline;
procedure glMultMatrix3x3(const m: pm3x3); inline;
procedure glRotateXi(angle: cint); inline;
procedure glRotateYi(angle: cint); inline;
procedure glRotateZi(angle: cint); inline;
procedure glOrthof32(left, right, bottom, top, zNear, zFar: cint32); inline;
procedure gluLookAtf32(eyex, eyey, eyez, lookAtx, lookAty, lookAtz, upx, upy, upz: cint32); inline;
procedure glFrustumf32(left, right, bottom, top, zNear, zFar: cint32); inline;
procedure gluPerspectivef32(fovy: cint; aspect, zNear, zFar: cint32); inline;
procedure gluPickMatrix(x, y, width, height: cint32; const viewport: TArr4ofInt); inline;
procedure glResetMatrixStack(); inline;
procedure glSetOutlineColor(id: cint; color: rgb); inline;
procedure glSetToonTable(const table: pcuint16); inline;
procedure glSetToonTableRange(_start, _end: cint; color: rgb); inline;
procedure glGetFixed(const param: GL_GET_ENUM; f: pcint32); inline;
procedure glAlphaFunc(alphaThreshold: cint); inline;
procedure glCutoffDepth(wVal: fixed12d3); inline;
procedure glInit(); inline;
procedure glClearColor(red, green, blue, alpha: cuint8); inline;
procedure glClearPolyID(ID: cuint8); inline;
procedure glGetInt(param: GL_GET_ENUM; var i: cint); inline;
procedure glVertex3f(x, y, z: cfloat); inline;
procedure glRotatef32(angle: cfloat; x, y, z: cint32); inline;
procedure glRotatef(angle, x, y, z: cfloat); inline;
procedure glColor3f(r, g, b: cfloat); inline;
procedure glScalef(x, y, z: cfloat); inline;
procedure glTranslatef(x, y, z: cfloat); inline;
procedure glNormal3f(x, y, z: cfloat); inline;
procedure glRotateX(angle: cfloat); inline;
procedure glRotateY(angle: cfloat); inline;
procedure glRotateZ(angle: cfloat); inline;
procedure glOrtho(left, right, bottom, top, zNear, zFar: cfloat); inline;
procedure gluLookAt(eyex, eyey, eyez, lookAtx, lookAty, lookAtz, upx, upy, upz: cfloat); inline;
procedure glFrustum(left, right, bottom, top, znear, zfar: cfloat); inline;
procedure gluPerspective(fovy, aspect, zNear, zFar: cfloat); inline;
procedure glTexCoord2f(s, t: cfloat); inline;
{$endif NDS_INTERFACE}



{$ifdef NDS_IMPLEMENTATION}
function int_to_12d3(n: cint): fixed12d3; inline;
begin
  int_to_12d3 := fixed12d3(n shl 3);
end;

function float_to_12d3(n: cfloat): fixed12d3; inline;
begin
  float_to_12d3 := fixed12d3(trunc( n  * (1 shl 3)));
end;

function inttof32(n: cint): cint32; inline;
begin
  inttof32 := ((n) shl 12);
end;

function f32toint(n: cint32): cint; inline;
begin
  f32toint := ((n) shr 12);
end;

function floattof32(n: cfloat): cint32; inline;
begin
  floattof32 := trunc((n) * (1 shl 12));
end;

// check it!
function f32tofloat(n: cint32): cfloat; inline;
begin
  f32tofloat := cfloat(n * 1.0) / cfloat((1 shl 12) * 1.0);
end;

function f32tot16(n: cint32): t16; inline;
begin
  f32tot16 := t16(n shr 8);
end;

function inttot16(n: cint): cint32; inline;    //???
begin
  inttot16 := (n shl 4);
end;

function t16toint(n: t16): cint; inline;
begin
  t16toint := cint((n) shr 4);
end;

function floattot16(n: cfloat): t16; inline;
begin
  floattot16 := t16(trunc(n * (1 shl 4)));
end;

function TEXTURE_PACK(u, v: cint): cint; inline;
//function TEXTURE_PACK(u, v: cshort): cuint; inline;
begin
  TEXTURE_PACK := (u and $FFFF) or (v shl 16);
end;

function inttov16(n: cint): cint{v16}; inline;
begin
  inttov16 := (n shl 12);
end;

function f32tov16(n: cint32): v16; inline;
begin
  f32tov16 := v16(n);
end;

function v16toint(n: v16): cint; inline;
begin
  v16toint := cint((n) shr 12);
end;

function floattov16(n: cfloat): v16; inline;
begin
  floattov16 := v16(trunc(n * (1 shl 12)));
end;

function VERTEX_PACK(x,y: cint): cint; inline;
begin
  VERTEX_PACK := (x and $FFFF) or (y shl 16);
end;

function inttov10(n: cint): cint; inline;
begin
  inttov10 := ((n) shl 9);
end;

function f32tov10(n: cint32): cint; inline;
begin
  f32tov10 := (n shr 3);
end;

function v10toint(n: v10): cint; inline;
begin
  v10toint := cint((n) shr 9);
end;

function floattov10(n: cfloat): v10; inline;
begin
  if n > 0.998 then 
	  floattov10 := v10($1FF)
	else
	  floattov10 := trunc(n * (1 shl 9));
end;

function NORMAL_PACK(x,y,z: cint): cint; inline;
begin
  NORMAL_PACK := ((x and $3FF) or ((y and $3FF) shl 10) or (z shl 20));
end;

//---------------------------------------------------------------------------------
//Fifo commands
//---------------------------------------------------------------------------------
function FIFO_COMMAND_PACK(c1,c2,c3,c4: cint): cint; inline;
begin
  FIFO_COMMAND_PACK := (((c4) shl 24) or ((c3) shl 16) or ((c2) shl 8) or (c1));
end;

function REG2ID(r: pcuint32): cuint8; inline;
begin
  REG2ID := cuint8((cuint32(r) - $04000400) shr 2);
end;

function FIFO_NOP(): cuint8; inline;
begin
  FIFO_NOP := REG2ID(GFX_FIFO);
end;

function FIFO_STATUS(): cuint8; inline;
begin
  FIFO_STATUS := REG2ID(GFX_STATUS);
end;

function FIFO_COLOR(): cuint8; inline;
begin
  FIFO_COLOR := REG2ID(GFX_COLOR);
end;

function FIFO_VERTEX16(): cuint8; inline;
begin
  FIFO_VERTEX16 := REG2ID(GFX_VERTEX16);
end;

function FIFO_TEX_COORD(): cuint8; inline;
begin
  FIFO_TEX_COORD := REG2ID(GFX_TEX_COORD);
end;

function FIFO_TEX_FORMAT(): cuint8; inline;
begin
  FIFO_TEX_FORMAT := REG2ID(GFX_TEX_FORMAT);
end;

function FIFO_PAL_FORMAT(): cuint8; inline;
begin
  FIFO_PAL_FORMAT := REG2ID(GFX_PAL_FORMAT);
end;

function FIFO_CLEAR_COLOR(): cuint8; inline;
begin
  FIFO_CLEAR_COLOR := REG2ID(GFX_CLEAR_COLOR);
end;

function FIFO_CLEAR_DEPTH(): cuint8; inline;
begin
  FIFO_CLEAR_DEPTH := REG2ID(pcuint32(GFX_CLEAR_DEPTH));
end;

function FIFO_LIGHT_VECTOR(): cuint8; inline;
begin
  FIFO_LIGHT_VECTOR := REG2ID(GFX_LIGHT_VECTOR);
end;

function FIFO_LIGHT_COLOR(): cuint8; inline;
begin
  FIFO_LIGHT_COLOR := REG2ID(GFX_LIGHT_COLOR);
end;

function FIFO_NORMAL(): cuint8; inline;
begin
  FIFO_NORMAL := REG2ID(GFX_NORMAL);
end;

function FIFO_DIFFUSE_AMBIENT(): cuint8; inline;
begin
  FIFO_DIFFUSE_AMBIENT := REG2ID(GFX_DIFFUSE_AMBIENT);
end;

function FIFO_SPECULAR_EMISSION(): cuint8; inline;
begin
  FIFO_SPECULAR_EMISSION := REG2ID(GFX_SPECULAR_EMISSION);
end;

function FIFO_SHININESS(): cuint8; inline;
begin
  FIFO_SHININESS := REG2ID(GFX_SHININESS);
end;

function FIFO_POLY_FORMAT(): cuint8; inline;
begin
  FIFO_POLY_FORMAT := REG2ID(GFX_POLY_FORMAT);
end;

function FIFO_BEGIN(): cuint8; inline;
begin
  FIFO_BEGIN := REG2ID(GFX_BEGIN);
end;

function FIFO_END(): cuint8; inline;
begin
  FIFO_END := REG2ID(GFX_END);
end;

function FIFO_FLUSH(): cuint8; inline;
begin
  FIFO_FLUSH := REG2ID(GFX_FLUSH);
end;

function FIFO_VIEWPORT(): cuint8; inline;
begin
  FIFO_VIEWPORT := REG2ID(GFX_VIEWPORT);
end;

{ $define FIFO_NOP               := REG2ID(GFX_FIFO)}
{ $define FIFO_STATUS            := REG2ID(GFX_STATUS)}
{ $define FIFO_COLOR             := REG2ID(GFX_COLOR)}

{ $define FIFO_VERTEX16          := REG2ID(GFX_VERTEX16)}
{ $define FIFO_TEX_COORD         := REG2ID(GFX_TEX_COORD)}
{ $define FIFO_TEX_FORMAT        := REG2ID(GFX_TEX_FORMAT)}
{ $define FIFO_PAL_FORMAT        := REG2ID(GFX_PAL_FORMAT)}

{ $define FIFO_CLEAR_COLOR       := REG2ID(GFX_CLEAR_COLOR)}
{ $define FIFO_CLEAR_DEPTH       := REG2ID(GFX_CLEAR_DEPTH)}

{ $define FIFO_LIGHT_VECTOR      := REG2ID(GFX_LIGHT_VECTOR)}
{ $define FIFO_LIGHT_COLOR       := REG2ID(GFX_LIGHT_COLOR)}
{ $define FIFO_NORMAL            := REG2ID(GFX_NORMAL)}

{ $define FIFO_DIFFUSE_AMBIENT   := REG2ID(GFX_DIFFUSE_AMBIENT)}
{ $define FIFO_SPECULAR_EMISSION := REG2ID(GFX_SPECULAR_EMISSION)}
{ $define FIFO_SHININESS         := REG2ID(GFX_SHININESS)}

{ $define FIFO_POLY_FORMAT       := REG2ID(GFX_POLY_FORMAT)}

{ $define FIFO_BEGIN             := REG2ID(GFX_BEGIN)}
{ $define FIFO_END               := REG2ID(GFX_END)}
{ $define FIFO_FLUSH             := REG2ID(GFX_FLUSH)} 
{ $define FIFO_VIEWPORT          := REG2ID(GFX_VIEWPORT)}




//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
////////////                                                                  ////////////
////////////                                                                  ////////////
////////////                                                                  ////////////
////////////                       INLINED FUNCTIONS                          ////////////
////////////                                                                  ////////////
////////////                                                                  ////////////
////////////                                                                  ////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////


function POLY_ALPHA(n: cint): cuint32; inline;
begin
  POLY_ALPHA := cuint32((n) shl 16);
end;

function POLY_ID(n: cint): cuint32; inline;	
begin
  POLY_ID := cuint32((n) shl 24);
end;

procedure glBegin(mode: GL_GLBEGIN_ENUM); inline;
begin
  GFX_BEGIN^ := mode;
end;

procedure glEnd(); inline;
begin 
  GFX_END^ := 0;
end;

procedure glClearDepth(depth: fixed12d3); inline;
begin
  GFX_CLEAR_DEPTH^ := depth;
end;

procedure glColor3b(red, green, blue: cuint8); inline;
begin
  GFX_COLOR^ := cuint32(RGB15(red shr 3, green shr 3, blue shr 3));
end;

procedure glColor(color: rgb); inline;
begin
  GFX_COLOR^ := cuint32(color);
end;

procedure glVertex3v16(x, y, z: v16); inline;
begin
	GFX_VERTEX16^ := cuint32((y shl 16) or (x and $FFFF));
	GFX_VERTEX16^ := cuint32(cuint16(z));
end;

procedure glTexCoord2t16(u, v: t16); inline; 
begin
  GFX_TEX_COORD^ := TEXTURE_PACK(u, v);
end;

procedure glPushMatrix(); inline; 
begin
  MATRIX_PUSH^ := 0;
end;

procedure glPopMatrix(num: cint32); inline;
begin
  MATRIX_POP^ := num;
end;

procedure glRestoreMatrix(index: cint32); inline;
begin
  MATRIX_RESTORE^ := index;
end;

procedure glStoreMatrix(index: cint32); inline;
begin
  MATRIX_STORE^ := index;
end;

procedure glScalev(const v: PGLvector); inline;
begin
	MATRIX_SCALE^ := v^.x;
	MATRIX_SCALE^ := v^.y;
	MATRIX_SCALE^ := v^.z;
end;


procedure glTranslatev(const v: PGLvector) ; inline;
begin
	MATRIX_TRANSLATE^ := v^.x;
	MATRIX_TRANSLATE^ := v^.y;
	MATRIX_TRANSLATE^ := v^.z;
end;

procedure glTranslate3f32(x, y, z: cint32); inline;
begin
	MATRIX_TRANSLATE^ := x;
	MATRIX_TRANSLATE^ := y;
	MATRIX_TRANSLATE^ := z;
end;

procedure glScalef32(factor: cint32); inline;
begin
	MATRIX_SCALE^ := factor;
	MATRIX_SCALE^ := factor;
	MATRIX_SCALE^ := factor;
end;

procedure glLight(id: cint; color: rgb; x, y, z: v10); inline;
begin
  id := (id and 3) shl 30;
	GFX_LIGHT_VECTOR^ := id or ((z and $3FF) shl 20) or ((y and $3FF) shl 10) or (x and $3FF);
	GFX_LIGHT_COLOR^ := id or color;
end;

procedure glNormal(normal: cuint32); inline;
begin
  GFX_NORMAL^ := normal;
end;

procedure glLoadIdentity(); inline;
begin
  MATRIX_IDENTITY^ := 0;
end;

procedure glMatrixMode(mode: GL_MATRIX_MODE_ENUM); inline;
begin
  MATRIX_CONTROL^ := mode;
end;

procedure glViewport(x1, y1, x2, y2: cuint8); inline;
begin
  GFX_VIEWPORT^ := (x1) + (y1 shl 8) + (x2 shl 16) + (y2 shl 24);
end;

procedure glFlush(mode: cuint32); inline;
begin
  GFX_FLUSH^ := mode;
end;

procedure glMaterialShinyness(); inline;
var
  shiny32: array [0..31] of cuint32;
  shiny8: pcuint8;
  i: integer;
begin
  shiny8 := pcuint8(@shiny32);

  i := 0;
  while i < (128 * 2) do
  begin
    shiny8[i shr 1] := i;
    inc(i, 2);
  end;

  for i := 0 to 31 do
    GFX_SHININESS[i] := shiny32[i];
end;

procedure glCallList(list: pcuint32); inline;
var
  count: cuint32;
begin
  count := list^;
  inc(list);
  DC_FlushRange(list, count*4);
  while ( ((DMA_CR(0)^ and DMA_BUSY) <> 0) or 
          ((DMA_CR(1)^ and DMA_BUSY) <> 0) or 
          ((DMA_CR(2)^ and DMA_BUSY) <> 0) or 
          ((DMA_CR(3)^ and DMA_BUSY) <> 0)) do;

  DMA_SRC(0)^ := cuint32(list);
  DMA_DEST(0)^ := $4000400;
  DMA_CR(0)^ := DMA_FIFO or count;
  while (DMA_CR(0)^ and DMA_BUSY) <> 0 do;
end;

procedure glPolyFmt(params: cuint32); inline;
begin
  GFX_POLY_FORMAT^ := params;
end;
////////////////////////////////////////////////////////////////

procedure glEnable(bits: cint); inline;
begin
  GFX_CONTROL^ := GFX_CONTROL^ or bits;
end;

procedure glDisable(bits: cint); inline;
begin
  GFX_CONTROL^ := GFX_CONTROL^ and not bits;
end;

procedure glLoadMatrix4x4(const m: pm4x4); inline;
begin
  MATRIX_LOAD4x4^ := m^.m[0];
  MATRIX_LOAD4x4^ := m^.m[1];
  MATRIX_LOAD4x4^ := m^.m[2];
  MATRIX_LOAD4x4^ := m^.m[3];

  MATRIX_LOAD4x4^ := m^.m[4];
  MATRIX_LOAD4x4^ := m^.m[5];
  MATRIX_LOAD4x4^ := m^.m[6];
  MATRIX_LOAD4x4^ := m^.m[7];

  MATRIX_LOAD4x4^ := m^.m[8];
  MATRIX_LOAD4x4^ := m^.m[9];
  MATRIX_LOAD4x4^ := m^.m[10];
  MATRIX_LOAD4x4^ := m^.m[11];

  MATRIX_LOAD4x4^ := m^.m[12];
  MATRIX_LOAD4x4^ := m^.m[13];
  MATRIX_LOAD4x4^ := m^.m[14];
  MATRIX_LOAD4x4^ := m^.m[15];
end;

procedure glLoadMatrix4x3(const m: pm4x3); inline;
begin
  MATRIX_LOAD4x3^ := m^.m[0];
  MATRIX_LOAD4x3^ := m^.m[1];
  MATRIX_LOAD4x3^ := m^.m[2];
  MATRIX_LOAD4x3^ := m^.m[3];

  MATRIX_LOAD4x3^ := m^.m[4];
  MATRIX_LOAD4x3^ := m^.m[5];
  MATRIX_LOAD4x3^ := m^.m[6];
  MATRIX_LOAD4x3^ := m^.m[7];

  MATRIX_LOAD4x3^ := m^.m[8];
  MATRIX_LOAD4x3^ := m^.m[9];
  MATRIX_LOAD4x3^ := m^.m[10];
  MATRIX_LOAD4x3^ := m^.m[11];
end;

procedure glMultMatrix4x4(const m: pm4x4); inline;
begin
  MATRIX_MULT4x4^ := m^.m[0];
  MATRIX_MULT4x4^ := m^.m[1];
  MATRIX_MULT4x4^ := m^.m[2];
  MATRIX_MULT4x4^ := m^.m[3];

  MATRIX_MULT4x4^ := m^.m[4];
  MATRIX_MULT4x4^ := m^.m[5];
  MATRIX_MULT4x4^ := m^.m[6];
  MATRIX_MULT4x4^ := m^.m[7];

  MATRIX_MULT4x4^ := m^.m[8];
  MATRIX_MULT4x4^ := m^.m[9];
  MATRIX_MULT4x4^ := m^.m[10];
  MATRIX_MULT4x4^ := m^.m[11];

  MATRIX_MULT4x4^ := m^.m[12];
  MATRIX_MULT4x4^ := m^.m[13];
  MATRIX_MULT4x4^ := m^.m[14];
  MATRIX_MULT4x4^ := m^.m[15];
end;

procedure glMultMatrix4x3(const m: pm4x3); inline;
begin
  MATRIX_MULT4x3^ := m^.m[0];
  MATRIX_MULT4x3^ := m^.m[1];
  MATRIX_MULT4x3^ := m^.m[2];
  MATRIX_MULT4x3^ := m^.m[3];

  MATRIX_MULT4x3^ := m^.m[4];
  MATRIX_MULT4x3^ := m^.m[5];
  MATRIX_MULT4x3^ := m^.m[6];
  MATRIX_MULT4x3^ := m^.m[7];

  MATRIX_MULT4x3^ := m^.m[8];
  MATRIX_MULT4x3^ := m^.m[9];
  MATRIX_MULT4x3^ := m^.m[10];
  MATRIX_MULT4x3^ := m^.m[11];
end;

procedure glMultMatrix3x3(const m: pm3x3); inline;
begin
  MATRIX_MULT3x3^ := m^.m[0];
  MATRIX_MULT3x3^ := m^.m[1];
  MATRIX_MULT3x3^ := m^.m[2];

  MATRIX_MULT3x3^ := m^.m[3];
  MATRIX_MULT3x3^ := m^.m[4];
  MATRIX_MULT3x3^ := m^.m[5];

  MATRIX_MULT3x3^ := m^.m[6];
  MATRIX_MULT3x3^ := m^.m[7];
  MATRIX_MULT3x3^ := m^.m[8];
end;

procedure glRotateXi(angle: cint); inline;
var
  sine, cosine: cint32;
begin
  sine := sinLerp(angle);
  cosine := cosLerp(angle);

  MATRIX_MULT3x3^ := inttof32(1);
  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := 0;

  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := cosine;
  MATRIX_MULT3x3^ := sine;

  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := -sine;
  MATRIX_MULT3x3^ := cosine;
end;

procedure glRotateYi(angle: cint); inline;
var
  sine, cosine: cint32;
begin
  sine := sinLerp(angle);
  cosine := cosLerp(angle);

  MATRIX_MULT3x3^ := cosine;
  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := -sine;

  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := inttof32(1);
  MATRIX_MULT3x3^ := 0;

  MATRIX_MULT3x3^ := sine;
  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := cosine;
end;

procedure glRotateZi(angle: cint); inline;
var
  sine, cosine: cint32;
begin
  sine := sinLerp(angle);
  cosine := cosLerp(angle);

  MATRIX_MULT3x3^ := cosine;
  MATRIX_MULT3x3^ := sine;
  MATRIX_MULT3x3^ := 0;

  MATRIX_MULT3x3^ := -sine;
  MATRIX_MULT3x3^ := cosine;
  MATRIX_MULT3x3^ := 0;

  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := 0;
  MATRIX_MULT3x3^ := inttof32(1);
end;

procedure glOrthof32(left, right, bottom, top, zNear, zFar: cint32); inline;
begin
  MATRIX_MULT4x4^ := divf32(inttof32(2), right - left);
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := divf32(inttof32(2), top - bottom);
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := divf32(inttof32(-2), zFar - zNear);
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := -divf32(right + left, right - left);
  MATRIX_MULT4x4^ := -divf32(top + bottom, top - bottom);
  MATRIX_MULT4x4^ := -divf32(zFar + zNear, zFar - zNear);
  MATRIX_MULT4x4^ := floattof32(1.0);
end;

procedure gluLookAtf32(eyex, eyey, eyez, lookAtx, lookAty, lookAtz, upx, upy, upz: cint32); inline;
var
  side, forwrd, up, eye: array [0..2] of cint32;
begin

  forwrd[0] := eyex - lookAtx;
  forwrd[1] := eyey - lookAty;
  forwrd[2] := eyez - lookAtz;

  normalizef32(@forwrd);

  up[0] := upx;
  up[1] := upy;
  up[2] := upz;
  eye[0] := eyex;
  eye[1] := eyey;
  eye[2] := eyez;

  crossf32(@up, @forwrd, @side);

  normalizef32(@side);

  // Recompute local up
  crossf32(@forwrd, @side, @up);

  glMatrixMode(GL_MODELVIEW);


  // should we use MATRIX_MULT4x3?
  MATRIX_MULT4x3^ := side[0];
  MATRIX_MULT4x3^ := up[0];
  MATRIX_MULT4x3^ := forwrd[0];

  MATRIX_MULT4x3^ := side[1];
  MATRIX_MULT4x3^ := up[1];
  MATRIX_MULT4x3^ := forwrd[1];

  MATRIX_MULT4x3^ := side[2];
  MATRIX_MULT4x3^ := up[2];
  MATRIX_MULT4x3^ := forwrd[2];

  MATRIX_MULT4x3^ := -dotf32(@eye,@side);
  MATRIX_MULT4x3^ := -dotf32(@eye,@up);
  MATRIX_MULT4x3^ := -dotf32(@eye,@forwrd);
end;

procedure glFrustumf32(left, right, bottom, top, zNear, zFar: cint32); inline;
begin
  MATRIX_MULT4x4^ := divf32(2*znear, right - left);
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := divf32(2*znear, top - bottom);
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := divf32(right + left, right - left);
  MATRIX_MULT4x4^ := divf32(top + bottom, top - bottom);
  MATRIX_MULT4x4^ := -divf32(zfar + znear, zfar - znear);
  MATRIX_MULT4x4^ := floattof32(-1.0);

  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := -divf32(2 * mulf32(zfar, znear), zfar - znear);
  MATRIX_MULT4x4^ := 0;
end;

procedure gluPerspectivef32(fovy: cint; aspect, zNear, zFar: cint32); inline;
var
  xmin, xmax, ymin, ymax: cint32;
begin
  ymax := mulf32(zNear, tanLerp(fovy shr 1));
  ymin := -ymax;
  xmin := mulf32(ymin, aspect);
  xmax := mulf32(ymax, aspect);

  glFrustumf32(xmin, xmax, ymin, ymax, zNear, zFar);
end;

procedure glTexCoord2f(s, t: cfloat); inline;
var
  x, y: cint;
begin
  x := ((glGlob^.textures[glGlob^.activeTexture]) shr 20) and 7;
  y := ((glGlob^.textures[glGlob^.activeTexture]) shr 23) and 7;

  glTexCoord2t16(floattot16(s*(8 shl x)), floattot16(t*(8 shl y)));
end;

procedure gluPickMatrix(x, y, width, height: cint32; const viewport: TArr4ofInt); inline;
begin
  MATRIX_MULT4x4^ := inttof32(viewport[2]) div width;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := inttof32(viewport[3]) div height;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := inttof32(1);
  MATRIX_MULT4x4^ := 0;

  MATRIX_MULT4x4^ := inttof32(viewport[2] + ((viewport[0] - x) shl 1)) div width;
  MATRIX_MULT4x4^ := inttof32(viewport[3] + ((viewport[1] - y) shl 1)) div height;
  MATRIX_MULT4x4^ := 0;
  MATRIX_MULT4x4^ := inttof32(1);
end;

procedure glResetMatrixStack(); inline;
begin
  // make sure there are no push/pops that haven't executed yet
  while (GFX_STATUS^ and (1 shl 14)) <> 0 do
  begin
    GFX_STATUS^ := GFX_STATUS^ or (1 shl 15); // clear push/pop errors or push/pop busy bit never clears
  end;

  // pop the projection stack to the top; poping 0 off an empty stack causes an error... weird?
  if  (GFX_STATUS^ and (1 shl 13)) <> 0 then
  begin
    glMatrixMode(GL_PROJECTION);
    glPopMatrix(1);
  end;

  // 31 deep modelview matrix; 32nd entry works but sets error flag
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix((GFX_STATUS^ shr 8) and $1F);

  // load identity to all the matrices
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();	
end;

procedure glSetOutlineColor(id: cint; color: rgb); inline;
begin
  GFX_EDGE_TABLE[id] := cuint16(color);
end;

procedure glSetToonTable(const table: pcuint16); inline;
var
  i: integer;
begin
  for i := 0 to 31 do
    GFX_TOON_TABLE[i] := table[i];
end;

procedure glSetToonTableRange(_start, _end: cint; color: rgb); inline;
var
  i: integer;
begin
  for i := _start to _end do
    GFX_TOON_TABLE[i] := cuint16(color);
end;

procedure glGetFixed(const param: GL_GET_ENUM; f: pcint32); inline;
var
  i: integer;
begin
  case param of 
    GL_GET_MATRIX_VECTOR:
      begin
        while (GFX_BUSY) do; // wait until the graphics engine has stopped to read matrixes
        for i := 0 to 8 do 
          f[i] := MATRIX_READ_VECTOR[i];
      end;
    GL_GET_MATRIX_CLIP:
      begin
        while (GFX_BUSY) do; // wait until the graphics engine has stopped to read matrixes
        for i := 0 to 15 do 
          f[i] := MATRIX_READ_CLIP[i];
      end;
    GL_GET_MATRIX_PROJECTION:
      begin
        glMatrixMode(GL_POSITION);
        glPushMatrix(); // save the current state of the position matrix
        glLoadIdentity(); // load an identity matrix into the position matrix so that the modelview matrix = projection matrix
        while (GFX_BUSY) do; // wait until the graphics engine has stopped to read matrixes
        for i := 0 to 15 do
          f[i] := MATRIX_READ_CLIP[i]; // read out the projection matrix
        glPopMatrix(1); // restore the position matrix
      end;
    GL_GET_MATRIX_POSITION:
      begin
        glMatrixMode(GL_PROJECTION);
        glPushMatrix(); // save the current state of the projection matrix
        glLoadIdentity(); // load a identity matrix into the projection matrix so that the modelview matrix = position matrix
        while (GFX_BUSY) do; // wait until the graphics engine has stopped to read matrixes
        for i := 0 to 15 do
          f[i] := MATRIX_READ_CLIP[i]; // read out the position matrix
        glPopMatrix(1); // restore the projection matrix
      end;
  end;
end;

procedure glAlphaFunc(alphaThreshold: cint); inline;
begin
  GFX_ALPHA_TEST^ := alphaThreshold;
end;

procedure glCutoffDepth(wVal: fixed12d3); inline;
begin
  GFX_CUTOFF_DEPTH^ := wVal;
end;

procedure glInit(); inline;
begin
  glInit_C(); // actually does the initialization
end;

procedure glClearColor(red, green, blue, alpha: cuint8); inline;
begin
  glGlob^.clearColor := (glGlob^.clearColor and $FFE08000) or ($7FFF and RGB15(red, green, blue)) or ((alpha and $1F) shl 16);
  GFX_CLEAR_COLOR^ := glGlob^.clearColor;
end;

procedure glClearPolyID(ID: cuint8); inline;
begin
  glGlob^.clearColor  := ( glGlob^.clearColor and $C0FFFFFF) or (( ID and $3F ) shl 24 );
  GFX_CLEAR_COLOR^ := glGlob^.clearColor; 
end;

procedure glGetInt(param: GL_GET_ENUM; var i: cint); inline;
begin
  case param of
    GL_GET_POLYGON_RAM_COUNT:
      i := GFX_POLYGON_RAM_USAGE^;
    GL_GET_VERTEX_RAM_COUNT:
      i := GFX_VERTEX_RAM_USAGE^;
    GL_GET_TEXTURE_WIDTH:
      i := 8 shl (((glGlob^.textures[glGlob^.activeTexture]) shr 20) and 7);
    GL_GET_TEXTURE_HEIGHT:
      i := 8 shl (((glGlob^.textures[glGlob^.activeTexture]) shr 23) and 7);
  end;
end;

//---------------------------------------------------------------------------------
//                    INLINED FlOAT WRAPPERS

procedure glVertex3f(x, y, z: cfloat); inline;
begin
  glVertex3v16(floattov16(x), floattov16(y), floattov16(z));
end;

procedure glRotatef32(angle: cfloat; x, y, z: cint32); inline;
begin
  glRotatef32i(trunc(angle * DEGREES_IN_CIRCLE / 360.0), x, y, z);
end;

procedure glRotatef(angle, x, y, z: cfloat); inline;
begin
  glRotatef32(angle, floattof32(x), floattof32(y), floattof32(z));
end;

procedure glColor3f(r, g, b: cfloat); inline;
begin
  glColor3b(trunc(r*255), trunc(g*255), trunc(b*255));
end;

procedure glScalef(x, y, z: cfloat); inline;
begin
  MATRIX_SCALE^ := floattof32(x);
  MATRIX_SCALE^ := floattof32(y);
  MATRIX_SCALE^ := floattof32(z);
end;

procedure glTranslatef(x, y, z: cfloat); inline;
begin
  MATRIX_TRANSLATE^ := floattof32(x);
  MATRIX_TRANSLATE^ := floattof32(y);
  MATRIX_TRANSLATE^ := floattof32(z);
end;

procedure glNormal3f(x, y, z: cfloat); inline;
begin
  glNormal(NORMAL_PACK(floattov10(x), floattov10(y), floattov10(z)));
end;

procedure glRotateX(angle: cfloat); inline;
begin
  glRotateXi(trunc(angle * DEGREES_IN_CIRCLE / 360.0));
end;

procedure glRotateY(angle: cfloat); inline;
begin
  glRotateYi(trunc(angle * DEGREES_IN_CIRCLE / 360.0));
end;

procedure glRotateZ(angle: cfloat); inline;
begin
  glRotateZi(trunc(angle * DEGREES_IN_CIRCLE / 360.0));
end;

procedure glOrtho(left, right, bottom, top, zNear, zFar: cfloat); inline;
begin
  glOrthof32(floattof32(left), floattof32(right), floattof32(bottom), floattof32(top), floattof32(zNear), floattof32(zFar));
end;

procedure gluLookAt(eyex, eyey, eyez, lookAtx, lookAty, lookAtz, upx, upy, upz: cfloat); inline;
begin
  gluLookAtf32(floattof32(eyex), floattof32(eyey), floattof32(eyez),
         floattof32(lookAtx), floattof32(lookAty), floattof32(lookAtz),
         floattof32(upx), floattof32(upy), floattof32(upz));
end;

procedure glFrustum(left, right, bottom, top, znear, zfar: cfloat); inline;
begin
  glFrustumf32(floattof32(left), floattof32(right), floattof32(bottom), floattof32(top), floattof32(znear), floattof32(zfar));
end;

procedure gluPerspective(fovy, aspect, zNear, zFar: cfloat); inline;
begin
  gluPerspectivef32(trunc(fovy * DEGREES_IN_CIRCLE / 360.0), floattof32(aspect), floattof32(zNear), floattof32(zFar));
end;
{$endif NDS_IMPLEMENTATION}
