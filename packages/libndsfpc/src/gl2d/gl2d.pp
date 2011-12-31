(*
  Easy GL2D
  Relminator 2011 
  Richard Eric M. Lope BSN RN
  Http://Rel.Phatcode.Net
  A very small, simple, yet very fast DS 2D rendering lib using the DS' 3D core.
  --
  Translated in Object Pascal by Francesco Lombardi - 2012
  http://itaprogaming.free.fr
*)


unit gl2d;

{$mode objfpc}
{$H+}

interface

uses
  ctypes, nds9;

type
  GL_FLIP_MODE = cint;

const
  GL_FLIP_NONE = 1 shl 0;
  GL_FLIP_V = 1 shl 1;
  GL_FLIP_H = 1 shl 2;


type
  glImage = record
    Width: cint;
    Height: cint;
    u_off: cint;
    v_off: cint;
    textureID: cint;
  end;
  TGLImage = glImage;
  PGLImage = ^glImage;


procedure glScreen2D();
procedure glBegin2D();
procedure glEnd2D();
function glGetActiveTexture(): cint; inline;
procedure glSetActiveTexture(TextureID: cint); inline;
procedure glPutPixel(x, y, color: cint);
procedure glLine(x1, y1, x2, y2, color: cint);
procedure glBox(x1, y1, x2, y2, color: cint);
procedure glBoxFilled(x1, y1, x2, y2, color: cint);
procedure glBoxFilledGradient(x1, y1, x2, y2, color1, color2, color3, color4: cint);
procedure glTriangle(x1, y1, x2, y2, x3, y3, color: cint);
procedure glTriangleFilled(x1, y1, x2, y2, x3, y3, color: cint);
procedure glTriangleFilledGradient(x1, y1, x2, y2, x3, y3, color1, color2, color3: cint);      
procedure glSprite(x, y, flipmode: cint; const spr: PglImage);
procedure glSpriteScale(x, y: cint; scale: cint32; flipmode: cint; const spr: PglImage);
procedure glSpriteScaleXY(x, y: cint; scaleX, scaleY: cint32; flipmode: cint; const spr: PglImage);
procedure glSpriteRotate(x, y: cint; angle: cint32; flipmode: cint; const spr: PglImage);
procedure glSpriteRotateScale(x, y: cint; angle, scale: cint32; flipmode: cint; const spr: PglImage);
procedure glSpriteRotateScaleXY(x, y: cint; angle, scaleX, scaleY: cint32; flipmode: cint; const spr: PglImage);
procedure glSpriteStretchHorizontal(x, y, length_x: cint; const spr: PglImage);
procedure glSpriteOnQuad(x1, y1, x2, y2, x3, y3, x4, y4, uoff, voff, flipmode: cint;
  const spr: PGLImage);
function glLoadSpriteSet(sprite: PGLImage; const numframes: cuint;
  const texcoords: pcuint; type_: GL_TEXTURE_TYPE_ENUM; sizeX, sizeY, param: cint;
  palette_width: cint; const palette: pcuint16;
  const texture: pcuint8): cint;
function glLoadTileSet(sprite: PGLImage; tile_wid, tile_hei, bmp_wid, bmp_hei: cint;
  type_: GL_TEXTURE_TYPE_ENUM; sizeX, sizeY, param: cint; 
  palette_width: cint; const palette: pcuint16;
  const texture: pcuint8): cint;


implementation

procedure gxVertex3i(x, y, z: v16); inline;
begin
	GFX_VERTEX16^ := cuint32((y shl 16) or (x and $FFFF));
	GFX_VERTEX16^ := z;
end;

procedure gxVertex2i(x, y: v16); inline;
begin
  GFX_VERTEX_XY^ := cuint32((y shl 16) or (x and $FFFF));
end;

procedure gxTexcoord2i(u, v: t16); inline;
begin
  GFX_TEX_COORD^ := (v shl 20) or ((u shl 4) and $FFFF);
end;

procedure gxScalef32(x, y, z: s32); inline;
begin
  MATRIX_SCALE^ := x;
  MATRIX_SCALE^ := y;
  MATRIX_SCALE^ := z;
end;

procedure gxTranslate3f32(x, y, z: cint32); inline;
begin
  MATRIX_TRANSLATE^ := x;
  MATRIX_TRANSLATE^ := y;
  MATRIX_TRANSLATE^ := z;
end;


const
  g_depth: v16 = 0;
var
  gCurrentTexture: cint = 0;

procedure SetOrtho();
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrthof32(0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, -1 shl 12, 1 shl 12);
end;

procedure glScreen2D();
begin

  // initialize gl
  glInit();

  //enable textures
  glEnable(GL_TEXTURE_2D);

  // enable antialiasing
  glEnable(GL_ANTIALIAS);

  // setup the rear plane
  glClearColor(0, 0, 0, 31); // BG must be opaque for AA to work
  glClearPolyID(63); // BG must have a unique polygon ID for AA to work

  glClearDepth(GL_MAX_DEPTH);

  //this should work the same as the normal gl call
  glViewport(0, 0, 255, 191);


  //any floating point gl call is being converted to fixed prior to being implemented
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(70, 256.0 / 192.0, 1, 200);

  gluLookAt(0.0, 0.0, 1.0,    //camera possition
    0.0, 0.0, 0.0,    //look at
    0.0, 1.0, 0.0);    //up

  glMaterialf(GL_AMBIENT, RGB15(31, 31, 31));
  glMaterialf(GL_DIFFUSE, RGB15(31, 31, 31));
  glMaterialf(GL_SPECULAR, BIT(15) or RGB15(31, 31, 31));
  glMaterialf(GL_EMISSION, RGB15(31, 31, 31));

  //ds uses a table for shinyness..this generates a half-ass one
  glMaterialShinyness();

  //not a real gl function and will likely change
  glPolyFmt(POLY_ALPHA(31) or POLY_CULL_BACK);

end;



procedure glBegin2D();
begin

  // save 3d perpective projection matrix
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();

  // save 3d modelview matrix for safety
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();


  //what?!! No glDisable(GL_DEPTH_TEST)?!!!!!!
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_ANTIALIAS);    // disable AA
  glDisable(GL_OUTLINE);      // disable edge-marking

  glColor($7FFF);         // max color

  glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE);  // no culling

  SetOrtho();

  glMatrixMode(GL_TEXTURE);
  // reset texture matrix just in case we did some funky stuff with it
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);    // reset modelview matrix. No need to scale up by << 12
  glLoadIdentity();

  gCurrentTexture := 0; // set current texture to 0
  g_depth := 0;   // set depth to 0. We need this var since we cannot disable depth testing

end;


procedure glEnd2D();
begin

  // restore 3d matrices and set current matrix to modelview
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(1);
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(1);
end;

function glGetActiveTexture(): cint; inline;
begin
	result := gCurrentTexture;
end;

procedure glSetActiveTexture(TextureID: cint); inline;
begin
	glBindTexture(0, TextureID);
	gCurrentTexture := TextureID;
end;


procedure glPutPixel(x, y, color: cint);
begin
  glBindTexture(0, 0);
  glColor(color);
  glBegin(GL_TRIANGLES);
    gxVertex3i(x, y, g_depth);
    gxVertex2i(x, y);
    gxVertex2i(x, y);
  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;
end;


procedure glLine(x1, y1, x2, y2, color: cint);
begin
  inc(x2);
  inc(y2);
  glBindTexture(0, 0);
  glColor(color);
  glBegin(GL_TRIANGLES);
    gxVertex3i(x1, y1, g_depth);
    gxVertex2i(x2, y2);
    gxVertex2i(x2, y2);
  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;
end;

procedure glBox(x1, y1, x2, y2, color: cint);
begin
  inc(x2);
  inc(y2);
  glBindTexture(0, 0);
  glColor(color);
  glBegin(GL_TRIANGLES);

  gxVertex3i(x1, y1, g_depth);
  gxVertex2i(x2, y1);
  gxVertex2i(x2, y1);

  gxVertex2i(x2, y1);
  gxVertex2i(x2, y2);
  gxVertex2i(x2, y2);
  Inc(x2);
  gxVertex2i(x2, y2);  // bug fix for lower-right corner disappearing pixel
  gxVertex2i(x1, y2);
  gxVertex2i(x1, y2);

  gxVertex2i(x1, y2);
  gxVertex2i(x1, y1);
  gxVertex2i(x1, y1);

  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;

end;

procedure glBoxFilled(x1, y1, x2, y2, color: cint);
begin
  inc(x2);
  inc(y2);
  glBindTexture(0, 0);
  glColor(color);
  glBegin(GL_QUADS);
  gxVertex3i(x1, y1, g_depth);
  // use 3i for first vertex so that we increment HW depth
  gxVertex2i(x1, y2);        // no need for 3 vertices as 2i would share last depth call
  gxVertex2i(x2, y2);
  gxVertex2i(x2, y1);
  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;

end;


procedure glBoxFilledGradient(x1, y1, x2, y2, color1, color2,
  color3, color4: cint);
begin
  inc(x2);
  inc(y2);
  glBindTexture(0, 0);
  glBegin(GL_QUADS);
  glColor(color1);
  gxVertex3i(x1, y1, g_depth);    // use 3i for first vertex so that we increment HW depth
  glColor(color2);
  gxVertex2i(x1, y2);        // no need for 3 vertices as 2i would share last depth call
  glColor(color3);
  gxVertex2i(x2, y2);
  glColor(color4);
  gxVertex2i(x2, y1);
  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;

end;


procedure glTriangle(x1, y1, x2, y2, x3, y3, color: cint);
begin

  glBindTexture(0, 0);
  glColor(color);
  glBegin(GL_TRIANGLES);

  gxVertex3i(x1, y1, g_depth);
  gxVertex2i(x2, y2);
  gxVertex2i(x2, y2);

  gxVertex2i(x2, y2);
  gxVertex2i(x3, y3);
  gxVertex2i(x3, y3);

  gxVertex2i(x3, y3);
  gxVertex2i(x1, y1);
  gxVertex2i(x1, y1);

  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;

end;

procedure glTriangleFilled(x1, y1, x2, y2, x3, y3, color: cint);
begin

  glBindTexture(0, 0);
  glColor(color);
  glBegin(GL_TRIANGLES);
  gxVertex3i(x1, y1, g_depth);
  // use 3i for first vertex so that we increment HW depth
  gxVertex2i(x2, y2);        // no need for 3 vertices as 2i would share last depth call
  gxVertex2i(x3, y3);
  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;

end;

procedure glTriangleFilledGradient(
  x1, y1, x2, y2, x3, y3, color1, color2, color3: cint);
begin

  glBindTexture(0, 0);
  glBegin(GL_TRIANGLES);
  glColor(color1);
  gxVertex3i(x1, y1, g_depth);    // use 3i for first vertex so that we increment HW depth
  glColor(color2);
  gxVertex2i(x2, y2);        // no need for 3 vertices as 2i would share last depth call
  glColor(color3);
  gxVertex2i(x3, y3);
  glEnd();
  glColor($7FFF);
  Inc(g_depth);
  gCurrentTexture := 0;

end;

procedure glSprite(x, y, flipmode: cint; const spr: PGLImage);
var
  x1, y1, x2, y2: cint;
  u1, u2, v1, v2: cint;
begin
  x1 := x;
  y1 := y;
  x2 := x + spr^.Width;
  y2 := y + spr^.Height;
  
  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;
    
  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width;
  
  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;
  
  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height;


  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glBegin(GL_QUADS);

    gxTexcoord2i(u1, v1); gxVertex3i(x1, y1, g_depth);
    gxTexcoord2i(u1, v2); gxVertex2i(x1, y2);
    gxTexcoord2i(u2, v2); gxVertex2i(x2, y2);
    gxTexcoord2i(u2, v1); gxVertex2i(x2, y1);

  glEnd();

  Inc(g_depth);

end;

procedure glSpriteScale(x, y: cint; scale: cint32; flipmode: cint;
  const spr: pglImage);
var
  x1, y1, x2, y2: cint;
  u1, u2, v1, v2: cint;
begin
  x1 := 0;
  y1 := 0;
  x2 := spr^.Width;
  y2 := spr^.Height;
                        
  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;
    
  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width - 1;
  
  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;
  
  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height - 1;


  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glPushMatrix();

  gxTranslate3f32(x, y, 0);
  gxScalef32(scale, scale, 1 shl 12);

  glBegin(GL_QUADS);

    gxTexcoord2i(u1, v1); gxVertex3i(x1, y1, g_depth);
    gxTexcoord2i(u1, v2); gxVertex2i(x1, y2);
    gxTexcoord2i(u2, v2); gxVertex2i(x2, y2);
    gxTexcoord2i(u2, v1); gxVertex2i(x2, y1);

  glEnd();

  glPopMatrix(1);
  Inc(g_depth);

end;



procedure glSpriteScaleXY(x, y: cint; scaleX, scaleY: cint32;
  flipmode: cint; const spr: pglImage);
var
  x1, y1, x2, y2: cint;
  u1, u2, v1, v2: cint;
begin
  x1 := 0;
  y1 := 0;
  x2 := spr^.Width;
  y2 := spr^.Height;

  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;
    
  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width - 1;
  
  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;
  
  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height - 1;


  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glPushMatrix();

  gxTranslate3f32(x, y, 0);
  gxScalef32(scaleX, scaleY, 1 shl 12);

  glBegin(GL_QUADS);

    gxTexcoord2i(u1, v1); gxVertex3i(x1, y1, g_depth);
    gxTexcoord2i(u1, v2); gxVertex2i(x1, y2);
    gxTexcoord2i(u2, v2); gxVertex2i(x2, y2);
    gxTexcoord2i(u2, v1); gxVertex2i(x2, y1);

  glEnd();

  glPopMatrix(1);
  Inc(g_depth);

end;

procedure glSpriteRotate(x, y: cint; angle: cint32; flipmode: cint;
  const spr: pglImage);
var
  s_half_x, s_half_y: cint;
  x1, y1, x2, y2: cint;
  u1, u2, v1, v2: cint;
begin
	s_half_x := ((spr^.width) + (spr^.width and 1)) div 2;
	s_half_y := ((spr^.height) + (spr^.height and 1)) div 2;

  x1 := -s_half_x;
  y1 := -s_half_y;

  x2 := s_half_x;
  y2 := s_half_y;


  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;

  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width - 1;

  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;

  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height - 1;

  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glPushMatrix();

  gxTranslate3f32(x, y, 0);
  glRotateZi(angle);


  glBegin(GL_QUADS);

    gxTexcoord2i(u1, v1); gxVertex3i(x1, y1, g_depth);
    gxTexcoord2i(u1, v2); gxVertex2i(x1, y2);
    gxTexcoord2i(u2, v2); gxVertex2i(x2, y2);
    gxTexcoord2i(u2, v1); gxVertex2i(x2, y1);

  glEnd();

  glPopMatrix(1);

  Inc(g_depth);

end;


procedure glSpriteRotateScale(x, y: cint; angle, scale: cint32;
  flipmode: cint; const spr: pglImage);
var
  s_half_x, s_half_y: cint;
  x1, y1, x2, y2: cint;
  u1, u2, v1, v2: cint;
begin
	s_half_x := ((spr^.width) + (spr^.width and 1)) div 2;
	s_half_y := ((spr^.height) + (spr^.height and 1)) div 2;

  x1 := -s_half_x;
  y1 := -s_half_y;

  x2 := s_half_x;
  y2 := s_half_y;

  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;
  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width - 1;
  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;
  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height - 1;

  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glPushMatrix();

  gxTranslate3f32(x, y, 0);
  gxScalef32(scale, scale, 1 shl 12);
  glRotateZi(angle);


  glBegin(GL_QUADS);

  gxTexcoord2i(u1, v1);
  gxVertex3i(x1, y1, g_depth);
  gxTexcoord2i(u1, v2);
  gxVertex2i(x1, y2);
  gxTexcoord2i(u2, v2);
  gxVertex2i(x2, y2);
  gxTexcoord2i(u2, v1);
  gxVertex2i(x2, y1);

  glEnd();

  glPopMatrix(1);

  Inc(g_depth);

end;



procedure glSpriteRotateScaleXY(x, y: cint; angle, scaleX, scaleY: cint32;
  flipmode: cint; const spr: pglImage);
var
  s_half_x, s_half_y: cint;
  x1, y1, x2, y2: cint;
  u1, u2, v1, v2: cint;
begin
	s_half_x := ((spr^.width) + (spr^.width and 1)) div 2;
	s_half_y := ((spr^.height) + (spr^.height and 1)) div 2;

  x1 := -s_half_x;
  y1 := -s_half_y;

  x2 := s_half_x;
  y2 := s_half_y;

  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;
  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width - 1;
  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;
  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height - 1;

  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glPushMatrix();

  gxTranslate3f32(x, y, 0);
  gxScalef32(scaleX, scaleY, 1 shl 12);
  glRotateZi(angle);


  glBegin(GL_QUADS);

  gxTexcoord2i(u1, v1);
  gxVertex3i(x1, y1, g_depth);
  gxTexcoord2i(u1, v2);
  gxVertex2i(x1, y2);
  gxTexcoord2i(u2, v2);
  gxVertex2i(x2, y2);
  gxTexcoord2i(u2, v1);
  gxVertex2i(x2, y1);

  glEnd();

  glPopMatrix(1);

  Inc(g_depth);

end;


procedure glSpriteStretchHorizontal(x, y, length_x: cint; const spr: pglImage);
var
  x1, y1, x2, y2: cint;
  su: cint;
  u1, u2, v1, v2: cint;
  x2l, x1l: cint;
begin
  x1 := x;
  y1 := y;
  x2 := x + length_x;
  y2 := y + spr^.Height;
  su := (spr^.Width div 2) - 1;


  u1 := spr^.u_off;
  u2 := spr^.u_off + spr^.Width;
  v1 := spr^.v_off;
  v2 := spr^.v_off + spr^.Height;


  if (spr^.textureID <> gCurrentTexture) then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  // left
  x2l := x + su;
  glBegin(GL_QUADS);

  gxTexcoord2i(u1, v1);
  gxVertex3i(x1, y1, g_depth);

  gxTexcoord2i(u1, v2);
  gxVertex2i(x1, y2);

  gxTexcoord2i(u1 + su, v2);
  gxVertex2i(x2l, y2);

  gxTexcoord2i(u1 + su, v1);
  gxVertex2i(x2l, y1);

  glEnd();

  // center
  x1l := x + su;
  x2l := x2 - su - 1;
  glBegin(GL_QUADS);

  gxTexcoord2i(u1 + su, v1);
  gxVertex2i(x1l, y1);

  gxTexcoord2i(u1 + su, v2);
  gxVertex2i(x1l, y2);

  gxTexcoord2i(u1 + su, v2);
  gxVertex2i(x2l, y2);

  gxTexcoord2i(u1 + su, v1);
  gxVertex2i(x2l, y1);

  glEnd();

  // right
  x1l := x2 - su - 1;
  glBegin(GL_QUADS);

  gxTexcoord2i(u1 + su, v1);
  gxVertex2i(x1l, y1);

  gxTexcoord2i(u1 + su, v2);
  gxVertex2i(x1l, y2);

  gxTexcoord2i(u2, v2);
  gxVertex2i(x2, y2);

  gxTexcoord2i(u2, v1);
  gxVertex2i(x2, y1);

  glEnd();

  Inc(g_depth);

end;


procedure glSpriteOnQuad(x1, y1, x2, y2, x3, y3, x4, y4, uoff, voff, flipmode: cint;
  const spr: PGLImage);
var
  u1, u2, v1, v2: cint;
begin
  if (flipmode and GL_FLIP_H) <> 0 then
    u1 := spr^.u_off + spr^.Width - 1
  else
    u1 := spr^.u_off;
  if (flipmode and GL_FLIP_H) <> 0 then
    u2 := spr^.u_off
  else
    u2 := spr^.u_off + spr^.Width;
  if (flipmode and GL_FLIP_V) <> 0 then
    v1 := spr^.v_off + spr^.Height - 1
  else
    v1 := spr^.v_off;
  if (flipmode and GL_FLIP_V) <> 0 then
    v2 := spr^.v_off
  else
    v2 := spr^.v_off + spr^.Height;


  if spr^.textureID <> gCurrentTexture then
  begin
    glBindTexture(GL_TEXTURE_2D, spr^.textureID);
    gCurrentTexture := spr^.textureID;
  end;

  glBegin(GL_QUADS);

  gxTexcoord2i(u1 + uoff, v1 + voff);
  gxVertex3i(x1, y1, g_depth);
  gxTexcoord2i(u1 + uoff, v2 + voff);
  gxVertex2i(x2, y2);
  gxTexcoord2i(u2 + uoff, v2 + voff);
  gxVertex2i(x3, y3);
  gxTexcoord2i(u2 + uoff, v1 + voff);
  gxVertex2i(x4, y4);

  glEnd();

  Inc(g_depth);

end;


function glLoadSpriteSet(sprite: PGLImage; const numframes: cuint;
  const texcoords: pcuint; type_: GL_TEXTURE_TYPE_ENUM; sizeX, sizeY, param: cint;
  palette_width: cint; const palette: pcuint16;
  const texture: pcuint8): cint;
var
  textureID: cint;
  i, j: cint;
begin
  glGenTextures(1, @textureID);
  glBindTexture(0, textureID);
  glTexImage2D(0, 0, type_, sizeX, sizeY, 0, param, texture);
	glColorTableEXT(0, 0, palette_width, 0, 0, palette);


  // init sprites texture coords and texture ID
  for i := 0 to numframes - 1 do
  begin
    j := i * 4; // texcoords array is u_off, wid, hei
    sprite[i].textureID := textureID;
    sprite[i].u_off := texcoords[j];      // set x-coord
    sprite[i].v_off := texcoords[j + 1];  // y-coord
    sprite[i].Width := texcoords[j + 2];  // don't decrease because NDS 3d core does not draw last vertical texel
    sprite[i].Height := texcoords[j + 3]; // ditto
  end;

  glLoadSpriteSet := textureID;
end;


function glLoadTileSet(sprite: PGLImage; tile_wid, tile_hei, bmp_wid, bmp_hei: cint;
  type_: GL_TEXTURE_TYPE_ENUM; sizeX, sizeY, param: cint; 
  palette_width: cint; const palette: pcuint16;
  const texture: pcuint8): cint;
var
  textureID: cint;
  i: cint;
  x, y: cint;
begin
  glGenTextures(1, @textureID);
  glBindTexture(0, textureID);
  glTexImage2D(0, 0, type_, sizeX, sizeY, 0, param, texture);
	glColorTableEXT(0, 0, palette_width, 0, 0, palette);

  i := 0;

  for y := 0 to (bmp_hei div tile_hei) - 1 do
  begin
    for x := 0 to (bmp_wid div tile_wid) - 1 do
    begin
      sprite[i].Width := tile_wid;
      sprite[i].Height := tile_hei;
      sprite[i].u_off := x * tile_wid;
      sprite[i].v_off := y * tile_hei;
      sprite[i].textureID := textureID;
      Inc(i);
    end;
  end;

  glLoadTileSet := textureID;
end;

end.

