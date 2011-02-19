(****************************************
 *    NDS NeHe Lesson 10          *
 *    Author: Dovoto          *
 ****************************************)
program Lesson10;
{$mode objfpc}

{$L build/Mud.pcx.o}
{$L build/World.txt.o}
{$L build/drunkenlogo.pcx.o}

uses
  ctypes, nds9;

{$include inc/Mud.pcx.inc}
{$include inc/World.txt.inc}
{$include inc/drunkenlogo.pcx.inc}

const
  piover180: cfloat = 0.0174532925;
var
  heading: cfloat = 0.0;
  xpos: cfloat = 0.0;
  zpos: cfloat = 0.0;
  yrot: cfloat = 0.0;       // Y Rotation
  walkbias: cfloat = 0.0;
  walkbiasangle: cfloat = 0.0;
  lookupdown: cfloat = 0.0;

  texture: array [0..2] of integer;  // Storage For 3 Textures (only going to use 1 on the DS for this demo)

type
  tagVERTEX = record
    x, y, z: cfloat;
    u, v: cfloat;
  end;
  VERTEX = tagVERTEX;

  tagTRIANGLE = record
    vertex: array [0..2] of VERTEX;
  end;
  TRIANGLE = tagTRIANGLE;
  TTriangle = TRIANGLE;
  PTriangle = ^TRIANGLE;

  tagSECTOR = record
    numtriangles: integer;
    triangle:   PTRIANGLE;
  end;
  SECTOR = tagSECTOR;

  TCubeRot = record
    x, y, z: cfloat;
  end;

var
  sector1: SECTOR;        // Our Model Goes Here:
  Myfile: pchar;
  cuberot: TCubeRot;

procedure ShadowDemo(); forward;
  
function DrawGLScene(): boolean;
var
  x_m, y_m, z_m:  cfloat;
  u_m, v_m: cfloat;
  xtrans, ztrans, ytrans: cfloat;
  sceneroty: cfloat;
  numtriangles: integer;
  loop_m: integer;
begin
  // Reset The View
  xtrans := -xpos;
  ztrans := -zpos;
  ytrans := -walkbias - 0.25;
  sceneroty := 360.0 - yrot;

  glLoadIdentity();

  glRotatef(lookupdown,1.0,0,0);
  glRotatef(sceneroty,0,1.0,0);

  glTranslatef(xtrans, ytrans, ztrans);
  glBindTexture(GL_TEXTURE_2D, texture[0]);

  numtriangles := sector1.numtriangles;


  // Process Each Triangle
  for loop_m := 0 to numtriangles - 1 do
  begin
    glBegin(GL_TRIANGLES);
      glNormal3f( 0.0, 0.0, 1.0);
      x_m := sector1.triangle[loop_m].vertex[0].x;
      y_m := sector1.triangle[loop_m].vertex[0].y;
      z_m := sector1.triangle[loop_m].vertex[0].z;
      u_m := sector1.triangle[loop_m].vertex[0].u;
      v_m := sector1.triangle[loop_m].vertex[0].v;
      glTexCoord2f(u_m,v_m); glVertex3f(x_m,y_m,z_m);

      x_m := sector1.triangle[loop_m].vertex[1].x;
      y_m := sector1.triangle[loop_m].vertex[1].y;
      z_m := sector1.triangle[loop_m].vertex[1].z;
      u_m := sector1.triangle[loop_m].vertex[1].u;
      v_m := sector1.triangle[loop_m].vertex[1].v;
      glTexCoord2f(u_m,v_m); glVertex3f(x_m,y_m,z_m);

      x_m := sector1.triangle[loop_m].vertex[2].x;
      y_m := sector1.triangle[loop_m].vertex[2].y;
      z_m := sector1.triangle[loop_m].vertex[2].z;
      u_m := sector1.triangle[loop_m].vertex[2].u;
      v_m := sector1.triangle[loop_m].vertex[2].v;
      glTexCoord2f(u_m,v_m); glVertex3f(x_m,y_m,z_m);
    glEnd();
  end;
  ShadowDemo();
  DrawGLScene := true;                    // Everything Went OK
end;

function tsin(angle: cfloat): cfloat;
var
  s: cint32;
begin
  s := sinLerp(trunc((angle * DEGREES_IN_CIRCLE) / 360.0));

  tsin := f32tofloat(s);
end;

function tcos(angle: cfloat): cfloat;
var
  c: cint32;
begin
  c := cosLerp(trunc((angle * DEGREES_IN_CIRCLE) / 360.0));

  tcos := f32tofloat(c);
end;

procedure myGetStr(buff: pchar; size: integer);
begin
  buff^ := Myfile^;
  inc(MyFile);

  while (buff^ <> #10) and (buff^ <> #13) do
  begin
    inc(buff);
    buff^ := Myfile^;
    inc(MyFile);
  end;

  buff[0] := #10;
  buff[1] := #0;
end;

procedure readstr(str: pchar);
begin
  repeat
    myGetStr(str, 255);
  until ((str[0] <> '/') and (str[0] <> #10));
end;


procedure SetupWorld();
var
  x, y, z: cfloat;
  u, v: cfloat;
  numtriangles: integer;
  oneline: array [0..254] of char;
  loop, vert: integer;
begin
  readstr(oneline);
  sscanf(oneline, 'NUMPOLLIES %d'#10, @numtriangles);

  GetMem(sector1.triangle, numtriangles * sizeof(TTRIANGLE));

  sector1.numtriangles := numtriangles;

  for loop := 0 to numtriangles - 1 do
  begin
    for vert := 0 to 2 do
    begin
      readstr(oneline);
      sscanf(oneline, '%f %f %f %f %f', @x, @y, @z, @u, @v);
      sector1.triangle[loop].vertex[vert].x := x;
      sector1.triangle[loop].vertex[vert].y := y;
      sector1.triangle[loop].vertex[vert].z := z;
      sector1.triangle[loop].vertex[vert].u := u;
      sector1.triangle[loop].vertex[vert].v := v;
    end;
  end;
end;

// Load PCX files And Convert To Textures
function LoadGLTextures(): boolean;
var
  pcx: sImage;
begin
  glGenTextures(2, @texture[0]);

  //load our texture
  loadPCX(pcuint8(Mud_pcx), @pcx);

  image8to16(@pcx);

  glBindTexture(0, texture[0]);
  glTexImage2D(0, 0, GL_RGB, TEXTURE_SIZE_128 , TEXTURE_SIZE_128, 0, TEXGEN_TEXCOORD or GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T, pcx.image.data8);

  imageDestroy(@pcx);

  loadPCX(pcuint8(drunkenlogo_pcx), @pcx);
  image8to16(@pcx);
  glBindTexture(0, texture[1]);
  glTexImage2D(0, 0, GL_RGB, TEXTURE_SIZE_128, TEXTURE_SIZE_128, 0, TEXGEN_TEXCOORD, pcx.image.data8);

  imageDestroy(@pcx);

  result := true;
end;

procedure TransformCube();
begin
  glRotatef(cubeRot.x, 1.0, 0.0, 0.0);
  glRotatef(cubeRot.y, 0.0, 1.0, 0.0);
  glRotatef(cubeRot.z, 0.0, 0.0, 1.0);
end;

procedure EmitCube();
begin
  glPushMatrix();
  glScalef(0.03, 0.03, 0.03);	
  
  glRotatef(cubeRot.x, 1.0, 0.0, 0.0);
  glRotatef(cubeRot.y, 0.0, 1.0, 0.0);
  glRotatef(cubeRot.z, 0.0, 0.0, 1.0);
  
  glBegin(GL_QUADS);
    // Front Face
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
    // Back Face
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
    // Top Face
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    // Bottom Face
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    // Right face
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    // Left Face
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
  glEnd();
  glPopMatrix(1);
end;

procedure ShadowDemo();
begin
  cubeRot.y := cubeRot.y + 0.8;
  
  //draw the actual cube
  glPushMatrix();
  glTranslatef(0.0, 0.4, -0.4); //draw the cube up in the air
  TransformCube();
  glBindTexture(GL_TEXTURE_2D, texture[1]);
  EmitCube();
  glPopMatrix(1);
  
  //draw the shadow:
  begin
    //draw the cube shadow on the ground
    glPushMatrix();
    glTranslatef(0.0, 0.0, -0.4);
    TransformCube();
    
    //use no texture. set shadow color: we'll use green just to show that it is possible
    glBindTexture(0, 0);
    glColor(RGB15(0, 8, 0));
    
    //1st shadow pass
    //be sure to use opaque here
    glPolyFmt(POLY_SHADOW or POLY_CULL_FRONT or POLY_ALPHA(31));
    EmitCube();
    
    //2nd shadow pass
    //be sure to use a different polyID here (shadow with polyID 0 can't be cast on surface with polyID 0)
    //we set the fog bit here because we want the shadow to be fogged later. i think it may be buggy but it looks better.
    glPolyFmt(POLY_SHADOW or POLY_CULL_BACK or POLY_ALPHA(15) or POLY_ID(1) or POLY_FOG);
    EmitCube();
    
    //reset poly attribute
    glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE or POLY_FORMAT_LIGHT0 or POLY_FOG);
    
    glPopMatrix(1);
  end;
end;

var
  thisXY: touchPosition;
  lastXY: touchPosition;
  dx, dy: cint16;
  i: integer;
begin
  MyFile := pchar(@World_txt);
  // Setup the Main screen for 3D
  videoSetMode(MODE_0_3D);
  vramSetBankA(VRAM_A_TEXTURE);                        //NEW  must set up some memory for textures

  consoleDemoInit();

  // initialize the geometry engine
  glInit();

  // enable textures
  glEnable(GL_TEXTURE_2D);

  // enable antialiasing
  glEnable(GL_ANTIALIAS);

  // enable alpha blending for shadow demo
  glEnable(GL_BLEND);

  // setup the rear plane
  glClearColor(0,0,0,31); // BG must be opaque for AA to work
  glClearPolyID(63); // BG must have a unique polygon ID for AA to work
  glClearDepth($7FFF);

  // Set our viewport to be the same size as the screen
  glViewport(0,0,255,191);

  LoadGLTextures();
  SetupWorld();

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(70, 256.0 / 192.0, 0.1, 100);

  glLight(0, RGB15(31,31,31) , 0, floattov10(-1.0), 0);

  //need to set up some material properties since DS does not have them set by default
  glMaterialf(GL_AMBIENT, RGB15(16,16,16));
  glMaterialf(GL_DIFFUSE, RGB15(16,16,16));
  glMaterialf(GL_SPECULAR, BIT(15) or RGB15(8,8,8));
  glMaterialf(GL_EMISSION, RGB15(16,16,16));

  //ds uses a table for shinyness..this generates a half-ass one
  glMaterialShinyness();

  //ds specific, several attributes can be set here
  glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE or POLY_FORMAT_LIGHT0 or POLY_FOG);

  // Set the current matrix to be the model matrix
  glMatrixMode(GL_MODELVIEW);

  //setup demo fog parameters
  //these parameters are somewhat arbitrary, and designed to illustrate fog in just this one case.
  //you will certainly need to tweak them for your own use.
  //be sure to have set the POLY_FOG bit on any material you want to be fogged.
  glEnable(GL_FOG);
  glFogShift(2);
  glFogColor(0, 0, 0, 0);

  for i := 0 to 31 do
    glFogDensity(i, i * 4);

  glFogDensity(31, 127);
  glFogOffset($6000);

  while true do
  begin
    //these little button functions are pretty handy
    scanKeys();

    if (keysHeld() and KEY_A) <> 0 then lookupdown := lookupdown - 1.0;

    if (keysHeld() and KEY_B) <> 0 then lookupdown := lookupdown + 1.0;

    if (keysHeld() and KEY_LEFT) <> 0 then
    begin
      heading := heading + 1.0;
      yrot := heading;
    end;

    if (keysHeld() and KEY_RIGHT) <> 0 then
    begin
      heading := heading - 1.0;
      yrot := heading;
    end;

    if (keysHeld() and KEY_DOWN) <> 0 then
    begin
      xpos := xpos + (tsin(heading)) * 0.05;
      zpos := zpos + (tcos(heading)) * 0.05;
      if (walkbiasangle >= 359.0) then
        walkbiasangle := 0.0
      else
        walkbiasangle:= walkbiasangle+10;

      walkbias := tsin(walkbiasangle) / 20.0;
    end;

    if (keysHeld() and KEY_UP) <> 0 then
    begin
      xpos := xpos - (tsin(heading)) * 0.05;
      zpos := zpos - (tcos(heading)) * 0.05;

      if (walkbiasangle <= 1.0) then
        walkbiasangle := 359.0
      else
        walkbiasangle := walkbiasangle- 10;

      walkbias := tsin(walkbiasangle) / 20.0;
    end;

    glColor3f(1,1,1);

    DrawGLScene();

    // flush to screen
    glFlush(0);

    // wait for the screen to refresh
    swiWaitForVBlank();
  end;

end.
