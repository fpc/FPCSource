(****************************************
 *    NDS NeHe Lesson 10b         *
 *    Author: Dovoto          *
 ****************************************)
program Lesson10;
{$mode objfpc}

{$L build/Mud.pcx.o}
{$L build/World.txt.o}

uses
  ctypes, nds9;

{$include inc/Mud.pcx.inc}
{$include inc/World.txt.inc}

var
  heading: integer;
  xpos: cint32;
  zpos: cint32;
  yrot: cint;       // Y Rotation
  walkbias: cint32 = 0;
  walkbiasangle: cint = 0;
  lookupdown: cint = 0;
  texture: array [0..0] of integer;     // Storage For 1 Textures (only going to use 1 on the DS for this demo)


type
  tagVERTEX = record
    x, y, z: v16;
    u, v: t16;
  end;
  TVERTEX = tagVERTEX;

  tagTRIANGLE = record
    vertex: array [0..2] of TVERTEX;
  end;
  TTRIANGLE = tagTRIANGLE;
  PTRIANGLE = ^tagTRIANGLE;

  tagSECTOR = record
    numtriangles: integer;
    triangle: PTRIANGLE;
  end;
  SECTOR = tagSECTOR;
  TSECTOR = SECTOR;

var
  sector1: TSECTOR;     // Our Model Goes Here:

  MyFile: pchar;



function DrawGLScene(): boolean;
var
  x_m, y_m, z_m:  v16;
  u_m, v_m: t16;
  xtrans, ztrans, ytrans: cint32;
  sceneroty: cint;
  numtriangles: integer;
  loop_m: integer;
begin
  // Reset The View
  xtrans := -xpos;
  ztrans := -zpos;
  ytrans := -walkbias - (1 shl 10);
  sceneroty := LUT_SIZE - yrot;

  glLoadIdentity();

  glRotatef32i(lookupdown, (1 shl 12),          0, 0);
  glRotatef32i( sceneroty,          0, (1 shl 12), 0);

  glTranslatef32(xtrans, ytrans, ztrans);
  glBindTexture(GL_TEXTURE_2D, texture[0]);

  numtriangles := sector1.numtriangles;


  // Process Each Triangle
  for loop_m := 0 to numtriangles - 1 do
  begin
    glBegin(GL_TRIANGLES);
      glNormal(NORMAL_PACK( 0, 0, 1 shl 10));
      x_m := sector1.triangle[loop_m].vertex[0].x;
      y_m := sector1.triangle[loop_m].vertex[0].y;
      z_m := sector1.triangle[loop_m].vertex[0].z;
      u_m := sector1.triangle[loop_m].vertex[0].u;
      v_m := sector1.triangle[loop_m].vertex[0].v;
      glTexCoord2t16(u_m,v_m); glVertex3v16(x_m,y_m,z_m);

      x_m := sector1.triangle[loop_m].vertex[1].x;
      y_m := sector1.triangle[loop_m].vertex[1].y;
      z_m := sector1.triangle[loop_m].vertex[1].z;
      u_m := sector1.triangle[loop_m].vertex[1].u;
      v_m := sector1.triangle[loop_m].vertex[1].v;
      glTexCoord2t16(u_m,v_m); glVertex3v16(x_m,y_m,z_m);

      x_m := sector1.triangle[loop_m].vertex[2].x;
      y_m := sector1.triangle[loop_m].vertex[2].y;
      z_m := sector1.triangle[loop_m].vertex[2].z;
      u_m := sector1.triangle[loop_m].vertex[2].u;
      v_m := sector1.triangle[loop_m].vertex[2].v;
      glTexCoord2t16(u_m,v_m); glVertex3v16(x_m,y_m,z_m);
    glEnd();
  end;
  result := true;                   // Everything Went OK
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
      sector1.triangle[loop].vertex[vert].x := floattov16(x);
      sector1.triangle[loop].vertex[vert].y := floattov16(y);
      sector1.triangle[loop].vertex[vert].z := floattov16(z);
      sector1.triangle[loop].vertex[vert].u := floattot16(u*128);
      sector1.triangle[loop].vertex[vert].v := floattot16(v*128);
    end;
  end;
end;

// Load PCX files And Convert To Textures
function LoadGLTextures(): boolean;
var
  pcx: sImage;
begin
  //load our texture
  loadPCX(pcuint8(Mud_pcx), @pcx);

  image8to16(@pcx);

  glGenTextures(1, @texture[0]);
  glBindTexture(0, texture[0]);
  glTexImage2D(0, 0, GL_RGB, TEXTURE_SIZE_128 , TEXTURE_SIZE_128, 0, TEXGEN_TEXCOORD or GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T, pcx.image.data8);

  imageDestroy(@pcx);

  result := true;
end;

var
  held: integer;

begin
  MyFile := pchar(@World_txt);

  // Setup the Main screen for 3D
  videoSetMode(MODE_0_3D);
  vramSetBankA(VRAM_A_TEXTURE);                        //NEW  must set up some memory for textures

  // initialize the geometry engine
  glInit();

  // enable textures
  glEnable(GL_TEXTURE_2D);

  // enable antialiasing
  glEnable(GL_ANTIALIAS);

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

  glColor3f(1,1,1);

  glLight(0, RGB15(31,31,31) , 0, floattov10(-1.0), 0);

  //need to set up some material properties since DS does not have them set by default
  glMaterialf(GL_AMBIENT, RGB15(16,16,16));
  glMaterialf(GL_DIFFUSE, RGB15(16,16,16));
  glMaterialf(GL_SPECULAR, BIT(15) or RGB15(8,8,8));
  glMaterialf(GL_EMISSION, RGB15(16,16,16));

  //ds uses a table for shinyness..this generates a half-ass one
  glMaterialShinyness();

  //ds specific, several attributes can be set here
  glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE or POLY_FORMAT_LIGHT0);

  // Set the current matrix to be the model matrix
  glMatrixMode(GL_MODELVIEW);

  while true do
  begin
    //these little button functions are pretty handy
    scanKeys();
    held := keysHeld();

    if (held and KEY_A) <> 0 then lookupdown := lookupdown - 1;

    if (held and KEY_B) <> 0 then lookupdown := lookupdown + 1;

    if (held and KEY_LEFT) <> 0 then
    begin
      heading := heading + 64;
      yrot := heading;
    end;

    if (held and KEY_RIGHT) <> 0 then
    begin
      heading := heading - 64;
      yrot := heading;
    end;

    if (held and KEY_DOWN) <> 0 then
    begin

      xpos := xpos + sinLerp(heading) div 20;
      zpos := zpos + cosLerp(heading) div 20;

      walkbiasangle := walkbiasangle + 640;

      walkbias := sinLerp(walkbiasangle) div 20;
    end;

    if (held and KEY_UP) <> 0 then
    begin
      xpos := xpos - sinLerp(heading) div 20;
      zpos := zpos - cosLerp(heading) div 20;

      walkbiasangle := walkbiasangle - 640;

      walkbias := sinLerp(walkbiasangle) div 20;
    end;

    DrawGLScene();

    // flush to screen
    glFlush(0);

    // wait for the screen to refresh
    swiWaitForVBlank();
  end;

end.
