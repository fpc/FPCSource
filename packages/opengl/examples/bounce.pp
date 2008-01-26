{
  Bouncing ball demo.  Color index mode only!

  This program is in the public domain
  Brian Paul

  Converted to Pascal by Peter Vreman
}
program bounce;

{$mode objfpc}

uses
  gl,glut;

const
  RED=1;
  WHITE=2;
  CYAN=3;

var
  IndexMode : Boolean;
  Ball : GLuint;

const
  Zrot  : GLfloat = 0.0;
  Zstep : GLfloat = 6.0;
  Xpos  : GLfloat = 0.0;
  Ypos  : GLfloat = 1.0;
  Xvel  : GLfloat = 0.2;
  Yvel  : GLfloat = 0.0;
  Xmin  : GLfloat = -4.0;
  Xmax  : GLfloat = 4.0;
  Ymin  : GLfloat = -3.8;
  Ymax  : GLfloat = 4.0;
  G     : GLfloat = -0.1;


function make_ball:GLuint;
var
  list   : GLuint;
  a,b,
  ar,br  : GLFloat;
  da,db,
  dar    : GLFloat;
  radius : GLFloat;
  color  : boolean;
  x,y,z  : GLFloat;
begin
  da:=18.0;
  db:=18.0;
  radius:=1.0;

  list := glGenLists(1);

  glNewList(list, GL_COMPILE);

  color := false;
  a:=-90.0;
  while (a+da<=90.0) do
   begin
     glBegin(GL_QUAD_STRIP);

     b:=0.0;
     while (b<=360.0) do
      begin
        if (color) then
         begin
           glIndexi(RED);
           glColor3f(1, 0, 0);
         end
        else
         begin
           glIndexi(WHITE);
           glColor3f(1, 1, 1);
         end;

        ar:=a * 3.14159/180.0;
        br:=b * 3.14159/180.0;
        x := COS(br) * COS(ar);
        y := SIN(br) * COS(ar);
        z := SIN(ar);
        glVertex3f(x, y, z);

        dar:=da * 3.14159/180.0;
        x := radius * COS(br) * COS(ar + dar);
        y := radius * SIN(br) * COS(ar + dar);
        z := radius * SIN(ar + dar);
        glVertex3f(x, y, z);

        color := not color;
        b:=b+db;
      end;

     glEnd();
     a:=a+da;
   end;

  glEndList();

  make_ball:=list;
end;


procedure reshape(width,height:longint); cdecl;
var
  aspect : glFloat;
begin
  aspect := glfloat(width) / glfloat(height);
  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-6.0 * aspect, 6.0 * aspect, -6.0, 6.0, -6.0, 6.0);
  glMatrixMode(GL_MODELVIEW);
end;


procedure key(k:byte;x,y:longint); cdecl;
begin
  case k of
    27 :
      halt(0);
  end;
end;


procedure draw; cdecl;
var
  i : GLint;
begin
  glClear(GL_COLOR_BUFFER_BIT);

  glIndexi(CYAN);
  glColor3f(0, 1, 1);
  glBegin(GL_LINES);
  for i:=-5 to 5 do
   begin
     glVertex2i(i, -5);
     glVertex2i(i, 5);
   end;
  for i:=-5 to 5 do
   begin
     glVertex2i(-5, i);
     glVertex2i(5, i);
   end;
  for i:=-5 to 5 do
   begin
     glVertex2i(i, -5);
     glVertex2f(i * 1.15, -5.9);
   end;
  glVertex2f(-5.3, -5.35);
  glVertex2f(5.3, -5.35);
  glVertex2f(-5.75, -5.9);
  glVertex2f(5.75, -5.9);
  glEnd();

  glPushMatrix();
  glTranslatef(Xpos, Ypos, 0.0);
  glScalef(2.0, 2.0, 2.0);
  glRotatef(8.0, 0.0, 0.0, 1.0);
  glRotatef(90.0, 1.0, 0.0, 0.0);
  glRotatef(Zrot, 0.0, 0.0, 1.0);

  glCallList(Ball);

  glPopMatrix();

  glFlush();
  glutSwapBuffers();
end;


const
  vel0 : glfloat = -100.0;
procedure idle; cdecl;
begin
  Zrot:=Zrot+Zstep;
  Xpos:=Xpos+Xvel;
  if (Xpos >= Xmax) then
   begin
     Xpos := Xmax;
     Xvel := -Xvel;
     Zstep := -Zstep;
   end;
  if (Xpos <= Xmin) then
   begin
     Xpos := Xmin;
     Xvel := -Xvel;
     Zstep := -Zstep;
   end;
  Ypos:=Ypos+Yvel;
  Yvel:=Yvel+G;
  if (Ypos < Ymin) then
   begin
     Ypos := Ymin;
     if (vel0 = -100.0) then
       vel0 := abs(Yvel);
     Yvel := vel0;
   end;
  glutPostRedisplay();
end;


procedure visible(vis:longint); cdecl;
begin
  if (vis=GLUT_VISIBLE) then
    glutIdleFunc(@idle)
  else
    glutIdleFunc(nil);
end;


begin
  glutInit(@argc, argv);
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(600, 450);

  if paramcount>1 then
   IndexMode:=(paramstr(1)='-ci');

  if (IndexMode) then
     glutInitDisplayMode(GLUT_INDEX or GLUT_DOUBLE)
  else
     glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE);

  glutCreateWindow('Bounce');
  Ball := make_ball();
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  glDisable(GL_DITHER);
  glShadeModel(GL_FLAT);

  glutDisplayFunc(@draw);
  glutReshapeFunc(@reshape);
  glutVisibilityFunc(@visible);
  glutKeyboardFunc(@key);

  if (IndexMode) then
   begin
     glutSetColor(RED, 1.0, 0.0, 0.0);
     glutSetColor(WHITE, 1.0, 1.0, 1.0);
     glutSetColor(CYAN, 0.0, 1.0, 1.0);
   end;

  glutMainLoop();
end.
