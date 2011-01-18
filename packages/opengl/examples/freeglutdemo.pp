{ Trivial demo of some freeglut extensions, by Michalis Kamburelis,
  parts based on glutdemo.pp. Public domain.

  freeglut features:
  - when you press escape key, program returns gracefully to main begin...end.
  - we show special geometric objects: Sierpinski sponge, cylinder.
  - mouse wheel up/down can be used to zoom in/out.
}

{$mode objfpc}

program FreeGlutDemo;

uses
  GL, GLU, GLUT, FreeGlut;

var
  T: GLFloat;
  Zoom: GLFloat = -3;

procedure Display; cdecl;
const
  Offset: TGLDouble3 = (0, 0, 0);
begin
  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

  glPushMatrix;
    glTranslatef(-1, -1, Zoom);
    glRotatef(T, 0, 1, 0);
    glutWireCylinder(0.5, 1, 32, 8);
  glPopMatrix;

  glPushMatrix;
    glTranslatef(-1,  1, Zoom);
    glRotatef(T, 0, 1, 0);
    glutSolidCylinder(0.5, 1, 32, 8);
  glPopMatrix;

  glPushMatrix;
    glTranslatef(1, -1, Zoom);
    glRotatef(T, 0, 1, 0);
    glutWireSierpinskiSponge(3, @Offset, 1);
  glPopMatrix;

  glPushMatrix;
    glTranslatef(1,  1, Zoom);
    glRotatef(T, 0, 1, 0);
    glutSolidSierpinskiSponge(3, @Offset, 1);
  glPopMatrix;

  glutSwapBuffers;
end;

procedure Timer(Value: Integer); cdecl;
begin
  glutPostRedisplay;
  T := T + 1.0;
  glutTimerFunc(20, @Timer, 0);
end;

procedure Key(K: Byte; X, Y: Integer); cdecl;
begin
  case K of
    27: glutLeaveMainLoop(); // using freeglut you can exit cleanly
  end;
end;

procedure Wheel(Wheel, Direction, X, Y: Integer); cdecl;
begin
  if Wheel = 0 then
  begin
    Zoom := Zoom + Direction / 2;
    glutPostRedisplay();
  end;
end;

begin
  glutInit(@argc, argv);
  glutInitWindowSize(400, 400);
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);
  glutCreateWindow('FreeGlut demo');

  glutDisplayFunc(@Display);
  glutTimerFunc(20, @Timer, 0);
  glutKeyboardFunc(@Key);
  glutMouseWheelFunc(@Wheel);

  glEnable(GL_CULL_FACE); // Enable backface culling

  // Set up depth buffer
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  // Set up projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(90, 1.3, 0.1, 100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_CONTINUE_EXECUTION);

  WriteLn('Starting...');
  glutMainLoop;
  Writeln('glutMainLoop finished');
end.
