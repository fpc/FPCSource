{
  GL units for Free Pascal - GLUT demo
  1999 Sebastian Guenther, sguenther@gmx.de

  You may use this source as starting point for your own programs; consider it
  as Public Domain.
}

{$mode objfpc}

program GLUTDemo;
uses
  GL, GLU, GLUT;

const

  FPCImg: array[0..4, 0..10] of Byte =
    ((1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1),
     (1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0),
     (1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0),
     (1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0),
     (1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1));

var
  counter: Integer;


const
  colors: array[0..7, 0..2] of Single =
    ((0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1),
     (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1));
  corners: array[0..7, 0..2] of Single =
    ((-1, -1, -1), (+1, -1, -1), (+1, +1, -1), (-1, +1, -1),
     (-1, -1, +1), (+1, -1, +1), (+1, +1, +1), (-1, +1, +1));


procedure DrawCube;
  procedure DrawSide(i1, i2, i3, i4: Integer);
  begin
    glColor4f (colors [i1, 0], colors [i1, 1], colors [i1, 2], 0.5);
    glVertex3f(corners[i1, 0], corners[i1, 1], corners[i1, 2]);
    glColor4f (colors [i2, 0], colors [i2, 1], colors [i2, 2], 0.5);
    glVertex3f(corners[i2, 0], corners[i2, 1], corners[i2, 2]);
    glColor4f (colors [i3, 0], colors [i3, 1], colors [i3, 2], 0.5);
    glVertex3f(corners[i3, 0], corners[i3, 1], corners[i3, 2]);

    glVertex3f(corners[i4, 0], corners[i4, 1], corners[i4, 2]);
  end;
begin
  glBegin(GL_QUADS);
  DrawSide(4, 5, 6, 7);         // Front
  DrawSide(3, 2, 1, 0);         // Back
  DrawSide(2, 3, 7, 6);         // Top
  DrawSide(0, 1, 5, 4);         // Bottom
  DrawSide(4, 7, 3, 0);         // Left
  DrawSide(1, 2, 6, 5);         // Right
  glEnd;
end;


procedure DisplayWindow; cdecl;
var
  x, y: Integer;
begin
  Inc(counter);

  glClearColor(0, 0, 0.2, 1);
  glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT);

  glPushMatrix;
  glTranslatef(0, 0, Sin(Single(counter) / 20.0) * 5.0 - 5.0);
  glRotatef(Sin(Single(counter) / 200.0) * 720.0, 0, 1, 0);
  glRotatef(counter, 0, 0, 1);

  for y := 0 to 4 do
    for x := 0 to 10 do
      if FPCImg[y, x] > 0 then begin
        glPushMatrix;
        glRotatef(x * Sin(Single(counter) / 5.0), 0, 1, 0);
        glRotatef(y * Sin(Single(counter) / 12.0) * 4.0, 0, 0, 1);
        glTranslatef((x - 5) * 1, (2 - y) * 1, 0);
        glScalef(0.4, 0.4, 0.4);
        glRotatef(counter, 0.5, 1, 0);
        DrawCube;
        glPopMatrix;
      end;

  glPopMatrix;

  Inc(counter);

  glutSwapBuffers;
end;

procedure OnTimer(value: Integer); cdecl;
begin
  glutPostRedisplay;
  glutTimerFunc(20, @OnTimer, 0);
end;

begin
  glutInit(@argc, argv);

  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);
  glutCreateWindow('Free Pascal GLUT demo');
  glutDisplayFunc(@DisplayWindow);
  glutTimerFunc(20, @OnTimer, 0);

  WriteLn;
  WriteLn('GL info:');
  WriteLn('  Vendor: ', PChar(glGetString(GL_VENDOR)));
  WriteLn('  Renderer: ', PChar(glGetString(GL_RENDERER)));
  WriteLn('  Version: ', PChar(glGetString(GL_VERSION)));
  WriteLn('  Extensions: ', PChar(glGetString(GL_EXTENSIONS)));

  // Enable backface culling
  glEnable(GL_CULL_FACE);

  // Set up depth buffer
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  // Set up projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(90, 1.3, 0.1, 100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glTranslatef(0, 0, -5.5);

  WriteLn('Starting...');
  glutMainLoop;

end.
