{
  GL units for Free Pascal - GLUT demo
  1999 Sebastian Guenther, sguenther@gmx.de
  2008 Jonas Maebe (converted to use vertex arrays)

  You may use this source as starting point for your own programs; consider it
  as Public Domain.
}

{$mode objfpc}

program GLUTDemoVA;
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

(*
    3 __ 2
    /|  |
  7/0|_ |1
   |/  /
   +--+
   4  5

back plane: 0-3-2-1
*)

const
  colors: array[0..7, 0..2] of Single =
    ((0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1),
     (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1));
  corners: array[0..7, 0..2] of Single =
    ((-1, -1, -1), (+1, -1, -1), (+1, +1, -1), (-1, +1, -1),
     (-1, -1, +1), (+1, -1, +1), (+1, +1, +1), (-1, +1, +1));

  indices: array[0..5] of array[0..3] of GLubyte =
    (
     { all vertices must be in ccw order }
     (0,3,1,2), // back
     (4,5,7,6), // front
     (3,0,7,4), // left
     (1,2,5,6), // right
     (3,7,2,6), // top
     (5,4,1,0)  // bottom
    );

procedure DrawCube;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);

  glVertexPointer(3,GL_FLOAT,0,@corners);
  glColorPointer(3,GL_FLOAT,0,@colors);

  // this will also draw a bunch of triangles inside the cube, but
  // may still be faster than the commented-out sequence below due
  // to less calls into the renderer
  glDrawElements(GL_TRIANGLE_STRIP,24,GL_UNSIGNED_BYTE,@indices[0]);

(*
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[0]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[1]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[2]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[3]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[4]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[5]);
*)
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
end;


procedure DisplayWindow; cdecl;
var
  x, y: Integer;
begin
  Inc(counter);

//  counter:=145;
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

//  Inc(counter);

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
