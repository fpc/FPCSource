{
  GL units for Free Pascal - GLUT demo
  1999 Sebastian Guenther, sguenther@gmx.de
  2008 Jonas Maebe (converted to use vertex arrays)

  You may use this source as starting point for your own programs; consider it
  as Public Domain.
}

{$mode objfpc}

library GLUTDemoES;
uses
  gles11, math;

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
  colors: array[0..7, 0..3] of Single =
    ((0, 0, 0, 1), (0, 0, 1, 1), (0, 1, 0, 1), (0, 1, 1, 1),
     (1, 0, 0, 1), (1, 0, 1, 1), (1, 1, 0, 1), (1, 1, 1, 1));
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
  if (glGetError() <> GL_NO_ERROR) then
    halt(10);
  glEnableClientState(GL_COLOR_ARRAY);
  if (glGetError() <> GL_NO_ERROR) then
    halt(11);

  glVertexPointer(3,GL_FLOAT,0,@corners);
  if (glGetError() <> GL_NO_ERROR) then
    halt(12);
  glColorPointer(4,GL_FLOAT,0,@colors);
  if (glGetError() <> GL_NO_ERROR) then
    halt(13);

  // this will also draw a bunch of triangles inside the cube, but
  // may still be faster than the commented-out sequence below due
  // to less calls into the renderer
  glDrawElements(GL_TRIANGLE_STRIP,24,GL_UNSIGNED_BYTE,@indices[0]);
  if (glGetError() <> GL_NO_ERROR) then
    halt(14);

(*
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[0]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[1]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[2]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[3]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[4]);
  glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_BYTE,@indices[5]);
*)
  glDisableClientState(GL_VERTEX_ARRAY);
  if (glGetError() <> GL_NO_ERROR) then
    halt(15);
  glDisableClientState(GL_COLOR_ARRAY);
  if (glGetError() <> GL_NO_ERROR) then
    halt(16);
end;


procedure glDraw; cdecl; export;
var
  x, y: Integer;
begin
  Inc(counter);

//  counter:=145;
  glClearColor(0, 0, 0.2, 1);
  if (glGetError() <> GL_NO_ERROR) then
    halt(8);
  glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT);
  if (glGetError() <> GL_NO_ERROR) then
    halt(9);

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
end;


{ gluperspective replacement, from http://iphonedevelopment.blogspot.com/2008/12/gluperspective.html }

procedure gluperspective(fovy, aspect, znear, zfar: GLfloat);
var
  xmin, xmax, ymin, ymax: GLfloat;
begin
// Start in projection mode.
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
 ymax := zNear * tan(fovy * pi / 360.0);
 ymin := -ymax;
 xmin := ymin * aspect;
 xmax := ymax * aspect;
 glfrustumf(xmin, xmax, ymin, ymax, zNear, zFar);
end;


procedure glInit; cdecl; export;
begin
  // Enable backface culling
  glEnable(GL_CULL_FACE);
  if (glGetError() <> GL_NO_ERROR) then
    halt(1);

  // Set up depth buffer
  glEnable(GL_DEPTH_TEST);
  if (glGetError() <> GL_NO_ERROR) then
    halt(2);
  glDepthFunc(GL_LESS);
  if (glGetError() <> GL_NO_ERROR) then
    halt(3);

  // Set up projection matrix
  glMatrixMode(GL_PROJECTION);
  if (glGetError() <> GL_NO_ERROR) then
    halt(4);
  glLoadIdentity;
  gluPerspective(90, 1.3, 0.1, 100);
  if (glGetError() <> GL_NO_ERROR) then
    halt(5);
  glMatrixMode(GL_MODELVIEW);
  if (glGetError() <> GL_NO_ERROR) then
    halt(6);
  glLoadIdentity;
  glTranslatef(0, 0, -5.5);
  if (glGetError() <> GL_NO_ERROR) then
    halt(7);
end;


end.
