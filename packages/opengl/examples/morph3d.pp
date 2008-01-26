{
 *-
 * morph3d.c - Shows 3D morphing objects
 *
 * Converted to GLUT by brianp on 1/1/98
 * Converted to FreePascal by Peter Vreman on 9/3/2000
 *
 * This program was inspired on a WindowsNT(R)'s screen saver. It was written
 * from scratch and it was not based on any other source code.
 *
 * Porting it to xlock (the final objective of this code since the moment I
 * decided to create it) was possible by comparing the original Mesa's gear
 * demo with it's ported version, so thanks for Danny Sung for his indirect
 * help (look at gear.c in xlock source tree). NOTE: At the moment this code
 * was sent to Brian Paul for package inclusion, the XLock Version was not
 * available. In fact, I'll wait it to appear on the next Mesa release (If you
 * are reading this, it means THIS release) to send it for xlock package
 * inclusion). It will probably there be a GLUT version too.
 *
 * Thanks goes also to Brian Paul for making it possible and inexpensive
 * to use OpenGL at home. *
 * Since I'm not a native english speaker, my apologies for any gramatical
 * mistake.
 *
 * My e-mail addresses are
 *
 * vianna@cat.cbpf.br
 *         and
 * marcelo@venus.rdc.puc-rio.br
 *
 * Marcelo F. Vianna (Feb-13-1997)
 */
}
program morph3d;

{$mode objfpc}

{
This document is VERY incomplete, but tries to describe the mathematics used
in the program. At this moment it just describes how the polyhedra are
generated. On futhurer versions, this document will be probabbly improved.

Since I'm not a native english speaker, my apologies for any gramatical
mistake.

Marcelo Fernandes Vianna
- Undergraduate in Computer Engeneering at Catholic Pontifical University
- of Rio de Janeiro (PUC-Rio) Brasil.
- e-mail: vianna@cat.cbpf.br or marcelo@venus.rdc.puc-rio.br
- Feb-13-1997

POLYHEDRA GENERATION

For the purpose of this program it's not sufficient to know the polyhedra
vertexes coordinates. Since the morphing algorithm applies a nonlinear
transformation over the surfaces (faces) of the polyhedron, each face has
to be divided into smaller ones. The morphing algorithm needs to transform
each vertex of these smaller faces individually. It's a very time consoming
task.

In order to reduce calculation overload, and since all the macro faces of
the polyhedron are transformed by the same way, the generation is made by
creating only one face of the polyhedron, morphing it and then rotating it
around the polyhedron center.

What we need to know is the face radius of the polyhedron (the radius of
the inscribed sphere) and the angle between the center of two adjacent
faces using the center of the sphere as the angle's vertex.

The face radius of the regular polyhedra are known values which I decided
to not waste my time calculating. Following is a table of face radius for
the regular polyhedra with edge length = 1:

    TETRAHEDRON  : 1/(2*sqrt(2))/sqrt(3)
    CUBE         : 1/2
    OCTAHEDRON   : 1/sqrt(6)
    DODECAHEDRON : T^2 * sqrt((T+2)/5) / 2     -> where T=(sqrt(5)+1)/2
    ICOSAHEDRON  : (3*sqrt(3)+sqrt(15))/12

I've not found any reference about the mentioned angles, so I needed to
calculate them, not a trivial task until I figured out how :)
Curiously these angles are the same for the tetrahedron and octahedron.
A way to obtain this value is inscribing the tetrahedron inside the cube
by matching their vertexes. So you'll notice that the remaining unmatched
vertexes are in the same straight line starting in the cube/tetrahedron
center and crossing the center of each tetrahedron's face. At this point
it's easy to obtain the bigger angle of the isosceles triangle formed by
the center of the cube and two opposite vertexes on the same cube face.
The edges of this triangle have the following lenghts: sqrt(2) for the base
and sqrt(3)/2 for the other two other edges. So the angle we want is:
     +-----------------------------------------------------------+
     | 2*ARCSIN(sqrt(2)/sqrt(3)) = 109.47122063449069174 degrees |
     +-----------------------------------------------------------+
For the cube this angle is obvious, but just for formality it can be
easily obtained because we also know it's isosceles edge lenghts:
sqrt(2)/2 for the base and 1/2 for the other two edges. So the angle we
want is:
     +-----------------------------------------------------------+
     | 2*ARCSIN((sqrt(2)/2)/1)   = 90.000000000000000000 degrees |
     +-----------------------------------------------------------+
For the octahedron we use the same idea used for the tetrahedron, but now
we inscribe the cube inside the octahedron so that all cubes's vertexes
matches excatly the center of each octahedron's face. It's now clear that
this angle is the same of the thetrahedron one:
     +-----------------------------------------------------------+
     | 2*ARCSIN(sqrt(2)/sqrt(3)) = 109.47122063449069174 degrees |
     +-----------------------------------------------------------+
For the dodecahedron it's a little bit harder because it's only relationship
with the cube is useless to us. So we need to solve the problem by another
way. The concept of Face radius also exists on 2D polygons with the name
Edge radius:
  Edge Radius For Pentagon (ERp)
  ERp = (1/2)/TAN(36 degrees) * VRp = 0.6881909602355867905
  (VRp is the pentagon's vertex radio).
  Face Radius For Dodecahedron
  FRd = T^2 * sqrt((T+2)/5) / 2 = 1.1135163644116068404
Why we need ERp? Well, ERp and FRd segments forms a 90 degrees angle,
completing this triangle, the lesser angle is a half of the angle we are
looking for, so this angle is:
     +-----------------------------------------------------------+
     | 2*ARCTAN(ERp/FRd)         = 63.434948822922009981 degrees |
     +-----------------------------------------------------------+
For the icosahedron we can use the same method used for dodecahedron (well
the method used for dodecahedron may be used for all regular polyhedra)
  Edge Radius For Triangle (this one is well known: 1/3 of the triangle height)
  ERt = sin(60)/3 = sqrt(3)/6 = 0.2886751345948128655
  Face Radius For Icosahedron
  FRi= (3*sqrt(3)+sqrt(15))/12 = 0.7557613140761707538
So the angle is:
     +-----------------------------------------------------------+
     | 2*ARCTAN(ERt/FRi)         = 41.810314895778596167 degrees |
     +-----------------------------------------------------------+
}

uses
  GL,GLUT;

type
  float = single;

const
  Scale = 0.3;

// #define sqr(A)                     ((A)*(A))

// Increasing this values produces better image quality, the price is speed.
// Very low values produces erroneous/incorrect plotting
  tetradivisions            = 23;
  cubedivisions             = 20;
  octadivisions             = 21;
  dodecadivisions           = 10;
  icodivisions              = 15;

  tetraangle                = 109.47122063449069174;
  cubeangle                 = 90.000000000000000000;
  octaangle                 = 109.47122063449069174;
  dodecaangle               = 63.434948822922009981;
  icoangle                  = 41.810314895778596167;

//#define Pi                         3.1415926535897932385
  SQRT2                     = 1.4142135623730951455;
  SQRT3                     = 1.7320508075688771932;
  SQRT5                     = 2.2360679774997898051;
  SQRT6                     = 2.4494897427831778813;
  SQRT15                    = 3.8729833462074170214;
  cossec36_2                = 0.8506508083520399322;
  cos72                     = 0.3090169943749474241;
  sin72                     = 0.9510565162951535721;
  cos36                     = 0.8090169943749474241;
  sin36                     = 0.5877852522924731292;

{*************************************************************************}

const
  mono   : boolean=false;
  smooth : boolean=true;
var
  WindH,WindW   : GLint;
  step,seno     : GLFloat;
  _object       : glint;
  edgedivisions : glint;
  draw_object   : procedure;
  Magnitude     : glfloat;
  MaterialColor : array[0..19] of pglfloat;

const
  front_shininess : array[0..0] of glfloat = (60.0);
  front_specular  : array[0..3] of glfloat = ( 0.7, 0.7, 0.7, 1.0 );
  ambient         : array[0..3] of glfloat = ( 0.0, 0.0, 0.0, 1.0 );
  diffuse         : array[0..3] of glfloat = ( 1.0, 1.0, 1.0, 1.0 );
  position0       : array[0..3] of glfloat = ( 1.0, 1.0, 1.0, 0.0 );
  position1       : array[0..3] of glfloat = (-1.0,-1.0, 1.0, 0.0 );
  lmodel_ambient  : array[0..3] of glfloat = ( 0.5, 0.5, 0.5, 1.0 );
  lmodel_twoside  : array[0..0] of glfloat = (GL_TRUE);

  MaterialRed     : array[0..3] of glfloat = ( 0.7, 0.0, 0.0, 1.0 );
  MaterialGreen   : array[0..3] of glfloat = ( 0.1, 0.5, 0.2, 1.0 );
  MaterialBlue    : array[0..3] of glfloat = ( 0.0, 0.0, 0.7, 1.0 );
  MaterialCyan    : array[0..3] of glfloat = ( 0.2, 0.5, 0.7, 1.0 );
  MaterialYellow  : array[0..3] of glfloat = ( 0.7, 0.7, 0.0, 1.0 );
  MaterialMagenta : array[0..3] of glfloat = ( 0.6, 0.2, 0.5, 1.0 );
  MaterialWhite   : array[0..3] of glfloat = ( 0.7, 0.7, 0.7, 1.0 );
  MaterialGray    : array[0..3] of glfloat = ( 0.2, 0.2, 0.2, 1.0 );


procedure TRIANGLE(Edge,Amp:GLFloat; Divisions: longint; Z:GLFloat);
var
  Xf,Yf,Xa,Yb,Xf2,Yf2 : Extended;
  Factor,Factor1,Factor2 : GLfloat;
  VertX,VertY,VertZ,NeiAX,NeiAY,NeiAZ,NeiBX,NeiBY,NeiBZ : GLfloat;
  Ax,Ay,Bx : GLfloat;
  Ri,Ti : longint;
  Vr : GLfloat;
  AmpVr2 : GLfloat;
  Zf : GLfloat;
begin
  Vr:=(Edge)*SQRT3/3;
  AmpVr2:=(Amp)/sqr(Vr);
  Zf:=(Edge)*(Z);

  Ax:=(Edge)*(+0.5/(Divisions));
  Ay:=(Edge)*(-SQRT3/(2*Divisions));
  Bx:=(Edge)*(-0.5/(Divisions));

  for Ri:=1 to Divisions do
   begin
    glBegin(GL_TRIANGLE_STRIP);
    for Ti:=0 to Ri-1 do
     begin
      Xf:=(Ri-Ti)*Ax + Ti*Bx;
      Yf:=Vr+(Ri-Ti)*Ay + Ti*Ay;
      Xa:=Xf+0.001; Yb:=Yf+0.001;
      Xf2:=sqr(Xf);
      Yf2:=sqr(Yf);
      Factor:=1-(((Xf2)+(Yf2))*AmpVr2);
      Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
      Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
      VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
      NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
      NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
      glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
      glVertex3f(VertX, VertY, VertZ);

      Xf:=(Ri-Ti-1)*Ax + Ti*Bx;
      Yf:=Vr+(Ri-Ti-1)*Ay + Ti*Ay;
      Xa:=Xf+0.001; Yb:=Yf+0.001;
      Xf2:=sqr(Xf);
      Yf2:=sqr(Yf);
      Factor:=1-(((Xf2)+(Yf2))*AmpVr2);
      Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
      Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
      VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
      NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
      NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
      glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
      glVertex3f(VertX, VertY, VertZ);
     end;
    Xf:=Ri*Bx;
    Yf:=Vr+Ri*Ay;
    Xa:=Xf+0.001; Yb:=Yf+0.001;
    Xf2:=sqr(Xf);
    Yf2:=sqr(Yf);
    Factor:=1-((Xf2+Yf2)*AmpVr2);
    Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
    Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
    VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
    NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
    NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
    glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
    glVertex3f(VertX, VertY, VertZ);
    glEnd();
  end;
end;


procedure SQUARE(Edge,Amp:GLFloat; Divisions: longint; Z:GLFloat);
var
  Xi,Yi : longint;
  Xf,Yf,Y,Y2,Xf2,Yf2,Xa,Yb : GLfloat;
  Factor,Factor1,Factor2 : GLfloat;
  VertX,VertY,VertZ,NeiAX,NeiAY,NeiAZ,NeiBX,NeiBY,NeiBZ : GLfloat;
  AmpVr2 : GLfloat;
  Zf : GLfloat;
begin
  AmpVr2:=(Amp)/sqr((Edge)*SQRT2/2);
  Zf:=(Edge)*(Z);

  for Yi:=0 to Divisions-1 do
   begin
    Yf:=-((Edge)/2.0) + (Yi)/(Divisions)*(Edge);
    Yf2:=sqr(Yf);
    Y:=Yf+1.0/(Divisions)*(Edge);
    Y2:=sqr(Y);
    glBegin(GL_QUAD_STRIP);
    for Xi:=0 to Divisions do
     begin
      Xf:=-((Edge)/2.0) + (Xi)/(Divisions)*(Edge);
      Xf2:=sqr(Xf);

      Xa:=Xf+0.001; Yb:=Y+0.001;
      Factor:=1-((Xf2+Y2)*AmpVr2);
      Factor1:=1-((sqr(Xa)+Y2)*AmpVr2);
      Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
      VertX:=Factor*Xf;        VertY:=Factor*Y;         VertZ:=Factor*Zf;
      NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Y-VertY;  NeiAZ:=Factor1*Zf-VertZ;
      NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
      glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
      glVertex3f(VertX, VertY, VertZ);

      Xa:=Xf+0.001; Yb:=Yf+0.001;
      Factor:=1-((Xf2+Yf2)*AmpVr2);
      Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
      Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
      VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
      NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
      NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
      glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
      glVertex3f(VertX, VertY, VertZ);
     end;
    glEnd();
  end;
end;

procedure PENTAGON(Edge,Amp:GLFloat; Divisions: longint; Z:GLFloat);
var
  Ri,Ti,Fi : longint;
  Xf,Yf,Xf2,Yf2,Xa,Yb : GLfloat;
  X,Y : array[0..5] of GLFloat;
  Factor,Factor1,Factor2 : GLfloat;
  VertX,VertY,VertZ,NeiAX,NeiAY,NeiAZ,NeiBX,NeiBY,NeiBZ : GLfloat;
  AmpVr2 : GLfloat;
  Zf : GLfloat;
begin
  AmpVr2:=(Amp)/sqr((Edge)*cossec36_2);
  Zf:=(Edge)*(Z);

  for Fi:=0 to 5 do
   begin
    x[Fi]:=-cos( Fi*2*Pi/5 + Pi/10 )/(Divisions)*cossec36_2*(Edge);
    y[Fi]:=sin( Fi*2*Pi/5 + Pi/10 )/(Divisions)*cossec36_2*(Edge);
   end;

  for Ri:=1 to Divisions do
   begin
    for Fi:=0 to 4 do
     begin
      glBegin(GL_TRIANGLE_STRIP);
      for Ti:=0 to Ri-1 do
       begin
        Xf:=(Ri-Ti)*x[Fi] + Ti*x[Fi+1];
        Yf:=(Ri-Ti)*y[Fi] + Ti*y[Fi+1];
        Xa:=Xf+0.001; Yb:=Yf+0.001;
        Xf2:=sqr(Xf);
        Yf2:=sqr(Yf);
        Factor:=1-(((Xf2)+(Yf2))*AmpVr2);
        Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
        Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
        VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
        NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
        NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
        glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
        glVertex3f(VertX, VertY, VertZ);

        Xf:=(Ri-Ti-1)*x[Fi] + Ti*x[Fi+1];
        Yf:=(Ri-Ti-1)*y[Fi] + Ti*y[Fi+1];
        Xa:=Xf+0.001; Yb:=Yf+0.001;
        Xf2:=sqr(Xf);
        Yf2:=sqr(Yf);
        Factor:=1-(((Xf2)+(Yf2))*AmpVr2);
        Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
        Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
        VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
        NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
        NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
        glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
        glVertex3f(VertX, VertY, VertZ);
       end;
      Xf:=Ri*x[Fi+1];
      Yf:=Ri*y[Fi+1];
      Xa:=Xf+0.001; Yb:=Yf+0.001;
      Xf2:=sqr(Xf);
      Yf2:=sqr(Yf);
      Factor:=1-(((Xf2)+(Yf2))*AmpVr2);
      Factor1:=1-((sqr(Xa)+Yf2)*AmpVr2);
      Factor2:=1-((Xf2+sqr(Yb))*AmpVr2);
      VertX:=Factor*Xf;        VertY:=Factor*Yf;        VertZ:=Factor*Zf;
      NeiAX:=Factor1*Xa-VertX; NeiAY:=Factor1*Yf-VertY; NeiAZ:=Factor1*Zf-VertZ;
      NeiBX:=Factor2*Xf-VertX; NeiBY:=Factor2*Yb-VertY; NeiBZ:=Factor2*Zf-VertZ;
      glNormal3f(NeiAY*NeiBZ-NeiAZ*NeiBY,NeiAZ*NeiBX-NeiAX*NeiBZ,NeiAX*NeiBY-NeiAY*NeiBX);
      glVertex3f(VertX, VertY, VertZ);
      glEnd();
     end;
   end;
end;


procedure draw_tetra;
var
  list : GLuint;
begin
  list := glGenLists( 1 );
  glNewList( list, GL_COMPILE );
  TRIANGLE(2,seno,edgedivisions,0.5/SQRT6);
  glEndList();

  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[0]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-tetraangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[1]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+tetraangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[2]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+tetraangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[3]);
  glCallList(list);

  glDeleteLists(list,1);
end;


procedure draw_cube;
var
  list : GLuint;
begin
  list := glGenLists( 1 );
  glNewList( list, GL_COMPILE );
  SQUARE(2, seno, edgedivisions, 0.5);
  glEndList();

  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[0]);
  glCallList(list);
  glRotatef(cubeangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[1]);
  glCallList(list);
  glRotatef(cubeangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[2]);
  glCallList(list);
  glRotatef(cubeangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[3]);
  glCallList(list);
  glRotatef(cubeangle,0,1,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[4]);
  glCallList(list);
  glRotatef(2*cubeangle,0,1,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[5]);
  glCallList(list);

  glDeleteLists(list,1);
end;


procedure draw_octa;
var
  list : GLuint;
begin
  list := glGenLists( 1 );
  glNewList( list, GL_COMPILE );
  TRIANGLE(2,seno,edgedivisions,1/SQRT6);
  glEndList();

  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[0]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-180+octaangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[1]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-octaangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[2]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-octaangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[3]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[4]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-180+octaangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[5]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-octaangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[6]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-octaangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[7]);
  glCallList(list);

  glDeleteLists(list,1);
end;


procedure draw_dodeca;
const
  TAU = ((SQRT5+1)/2);
var
  list : GLuint;
begin
  list := glGenLists( 1 );
  glNewList( list, GL_COMPILE );
  PENTAGON(1,seno,edgedivisions,sqr(TAU) * sqrt((TAU+2)/5) / 2);
  glEndList();

  glPushMatrix();
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[0]);
  glCallList(list);
  glRotatef(180,0,0,1);
  glPushMatrix();
  glRotatef(-dodecaangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[1]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(-dodecaangle,cos72,sin72,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[2]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(-dodecaangle,cos72,-sin72,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[3]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(dodecaangle,cos36,-sin36,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[4]);
  glCallList(list);
  glPopMatrix();
  glRotatef(dodecaangle,cos36,sin36,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[5]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[6]);
  glCallList(list);
  glRotatef(180,0,0,1);
  glPushMatrix();
  glRotatef(-dodecaangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[7]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(-dodecaangle,cos72,sin72,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[8]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(-dodecaangle,cos72,-sin72,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[9]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(dodecaangle,cos36,-sin36,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[10]);
  glCallList(list);
  glPopMatrix();
  glRotatef(dodecaangle,cos36,sin36,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[11]);
  glCallList(list);

  glDeleteLists(list,1);
end;


procedure draw_ico;
var
  list : GLuint;
begin
  list := glGenLists( 1 );
  glNewList( list, GL_COMPILE );
  TRIANGLE(1.5,seno,edgedivisions,(3*SQRT3+SQRT15)/12);
  glEndList();

  glPushMatrix();

  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[0]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-icoangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[1]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[2]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[3]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[4]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[5]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-icoangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[6]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[7]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[8]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-icoangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[9]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[10]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-icoangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[11]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[12]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[13]);
  glCallList(list);
  glPopMatrix();
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[14]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[15]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-icoangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[16]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[17]);
  glCallList(list);
  glPushMatrix();
  glRotatef(180,0,1,0);
  glRotatef(-180+icoangle,0.5,-SQRT3/2,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[18]);
  glCallList(list);
  glPopMatrix();
  glRotatef(180,0,0,1);
  glRotatef(-icoangle,1,0,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, MaterialColor[19]);
  glCallList(list);

  glDeleteLists(list,1);
end;


procedure do_draw; cdecl;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  glPushMatrix();

    glTranslatef( 0.0, 0.0, -10.0 );
    glScalef( Scale*WindH/WindW, Scale, Scale );
    glTranslatef(2.5*WindW/WindH*sin(step*1.11),2.5*cos(step*1.25*1.11),0);
    glRotatef(step*100,1,0,0);
    glRotatef(step*95,0,1,0);
    glRotatef(step*90,0,0,1);

  seno:=(sin(step)+1.0/3.0)*(4.0/5.0)*Magnitude;

  draw_object();

  glPopMatrix();

  glFlush();

  glutSwapBuffers();

  step:=step+0.05;
end;


procedure do_idle; cdecl;
begin
  glutPostRedisplay();
end;


procedure do_reshape(width,height:longint); cdecl;
begin
  WindW:=width;
  WindH:=height;
  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum( -1.0, 1.0, -1.0, 1.0, 5.0, 15.0 );
  glMatrixMode(GL_MODELVIEW);
end;


procedure pinit;
var
  loop : longint;
begin
  case _object of
    1 :
    begin
      draw_object:=@draw_tetra;
      MaterialColor[0]:=@MaterialRed;
      MaterialColor[1]:=@MaterialGreen;
      MaterialColor[2]:=@MaterialBlue;
      MaterialColor[3]:=@MaterialWhite;
      edgedivisions:=tetradivisions;
      Magnitude:=2.5;
    end;
    2:
    begin
      draw_object:=@draw_cube;
      MaterialColor[0]:=@MaterialRed;
      MaterialColor[1]:=@MaterialGreen;
      MaterialColor[2]:=@MaterialCyan;
      MaterialColor[3]:=@MaterialMagenta;
      MaterialColor[4]:=@MaterialYellow;
      MaterialColor[5]:=@MaterialBlue;
      edgedivisions:=cubedivisions;
      Magnitude:=2.0;
    end;
    3:
    begin
      draw_object:=@draw_octa;
      MaterialColor[0]:=MaterialRed;
      MaterialColor[1]:=MaterialGreen;
      MaterialColor[2]:=MaterialBlue;
      MaterialColor[3]:=MaterialWhite;
      MaterialColor[4]:=MaterialCyan;
      MaterialColor[5]:=MaterialMagenta;
      MaterialColor[6]:=MaterialGray;
      MaterialColor[7]:=MaterialYellow;
      edgedivisions:=octadivisions;
      Magnitude:=2.5;
    end;
    4:
    begin
      draw_object:=@draw_dodeca;
      MaterialColor[ 0]:=MaterialRed;
      MaterialColor[ 1]:=MaterialGreen;
      MaterialColor[ 2]:=MaterialCyan;
      MaterialColor[ 3]:=MaterialBlue;
      MaterialColor[ 4]:=MaterialMagenta;
      MaterialColor[ 5]:=MaterialYellow;
      MaterialColor[ 6]:=MaterialGreen;
      MaterialColor[ 7]:=MaterialCyan;
      MaterialColor[ 8]:=MaterialRed;
      MaterialColor[ 9]:=MaterialMagenta;
      MaterialColor[10]:=MaterialBlue;
      MaterialColor[11]:=MaterialYellow;
      edgedivisions:=dodecadivisions;
      Magnitude:=2.0;
    end;
    5:
    begin
      draw_object:=@draw_ico;
      MaterialColor[ 0]:=MaterialRed;
      MaterialColor[ 1]:=MaterialGreen;
      MaterialColor[ 2]:=MaterialBlue;
      MaterialColor[ 3]:=MaterialCyan;
      MaterialColor[ 4]:=MaterialYellow;
      MaterialColor[ 5]:=MaterialMagenta;
      MaterialColor[ 6]:=MaterialRed;
      MaterialColor[ 7]:=MaterialGreen;
      MaterialColor[ 8]:=MaterialBlue;
      MaterialColor[ 9]:=MaterialWhite;
      MaterialColor[10]:=MaterialCyan;
      MaterialColor[11]:=MaterialYellow;
      MaterialColor[12]:=MaterialMagenta;
      MaterialColor[13]:=MaterialRed;
      MaterialColor[14]:=MaterialGreen;
      MaterialColor[15]:=MaterialBlue;
      MaterialColor[16]:=MaterialCyan;
      MaterialColor[17]:=MaterialYellow;
      MaterialColor[18]:=MaterialMagenta;
      MaterialColor[19]:=MaterialGray;
      edgedivisions:=icodivisions;
      Magnitude:=2.5;
    end;
  end;
  if (mono) then
   begin
     for loop:=0 to 19 do
      MaterialColor[loop]:=MaterialGray;
   end;
  if (smooth) then
    glShadeModel( GL_SMOOTH )
  else
    glShadeModel( GL_FLAT );
end;


procedure do_key(k:byte;x,y:integer); cdecl;
begin
  case Char(k) of
    '1' : _object:=1;
    '2' : _object:=2;
    '3' : _object:=3;
    '4' : _object:=4;
    '5' : _object:=5;
    ' ' : mono:=not mono;
    #13 : smooth:=not smooth;
    #27 : halt(0);
  end;
  pinit;
end;


begin
  writeln('Morph 3D - Shows morphing platonic polyhedra');
  writeln('Author: Marcelo Fernandes Vianna (vianna@cat.cbpf.br)');
  writeln('  [1]    - Tetrahedron');
  writeln('  [2]    - Hexahedron (Cube)');
  writeln('  [3]    - Octahedron');
  writeln('  [4]    - Dodecahedron');
  writeln('  [5]    - Icosahedron');
  writeln('[SPACE]  - Toggle colored faces');
  writeln('[RETURN] - Toggle smooth/flat shading');
  writeln(' [ESC]   - Quit');
  _object:=3;

  glutInit(@argc, argv);
  glutInitWindowPosition(0,0);
  glutInitWindowSize(640,480);

  glutInitDisplayMode( GLUT_DEPTH + GLUT_DOUBLE + GLUT_RGB );

  if (glutCreateWindow('Morph 3D - Shows morphing platonic polyhedra') <= 0) then
    halt(1);

  glClearDepth(1.0);
  glClearColor( 0.0, 0.0, 0.0, 1.0 );
  glColor3f( 1.0, 1.0, 1.0 );

  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glFlush();
  glutSwapBuffers();

  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, position0);
  glLightfv(GL_LIGHT1, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT1, GL_POSITION, position1);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
  glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE);

  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, front_shininess);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, front_specular);

  glHint(GL_FOG_HINT, GL_FASTEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);

  pinit();

  glutReshapeFunc( @do_reshape );
  glutKeyboardFunc( @do_key );
  glutIdleFunc( @do_idle );
  glutDisplayFunc( @do_draw );
  glutMainLoop();
end.
