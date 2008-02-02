//------------------------------------------------------------------------
//
// Author              : Dario Corno (rIo) / Jeff Molofee (NeHe)
// Converted to Delphi : Jan Horn
// Email               : jhorn@global.co.za
// Website             : http://www.sulaco.co.za
// Authors Web Site    : http://www.spinningkids.org/rio
// Date                : 14 October 2001
// Version             : 1.0
// Description         : Radial Blur
//
// Adapted to FPC      : Sebastian Guenther (sg@freepascal.org) 2001-11-21
//
//------------------------------------------------------------------------
program RadialBlur;

{$mode objfpc}

uses GL, GLU, GLUT;

const
  WND_TITLE = 'Radial Blur';

type TVector = Array[0..2] of glFloat;
var
  ElapsedTime : Integer;             // Elapsed time between frames

  // Textures
  BlurTexture : glUint;              // An Unsigned Int To Store The Texture Number

  // User vaiables
  Angle : glFloat;
  Vertexes : Array[0..3] of TVector;
  normal : TVector;

const
  FPSCount : Integer = 0;            // Counter for FPS
  // Lights and Materials
  globalAmbient  : Array[0..3] of glFloat = (0.2, 0.2,  0.2, 1.0);      // Set Ambient Lighting To Fairly Dark Light (No Color)
  Light0Pos      : Array[0..3] of glFloat = (0.0, 5.0, 10.0, 1.0);      // Set The Light Position
  Light0Ambient  : Array[0..3] of glFloat = (0.2, 0.2,  0.2, 1.0);      // More Ambient Light
  Light0Diffuse  : Array[0..3] of glFloat = (0.3, 0.3,  0.3, 1.0);      // Set The Diffuse Light A Bit Brighter
  Light0Specular : Array[0..3] of glFloat = (0.8, 0.8,  0.8, 1.0);      // Fairly Bright Specular Lighting

  LmodelAmbient  : Array[0..3] of glFloat = (0.2, 0.2,  0.2, 1.0);      // And More Ambient Light


function EmptyTexture : glUint;
var txtnumber : glUint;
    pData : Pointer;
begin
  // Create Storage Space For Texture Data (128x128x4)
  GetMem(pData, 128*128*4);

  glGenTextures(1, @txtnumber);                         // Create 1 Texture
  glBindTexture(GL_TEXTURE_2D, txtnumber);              // Bind The Texture
  glTexImage2D(GL_TEXTURE_2D, 0, 4, 128, 128, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);

  result :=txtNumber;
end;


procedure ReduceToUnit(var vector : Array of glFloat);
var length : glFLoat;
begin
  // Calculates The Length Of The Vector
  length := sqrt((vector[0]*vector[0]) + (vector[1]*vector[1]) + (vector[2]*vector[2]));
  if Length = 0 then
    Length :=1;

  vector[0] :=vector[0] / length;
  vector[1] :=vector[1] / length;
  vector[2] :=vector[2] / length;
end;


procedure calcNormal(const v : Array of TVector; var cross : Array of glFloat);
var v1, v2 : Array[0..2] of glFloat;
begin
  // Finds The Vector Between 2 Points By Subtracting
  // The x,y,z Coordinates From One Point To Another.

  // Calculate The Vector From Point 1 To Point 0
  v1[0] := v[0][0] - v[1][0];                   // Vector 1.x=Vertex[0].x-Vertex[1].x
  v1[1] := v[0][1] - v[1][1];                   // Vector 1.y=Vertex[0].y-Vertex[1].y
  v1[2] := v[0][2] - v[1][2];                   // Vector 1.z=Vertex[0].y-Vertex[1].z
  // Calculate The Vector From Point 2 To Point 1
  v2[0] := v[1][0] - v[2][0];                   // Vector 2.x=Vertex[0].x-Vertex[1].x
  v2[1] := v[1][1] - v[2][1];                   // Vector 2.y=Vertex[0].y-Vertex[1].y
  v2[2] := v[1][2] - v[2][2];                   // Vector 2.z=Vertex[0].z-Vertex[1].z
  // Compute The Cross Product To Give Us A Surface Normal
  cross[0] := v1[1]*v2[2] - v1[2]*v2[1];        // Cross Product For Y - Z
  cross[1] := v1[2]*v2[0] - v1[0]*v2[2];        // Cross Product For X - Z
  cross[2] := v1[0]*v2[1] - v1[1]*v2[0];        // Cross Product For X - Y

  ReduceToUnit(cross);                          // Normalize The Vectors
end;


// Draws A Helix
procedure ProcessHelix;
const Twists = 5;
      MaterialColor : Array[1..4] of glFloat = (0.4, 0.2, 0.8, 1.0);
      Specular      : Array[1..4] of glFloat = (1, 1, 1, 1);
var x, y, z : glFLoat;
    phi, theta : Integer;
    r, u, v : glFLoat;
begin
  glLoadIdentity();                             // Reset The Modelview Matrix
  gluLookAt(0, 5, 50, 0, 0, 0, 0, 1, 0);        // Eye Position (0,5,50) Center Of Scene (0,0,0), Up On Y Axis

  glPushMatrix();                               // Push The Modelview Matrix
    glTranslatef(0,0,-50);                      // Translate 50 Units Into The Screen
    glRotatef(angle/2.0, 1, 0, 0);              // Rotate By angle/2 On The X-Axis
    glRotatef(angle/3.0, 0, 1, 0);              // Rotate By angle/3 On The Y-Axis

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @MaterialColor);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @specular);

    r :=1.5;                                    // Radius

    glBegin(GL_QUADS);                          // Begin Drawing Quads
      phi :=0;
      while phi < 360 do
      begin
        theta :=0;
        while theta < 360*twists do
        begin
          v := phi / 180.0 * pi;                // Calculate Angle Of First Point       (  0 )
          u := theta / 180.0 * pi;              // Calculate Angle Of First Point       (  0 )

          x :=cos(u)*(2 + cos(v))*r;            // Calculate x Position (1st Point)
          y :=sin(u)*(2 + cos(v))*r;            // Calculate y Position (1st Point)
          z :=(u-(2*pi) + sin(v))*r;            // Calculate z Position (1st Point)

          vertexes[0][0] :=x;                   // Set x Value Of First Vertex
          vertexes[0][1] :=y;                   // Set y Value Of First Vertex
          vertexes[0][2] :=z;                   // Set z Value Of First Vertex

          v :=(phi/180.0 * pi);                 // Calculate Angle Of Second Point      (  0 )
          u :=((theta+20)/180.0 * pi);          // Calculate Angle Of Second Point      ( 20 )

          x :=cos(u)*(2 + cos(v))*r;            // Calculate x Position (2nd Point)
          y :=sin(u)*(2 + cos(v))*r;            // Calculate y Position (2nd Point)
          z :=(u-(2*pi) + sin(v))*r;            // Calculate z Position (2nd Point)

          vertexes[1][0] :=x;                   // Set x Value Of Second Vertex
          vertexes[1][1] :=y;                   // Set y Value Of Second Vertex
          vertexes[1][2] :=z;                   // Set z Value Of Second Vertex

          v :=(phi+20)/180.0*pi;                // Calculate Angle Of Third Point       ( 20 )
          u :=(theta+20)/180.0*pi;              // Calculate Angle Of Third Point       ( 20 )

          x :=cos(u)*(2 + cos(v))*r;            // Calculate x Position (3rd Point)
          y :=sin(u)*(2 + cos(v))*r;            // Calculate y Position (3rd Point)
          z :=(u-(2*pi) + sin(v))*r;            // Calculate z Position (3rd Point)

          vertexes[2][0] :=x;                   // Set x Value Of Third Vertex
          vertexes[2][1] :=y;                   // Set y Value Of Third Vertex
          vertexes[2][2] :=z;                   // Set z Value Of Third Vertex

          v :=(phi+20)/180.0*pi;                // Calculate Angle Of Fourth Point      ( 20 )
          u :=theta / 180.0*pi;                 // Calculate Angle Of Fourth Point      (  0 )

          x :=cos(u)*(2 + cos(v))*r;            // Calculate x Position (4th Point)
          y :=sin(u)*(2 + cos(v))*r;            // Calculate y Position (4th Point)
          z :=(u-(2*pi) + sin(v))*r;            // Calculate z Position (4th Point)

          vertexes[3][0] :=x;                   // Set x Value Of Fourth Vertex
          vertexes[3][1] :=y;                   // Set y Value Of Fourth Vertex
          vertexes[3][2] :=z;                   // Set z Value Of Fourth Vertex

          calcNormal(vertexes, normal);         // Calculate The Quad Normal

          glNormal3f(normal[0],normal[1],normal[2]);    // Set The Normal

          // Render The Quad
          glVertex3f(vertexes[0][0],vertexes[0][1],vertexes[0][2]);
          glVertex3f(vertexes[1][0],vertexes[1][1],vertexes[1][2]);
          glVertex3f(vertexes[2][0],vertexes[2][1],vertexes[2][2]);
          glVertex3f(vertexes[3][0],vertexes[3][1],vertexes[3][2]);
          theta := theta + 20;
        end;
        phi :=phi + 20;
      end;
    glEnd();                                   // Done Rendering Quads
  glPopMatrix();                               // Pop The Matrix
end;


// Set Up An Ortho View
procedure ViewOrtho;
begin
  glMatrixMode(GL_PROJECTION);                  // Select Projection
  glPushMatrix();                               // Push The Matrix
  glLoadIdentity();                             // Reset The Matrix
  glOrtho( 0, 640 , 480 , 0, -1, 1 );           // Select Ortho Mode (640x480)
  glMatrixMode(GL_MODELVIEW);                   // Select Modelview Matrix
  glPushMatrix();                               // Push The Matrix
  glLoadIdentity();                             // Reset The Matrix
end;


// Set Up A Perspective View
procedure ViewPerspective;
begin
  glMatrixMode( GL_PROJECTION );                // Select Projection
  glPopMatrix();                                // Pop The Matrix
  glMatrixMode( GL_MODELVIEW );                 // Select Modelview
  glPopMatrix();                                // Pop The Matrix
end;


// Renders To A Texture
procedure RenderToTexture;
begin
  glViewport(0, 0, 128, 128);                           // Set Our Viewport (Match Texture Size)
  ProcessHelix();                                       // Render The Helix
  glBindTexture(GL_TEXTURE_2D,BlurTexture);             // Bind To The Blur Texture

  // Copy Our ViewPort To The Blur Texture (From 0,0 To 128,128... No Border)
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, 0, 0, 128, 128, 0);
  glClearColor(0.0, 0.0, 0.5, 0.5);                     // Set The Clear Color To Medium Blue
  glClear(GL_COLOR_BUFFER_BIT OR GL_DEPTH_BUFFER_BIT);  // Clear The Screen And Depth Buffer
  glViewport(0, 0, 640 ,480);                           // Set Viewport (0,0 to 640x480)
end;


// Draw The Blurred Image
procedure DrawBlur(const times : Integer; const inc : glFloat);
var spost, alpha, alphainc : glFloat;
    I : Integer;
begin
  alpha := 0.2;
  spost := 0.0;
  glEnable(GL_TEXTURE_2D);                      // Enable 2D Texture Mapping
  glDisable(GL_DEPTH_TEST);                     // Disable Depth Testing
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);             // Set Blending Mode
  glEnable(GL_BLEND);                           // Enable Blending
  glBindTexture(GL_TEXTURE_2D,BlurTexture);     // Bind To The Blur Texture
  ViewOrtho();                                  // Switch To An Ortho View

  alphainc := alpha / times;                    // alphainc=0.2f / Times To Render Blur

  glBegin(GL_QUADS);                            // Begin Drawing Quads
    // Number Of Times To Render Blur
    For I :=0 to times-1 do
    begin
      glColor4f(1.0, 1.0, 1.0, alpha);          // Set The Alpha Value (Starts At 0.2)
      glTexCoord2f(0+spost,1-spost);            // Texture Coordinate   ( 0, 1 )
      glVertex2f(0,0);                          // First Vertex         (   0,   0 )

      glTexCoord2f(0+spost,0+spost);            // Texture Coordinate   ( 0, 0 )
      glVertex2f(0,480);                        // Second Vertex        (   0, 480 )

      glTexCoord2f(1-spost,0+spost);            // Texture Coordinate   ( 1, 0 )
      glVertex2f(640,480);                      // Third Vertex         ( 640, 480 )

      glTexCoord2f(1-spost,1-spost);            // Texture Coordinate   ( 1, 1 )
      glVertex2f(640,0);                        // Fourth Vertex        ( 640,   0 )

      spost := spost + inc;                     // Gradually Increase spost (Zooming Closer To Texture Center)
      alpha := alpha - alphainc;                // Gradually Decrease alpha (Gradually Fading Image Out)
    end;
  glEnd();                                      // Done Drawing Quads

  ViewPerspective();                            // Switch To A Perspective View

  glEnable(GL_DEPTH_TEST);                      // Enable Depth Testing
  glDisable(GL_TEXTURE_2D);                     // Disable 2D Texture Mapping
  glDisable(GL_BLEND);                          // Disable Blending
  glBindTexture(GL_TEXTURE_2D,0);               // Unbind The Blur Texture
end;

{------------------------------------------------------------------}
{  Function to draw the actual scene                               }
{------------------------------------------------------------------}
procedure glDraw;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity();                     // Reset The View
  RenderToTexture;                      // Render To A Texture
  ProcessHelix;                         // Draw Our Helix
  DrawBlur(25, 0.02);                   // Draw The Blur Effect

  angle :=ElapsedTime / 5.0;            // Update angle Based On The Clock
end;


{------------------------------------------------------------------}
{  Initialise OpenGL                                               }
{------------------------------------------------------------------}
procedure glInit;
begin
  glClearColor(0.0, 0.0, 0.0, 0.5);        // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glDepthFunc(GL_LESS);                    // The Type Of Depth Test To Do

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glEnable(GL_TEXTURE_2D);                 // Enable Texture Mapping

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LmodelAmbient);       // Set The Ambient Light Model

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @GlobalAmbient);       // Set The Global Ambient Light Model
  glLightfv(GL_LIGHT0, GL_POSITION, @light0Pos);                // Set The Lights Position
  glLightfv(GL_LIGHT0, GL_AMBIENT, @light0Ambient);             // Set The Ambient Light
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @light0Diffuse);             // Set The Diffuse Light
  glLightfv(GL_LIGHT0, GL_SPECULAR, @light0Specular);           // Set Up Specular Lighting
  glEnable(GL_LIGHTING);                                                                                // Enable Lighting
  glEnable(GL_LIGHT0);                                                                          // Enable Light0

  BlurTexture := EmptyTexture();                                                                // Create Our Empty Texture
  glShadeModel(GL_SMOOTH);                                                                      // Select Smooth Shading
  glMateriali(GL_FRONT, GL_SHININESS, 128);
end;


{------------------------------------------------------------------}
{  Handle window resize                                            }
{------------------------------------------------------------------}
procedure glResizeWnd(Width, Height : Integer);
begin
  if (Height = 0) then                // prevent divide by zero exception
    Height := 1;
  glViewport(0, 0, Width, Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity();                   // Reset View
  gluPerspective(45.0, Width/glFloat(Height), 2.0, 200.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity();                   // Reset View
end;


var
  DemoStart, LastTime : LongWord;


procedure DisplayWindow; cdecl;
begin
  Inc(FPSCount);                      // Increment FPS Counter

  LastTime :=ElapsedTime;
  ElapsedTime := glutGet(GLUT_ELAPSED_TIME) - DemoStart;     // Calculate Elapsed Time
  ElapsedTime :=(LastTime + ElapsedTime) DIV 2; // Average it out for smoother movement
  glDraw;
  glutSwapBuffers;
  Inc(ElapsedTime, 10);
  glutPostRedisplay;
end;

procedure OnReshape(width, height: Integer); cdecl;
begin
  glResizeWnd(width, height);
end;


begin
  glutInit(@argc, argv);
  glutInitDisplayMode(GLUT_RGB or GLUT_DOUBLE or GLUT_DEPTH);
  glutCreateWindow(WND_TITLE);
  glutDisplayFunc(@DisplayWindow);
  glutReshapeFunc(@OnReshape);
  glutInitWindowSize(640, 480);

  glInit;

  DemoStart := glutGet(GLUT_ELAPSED_TIME);
  glutMainLoop;
end.
