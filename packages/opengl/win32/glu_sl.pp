{
  $Id$

  Translation of the Mesa GLU headers for FreePascal
  Copyright (C) 1999 Sebastian Guenther
  Version for static linking in Win32 environment.
  Latest change: 1999-11-11

  Original copyright notice:

  Mesa 3-D graphics library
  Version:  3.0
  Copyright (C) 1995-1998  Brian Paul

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$MODE delphi}  // objfpc would not work because of direct proc var assignments

{You have to enable Macros (compiler switch "-Sm") for compiling this unit!
 This is necessary for supporting different platforms with different calling
 conventions via a single unit.}

unit GLU; // version with static linking


interface

{$MACRO ON}

{$IFDEF Win32}
  {$DEFINE glu_dll := external 'glu32.dll'}
  uses
    Windows,
    GL;        { for definition of GL_TRUE/FALS }
{$ELSE}
  {$MESSAGE Unsupported platform.}
{$ENDIF}

// =======================================================
//   Unit specific extensions
// =======================================================

// none - no special init required


// =======================================================
//   GLU consts, types and functions
// =======================================================

const
  GLU_TRUE                              = GL_TRUE;
  GLU_FALSE                             = GL_FALSE;

  // Normal vectors
  GLU_SMOOTH                            = 100000;
  GLU_FLAT                              = 100001;
  GLU_NONE                              = 100002;

  // Quadric draw styles
  GLU_POINT                             = 100010;
  GLU_LINE                              = 100011;
  GLU_FILL                              = 100012;
  GLU_SILHOUETTE                        = 100013;

  // Quadric orientation
  GLU_OUTSIDE                           = 100020;
  GLU_INSIDE                            = 100021;

  // Tesselator
  GLU_BEGIN                             = 100100;
  GLU_VERTEX                            = 100101;
  GLU_END                               = 100102;
  GLU_ERROR                             = 100103;
  GLU_EDGE_FLAG                         = 100104;

  // Contour types
  GLU_CW                                = 100120;
  GLU_CCW                               = 100121;
  GLU_INTERIOR                          = 100122;
  GLU_EXTERIOR                          = 100123;
  GLU_UNKNOWN                           = 100124;

  // Tesselation errors
  GLU_TESS_ERROR1                       = 100151;  // missing gluEndPolygon
  GLU_TESS_ERROR2                       = 100152;  // missing gluBeginPolygon
  GLU_TESS_ERROR3                       = 100153;  // misoriented contour
  GLU_TESS_ERROR4                       = 100154;  // vertex/edge intersection
  GLU_TESS_ERROR5                       = 100155;  // misoriented or self-intersecting loops
  GLU_TESS_ERROR6                       = 100156;  // coincident vertices
  GLU_TESS_ERROR7                       = 100157;  // all vertices collinear
  GLU_TESS_ERROR8                       = 100158;  // intersecting edges
  GLU_TESS_ERROR9                       = 100159;  // not coplanar contours

  // NURBS
  GLU_AUTO_LOAD_MATRIX                  = 100200;
  GLU_CULLING                           = 100201;
  GLU_PARAMETRIC_TOLERANCE              = 100202;
  GLU_SAMPLING_TOLERANCE                = 100203;
  GLU_DISPLAY_MODE                      = 100204;
  GLU_SAMPLING_METHOD                   = 100205;
  GLU_U_STEP                            = 100206;
  GLU_V_STEP                            = 100207;

  GLU_PATH_LENGTH                       = 100215;
  GLU_PARAMETRIC_ERROR                  = 100216;
  GLU_DOMAIN_DISTANCE                   = 100217;

  GLU_MAP1_TRIM_2                       = 100210;
  GLU_MAP1_TRIM_3                       = 100211;

  GLU_OUTLINE_POLYGON                   = 100240;
  GLU_OUTLINE_PATCH                     = 100241;

  GLU_NURBS_ERROR1                      = 100251;   // spline order un-supported
  GLU_NURBS_ERROR2                      = 100252;   // too few knots
  GLU_NURBS_ERROR3                      = 100253;   // valid knot range is empty
  GLU_NURBS_ERROR4                      = 100254;   // decreasing knot sequence
  GLU_NURBS_ERROR5                      = 100255;   // knot multiplicity > spline order
  GLU_NURBS_ERROR6                      = 100256;   // endcurve() must follow bgncurve()
  GLU_NURBS_ERROR7                      = 100257;   // bgncurve() must precede endcurve()
  GLU_NURBS_ERROR8                      = 100258;   // ctrlarray or knot vector is NULL
  GLU_NURBS_ERROR9                      = 100259;   // cannot draw pwlcurves
  GLU_NURBS_ERROR10                     = 100260;   // missing gluNurbsCurve()
  GLU_NURBS_ERROR11                     = 100261;   // missing gluNurbsSurface()
  GLU_NURBS_ERROR12                     = 100262;   // endtrim() must precede endsurface()
  GLU_NURBS_ERROR13                     = 100263;   // bgnsurface() must precede endsurface()
  GLU_NURBS_ERROR14                     = 100264;   // curve of improper type passed as trim curve
  GLU_NURBS_ERROR15                     = 100265;   // bgnsurface() must precede bgntrim()
  GLU_NURBS_ERROR16                     = 100266;   // endtrim() must follow bgntrim()
  GLU_NURBS_ERROR17                     = 100267;   // bgntrim() must precede endtrim()*/
  GLU_NURBS_ERROR18                     = 100268;   // invalid or missing trim curve*/
  GLU_NURBS_ERROR19                     = 100269;   // bgntrim() must precede pwlcurve()
  GLU_NURBS_ERROR20                     = 100270;   // pwlcurve referenced twice*/
  GLU_NURBS_ERROR21                     = 100271;   // pwlcurve and nurbscurve mixed
  GLU_NURBS_ERROR22                     = 100272;   // improper usage of trim data type
  GLU_NURBS_ERROR23                     = 100273;   // nurbscurve referenced twice
  GLU_NURBS_ERROR24                     = 100274;   // nurbscurve and pwlcurve mixed
  GLU_NURBS_ERROR25                     = 100275;   // nurbssurface referenced twice
  GLU_NURBS_ERROR26                     = 100276;   // invalid property
  GLU_NURBS_ERROR27                     = 100277;   // endsurface() must follow bgnsurface()
  GLU_NURBS_ERROR28                     = 100278;   // intersecting or misoriented trim curves
  GLU_NURBS_ERROR29                     = 100279;   // intersecting trim curves
  GLU_NURBS_ERROR30                     = 100280;   // UNUSED
  GLU_NURBS_ERROR31                     = 100281;   // unconnected trim curves
  GLU_NURBS_ERROR32                     = 100282;   // unknown knot error
  GLU_NURBS_ERROR33                     = 100283;   // negative vertex count encountered
  GLU_NURBS_ERROR34                     = 100284;   // negative byte-stride
  GLU_NURBS_ERROR35                     = 100285;   // unknown type descriptor
  GLU_NURBS_ERROR36                     = 100286;   // null control point reference
  GLU_NURBS_ERROR37                     = 100287;   // duplicate point on pwlcurve

  // Errors
  GLU_INVALID_ENUM                      = 100900;
  GLU_INVALID_VALUE                     = 100901;
  GLU_OUT_OF_MEMORY                     = 100902;
  GLU_INCOMPATIBLE_GL_VERSION           = 100903;

  // New in GLU 1.1
  GLU_VERSION                           = 100800;
  GLU_EXTENSIONS                        = 100801;

type
  PGLUquadricObj = ^TGLUquadricObj;
  TGLUquadricObj = record end;
  PGLUtriangulatorObj = ^TGLUtriangulatorObj;
  TGLUtriangulatorObj = record end;
  PGLUnurbsObj = ^TGLUnurbsObj;
  TGLUnurbsObj = record end;

  TGLUQuadricCallback = procedure; cdecl;
  TGLUNurbsCallback = procedure; cdecl;
  TGLUTessCallback = procedure; cdecl;

  TGLUViewport = array[0..3] of LongInt;
  TGLUMatrixd = array[0..15] of Double;
  TGLUMatrixf = array[0..15] of Single;
  TGLUVectord = array[0..2] of Double;


  // Miscellaneous functions
procedure gluLookAt(eye, eyey, eyez, centerx, centery, centerz, upx, upy, upz: Double); glu_dll;
procedure gluOrtho2D(left, right, bottom, top: Double); glu_dll;
procedure gluPerspective(fovy, aspect, zNear, zFar: Double); glu_dll;
procedure gluPickMatrix(x, y, width, height: Double; const viewport: TGLUViewport); glu_dll;
procedure gluProject(objx, objy, objz: Double; const modelMatrix, projMatrix: TGLUMatrixd; const viewport: TGLUViewport; winx, winy, winz: Double); glu_dll;
procedure gluUnProject(winx, winy, winz: Double; const modelMatrix, projMatrix: TGLUMatrixd; const viewport: TGLUViewport; objx, objy, objz: Double); glu_dll;
procedure gluErrorString(errorCode: GLenum); glu_dll;

  // Mipmapping and image scaling
procedure gluScaleImage(format: GLenum; within, heightin: LongInt; typein: GLenum; var datain; widthout, heightout: LongInt; typeout: GLenum; var dataout); glu_dll;
procedure gluBuild1DMipmaps(target: GLenum; components, width: LongInt; format, AType: GLEnum; var data); glu_dll;
procedure gluBuild2DMipmaps(target: GLenum; components, width, height: LongInt; format, AType: GLEnum; var data); glu_dll;

  // Quadrics
function gluNewQuadric: PGLUquadricObj; glu_dll;
procedure gluDeleteQuadric(state: PGLUquadricObj); glu_dll;
procedure gluQuadricDrawStyle(quadObject: PGLUquadricObj; drawStyle: GLenum); glu_dll;
procedure gluQuadricOrientation(quadObject: PGLUquadricObj; orientation: GLenum); glu_dll;
procedure gluQuadricNormals(quadObject: PGLUquadricObj; normals: GLenum); glu_dll;
procedure gluQuadricTexture(quadObject: PGLUquadricObj; textureCoords: Boolean); glu_dll;
procedure gluQuadricCallback(quadObject: PGLUquadricObj; which: GLenum; fn: TGLUQuadricCallback); glu_dll;
procedure gluCylinder(qobj: PGLUquadricObj; baseRadius, topRadius, height: Double; slices, stacks: LongInt); glu_dll;
procedure gluSphere(qobj: PGLUquadricObj; radius: Double; slices, stacks: LongInt); glu_dll;
procedure gluDisk(qobj: PGLUquadricObj; innerRadius, outerRadius: Double; slices, loops: LongInt); glu_dll;
procedure gluPartialDisk(qobj: PGLUquadricObj; innerRadius, outerRadius: Double; slices, loops: LongInt; startAngle, sweepAngle: Double); glu_dll;

  // Nurbs
function gluNewNurbsRenderer: PGLUnurbsObj; glu_dll;
procedure gluDeleteNurbsRenderer(nobj: PGLUnurbsObj); glu_dll;
procedure gluLoadSamplingMatrices(nobj: PGLUnurbsObj; const modelMatrix, projMatrix: TGLUMatrixf; const viewport: TGLUViewport); glu_dll;
procedure gluNurbsProperty(nobj: PGLUnurbsObj; AProperty: GLenum; value: Single); glu_dll;
procedure gluGetNurbsProperty(nobj: PGLUnurbsObj; AProperty: GLEnum; var value: Single); glu_dll;
procedure gluBeginCurve(nobj: PGLUnurbsObj); glu_dll;
procedure gluEndCurve(nobj: PGLUnurbsObj); glu_dll;
procedure gluNurbsCurve(nobj: PGLUnurbsObj; nknots: LongInt; var know: Single; stride: LongInt; var ctlarray: Single; order: LongInt; AType: GLenum); glu_dll;
procedure gluBeginSurface(nobj: PGLUnurbsObj); glu_dll;
procedure gluEndSurface(nobj: PGLUnurbsObj); glu_dll;
procedure gluNurbsSurface(nobj: PGLUnurbsObj; sknot_count: LongInt; var sknot: Single; tknot_count: LongInt; var tknot: Single; s_stride, t_stride: LongInt; var ctlarray: Single; sorder, torder: LongInt; AType: GLenum); glu_dll;
procedure gluBeginTrim(nobj: PGLUnurbsObj); glu_dll;
procedure gluEndTrim(nobj: PGLUnurbsObj); glu_dll;
procedure gluPwlCurve(nobj: PGLUnurbsObj; count: LongInt; var AArray: Single; stride: LongInt; AType: GLenum); glu_dll;
procedure gluNurbsCallback(nobj: PGLUnurbsObj; which: GLenum; fn: TGLUNurbsCallback); glu_dll;

  // Polygon tesselation
function gluNewTess: PGLUtriangulatorObj; glu_dll;
procedure gluTessCallback(tobj: PGLUtriangulatorObj; which: GLenum; fn: TGLUTessCallback); glu_dll;

procedure gluDeleteTess(tobj: PGLUtriangulatorObj); glu_dll;
procedure gluBeginPolygon(tobj: PGLUtriangulatorObj); glu_dll;
procedure gluEndPolygon(tobj: PGLUtriangulatorObj); glu_dll;
procedure gluNextContour(tobj: PGLUtriangulatorObj; AType: GLenum); glu_dll;
procedure gluTessVertex(tobj: PGLUtriangulatorObj; v: TGLUVectord; var data); glu_dll;

  // New functions in GLU 1.1
function gluGetString(name: GLenum): PChar; glu_dll;

// =======================================================
//
// =======================================================

implementation


{BEGIN{OF INIT}
end.


{
  $Log$
  Revision 1.2  2000-09-03 22:17:18  peter
    * merged

  Revision 1.1.2.1  2000/09/03 22:14:41  peter
    * regenerated

  Revision 1.1  2000/09/03 21:25:45  peter
    * new updated version
    * gtkglarea unit and demo
    * win32 opengl headers
    * morph3d demo

  Revision 1.1  2000/07/13 06:34:18  michael
  + Initial import

  Revision 1.1  2000/05/31 00:35:14  alex
  added working templates

}


{
  $Log$
  Revision 1.2  2000-09-03 22:17:18  peter
    * merged

  Revision 1.1.2.1  2000/09/03 22:14:41  peter
    * regenerated

}
