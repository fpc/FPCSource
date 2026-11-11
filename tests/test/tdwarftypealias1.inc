(* included by different unit names - ensure no ppu is re-used with incorrect dwarf settings *)

type
  TTestInt1 = -9..9;
  TTestInt2 = type TTestInt1;

  // field in base in other unit
  TTestChildObject = object(ua_TTestBaseObject)
    FldChild1: Integer;
  end;
  TTestChildObjectX = type ua_TTestChildObject;


  TUa_TestInt1_X = type ua_TTestInt1; // alias to other unit
  TUa_TestInt1X_X = type ua_TTestInt1X; // double alias to other unit

  // record

  TTestRec1 = record
    FldA: integer;
    FldB: integer;
    FldRec1: ua_TTestRec1;
    FldRec2: ua_TTestRec1X;
    function P1: integer;
    function PN0: ua_TTestRec1;
    function PN0X: ua_TTestRec1X;
    function PN1: ua_TTestRec1.ua_TTestRec1Nest1;
    function PN1X: ua_TTestRec1.ua_TTestRec1Nest1X;
    // access via the type-alias ua_TTestRec1 does not affect nested types
    function PN2: ua_TTestRec1X.ua_TTestRec1Nest2;
    function PN2X: ua_TTestRec1X.ua_TTestRec1Nest2X;

    property P_ua_FldB: integer read FldRec1.ua_FldR_B;
    property P_uaX_FldB: integer read FldRec2.ua_FldR_B;
    property P_P1: integer read P1;
  end;
  TTestRec1X = type TTestRec1;
  TTestRec1Y = type TTestRec1; // 2nd alias
  TTestRec1XX = type TTestRec1X; // double alias

  TUa_TestRec1_X = type ua_TTestRec1; // alias to other unit
  TUa_TestRec1X_X = type ua_TTestRec1X; // double alias to other unit

  TTestRec2 = record
    FR1: integer;
    contains r: ua_TTestRec2;
    property P_A: integer read r.ua_FldR2_A write r.ua_FldR2_A;
  end;

  TTestRec3 = record // layout is subset of TTestClass1
  public
    ua_FldC_A: integer;
    ua_FldC_B: integer;
    ua_FldC_C: ua_TTestClass1.ua_TTestClass1Nest1;
    ua_FldC_CX: ua_TTestClass1.ua_TTestClass1Nest1X;
    ua_FldC_D: ua_TTestClass1.ua_TTestClass1Nest2Rec;
    ua_FldC_DX: ua_TTestClass1.ua_TTestClass1Nest2RecX;
    ua_FldC_E: ua_TTestRec2;
    ua_FldC_EX: ua_TTestRec2X;
    ua_FldC_G:   ua_TTestRec2_array;
    ua_FldC_GX:  ua_TTestRec2X_array;
    ua_FldC_XG:  ua_TTestRec2_arrayX;
    ua_FldC_XGX: ua_TTestRec2X_arrayX;
    ua_FldC_YG:   array [0..3] of ua_TTestRec2;
    ua_FldC_YGX:  array [0..3] of ua_TTestRec2X;
    ua_FldC_H:   ua_TTestChildObject;
    ua_FldC_HX:  ua_TTestChildObjectX;
    ua_FldC_XH:   ua_TTestXChildObject;
    ua_FldC_XHX:  ua_TTestXChildObjectX;
    FldC_H:   TTestChildObject;
    FldC_HX:  TTestChildObjectX;

    function  ua_Func1: ua_TTestInt1;
    function  ua_Func1X: ua_TTestInt1X;
    procedure ua_Proc1(a: ua_TTestInt1);
    procedure ua_Proc1X(a: ua_TTestInt1X);

    function  ua_Func2(AnIdx:integer): ua_TTestInt1;
    function  ua_Func2X(AnIdx:integer): ua_TTestInt1X;
    procedure ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1);
    procedure ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X);
    function  ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1;
    function  ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X;
    procedure ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1);
    procedure ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X);

    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;

    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;

  end;
  TTestRec3X = type TTestRec3;

  TTestRec4 = record
    FR1: integer;
    contains TTestRec3;
  end;
  TTestRec4X = type TTestRec4;

function TTestRec1.P1: integer; begin Result := default(type of result); end;
function TTestRec1.PN0: ua_TTestRec1;   begin Result := default(type of result); end;
function TTestRec1.PN0X: ua_TTestRec1X; begin Result := default(type of result); end;
function TTestRec1.PN1: ua_TTestRec1.ua_TTestRec1Nest1;    begin Result := default(type of result); end;
function TTestRec1.PN1X: ua_TTestRec1.ua_TTestRec1Nest1X;  begin Result := default(type of result); end;
function TTestRec1.PN2: ua_TTestRec1X.ua_TTestRec1Nest2;   begin Result := default(type of result); end;
function TTestRec1.PN2X: ua_TTestRec1X.ua_TTestRec1Nest2X; begin Result := default(type of result); end;

function  TTestRec3.ua_Func1: ua_TTestInt1;  begin Result := default(type of result); end;
function  TTestRec3.ua_Func1X: ua_TTestInt1X;  begin Result := default(type of result); end;
procedure TTestRec3.ua_Proc1(a: ua_TTestInt1);  begin {} end;
procedure TTestRec3.ua_Proc1X(a: ua_TTestInt1X);  begin {} end;
function  TTestRec3.ua_Func2(AnIdx:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  TTestRec3.ua_Func2X(AnIdx:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure TTestRec3.ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1); begin {} end;
procedure TTestRec3.ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X); begin {} end;
function  TTestRec3.ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  TTestRec3.ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure TTestRec3.ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1); begin {} end;
procedure TTestRec3.ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X); begin {} end;

var
  i1:  TTestInt1;
  i2:  TTestInt2;
  iA1: TUa_TestInt1_X;
  iA2: TUa_TestInt1X_X;

  Rec1:  TTestRec1;
  Rec1X:  TTestRec1X;
  Rec1Y: TTestRec1Y;
  Rec1XX:  TTestRec1XX;
  RecXUa1: TUa_TestRec1_X;
  RecXUa1X: TUa_TestRec1X_X;
  Rec3: TTestRec3;
  Rec3X: TTestRec3X;


type
  // object

  TTestObj1 = object(ua_TTestObj1)
    FldA: integer;
  end;
  TTestObj1X = TTestObj1;
  //TTestObj1X = type TTestObj1;

  TTestXObj1 = object(ua_TTestObj1X)
    FldA: integer;
  end;
  TTestXObj1X = TTestXObj1;
  //TTestXObj1X = type TTestXObj1;

  // object with vmt

  TTestObjV1 = object(ua_TTestObjV1)
    FldA: integer;
  end;
  TTestObjV1X = TTestObjV1;
  //TTestObjV1X = type TTestObjV1;

  TTestXObjV1 = object(ua_TTestObjV1X)
    FldA: integer;
    function PO_1: integer;
  end;
  TTestXObjV1X = TTestXObjV1;
  //TTestXObjV1X = type TTestXObjV1;

  TTestObj3 = object // layout is subset of TTestClass1
  public
    ua_FldC_A: integer;
    ua_FldC_B: integer;
    ua_FldC_C: ua_TTestClass1.ua_TTestClass1Nest1;
    ua_FldC_CX: ua_TTestClass1.ua_TTestClass1Nest1X;
    ua_FldC_D: ua_TTestClass1.ua_TTestClass1Nest2Rec;
    ua_FldC_DX: ua_TTestClass1.ua_TTestClass1Nest2RecX;
    ua_FldC_E: ua_TTestRec2;
    ua_FldC_EX: ua_TTestRec2X;
    ua_FldC_G:   ua_TTestRec2_array;
    ua_FldC_GX:  ua_TTestRec2X_array;
    ua_FldC_XG:  ua_TTestRec2_arrayX;
    ua_FldC_XGX: ua_TTestRec2X_arrayX;
    ua_FldC_YG:   array [0..3] of ua_TTestRec2;
    ua_FldC_YGX:  array [0..3] of ua_TTestRec2X;
    ua_FldC_H:   ua_TTestChildObject;
    ua_FldC_HX:  ua_TTestChildObjectX;
    ua_FldC_XH:   ua_TTestXChildObject;
    ua_FldC_XHX:  ua_TTestXChildObjectX;
    FldC_H:   TTestChildObject;
    FldC_HX:  TTestChildObjectX;

    function  ua_Func1: ua_TTestInt1;
    function  ua_Func1X: ua_TTestInt1X;
    procedure ua_Proc1(a: ua_TTestInt1);
    procedure ua_Proc1X(a: ua_TTestInt1X);

    function  ua_Func2(AnIdx:integer): ua_TTestInt1;
    function  ua_Func2X(AnIdx:integer): ua_TTestInt1X;
    procedure ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1);
    procedure ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X);
    function  ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1;
    function  ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X;
    procedure ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1);
    procedure ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X);


    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;

    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;
  end;
  TTestObj3X = type TTestObj3;

  // fields in base
  TTestObj4= object // layout is subset of TTestClass1
  public
    ua_FldC_A: integer;
    ua_FldC_B: integer;
    ua_FldC_C: ua_TTestClass1.ua_TTestClass1Nest1;
    ua_FldC_CX: ua_TTestClass1.ua_TTestClass1Nest1X;
    ua_FldC_D: ua_TTestClass1.ua_TTestClass1Nest2Rec;
    ua_FldC_DX: ua_TTestClass1.ua_TTestClass1Nest2RecX;
    ua_FldC_E: ua_TTestRec2;
    ua_FldC_EX: ua_TTestRec2X;
    ua_FldC_G:   ua_TTestRec2_array;
    ua_FldC_GX:  ua_TTestRec2X_array;
    ua_FldC_XG:  ua_TTestRec2_arrayX;
    ua_FldC_XGX: ua_TTestRec2X_arrayX;
    ua_FldC_YG:   array [0..3] of ua_TTestRec2;
    ua_FldC_YGX:  array [0..3] of ua_TTestRec2X;
    ua_FldC_H:   ua_TTestChildObject;
    ua_FldC_HX:  ua_TTestChildObjectX;
    ua_FldC_XH:   ua_TTestXChildObject;
    ua_FldC_XHX:  ua_TTestXChildObjectX;
    FldC_H:   TTestChildObject;
    FldC_HX:  TTestChildObjectX;

    function  ua_Func1: ua_TTestInt1;
    function  ua_Func1X: ua_TTestInt1X;
    procedure ua_Proc1(a: ua_TTestInt1);
    procedure ua_Proc1X(a: ua_TTestInt1X);

    function  ua_Func2(AnIdx:integer): ua_TTestInt1;
    function  ua_Func2X(AnIdx:integer): ua_TTestInt1X;
    procedure ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1);
    procedure ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X);
    function  ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1;
    function  ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X;
    procedure ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1);
    procedure ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X);
  end;
  TTestObj4X = type TTestObj4;

  TTestObj5= object(TTestObj4) // layout is subset of TTestClass1
    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;

    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;
  end;
  TTestObj5X = type TTestObj5;

  TTestObjX5= object(TTestObj4X) // layout is subset of TTestClass1
    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;

    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;
  end;
  TTestObjX5X = type TTestObjX5;

  // base in other unit
  TTestObj6= object(ua_TTestObj4) // layout is subset of TTestClass1
    // can't have those in the other unit
    FldC_H:   TTestChildObject;
    FldC_HX:  TTestChildObjectX;

    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;

    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;
  end;
  TTestObj6X = type TTestObj6;


function TTestXObjV1.PO_1: integer; begin Result := 1; end;

function  TTestObj3.ua_Func1: ua_TTestInt1;  begin Result := default(type of result); end;
function  TTestObj3.ua_Func1X: ua_TTestInt1X;  begin Result := default(type of result); end;
procedure TTestObj3.ua_Proc1(a: ua_TTestInt1);  begin {} end;
procedure TTestObj3.ua_Proc1X(a: ua_TTestInt1X);  begin {} end;
function  TTestObj3.ua_Func2(AnIdx:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  TTestObj3.ua_Func2X(AnIdx:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure TTestObj3.ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1); begin {} end;
procedure TTestObj3.ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X); begin {} end;
function  TTestObj3.ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  TTestObj3.ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure TTestObj3.ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1); begin {} end;
procedure TTestObj3.ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X); begin {} end;

function  TTestObj4.ua_Func1: ua_TTestInt1;  begin Result := default(type of result); end;
function  TTestObj4.ua_Func1X: ua_TTestInt1X;  begin Result := default(type of result); end;
procedure TTestObj4.ua_Proc1(a: ua_TTestInt1);  begin {} end;
procedure TTestObj4.ua_Proc1X(a: ua_TTestInt1X);  begin {} end;
function  TTestObj4.ua_Func2(AnIdx:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  TTestObj4.ua_Func2X(AnIdx:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure TTestObj4.ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1); begin {} end;
procedure TTestObj4.ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X); begin {} end;
function  TTestObj4.ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  TTestObj4.ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure TTestObj4.ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1); begin {} end;
procedure TTestObj4.ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X); begin {} end;

var
  Obj1:   TTestObj1;
  Obj1X:  TTestObj1X;
  ObjX1:  TTestXObj1;
  ObjX1X: TTestXObj1X;
  ObjV1:   TTestObjV1;
  ObjV1X:  TTestObjV1X;
  ObjVX1:  TTestXObjV1;
  ObjVX1X: TTestXObjV1X;
  Obj3:  TTestObj3;
  Obj3X: TTestObj3X;
  Obj5:  TTestObj5;
  Obj5X: TTestObj5X;
  ObjX5:  TTestObjX5;
  ObjX5X: TTestObjX5X;
  Obj6:  TTestObj6;
  Obj6X: TTestObj6X;

type
  // class
  TTestClass1 = class(ua_TTestClass1)
  public
    class var
      FVarC1: integer;
      class function P_CLass: integer; static;
  public type
    TSpecializedTestObject2 = specialize ua_GTestObject2<integer>;
  public
    FSpecial1: specialize ua_GTestObject1<integer>;
    FSpecial2: specialize ua_GTestObject2<integer>;
    FSpecial3: TSpecializedTestObject2;
    FldC1: integer;
    //function PFoo1: integer;

    FldC_H:   TTestChildObject;
    FldC_HX:  TTestChildObjectX;

    property P_Special1: integer read FSpecial1.Fld_GA;
    property P_Special12: integer read FSpecial2.Fld_GB.Fld_GA;
    property P_Special13: integer read FSpecial3.Fld_GB.Fld_GA;

    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_Ar:  integer read ua_FldC_A;
    property P_ua_FldD_Arw: integer read ua_FldC_A write ua_FldC_B;
    property P_ua_FldD_Aw:  integer                write ua_FldC_B;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;


    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;

    //TODO: flisd: record_stat_array[1].fld
    class property P_VarC1: integer read FVarC1;
    class property P_ClassC1: integer read P_CLass;
  end;
  TTestClass1X = type TTestClass1;

  TTestClass3 = class
  public
    ua_FldC_A: integer;
    ua_FldC_B: integer;
    ua_FldC_C: ua_TTestClass1.ua_TTestClass1Nest1;
    // Issue #42082
    //ua_FldC_CX: ua_TTestClass1.ua_TTestClass1Nest1X;
    ua_FldC_CX: ua_TTestClass1.ua_TTestClass1Nest1;
    ua_FldC_D: ua_TTestClass1.ua_TTestClass1Nest2Rec;
    ua_FldC_DX: ua_TTestClass1.ua_TTestClass1Nest2RecX;
    ua_FldC_E: ua_TTestRec2;
    ua_FldC_EX: ua_TTestRec2X;
    ua_FldC_G:   ua_TTestRec2_array;
    ua_FldC_GX:  ua_TTestRec2X_array;
    ua_FldC_XG:  ua_TTestRec2_arrayX;
    ua_FldC_XGX: ua_TTestRec2X_arrayX;
    ua_FldC_YG:   array [0..3] of ua_TTestRec2;
    ua_FldC_YGX:  array [0..3] of ua_TTestRec2X;
    ua_FldC_H:   ua_TTestChildObject;
    ua_FldC_HX:  ua_TTestChildObjectX;
    ua_FldC_XH:   ua_TTestXChildObject;
    ua_FldC_XHX:  ua_TTestXChildObjectX;
    FldC_H:   TTestChildObject;
    FldC_HX:  TTestChildObjectX;

    function  ua_Func1: ua_TTestInt1; virtual; abstract;
    function  ua_Func1X: ua_TTestInt1X; virtual; abstract;
    procedure ua_Proc1(a: ua_TTestInt1); virtual; abstract;
    procedure ua_Proc1X(a: ua_TTestInt1X); virtual; abstract;

    function  ua_Func2(AnIdx:integer): ua_TTestInt1; virtual; abstract;
    function  ua_Func2X(AnIdx:integer): ua_TTestInt1X; virtual; abstract;
    procedure ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1); virtual; abstract;
    procedure ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X); virtual; abstract;
    function  ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1; virtual; abstract;
    function  ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X; virtual; abstract;
    procedure ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1); virtual; abstract;
    procedure ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X); virtual; abstract;


    property P_ua_FldD_A: integer read ua_FldC_A;

    property P_ua_FldD_D_A: integer read ua_FldC_D.ua_Nest2FldR_A;
    property P_ua_FldD_DX_A: integer read ua_FldC_DX.ua_Nest2FldR_A;

    property P_ua_FldD_E_A: integer read ua_FldC_E.ua_FldR2_A;

    property P_ua_FldD_E_BA: integer  read ua_FldC_E.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_E_BXA: integer read ua_FldC_E.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_E_CA: integer  read ua_FldC_E.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_E_CXA: integer read ua_FldC_E.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_E_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_EX_BA: integer  read ua_FldC_EX.ua_FldR2_B.   ua_FldR_A;
    property P_ua_FldD_EX_BXA: integer read ua_FldC_EX.ua_FldR2_BX.  ua_FldR_A;
    property P_ua_FldD_EX_CA: integer  read ua_FldC_EX.ua_FldR2_C. c.ua_FldR_A;
    property P_ua_FldD_EX_CXA: integer read ua_FldC_EX.ua_FldR2_CX.c.ua_FldR_A;
    property P_ua_FldD_EX_Ci: integer  read ua_FldC_E.ua_FldR2_CX.i;

    property P_ua_FldD_G_1:   ua_TTestRec2  read ua_FldC_G[1];
    property P_ua_FldD_GX_1:  ua_TTestRec2X read ua_FldC_GX[1];
    property P_ua_FldD_XG_1:  ua_TTestRec2  read ua_FldC_XG[1];
    property P_ua_FldD_XGX_1: ua_TTestRec2X read ua_FldC_XGX[1];
    property P_ua_FldD_GY_1:  ua_TTestRec2  read ua_FldC_YG[1];
    property P_ua_FldD_GYX_1: ua_TTestRec2X read ua_FldC_YGX[1];

    property P_ua_FldD_G_A:   integer read ua_FldC_G[1].ua_FldR2_A;
    property P_ua_FldD_GX_A:  integer read ua_FldC_GX[1].ua_FldR2_A;
    property P_ua_FldD_XG_A:  integer read ua_FldC_XG[1].ua_FldR2_A;
    property P_ua_FldD_XGX_A: integer read ua_FldC_XGX[1].ua_FldR2_A;
    property P_ua_FldD_GY_A:  integer read ua_FldC_YG[1].ua_FldR2_A;
    property P_ua_FldD_GYX_A: integer read ua_FldC_YGX[1].ua_FldR2_A;

    property P_ua_Func1:     ua_TTestInt1 read ua_Func1;
    property P_ua_Func1_rw:  ua_TTestInt1 read ua_Func1 write ua_Proc1;
    property P_ua_Func1_w:   ua_TTestInt1               write ua_Proc1;
    property P_ua_Func1X:    ua_TTestInt1X read ua_Func1X;
    property P_ua_Func1X_rw: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
    property P_ua_Func1X_w:  ua_TTestInt1X                write ua_Proc1X;

    property P_ua_Func2[AnIdx:integer]:     ua_TTestInt1 read ua_Func2;
    property P_ua_Func2_rw[AnIdx:integer]:  ua_TTestInt1 read ua_Func2 write ua_Proc2;
    property P_ua_Func2_w[AnIdx:integer]:   ua_TTestInt1               write ua_Proc2;
    property P_ua_Func2X[AnIdx:integer]:     ua_TTestInt1X read ua_Func2X;
    property P_ua_Func2X_rw[AnIdx:integer]:  ua_TTestInt1X read ua_Func2X write ua_Proc2X;
    property P_ua_Func2X_w[AnIdx:integer]:   ua_TTestInt1X                write ua_Proc2X;

    property P_ua_Func2i:     ua_TTestInt1 index 42 read ua_Func2;
    property P_ua_Func2i_rw:  ua_TTestInt1 index 42 read ua_Func2 write ua_Proc2;
    property P_ua_Func2i_w:   ua_TTestInt1 index 42               write ua_Proc2;
    property P_ua_Func2iX:     ua_TTestInt1X index 42 read ua_Func2X;
    property P_ua_Func2iX_rw:  ua_TTestInt1X index 42 read ua_Func2X write ua_Proc2X;
    property P_ua_Func2iX_w:   ua_TTestInt1X index 42                write ua_Proc2X;

    property P_ua_Func3[AnIdx, AnIdx2:integer]:     ua_TTestInt1 read ua_Func3;
    property P_ua_Func3_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1 read ua_Func3 write ua_Proc3;
    property P_ua_Func3_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1               write ua_Proc3;
    property P_ua_Func3X[AnIdx, AnIdx2:integer]:     ua_TTestInt1X read ua_Func3X;
    property P_ua_Func3X_rw[AnIdx, AnIdx2:integer]:  ua_TTestInt1X read ua_Func3X write ua_Proc3X;
    property P_ua_Func3X_w[AnIdx, AnIdx2:integer]:   ua_TTestInt1X                write ua_Proc3X;

    property P_ua_Func3i[AnIdx:integer]:     ua_TTestInt1 index -42 read ua_Func3;
    property P_ua_Func3i_rw[AnIdx:integer]:  ua_TTestInt1 index -42 read ua_Func3 write ua_Proc3;
    property P_ua_Func3i_w[AnIdx:integer]:   ua_TTestInt1 index -42               write ua_Proc3;
    property P_ua_Func3iX[AnIdx:integer]:     ua_TTestInt1X index -42 read ua_Func3X;
    property P_ua_Func3iX_rw[AnIdx:integer]:  ua_TTestInt1X index -42 read ua_Func3X write ua_Proc3X;
    property P_ua_Func3iX_w[AnIdx:integer]:   ua_TTestInt1X index -42                write ua_Proc3X;

    property P_ua_FldC_H:   integer read ua_FldC_H.FldBase1;
    property P_ua_FldC_HX:  integer read ua_FldC_HX.FldBase1;
    property P_ua_FldC_XH:  integer read ua_FldC_XH.FldBase1;
    property P_ua_FldC_XHX: integer read ua_FldC_XHX.FldBase1;
    property P_FldC_H:   integer read FldC_H.FldBase1;
    property P_FldC_HX:  integer read FldC_HX.FldBase1;
  end;
  TTestClass3X = type TTestClass3;


  TTestClassIntf1 = class(TObject, ua_TIntfCorba2)
    function ua_PI1: integer; virtual; abstract;
    function ua_PI2: integer; virtual; abstract;
    function  ua_Func1: ua_TTestInt1; virtual; abstract;
    function  ua_Func1X: ua_TTestInt1X; virtual; abstract;
    procedure ua_Proc1(a: ua_TTestInt1); virtual; abstract;
    procedure ua_Proc1X(a: ua_TTestInt1X); virtual; abstract;
    function ua_PI3: ua_TIntfCorba1; virtual; abstract;
    function ua_PI3X: ua_TIntfCorba1X; virtual; abstract;
  end;
  TTestClassIntf2 = class(TInterfacedObject, ua_TIntfCom2)
    function ua_PJ1: integer; virtual; abstract;
    function ua_PJ2: integer; virtual; abstract;
    function  ua_Func1: ua_TTestInt1; virtual; abstract;
    function  ua_Func1X: ua_TTestInt1X; virtual; abstract;
    procedure ua_Proc1(a: ua_TTestInt1); virtual; abstract;
    procedure ua_Proc1X(a: ua_TTestInt1X); virtual; abstract;
    function ua_PJ3: ua_TIntfCorba1; virtual; abstract;
    function ua_PJ3X: ua_TIntfCorba1X; virtual; abstract;
  end;

class function TTestClass1.P_CLass: integer; begin Result := 1; end;

var
  Class1: TTestClass1;
  Class1X: TTestClass1X;
  Class3: TTestClass3;
  Class3X: TTestClass3X;
  ClassIntf1: TTestClassIntf1;
  ClassIntf2: TTestClassIntf2;
  Intf1: ua_TIntfCorba2;
  Intf2: ua_TIntfCom2;

type
  // interface
  {$Interfaces CORBA}
  TIntfCorba1 = interface(ua_TIntfCorba1) ['{BEEF143F-D3EC-419C-B818-D313A4957988}']
    function PI1: ua_TIntfCorba1;
    function PI1X: ua_TIntfCorba1X;

    function  Func1: ua_TTestInt1;
    function  Func1X: ua_TTestInt1X;
    procedure Proc1(a: ua_TTestInt1);
    procedure Proc1X(a: ua_TTestInt1X);

    property P_I_1: ua_TTestInt1   read Func1  write Proc1;
    property P_I_1X: ua_TTestInt1X read Func1X write Proc1X;
    property P_uaI_1: ua_TTestInt1   read ua_Func1  write ua_Proc1;
    property P_uaI_1X: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
  end;
  TIntfCorba1X = type TIntfCorba1;

  {$Interfaces COM}
  TIntfCom1 = interface(ua_TIntfCom1) ['{2BF620A8-018A-41A8-836A-28B44B041054}']
    function PJ1: ua_TIntfCom1;
    function PJ1X: ua_TIntfCom1X;

    function  Func1: ua_TTestInt1;
    function  Func1X: ua_TTestInt1X;
    procedure Proc1(a: ua_TTestInt1);
    procedure Proc1X(a: ua_TTestInt1X);

    property P_I_1: ua_TTestInt1   read Func1  write Proc1;
    property P_I_1X: ua_TTestInt1X read Func1X write Proc1X;
    property P_uaI_1: ua_TTestInt1   read ua_Func1  write ua_Proc1;
    property P_uaI_1X: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
  end;
  TIntfCom1X = type TIntfCom1;

var
  IntfCorba1:  TIntfCorba1;
  IntfCorba1X: TIntfCorba1X;
  IntfCom1:  TIntfCom1;
  IntfCom1X: TIntfCom1X;

function Func1: integer;  begin Result := default(type of result); end;
procedure Proc1(a: integer);  begin {} end;

property P_uaGlobal: integer read ua_Func1 write ua_Proc1;
property P_Global: integer read ua_Func1 write ua_Proc1;


procedure init;
begin
  Rec1.FldA := 1;
  Rec1X.FldA := Rec1.FldA;
  Rec1Y.FldA := Rec1.FldA;
  Rec1XX.FldA := Rec1.FldA;
  RecXUa1.ua_FldR_A := 1;
  RecXUa1X.ua_FldR_A := Rec1.FldA;
  Rec3.ua_FldC_A := 11;
  Rec3x.ua_FldC_A := 12;

  Obj1.FldA := 1;
  Obj1X.FldA := 1;
  ObjX1.FldA := 1;
  ObjX1X.FldA := 1;

  ObjV1.FldA := 1;
  ObjV1X.FldA := 1;
  ObjVX1.FldA := 1;
  ObjVX1X.FldA := 1;

  Obj3.ua_FldC_A := 11;
  Obj3x.ua_FldC_A := 12;
  Obj5.ua_FldC_A := 11;
  Obj5x.ua_FldC_A := 12;
  ObjX5.ua_FldC_A := 11;
  ObjX5x.ua_FldC_A := 12;
  Obj6.ua_FldC_A := 11;
  Obj6x.ua_FldC_A := 12;

  Class1 := TTestClass1.Create;
  Class1X := TTestClass1X.Create;
  Class3 := TTestClass3.Create;
  Class3X := TTestClass3X.Create;
  ClassIntf1:= TTestClassIntf1.Create;
  ClassIntf2:= TTestClassIntf2.Create;
  Intf1:= ClassIntf1;
  Intf2:= ClassIntf2;
  if false then Intf1.ua_Func1;
  if false then Intf2.ua_Func1;


  IntfCorba1:= nil;
  IntfCorba1X:= nil;
  IntfCom1:= nil;
  IntfCom1X:= nil;
end;


