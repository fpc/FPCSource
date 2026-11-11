(* included by different unit names - ensure no ppu is re-used with incorrect dwarf settings *)
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch recordcomposition}
interface

type
  (* record *)

  ua_TTestInt1 = -9..9;
  ua_TTestInt1X = type ua_TTestInt1;

  ua_PTestRec1  = ^ua_TTestRec1;
  ua_PTestRec1X = ^ua_TTestRec1X;
  ua_TTestRec1 = record
    public type
      ua_TTestRec1Nest1 = record
        ua_Nest1FldR_A: integer;
        ua_Nest1FldR_B: integer;
      end;
      ua_TTestRec1Nest1X = type ua_TTestRec1Nest1;
      ua_TTestRec1Nest2 = record
        ua_Nest2FldR_A: integer;
        ua_Nest2FldR_B: integer;
      end;
      ua_TTestRec1Nest2X = type ua_TTestRec1Nest2;
    public
    ua_FldR_A: integer;
    ua_FldR_B: integer;
    ua_FldR_C: ua_PTestRec1;
    ua_FldR_CX: ua_PTestRec1X;
    ua_FldR_D: ua_TTestRec1Nest1;
    ua_FldR_DX: ua_TTestRec1Nest1X;
    ua_FldR_R1: record R1: integer; end;
    ua_FldR_R2: record R2: integer; end;

    contains ua_TTestRec1Nest1X;
    contains ua_Nst2: ua_TTestRec1Nest2X;

    function ua_P1: integer;
    function ua_PN1: ua_TTestRec1Nest1;
    function ua_PN1X: ua_TTestRec1Nest1X;
    function ua_PN2(a: ua_TTestRec1): ua_TTestRec1Nest2;
    function ua_PN2X(a: ua_TTestRec1): ua_TTestRec1Nest2X;

    class operator = (a: integer; b: ua_TTestRec1): boolean;
    class operator < (a: integer; b: ua_TTestRec1): boolean;

    public type
      ua_ToTTestRec1Nest1 =  type of ua_FldR_D;
      ua_ToTTestRec1Nest1X = type of ua_FldR_DX;
      ua_XoTTestRec1Nest1 =  type type of ua_FldR_D;
      ua_XoTTestRec1Nest1X = type type of ua_FldR_DX;
      ua_XoTTestRec1RX = type type of ua_FldR_R2;
      ua_XoTTestRec1RY = type type of ua_FldR_R2;
    public
      ua_FldR_E:  ua_ToTTestRec1Nest1;
      ua_FldR_EX: ua_ToTTestRec1Nest1X;
      ua_FldR_F:  ua_XoTTestRec1Nest1;
      ua_FldR_FX: ua_XoTTestRec1Nest1X;
      ua_FldR_G:  type of ua_FldR_D;
      ua_FldR_GX: type of ua_FldR_DX;
      ua_FldR_R3: type of ua_FldR_R1;
      ua_FldR_R4: ua_XoTTestRec1RX;
      ua_FldR_R5: ua_XoTTestRec1Ry;
  end;
  ua_TTestRec1X = type ua_TTestRec1;
  ua_TTestRec1XX = type ua_TTestRec1;

  ua_TTestRec2 = record
    ua_FldR2_A: integer;
    ua_FldR2_B: ua_TTestRec1;
    ua_FldR2_BX: ua_TTestRec1X;
    ua_FldR2_C: record c: ua_TTestRec1; i: integer; end; // "i" has an un-named containing-structure
    ua_FldR2_CX: record c: ua_TTestRec1X; i: integer; end;
  end;
  ua_TTestRec2X = type ua_TTestRec2;

  ua_TTestRec2_array = array [0..3] of ua_TTestRec2;
  ua_TTestRec2_arrayX = type ua_TTestRec2_array;
  ua_TTestRec2X_array = array [0..3] of ua_TTestRec2X;
  ua_TTestRec2X_arrayX = type ua_TTestRec2X_array;

var
  UA_iA1: ua_TTestInt1;
  UA_iA2: ua_TTestInt1X;
  UA_Rec1: ua_TTestRec1;
  UA_Rec1X: ua_TTestRec1X;
  UA_Rec1XX: ua_TTestRec1XX;
  UA_PRec1: ua_PTestRec1;
  UA_PRec1X: ua_PTestRec1X;
  UA_Rec2: ua_TTestRec2;
  UA_Rec2X: ua_TTestRec2X;

type
  (* object *)

  ua_TTestObj1 = object
  public type
    ua_TTestObj1Nest1 = object
      ua_Nest1FldO_A: integer;
    end;
    ua_TTestObj1Nest1X = type ua_TTestObj1Nest1;

    ua_TTestObj1Nest2Rec = record
      ua_Nest2FldR_A: integer;
    end;
    ua_TTestObj1Nest2RecX = type ua_TTestObj1Nest2Rec;
  public
    ua_FldO_A: integer;
    ua_FldO_B: integer;
    ua_FldO_C: ua_TTestObj1Nest1;
    ua_FldO_CX: ua_TTestObj1Nest1X;
    ua_FldO_D: ua_TTestObj1Nest2Rec;
    ua_FldO_DX: ua_TTestObj1Nest2RecX;
    function ua_PO1: ua_TTestRec1;
    function ua_PO1X: ua_TTestRec1X;
    function ua_PO2: ua_TTestRec1;
    function ua_PO2X: ua_TTestRec1X;
  end;
// Issue # 42047 incorrectly creates inheritance
  ua_TTestObj1X = type ua_TTestObj1;

  ua_TTestObj2 = object(ua_TTestObj1)
  public
    ua_FldO_E: ua_TTestObj1;
    ua_FldO_EX: ua_TTestObj1X;
  end;
  ua_TTestObj2X = type ua_TTestObj2;

  ua_TTestXObj3 = object(ua_TTestObj1X) // inherit from alias
    ua_FldO_F: ua_TTestObj2;
    ua_FldO_FX: ua_TTestObj2X;
    function ua_PO2: ua_TTestRec1; reintroduce;
    function ua_PO2X: ua_TTestRec1X; reintroduce;
  end;
  ua_TTestXObj3X = type ua_TTestXObj3;

  (* object with constructor/vmt *)
  ua_TTestObjV1 = object
  public
    ua_FldO11_A: integer;
    ua_FldO11_B: integer;
    ua_FldO11_C: ua_TTestObj1.ua_TTestObj1Nest1;
    ua_FldO11_CX: ua_TTestObj1.ua_TTestObj1Nest1X;
    ua_FldO11_D: ua_TTestObj1.ua_TTestObj1Nest2Rec;
    ua_FldO11_DX: ua_TTestObj1.ua_TTestObj1Nest2RecX;
    constructor Create;
    function ua_PO11: ua_TTestRec1; virtual;
    function ua_PO11X: ua_TTestRec1X; virtual;
    function ua_PO12: ua_TTestRec1; virtual;
    function ua_PO12X: ua_TTestRec1X; virtual;
  end;
  //ua_TTestObjV1X = type ua_TTestObjV1;
  ua_TTestObjV1X = ua_TTestObjV1; // issue #42046

  ua_TTestObjV2 = object(ua_TTestObjV1)
  public
    ua_FldO11_E: ua_TTestObjV1;
    ua_FldO11_EX: ua_TTestObjV1X;
    constructor Create;
  end;
  //ua_TTestObjV2X = type ua_TTestObjV2;
  ua_TTestObjV2X = ua_TTestObjV2; // issue #42046

  ua_TTestXObjV3 = object(ua_TTestObjV1X) // inherit from alias
    ua_FldO11_F: ua_TTestObjV2;
    ua_FldO11_FX: ua_TTestObjV2X;
    constructor Create;
    function ua_PO12: ua_TTestRec1; virtual;
    function ua_PO12X: ua_TTestRec1X; virtual;
  end;
  //ua_TTestXObjV3X = type ua_TTestXObjV3;
  ua_TTestXObjV3X = ua_TTestXObjV3; // issue #42046

  // property access to field in base of object
  ua_TTestBaseObject = object
    FldBase1: integer;
  end;
  ua_TTestBaseObjectX = type ua_TTestBaseObject;

  ua_TTestChildObject = object(ua_TTestBaseObject)
    FldChild1: Integer;
  end;
  ua_TTestChildObjectX = type ua_TTestChildObject;

  ua_TTestXChildObject = object(ua_TTestBaseObjectX)
    FldChild1: Integer;
  end;
  ua_TTestXChildObjectX = type ua_TTestXChildObject;

var
  UA_Obj1:   ua_TTestObj1;
  UA_Obj1X:  ua_TTestObj1X;
  UA_Obj2:   ua_TTestObj2;
  UA_Obj2X:  ua_TTestObj2X;
  UA_ObjX3:  ua_TTestXObj3;
  UA_ObjX3X: ua_TTestXObj3X;
  UA_ObjV1:   ua_TTestObjV1;
  UA_ObjV1X:  ua_TTestObjV1X;
  UA_ObjV2:   ua_TTestObjV2;
  UA_ObjV2X:  ua_TTestObjV2X;
  UA_ObjXV3:  ua_TTestXObjV3;
  UA_ObjXV3X: ua_TTestXObjV3X;

type
  (* class *)

  ua_TTestClass1 = class
  public type
    ua_TTestClass1Nest1 = class
      ua_Nest1FldC_A: integer;
    end;
    ua_TTestClass1Nest1X = type ua_TTestClass1Nest1;

    ua_TTestClass1Nest2Rec = record
      ua_Nest2FldR_A: integer;
    end;
    ua_TTestClass1Nest2RecX = type ua_TTestClass1Nest2Rec;
  public
    ua_FldC_A:   integer;
    ua_FldC_B:   integer;
    ua_FldC_C:   ua_TTestClass1Nest1;
    ua_FldC_CX:  ua_TTestClass1Nest1X;
    ua_FldC_D:   ua_TTestClass1Nest2Rec;
    ua_FldC_DX:  ua_TTestClass1Nest2RecX;
    ua_FldC_E:   ua_TTestRec2;
    ua_FldC_EX:  ua_TTestRec2X;
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

    function  ua_Func1: ua_TTestInt1;      virtual; abstract;
    function  ua_Func1X: ua_TTestInt1X;    virtual; abstract;
    procedure ua_Proc1(a: ua_TTestInt1);   virtual; abstract;
    procedure ua_Proc1X(a: ua_TTestInt1X); virtual; abstract;

    function  ua_Func2(AnIdx:integer): ua_TTestInt1;      virtual; abstract;
    function  ua_Func2X(AnIdx:integer): ua_TTestInt1X;    virtual; abstract;
    procedure ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1);   virtual; abstract;
    procedure ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X); virtual; abstract;
    function  ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1;      virtual; abstract;
    function  ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X;    virtual; abstract;
    procedure ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1);   virtual; abstract;
    procedure ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X); virtual; abstract;

    function ua_PO1: ua_TTestRec1;
    function ua_PO1X: ua_TTestRec1X;
    function ua_PO2: ua_TTestRec1; virtual;
    function ua_PO2X: ua_TTestRec1X; virtual;
  end;
// Issue # 42047 incorrectly creates inheritance
  ua_TTestClass1X = type ua_TTestClass1;

  ua_TTestClass2 = Class(ua_TTestClass1)
  public
    ua_FldC2_A: ua_TTestClass1;
    ua_FldC2_AX: ua_TTestClass1X;
  end;
  ua_TTestClass2X = type ua_TTestClass2;

  ua_TTestXClass3 = Class(ua_TTestClass1X) // inherit from alias
    ua_FldC_F: ua_TTestClass2;
    ua_FldC_FX: ua_TTestClass2X;
    function ua_PO2: ua_TTestRec1; override;
    function ua_PO2X: ua_TTestRec1X; override;
  end;
  ua_TTestXClass3X = type ua_TTestXClass3;

  ua_TTestClassOF = type of ua_TTestClass1(nil);
  ua_TTestClassXOF = type type of ua_TTestClass1(nil);


var
  UA_Class1:   ua_TTestClass1;
  UA_Class1X:  ua_TTestClass1X;
  UA_Class2:   ua_TTestClass2;
  UA_Class2X:  ua_TTestClass2X;
  UA_ClassX3:  ua_TTestXClass3;
  UA_ClassX3X: ua_TTestXClass3X;
  UA_ClassOF4: ua_TTestClassOF;
  UA_ClassXOF4: ua_TTestClassXOF;

type

  generic ua_GTestObject1<A> = record
    Fld_GA: A;
  end;
  generic ua_GTestObject2<A> = object
    Fld_GB: specialize ua_GTestObject1<A>;
  end;

  ua_TTestObj4= object // layout is subset of TTestClass1
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
//  ua_TTestObj4X = type ua_TTestObj4;


type
  {$Interfaces CORBA}

  ua_TIntfCorba1 = interface ['{B3E5B020-FCF3-40A4-AF2C-79A7D299105E}']
    function ua_PI1: integer;
    function ua_PI2: integer;

    function  ua_Func1: ua_TTestInt1;
    function  ua_Func1X: ua_TTestInt1X;
    procedure ua_Proc1(a: ua_TTestInt1);
    procedure ua_Proc1X(a: ua_TTestInt1X);
  end;
  ua_TIntfCorba1X = type ua_TIntfCorba1;

  ua_TIntfCorba2 = interface(ua_TIntfCorba1) ['{A8CF3B4C-4767-4761-B27D-C7963619A068}']
    function ua_PI3: ua_TIntfCorba1;
    function ua_PI3X: ua_TIntfCorba1X;

    property P_uaI_1: ua_TTestInt1   read ua_Func1  write ua_Proc1;
    property P_uaI_1X: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
  end;
  ua_TIntfCorba2X = type ua_TIntfCorba2;

  {$Interfaces COM}

  ua_TIntfCom1 = interface ['{71BC853A-66B3-44B9-8CF9-7B1533A6E2E2}']
    function ua_PJ1: integer;
    function ua_PJ2: integer;

    function  ua_Func1: ua_TTestInt1;
    function  ua_Func1X: ua_TTestInt1X;
    procedure ua_Proc1(a: ua_TTestInt1);
    procedure ua_Proc1X(a: ua_TTestInt1X);
  end;
  ua_TIntfCom1X = type ua_TIntfCom1;

  ua_TIntfCom2 = interface(ua_TIntfCom1) ['{29C9CD3A-8846-4694-BAB0-4299C30E1EC6}']
    function ua_PJ3: ua_TIntfCorba1;
    function ua_PJ3X: ua_TIntfCorba1X;

    property P_uaI_1: ua_TTestInt1   read ua_Func1  write ua_Proc1;
    property P_uaI_1X: ua_TTestInt1X read ua_Func1X write ua_Proc1X;
  end;
  ua_TIntfCom2X = type ua_TIntfCom2;

var
  UA_IntfCorba1:  ua_TIntfCorba1;
  UA_IntfCorba1X: ua_TIntfCorba1X;
  UA_IntfCorba2:  ua_TIntfCorba2;
  UA_IntfCorba2X: ua_TIntfCorba2X;
  UA_IntfCom1:  ua_TIntfCom1;
  UA_IntfCom1X: ua_TIntfCom1X;
  UA_IntfCom2:  ua_TIntfCom2;
  UA_IntfCom2X: ua_TIntfCom2X;

function ua_Func1: integer;
procedure ua_Proc1(a: integer);

property ua_P_Global: integer read ua_Func1 write ua_Proc1;

implementation

function ua_TTestRec1.ua_P1: integer;
  procedure ua_P1_nest; begin Result := default(type of result); end;
begin Result := default(type of result); ua_P1_nest; end;
function ua_TTestRec1.ua_PN1: ua_TTestRec1Nest1;   begin Result := default(type of result); end;
function ua_TTestRec1.ua_PN1X: ua_TTestRec1Nest1X; begin Result := default(type of result); end;
function ua_TTestRec1.ua_PN2(a: ua_TTestRec1): ua_TTestRec1Nest2;   begin Result := default(type of result); end;
function ua_TTestRec1.ua_PN2X(a: ua_TTestRec1): ua_TTestRec1Nest2X; begin Result := default(type of result); end;
class operator ua_TTestRec1.= (a: integer; b: ua_TTestRec1): boolean; begin Result := default(type of result); end;
class operator ua_TTestRec1.< (a: integer; b: ua_TTestRec1): boolean; begin Result := default(type of result); end;

function ua_TTestObj1.ua_PO1: ua_TTestRec1;     begin Result := default(type of result); end;
function ua_TTestObj1.ua_PO1X: ua_TTestRec1X;   begin Result := default(type of result); end;
function ua_TTestObj1.ua_PO2: ua_TTestRec1;     begin Result := default(type of result); end;
function ua_TTestObj1.ua_PO2X: ua_TTestRec1X;   begin Result := default(type of result); end;
function ua_TTestXObj3.ua_PO2: ua_TTestRec1;    begin Result := default(type of result); end;
function ua_TTestXObj3.ua_PO2X: ua_TTestRec1X;  begin Result := default(type of result); end;

constructor ua_TTestObjV1.Create; begin {} end;
function ua_TTestObjV1.ua_PO11: ua_TTestRec1;     begin Result := default(type of result); end;
function ua_TTestObjV1.ua_PO11X: ua_TTestRec1X;   begin Result := default(type of result); end;
function ua_TTestObjV1.ua_PO12: ua_TTestRec1;     begin Result := default(type of result); end;
function ua_TTestObjV1.ua_PO12X: ua_TTestRec1X;   begin Result := default(type of result); end;
constructor ua_TTestObjV2.Create; begin {} end;
constructor ua_TTestXObjV3.Create; begin {} end;
function ua_TTestXObjV3.ua_PO12: ua_TTestRec1;    begin Result := default(type of result); end;
function ua_TTestXObjV3.ua_PO12X: ua_TTestRec1X;  begin Result := default(type of result); end;

function ua_TTestClass1.ua_PO1: ua_TTestRec1;     begin Result := default(type of result); end;
function ua_TTestClass1.ua_PO1X: ua_TTestRec1X;   begin Result := default(type of result); end;
function ua_TTestClass1.ua_PO2: ua_TTestRec1;     begin Result := default(type of result); end;
function ua_TTestClass1.ua_PO2X: ua_TTestRec1X;   begin Result := default(type of result); end;
function ua_TTestXClass3.ua_PO2: ua_TTestRec1;    begin Result := default(type of result); end;
function ua_TTestXClass3.ua_PO2X: ua_TTestRec1X;  begin Result := default(type of result); end;

function  ua_TTestObj4.ua_Func1: ua_TTestInt1;  begin Result := default(type of result); end;
function  ua_TTestObj4.ua_Func1X: ua_TTestInt1X;  begin Result := default(type of result); end;
procedure ua_TTestObj4.ua_Proc1(a: ua_TTestInt1);  begin {} end;
procedure ua_TTestObj4.ua_Proc1X(a: ua_TTestInt1X);  begin {} end;
function  ua_TTestObj4.ua_Func2(AnIdx:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  ua_TTestObj4.ua_Func2X(AnIdx:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure ua_TTestObj4.ua_Proc2(AnIdx:integer; AVal: ua_TTestInt1); begin {} end;
procedure ua_TTestObj4.ua_Proc2X(AnIdx:integer; AVal: ua_TTestInt1X); begin {} end;
function  ua_TTestObj4.ua_Func3(AnIdx1, AnIdx2:integer): ua_TTestInt1; begin Result := default(type of result); end;
function  ua_TTestObj4.ua_Func3X(AnIdx1, AnIdx2:integer): ua_TTestInt1X; begin Result := default(type of result); end;
procedure ua_TTestObj4.ua_Proc3(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1); begin {} end;
procedure ua_TTestObj4.ua_Proc3X(AnIdx1, AnIdx2:integer; AVal: ua_TTestInt1X); begin {} end;

function ua_Func1: integer;  begin Result := default(type of result); end;
procedure ua_Proc1(a: integer);  begin {} end;

initialization
  // use all types, so they get linked in

  UA_iA1 := 1;
  UA_iA2 := 1;
  UA_Rec1.ua_FldR_A := 1;
  UA_Rec1X.ua_FldR_A := 1;
  UA_Rec1XX.ua_FldR_A := 1;
  UA_PRec1 := nil;
  UA_PRec1X := nil;
  UA_Rec2.ua_FldR2_A := 1;
  UA_Rec2X.ua_FldR2_A := 1;

  UA_Obj1.ua_FldO_A := 1;
  UA_Obj1X.ua_FldO_A := 1;
  UA_Obj2.ua_FldO_A := 1;
  UA_Obj2X.ua_FldO_A := 1;
  UA_ObjX3.ua_FldO_A := 1;
  UA_ObjX3X.ua_FldO_A := 1;
  UA_ObjV1.ua_FldO11_A := 1;
  UA_ObjV1X.ua_FldO11_A := 1;
  UA_ObjV2.ua_FldO11_A := 1;
  UA_ObjV2X.ua_FldO11_A := 1;
  UA_ObjXV3.ua_FldO11_A := 1;
  UA_ObjXV3X.ua_FldO11_A := 1;

  UA_Class1   := ua_TTestClass1.Create;
  UA_Class1X  := ua_TTestClass1X.Create;
  UA_Class2   := ua_TTestClass2.Create;
  UA_Class2X  := ua_TTestClass2X.Create;
  UA_ClassX3  := ua_TTestXClass3.Create;
  UA_ClassX3X := ua_TTestXClass3X.Create;
  UA_ClassOF4 := ua_TTestClassOF.Create;
  UA_ClassXOF4:= ua_TTestClassXOF.Create;

  UA_IntfCorba1  := nil;
  UA_IntfCorba1X := nil;
  UA_IntfCorba2  := nil;
  UA_IntfCorba2X := nil;
  UA_IntfCom1  := nil;
  UA_IntfCom1X := nil;
  UA_IntfCom2  := nil;
  UA_IntfCom2X := nil;

end.
