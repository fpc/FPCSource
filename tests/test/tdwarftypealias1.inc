(* included by different unit names - ensure no ppu is re-used with incorrect dwarf settings *)

type
  TTestInt1 = -9..9;
  TTestInt2 = type TTestInt1;

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
  end;
  TTestRec1X = type TTestRec1;
  TTestRec1Y = type TTestRec1; // 2nd alias
  TTestRec1XX = type TTestRec1X; // double alias

  TUa_TestRec1_X = type ua_TTestRec1; // alias to other unit
  TUa_TestRec1X_X = type ua_TTestRec1X; // double alias to other unit

function TTestRec1.P1: integer; begin Result := default(type of result); end;
function TTestRec1.PN0: ua_TTestRec1;   begin Result := default(type of result); end;
function TTestRec1.PN0X: ua_TTestRec1X; begin Result := default(type of result); end;
function TTestRec1.PN1: ua_TTestRec1.ua_TTestRec1Nest1;    begin Result := default(type of result); end;
function TTestRec1.PN1X: ua_TTestRec1.ua_TTestRec1Nest1X;  begin Result := default(type of result); end;
function TTestRec1.PN2: ua_TTestRec1X.ua_TTestRec1Nest2;   begin Result := default(type of result); end;
function TTestRec1.PN2X: ua_TTestRec1X.ua_TTestRec1Nest2X; begin Result := default(type of result); end;

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


function TTestXObjV1.PO_1: integer; begin Result := 1; end;

var
  Obj1:   TTestObj1;
  Obj1X:  TTestObj1X;
  ObjX1:  TTestXObj1;
  ObjX1X: TTestXObj1X;
  ObjV1:   TTestObjV1;
  ObjV1X:  TTestObjV1X;
  ObjVX1:  TTestXObjV1;
  ObjVX1X: TTestXObjV1X;

type
  // class
  TTestClass1 = class(ua_TTestClass1)
    FldC1: integer;
    //function PC_1: integer; // fails ptype
  end;
  TTestClass1X = type TTestClass1;

//function TTestClass1.PC_1: integer; begin Result := 1; end;

var
  Class1: TTestClass1;

type
  // interface
  {$Interfaces CORBA}
  TIntfCorba1 = interface(ua_TIntfCorba1) ['{BEEF143F-D3EC-419C-B818-D313A4957988}']
    function PI1: ua_TIntfCorba1;
    function PI1X: ua_TIntfCorba1X;
  end;
  TIntfCorba1X = type TIntfCorba1;

  {$Interfaces COM}
  TIntfCom1 = interface(ua_TIntfCom1) ['{2BF620A8-018A-41A8-836A-28B44B041054}']
    function PJ1: ua_TIntfCom1;
    function PJ1X: ua_TIntfCom1X;
  end;
  TIntfCom1X = type TIntfCom1;

var
  IntfCorba1:  TIntfCorba1;
  IntfCorba1X: TIntfCorba1X;
  IntfCom1:  TIntfCom1;
  IntfCom1X: TIntfCom1X;

procedure init;
begin
  Rec1.FldA := 1;
  Rec1X.FldA := Rec1.FldA;
  Rec1Y.FldA := Rec1.FldA;
  Rec1XX.FldA := Rec1.FldA;
  RecXUa1.ua_FldR_A := 1;
  RecXUa1X.ua_FldR_A := Rec1.FldA;

  Obj1.FldA := 1;
  Obj1X.FldA := 1;
  ObjX1.FldA := 1;
  ObjX1X.FldA := 1;

  ObjV1.FldA := 1;
  ObjV1X.FldA := 1;
  ObjVX1.FldA := 1;
  ObjVX1X.FldA := 1;

  Class1 := TTestClass1.Create;


  IntfCorba1:= nil;
  IntfCorba1X:= nil;
  IntfCom1:= nil;
  IntfCom1X:= nil;
end;



