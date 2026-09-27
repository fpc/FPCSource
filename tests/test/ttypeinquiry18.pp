{ type of sibling field, property or method inside a record/object/class declaration. }
program ttypeinquiry18;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TPoint = record
    X, Y: word;
  end;

  TRec = record
    a: int64;
    b: type of a;
    function GetB: word;
    function Twice(v: int64): int64;
    property PA: int64 read a;
    property PB: word read GetB;
  public type
    TA = type of a;
    TPA = type of PA;
    TPB = type of PB;
    TMB = type of GetB;
    TMT = type of Twice(1);
  end;

  TObj = object
    F: shortstring;
    G: type of F;
    function GetIdx(Index: integer): int64;
    function Virt: currency; virtual;
    constructor Init;
    property PF: shortstring read F;
    property PX: int64 index 3 read GetIdx;
  public type
    TF = type of F;
    TPF = type of PF;
    TPX = type of PX;
    TMI = type of GetIdx(1);
    TMV = type of Virt;
  end;

  TBird = class
  private
    function GetPG: word;
    function GetPI(Index: integer): byte;
    class function GetPC: currency; static;
    function Twice(v: int64): int64; overload;
    function Twice(v: single): double; overload;
    function Virt: shortstring; virtual;
    class function CF: int64;
  public
    F: integer;
    P: TPoint;
    G: type of F;
    Q: type of P;
    property PF: integer read F;
    property PG: word read GetPG;
    property PI[Index: integer]: byte read GetPI;
    class property PC: currency read GetPC;
    property PP: TPoint read P;
  public type
    T = type of F;
    TP = type of P;
    TX = type of P.X;
    TPF = type of PF;
    TPG = type of PG;
    TPI = type of PI[0];
    TPC = type of PC;
    TPY = type of PP.Y;
    TMG = type of GetPG;
    TMT = type of Twice(1);
    TMD = type of Twice(single(1.0));
    TMV = type of Virt;
    TMC = type of CF;
    TMS = type of GetPC;
  end;

  TChild = class(TBird)
    H: type of F;
    K: type of PG;
    M: type of GetPG;
  public type
    TH = type of Q;
    TK = type of PI[1];
    TMT2 = type of Twice(2);
    TMV2 = type of Virt;
  end;

function TRec.GetB: word;
begin
  Result:=0;
end;

function TRec.Twice(v: int64): int64;
begin
  Result:=2*v;
end;

function TObj.GetIdx(Index: integer): int64;
begin
  Result:=Index;
end;

function TObj.Virt: currency;
begin
  Result:=0;
end;

constructor TObj.Init;
begin
end;

function TBird.GetPG: word;
begin
  Result:=0;
end;

function TBird.GetPI(Index: integer): byte;
begin
  Result:=Index;
end;

class function TBird.GetPC: currency;
begin
  Result:=0;
end;

function TBird.Twice(v: int64): int64;
begin
  Result:=2*v;
end;

function TBird.Twice(v: single): double;
begin
  Result:=2*v;
end;

function TBird.Virt: shortstring;
begin
  Result:='';
end;

class function TBird.CF: int64;
begin
  Result:=0;
end;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

var
  r: TRec;
  ra: TRec.TA;
  o: TObj;
  of_: TObj.TF;
  bird: TBird;
  t: TBird.T;
  tp: TBird.TP;
  tx: TBird.TX;
  ch: TChild;
  th: TChild.TH;
begin
  { record }
  Check(SizeOf(r.b)=8,1);
  Check(SizeOf(ra)=8,2);
  Check(TypeInfo(TRec.TA)=TypeInfo(int64),3);
  r.b:=high(int64);
  Check(r.b=high(int64),4);
  Check(TypeInfo(TRec.TPA)=TypeInfo(int64),5);
  Check(TypeInfo(TRec.TPB)=TypeInfo(word),6);
  Check(TypeInfo(TRec.TMB)=TypeInfo(word),7);
  Check(TypeInfo(TRec.TMT)=TypeInfo(int64),8);

  { object }
  Check(SizeOf(o.G)=256,10);
  Check(SizeOf(of_)=256,11);
  Check(TypeInfo(TObj.TF)=TypeInfo(shortstring),12);
  Check(TypeInfo(TObj.TPF)=TypeInfo(shortstring),13);
  Check(TypeInfo(TObj.TPX)=TypeInfo(int64),14);
  Check(TypeInfo(TObj.TMI)=TypeInfo(int64),15);
  Check(TypeInfo(TObj.TMV)=TypeInfo(currency),16);

  { class }
  bird:=TBird.Create;
  Check(SizeOf(bird.G)=SizeOf(integer),20);
  Check(TypeInfo(TBird.T)=TypeInfo(integer),21);
  Check(SizeOf(t)=SizeOf(integer),22);
  Check(TypeInfo(TBird.TP)=TypeInfo(TPoint),23);
  Check(SizeOf(tp)=4,24);
  Check(TypeInfo(TBird.TX)=TypeInfo(word),25);
  Check(SizeOf(tx)=2,26);
  bird.Q.X:=3;
  tp:=bird.Q;
  Check(tp.X=3,27);
  Check(TypeInfo(TBird.TPF)=TypeInfo(integer),40);
  Check(TypeInfo(TBird.TPG)=TypeInfo(word),41);
  Check(TypeInfo(TBird.TPI)=TypeInfo(byte),42);
  Check(TypeInfo(TBird.TPC)=TypeInfo(currency),43);
  Check(TypeInfo(TBird.TPY)=TypeInfo(word),44);
  Check(TypeInfo(TBird.TMG)=TypeInfo(word),50);
  Check(TypeInfo(TBird.TMT)=TypeInfo(int64),51);
  Check(TypeInfo(TBird.TMD)=TypeInfo(double),52);
  Check(TypeInfo(TBird.TMV)=TypeInfo(shortstring),53);
  Check(TypeInfo(TBird.TMC)=TypeInfo(int64),54);
  Check(TypeInfo(TBird.TMS)=TypeInfo(currency),55);
  bird.Free;

  { inherited field, property and method }
  ch:=TChild.Create;
  Check(SizeOf(ch.H)=SizeOf(integer),30);
  Check(TypeInfo(TChild.TH)=TypeInfo(TPoint),31);
  th.Y:=5;
  Check(th.Y=5,32);
  Check(SizeOf(ch.K)=SizeOf(word),33);
  Check(TypeInfo(TChild.TK)=TypeInfo(byte),34);
  Check(SizeOf(ch.M)=SizeOf(word),35);
  Check(TypeInfo(TChild.TMT2)=TypeInfo(int64),36);
  Check(TypeInfo(TChild.TMV2)=TypeInfo(shortstring),37);
  ch.Free;

  writeln('ok');
end.
