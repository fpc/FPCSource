{ A sibling field as operand of "type of" inside a record/object/class
  declaration, where there is no self. }
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
  public type
    TA = type of a;
  end;

  TObj = object
    F: shortstring;
    G: type of F;
  public type
    TF = type of F;
  end;

  TBird = class
    F: integer;
    P: TPoint;
    G: type of F;
    Q: type of P;
  public type
    T = type of F;
    TP = type of P;
    TX = type of P.X;
  end;

  TChild = class(TBird)
    H: type of F;
  public type
    TH = type of Q;
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

  { object }
  Check(SizeOf(o.G)=256,10);
  Check(SizeOf(of_)=256,11);
  Check(TypeInfo(TObj.TF)=TypeInfo(shortstring),12);

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
  bird.Free;

  { inherited field }
  ch:=TChild.Create;
  Check(SizeOf(ch.H)=SizeOf(integer),30);
  Check(TypeInfo(TChild.TH)=TypeInfo(TPoint),31);
  th.Y:=5;
  Check(th.Y=5,32);
  ch.Free;

  writeln('ok');
end.
