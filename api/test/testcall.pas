program testcall;
uses CallSpec;

{ Testing program for calls of special functions. }

procedure TestLocal;
var
  I: Integer;

        procedure VoidLocal; {$ifndef FPC}far;{$endif}
        begin
          Write(I)
        end;

        procedure PointerLocal(J: LongInt); {$ifndef FPC}far;{$endif}
        begin
          Write('(', I, ',', J, ')')
        end;

begin
  I := 4711;
  Write('VoidLocal test: ');
  VoidLocal;
  Write(' = ');
  CallVoidLocal(@VoidLocal, CurrentFramePointer);
  WriteLn;

  Write('PointerLocal test: ');
  PointerLocal(0815);
  Write(' = ');
  CallPointerLocal(@PointerLocal, CurrentFramePointer, pointer(0815));
  WriteLn
end;

type
  TTest = object
    K: Integer;
    procedure TestMethodLocal;
  end;

var
  Test: TTest;

procedure TTest.TestMethodLocal;
var
  I: Integer;

        procedure VoidMethodLocal; {$ifndef FPC}far;{$endif}
        var
          t: Integer;
        begin
          t := K;
          Write('(', K, ',', I, ',', t, ')')
        end;

        procedure PointerMethodLocal(J: LongInt); {$ifndef FPC}far;{$endif}
        var
          t: Integer;
        begin
          t := K;
          Write('(', K, ',', I, ',', J, ',', t, ')')
        end;

begin
  I := 123;
  Write('VoidMethodLocal test: ');
  VoidMethodLocal;
  Write(' = ');
  CallVoidMethodLocal(@VoidMethodLocal,
    CurrentFramePointer, @Self);
  WriteLn;

  Write('PointerMethodLocal test: ');
  PointerMethodLocal(987);
  Write(' = ');
  CallPointerMethodLocal(@PointerMethodLocal,
    CurrentFramePointer, @Self, pointer(987));
  WriteLn
end;

type
  PA = ^TA;
  TA = object
    I: LongInt;
    constructor VoidInit;
    constructor PointerInit(P: Pointer);
    destructor Done;
    procedure VoidMethod;
    procedure PointerMethod(P: Pointer);
  end;

constructor TA.VoidInit;
begin
  I := 2718
end;

constructor TA.PointerInit(P: Pointer);
begin
  I := LongInt(P)
end;

destructor TA.Done;
begin
end;

procedure TestConstructor;
var
  P, Q: PA;
begin
  P := CallVoidConstructor(@TA.VoidInit, nil, TypeOf(TA));
  WriteLn('CallVoidConstructor test:  2718 = ', P^.I);
  Dispose(P,Done);
  Q := CallPointerConstructor(@TA.PointerInit, nil, TypeOf(TA), pointer(14142));
  WriteLn('CallPointerConstructor test:  14142 = ', Q^.I);
  Dispose(Q,Done);
end;

procedure TA.VoidMethod;
begin
  I := 2718;
end;

procedure TA.PointerMethod(P: Pointer);
begin
  I := LongInt(P)
end;

procedure TestMethod;
var
  A: TA;
begin
  CallVoidMethod(@TA.VoidMethod, @A);
  WriteLn('CallVoidMethod test:  2718 = ', A.I);
  CallPointerMethod(@TA.PointerMethod, @A, pointer(14142));
  WriteLn('CallPointerMethod test:  14142 = ', A.I);
end;

begin
  WriteLn('If the CallSpec unit is implemented properly for your');
  WriteLn('Pascal compiler, you will see 8 correct equations, and');
  WriteLn('this program will terminate without error.');
  WriteLn;

  TestLocal;

  Test.K := 3141;
  Test.TestMethodLocal;

  TestConstructor;

  TestMethod;

  WriteLn;
  WriteLn('Finished.');
end.
