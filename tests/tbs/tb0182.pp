{ Old file: tbs0215.pp }
{ more bugss with static methods                        OK 0.99.11 (PM) }

{ allow static keyword }
{ submitted by Andrew Wilson }

Program X;

Type
   PY=^Y;
   Y=Object
      A : LongInt;
      P : PY; static;
      Constructor Init(NewA:LongInt);
      Procedure StaticMethod; static;
      Procedure VirtualMethod; virtual;
   End;

Constructor Y.Init(NewA:LongInt);
   Begin
      A:=NewA;
      P:=@self;
   End;

Procedure Y.StaticMethod;
   Begin
      Writeln(P^.A);    // Compiler complains about using A.
      P^.VirtualMethod; // Same with the virtual method.
      With P^ do begin
         Writeln(A);    // These two seem to compile, but I
         VirtualMethod; // can't get them to work. It seems to
      End;              // be the same problem as last time, so
   End;                 // I'll check it again when I get the
                        // new snapshot.
Procedure Y.VirtualMethod;
   Begin
      Writeln('VirtualMethod ',A);
   End;

var T1,T2 :  PY;

Begin
  New(T1,init(1));
  New(T2,init(2));
  T1^.VirtualMethod;
  T2^.VirtualMethod;
  Y.StaticMethod;
  T1^.StaticMethod;
  T2^.StaticMethod;
End.
