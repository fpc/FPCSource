Program example83;

{ Program to demonstrate the Assigned function }

Procedure DoSomething;

begin
  Writeln ('Hello from doseomething!')
end;

Type
    TProcType = Procedure;

Var P : Pointer;
    Procvar : TProcType;

begin
  P:=Nil;
  If not Assigned(P) then
    Writeln('P is nil');
  Getmem(P,1000);
  If Assigned(P) Then
    writeln ('P Points in the heap.');
  FreeMem(P,1000);
  procvar:=@DoSomething;
  If Assigned(ProcVar) then
    Writeln ('Procvar is non-nil');
  procvar:=TProcType(Nil);
  If Not Assigned(Procvar) then
    Writeln ('Procvar is nil');
end.
