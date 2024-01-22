program tw40594;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TNotifyProc = reference to procedure(aArg: LongInt{Sender : TObject});

Procedure DoCall(aProc : TNotifyProc; aArg: LongInt);

begin
  aProc(aArg);
end;

Procedure DoTest;

var
  a: LongInt;

  procedure HandleCall(aArg: LongInt{Sender : TObject});
  begin
    //Writeln('Nil passed: ',Sender=Nil);
    a := aArg;
  end;

var
 p : TNotifyProc;

begin
  P:=@HandleCall;
  a := 0;
  DoCall(P, 42); // OK
  if a <> 42 then
    Halt(1);
  DoCall(@HandleCall, 21); // Not OK
  if a <> 21 then
    Halt(2);
end;

begin
  DoTest;
end.

