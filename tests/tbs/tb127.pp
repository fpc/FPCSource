{ Old file: tbs0144.pp }
{ problem with 'with object do'                         OK 0.99.7 (PFV) }

program done_bug;

type
TObject = object
  Constructor Init;
  Destructor Done;
end;
PObject = ^TObject;

Constructor TObject.Init;
begin end;
Destructor TObject.Done;
begin end;

var P:PObject;

begin
New(P,Init);
with P^ do Done; { Compiler PANIC here ! }
Dispose(P);
end.
