{ Old file: tbs0096.pp }
{ problem with objects as parameters                    OK 0.99.6 (PM) }

type
   TParent = object
   end;

   PParent = ^TParent;

   TChild = object(TParent)
   end;

procedure aProc(const x : TParent );
begin
end;

procedure anotherProc(var x : TParent );
begin
end;

var
   y : TChild;

   begin
      aProc(y);
      anotherProc(y);
   end.
