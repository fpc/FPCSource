{$MODE OBJFPC}
program test;

type
   TBaseClass = class
      function PrintSelf(): TBaseClass; inline; // has to be inline for the bug to manifest
   end;
   
   TSubClass = class(TBaseClass)
   end;

function TBaseClass.PrintSelf(): TBaseClass; inline;
begin
   Writeln(PtrUInt(Self));
   Result := nil;
   Writeln(PtrUInt(Self)); // prints 0!
   if not assigned(self) then
     halt(1);
end;

procedure NoOp(var Dummy: TBaseClass);
begin
end;


var
   Instance, Variable: TBaseClass;
   res: longint;
begin
   Instance := TSubClass.Create();
   Variable := nil;

   NoOp(Variable); // this call is important for the bug to manifest
   Variable := Instance;
   // object being invoked has to be cast to a different type for the bug to manifest
   // return value has to be assigned to the variable being used as "self"
   Variable := TSubClass(Variable).PrintSelf();

   Instance.Free();
end.
