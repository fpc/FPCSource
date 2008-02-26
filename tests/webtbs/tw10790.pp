{ %OPT=-gh }

{$ifdef fpc}
{$mode delphi}
{$endif}

program failtest;

type
   TMyClass = class
      constructor Create;
   end;
    
constructor TMyClass.Create;
begin
   Fail;
end;

var
   MyClass : TMyClass;

begin
   HaltOnNotReleased := true;
   MyClass := TMyClass.Create;
end.
