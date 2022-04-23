{ %OPT=-vh }

program tw39684;
{$mode objfpc}
{$ModeSwitch ImplicitFunctionSpecialization }

{$warn 5028 error}

generic function Add<T>(aArg1, aArg2: T): T;
begin
  Result := aArg1 + aArg2;
end;

begin
  //specialize Add<byte>(byte(0), byte(0));  // No Hint
  Add(byte(0), byte(0));
end.

