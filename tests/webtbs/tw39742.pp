{ %NORUN }

program tw39742;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}
{$ModeSwitch functionreferences}

type
  TIntFunction = reference to function: Integer;

// Works
function FourtyTwo(const AParam: Integer): TIntFunction;
function Helper: Integer;
begin
  Result := 42;
end;
begin
  Result := @Helper
end;

// Error
generic function GenericFourtyTwo<T>: TIntFunction;
function Helper: Integer;
begin
  Result := 42;
end;
begin
  Result := @Helper
end;

begin
end.

