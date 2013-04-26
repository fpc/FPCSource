program tw23568;

{$MODE DELPHI}

type
  TInteger32Boolean = record
  public
    Value: Integer;
    
    class operator Implicit(const Operand: TInteger32Boolean): Boolean;
  end;
  
{ TInteger32Boolean }

class operator TInteger32Boolean.Implicit(const Operand: TInteger32Boolean): Boolean;
begin
  Result := Operand.Value <> 0;
  WriteLn('In TInteger32Boolean.Implicit()');
end;

var
  Value:        TInteger32Boolean;
  Intermediate: Boolean;

begin
  // Assign True to TInteger32Boolean
  Value.Value := 1;
  
  // If statement using intermediate assignment through Intermediate
  Intermediate := Value;

  if Intermediate then
    WriteLn('True')
  else
    halt(1);
    
  // If statement should perform implicit type conversion from TInteger32Boolean to Boolean
  if Value then
    WriteLn('True')
  else
    halt(2);
    
  // While statement should perform implicit type conversion as well
  while Value do
  begin
    if Value.Value=0 then
      halt(3);
    Value.Value := 0;
    WriteLn('While');
  end;
  
  // Repeat until statement should perform implicit type conversion as well
  repeat 
    if Value.Value=1 then
      halt(4);
    Value.Value := 1;
    WriteLn('Repeat until');
  until Value;
end.
