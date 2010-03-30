program tclass12d;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TR = class
  private
    type
      TSomeType = integer;
    const
      SomeValue: TSomeType = 1;
    class function GetSomeProp: TSomeType; static;
  public
    class property SomeProp: TSomeType read GetSomeProp;
  end;

class function TR.GetSomeProp: TSomeType;
begin
  Result := SomeValue;
end;

begin
  if TR.SomeValue <> 1 then
    halt(1);
  if TR.SomeProp <> 1 then
    halt(1);
  WriteLn('ok');
end.
