program tobject6;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TR = object
  private
    type
      tsometype = integer;
    class var
      ffield1: tsometype;
    var
      ffield2: string;
    const
      somevalue = 1;
    class procedure SetField1(const Value: tsometype); static;
  public
    class property field1: tsometype read ffield1 write SetField1;
  end;

{ TR }

class procedure TR.SetField1(const Value: tsometype);
begin
  ffield1 := Value;
end;

begin
  TR.field1 := 10;
  if TR.field1 <> 10 then
    halt(1);
  WriteLn(TR.somevalue);
  if TR.somevalue <> 1 then
    halt(2);
  WriteLn('ok');
end.      
