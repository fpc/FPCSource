program tb0614;

{$mode objfpc}
{$modeswitch nestedprocvars}

type
  tobjectmethod = procedure of object;
  tnestedprocvar = procedure is nested;

  TMyClass = class
    procedure Moo;
  end;

var
  obj: TMyClass;
  NumCalls: Integer;

procedure TMyClass.Moo;
begin
  Writeln('TMyClass.Moo');
end;

function get_objmethod: tobjectmethod;
begin
  Writeln('get_objmethod');
  Inc(NumCalls);
  Result := @obj.Moo;
end;

function get_nestedprocvar: tnestedprocvar;
  procedure nested;
  begin
    Writeln('nested');
  end;
begin
  Writeln('get_nestedprocvar');
  Inc(NumCalls);
  Result := @nested;
end;

var
  Errors: Boolean = False;
begin
  NumCalls := 0;
  obj := TMyClass.Create;
  get_objmethod()();
  obj.Free;
  if NumCalls <> 1 then
  begin
    Writeln('Error: get_objmethod should have been called once, but instead it was called ', NumCalls, ' times');
    Errors := True;
  end;

  NumCalls := 0;
  get_nestedprocvar()();
  if NumCalls <> 1 then
  begin
    Writeln('Error: get_nestedprocvar should have been called once, but instead it was called ', NumCalls, ' times');
    Errors := True;
  end;

  if Errors then
  begin
    Writeln('Errors found!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end.
