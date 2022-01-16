program genTest;

{$IFDEF FPC}{$mode Delphi}{$ENDIF}

type
  TTest<T: Record> = class(TObject)
    procedure testit();
  end;

procedure TTest<T>.testit();
begin
  WriteLn('=== ', 1 div SizeOf(T));
  if SizeOf(T) > 0 then
    WriteLn('I''m reachable!')
end;
  
begin
  TTest<Char>.Create().TestIt();
end.
