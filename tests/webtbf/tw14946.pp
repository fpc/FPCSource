{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  ta = class
     procedure MyProc(var Msg); message 1; message 'abc'; 
  end;

procedure ta.myproc;
begin
end;

begin
end.
