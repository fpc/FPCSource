program tw25059;

{$mode delphi}

uses
  uw25059.test;

begin
  if ExecuteTest() <> 1 then
    Halt(1);
end.
