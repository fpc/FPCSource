unit uunit1;

  interface

    var
       testvar : longint;

  implementation

    uses
       erroru;

initialization
  testvar:=1234567;
finalization
  if testvar<>1234567 then
    do_error(1001)
  else
    halt(0);
end.
