{ %INTERACTIVE }

{ Source provided for Free Pascal Bug Report 1228 }
{ Submitted by "Rich Pasco" on  2000-11-10 }
{ e-mail: pasco@acm.org }
uses
  CRT;
var
  k: char;
begin
  WriteLn('Type a few words.  Hit Enter to quit:');
  repeat
    delay(400);
    k := ReadKey;
    if k=#0 then Readkey (* discard second half of function keys *)
    else write(K) (* but echo ASCII to screen *)
  until k=#$0D;   (* terminate with ENTER key *)
end.
