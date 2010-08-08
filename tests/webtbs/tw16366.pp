{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$mode objfpc}
{$modeswitch objectivec1}

type
  ta = array of nsobject;
var
  a: ta;
  i: longint;
begin
  setlength(a,5);
  for i := low(a) to high(a) do
    begin
      if a[i]<>nil then
        halt(1);
      { crash if the rtl tries to "finalise" the nsobject elements }
      a[i]:=nsobject(i*10000+12345);
    end;
end.
