{ Source provided for Free Pascal Bug Report 2713 }
{ Submitted by "Wiktor Sywula" on  2003-10-05 }
{ e-mail: wswiktor@poczta.fm }
UNIT tw2713;

{$MODE OBJFPC}

INTERFACE

type trek=record
        a,b,c:word;
     end;

procedure rek(var r:trek; a,b,c:word);
function rek(a,b,c:word):trek;

IMPLEMENTATION

procedure rek(var r:trek; a,b,c:word);
begin
  r.a:=a;
  r.b:=b;
  r.c:=c;
end;

function rek(a,b,c:word):trek;
begin
  rek(rek,a,b,c);
end;


END.
