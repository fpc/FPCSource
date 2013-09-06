{$mode delphi}

type
     TVector2=record
      case byte of
       0:(x,y:single);
       1:(u,v:single);
       2:(s,t:single);
       3:(xy:array[0..1] of single);
       4:(uv:array[0..1] of single);
       5:(st:array[0..1] of single);
     end;


function Vector2Length(const v:TVector2):single;
begin
 result:=sqrt(sqr(v.x)+sqr(v.y));
end;

function Vector2Sub(const v1,v2:TVector2):TVector2;
begin
 result.x:=v1.x-v2.x;
 result.y:=v1.y-v2.y;
end;

function Vector2Dist(const v1,v2:TVector2):single;
begin
 result:=Vector2Length(Vector2Sub(v2,v1));
end;

var
  v1, v2: tvector2;
begin
  v1.x:=2.0;
  v1.y:=3.0;
  v2.x:=5.0;
  v2.y:=7.0;
  if trunc(Vector2Dist(v1,v2))<>5 then
    halt(1);
end.
