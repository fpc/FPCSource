{ %KNOWNRUNERROR=2 v1.0 computes binary nodes with longint and cardinals as cardinals }
{ Testing longint and cardinal addtions }
{ The current 1.0 compiler does handle these operations
  differently depending on range check state,
  which is rather bad thing PM }
const
  has_errors : boolean = false;
  has_severe_errors : boolean = false;

procedure fail(a,b,c,d : int64;range_check_on : boolean);
var
  r1,r2 : longint;
begin
  Write('Error: ',a,'+',b,' does not give ',c,' but ',d,'($',hexstr(d,16),') with $R');
  if range_check_on then
    Writeln('+')
  else
    Writeln('-');
  has_errors:=true;
{$R-}
  r1:=c;
  r2:=d;
  if r1<>r2 then
    has_severe_errors:=true;
end;

var
  a,b,c : longint;
  d,e,f : cardinal;
  res,res2 : int64;
begin
  a:=2;
  b:=-2;
  c:=-5;
  d:=1;
  e:=$ffffffff;
  f:=$fffffffe;

{$R+}

  res:=a+d;
  if res<>3 then
    fail(a,d,3,res,true);

  res:=a+e;
  res2:=e;
  res2:=res2+a;
  if (res-2<>e) or ((res and $ffff) <>1) or (res<>res2) then
    fail(a,e,res2,res,true);

  res:=a+f;
  res2:=f;
  res2:=res2+a;
  if (res-2<>f) or ((res and $ffff) <>0) or (res<>res2) then
    fail(a,f,res2,res,true);

  res:=b+d;
  if res<>-1 then
    fail(b,d,-1,res,true);

  res:=b+e;
  res2:=e;
  res2:=res2+b;
  if (res+2<>e) or ((res and $ffff) <>$fffd) or (res<>res2) then
    fail(b,e,res2,res,true);

  res:=b+f;
  res2:=f;
  res2:=res2+b;
  if (res+2<>f) or ((res and $ffff) <>$fffc) or (res<>res2) then
    fail(b,f,res2,res,true);

  res:=c+d;
  if res<>-4 then
    fail(c,d,-4,res,true);

  res:=c+e;
  res2:=e;
  res2:=res2+c;
  if (res+5<>e) or ((res and $ffff) <>$fffa) or (res<>res2) then
    fail(c,e,res2,res,true);

  res:=c+f;
  res2:=f;
  res2:=res2+c;
  if (res+5<>f) or ((res and $ffff) <>$fff9) or (res<>res2) then
    fail(c,f,res2,res,true);

  res:=d+a;
  if res<>3 then
    fail(d,a,3,res,true);

  res:=e+a;
  res2:=e;
  res2:=res2+a;
  if (res-2<>e) or ((res and $ffff) <>1) or (res<>res2) then
    fail(e,a,res2,res,true);

  res:=f+a;
  res2:=f;
  res2:=res2+a;
  if (res-2<>f) or ((res and $ffff) <>0) or (res<>res2) then
    fail(f,a,res2,res,true);

  res:=d+b;
  if res<>-1 then
    fail(d,b,-1,res,true);

  res:=e+b;
  res2:=e;
  res2:=res2+b;
  if (res+2<>e) or ((res and $ffff) <>$fffd) or (res<>res2) then
    fail(e,b,res2,res,true);

  res:=f+b;
  res2:=f;
  res2:=res2+b;
  if (res+2<>f) or ((res and $ffff) <>$fffc) or (res<>res2) then
    fail(f,b,res2,res,true);

  res:=d+c;
  if res<>-4 then
    fail(d,c,-4,res,true);

  res:=e+c;
  res2:=e;
  res2:=res2+c;
  if (res+5<>e) or ((res and $ffff) <>$fffa) or (res<>res2) then
    fail(e,c,res2,res,true);

  res:=f+c;
  res2:=f;
  res2:=res2+c;
  if (res+5<>f) or ((res and $ffff) <>$fff9) or (res<>res2) then
    fail(f,c,res2,res,true);

{$R-}

  res:=a+d;
  if res<>3 then
    fail(a,d,3,res,false);

  res:=a+e;
  res2:=e;
  res2:=res2+a;
  if (res-2<>e) or ((res and $ffff) <>1) or (res<>res2) then
    fail(a,e,res2,res,false);

  res:=a+f;
  res2:=f;
  res2:=res2+a;
  if (res-2<>f) or ((res and $ffff) <>0) or (res<>res2) then
    fail(a,f,res2,res,false);

  res:=b+d;
  if res<>-1 then
    fail(b,d,-1,res,false);

  res:=b+e;
  res2:=e;
  res2:=res2+b;
  if (res+2<>e) or ((res and $ffff) <>$fffd) or (res<>res2) then
    fail(b,e,res2,res,false);

  res:=b+f;
  res2:=f;
  res2:=res2+b;
  if (res+2<>f) or ((res and $ffff) <>$fffc) or (res<>res2) then
    fail(b,f,res2,res,false);

  res:=c+d;
  if res<>-4 then
    fail(c,d,-4,res,false);

  res:=c+e;
  res2:=e;
  res2:=res2+c;
  if (res+5<>e) or ((res and $ffff) <>$fffa) or (res<>res2) then
    fail(c,e,res2,res,false);

  res:=c+f;
  res2:=f;
  res2:=res2+c;
  if (res+5<>f) or ((res and $ffff) <>$fff9) or (res<>res2) then
    fail(c,f,res2,res,false);

  res:=d+a;
  if res<>3 then
    fail(d,a,3,res,false);

  res:=e+a;
  res2:=e;
  res2:=res2+a;
  if (res-2<>e) or ((res and $ffff) <>1) or (res<>res2) then
    fail(e,a,res2,res,false);

  res:=f+a;
  res2:=f;
  res2:=res2+a;
  if (res-2<>f) or ((res and $ffff) <>0) or (res<>res2) then
    fail(f,a,res2,res,false);

  res:=d+b;
  if res<>-1 then
    fail(d,b,-1,res,false);

  res:=e+b;
  res2:=e;
  res2:=res2+b;
  if (res+2<>e) or ((res and $ffff) <>$fffd) or (res<>res2) then
    fail(e,b,res2,res,false);

  res:=f+b;
  res2:=f;
  res2:=res2+b;
  if (res+2<>f) or ((res and $ffff) <>$fffc) or (res<>res2) then
    fail(f,b,res2,res,false);

  res:=d+c;
  if res<>-4 then
    fail(d,c,-4,res,false);

  res:=e+c;
  res2:=e;
  res2:=res2+c;
  if (res+5<>e) or ((res and $ffff) <>$fffa) or (res<>res2) then
    fail(e,c,res2,res,false);

  res:=f+c;
  res2:=f;
  res2:=res2+c;
  if (res+5<>f) or ((res and $ffff) <>$fff9) or (res<>res2) then
    fail(f,c,res2,res,false);

  if {$R-} a+e <> {$R+} a+e then
    has_severe_errors:=true;
  if has_severe_errors then
    halt(1);

  if has_errors then
    halt(2);
end.
