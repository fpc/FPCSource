{ %norun }
library tw12987a;


procedure test;
  var
    p1,p2 : pointer;
    i : longint;
  begin
    for i:=1 to 200000 do
      begin
        getmem(p1,random(1000));
        getmem(p2,random(100));
        freemem(p1);
        freemem(p2);
      end;
  end;

exports
  test;

begin
end.
