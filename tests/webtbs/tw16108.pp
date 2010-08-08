uses
  Math;
var
  i: longint;
  ires: longint;
  qres: int64;
begin
  randomize;
  for i:=1 to 100 do
    begin
      ires:=randomrange(-4,-7);
      if (ires<-7) or
         (ires>=-4) then
        halt(1);
      ires:=randomrange(-7,-4);
      if (ires<-7) or
         (ires>=-4) then
        halt(2);
      ires:=randomrange(10,-3);
      if (ires<-3) or
         (ires>=10) then
        halt(3);
      ires:=randomrange(-3,10);
      if (ires<-3) or
         (ires>=10) then
        halt(4);
      ires:=randomrange(5,10);
      if (ires<5) or
         (ires>=10) then
        halt(5);
      ires:=randomrange(10,5);
      if (ires<5) or
         (ires>=10) then
        halt(6);

      qres:=randomrange(low(int64)+1,low(int64)+10);
      if (qres<low(int64)+1) or
         (qres>=low(int64)+10) then
        halt(7);
      qres:=randomrange(low(int64)+10,low(int64)+1);
      if (qres<low(int64)+1) or
         (qres>=low(int64)+10) then
        halt(8);
      qres:=randomrange(int64(10),int64(-3));
      if (qres<-3) or
         (qres>=10) then
        halt(3);
      qres:=randomrange(int64(-3),int64(10));
      if (qres<-3) or
         (qres>=10) then
        halt(4);
      qres:=randomrange(int64(5),int64(10));
      if (qres<5) or
         (qres>=10) then
        halt(5);
      qres:=randomrange(int64(10),int64(5));
      if (qres<5) or
         (qres>=10) then
        halt(6);
    end;
end.

