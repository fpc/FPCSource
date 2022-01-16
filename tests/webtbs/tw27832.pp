uses
  Math,
  DateUtils,
  SysUtils;

const
  dteps = TDateTime(0.5)/(24*60*60*1000);

procedure incday(var d,mo,y: longint; val: longint); overload;
begin
  { only month 12 -> 11 or 1 is supported }
  inc(d,val);
  if d>=32 then
    begin
      inc(mo);
      dec(d,31);
      if mo>=13 then
        begin
          inc(y);
          dec(mo,12);
        end;
    end
  else if d<=0 then
    begin
      dec(mo);
      inc(d,31);
    end;
end;

procedure inchour(var d,mo,y,h: longint; val: longint); overload;
begin
  inc(h,val);
  if h>=24 then
    begin
      incday(d,mo,y,h div 24);
      h:=h mod 24
    end
  else if h<0 then
    begin
      incday(d,mo,y,(h-23) div 24);
      h:=h mod 24;
      if h<>0 then
        h:=h+24
    end;
end;

procedure incmin(var d,mo,y,h,m: longint; val: longint);
begin
  inc(m,val);
  if m>=60 then
    begin
      inchour(d,mo,y,h,m div 60);
      m:=m mod 60
    end
  else if m<0 then
    begin
      inchour(d,mo,y,h,(m-59) div 60);
      m:=m mod 60;
      if m<>0 then
        m:=m+60;
    end;
end;

procedure incsec(var d,mo,y,h,m,s: longint; val: longint);
begin
  inc(s,val);
  if s>=60 then
    begin
      incmin(d,mo,y,h,m,s div 60);
      s:=s mod 60;
    end
  else if s<0 then
    begin
      incmin(d,mo,y,h,m,(s-59) div 60);
      s:=s mod 60;
      if s<>0 then
        s:=s+60;
    end;
end;

procedure incmsec(var d,mo,y,h,m,s,mm: longint; val: longint);
begin
  inc(mm,val);
  if mm>=1000 then
    begin
      incsec(d,mo,y,h,m,s,mm div 1000);
      mm:=mm mod 1000;
    end
  else if mm<0 then
    begin
      incsec(d,mo,y,h,m,s,(mm-999) div 1000);
      mm:=mm mod 1000;
      if mm<>0 then
        mm:=mm+1000;
    end;
end;

Procedure DoIt(d,mo,y,h,m,s,mm : Word) ;

Var
  T : TDateTime;
  T2 : TDateTime;
  T3 : TDateTime;
  error: boolean;
  d1, mo1, y1, h1, m1, s1, mm1: longint;

  procedure initdatetime;
    begin
      d1:=d;
      mo1:=mo;
      y1:=y;
      h1:=h;
      m1:=m;
      s1:=s;
      mm1:=mm;
    end;

begin
  error:=false;
  T:=EncodeDateTime(y,mo,d,h,m,s,mm);

  { IncMilliSecond }
  initdatetime;
  incmsec(d1,mo1,y1,h1,s1,m1,mm1,1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncMilliSecond(T);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 1 msec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  incmsec(d1,mo1,y1,h1,s1,m1,mm1,2*MSecsPerDay);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);

  T3:=IncMilliSecond(T,2*MSecsPerDay);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 1 msec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  initdatetime;
  incmsec(d1,mo1,y1,h1,m1,s1,mm1,-1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncMilliSecond(T,-1);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 1 msec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  incmsec(d1,mo1,y1,h1,m1,s1,mm1,-2*MSecsPerDay);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);

  T3:=IncMilliSecond(T,-2*MSecsPerDay);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 2 days -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  { IncSecond }
  initdatetime;
  incsec(d1,m1,y1,h1,m1,s1,1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncSecond(T);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 1 sec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  incsec(d1,mo1,y1,h1,m1,s1,140);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncSecond(T,140);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 140 sec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  initdatetime;
  incsec(d1,mo1,y1,h1,m1,s1,-1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncSecond(T,-1);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 1 sec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  incsec(d1,mo1,y1,h1,m1,s1,-140);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncSecond(T,-140);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 140 sec -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  { IncMinute }
  initdatetime;
  incmin(d1,mo1,y1,h1,m1,1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncMinute(T);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 1 min -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  incmin(d1,mo1,y1,h1,m1,200);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncMinute(T,200);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 200 min -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  initdatetime;
  incmin(d1,mo1,y1,h1,m1,-1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncMinute(T,-1);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 1 min -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  incmin(d1,mo1,y1,h1,m1,-2);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncMinute(T3,-1);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 2 min -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  { IncHour }
  initdatetime;
  inchour(d1,mo1,y1,h1,1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncHour(T);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 1 hour -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  inchour(d1,mo1,y1,h1,2);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncHour(T3);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' + 2 hours -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  initdatetime;
  inchour(d1,mo1,y1,h1,-1);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncHour(T,-1);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 1 hour -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;
  initdatetime;
  inchour(d1,mo1,y1,h1,-2);
  T2:=EncodeDateTime(y1,mo1,d1,h1,m1,s1,mm1);
  T3:=IncHour(T3,-1);
  if not samevalue(t2,t3,dteps) then
    begin
      WriteLn('error: ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T),' - 2 hours -> ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T3),' instead of ',FormatDateTime('yyyy/mm/dd - hh:nn:ss:zzz',T2));
      error:=true;
    end;

  if error then
    halt(1);
end;

begin
  { warning: the helpers used by Doit only support month 12 roll-overs (to either
    month 1 or month 11) }
  Doit(28,12,1899,23,59,59,999);
  Doit(28,12,1899,23,59,59,0);
  Doit(28,12,1899,23,59,0,0);
  Doit(28,12,1899,23,0,0,0);
  Doit(29,12,1899,23,59,59,999);
  Doit(29,12,1899,23,59,59,0);
  Doit(29,12,1899,23,59,0,0);
  Doit(29,12,1899,23,0,0,0);
  Doit(29,12,1899,0,0,0,0);
  Doit(30,12,1899,0,0,0,0);
  Doit(30,12,1899,0,0,0,1);
  Doit(30,12,1899,0,0,1,0);
  Doit(30,12,1899,0,1,0,0);
  Doit(30,12,1899,1,0,0,0);
  Doit(31,12,1899,0,0,0,1);
  Doit(31,12,1899,0,0,1,0);
  Doit(31,12,1899,0,1,0,0);
  Doit(31,12,1899,1,0,0,0);
end.
