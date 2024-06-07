uses
  sysutils;

procedure Check(b: boolean;i: longint);
begin
  if not(b) then
    begin
      writeln('Error: ',i);
      halt(1);
    end;
end;

procedure testtrim_ansistr;
  var
    s: ansistring;
  begin
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(Trim(s)='asdfA SDF'#10#9#13'asdfASDF',1);
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(TrimRight(s)=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF',2);
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(TrimLeft(s)='asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ',3);
  end;


procedure testtrim_unicodestr;
  var
    s: unicodestring;
  begin
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(Trim(s)='asdfA SDF'#10#9#13'asdfASDF',1001);
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(TrimRight(s)=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF',1002);
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(TrimLeft(s)='asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ',1003);
  end;


procedure testtrim_widestr;
  var
    s: widestring;
  begin
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(Trim(s)='asdfA SDF'#10#9#13'asdfASDF',1001);
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(TrimRight(s)=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF',1002);
    s:=#10#9#13'  asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ';
    Check(TrimLeft(s)='asdfA SDF'#10#9#13'asdfASDF'#10#9#13'  ',1003);
  end;

begin
  testtrim_ansistr;
  testtrim_unicodestr;
  testtrim_widestr;
end.
