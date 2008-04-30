// test  AnsiExtractQuotedStr

{$mode delphi}

Uses Strings,SysUtils;

var testok : boolean;

procedure dotest(str,val2,val3:string);

var p: pchar;
    s2:string;
    b: boolean;
begin
  Write('testing: *',str,'*: ');
  p:=pchar(Str);
  b:=true;
  s2:=AnsiExtractQuotedStr( p, '"' );
  if s2<>val2 then
     begin
       write(' return value wrong:*',s2,'*');
       b:=false;
     end;
  if ansistring(p)<>val3 then
     begin
       write(' left in str wrong:*',ansistring(p),'*');
       b:=false;
     end;
  if b then writeln('ok') else begin testok:=false; writeln; end;
end;

var str : string;
    p   : pchar;

begin
  testok:=true;
  dotest('"test1""test2"','test1"test2','');
  dotest('"test1" "test2"','test1',' "test2"');
  dotest('"test1 test2"','test1 test2','');
  dotest('"test1 test2','test1 test2','');
  dotest('','','');
  dotest('"','','');
  dotest('""','','');
  dotest('"x"','x','');   // one char only case.
  if testok then halt(0) else halt(1);
end.
