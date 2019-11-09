unit utextractquote;

interface
// test  AnsiExtractQuotedStr

{$mode objfpc}
{$h+}

Uses SysUtils;

implementation

uses punit, utrtl;

Function TestAnsiExtractQuotedStr : String;

  Function dotest(str,val2,val3:string) : Boolean;

  var
    p : pchar;
    s2 : string;

  begin
    p:=pchar(Str);
    s2:=AnsiExtractQuotedStr(p,'"');
    Result:=AssertEquals('Testing >'+Str+'< return value',val2,S2);
    if Not Result then exit;
    Result:=AssertEquals('Testing >'+Str+'< left value',val3,ansistring(p));
  end;

begin
  Result:='';
  if not dotest('"test1""test2"','test1"test2','') then exit;
  if not dotest('"test1" "test2"','test1',' "test2"') then exit;
  if not dotest('"test1 test2"','test1 test2','') then exit;
  if not dotest('"test1 test2','test1 test2','') then exit;
  if not dotest('','','') then exit;
  if not dotest('"','','') then exit;
  if not dotest('""','','') then exit;
  if not dotest('"x"','x','') then exit;
end;

begin  
  SysUtilsTest('TestAnsiExtractQuotedStr',@TestAnsiExtractQuotedStr);
end.
