{$mode objfpc}
{$H+}
uses cwstring, sysutils, classes, uregexpr;

Var
  Split : TStringList;
  S : String;
  R : TRegexpr;  
  E : TEncoding;

begin
  R:=nil;
  Split:=TStringList.Create;
  try
    E:=TEncoding.UTF8;
    Split.LoadFromFile(ParamStr(1),E);
    S:=Split.Text;
    r := TRegExpr.Create;
    r.spaceChars:=r.spaceChars+'|&@#"''(§^!{})-[]*%`=+/.;:,?';
    r.LineSeparators:=#10;
    r.Expression :='(\b[^\d\s]+\b)';
    if R.Exec(S) then
      repeat 
        Writeln('Found (pos: ',R.MatchPos[0],'): ',System.Copy (S, R.MatchPos [0], R.MatchLen[0]));
      until not R.ExecNext;
  finally 
    r.Free;
    split.free;
  end;
end.
