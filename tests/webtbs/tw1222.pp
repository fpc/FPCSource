CONST
    c1=$80000000;
    c2=$80000001;
    c3=$80000002;
    c4=$80000003;
    cm=$80000007;

VAR v:dword;

BEGIN
  v:=c2;
  CASE (v AND cm) OF
    c1,c2:writeln('case c1,c2');
    c3,c4:
    begin
      writeln('case c3,c4');
      halt(1);
    end;
  ELSE
    begin
      writeln('case failed, but it should not');
      halt(1);
    end;
  END;
END.
