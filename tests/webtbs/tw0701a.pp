var
   s : string;

  procedure UseString(const as : string);
  begin
    s:=as;
  end;

  procedure MyExit;
  begin
    Writeln('Last call to UseString was with as = ',s);
  end;

begin
  exitproc:=@MyExit;
  UseString('Dummy test');
end.
