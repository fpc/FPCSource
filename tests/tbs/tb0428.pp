{ Testing smallset + normset }
{ with respect to normset + smallset }


type
  charset=set of char;

  var
     tr,tr2    : charset;


  procedure test(const k:charset);

    begin
       tr:=[#7..#10]+k;
       tr2:=k+[#7..#10];
     if (tr<>tr2) then
       begin
         Writeln('Bug in set handling');
         halt(1);
       end;
    end;

  begin
     Test([#20..#32]);
     if not(#32 in tr) or ([#33..#255]*tr <> []) or
        (tr<>[#7..#10,#20..#32]) or
        (tr<>tr2) then
       begin
         Writeln('Bug in set handling');
         halt(1);
       end;

  end.
