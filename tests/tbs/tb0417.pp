{ Testing smallset + normset }
{ with respect to normset + smallset }


type
  charset=set of char;

  var
     err : byte;
     tr,tr2    : charset;


  procedure test(const k:charset);

    begin
       tr:=[#7..#10]+k;
       tr2:=k+[#7..#10];
    end;

  begin
     err:=0;
     Test([#20..#32]);
     if not(#32 in tr) then
      err:=1;
     if ([#33..#255]*tr <> []) then
      err:=2;
     if (tr<>[#7..#10,#20..#32]) then
      err:=3;
     if (tr<>tr2) then
      err:=4;
     if err<>0 then
       begin
         Writeln('Bug in set handling, see err:=',err);
         halt(1);
       end;
  end.
