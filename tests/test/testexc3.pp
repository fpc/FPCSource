{$mode objfpc}
uses
   dotest,sysutils;

var
   i : longint;

procedure test1;

  begin
     try
        i:=0;
        exit;
     finally
        inc(i);
     end;
     i:=-2;
  end;

procedure test2;

  begin
     try
        i:=0;
        raise exception.create('');
     finally
        inc(i);
     end;
     i:=-2;
  end;

procedure test3;

  begin
     try
       try
          i:=0;
          raise exception.create('');
       finally
          inc(i);
       end;
     finally
       inc(i);
     end;
     i:=-2;
  end;

procedure test4;

  begin
     try
       try
          i:=0;
          exit;
       finally
          inc(i);
       end;
     finally
       inc(i);
     end;
     i:=-2;
  end;

procedure test5;

  var
     j : longint;

  begin
     for j:=1 to 10 do
       begin
          try
             i:=0;
             break;
          finally
            inc(i);
          end;
          dec(i);
       end;
  end;

procedure test6;

  var
     j : longint;

  begin
     i:=0;
     for j:=1 to 10 do
       begin
          try
             continue;
          finally
             inc(i);
          end;
          dec(i);
     end;
  end;

procedure test7;

  var
     j : longint;

  begin
     for j:=1 to 10 do
       begin
          try
            try
               i:=0;
               break;
             finally
               inc(i);
             end;
             dec(i);
          finally
            inc(i);
          end;
       end;
  end;

procedure test8;

  var
     j : longint;

  begin
     i:=0;
     for j:=1 to 10 do
       begin
          try
             try
                continue;
             finally
                inc(i);
             end;
          finally
             inc(i);
          end;
          dec(i);
       end;
  end;


{ some combined test ... }

procedure test9;

  var
     j : longint;

  begin
     try
        i:=0;
     finally
        for j:=1 to 10 do
           begin
              try
                 if j<2 then
                   continue
                 else
                   break;
              finally
                 inc(i);
              end;
              dec(i);
           end;
     end;
  end;

procedure test10;

  var
     j : longint;

  begin
     try
        i:=0;
        j:=1;
     finally
       while j<=10 do
         begin
            try
               if j<2 then
                 continue
               else
                 break;
            finally
               inc(i);
               inc(j);
            end;
            dec(i);
         end;
     end;
  end;

{ the do_raise function is a little bit more complicated }
{ so we also check if memory is lost                     }
function do_raise : ansistring;

  var
     a1,a2 : ansistring;
     j : longint;

  begin
     for j:=1 to 3 do
       begin
          a1:=copy('Hello world',1,5);
          do_raise:=copy(a2,1,1);
       end;
     raise exception.create('A string to test memory allocation');
     do_error(99998);
  end;


{ now test real exceptions }
procedure test100;

  begin
     try
        i:=0;
        do_raise;
     except
        inc(i);
     end;
  end;

procedure test101;

  begin
     try
        try
           i:=0;
           do_raise;
        except
           inc(i);
           do_raise;
        end;
     except
        inc(i);
     end;
  end;

procedure test102;

  begin
     try
        try
           i:=0;
           do_raise;
        except
           inc(i);
           raise;
        end;
     except
        inc(i);
     end;
  end;

var
   startmemavail : longint;

begin
   startmemavail:=memavail;
   i:=-1;
   try
     test1;
   finally
     inc(i);
   end;
   if i<>2 then
     do_error(1001);

   i:=-1;
   try
     test2;
   except
     inc(i);
   end;
   if i<>2 then
     do_error(1002);

   i:=-1;
   try
      test3;
   except
      inc(i);
   end;
   if i<>3 then
     do_error(1003);

   i:=-1;
   test4;
   if i<>2 then
     do_error(1004);

   i:=-1;
   test5;
   if i<>1 then
     do_error(1005);

   i:=-1;
   test6;
   if i<>10 then
     do_error(1006);

   i:=-1;
   test7;
   if i<>2 then
     do_error(1007);

   i:=-1;
   test8;
   if i<>20 then
     do_error(1008);

   i:=-1;
   test9;
   if i<>2 then
     do_error(1009);

   i:=-1;
   test10;
   if i<>2 then
     do_error(1010);

   i:=-1;
   test100;
   if i<>1 then
     do_error(1100);
   
   i:=-1;
   test101;
   if i<>2 then
     do_error(1101);
   
   i:=-1;
   test102;
   if i<>2 then
     do_error(1102);
   
   if memavail<>startmemavail then
     do_error(99999);
   writeln('Test successfully passed');
   halt(0);
end.
