{$mode objfpc}
uses
  erroru,sysutils;

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

{ tests continue in try...except...end; statements }
procedure test103;

  var
     j,k : longint;

  begin
     i:=0;
     for j:=1 to 10 do
       try
          for k:=1 to 10 do
            try
               inc(i);
               if (i mod 10)>5 then
                 do_raise
               else
                 continue;
            except
               continue
            end;
          if i>50 then
            do_raise
          else
            continue;
       except
         continue;
       end;
  end;

procedure test104;

  begin
     try
        i:=1;
        exit;
        // we should never get there
        do_raise;
     except
        i:=-1;
     end;
     i:=-2;
  end;

procedure test105;

  begin
     try
        i:=0;
        do_raise;
        // we should never get there
        i:=-1;
     except
        inc(i);
        exit;
     end;
  end;

procedure test106;

  begin
     try
        try
           i:=1;
           exit;
           // we should never get there
           do_raise;
        except
           i:=-1;
        end;
        i:=-2;
     except
     end;
  end;

procedure test107;

  begin
     try
        do_raise;
     except
        try
           i:=0;
           do_raise;
           // we should never get there
           i:=-1;
        except
           inc(i);
           exit;
        end;
     end;
  end;

{ tests break in try...except...end; statements }
procedure test108;

  begin
     i:=0;
     while true do
       try
          while true do
            try
               inc(i);
               break;
            except
            end;
          inc(i);
          break;
       except
       end;
  end;

procedure test109;

  begin
     i:=0;
     while true do
       try
          repeat
            try
               do_raise;
               i:=-1;
            except
               inc(i);
               break;
            end;
          until false;
          do_raise;
          i:=-1;
       except
          inc(i);
          break;
       end;
  end;

{ test the on statement }
procedure test110;

  begin
     try
        i:=0;
        do_raise;
     except
        on e : exception do
          inc(i);
     end;
  end;

procedure test111;

  begin
     try
        try
           i:=0;
           do_raise;
        except
           on e : exception do
             begin
                inc(i);
                do_raise;
             end;
        end;
     except
        on e : exception do
          inc(i);
     end;
  end;

procedure test112;

  begin
     try
        try
           i:=0;
           do_raise;
        except
           on e : exception do
             begin
                inc(i);
                raise;
             end;
        end;
     except
        on e : exception do
          inc(i);
     end;
  end;

procedure test113;

  var
     j,k : longint;

  begin
     i:=0;
     for j:=1 to 10 do
       try
          for k:=1 to 10 do
            try
               inc(i);
               if (i mod 10)>5 then
                 do_raise
               else
                 continue;
            except
               on e : exception do
                 continue
            end;
          if i>50 then
            do_raise
          else
            continue;
       except
         on e : exception do
           continue;
       end;
  end;

procedure test114;

  begin
     try
        i:=1;
        exit;
        // we should never get there
        do_raise;
     except
        on e : exception do
          i:=-1;
     end;
     i:=-2;
  end;

procedure test115;

  begin
     try
        i:=0;
        do_raise;
        // we should never get there
        i:=-1;
     except
        on e : exception do
          begin
             inc(i);
             exit;
          end;
     end;
  end;

procedure test116;

  begin
     try
        try
           i:=1;
           exit;
           // we should never get there
           do_raise;
        except
           on e : exception do
             i:=-1;
        end;
        i:=-2;
     except
        on e : exception do
          ;
     end;
  end;

procedure test117;

  begin
     try
        do_raise;
     except
        try
           i:=0;
           do_raise;
           // we should never get there
           i:=-1;
        except
           on e : exception do
             begin
                inc(i);
                exit;
             end;
        end;
     end;
  end;

{ tests break in try...except...end; statements }
procedure test118;

  begin
     i:=0;
     while true do
       try
          while true do
            try
               inc(i);
               break;
            except
              on e : exception do
                ;
            end;
          inc(i);
          break;
       except
          on e : exception do
            ;
       end;
  end;

procedure test119;

  begin
     i:=0;
     while true do
       try
          repeat
            try
               do_raise;
               i:=-1;
            except
               on e : exception do
                 begin
                    inc(i);
                    break;
                 end;
            end;
          until false;
          do_raise;
          i:=-1;
       except
          on e : exception do
            begin
               inc(i);
               break;
            end;
       end;
  end;

var
  mem : sizeint;
begin
   writeln('Testing exception handling');
   
   mem:=0;
   DoMem(mem);
   
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

   i:=-1;
   test103;
   if i<>100 then
     do_error(1103);


   i:=-1;
   test104;
   if i<>1 then
     do_error(1104);

   i:=-1;
   test105;
   if i<>1 then
     do_error(1105);

   i:=-1;
   test106;
   if i<>1 then
     do_error(1106);

   i:=-1;
   test107;
   if i<>1 then
     do_error(1107);

   i:=-1;
   test108;
   if i<>2 then
     do_error(1108);

   i:=-1;
   test109;
   if i<>2 then
     do_error(1109);

   i:=-1;
   test110;
   if i<>1 then
     do_error(1110);

   i:=-1;
   test111;
   if i<>2 then
     do_error(1111);

   i:=-1;
   test112;
   if i<>2 then
     do_error(1112);

   i:=-1;
   test113;
   if i<>100 then
     do_error(1113);


   i:=-1;
   test114;
   if i<>1 then
     do_error(1114);

   i:=-1;
   test115;
   if i<>1 then
     do_error(1115);

   i:=-1;
   test116;
   if i<>1 then
     do_error(1116);

   i:=-1;
   test117;
   if i<>1 then
     do_error(1117);

   i:=-1;
   test118;
   if i<>2 then
     do_error(1118);

   i:=-1;
   test119;
   if i<>2 then
     do_error(1119);


   if DoMem(mem)<>0 then
     begin
       writeln('exception generates memory holes');
       do_error(99999);
     end;
   writeln('Test successfully passed');
   halt(0);
end.
