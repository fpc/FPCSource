program test_open_files;

const
   MaxOpenTest = 150;

var
   f : array [1..MaxOpenTest] of text;
   i,count : longint;
   s : string;
   storeexit : pointer;

procedure Errorexit;
begin
  exitproc:=storeexit;
  if errorcode=4 then
    begin
       if count<=15 then
         begin
           Writeln('The program could not open more than 15 files !');
           Writeln('Retry after addition of the following line to config.sys file');
           Writeln('FILES=60');
           Writeln('If it still does not work after this change');
           Writeln('you probably use a too old RTL version');
           Writeln('that does not support more than 15 files');
           Writeln('open at the same time');
         end
       else
         begin
           Writeln('The program was able to open ',count,' files simultaneously');
           Writeln('If you need to be able to have more opened files');
           Writeln('Try to increase the FILES=XX value in config.sys file');
           { This is not a RTL error anymore
             as we increased the size over the ordinary 15 limit }
           erroraddr:=nil;
           errorcode:=0;
           exitcode:=0;
         end;
       { close all left open files }
       for i:=count downto 1 do
         close(f[i]);
    end;
end;

begin
  StoreExit:=exitproc;
  ExitProc:=@ErrorExit;

  for i:=1 to MaxOpenTest do
    begin
      str(i,s);
      s:='file'+s+'.tmp';
      assign(f[i],s);
      rewrite(f[i]);
      count:=i;
      Writeln(f[i],'This is file ',i);
      Writeln(i,' files open');
      { no closing so they are finally all open }
    end;

  for i:=MaxOpenTest downto 1 do
    close(f[i]);
end.
