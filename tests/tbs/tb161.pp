{ Old file: tbs0185.pp }
{ missing range checking for Val and subrange types     OK 0.99.11 (JM/PFV) }

Program bug0185;

{shows some bugs with rangechecks}
{ readln from input changed to from a file to render it non-interactive }

var s: String;
    i: integer;
    code: word;
    e: 0..10;
    f : text;
    should_generate_error : boolean;
    oldexit : pointer;

  procedure myexit;
   begin
     exitproc:=oldexit;
     if should_generate_error and (exitcode=201) then
       begin
         Writeln('Program generates a range check error correctly');
         errorcode:=0;
         exitcode:=0;
         erroraddr:=nil;
         close(f);
         erase(f);
       end;
   end;

Begin
  oldexit:=exitproc;
  exitproc:=@myexit;
  should_generate_error:=false;
{$R-}
  s := '$fffff';
  val(s, i, code); {no range check error may occur here}
  Writeln('Integer($fffff) = ',i);

  assign(f,'tbs0185.tmp');
  rewrite(f);
  Writeln(f,'20');
  Writeln(f,'34');
  close(f);
  reset(f);
  Write('Enter the value 20 (should not give a rangecheck error): ');
  Readln(f,e);


{$R+}
  s := '$ffff';
  val(s, i, code); {no range check error may occur here}
  Writeln('integer($ffff) = ', i,'(should not give range check error)');

  Writeln('Enter value from 0-10 to test Val rangecheck, another for subrange rangecheck: ');
  should_generate_error:=true;
  Readln(f,e);

  Writeln('If you entered a value different from 0-10, subrange range checks don''t work!');
  s := '65535';
  val(s, i, code); {must give a range check error}
  Writeln('Val range check failed!');
  close(f);
  erase(f);
  Halt(1);
End.
