{ %RESULT=210 }
{$R+}

{$mode objfpc}

program test_fail;

  type
     parrayobj = ^tarraycla;
     tarraycla = class
       ar : array [1..4] of real;
       constructor create(do_fail : boolean);
       procedure test;virtual;
       destructor done;virtual;
       end;
     pbigarrayobj = ^tbigarraycla;
     tbigarraycla = class(tarraycla)
       ar2 : array [1..10000] of real;
       constructor good_init;
       constructor wrong_init;
       procedure test;virtual;
       end;
  var
    ta1, ta2 : tarraycla;

  constructor tarraycla.create(do_fail : boolean);
    begin
       ar[1]:=1;
       if do_fail then
         fail;
       ar[2]:=2;
    end;

  destructor tarraycla.done;
    begin
    end;

  procedure  tarraycla.test;
    begin
      if ar[1]=1 then
        Writeln('Init called');
      if ar[2]=2 then
        Writeln('Init successful');
    end;

  constructor tbigarraycla.good_init;
    begin
      inherited create(false);
      Writeln('End of tbigarraycla.good_init');
    end;

  constructor tbigarraycla.wrong_init;
    begin
      inherited create(true);
      Writeln('End of tbigarraycla.wrong_init');
    end;

  procedure tbigarraycla.test;
    begin
      Writeln('tbigarraycla.test called');
      Inherited test;
    end;

  begin
     ta1:=tarraycla.create(false);
     writeln('Call to ta1.test after successful init');
     ta1.test;
     ta2:=tarraycla.create(true);
     writeln('ta2 = ',ptrint(ta2),' after unsuccessful init');
     Writeln('Trying to call ta2.test (should generate a Run Time Error)');
     ta2.test;
  end.
