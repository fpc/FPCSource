{ %RESULT=210 }
{$R+}

program test_fail;

  uses
    erroru;
    
  type
     parrayobj = ^tarrayobj;
     tarrayobj = object
       ar : array [1..4] of real;
       constructor init(do_fail : boolean);
       procedure test;virtual;
       destructor done;virtual;
       end;
     pbigarrayobj = ^tbigarrayobj;
     tbigarrayobj = object(tarrayobj)
       ar2 : array [1..10000] of real;
       constructor good_init;
       constructor wrong_init;
       procedure test;virtual;
       end;
  var
    pa1, pa2 : parrayobj;
    ta1, ta2 : tarrayobj;
    availmem : longint;

  constructor tarrayobj.init(do_fail : boolean);
    begin
       ar[1]:=1;
       if do_fail then
         fail;
       ar[2]:=2;
    end;

  destructor tarrayobj.done;
    begin
    end;

  procedure  tarrayobj.test;
    begin
      Writeln('@self = ',longint(@self));
      Writeln('typeof = ',longint(typeof(self)));
      if ar[1]=1 then
        Writeln('Init called');
      if ar[2]=2 then
        Writeln('Init successful');
    end;

  constructor tbigarrayobj.good_init;
    begin
      inherited init(false);
      Writeln('End of tbigarrayobj.good_init');
    end;

  constructor tbigarrayobj.wrong_init;
    begin
      inherited init(true);
      Writeln('End of tbigarrayobj.wrong_init');
    end;

  procedure tbigarrayobj.test;
    begin
      Writeln('tbigarrayobj.test called');
      Inherited test;
    end;

  begin
     new(pa1,init(false));
     getheapstatus(hstatus);
     writeln('After successful new(pa1,init), memory used = ',hstatus.CurrHeapUsed);
     new(pa2,init(true));
     getheapstatus(hstatus);
     writeln('After unsuccessful new(pa2,init), memory used = ',hstatus.CurrHeapUsed);
     writeln('pa1 = ',longint(pa1),' pa2 = ',longint(pa2));
     writeln('Call to pa1^.test after successful init');
     pa1^.test;
     dispose(pa1,done);
     getheapstatus(hstatus);
     writeln('After release of pa1, memory used = ',hstatus.CurrHeapUsed);
     pa1:=new(pbigarrayobj,good_init);
     getheapstatus(hstatus);
     writeln('After successful pa1:=new(pbigarrayobj,good_init), memory used = ',hstatus.CurrHeapUsed);
     pa2:=new(pbigarrayobj,wrong_init);
     getheapstatus(hstatus);
     writeln('After unsuccessful pa2:=new(pbigarrayobj,wrong_init), memory used = ',hstatus.CurrHeapUsed);
     writeln('pa1 = ',longint(pa1),' pa2 = ',longint(pa2));
     writeln('Call to pa1^.test after successful init');
     pa1^.test;
     ta1.init(false);
     writeln('Call to ta1.test after successful init');
     ta1.test;
     ta2.init(true);
     writeln('typeof(ta2) = ',longint(typeof(ta2)),' after unsuccessful init');
     Writeln('Trying to call ta2.test (should generate a Run Time Error)');
     ta2.test;
  end.
