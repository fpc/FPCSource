program crittest;
// originally a test to test .tryenter.
// A thread holds a lock for 5sec, while the main thread tries to lock
// it.  

{$mode Delphi}

Uses {$ifdef unix}cthreads,{$endif} syncobjs,sysutils,classes;

type TTestthread = class(tthread)
	    	     procedure execute; override;
                    end;

var crit : TCriticalSection;

procedure TTestThread.Execute;

begin
 crit.acquire;
 sleep(5000);
 crit.release;
end;


var thr : TTestthread;  
    I : integer;

begin
 crit:=TCriticalsection.create;
 thr :=TTestthread.Create(false);

 sleep(500);  // give thread time to start.

 writeln('tryenter');
 
 i:=0;
 while not(crit.tryenter) do
  begin
    writeln('tryenter attempt ',i);
    inc(i);
    sleep(100);
  end;
 writeln('lock acquired in mainthread!');
 writeln('no payload, so releasing');
 crit.release;
 thr.waitfor;
end.