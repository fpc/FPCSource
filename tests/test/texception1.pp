{ %RESULT=217 }
program testexceptions;

{$mode objfpc}

Type
  TAObject = class(TObject)
    a : longint;
    end;
  TBObject = Class(TObject)
    b : longint;
    end;

Procedure raiseanexception;

Var A : TAObject;

begin
  Writeln ('Creating exception object');
  A:=TAObject.Create;
  Writeln ('Raising with this object');
  raise A;
  Writeln ('This can''t happen');
end;

Var MaxLevel : longint;

Procedure DoTryFinally (Level : Longint; DoRaise : Boolean);


Var Raised,Reraised : Boolean;
    I : Longint;

begin
  Try
    writeln ('Try(',level,') : Checking for exception');
    If Level=MaxLevel then
      begin
      if DoRaise then
        begin
        Writeln ('Try(',level,'): Level ',maxlevel,' reached, raising exception.');
        Raiseanexception
        end
      else
        Writeln ('Try(',Level,'):  Not raising exception')
      end
    else
      begin
      Writeln ('Try(',level,') : jumping to next level');
      DoTryFinally(Level+1,DoRaise);
      end;
  finally
    Writeln ('Finally (',level,'): Starting code.');
  end;
  writeln ('Out of try/finally at level (',level,')');
end;

Procedure DoTryExcept (Level : Longint; DoRaise : Boolean);

Var Raised : Boolean;
    I : Longint;
    Caught : TObject;

begin
  Try
    writeln ('Try(',level,') : Checking for exception');
    If Level=MaxLevel then
      if DoRaise then
        begin
        Writeln ('Try(',level,'): Level ',maxlevel,', raising exception.');
        Raiseanexception
        end
      else
        Writeln ('Try(',Level,'): level ',maxlevel,'. Not raising exception')
    else
      begin
      Writeln ('Try(',level,') : jumping to next level');
      DoTryExcept(Level+1,DoRaise);
      end;
  except
    On TAObject do Writeln ('Exception was caught by TAObject');
    On TBobject do Writeln ('Exception was caught by TBObject');
    On E : TObject do Writeln ('Caught object ',E.ClassName);
//    writeln ('Except (',level,') : Exception caught by default handler');
  end;
  writeln ('Out of try/except at level (',level,')');
end;

Procedure DoMix (Level : Longint; DoRaise : Boolean);

Var Raised : Boolean;
    I : Longint;
    Caught : TObject;

begin
  Try
    Try
    writeln ('Try(',level,') : Checking for exception');
    If Level=MaxLevel then
      if DoRaise then
        begin
        Writeln ('Try(',level,'): Level ',maxlevel,', raising exception.');
        Raiseanexception
        end
      else
        Writeln ('Try(',Level,'): level ',maxlevel,'. Not raising exception')
    else
      begin
      Writeln ('Try(',level,') : jumping to next level');
      DoMix(Level+1,DoRaise);
      end;
    finally
      Writeln ('Mix:Finally (',level,'): Starting code.');
    end;
  Writeln ('Level (',level,') : Out of try/finally');
  except
   On TAObject do Writeln ('Exception was caught by TAObject');
   On TBobject do Writeln ('Exception was caught by TBObject');
   On TObject do writeln ('Except (',level,') : Exception caught by TObject');
//  The following don't work...
     On E : TObject do Writeln ('Caught object ',E.ClassName);
   else
    writeln ('Except (',level,') : Exception caught by default handler');
  end;
  writeln ('Out of try/except at level (',level,')');
end;

function _dotryfinally : boolean;

var
   problem : boolean;

begin
   result:=false;
   try
     try
     finally
       writeln('Raising an exception in finally statement');
       Raiseanexception
     end;
   except
   end;
   try
      exit;
   finally
      result:=true;
   end;
   writeln('Problem with finally and exit !!!!');
   halt(1);
end;

procedure dotryfinally;

  begin
     if not(_dotryfinally) then
       begin
          writeln('Problem with finally and exit !!!!');
          halt(1);
       end;
  end;

Procedure Start(Const Msg : string);

begin
  Writeln (Msg);
  Writeln;
end;

Procedure Finish;

begin
  Writeln;
  Writeln ('Finished.');
  writeln;
  { Press enter to continue.');
  Readln; tests/test/test... must be non interactive !! PM }
end;


begin
  Maxlevel:=3;
  Start ('Testing Try/Finally without raise');
  DoTryFinally (1,False);
  Finish;
  Start ('Testing Try/except without raise');
  DoTryExcept (1,FAlse);
  Finish;
  Start ('Testing Mix without raise');
  DoMix (1,False);
  Finish;
  Start ('Testing Try/except with raise');
  DoTryExcept (1,true);
  Finish;
  Start ('Testing Mix with raise');
  DoMix (1,true);
  Finish;
  Start ('Testing Try/Finally with Exit');
  dotryfinally;
  Finish;
  Writeln ('Testing Try/Finally with raise');
  Start ('This one should end with an error message !!.');
  DoTryFinally (1,True);
end.
