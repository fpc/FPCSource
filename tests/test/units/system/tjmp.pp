{ Tests the system unit routines for longjmp and setjmp }
{ By Carl Eric Codere - Copyright (c) 2002              }
program tjmp;

var
 global_envbuf: jmp_buf;


 type
   tmyobject = object
     envbuf : jmp_buf;
     j : integer;
     jmpresult : integer;
     constructor init;
     destructor done;
     procedure testlongjmp;virtual;
     procedure testsetjmp;virtual;
   end;

  pderivedobject = ^tderivedobject;
  tderivedobject = object(tmyobject)
     procedure testlongjmp; virtual;
     procedure testsetjmp; virtual;
  end;





constructor tmyobject.init;
 begin
 end;



destructor tmyobject.done;
 begin
 end;


procedure tmyobject.testlongjmp;
 begin
 end;

procedure tmyobject.testsetjmp;
 begin
 end;

procedure tderivedobject.testlongjmp;
 begin
    longjmp(envbuf, 255);
 end;

procedure tderivedobject.testsetjmp;
  var
    i : integer;
 begin
   j:=0;
   jmpresult:=setjmp(envbuf);
   case jmpresult of
   0 :
     begin
       for i:=0 to 255 do
        begin
          j:=j + 13;
          if j = 13 then
            self.testlongjmp;
        end;
     end;
   255 : WriteLn('Sucess!');
   else
     begin
       WriteLn('Failure!');
       halt(1);
     end
   end;
 end;

procedure testlongjmp;
 begin
    longjmp(global_envbuf, 255);
 end;


procedure testsetjmp;
  var
     i, j : integer;
     jmpresult : integer;
 begin
   j:=0;
   jmpresult:=setjmp(global_envbuf);
   case jmpresult of
   0 :
     begin
       for i:=0 to 255 do
        begin
          j:=j + 13;
          if j = 13 then
            testlongjmp;
        end;
     end;
   255 : WriteLn('Sucess!');
   else
     begin
       WriteLn('Failure!');
       halt(1);
     end
   end;
 end;

var
 pobj : pderivedobject;
begin
  Write('Testing setjmp/longjmp simple case...');
  testsetjmp;
  Write('Testing setjmp/longjmp in virtual method...');
  pobj:=new(pderivedobject, init);
  pobj^.testsetjmp;
  dispose(pobj, done);
end.
