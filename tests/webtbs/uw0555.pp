{ %CPU=i386 }

{ FPC behaves interestingly once encountered virtual method
 declared as
  procedure TWhateverObject.Method1; assembler; asm ... end;
 if you ever try to overload such method _in another unit_,
 than compile _second unit_, and than try to compile it again (???)-
 you will end up with the message "Function header does not match
 forward declaration of TNewObject.Method1" although in reality
 it does match perfectly.
 sometimes i encounter the same message even on non-assembler methods,
 but i have not been able to reproduce them cleanly nor find the
 reason for such behavior.}

 unit uw0555;

 interface

 type

   TBugObj = Object
     constructor Init;
     procedure Method1;
     procedure Method2;virtual;
     procedure Method3;
     procedure Method4;virtual;
     destructor Done;virtual;
    end;

 implementation

  Constructor TBugObj.Init;
  begin
  end;

{$ASMMODE ATT}
  procedure TBugObj.Method1;assembler;
  asm
     movl $1,%eax
  end;

  procedure TBugObj.Method2;assembler;
  asm
     movl $1,%eax
  end;

  procedure TBugObj.Method3;
  begin
  end;

  procedure TBugObj.Method4;
  begin
  end;

  Destructor TBugObj.Done;
  begin
  end;

end.
