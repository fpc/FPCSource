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

 unit tw0555;


 interface
 uses
   uw0555;

 type
   TBugObjChild = Object(TBugObj)
     procedure Method1;
     procedure Method2;virtual;
     procedure Method3;
     procedure Method4;virtual;
    end;

 implementation

  procedure TBugObjChild.Method1;
  begin
  end;

  procedure TBugObjChild.Method2;
  begin
  end;

{$ASMMODE ATT}
  procedure TBugObjChild.Method3;assembler;
  asm
     movl $1,%eax
  end;

  procedure TBugObjChild.Method4;assembler;
  asm
     movl $1,%eax
  end;


end.
