{ %OPT=-Sg }

{ Old file: tbs0178.pp }
{ problems with undefined labels and fail outside constructor OK 0.99.9 (PM) }

PROGRAM NoLabel; { this program compiles fine with TP but not with FP }

 type
    ptestobj = ^ttestobj;
    ttestobj = object
            constructor init;
            procedure test_self;
            end;

 const
    allowed : boolean = false;

    constructor ttestobj.init;
      begin
        if not allowed then
          fail;
      end;
    procedure ttestobj.test_self;
      function myself : ptestobj;
        begin
           myself:=@self;
        end;

      begin
         if myself<>@self then
           begin
              Writeln('problem with self');
              Halt(1);
           end;
      end;


LABEL
  N1,
  N2,
  FAIL, { this is a reserved word in constructors only! - FP fails here
}
  More; { label not defined - FP fails, but a warning is enough for that
}
           { since label referenced nowhere }
 var ptest : ptestobj;
     self : longint;
BEGIN
  new(ptest,init);
  if ptest<>nil then
    begin
       Writeln('Fail does not work !!');
       Halt(1);
    end;
  allowed:=true;
  new(ptest,init);
  if ptest=nil then
    begin
       Writeln('Constructor does not work !!');
       Halt(1);
    end
  else
    ptest^.test_self;
  N1: Write;
  N2: Write;
  FAIL: Write;
  self:=1;
END.
