{ %opt=-g }
{ %interactive }

{ test to see whether parameters are properly visible in debugger }

PROGRAM Test;


{ when this routine is entered, the debugger should show:
"WRITELN2 (STR='Hello, World!', A=5, B=10) at tb0565.pp:17"
}
 PROCEDURE WriteLn2(str: STRING; a: longint; var b: longint);

 VAR   ptr: ^INTEGER;

 BEGIN
   WriteLn(str);
   ptr:= NIL;
//   IF ptr^ = 0 THEN
//     HALT;
   WriteLn(str)
 END;

{ when this routine is entered, the debugger should show:
"WRITELN3 (STR='Hello, World 3') at tb0565.pp:32"
}
 PROCEDURE WriteLn3(var str: STRING);

 VAR   ptr: ^INTEGER;

 BEGIN
   WriteLn(str);
   ptr:= NIL;
//   IF ptr^ = 0 THEN
//     HALT;
   WriteLn(str)
 END;

var
  s: string;
  b: longint;
BEGIN
 b:=10;
 WriteLn2('Hello, World!',5,b);
 s:='Hello, World 3';
 Writeln3(s);
END.

