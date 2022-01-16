{ Test against Mantis #30166, an issue in the i386 fpc_div_int64 helper. }
Uses sysutils;

Var cc, dd: int64;
    a, b: int64;

Begin
 dd:=int64($0000000A6BB38805);
 cc:=int64($0000000000142E04);
 cc:=cc-2822400; { $FF11813BCCC3B114 }
 dd:=dd*cc;

{ dd:=int64($FF11813BCCC3B114); }

 a:=dd div int64($00000000002B1100);
 b:=int64($FF11813BCCC3B114) div int64($00000000002B1100);

 WriteLn(IntToHex(cc,16));
 WriteLn(IntToHex(dd,16));
 WriteLn('FF11813BCCC3B114:00000000002B1100='#9, IntToHex(a,16));
 WriteLn('FF11813BCCC3B114:00000000002B1100='#9, IntToHex(b,16));
 
 if a<>b then
  begin
    writeln('Failed!');
    halt(1);
  end
 else
  begin
    writeln('Passed!');
  end;
End.
