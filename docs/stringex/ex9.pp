Program Example9;

Uses strings;

{ Program to demonstrate the StrIComp function. }

Const P1 : PCHAR = 'This is a pchar string.';
      P2 : PCHAR = 'This is a PCHAR string.';
      P3 : PCHAR = 'tHiS iS aLsO a PCHAR string';
      p4 : pchar = 'AAbcd';
      p5 : pchar = 'AEbcd';

Var L : Longint;

begin
  If StrIComp (P1,P2)<>0 then writeln ('Something wrong here !');
  Write ('P2 and P3 match in their first ');
  L:=1;
  While StrLIComp (P2,P3,L)=0 do inc(L);
  Dec(L);
  Writeln (L,' characters, case insensitive.');
  if strIcomp(p4,p5)=0 then writeln ('This can''t happen!');
end.
