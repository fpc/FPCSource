Program Example7;

{ Program to demonstrate the ChDir function. }

begin
  {$I-}
  ChDir (ParamStr(1));
  if IOresult<>0 then
    Writeln ('Cannot change to directory : ',paramstr (1));
end.

