Program Example11;
uses Dos;

{ Program to demonstrate the FExpand function. }

begin
  WriteLn('Expanded Name of this program is ',FExpand(ParamStr(0)));
end.
