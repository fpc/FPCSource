Program MathX;

  Const
{$ifdef FPC_HAS_TYPE_EXTENDED}
    MinExtendedStr=' 3.6451995318824746E-4951';
    MinExtended=3.64519953188247460E-4951;
    ToWidth=25;
{$else}
    MinExtendedStr=' 4.94065645841247E-324';
    MinExtended=4.94065645841247E-324;
    ToWidth=22;
{$endif}

  Var
    x:extended;
    s:shortstring;

  Begin
    val(MinExtendedStr,x);
    str(x:ToWidth,s);
    if (x=0.0) or
       (x<>minextended) or
       (s<>MinExtendedStr) then
      halt(1);
  End.

