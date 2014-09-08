program tw10670;

{$IFDEF FPC}
  {$MODE Delphi}
  {$MACRO ON}
{$ENDIF}

const version = 2.01;

begin
  {$IF version >= 2.03}
    {$MESSAGE Error 'Float compile-time expressions failed!'}
  {$ELSE}
    {$MESSAGE Note 'Float compile-time expressions work!'}
  {$IFEND}

  {$DEFINE FLT := 1e+2}
  {$IF FLT < 99}
    {$MESSAGE Error 'Float expressions with macro failed!'}
  {$ELSE}
    {$MESSAGE Note 'Float expressions with macro work!'}
  {$IFEND}
end.
