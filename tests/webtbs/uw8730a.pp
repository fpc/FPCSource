{$mode delphi}

{$ifdef darwin}
{$PIC+}
{$endif darwin}

{$ifdef CPUX86_64}
{$ifndef WINDOWS}
{$PIC+}
{$endif WINDOWS}
{$endif CPUX86_64}
unit uw8730a;

interface

function _Lib1Func: pchar;

implementation

function _Lib1Func: pchar;
begin
  result := 'result of function Lib1Func';
end;

var
  t: text;

initialization
assign(t,'tw8730a.txt');
rewrite(t);
close(t);
WriteLn( 'Init of Unit1' );

end.

//= END OF FILE ===============================================================
