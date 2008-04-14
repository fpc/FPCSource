{$mode delphi}

{$ifdef darwin}
{$PIC+}
{$endif darwin}

{$ifdef CPUX86_64}
{$ifndef WINDOWS}
{$PIC+}
{$endif WINDOWS}
{$endif CPUX86_64}
unit uw8730b;

interface

function Lib2Func: pchar; CDecl;

implementation

const
{$ifdef windows}
  alibname='tw8730a.dll';
{$else}
  alibname='tw8730a';
  {$linklib tw8730a}
{$endif}

function Lib1Func: pchar; external alibname name '_Lib1Func';

function Lib2Func: pchar;
begin
  Writeln( Lib1Func );
  result := 'result of function Lib2Func';
end;

var
  t: text;
initialization
assign(t,'tw8730b.txt');
rewrite(t);
close(t);
WriteLn( 'Init of Unit 2' );

end.

//= END OF FILE ===============================================================
