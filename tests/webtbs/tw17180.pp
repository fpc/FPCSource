program testbug;
{$APPTYPE CONSOLE}
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {$define caninline}
{$else}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$hints off}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$j+}
{$define UseRegister}

type TBla=(b0,b1,b2,b3,b4,b5);

     TBlas=set of TBla;
     
var b,ba:longbool;
    Blas:TBlas;
    
function BlaFunc(ABool:longbool):longbool;
begin
 result:=ABool;
end;

begin

 Blas:=[b1,b2,b3];
 b:=BlaFunc(b1 in Blas);
 ba:=true;
 writeln(b);
 writeln(longword(pointer(@b)^));
 if longword(pointer(@b)^)<>longword(pointer(@ba)^) then
   halt(1);
 if b<>ba then
   halt(1);
end.

