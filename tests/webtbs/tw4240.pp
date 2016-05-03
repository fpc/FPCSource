{ %cpu=i386 }
{ %OPT=-Cg- }
{ Source provided for Free Pascal Bug Report 4240 }
{ Submitted by "Den Jean" on  2005-07-30 }
{ e-mail: Den.Jean@telenet.be }
program TestPointArray;
{$H+}
{$asmmode intel}
{$mode delphi}

uses
  Classes, SysUtils, Types;

type
  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;

var
  Points     : TPointArray;
  p          : PPointArray;
  i          : integer;


function GetPointsLength1 (PA: pointer): Integer; cdecl;
begin
  asm
    mov EAX,[EBP+$08]
    mov i,eax
  end;
writeln('Within GetPointsLength using Pointer argument:');

writeln(Format('--- Address on Stack:$%p',[Pointer(i)]));
if i <> integer (p)
then writeln ('  * Wrong Address passed on stack');

writeln(Format('--- Address using Parameter:$%p',[PA]));
if integer(PA) <> integer (p)
then writeln ('  * Parameter addresss different from given variable');

Result:=Length(TPointArray(PA));
writeln('--- Array Length:',Result);
end;

{$IFDEF FPC}
{$asmmode intel}
{$ENDIF}

function GetPointsLength2 (const PA: TPointArray): Integer; cdecl;
begin
  asm
    mov EAX,[EBP+$08]
    mov i,eax
  end;
writeln('Within GetPointsLength using const TPointArray argument:');

writeln(Format('--- Address on Stack:$%p',[Pointer(i)]));
if i <> integer (p)
then writeln ('  * Wrong Address passed on stack');

writeln(Format('--- Address using Parameter:$%p',[pointer(PA)]));
if integer(PA) <> integer (p)
then writeln ('  * Parameter addresss different from given variable');

Result:=Length(PA);
writeln('--- Array Length:',Result);
end;


begin
SetLength (Points, 3);
Points [0] := Point (1,2);
Points [1] := Point (3,4);
Points [2] := Point (5,6);
p:=@Points[0];
writeln(Format('Address of TPointArray:$%p',[p]));
getPointsLength1(Points);
getPointsLength2(Points);
end.
