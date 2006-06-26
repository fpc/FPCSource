{ %maxversion=2.0.99 }
{ %opt=-gh }
{ Source provided for Free Pascal Bug Report 4098 }
{ Submitted by "Vincent Snijders" on  2005-06-19 }
{ e-mail: vincent.snijders@gmail.com }
Program project1;

{ Program to demonstrate the SetResourceStringValue function. }
{$Mode Delphi}

uses
  sysutils;

ResourceString

  First  = 'First string';
  Second = 'Second String';

Var I,J : Longint;
    S : AnsiString;

begin
  { Print current values of all resourcestrings }
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      begin
      Writeln ('Translate => ',GetResourceStringDefaultValue(I,J));
      Write   ('->');
      s:=inttostr(j)+'. Zeichenkette';
      SetResourceStringValue(I,J,S);
      end;
  Writeln ('Translated strings : ');
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      begin
      Writeln (GetResourceStringDefaultValue(I,J));
      Writeln ('Translates to : ');
      Writeln (GetResourceStringCurrentValue(I,J));
      end;
end.
