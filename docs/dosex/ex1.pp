Program Example1;
uses Dos;

{ Program to demonstrate the DosVersion function. }

var
  OS      : string[32];
  Version : word;
begin
{$IFDEF LINUX}
  OS:='Linux';
{$ENDIF}
{$IFDEF DOS}
  OS:='Dos';
{$ENDIF}  
  Version:=DosVersion;
  WriteLn('Current ',OS,' version is ',Lo(Version),'.',Hi(Version));
end.
