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
{$ifdef FreeBSD}
  OS:='FreeBSD';
{$endif}
{$ifdef NetBSD}
  OS:='NetBSD';
{$endif}
{$ifdef Solaris}
  OS:='Solaris';
{$endif}
{$ifdef QNX}
  OS:='QNX';
{$endif}

{$IFDEF DOS}
  OS:='Dos';
{$ENDIF}
  Version:=DosVersion;
  WriteLn('Current ',OS,' version is ',Lo(Version),'.',Hi(Version));
end.
