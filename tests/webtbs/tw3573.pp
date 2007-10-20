{ Source provided for Free Pascal Bug Report 3573 }
{ Submitted by "Simon Kissel" on  2005-01-18 }
{ e-mail: scamp@untergrund.net }

{$ifdef fpc}{$mode delphi}{$endif}

{$ifdef darwin}
{$PIC+}
{$endif darwin}

resourcestring
  wurst = 'jo' deprecated;

var
  VersionNumber: Real library;

type
   AppError = class(TObject)
   end platform;

procedure SomeOldRoutine; stdcall; deprecated;
begin
end;

begin
end.
