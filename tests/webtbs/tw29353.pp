program project1;

{$mode objfpc}{$H+}
{$codepage UTF8}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

Var
  AString : String;
begin
  AString := 'öö';
  Case AString of
    'öö' : WriteLn('match');
    else
      halt(1);
  end;
end.

