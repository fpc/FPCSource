program basic;

{$IFDEF FPC}
 {$MODE OBJFPC}
 {$H+}
{$ENDIF}

{$IFDEF MSWINDOWS}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses libmagic;

const
  MAGIC_MGC = {$IFDEF MSWINFOWS}'magic.mgc'{$ELSE}nil{$ENDIF};

var
  filename: string;
  cookie: magic_t;
begin
  cookie := magic_open(MAGIC_ERROR_ or MAGIC_MIME);
  magic_load(cookie, MAGIC_MGC);
  filename := {$I %file%};
  WriteLn('The content-type of ''', filename, ''' is: ',
    magic_file(cookie, Pcchar(filename)));
  magic_close(cookie);
{$IFDEF MSWINDOWS}
  WriteLn('Press [ENTER] to exit ...');
  ReadLn;
{$ENDIF}
end.
