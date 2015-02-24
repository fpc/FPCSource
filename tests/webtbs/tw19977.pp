{$mode objfpc}{$H+}
{$I+}

{$IFDEF GO32V2}
 {$DEFINE DOSLIKEDIR}
{$ENDIF GO32V2}

{$IFDEF MSDOS}
 {$DEFINE DOSLIKEDIR}
{$ENDIF MSDOS}

{$IFDEF OS2}
 {$DEFINE DOSLIKEDIR}
{$ENDIF OS2}

{$IFDEF MSWINDOWS}
 {$DEFINE DOSLIKEDIR}
{$ENDIF MSWINDOWS}

{$IFNDEF UNIX}
 {$IFNDEF DOSLIKEDIR}
  {$ERROR Test misses path setting for this platform!}
 {$ENDIF DOSLIKEDIR}
{$ENDIF UNIX}

uses SysUtils;

const
  NotExistingDir = {$ifdef UNIX} '/not_existing_directory_kambi_test' {$endif}
                   {$ifdef DOSLIKEDIR} 'c:/not_existing_directory_kambi_test' {$endif}
                                                                                      ;
begin
  try
    ChDir(NotExistingDir);
    Assert(false, 'ChDir to ' + NotExistingDir + ' didn''t raise an exception');
  except
    on E: EInOutError do Writeln('Ok, ChDir raised exception');
  end;

  try
    Writeln('We are somewhere after ChDir');
  except
    on E: EInOutError do 
      begin
        Writeln('Ups, Writeln raised exception');
        halt(1);
      end;
  end;
end.
