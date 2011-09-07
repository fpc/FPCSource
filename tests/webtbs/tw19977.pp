{$mode objfpc}{$H+}
{$I+}

uses SysUtils;

const
  NotExistingDir = {$ifdef UNIX} '/not_existing_directory_kambi_test' {$endif}
                   {$ifdef MSWINDOWS} 'c:/not_existing_directory_kambi_test' {$endif}
                   {$ifdef GO32V2} 'c:/not_existing_directory_kambi_test' {$endif};
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
