{ %TARGET=win32,win64,wince }
program tw37355;
{$MODE OBJFPC}
{$macro on}
uses SysUtils, ComObj;
var w:variant; err:string;
begin
  writeln('FPC ver.: '+ IntToStr(FPC_FULLVERSION));
  write('Press Enter to start IE...');
  //readln;
  err := 'no error';
  try
  w := CreateOleObject('InternetExplorer.Application');
  w.Visible := true;
  try
  w.Navigate(url:='https://bugs.freepascal.org/view.php?id=37355');
  except
    on E:Exception do begin
      err := 'ERROR: ' + e.Message;
      halt(1);
    end;
  end;
  w.Quit;
  w := UnAssigned;
  except
    on E:Exception do begin
      err := 'ERROR: ' + e.Message;
      halt(2);
    end;
  end;
  writeln(err);
  write('Press Enter to exit...');
  //readln;
end.
