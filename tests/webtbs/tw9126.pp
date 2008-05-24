{$ifdef fpc}
{$mode delphi}
{$endif}

uses sysutils;

var
  failed : boolean;

procedure testconvert(s : string; shouldsucceed: boolean);
var
  succeeded: boolean;
begin
  succeeded:=true;
  try
    writeln(strtofloat(s));
  except 
    on EConvertError do begin
      writeln('Failed to convert ', s, ' to a float value');
      succeeded := false;
    end;
  end;
  failed:=failed or (succeeded<>shouldsucceed);
end;

begin
  failed := false;
  
  thousandseparator := '.';
  decimalseparator := ',';
  
  testconvert('1.200',false); // fails
  testconvert('1,200',true); // working
  testconvert('1.200,23',false); // fails
  testconvert('1.200.300',false); // fails
  
  if (failed) then halt(1);
end.
