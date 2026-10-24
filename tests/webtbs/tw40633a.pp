{ %norun }
{ %needlibrary }
{ %target=linux }
{ %opt=-O- }

library tw40633a;

{$mode objfpc}

uses
  SysUtils;

// Raises an exception
procedure Raiser;

begin
  raise TObject.Create;
end;


// Returns the number of backtrace frames of an exception raised inside the library
function RaiseFrameCount: longint; cdecl;

begin
  Result:=-1;
  try
    Raiser;
  except
    Result:=ExceptFrameCount;
  end;
end;


exports
  RaiseFrameCount;

end.
