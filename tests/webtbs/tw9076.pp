{ %norun }

unit tw9076;

{$mode objfpc}

interface

type
  pfdset = pointer;

function __Select(N: Longint; ReadFds: PFDSet; WriteFds: PFDSet;
ExceptFds: PFDSet): Longint; inline;

implementation

function __Select(N: Longint; ReadFds: PFDSet; WriteFds: PFDSet;
ExceptFds: PFDSet): Longint;
begin
  try
    result := 2
  except
    Result := -1
  end
end;

end.


