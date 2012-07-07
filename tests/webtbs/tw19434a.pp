{ %norun }

unit tw19434a;
{$mode delphi}

interface

    function Connect(const aHost: string; const aPort: Word = 21): Boolean; overload;
    function Connect: Boolean; overload;

implementation

    function Connect(const aHost: string; const aPort: Word): Boolean;
      begin
      end;


    function Connect: Boolean;
      begin
      end;

end.

