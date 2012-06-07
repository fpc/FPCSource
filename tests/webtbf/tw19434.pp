{ %fail }

unit tw19434;
{$mode delphi}

interface

function PostMessage2MainWnd(Msg: cardinal; wParam: longint;
  lParam: longint): boolean;

implementation

function PostMessage2MainWnd(Msg: cardinal; wParam: longint = 0;
  lParam: longint = 0): boolean; 
begin
end;

end.
