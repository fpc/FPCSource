{ %fail }

{ Source provided for Free Pascal Bug Report 2719 }
{ Submitted by "Samuel L.B." on  2003-10-06 }
{ e-mail: NLBCHGPOHSXQ@spammotel.com }
program A;

const
  cmConnect: Char =    #1;
  cmDisconnect: Char = #2;

function GetCh(Id: Byte): Char;
begin
end;

begin
  case GetCh(1) of
    cmConnect:  ;
    cmDisconnect: ;
  end;
end.
