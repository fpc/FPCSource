{ %fail }

{ Source provided for Free Pascal Bug Report 3116 }
{ Submitted by "Maarten Bekers" on  2004-05-23 }
{ e-mail: fpc@elexer.com }
program eleforum;

type
  Longint       = Integer[4];


const
  forum_DiffFromLocal                = -2;


function forum_ConvertDateToUser(ThisDate: Longint): Longint;
begin
  if forum_DiffFromLocal < 0 then
    forum_ConvertDateToUser := ThisDate + Abs(forum_DiffFromLocal * 3600)
      else forum_ConvertDateToUser := ThisDate - Abs(forum_DiffFromLocal * 3600)
end; { func. forum_ConvertDateToUser }

end. { eleforum }
