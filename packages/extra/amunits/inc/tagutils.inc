
{
   tagutils.inc

   Some handy functions to deal with taglist.
   At least until fpc have array of const.
}

procedure SetTags(ti : pTagItem; item, data : Longint);
begin
    with ti^ do begin
       ti_Tag := item;
       ti_Data := data;
    end;
end;

function TagItem(item, data : Longint): tTagItem;
var
   temp : tTagItem;
begin
   with temp do begin
      ti_Tag := item;
      ti_Data := data;
   end;
   TagItem := temp;
end;
