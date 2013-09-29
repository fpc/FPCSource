{$mode objfpc}
type 
  TT_Stream   = record z : Pointer; end;

  TFreeTypeStream = class
    FUsed : Boolean;
  end;

 procedure TT_Done_Stream( stream : TT_Stream );
 begin
   if stream.z = nil then exit;
   TFreeTypeStream(stream.z).FUsed := false;
 end;


begin
end.
