unit uverylongunitname3_123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345;
{$mode delphi}
(* This is a copy - This is deliberate
   This is used by "tlongname2" compiled with -gw3
   The other copy is compiled with -gw / Both tests need their own unit, to avoid re-using ppu
*)
interface
type TItem=record
      Name:String;
      Values:array of Integer;
     end;
     TItems=array of TItem;
procedure Fill(var aItems:TItems);
implementation
procedure Fill(var aItems:TItems);
begin
 SetLength(aItems,1);
 aItems[0].Name:='x';
end;
end.
