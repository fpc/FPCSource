{ Source provided for Free Pascal Bug Report 3589 }
{ Submitted by "Michalis Kamburelis" on  2005-01-23 }
{ e-mail: michalis@camelot.homedns.org }
{$ifdef FPC} {$mode objfpc} {$endif}
{$assertions on}

uses Classes;

type
  TBoxedInt = class(TCollectionItem)
  public
    Value: Integer;
  end;
  
var 
  C: TCollection;
begin
 C := TCollection.Create(TBoxedInt);
 try
  TBoxedInt(C.Add).Value := 1;
  TBoxedInt(C.Add).Value := 2;
  TBoxedInt(C.Add).Value := 3;
  TBoxedInt(C.Insert(0)).Value := 4; { 4 1 2 3 }
  TBoxedInt(C.Insert(3)).Value := 5; { 4 1 2 5 3 }
  TBoxedInt(C.Insert(5)).Value := 6; { 4 1 2 5 3 6 }
{  Writeln(
    TBoxedInt(C.Items[0]).Value, ' ',
    TBoxedInt(C.Items[1]).Value, ' ',
    TBoxedInt(C.Items[2]).Value, ' ',
    TBoxedInt(C.Items[3]).Value, ' ',
    TBoxedInt(C.Items[4]).Value, ' ',
    TBoxedInt(C.Items[5]).Value);}
  Assert( 
    (TBoxedInt(C.Items[0]).Value = 4) and
    (TBoxedInt(C.Items[1]).Value = 1) and
    (TBoxedInt(C.Items[2]).Value = 2) and
    (TBoxedInt(C.Items[3]).Value = 5) and
    (TBoxedInt(C.Items[4]).Value = 3) and
    (TBoxedInt(C.Items[5]).Value = 6));
 finally C.Free end;
end.
