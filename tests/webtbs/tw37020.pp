{$mode objfpc}

program test;

type
 TItem = (A, B, C);
 TItems = set of TItem;
 generic GType<const T: TItems> = class
 end;

const
 TOtherItems = [];   // no problems here

type
 // error: Incompatible types: got "Empty Set" expected "TItems"
 TType = specialize GType<[]>;

begin
end.
