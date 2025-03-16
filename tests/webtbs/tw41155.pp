{ %OPT=-O3 }
program tw41155;

{$mode objfpc} {$modeswitch anonymousfunctions}
type
  pAdapter = ^Adapter;
  Adapter = record
    start: SizeUint;
    adaptee: procedure(p: pointer; a, b: SizeUint);
  end;

  procedure UseAdapter(ad: pAdapter; a, b: SizeUint);
  begin
    ad^.adaptee(nil, ad^.start + a, ad^.start + b);
  end;

var
  ad: Adapter;
  oops: boolean = false;

begin
  ad.start := 10;
  ad.adaptee :=
    procedure(p: pointer; a, b: SizeUint)
    begin
      if p <> nil then begin writeln('p = ', PtrUint(p), ', should be 0.'); oops := true; end;
      if a <> 110 then begin writeln('a = ', a, ', should be 110.'); oops := true; end;
      if b <> 1010 then begin writeln('b = ', b, ', should be 1010.'); oops := true; end;
    end;
  UseAdapter(@ad, 100, 1000);
  if oops then halt(1);
  WriteLn('ok');
end.
