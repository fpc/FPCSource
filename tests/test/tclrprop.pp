{$mode delphi}

type
  tc = class
  end;

  tcc = class of tc;

  tc1 = class
   private
    fa: tcc;
   published
    property pa: tcc read fa write fa;
  end;

begin
end.
