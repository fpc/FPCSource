{ Old file: tbs0024.pp }
{  }


type
  charset=set of char;

  trec=record
     junk : array[1..32] of byte;
     t    : charset;
  end;

  var
     tr    : trec;
     tp    : ^trec;


  procedure Crash(const k:charset);

    begin
       tp^.t:=[#7..#10]+k;
    end;

  begin
     tp:=@tr;
     Crash([#20..#32]);
  end.
