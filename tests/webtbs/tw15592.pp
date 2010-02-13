{ %norun }

program project1;
TYPE
  TRTime = Record
    rtDay : Integer;
  end;
  TTimeRange = Record
    trFlags : Integer;
    trTime : TRTime;
    case trType : Integer of
     0 : (trTime2 : TRTime);
     1 : (trMinutes : Integer);
  end;

begin
end.
