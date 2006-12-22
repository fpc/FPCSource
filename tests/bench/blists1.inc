  L:=TFPList.Create;
  Try
    T1:=Now;
    L.Capacity:=Max;
    For I:=0 to Max-1 do
      L.Add(Pointer(I));
    L.Clear;
    For I:=0 to Max-1 do
      L.Add(Pointer(I));
    // Hustle
    For I:=0 to Max-1 do
      begin
      J:=Random(Max);
      K:=Random(Max);
      L.Exchange(J,K);
      end;
    // Simple search
    For I:=0 to Max-1 do
      begin
      J:=L.IndexOf(Pointer(I));
      end;
    // Simple search and remove
    For I:=Max-1 downto 0 do
      L.Remove(Pointer(I));
    T2:=Now;
    Writeln('Time : ',FormatDateTime('hh:nn:ss.zzz',T2-T1));
  Finally
    L.Free;
  end;
