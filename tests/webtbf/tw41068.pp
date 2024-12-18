program sl;
{$mode objfpc}
{$assertions on}
{$WARN 5024 off : Parameter "$1" not used}
uses
  SysUtils, Classes;

var
  Source, DestP, DestF: TStringList;
  i, StartIdx, EndIdx: Integer;


begin
  {$if declared(TMyStringsHelper)}
  writeln('Using TMyStringsHelper class');
  {$endif}
  Source := TStringList.Create;
  DestP := TStringList.Create;
  try
    for i := 0 to 19 do Source.Add(i.ToString);
    StartIDx := 7;
    EndIdx := 16;
    Source.Slice(StartIdx, EndIdx, DestP);
    TStrings(DestF) := Source.Slice(StartIdx, EndIdx);
    Assert(DestP.Count = DestF.Count,Format('DestP.Count (%d) <> DestF.Count (%d)',[DestP.Count, DestF.Count]));
    Assert(DestP.Count = EndIdx-StartIdx+1, Format('Dest.Count=%d, Expected: %d (%d-%d+1)',[DestP.Count,StartIdx-EndIdx+1,StartIdx,EndIdx]));
    writeln('Dest.Count=',DestP.Count, ' [Ok]');
    for i := 0 to DestP.Count-1 do
    begin
      Assert(DestP[i]=DestF[i],Format('Dest[%d] (%s) <> DestF[%d] (%s)',[i,DestP[i],i,DestF[i]]));
      Assert((DestP[i] = (i + StartIdx).ToString),Format('Dest[%d]: Found %s, Expected: %s',[i,DestP[i],(i + StartIdx).ToString]));
      writeln(i:2,': ',DestP[i]);
    end;
  finally
    Source.Free;
    DestP.Free;
    DestF.Free;
  end;
  writeln('TStrings.Slice test: Ok');
end.

