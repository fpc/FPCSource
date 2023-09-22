program crctest;

{$mode objfpc}
{$h+}

uses
  sysutils,
  crc;

const
  testseq1: RawByteString = 'MNIIQGNLVGTGLKIGIVVGRFNDFITSKLLSGAEDALLRHGVDTNDIDVAWVPGAFEIPFAAKKMAETKKYDAIITLGTVIRGATTSYDYVCNEAAKGIAQAANTTGVPVIFGIVTTENIEQAIERAGTKAGNKGVDCAVSAIEMANLNRSFE';
  testseq2: RawByteString = 'MNIIQGNLVGTGLKIGIVVGRFNDFITSKLLSGAEDALLRHGVDTNDIDVAWVPGAFEIPFAAKKMAETKKYDAIITLGDVIRGATTHYDYVCNEAAKGIAQAANTTGVPVIFGIVTTENIEQAIERAGTKAGNKGVDCAVSAIEMANLNRSFE';

  test1_crc128: u128 = (lo:7787709990548801016; hi:8484981956151821693);
  test2_crc128: u128 = (lo:17574279593289983859; hi:10166839289973635932);
  test1_crc64: qword = 14444300186948028230;
  test2_crc64: qword = 3310614217963326015;
  test1_crc32: longword = 3405150022;
  test2_crc32: longword = 1264209917;


function IntToStr128(v: u128): RawByteString;
begin
  result := '$'+hexstr(v.hi,16)+hexstr(v.lo,16);
end;


procedure perform_crc32(const name, testcase: RawByteString; result: longword);
var
  crc: longword;
begin
  crc := crc32(0,nil,0);
  crc := crc32(crc,@testcase[1],length(testcase));

  write(name,'(size=',length(testcase),'): ');
  if crc=result then
    writeln('passed')
  else
    begin
      writeln('failed (got=',crc,',expected=',result,')');
      halt(1);
    end;
end;

procedure perform_crc64(const name, testcase: RawByteString; result: qword);
var
  crc: qword;
begin
  crc := crc64(0,nil,0);
  crc := crc64(crc,@testcase[1],length(testcase));

  write(name,'(size=',length(testcase),'): ');
  if crc=result then
    writeln('passed')
  else
    begin
      writeln('failed (got=',crc,',expected=',result,')');
      halt(1);
    end;
end;

procedure perform_crc128(const name, testcase: RawByteString; result: u128);
var
  crc: u128;
begin
  crc := crc128(0,nil,0);
  crc := crc128(crc,@testcase[1],length(testcase));

  write(name,'(size=',length(testcase),'): ');
  if crc=result then
    writeln('passed')
  else
    begin
      writeln('failed (got=',IntToStr128(crc),',expected=',IntToStr128(result),')');
      halt(1);
    end;
end;


begin
  perform_crc32('crc32', testseq1, test1_crc32);
  perform_crc32('crc32', testseq2, test2_crc32);
  perform_crc64('crc64', testseq1, test1_crc64);
  perform_crc64('crc64', testseq2, test2_crc64);
  perform_crc128('crc128', testseq1, test1_crc128);
  perform_crc128('crc128', testseq2, test2_crc128);
end.
