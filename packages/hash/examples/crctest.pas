program crctest;

{$mode objfpc}
{$h+}

uses
  sysutils,
  crc;

const
  testseq1: string = 'MNIIQGNLVGTGLKIGIVVGRFNDFITSKLLSGAEDALLRHGVDTNDIDVAWVPGAFEIPFAAKKMAETKKYDAIITLGTVIRGATTSYDYVCNEAAKGIAQAANTTGVPVIFGIVTTENIEQAIERAGTKAGNKGVDCAVSAIEMANLNRSFE';
  testseq2: string = 'MNIIQGNLVGTGLKIGIVVGRFNDFITSKLLSGAEDALLRHGVDTNDIDVAWVPGAFEIPFAAKKMAETKKYDAIITLGDVIRGATTHYDYVCNEAAKGIAQAANTTGVPVIFGIVTTENIEQAIERAGTKAGNKGVDCAVSAIEMANLNRSFE';

  test1_crc64: qword = 1;
  test2_crc64: qword = 1;
  test1_crc32: longword = 1;
  test2_crc32: longword = 1;


procedure perform_crc32(const name, testcase: string; result: longword);
var
  crc: longword;
begin
  crc := crc32(0,nil,0);
  crc := crc32(crc,@testcase[1],length(testcase));

  write(name,': ');
  if crc=result then
    writeln('passed')
  else
    writeln('failed');
end;

procedure perform_crc64(const name, testcase: string; result: qword);
var
  crc: qword;
begin
  crc := crc64(0,nil,0);
  crc := crc64(crc,@testcase[1],length(testcase));

  write(name,': ');
  if crc=result then
    writeln('passed')
  else
    writeln('failed');
end;


begin
  perform_crc32('crc32', testseq1, test1_crc32);
  perform_crc32('crc32', testseq2, test2_crc32);
  perform_crc64('crc64', testseq1, test1_crc64);
  perform_crc64('crc64', testseq2, test2_crc64);
end.
