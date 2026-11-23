unit netdbtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Sockets, math, netdb;

const
  FAKETLD = 'doesnotexist';
  FAKEDOMAIN = 'fakedomain';

  FAKEFQDN=FAKEDOMAIN+'.'+FAKETLD;

type
  TDomainCompressionOffset = packed record
    nm: String;
    offset: Word;
  end;
  TDomainCompressionTable = Array of TDomainCompressionOffset;

  TTwoByteArr = array[0 .. 1] of Byte;
  TDNSDomainPointer = packed record
    case b: boolean of
      true: (ba: TTwoByteArr);
      false: (b1,b2: Byte);
  end;

  TDNSDomainByteStream = packed record
    ulabels: Array of byte;
    cptr: Word;
  end;

  TBuffer = Array of Byte;

  // can't use dynamic arrays in variant records, so fudge things by
  // having between 1 and 5 subsstrings per text RR. it's good enough
  // for these tests.
  TTextArray = array [1 .. 5] of ShortString;

  TFakeQuery = record
    nm: ShortString;
    qtype, qclass: Word;
  end;

  TFakeSOA = record
    mn,rn: ShortString;
    serial,refresh,retry,expire,min: Cardinal;
  end;
  TFakeMX = record
    pref: Word;
    exch: ShortString;
  end;
  TFakeSRV = record
    priority, weight, port: Word;
    target: ShortString;
  end;

  TFakeRR = record
    RRName   : ShortString;
    AClass   : Word;
    TTL      : Cardinal;
    RDLength : Word;
    case Atype: Word of
      DNSQRY_A: (ip: THostAddr);
      DNSQRY_AAAA: (ip6: THostAddr6);
      DNSQRY_CNAME: (cn: ShortString);
      DNSQRY_MX: (fmx: TFakeMX);
      DNSQRY_NS: (nsh: ShortString);
      DNSQRY_PTR: (ptr: ShortString);
      DNSQRY_SOA: (fsoa: TFakeSoa);
      DNSQRY_TXT: (sstrcount: Byte; txtarr: TTextArray);
      DNSQRY_SRV: (fsrv: TFakeSRV);
  end;

  TRRSection = Array of TFakeRR;

  TFakeDNSResponse = record
    strtable: TDomainCompressionTable;
    compresslabels: Boolean;
    hdr: TDNSHeader;
    qry: TFakeQuery;
    answers, authority, additional: TRRSection;
  end;

  TRDataWriteRes = packed record
    bw, etw: Word;
  end;

  { TNetDbTest }

  TNetDbTest= class(TTestCase)
  strict private
    tsl: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure BuildFakeRR_A(out RR: TFakeRR; nm: String; ttl: Cardinal;
      val: String);
    procedure BuildFakeRR_AAAA(out RR: TFakeRR; nm: String; ttl: Cardinal;
      val: String);
    procedure BuildFakeRR_MX(out RR: TFakeRR; nm: String; ttl: Cardinal;
      pref: Word; exch: ShortString );
    procedure BuildFakeRR_NS(out RR: TFakeRR; nm: String; ttl: Cardinal;
      val: String);
    procedure BuildFakeRR_PTR(out RR: TFakeRR; nm: String; ttl: Cardinal;
      val: String);
    procedure BuildFakeRR_CNAME(out RR: TFakeRR; nm: String; ttl: Cardinal;
      val: String);
    procedure BuildFakeRR_SOA(out RR: TFakeRR; nm: String; ttl: Cardinal;
      mn,rn: ShortString; serial,refresh,retry,expire,min: Cardinal);
    procedure BuildFakeRR_TXT(out RR: TFakeRR; nm: String; ttl: Cardinal;
      n: Byte; txt: TTextArray);
    procedure BuildFakeRR_SRV(out RR: TFakeRR; nm: String; ttl: Cardinal;
       priority, weight, port: Word; target: ShortString);

    procedure CopyBytesTo(var buf: TPayLoad; startidx,destidx,count: Word);
    procedure CopyBytesTo(var buf: TPayLoadTCP; startidx,destidx,count: Word);

    function WriteNumToBuffer(var buf: TBuffer; var offset: Cardinal;
       val: Word): Word;
    function WriteNumToBuffer(var buf: TBuffer; var offset: Cardinal;
       val: Cardinal): Word;

    function WriteNumToBufferN(var buf: TBuffer; var offset: Cardinal;
       val: Word): Word;
    function WriteNumToBufferN(var buf: TBuffer; var offset: Cardinal;
       val: Cardinal): Word;

    function WriteMXAsRData(var buf: TBuffer; var offset: Cardinal;
      fmx: TFakeMX): TRDataWriteRes;
    function WriteSOAasRData(var buf: TBuffer; var offset: Cardinal;
      fsoa: TFakeSOA): TRDataWriteRes;
    function WriteSOAasRData(var buf: TBuffer; var offset: Cardinal;
      fsoa: TFakeSOA; var ctbl: TDomainCompressionTable): TRDataWriteRes;
    function WriteAAAAasRData(var buf: TBuffer; var offset: Cardinal;
      ip6: THostAddr6): TRDataWriteRes;
    function WriteAasRData(var buf: TBuffer; var offset: Cardinal;
      ip: THostAddr): TRDataWriteRes;

    function WriteSRVasRData(var buf: TBuffer; var offset: Cardinal;
      fsrv: TFakeSRV): TRDataWriteRes;
    function WriteSRVasRData(var buf: TBuffer; var offset: Cardinal;
      fsrv: TFakeSRV; var ctbl: TDomainCompressionTable): TRDataWriteRes;

    function WriteMXAsRData(var buf: TBuffer; var offset: Cardinal;
      fmx: TFakeMX; var ctbl: TDomainCompressionTable): TRDataWriteRes;

    function CalcRdLength(o: TDNSDomainByteStream): Word;
    function CalcRdLength(o: TTextArray): Word;
    function WriteTextRecAsRData(var buf: TBuffer; var offset: Cardinal;
      tt: TTextArray): TRDataWriteRes;

    function DomainNameToByteStream(nm: ShortString;
      var ctbl: TDomainCompressionTable): TDNSDomainByteStream;
    function DomainNameToByteStream(nm: ShortString): TDNSDomainByteStream;

    function WriteDNSDomainByteStreamToBuffer(var buf: TBuffer;
      var offset: Cardinal; dbs: TDNSDomainByteStream): Word;

    function WriteDomainAsRdata(var buf: TBuffer; var offset: Cardinal;
      dbs: TDNSDomainByteStream): TRDataWriteRes;

    function WriteRRToBuffer(var buf: TBuffer; var offset: Cardinal;
      rr: TFakeRR): Word;
    function WriteRRToBuffer(var buf: TBuffer; var offset: Cardinal;
      rr: TFakeRR; var ctbl: TDomainCompressionTable): Word;
    function FakeDNSResponseToByteBuffer(fdr: TFakeDNSResponse;
      out buf: TBuffer; compress: Boolean = False): Cardinal;
    function BufferToPayload(const buf: TBuffer; out pl: TPayload): Boolean;
    function BufferToPayload(const buf: TBuffer; out pl: TPayLoadTCP): Boolean;

    function BuildQueryData(fdr: TFakeDNSResponse; out qd: TQueryData;
      out qlen: Word; Compress: Boolean = False): Boolean;

    function BuildQueryData(fdr: TFakeDNSResponse;
      out qd: TQueryDataLengthTCP; out qlen: Word;
        Compress: Boolean = False): Boolean;

    function BuildTruncatedQueryData(fdr: TFakeDNSResponse; out qd: TQueryData;
      out qlen: Word; truncoffset: Word): Boolean;

    procedure BuildFakeResponseA(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseAAAA(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseMX(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseSOA(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseCNAME(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseNS(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponsePTR(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseTXT(nm: ShortString; out fr: TFakeDNSResponse);
    procedure BuildFakeResponseSRV(nm: ShortString; out fr: TFakeDNSResponse);

  published
    procedure TestBuildPayloadSimple;
    procedure TestBuildPayloadSimpleEmpty;
    procedure TestBuildPayloadSimpleEndDot;
    procedure TestBuildPayloadSimpleStartDot;
    procedure TestBuildPayloadSimpleMultipleDot;
    procedure TestBuildPayloadLabelBytes;
    procedure TestStringFromLabelBytes;

    { * straightforward tests for the api with valid data. Have to test each
      * known RR type with both TCP and UDP buffer functions, and with and
      * without compression of domain names.
      * No network calls will be made. These tests hit all functions for
      * processing dns requests except network functions.}
    procedure TestDnsQueryUDP_A;
    procedure TestDnsQueryTCP_A;
    procedure TestDnsQueryCompressUDP_A;
    procedure TestDnsQueryCompressTCP_A;

    procedure TestDnsQueryUDP_AAAA;
    procedure TestDnsQueryTCP_AAAA;
    procedure TestDnsQueryCompressUDP_AAAA;
    procedure TestDnsQueryCompressTCP_AAAA;

    procedure TestDnsQueryUDP_MX;
    procedure TestDnsQueryTCP_MX;
    procedure TestDnsQueryCompressUDP_MX;
    procedure TestDnsQueryCompressTCP_MX;

    procedure TestDnsQueryUDP_SOA;
    procedure TestDnsQueryTCP_SOA;
    procedure TestDnsQueryCompressUDP_SOA;
    procedure TestDnsQueryCompressTCP_SOA;

    procedure TestDnsQueryUDP_CNAME;
    procedure TestDnsQueryTCP_CNAME;
    procedure TestDnsQueryCompressUDP_CNAME;
    procedure TestDnsQueryCompressTCP_CNAME;

    procedure TestDnsQueryUDP_NS;
    procedure TestDnsQueryTCP_NS;
    procedure TestDnsQueryCompressUDP_NS;
    procedure TestDnsQueryCompressTCP_NS;

    procedure TestDnsQueryUDP_PTR;
    procedure TestDnsQueryTCP_PTR;
    procedure TestDnsQueryCompressUDP_PTR;
    procedure TestDnsQueryCompressTCP_PTR;

    procedure TestDnsQueryUDP_TXT;
    procedure TestDnsQueryTCP_TXT;
    procedure TestDnsQueryCompressUDP_TXT;
    procedure TestDnsQueryCompressTCP_TXT;

    procedure TestDnsQueryUDP_SRV;
    procedure TestDnsQueryTCP_SRV;
    procedure TestDnsQueryCompressUDP_SRV;
    procedure TestDnsQueryCompressTCP_SRV;

    {
      * Tests with invalid input data. These attempt to simulate a hostile
      * dns server returning deliberately invalid data in an attempt to
      * cause a buffer overflow, memory corruption, or DDOS.
    }

    // buffer truncated so RRs have invalid types.
    procedure TestDnsQueryTruncateRR_UDP_A;

    {
     * Tests of DNSRRGet* functions where RR is near the end of the buffer,
     * testing both when the RR just fits, and when it doesn't.
     }
    procedure TestDnsRRBufferEdgeA;
    procedure TestDnsRRBufferPastEdgeA;
    procedure TestDnsRRBufferEdgeAAAA;
    procedure TestDNsRRBufferPastEdgeAAAA;
    procedure TestDnsRRBufferEdgeMX;
    procedure TestDnsRRBufferPastEdgeMX;
    procedure TestDnsRRBufferEdgeSOA;
    procedure TestDnsRRBufferPastEdgeSOA;
    procedure TestDnsRRBufferEdgeSRV;
    procedure TestDnsRRBufferPastEdgeSRV;
    procedure TestDnsRRBufferEdgeCNAME;
    procedure TestDnsRRBufferPastEdgeCNAME;
    procedure TestDnsRRBufferEdgeNS;
    procedure TestDnsRRBufferPastEdgeNS;
    procedure TestDnsRRBufferEdgePTR;
    procedure TestDnsRRBufferPastEdgePTR;
    procedure TestDnsRRBufferEdgeTXT;
    procedure TestDnsRRBufferPastEdgeTXT;


    {
     * the TCP variants. identical code, but qd variable is a different type
     * and so different paths get followed in netdb.
    }
    procedure TestDnsRRBufferEdgeTCPA;
    procedure TestDnsRRBufferPastEdgeTCPA;
    procedure TestDnsRRBufferEdgeTCPAAAA;
    procedure TestDNsRRBufferPastEdgeTCPAAAA;
    procedure TestDnsRRBufferEdgeTCPMX;
    procedure TestDnsRRBufferPastEdgeTCPMX;
    procedure TestDnsRRBufferEdgeTCPSOA;
    procedure TestDnsRRBufferPastEdgeTCPSOA;
    procedure TestDnsRRBufferEdgeTCPSRV;
    procedure TestDnsRRBufferPastEdgeTCPSRV;
    procedure TestDnsRRBufferEdgeTCPCNAME;
    procedure TestDnsRRBufferPastEdgeTCPCNAME;
    procedure TestDnsRRBufferEdgeTCPNS;
    procedure TestDnsRRBufferPastEdgeTCPNS;
    procedure TestDnsRRBufferEdgeTCPPTR;
    procedure TestDnsRRBufferPastEdgeTCPPTR;
    procedure TestDnsRRBufferEdgeTCPTXT;
    procedure TestDnsRRBufferPastEdgeTCPTXT;

    // Testing of NextNameRR at buffer edge and beyond. this differs from
    // the above tests in that they tests DNSGet* at the edge, but NextNameRR
    // is never called to read at the edge in those functions.
    // Because NextNameRR does nothing that is specific to RR types it's
    // not necessary to test with each type of RR.

    procedure TestNextNameRREdgeA;
    procedure TestNextNameRRPastEdgeA;
    procedure TestNextNameRREdgeTCPA;
    procedure TestNextNameRRPastEdgeTCPA;

    {
     * Test GetRRrecords at and beyond buffer boundaries.
    }
    procedure TestGetRRrecordsInvalidStart;
    procedure TestGetRRrecordsInvalidStartTCP;

    {
    Tests for GetFixlenStr
    }
    procedure TestGetFixLenStrSimple;
    procedure TestGetFixLenStrSimpleTCP;
    procedure TestGetFixLenStrSimpleAtEdge;
    procedure TestGetFixLenStrSimpleTCPAtEdge;
    procedure TestGetFixLenStrSimplePastEdge;
    procedure TestGetFixLenStrSimpleTCPPastEdge;


    {
     * Test stringfromlabel with buffer edges and beyond. Its behaviour
     * at present is to drop any label that would exceed the buffer boundary
     * but still return any other labels successfully received.

     * Some of the previous tests already verify what happens with a label
     * that occurs on the edge. See the tests for TestDnsRRBufferEdgeSRV
     * and TestDnsRRBufferEdgeTCPSRV, TestDnsRRBufferEdgeCNAME, etc.
    }

    // read a label starting at the end of the buffer where the count is
    // greater than 0.
    procedure TestStringFromLabelCountAsLastByte;
    procedure TestStringFromLabelCountAsLastByteTCP;

    // compressed label
    procedure TestStringFromLabelCompress;
    procedure TestStringFromLabelCompressTCP;
    // another compressed label test, this time with one uncompressed label
    procedure TestStringFromLabelCompressWithUncompressedLabel;
    // as above, but on the tcp payload buffer
    procedure TestStringFromLabelCompressWithUncompressedLabelTCP;
    // compressed label at the edge of the buffer
    procedure TestStringFromLabelCompressEndBuffer;
    // compressed label at the edge of the tcp buffer
    procedure TestStringFromLabelCompressEndBufferTCP;
    // test stringfromlabel when last byte is 192. 192 is the signal
    // that the next byte is a pointer offset, but of course there's
    // no next byte.
    procedure TestStringFromLabelCompressSplit;
    // repeat using TCP buffer variant
    procedure TestStringFromLabelCompressSplitTCP;
    // test that stringfromlabel rejects pointers that go forward. per
    // rfc 1035, pointers must go backward.
    procedure TestStringFromLabelCompressPtrFwd;
    procedure TestStringFromLabelCompressPtrFwdTCP;
    // fill buffer with 192, pointer marker, then try stringfromlabel on it.
    procedure TestStringFromLabelCompressAllPtrStart;
    procedure TestStringFromLabelCompressAllPtrStartTCP;

    // test string from label where second byte is 0.
    procedure TestStringFromLabelCompressedZero;
    procedure TestStringFromLabelCompressedZeroTCP;

    // test whether an infinite loop can be triggered.
    procedure TestStringFromLabelInfiniteLoop;
    procedure TestStringFromLabelInfiniteLoopTCP;

    // test short domain less than 12 chars. this tests that dns pointer
    // calculations in stringfromlabel are correct
    procedure TestCompressShortDomain;
    procedure TestCompressShortDomainTCP;

    // ExtractAnswer*, the answer parsing used by ResolveName, ResolveName6 and ResolveAddress
    procedure TestExtractAnswerAddresses;
    procedure TestExtractAnswerAddressesCompressed;
    procedure TestExtractAnswerAddresses6;
    procedure TestExtractAnswerNames;
    procedure TestExtractAnswerCName;
    procedure TestExtractAnswerAddressesArrayFull;
    procedure TestExtractAnswerAddressesAnCountTooHigh;
    procedure TestExtractAnswerAddressesSkipsBadRDLength;
    procedure TestExtractAnswerAddressesRDLengthPastEnd;
    procedure TestExtractAnswerAddressesRecordPastAnsLen;
    procedure TestExtractAnswerAddressesLabelPastEnd;
    procedure TestExtractAnswerAddressesPointerByteAtEnd;
    procedure TestExtractAnswerNamesPointerLoop;
    procedure TestExtractAnswerNamesSelfPointer;

    // RandomQueryID and IsAnswerTo, the reply checks used by Query
    procedure TestRandomQueryIDIgnoresRandSeed;
    procedure TestRandomQueryIDSpread;
    procedure TestIsAnswerTo;
    procedure TestIsAnswerToCaseInsensitive;
    procedure TestIsAnswerToWrongID;
    procedure TestIsAnswerToNotResponse;
    procedure TestIsAnswerToOtherName;
    procedure TestIsAnswerToOtherType;
    procedure TestIsAnswerToShort;
    procedure TestIsAnswerToErrorWithoutQuestion;
    procedure TestIsAnswerToNoQuestionNoError;

    // Query against a UDP server on the loopback interface
    procedure TestQueryGoodReply;
    procedure TestQuerySkipsWrongID;
    procedure TestQuerySkipsOtherQuestion;
    procedure TestQueryIgnoresOtherSource;
    procedure TestQueryErrorReply;
    procedure TestQueryOnlyWrongReplies;
    procedure TestQueryTimeout;
  end;

implementation

uses
  BaseUnix;

procedure dump_payload(const pl: TBuffer);
var
  idx,llen: Cardinal;
begin
  idx := 0;
  llen := 0;
  for idx := 0 to Length(pl) - 1 do
  begin
    write('['+inttostr(idx)+'] '+IntToHex(pl[idx],2));
    if (pl[idx] > 48) and (pl[idx] < 123) then
      write(' ' + chr(pl[idx]))
    else
      write(' .');
    write(' ');
    Inc(llen);
    if llen >= 6 then
    begin
      llen := 0;
      writeln();
    end;
  end;
  if llen > 0 then
  begin
    writeln();
  end;
end;

procedure dump_payload(const pl: TPayload; count: Word);
var
  idx,llen: Cardinal;
begin
  idx := 0;
  llen := 0;
  for idx := 0 to count - 1 do
  begin
    write('['+inttostr(idx)+'] '+IntToHex(pl[idx],2));
    if (pl[idx] > 48) and (pl[idx] < 123) then
      write(' ' + chr(pl[idx]))
    else
      write(' .');
    write(' ');
    Inc(llen);
    if llen >= 6 then
    begin
      llen := 0;
      writeln();
    end;
  end;
  if llen > 0 then
  begin
    writeln();
  end;
end;

function LookupStr(ls: String; stt: TDomainCompressionTable; out idx: Word): Boolean;
var
  so: TDomainCompressionOffset;
begin
  Result := False;
  for so in stt do
  begin
    if ls = so.nm then
    begin
      Result := True;
      idx := so.offset;
      exit;
    end;
  end;
end;

function AddStr(ls: String; var stt: TDomainCompressionTable; idx: Word): Boolean;
var
  so: TDomainCompressionOffset;
begin
  so.nm := ls;
  so.offset := idx;
  SetLength(stt, Length(stt)+1);
  stt[Length(stt)-1] := so;
  Result := True;
end;

function GetDnsDomainPointer(offset: Word): TDNSDomainPointer;
begin
  Result.b1 := 0;
  Result.b2 := 0;
  // dns comp. ptr can't be > 2 ** 14 or 16383
  if offset > 16383 then exit;
  Result.b1 := (offset SHR 8) OR 192;
  Result.b2 := (offset AND $00FF);
end;

procedure DomainNameToLabels(const dmn: String; var labels: TStringList);
begin
  labels.Clear;
  labels.Delimiter := '.';
  labels.StrictDelimiter := True;
  labels.DelimitedText := dmn;
end;

procedure TNetDbTest.BuildFakeRR_A(out RR: TFakeRR; nm: String; ttl: Cardinal;
  val: String);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_A;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.ip := StrToNetAddr(val);
  RR.RDLength := 4;
end;

procedure TNetDbTest.BuildFakeRR_AAAA(out RR: TFakeRR; nm: String;
  ttl: Cardinal; val: String);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_AAAA;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.ip6 := StrToNetAddr6(val);
  RR.RDLength := 16;
end;

procedure TNetDbTest.BuildFakeRR_MX(out RR: TFakeRR; nm: String; ttl: Cardinal;
  pref: Word; exch: ShortString );
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_MX;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.fmx.pref := pref;
  RR.fmx.exch := exch;
end;

procedure TNetDbTest.BuildFakeRR_NS(out RR: TFakeRR; nm: String; ttl: Cardinal;
  val: String);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_NS;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.nsh := val;
end;

procedure TNetDbTest.BuildFakeRR_PTR(out RR: TFakeRR; nm: String; ttl: Cardinal;
  val: String);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_PTR;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.ptr := val;
end;

procedure TNetDbTest.BuildFakeRR_CNAME(out RR: TFakeRR; nm: String;
  ttl: Cardinal; val: String);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_CNAME;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.cn := val;
end;

procedure TNetDbTest.BuildFakeRR_SOA(out RR: TFakeRR; nm: String; ttl: Cardinal;
  mn,rn: ShortString; serial,refresh,retry,expire,min: Cardinal);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_SOA;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.fsoa.mn := mn;
  RR.fsoa.rn := rn;
  RR.fsoa.serial := serial;
  RR.fsoa.refresh := refresh;
  RR.fsoa.retry := retry;
  RR.fsoa.expire := expire;
  RR.fsoa.min := min;
end;

procedure TNetDbTest.BuildFakeRR_TXT(out RR: TFakeRR; nm: String; ttl: Cardinal;
  n: Byte; txt: TTextArray);
var
  idx: Byte;
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_TXT;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.sstrcount := n;
  RR.txtarr[1] := '';
  RR.txtarr[2] := '';
  RR.txtarr[3] := '';
  RR.txtarr[4] := '';
  RR.txtarr[5] := '';
  for idx := Low(txt) to Min(n, High(txt)) do
    RR.txtarr[idx] := txt[idx];
end;

procedure TNetDbTest.BuildFakeRR_SRV(out RR: TFakeRR; nm: String; ttl: Cardinal;
   priority, weight, port: Word; target: ShortString);
begin
  RR.RRName := nm;
  RR.Atype := DNSQRY_SRV;
  RR.AClass := 1;
  RR.TTL := ttl;
  RR.fsrv.priority := priority;
  RR.fsrv.weight := weight;
  RR.fsrv.port := port;
  RR.fsrv.target := target;
end;

function TNetDbTest.CalcRdLength(o: TTextArray): Word;
var
  tmps: ShortString;
begin
  Result := 0;
  for tmps in o do
  begin
    if tmps = '' then break;
    Result := Result + Length(tmps)+1; // don't forget length byte!
  end;
end;

function TNetDbTest.WriteAasRData(var buf: TBuffer; var offset: Cardinal;
  ip: THostAddr): TRDataWriteRes;
var
  s,l: Word;
begin
  s := offset;
  l := SizeOf(ip.s_addr);
  Result.etw := l + 2; //rdlength +2 for length itself
  // rdlength
  WriteNumToBuffer(buf, offset, l);
  // rr data
  WriteNumToBufferN(buf, offset, ip.s_addr);
  Result.bw := offset - s;
end;

function TNetDbTest.WriteSRVasRData(var buf: TBuffer; var offset: Cardinal;
  fsrv: TFakeSRV): TRDataWriteRes;
var
  s, l: Word;
  dmbs: TDNSDomainByteStream;
begin
  s := offset;
  dmbs := DomainNameToByteStream(fsrv.target);
  l := CalcRdLength(dmbs) + SizeOf(Word) * 3;
  Result.etw := l + 2; //rdlength +2 for length byte

  // rdlength
  WriteNumToBuffer(buf, offset, l);

  // RR data
  WriteNumToBuffer(buf, offset, fsrv.priority);
  WriteNumToBuffer(buf, offset, fsrv.weight);
  WriteNumToBuffer(buf, offset, fsrv.port);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  Result.bw := offset - s;
end;

function TNetDbTest.WriteSRVasRData(var buf: TBuffer; var offset: Cardinal;
  fsrv: TFakeSRV; var ctbl: TDomainCompressionTable): TRDataWriteRes;
var
  s, l: Word;
  dmbs: TDNSDomainByteStream;
begin
  s := offset;
  dmbs := DomainNameToByteStream(fsrv.target, ctbl);
  l := CalcRdLength(dmbs) + SizeOf(Word) * 3;
  Result.etw := l + 2; //rdlength +2 for length byte

  // rdlength
  WriteNumToBuffer(buf, offset, l);

  // RR data
  WriteNumToBuffer(buf, offset, fsrv.priority);
  WriteNumToBuffer(buf, offset, fsrv.weight);
  WriteNumToBuffer(buf, offset, fsrv.port);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  Result.bw := offset - s;
end;

function TNetDbTest.WriteMXAsRData(var buf: TBuffer; var offset: Cardinal;
  fmx: TFakeMX; var ctbl: TDomainCompressionTable): TRDataWriteRes;
var
  s, l: Word;
  dmbs: TDNSDomainByteStream;
begin
  s := offset;
  dmbs := DomainNameToByteStream(fmx.exch, ctbl);
  l := SizeOf(fmx.pref) + CalcRdLength(dmbs);
  Result.etw := l + 2; // we'll write rdlength bytes+2 bytes for length itself.

  // rdlength
  WriteNumToBuffer(buf, offset, l);

  // RR data
  // pref
  WriteNumToBuffer(buf, offset, fmx.pref);
  // exchange
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  Result.bw := offset - s;
end;

function TNetDbTest.CalcRdLength(o: TDNSDomainByteStream): Word;
begin
  Result := Length(o.ulabels);
  if o.cptr > 0 then Inc(Result,2);
end;

function TNetDbTest.WriteAAAAasRData(var buf: TBuffer; var offset: Cardinal;
  ip6: THostAddr6): TRDataWriteRes;
var
  s,l: Word;
begin
  s := offset;
  l := SizeOf(ip6.u6_addr32);
  Result.etw := l + 2; //rdlength + 2 for length itself
  // rdlength
  WriteNumToBuffer(buf, offset, l);
  // rr data
  Move(ip6.s6_addr, buf[offset], l);
  Inc(offset, l);
  Result.bw := offset - s;
end;

function TNetDbTest.WriteSOAasRData(var buf: TBuffer; var offset: Cardinal;
  fsoa: TFakeSOA): TRDataWriteRes;
var
  s, l: Word;
  dmbsmn, dmbsrn: TDNSDomainByteStream;
begin
  s := offset;
  dmbsmn := DomainNameToByteStream(fsoa.mn);
  dmbsrn := DomainNameToByteStream(fsoa.rn);
  l := CalcRdLength(dmbsmn) + CalcRdLength(dmbsrn) + (SizeOf(Cardinal) * 5);

  Result.etw := l + 2; // rdlength bytes + 2 for length itself

  // rdlength
  WriteNumToBuffer(buf, offset, l);

  // rr data
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbsmn);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbsrn);

  WriteNumToBuffer(buf, offset, fsoa.serial);
  WriteNumToBuffer(buf, offset, fsoa.refresh);
  WriteNumToBuffer(buf, offset, fsoa.retry);
  WriteNumToBuffer(buf, offset, fsoa.expire);
  WriteNumToBuffer(buf, offset, fsoa.min);

  Result.bw := offset - s;
end;

function TNetDbTest.WriteSOAasRData(var buf: TBuffer; var offset: Cardinal;
  fsoa: TFakeSOA; var ctbl: TDomainCompressionTable): TRDataWriteRes;
var
  s, l: Word;
  dmbsmn, dmbsrn: TDNSDomainByteStream;
begin
  s := offset;
  dmbsmn := DomainNameToByteStream(fsoa.mn, ctbl);
  dmbsrn := DomainNameToByteStream(fsoa.rn, ctbl);
  l := CalcRdLength(dmbsmn) + CalcRdLength(dmbsrn) + (SizeOf(Cardinal) * 5);
  Result.etw := l + 2; // rdlength bytes + 2 for length itself

  // rdlength
  WriteNumToBuffer(buf, offset, l);

  // rr data
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbsmn);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbsrn);

  WriteNumToBuffer(buf, offset, fsoa.serial);
  WriteNumToBuffer(buf, offset, fsoa.refresh);
  WriteNumToBuffer(buf, offset, fsoa.retry);
  WriteNumToBuffer(buf, offset, fsoa.expire);
  WriteNumToBuffer(buf, offset, fsoa.min);

  Result.bw := offset - s;
end;

function TNetDbTest.WriteMXAsRData(var buf: TBuffer; var offset: Cardinal;
  fmx: TFakeMX): TRDataWriteRes;
var
  s, l: Word;
  dmbs: TDNSDomainByteStream;
begin
  Result.bw := 0;
  s := offset;
  dmbs := DomainNameToByteStream(fmx.exch);
  l := SizeOf(fmx.pref) + CalcRdLength(dmbs);
  Result.etw := l + 2; // we'll write rdlength + 2 bytes for the length itself.

  // rdlength
  WriteNumToBuffer(buf, offset, l);

  // RR data
  // pref
  WriteNumToBuffer(buf, offset, fmx.pref);
  // exchange
  dmbs := DomainNameToByteStream(fmx.exch);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  Result.bw := offset - s;
end;

function TNetDbTest.WriteTextRecAsRData(var buf: TBuffer; var offset: Cardinal;
  tt: TTextArray): TRDataWriteRes;
var
  s, l: Word;
  ws: ShortString;
begin
  s := offset;
  l := CalcRdLength(tt);
  Result.etw := l + 2; // rdlength +2 for length itself
  // rdlength
  WriteNumToBuffer(buf, offset, l);

  for ws in tt do
  begin
    if  ws = '' then break;
    Move(ws, buf[offset], Length(ws)+1);
    Inc(offset,Length(ws)+1);
  end;

  Result.bw := offset - s;
end;

{
Convert a domain name into a byte stream. Compression is supported using the
supplied compression table.
}
function TNetDbTest.DomainNameToByteStream(nm: ShortString;
  var ctbl: TDomainCompressionTable): TDNSDomainByteStream;
var
  dmn: ShortString;
  offset,cmpoffset: Word;
  ptrseen: Boolean = False;
begin
  SetLength(Result.ulabels, 0);
  Result.cptr := 0;
  offset := 0;

  if nm = '' then exit;
  DomainNameToLabels(nm, tsl);
  if tsl.Count = 0 then exit;

  dmn := '';
  cmpoffset := 0;

  {
  for a domain a.b.c, using the lookup table,
   -> lookup (a.b.c), if not found, add to table,
   ->   lookup (b.c), if not found, add to table,
   ->   lookup   (c), if not found, add to table,

   buf if any label domain is found, add the pointer to the buffer and stop.
  }
  repeat
    dmn := tsl.DelimitedText;
    ptrseen := LookupStr(dmn, ctbl, cmpoffset);
    if ptrseen then
    begin
      // found the domain name. add a pointer, then we're done. Per RFC1035,
      // section 4.1.4, a domain name is either a series of labels, a pointer,
      // or a series of labels ending with a pointer. There's just one pointer
      // for a domain name.
      Result.cptr := cmpoffset;
      break;
    end
    else
    begin
      // add the last full domain we looked up, not the working label,
      // to the compression lookup table. E.g, add a.b.c rather than a.
      // Add 12 for the dns header, which our buffer doesn't include, but
      // api methods like stringfromlabel adjust offsets to account for it.
      if Length(dmn) > 0 then AddStr(dmn, ctbl, offset+12);
      // write the label to the buffer
      dmn := tsl[0];
      tsl.Delete(0);
      SetLength(Result.ulabels, (Length(Result.ulabels) + Length(dmn)+1));
      Result.ulabels[offset] := Length(dmn);
      Inc(offset);
      Move(dmn[1], Result.ulabels[offset], Length(dmn));
      Inc(offset, Length(dmn));
    end;
  until tsl.Count = 0;

  // if we didn't see a pointer then we have to write a 0. see rfc1035, s4.1.4.
  if not ptrseen then
  begin
    SetLength(Result.ulabels, Length(Result.ulabels) + 1);
    Result.ulabels[offset] := 0;
    Inc(offset);
  end;
end;

{
This version of DomainNameToByteStream doesn't compress.
}
function TNetDbTest.DomainNameToByteStream(nm: ShortString
  ): TDNSDomainByteStream;
var
  dmn: ShortString;
  offset: Word;
begin
  SetLength(Result.ulabels, 0);
  Result.cptr := 0;
  offset := 0;

  if nm = '' then exit;
  DomainNameToLabels(nm, tsl);
  if tsl.Count = 0 then exit;

  for dmn in tsl do
  begin
    SetLength(Result.ulabels, (Length(Result.ulabels) + Length(dmn)+1));
    Result.ulabels[offset] := Length(dmn);
    Inc(offset);
    Move(dmn[1], Result.ulabels[offset], Length(dmn));
    Inc(offset, Length(dmn));
  end;

  SetLength(Result.ulabels, Length(Result.ulabels) + 1);
  Result.ulabels[offset] := 0;
end;

function TNetDbTest.WriteDNSDomainByteStreamToBuffer(var buf: TBuffer;
  var offset: Cardinal; dbs: TDNSDomainByteStream): Word;
var
  p: TDNSDomainPointer;
  so: Word;
begin
  Result := 0;
  // no label, no pointer, no write for you.
  if (Length(dbs.ulabels) = 0) and (dbs.cptr = 0) then exit;
  if (offset + CalcRdLength(dbs)) > Length(buf) then exit;

  so := offset;
  // labels can be empty, in which case we're writing just a pointer.
  if Length(dbs.ulabels) > 0 then
  begin
    Move(dbs.ulabels[0], buf[offset], Length(dbs.ulabels));
    Inc(offset, Length(dbs.ulabels));
  end;
  if dbs.cptr > 0 then
  begin
    p := GetDnsDomainPointer(dbs.cptr);
    Move(p.ba, buf[offset], Length(p.ba));
    Inc(offset, Min(Length(p.ba), (Length(buf) - offset)));
  end;
  Result := offset - so;
end;

{
Write a domain name as RDATA. This means an RDLength (Word) and the
domain labels.
}
function TNetDbTest.WriteDomainAsRdata(var buf: TBuffer; var offset: Cardinal;
  dbs: TDNSDomainByteStream): TRDataWriteRes;
var
  s,l: Word;
begin
  l := CalcRdLength(dbs);
  Result.etw := l + 2;
  s := offset;
  WriteNumToBuffer(buf, offset,l);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dbs);
  Result.bw := offset - s;
end;


procedure TNetDbTest.BuildFakeResponseA(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 2;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_A;

  // now the answer RRs
  SetLength(fr.answers,2);
  BuildFakeRR_A(fr.answers[0], nm, 300, '127.0.0.1');
  BuildFakeRR_A(fr.answers[1], nm, 215, '127.0.5.1');
end;

procedure TNetDbTest.BuildFakeResponseAAAA(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 2;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_AAAA;

  // now the answer RRs
  SetLength(fr.answers,2);
  BuildFakeRR_AAAA(fr.answers[0], nm, 300, 'fe80::3b92:3429:ff16:a3e4');
  BuildFakeRR_AAAA(fr.answers[1], nm, 215, 'fe80::92e6:baff:fe44:ffbb');
end;

procedure TNetDbTest.BuildFakeResponseMX(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 2;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_MX;

  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_MX(fr.answers[0], nm, 0, 10, 'mailer.'+FAKEFQDN);
  // now an additional rr with the A record for the above.
  SetLength(fr.additional, 2);
  BuildFakeRR_A(fr.additional[0], 'mailer.'+FAKEFQDN, 0,
    '172.16.27.238');
  BuildFakeRR_AAAA(fr.additional[1], 'mailer.'+FAKEFQDN, 0,
    'fe80::3b92:3429:ff16:a3e4');
end;

procedure TNetDbTest.BuildFakeResponseSOA(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_SOA;
  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_SOA(fr.answers[0],FAKEFQDN,33,
    'mn.'+FAKEFQDN,'rn.'+FAKEFQDN,76543210,
      123,456,789,60);
end;

procedure TNetDbTest.BuildFakeResponseCNAME(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_CNAME;

  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_CNAME(fr.answers[0], nm, 300, 'fakecname.'+FAKEFQDN);
end;

procedure TNetDbTest.BuildFakeResponseNS(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_NS;

  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_NS(fr.answers[0], nm, 300, 'fakens.'+FAKEFQDN);
end;

procedure TNetDbTest.BuildFakeResponsePTR(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_PTR;

  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_PTR(fr.answers[0], nm, 300, 'fakeptrans.'+FAKEFQDN);
end;

procedure TNetDbTest.BuildFakeResponseTXT(nm: ShortString; out
  fr: TFakeDNSResponse);
var
  txtarr: TTextArray;
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_TXT;

  txtarr[1] := 'v=spf1 mx a:lists.'+FAKEFQDN;
  txtarr[2] := 'Always look on the bright side of life!';
  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_TXT(fr.answers[0], nm, 300, 2, txtarr);
end;

procedure TNetDbTest.BuildFakeResponseSRV(nm: ShortString; out
  fr: TFakeDNSResponse);
begin
  // metadata
  SetLength(fr.strtable, 0);

  // start by building a fake header.
  fr.hdr.ID[0] := 12;
  fr.hdr.ID[1] := 34;
  fr.hdr.flags1 := QF_QR or QF_RD;
  fr.hdr.flags2 := 0;
  fr.hdr.qdcount := 1;
  fr.hdr.ancount := 1;
  fr.hdr.nscount := 0;
  fr.hdr.arcount := 0;

  // Next is the query part
  fr.qry.nm := nm;
  fr.qry.qclass := 1;
  fr.qry.qtype := DNSQRY_SRV;
  // now the answer RRs
  SetLength(fr.answers,1);
  BuildFakeRR_SRV(fr.answers[0],FAKEFQDN,3300,22,44,2201,'_this._that._other');
end;

{
Test that BuildPayload puts the right values into the payload buffer.
}
procedure TNetDbTest.TestBuildPayloadSimple;
var
  Q: TQueryData;
  R, I,J,el: Integer;
  S: String;
begin
  R := BuildPayLoad(Q, FAKEFQDN, DNSQRY_A, 1);
  // this is the expected length. Essentially, for each label, len(label)+1,
  // then 4 bytes for the qclass and qtype, and 1 more for a 0 byte.
  // rather than hardwire the length we calculate it so that no matter
  // what the fake domain the test passes.
  el := (Length(FAKEDOMAIN)+1)+(Length(FAKETLD)+1)+5;
  AssertEquals('Payload byte count wrong:', el, R);
  I := 0;
  J := 0;
  S := stringfromlabel(Q.Payload,I);
  AssertEquals('Wrong domain name returned:',FAKEFQDN, S);
  Move(Q.Payload[I],J,SizeOf(Word));
  AssertEquals('Wrong query type', DNSQRY_A, NToHs(J));
  Inc(I,2);
  Move(Q.Payload[I],J,SizeOf(Word));
  AssertEquals('Wrong class', 1, NToHs(J));
end;

{
Test building a payload with an empty str.
}
procedure TNetDbTest.TestBuildPayloadSimpleEmpty;
var
  Q: TQueryData;
  R: Integer;
begin
  R := BuildPayLoad(Q, '', DNSQRY_A, 1);
  AssertEquals('Payload byte count wrong:',-1, R);
end;

{
Test BuildQuery with a label that ends in a dot. This should be allowed.
A dot at the end is an empty label but we must not count its 0 byte twice.
}
procedure TNetDbTest.TestBuildPayloadSimpleEndDot;
var
  Q: TQueryData;
  R,el: Integer;
begin
  // this is the expected length. Essentially, for each label, len(label)+1,
  // then 4 bytes for the qclass and qtype, and 1 more for a 0 byte.
  // rather than hardwire the length we calculate it so that no matter
  // what the fake domain the test passes.
  el := (Length(FAKEDOMAIN)+1)+(Length(FAKETLD)+1)+5;
  R := BuildPayLoad(Q, FAKEFQDN+'.', DNSQRY_A, 1);
  AssertEquals('Payload byte count wrong:',el, R);
end;

{
Test BuildPayload with a label that starts with a dot. This should be
rejected outright.
}
procedure TNetDbTest.TestBuildPayloadSimpleStartDot;
var
  Q: TQueryData;
  R: Integer;
begin
  R := BuildPayLoad(Q, '.'+FAKEFQDN, DNSQRY_A, 1);
  AssertEquals('Payload byte count wrong:',-1, R);
end;

{
Test BuildPayload with multiple dots (empty labels) in the middle of the domain
name. This should be rejected outright.
}
procedure TNetDbTest.TestBuildPayloadSimpleMultipleDot;
var
  Q: TQueryData;
  R: Integer;
begin
  R := BuildPayLoad(Q, FAKEDOMAIN+'.....'+FAKETLD, DNSQRY_A, 1);
  AssertEquals('Payload byte count wrong:',-1, R);
end;

procedure TNetDbTest.TestBuildPayloadLabelBytes;
var
  Q: TQueryData;
  R, I, lOffset: Integer;
begin
  R := BuildPayLoad(Q, FAKEFQDN, DNSQRY_A, 1);
  AssertEquals('Payload byte count wrong:',
    Length(FAKEDOMAIN)+Length(FAKETLD)+7, R);
  AssertEquals('Length of first label', Length(FAKEDOMAIN), Q.Payload[0]);
  for I := 1 to Length(FAKEDOMAIN) do
    AssertEquals('Byte '+IntToStr(I)+' of first label',
      Ord(FAKEDOMAIN[I]), Q.Payload[I]);
  lOffset := Length(FAKEDOMAIN)+1;
  AssertEquals('Length of second label', Length(FAKETLD), Q.Payload[lOffset]);
  for I := 1 to Length(FAKETLD) do
    AssertEquals('Byte '+IntToStr(I)+' of second label',
      Ord(FAKETLD[I]), Q.Payload[lOffset+I]);
  AssertEquals('Terminating zero label', 0,
    Q.Payload[lOffset+Length(FAKETLD)+1]);
end;

procedure TNetDbTest.TestStringFromLabelBytes;
var
  PL: TPayLoad;
  PLTCP: TPayLoadTCP;
  I, lStart: Integer;
begin
  FillChar(PL, SizeOf(PL), 0);
  PL[0] := Length(FAKEDOMAIN);
  for I := 1 to Length(FAKEDOMAIN) do
    PL[I] := Ord(FAKEDOMAIN[I]);
  PL[Length(FAKEDOMAIN)+1] := Length(FAKETLD);
  for I := 1 to Length(FAKETLD) do
    PL[Length(FAKEDOMAIN)+1+I] := Ord(FAKETLD[I]);
  lStart := 0;
  AssertEquals('Name read from UDP payload', FAKEFQDN,
    stringfromlabel(PL, lStart));
  AssertEquals('Index after UDP name', Length(FAKEFQDN)+2, lStart);
  FillChar(PLTCP, SizeOf(PLTCP), 0);
  Move(PL, PLTCP, SizeOf(PL));
  lStart := 0;
  AssertEquals('Name read from TCP payload', FAKEFQDN,
    stringfromlabel(PLTCP, lStart));
  AssertEquals('Index after TCP name', Length(FAKEFQDN)+2, lStart);
end;

procedure TNetDbTest.TestDnsQueryUDP_A;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an A RR.', DNSQRY_A, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong A record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong A record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.0.1', HostAddrToStr(ip));
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[1], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.5.1', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestDnsQueryTCP_A;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an A RR.', DNSQRY_A, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong A record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong A record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.0.1', HostAddrToStr(ip));
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[1], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.5.1', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_A;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an A RR.', DNSQRY_A, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong A record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong A record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.0.1', HostAddrToStr(ip));
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[1], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.5.1', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_A;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an A RR.', DNSQRY_A, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong A record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong A record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.0.1', HostAddrToStr(ip));
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[1], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.5.1', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestDnsQueryUDP_AAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an AAAA RR.',DNSQRY_AAAA, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong AAAA record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong AAAA record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4', HostAddrToStr6(ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1], qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::92e6:baff:fe44:ffbb', HostAddrToStr6(ip));
end;

procedure TNetDbTest.TestDnsQueryTCP_AAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an AAAA RR.',DNSQRY_AAAA, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong AAAA record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong AAAA record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[0],
    qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1],
    qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::92e6:baff:fe44:ffbb',
    HostAddrToStr6(ip));
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_AAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an AAAA RR.',DNSQRY_AAAA, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong AAAA record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong AAAA record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[0],
    qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1],
    qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::92e6:baff:fe44:ffbb',
    HostAddrToStr6(ip));
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_AAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an AAAA RR.',DNSQRY_AAAA, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertEquals('Wrong AAAA record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertEquals('Wrong AAAA record name for RR 1', FAKEFQDN,
    RRArr[1].RRName);
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[0],
    qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1],
    qd.Payload, ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::92e6:baff:fe44:ffbb',
    HostAddrToStr6(ip));
end;

procedure TNetDbTest.TestDnsQueryUDP_MX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  mxrec: TDNSRR_MX;
  ip: THostAddr;
  ip6: THostAddr6;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an MX RR.',DNSQRY_MX, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, mxrec));
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEFQDN,
    mxrec.exchange);
  AssertEquals('Wrong MX preference', 10, mxrec.preference);

  AssertEquals('Should be 2 additional RR records.',2,NToHs(qd.h.arcount));
  RRArr := GetRRrecords(qd.Payload, ansstart, NToHs(qd.h.arcount));
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1], qd.Payload, ip6));

  AssertEquals('Wrong ip for A.', '172.16.27.238', HostAddrToStr(ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip6));
end;

procedure TNetDbTest.TestDnsQueryTCP_MX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  mxrec: TDNSRR_MX;
  ip: THostAddr;
  ip6: THostAddr6;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an MX RR.',DNSQRY_MX, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, mxrec));
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEFQDN,
    mxrec.exchange);
  AssertEquals('Wrong MX preference', 10, mxrec.preference);

  AssertEquals('Should be 2 additional RR records.',2,NToHs(qd.h.arcount));
  RRArr := GetRRrecords(qd.Payload, ansstart, NToHs(qd.h.arcount));
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1], qd.Payload, ip6));

  AssertEquals('Wrong ip for A.', '172.16.27.238', HostAddrToStr(ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip6));
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_MX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  mxrec: TDNSRR_MX;
  ip: THostAddr;
  ip6: THostAddr6;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an MX RR.',DNSQRY_MX, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, mxrec));
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEFQDN,
    mxrec.exchange);
  AssertEquals('Wrong MX preference', 10, mxrec.preference);

  AssertEquals('Should be 2 additional RR records.',2,NToHs(qd.h.arcount));
  RRArr := GetRRrecords(qd.Payload, ansstart, NToHs(qd.h.arcount));
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1], qd.Payload,
    ip6));

  AssertEquals('Wrong ip for A.', '172.16.27.238', HostAddrToStr(ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip6));
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_MX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  mxrec: TDNSRR_MX;
  ip: THostAddr;
  ip6: THostAddr6;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an MX RR.',DNSQRY_MX, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, mxrec));
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEFQDN,
    mxrec.exchange);
  AssertEquals('Wrong MX preference', 10, mxrec.preference);

  AssertEquals('Should be 2 additional RR records.',2,NToHs(qd.h.arcount));
  RRArr := GetRRrecords(qd.Payload, ansstart, NToHs(qd.h.arcount));
  AssertEquals('Wrong number of resource records.', 2, Length(RRArr));
  AssertEquals('RR 0 is not an A RR.',DNSQRY_A, RRarr[0].RRMeta.Atype);
  AssertEquals('RR 1 is not an AAAA RR.', DNSQRY_AAAA, RRarr[1].RRMeta.Atype);
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertTrue('Did not get RR AAAA data.', DNSRRGetAAAA(RRArr[1], qd.Payload,
    ip6));

  AssertEquals('Wrong ip for A.', '172.16.27.238', HostAddrToStr(ip));
  AssertEquals('Wrong ip for AAAA.', 'fe80::3b92:3429:ff16:a3e4',
    HostAddrToStr6(ip6));
end;

procedure TNetDbTest.TestDnsQueryUDP_SOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
  AssertEquals('Wrong mname hostname', 'mn.'+FAKEFQDN,
    soarec.mname);
  AssertEquals('Wrong rname hostname', 'rn.'+FAKEFQDN,
    soarec.rname);
  AssertEquals('Wrong SOA serial', 76543210, soarec.serial);
  AssertEquals('Wrong SOA refresh', 123, soarec.refresh);
  AssertEquals('Wrong SOA retry', 456, soarec.retry);
  AssertEquals('Wrong SOA expire', 789, soarec.expire);
  AssertEquals('Wrong SOA min', 60, soarec.min);
end;

procedure TNetDbTest.TestDnsQueryTCP_SOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
  AssertEquals('Wrong mname hostname', 'mn.'+FAKEFQDN,
    soarec.mname);
  AssertEquals('Wrong rname hostname', 'rn.'+FAKEFQDN,
    soarec.rname);
  AssertEquals('Wrong SOA serial', 76543210, soarec.serial);
  AssertEquals('Wrong SOA refresh', 123, soarec.refresh);
  AssertEquals('Wrong SOA retry', 456, soarec.retry);
  AssertEquals('Wrong SOA expire', 789, soarec.expire);
  AssertEquals('Wrong SOA min', 60, soarec.min);
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_SOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
  AssertEquals('Wrong mname hostname', 'mn.'+FAKEFQDN,
    soarec.mname);
  AssertEquals('Wrong rname hostname', 'rn.'+FAKEFQDN,
    soarec.rname);
  AssertEquals('Wrong SOA serial', 76543210, soarec.serial);
  AssertEquals('Wrong SOA refresh', 123, soarec.refresh);
  AssertEquals('Wrong SOA retry', 456, soarec.retry);
  AssertEquals('Wrong SOA expire', 789, soarec.expire);
  AssertEquals('Wrong SOA min', 60, soarec.min);
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_SOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
  AssertEquals('Wrong mname hostname', 'mn.'+FAKEFQDN,
    soarec.mname);
  AssertEquals('Wrong rname hostname', 'rn.'+FAKEFQDN,
    soarec.rname);
  AssertEquals('Wrong SOA serial', 76543210, soarec.serial);
  AssertEquals('Wrong SOA refresh', 123, soarec.refresh);
  AssertEquals('Wrong SOA retry', 456, soarec.retry);
  AssertEquals('Wrong SOA expire', 789, soarec.expire);
  AssertEquals('Wrong SOA min', 60, soarec.min);
end;

procedure TNetDbTest.TestDnsQueryUDP_CNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong CNAME record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryTCP_CNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong CNAME record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_CNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong CNAME record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_CNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong CNAME record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload,
    s));
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryUDP_NS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong NS record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong NS.', 'fakens.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryTCP_NS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong NS record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong NS.', 'fakens.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_NS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong NS record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong NS.', 'fakens.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_NS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong NS record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong NS.', 'fakens.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryUDP_PTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);
  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryTCP_PTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);
  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_PTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);
  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_PTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);
  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsQueryUDP_TXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR TXT data.', DNSRRGetText(RRArr[0], qd.Payload, s));
  AssertEquals(
    'v=spf1 mx a:lists.'+FAKEFQDN+'Always look on the bright side of life!',
      s);
end;

procedure TNetDbTest.TestDnsQueryTCP_TXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR TXT data.', DNSRRGetText(RRArr[0], qd.Payload, s));
  AssertEquals(
    'v=spf1 mx a:lists.'+FAKEFQDN+'Always look on the bright side of life!',
      s);
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_TXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR TXT data.', DNSRRGetText(RRArr[0], qd.Payload, s));
  AssertEquals(
    'v=spf1 mx a:lists.'+FAKEFQDN+'Always look on the bright side of life!',
      s);
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_TXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);
  AssertTrue('Did not get RR TXT data.', DNSRRGetText(RRArr[0], qd.Payload, s));
  AssertEquals(
    'v=spf1 mx a:lists.'+FAKEFQDN+'Always look on the bright side of life!',
      s);
end;

procedure TNetDbTest.TestDnsQueryUDP_SRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SRV data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
  AssertEquals('Wrong SRV priority', 22, srvrec.priority);
  AssertEquals('Wrong SRV weight', 44, srvrec.weight);
  AssertEquals('Wrong SRV port', 2201, srvrec.port);

  AssertEquals('Wrong SRV hostname', '_this._that._other', srvrec.target);
end;

procedure TNetDbTest.TestDnsQueryTCP_SRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
  AssertEquals('Wrong SRV priority', 22, srvrec.priority);
  AssertEquals('Wrong SRV weight', 44, srvrec.weight);
  AssertEquals('Wrong SRV port', 2201, srvrec.port);

  AssertEquals('Wrong SRV hostname', '_this._that._other', srvrec.target);
end;

procedure TNetDbTest.TestDnsQueryCompressUDP_SRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SRV data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
  AssertEquals('Wrong SRV priority', 22, srvrec.priority);
  AssertEquals('Wrong SRV weight', 44, srvrec.weight);
  AssertEquals('Wrong SRV port', 2201, srvrec.port);

  AssertEquals('Wrong SRV hostname', '_this._that._other', srvrec.target);
end;

procedure TNetDbTest.TestDnsQueryCompressTCP_SRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to compressed querydata',
    BuildQueryData(fakeresp, qd, anslen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  AssertEquals('Wrong record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
  AssertEquals('Wrong SRV priority', 22, srvrec.priority);
  AssertEquals('Wrong SRV weight', 44, srvrec.weight);
  AssertEquals('Wrong SRV port', 2201, srvrec.port);

  AssertEquals('Wrong SRV hostname', '_this._that._other', srvrec.target);
end;

{
This test is of debatable value, as it only detects truncation if the buffer
contents are zeroed which gives an invalid RR type.
}
procedure TNetDbTest.TestDnsQueryTruncateRR_UDP_A;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildTruncatedQueryData(fakeresp, qd, anslen,40));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  // the header says there are 2 A records, but it's a trap!
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  // truncation of buffer means this call returns 0 RRs.
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of RRs', 0, Length(RRArr));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // Change start position for RR[0] to end of buffer - 4
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  qd.Payload[Length(qd.Payload)-1] := $AA; // sentinel marker we can look for

  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '0.0.0.170', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // Change start position for RR[0] to end of buffer - 3
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 3);
  AssertFalse('Got RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
end;

{
Test that we read the AAAA right at the buffer edge, with the last byte
being a special value we can test for.
}
procedure TNetDbTest.TestDnsRRBufferEdgeAAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  // Change start position for RR[0]
  RRArr[0].RDataSt := Length(qd.Payload) - SizeOf(THostAddr6);
  qd.Payload[Length(qd.Payload)-1] := $AA;
  AssertTrue('Got RR AAAA data.', DNSRRGetAAAA(RRArr[0], qd.Payload, ip));
  AssertEquals($AA, ip.u6_addr8[15]);
end;

{
Attempt to read an AAAA that goes past the end of the buffer.
}
procedure TNetDbTest.TestDNsRRBufferPastEdgeAAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  // Change start position for RR[0]. attempting to read 16 bytes
  // from this position will pass the end of the buffer.
  RRArr[0].RDataSt := Length(qd.Payload) - (SizeOf(THostAddr6)-1);
  qd.Payload[Length(qd.Payload)-1] := $AA;
  AssertFalse('Got RR AAAA data.', DNSRRGetAAAA(RRArr[0], qd.Payload, ip));
end;

{
Test reading an MX RR that terminates on the last byte of the buffer.
}
procedure TNetDbTest.TestDnsRRBufferEdgeMX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  fmx: TDNSRR_MX;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // move the MX RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Got RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, fmx));
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEFQDN,
    fmx.exchange);
  AssertEquals('Wrong MX preference', 10, fmx.preference);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeMX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  fmx: TDNSRR_MX;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // move the MX RR bytes to the end of the payload buffer. We omit the last
  // 2 bytes of the MX to attempt to trick the code into reading past the buffer
  // edge.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  AssertTrue('Got RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, fmx));
  // stringfromlabel should drop the last label, so the result should be just
  // missing the tld.
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEDOMAIN,
    fmx.exchange);
  AssertEquals('Wrong MX preference', 10, fmx.preference);
end;

procedure TNetDbTest.TestDnsRRBufferEdgeSOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);

  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  // move the SOA RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
  AssertEquals('Wrong mname hostname', 'mn.'+FAKEFQDN,
    soarec.mname);
  AssertEquals('Wrong rname hostname', 'rn.'+FAKEFQDN,
    soarec.rname);
  AssertEquals('Wrong SOA serial', 76543210, soarec.serial);
  AssertEquals('Wrong SOA refresh', 123, soarec.refresh);
  AssertEquals('Wrong SOA retry', 456, soarec.retry);
  AssertEquals('Wrong SOA expire', 789, soarec.expire);
  AssertEquals('Wrong SOA min', 60, soarec.min);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeSOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);

  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  // move the SOA RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-1);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-1));

  AssertFalse('Got RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeSRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  // move the SRV RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR SRV data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
  AssertEquals('Wrong SRV priority', 22, srvrec.priority);
  AssertEquals('Wrong SRV weight', 44, srvrec.weight);
  AssertEquals('Wrong SRV port', 2201, srvrec.port);

  AssertEquals('Wrong SRV hostname', '_this._that._other', srvrec.target);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeSRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  // move the SRV RR bytes to the end of the payload buffer. ensure that
  // we're one byte short to try and trick the code into reading past the
  // end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 1);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength - 1));

  AssertFalse('Got RR SRV data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeCNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);

  // move the cname to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEFQDN, s);
end;

{
Test retrieving a cname when the actual string is longer than rdlength says it
is. The bytes in the payload buffer try to point past the end of the buffer.
}
procedure TNetDbTest.TestDnsRRBufferPastEdgeCNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);

  // move the cname to the end of the buffer. we drop two bytes off the end of
  // the cname, because there's a 0 byte at the end of a label if not a ptr.
  // now, the last label's size is greater than the number of bytes left in
  // the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  // lie about the rdlength too!
  Dec(RRArr[0].RRMeta.RDLength,2);
  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  // last label will get removed, leaving just the domain part.
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEDOMAIN, s);
end;

{
Test retrieving an NS RR when it's at the end of the payload buffer.
}
procedure TNetDbTest.TestDnsRRBufferEdgeNS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong NS record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  // move the ns to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong NS.', 'fakens.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeNS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);

  // move the ns to the end of the buffer. we drop two bytes off the end of
  // the ns, because there's a 0 byte at the end of a label if not a ptr.
  // now, the last label's size is greater than the number of bytes left in
  // the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  // lie about the rdlength too!
  Dec(RRArr[0].RRMeta.RDLength,2);
  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  // last label will get removed, leaving just the domain part.
  AssertEquals('Wrong NS.', 'fakens.'+FAKEDOMAIN, s);
end;

procedure TNetDbTest.TestDnsRRBufferEdgePTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);

  // move the ptr to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgePTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);

  // move the ns to the end of the buffer. we drop two bytes off the end of
  // the ns, because there's a 0 byte at the end of a label if not a ptr.
  // now, the last label's size is greater than the number of bytes left in
  // the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  // lie about the rdlength too!
  Dec(RRArr[0].RRMeta.RDLength,2);
  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  // last label will get removed, leaving just the domain part.
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEDOMAIN, s);
end;

{
Test reading a text record right at the edge of the payload buffer.
}
procedure TNetDbTest.TestDnsRRBufferEdgeTXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart,oldstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  // Move the text record to the end of the buffer
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);
  AssertTrue('Did not get RR TXT data.', DNSRRGetText(RRArr[0], qd.Payload, s));
  AssertEquals(
    'v=spf1 mx a:lists.'+FAKEFQDN+'Always look on the bright side of life!',
      s);
end;

{
Try reading a TXT record that points past the end of the payload buffer.
}
procedure TNetDbTest.TestDnsRRBufferPastEdgeTXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart,oldstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  // Move the text record to the end of the buffer, cutting off the last
  // 2 bytes. this means the length byte for the second string will point
  // past the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength - 2));
  AssertFalse('Did not get RR TXT data.',
    DNSRRGetText(RRArr[0], qd.Payload, s));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // Change start position for RR[0] to end of buffer - 4
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  qd.Payload[Length(qd.Payload)-1] := $AA; // sentinel marker we can look for
  AssertTrue('Did not get RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '0.0.0.170', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // Change start position for RR[0] to end of buffer - 3
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 2);
  AssertFalse('Got RR A data.', DNSRRGetA(RRArr[0], qd.Payload, ip));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPAAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  // Change start position for RR[0]
  RRArr[0].RDataSt := Length(qd.Payload) - SizeOf(THostAddr6);
  qd.Payload[Length(qd.Payload)-1] := $AA;
  AssertTrue('Got RR AAAA data.', DNSRRGetAAAA(RRArr[0], qd.Payload, ip));
  AssertEquals($AA, ip.u6_addr8[15]);
end;

procedure TNetDbTest.TestDNsRRBufferPastEdgeTCPAAAA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
  ip: THostAddr6;
begin
  BuildFakeResponseAAAA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of AAAA records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  // Change start position for RR[0]. attempting to read 16 bytes
  // from this position will pass the end of the buffer.
  RRArr[0].RDataSt := Length(qd.Payload) - (SizeOf(THostAddr6)-1);
  AssertFalse('Got RR AAAA data.', DNSRRGetAAAA(RRArr[0], qd.Payload, ip));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPMX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  fmx: TDNSRR_MX;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // move the MX RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Got RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, fmx));
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEFQDN,
    fmx.exchange);
  AssertEquals('Wrong MX preference', 10, fmx.preference);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPMX;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  fmx: TDNSRR_MX;
begin
  BuildFakeResponseMX(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of MX records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);

  // move the MX RR bytes to the end of the payload buffer. We omit the last
  // 2 bytes of the MX to attempt to trick the code into reading past the buffer
  // edge.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  AssertTrue('Got RR MX data.', DNSRRGetMX(RRArr[0], qd.Payload, fmx));
  // stringfromlabel should drop the last label, so the result should be just
  // missing the tld.
  AssertEquals('Wrong MX hostname', 'mailer.'+FAKEDOMAIN,
    fmx.exchange);
  AssertEquals('Wrong MX preference', 10, fmx.preference);
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPSOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);

  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  // move the SOA RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
  AssertEquals('Wrong mname hostname', 'mn.'+FAKEFQDN,
    soarec.mname);
  AssertEquals('Wrong rname hostname', 'rn.'+FAKEFQDN,
    soarec.rname);
  AssertEquals('Wrong SOA serial', 76543210, soarec.serial);
  AssertEquals('Wrong SOA refresh', 123, soarec.refresh);
  AssertEquals('Wrong SOA retry', 456, soarec.retry);
  AssertEquals('Wrong SOA expire', 789, soarec.expire);
  AssertEquals('Wrong SOA min', 60, soarec.min);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPSOA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  soarec: TDNSRR_SOA;
begin
  BuildFakeResponseSOA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SOA records.', 1, qd.h.ancount);

  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SOA RR.',DNSQRY_SOA, RRarr[0].RRMeta.Atype);

  // move the SOA RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-1);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-1));

  AssertFalse('Got RR SOA data.', DNSRRGetSOA(RRArr[0], qd.Payload,
    soarec));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPSRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  // move the SRV RR bytes to the end of the payload buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR SRV data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
  AssertEquals('Wrong SRV priority', 22, srvrec.priority);
  AssertEquals('Wrong SRV weight', 44, srvrec.weight);
  AssertEquals('Wrong SRV port', 2201, srvrec.port);

  AssertEquals('Wrong SRV hostname', '_this._that._other', srvrec.target);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPSRV;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  srvrec: TDNSRR_SRV;
begin
  BuildFakeResponseSRV(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of SRV records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an SRV RR.',DNSQRY_SRV, RRarr[0].RRMeta.Atype);

  // move the SRV RR bytes to the end of the payload buffer. ensure that
  // we're one byte short to try and trick the code into reading past the
  // end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 1);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength - 1));

  AssertFalse('Got RR SRV data.', DNSRRGetSRV(RRArr[0], qd.Payload,
    srvrec));
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPCNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);

  // move the cname to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPCNAME;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseCNAME(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of CNAME records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an CNAME RR.',DNSQRY_CNAME, RRarr[0].RRMeta.Atype);

  // move the cname to the end of the buffer. we drop two bytes off the end of
  // the cname, because there's a 0 byte at the end of a label if not a ptr.
  // now, the last label's size is greater than the number of bytes left in
  // the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  // lie about the rdlength too!
  Dec(RRArr[0].RRMeta.RDLength,2);
  AssertTrue('Did not get RR CNAME data.', DNSRRGetCNAME(RRArr[0], qd.Payload, s));
  // last label will get removed, leaving just the domain part.
  AssertEquals('Wrong CNAME.', 'fakecname.'+FAKEDOMAIN, s);
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPNS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong NS record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  // move the ns to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong NS.', 'fakens.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPNS;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);

  // move the ns to the end of the buffer. we drop two bytes off the end of
  // the ns, because there's a 0 byte at the end of a label if not a ptr.
  // now, the last label's size is greater than the number of bytes left in
  // the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  // lie about the rdlength too!
  Dec(RRArr[0].RRMeta.RDLength,2);
  AssertTrue('Did not get RR NS data.', DNSRRGetNS(RRArr[0], qd.Payload, s));
  // last label will get removed, leaving just the domain part.
  AssertEquals('Wrong NS.', 'fakens.'+FAKEDOMAIN, s);
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPPTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);

  // move the ptr to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEFQDN, s);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPPTR;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
begin
  // the str passed in to this function doesn't really matter, but using
  // a proper in-addr.arpa domain helps keep it clear what we're testing.
  BuildFakeResponsePTR('0.5.0.127.in-addr.arpa', fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of PTR records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an PTR RR.',DNSQRY_PTR, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong PTR record name for RR 0', '0.5.0.127.in-addr.arpa',
    RRArr[0].RRName);

  // move the ns to the end of the buffer. we drop two bytes off the end of
  // the ns, because there's a 0 byte at the end of a label if not a ptr.
  // now, the last label's size is greater than the number of bytes left in
  // the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength-2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength-2));

  // lie about the rdlength too!
  Dec(RRArr[0].RRMeta.RDLength,2);
  AssertTrue('Did not get RR PTR data.', DNSRRGetPTR(RRArr[0], qd.Payload, s));
  // last label will get removed, leaving just the domain part.
  AssertEquals('Wrong PTR.', 'fakeptrans.'+FAKEDOMAIN, s);
end;

procedure TNetDbTest.TestDnsRRBufferEdgeTCPTXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart,oldstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  // Move the text record to the end of the buffer
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);
  AssertTrue('Did not get RR TXT data.', DNSRRGetText(RRArr[0], qd.Payload, s));
  AssertEquals(
    'v=spf1 mx a:lists.'+FAKEFQDN+'Always look on the bright side of life!',
      s);
end;

procedure TNetDbTest.TestDnsRRBufferPastEdgeTCPTXT;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart,oldstart: Word;
  RRArr: TRRNameDataArray;
  s: AnsiString;
begin
  s := '';
  BuildFakeResponseTXT(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of TXT records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not a TXT RR.',DNSQRY_TXT, RRarr[0].RRMeta.Atype);
  AssertEquals('Wrong TXT record name for RR 0', FAKEFQDN,
    RRArr[0].RRName);

  // Move the text record to the end of the buffer, cutting off the last
  // 2 bytes. this means the length byte for the second string will point
  // past the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - (RRArr[0].RRMeta.RDLength - 2);
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt,
    (RRArr[0].RRMeta.RDLength - 2));
  AssertFalse('Did not get RR TXT data.',
    DNSRRGetText(RRArr[0], qd.Payload, s));
end;

{
Test that NextNameRR correctly reads an RR on the edge of the buffer.
}
procedure TNetDbTest.TestNextNameRREdgeA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  rrn: TRRNameData;
  ip: THostAddr;
  t: Cardinal;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);

  // get an RR from its normal position. need this to calculate the length.
  AssertTrue('NextNameRR should succeed.',
    NextNameRR(qd.Payload, ansstart,  rrn));

  // calculate the size in bytes of the rr so we can copy it to the end
  // of the payload buffer
  t := (rrn.RDataSt + rrn.RRMeta.RDLength) - ansstart;
  CopyBytesTo(qd.Payload,ansstart,Length(qd.Payload)-t, t);
  AssertTrue('NextNameRR should succeed.',
    NextNameRR(qd.Payload, Length(qd.Payload)-t,  rrn));
  AssertEquals(DNSQRY_A, rrn.RRMeta.Atype);
  AssertEquals(300, rrn.RRMeta.TTL);
  AssertTrue(DNSRRGetA(rrn, qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.0.1', HostAddrToStr(ip));
end;

{
Try to trick NextNameRR into reading past the end of the payload buffer.
}
procedure TNetDbTest.TestNextNameRRPastEdgeA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  rrn: TRRNameData;
  t: Cardinal;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);

  // get an RR from its normal position. need this to calculate the length.
  AssertTrue('NextNameRR should succeed.',
    NextNameRR(qd.Payload, ansstart,  rrn));

  // calculate the size in bytes of the rr so we can copy it to the end
  // of the payload buffer
  t := (rrn.RDataSt + rrn.RRMeta.RDLength) - ansstart;
  // copy the bytes, but leave off the last one. leave the rdlength unchanged.
  CopyBytesTo(qd.Payload,ansstart,Length(qd.Payload)-(t-1), t-1);
  AssertFalse('NextNameRR should fail.',
    NextNameRR(qd.Payload, Length(qd.Payload)-(t-1),  rrn));
end;

procedure TNetDbTest.TestNextNameRREdgeTCPA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  rrn: TRRNameData;
  ip: THostAddr;
  t: Cardinal;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);

  // get an RR from its normal position. need this to calculate the length.
  AssertTrue('NextNameRR should succeed.',
    NextNameRR(qd.Payload, ansstart,  rrn));

  // calculate the size in bytes of the rr so we can copy it to the end
  // of the payload buffer
  t := (rrn.RDataSt + rrn.RRMeta.RDLength) - ansstart;
  CopyBytesTo(qd.Payload,ansstart,Length(qd.Payload)-t, t);
  AssertTrue('NextNameRR should succeed.',
    NextNameRR(qd.Payload, Length(qd.Payload)-t,  rrn));
  AssertEquals(DNSQRY_A, rrn.RRMeta.Atype);
  AssertEquals(300, rrn.RRMeta.TTL);
  AssertTrue(DNSRRGetA(rrn, qd.Payload, ip));
  AssertEquals('Wrong ip for A.', '127.0.0.1', HostAddrToStr(ip));
end;

procedure TNetDbTest.TestNextNameRRPastEdgeTCPA;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  rrn: TRRNameData;
  t: Cardinal;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);

  // get an RR from its normal position. need this to calculate the length.
  AssertTrue('NextNameRR should succeed.',
    NextNameRR(qd.Payload, ansstart,  rrn));

  // calculate the size in bytes of the rr so we can copy it to the end
  // of the payload buffer
  t := (rrn.RDataSt + rrn.RRMeta.RDLength) - ansstart;
  // copy the bytes, but leave off the last one. leave the rdlength unchanged.
  CopyBytesTo(qd.Payload,ansstart,Length(qd.Payload)-(t-1), t-1);
  AssertFalse('NextNameRR should fail.',
    NextNameRR(qd.Payload, Length(qd.Payload)-(t-1),  rrn));
end;

{
Call GetRRrecords with a start position past the end of the buffer.
}

procedure TNetDbTest.TestGetRRrecordsInvalidStart;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := High(Word);
  anslen := High(Word);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, anslen);
  AssertEquals(0, Length(RRArr));
end;

procedure TNetDbTest.TestGetRRrecordsInvalidStartTCP;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart: Word;
  RRArr: TRRNameDataArray;
begin
  BuildFakeResponseA(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := High(Word);
  anslen := High(Word);
  AssertEquals('Wrong number of A records.', 2, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, anslen);
  AssertEquals(0, Length(RRArr));
end;

procedure TNetDbTest.TestGetFixLenStrSimple;
const
  s = 'another fine mess';
var
  buf: TBuffer;
  pl: TPayload;
  tr: TTextArray;
  offset: Cardinal;
  res: ShortString;
begin
  tr[1] := s;
  tr[2] := '';
  tr[3] := '';
  tr[4] := '';
  tr[5] := '';
  SetLength(buf, 1024);
  offset := 0;
  WriteTextRecAsRData(buf, offset, tr);
  SetLength(buf, offset);
  BufferToPayload(buf, pl);
  // rdlength is word, so len byte for str is at offset 2 and str starts
  // at offset 3.
  GetFixlenStr(pl, 3, pl[2], res);
  AssertEquals(s, res);
end;

procedure TNetDbTest.TestGetFixLenStrSimpleTCP;
const
  s = 'another fine mess';
var
  buf: TBuffer;
  pl: TPayLoadTCP;
  tr: TTextArray;
  offset: Cardinal;
  res: ShortString;
begin
  tr[1] := s;
  tr[2] := '';
  tr[3] := '';
  tr[4] := '';
  tr[5] := '';
  SetLength(buf, 1024);
  offset := 0;
  WriteTextRecAsRData(buf, offset, tr);
  SetLength(buf, offset);
  BufferToPayload(buf, pl);
  // rdlength is word, so len byte for str is at offset 2 and str starts
  // at offset 3.
  GetFixlenStr(pl, 3, pl[2], res);
  AssertEquals(s, res);
end;

procedure TNetDbTest.TestGetFixLenStrSimpleAtEdge;
const
  s = 'another fine mess';
var
  buf: TBuffer;
  pl: TPayload;
  tr: TTextArray;
  offset,n: Cardinal;
  res: ShortString;
begin
  tr[1] := s;
  tr[2] := '';
  tr[3] := '';
  tr[4] := '';
  tr[5] := '';
  SetLength(buf, Length(pl));
  offset := Length(pl) - (Length(s)+3);
  n := offset+2;
  WriteTextRecAsRData(buf, offset, tr);
  SetLength(buf, offset);
  BufferToPayload(buf, pl);
  GetFixlenStr(pl, n+1, pl[n], res);
  AssertEquals(s, res);
end;

procedure TNetDbTest.TestGetFixLenStrSimpleTCPAtEdge;
const
  s = 'another fine mess';
var
  buf: TBuffer;
  pl: TPayLoadTCP;
  tr: TTextArray;
  offset,n: Cardinal;
  res: ShortString;
begin
  tr[1] := s;
  tr[2] := '';
  tr[3] := '';
  tr[4] := '';
  tr[5] := '';
  SetLength(buf, Length(pl));
  offset := Length(pl) - (Length(s)+3);
  n := offset+2;
  WriteTextRecAsRData(buf, offset, tr);
  SetLength(buf, offset);
  BufferToPayload(buf, pl);
  GetFixlenStr(pl, n+1, pl[n], res);
  AssertEquals(s, res);
end;

{
Test GetFixLenStr where len would take string past edge of buffer.
}
procedure TNetDbTest.TestGetFixLenStrSimplePastEdge;
var
  pl: TPayLoadTCP;
  res: ShortString;
begin
  pl[Length(pl) - 2] := 30;
  pl[Length(pl) - 1] := Ord('a');
  GetFixlenStr(pl, Length(pl)-1, pl[Length(pl)-2], res);
  AssertEquals('', res);
end;

procedure TNetDbTest.TestGetFixLenStrSimpleTCPPastEdge;
var
  pl: TPayLoadTCP;
  res: ShortString;
begin
  pl[Length(pl) - 2] := 30;
  pl[Length(pl) - 1] := Ord('a');
  GetFixlenStr(pl, Length(pl)-1, pl[Length(pl)-2], res);
  AssertEquals('', res);
end;

{
 read a label at the end of the buffer where the last byte is a count
 greater than 0. this is to try and trick stringfromlabel into reading past
 the end of the buffer.
}
procedure TNetDbTest.TestStringFromLabelCountAsLastByte;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryData;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
  startpos: Longint;
begin
  // we can use any of CNAME, NS or PTR because these RRs are just a single
  // domain name or series of labels.
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);

  // move the ns to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  // Set the last byte in the buffer to a high count
  qd.Payload[Length(qd.Payload)-1] := 63; // must be less than 64

  // need this var because stringfromlabel expects a longint that's a var type.
  startpos := RRarr[0].RDataSt;
  s := stringfromlabel(qd.Payload, startpos);
  AssertEquals('fakens.'+FAKEFQDN, s);
  AssertEquals(Length(qd.Payload), startpos);
end;

procedure TNetDbTest.TestStringFromLabelCountAsLastByteTCP;
var
  fakeresp: TFakeDNSResponse;
  qd: TQueryDataLengthTCP;
  anslen, ansstart, oldstart: Word;
  RRArr: TRRNameDataArray;
  s: TDNSDomainName;
  startpos: Longint;
begin
  // we can use any of CNAME, NS or PTR because these RRs are just a single
  // domain name or series of labels.
  BuildFakeResponseNS(FAKEFQDN, fakeresp);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(fakeresp, qd, anslen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(qd.h,qd.h));
  ansstart := SkipAnsQueries(qd, anslen);
  AssertEquals('Wrong number of NS records.', 1, qd.h.ancount);
  RRArr := GetRRrecords(qd.Payload, ansstart, qd.h.ancount);
  AssertEquals('Wrong number of resource records.', 1, Length(RRArr));
  AssertEquals('RR 0 is not an NS RR.',DNSQRY_NS, RRarr[0].RRMeta.Atype);

  // move the ns to the end of the buffer.
  oldstart := RRArr[0].RDataSt;
  RRArr[0].RDataSt := Length(qd.Payload) - RRArr[0].RRMeta.RDLength;
  CopyBytesTo(qd.Payload, oldstart, RRArr[0].RDataSt, RRArr[0].RRMeta.RDLength);

  // Set the last byte in the buffer to a high count
  qd.Payload[Length(qd.Payload)-1] := 63; // must be less than 64

  // need this var because stringfromlabel expects a longint that's a var type.
  startpos := RRarr[0].RDataSt;
  s := stringfromlabel(qd.Payload, startpos);
  AssertEquals('fakens.'+FAKEFQDN, s);
  AssertEquals(Length(qd.Payload), startpos);
end;

procedure TNetDbTest.TestStringFromLabelCompress;
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayload;
  s: String;
  dmbs: TDNSDomainByteStream;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;
  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  offset2 := offset;
  // write same domain, this time we get compression.
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  BufferToPayload(buf,pl);
  s := stringfromlabel(pl, offset2);
  AssertEquals(FAKEFQDN,s);
end;

procedure TNetDbTest.TestStringFromLabelCompressTCP;
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayLoadTCP;
  s: String;
  dmbs: TDNSDomainByteStream;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;
  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  offset2 := offset;
  // write same domain, this time we get compression.
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  BufferToPayload(buf,pl);
  s := stringfromlabel(pl, offset2);
  AssertEquals(FAKEFQDN,s);
end;

procedure TNetDbTest.TestStringFromLabelCompressWithUncompressedLabel;
var
  buf: TBuffer;
  dmbs: TDNSDomainByteStream;
  offset: Cardinal;
  so: Longint;
  stt: TDomainCompressionTable;
  len: Word;
  pl: TPayload;
  s: String;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  // compress table empty so no compression here.
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  offset := 0;
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);

  so := offset;
  // should get compression on FAKEFQDN but label "foo" is written as full label.
  dmbs := DomainNameToByteStream('foo.' + FAKEFQDN, stt);
  len := CalcRdLength(dmbs);
  // len is 4 for 'foo' (including its length byte) and 2 for the pointer.
  AssertEquals(6, len);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  BufferToPayload(buf,pl);
  s := stringfromlabel(pl, so);
  AssertEquals('foo.'+FAKEFQDN,s);
end;

procedure TNetDbTest.TestStringFromLabelCompressWithUncompressedLabelTCP;
var
  buf: TBuffer;
  dmbs: TDNSDomainByteStream;
  offset: Cardinal;
  so: Longint;
  stt: TDomainCompressionTable;
  len: Word;
  pl: TPayLoadTCP;
  s: String;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  // compress table empty so no compression here.
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  offset := 0;
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);

  so := offset;
  // should get compression on FAKEFQDN but label "foo" is written as full label.
  dmbs := DomainNameToByteStream('foo.' + FAKEFQDN, stt);
  len := CalcRdLength(dmbs);
  // len is 4 for 'foo' (including its length byte) and 2 for the pointer.
  AssertEquals(6, len);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  BufferToPayload(buf,pl);
  s := stringfromlabel(pl, so);
  AssertEquals('foo.'+FAKEFQDN,s);
end;

{
Test stringfromlabel with a compressed label at the end of the buffer.
}
procedure TNetDbTest.TestStringFromLabelCompressEndBuffer;
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayload;
  s: String;
  dmbs: TDNSDomainByteStream;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;

  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  offset2 := offset;
  // write same domain, this time we get compression.
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);

  // write the pointer at the end of the payload buffer
  offset := Length(pl) - 2;
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  BufferToPayload(buf,pl);

  // read back the label.
  offset2 := Length(pl) - 2;
  s := stringfromlabel(pl, offset2);
  AssertEquals(FAKEFQDN,s);
end;

procedure TNetDbTest.TestStringFromLabelCompressEndBufferTCP;
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayLoadTCP;
  s: String;
  dmbs: TDNSDomainByteStream;
begin
  SetLength(buf, Length(pl));
  SetLength(stt,0);
  offset := 0;

  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  offset2 := offset;
  // write same domain, this time we get compression.
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);

  // write the pointer at the end of the payload buffer
  offset := Length(pl) - 2;
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  BufferToPayload(buf,pl);

  // read back the label.
  offset2 := Length(pl) - 2;
  s := stringfromlabel(pl, offset2);
  AssertEquals(FAKEFQDN,s);
end;

procedure TNetDbTest.TestStringFromLabelCompressSplit;
var
  pl: TPayload;
  s: String;
  offset: Longint;
begin
  // fill the buffer with 'A' so that we'll know if stringfromlabel read any
  // of it.
  FillByte(pl, Length(pl), 65);
  offset := Length(pl) - 1;
  pl[offset] := 192;
  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

procedure TNetDbTest.TestStringFromLabelCompressSplitTCP;
var
  pl: TPayLoadTCP;
  s: String;
  offset: Longint;
begin
  // fill the buffer with 'A' so that we'll know if stringfromlabel read any
  // of it.
  FillByte(pl, Length(pl), 65);
  offset := Length(pl) - 1;
  pl[offset] := 192;
  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

procedure TNetDbTest.TestStringFromLabelCompressPtrFwd;
var
  pl: TPayload;
  s: String;
  offset: Longint;
  ptr: TDNSDomainPointer;
begin
  FillByte(pl, Length(pl), 0);
  Move('foo', pl[21], 3);
  pl[20] := 3;

  ptr := GetDnsDomainPointer(32); // offset 20 + 12 for the header
  offset := 0;
  pl[offset] := ptr.b1;
  pl[offset+1] := ptr.b2;
  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

procedure TNetDbTest.TestStringFromLabelCompressPtrFwdTCP;
var
  pl: TPayLoadTCP;
  s: String;
  offset: Longint;
  ptr: TDNSDomainPointer;
begin
  FillByte(pl, Length(pl), 0);
  Move('foo', pl[21], 3);
  pl[20] := 3;

  ptr := GetDnsDomainPointer(32); // offset 20 + 12 for the header
  offset := 0;
  pl[offset] := ptr.b1;
  pl[offset+1] := ptr.b2;
  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

procedure TNetDbTest.TestStringFromLabelCompressAllPtrStart;
var
  pl: TPayload;
  s: String;
  offset: Longint;
begin
  FillByte(pl, Length(pl), 192);
  offset := 0;
  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

procedure TNetDbTest.TestStringFromLabelCompressAllPtrStartTCP;
var
  pl: TPayLoadTCP;
  s: String;
  offset: Longint;
begin
  FillByte(pl, Length(pl), 192);
  offset := 0;
  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

{
Test what happens when pointer is 0.
}
procedure TNetDbTest.TestStringFromLabelCompressedZero;
var
  pl: TPayLoad;
  s: String;
  offset: Longint;
  ptr: TDNSDomainPointer;
begin
  FillByte(pl, Length(pl), 0);
  pl[0] := 1;
  pl[1] := Ord('a');
  ptr := GetDnsDomainPointer(0);
  offset := 5;
  pl[offset] := ptr.b1;
  pl[offset+1] := ptr.b2;

  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

{
Test what happens when pointer is 0.
}
procedure TNetDbTest.TestStringFromLabelCompressedZeroTCP;
var
  pl: TPayLoadTCP;
  s: String;
  offset: Longint;
  ptr: TDNSDomainPointer;
begin
  FillByte(pl, Length(pl), 0);
  pl[0] := 1;
  pl[1] := Ord('a');
  ptr := GetDnsDomainPointer(0);
  offset := 5;
  pl[offset] := ptr.b1;
  pl[offset+1] := ptr.b2;

  s := stringfromlabel(pl, offset);
  AssertEquals('', s);
end;

procedure TNetDbTest.TestStringFromLabelInfiniteLoop;
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayload;
  s: String;
  dmbs: TDNSDomainByteStream;
  ptr: TDNSDomainPointer;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;
  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  ptr := GetDnsDomainPointer(12);

  // offset now points to 0 byte at end of label. We're overwriting that
  // 0 so that stringfromlabel will be tricked into a loop.
  Dec(offset);
  Move(ptr.ba, buf[offset], 2);

  BufferToPayload(buf,pl);
  offset2 := 0;
  s := stringfromlabel(pl, offset2);
  // if stringfromlabel returns at all then the test passed.
end;

procedure TNetDbTest.TestStringFromLabelInfiniteLoopTCP;
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayLoadTCP;
  s: String;
  dmbs: TDNSDomainByteStream;
  ptr: TDNSDomainPointer;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;
  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(FAKEFQDN, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  ptr := GetDnsDomainPointer(12);

  // offset now points to 0 byte at end of label. We're overwriting that
  // 0 so that stringfromlabel will be tricked into a loop.
  Dec(offset);
  Move(ptr.ba, buf[offset], 2);

  BufferToPayload(buf,pl);
  offset2 := 0;
  s := stringfromlabel(pl, offset2);
  // if stringfromlabel returns at all then the test passed.
end;

procedure TNetDbTest.TestCompressShortDomain;
const
  shortdomain = 'a.b';
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayload;
  s: String;
  dmbs: TDNSDomainByteStream;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;
  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(shortdomain, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  offset2 := offset;
  // second str is compressed
  dmbs := DomainNameToByteStream(shortdomain, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);

  BufferToPayload(buf,pl);
  s := stringfromlabel(pl, offset2);
  AssertEquals(shortdomain, s);
end;

procedure TNetDbTest.TestCompressShortDomainTCP;
const
  shortdomain = 'a.b';
var
  buf: TBuffer;
  stt: TDomainCompressionTable;
  offset: Cardinal;
  offset2: Longint;
  pl: TPayLoadTCP;
  s: String;
  dmbs: TDNSDomainByteStream;
begin
  SetLength(buf, 1024);
  SetLength(stt,0);
  offset := 0;
  // initial str is uncompressed because compress table empty
  dmbs := DomainNameToByteStream(shortdomain, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);
  offset2 := offset;
  // second str is compressed
  dmbs := DomainNameToByteStream(shortdomain, stt);
  WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs);

  BufferToPayload(buf,pl);
  s := stringfromlabel(pl, offset2);
  AssertEquals(shortdomain, s);
end;

procedure TNetDbTest.SetUp;
begin
  tsl := TStringList.Create;
end;

procedure TNetDbTest.TearDown;
begin
  tsl.Free;
end;

procedure TNetDbTest.CopyBytesTo(var buf: TPayLoad; startidx, destidx,
  count: Word);
begin
  // no tests for overlapping source and dest.
  if ((startidx+count) > Length(buf)) or ((destidx+count) > Length(buf)) then
    exit;
  Move(buf[startidx], buf[destidx], count);
end;

procedure TNetDbTest.CopyBytesTo(var buf: TPayLoadTCP; startidx, destidx,
  count: Word);
begin
  // no tests for overlapping source and dest.
  if ((startidx+count) > Length(buf)) or ((destidx+count) > Length(buf)) then
    exit;
  Move(buf[startidx], buf[destidx], count);
end;

function TNetDbTest.WriteNumToBuffer(var buf: TBuffer; var offset: Cardinal;
  val: Word): Word;
begin
  Result := 0;
  if (offset + SizeOf(val)) > Length(buf) then exit;
  Move(HToNs(val), buf[offset], SizeOf(val));
  Inc(offset, SizeOf(val));
  Result := SizeOf(val);
end;

function TNetDbTest.WriteNumToBuffer(var buf: TBuffer; var offset: Cardinal;
  val: Cardinal): Word;
begin
  Result := 0;
  if (offset + SizeOf(val)) > Length(buf) then exit;
  Move(HToNl(val), buf[offset], SizeOf(val));
  Inc(offset, SizeOf(val));
  Result := SizeOf(val);
end;

{
Write a number to the buffer without converting it to network byte order.
}
function TNetDbTest.WriteNumToBufferN(var buf: TBuffer; var offset: Cardinal;
  val: Word): Word;
begin
  Result := 0;
  if (offset + SizeOf(val)) > Length(buf) then exit;
  Move(val, buf[offset], SizeOf(val));
  Inc(offset, SizeOf(val));
  Result := SizeOf(val);
end;

{
Write a number to the buffer without converting it to network byte order.
}
function TNetDbTest.WriteNumToBufferN(var buf: TBuffer; var offset: Cardinal;
  val: Cardinal): Word;
begin
  Result := 0;
  if (offset + SizeOf(val)) > Length(buf) then exit;
  Move(val, buf[offset], SizeOf(val));
  Inc(offset, SizeOf(val));
  Result := SizeOf(val);
end;

{
Write an RR to the byte buffer. No compression of domain names will occur.
}
function TNetDbTest.WriteRRToBuffer(var buf: TBuffer; var offset: Cardinal;
  rr: TFakeRR): Word;
var
  s,etw: Word;
  dmbs: TDNSDomainByteStream;
  res: TRDataWriteRes;
begin
  etw := 0;
  s := offset;
  // write the RR Name
  dmbs := DomainNameToByteStream(rr.RRName);
  etw := CalcRdLength(dmbs);
  if WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs) < etw
  then
    Fail('Cannot write RR name to buffer at offset '+ inttostr(offset));

  if (offset + SizeOf(rr.Atype) + SizeOf(rr.AClass) + SizeOf(rr.TTL)) >
    Length(buf)
  then
    Fail('Not enough space to add RR type,class,ttl at offset '+ inttostr(offset));

  // Write the RR type, class and TTL.
  WriteNumToBuffer(buf, offset,rr.Atype);
  WriteNumToBuffer(buf, offset, rr.AClass);
  WriteNumToBuffer(buf, offset, rr.TTL);

  // now the RR data, which is type specific. Each type-specific method
  // also writes the RDLength word, so we have to account for 2 additional
  // bytes.
  case rr.Atype of
    DNSQRY_A:
        res := WriteAasRData(buf, offset, rr.ip);
    DNSQRY_AAAA:
        res := WriteAAAAasRData(buf, offset, rr.ip6);
    DNSQRY_SOA:
        res := WriteSOAasRData(buf, offset, rr.fsoa);
    DNSQRY_MX:
        res := WriteMXAsRData(buf, offset, rr.fmx);
    DNSQRY_NS:
      begin
        dmbs := DomainNameToByteStream(rr.nsh);
        res := WriteDomainAsRdata(buf,offset,dmbs);
      end;
    DNSQRY_PTR:
      begin
        dmbs := DomainNameToByteStream(rr.nsh);
        res := WriteDomainAsRdata(buf,offset,dmbs);
      end;
    DNSQRY_CNAME:
      begin
        dmbs := DomainNameToByteStream(rr.cn);
        res := WriteDomainAsRdata(buf,offset,dmbs);
      end;
    DNSQRY_TXT:
        res := WriteTextRecAsRData(buf, offset, rr.txtarr);
    DNSQRY_SRV:
        res := WriteSRVasRData(buf, offset, rr.fsrv);
  else
    Fail('Called to handle RR type '+inttostr(rr.Atype)+
      ' but no code to handle it.');
  end;

  if res.bw < res.etw then
    Fail('Unable to write RR of type ' +inttostr(RR.Atype) +
      ', name "'  + rr.RRName + '" to buffer at offset '+inttostr(offset)+
      '. Wrote '+inttostr(res.bw)+' bytes, expected to write '+
        inttostr(res.etw)+' bytes.');

  Result := offset - s;
end;

{
Write an RR to the output buffer, with compression of domain names turned on.
}
function TNetDbTest.WriteRRToBuffer(var buf: TBuffer; var offset: Cardinal;
  rr: TFakeRR; var ctbl: TDomainCompressionTable): Word;
var
  s, etw: Word;
  dmbs: TDNSDomainByteStream;
  res: TRDataWriteRes;
begin
  etw := 0;
  s := offset;
  // write the RR Name
  dmbs := DomainNameToByteStream(rr.RRName, ctbl);
  etw := CalcRdLength(dmbs);
  if WriteDNSDomainByteStreamToBuffer(buf, offset, dmbs) < etw
  then
    Fail('Cannot write RR name to buffer at offset '+ inttostr(offset));

  if (offset + SizeOf(rr.Atype) + SizeOf(rr.AClass) + SizeOf(rr.TTL)) >
    Length(buf)
  then
    Fail('Not enough space to add RR type,class,ttl at offset '+ inttostr(offset));

  // Write the RR type, class and TTL.
  WriteNumToBuffer(buf, offset,rr.Atype);
  WriteNumToBuffer(buf, offset, rr.AClass);
  WriteNumToBuffer(buf, offset, rr.TTL);

  // now the RR data, which is type specific. Each type-specific method
  // also writes the RDLength word, so we have to account for 2 additional
  // bytes.
  case rr.Atype of
    DNSQRY_A:
      begin
        res := WriteAasRData(buf, offset, rr.ip);
      end;
    DNSQRY_AAAA:
      begin
        res := WriteAAAAasRData(buf, offset, rr.ip6);
      end;
    DNSQRY_SOA:
      begin
        res := WriteSOAasRData(buf, offset, rr.fsoa);
      end;
    DNSQRY_MX:
      begin
        res := WriteMXAsRData(buf, offset, rr.fmx, ctbl);
      end;
    DNSQRY_NS:
      begin
        dmbs := DomainNameToByteStream(rr.nsh, ctbl);
        res := WriteDomainAsRdata(buf,offset,dmbs);
      end;
    DNSQRY_PTR:
      begin
        dmbs := DomainNameToByteStream(rr.nsh, ctbl);
        res := WriteDomainAsRdata(buf,offset,dmbs);
      end;
    DNSQRY_CNAME:
      begin
        dmbs := DomainNameToByteStream(rr.cn, ctbl);
        res := WriteDomainAsRdata(buf,offset,dmbs);
      end;
    DNSQRY_TXT:
      begin
        res := WriteTextRecAsRData(buf, offset, rr.txtarr);
      end;
    DNSQRY_SRV:
      begin
        res := WriteSRVasRData(buf, offset, rr.fsrv);
      end;
  else
    Fail('Called to handle RR type '+inttostr(rr.Atype)+
      ' but no code to handle it.');
  end;

  if res.bw < res.etw then
    Fail('Unable to write RR of type ' +inttostr(RR.Atype) +
      ', name "'  + rr.RRName + '" to buffer at offset '+inttostr(offset)+
      '. Wrote '+inttostr(res.bw)+' bytes, expected to write '+
        inttostr(res.etw)+' bytes.');

  Result := offset - s;
end;

{
Turn a fake DNS response into a payload buffer. This is a byte buffer minus the
DNS header. That is, the buffer begins with the question part of the response,
after which comes the RRs of the answers, authority, and additional sections.
}
function TNetDbTest.FakeDNSResponseToByteBuffer(fdr: TFakeDNSResponse; out
  buf: TBuffer; compress: Boolean): Cardinal;
var
  offset: Cardinal;
  rr: TFakeRR;
  dbs: TDNSDomainByteStream;
begin
  // plenty of room for our test responses. could precalculate this, but there's
  // no benefit. The return value of this function is the length of the
  // DNS reply, which we get for free since we have to track our offset into
  // the buffer as we write it.
  SetLength(buf, 2048);
  offset := 0;

  if compress then
    dbs := DomainNameToByteStream(fdr.qry.nm,fdr.strtable)
  else
    dbs := DomainNameToByteStream(fdr.qry.nm);

  // The question section consists of the dns query name, the qtype and
  // qclass.
  if WriteDNSDomainByteStreamToBuffer(buf, offset, dbs) < CalcRdLength(dbs)
  then
    Fail('Cannot write name to buffer at offset '+ inttostr(offset));

  WriteNumToBuffer(buf, offset, fdr.qry.qtype);
  WriteNumToBuffer(buf, offset, fdr.qry.qclass);

  // Now the answer sections.
  for rr in fdr.answers do
    if compress then
      WriteRRToBuffer(buf, offset, rr, fdr.strtable)
    else
      WriteRRToBuffer(buf, offset, rr);
  for rr in fdr.authority do
    if compress then
      WriteRRToBuffer(buf, offset, rr, fdr.strtable)
    else
      WriteRRToBuffer(buf, offset, rr);
  for rr in fdr.additional do
    if compress then
      WriteRRToBuffer(buf, offset, rr, fdr.strtable)
    else
      WriteRRToBuffer(buf, offset, rr);

  SetLength(buf, offset);
  Result := offset;
end;

{
Generate a TPayload buffer, a fixed-length array of byte, from the TBuffer
type, which is a variable-length array of byte.
}
function TNetDbTest.BufferToPayload(const buf: TBuffer;
  out pl: TPayload): Boolean;
begin
  Result := False;
  FillChar(pl,Length(pl),0);
  Move(buf[0], pl[0], Min(Length(pl),Length(buf)));
  Result := True;
end;

function TNetDbTest.BufferToPayload(const buf: TBuffer;
  out pl: TPayLoadTCP): Boolean;
begin
  Result := False;
  FillChar(pl,Length(pl),0);
  Move(buf[0], pl[0], Min(Length(pl),Length(buf)));
  Result := True;
end;

function TNetDbTest.BuildQueryData(fdr: TFakeDNSResponse; out qd: TQueryData;
  out qlen: Word; Compress: Boolean = False): Boolean;
var
  buf: TBuffer;
begin
  qlen := FakeDNSResponseToByteBuffer(fdr, buf, Compress);
  qd.h.ancount := HToNs(fdr.hdr.ancount);
  qd.h.arcount := HToNs(fdr.hdr.arcount);
  qd.h.nscount := HToNs(fdr.hdr.nscount);
  qd.h.qdcount := HToNs(fdr.hdr.qdcount);
  qd.h.flags1 := fdr.hdr.flags1;
  qd.h.flags2 := fdr.hdr.flags2;
  qd.h.id[0] := fdr.hdr.id[0];
  qd.h.id[1] := fdr.hdr.id[1];
  Result := BufferToPayload(buf, qd.Payload);
end;

function TNetDbTest.BuildQueryData(fdr: TFakeDNSResponse; out
  qd: TQueryDataLengthTCP; out qlen: Word; Compress: Boolean = False): Boolean;
var
  buf: TBuffer;
begin
  qlen := FakeDNSResponseToByteBuffer(fdr, buf, Compress);
  qd.h.ancount := HToNs(fdr.hdr.ancount);
  qd.h.arcount := HToNs(fdr.hdr.arcount);
  qd.h.nscount := HToNs(fdr.hdr.nscount);
  qd.h.qdcount := HToNs(fdr.hdr.qdcount);
  qd.h.flags1 := fdr.hdr.flags1;
  qd.h.flags2 := fdr.hdr.flags2;
  qd.h.id[0] := fdr.hdr.id[0];
  qd.h.id[1] := fdr.hdr.id[1];
  Result := BufferToPayload(buf, qd.Payload);
end;

{
Create a deliberately invalid DNS response to test our API's ability to cope
with invalid data without causing memory corruption.

After building a valid DNS response as normal, we truncate it at the given
offset.}
function TNetDbTest.BuildTruncatedQueryData(fdr: TFakeDNSResponse; out
  qd: TQueryData; out qlen: Word; truncoffset: Word): Boolean;
var
  buf: TBuffer;
begin
  qlen := FakeDNSResponseToByteBuffer(fdr, buf);
  qd.h.ancount := HToNs(fdr.hdr.ancount);
  qd.h.arcount := HToNs(fdr.hdr.arcount);
  qd.h.nscount := HToNs(fdr.hdr.nscount);
  qd.h.qdcount := HToNs(fdr.hdr.qdcount);
  qd.h.flags1 := fdr.hdr.flags1;
  qd.h.flags2 := fdr.hdr.flags2;
  qd.h.id[0] := fdr.hdr.id[0];
  qd.h.id[1] := fdr.hdr.id[1];
  SetLength(buf, truncoffset);
  Result := BufferToPayload(buf, qd.Payload);
end;

const
  cAnswerQuestion : array[0..6] of Byte = (1,Ord('a'),0, 0,1, 0,1);
  cAnswerA : array[0..15] of Byte = ($C0,12, 0,1, 0,1, 0,0,0,0, 0,4, 127,0,0,1);
  cAnswerA2 : array[0..15] of Byte = ($C0,12, 0,1, 0,1, 0,0,0,0, 0,4, 127,0,5,1);


// Fill aQD with a response for aAnCount answers whose payload is a question for "a" followed by aAnswer.
procedure BuildRawAnswer(out aQD: TQueryData; aAnCount: Word; const aAnswer: array of Byte;
  out aAnsLen: Integer);

begin
  aQD:=Default(TQueryData);
  aQD.h.flags1:=QF_QR;
  aQD.h.qdcount:=HToNs(1);
  aQD.h.ancount:=aAnCount;
  Move(cAnswerQuestion,aQD.Payload[0],Length(cAnswerQuestion));
  if Length(aAnswer)>0 then
    Move(aAnswer[0],aQD.Payload[Length(cAnswerQuestion)],Length(aAnswer));
  aAnsLen:=Length(cAnswerQuestion)+Length(aAnswer);
end;


// Concatenate two byte arrays.
function JoinBytes(const aFirst, aSecond: array of Byte): TBytes;

begin
  Result:=Default(TBytes);
  SetLength(Result,Length(aFirst)+Length(aSecond));
  Move(aFirst[0],Result[0],Length(aFirst));
  Move(aSecond[0],Result[Length(aFirst)],Length(aSecond));
end;


procedure TNetDbTest.TestExtractAnswerAddresses;

var
  lFake: TFakeDNSResponse;
  lQD: TQueryData;
  lAnsLen: Word;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildFakeResponseA(FAKEFQDN, lFake);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(lFake, lQD, lAnsLen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(lQD.h,lQD.h));
  AssertEquals('Number of addresses', 2, ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
  AssertEquals('No CNAME', '', lCName);
  AssertEquals('First address in network order', '127.0.0.1', NetAddrToStr(lAddr[0]));
  AssertEquals('Second address in network order', '127.0.5.1', NetAddrToStr(lAddr[1]));
end;


procedure TNetDbTest.TestExtractAnswerAddressesCompressed;

var
  lFake: TFakeDNSResponse;
  lQD: TQueryData;
  lAnsLen: Word;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildFakeResponseA(FAKEFQDN, lFake);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(lFake, lQD, lAnsLen, True));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(lQD.h,lQD.h));
  AssertEquals('Number of addresses', 2, ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
  AssertEquals('First address', '127.0.0.1', NetAddrToStr(lAddr[0]));
  AssertEquals('Second address', '127.0.5.1', NetAddrToStr(lAddr[1]));
end;


procedure TNetDbTest.TestExtractAnswerAddresses6;

var
  lFake: TFakeDNSResponse;
  lQD: TQueryData;
  lAnsLen: Word;
  lAddr: array[0..4] of THostAddr6;
  lExpected: THostAddr6;
  lCName: String;

begin
  BuildFakeResponseAAAA(FAKEFQDN, lFake);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(lFake, lQD, lAnsLen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(lQD.h,lQD.h));
  AssertEquals('Number of addresses', 2, ExtractAnswerAddresses6(lQD, lAnsLen, lAddr, lCName));
  AssertEquals('No CNAME', '', lCName);
  lExpected:=StrToNetAddr6('fe80::3b92:3429:ff16:a3e4');
  AssertTrue('First address', CompareMem(@lAddr[0], @lExpected, SizeOf(THostAddr6)));
  lExpected:=StrToNetAddr6('fe80::92e6:baff:fe44:ffbb');
  AssertTrue('Second address', CompareMem(@lAddr[1], @lExpected, SizeOf(THostAddr6)));
end;


procedure TNetDbTest.TestExtractAnswerNames;

var
  lFake: TFakeDNSResponse;
  lQD: TQueryData;
  lAnsLen: Word;
  lNames: array[0..4] of String;
  lCName: String;

begin
  BuildFakeResponsePTR(FAKEFQDN, lFake);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(lFake, lQD, lAnsLen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(lQD.h,lQD.h));
  AssertEquals('Number of names', 1, ExtractAnswerNames(lQD, lAnsLen, lNames, lCName));
  AssertEquals('No CNAME', '', lCName);
  AssertEquals('PTR name', 'fakeptrans.'+FAKEFQDN, lNames[0]);
end;


procedure TNetDbTest.TestExtractAnswerCName;

var
  lFake: TFakeDNSResponse;
  lQD: TQueryData;
  lAnsLen: Word;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildFakeResponseCNAME(FAKEFQDN, lFake);
  AssertTrue('Unable to convert fake dns response to querydata',
    BuildQueryData(lFake, lQD, lAnsLen));
  AssertTrue('CheckAnswer should return true.', CheckAnswer(lQD.h,lQD.h));
  AssertEquals('Number of addresses', 0, ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
  AssertEquals('CNAME target', 'fakecname.'+FAKEFQDN, lCName);
end;


procedure TNetDbTest.TestExtractAnswerAddressesArrayFull;

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..0] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 2, JoinBytes(cAnswerA, cAnswerA2), lAnsLen);
  AssertEquals('Only as many addresses as the array holds', 1,
    ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
  AssertEquals('First address', '127.0.0.1', NetAddrToStr(lAddr[0]));
end;


procedure TNetDbTest.TestExtractAnswerAddressesAnCountTooHigh;

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 5, cAnswerA, lAnsLen);
  AssertEquals('Records missing from the payload are not returned', 1,
    ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
end;


procedure TNetDbTest.TestExtractAnswerAddressesSkipsBadRDLength;

const
  cShortA : array[0..14] of Byte = ($C0,12, 0,1, 0,1, 0,0,0,0, 0,3, 10,0,0);

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 2, JoinBytes(cShortA, cAnswerA), lAnsLen);
  AssertEquals('A record with RDLength 3 is skipped', 1,
    ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
  AssertEquals('Record after the bad one', '127.0.0.1', NetAddrToStr(lAddr[0]));
end;


procedure TNetDbTest.TestExtractAnswerAddressesRDLengthPastEnd;

const
  cLongA : array[0..15] of Byte = ($C0,12, 0,1, 0,1, 0,0,0,0, $FF,$FF, 127,0,0,1);

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 2, JoinBytes(cLongA, cAnswerA), lAnsLen);
  AssertEquals('RDLength 65535 ends parsing', 0,
    ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
end;


procedure TNetDbTest.TestExtractAnswerAddressesRecordPastAnsLen;

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 1, cAnswerA, lAnsLen);
  AssertEquals('Record ending one byte past the answer length is rejected', 0,
    ExtractAnswerAddresses(lQD, lAnsLen-1, lAddr, lCName));
end;


procedure TNetDbTest.TestExtractAnswerAddressesLabelPastEnd;

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 1, [63], lAnsLen);
  AssertEquals('Owner label longer than the answer', 0,
    ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
end;


procedure TNetDbTest.TestExtractAnswerAddressesPointerByteAtEnd;

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  BuildRawAnswer(lQD, 1, [$FF], lAnsLen);
  AssertEquals('Pointer byte as the last answer byte', 0,
    ExtractAnswerAddresses(lQD, lAnsLen, lAddr, lCName));
end;


procedure TNetDbTest.TestExtractAnswerNamesPointerLoop;

const
  cLoopPTR : array[0..15] of Byte = ($C0,12, 0,12, 0,1, 0,0,0,0, 0,4, 1,Ord('x'), $C0,31);

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lNames: array[0..4] of String;
  lCName: String;

begin
  BuildRawAnswer(lQD, 1, cLoopPTR, lAnsLen);
  AssertEquals('PTR whose name points back at its own label', 1,
    ExtractAnswerNames(lQD, lAnsLen, lNames, lCName));
  AssertTrue('Looping name is cut off at 255 characters', Length(lNames[0])<=255);
  AssertEquals('Looping name starts with the label', 'x.x.', Copy(lNames[0],1,4));
end;


procedure TNetDbTest.TestExtractAnswerNamesSelfPointer;

const
  cSelfPTR : array[0..13] of Byte = ($C0,12, 0,12, 0,1, 0,0,0,0, 0,2, $C0,31);

var
  lQD: TQueryData;
  lAnsLen: Integer;
  lNames: array[0..4] of String;
  lCName: String;

begin
  BuildRawAnswer(lQD, 1, cSelfPTR, lAnsLen);
  AssertEquals('PTR whose name is a pointer to itself', 1,
    ExtractAnswerNames(lQD, lAnsLen, lNames, lCName));
  AssertEquals('Self pointer gives an empty name', '', lNames[0]);
end;


// Fill aQry with a query for example.test and aAns with its matching response.
procedure BuildQueryPair(out aQry, aAns: TQueryData; out aQryLen: Integer);

begin
  aQry:=Default(TQueryData);
  aQryLen:=BuildPayLoad(aQry,'example.test',DNSQRY_A,1);
  aQry.h.ID[0]:=$12;
  aQry.h.ID[1]:=$34;
  aQry.h.Flags1:=QF_RD;
  aQry.h.qdcount:=HToNs(1);
  aAns:=aQry;
  aAns.h.Flags1:=QF_QR or QF_RD;
end;


type
  TFakeReplyKind = (frGood, frWrongID, frOtherName, frOtherSource, frNXDomain);

  // UDP server on 127.0.0.1 that answers one query with a list of replies.
  TFakeDNSServer = class(TThread)
  private
    FSock: TSocket;
    FOtherSock: TSocket;
    FPort: Word;
    FReplies: array of TFakeReplyKind;
    procedure SendReply(aKind: TFakeReplyKind; const aQry: TQueryData; aQryLen: Integer;
      const aTo: TInetSockAddr);
  protected
    procedure Execute; override;
  public
    // Bind 127.0.0.1 for the server and 127.0.0.2 for frOtherSource, then start.
    constructor Create(const aReplies: array of TFakeReplyKind);
    destructor Destroy; override;
    // Port the server listens on, in host order.
    property Port: Word read FPort;
  end;


// Open a UDP socket bound to aAddress on a free port.
function BindUDP(const aAddress: String): TSocket;

var
  lAddr: TInetSockAddr;

begin
  Result:=fpSocket(AF_INET,SOCK_DGRAM,0);
  if Result<0 then
    raise Exception.Create('Cannot create UDP socket');
  lAddr:=Default(TInetSockAddr);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr(aAddress);
  if fpBind(Result,@lAddr,SizeOf(lAddr))<>0 then
    raise Exception.CreateFmt('Cannot bind UDP socket to %s',[aAddress]);
end;


constructor TFakeDNSServer.Create(const aReplies: array of TFakeReplyKind);

var
  lAddr: TInetSockAddr;
  lLen: TSockLen;
  i: Integer;

begin
  inherited Create(True);
  FSock:=-1;
  FOtherSock:=-1;
  SetLength(FReplies,Length(aReplies));
  for i:=0 to Length(aReplies)-1 do
    FReplies[i]:=aReplies[i];
  FSock:=BindUDP('127.0.0.1');
  FOtherSock:=BindUDP('127.0.0.2');
  lAddr:=Default(TInetSockAddr);
  lLen:=SizeOf(lAddr);
  fpGetSockName(FSock,@lAddr,@lLen);
  FPort:=NToHs(lAddr.sin_port);
  Start;
end;


destructor TFakeDNSServer.Destroy;

begin
  if FSock>=0 then
    CloseSocket(FSock);
  if FOtherSock>=0 then
    CloseSocket(FOtherSock);
  inherited Destroy;
end;


procedure TFakeDNSServer.SendReply(aKind: TFakeReplyKind; const aQry: TQueryData;
  aQryLen: Integer; const aTo: TInetSockAddr);

const
  cRecord : array[0..11] of Byte = ($C0,12, 0,1, 0,1, 0,0,0,0, 0,4);
  cGood : array[0..3] of Byte = (127,0,0,1);
  cBad : array[0..3] of Byte = (10,6,6,6);

var
  lAns: TQueryData;
  lLen: Integer;
  lSock: TSocket;

begin
  lAns:=aQry;
  lAns.h.Flags1:=lAns.h.Flags1 or QF_QR;
  lLen:=aQryLen;
  lSock:=FSock;
  if aKind=frNXDomain then
    lAns.h.Flags2:=3
  else
    begin
    lAns.h.ancount:=HToNs(1);
    Move(cRecord,lAns.Payload[lLen],SizeOf(cRecord));
    Inc(lLen,SizeOf(cRecord));
    if aKind=frGood then
      Move(cGood,lAns.Payload[lLen],4)
    else
      Move(cBad,lAns.Payload[lLen],4);
    Inc(lLen,4);
    end;
  case aKind of
    frWrongID: lAns.h.ID[0]:=lAns.h.ID[0] xor $FF;
    frOtherName: lAns.Payload[1]:=lAns.Payload[1] xor 1;
    frOtherSource: lSock:=FOtherSock;
  else
  end;
  fpSendTo(lSock,@lAns,lLen+SizeOf(lAns.h),0,@aTo,SizeOf(aTo));
end;


procedure TFakeDNSServer.Execute;

var
  lQry: TQueryData;
  lFrom: TInetSockAddr;
  lFromLen: TSockLen;
  lLen: Integer;
  lFDS: TFDSet;
  lKind: TFakeReplyKind;

begin
  fpFD_ZERO(lFDS);
  fpFD_SET(FSock,lFDS);
  if fpSelect(FSock+1,@lFDS,nil,nil,3000)<=0 then
    Exit;
  lQry:=Default(TQueryData);
  lFrom:=Default(TInetSockAddr);
  lFromLen:=SizeOf(lFrom);
  lLen:=fpRecvFrom(FSock,@lQry,SizeOf(lQry),0,@lFrom,@lFromLen);
  if lLen<SizeOf(lQry.h) then
    Exit;
  for lKind in FReplies do
    SendReply(lKind,lQry,lLen-SizeOf(lQry.h),lFrom);
end;


// Send a query for example.test with Query to a TFakeDNSServer that sends aReplies.
function RunFakeQuery(const aReplies: array of TFakeReplyKind; aTimeOutMS: Integer;
  out aAns: TQueryData; out aAnsLen: Integer): Boolean;

var
  lServer: TFakeDNSServer;
  lQry: TQueryData;
  lQryLen: Integer;
  lServers: TDNSServerArray;
  lPort: Word;
  lTimeOutS, lTimeOutMS: Longint;

begin
  aAns:=Default(TQueryData);
  aAnsLen:=-1;
  lQry:=Default(TQueryData);
  lQryLen:=BuildPayLoad(lQry,'example.test',DNSQRY_A,1);
  lServers:=Copy(DNSServers);
  lPort:=DNSServerPort;
  lTimeOutS:=TimeOutS;
  lTimeOutMS:=TimeOutMS;
  lServer:=TFakeDNSServer.Create(aReplies);
  try
    SetLength(DNSServers,1);
    DNSServers[0]:=StrToNetAddr('127.0.0.1');
    DNSServerPort:=lServer.Port;
    TimeOutS:=0;
    TimeOutMS:=aTimeOutMS;
    Result:=Query(0,lQry,aAns,lQryLen,aAnsLen);
  finally
    DNSServers:=lServers;
    DNSServerPort:=lPort;
    TimeOutS:=lTimeOutS;
    TimeOutMS:=lTimeOutMS;
    lServer.WaitFor;
    lServer.Free;
  end;
end;


// Return the first A record address of a response returned by Query.
function FirstAddress(var aAns: TQueryData; aAnsLen: Integer): String;

var
  lAddr: array[0..4] of THostAddr;
  lCName: String;

begin
  if ExtractAnswerAddresses(aAns,aAnsLen,lAddr,lCName)>0 then
    Result:=NetAddrToStr(lAddr[0])
  else
    Result:='';
end;


procedure TNetDbTest.TestRandomQueryIDIgnoresRandSeed;

var
  lSeed: Cardinal;
  lFirst, lSecond: array[0..1] of Word;

begin
  lSeed:=RandSeed;
  try
    RandSeed:=42;
    lFirst[0]:=RandomQueryID;
    lFirst[1]:=RandomQueryID;
    RandSeed:=42;
    lSecond[0]:=RandomQueryID;
    lSecond[1]:=RandomQueryID;
  finally
    RandSeed:=lSeed;
  end;
  AssertTrue('Query IDs do not repeat after resetting RandSeed',
    (lFirst[0]<>lSecond[0]) or (lFirst[1]<>lSecond[1]));
end;


procedure TNetDbTest.TestRandomQueryIDSpread;

var
  lSeen: array of Boolean;
  i, lDistinct: Integer;
  lID: Word;

begin
  lSeen:=nil;
  SetLength(lSeen,65536);
  lDistinct:=0;
  for i:=1 to 64 do
    begin
    lID:=RandomQueryID;
    if not lSeen[lID] then
      Inc(lDistinct);
    lSeen[lID]:=True;
    end;
  AssertTrue('64 query IDs have at least 60 distinct values', lDistinct>=60);
end;


procedure TNetDbTest.TestIsAnswerTo;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  AssertTrue('Response with the same ID and question', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestIsAnswerToCaseInsensitive;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.Payload[1]:=Ord('E');
  AssertTrue('Question name differing only in case', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestIsAnswerToWrongID;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.h.ID[1]:=lAns.h.ID[1] xor 1;
  AssertFalse('Response with another ID', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestIsAnswerToNotResponse;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.h.Flags1:=QF_RD;
  AssertFalse('Packet without the response flag', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestIsAnswerToOtherName;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.Payload[1]:=Ord('f');
  AssertFalse('Response for another name', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestIsAnswerToOtherType;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.Payload[lQryLen-3]:=DNSQRY_AAAA;
  AssertFalse('Response for another record type', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestIsAnswerToShort;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  AssertFalse('Response shorter than the question', IsAnswerTo(lQry,lQryLen,lAns,lQryLen-1));
end;


procedure TNetDbTest.TestIsAnswerToErrorWithoutQuestion;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.h.qdcount:=0;
  lAns.h.Flags2:=1;
  AssertTrue('Error response without a question section', IsAnswerTo(lQry,lQryLen,lAns,0));
end;


procedure TNetDbTest.TestIsAnswerToNoQuestionNoError;

var
  lQry, lAns: TQueryData;
  lQryLen: Integer;

begin
  BuildQueryPair(lQry,lAns,lQryLen);
  lAns.h.qdcount:=0;
  AssertFalse('Response without a question section or error code', IsAnswerTo(lQry,lQryLen,lAns,lQryLen));
end;


procedure TNetDbTest.TestQueryGoodReply;

var
  lAns: TQueryData;
  lAnsLen: Integer;

begin
  AssertTrue('Query succeeds', RunFakeQuery([frGood],2000,lAns,lAnsLen));
  AssertEquals('Address from the reply', '127.0.0.1', FirstAddress(lAns,lAnsLen));
end;


procedure TNetDbTest.TestQuerySkipsWrongID;

var
  lAns: TQueryData;
  lAnsLen: Integer;

begin
  AssertTrue('Query succeeds after a reply with another ID',
    RunFakeQuery([frWrongID,frGood],2000,lAns,lAnsLen));
  AssertEquals('Address from the reply with the right ID', '127.0.0.1', FirstAddress(lAns,lAnsLen));
end;


procedure TNetDbTest.TestQuerySkipsOtherQuestion;

var
  lAns: TQueryData;
  lAnsLen: Integer;

begin
  AssertTrue('Query succeeds after a reply for another name',
    RunFakeQuery([frOtherName,frGood],2000,lAns,lAnsLen));
  AssertEquals('Address from the reply for the queried name', '127.0.0.1', FirstAddress(lAns,lAnsLen));
end;


procedure TNetDbTest.TestQueryIgnoresOtherSource;

var
  lAns: TQueryData;
  lAnsLen: Integer;

begin
  AssertTrue('Query succeeds after a reply from another address',
    RunFakeQuery([frOtherSource,frGood],2000,lAns,lAnsLen));
  AssertEquals('Address from the resolver, not from 127.0.0.2', '127.0.0.1', FirstAddress(lAns,lAnsLen));
end;


procedure TNetDbTest.TestQueryErrorReply;

var
  lAns: TQueryData;
  lAnsLen: Integer;

begin
  AssertFalse('NXDOMAIN reply fails the query', RunFakeQuery([frNXDomain],2000,lAns,lAnsLen));
  AssertTrue('AnsLen shows that the server responded', lAnsLen>=0);
end;


procedure TNetDbTest.TestQueryOnlyWrongReplies;

var
  lAns: TQueryData;
  lAnsLen: Integer;
  lStart: QWord;

begin
  lStart:=GetTickCount64;
  AssertFalse('Query fails when no reply matches',
    RunFakeQuery([frWrongID,frOtherName,frOtherSource],300,lAns,lAnsLen));
  AssertEquals('AnsLen shows that no response was accepted', -1, lAnsLen);
  AssertTrue('Wrong replies do not end the wait before the timeout', GetTickCount64-lStart>=250);
end;


procedure TNetDbTest.TestQueryTimeout;

var
  lAns: TQueryData;
  lAnsLen: Integer;

begin
  AssertFalse('Query fails without a reply', RunFakeQuery([],300,lAns,lAnsLen));
  AssertEquals('AnsLen shows that no response was accepted', -1, lAnsLen);
end;

initialization

  RegisterTest(TNetDbTest);
end.

