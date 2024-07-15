{$mode objfpc} {$longstrings on} {$coperators on} {$zerobasedstrings on}
uses
	SysUtils, Math;

type
	RandgFunc = function(mean,stddev : float) : float;

procedure Demo(const name: string; randgf: RandgFunc);
const
	Mean = 5;
	StdDev = 1.5;
	HistogramMin = 0;
	HistogramMax = 10;
	NHistogramBuckets = 40;
	NRows = 12;
	NSamples = 100 * 1000 * 1000;
var
	hist: array of uint32;
	iSample, nOutOfRange, maxInBucket: uint32;
	iHist, y: SizeInt;
	row, msg, newMsg: string;
	time: double;
begin
	RandSeed := 1;
	nOutOfRange := 0;
	maxInBucket := 0;
	SetLength(hist, NHistogramBuckets);
	msg := '';
	time := Now;
	for iSample := 0 to NSamples - 1 do
	begin
		if iSample and (1 shl 21 - 1) = 0 then
		begin
			WriteStr(newMsg, name, ': ', iSample / NSamples * 100:0:1, '%');
			write(stderr, #13, StringOfChar(' ', length(msg)), #13, newMsg);
			msg := newMsg;
		end;
		iHist := round(EnsureRange((randgf(Mean, StdDev) - HistogramMin) / (HistogramMax - HistogramMin) * NHistogramBuckets, -1, NHistogramBuckets));
		if (iHist >= 0) and (iHist < NHistogramBuckets) then
		begin
			hist[iHist] += 1;
			if hist[iHist] > maxInBucket then
				maxInBucket := hist[iHist];
		end else
			nOutOfRange += 1;
	end;
	time := (Now - time) * SecsPerDay;
	write(stderr, #13, StringOfChar(' ', length(msg)), #13);

	SetLength(row, NHistogramBuckets);
	for y := 0 to NRows - 1 do
	begin
		for iHist := 0 to NHistogramBuckets - 1 do
			if (y = 0) and (iHist < length(name)) then
				pChar(pointer(row))[iHist] := name[iHist]
			else
				pChar(pointer(row))[iHist] := pChar(' #')[ord(hist[iHist] / maxInBucket >= (NRows - y - 0.5) / NRows)];
		writeln(row);
	end;
	writeln('Out of range: ', nOutOfRange, ' / ', NSamples, ' (', nOutOfRange / nSamples * 100:0:1, '%).', LineEnding,
		'Took ', time:0:1, ' s.', LineEnding);
	if nOutOfRange / nSamples>0.001 then
	  halt(1);
end;

begin
	Demo('Math.RandG', @math.randg);
end.
