{$ifdef FPUSOFT}
{$define SMALL_TEST}
{$endif FPUSOFT}

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
	NHistogramBuckets = 80;
	NRows = 16;
{$ifdef SMALL_TEST}
	NSamples = 100 * 1000;
{$else SMALL_TEST}
	NSamples = 1 * 1000 * 1000;
{$endif SMALL_TEST}
	Perfect: array[0 .. NRows - 1, 0 .. NHistogramBuckets - 1] of char =
	(
		'                                     #######                                    ',
		'                                   ###########                                  ',
		'                                  #############                                 ',
		'                                #################                               ',
		'                               ###################                              ',
		'                             #######################                            ',
		'                            #########################                           ',
		'                           ###########################                          ',
		'                          #############################                         ',
		'                        #################################                       ',
		'                       ###################################                      ',
		'                     #######################################                    ',
		'                    #########################################                   ',
		'                 ###############################################                ',
		'              #####################################################             ',
		'         ###############################################################        '
	);
var
	hist: array of uint32;
	iSample, nOutOfRange, maxInBucket: uint32;
	iHist, y, imperfections: SizeInt;
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
{$ifdef SMALL_TEST}
		if iSample and (1 shl 11 - 1) = 0 then
{$else SMALL_TEST}
		if iSample and (1 shl 21 - 1) = 0 then
{$endif SMALL_TEST}
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

	imperfections := 0;
	SetLength(row, NHistogramBuckets);
	for y := 0 to NRows - 1 do
	begin
		for iHist := 0 to NHistogramBuckets - 1 do
			if (y = 0) and (iHist < length(name)) then
				pChar(pointer(row))[iHist] := name[iHist]
			else
			begin
				pChar(pointer(row))[iHist] := pChar(' #')[ord(hist[iHist] / maxInBucket >= (NRows - y - 0.5) / NRows)];
				if row[iHist] <> Perfect[y, iHist] then
					if (y > 0) and (y + 1 < NRows) and (row[iHist] <> Perfect[y - 1, iHist]) and (row[iHist] <> Perfect[y + 1, iHist])
{$ifdef SMALL_TEST}
						and
						(
							// Allow 2-storey imperfections for 25 columns in the middle.
							(abs(iHist - round((Mean - HistogramMin) / (HistogramMax - HistogramMin) * NHistogramBuckets)) > 12) or
							(y > 1) and (y + 2 < NRows) and (row[iHist] <> Perfect[y - 2, iHist]) and (row[iHist] <> Perfect[y + 2, iHist])
						)
{$endif}
					then
					begin
						pChar(pointer(row))[iHist] := '!';
						imperfections := High(imperfections) div 2;
					end else
					begin
						pChar(pointer(row))[iHist] := '*';
						imperfections += 1;
					end;
			end;
		writeln(row);
	end;
	writeln('Out of range: ', nOutOfRange, ' / ', NSamples, ' (', nOutOfRange / nSamples * 100:0:1, '%).', LineEnding,
		'Took ', time:0:1, ' s.', LineEnding);
	if nOutOfRange / nSamples>0.001 {$ifdef SMALL_TEST} +0.0003 {$endif} then
	  halt(1);
	if imperfections > {$ifdef SMALL_TEST} 40 {$else} 16 {$endif} then
	  halt(1);
end;

begin
	Demo('Math.RandG', @math.randg);
end.
