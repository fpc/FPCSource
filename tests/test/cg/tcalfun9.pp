program TestManyParams;

{
  Test: Can a procedure accept more than 256 parameters in Free Pascal?

  Background:
    Some Pascal compilers historically limited the number of formal
    parameters a routine could declare (an 8-bit count => max 255/256).
    This program declares a procedure with 300 integer parameters,
    calls it with 300 distinct arguments, and verifies that every
    argument arrives intact and in the correct position.

  How to read the result:
    - If the program does not COMPILE, the compiler rejects > 256 params.
      (Look for an error such as "Too many parameters" / overflow.)
    - If it compiles and prints "PASS", more than 256 parameters are
      supported and are passed correctly.
    - If it compiles but prints "FAIL", parameters are accepted but
      some values were corrupted/misaligned.
}

{$mode objfpc}{$H+}

const
  PARAM_COUNT = 300;   { deliberately > 256 }

var
  GlobalChecksum : Int64;
  GlobalMismatch : Integer;

{ The procedure under test: 300 separate value parameters.
  Each parameter pNNN is expected to equal its own index number. }
procedure TakesManyParams(
  p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
  p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
  p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,
  p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,
  p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,
  p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,
  p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,
  p71,p72,p73,p74,p75,p76,p77,p78,p79,p80,
  p81,p82,p83,p84,p85,p86,p87,p88,p89,p90,
  p91,p92,p93,p94,p95,p96,p97,p98,p99,p100,
  p101,p102,p103,p104,p105,p106,p107,p108,p109,p110,
  p111,p112,p113,p114,p115,p116,p117,p118,p119,p120,
  p121,p122,p123,p124,p125,p126,p127,p128,p129,p130,
  p131,p132,p133,p134,p135,p136,p137,p138,p139,p140,
  p141,p142,p143,p144,p145,p146,p147,p148,p149,p150,
  p151,p152,p153,p154,p155,p156,p157,p158,p159,p160,
  p161,p162,p163,p164,p165,p166,p167,p168,p169,p170,
  p171,p172,p173,p174,p175,p176,p177,p178,p179,p180,
  p181,p182,p183,p184,p185,p186,p187,p188,p189,p190,
  p191,p192,p193,p194,p195,p196,p197,p198,p199,p200,
  p201,p202,p203,p204,p205,p206,p207,p208,p209,p210,
  p211,p212,p213,p214,p215,p216,p217,p218,p219,p220,
  p221,p222,p223,p224,p225,p226,p227,p228,p229,p230,
  p231,p232,p233,p234,p235,p236,p237,p238,p239,p240,
  p241,p242,p243,p244,p245,p246,p247,p248,p249,p250,
  p251,p252,p253,p254,p255,p256,p257,p258,p259,p260,
  p261,p262,p263,p264,p265,p266,p267,p268,p269,p270,
  p271,p272,p273,p274,p275,p276,p277,p278,p279,p280,
  p281,p282,p283,p284,p285,p286,p287,p288,p289,p290,
  p291,p292,p293,p294,p295,p296,p297,p298,p299,p300 : Integer);

  { Helper: verify one parameter equals its expected index. }
  procedure Check(value, expected: Integer);
  begin
    GlobalChecksum := GlobalChecksum + value;
    if value <> expected then
    begin
      Inc(GlobalMismatch);
      WriteLn('  mismatch: param #', expected, ' got ', value);
    end;
  end;

begin
  GlobalChecksum := 0;
  GlobalMismatch := 0;

  Check(p1,1);     Check(p2,2);     Check(p3,3);     Check(p4,4);
  Check(p5,5);     Check(p6,6);     Check(p7,7);     Check(p8,8);
  Check(p9,9);     Check(p10,10);   Check(p11,11);   Check(p12,12);
  Check(p13,13);   Check(p14,14);   Check(p15,15);   Check(p16,16);
  Check(p17,17);   Check(p18,18);   Check(p19,19);   Check(p20,20);
  Check(p21,21);   Check(p22,22);   Check(p23,23);   Check(p24,24);
  Check(p25,25);   Check(p26,26);   Check(p27,27);   Check(p28,28);
  Check(p29,29);   Check(p30,30);   Check(p31,31);   Check(p32,32);
  Check(p33,33);   Check(p34,34);   Check(p35,35);   Check(p36,36);
  Check(p37,37);   Check(p38,38);   Check(p39,39);   Check(p40,40);
  Check(p41,41);   Check(p42,42);   Check(p43,43);   Check(p44,44);
  Check(p45,45);   Check(p46,46);   Check(p47,47);   Check(p48,48);
  Check(p49,49);   Check(p50,50);   Check(p51,51);   Check(p52,52);
  Check(p53,53);   Check(p54,54);   Check(p55,55);   Check(p56,56);
  Check(p57,57);   Check(p58,58);   Check(p59,59);   Check(p60,60);
  Check(p61,61);   Check(p62,62);   Check(p63,63);   Check(p64,64);
  Check(p65,65);   Check(p66,66);   Check(p67,67);   Check(p68,68);
  Check(p69,69);   Check(p70,70);   Check(p71,71);   Check(p72,72);
  Check(p73,73);   Check(p74,74);   Check(p75,75);   Check(p76,76);
  Check(p77,77);   Check(p78,78);   Check(p79,79);   Check(p80,80);
  Check(p81,81);   Check(p82,82);   Check(p83,83);   Check(p84,84);
  Check(p85,85);   Check(p86,86);   Check(p87,87);   Check(p88,88);
  Check(p89,89);   Check(p90,90);   Check(p91,91);   Check(p92,92);
  Check(p93,93);   Check(p94,94);   Check(p95,95);   Check(p96,96);
  Check(p97,97);   Check(p98,98);   Check(p99,99);   Check(p100,100);
  Check(p101,101); Check(p102,102); Check(p103,103); Check(p104,104);
  Check(p105,105); Check(p106,106); Check(p107,107); Check(p108,108);
  Check(p109,109); Check(p110,110); Check(p111,111); Check(p112,112);
  Check(p113,113); Check(p114,114); Check(p115,115); Check(p116,116);
  Check(p117,117); Check(p118,118); Check(p119,119); Check(p120,120);
  Check(p121,121); Check(p122,122); Check(p123,123); Check(p124,124);
  Check(p125,125); Check(p126,126); Check(p127,127); Check(p128,128);
  Check(p129,129); Check(p130,130); Check(p131,131); Check(p132,132);
  Check(p133,133); Check(p134,134); Check(p135,135); Check(p136,136);
  Check(p137,137); Check(p138,138); Check(p139,139); Check(p140,140);
  Check(p141,141); Check(p142,142); Check(p143,143); Check(p144,144);
  Check(p145,145); Check(p146,146); Check(p147,147); Check(p148,148);
  Check(p149,149); Check(p150,150); Check(p151,151); Check(p152,152);
  Check(p153,153); Check(p154,154); Check(p155,155); Check(p156,156);
  Check(p157,157); Check(p158,158); Check(p159,159); Check(p160,160);
  Check(p161,161); Check(p162,162); Check(p163,163); Check(p164,164);
  Check(p165,165); Check(p166,166); Check(p167,167); Check(p168,168);
  Check(p169,169); Check(p170,170); Check(p171,171); Check(p172,172);
  Check(p173,173); Check(p174,174); Check(p175,175); Check(p176,176);
  Check(p177,177); Check(p178,178); Check(p179,179); Check(p180,180);
  Check(p181,181); Check(p182,182); Check(p183,183); Check(p184,184);
  Check(p185,185); Check(p186,186); Check(p187,187); Check(p188,188);
  Check(p189,189); Check(p190,190); Check(p191,191); Check(p192,192);
  Check(p193,193); Check(p194,194); Check(p195,195); Check(p196,196);
  Check(p197,197); Check(p198,198); Check(p199,199); Check(p200,200);
  Check(p201,201); Check(p202,202); Check(p203,203); Check(p204,204);
  Check(p205,205); Check(p206,206); Check(p207,207); Check(p208,208);
  Check(p209,209); Check(p210,210); Check(p211,211); Check(p212,212);
  Check(p213,213); Check(p214,214); Check(p215,215); Check(p216,216);
  Check(p217,217); Check(p218,218); Check(p219,219); Check(p220,220);
  Check(p221,221); Check(p222,222); Check(p223,223); Check(p224,224);
  Check(p225,225); Check(p226,226); Check(p227,227); Check(p228,228);
  Check(p229,229); Check(p230,230); Check(p231,231); Check(p232,232);
  Check(p233,233); Check(p234,234); Check(p235,235); Check(p236,236);
  Check(p237,237); Check(p238,238); Check(p239,239); Check(p240,240);
  Check(p241,241); Check(p242,242); Check(p243,243); Check(p244,244);
  Check(p245,245); Check(p246,246); Check(p247,247); Check(p248,248);
  Check(p249,249); Check(p250,250); Check(p251,251); Check(p252,252);
  Check(p253,253); Check(p254,254); Check(p255,255); Check(p256,256);
  Check(p257,257); Check(p258,258); Check(p259,259); Check(p260,260);
  Check(p261,261); Check(p262,262); Check(p263,263); Check(p264,264);
  Check(p265,265); Check(p266,266); Check(p267,267); Check(p268,268);
  Check(p269,269); Check(p270,270); Check(p271,271); Check(p272,272);
  Check(p273,273); Check(p274,274); Check(p275,275); Check(p276,276);
  Check(p277,277); Check(p278,278); Check(p279,279); Check(p280,280);
  Check(p281,281); Check(p282,282); Check(p283,283); Check(p284,284);
  Check(p285,285); Check(p286,286); Check(p287,287); Check(p288,288);
  Check(p289,289); Check(p290,290); Check(p291,291); Check(p292,292);
  Check(p293,293); Check(p294,294); Check(p295,295); Check(p296,296);
  Check(p297,297); Check(p298,298); Check(p299,299); Check(p300,300);
end;

var
  ExpectedChecksum : Int64;
  i : Integer;

begin
  WriteLn('Free Pascal: > 256 parameters test');
  WriteLn('Declared parameter count: ', PARAM_COUNT);
  WriteLn;

  { Sum 1..300 = n*(n+1)/2 = 45150 }
  ExpectedChecksum := 0;
  for i := 1 to PARAM_COUNT do
    ExpectedChecksum := ExpectedChecksum + i;

  { Call with 300 literal arguments, each equal to its position. }
  TakesManyParams(
    1,2,3,4,5,6,7,8,9,10,
    11,12,13,14,15,16,17,18,19,20,
    21,22,23,24,25,26,27,28,29,30,
    31,32,33,34,35,36,37,38,39,40,
    41,42,43,44,45,46,47,48,49,50,
    51,52,53,54,55,56,57,58,59,60,
    61,62,63,64,65,66,67,68,69,70,
    71,72,73,74,75,76,77,78,79,80,
    81,82,83,84,85,86,87,88,89,90,
    91,92,93,94,95,96,97,98,99,100,
    101,102,103,104,105,106,107,108,109,110,
    111,112,113,114,115,116,117,118,119,120,
    121,122,123,124,125,126,127,128,129,130,
    131,132,133,134,135,136,137,138,139,140,
    141,142,143,144,145,146,147,148,149,150,
    151,152,153,154,155,156,157,158,159,160,
    161,162,163,164,165,166,167,168,169,170,
    171,172,173,174,175,176,177,178,179,180,
    181,182,183,184,185,186,187,188,189,190,
    191,192,193,194,195,196,197,198,199,200,
    201,202,203,204,205,206,207,208,209,210,
    211,212,213,214,215,216,217,218,219,220,
    221,222,223,224,225,226,227,228,229,230,
    231,232,233,234,235,236,237,238,239,240,
    241,242,243,244,245,246,247,248,249,250,
    251,252,253,254,255,256,257,258,259,260,
    261,262,263,264,265,266,267,268,269,270,
    271,272,273,274,275,276,277,278,279,280,
    281,282,283,284,285,286,287,288,289,290,
    291,292,293,294,295,296,297,298,299,300);

  WriteLn('Expected checksum (sum 1..', PARAM_COUNT, '): ', ExpectedChecksum);
  WriteLn('Actual   checksum             : ', GlobalChecksum);
  WriteLn('Mismatched parameters         : ', GlobalMismatch);
  WriteLn;

  if (GlobalMismatch = 0) and (GlobalChecksum = ExpectedChecksum) then
  begin
    WriteLn('RESULT: PASS - more than 256 parameters passed correctly.');
    Halt(0);
  end
  else
  begin
    WriteLn('RESULT: FAIL - parameters were accepted but corrupted.');
    Halt(1);
  end;
end.
