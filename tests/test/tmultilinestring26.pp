program tmultilinestring26;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 5}

const middleSpaceBug: array[0..5] of string = (
#$41` `#$42   //<--- Original bug, now fixed: "this becomes #$41#$42 instead of #$41#$20#$42"
,
#$41` - `#$42 //<--- Original bug, now fixed: "this becomes #$41#$2D#$20#$42 instead of #$41#$20#$2D#$20#$42"
,
#$41`      -      `#$42  //<--- Original bug, now fixed: "one space left instead of six spaces"
,
#$41`      -      `#$42`  --  `  //<---- Original bug, now fixed: "the same bug twice"
,
'  '#$41` `#$42   //<--- Original bug, now fixed: "the space between backticks disappears: #$20#$20#$41#$42"
,
^T' e s'` t`  //<--- Original bug, now fixed: "last two: $73$74 instead of $73$20$74"
);

var S: String;

begin
  for S in middleSpaceBug do WriteLn(S);
end.
