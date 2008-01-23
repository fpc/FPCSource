#!/bin/sh

ppudump=ppudump

for f in $*; do
  $ppudump -vim $f | awk "
BEGIN { first=1; }
/^Source/ {
  if (first==1)
  {
    printf(\"T:=P.Targets.AddUnit('%s');\\n  with T.Dependencies do\\n    begin\\n\",\$5);
    first=0;
  }
  else
  {
    printf(\"      AddInclude('%s');\\n\",\$5);
  }
}
/^Uses unit/ {
  printf(\"      AddUnit('%s');\\n\",tolower(\$3));
}
END { if (first==0) { printf(\"    end;\\n\"); } }
"
done
