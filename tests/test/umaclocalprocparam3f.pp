{$modeswitch nestedprocvars}

unit umaclocalprocparam3f;

interface

type
  tnestedprocvar = procedure is nested;
  tnestedprocvar2 = procedure(pp: tnestedprocvar) is nested;

implementation

end.
