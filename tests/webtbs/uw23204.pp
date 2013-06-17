unit uw23204;

//{$mode Delphi}{$H+} // error disappears!
{$mode objfpc}{$H+}

interface

type
//  TPColor = (pc1, pc2);
  TPType = (pt_0, pt_1);
  TP = record
//    AColor: TPColor;
    AType: TPType;
  end;
  TPs = set of TPType;

implementation
end.
