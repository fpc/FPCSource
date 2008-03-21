program tester;

{$mode delphi}

type TXMLElemKind = (
 elErrorFrm,
 elInvolutiveness,
 elIrreflexivity,
 elIs,
 elIt,
 elIterEquality,
 elIterStep,
 elJustifiedProperty,
 elJustifiedTheorem,
 elLambdaVar,
 elLet,
 elLocusVar,
 elMonomial,
 elNot,
 elPoweredVar,
 elPred,
 elPredInstance,
 elPriority,
 elPrivFunc,
 elPrivPred,
 elProjectivity,
 elProof,
 elTakeAsVar,
 elTheorem,
 elTheorems,
 elThesis,
 elThesisExpansions,
 elTransitivity,
 elTyp,
 elUnexpectedProp,
 elUniqueness,
 elUnknownCorrCond,
 elVar,
 elVerum
 );

const TermElKinds = [
 elVar
 ];

const FrmElKinds = [
 elErrorFrm,
 elIs,
 elNot,
 elPred,
 elPrivPred,
 elVerum
 ];

var a:TXMLElemKind;

begin
  a:=elVerum;
  if not(a in (FrmElKinds + TermElKinds)) then
    halt(1);
end.
