{ Parsed from GLKit.framework GLKNamedEffect.h }


{$ifdef TYPES}
type
  GLKNamedEffectProtocolPtr = ^GLKNamedEffectProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  GLKNamedEffectProtocol = objcprotocol external name 'GLKNamedEffect'
  required
    procedure prepareToDraw; message 'prepareToDraw';
  end;
{$endif}

