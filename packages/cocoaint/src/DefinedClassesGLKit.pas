{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesGLKit;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  GLKBaseEffect = objcclass external;
  GLKEffectProperty = objcclass external;
  GLKEffectPropertyFog = objcclass external;
  GLKEffectPropertyLight = objcclass external;
  GLKEffectPropertyMaterial = objcclass external;
  GLKEffectPropertyTexture = objcclass external;
  GLKEffectPropertyTransform = objcclass external;
  GLKReflectionMapEffect = objcclass external;
  GLKSkyboxEffect = objcclass external;
  GLKTextureInfo = objcclass external;
  GLKTextureLoader = objcclass external;
  GLKNamedEffectProtocol = objcprotocol external name 'GLKNamedEffect';

implementation
end.
