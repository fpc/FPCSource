{ Parsed from GLKit.framework GLKEffectProperty.h }


{$ifdef TYPES}
type
  GLKEffectPropertyPtr = ^GLKEffectProperty;
{$endif}

{$ifdef TYPES}
type
  GLKEffectPropertyPrvPtr = OpaqueType;
{$endif}

{$ifdef CLASSES}

type
  GLKEffectProperty = objcclass external (NSObject)
  private
    _location: GLint;
    _nameString: GLcharPtr;
    _prv: GLKEffectPropertyPrvPtr;
  end;
{$endif}

