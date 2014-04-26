{$IFNDEF CPUX64} 
  {$IFNDEF CPUX86}
    {$DEFINE CPUX86}
  {$ENDIF}
{$ENDIF}

{$IFDEF DEBUG}
{$R+} // Range check On
{$ENDIF}

{$DEFINE UseInline}
{$DEFINE BuildThreadSafe}

{$IF CompilerVersion >17}
{$DEFINE CanInline}
{$IFEND}

{$IF DEFINED(UseInline) and DEFINED(CanInline)}
{$DEFINE MustInline}
{$IFEND}
