unit Colorizer.uxThemeHelper;

interface

uses
 Windows,
 Graphics,
 Themes,
 UxTheme;


{$IF CompilerVersion >= 23}
{$ELSE}
 type
  TTextFormats = (tfBottom, tfCalcRect, tfCenter, tfEditControl, tfEndEllipsis,
    tfPathEllipsis, tfExpandTabs, tfExternalLeading, tfLeft, tfModifyString,
    tfNoClip, tfNoPrefix, tfRight, tfRtlReading, tfSingleLine, tfTop,
    tfVerticalCenter, tfWordBreak, tfHidePrefix, tfNoFullWidthCharBreak,
    tfPrefixOnly, tfTabStop, tfWordEllipsis, tfComposited);

   TTextFormat = set of TTextFormats;

   TDrawTextFlags = Cardinal;
   TTextFormatFlags = record
   private
     FValue: TTextFormat;
   public
     class operator Implicit(Value: TTextFormat): TTextFormatFlags;
     class operator Implicit(Value: TTextFormatFlags): TTextFormat;
     class operator Implicit(Value: TDrawTextFlags): TTextFormatFlags;
     class operator Implicit(Value: TTextFormatFlags): TDrawTextFlags;
   end;

  TStyleTextFlag = (stfTextColor, stfBorderColor, stfBorderSize,
    stfShadowColor, stfShadowOffset, stfGlowSize);
  TStyleTextFlags = set of TStyleTextFlag;

  TStyleTextOptions = record
    Flags: TStyleTextFlags;
    TextColor: TColor;
    BorderColor: TColor;
    BorderSize: Integer;
    ShadowColor: TColor;
    ShadowOffset: TPoint;
    GlowSize: Integer;
  end;

  TThemeServicesHelper = class helper for TThemeServices
  public
   function DrawText(DC: HDC; Details: TThemedElementDetails; const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean;
   function DoDrawText(DC: HDC; Details: TThemedElementDetails;  const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean;
  end;

{$IFEND}

implementation

uses
 Sysutils;

{$IFDEF DELPHIXE2_UP}

{$ELSE}
const
  DT_NOFULLWIDTHCHARBREAK = $0080000;
  MASK_TF_COMPOSITED      = $00800000;

{ TTextFormatFlags }

class operator TTextFormatFlags.Implicit(Value: TTextFormatFlags): TTextFormat;
begin
  Result := Value.FValue;
end;

class operator TTextFormatFlags.Implicit(Value: TTextFormat): TTextFormatFlags;
begin
  Result.FValue := Value;
end;

class operator TTextFormatFlags.Implicit(
  Value: TTextFormatFlags): TDrawTextFlags;
const
  CFlags: array[TTextFormats] of Cardinal = (
    DT_BOTTOM, DT_CALCRECT, DT_CENTER, DT_EDITCONTROL, DT_END_ELLIPSIS,
    DT_PATH_ELLIPSIS, DT_EXPANDTABS, DT_EXTERNALLEADING, DT_LEFT,
    DT_MODIFYSTRING, DT_NOCLIP, DT_NOPREFIX, DT_RIGHT, DT_RTLREADING,
    DT_SINGLELINE, DT_TOP, DT_VCENTER, DT_WORDBREAK, DT_HIDEPREFIX,
    DT_NOFULLWIDTHCHARBREAK, DT_PREFIXONLY, DT_TABSTOP, DT_WORD_ELLIPSIS,
    MASK_TF_COMPOSITED {tfComposited});
var
  LDrawTextFlag: TTextFormats;
begin
  Result := 0;
  for LDrawTextFlag := Low(TTextFormats) to High(TTextFormats) do
    if (LDrawTextFlag in Value.FValue) then
      Result := Result or CFlags[LDrawTextFlag];
end;

class operator TTextFormatFlags.Implicit(
  Value: TDrawTextFlags): TTextFormatFlags;
begin
  Result.FValue := [];
  if (Value and DT_BOTTOM) = DT_BOTTOM then
    Include(Result.FValue, tfBottom);
  if (Value and DT_CALCRECT) = DT_CALCRECT then
    Include(Result.FValue, tfCalcRect);
  if (Value and DT_CENTER) = DT_CENTER then
    Include(Result.FValue, tfCenter);
  if (Value and DT_EDITCONTROL) = DT_EDITCONTROL then
    Include(Result.FValue, tfEditControl);
  if (Value and DT_END_ELLIPSIS) = DT_END_ELLIPSIS then
    Include(Result.FValue, tfEndEllipsis);
  if (Value and DT_PATH_ELLIPSIS) = DT_PATH_ELLIPSIS then
    Include(Result.FValue, tfPathEllipsis);
  if (Value and DT_EXPANDTABS) = DT_EXPANDTABS then
    Include(Result.FValue, tfExpandTabs);
  if (Value and DT_EXTERNALLEADING) = DT_EXTERNALLEADING then
    Include(Result.FValue, tfExternalLeading);
  if (Value and DT_LEFT) = DT_LEFT then
    Include(Result.FValue, tfLeft);
  if (Value and DT_MODIFYSTRING) = DT_MODIFYSTRING then
    Include(Result.FValue, tfModifyString);
  if (Value and DT_NOCLIP) = DT_NOCLIP then
    Include(Result.FValue, tfNoClip);
  if (Value and DT_NOPREFIX) = DT_NOPREFIX then
    Include(Result.FValue, tfNoPrefix);
  if (Value and DT_RIGHT) = DT_RIGHT then
    Include(Result.FValue, tfRight);
  if (Value and DT_RTLREADING) = DT_RTLREADING then
    Include(Result.FValue, tfRtlReading);
  if (Value and DT_SINGLELINE) = DT_SINGLELINE then
    Include(Result.FValue, tfSingleLine);
  if (Value and DT_TOP) = DT_TOP then
    Include(Result.FValue, tfTop);
  if (Value and DT_VCENTER) = DT_VCENTER then
    Include(Result.FValue, tfVerticalCenter);
  if (Value and DT_WORDBREAK) = DT_WORDBREAK then
    Include(Result.FValue, tfWordBreak);
  if (Value and DT_HIDEPREFIX) = DT_HIDEPREFIX then
    Include(Result.FValue, tfHidePrefix);
  if (Value and DT_NOFULLWIDTHCHARBREAK) = DT_NOFULLWIDTHCHARBREAK then
    Include(Result.FValue, tfNoFullWidthCharBreak);
  if (Value and DT_PREFIXONLY) = DT_PREFIXONLY then
    Include(Result.FValue, tfPrefixOnly);
  if (Value and DT_TABSTOP) = DT_TABSTOP then
    Include(Result.FValue, tfTabStop);
  if (Value and DT_WORD_ELLIPSIS) = DT_WORD_ELLIPSIS then
    Include(Result.FValue, tfWordEllipsis);
  if (Value and MASK_TF_COMPOSITED) = MASK_TF_COMPOSITED then
    Include(Result.FValue, tfComposited);
end;


function TThemeServicesHelper.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean;
var
  LOptions: TStyleTextOptions;
begin
  if Color <> clNone then
  begin
    LOptions.Flags := [stfTextColor];
    LOptions.TextColor := Color;
  end
  else
    LOptions.Flags := [];
  Result := DoDrawText(DC, Details, S, R, Flags, LOptions);
end;

function TThemeServicesHelper.DoDrawText(DC: HDC; Details: TThemedElementDetails;
  const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean;
const
  COptions: array[TStyleTextFlag] of Cardinal = (DTT_TEXTCOLOR, DTT_BORDERCOLOR,
    DTT_BORDERSIZE, DTT_SHADOWCOLOR, DTT_SHADOWOFFSET, DTT_GLOWSIZE);
var
  LFlags: Cardinal;
  LOptions: TDTTOpts;
  LTextOption: TStyleTextFlag;
  LColorRef: TColorRef;
begin
    LFlags := TTextFormatFlags(Flags);
    if Win32MajorVersion>=6 then
    begin
      FillChar(LOptions, SizeOf(LOptions), 0);
      LOptions.dwSize := SizeOf(LOptions);
      for LTextOption := Low(TStyleTextFlag) to High(TStyleTextFlag) do
        if (LTextOption in Options.Flags) then
          LOptions.dwFlags := LOptions.dwFlags or COptions[LTextOption];
      LOptions.crText := Graphics.ColorToRGB(Options.TextColor);
      LOptions.crBorder := Graphics.ColorToRGB(Options.BorderColor);
      LOptions.iBorderSize := Options.BorderSize;
      LOptions.crShadow := Graphics.ColorToRGB(Options.ShadowColor);
      LOptions.ptShadowOffset := Options.ShadowOffset;
      LOptions.iGlowSize := Options.GlowSize;
      if (tfComposited in Flags) then
        LOptions.dwFlags := LOptions.dwFlags or DTT_COMPOSITED;
      if (tfCalcRect in Flags) then
        LOptions.dwFlags := LOptions.dwFlags or DTT_CALCRECT;

      Result := DrawThemeTextEx(Theme[Details.Element], DC, Details.Part, Details.State,
        PChar(S), Length(S), LFlags, @R, LOptions) = S_OK;
    end
    else
    begin
      if (stfTextColor in Options.Flags) then
      begin
        LColorRef := SetTextColor(DC, Graphics.ColorToRGB(Options.TextColor));
        try
          Windows.DrawText(DC, PChar(S), Length(S), R, LFlags);
        finally
          SetTextColor(DC, LColorRef);
        end;
        Result := True;
      end
      else
        Result := DrawThemeText(Theme[Details.Element], DC, Details.Part, Details.State,
          PChar(S), Length(S), LFlags, 0, R) = S_OK;
    end;
end;
{$ENDIF}

end.
