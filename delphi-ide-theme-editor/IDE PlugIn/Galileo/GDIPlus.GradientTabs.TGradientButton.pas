type
  TGradientButton=class(TSpeedButton)
  private
   FBackgroundColor    :TColor;
   FDisabledSingleColor:TColor;
   FHotSingleColor     :TColor;
   FOutline            :Boolean;
  public
   constructor Create(AOwner: TComponent);
   property BackgroundColor: TColor;
   property HotSingleColor: TColor;
   property DisabledSingleColor: TColor;
   property Outline: Boolean;
  end;
