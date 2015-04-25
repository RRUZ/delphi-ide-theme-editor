type
  TScrollerButton=class(TGradientButton)
  private
   FDownTimer          :TTimer;
   FScrollerButtonStyle:TScrollerButtonStyle;
  public
   class destructor Destroy;
   property ScrollerButtonStyle: TScrollerButtonStyle;
  end;
