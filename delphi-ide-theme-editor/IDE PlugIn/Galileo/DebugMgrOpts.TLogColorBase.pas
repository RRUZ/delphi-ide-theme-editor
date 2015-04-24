type
  TLogColorBase=class(TObject)
  private
   FForegroundColor    :TColor;
   FBackgroundColor    :TColor;
  public
   constructor Create(const AForegroundColor: TColor; const ABackgroundColor: TColor);
   property ForegroundColor: TColor;
   property BackgroundColor: TColor;
  end;
