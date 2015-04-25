type
  TTabScroller=class(TCustomControl)
  private
   FShowScrollers      :Boolean;
   FLeftButton         :TScrollerButton;
   FRightButton        :TScrollerButton;
   FOutline            :Boolean;
  public
   constructor Create(AOwner: TComponent);
   function GetSuggestedWidth(ShowScrollers: Boolean): Integer;
   property ShowScrollers: Boolean;
   property Outline: Boolean;
  published
   property LeftButton: TScrollerButton;
   property RightButton: TScrollerButton;
   property Color: TColor;
  end;
