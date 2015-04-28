type
  TCodeEditorTabControl=class(TTabSet)
  private
   FFakeBool           :Boolean;
   FTabOrder           :Integer;
   FOnTabChanging      :TTabChangingEvent;
   FOnChange           :TNotifyEvent;
   FBorderStyle        :TBorderStyle;
   FLastTabHintIndex   :Integer;
   FOnGetTabHintText   :TGetTabHintTextEvent;
  public
   constructor Create(AOwner: TComponent);
   procedure DoUpdateTabImages;
   procedure UpdateImageAtIndex(const Index: Integer);
   function IndexOfTabAt(const X: Integer; const Y: Integer): Integer;
   function TabRect(const Index: Integer): TRect;
   function GetHitTestInfoAt(X: Integer; Y: Integer): THitTests;
   property MouseCapture: Boolean;
  published
   property HotTrack: Boolean;
   property TabOrder: Integer;
   property TabStop: Boolean;
   property OnChanging: TTabChangingEvent;
   property OnChange: TNotifyEvent;
   property BorderStyle: TBorderStyle;
   property OnGetTabHintText: TGetTabHintTextEvent;
  end;
