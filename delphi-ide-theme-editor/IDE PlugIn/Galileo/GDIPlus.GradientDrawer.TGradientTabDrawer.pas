type
  TGradientTabDrawer=class(TObject)
  private
   FCanvas             :TCanvas;
   FTabTop             :Integer;
   FPath               :TGPGraphicsPath;
   FGraphics           :TGPGraphics;
   FOutlinePen         :TGPPen;
   FActiveGradientBrush:TGPLinearGradientBrush;
   FInActiveGradientBrush:TGPLinearGradientBrush;
   FTabBottom          :Integer;
   FTotalWidth         :Integer;
   FLeftPos            :Integer;
   FImageList          :TCustomImageList;
   FLastHandle         :HDC;
   FDrawingStyle       :TTabDrawingStyle;
   FTabColors          :TTabColors;
  public
   constructor Create(const ACanvas: TCanvas);
   class destructor Destroy;
   procedure DrawActiveTab(const TabLeft: Integer; const TabWidth: Integer; const Caption: WideString; const ImageIndex: Integer; const OverlayIndex: Integer);
   procedure DrawInactiveTab(const TabLeft: Integer; const TabWidth: Integer; const Caption: WideString; const ImageIndex: Integer; const OverlayIndex: Integer);
   function GetTextWidth(const Caption: WideString): Integer;
   procedure SetDrawingOffsets(const Left: Integer; const Bottom: Integer; const Right: Integer);
   procedure ResetColors;
   property DrawingStyle: TTabDrawingStyle;
   property ImageList: TCustomImageList;
   property TabColors: TTabColors;
   property TabTop: Integer;
   property TabBottom: Integer;
  end;
