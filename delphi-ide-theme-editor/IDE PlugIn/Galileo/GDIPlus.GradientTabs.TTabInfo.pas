type
  TTabInfo=class(TObject)
  public
   StartPos            :Integer;
   Size                :Integer;
   ImageIndex          :Integer;
   OverlayIndex        :Integer;
   constructor Create(StartPos: Integer; Size: Integer; ImageIndex: Integer; OverlayIndex: Integer);
  end;
