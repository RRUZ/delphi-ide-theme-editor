type
  TTabColors=class(TPersistent)
  private
   FChangedEvent       :TNotifyEvent;
   FColors             :Unknow;
  published
   constructor Create(ChangedEvent: TNotifyEvent = procedure(Sender: TObject) of object);
   property ActiveStart: TColor;
   property ActiveEnd: TColor;
   property InActiveStart: TColor;
   property InActiveEnd: TColor;
  end;
