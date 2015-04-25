type
  TPropListBox=class(TCustomPropListBox)
  published
   property BevelEdges: TBevelEdges;
   property BevelInner: TBevelCut;
   property BevelKind: TBevelKind;
   property BevelOuter: TBevelCut;
   property BackgroundColor: TColor;
   property PropNameColor: TColor;
   property PropValueColor: TColor;
   property EditBackgroundColor: TColor;
   property EditValueColor: TColor;
   property CategoryColor: TColor;
   property GutterColor: TColor;
   property GutterEdgeColor: TColor;
   property ReferenceColor: TColor;
   property SubPropColor: TColor;
   property ReadOnlyColor: TColor;
   property NonDefaultColor: TColor;
   property NonDefaultFontStyle: TFontStyles;
   property HighlightColor: TColor;
   property HighlightFontColor: TColor;
   property Selection: IPropListBoxSelection;
   property ShowGutter: Boolean;
   property UseVirtualList: Boolean;
   property OnAfterReset: TNotifyEvent;
   property OnAllowExpansion: TAllowExpansionEvent;
   property OnBeforeReset: TNotifyEvent;
   property OnGetProperties: TGetPropertiesEvent;
   property OnGetSubProperties: TGetSubPropertiesEvent;
  end;
