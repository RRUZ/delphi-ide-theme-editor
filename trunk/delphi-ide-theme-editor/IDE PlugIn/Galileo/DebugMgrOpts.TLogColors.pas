type
  TLogColors=class(TObject)
  private
   FColors             :TLogColorsArray;
   function GetColor(Index: TLogItemType): TLogColorItem;
  public
   constructor Create;
   class destructor Destroy;
   function Assign(Source: TLogColors): Boolean;
   procedure SaveOptions(IniFile: TCustomIniFile);
   procedure LoadOptions(IniFile: TCustomIniFile);
  end;
