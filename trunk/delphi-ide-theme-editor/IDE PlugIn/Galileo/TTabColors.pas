type
  TTabColors=class(TPersistent)
  private
   FChangedEvent       :TNotifyEvent;
   FColors             :Unknow;
  public
   class destructor Destroy;
   procedure Assign(Source: TPersistent);
   function GetNamePath: string;
   constructor Create;
   procedure Free;
   procedure DisposeOf;
   class function InitInstance(Instance: Pointer): TObject;
   procedure CleanupInstance;
   function ClassType: TClass;
   class function ClassName: string;
   class function ClassNameIs(const Name: string): Boolean;
   class function ClassParent: TClass;
   class function ClassInfo: Pointer;
   class function InstanceSize: Integer;
   class function InheritsFrom(AClass: TClass): Boolean;
   class function MethodAddress(const Name: ShortString): Pointer;
   class function MethodAddress(const Name: string): Pointer;
   class function MethodName(Address: Pointer): string;
   class function QualifiedClassName: string;
   function FieldAddress(const Name: ShortString): Pointer;
   function FieldAddress(const Name: string): Pointer;
   function GetInterface(const IID: TGUID; out Obj): Boolean;
   class function GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
   class function GetInterfaceTable: PInterfaceTable;
   class function UnitName: string;
   class function UnitScope: string;
   function Equals(Obj: TObject): Boolean;
   function GetHashCode: Integer;
   function ToString: string;
   function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT;
   procedure AfterConstruction;
   procedure BeforeDestruction;
   procedure Dispatch(var Message);
   procedure DefaultHandler(var Message);
   class function NewInstance: TObject;
   procedure FreeInstance;
   class destructor Destroy;
  published
   constructor Create(ChangedEvent: TNotifyEvent = procedure(Sender: TObject) of object);
   property ActiveStart: TColor;
   property ActiveEnd: TColor;
   property InActiveStart: TColor;
   property InActiveEnd: TColor;
  end;
