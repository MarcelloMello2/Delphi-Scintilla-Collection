unit ModernTabs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ImgList, GDIPlus, ComCtrls, Themes;

type
  TModernTabPosition = (mtpTop, mtpBottom, mtpLeft, mtpRight);
  TModernButtonState = (mbsNormal, mbsHot, mbsPressed);
  TModernPageStage = (mpsDragging, mpsCreating, mpsDeleting);
  TModernPageState = set of TModernPageStage;
  TTabAnimationStage = (tasShowing, tasMoving, tasCreating, tasDeleting);
  TModernTabPageChangeEvent = procedure(Sender: TObject;
    TabIndex, PrevTabIndex: Integer) of object;
  TModernTabCloseEvent = procedure(Sender: TObject; TabIndex: Integer;
    var CanClose: Boolean) of object;
  TModernTabGetImageEvent = procedure(Sender: TObject; TabIndex: Integer;
    var ImageIndex: Integer) of object;

  TModernPageControl = class;

  TModernTabSheet = class(TCustomControl)
  private
    FImageIndex: TImageIndex;
    FData: TObject;
    FPageControl: TModernPageControl;
    FStartTime: Cardinal;
    FAnimationStage: TTabAnimationStage;
    FVirtualOffset: Integer;
    FTargetOffset: Integer;
    FVirtualWidth: Integer;
    FTargetWidht: Integer;
    procedure SetImageIndex(Value: TImageIndex);
    function GetSelected: Boolean;
    function GetPageIndex: Integer;
    procedure SetPageIndex(Value: Integer);
    procedure SetPageControl(Value: TModernPageControl);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: TObject read FData write FData;
    property Selected: Boolean read GetSelected;
  published
    property Color;
    property Caption;
    property Enabled;
    property Font;
    property Height stored False;
    property ImageIndex: TImageIndex read FImageIndex
      write SetImageIndex default -1;
    property Left stored False;
    property PageControl: TModernPageControl read FPageControl
      write SetPageControl;
    property PageIndex: Integer read GetPageIndex
      write SetPageIndex stored False;
    property Width stored False;
    property Top stored False;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  TModernPageControl = class(TCustomControl)
  private
    FPages: TList;
    FBuffer: TBitmap;
    FCanDragTabs: Boolean;
    FSmoothTimer: Cardinal;
    FMouseDown: Boolean;
    FPageState: TModernPageState;
    FImediateEndDrag: Boolean;
    FImediateDragCancel: Boolean;
    FDragTabIndex: Integer;
    FAnimationLockCount: Integer;
    FDragStartPoint: TPoint;
    FDragMousePoint: TPoint;
    FPressCloseButtonIndex: Integer;
    FOverCloseButton: Integer;
    FOverTabIndex: Integer;
    FPressNavButton: Integer;
    FOverNavButton: Integer;
    FFocusedColor: TColor;
    FNormalColor: TColor;
    FBorderWidth: Integer;
    FStartIndex: Integer;
    FEndIndex: Integer;
    FActivePage: TModernTabSheet;
    FNewDockSheet: TModernTabSheet;
    FUndockingPage: TModernTabSheet;
    FImages: TCustomImageList;
    FGetImageIndex: TModernTabGetImageEvent;
    FOnChange: TNotifyEvent;
    FOnPageChange: TModernTabPageChangeEvent;
    FOnClose: TModernTabCloseEvent;
    FTabPosition: TModernTabPosition;
    FMultiLine: Boolean;
    FTabHeight: Integer;
    FTabWidth: Integer;
    FFixedTabWidth: Boolean;
    FShowCloseButton: Boolean;
    FParentColor: Boolean;
    FActivePageIndex: Integer;
    procedure SetActivePage(Value: TModernTabSheet);
    procedure SetBorderWidth(Value: Integer);
    procedure SetFixedTabWidth(Value: Boolean);
    procedure SetTabPosition(Value: TModernTabPosition);
    procedure SetImages(Value: TCustomImageList);
    procedure SetMultiLine(Value: Boolean);
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetParentColor(Value: Boolean);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabWidth(Value: Integer);
    procedure SetNormalColor(Value: TColor);
    procedure SetFocusedColor(Value: TColor);
    function GetActivePageIndex: Integer;
    procedure SetActivePageIndex(Value: Integer);
    function GetPageCount: Integer;
    function GetPage(Index: Integer): TModernTabSheet;
    procedure InsertPage(Page: TModernTabSheet);
    procedure RemovePage(Page: TModernTabSheet);
    procedure ClosePage(Index: Integer);
    function FindNextPage(CurPage: TModernTabSheet;
      GoForward: Boolean = True): TModernTabSheet;
    procedure Move(Page: TModernTabSheet; NewIndex: Integer);
    procedure InvalidateRect(const aRect: TRect; aErase: boolean); virtual;
    procedure InvalidateTab(Tab: TModernTabSheet);
    procedure InvalidateTabs;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    function GetDisplayRect: TRect;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
      WM_WINDOWPOSCHANGED;
    procedure DrawTab(const TabRect: TRect; G: IGPGraphics;
      TabSheet: TModernTabSheet; Selected: Boolean; TabIndex: Integer);
    procedure DrawTabs(g: IGPGraphics);
    procedure DrawCloseButton(const Rect: TRect; g: IGPGraphics;
      State: TModernButtonState);
    procedure DoChange;
    procedure DoPageChange(Index, PrevTabIndex: Integer);
    procedure GetTabRect(Index: Integer; var R: TRect;
      NoChangeOffset: Boolean = False);
    function GetCloseButtonRect(const TabRect: TRect): TRect;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetTabsRect: TRect;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure EndDragging(Cancel: Boolean);
    procedure BeginDragging;
    function GetCenterOfDraggingTab(const TabRect: TRect): TPoint;
    function GetDragDestinationIndex: Integer;
    procedure EndSmoothDragging;
    procedure StartSmoothDragging;
    procedure UpdateTabsPositions;
    procedure DrawNavButton(g: IGPGraphics; StartNav: Boolean);
    function GetNavRect(StartNav: Boolean): TRect;
    function GetTabsWidth(R: TRect; Index: Integer; var aTabOffset,
      aTabWidth, aAvailableWidth: Integer; var CuttingCaption: Boolean;
      TestCaptionCut: Boolean): Integer;
    procedure InitNavPoints(var points: array of TPoint;
      StartNav: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
    procedure PaintTabs;
    procedure Resize; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    property DisplayRect: TRect read GetDisplayRect;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function GetPageFromDockClient(Client: TControl): TModernTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectNextPage(GoForward: Boolean);
    function IndexOfTabAt(X, Y: Integer): Integer;
    function DirectionOfNavAt(X, Y: Integer): Integer;
    procedure CloseActiveTab;
    procedure LockAnimation;
    procedure UnlockAnimation;
    function IsDragging: Boolean;
    procedure CancelDragging;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TModernTabSheet read GetPage;
    property ActivePageIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;
  published
    property ActivePage: TModernTabSheet read FActivePage write SetActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property CanDragTabs: Boolean read FCanDragTabs write FCanDragTabs default False;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property NormalColor: TColor read FNormalColor write SetNormalColor;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor;
    property Font;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine
      write SetMultiLine default False;
    property ParentBiDiMode;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property BorderWidth: Integer read FBorderWidth
      write SetBorderWidth default 2;
    property ShowCloseButton: Boolean read FShowCloseButton
      write SetShowCloseButton default True;
    property TabHeight: Integer read FTabHeight write SetTabHeight default 25;
    property TabIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;
    property TabOrder;
    property TabPosition: TModernTabPosition read FTabPosition
      write SetTabPosition default mtpTop;
    property TabStop;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 208;
    property FixedTabWidth: Boolean read FFixedTabWidth write SetFixedTabWidth;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClose: TModernTabCloseEvent read FOnClose write FOnClose;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex: TModernTabGetImageEvent read FGetImageIndex
      write FGetImageIndex;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPageChange: TModernTabPageChangeEvent read FOnPageChange
      write FOnPageChange;
    property OnResize;
  end;

implementation

uses Types, Math, FalconDrawUtils;

const
  EdgeWidth = 14;
  UnionTabWidth = 17;
  TabsOffset = 4;
  NavWidth = 20;
  NavSpacing = 5;
  ButtonRightOffset = 15;
  MinDistStartDrag = 8;
  MinDistAfterTab = 4;
  // Drag Animating
  tabsMovingFactor = 250;
  tabsMovingVelocity = 20;
  tabsStepsPerSecond = 60;


{ TModernTabSheet }

function MakeGDIColor(C: TColor): Cardinal;
var
  tmpRGB: TColorRef;
begin
  tmpRGB := ColorToRGB(C);
  result := ((DWORD(GetBValue(tmpRGB)) shl BLUE_SHIFT) or
    (DWORD(GetGValue(tmpRGB)) shl GREEN_SHIFT) or
    (DWORD(GetRValue(tmpRGB)) shl RED_SHIFT) or
    ALPHA_MASK);
end;

function RectToGPRect(const Rect: TRect): TGPRect;
begin
  Result.X := Rect.Left;
  Result.Y := Rect.Top;
  Result.Width := Rect.Right - Rect.Left;
  Result.Height := Rect.Bottom - Rect.Top;
end;

function PtInPoly(P: TPoint; const points: array of TPoint): Boolean;
var
  p0, p1: TPoint;
  i: integer;
  UpSide: boolean;
begin
  result := false;
  p1 := points[High(points)];
  UpSide := (p.y < p1.y);
  for i := 0 to High(points) do begin
    p0 := p1;
    p1 := points[i];
    if (UpSide xor (p.y < p1.y)) then begin
      if (p.x < (p0.x - p1.x) * (p.y - p1.y) / (p0.y - p1.y) + p1.x) then
        result := not result;
      UpSide := not UpSide;
    end;
  end;
end;

procedure TModernTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.Style := Params.WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
end;

constructor TModernTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  if AOwner is TWinControl then
    Parent := TWinControl(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque, csNoDesignVisible];
  Color := $F6F6F6;
  Visible := False;
  FAnimationStage := tasShowing;
end;

destructor TModernTabSheet.Destroy;
begin
  if FPageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then
      FPageControl.FUndockingPage := nil;
    FPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TModernTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TModernPageControl then
    PageControl := TModernPageControl(Reader.Parent);
end;

function TModernTabSheet.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

function TModernTabSheet.GetSelected: Boolean;
begin
  if FPageControl <> nil then
    Result := FPageControl.ActivePage = Self
  else
    Result := False;
end;

procedure TModernTabSheet.SetImageIndex(Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    if Assigned(FPageControl) then
      FPageControl.InvalidateTab(Self);
  end;
end;

procedure TModernTabSheet.SetPageControl(Value: TModernPageControl);
begin
  if Value <> FPageControl then
  begin
    if FPageControl <> nil then
      FPageControl.RemovePage(Self);
    FPageControl := Value;
    if FPageControl <> nil then
      FPageControl.InsertPage(Self);
  end;
end;

procedure TModernTabSheet.SetPageIndex(Value: Integer);
begin
  if FPageControl <> nil then
    FPageControl.Move(Self, Value);
end;

procedure TModernTabSheet.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FPageControl <> nil then
    FPageControl.InvalidateTabs;
end;

procedure TModernTabSheet.CMTextChanged(var Message: TMessage);
begin
  if FPageControl <> nil then
    FPageControl.InvalidateTabs;
end;

procedure TModernTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TModernTabSheet.Paint;
begin
  //Canvas.Brush.Color := Color;
  //Canvas.FillRect(ClientRect);
  DrawParentBackground(Self, Canvas.Handle, Canvas.ClipRect);
end;

{ TModernPageControl }

constructor TModernPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque, csCaptureMouse];
  FOverCloseButton := -1;
  FPressCloseButtonIndex := -1;
  FOverTabIndex := -1;
  FDragTabIndex := -1;
  FEndIndex := -1;
  FActivePageIndex := -1;
  FCanDragTabs := False;
  Width := 289;
  Height := 193;
  FBorderWidth := 2;
  FTabHeight := 25;
  FTabWidth := 208;
  FFixedTabWidth := True;
  TabStop := True;
  FNormalColor := $E3D6CB;
  FFocusedColor := $F6F6F6;
  FPages := TList.Create;
  FBuffer := TBitmap.Create;
  FBuffer.Canvas.Font.Name := 'Microsoft Sans Serif';
  FBuffer.Canvas.Font.Height := 14;
  FShowCloseButton := True;
end;

destructor TModernPageControl.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    TModernTabSheet(FPages.Items[I]).FPageControl := nil;
  FPages.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TModernPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params.WindowClass do begin
    style := style and not (CS_VREDRAW or CS_HREDRAW);
  end;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

function TModernPageControl.GetPage(Index: Integer): TModernTabSheet;
begin
  Result := TModernTabSheet(FPages.Items[Index]);
end;

function TModernPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TModernPageControl.GetActivePageIndex: Integer;
begin
  Result := FActivePageIndex;
end;

function TModernPageControl.IndexOfTabAt(X, Y: Integer): Integer;
var
  I: Integer;
  Pt: TPoint;
  TabRect: TRect;
begin
  Pt := Point(X, Y);
  for I := FStartIndex to FEndIndex do
  begin
    GetTabRect(I, TabRect);
    if PtInRect(TabRect, Pt) then
    begin
      Result := I;
      Exit;
    end;
    if not FMultiLine then
    begin
      if TabPosition in [mtpTop, mtpBottom] then
      begin
        if (TabRect.Right > X) or (TabRect.Bottom < Y) then
          Break;
      end
      else
      begin
        if (TabRect.Right < X) or (TabRect.Bottom > Y) then
          Break;
      end;
    end;
  end;
  Result := -1;
end;

procedure TModernPageControl.InitNavPoints(var points: array of TPoint;
  StartNav: Boolean);
const
    EdgeWidth = 11;
var
  R: TRect;
begin
  R := GetNavRect(StartNav);
  if StartNav then
  begin
    points[0] := Point(R.Left, R.Bottom);
    points[1] := Point(R.Left + EdgeWidth, R.Top);
    points[2] := Point(R.Right + EdgeWidth, R.Top);
    points[3] := Point(R.Right, R.Bottom);
  end
  else
  begin
    points[0] := Point(R.Right, R.Bottom);
    points[1] := Point(R.Right - EdgeWidth, R.Top);
    points[2] := Point(R.Left - EdgeWidth, R.Top);
    points[3] := Point(R.Left, R.Bottom);
  end;
end;

function TModernPageControl.DirectionOfNavAt(X, Y: Integer): Integer;
var
  P: TPoint;
  points: array[0..3] of TPoint;
begin
  Result := 0;
  P := Point(X, Y);
  InitNavPoints(points, True);
  if (FStartIndex > 0) and PtInPoly(P, points) then
  begin
    Result := -1;
    Exit;
  end;
  InitNavPoints(points, False);
  if (FEndIndex < PageCount - 1) and PtInPoly(P, points) then
    Result := 1;
end;

procedure TModernPageControl.InvalidateTab(Tab: TModernTabSheet);
begin
  Invalidate;
  //TODO
end;

procedure TModernPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

procedure TModernPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TModernTabSheet(Child).PageIndex := Order;
end;

procedure TModernPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TModernTabSheet) and (TModernTabSheet(AControl).PageControl = Self) then
    SetActivePage(TModernTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TModernPageControl.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    InvalidateTabs;
  end;
end;

procedure TModernPageControl.SetMultiLine(Value: Boolean);
begin
  if Value <> FMultiLine then
  begin
    FMultiLine := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TModernPageControl.SetShowCloseButton(Value: Boolean);
begin
  if Value <> FShowCloseButton then
  begin
    FShowCloseButton := Value;
    InvalidateTabs;
  end;
end;

procedure TModernPageControl.SetTabHeight(Value: Integer);
begin
  if Value <> FTabHeight then
  begin
    FTabHeight := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TModernPageControl.SetActivePageIndex(Value: Integer);
begin
  if (Value > -1) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TModernPageControl.SetTabPosition(Value: TModernTabPosition);
begin
  if Value <> FTabPosition then
  begin
    FTabPosition := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TModernPageControl.SetTabWidth(Value: Integer);
begin
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    Realign;
    InvalidateTabs;
  end;
end;

procedure TModernPageControl.SetActivePage(Value: TModernTabSheet);
var
  ParentForm: TCustomForm;
  ToIndex: Integer;
  R: TRect;
  aTabOffset, aTabWidth, aAvailableWidth, PrevTabIndex: Integer;
  CuttingCaption: boolean;
begin
  if (Value <> nil) and (Value.FPageControl <> Self) then
    Exit;
  PrevTabIndex := ActivePageIndex;
  if FActivePage <> Value then
  begin
    if mpsDragging in FPageState then
      EndDragging(True);
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
    end;
    if Value <> nil then
    begin
      Value.BringToFront;
      Value.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
      begin
        if Value.CanFocus then
          ParentForm.ActiveControl := Value
        else
          ParentForm.ActiveControl := Self;
      end;
    end;
    if Value <> nil then
    begin
      FActivePageIndex := Value.PageIndex;
      ToIndex := FActivePageIndex;
      if (ToIndex < FStartIndex) then
      begin
        FEndIndex := ToIndex + (FEndIndex - FStartIndex);
        FStartIndex := ToIndex;
      end
      else if (ToIndex > FEndIndex) then
      begin
        FStartIndex := ToIndex - (FEndIndex - FStartIndex);
        FEndIndex := ToIndex;
      end;
      R := GetTabsRect;
      GetTabsWidth(R, FStartIndex, aTabOffset, aTabWidth, aAvailableWidth,
        CuttingCaption, True);
      if CuttingCaption then
      begin
        if ToIndex < FEndIndex then
          Dec(FEndIndex)
        else
          Inc(FStartIndex);
      end;
    end;
    if FActivePage <> nil then
      FActivePage.Visible := False;
    FActivePage := Value;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
    if FActivePage <> nil then
      InvalidateTabs
    else
    begin
      FActivePageIndex := -1;
      Invalidate;
    end;
    if FActivePage <> nil then
      DoPageChange(FActivePage.GetPageIndex, PrevTabIndex);
  end;
end;

procedure TModernPageControl.InsertPage(Page: TModernTabSheet);
var
  TabRect, R: TRect;
  aTabOffset, aTabWidth, aAvailableWidth, I: Integer;
  CuttingCaption: boolean;
begin
  I := FPages.Add(Page);
  Page.FPageControl := Self;
  // control navigation here
  Inc(FEndIndex);
  R := GetTabsRect;
  GetTabsWidth(R, I, aTabOffset, aTabWidth, aAvailableWidth,
    CuttingCaption, True);
  if CuttingCaption then
    Dec(FEndIndex);
  // creating animation
  if not (csDesigning in ComponentState) and
    not (csLoading in ComponentState) and (FAnimationLockCount = 0) then
  begin
    GetTabRect(I, TabRect);
    Page.FAnimationStage := tasCreating;
    Page.FVirtualWidth := 2 * EdgeWidth + 8;
    Page.FTargetWidht := TabRect.Right - TabRect.Left;
    Page.FStartTime := GetTickCount;
    Include(FPageState, mpsCreating);
    StartSmoothDragging;
  end;
  // end creating animation
  if (csDesigning in ComponentState) then
    SetActivePage(Page)
  else
    InvalidateTabs;
  DoChange;
end;

procedure TModernPageControl.RemovePage(Page: TModernTabSheet);
var
  NextPage: TModernTabSheet;
  SelectOtherPage: Boolean;
  I: Integer;
begin
  if mpsDragging in FPageState then
    EndDragging(True);
  if FEndIndex = PageCount - 1 then
  begin
    NextPage := FindNextPage(Page, False);
    if NextPage = nil then
      NextPage := FindNextPage(Page);
  end
  else
  begin
    NextPage := FindNextPage(Page);
    if NextPage = nil then
      NextPage := FindNextPage(Page, False);
  end;
  SelectOtherPage := (Page <> nil) and (Page = FActivePage);
  I := FPages.Remove(Page);
  if I >= 0 then
  begin
    if Page <> nil then
      Page.FPageControl := nil;
    // control navigation here
    if (I < FStartIndex) then // update, invalidate when starts from 0
      Dec(FStartIndex);
    if FEndIndex > PageCount - 1 then
    begin
      FEndIndex := PageCount - 1;
      if (FStartIndex > 0) then
        Dec(FStartIndex);
    end;
    if I < FActivePageIndex then
      Dec(FActivePageIndex)
    else if I = FActivePageIndex then
      FActivePageIndex := -1;
    DoChange;
    if SelectOtherPage then
      SetActivePage(NextPage)
    else
      InvalidateTabs;
  end;
end;

procedure TModernPageControl.InvalidateTabs;
var
  rect: TRect;
begin
  rect := GetTabsRect;
  case TabPosition of
    mtpTop: Inc(rect.Bottom, 2);
    mtpBottom: Dec(rect.Top, 2);
    mtpLeft: Inc(rect.Right, 2);
  else
    Dec(rect.Left, 2);
  end;
  InvalidateRect(rect, FALSE);
end;

procedure TModernPageControl.InvalidateRect(const aRect: TRect;
  aErase: boolean);
begin
  Windows.InvalidateRect(Handle, @aRect, aErase);
end;

function TModernPageControl.FindNextPage(CurPage: TModernTabSheet;
  GoForward: Boolean): TModernTabSheet;
var
  Index: Integer;
begin
  Result := nil;
  if (CurPage <> nil) then
  begin
    Index := CurPage.PageIndex;
    if GoForward then
    begin
      if (Index + 1 < PageCount) and (PageCount > 0) then
        Result := Pages[Index + 1];
    end
    else
    begin
      if (Index - 1 >= 0) then
        Result := Pages[Index - 1];
    end;
  end;
end;

procedure TModernPageControl.Move(Page: TModernTabSheet;
  NewIndex: Integer);
var
  Index: Integer;
begin
  Index := Page.PageIndex;
  FPages.Move(Index, NewIndex);
  // moving active page
  if Index = FActivePageIndex then
    FActivePageIndex := NewIndex
  // moving page to after active page
  else if (Index < FActivePageIndex) and (NewIndex >= FActivePageIndex) then
    Dec(FActivePageIndex)
  // moving page to before active page
  else if (Index > FActivePageIndex) and (NewIndex <= FActivePageIndex) then
    Inc(FActivePageIndex);
  InvalidateTabs;
end;

procedure TModernPageControl.DoAddDockClient(Client: TControl;
  const ARect: TRect);
begin
  if FNewDockSheet <> nil then Client.Parent := FNewDockSheet;
end;

procedure TModernPageControl.DockOver(Source: TDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TModernPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    SelectNextPage(True);
    FUndockingPage.Free;
    FUndockingPage := nil;
  end;
end;

function TModernPageControl.GetPageFromDockClient(
  Client: TControl): TModernTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

procedure TModernPageControl.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

procedure TModernPageControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    SetImages(nil);
end;


procedure TModernPageControl.SelectNextPage(GoForward: Boolean);
var
  Page: TModernTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward);
  if (Page <> nil) and (Page <> ActivePage) then
    SetActivePage(Page);
end;

function TModernPageControl.GetDisplayRect: TRect;
var
  LineBorder: Integer;
begin
  Result := ClientRect;
  LineBorder := 1;
  InflateRect(Result, -LineBorder, -LineBorder);
  if PageCount = 0 then
    Exit;
  case TabPosition of
    mtpTop: Inc(Result.Top, FTabHeight);
    mtpBottom: Dec(Result.Bottom, FTabHeight);
    mtpLeft: Inc(Result.Left, FTabHeight);
    mtpRight: Dec(Result.Right, FTabHeight);
  end;
  InflateRect(Result, -FBorderWidth, -FBorderWidth);
end;

function TModernPageControl.GetTabsRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if PageCount = 0 then
    Exit;
  Result := ClientRect;
  case TabPosition of
    mtpTop: Result.Bottom := Result.Top + FTabHeight - 1;
    mtpBottom: Result.Top := Result.Bottom - FTabHeight + 1;
    mtpLeft: Result.Right := Result.Left + FTabHeight - 1;
    mtpRight: Result.Left := Result.Right - FTabHeight + 1;
  end;
end;

procedure TModernPageControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := DisplayRect;
  inherited AdjustClientRect(Rect);
end;

procedure TModernPageControl.SetBorderWidth(Value: Integer);
begin
  if Value <> FBorderWidth then
  begin
    FBorderWidth := Value;
    Realign;
    Invalidate;
  end;
end;

function CreateRoundRectangle(rectangle: TRect;
  radius: integer; Position: TModernTabPosition): IGPGraphicsPath;
var
  path: IGPGraphicsPath;
  l, t, w, h, d: integer;
begin
  path := TGPGraphicsPath.Create;
  l := rectangle.Left;
  t := rectangle.Top;
  w := rectangle.Right - rectangle.Left;
  h := rectangle.Bottom - rectangle.Top;
  d := radius div 2; // divide by 2

  // the lines beween the arcs are automatically added by the path
  if Position = mtpTop then
  begin
    path.AddLine(l, t + h - 1, l, t + d); // left line
    path.AddArc(l, t, d, d, 180, 90); // topleft
    path.AddLine(l + d, t, l + w - d - 1, t); // top line
    path.AddArc(l + w - d - 1, t, d, d, 270, 90); // topright
    path.AddLine(l + w - 1, t + d, l + w - 1, t + h - 1); // right line
    path.AddLine(l + w - 1, t + h - 1, l, t + h - 1); // bottom line
  end
  else if Position = mtpBottom then
  begin
    path.AddLine(l, t, l + w - 1, t); // top line
    path.AddLine(l + w - 1, t, l + w - 1, t + h - 1 - d); // right line
    path.AddArc(l + w - 1 - d, t + h - 1 - d, d, d, 0, 90); // bottomright
    path.AddLine(l + w - d - 1, t + h - 1, l + d, t + h - 1); // bottom line
    path.AddArc(l, t + h - 1 - d, d, d, 90, 90); // bottomleft
    path.AddLine(l, t + h - 1 - d, l, t); // left line
  end
  else if Position = mtpLeft then
  begin
    path.AddLine(l + d, t, l + w - d - 1, t); // top line
    path.AddLine(l + w - 1, t, l + w - 1, t + h - 1); // right line
    path.AddLine(l + w - 1, t + h - 1, l + d, t + h - 1); // bottom line
    path.AddArc(l, t + h - 1 - d, d, d, 90, 90); // bottomleft
    path.AddLine(l, t + h - 1 - d, l, t + d); // left line
    path.AddArc(l, t, d, d, 180, 90); // topleft
  end
  else
  begin
    path.AddLine(l, t + h - 1, l, t); // left line
    path.AddLine(l, t, l + w - 1 - d, t); // top line
    path.AddArc(l + w - d - 1, t, d, d, 270, 90); // topright
    path.AddLine(l + w - 1, t + d, l + w - 1, t + h - 1 - d); // right line
    path.AddArc(l + w - 1 - d, t + h - 1 - d, d, d, 0, 90); // bottomright
    path.AddLine(l + w - d - 1, t + h - 1, l, t + h - 1); // bottom line
  end;
  //path.CloseFigure();
  result := path;
end;

procedure TModernPageControl.DrawCloseButton(const Rect: TRect; g: IGPGraphics;
  State: TModernButtonState);
var
  brush: IGPSolidBrush;
  pen: IGPPen;
  PenColor: Cardinal;
  OldPixelOffsetMode: TGPPixelOffsetMode;
  OldSmoothingMode: TGPSmoothingMode;
begin
  brush := TGPSolidBrush.Create(MakegDIColor($003535C1));
  pen := TGPPen.Create(MakeGDIColor(clBlack));
  OldPixelOffsetMode := g.GetPixelOffsetMode;
  OldSmoothingMode := g.GetSmoothingMode;
  g.SetPixelOffsetMode(PixelOffsetModeHalf);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  if State in [mbsHot, mbsPressed] then
  begin
    if State = mbsPressed then
      brush.SetColor(MakeGDIColor($00201E43))
    else
      brush.SetColor(MakeGDIColor($003535C1));
    g.FillEllipse(brush, Rect.Left, Rect.Top,
      Rect.Right - Rect.Left,
      Rect.Bottom - Rect.Top);
    PenColor := MakeGDIColor(clWhite);
  end
  else
    PenColor := MakeGDIColor($00AEAFAE);
  pen.SetColor(PenColor);
  if State = mbsNormal then
    g.SetSmoothingMode(SmoothingModeAntiAlias)
  else
    g.SetSmoothingMode(SmoothingModeDefault);
  //   \ solid
  g.DrawLine(pen, Rect.Left + 3, Rect.Top + 3,
    Rect.Right - 3, Rect.Bottom - 3);
  //   / solid
  g.DrawLine(pen, Rect.Left + 3, Rect.Bottom - 3,
    Rect.Right - 3, Rect.Top + 3);
  g.SetSmoothingMode(SmoothingModeDefault);
  brush.SetColor(PenColor);
  g.FillRectangle(brush, Rect.Left + 5, Rect.Top + 5, 2, 2);
  PenColor := (PenColor and not ALPHA_MASK) or (100 shl ALPHA_SHIFT);
  brush.SetColor(PenColor);
  g.FillRectangle(brush, Rect.Left + 4, Rect.Top + 4, 4, 4);
  g.FillRectangle(brush, Rect.Left + 3, Rect.Top + 3, 2, 2);
  g.FillRectangle(brush, Rect.Right - 5, Rect.Top + 3, 2, 2);
  g.FillRectangle(brush, Rect.Left + 3, Rect.Bottom - 5, 2, 2);
  g.FillRectangle(brush, Rect.Right - 5, Rect.Bottom - 5, 2, 2);
  g.SetSmoothingMode(OldSmoothingMode);
  g.SetPixelOffsetMode(OldPixelOffsetMode);
end;

function TModernPageControl.GetNavRect(StartNav: Boolean): TRect;
begin
  Result := GetTabsRect;
  if TabPosition in [mtpTop] then
    Dec(Result.Bottom, 5)
  else
  begin
    Inc(Result.Top, 5);
    Dec(Result.Top, 1);
    Dec(Result.Bottom, 1);
  end;
  if StartNav then
  begin
    Inc(Result.Left, TabsOffset);
    Result.Right := NavWidth + Result.Left;
  end
  else
  begin
    Dec(Result.Right, TabsOffset);
    Result.Left := Result.Right - NavWidth;
  end;
end;

procedure TModernPageControl.DrawNavButton(g: IGPGraphics; StartNav: Boolean);

  procedure MakeArrowPath(const Rect: TRect; path: IGPGraphicsPath;
    StartNav: Boolean; Inflate: Integer = 0);
  const
    LeftOffset = 10;
    TopOffset = 3;
  begin
    path.Reset;
    case TabPosition of
      mtpTop:
        begin
          if StartNav then
          begin
            path.AddLine(LeftOffset + Rect.Left + 0, TopOffset + Rect.Top + 6 - Inflate,
              LeftOffset + Rect.Left + 6, TopOffset + Rect.Top + 0 - Inflate);
            path.AddLine(LeftOffset + Rect.Left + 6, TopOffset + Rect.Top + 0 - Inflate,
              LeftOffset + Rect.Left + 8 + Inflate, TopOffset + Rect.Top + 2);
            path.AddLine(LeftOffset + Rect.Left + 8 + Inflate, TopOffset + Rect.Top + 2,
              LeftOffset + Rect.Left + 4 + Inflate, TopOffset + Rect.Top + 6);
            path.AddLine(LeftOffset + Rect.Left + 4 + Inflate, TopOffset + Rect.Top + 6,
              LeftOffset + Rect.Left + 4 + Inflate, TopOffset + Rect.Top + 7);
            path.AddLine(LeftOffset + Rect.Left + 4 + Inflate, TopOffset + Rect.Top + 7,
              LeftOffset + Rect.Left + 8 + Inflate, TopOffset + Rect.Top + 11);
            path.AddLine(LeftOffset + Rect.Left + 8 + Inflate, TopOffset + Rect.Top + 11,
              LeftOffset + Rect.Left + 6, TopOffset + Rect.Top + 13 + Inflate);
            path.AddLine(LeftOffset + Rect.Left + 6, TopOffset + Rect.Top + 13 + Inflate,
              LeftOffset + Rect.Left + 0, TopOffset + Rect.Top + 7 + Inflate);
            path.AddLine(LeftOffset + Rect.Left + 0, TopOffset + Rect.Top + 7 + Inflate,
              LeftOffset + Rect.Left + 0, TopOffset + Rect.Top + 6 - Inflate);
          end
          else
          begin
            path.AddLine(-LeftOffset + Rect.Right - 0, TopOffset + Rect.Top + 6 - Inflate,
              -LeftOffset + Rect.Right - 6, TopOffset + Rect.Top + 0 - Inflate);
            path.AddLine(-LeftOffset + Rect.Right - 6, TopOffset + Rect.Top + 0 - Inflate,
              -LeftOffset + Rect.Right - 8 - Inflate, TopOffset + Rect.Top + 2);
            path.AddLine(-LeftOffset + Rect.Right - 8 + Inflate, TopOffset + Rect.Top + 2,
              -LeftOffset + Rect.Right - 4 - Inflate, TopOffset + Rect.Top + 6);
            path.AddLine(-LeftOffset + Rect.Right - 4 + Inflate, TopOffset + Rect.Top + 6,
              -LeftOffset + Rect.Right - 4 - Inflate, TopOffset + Rect.Top + 7);
            path.AddLine(-LeftOffset + Rect.Right - 4 + Inflate, TopOffset + Rect.Top + 7,
              -LeftOffset + Rect.Right - 8 - Inflate, TopOffset + Rect.Top + 11);
            path.AddLine(-LeftOffset + Rect.Right - 8 + Inflate, TopOffset + Rect.Top + 11,
              -LeftOffset + Rect.Right - 6, TopOffset + Rect.Top + 13 + Inflate);
            path.AddLine(-LeftOffset + Rect.Right - 6, TopOffset + Rect.Top + 13 + Inflate,
              -LeftOffset + Rect.Right - 0, TopOffset + Rect.Top + 7 + Inflate);
            path.AddLine(-LeftOffset + Rect.Right - 0, TopOffset + Rect.Top + 7 + Inflate,
              -LeftOffset + Rect.Right - 0, TopOffset + Rect.Top + 6 - Inflate);
          end;
        end;
      mtpBottom:
        begin
          if StartNav then
          begin
            path.AddLine(LeftOffset + Rect.Left + 0, - TopOffset + Rect.Bottom - 6 + Inflate,
              LeftOffset + Rect.Left + 6, - TopOffset + Rect.Bottom - 0 + Inflate);
            path.AddLine(LeftOffset + Rect.Left + 6, - TopOffset + Rect.Bottom - 0 + Inflate,
              LeftOffset + Rect.Left + 8 + Inflate, - TopOffset + Rect.Bottom - 2);
            path.AddLine(LeftOffset + Rect.Left + 8 + Inflate, - TopOffset + Rect.Bottom - 2,
              LeftOffset + Rect.Left + 4 + Inflate, - TopOffset + Rect.Bottom - 6);
            path.AddLine(LeftOffset + Rect.Left + 4 + Inflate, - TopOffset + Rect.Bottom - 6,
              LeftOffset + Rect.Left + 4 + Inflate, - TopOffset + Rect.Bottom - 7);
            path.AddLine(LeftOffset + Rect.Left + 4 + Inflate, - TopOffset + Rect.Bottom - 7,
              LeftOffset + Rect.Left + 8 + Inflate, - TopOffset + Rect.Bottom - 11);
            path.AddLine(LeftOffset + Rect.Left + 8 + Inflate, - TopOffset + Rect.Bottom - 11,
              LeftOffset + Rect.Left + 6, - TopOffset + Rect.Bottom - 13 - Inflate);
            path.AddLine(LeftOffset + Rect.Left + 6, - TopOffset + Rect.Bottom - 13 - Inflate,
              LeftOffset + Rect.Left + 0, - TopOffset + Rect.Bottom - 7 - Inflate);
            path.AddLine(LeftOffset + Rect.Left + 0, - TopOffset + Rect.Bottom - 7 - Inflate,
              LeftOffset + Rect.Left + 0, - TopOffset + Rect.Bottom - 6 + Inflate);
          end
          else
          begin
            path.AddLine(-LeftOffset + Rect.Right - 0, - TopOffset + Rect.Bottom - 6 + Inflate,
              -LeftOffset + Rect.Right - 6, - TopOffset + Rect.Bottom - 0 + Inflate);
            path.AddLine(-LeftOffset + Rect.Right - 6, - TopOffset + Rect.Bottom - 0 + Inflate,
              -LeftOffset + Rect.Right - 8 - Inflate, - TopOffset + Rect.Bottom - 2);
            path.AddLine(-LeftOffset + Rect.Right - 8 + Inflate, - TopOffset + Rect.Bottom - 2,
              -LeftOffset + Rect.Right - 4 - Inflate, - TopOffset + Rect.Bottom - 6);
            path.AddLine(-LeftOffset + Rect.Right - 4 + Inflate, - TopOffset + Rect.Bottom - 6,
              -LeftOffset + Rect.Right - 4 - Inflate, - TopOffset + Rect.Bottom - 7);
            path.AddLine(-LeftOffset + Rect.Right - 4 + Inflate, - TopOffset + Rect.Bottom - 7,
              -LeftOffset + Rect.Right - 8 - Inflate, - TopOffset + Rect.Bottom - 11);
            path.AddLine(-LeftOffset + Rect.Right - 8 + Inflate, - TopOffset + Rect.Bottom - 11,
              -LeftOffset + Rect.Right - 6, - TopOffset + Rect.Bottom - 13 - Inflate);
            path.AddLine(-LeftOffset + Rect.Right - 6, - TopOffset + Rect.Bottom - 13 - Inflate,
              -LeftOffset + Rect.Right - 0, - TopOffset + Rect.Bottom - 7 - Inflate);
            path.AddLine(-LeftOffset + Rect.Right - 0, - TopOffset + Rect.Bottom - 7 - Inflate,
              -LeftOffset + Rect.Right - 0, - TopOffset + Rect.Bottom - 6 + Inflate);
          end;
        end;
    end;
  end;

  procedure MakeNavPath(const Rect: TRect; path: IGPGraphicsPath;
    StartNav: Boolean; Inflate: Integer = 0);
  const
    BezierRadius = 2;
    BezierXDist = 1;
    BezierYDist = 1;
    EdgeWidth = 11;
  begin
    path.Reset;
    case TabPosition of
      mtpTop:
        begin
          if StartNav then
          begin
            path.AddBezier(
              Rect.Left - Inflate, Rect.Bottom,
              Rect.Left - Inflate + BezierXDist, Rect.Bottom - BezierYDist,
              Rect.Left - Inflate + (BezierRadius - BezierYDist), Rect.Bottom - BezierXDist,
              Rect.Left - Inflate + BezierRadius, Rect.Bottom - BezierRadius);
            path.AddLine(
              Rect.Left - Inflate + BezierRadius, Rect.Bottom - BezierRadius,
              Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Top - Inflate + BezierRadius);
            path.AddBezier(
              Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Top - Inflate + BezierRadius,
              Rect.Left - Inflate + EdgeWidth - (BezierRadius - BezierYDist), Rect.Top - Inflate + BezierXDist,
              Rect.Left - Inflate + EdgeWidth - BezierXDist, Rect.Top - Inflate + BezierYDist,
              Rect.Left - Inflate + EdgeWidth, Rect.Top - Inflate);
            path.AddLine(
              Rect.Left - Inflate + EdgeWidth, Rect.Top - Inflate,
              Rect.Right + Inflate + EdgeWidth, Rect.Top - Inflate);
            path.AddBezier(
              Rect.Right + Inflate + EdgeWidth, Rect.Top - Inflate,
              Rect.Right + Inflate + EdgeWidth - BezierXDist, Rect.Top - Inflate + BezierYDist,
              Rect.Right + Inflate + EdgeWidth - (BezierRadius - BezierYDist), Rect.Top - Inflate + BezierXDist,
              Rect.Right + Inflate + EdgeWidth - BezierRadius, Rect.Top - Inflate + BezierRadius);
            path.AddLine(
              Rect.Right + Inflate + EdgeWidth - BezierRadius, Rect.Top - Inflate + BezierRadius,
              Rect.Right + Inflate + BezierRadius, Rect.Bottom - BezierRadius);
            path.AddBezier(
              Rect.Right + Inflate + BezierRadius, Rect.Bottom - BezierRadius,
              Rect.Right + Inflate + (BezierRadius - BezierYDist), Rect.Bottom - BezierXDist,
              Rect.Right + Inflate + BezierXDist, Rect.Bottom - BezierYDist,
              Rect.Right + Inflate, Rect.Bottom);
            path.AddLine(Rect.Right + Inflate, Rect.Bottom, Rect.Left - Inflate, Rect.Bottom);
          end
          else
          begin
            path.AddBezier(
              Rect.Right + Inflate, Rect.Bottom,
              Rect.Right + Inflate - BezierXDist, Rect.Bottom - BezierYDist,
              Rect.Right + Inflate - (BezierRadius - BezierYDist), Rect.Bottom - BezierXDist,
              Rect.Right + Inflate - BezierRadius, Rect.Bottom - BezierRadius);
            path.AddLine(
              Rect.Right + Inflate - BezierRadius, Rect.Bottom - BezierRadius,
              Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Top - Inflate + BezierRadius);
            path.AddBezier(
              Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Top - Inflate + BezierRadius,
              Rect.Right + Inflate - EdgeWidth + (BezierRadius - BezierYDist), Rect.Top - Inflate + BezierXDist,
              Rect.Right + Inflate - EdgeWidth + BezierXDist, Rect.Top - Inflate + BezierYDist,
              Rect.Right + Inflate - EdgeWidth, Rect.Top - Inflate);
            path.AddLine(
              Rect.Right + Inflate - EdgeWidth, Rect.Top - Inflate,
              Rect.Left - Inflate - EdgeWidth, Rect.Top - Inflate);
            path.AddBezier(
              Rect.Left + Inflate - EdgeWidth, Rect.Top - Inflate,
              Rect.Left + Inflate - EdgeWidth + BezierXDist, Rect.Top - Inflate + BezierYDist,
              Rect.Left + Inflate - EdgeWidth + (BezierRadius - BezierYDist), Rect.Top - Inflate + BezierXDist,
              Rect.Left + Inflate - EdgeWidth + BezierRadius, Rect.Top - Inflate + BezierRadius);
            path.AddLine(
              Rect.Left - Inflate - EdgeWidth + BezierRadius, Rect.Top - Inflate + BezierRadius,
              Rect.Left - Inflate - BezierRadius, Rect.Bottom - BezierRadius);
            path.AddBezier(
              Rect.Left - Inflate - BezierRadius, Rect.Bottom - BezierRadius,
              Rect.Left - Inflate - (BezierRadius - BezierYDist), Rect.Bottom - BezierXDist,
              Rect.Left - Inflate - BezierXDist, Rect.Bottom - BezierYDist,
              Rect.Left - Inflate, Rect.Bottom);
            path.AddLine(Rect.Left - Inflate, Rect.Bottom, Rect.Right + Inflate, Rect.Bottom);
          end;
        end;
      mtpBottom:
        begin
          if StartNav then
          begin
            path.AddBezier(
              Rect.Left - Inflate, Rect.Top,
              Rect.Left - Inflate + BezierXDist, Rect.Top + BezierYDist,
              Rect.Left - Inflate + (BezierRadius - BezierYDist), Rect.Top + BezierXDist,
              Rect.Left - Inflate + BezierRadius, Rect.Top + BezierRadius);
            path.AddLine(
              Rect.Left - Inflate + BezierRadius, Rect.Top + BezierRadius,
              Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Bottom + Inflate - BezierRadius);
            path.AddBezier(
              Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Bottom + Inflate - BezierRadius,
              Rect.Left - Inflate + EdgeWidth - (BezierRadius - BezierYDist), Rect.Bottom + Inflate - BezierXDist,
              Rect.Left - Inflate + EdgeWidth - BezierXDist, Rect.Bottom + Inflate - BezierYDist,
              Rect.Left - Inflate + EdgeWidth, Rect.Bottom + Inflate);
            path.AddLine(
              Rect.Left - Inflate + EdgeWidth, Rect.Bottom + Inflate,
              Rect.Right + Inflate + EdgeWidth, Rect.Bottom + Inflate);
            path.AddBezier(
              Rect.Right + Inflate + EdgeWidth, Rect.Bottom + Inflate,
              Rect.Right + Inflate + EdgeWidth - BezierXDist, Rect.Bottom + Inflate - BezierYDist,
              Rect.Right + Inflate + EdgeWidth - (BezierRadius - BezierYDist), Rect.Bottom + Inflate - BezierXDist,
              Rect.Right + Inflate + EdgeWidth - BezierRadius, Rect.Bottom + Inflate - BezierRadius);
            path.AddLine(
              Rect.Right + Inflate + EdgeWidth - BezierRadius, Rect.Bottom + Inflate - BezierRadius,
              Rect.Right + Inflate + BezierRadius, Rect.Top + BezierRadius);
            path.AddBezier(
              Rect.Right + Inflate + BezierRadius, Rect.Top + BezierRadius,
              Rect.Right + Inflate + (BezierRadius - BezierYDist), Rect.Top + BezierXDist,
              Rect.Right + Inflate + BezierXDist, Rect.Top + BezierYDist,
              Rect.Right + Inflate, Rect.Top);
            path.AddLine(Rect.Right + Inflate, Rect.Top, Rect.Left - Inflate, Rect.Top);
          end
          else
          begin
            path.AddBezier(
              Rect.Right + Inflate, Rect.Top,
              Rect.Right + Inflate - BezierXDist, Rect.Top + BezierYDist,
              Rect.Right + Inflate - (BezierRadius - BezierYDist), Rect.Top + BezierXDist,
              Rect.Right + Inflate - BezierRadius, Rect.Top + BezierRadius);
            path.AddLine(
              Rect.Right + Inflate - BezierRadius, Rect.Top + BezierRadius,
              Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Bottom + Inflate - BezierRadius);
            path.AddBezier(
              Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Bottom + Inflate - BezierRadius,
              Rect.Right + Inflate - EdgeWidth + (BezierRadius - BezierYDist), Rect.Bottom + Inflate - BezierXDist,
              Rect.Right + Inflate - EdgeWidth + BezierXDist, Rect.Bottom + Inflate - BezierYDist,
              Rect.Right + Inflate - EdgeWidth, Rect.Bottom + Inflate);
            path.AddLine(
              Rect.Right + Inflate - EdgeWidth, Rect.Bottom + Inflate,
              Rect.Left - Inflate - EdgeWidth, Rect.Bottom + Inflate);
            path.AddBezier(
              Rect.Left + Inflate - EdgeWidth, Rect.Bottom + Inflate,
              Rect.Left + Inflate - EdgeWidth + BezierXDist, Rect.Bottom + Inflate - BezierYDist,
              Rect.Left + Inflate - EdgeWidth + (BezierRadius - BezierYDist), Rect.Bottom + Inflate - BezierXDist,
              Rect.Left + Inflate - EdgeWidth + BezierRadius, Rect.Bottom + Inflate - BezierRadius);
            path.AddLine(
              Rect.Left - Inflate - EdgeWidth + BezierRadius, Rect.Bottom + Inflate - BezierRadius,
              Rect.Left - Inflate - BezierRadius, Rect.Top + BezierRadius);
            path.AddBezier(
              Rect.Left - Inflate - BezierRadius, Rect.Top + BezierRadius,
              Rect.Left - Inflate - (BezierRadius - BezierYDist), Rect.Top + BezierXDist,
              Rect.Left - Inflate - BezierXDist, Rect.Top + BezierYDist,
              Rect.Left - Inflate, Rect.Top);
            path.AddLine(Rect.Left - Inflate, Rect.Top, Rect.Right + Inflate, Rect.Top);
          end;
        end;
    end;
  end;

var
  GradientRect, NavRect: TRect;
  path: IGPGraphicsPath;
  gradientBrush: IGPLinearGradientBrush;
  pen: IGPPen;
  gradientColorStart, gradientColorEnd, penColor: Cardinal;
  gradientAngle: Single;
  OldSmoothingMode: TGPSmoothingMode;
  LinearColors: TGPLinearColors;
begin
  NavRect := GetNavRect(StartNav);
  if ((FOverNavButton = -1) and StartNav) or
    ((FOverNavButton = 1) and not StartNav) then
  begin
    gradientColorStart := MakeGDIColor($FFFFFF);
    gradientColorEnd := MakeGDIColor($F6F6F6);
  end
  else
  begin
    gradientColorStart := MakeGDIColor($E7DACE);
    gradientColorEnd := MakeGDIColor($E7DACE);
  end;
  case TabPosition of
    mtpTop: GradientAngle := 90;
    mtpBottom: GradientAngle := 270;
    mtpLeft: GradientAngle := 180;
  else
    gradientAngle := 0;
  end;
  if ((FOverNavButton = -1) and StartNav) or
    ((FOverNavButton = 1) and not StartNav) then
    penColor := MakeGDIColor($C9AD91)
  else
    penColor := MakeGDIColor($D2B79D);
  pen := TGPPen.Create(penColor);
  path := TGPGraphicsPath.Create;
  GradientRect := NavRect;
  case TabPosition of
    mtpTop: Inc(GradientRect.Bottom, 3);
    mtpBottom: Dec(GradientRect.Top, 3);
    mtpLeft: Inc(GradientRect.Right, 3);
  else
    Dec(GradientRect.Left, 3);
  end;
  gradientBrush := TGPLinearGradientBrush.Create(RectToGPRect(GradientRect),
    TGPColor.Create($00, $00, $00, $00), TGPColor.Create($20, $00, $00, $00), gradientAngle);
  OldSmoothingMode := g.GetSmoothingMode;
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  //draw shadow
  MakeNavPath(NavRect, path, StartNav, 1);
  g.FillPath(gradientBrush, path);
  if ((FOverNavButton = -1) and StartNav) or
    ((FOverNavButton = 1) and not StartNav) then
  begin
    LinearColors[0] := TGPColor.Create($00, $00, $00, $00);
    LinearColors[1] := TGPColor.Create($10, $00, $00, $00);
    gradientBrush.SetLinearColors(LinearColors);
    MakeNavPath(NavRect, path, StartNav, 2);
    g.FillPath(gradientBrush, path);
  end;
  LinearColors[0] := TGPColor.Create(gradientColorStart);
  LinearColors[1] := TGPColor.Create(gradientColorEnd);
  gradientBrush.SetLinearColors(LinearColors);
  //draw gradient background
  MakeNavPath(NavRect, path, StartNav);
  g.FillPath(gradientBrush, path);
  // draw around line
  MakeNavPath(NavRect, path, StartNav);
  g.DrawPath(pen, path);
  // draw around highlight line
  MakeNavPath(NavRect, path, StartNav, -1);
  pen.SetColor(TGPColor.Create($40, $FF, $FF, $FF));
  g.DrawPath(pen, path);
  // draw arrow
  if ((FPressNavButton = -1) and StartNav) or
    ((FPressNavButton = 1) and not StartNav) then
  begin
    LinearColors[0] := MakeGDIColor($C9AD91);
    LinearColors[1] := MakeGDIColor($E7DACE);
    gradientBrush.SetLinearColors(LinearColors);
  end
  else
  begin
    LinearColors[0] := MakeGDIColor($FFFFFF);
    LinearColors[1] := MakeGDIColor($F6F6F6);
    gradientBrush.SetLinearColors(LinearColors);
  end;
  MakeArrowPath(NavRect, path, StartNav);
  g.FillPath(gradientBrush, path);
  // draw arrow line
  pen.SetColor(TGPColor.Create($A0, $00, $00, $00));
  g.DrawPath(pen, path);
  // draw shadow arrow
  pen.SetColor(TGPColor.Create($40, $00, $00, $00));
  MakeArrowPath(NavRect, path, StartNav, 1);
  g.DrawPath(pen, path);

  g.SetSmoothingMode(OldSmoothingMode);
end;

procedure TModernPageControl.DrawTab(const TabRect: TRect; g: IGPGraphics;
  TabSheet: TModernTabSheet; Selected: Boolean; TabIndex: Integer);



  procedure MakePath(const Rect: TRect; path: IGPGraphicsPath;
    CloseFigure, Extend: Boolean; Inflate: Integer = 0);
  const
    BezierRadius = 3;
    BezierXDist = 1;
    BezierYDist = 1;
  begin
    path.Reset;
    case TabPosition of
      mtpTop:
        begin
          path.AddBezier(
            Rect.Left - Inflate, Rect.Bottom,
            Rect.Left - Inflate + BezierXDist, Rect.Bottom - BezierYDist,
            Rect.Left - Inflate + (BezierRadius - BezierYDist), Rect.Bottom - BezierXDist,
            Rect.Left - Inflate + BezierRadius, Rect.Bottom - BezierRadius);
          path.AddLine(
            Rect.Left - Inflate + BezierRadius, Rect.Bottom - BezierRadius,
            Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Top - Inflate + BezierRadius);
          path.AddBezier(
            Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Top - Inflate + BezierRadius,
            Rect.Left - Inflate + EdgeWidth - (BezierRadius - BezierYDist), Rect.Top - Inflate + BezierXDist,
            Rect.Left - Inflate + EdgeWidth - BezierXDist, Rect.Top - Inflate + BezierYDist,
            Rect.Left - Inflate + EdgeWidth, Rect.Top - Inflate);
          path.AddLine(
            Rect.Left - Inflate + EdgeWidth, Rect.Top - Inflate,
            Rect.Right + Inflate - EdgeWidth, Rect.Top - Inflate);
          path.AddBezier(
            Rect.Right + Inflate - EdgeWidth, Rect.Top - Inflate,
            Rect.Right + Inflate - EdgeWidth + BezierXDist, Rect.Top - Inflate + BezierYDist,
            Rect.Right + Inflate - EdgeWidth + (BezierRadius - BezierYDist), Rect.Top - Inflate + BezierXDist,
            Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Top - Inflate + BezierRadius);
          path.AddLine(
            Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Top - Inflate + BezierRadius,
            Rect.Right + Inflate - BezierRadius, Rect.Bottom - BezierRadius);
          path.AddBezier(
            Rect.Right + Inflate - BezierRadius, Rect.Bottom - BezierRadius,
            Rect.Right + Inflate - (BezierRadius - BezierYDist), Rect.Bottom - BezierXDist,
            Rect.Right + Inflate - BezierXDist, Rect.Bottom - BezierYDist,
            Rect.Right + Inflate, Rect.Bottom);
          if Extend and CloseFigure then
          begin
            path.AddLine(Rect.Right + Inflate, Rect.Bottom, Rect.Right + Inflate, Rect.Bottom + 3);
            path.AddLine(Rect.Right + Inflate, Rect.Bottom + 3, Rect.Left - Inflate, Rect.Bottom + 3);
            path.AddLine(Rect.Left - Inflate, Rect.Bottom + 3, Rect.Left - Inflate, Rect.Bottom);
          end
          else if CloseFigure then
            path.AddLine(Rect.Right + Inflate, Rect.Bottom, Rect.Left - Inflate, Rect.Bottom);
        end;
      mtpBottom:
        begin
          path.AddBezier(
            Rect.Left - Inflate, Rect.Top,
            Rect.Left - Inflate + BezierXDist, Rect.Top + BezierYDist,
            Rect.Left - Inflate + (BezierRadius - BezierYDist), Rect.Top + BezierXDist,
            Rect.Left - Inflate + BezierRadius, Rect.Top + BezierRadius);
          path.AddLine(
            Rect.Left - Inflate + BezierRadius, Rect.Top + BezierRadius,
            Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Bottom + Inflate - BezierRadius);
          path.AddBezier(
            Rect.Left - Inflate + EdgeWidth - BezierRadius, Rect.Bottom + Inflate - BezierRadius,
            Rect.Left - Inflate + EdgeWidth - (BezierRadius - BezierYDist), Rect.Bottom + Inflate - BezierXDist,
            Rect.Left - Inflate + EdgeWidth - BezierXDist, Rect.Bottom + Inflate - BezierYDist,
            Rect.Left - Inflate + EdgeWidth, Rect.Bottom + Inflate);
          path.AddLine(
            Rect.Left - Inflate + EdgeWidth, Rect.Bottom + Inflate,
            Rect.Right + Inflate - EdgeWidth, Rect.Bottom + Inflate);
          path.AddBezier(
            Rect.Right + Inflate - EdgeWidth, Rect.Bottom + Inflate,
            Rect.Right + Inflate - EdgeWidth + BezierXDist, Rect.Bottom + Inflate - BezierYDist,
            Rect.Right + Inflate - EdgeWidth + (BezierRadius - BezierYDist), Rect.Bottom + Inflate - BezierXDist,
            Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Bottom + Inflate - BezierRadius);
          path.AddLine(
            Rect.Right + Inflate - EdgeWidth + BezierRadius, Rect.Bottom + Inflate - BezierRadius,
            Rect.Right + Inflate - BezierRadius, Rect.Top + BezierRadius);
          path.AddBezier(
            Rect.Right + Inflate - BezierRadius, Rect.Top + BezierRadius,
            Rect.Right + Inflate - (BezierRadius - BezierYDist), Rect.Top + BezierXDist,
            Rect.Right + Inflate - BezierXDist, Rect.Top + BezierYDist,
            Rect.Right + Inflate, Rect.Top);
          if Extend and CloseFigure then
          begin
            path.AddLine(Rect.Right + Inflate, Rect.Top, Rect.Right + Inflate, Rect.Top - 3);
            path.AddLine(Rect.Right + Inflate, Rect.Top - 3, Rect.Left - Inflate, Rect.Top - 3);
            path.AddLine(Rect.Left - Inflate, Rect.Top - 3, Rect.Left - Inflate, Rect.Top);
          end
          else if CloseFigure then
            path.AddLine(Rect.Right + Inflate, Rect.Top, Rect.Left - Inflate, Rect.Top);
        end;
    end;
  end;

var
  TextHeight, ImageIndex, TextOffset, CloseButtonWidth: Integer;
  Text: string;
  CloseButtonRect, TextRect, GradientRect: TRect;
  CloseButtonState: TModernButtonState;
  path: IGPGraphicsPath;
  gradientBrush: IGPLinearGradientBrush;
  pen: IGPPen;
  gradientColorStart, gradientColorEnd, penColor: Cardinal;
  gradientAngle: Single;
  OldSmoothingMode: TGPSmoothingMode;
  LinearColors: TGPLinearColors;
begin
  if Selected then
  begin
    gradientColorStart := MakeGDIColor($FFFFFF);
    gradientColorEnd := MakeGDIColor($F6F6F6);
  end
  else
  begin
    if FOverTabIndex = TabIndex then
    begin
      gradientColorStart := MakeGDIColor($F7EADE);
      gradientColorEnd := MakeGDIColor($F7EADE);
    end
    else
    begin
      gradientColorStart := MakeGDIColor($E7DACE);
      gradientColorEnd := MakeGDIColor($E7DACE);
    end;
  end;
  case TabPosition of
    mtpTop: GradientAngle := 90;
    mtpBottom: GradientAngle := 270;
    mtpLeft: GradientAngle := 180;
  else
    gradientAngle := 0;
  end;
  if Selected then
    penColor := MakeGDIColor($C9AD91)
  else
    penColor := MakeGDIColor($D2B79D);
  pen := TGPPen.Create(penColor);
  path := TGPGraphicsPath.Create;
  GradientRect := TabRect;
  case TabPosition of
    mtpTop: Inc(GradientRect.Bottom, 3);
    mtpBottom: Dec(GradientRect.Top, 3);
    mtpLeft: Inc(GradientRect.Right, 3);
  else
    Dec(GradientRect.Left, 3);
  end;
  gradientBrush := TGPLinearGradientBrush.Create(RectToGPRect(GradientRect),
    TGPColor.Create($00, $00, $00, $00), TGPColor.Create($20, $00, $00, $00), gradientAngle);
  OldSmoothingMode := g.GetSmoothingMode;
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  //draw shadow
  MakePath(TabRect, path, True, False, 1);
  g.FillPath(gradientBrush, path);
  if Selected then
  begin
    LinearColors[0] := TGPColor.Create($00, $00, $00, $00);
    LinearColors[1] := TGPColor.Create($10, $00, $00, $00);
    gradientBrush.SetLinearColors(LinearColors);
    MakePath(TabRect, path, True, False, 2);
    g.FillPath(gradientBrush, path);
  end;
    LinearColors[0] := TGPColor.Create(gradientColorStart);
    LinearColors[1] := TGPColor.Create(gradientColorEnd);
  gradientBrush.SetLinearColors(LinearColors);
  //draw gradient background
  MakePath(TabRect, path, True, Selected);
  g.FillPath(gradientBrush, path);
  // draw around line
  MakePath(TabRect, path, False, False);
  g.DrawPath(pen, path);
  // draw around highlight line
  MakePath(TabRect, path, False, False, -1);
  pen.SetColor(TGPColor.Create($40, $FF, $FF, $FF));
  g.DrawPath(pen, path);
  g.SetSmoothingMode(OldSmoothingMode);
  ImageIndex := TabSheet.ImageIndex;
  TextOffset := 16;
  if Assigned(FImages) then
  begin
    if Assigned(FGetImageIndex) then
      FGetImageIndex(Self, TabIndex, ImageIndex);
    Inc(TextOffset, FImages.Width + 4);
    FImages.Draw(FBuffer.Canvas, TabRect.Left + 16,
      TabRect.Top + (FTabHeight - FImages.Height) - (FTabHeight - FImages.Height) div 2, ImageIndex);
  end
  else
    Inc(TextOffset, 2);
  //draw close button
  CloseButtonWidth := 0;
  if (Selected or (FOverTabIndex = TabIndex)) and ShowCloseButton then
  begin
    CloseButtonRect := GetCloseButtonRect(TabRect);
    if FOverCloseButton <> TabIndex then
      CloseButtonState := mbsNormal
    else if FPressCloseButtonIndex = TabIndex then
      CloseButtonState := mbsPressed
    else
      CloseButtonState := mbsHot;
    CloseButtonWidth := CloseButtonRect.Right - CloseButtonRect.Left;
    DrawCloseButton(CloseButtonRect, g, CloseButtonState);
  end;
  //draw text
  Text := TabSheet.Caption;
  FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.Font.Color := Pages[TabIndex].Font.Color;
  TextHeight := FBuffer.Canvas.TextHeight(Text);
  TextRect := TabRect;
  TextRect.Top := TabRect.Top + (FTabHeight - TextHeight) div 2;
  Dec(TextRect.Right, CloseButtonWidth + ButtonRightOffset);
  Inc(TextRect.Left, TextOffset);
  DrawText(FBuffer.Canvas.Handle, PChar(Text), Length(Text), TextRect,
    DT_SINGLELINE or DT_NOPREFIX);
end;

procedure TModernPageControl.DrawTabs(g: IGPGraphics);
var
  I, SelectedIndex: Integer;
  TabRect: TRect;
  TabSheet: TModernTabSheet;
begin
  // draw nav button
  if FStartIndex > 0 then
    DrawNavButton(g, True);
  if FEndIndex < PageCount - 1 then
    DrawNavButton(g, False);
  // draw unselected tabs
  SelectedIndex := ActivePageIndex;
  for I := FEndIndex downto FStartIndex do
  begin
    if I = SelectedIndex then
      Continue;
    TabSheet := Pages[I];
    GetTabRect(I, TabRect);
    DrawTab(TabRect, g, TabSheet, False, I);
  end;
  // draw active tab
  if (SelectedIndex > -1) and (SelectedIndex >= FStartIndex)
    and (SelectedIndex <= FEndIndex) then
  begin
    I := SelectedIndex;
    TabSheet := Pages[I];
    GetTabRect(I, TabRect);
    DrawTab(TabRect, g, TabSheet, True, I);
  end;
end;

procedure TModernPageControl.PaintTabs;
const
  gradientDivisionHeight = 50;
var
  g: IGPGraphics;
  gradientBrush: IGPLinearGradientBrush;
  brush: IGPSolidBrush;
  path, firstPath, lastPath: IGPGraphicsPath;
  pen: IGPPen;
  AreaRect, TabsRect, FirstAreaRect, LastAreaRect: TRect;
  rcClip, rcTabBg: TRect;
  OldSmoothingMode: TGPSmoothingMode;
  gradientAngle: Single;
  bgColorBottom, bgColorMiddle, bgColorTop: Cardinal;
begin
  if (FBuffer.Width < Round(Width * 1.1)) or (FBuffer.Height < Round(Height * 1.1)) or
     (FBuffer.Width > Round(Width * 1.3)) or (FBuffer.Height > Round(Height * 1.3)) then
  begin
    FBuffer.Width := Round(Width * 1.2);
    FBuffer.Height := Round(Height * 1.2);
  end;
  rcClip := Canvas.ClipRect;
  TabsRect := GetTabsRect;
  rcTabBg := TabsRect;
  AreaRect := ClientRect;
  case TabPosition of
    mtpTop:
      begin
        Inc(rcTabBg.Bottom, 2);
        AreaRect.Top := TabsRect.Bottom;
        GradientAngle := 90;
        FirstAreaRect := AreaRect;
        Inc(FirstAreaRect.Top, gradientDivisionHeight);
        LastAreaRect := AreaRect;
        LastAreaRect.Bottom := FirstAreaRect.Top + 2;
      end;
    mtpBottom:
      begin
        Dec(rcTabBg.Top, 2);
        AreaRect.Bottom := TabsRect.Top;
        GradientAngle := 270;
        FirstAreaRect := AreaRect;
        Dec(FirstAreaRect.Bottom, gradientDivisionHeight);
        LastAreaRect := AreaRect;
        LastAreaRect.Top := FirstAreaRect.Bottom - 2;
      end;
    mtpLeft:
      begin
        Inc(rcTabBg.Right, 2);
        AreaRect.Left := TabsRect.Right;
        GradientAngle := 180;
        FirstAreaRect := AreaRect;
        Inc(FirstAreaRect.Left, gradientDivisionHeight);
        LastAreaRect := AreaRect;
        LastAreaRect.Right := FirstAreaRect.Left + 2;
      end;
  else
    Dec(rcTabBg.Left, 2);
    AreaRect.Right := TabsRect.Left;
    gradientAngle := 0;
    FirstAreaRect := AreaRect;
    Dec(FirstAreaRect.Right, gradientDivisionHeight);
    LastAreaRect := AreaRect;
    LastAreaRect.Left := FirstAreaRect.Right - 2;
  end;
  // draw background
  DrawParentBackground(Self, FBuffer.Canvas.Handle, rcTabBg);
  g := TGPGraphics.Create(FBuffer.Canvas.Handle);
  brush := TGPSolidBrush.Create(MakeGDIColor(Color));
  path := CreateRoundRectangle(AreaRect, 12, TabPosition);
  pen := TGPPen.Create(MakeGDIColor($9F9A96));
  bgColorMiddle := MakeGDIColor($DFDFDF);
  bgColorTop := MakeGDIColor($F6F6F6);
  OldSmoothingMode := g.GetSmoothingMode;
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  if (((rcClip.Bottom - rcClip.Top - 1) <> FTabHeight) and
    (FTabPosition in [mtpTop, mtpBottom])) or (((rcClip.Right - rcClip.Left - 1) <> FTabHeight) and
    (FTabPosition in [mtpLeft, mtpRight])) then
  begin
    bgColorBottom := MakeGDIColor($DFDFDF);
    gradientBrush := TGPLinearGradientBrush.Create(RectToGPRect(FirstAreaRect),
      bgColorMiddle, bgColorBottom, gradientAngle);
    //draw background
    firstPath := TGPGraphicsPath.Create;
    firstPath.AddLine(FirstAreaRect.Left, FirstAreaRect.Bottom, FirstAreaRect.Left, FirstAreaRect.Top);
    firstPath.AddLine(FirstAreaRect.Left, FirstAreaRect.Top, FirstAreaRect.Right, FirstAreaRect.Top);
    firstPath.AddLine(FirstAreaRect.Right, FirstAreaRect.Top, FirstAreaRect.Right, FirstAreaRect.Bottom);
    firstPath.AddLine(FirstAreaRect.Right, FirstAreaRect.Bottom, FirstAreaRect.Left, FirstAreaRect.Bottom);
    g.FillPath(gradientBrush, firstPath);
  end;
  gradientBrush := TGPLinearGradientBrush.Create(RectToGPRect(LastAreaRect),
    bgColorTop, bgColorMiddle, gradientAngle);
  lastPath := CreateRoundRectangle(LastAreaRect, 12, TabPosition);
  g.FillPath(gradientBrush, lastPath);
  g.DrawPath(pen, path);
  g.SetSmoothingMode(OldSmoothingMode);
  //Draw Tabs
  DrawTabs(g);
  Canvas.CopyRect(rcClip, FBuffer.Canvas, rcClip);
end;

procedure TModernPageControl.SetFocusedColor(Value: TColor);
begin
  if Value <> FFocusedColor then
  begin
    FFocusedColor := Value;
    Invalidate;
  end;
end;

procedure TModernPageControl.SetNormalColor(Value: TColor);
begin
  if Value <> FNormalColor then
  begin
    FNormalColor := Value;
    Invalidate;
  end;
end;

procedure TModernPageControl.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TModernPageControl.DoPageChange(Index, PrevTabIndex: Integer);
begin
  if Assigned(FOnPageChange) then
    FOnPageChange(Self, Index, PrevTabIndex);
end;

function TModernPageControl.GetTabsWidth(R: TRect; Index: Integer;
  var aTabOffset, aTabWidth, aAvailableWidth: Integer; var CuttingCaption: Boolean;
  TestCaptionCut: Boolean): Integer;
var
  I, ImageWidth, TextWidth, ImageSpace, BtnCloseWidth, iTabWidth: Integer;
  StartNavWidth, EndNavWidth: Integer;
  Page: TModernTabSheet;
begin
  StartNavWidth := 0;
  CuttingCaption := False;
  if FStartIndex > 0 then
    StartNavWidth := NavWidth + NavSpacing;
  EndNavWidth := 0;
  if FEndIndex < PageCount - 1 then
    EndNavWidth := NavWidth + NavSpacing;
  if FFixedTabWidth and not TestCaptionCut then
  begin
    Result := FTabWidth * (FEndIndex - FStartIndex + 1) -
      (FEndIndex - FStartIndex) * UnionTabWidth + 2 * EdgeWidth;
    aTabWidth := FTabWidth;
    aTabOffset := StartNavWidth + TabsOffset + (FTabWidth - UnionTabWidth) * (Index - FStartIndex);
  end
  else
  begin
    Result := 0;
    ImageWidth := 0;
    ImageSpace := 0;
    aTabWidth := 0;
    BtnCloseWidth := 0;
    if ShowCloseButton then
      BtnCloseWidth := 12 + 10;
    if Assigned(FImages) then
    begin
      if TabPosition in [mtpTop, mtpBottom] then
        ImageWidth := FImages.Width
      else
        ImageWidth := FImages.Height;
      ImageSpace := 2;
    end;
    aTabOffset := TabsOffset + StartNavWidth;
    for I := FStartIndex to FEndIndex do
    begin
      Page := Pages[I];
      TextWidth := FBuffer.Canvas.TextWidth(Page.Caption);
      TextWidth := Max(TextWidth, 36); // don't allow tab too short
      iTabWidth := 4 + ImageWidth + ImageSpace + TextWidth + 6 + BtnCloseWidth + EdgeWidth * 2;
      if iTabWidth > FTabWidth then // don't allow tab too large
        iTabWidth := FTabWidth;
      if I = Index then
      begin
        aTabOffset := TabsOffset + StartNavWidth + Result;
        aTabWidth := iTabWidth;
      end;
      Inc(Result, iTabWidth - EdgeWidth);
    end;
    Inc(Result, EdgeWidth);
  end;
  aAvailableWidth := (R.Right - R.Left) - 2 * TabsOffset - StartNavWidth - EndNavWidth;
  if Result > aAvailableWidth then
  begin
    if TestCaptionCut then
      CuttingCaption := True;
    Result := aAvailableWidth;
    if (FEndIndex - FStartIndex + 1) > 0 then
      aTabWidth := (aAvailableWidth + ((FEndIndex - FStartIndex) * UnionTabWidth)) div (FEndIndex - FStartIndex + 1)
    else
      aTabWidth := (aAvailableWidth + ((FEndIndex - FStartIndex) * UnionTabWidth));
    aTabOffset := TabsOffset + StartNavWidth + (aTabWidth - UnionTabWidth) * (Index - FStartIndex);
  end;
end;

procedure TModernPageControl.GetTabRect(Index: Integer; var R: TRect;
  NoChangeOffset: Boolean);
var
  TabOffset, AllTabsWidth, aTabWidth, aAvailableWidth: Integer;
  SaveTabOffset: Integer;
  DragTabCenter: TPoint;
  DragTabRect: TRect;
  TabSheet: TModernTabSheet;
  StartNavWidth, EndNavWidth: Integer;
  CuttingCaption: Boolean;
begin
  R := GetTabsRect;
  if TabPosition = mtpBottom then
  begin
    Dec(R.Bottom, 1);
    Dec(R.Top, 1);
  end;
  StartNavWidth := 0;
  if FStartIndex > 0 then
    StartNavWidth := NavWidth + NavSpacing;
  EndNavWidth := 0;
  if FEndIndex < PageCount - 1 then
    EndNavWidth := NavWidth + NavSpacing;
  AllTabsWidth := GetTabsWidth(R, Index, TabOffset, aTabWidth, aAvailableWidth, CuttingCaption, False);
  if TabPosition in [mtpTop, mtpBottom] then
  begin
    TabSheet := Pages[Index];
    if (mpsDragging in FPageState) and not NoChangeOffset then
    begin
      if (Index = FDragTabIndex) then
      begin
        SaveTabOffset := TabOffset;
        if not FImediateEndDrag then
        begin
          Inc(TabOffset, FDragMousePoint.X - FDragStartPoint.X);
          // dragging tab out of tabs area
          if TabOffset < (R.Left + TabsOffset) then
            TabOffset := (R.Left + TabsOffset)
          else if (TabOffset + aTabWidth) > (TabsOffset + AllTabsWidth + StartNavWidth + EndNavWidth) then
            TabOffset := TabsOffset + StartNavWidth + AllTabsWidth + EndNavWidth - aTabWidth;
        end
        else
          TabOffset := TabSheet.FTargetOffset;
      end // calc offset of others tabs
      else
      begin
        GetTabRect(FDragTabIndex, DragTabRect);
        DragTabCenter := GetCenterOfDraggingTab(DragTabRect);
        SaveTabOffset := TabOffset;
        if (Index < FDragTabIndex) and (DragTabCenter.X < (TabOffset + aTabWidth) - MinDistAfterTab) then
          Inc(TabOffset, DragTabRect.Right - DragTabRect.Left - UnionTabWidth)
        else if (Index > FDragTabIndex) and (DragTabCenter.X > TabOffset + MinDistAfterTab) then
          Dec(TabOffset, DragTabRect.Right - DragTabRect.Left - UnionTabWidth);
      end;
      // smooth dragging
      if (TabSheet.FTargetOffset <> TabOffset) and ((Index <> FDragTabIndex) or FImediateEndDrag) then
      begin
        TabSheet.FTargetOffset := TabOffset;
        TabSheet.FStartTime := GetTickCount;
        if TabSheet.FAnimationStage = tasShowing then
        begin
          TabSheet.FAnimationStage := tasMoving;
          TabSheet.FVirtualOffset := SaveTabOffset;
        end;
      end;
      if TabSheet.FAnimationStage = tasMoving then
      begin
        if (TabOffset = SaveTabOffset) and (TabSheet.FTargetOffset = TabSheet.FVirtualOffset) then
          TabSheet.FAnimationStage := tasShowing;
        TabOffset := TabSheet.FVirtualOffset;
      end;
    end
    else if (TabSheet.FAnimationStage = tasMoving) and not NoChangeOffset then
      TabSheet.FAnimationStage := tasShowing;
    if (TabSheet.FAnimationStage = tasCreating) then
    begin
      if (mpsCreating in FPageState) and not NoChangeOffset then
      begin
        if (TabSheet.FVirtualWidth = aTabWidth) then
          TabSheet.FAnimationStage := tasShowing
        else
          aTabWidth := TabSheet.FVirtualWidth;
      end
      else if not (mpsCreating in FPageState) then
        TabSheet.FAnimationStage := tasShowing;
    end;
    R.Left := TabOffset;
    R.Right := R.Left + aTabWidth;
  end
  else
  begin
    if (mpsDragging in FPageState) and (Index = FDragTabIndex) then
      Inc(TabOffset, FDragMousePoint.Y - FDragStartPoint.Y);
    R.Top := TabOffset;
    R.Bottom := R.Top + aTabWidth;
  end;
end;

function TModernPageControl.GetCloseButtonRect(const TabRect: TRect): TRect;
const
  ButtonHeight = 12;
var
  aTabHeight: Integer;
begin
  Result := TabRect;
  if TabPosition in [mtpTop, mtpBottom] then
  begin
    Dec(Result.Right, ButtonRightOffset);
    aTabHeight := Result.Bottom - Result.Top;
    Result.Left := Result.Right - ButtonHeight;
    Inc(Result.Top, (aTabHeight - ButtonHeight) - ((aTabHeight - ButtonHeight) div 2));
    Result.Bottom := Result.Top + ButtonHeight;
  end
  else
  begin
    Dec(Result.Bottom, ButtonRightOffset);
    aTabHeight := Result.Right - Result.Left;
    Result.Top := Result.Bottom - ButtonHeight;
    Inc(Result.Left, (aTabHeight - ButtonHeight) - ((aTabHeight - ButtonHeight) div 2));
    Result.Right := Result.Left + ButtonHeight;
  end;
end;

procedure TModernPageControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_ESCAPE) and (mpsDragging in FPageState) then
    EndDragging(True);
end;

procedure TModernPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Pt: TPoint;
  R: TRect;
begin
  inherited;
  I := IndexOfTabAt(X, Y);
  if (I >= 0) then
  begin
    if (Button = mbLeft) then
    begin
      FMouseDown := True;
      if not (mpsDragging in FPageState) then
      begin
        FDragStartPoint := Point(X, Y);
        FDragMousePoint := FDragStartPoint;
        FDragTabIndex := I;
      end;
      if ShowCloseButton then
      begin
        GetTabRect(I, R);
        R := GetCloseButtonRect(R);
        Pt := Point(X, Y);
        if PtInRect(R, Pt) then
        begin
          FPressCloseButtonIndex := I;
          InvalidateTabs;
        end
        else
          ActivePageIndex := I;
      end
      else
        ActivePageIndex := I;
    end;
  end
  else
  begin // check for nav button click
    I := DirectionOfNavAt(X, Y);
    if I <> 0 then
    begin
      FPressNavButton := I;
      InvalidateTabs;
    end;
  end;
end;

procedure TModernPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  R: TRect;
  Pt: TPoint;
  CanInvalidate: Boolean;
begin
  inherited;
  CanInvalidate := False;
  if CanDragTabs and FMouseDown and not (mpsDragging in FPageState) and (FPressCloseButtonIndex = -1)
    and (FDragTabIndex > -1) then
  begin
    case TabPosition of
      mtpTop, mtpBottom:
        begin
          if Abs(X - FDragStartPoint.X) > MinDistStartDrag then
            BeginDragging;
        end;
    else
      if Abs(Y - FDragStartPoint.Y) > MinDistStartDrag then
        BeginDragging;
    end;
  end;
  if (mpsDragging in FPageState) and FMouseDown then
  begin
    FDragMousePoint := Point(X, Y);
    // update tab contents
    Application.ProcessMessages;
    InvalidateTabs;
    Exit;
  end;
  I := IndexOfTabAt(X, Y);
  if (I >= 0) then
  begin
    if (I <> FOverTabIndex) then
    begin
      if (I <> TabIndex) or (FOverTabIndex <> - 1) then
        CanInvalidate := True;
      FOverTabIndex := I;
    end;
    if ShowCloseButton then
    begin
      GetTabRect(I, R);
      R := GetCloseButtonRect(R);
      Pt := Point(X, Y);
      if PtInRect(R, Pt) then
      begin
        if FOverCloseButton <> I then
          CanInvalidate := True;
        FOverCloseButton := I;
      end
      else
      begin
        if FOverCloseButton > -1 then
          CanInvalidate := True;
        FOverCloseButton := -1;
      end;
    end;
    if FOverNavButton <> 0 then
    begin
      FOverNavButton := 0;
      CanInvalidate := True;
    end;
  end
  else
  begin
    I := DirectionOfNavAt(X, Y);
    if I <> FOverNavButton then
    begin
      FOverNavButton := I;
      CanInvalidate := True;
    end;
    if ShowCloseButton and (FOverCloseButton > -1) then
      CanInvalidate := True;
    if (FOverTabIndex > -1) and (FOverTabIndex <> TabIndex) then
      CanInvalidate := True;
    FOverCloseButton := -1;
    FOverTabIndex := -1;
  end;
  if CanInvalidate then
    InvalidateTabs;
end;

procedure TModernPageControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Pt: TPoint;
  R: TRect;
  aTabOffset, aTabWidth, aAvailableWidth: Integer;
  CuttingCaption: boolean;
  CanInvalidate: Boolean;
begin
  inherited;
  I := IndexOfTabAt(X, Y);
  FMouseDown := False;
  if (mpsDragging in FPageState) then
  begin
    EndDragging(False);
    Exit;
  end;
  CanInvalidate := False;
  if ShowCloseButton and (I >= 0) then
  begin
    GetTabRect(I, R);
    R := GetCloseButtonRect(R);
    Pt := Point(X, Y);
    if PtInRect(R, Pt) and (FPressCloseButtonIndex = I) then
    begin
      FPressCloseButtonIndex := -1;
      InvalidateTabs;
      ClosePage(I);
    end;
  end
  else
  begin
    // check for nav button click
    I := DirectionOfNavAt(X, Y);
    if (I = FPressNavButton) and (I <> 0) then
    begin
      if I < 0 then
      begin
        Dec(FStartIndex);
        Dec(FEndIndex);
      end
      else
      begin
        Inc(FStartIndex);
        Inc(FEndIndex);
      end;
      R := GetTabsRect;
      GetTabsWidth(R, FStartIndex, aTabOffset, aTabWidth, aAvailableWidth,
        CuttingCaption, True);
      if CuttingCaption then
      begin
        if FStartIndex < FEndIndex then
          Dec(FEndIndex);
      end
      else
      begin
        if FEndIndex < PageCount - 1 then
          Inc(FEndIndex);
        GetTabsWidth(R, FStartIndex, aTabOffset, aTabWidth, aAvailableWidth,
          CuttingCaption, True);
        if CuttingCaption then
          Dec(FEndIndex);
      end;
      CanInvalidate := True;
    end;
  end;
  if FPressNavButton <> 0 then
  begin
    FPressNavButton := 0;
    CanInvalidate := True;
  end;
  if ShowCloseButton and (FPressCloseButtonIndex > -1) then
  begin
    FPressCloseButtonIndex := -1;
    CanInvalidate := True;
  end;
  if CanInvalidate then
    InvalidateTabs;
end;

function TModernPageControl.GetCenterOfDraggingTab(const TabRect: TRect): TPoint;
begin
  Result.X := TabRect.Left + (TabRect.Right - TabRect.Left) div 2;
  Result.Y := TabRect.Top + (TabRect.Bottom - TabRect.Top) div 2;
end;

procedure TModernPageControl.StartSmoothDragging;
begin
  if FSmoothTimer = 0 then
    FSmoothTimer := SetTimer(Handle, 10123, 1000 div tabsStepsPerSecond, nil);
end;

procedure TModernPageControl.EndSmoothDragging;
begin
  if FSmoothTimer <> 0 then
  begin
    KillTimer(Handle, 10123);
    FSmoothTimer := 0;
  end;
end;

procedure TModernPageControl.BeginDragging;
begin
  if (mpsDragging in FPageState) or (csDesigning in ComponentState) then
    Exit;
  Include(FPageState, mpsDragging);
  FImediateEndDrag := False;
  FImediateDragCancel := False;
  StartSmoothDragging;
end;

function TModernPageControl.GetDragDestinationIndex: Integer;
var
  TabRect, DragTabRect: TRect;
  I: Integer;
  DragTabCenter: TPoint;
begin
  Result := -1;
  if not (mpsDragging in FPageState) then
    Exit;
  GetTabRect(FDragTabIndex, DragTabRect);
  DragTabCenter := GetCenterOfDraggingTab(DragTabRect);
  for I := FStartIndex to FDragTabIndex - 1 do
  begin
    GetTabRect(I, TabRect, True);
    if DragTabCenter.X < TabRect.Right - MinDistAfterTab then
    begin
      Result := I;
      Exit;
    end;
  end;
  for I := FEndIndex downto FDragTabIndex + 1 do
  begin
    GetTabRect(I, TabRect, True);
    if DragTabCenter.X > TabRect.Left + MinDistAfterTab then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := FDragTabIndex;
end;

procedure TModernPageControl.EndDragging(Cancel: Boolean);
var
  DragDestIndex: Integer;
  TabSheet: TModernTabSheet;
  TabRect, DragTabRect: TRect;
begin
  if not (mpsDragging in FPageState) then
    Exit;
  if not FImediateEndDrag then
  begin
    if not Cancel then
      DragDestIndex := GetDragDestinationIndex
    else
      DragDestIndex := FDragTabIndex;
    GetTabRect(FDragTabIndex, DragTabRect);
    GetTabRect(DragDestIndex, TabRect, True);
    TabSheet := Pages[FDragTabIndex];
    case TabPosition of
      mtpTop, mtpBottom:
        begin
          if FDragTabIndex >= DragDestIndex then
            TabSheet.FTargetOffset := TabRect.Left
          else
            TabSheet.FTargetOffset := TabRect.Right - (DragTabRect.Right - DragTabRect.Left);
          TabSheet.FVirtualOffset := DragTabRect.Left;
        end;
    else
      if FDragTabIndex >= DragDestIndex then
        TabSheet.FTargetOffset := TabRect.Bottom
      else
        TabSheet.FTargetOffset := TabRect.Top - (DragTabRect.Top - DragTabRect.Bottom);
      TabSheet.FVirtualOffset := DragTabRect.Bottom;
    end;
    TabSheet.FAnimationStage := tasMoving;
    TabSheet.FStartTime := GetTickCount;
    FImediateDragCancel := Cancel;
    FImediateEndDrag := True;
    Exit;
  end;
  if not Cancel then
  begin
    DragDestIndex := GetDragDestinationIndex;
    if (DragDestIndex > -1) and (DragDestIndex <> FDragTabIndex) then
    begin
      Pages[FDragTabIndex].PageIndex := DragDestIndex;
      FOverTabIndex := -1;
    end;
  end;
  EndSmoothDragging;
  Exclude(FPageState, mpsDragging);
  FDragTabIndex := -1;
  InvalidateTabs;
end;

procedure TModernPageControl.UpdateTabsPositions;
var
  I, OffsetFromTime, Dist: Integer;
  TabSheet: TModernTabSheet;
  EllapsedTime: Cardinal;
  CanInvalidate, AllZeroDist: Boolean;
begin
  CanInvalidate := False;
  AllZeroDist := True;
  for I := 0 to PageCount - 1 do
  begin
    TabSheet := Pages[I];
    if (TabSheet.FAnimationStage = tasMoving) and (TabSheet.FVirtualOffset <> TabSheet.FTargetOffset) then
    begin
      EllapsedTime := GetTickCount - TabSheet.FStartTime;
      Dist := Abs(TabSheet.FVirtualOffset - TabSheet.FTargetOffset);
      if Dist > 0 then
        AllZeroDist := False;
      // with acceleration
      OffsetFromTime := Round(EllapsedTime * (Dist / tabsMovingFactor));
      // constant velocity
      //OffsetFromTime := Round(EllapsedTime * tabsMovingVelocity / 16);
      //TabSheet.FStartTime := GetTickCount;
      if OffsetFromTime > Dist then
        OffsetFromTime := Dist;
      if TabSheet.FVirtualOffset < TabSheet.FTargetOffset then
        Inc(TabSheet.FVirtualOffset, OffsetFromTime)
      else
        Dec(TabSheet.FVirtualOffset, OffsetFromTime);
      if OffsetFromTime > 0 then
        CanInvalidate := True;
    end
    else if (TabSheet.FAnimationStage = tasCreating) then
    begin
      EllapsedTime := GetTickCount - TabSheet.FStartTime;
      Dist := Abs(TabSheet.FTargetWidht - TabSheet.FVirtualWidth);
      if Dist > 0 then
        AllZeroDist := False;
      // with acceleration
      OffsetFromTime := Round(EllapsedTime * (Dist / tabsMovingFactor));
      // constant velocity
      //OffsetFromTime := Round(EllapsedTime * tabsMovingVelocity / 16);
      //TabSheet.FStartTime := GetTickCount;
      if OffsetFromTime > Dist then
        OffsetFromTime := Dist;
      Inc(TabSheet.FVirtualWidth, OffsetFromTime);
      if OffsetFromTime > 0 then
        CanInvalidate := True;
    end;
  end;
  if CanInvalidate then
    InvalidateTabs
  else if FImediateEndDrag and (mpsDragging in FPageState) and AllZeroDist then
    EndDragging(FImediateDragCancel)
  else if ((mpsCreating in FPageState) or (mpsDeleting in FPageState)) and AllZeroDist then
  begin
    Exclude(FPageState, mpsCreating);
    Exclude(FPageState, mpsDeleting);
    EndSmoothDragging;
    InvalidateTabs;
  end;
end;

procedure TModernPageControl.ClosePage(Index: Integer);
var
  CanClose: Boolean;
  Page: TModernTabSheet;
begin
  CanClose := False;
  if Assigned(FOnClose) then
    FOnClose(Self, Index, CanClose);
  if CanClose then
  begin
    Page := Pages[Index];
    RemovePage(Pages[Index]);
    Page.Free;
  end;
end;

procedure TModernPageControl.CloseActiveTab;
begin
  if ActivePage <> nil then
    ClosePage(ActivePageIndex);
end;

procedure TModernPageControl.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      PaintTabs;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TModernPageControl.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TModernPageControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TModernPageControl.CMDockClient(var Message: TCMDockClient);
var
  DockCtl: TControl;
begin
  Message.Result := 0;
  FNewDockSheet := TModernTabSheet.Create(Self);
  try
    try
      DockCtl := Message.DockSource.Control;
      if DockCtl is TCustomForm then
        FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
      FNewDockSheet.PageControl := Self;
      DockCtl.Dock(Self, Message.DockSource.DockRect);
    except
      FNewDockSheet.Free;
      raise;
    end;
    ActivePage := FNewDockSheet;
    DockCtl.Align := alClient;
  finally
    FNewDockSheet := nil;
  end;
end;

procedure TModernPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TModernTabSheet;
begin
  Message.Result := 0;
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
  begin
    FUndockingPage := Page;
    Message.Client.Align := alNone;
  end;
end;

procedure TModernPageControl.CMDialogKey(var Message: TCMDialogKey);
var
  Form: TCustomForm;
begin
  inherited;
  if Message.Result <> 0 then
    Exit;
  Form := GetParentForm(Self);
  if (Form <> nil) and ContainsControl(Form.ActiveControl) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end;
end;

procedure TModernPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  NewTabIndex: Integer;
begin
  NewTabIndex := IndexOfTabAt(Message.Pos.X, Message.Pos.Y);
  if (NewTabIndex >= 0) and (NewTabIndex <> TabIndex) then
    Message.Result := 1;
end;

procedure TModernPageControl.CMMouseLeave(var Message: TMessage);
var
  CanInvalidate: Boolean;
begin
  inherited;
  CanInvalidate := ((FOverCloseButton > -1) and ShowCloseButton) or
    ((FOverTabIndex > -1) and (FOverTabIndex <> ActivePageIndex)) or (FOverNavButton <> 0);
  FOverCloseButton := -1;
  FOverTabIndex := -1;
  FOverNavButton := 0;
  if CanInvalidate then
    InvalidateTabs;
end;

procedure TModernPageControl.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Message.TimerID = 10123 then
    UpdateTabsPositions;
end;

procedure TModernPageControl.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TModernPageControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FMouseDown := False;
  if (mpsDragging in FPageState) then
    EndDragging(True);
end;

procedure TModernPageControl.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if FParentColor and (Parent <> nil) then
    Color := TGroupBox(Parent).Color;
end;

procedure TModernPageControl.Resize;
var
  R: TRect;
  aActiveIndex, aTabOffset, aTabWidth, aAvailableWidth: Integer;
  CuttingCaption: Boolean;
begin
  aActiveIndex := ActivePageIndex;
  R := GetTabsRect;
  repeat
    GetTabsWidth(R, 0, aTabOffset, aTabWidth, aAvailableWidth, CuttingCaption, True);
    if not CuttingCaption and not ((FStartIndex = 0) and (FEndIndex = PageCount - 1)) then
    begin
      if ((aActiveIndex < 0) or (aActiveIndex > FStartIndex)) and (FStartIndex > 0) then
        Dec(FStartIndex)
      else if ((aActiveIndex < 0) or (aActiveIndex < FEndIndex)) and (FEndIndex < PageCount - 1) then
        Inc(FEndIndex)
      else if (FStartIndex > 0) then
        Dec(FStartIndex)
      else if (FEndIndex < PageCount - 1) then
        Inc(FEndIndex)
      else
        Break;
    end;
  until CuttingCaption or ((FStartIndex = 0) and (FEndIndex = PageCount - 1));
  repeat
    GetTabsWidth(R, 0, aTabOffset, aTabWidth, aAvailableWidth, CuttingCaption, True);
    if CuttingCaption then
    begin
      if ((aActiveIndex < 0) or (aActiveIndex > FStartIndex)) and (FStartIndex < FEndIndex) then
        Inc(FStartIndex)
      else if ((aActiveIndex < 0) or (aActiveIndex < FEndIndex)) and (FEndIndex > FStartIndex) then
        Dec(FEndIndex)
      else if (FStartIndex < FEndIndex) then
        Inc(FStartIndex)
      else if (FEndIndex > FStartIndex) then
        Dec(FEndIndex)
      else
        Break;
    end;
  until not CuttingCaption or (FEndIndex - FStartIndex = 0);
  Invalidate;
end;

procedure TModernPageControl.SetParentColor(Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if Parent <> nil then
      Perform(CM_PARENTCOLORCHANGED, 0, 0);
  end;
end;

procedure TModernPageControl.SetFixedTabWidth(Value: Boolean);
begin
  if Value <> FFixedTabWidth then
  begin
    FFixedTabWidth := Value;
    InvalidateTabs;
  end;
end;

procedure TModernPageControl.LockAnimation;
begin
  Inc(FAnimationLockCount);
end;

procedure TModernPageControl.UnlockAnimation;
begin
  if FAnimationLockCount = 0 then
    Exit;
  Dec(FAnimationLockCount);
end;

procedure TModernPageControl.CancelDragging;
begin
  if IsDragging then
    EndDragging(True);
end;

function TModernPageControl.IsDragging: Boolean;
begin
  Result := mpsDragging in FPageState;
end;

end.

