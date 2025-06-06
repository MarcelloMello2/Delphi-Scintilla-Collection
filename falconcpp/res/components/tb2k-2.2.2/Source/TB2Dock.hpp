// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tb2dock.pas' rev: 21.00

#ifndef Tb2dockHPP
#define Tb2dockHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Inifiles.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Types.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tb2dock
{
//-- type declarations -------------------------------------------------------
typedef Forms::TCustomForm TTBCustomForm;

#pragma option push -b-
enum TTBDockBoundLinesValues { blTop, blBottom, blLeft, blRight };
#pragma option pop

typedef Set<TTBDockBoundLinesValues, blTop, blRight>  TTBDockBoundLines;

#pragma option push -b-
enum TTBDockPosition { dpTop, dpBottom, dpLeft, dpRight };
#pragma option pop

#pragma option push -b-
enum TTBDockType { dtNotDocked, dtFloating, dtTopBottom, dtLeftRight };
#pragma option pop

typedef Set<TTBDockPosition, dpTop, dpRight>  TTBDockableTo;

class DELPHICLASS TTBCustomDockableWindow;
typedef void __fastcall (__closure *TTBInsertRemoveEvent)(System::TObject* Sender, bool Inserting, TTBCustomDockableWindow* Bar);

typedef void __fastcall (__closure *TTBRequestDockEvent)(System::TObject* Sender, TTBCustomDockableWindow* Bar, bool &Accept);

class DELPHICLASS TTBDock;
class DELPHICLASS TTBBasicBackground;
class PASCALIMPLEMENTATION TTBDock : public Controls::TCustomControl
{
	typedef Controls::TCustomControl inherited;
	
private:
	TTBDockPosition FPosition;
	bool FAllowDrag;
	TTBDockBoundLines FBoundLines;
	TTBBasicBackground* FBackground;
	bool FBkgOnToolbars;
	bool FFixAlign;
	bool FCommitNewPositions;
	bool FLimitToOneRow;
	TTBInsertRemoveEvent FOnInsertRemoveBar;
	TTBRequestDockEvent FOnRequestDock;
	int FDisableArrangeToolbars;
	bool FArrangeToolbarsNeeded;
	int FNonClientWidth;
	int FNonClientHeight;
	void __fastcall SetAllowDrag(bool Value);
	void __fastcall SetBackground(TTBBasicBackground* Value);
	void __fastcall SetBackgroundOnToolbars(bool Value);
	void __fastcall SetBoundLines(TTBDockBoundLines Value);
	void __fastcall SetFixAlign(bool Value);
	void __fastcall SetPosition(TTBDockPosition Value);
	int __fastcall GetToolbarCount(void);
	TTBCustomDockableWindow* __fastcall GetToolbars(int Index);
	void __fastcall BackgroundChanged(System::TObject* Sender);
	void __fastcall ChangeDockList(const bool Insert, const TTBCustomDockableWindow* Bar);
	void __fastcall CommitPositions(void);
	void __fastcall DrawNCArea(const bool DrawToDC, const HDC ADC, const HRGN Clip);
	int __fastcall GetDesignModeRowOf(const int XY);
	void __fastcall RelayMsgToFloatingBars(Messages::TMessage &Message);
	void __fastcall ToolbarVisibilityChanged(const TTBCustomDockableWindow* Bar, const bool ForceRemove);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMMove(Messages::TWMMove &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TMessage &Message);
	MESSAGE void __fastcall WMPrint(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPrintClient(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSysCommand(Messages::TWMSysCommand &Message);
	
protected:
	Classes::TList* DockList;
	Classes::TList* DockVisibleList;
	virtual bool __fastcall Accepts(TTBCustomDockableWindow* ADockableWindow);
	virtual void __fastcall AlignControls(Controls::TControl* AControl, Types::TRect &Rect);
	void __fastcall ChangeWidthHeight(const int NewWidth, const int NewHeight);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall DrawBackground(HDC DC, const Types::TRect &DrawRect);
	DYNAMIC HPALETTE __fastcall GetPalette(void);
	bool __fastcall HasVisibleToolbars(void);
	void __fastcall InvalidateBackgrounds(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall SetParent(Controls::TWinControl* AParent);
	bool __fastcall ToolbarVisibleOnDock(const TTBCustomDockableWindow* AToolbar);
	virtual void __fastcall Paint(void);
	virtual bool __fastcall UsingBackground(void);
	__property bool ArrangeToolbarsNeeded = {read=FArrangeToolbarsNeeded, write=FArrangeToolbarsNeeded, nodefault};
	__property int DisableArrangeToolbars = {read=FDisableArrangeToolbars, write=FDisableArrangeToolbars, nodefault};
	
public:
	__fastcall virtual TTBDock(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBDock(void);
	virtual void __fastcall ArrangeToolbars(void);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	int __fastcall GetCurrentRowSize(const int Row, bool &AFullSize);
	int __fastcall GetHighestRow(const bool HighestEffective);
	int __fastcall GetMinRowSize(const int Row, const TTBCustomDockableWindow* ExcludeControl);
	__property bool CommitNewPositions = {read=FCommitNewPositions, write=FCommitNewPositions, nodefault};
	__property int NonClientWidth = {read=FNonClientWidth, nodefault};
	__property int NonClientHeight = {read=FNonClientHeight, nodefault};
	__property int ToolbarCount = {read=GetToolbarCount, nodefault};
	__property TTBCustomDockableWindow* Toolbars[int Index] = {read=GetToolbars};
	
__published:
	__property bool AllowDrag = {read=FAllowDrag, write=SetAllowDrag, default=1};
	__property TTBBasicBackground* Background = {read=FBackground, write=SetBackground};
	__property bool BackgroundOnToolbars = {read=FBkgOnToolbars, write=SetBackgroundOnToolbars, default=1};
	__property TTBDockBoundLines BoundLines = {read=FBoundLines, write=SetBoundLines, default=0};
	__property Color = {default=-16777201};
	__property bool FixAlign = {read=FFixAlign, write=SetFixAlign, default=0};
	__property bool LimitToOneRow = {read=FLimitToOneRow, write=FLimitToOneRow, default=0};
	__property PopupMenu;
	__property TTBDockPosition Position = {read=FPosition, write=SetPosition, default=0};
	__property Visible = {default=1};
	__property OnContextPopup;
	__property TTBInsertRemoveEvent OnInsertRemoveBar = {read=FOnInsertRemoveBar, write=FOnInsertRemoveBar};
	__property OnMouseActivate;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property TTBRequestDockEvent OnRequestDock = {read=FOnRequestDock, write=FOnRequestDock};
	__property OnResize;
public:
	/* TWinControl.CreateParented */ inline __fastcall TTBDock(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	
};


#pragma option push -b-
enum TTBToolWindowNCRedrawWhatElement { twrdBorder, twrdCaption, twrdCloseButton };
#pragma option pop

typedef Set<TTBToolWindowNCRedrawWhatElement, twrdBorder, twrdCloseButton>  TTBToolWindowNCRedrawWhat;

typedef TMetaClass* TTBFloatingWindowParentClass;

class DELPHICLASS TTBFloatingWindowParent;
class PASCALIMPLEMENTATION TTBFloatingWindowParent : public Forms::TCustomForm
{
	typedef Forms::TCustomForm inherited;
	
private:
	bool FCloseButtonDown;
	TTBCustomDockableWindow* FDockableWindow;
	Forms::TCustomForm* FParentForm;
	bool FShouldShow;
	void __fastcall CallRecreateWnd(void);
	Types::TRect __fastcall GetCaptionRect(const bool AdjustForBorder, const bool MinusCloseButton);
	Types::TRect __fastcall GetCloseButtonRect(const bool AdjustForBorder);
	void __fastcall SetCloseButtonState(bool Pushed);
	void __fastcall RedrawNCArea(const TTBToolWindowNCRedrawWhat RedrawWhat);
	HIDESBASE MESSAGE void __fastcall CMShowingChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMActivate(Messages::TWMActivate &Message);
	HIDESBASE MESSAGE void __fastcall WMClose(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMGetMinMaxInfo(Messages::TWMGetMinMaxInfo &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseActivate(Messages::TWMMouseActivate &Message);
	HIDESBASE MESSAGE void __fastcall WMMove(Messages::TWMMove &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Messages::TWMNCHitTest &Message);
	MESSAGE void __fastcall WMNCLButtonDblClk(Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCLButtonDown(Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCRButtonUp(Messages::TWMNCHitMessage &Message);
	MESSAGE void __fastcall WMPrint(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPrintClient(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall AlignControls(Controls::TControl* AControl, Types::TRect &Rect);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	DYNAMIC void __fastcall DrawNCArea(const bool DrawToDC, const HDC ADC, const HRGN Clip, TTBToolWindowNCRedrawWhat RedrawWhat);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	__property TTBCustomDockableWindow* DockableWindow = {read=FDockableWindow};
	__property bool CloseButtonDown = {read=FCloseButtonDown, nodefault};
	
public:
	__property Forms::TCustomForm* ParentForm = {read=FParentForm};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TTBFloatingWindowParent(Classes::TComponent* AOwner) : Forms::TCustomForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TTBFloatingWindowParent(Classes::TComponent* AOwner, int Dummy) : Forms::TCustomForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TTBFloatingWindowParent(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TTBFloatingWindowParent(HWND ParentWindow) : Forms::TCustomForm(ParentWindow) { }
	
};


typedef void __fastcall (__closure *TTBDockChangingEvent)(System::TObject* Sender, bool Floating, TTBDock* DockingTo);

#pragma option push -b-
enum TTBDragHandleStyle { dhDouble, dhNone, dhSingle };
#pragma option pop

#pragma option push -b-
enum TTBDockMode { dmCanFloat, dmCannotFloat, dmCannotFloatOrChangeDocks };
#pragma option pop

#pragma option push -b-
enum TTBFloatingMode { fmOnTopOfParentForm, fmOnTopOfAllForms };
#pragma option pop

#pragma option push -b-
enum TTBSizeHandle { twshLeft, twshRight, twshTop, twshTopLeft, twshTopRight, twshBottom, twshBottomLeft, twshBottomRight };
#pragma option pop

typedef void * TTBPositionExtraData;

typedef int __fastcall (*TTBPositionReadIntProc)(const System::UnicodeString ToolbarName, const System::UnicodeString Value, const int Default, const void * ExtraData);

typedef System::UnicodeString __fastcall (*TTBPositionReadStringProc)(const System::UnicodeString ToolbarName, const System::UnicodeString Value, const System::UnicodeString Default, const void * ExtraData);

typedef void __fastcall (*TTBPositionWriteIntProc)(const System::UnicodeString ToolbarName, const System::UnicodeString Value, const int Data, const void * ExtraData);

typedef void __fastcall (*TTBPositionWriteStringProc)(const System::UnicodeString ToolbarName, const System::UnicodeString Value, const System::UnicodeString Data, const void * ExtraData);

struct TTBReadPositionData
{
	
public:
	TTBPositionReadIntProc ReadIntProc;
	TTBPositionReadStringProc ReadStringProc;
	void *ExtraData;
};


struct TTBWritePositionData
{
	
public:
	TTBPositionWriteIntProc WriteIntProc;
	TTBPositionWriteStringProc WriteStringProc;
	void *ExtraData;
};


#pragma option push -b-
enum Tb2dock__3 { tbdsResizeEightCorner, tbdsResizeClipCursor };
#pragma option pop

typedef Set<Tb2dock__3, tbdsResizeEightCorner, tbdsResizeClipCursor>  TTBDockableWindowStyles;

#pragma option push -b-
enum TTBShrinkMode { tbsmNone, tbsmWrap, tbsmChevron };
#pragma option pop

class PASCALIMPLEMENTATION TTBCustomDockableWindow : public Controls::TCustomControl
{
	typedef Controls::TCustomControl inherited;
	
private:
	bool FAutoResize;
	bool FDblClickUndock;
	int FDockPos;
	int FDockRow;
	int FEffectiveDockPos;
	int FEffectiveDockRow;
	bool FDocked;
	TTBDock* FCurrentDock;
	TTBDock* FDefaultDock;
	TTBDock* FLastDock;
	int FCurrentSize;
	bool FFloating;
	Classes::TNotifyEvent FOnClose;
	Classes::TNotifyEvent FOnDockChanged;
	Classes::TNotifyEvent FOnMove;
	Classes::TNotifyEvent FOnRecreated;
	Classes::TNotifyEvent FOnRecreating;
	Classes::TNotifyEvent FOnVisibleChanged;
	Forms::TCloseQueryEvent FOnCloseQuery;
	TTBDockChangingEvent FOnDockChanging;
	TTBDockChangingEvent FOnDockChangingHidden;
	bool FActivateParent;
	bool FHideWhenInactive;
	bool FCloseButton;
	bool FCloseButtonWhenDocked;
	bool FFullSize;
	bool FResizable;
	bool FShowCaption;
	bool FStretch;
	bool FUseLastDock;
	Forms::TFormBorderStyle FBorderStyle;
	TTBDockMode FDockMode;
	TTBDragHandleStyle FDragHandleStyle;
	TTBDockableTo FDockableTo;
	TTBFloatingMode FFloatingMode;
	bool FSmoothDrag;
	TTBDockableWindowStyles FDockableWindowStyles;
	int FLastRowSize;
	bool FInsertRowBefore;
	int FUpdatingBounds;
	int FDisableArrange;
	int FDisableOnMove;
	int FHidden;
	bool FArrangeNeeded;
	bool FMoved;
	bool FInactiveCaption;
	Types::TPoint FFloatingPosition;
	Classes::TList* FDockForms;
	bool FSavedAtRunTime;
	bool FDragMode;
	bool FDragSplitting;
	bool FDragCanSplit;
	bool FSmoothDragging;
	bool FCloseButtonDown;
	bool FCloseButtonHover;
	TTBFloatingWindowParent* FFloatParent;
	int __fastcall GetNonClientWidth(void);
	int __fastcall GetNonClientHeight(void);
	bool __fastcall IsLastDockStored(void);
	bool __fastcall IsWidthAndHeightStored(void);
	void __fastcall SetAutoResize(bool Value);
	void __fastcall SetBorderStyle(Forms::TBorderStyle Value);
	void __fastcall SetCloseButton(bool Value);
	void __fastcall SetCloseButtonWhenDocked(bool Value);
	void __fastcall SetCurrentDock(TTBDock* Value);
	void __fastcall SetDefaultDock(TTBDock* Value);
	void __fastcall SetDockPos(int Value);
	void __fastcall SetDockRow(int Value);
	void __fastcall SetDragHandleStyle(TTBDragHandleStyle Value);
	void __fastcall SetFloating(bool Value);
	void __fastcall SetFloatingMode(TTBFloatingMode Value);
	void __fastcall SetFloatingPosition(const Types::TPoint &Value);
	void __fastcall SetFullSize(bool Value);
	void __fastcall SetLastDock(TTBDock* Value);
	void __fastcall SetResizable(bool Value);
	void __fastcall SetShowCaption(bool Value);
	void __fastcall SetStretch(bool Value);
	void __fastcall SetUseLastDock(bool Value);
	void __fastcall CancelNCHover(void);
	void __fastcall DrawDraggingOutline(const HDC DC, const Types::TRect &NewRect, const Types::TRect &OldRect, const bool NewDocking, const bool OldDocking);
	void __fastcall RedrawNCArea(void);
	void __fastcall SetCloseButtonState(bool Pushed);
	void __fastcall ShowNCContextMenu(const short PosX, const short PosY);
	void __fastcall Moved(void);
	bool __fastcall GetShowingState(void);
	void __fastcall UpdateCaptionState(void);
	void __fastcall UpdateTopmostFlag(void);
	void __fastcall UpdateVisibility(void);
	void __fastcall ReadSavedAtRunTime(Classes::TReader* Reader);
	void __fastcall WriteSavedAtRunTime(Classes::TWriter* Writer);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMShowingChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMVisibleChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Messages::TWMContextMenu &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMMove(Messages::TWMMove &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Messages::TWMNCHitTest &Message);
	MESSAGE void __fastcall WMNCMouseLeave(Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCMouseMove(Messages::TWMNCHitMessage &Message);
	MESSAGE void __fastcall WMNCLButtonDblClk(Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCLButtonDown(Messages::TWMNCHitMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TMessage &Message);
	MESSAGE void __fastcall WMNCRButtonUp(Messages::TWMNCHitMessage &Message);
	MESSAGE void __fastcall WMCaptureChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall WMPrint(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPrintClient(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Messages::TWMSetCursor &Message);
	
protected:
	__property bool ActivateParent = {read=FActivateParent, write=FActivateParent, default=1};
	__property bool AutoResize = {read=FAutoResize, write=SetAutoResize, default=1};
	__property Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property Color = {default=-16777201};
	__property bool CloseButton = {read=FCloseButton, write=SetCloseButton, default=1};
	__property bool CloseButtonDown = {read=FCloseButtonDown, nodefault};
	__property bool CloseButtonHover = {read=FCloseButtonHover, nodefault};
	__property bool CloseButtonWhenDocked = {read=FCloseButtonWhenDocked, write=SetCloseButtonWhenDocked, default=0};
	__property TTBDock* DefaultDock = {read=FDefaultDock, write=SetDefaultDock};
	__property TTBDockableTo DockableTo = {read=FDockableTo, write=FDockableTo, default=15};
	__property TTBDockableWindowStyles DockableWindowStyles = {read=FDockableWindowStyles, write=FDockableWindowStyles, nodefault};
	__property TTBDockMode DockMode = {read=FDockMode, write=FDockMode, default=0};
	__property TTBDragHandleStyle DragHandleStyle = {read=FDragHandleStyle, write=SetDragHandleStyle, default=2};
	__property TTBFloatingMode FloatingMode = {read=FFloatingMode, write=SetFloatingMode, default=0};
	__property bool FullSize = {read=FFullSize, write=SetFullSize, default=0};
	__property bool InactiveCaption = {read=FInactiveCaption, nodefault};
	__property bool HideWhenInactive = {read=FHideWhenInactive, write=FHideWhenInactive, default=1};
	__property bool Resizable = {read=FResizable, write=SetResizable, default=1};
	__property bool ShowCaption = {read=FShowCaption, write=SetShowCaption, default=1};
	__property bool SmoothDrag = {read=FSmoothDrag, write=FSmoothDrag, default=1};
	__property bool Stretch = {read=FStretch, write=SetStretch, default=0};
	__property bool UseLastDock = {read=FUseLastDock, write=SetUseLastDock, default=1};
	__property Classes::TNotifyEvent OnClose = {read=FOnClose, write=FOnClose};
	__property Forms::TCloseQueryEvent OnCloseQuery = {read=FOnCloseQuery, write=FOnCloseQuery};
	__property Classes::TNotifyEvent OnDockChanged = {read=FOnDockChanged, write=FOnDockChanged};
	__property TTBDockChangingEvent OnDockChanging = {read=FOnDockChanging, write=FOnDockChanging};
	__property TTBDockChangingEvent OnDockChangingHidden = {read=FOnDockChangingHidden, write=FOnDockChangingHidden};
	__property Classes::TNotifyEvent OnMove = {read=FOnMove, write=FOnMove};
	__property Classes::TNotifyEvent OnRecreated = {read=FOnRecreated, write=FOnRecreated};
	__property Classes::TNotifyEvent OnRecreating = {read=FOnRecreating, write=FOnRecreating};
	__property Classes::TNotifyEvent OnVisibleChanged = {read=FOnVisibleChanged, write=FOnVisibleChanged};
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC HPALETTE __fastcall GetPalette(void);
	virtual void __fastcall Loaded(void);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	DYNAMIC bool __fastcall PaletteChanged(bool Foreground);
	virtual void __fastcall SetParent(Controls::TWinControl* AParent);
	void __fastcall Arrange(void);
	virtual Types::TPoint __fastcall CalcNCSizes(void);
	virtual bool __fastcall CanDockTo(TTBDock* ADock);
	void __fastcall ChangeSize(int AWidth, int AHeight);
	DYNAMIC bool __fastcall ChildControlTransparent(Controls::TControl* Ctl);
	void __fastcall Close(void);
	virtual void __fastcall ControlExistsAtPos(const Types::TPoint &P, bool &ControlExists);
	virtual Types::TPoint __fastcall DoArrange(bool CanMoveControls, TTBDockType PreviousDockType, bool NewFloating, TTBDock* NewDock) = 0 ;
	DYNAMIC void __fastcall DoDockChangingHidden(bool NewFloating, TTBDock* DockingTo);
	void __fastcall DoubleClick(void);
	virtual void __fastcall DrawNCArea(const bool DrawToDC, const HDC ADC, const HRGN Clip);
	virtual void __fastcall GetBaseSize(Types::TPoint &ASize) = 0 ;
	virtual Types::TRect __fastcall GetDockedCloseButtonRect(bool LeftRight);
	DYNAMIC TTBFloatingWindowParentClass __fastcall GetFloatingWindowParentClass(void);
	virtual void __fastcall GetMinShrinkSize(int &AMinimumSize);
	virtual void __fastcall GetMinMaxSize(int &AMinClientWidth, int &AMinClientHeight, int &AMaxClientWidth, int &AMaxClientHeight);
	virtual TTBShrinkMode __fastcall GetShrinkMode(void);
	DYNAMIC void __fastcall InitializeOrdering(void);
	bool __fastcall IsAutoResized(void);
	DYNAMIC void __fastcall ResizeBegin(TTBSizeHandle SizeHandle);
	DYNAMIC void __fastcall ResizeEnd(void);
	DYNAMIC void __fastcall ResizeTrack(Types::TRect &Rect, const Types::TRect &OrigRect);
	DYNAMIC void __fastcall ResizeTrackAccept(void);
	virtual void __fastcall SizeChanging(const int AWidth, const int AHeight);
	__property int EffectiveDockPosAccess = {read=FEffectiveDockPos, write=FEffectiveDockPos, nodefault};
	__property int EffectiveDockRowAccess = {read=FEffectiveDockRow, write=FEffectiveDockRow, nodefault};
	
public:
	__property bool DblClickUndock = {read=FDblClickUndock, write=FDblClickUndock, default=1};
	__property bool Docked = {read=FDocked, nodefault};
	__property Canvas;
	__property TTBDock* CurrentDock = {read=FCurrentDock, write=SetCurrentDock, stored=false};
	__property int CurrentSize = {read=FCurrentSize, write=FCurrentSize, nodefault};
	__property int DockPos = {read=FDockPos, write=SetDockPos, default=-1};
	__property int DockRow = {read=FDockRow, write=SetDockRow, default=0};
	__property bool DragMode = {read=FDragMode, nodefault};
	__property bool DragSplitting = {read=FDragSplitting, nodefault};
	__property int EffectiveDockPos = {read=FEffectiveDockPos, nodefault};
	__property int EffectiveDockRow = {read=FEffectiveDockRow, nodefault};
	__property bool Floating = {read=FFloating, write=SetFloating, default=0};
	__property Types::TPoint FloatingPosition = {read=FFloatingPosition, write=SetFloatingPosition};
	__property TTBDock* LastDock = {read=FLastDock, write=SetLastDock, stored=IsLastDockStored};
	__property int NonClientWidth = {read=GetNonClientWidth, nodefault};
	__property int NonClientHeight = {read=GetNonClientHeight, nodefault};
	__fastcall virtual TTBCustomDockableWindow(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBCustomDockableWindow(void);
	DYNAMIC Classes::TComponent* __fastcall GetParentComponent(void);
	DYNAMIC bool __fastcall HasParent(void);
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
	void __fastcall AddDockForm(const Forms::TCustomForm* Form);
	void __fastcall AddDockedNCAreaToSize(Types::TPoint &S, const bool LeftRight);
	void __fastcall AddFloatingNCAreaToSize(Types::TPoint &S);
	void __fastcall BeginMoving(const int InitX, const int InitY);
	void __fastcall BeginSizing(const TTBSizeHandle ASizeHandle);
	void __fastcall BeginUpdate(void);
	DYNAMIC void __fastcall DoneReadingPositionData(const TTBReadPositionData &Data);
	void __fastcall EndUpdate(void);
	void __fastcall GetDockedNCArea(Types::TPoint &TopLeft, Types::TPoint &BottomRight, const bool LeftRight);
	virtual Types::TPoint __fastcall GetFloatingBorderSize(void);
	void __fastcall GetFloatingNCArea(Types::TPoint &TopLeft, Types::TPoint &BottomRight);
	bool __fastcall IsMovable(void);
	void __fastcall MoveOnScreen(const bool OnlyIfFullyOffscreen);
	DYNAMIC void __fastcall ReadPositionData(const TTBReadPositionData &Data);
	void __fastcall RemoveDockForm(const Forms::TCustomForm* Form);
	DYNAMIC void __fastcall WritePositionData(const TTBWritePositionData &Data);
	
__published:
	__property Height = {stored=IsWidthAndHeightStored};
	__property Width = {stored=IsWidthAndHeightStored};
public:
	/* TWinControl.CreateParented */ inline __fastcall TTBCustomDockableWindow(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TTBBasicBackground : public Classes::TComponent
{
	typedef Classes::TComponent inherited;
	
protected:
	virtual void __fastcall Draw(HDC DC, const Types::TRect &DrawRect) = 0 ;
	virtual HPALETTE __fastcall GetPalette(void) = 0 ;
	virtual void __fastcall RegisterChanges(Classes::TNotifyEvent Proc) = 0 ;
	virtual void __fastcall SysColorChanged(void) = 0 ;
	virtual void __fastcall UnregisterChanges(Classes::TNotifyEvent Proc) = 0 ;
	virtual bool __fastcall UsingBackground(void) = 0 ;
public:
	/* TComponent.Create */ inline __fastcall virtual TTBBasicBackground(Classes::TComponent* AOwner) : Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TTBBasicBackground(void) { }
	
};


class DELPHICLASS TTBBackground;
class PASCALIMPLEMENTATION TTBBackground : public TTBBasicBackground
{
	typedef TTBBasicBackground inherited;
	
private:
	Graphics::TBitmap* FBitmap;
	Graphics::TBitmap* FBitmapCache;
	Graphics::TColor FBkColor;
	Classes::TList* FNotifyList;
	bool FTransparent;
	void __fastcall BitmapChanged(System::TObject* Sender);
	void __fastcall SetBitmap(Graphics::TBitmap* Value);
	void __fastcall SetBkColor(Graphics::TColor Value);
	void __fastcall SetTransparent(bool Value);
	
protected:
	virtual void __fastcall Draw(HDC DC, const Types::TRect &DrawRect);
	virtual HPALETTE __fastcall GetPalette(void);
	virtual void __fastcall RegisterChanges(Classes::TNotifyEvent Proc);
	virtual void __fastcall SysColorChanged(void);
	virtual void __fastcall UnregisterChanges(Classes::TNotifyEvent Proc);
	virtual bool __fastcall UsingBackground(void);
	
public:
	__fastcall virtual TTBBackground(Classes::TComponent* AOwner);
	__fastcall virtual ~TTBBackground(void);
	
__published:
	__property Graphics::TBitmap* Bitmap = {read=FBitmap, write=SetBitmap};
	__property Graphics::TColor BkColor = {read=FBkColor, write=SetBkColor, default=-16777201};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TTBDockType __fastcall TBGetDockTypeOf(const TTBDock* Control, const bool Floating);
extern PACKAGE Forms::TCustomForm* __fastcall TBGetToolWindowParentForm(const TTBCustomDockableWindow* ToolWindow);
extern PACKAGE Forms::TCustomForm* __fastcall TBValidToolWindowParentForm(const TTBCustomDockableWindow* ToolWindow);
extern PACKAGE void __fastcall TBCustomLoadPositions(const Classes::TComponent* OwnerComponent, const TTBPositionReadIntProc ReadIntProc, const TTBPositionReadStringProc ReadStringProc, const void * ExtraData);
extern PACKAGE void __fastcall TBCustomSavePositions(const Classes::TComponent* OwnerComponent, const TTBPositionWriteIntProc WriteIntProc, const TTBPositionWriteStringProc WriteStringProc, const void * ExtraData);
extern PACKAGE void __fastcall TBIniLoadPositions(const Classes::TComponent* OwnerComponent, const Inifiles::TCustomIniFile* IniFile, const System::UnicodeString SectionNamePrefix)/* overload */;
extern PACKAGE void __fastcall TBIniLoadPositions(const Classes::TComponent* OwnerComponent, const System::UnicodeString Filename, const System::UnicodeString SectionNamePrefix)/* overload */;
extern PACKAGE void __fastcall TBIniSavePositions(const Classes::TComponent* OwnerComponent, const Inifiles::TCustomIniFile* IniFile, const System::UnicodeString SectionNamePrefix)/* overload */;
extern PACKAGE void __fastcall TBIniSavePositions(const Classes::TComponent* OwnerComponent, const System::UnicodeString Filename, const System::UnicodeString SectionNamePrefix)/* overload */;
extern PACKAGE void __fastcall TBRegLoadPositions(const Classes::TComponent* OwnerComponent, const unsigned RootKey, const System::UnicodeString BaseRegistryKey);
extern PACKAGE void __fastcall TBRegSavePositions(const Classes::TComponent* OwnerComponent, const unsigned RootKey, const System::UnicodeString BaseRegistryKey);

}	/* namespace Tb2dock */
using namespace Tb2dock;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tb2dockHPP
