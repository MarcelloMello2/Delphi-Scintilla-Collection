//CE_Desc_Include(helpdescriptions.txt)
{
	Delphi Scintilla Interface Components
	Copyright (C) 2004, Jan Martin Pettersen (hdalis)

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later
	version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free
	Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
 Author : hdalis (hdalis@users.sourceforge.net)
 Created: 30/09/2004, 04:24:38
     $Id: SciDocuments.pas,v 1.6 2004/12/03 17:18:58 hdalis Exp $
 Purpose: Tabbed scintilla component.
 Usage: Drop the TScintilla derived control and a TSciDocumentTabControl
        on a form, select the TScintillaBase derived control in the Editor property of TSciDocumentTabControl
        and viola.. Tabbed scintilla control.
     
 History 30/09/2004 Initial Release
         13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                    generate the help.
         11/11/2004 Added per tab hinting. Displays the full pathname of the file when ShowHint is True.
         14/11/2004 At last the TSciDoc saves foldingstates.
         26/11/2004 Added the property DefaultExt for setting the default file extension..
                    Added calls to the OnDetectHighlighter when we're attached to
                    a TScintilla component, and we're attaching ourself.
}
unit SciDocuments;
interface
Uses
	Windows, Classes, Controls,Contnrs,ComCtrls,Messages,SciLexer;

{$Include commondefs.inc}

type
	TSCEvent_onclosing = procedure(Sender : TObject; const TabIndex : Integer;var AllowClose : Boolean) of object;
  TSCEvent_detecthighlighter = procedure(Sender : TObject; const Extension : String;var Language : String) of object;

  TSciFoldStateRec=record
    Line : Integer;
    Expanded : Boolean;
  end;
  TSciDocumentTabControl=class;
	TSciDoc = class(TObject)
	private
		fEditor      : TScintillaBase;
    fTabCtrl     : TCustomTabControl;
		FTabName,
    fHighlighter,
    FFileName,
    FWordChars,
		FAutoStartChars,
		FAutoStopChars : string;
		documentid,
		FSelStart,
		FSelLength,
		FFirstLineInView  : LongInt;
		FIndex       : Integer;
		FUnicode,
		FReadOnly,
		FModified,
		FAutoIgnoreCase  : Boolean;
    FFoldStates : array of TSciFoldStateRec;
		FOnChanged   : TNotifyEvent;
		procedure   SetTabName(const Value : string);
		procedure   SetHighlighter(const Value : String);
		procedure   SetFileName(const Value : string);
		procedure   Changed;
		procedure   AddRef;
		procedure   Release;
		procedure   SetWordChars(const Value : String);
		procedure   AssignFromEditor;
		procedure   AssignToEditor;
    procedure   StoreFoldStates;
    procedure   RestoreFoldStates;
		property    OnChanged : TNotifyEvent read FOnChanged write FOnChanged; // Used by TSciDocumentTabControl, internal
	public
		constructor Create(pp : TScintillaBase;ttabctrl : TSciDocumentTabControl;const getcurrent : Boolean=false);
		destructor  Destroy;override;
		procedure   Activate;
    function    IsUntitled  : Boolean;
		function 	  IsActive    : Boolean;
		property    TabName     : string read FTabName Write SetTabName;
		// If the current editor is a TScintilla derived class then this property are automatically saved and used
		property    Highlighter : String read fHighlighter Write SetHighlighter;
		property    FileName    : String read FFileName write SetFileName;
		property    Index       : Integer read FIndex write FIndex;
    property    Modified    : Boolean read FModified write FModified;
		property    WordChars   : String read FWordChars write SetWordChars;
		property    AutoIgnoreCase : Boolean read FAutoIgnoreCase write FAutoIgnoreCase;
		property    AutoStartChars : String read FAutoStartChars write FAutoStartChars;
		property    AutoStopChars  : String read FAutoStopChars write FAutoStopChars;
	end;


	TSciDocumentTabControl = class(TCustomTabControl)
		private
			docs                 : TObjectList;
			fEditor              : TScintillaBase;
			FOnClosing           : TSCEvent_onclosing;
			FOnDetectHighlighter : TSCEvent_detecthighlighter;
      FDefaultExt          : String;
      inactivate           : Boolean;
      FDefaultTabName     : String;
			function    getCount : Integer;
			procedure   ChgTab(const newtab : Integer);
			function    GetDocItm(const Index : Integer) : TSciDoc;
			procedure   tabChanged(Sender : TObject);
			procedure   SetEditor(Value : TScintillaBase);
      function    GetActiveDocument : TSciDoc;
			procedure   Remove(const index : Integer);           // Remove the tab 'index'
      procedure   SetDefaultExt(const Value : String);
		protected
			procedure   Change; override;
			function    CanChange : Boolean; override;
			procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure   Attach;                                  // Attaches the editor to this tabcontrol
			procedure   RefreshTabs;                             // Refreshes the tablist
      procedure   CMHintShow(var Message: TMessage);message CM_HINTSHOW;
		public
			constructor Create(AOwner : TComponent);override;
			destructor  Destroy;override;
			function    Open(const filename : String) : Boolean; // Open a file
			procedure		Close(const index : Integer=-1;const askifneeded : Boolean=True);         // If 'index' is -1 then close the currently selected Tab, else close the selected index.
			function    NewDocument : Integer;                   // Init a new document, return tabindex
			procedure   Activate(const docno : LongInt);         // Activate the tab 'docno'
			function    Add(const tabname : String) : Integer;   // Add a tab, set the tabname to 'tabname'
			property    Count : Integer read getCount;           // Returns number of tabs/documents
			property    Document [const Index : Integer] : TSciDoc read GetDocItm; // Returns the document item for the specified tabindex
			property 	  DisplayRect;
			property    Tabs;
			property    TabIndex;  // must be after Tabs
      property    ActiveDocument : TSciDoc read GetActiveDocument;
		published
			// The current editor used.
			property    Editor : TScintillaBase read fEditor write SetEditor;
			// Fired when a Tab is about to be removed etc. Good place for confirmation dialogs etc.
			property    OnClosing : TSCEvent_onclosing read FOnClosing write FOnClosing;
			// This event is only active when a descentant of TScintilla is used
			property    OnDetectHighlighter : TSCEvent_detecthighlighter read FOnDetectHighlighter write FOnDetectHighlighter;
			property    Align;
			property    Anchors;
			property    BiDiMode;
			property    Constraints;
			property    DragCursor;
			property    DragMode;
			property    Enabled;
			property    Font;
			property    HotTrack default True;
			property    MultiLine;
			property    OwnerDraw;
			property    ParentBiDiMode;
			property    ParentShowHint default False;
			property    PopupMenu;
			property    RaggedRight;
			property    ScrollOpposite;
			property    ShowHint default True;
			property    Style;
			property    TabHeight;
			property    TabOrder;
			property    TabPosition;
			property    TabWidth;
			property    Visible;
			property    OnChange;
			property    OnChanging;
			property    OnContextPopup;
			property    OnDrawTab;
			property    OnEndDock;
			property    OnEndDrag;
			property    OnEnter;
			property    OnExit;
			property    OnMouseDown;
			property    OnMouseMove;
			property    OnMouseUp;
			property    OnResize;
			property    OnStartDock;
			property    OnUnDock;
      property    ParentFont default False;
      property    TabStop default False;
      property    DefaultExt : String read FDefaultExt write SetDefaultExt;
	end;

implementation
uses SysUtils,SciLexerMod,Graphics,Shellapi,Forms,sciSupport,SciResLang;//,RTLConsts;
const
cDefaultLexer='null';

procedure   TSciDocumentTabControl.Attach;
var
	sci : TSciDoc;
  highl,theext : String;
begin
	if (docs<>nil) then
	begin
    try
      Tabs.BeginUpdate;
      if (Tabs<>nil) and (Tabs.Count>0) then
        Tabs.Clear;
      if (docs.Count>0) then docs.Clear;
      if fEditor<>nil then
      begin
        sci:=TSciDoc.Create(fEditor,Self,true);
        sci.Index:=0;
        sci.TabName:=FDefaultTabName;
        sci.OnChanged:=tabChanged;
        if fEditor is TScintilla then
        begin
          highl:=cDefaultLexer;
          theext :=LowerCase(ExtractFileExt(FDefaultExt));
          if theext='' then
            theext:='.txt';
          if assigned(FOnDetectHighlighter) then
          begin
            FOnDetectHighlighter(self,theext,highl);
          end;
          sci.Highlighter:=highl;
        end;
        docs.Add(sci);
        Tabs.Add(sci.TabName);
      end;
    finally
      Tabs.EndUpdate;
    end;
	end;
end;

procedure   TSciDocumentTabControl.SetEditor(Value : TScintillaBase);
var
	tp: TPoint;
	r : TRect;
begin
	if (Value<>fEditor) then
	begin
		if (Value<>nil) then
		begin
			fEditor:=Value;
			if (not (csWriting in ComponentState)) and (not(csDestroying in ComponentState)) then
			begin
				fEditor.Parent:=self;
				fEditor.Visible:=true;
				fEditor.Align:=alClient;
				docs:=TObjectList.Create(true);
				Attach;
				RefreshTabs;
			end;
		end else
		begin
			if docs<>nil then
			begin
				FreeAndNil(docs);
			end;
			if (Tabs<>nil) and (Tabs.Count>0) then
				Tabs.Clear;
			if fEditor.HandleAllocated then
			begin
				if Parent<>nil then
				begin
					fEditor.Align:=alNone;
					r :=ClientRect;
					tp.x:=r.Left;
					tp.y:=r.Top;
					tp:=fEditor.ClientToParent(tp);
					fEditor.Parent:=Parent;
					fEditor.Left:=tp.x;
					fEditor.Top:=tp.y;
				end;
			end;
			fEditor:=nil;
		end;
	end;
end;

function TSciDocumentTabControl.getCount : Integer;
begin
	if docs<>nil then
		result:=docs.Count
	else
		result:=0;
end;

procedure TSciDocumentTabControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited;
	if Operation=opRemove then
	begin
		if AComponent=fEditor then
		begin
			if docs<>nil then	FreeAndNil(docs);
			if Tabs<>nil then Tabs.Clear;
			fEditor:=nil;
		end;
	end;
end;

procedure TSciDocumentTabControl.tabChanged(Sender : TObject);
var
	tmp : TSciDoc;
	ist : String;
begin
	tmp:=TSciDoc(Sender);
	if tmp.Index<Tabs.Count then
	begin
		Str(tmp.Index+1,ist);
	  Tabs.Strings[tmp.Index]:=ist+' '+tmp.TabName;
	end;
end;

constructor TSciDocumentTabControl.Create(AOwner : TComponent);
begin
	fEditor:=nil;
	docs:=nil;
  inactivate:=False;
	inherited Create(AOwner);
	DragKind:=dkDock;
	MultiSelect:=False;
  ParentFont:=False;
	TabStop:=False;
  ShowHint:=True;
  FDefaultExt:='.txt';
  FDefaultTabName:='<'+sUntitled+FDefaultExt+'>';

  ControlStyle:=ControlStyle-[csAcceptsControls];
end;


function TSciDocumentTabControl.GetDocItm(const Index : Integer) : TSciDoc;
begin
	if docs<>nil then
		result :=TSciDoc(docs.Items[Index])
	else
		result:=nil;
end;

function TSciDocumentTabControl.NewDocument : Integer;
begin
	result:=Add(FDefaultTabName);
  if result<>-1 then
  TSciDoc(docs.Items[result]).Changed;
end;

function    TSciDocumentTabControl.Open(const filename : String) : Boolean;
var
	itm : TSciDoc;
  highl,theext : String;
	doccnt : Integer;
begin
	if FileExists(filename) then
	begin
		doccnt:=docs.Count;
		if (doccnt>1) or (fEditor.Modified) or (StrLIComp(PChar(TSciDoc(docs.Items[0]).TabName),PChar(FDefaultTabName),Length(FDefaultTabName))<>0) then
		begin
			itm:=TSciDoc(docs.Items[Add(ExtractFileName(filename))]);
      itm.FileName:=filename;
			doccnt:=doccnt+1;
		end else
		begin
			itm:=TSciDoc(docs.Items[0]);
			itm.FileName:=filename;
		end;

		if fEditor is TScintilla then
		begin
      highl:=cDefaultLexer;
			theext :=LowerCase(ExtractFileExt(filename));
			if theext='' then
				theext:=LowerCase(ExtractFileName(filename));
			if assigned(FOnDetectHighlighter) then
			begin
				FOnDetectHighlighter(self,theext,highl);
			end;
		  itm.Highlighter:=highl;
		end;
		fEditor.LoadFromFile(filename);
		if doccnt=1 then ChgTab(0);

		result:=true;
	end else
  begin
    result:=false;
    //raise EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
  end;
end;

procedure		TSciDocumentTabControl.Close(const index : Integer;const askifneeded : Boolean);
var
	allowclose : Boolean;
  idx : Integer;
begin
	allowclose:=true;
  if index=-1 then
    idx:=TabIndex
  else
    idx:=Index;

	if (askifneeded) and (assigned(FOnClosing)) then
	begin
    if TSciDoc(docs[idx]).IsActive<>True then
    begin
      Activate(idx);
    end;
	  FOnClosing(self,idx,allowclose)
  end;
	if allowclose then
    Remove(idx);
end;

function TSciDocumentTabControl.Add(const tabname : string) : Integer;
var
	sci,itm : TSciDoc;
	atidx : Integer;
  theext,highl : String;

begin
	if (fEditor<>nil) and (docs<>nil) then
	begin
    try
    Tabs.BeginUpdate;
		itm:=ActiveDocument;
		itm.AssignFromEditor;
		sci:=TSciDoc.Create(fEditor,Self,false);
		if tabname<>'' then
			sci.TabName:=tabname;
		atidx:=docs.Add(sci);
		sci.Index:=atidx;
		Tabs.Insert(atidx,tabname);
		sci.OnChanged:=tabChanged;
		if fEditor is TScintilla then
		begin
      highl:=cDefaultLexer;
			theext :=LowerCase(ExtractFileExt(FDefaultExt));
			if theext='' then
				theext:='.txt';
			if assigned(FOnDetectHighlighter) then
			begin
				FOnDetectHighlighter(self,theext,highl);
			end;
		  sci.Highlighter:=highl;
		end;
		ChgTab(atidx);
		Result :=atidx;
    finally
      Tabs.EndUpdate;
    end;
	end else
		result:=-1;
end;

function    TSciDocumentTabControl.GetActiveDocument : TSciDoc;
begin
  Result :=TSciDoc(docs.Items[TabIndex]);
end;
procedure TSciDocumentTabControl.ChgTab(const newtab : Integer);
begin
	TabIndex:=newtab;
	Change;

end;
procedure   TSciDocumentTabControl.Remove(const index : Integer);
var
	sci : TSciDoc;
	id,cur,i : Integer;
begin
	if (fEditor<>nil) and (docs<>nil) then
	begin
    try
      Tabs.BeginUpdate;
      sci:=TSciDoc(docs.Items[index]);
      if docs.Count=1 then
      begin
        fEditor.ClearAll;
      end else
      begin
        docs.Remove(sci);
        docs.Pack;
      end;
      if Tabs.Count>1 then
      begin
        cur:=TabIndex;
        Tabs.Delete(Index);
        id:=Tabs.Count;
        if(id>cur+1) then	TabIndex:=cur
        else if cur=0 then ChgTab(0)
        else ChgTab(cur-1);
      end else
      begin
        ChgTab(0);
        TSciDoc(docs.Items[0]).FileName:=sUntitledFileName+FDefaultExt;
        fEditor.ClearDocument;
      end;
      for i:=0 to (docs.Count-1) do
      begin
        TSciDoc(docs.Items[i]).Index:=i;
      end;
    finally
      Tabs.EndUpdate;
    end;
	end;
end;

function TSciDocumentTabControl.CanChange : Boolean;
var
	itm : TSciDoc;
begin
	if (fEditor<>nil) and (docs<>nil) then
	begin
		itm :=ActiveDocument;
		if assigned(itm) then	itm.AssignFromEditor;
		result:=Inherited CanChange;
	end else
		result:=false;
end;

procedure TSciDocumentTabControl.Change;
begin
	if docs<>nil then
	begin
		Activate(TabIndex);
	end;
	inherited;
end;

procedure TSciDocumentTabControl.CMHintShow(var Message: TMessage);
var
Index: Integer;
begin
  with TCMHintShow(Message) do
  begin
    if not ShowHint then Message.Result := 1
    else
    begin
      with HintInfo^ do
      begin
        // show that we want a hint
        Result := 0;
        // predefined colors always get their names as tooltip
        index:=IndexOfTabAt(CursorPos.X,CursorPos.Y);
        if index<>-1 then
        begin
          if StrLIComp(PChar(TSciDoc(docs.Items[index]).TabName),PChar(FDefaultTabName),Length(FDefaultTabName))<>0 then
          HintStr:=TSciDoc(docs.Items[index]).FileName
          else HintStr :=Hint;
          CursorRect:=TabRect(index);
        end
        else
          HintStr:=Hint;
      end;
    end;
  end;
end;

procedure TSciDocumentTabControl.Activate(const docno : LongInt);
begin
  if TabIndex<>docno then
    TabIndex:=docno;
	if docs<>nil then
		TSciDoc(docs.Items[docno]).Activate;
end;

destructor TSciDocumentTabControl.Destroy;
var
	i : Integer;
begin
	if (docs<>nil) and (assigned(docs)) and (fEditor<>nil) then
	begin
		for i:=0 to (docs.Count-1) do
		begin
			TSciDoc(docs.Items[i]).OnChanged:=nil;
      //Close(i);
		end;
		FreeAndNil(docs);
	end;
	inherited;
end;

procedure   TSciDocumentTabControl.SetDefaultExt(const Value : String);
var
oldtab : String;
i,cnt : Integer;
itm : TSciDoc;
begin
  oldtab:=FDefaultTabName;
  FDefaultExt:=Value;
  if Pos('.',FDefaultExt)=0 then
    Insert('.',FDefaultExt,1);
  FDefaultTabName:='<'+sUntitled+FDefaultExt+'>';
  cnt:=Tabs.Count;
  for i:=0 to (cnt-1) do
  begin
    itm:=TSciDoc(docs.Items[i]);
    if StrLIComp(PChar(itm.TabName),PChar(oldtab),Length(oldtab))=0 then
    begin
      itm.TabName:=FDefaultTabName;
      itm.FileName:=sUntitledFileName+FDefaultExt;
    end;
  end;
  
end;

procedure   TSciDocumentTabControl.RefreshTabs;
var
	cnt : Integer;
	i : Integer;
	ist : String;
begin
  try
    Tabs.BeginUpdate;
    Tabs.Clear;
    if docs<>nil then
    begin
      cnt:=docs.Count;
      for i:=0 to (cnt-1) do
      begin
        str(i+1,ist);
        Tabs.Add(ist+' '+TSciDoc(docs.Items[i]).TabName);
      end;
    end;
  finally
    Tabs.EndUpdate;
  end;
end;

procedure TSciDoc.SetFileName(const Value : string);
begin
	if Value<>FFileName then
	begin
		FFileName:=Value;
		if Value=(sUntitledFileName+TSciDocumentTabControl(fTabCtrl).FDefaultExt) then
			FTabName:='<'+sUntitled+TSciDocumentTabControl(fTabCtrl).FDefaultExt+'>'
		else
			FTabName:=ExtractFileName(Value);

		Changed;
	end;
end;

procedure   TSciDoc.SetWordChars(const Value : String);
begin
	if FWordChars<>Value then
	begin
		FWordChars:=Value;
		if IsActive then
			TScintillaBase(fEditor).WordChars:=FWordChars;
	end;
end;
procedure TSciDoc.Changed;
begin
	if assigned(FOnChanged) then FOnChanged(self);
end;


procedure TSciDoc.SetTabName(const Value : string);
begin
	if Value<>FTabName then
	begin
		FTabName:=Value;
		Changed;
	end;
end;

function TSciDoc.IsActive : Boolean;
var
	ptr : Longint;
begin
	if fEditor=nil then
	begin
		Result:=false;
		Exit;
	end;
	ptr :=fEditor.GetDocPointer;
	if ptr=documentid then
		result :=true
	else
		result :=false;
end;

constructor TSciDoc.Create(pp : TScintillaBase;ttabctrl : TSciDocumentTabControl;const getcurrent : Boolean);
begin
	inherited Create;
  fTabCtrl:=ttabctrl;
	fEditor :=pp;
	FTabName:='<'+sUntitled+TSciDocumentTabControl(fTabCtrl).FDefaultExt+'>';
	FModified:=false;
	FWordChars :='_'+sci_alphachars+sci_numericchars;
  FHighlighter:=cDefaultLexer;
  FAutoIgnoreCase:=False;
  FAutoStartChars:='';
  FAutoStopChars:='';
	FFileName :=sUntitledFileName+TSciDocumentTabControl(fTabCtrl).FDefaultExt;

	if getcurrent=false then
	begin
		documentid:=fEditor.CreateDocument;
		AddRef;
	end	else
	begin
		documentid:=fEditor.GetDocPointer;
		AddRef;
	end;
	AssignFromEditor;
end;

procedure   TSciDoc.StoreFoldStates;
var
maxLine : LongInt;
foldPoints : LongInt;
line : LongInt;
level : LongInt;
expanded : Boolean;
atpos : Integer;
begin
//FFoldStates : array of TSciFoldStateRec;
  FFoldStates:=nil;
  foldPoints:=0;
  maxline:=fEditor.GetLineCount;
  for line := 0 to (maxLine-1) do
  begin
    level := fEditor.GetFoldLevel(line);
    if ((level and SC_FOLDLEVELHEADERFLAG)<>0) then Inc(foldPoints);
  end;
  if foldPoints>0 then
  begin
    SetLength(FFoldStates,foldPoints);
    atpos:=0;
		for line := 0 to  (maxLine-1) do
    begin
      level := fEditor.GetFoldLevel(line);
      if ((level and SC_FOLDLEVELHEADERFLAG)<>0) then
      begin
        expanded:=fEditor.GetFoldExpanded(line);
        FFoldStates[atpos].Line:=line;
        FFoldStates[atpos].Expanded:=expanded;
        Inc(atpos);
      end;
	  end;
  end;
end;
procedure   TSciDoc.RestoreFoldStates;
var
i : LongInt;
expanded : Boolean;
begin
  if FFoldStates=nil then Exit;
  for i:=Low(FFoldStates) to High(FFoldStates) do
  begin
    expanded:=fEditor.GetFoldExpanded(FFoldStates[i].Line);
    if (FFoldStates[i].Expanded=False) and (expanded=True) then
    begin
      fEditor.ToggleFold(FFoldStates[i].Line);
    end;
    if (FFoldStates[i].Expanded=True) and (expanded=False) then
    begin
      fEditor.ToggleFold(FFoldStates[i].Line);
    end;
  end;
end;

procedure   TSciDoc.AssignFromEditor;
begin
	with fEditor do
	begin
    StoreFoldStates;
		FFirstLineInView:=GetCurrentScrollPosition;
		FSelStart:=SelStart;
		FSelLength:=SelLength;
		FModified:=Modified;
		FUnicode :=UseUnicode;
		FReadOnly :=ReadOnly;
		FWordChars :=WordChars;
	end;
	if fEditor is TScintilla then
  begin
    fHighlighter:=TScintilla(fEditor).LanguageManager.SelectedLanguage;
  end;

end;

procedure   TSciDoc.AssignToEditor;
var
	curtop : LongInt;
	xlinestart : LongInt;
	xlineend : LongInt;
	xlinetop : LongInt;
begin
	if fEditor is TScintilla then
  begin
    TScintilla(fEditor).LanguageManager.SelectedLanguage:=self.fHighlighter;
  end;
	with fEditor do
	begin
    RestoreFoldStates;
		xlinestart:=LineFromPosition(FSelStart);
		xlineend:=LineFromPosition(FSelStart+FSelLength);
		EnsureVisibleEnforcePolicy(xlinestart);
		EnsureVisibleEnforcePolicy(xlineend);
		SelStart:=FSelStart;
		SelLength:=FSelLength;
		Modified:=FModified;
		UseUnicode:=FUnicode;
		ReadOnly:=FReadOnly;
		WordChars :=FWordChars;
    //Position the cursor back where it was
		curtop:=GetFirstVisibleLine;
		xlinetop:=VisibleFromDocLine(FFirstLineInView);
		LineScroll(0,xlinetop-curtop);
		SetFocus;
	end;
		//TScintilla(fEditor).Highlighter.Update;
end;

procedure TSciDoc.Activate;
begin
	if assigned(fEditor) then
	begin
		if not IsActive then
		begin
			fEditor.SetDocPointer(documentid);
			AssignToEditor;
		end;
	end;
end;

destructor TSciDoc.Destroy;
begin
  FFoldStates:=nil;
	Release;
	inherited;
end;
procedure TSciDoc.SetHighlighter(const Value : String);
begin
	fHighlighter:=Value;
	if IsActive then
	begin
		if fEditor is TScintilla then
		begin
      if Value<>TScintilla(fEditor).LanguageManager.SelectedLanguage then
			begin
        TScintilla(fEditor).LanguageManager.SelectedLanguage:=fHighlighter;
			end;
			//fEditor.Highlighter.Update;
		end;
	end;
end;

procedure TSciDoc.AddRef;
begin
	if (fEditor<>nil) and (documentid<>0) then
	begin
		fEditor.AddRefDocument(documentid);
	end;
end;

procedure TSciDoc.Release;
begin
	if (fEditor<>nil) and (documentid<>0) then
	begin
		if fEditor.HandleAllocated then
			fEditor.ReleaseDocument(documentid);
	end;
	documentid:=0;
end;

function    TSciDoc.IsUntitled  : Boolean;
begin
  if Pos(TabName,'<')<>0 then
    Result:=True
  else
    Result:=False;
end;
end.