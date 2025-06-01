function TDScintilla.GetLength: NativeInt;
begin
  Result := SendEditor(SCI_GETLENGTH, 0, 0);
end;

function TDScintilla.GetCharAt(APos: NativeInt): Integer;
begin
  Result := SendEditor(SCI_GETCHARAT, APos, 0);
end;

function TDScintilla.GetCurrentPos: NativeInt;
begin
  Result := SendEditor(SCI_GETCURRENTPOS, 0, 0);
end;

function TDScintilla.GetAnchor: NativeInt;
begin
  Result := SendEditor(SCI_GETANCHOR, 0, 0);
end;

function TDScintilla.GetStyleAt(APos: NativeInt): Integer;
begin
  Result := SendEditor(SCI_GETSTYLEAT, APos, 0);
end;

procedure TDScintilla.SetUndoCollection(ACollectUndo: Boolean);
begin
  SendEditor(SCI_SETUNDOCOLLECTION, Integer(ACollectUndo), 0);
end;

function TDScintilla.GetUndoCollection: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETUNDOCOLLECTION, 0, 0));
end;

function TDScintilla.GetViewWS: Integer;
begin
  Result := SendEditor(SCI_GETVIEWWS, 0, 0);
end;

procedure TDScintilla.SetViewWS(AViewWS: Integer);
begin
  SendEditor(SCI_SETVIEWWS, AViewWS, 0);
end;

procedure TDScintilla.SetAnchor(APosAnchor: NativeInt);
begin
  SendEditor(SCI_SETANCHOR, APosAnchor, 0);
end;

function TDScintilla.GetEndStyled: NativeInt;
begin
  Result := SendEditor(SCI_GETENDSTYLED, 0, 0);
end;

function TDScintilla.GetEOLMode: Integer;
begin
  Result := SendEditor(SCI_GETEOLMODE, 0, 0);
end;

procedure TDScintilla.SetEOLMode(AEolMode: Integer);
begin
  SendEditor(SCI_SETEOLMODE, AEolMode, 0);
end;

function TDScintilla.GetBufferedDraw: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETBUFFEREDDRAW, 0, 0));
end;

procedure TDScintilla.SetBufferedDraw(ABuffered: Boolean);
begin
  SendEditor(SCI_SETBUFFEREDDRAW, Integer(ABuffered), 0);
end;

procedure TDScintilla.SetTabWidth(ATabWidth: Integer);
begin
  SendEditor(SCI_SETTABWIDTH, ATabWidth, 0);
end;

function TDScintilla.GetTabWidth: Integer;
begin
  Result := SendEditor(SCI_GETTABWIDTH, 0, 0);
end;

procedure TDScintilla.SetCodePage(ACodePage: Integer);
begin
  SendEditor(SCI_SETCODEPAGE, ACodePage, 0);
end;

procedure TDScintilla.SetMarginTypeN(AMargin: Integer; AMarginType: Integer);
begin
  SendEditor(SCI_SETMARGINTYPEN, AMargin, AMarginType);
end;

function TDScintilla.GetMarginTypeN(AMargin: Integer): Integer;
begin
  Result := SendEditor(SCI_GETMARGINTYPEN, AMargin, 0);
end;

procedure TDScintilla.SetMarginWidthN(AMargin: Integer; APixelWidth: Integer);
begin
  SendEditor(SCI_SETMARGINWIDTHN, AMargin, APixelWidth);
end;

function TDScintilla.GetMarginWidthN(AMargin: Integer): Integer;
begin
  Result := SendEditor(SCI_GETMARGINWIDTHN, AMargin, 0);
end;

procedure TDScintilla.SetMarginMaskN(AMargin: Integer; AMask: Integer);
begin
  SendEditor(SCI_SETMARGINMASKN, AMargin, AMask);
end;

function TDScintilla.GetMarginMaskN(AMargin: Integer): Integer;
begin
  Result := SendEditor(SCI_GETMARGINMASKN, AMargin, 0);
end;

procedure TDScintilla.SetMarginSensitiveN(AMargin: Integer; ASensitive: Boolean);
begin
  SendEditor(SCI_SETMARGINSENSITIVEN, AMargin, Integer(ASensitive));
end;

function TDScintilla.GetMarginSensitiveN(AMargin: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETMARGINSENSITIVEN, AMargin, 0));
end;

procedure TDScintilla.SetMarginCursorN(AMargin: Integer; ACursor: Integer);
begin
  SendEditor(SCI_SETMARGINCURSORN, AMargin, ACursor);
end;

function TDScintilla.GetMarginCursorN(AMargin: Integer): Integer;
begin
  Result := SendEditor(SCI_GETMARGINCURSORN, AMargin, 0);
end;

procedure TDScintilla.StyleSetFore(AStyle: Integer; AFore: TColor);
begin
  SendEditor(SCI_STYLESETFORE, AStyle, Integer(AFore));
end;

procedure TDScintilla.StyleSetBack(AStyle: Integer; ABack: TColor);
begin
  SendEditor(SCI_STYLESETBACK, AStyle, Integer(ABack));
end;

procedure TDScintilla.StyleSetBold(AStyle: Integer; ABold: Boolean);
begin
  SendEditor(SCI_STYLESETBOLD, AStyle, Integer(ABold));
end;

procedure TDScintilla.StyleSetItalic(AStyle: Integer; AItalic: Boolean);
begin
  SendEditor(SCI_STYLESETITALIC, AStyle, Integer(AItalic));
end;

procedure TDScintilla.StyleSetSize(AStyle: Integer; ASizePoints: Integer);
begin
  SendEditor(SCI_STYLESETSIZE, AStyle, ASizePoints);
end;

procedure TDScintilla.StyleSetFont(AStyle: Integer; const AFontName: UnicodeString);
begin
  FHelper.SetText(SCI_STYLESETFONT, AStyle, AFontName);
end;

procedure TDScintilla.StyleSetEOLFilled(AStyle: Integer; AFilled: Boolean);
begin
  SendEditor(SCI_STYLESETEOLFILLED, AStyle, Integer(AFilled));
end;

procedure TDScintilla.StyleSetUnderline(AStyle: Integer; AUnderline: Boolean);
begin
  SendEditor(SCI_STYLESETUNDERLINE, AStyle, Integer(AUnderline));
end;

function TDScintilla.StyleGetFore(AStyle: Integer): TColor;
begin
  Result := TColor(SendEditor(SCI_STYLEGETFORE, AStyle, 0));
end;

function TDScintilla.StyleGetBack(AStyle: Integer): TColor;
begin
  Result := TColor(SendEditor(SCI_STYLEGETBACK, AStyle, 0));
end;

function TDScintilla.StyleGetBold(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETBOLD, AStyle, 0));
end;

function TDScintilla.StyleGetItalic(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETITALIC, AStyle, 0));
end;

function TDScintilla.StyleGetSize(AStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_STYLEGETSIZE, AStyle, 0);
end;

function TDScintilla.StyleGetEOLFilled(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETEOLFILLED, AStyle, 0));
end;

function TDScintilla.StyleGetUnderline(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETUNDERLINE, AStyle, 0));
end;

function TDScintilla.StyleGetCase(AStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_STYLEGETCASE, AStyle, 0);
end;

function TDScintilla.StyleGetCharacterSet(AStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_STYLEGETCHARACTERSET, AStyle, 0);
end;

function TDScintilla.StyleGetVisible(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETVISIBLE, AStyle, 0));
end;

function TDScintilla.StyleGetChangeable(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETCHANGEABLE, AStyle, 0));
end;

function TDScintilla.StyleGetHotSpot(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETHOTSPOT, AStyle, 0));
end;

procedure TDScintilla.StyleSetCase(AStyle: Integer; ACaseVisible: Integer);
begin
  SendEditor(SCI_STYLESETCASE, AStyle, ACaseVisible);
end;

procedure TDScintilla.StyleSetSizeFractional(AStyle: Integer; ASizeHundredthPoints: Integer);
begin
  SendEditor(SCI_STYLESETSIZEFRACTIONAL, AStyle, ASizeHundredthPoints);
end;

function TDScintilla.StyleGetSizeFractional(AStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_STYLEGETSIZEFRACTIONAL, AStyle, 0);
end;

procedure TDScintilla.StyleSetWeight(AStyle: Integer; AWeight: Integer);
begin
  SendEditor(SCI_STYLESETWEIGHT, AStyle, AWeight);
end;

function TDScintilla.StyleGetWeight(AStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_STYLEGETWEIGHT, AStyle, 0);
end;

procedure TDScintilla.StyleSetCharacterSet(AStyle: Integer; ACharacterSet: Integer);
begin
  SendEditor(SCI_STYLESETCHARACTERSET, AStyle, ACharacterSet);
end;

procedure TDScintilla.StyleSetHotSpot(AStyle: Integer; AHotspot: Boolean);
begin
  SendEditor(SCI_STYLESETHOTSPOT, AStyle, Integer(AHotspot));
end;

function TDScintilla.GetSelAlpha: Integer;
begin
  Result := SendEditor(SCI_GETSELALPHA, 0, 0);
end;

procedure TDScintilla.SetSelAlpha(AAlpha: Integer);
begin
  SendEditor(SCI_SETSELALPHA, AAlpha, 0);
end;

function TDScintilla.GetSelEOLFilled: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETSELEOLFILLED, 0, 0));
end;

procedure TDScintilla.SetSelEOLFilled(AFilled: Boolean);
begin
  SendEditor(SCI_SETSELEOLFILLED, Integer(AFilled), 0);
end;

procedure TDScintilla.SetCaretFore(AFore: TColor);
begin
  SendEditor(SCI_SETCARETFORE, Integer(AFore), 0);
end;

procedure TDScintilla.StyleSetVisible(AStyle: Integer; AVisible: Boolean);
begin
  SendEditor(SCI_STYLESETVISIBLE, AStyle, Integer(AVisible));
end;

function TDScintilla.GetCaretPeriod: Integer;
begin
  Result := SendEditor(SCI_GETCARETPERIOD, 0, 0);
end;

procedure TDScintilla.SetCaretPeriod(APeriodMilliseconds: Integer);
begin
  SendEditor(SCI_SETCARETPERIOD, APeriodMilliseconds, 0);
end;

procedure TDScintilla.SetWordChars(const ACharacters: AnsiString);
begin
  FHelper.SetTextA(SCI_SETWORDCHARS, 0, ACharacters);
end;

function TDScintilla.GetWordChars: AnsiString;
begin
  FHelper.GetTextA(SCI_GETWORDCHARS, 0, Result);
end;

procedure TDScintilla.IndicSetStyle(AIndic: Integer; AStyle: Integer);
begin
  SendEditor(SCI_INDICSETSTYLE, AIndic, AStyle);
end;

function TDScintilla.IndicGetStyle(AIndic: Integer): Integer;
begin
  Result := SendEditor(SCI_INDICGETSTYLE, AIndic, 0);
end;

procedure TDScintilla.IndicSetFore(AIndic: Integer; AFore: TColor);
begin
  SendEditor(SCI_INDICSETFORE, AIndic, Integer(AFore));
end;

function TDScintilla.IndicGetFore(AIndic: Integer): TColor;
begin
  Result := TColor(SendEditor(SCI_INDICGETFORE, AIndic, 0));
end;

procedure TDScintilla.IndicSetUnder(AIndic: Integer; AUnder: Boolean);
begin
  SendEditor(SCI_INDICSETUNDER, AIndic, Integer(AUnder));
end;

function TDScintilla.IndicGetUnder(AIndic: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_INDICGETUNDER, AIndic, 0));
end;

procedure TDScintilla.SetWhitespaceSize(ASize: Integer);
begin
  SendEditor(SCI_SETWHITESPACESIZE, ASize, 0);
end;

function TDScintilla.GetWhitespaceSize: Integer;
begin
  Result := SendEditor(SCI_GETWHITESPACESIZE, 0, 0);
end;

procedure TDScintilla.SetLineState(ALine: NativeInt; AState: Integer);
begin
  SendEditor(SCI_SETLINESTATE, ALine, AState);
end;

function TDScintilla.GetLineState(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_GETLINESTATE, ALine, 0);
end;

function TDScintilla.GetMaxLineState: NativeInt;
begin
  Result := SendEditor(SCI_GETMAXLINESTATE, 0, 0);
end;

function TDScintilla.GetCaretLineVisible: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETCARETLINEVISIBLE, 0, 0));
end;

procedure TDScintilla.SetCaretLineVisible(AShow: Boolean);
begin
  SendEditor(SCI_SETCARETLINEVISIBLE, Integer(AShow), 0);
end;

function TDScintilla.GetCaretLineBack: TColor;
begin
  Result := TColor(SendEditor(SCI_GETCARETLINEBACK, 0, 0));
end;

procedure TDScintilla.SetCaretLineBack(ABack: TColor);
begin
  SendEditor(SCI_SETCARETLINEBACK, Integer(ABack), 0);
end;

procedure TDScintilla.StyleSetChangeable(AStyle: Integer; AChangeable: Boolean);
begin
  SendEditor(SCI_STYLESETCHANGEABLE, AStyle, Integer(AChangeable));
end;

procedure TDScintilla.AutoCSetSeparator(ASeparatorCharacter: AnsiChar);
begin
  SendEditor(SCI_AUTOCSETSEPARATOR, Integer(ASeparatorCharacter), 0);
end;

function TDScintilla.AutoCGetSeparator: AnsiChar;
begin
  Result := AnsiChar(SendEditor(SCI_AUTOCGETSEPARATOR, 0, 0));
end;

procedure TDScintilla.AutoCSetCancelAtStart(ACancel: Boolean);
begin
  SendEditor(SCI_AUTOCSETCANCELATSTART, Integer(ACancel), 0);
end;

function TDScintilla.AutoCGetCancelAtStart: Boolean;
begin
  Result := Boolean(SendEditor(SCI_AUTOCGETCANCELATSTART, 0, 0));
end;

procedure TDScintilla.AutoCSetFillUps(const ACharacterSet: UnicodeString);
begin
  FHelper.SetText(SCI_AUTOCSETFILLUPS, 0, ACharacterSet);
end;

procedure TDScintilla.AutoCSetChooseSingle(AChooseSingle: Boolean);
begin
  SendEditor(SCI_AUTOCSETCHOOSESINGLE, Integer(AChooseSingle), 0);
end;

function TDScintilla.AutoCGetChooseSingle: Boolean;
begin
  Result := Boolean(SendEditor(SCI_AUTOCGETCHOOSESINGLE, 0, 0));
end;

procedure TDScintilla.AutoCSetIgnoreCase(AIgnoreCase: Boolean);
begin
  SendEditor(SCI_AUTOCSETIGNORECASE, Integer(AIgnoreCase), 0);
end;

function TDScintilla.AutoCGetIgnoreCase: Boolean;
begin
  Result := Boolean(SendEditor(SCI_AUTOCGETIGNORECASE, 0, 0));
end;

procedure TDScintilla.AutoCSetAutoHide(AAutoHide: Boolean);
begin
  SendEditor(SCI_AUTOCSETAUTOHIDE, Integer(AAutoHide), 0);
end;

function TDScintilla.AutoCGetAutoHide: Boolean;
begin
  Result := Boolean(SendEditor(SCI_AUTOCGETAUTOHIDE, 0, 0));
end;

procedure TDScintilla.AutoCSetDropRestOfWord(ADropRestOfWord: Boolean);
begin
  SendEditor(SCI_AUTOCSETDROPRESTOFWORD, Integer(ADropRestOfWord), 0);
end;

function TDScintilla.AutoCGetDropRestOfWord: Boolean;
begin
  Result := Boolean(SendEditor(SCI_AUTOCGETDROPRESTOFWORD, 0, 0));
end;

function TDScintilla.AutoCGetTypeSeparator: AnsiChar;
begin
  Result := AnsiChar(SendEditor(SCI_AUTOCGETTYPESEPARATOR, 0, 0));
end;

procedure TDScintilla.AutoCSetTypeSeparator(ASeparatorCharacter: AnsiChar);
begin
  SendEditor(SCI_AUTOCSETTYPESEPARATOR, Integer(ASeparatorCharacter), 0);
end;

procedure TDScintilla.AutoCSetMaxWidth(ACharacterCount: Integer);
begin
  SendEditor(SCI_AUTOCSETMAXWIDTH, ACharacterCount, 0);
end;

function TDScintilla.AutoCGetMaxWidth: Integer;
begin
  Result := SendEditor(SCI_AUTOCGETMAXWIDTH, 0, 0);
end;

procedure TDScintilla.AutoCSetMaxHeight(ARowCount: Integer);
begin
  SendEditor(SCI_AUTOCSETMAXHEIGHT, ARowCount, 0);
end;

function TDScintilla.AutoCGetMaxHeight: Integer;
begin
  Result := SendEditor(SCI_AUTOCGETMAXHEIGHT, 0, 0);
end;

procedure TDScintilla.SetIndent(AIndentSize: Integer);
begin
  SendEditor(SCI_SETINDENT, AIndentSize, 0);
end;

function TDScintilla.GetIndent: Integer;
begin
  Result := SendEditor(SCI_GETINDENT, 0, 0);
end;

procedure TDScintilla.SetUseTabs(AUseTabs: Boolean);
begin
  SendEditor(SCI_SETUSETABS, Integer(AUseTabs), 0);
end;

function TDScintilla.GetUseTabs: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETUSETABS, 0, 0));
end;

procedure TDScintilla.SetLineIndentation(ALine: NativeInt; AIndentSize: Integer);
begin
  SendEditor(SCI_SETLINEINDENTATION, ALine, AIndentSize);
end;

function TDScintilla.GetLineIndentation(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_GETLINEINDENTATION, ALine, 0);
end;

function TDScintilla.GetLineIndentPosition(ALine: NativeInt): NativeInt;
begin
  Result := SendEditor(SCI_GETLINEINDENTPOSITION, ALine, 0);
end;

function TDScintilla.GetColumn(APos: NativeInt): NativeInt;
begin
  Result := SendEditor(SCI_GETCOLUMN, APos, 0);
end;

procedure TDScintilla.SetHScrollBar(AShow: Boolean);
begin
  SendEditor(SCI_SETHSCROLLBAR, Integer(AShow), 0);
end;

function TDScintilla.GetHScrollBar: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETHSCROLLBAR, 0, 0));
end;

procedure TDScintilla.SetIndentationGuides(AIndentView: Integer);
begin
  SendEditor(SCI_SETINDENTATIONGUIDES, AIndentView, 0);
end;

function TDScintilla.GetIndentationGuides: Integer;
begin
  Result := SendEditor(SCI_GETINDENTATIONGUIDES, 0, 0);
end;

procedure TDScintilla.SetHighlightGuide(AColumn: Integer);
begin
  SendEditor(SCI_SETHIGHLIGHTGUIDE, AColumn, 0);
end;

function TDScintilla.GetHighlightGuide: Integer;
begin
  Result := SendEditor(SCI_GETHIGHLIGHTGUIDE, 0, 0);
end;

function TDScintilla.GetLineEndPosition(ALine: NativeInt): NativeInt;
begin
  Result := SendEditor(SCI_GETLINEENDPOSITION, ALine, 0);
end;

function TDScintilla.GetCodePage: Integer;
begin
  Result := SendEditor(SCI_GETCODEPAGE, 0, 0);
end;

function TDScintilla.GetCaretFore: TColor;
begin
  Result := TColor(SendEditor(SCI_GETCARETFORE, 0, 0));
end;

function TDScintilla.GetReadOnly: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETREADONLY, 0, 0));
end;

procedure TDScintilla.SetCurrentPos(APos: NativeInt);
begin
  SendEditor(SCI_SETCURRENTPOS, APos, 0);
end;

procedure TDScintilla.SetSelectionStart(APos: NativeInt);
begin
  SendEditor(SCI_SETSELECTIONSTART, APos, 0);
end;

function TDScintilla.GetSelectionStart: NativeInt;
begin
  Result := SendEditor(SCI_GETSELECTIONSTART, 0, 0);
end;

procedure TDScintilla.SetSelectionEnd(APos: NativeInt);
begin
  SendEditor(SCI_SETSELECTIONEND, APos, 0);
end;

function TDScintilla.GetSelectionEnd: NativeInt;
begin
  Result := SendEditor(SCI_GETSELECTIONEND, 0, 0);
end;

function TDScintilla.GetSelectionNStartVirtualSpace(ASelection: Integer): NativeInt;
begin
  Result := SendEditor(SCI_GETSELECTIONNSTARTVIRTUALSPACE, ASelection, 0);
end;

function TDScintilla.GetSelectionNEndVirtualSpace(ASelection: Integer): NativeInt;
begin
  Result := SendEditor(SCI_GETSELECTIONNENDVIRTUALSPACE, ASelection, 0);
end;

procedure TDScintilla.SetPrintMagnification(AMagnification: Integer);
begin
  SendEditor(SCI_SETPRINTMAGNIFICATION, AMagnification, 0);
end;

function TDScintilla.GetPrintMagnification: Integer;
begin
  Result := SendEditor(SCI_GETPRINTMAGNIFICATION, 0, 0);
end;

procedure TDScintilla.SetPrintColourMode(AMode: Integer);
begin
  SendEditor(SCI_SETPRINTCOLOURMODE, AMode, 0);
end;

function TDScintilla.GetPrintColourMode: Integer;
begin
  Result := SendEditor(SCI_GETPRINTCOLOURMODE, 0, 0);
end;

function TDScintilla.GetFirstVisibleLine: NativeInt;
begin
  Result := SendEditor(SCI_GETFIRSTVISIBLELINE, 0, 0);
end;

function TDScintilla.GetLineCount: NativeInt;
begin
  Result := SendEditor(SCI_GETLINECOUNT, 0, 0);
end;

procedure TDScintilla.SetMarginLeft(APixelWidth: Integer);
begin
  SendEditor(SCI_SETMARGINLEFT, 0, APixelWidth);
end;

function TDScintilla.GetMarginLeft: Integer;
begin
  Result := SendEditor(SCI_GETMARGINLEFT, 0, 0);
end;

procedure TDScintilla.SetMarginRight(APixelWidth: Integer);
begin
  SendEditor(SCI_SETMARGINRIGHT, 0, APixelWidth);
end;

function TDScintilla.GetMarginRight: Integer;
begin
  Result := SendEditor(SCI_GETMARGINRIGHT, 0, 0);
end;

function TDScintilla.GetModify: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETMODIFY, 0, 0));
end;

procedure TDScintilla.SetReadOnly(AReadOnly: Boolean);
begin
  SendEditor(SCI_SETREADONLY, Integer(AReadOnly), 0);
end;

function TDScintilla.GetTextLength: NativeInt;
begin
  Result := SendEditor(SCI_GETTEXTLENGTH, 0, 0);
end;

function TDScintilla.GetDirectFunction: TDScintillaDirectFunction;
begin
  Result := TDScintillaDirectFunction(SendEditor(SCI_GETDIRECTFUNCTION, 0, 0));
end;

function TDScintilla.GetDirectStatusFunction: TDScintillaDirectStatusFunction;
begin
  Result := TDScintillaDirectStatusFunction(SendEditor(SCI_GETDIRECTSTATUSFUNCTION, 0, 0));
end;

function TDScintilla.GetDirectPointer: Pointer;
begin
  Result := Pointer(SendEditor(SCI_GETDIRECTPOINTER, 0, 0));
end;

procedure TDScintilla.SetOvertype(AOvertype: Boolean);
begin
  SendEditor(SCI_SETOVERTYPE, Integer(AOvertype), 0);
end;

function TDScintilla.GetOvertype: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETOVERTYPE, 0, 0));
end;

procedure TDScintilla.SetCaretWidth(APixelWidth: Integer);
begin
  SendEditor(SCI_SETCARETWIDTH, APixelWidth, 0);
end;

function TDScintilla.GetCaretWidth: Integer;
begin
  Result := SendEditor(SCI_GETCARETWIDTH, 0, 0);
end;

procedure TDScintilla.SetTargetStart(APos: NativeInt);
begin
  SendEditor(SCI_SETTARGETSTART, APos, 0);
end;

function TDScintilla.GetTargetStart: NativeInt;
begin
  Result := SendEditor(SCI_GETTARGETSTART, 0, 0);
end;

procedure TDScintilla.SetTargetEnd(APos: NativeInt);
begin
  SendEditor(SCI_SETTARGETEND, APos, 0);
end;

function TDScintilla.GetTargetEnd: NativeInt;
begin
  Result := SendEditor(SCI_GETTARGETEND, 0, 0);
end;

procedure TDScintilla.SetSearchFlags(AFlags: Integer);
begin
  SendEditor(SCI_SETSEARCHFLAGS, AFlags, 0);
end;

function TDScintilla.GetSearchFlags: Integer;
begin
  Result := SendEditor(SCI_GETSEARCHFLAGS, 0, 0);
end;

procedure TDScintilla.CallTipSetPosStart(APosStart: NativeInt);
begin
  SendEditor(SCI_CALLTIPSETPOSSTART, APosStart, 0);
end;

procedure TDScintilla.CallTipSetBack(ABack: TColor);
begin
  SendEditor(SCI_CALLTIPSETBACK, Integer(ABack), 0);
end;

procedure TDScintilla.CallTipSetFore(AFore: TColor);
begin
  SendEditor(SCI_CALLTIPSETFORE, Integer(AFore), 0);
end;

procedure TDScintilla.CallTipSetForeHlt(AFore: TColor);
begin
  SendEditor(SCI_CALLTIPSETFOREHLT, Integer(AFore), 0);
end;

procedure TDScintilla.CallTipUseStyle(ATabSize: Integer);
begin
  SendEditor(SCI_CALLTIPUSESTYLE, ATabSize, 0);
end;

procedure TDScintilla.CallTipSetPosition(AAbove: Boolean);
begin
  SendEditor(SCI_CALLTIPSETPOSITION, Integer(AAbove), 0);
end;

procedure TDScintilla.SetFoldLevel(ALine: NativeInt; ALevel: Integer);
begin
  SendEditor(SCI_SETFOLDLEVEL, ALine, ALevel);
end;

function TDScintilla.GetFoldLevel(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_GETFOLDLEVEL, ALine, 0);
end;

function TDScintilla.GetLastChild(ALine: NativeInt; ALevel: Integer): NativeInt;
begin
  Result := SendEditor(SCI_GETLASTCHILD, ALine, ALevel);
end;

function TDScintilla.GetFoldParent(ALine: NativeInt): NativeInt;
begin
  Result := SendEditor(SCI_GETFOLDPARENT, ALine, 0);
end;

function TDScintilla.GetLineVisible(ALine: NativeInt): Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETLINEVISIBLE, ALine, 0));
end;

function TDScintilla.GetAllLinesVisible: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETALLLINESVISIBLE, 0, 0));
end;

procedure TDScintilla.SetFoldExpanded(ALine: NativeInt; AExpanded: Boolean);
begin
  SendEditor(SCI_SETFOLDEXPANDED, ALine, Integer(AExpanded));
end;

function TDScintilla.GetFoldExpanded(ALine: NativeInt): Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETFOLDEXPANDED, ALine, 0));
end;

procedure TDScintilla.SetAutomaticFold(AAutomaticFold: Integer);
begin
  SendEditor(SCI_SETAUTOMATICFOLD, AAutomaticFold, 0);
end;

function TDScintilla.GetAutomaticFold: Integer;
begin
  Result := SendEditor(SCI_GETAUTOMATICFOLD, 0, 0);
end;

procedure TDScintilla.SetTabIndents(ATabIndents: Boolean);
begin
  SendEditor(SCI_SETTABINDENTS, Integer(ATabIndents), 0);
end;

function TDScintilla.GetTabIndents: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETTABINDENTS, 0, 0));
end;

procedure TDScintilla.SetBackSpaceUnIndents(ABsUnIndents: Boolean);
begin
  SendEditor(SCI_SETBACKSPACEUNINDENTS, Integer(ABsUnIndents), 0);
end;

function TDScintilla.GetBackSpaceUnIndents: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETBACKSPACEUNINDENTS, 0, 0));
end;

procedure TDScintilla.SetMouseDwellTime(APeriodMilliseconds: Integer);
begin
  SendEditor(SCI_SETMOUSEDWELLTIME, APeriodMilliseconds, 0);
end;

function TDScintilla.GetMouseDwellTime: Integer;
begin
  Result := SendEditor(SCI_GETMOUSEDWELLTIME, 0, 0);
end;

procedure TDScintilla.SetWrapMode(AMode: Integer);
begin
  SendEditor(SCI_SETWRAPMODE, AMode, 0);
end;

function TDScintilla.GetWrapMode: Integer;
begin
  Result := SendEditor(SCI_GETWRAPMODE, 0, 0);
end;

procedure TDScintilla.SetWrapVisualFlags(AWrapVisualFlags: Integer);
begin
  SendEditor(SCI_SETWRAPVISUALFLAGS, AWrapVisualFlags, 0);
end;

function TDScintilla.GetWrapVisualFlags: Integer;
begin
  Result := SendEditor(SCI_GETWRAPVISUALFLAGS, 0, 0);
end;

procedure TDScintilla.SetWrapVisualFlagsLocation(AWrapVisualFlagsLocation: Integer);
begin
  SendEditor(SCI_SETWRAPVISUALFLAGSLOCATION, AWrapVisualFlagsLocation, 0);
end;

function TDScintilla.GetWrapVisualFlagsLocation: Integer;
begin
  Result := SendEditor(SCI_GETWRAPVISUALFLAGSLOCATION, 0, 0);
end;

procedure TDScintilla.SetWrapStartIndent(AIndent: Integer);
begin
  SendEditor(SCI_SETWRAPSTARTINDENT, AIndent, 0);
end;

function TDScintilla.GetWrapStartIndent: Integer;
begin
  Result := SendEditor(SCI_GETWRAPSTARTINDENT, 0, 0);
end;

procedure TDScintilla.SetWrapIndentMode(AMode: Integer);
begin
  SendEditor(SCI_SETWRAPINDENTMODE, AMode, 0);
end;

function TDScintilla.GetWrapIndentMode: Integer;
begin
  Result := SendEditor(SCI_GETWRAPINDENTMODE, 0, 0);
end;

procedure TDScintilla.SetLayoutCache(AMode: Integer);
begin
  SendEditor(SCI_SETLAYOUTCACHE, AMode, 0);
end;

function TDScintilla.GetLayoutCache: Integer;
begin
  Result := SendEditor(SCI_GETLAYOUTCACHE, 0, 0);
end;

procedure TDScintilla.SetScrollWidth(APixelWidth: Integer);
begin
  SendEditor(SCI_SETSCROLLWIDTH, APixelWidth, 0);
end;

function TDScintilla.GetScrollWidth: Integer;
begin
  Result := SendEditor(SCI_GETSCROLLWIDTH, 0, 0);
end;

procedure TDScintilla.SetScrollWidthTracking(ATracking: Boolean);
begin
  SendEditor(SCI_SETSCROLLWIDTHTRACKING, Integer(ATracking), 0);
end;

function TDScintilla.GetScrollWidthTracking: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETSCROLLWIDTHTRACKING, 0, 0));
end;

procedure TDScintilla.SetEndAtLastLine(AEndAtLastLine: Boolean);
begin
  SendEditor(SCI_SETENDATLASTLINE, Integer(AEndAtLastLine), 0);
end;

function TDScintilla.GetEndAtLastLine: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETENDATLASTLINE, 0, 0));
end;

procedure TDScintilla.SetVScrollBar(AShow: Boolean);
begin
  SendEditor(SCI_SETVSCROLLBAR, Integer(AShow), 0);
end;

function TDScintilla.GetVScrollBar: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETVSCROLLBAR, 0, 0));
end;

procedure TDScintilla.SetFontQuality(AFontQuality: Integer);
begin
  SendEditor(SCI_SETFONTQUALITY, AFontQuality, 0);
end;

function TDScintilla.GetFontQuality: Integer;
begin
  Result := SendEditor(SCI_GETFONTQUALITY, 0, 0);
end;

procedure TDScintilla.SetFirstVisibleLine(ALineDisplay: NativeInt);
begin
  SendEditor(SCI_SETFIRSTVISIBLELINE, ALineDisplay, 0);
end;

procedure TDScintilla.SetMultiPaste(AMultiPaste: Integer);
begin
  SendEditor(SCI_SETMULTIPASTE, AMultiPaste, 0);
end;

function TDScintilla.GetMultiPaste: Integer;
begin
  Result := SendEditor(SCI_GETMULTIPASTE, 0, 0);
end;

function TDScintilla.GetViewEOL: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETVIEWEOL, 0, 0));
end;

procedure TDScintilla.SetViewEOL(AVisible: Boolean);
begin
  SendEditor(SCI_SETVIEWEOL, Integer(AVisible), 0);
end;

function TDScintilla.GetDocPointer: TDSciDocument;
begin
  Result := TDSciDocument(SendEditor(SCI_GETDOCPOINTER, 0, 0));
end;

procedure TDScintilla.SetDocPointer(APointer: TDSciDocument);
begin
  if APointer <> nil then
    SendEditor(SCI_SETDOCPOINTER, 0, NativeInt(APointer));
end;

procedure TDScintilla.SetModEventMask(AMask: Integer);
begin
  SendEditor(SCI_SETMODEVENTMASK, AMask, 0);
end;

function TDScintilla.GetEdgeColumn: NativeInt;
begin
  Result := SendEditor(SCI_GETEDGECOLUMN, 0, 0);
end;

procedure TDScintilla.SetEdgeColumn(AColumn: NativeInt);
begin
  SendEditor(SCI_SETEDGECOLUMN, AColumn, 0);
end;

function TDScintilla.GetEdgeMode: Integer;
begin
  Result := SendEditor(SCI_GETEDGEMODE, 0, 0);
end;

procedure TDScintilla.SetEdgeMode(AMode: Integer);
begin
  SendEditor(SCI_SETEDGEMODE, AMode, 0);
end;

function TDScintilla.GetEdgeColour: TColor;
begin
  Result := TColor(SendEditor(SCI_GETEDGECOLOUR, 0, 0));
end;

procedure TDScintilla.SetEdgeColour(AEdgeColour: TColor);
begin
  SendEditor(SCI_SETEDGECOLOUR, Integer(AEdgeColour), 0);
end;

procedure TDScintilla.SetZoom(AZoom: Integer);
begin
  SendEditor(SCI_SETZOOM, AZoom, 0);
end;

function TDScintilla.GetZoom: Integer;
begin
  Result := SendEditor(SCI_GETZOOM, 0, 0);
end;

function TDScintilla.GetModEventMask: Integer;
begin
  Result := SendEditor(SCI_GETMODEVENTMASK, 0, 0);
end;

procedure TDScintilla.SetFocus(AFocus: Boolean);
begin
  SendEditor(SCI_SETFOCUS, Integer(AFocus), 0);
end;

function TDScintilla.GetFocus: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETFOCUS, 0, 0));
end;

procedure TDScintilla.SetStatus(AStatusCode: Integer);
begin
  SendEditor(SCI_SETSTATUS, AStatusCode, 0);
end;

function TDScintilla.GetStatus: Integer;
begin
  Result := SendEditor(SCI_GETSTATUS, 0, 0);
end;

procedure TDScintilla.SetMouseDownCaptures(ACaptures: Boolean);
begin
  SendEditor(SCI_SETMOUSEDOWNCAPTURES, Integer(ACaptures), 0);
end;

function TDScintilla.GetMouseDownCaptures: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETMOUSEDOWNCAPTURES, 0, 0));
end;

procedure TDScintilla.SetCursor(ACursorType: Integer);
begin
  SendEditor(SCI_SETCURSOR, ACursorType, 0);
end;

function TDScintilla.GetCursor: Integer;
begin
  Result := SendEditor(SCI_GETCURSOR, 0, 0);
end;

procedure TDScintilla.SetControlCharSymbol(ASymbol: Integer);
begin
  SendEditor(SCI_SETCONTROLCHARSYMBOL, ASymbol, 0);
end;

function TDScintilla.GetControlCharSymbol: Integer;
begin
  Result := SendEditor(SCI_GETCONTROLCHARSYMBOL, 0, 0);
end;

procedure TDScintilla.SetXOffset(ANewOffset: Integer);
begin
  SendEditor(SCI_SETXOFFSET, ANewOffset, 0);
end;

function TDScintilla.GetXOffset: Integer;
begin
  Result := SendEditor(SCI_GETXOFFSET, 0, 0);
end;

procedure TDScintilla.SetPrintWrapMode(AMode: Integer);
begin
  SendEditor(SCI_SETPRINTWRAPMODE, AMode, 0);
end;

function TDScintilla.GetPrintWrapMode: Integer;
begin
  Result := SendEditor(SCI_GETPRINTWRAPMODE, 0, 0);
end;

procedure TDScintilla.SetHotspotActiveFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendEditor(SCI_SETHOTSPOTACTIVEFORE, Integer(AUseSetting), Integer(AFore));
end;

function TDScintilla.GetHotspotActiveFore: TColor;
begin
  Result := TColor(SendEditor(SCI_GETHOTSPOTACTIVEFORE, 0, 0));
end;

procedure TDScintilla.SetHotspotActiveBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendEditor(SCI_SETHOTSPOTACTIVEBACK, Integer(AUseSetting), Integer(ABack));
end;

function TDScintilla.GetHotspotActiveBack: TColor;
begin
  Result := TColor(SendEditor(SCI_GETHOTSPOTACTIVEBACK, 0, 0));
end;

procedure TDScintilla.SetHotspotActiveUnderline(AUnderline: Boolean);
begin
  SendEditor(SCI_SETHOTSPOTACTIVEUNDERLINE, Integer(AUnderline), 0);
end;

function TDScintilla.GetHotspotActiveUnderline: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETHOTSPOTACTIVEUNDERLINE, 0, 0));
end;

procedure TDScintilla.SetHotspotSingleLine(ASingleLine: Boolean);
begin
  SendEditor(SCI_SETHOTSPOTSINGLELINE, Integer(ASingleLine), 0);
end;

function TDScintilla.GetHotspotSingleLine: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETHOTSPOTSINGLELINE, 0, 0));
end;

procedure TDScintilla.SetSelectionMode(AMode: Integer);
begin
  SendEditor(SCI_SETSELECTIONMODE, AMode, 0);
end;

function TDScintilla.GetSelectionMode: Integer;
begin
  Result := SendEditor(SCI_GETSELECTIONMODE, 0, 0);
end;

procedure TDScintilla.SetWhitespaceChars(const ACharacters: AnsiString);
begin
  FHelper.SetTextA(SCI_SETWHITESPACECHARS, 0, ACharacters);
end;

function TDScintilla.GetWhitespaceChars: AnsiString;
begin
  FHelper.GetTextA(SCI_GETWHITESPACECHARS, 0, Result);
end;

procedure TDScintilla.SetPunctuationChars(const ACharacters: AnsiString);
begin
  FHelper.SetTextA(SCI_SETPUNCTUATIONCHARS, 0, ACharacters);
end;

function TDScintilla.GetPunctuationChars: AnsiString;
begin
  FHelper.GetTextA(SCI_GETPUNCTUATIONCHARS, 0, Result);
end;

procedure TDScintilla.AutoCSetCaseInsensitiveBehaviour(ABehaviour: Integer);
begin
  SendEditor(SCI_AUTOCSETCASEINSENSITIVEBEHAVIOUR, ABehaviour, 0);
end;

function TDScintilla.AutoCGetCaseInsensitiveBehaviour: Integer;
begin
  Result := SendEditor(SCI_AUTOCGETCASEINSENSITIVEBEHAVIOUR, 0, 0);
end;

procedure TDScintilla.AutoCSetOrder(AOrder: Integer);
begin
  SendEditor(SCI_AUTOCSETORDER, AOrder, 0);
end;

function TDScintilla.AutoCGetOrder: Integer;
begin
  Result := SendEditor(SCI_AUTOCGETORDER, 0, 0);
end;

function TDScintilla.GetCaretSticky: Integer;
begin
  Result := SendEditor(SCI_GETCARETSTICKY, 0, 0);
end;

procedure TDScintilla.SetCaretSticky(AUseCaretStickyBehaviour: Integer);
begin
  SendEditor(SCI_SETCARETSTICKY, AUseCaretStickyBehaviour, 0);
end;

procedure TDScintilla.SetPasteConvertEndings(AConvert: Boolean);
begin
  SendEditor(SCI_SETPASTECONVERTENDINGS, Integer(AConvert), 0);
end;

function TDScintilla.GetPasteConvertEndings: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETPASTECONVERTENDINGS, 0, 0));
end;

procedure TDScintilla.SetCaretLineBackAlpha(AAlpha: Integer);
begin
  SendEditor(SCI_SETCARETLINEBACKALPHA, AAlpha, 0);
end;

function TDScintilla.GetCaretLineBackAlpha: Integer;
begin
  Result := SendEditor(SCI_GETCARETLINEBACKALPHA, 0, 0);
end;

procedure TDScintilla.SetCaretStyle(ACaretStyle: Integer);
begin
  SendEditor(SCI_SETCARETSTYLE, ACaretStyle, 0);
end;

function TDScintilla.GetCaretStyle: Integer;
begin
  Result := SendEditor(SCI_GETCARETSTYLE, 0, 0);
end;

procedure TDScintilla.SetIndicatorCurrent(AIndicator: Integer);
begin
  SendEditor(SCI_SETINDICATORCURRENT, AIndicator, 0);
end;

function TDScintilla.GetIndicatorCurrent: Integer;
begin
  Result := SendEditor(SCI_GETINDICATORCURRENT, 0, 0);
end;

procedure TDScintilla.SetIndicatorValue(AValue: Integer);
begin
  SendEditor(SCI_SETINDICATORVALUE, AValue, 0);
end;

function TDScintilla.GetIndicatorValue: Integer;
begin
  Result := SendEditor(SCI_GETINDICATORVALUE, 0, 0);
end;

procedure TDScintilla.SetPositionCache(ASize: NativeInt);
begin
  SendEditor(SCI_SETPOSITIONCACHE, ASize, 0);
end;

function TDScintilla.GetPositionCache: NativeInt;
begin
  Result := SendEditor(SCI_GETPOSITIONCACHE, 0, 0);
end;

function TDScintilla.GetCharacterPointer: PByte;
begin
  Result := PByte(SendEditor(SCI_GETCHARACTERPOINTER, 0, 0));
end;

function TDScintilla.GetRangePointer(APosition: NativeInt; ARangeLength: NativeInt): Pointer;
begin
  Result := Pointer(SendEditor(SCI_GETRANGEPOINTER, APosition, ARangeLength));
end;

procedure TDScintilla.IndicSetAlpha(AIndicator: Integer; AAlpha: Integer);
begin
  SendEditor(SCI_INDICSETALPHA, AIndicator, AAlpha);
end;

function TDScintilla.IndicGetAlpha(AIndicator: Integer): Integer;
begin
  Result := SendEditor(SCI_INDICGETALPHA, AIndicator, 0);
end;

procedure TDScintilla.IndicSetOutlineAlpha(AIndicator: Integer; AAlpha: Integer);
begin
  SendEditor(SCI_INDICSETOUTLINEALPHA, AIndicator, AAlpha);
end;

function TDScintilla.IndicGetOutlineAlpha(AIndicator: Integer): Integer;
begin
  Result := SendEditor(SCI_INDICGETOUTLINEALPHA, AIndicator, 0);
end;

procedure TDScintilla.SetExtraAscent(AExtraAscent: Integer);
begin
  SendEditor(SCI_SETEXTRAASCENT, AExtraAscent, 0);
end;

function TDScintilla.GetExtraAscent: Integer;
begin
  Result := SendEditor(SCI_GETEXTRAASCENT, 0, 0);
end;

procedure TDScintilla.SetExtraDescent(AExtraDescent: Integer);
begin
  SendEditor(SCI_SETEXTRADESCENT, AExtraDescent, 0);
end;

function TDScintilla.GetExtraDescent: Integer;
begin
  Result := SendEditor(SCI_GETEXTRADESCENT, 0, 0);
end;

procedure TDScintilla.MarginSetText(ALine: NativeInt; const AText: UnicodeString);
begin
  FHelper.SetText(SCI_MARGINSETTEXT, ALine, AText);
end;

function TDScintilla.MarginGetText(ALine: NativeInt): UnicodeString;
begin
  FHelper.GetText(SCI_MARGINGETTEXT, ALine, Result);
end;

procedure TDScintilla.MarginSetStyle(ALine: NativeInt; AStyle: Integer);
begin
  SendEditor(SCI_MARGINSETSTYLE, ALine, AStyle);
end;

function TDScintilla.MarginGetStyle(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_MARGINGETSTYLE, ALine, 0);
end;

procedure TDScintilla.MarginSetStyles(ALine: NativeInt; const AStyles: TDSciStyles);
begin
  SendEditor(SCI_MARGINSETSTYLES, ALine, NativeInt(AStyles));
end;

function TDScintilla.MarginGetStyles(ALine: NativeInt): TDSciStyles;
begin
  SetLength(Result, SendEditor(SCI_MARGINGETSTYLES, ALine));
  if Length(Result) > 0 then
    SendEditor(SCI_MARGINGETSTYLES, ALine, NativeInt(Result));
end;

procedure TDScintilla.MarginSetStyleOffset(AStyle: Integer);
begin
  SendEditor(SCI_MARGINSETSTYLEOFFSET, AStyle, 0);
end;

function TDScintilla.MarginGetStyleOffset: Integer;
begin
  Result := SendEditor(SCI_MARGINGETSTYLEOFFSET, 0, 0);
end;

procedure TDScintilla.SetMarginOptions(AMarginOptions: Integer);
begin
  SendEditor(SCI_SETMARGINOPTIONS, AMarginOptions, 0);
end;

function TDScintilla.GetMarginOptions: Integer;
begin
  Result := SendEditor(SCI_GETMARGINOPTIONS, 0, 0);
end;

procedure TDScintilla.AnnotationSetText(ALine: NativeInt; const AText: UnicodeString);
begin
  FHelper.SetText(SCI_ANNOTATIONSETTEXT, ALine, AText);
end;

function TDScintilla.AnnotationGetText(ALine: NativeInt): UnicodeString;
begin
  FHelper.SetText(SCI_ANNOTATIONGETTEXT, ALine, Result);
end;
procedure TDScintilla.AnnotationSetStyle(ALine: NativeInt; AStyle: Integer);
begin
  SendEditor(SCI_ANNOTATIONSETSTYLE, ALine, AStyle);
end;

function TDScintilla.AnnotationGetStyle(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_ANNOTATIONGETSTYLE, ALine, 0);
end;

procedure TDScintilla.AnnotationSetStyles(ALine: NativeInt; const AStyles: TDSciStyles);
begin
  SendEditor(SCI_ANNOTATIONSETSTYLES, ALine, NativeInt(AStyles));
end;

function TDScintilla.AnnotationGetStyles(ALine: NativeInt): TDSciStyles;
begin
  SetLength(Result, SendEditor(SCI_ANNOTATIONGETSTYLES, ALine));
  if Length(Result) > 0 then
    SendEditor(SCI_ANNOTATIONGETSTYLES, ALine, NativeInt(Result));
end;

function TDScintilla.AnnotationGetLines(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_ANNOTATIONGETLINES, ALine, 0);
end;

procedure TDScintilla.AnnotationSetVisible(AVisible: Integer);
begin
  SendEditor(SCI_ANNOTATIONSETVISIBLE, AVisible, 0);
end;

function TDScintilla.AnnotationGetVisible: Integer;
begin
  Result := SendEditor(SCI_ANNOTATIONGETVISIBLE, 0, 0);
end;

procedure TDScintilla.AnnotationSetStyleOffset(AStyle: Integer);
begin
  SendEditor(SCI_ANNOTATIONSETSTYLEOFFSET, AStyle, 0);
end;

function TDScintilla.AnnotationGetStyleOffset: Integer;
begin
  Result := SendEditor(SCI_ANNOTATIONGETSTYLEOFFSET, 0, 0);
end;

procedure TDScintilla.SetMouseSelectionRectangularSwitch(AMouseSelectionRectangularSwitch: Boolean);
begin
  SendEditor(SCI_SETMOUSESELECTIONRECTANGULARSWITCH, Integer(AMouseSelectionRectangularSwitch), 0);
end;

function TDScintilla.GetMouseSelectionRectangularSwitch: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETMOUSESELECTIONRECTANGULARSWITCH, 0, 0));
end;

procedure TDScintilla.SetMultipleSelection(AMultipleSelection: Boolean);
begin
  SendEditor(SCI_SETMULTIPLESELECTION, Integer(AMultipleSelection), 0);
end;

function TDScintilla.GetMultipleSelection: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETMULTIPLESELECTION, 0, 0));
end;

procedure TDScintilla.SetAdditionalSelectionTyping(AAdditionalSelectionTyping: Boolean);
begin
  SendEditor(SCI_SETADDITIONALSELECTIONTYPING, Integer(AAdditionalSelectionTyping), 0);
end;

function TDScintilla.GetAdditionalSelectionTyping: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETADDITIONALSELECTIONTYPING, 0, 0));
end;

procedure TDScintilla.SetAdditionalCaretsBlink(AAdditionalCaretsBlink: Boolean);
begin
  SendEditor(SCI_SETADDITIONALCARETSBLINK, Integer(AAdditionalCaretsBlink), 0);
end;

function TDScintilla.GetAdditionalCaretsBlink: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETADDITIONALCARETSBLINK, 0, 0));
end;

procedure TDScintilla.SetAdditionalCaretsVisible(AAdditionalCaretsBlink: Boolean);
begin
  SendEditor(SCI_SETADDITIONALCARETSVISIBLE, Integer(AAdditionalCaretsBlink), 0);
end;

function TDScintilla.GetAdditionalCaretsVisible: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETADDITIONALCARETSVISIBLE, 0, 0));
end;

function TDScintilla.GetSelections: NativeInt;
begin
  Result := SendEditor(SCI_GETSELECTIONS, 0, 0);
end;

function TDScintilla.GetSelectionEmpty: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETSELECTIONEMPTY, 0, 0));
end;

procedure TDScintilla.SetMainSelection(ASelection: NativeInt);
begin
  SendEditor(SCI_SETMAINSELECTION, ASelection, 0);
end;

function TDScintilla.GetMainSelection: NativeInt;
begin
  Result := SendEditor(SCI_GETMAINSELECTION, 0, 0);
end;

procedure TDScintilla.SetSelectionNCaret(ASelection: NativeInt; APos: NativeInt);
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    SendEditor(SCI_SETSELECTIONNCARET, ASelection, APos);
end;

function TDScintilla.GetSelectionNCaret(ASelection: NativeInt): NativeInt;
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    Result := SendEditor(SCI_GETSELECTIONNCARET, ASelection, 0)
  else
    Result := INVALID_POSITION;
end;

procedure TDScintilla.SetSelectionNAnchor(ASelection: NativeInt; APosAnchor: NativeInt);
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    SendEditor(SCI_SETSELECTIONNANCHOR, ASelection, APosAnchor);
end;

function TDScintilla.GetSelectionNAnchor(ASelection: NativeInt): NativeInt;
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    Result := SendEditor(SCI_GETSELECTIONNANCHOR, ASelection, 0)
  else
    Result := INVALID_POSITION;
end;

procedure TDScintilla.SetSelectionNCaretVirtualSpace(ASelection: NativeInt; ASpace: NativeInt);
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    SendEditor(SCI_SETSELECTIONNCARETVIRTUALSPACE, ASelection, ASpace);
end;

function TDScintilla.GetSelectionNCaretVirtualSpace(ASelection: NativeInt): NativeInt;
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    Result := SendEditor(SCI_GETSELECTIONNCARETVIRTUALSPACE, ASelection, 0)
  else
    Result := INVALID_POSITION;
end;

procedure TDScintilla.SetSelectionNAnchorVirtualSpace(ASelection: NativeInt; ASpace: NativeInt);
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    SendEditor(SCI_SETSELECTIONNANCHORVIRTUALSPACE, ASelection, ASpace);
end;

function TDScintilla.GetSelectionNAnchorVirtualSpace(ASelection: NativeInt): NativeInt;
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    Result := SendEditor(SCI_GETSELECTIONNANCHORVIRTUALSPACE, ASelection, 0)
  else
    Result := INVALID_POSITION;
end;

procedure TDScintilla.SetSelectionNStart(ASelection: NativeInt; APos: NativeInt);
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    SendEditor(SCI_SETSELECTIONNSTART, ASelection, APos);
end;

function TDScintilla.GetSelectionNStart(ASelection: NativeInt): NativeInt;
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    Result := SendEditor(SCI_GETSELECTIONNSTART, ASelection, 0)
  else
    Result := INVALID_POSITION;
end;

procedure TDScintilla.SetSelectionNEnd(ASelection: NativeInt; APos: NativeInt);
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    SendEditor(SCI_SETSELECTIONNEND, ASelection, APos);
end;

function TDScintilla.GetSelectionNEnd(ASelection: NativeInt): NativeInt;
begin
  if (ASelection >= 0) and (ASelection < GetSelections) then
    Result := SendEditor(SCI_GETSELECTIONNEND, ASelection, 0)
  else
    Result := INVALID_POSITION;
end;

procedure TDScintilla.SetRectangularSelectionCaret(APos: NativeInt);
begin
  SendEditor(SCI_SETRECTANGULARSELECTIONCARET, APos, 0);
end;

function TDScintilla.GetRectangularSelectionCaret: NativeInt;
begin
  Result := SendEditor(SCI_GETRECTANGULARSELECTIONCARET, 0, 0);
end;

procedure TDScintilla.SetRectangularSelectionAnchor(APosAnchor: NativeInt);
begin
  SendEditor(SCI_SETRECTANGULARSELECTIONANCHOR, APosAnchor, 0);
end;

function TDScintilla.GetRectangularSelectionAnchor: NativeInt;
begin
  Result := SendEditor(SCI_GETRECTANGULARSELECTIONANCHOR, 0, 0);
end;

procedure TDScintilla.SetRectangularSelectionCaretVirtualSpace(ASpace: NativeInt);
begin
  SendEditor(SCI_SETRECTANGULARSELECTIONCARETVIRTUALSPACE, ASpace, 0);
end;

function TDScintilla.GetRectangularSelectionCaretVirtualSpace: NativeInt;
begin
  Result := SendEditor(SCI_GETRECTANGULARSELECTIONCARETVIRTUALSPACE, 0, 0);
end;

procedure TDScintilla.SetRectangularSelectionAnchorVirtualSpace(ASpace: NativeInt);
begin
  SendEditor(SCI_SETRECTANGULARSELECTIONANCHORVIRTUALSPACE, ASpace, 0);
end;

function TDScintilla.GetRectangularSelectionAnchorVirtualSpace: NativeInt;
begin
  Result := SendEditor(SCI_GETRECTANGULARSELECTIONANCHORVIRTUALSPACE, 0, 0);
end;

procedure TDScintilla.SetVirtualSpaceOptions(AVirtualSpaceOptions: Integer);
begin
  SendEditor(SCI_SETVIRTUALSPACEOPTIONS, AVirtualSpaceOptions, 0);
end;

function TDScintilla.GetVirtualSpaceOptions: Integer;
begin
  Result := SendEditor(SCI_GETVIRTUALSPACEOPTIONS, 0, 0);
end;

procedure TDScintilla.SetRectangularSelectionModifier(AModifier: Integer);
begin
  SendEditor(SCI_SETRECTANGULARSELECTIONMODIFIER, AModifier, 0);
end;

function TDScintilla.GetRectangularSelectionModifier: Integer;
begin
  Result := SendEditor(SCI_GETRECTANGULARSELECTIONMODIFIER, 0, 0);
end;

procedure TDScintilla.SetAdditionalSelFore(AFore: TColor);
begin
  SendEditor(SCI_SETADDITIONALSELFORE, Integer(AFore), 0);
end;

procedure TDScintilla.SetAdditionalSelBack(ABack: TColor);
begin
  SendEditor(SCI_SETADDITIONALSELBACK, Integer(ABack), 0);
end;

procedure TDScintilla.SetAdditionalSelAlpha(AAlpha: Integer);
begin
  SendEditor(SCI_SETADDITIONALSELALPHA, AAlpha, 0);
end;

function TDScintilla.GetAdditionalSelAlpha: Integer;
begin
  Result := SendEditor(SCI_GETADDITIONALSELALPHA, 0, 0);
end;

procedure TDScintilla.SetAdditionalCaretFore(AFore: TColor);
begin
  SendEditor(SCI_SETADDITIONALCARETFORE, Integer(AFore), 0);
end;

function TDScintilla.GetAdditionalCaretFore: TColor;
begin
  Result := TColor(SendEditor(SCI_GETADDITIONALCARETFORE, 0, 0));
end;

procedure TDScintilla.SetIdentifier(AIdentifier: Integer);
begin
  SendEditor(SCI_SETIDENTIFIER, AIdentifier, 0);
end;

function TDScintilla.GetIdentifier: Integer;
begin
  Result := SendEditor(SCI_GETIDENTIFIER, 0, 0);
end;

procedure TDScintilla.RGBAImageSetWidth(AWidth: Integer);
begin
  SendEditor(SCI_RGBAIMAGESETWIDTH, AWidth, 0);
end;

procedure TDScintilla.RGBAImageSetHeight(AHeight: Integer);
begin
  SendEditor(SCI_RGBAIMAGESETHEIGHT, AHeight, 0);
end;

procedure TDScintilla.SetTechnology(ATechnology: Integer);
begin
  SendEditor(SCI_SETTECHNOLOGY, ATechnology, 0);
end;

function TDScintilla.GetTechnology: Integer;
begin
  Result := SendEditor(SCI_GETTECHNOLOGY, 0, 0);
end;

function TDScintilla.GetCaretLineVisibleAlways: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETCARETLINEVISIBLEALWAYS, 0, 0));
end;

procedure TDScintilla.SetCaretLineVisibleAlways(AAlwaysVisible: Boolean);
begin
  SendEditor(SCI_SETCARETLINEVISIBLEALWAYS, Integer(AAlwaysVisible), 0);
end;

procedure TDScintilla.SetLineEndTypesAllowed(ALineEndBitSet: Integer);
begin
  SendEditor(SCI_SETLINEENDTYPESALLOWED, ALineEndBitSet, 0);
end;

function TDScintilla.GetLineEndTypesAllowed: Integer;
begin
  Result := SendEditor(SCI_GETLINEENDTYPESALLOWED, 0, 0);
end;

function TDScintilla.GetLineEndTypesActive: Integer;
begin
  Result := SendEditor(SCI_GETLINEENDTYPESACTIVE, 0, 0);
end;

procedure TDScintilla.SetRepresentation(AEncodedCharacter: AnsiString; ARepresentation: AnsiString);
begin
  FHelper.SetTextA(SCI_SETREPRESENTATION, AEncodedCharacter, ARepresentation);
end;

function TDScintilla.GetRepresentation(AEncodedCharacter: AnsiString): AnsiString;
begin
  FHelper.GetTextA(SCI_GETREPRESENTATION, AEncodedCharacter, Result);
end;

procedure TDScintilla.SetRepresentationAppearance(AEncodedCharacter: AnsiString; AAppearance: Integer);
begin
  SendEditor(SCI_SETREPRESENTATIONAPPEARANCE, NativeInt(FHelper.GetPtrFromAStr(AEncodedCharacter)), AAppearance);
end;

function TDScintilla.GetRepresentationAppearance(AEncodedCharacter: AnsiString): Integer;
begin
  Result := SendEditor(SCI_GETREPRESENTATIONAPPEARANCE, NativeInt(FHelper.GetPtrFromAStr(AEncodedCharacter)), 0);
end;

procedure TDScintilla.SetRepresentationColour(AEncodedCharacter: AnsiString; AColour: TDSciColourAlpha);
begin
  SendEditor(SCI_SETREPRESENTATIONCOLOUR, NativeInt(FHelper.GetPtrFromAStr(AEncodedCharacter)), Integer(AColour));
end;

function TDScintilla.GetRepresentationColour(AEncodedCharacter: AnsiString): TDSciColourAlpha;
begin
  Result := TDSciColourAlpha(SendEditor(SCI_GETREPRESENTATIONCOLOUR, NativeInt(FHelper.GetPtrFromAStr(AEncodedCharacter)), 0));
end;

procedure TDScintilla.SetILexer(ALexer: TDSciLexer);
begin
  SendEditor(SCI_SETILEXER, 0, NativeInt(ALexer));
end;

function TDScintilla.GetLexer: Integer;
begin
  Result := SendEditor(SCI_GETLEXER, 0, 0);
end;

procedure TDScintilla.SetProperty(const AKey: UnicodeString; const AValue: UnicodeString);
begin
  if AKey <> '' then
    FHelper.SetText(SCI_SETPROPERTY, AKey, AValue);
end;

function TDScintilla.GetPropertyInt(const AKey: UnicodeString; ADefault: Integer): Integer;
begin
  if AKey = '' then
    Result := ADefault
  else
    Result := SendEditor(SCI_GETPROPERTYINT, NativeInt(AnsiString(AKey)), ADefault);
end;

function TDScintilla.GetLexerLanguage: UnicodeString;
begin
  FHelper.GetText(SCI_GETLEXERLANGUAGE, 0, Result);
end;

function TDScintilla.GetLineEndTypesSupported: Integer;
begin
  Result := SendEditor(SCI_GETLINEENDTYPESSUPPORTED, 0, 0);
end;

function TDScintilla.GetSubStylesStart(AStyleBase: Integer): Integer;
begin
  Result := SendEditor(SCI_GETSUBSTYLESSTART, AStyleBase, 0);
end;

function TDScintilla.GetSubStylesLength(AStyleBase: Integer): Integer;
begin
  Result := SendEditor(SCI_GETSUBSTYLESLENGTH, AStyleBase, 0);
end;

function TDScintilla.GetStyleFromSubStyle(ASubStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_GETSTYLEFROMSUBSTYLE, ASubStyle, 0);
end;

function TDScintilla.GetPrimaryStyleFromStyle(AStyle: Integer): Integer;
begin
  Result := SendEditor(SCI_GETPRIMARYSTYLEFROMSTYLE, AStyle, 0);
end;

procedure TDScintilla.SetIdentifiers(AStyle: Integer; const AIdentifiers: AnsiString);
begin
  FHelper.SetTextA(SCI_SETIDENTIFIERS, AStyle, AIdentifiers);
end;

function TDScintilla.DistanceToSecondaryStyles: Integer;
begin
  Result := SendEditor(SCI_DISTANCETOSECONDARYSTYLES, 0, 0);
end;

function TDScintilla.GetSubStyleBases: UnicodeString;
begin
  FHelper.GetText(SCI_GETSUBSTYLEBASES, 0, Result);
end;

procedure TDScintilla.AnnotationSetText(ALine: NativeInt);
begin
  SendEditor(SCI_ANNOTATIONSETTEXT, ALine, 0);
end;

function TDScintilla.GetNamedStyles: Integer;
begin
  Result := SendEditor(SCI_GETNAMEDSTYLES, 0, 0);
end;

function TDScintilla.NameOfStyle(AStyle: Integer): UnicodeString;
begin
  FHelper.GetText(SCI_NAMEOFSTYLE, AStyle, Result);
end;

function TDScintilla.TagsOfStyle(AStyle: Integer): UnicodeString;
begin
  FHelper.GetText(SCI_TAGSOFSTYLE, AStyle, Result);
end;

function TDScintilla.DescriptionOfStyle(AStyle: Integer): UnicodeString;
begin
  FHelper.GetText(SCI_DESCRIPTIONOFSTYLE, AStyle, Result);
end;

procedure TDScintilla.SetElementColour(AElement: Integer; AColorElement: TDSciColourAlpha);
begin
  SendEditor(SCI_SETELEMENTCOLOUR, AElement, Integer(AColorElement));
end;

function TDScintilla.GetElementColour(AElement: Integer): TDSciColourAlpha;
begin
  Result := TDSciColourAlpha(SendEditor(SCI_GETELEMENTCOLOUR, AElement, 0));
end;

procedure TDScintilla.ResetElementColour(AElement: Integer);
begin
  SendEditor(SCI_RESETELEMENTCOLOUR, AElement, 0);
end;

function TDScintilla.GetElementIsSet(AElement: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETELEMENTISSET, AElement, 0));
end;

function TDScintilla.GetElementAllowsTranslucent(AElement: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETELEMENTALLOWSTRANSLUCENT, AElement, 0));
end;

function TDScintilla.GetElementBaseColour(AElement: Integer): TDSciColourAlpha;
begin
  Result := TDSciColourAlpha(SendEditor(SCI_GETELEMENTBASECOLOUR, AElement, 0));
end;

function TDScintilla.GetSelectionLayer: Integer;
begin
  Result := SendEditor(SCI_GETSELECTIONLAYER, 0, 0);
end;

procedure TDScintilla.SetSelectionLayer(ALayer: Integer);
begin
  SendEditor(SCI_SETSELECTIONLAYER, ALayer, 0);
end;

function TDScintilla.GetCaretLineLayer: Integer;
begin
  Result := SendEditor(SCI_GETCARETLINELAYER, 0, 0);
end;

procedure TDScintilla.SetCaretLineLayer(ALayer: Integer);
begin
  SendEditor(SCI_SETCARETLINELAYER, ALayer, 0);
end;

procedure TDScintilla.SetCommandEvents(ASendCommandEvents: Boolean);
begin
  SendEditor(SCI_SETCOMMANDEVENTS, Integer(ASendCommandEvents), 0);
end;

function TDScintilla.GetCommandEvents: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETCOMMANDEVENTS, 0, 0));
end;

procedure TDScintilla.SetTargetStartVirtualSpace(ASpace: NativeInt);
begin
  SendEditor(SCI_SETTARGETSTARTVIRTUALSPACE, ASpace, 0);
end;

function TDScintilla.GetTargetStartVirtualSpace: NativeInt;
begin
  Result := SendEditor(SCI_GETTARGETSTARTVIRTUALSPACE, 0, 0);
end;

procedure TDScintilla.SetTargetEndVirtualSpace(ASpace: NativeInt);
begin
  SendEditor(SCI_SETTARGETENDVIRTUALSPACE, ASpace, 0);
end;

function TDScintilla.GetTargetEndVirtualSpace: NativeInt;
begin
  Result := SendEditor(SCI_GETTARGETENDVIRTUALSPACE, 0, 0);
end;

procedure TDScintilla.SetMarginBackN(AMargin: Integer; ABack: TColor);
begin
  SendEditor(SCI_SETMARGINBACKN, AMargin, Integer(ABack));
end;

function TDScintilla.GetMarginBackN(AMargin: Integer): TColor;
begin
  Result := TColor(SendEditor(SCI_GETMARGINBACKN, AMargin, 0));
end;

procedure TDScintilla.SetMargins(AMargins: Integer);
begin
  SendEditor(SCI_SETMARGINS, AMargins, 0);
end;

function TDScintilla.GetMargins: Integer;
begin
  Result := SendEditor(SCI_GETMARGINS, 0, 0);
end;

procedure TDScintilla.StyleSetCheckMonospaced(AStyle: Integer; ACheckMonospaced: Boolean);
begin
  SendEditor(SCI_STYLESETCHECKMONOSPACED, AStyle, Integer(ACheckMonospaced));
end;

function TDScintilla.StyleGetCheckMonospaced(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendEditor(SCI_STYLEGETCHECKMONOSPACED, AStyle, 0));
end;

procedure TDScintilla.AutoCSetMulti(AMulti: Integer);
begin
  SendEditor(SCI_AUTOCSETMULTI, AMulti, 0);
end;

function TDScintilla.AutoCGetMulti: Integer;
begin
  Result := SendEditor(SCI_AUTOCGETMULTI, 0, 0);
end;

procedure TDScintilla.AutoCSetOptions(AOptions: Integer);
begin
  SendEditor(SCI_AUTOCSETOPTIONS, AOptions, 0);
end;

function TDScintilla.AutoCGetOptions: Integer;
begin
  Result := SendEditor(SCI_AUTOCGETOPTIONS, 0, 0);
end;

procedure TDScintilla.SetPhasesDraw(APhases: Integer);
begin
  SendEditor(SCI_SETPHASESDRAW, APhases, 0);
end;

function TDScintilla.GetPhasesDraw: Integer;
begin
  Result := SendEditor(SCI_GETPHASESDRAW, 0, 0);
end;

procedure TDScintilla.SetIMEInteraction(AIMEInteration: Integer);
begin
  SendEditor(SCI_SETIMEINTERACTION, AIMEInteration, 0);
end;

function TDScintilla.GetIMEInteraction: Integer;
begin
  Result := SendEditor(SCI_GETIMEINTERACTION, 0, 0);
end;

procedure TDScintilla.IndicSetHoverStyle(AIndicator, AIndicatorStyle: Integer);
begin
  SendEditor(SCI_INDICSETHOVERSTYLE, AIndicator, AIndicatorStyle);
end;

function TDScintilla.IndicGetHoverStyle(AIndicator: Integer): Integer;
begin
  Result := SendEditor(SCI_INDICGETHOVERSTYLE, AIndicator, 0);
end;

procedure TDScintilla.IndicSetHoverFore(AIndicator: Integer; AFore: TColor);
begin
  SendEditor(SCI_INDICSETHOVERFORE, AIndicator, Integer(AFore));
end;

function TDScintilla.IndicGetHoverFore(AIndicator: Integer): TColor;
begin
  Result := TColor(SendEditor(SCI_INDICGETHOVERFORE, AIndicator, 0));
end;

procedure TDScintilla.IndicSetFlags(AIndicator, AFlags: Integer);
begin
  SendEditor(SCI_INDICSETFLAGS, AIndicator, AFlags);
end;

function TDScintilla.IndicGetFlags(AIndicator: Integer): Integer;
begin
  Result := SendEditor(SCI_INDICGETFLAGS, AIndicator, 0);
end;

procedure TDScintilla.SetIdleStyling(AStyling: Integer);
begin
  SendEditor(SCI_SETIDLESTYLING, AStyling, 0);
end;

function TDScintilla.GetIdleStyling: Integer;
begin
  Result := SendEditor(SCI_GETIDLESTYLING, 0, 0);
end;

procedure TDScintilla.SetMouseWheelCaptures(ACaptures: Boolean);
begin
  SendEditor(SCI_SETMOUSEWHEELCAPTURES, Integer(ACaptures), 0);
end;

function TDScintilla.GetMouseWheelCaptures: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETMOUSEWHEELCAPTURES, 0, 0));
end;

procedure TDScintilla.SetTabDrawMode(ATabDrawMode: Integer);
begin
  SendEditor(SCI_SETTABDRAWMODE, ATabDrawMode, 0);
end;

function TDScintilla.GetTabDrawMode: Integer;
begin
  Result := SendEditor(SCI_GETTABDRAWMODE, 0, 0);
end;

procedure TDScintilla.FoldDisplayTextSetStyle(AStyle: Integer);
begin
  SendEditor(SCI_FOLDDISPLAYTEXTSETSTYLE, AStyle, 0);
end;

function TDScintilla.FoldDisplayTextGetStyle: Integer;
begin
  Result := SendEditor(SCI_FOLDDISPLAYTEXTGETSTYLE, 0, 0);
end;

procedure TDScintilla.SetAccessibility(AAccessibility: Integer);
begin
  SendEditor(SCI_SETACCESSIBILITY, AAccessibility, 0);
end;

function TDScintilla.GetAccessibility: Integer;
begin
  Result := SendEditor(SCI_GETACCESSIBILITY, 0, 0);
end;

procedure TDScintilla.SetCaretLineFrame(AWidth: Integer);
begin
  SendEditor(SCI_SETCARETLINEFRAME, AWidth, 0);
end;

function TDScintilla.GetCaretLineFrame: Integer;
begin
  Result := SendEditor(SCI_GETCARETLINEFRAME, 0, 0);
end;

procedure TDScintilla.SetBidirectional(ABidirectional: Integer);
begin
  SendEditor(SCI_SETBIDIRECTIONAL, ABidirectional, 0);
end;

function TDScintilla.GetBidirectional: Integer;
begin
  Result := SendEditor(SCI_GETBIDIRECTIONAL, 0, 0);
end;

procedure TDScintilla.SetCharacterCategoryOptimization(ACountCharacters: Integer);
begin
  SendEditor(SCI_SETCHARACTERCATEGORYOPTIMIZATION, ACountCharacters, 0);
end;

function TDScintilla.GetCharacterCategoryOptimization: Integer;
begin
  Result := SendEditor(SCI_GETCHARACTERCATEGORYOPTIMIZATION, 0, 0);
end;

procedure TDScintilla.SetDefaultFoldDisplayText(AText: UnicodeString);
begin
  FHelper.SetText(SCI_SETDEFAULTFOLDDISPLAYTEXT, 0, AText);
end;

function TDScintilla.GetDefaultFoldDisplayText: UnicodeString;
begin
  FHelper.GetText(SCI_GETDEFAULTFOLDDISPLAYTEXT, 0, Result);
end;

procedure TDScintilla.SetTabMinimumWidth(APixels: Integer);
begin
  SendEditor(SCI_SETTABMINIMUMWIDTH, APixels, 0);
end;

function TDScintilla.GetTabMinimumWidth: Integer;
begin
  Result := SendEditor(SCI_GETTABMINIMUMWIDTH, 0, 0);
end;

procedure TDScintilla.MarkerSetLayer(AMarkerNumber, ALayer: Integer);
begin
  SendEditor(SCI_MARKERSETLAYER, AMarkerNumber, ALayer);
end;

function TDScintilla.MarkerGetLayer(AMarkerNumber: Integer): Integer;
begin
  Result := SendEditor(SCI_MARKERGETLAYER, AMarkerNumber, 0);
end;

procedure TDScintilla.EOLAnnotationSetText(ALine: NativeInt; const AText: UnicodeString);
begin
  FHelper.SetText(SCI_EOLANNOTATIONSETTEXT, ALine, AText);
end;

function TDScintilla.EOLAnnotationGetText(ALine: NativeInt): UnicodeString;
begin
  FHelper.GetText(SCI_EOLANNOTATIONGETTEXT, ALine, Result);
end;

procedure TDScintilla.EOLAnnotationSetStyle(ALine: NativeInt; AStyle: Integer);
begin
  SendEditor(SCI_EOLANNOTATIONSETSTYLE, ALine, AStyle);
end;

function TDScintilla.EOLAnnotationGetStyle(ALine: NativeInt): Integer;
begin
  Result := SendEditor(SCI_EOLANNOTATIONGETSTYLE, ALine, 0);
end;

procedure TDScintilla.EOLAnnotationSetVisible(AVisible: Integer);
begin
  SendEditor(SCI_EOLANNOTATIONSETVISIBLE, AVisible, 0);
end;

function TDScintilla.EOLAnnotationGetVisible: Integer;
begin
  Result := SendEditor(SCI_EOLANNOTATIONGETVISIBLE, 0, 0);
end;

procedure TDScintilla.EOLAnnotationSetStyleOffset(AStyle: Integer);
begin
  SendEditor(SCI_EOLANNOTATIONSETSTYLEOFFSET, AStyle, 0);
end;

function TDScintilla.EOLAnnotationGetStyleOffset: Integer;
begin
  Result := SendEditor(SCI_EOLANNOTATIONGETSTYLEOFFSET, 0, 0);
end;

procedure TDScintilla.IndicSetStrokeWidth(AIndicator, AHundredths: Integer);
begin
  SendEditor(SCI_INDICSETSTROKEWIDTH, AIndicator, AHundredths);
end;

function TDScintilla.IndicGetStrokeWidth(AIndicator: Integer): Integer;
begin
  Result := SendEditor(SCI_INDICGETSTROKEWIDTH, AIndicator, 0);
end;

procedure TDScintilla.SetFontLocale(ALocaleName: UnicodeString);
begin
  FHelper.SetText(SCI_SETFONTLOCALE, 0, ALocaleName);
end;

function TDScintilla.GetFontLocale: UnicodeString;
begin
  FHelper.GetText(SCI_GETFONTLOCALE, 0, Result);
end;

procedure TDScintilla.SetCaretLineHighlightSubline(ASubline: Boolean);
begin
  SendEditor(SCI_SETCARETLINEHIGHLIGHTSUBLINE, Integer(ASubline), 0);
end;

function TDScintilla.GetCaretLineHighlightSubline: Boolean;
begin
  Result := Boolean(SendEditor(SCI_GETCARETLINEHIGHLIGHTSUBLINE, 0, 0));
end;

procedure TDScintilla.SetLayoutThreads(AThreads: Integer);
begin
  SendEditor(SCI_SETLAYOUTTHREADS, AThreads, 0);
end;

function TDScintilla.GetLayoutThreads: Integer;
begin
  Result := SendEditor(SCI_GETLAYOUTTHREADS, 0, 0);
end;
