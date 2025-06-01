//CE_Desc_Include(helpdescriptions.txt)
unit SciSearchReplace;
{
 Unit    : SciSearchReplace
 Purpose : Search and Replace for TScintilla based on Synedit Dialogs
 Created : 20/03/2003
 Author  : Kiriakos Vlahos (kvlahos@london.edu)
 History : 29/09/2004 Initial Release with Delphi Scintilla Interface Components
                      Changed Editor property from TScintilla to TScintillaBase class.
                      Wasn't any need for the extra properties to use this dialog.
                      hdalis (hdalis@users.sourceforge.net)

}

interface
Uses
  Types, Classes, Controls, Forms, SciLexer;

Type

  TSciSearchReplace = class(TComponent)
  private
    FSearchForSelWord : boolean;
    FEditor : TScintillaBase;
    FSearchFromCaretInt: boolean;
    FFoundText : String;
    FOnTextFound : TNotifyEvent;
    FOnTextNotFound : TNotifyEvent;
    FOnTextReplaced : TNotifyEvent;
  protected
      procedure Notification(AComponent: TComponent;
                Operation: TOperation);  override;
  public
    // Search Options
    SearchBackwards: boolean;
    SearchCaseSensitive: boolean;
    SearchFromCaret: boolean;
    SearchSelectionOnly: boolean;
    SearchWholeWords: boolean;
    SearchRegex: boolean;
    SearchText: string;
    SearchTextHistory: string;
    ReplaceText: string;
    ReplaceTextHistory: string;
    property FoundText : string read fFoundText;
    procedure DoSearchReplaceText(AReplace, ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
  published
    property SearchForSelWord : boolean read FSearchForSelWord write FSearchForSelWord;
    property Editor : TScintillaBase read FEditor write FEditor;
    property OnTextFound : TNotifyEvent read FOnTextFound write FOnTextFound;
    property OnTextNotFound : TNotifyEvent read FOnTextNotFound write FOnTextNotFound;
    property OnTextReplaced : TNotifyEvent read FOnTextReplaced write FOnTextReplaced;
  end;

implementation

Uses
 SciSearchTextDlg, SciConfirmReplaceDlg, SciReplaceTextDlg, SciSupport;

{ TSciSearchReplace }

procedure TSciSearchReplace.DoSearchReplaceText(AReplace, ABackwards: boolean);
var
  Options: Integer;
  StartPosition, EndPosition : integer;
  TargetStart, TargetEnd, posFind : integer;
  APos: TPoint;
  EditRect: TRect;
  DlgRes : integer;
  LenFound, LenReplaced : integer;
begin
  if not Assigned(FEditor) then Exit;

  Options := 0;
  if SearchCaseSensitive then
    Options := Options or SCFIND_MATCHCASE;
  if SearchWholeWords then
    Options := Options or SCFIND_WHOLEWORD;
  if SearchRegex then
    Options := Options or SCFIND_REGEXP;

  if ABackwards then
  begin
    if fSearchFromCaretInt and not SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionStart - 1
    else if SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionEnd
    else
      StartPosition := FEditor.GetLength;
    if SearchSelectionOnly then
      EndPosition := FEditor.GetSelectionStart
    else
      EndPosition := 0;
  end else
  begin
    if fSearchFromCaretInt and not SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionEnd + 1
    else if SearchSelectionOnly then
      StartPosition := FEditor.GetSelectionStart
    else
      StartPosition := 0;
    if SearchSelectionOnly then
      EndPosition := FEditor.GetSelectionEnd
    else
      EndPosition := FEditor.GetLength;
  end;

  with FEditor do begin
    SetTargetStart(StartPosition);
    SetTargetEnd(EndPosition);
    SetSearchFlags(Options);
    posFind := SearchInTarget(Length(SearchText), PChar(SearchText));
    if (posFind < 0) then begin
      if Assigned(FOnTextNotFound) then
        FOnTextNotFound(Self);
    end else
    begin
      TargetStart := GetTargetStart;
      TargetEnd := GetTargetEnd;
      LenFound := TargetEnd - TargetStart;
      LenReplaced := LenFound;
      EnsureRangeVisible(TargetStart, TargetEnd);
      SetSel(TargetStart, TargetEnd);
			FFoundText := FEditor.SelText;
      if Assigned(FOnTextFound) then
        FOnTextFound(Self);

      // Replace code
      if AReplace then
      begin
        DlgRes := mrYes;

        if ConfirmReplaceDialog = nil then
          ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);

        while (posFind >= 0) and (DlgRes <> mrCancel) do
        begin
          if not (DlgRes = mrYesToAll) then
          begin
            APos := Point(PointXFromPosition(TargetStart), PointYFromPosition(TargetStart));
            APos := ClientToScreen(APos);
            EditRect := FEditor.ClientRect;
            EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
            EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

            ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
              APos.Y + 2 * FEditor.TextHeight(LineFromPosition(TargetStart)), SearchText);
            DlgRes :=ConfirmReplaceDialog.ShowModal;
          end;

          if DlgRes = mrYesToAll then FEditor.BeginUndoAction;

          if DlgRes in [mrYes, mrYesToAll] then
          begin
            // Replace
            if SearchRegex then
              LenReplaced := ReplaceTargetRE(Length(ReplaceText), PChar(ReplaceText))
            else
              LenReplaced := ReplaceTarget(Length(ReplaceText), PChar(ReplaceText));
            TargetEnd := TargetStart + LenReplaced -1;
            if Assigned(FOnTextReplaced) then FOnTextReplaced(Self);
          end;
          if DlgRes in [mrYes, mrNo, mrYesToAll] then
          begin
            // carry on
            if ABackwards then
            begin
              SetTargetStart(TargetStart - 1);
              SetTargetEnd(EndPosition);
            end else
            begin
              SetTargetStart(TargetEnd + 1);
              EndPosition := EndPosition + LenReplaced - LenFound;
              SetTargetEnd(EndPosition);
            end;
            SetTargetEnd(EndPosition);
            SetSearchFlags(Options);
            posFind := SearchInTarget(Length(SearchText), PChar(SearchText));
            if posFind >= 0 then
            begin
              TargetStart := GetTargetStart;
              TargetEnd := GetTargetEnd;
              LenFound := TargetEnd - TargetStart;
              LenReplaced := LenFound;
              EnsureRangeVisible(TargetStart, TargetEnd);
              SetSel(TargetStart, TargetEnd);
            end;
          end else
            break;
        end;   // While

        if DlgRes = mrYesToAll then FEditor.EndUndoAction;

        // Restore original selection if Searching in Selection
        if SearchSelectionOnly then
        begin
          if ABackwards then
            SetSel(EndPosition, StartPosition)
          else
            SetSel(StartPosition, EndPosition);
          EnsureRangeVisible(GetSelectionStart, GetSelectionEnd);
        end;
      end;  // if AReplace
    end;  //if (posFind < 0)
  end; // with FEditor

  if ConfirmReplaceDialog <> nil then begin
    ConfirmReplaceDialog.Free;
    ConfirmReplaceDialog := nil;
  end;
end;

procedure TSciSearchReplace.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditor) and (Operation = opRemove) then FEditor := nil;
end;

procedure TSciSearchReplace.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TTextSearchDialog;
  SelectedText : string;
begin
  if not Assigned(FEditor) then Exit;

  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := Self.SearchBackwards;
    SearchCaseSensitive := Self.SearchCaseSensitive;
    SearchFromCursor := Self.SearchFromCaret;
    SearchInSelectionOnly := Self.SearchSelectionOnly;
		SelectedText := FEditor.SelText;

    if (SelectedText <> '') and
       (Pos(#10, SelectedText) > 0) or (Pos(#13, SelectedText) > 0) then
      SearchInSelectionOnly := True
    else
      SearchInSelectionOnly := False;

    // start with last search text
    SearchText := Self.SearchText;
    if FSearchForSelWord and not SearchInSelectionOnly and
       (SelectedText <> '') and (Pos(' ', SelectedText) <= 0) then
      SearchText := SelectedText;

    SearchTextHistory := Self.SearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := Self.ReplaceText;
      ReplaceTextHistory := Self.ReplaceTextHistory;
    end;
    SearchWholeWords := Self.SearchWholeWords;
    if ShowModal = mrOK then begin
      Self.SearchBackwards := SearchBackwards;
      Self.SearchCaseSensitive := SearchCaseSensitive;
      Self.SearchFromCaret := SearchFromCursor;
      Self.SearchSelectionOnly := SearchInSelectionOnly;
      Self.SearchWholeWords := SearchWholeWords;
      Self.SearchRegex := SearchRegularExpression;
      Self.SearchText := SearchText;
      Self.SearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do begin
        Self.ReplaceText := ReplaceText;
        Self.ReplaceTextHistory := ReplaceTextHistory;
      end;
      fSearchFromCaretInt := Self.SearchFromCaret;
      if SearchText <> '' then begin
        DoSearchReplaceText(AReplace, Self.SearchBackwards);
        fSearchFromCaretInt := True;
      end;
      Self.SearchSelectionOnly := False;
    end;
  finally
    dlg.Free;
  end;
end;

initialization
  ConfirmReplaceDialog := nil;
end.
