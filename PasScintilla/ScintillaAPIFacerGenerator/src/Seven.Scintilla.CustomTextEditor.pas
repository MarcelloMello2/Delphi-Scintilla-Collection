unit Seven.Scintilla.CustomTextEditor;

{
  Wrapper Pascal para Scintilla
  Gerado automaticamente por ScintillaFacerGenerator.exe
  Data: 05/06/2025 01:49:50
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Seven.Scintilla.Types,
  Seven.Scintilla.BaseTextEditor;

type
  TCustomSciTextEditor = class(TBaseSciTextEditor)
  strict private
  strict protected

    // Basics
    /// <summary>
    /// Add text to the document at current position.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure AddText(ALength: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// Add array of cells to document.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="c">
    /// The c parameter
    /// </param>
    procedure AddStyledText(ALength: TSciPosition; c: PAnsiChar);
    /// <summary>
    /// Insert string at a position.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure InsertText(APos: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// Change the text that is being inserted in response to SC_MOD_INSERTCHECK
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure ChangeInsertion(ALength: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// Delete all text in the document.
    /// </summary>
    procedure ClearAll();
    /// <summary>
    /// Delete a range of text in the document.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="ALengthDelete">
    /// Position in the document
    /// </param>
    procedure DeleteRange(AStart: TSciPosition; ALengthDelete: TSciPosition);
    /// <summary>
    /// Set all style bytes to 0, remove all folding information.
    /// </summary>
    procedure ClearDocumentStyle();
    /// <summary>
    /// Returns the number of bytes in the document.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLength(): TSciPosition;
    /// <summary>
    /// Returns the character byte at the position.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCharAt(APos: TSciPosition): Integer;
    /// <summary>
    /// Returns the position of the caret.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetCurrentPos(): TSciPosition;
    /// <summary>
    /// Returns the position of the opposite end of the selection to the caret.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetAnchor(): TSciPosition;
    /// <summary>
    /// Returns the style byte at the position.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetStyleAt(APos: TSciPosition): Integer;
    /// <summary>
    /// Returns the unsigned style byte at the position.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetStyleIndexAt(APos: TSciPosition): Integer;
    /// <summary>
    /// Redoes the next action on the undo history.
    /// </summary>
    procedure Redo();
    /// <summary>
    /// Choose between collecting actions into the undo
    /// history and discarding them.
    /// </summary>
    /// <param name="ACollectUndo">
    /// Boolean value
    /// </param>
    procedure SetUndoCollection(ACollectUndo: Boolean);
    /// <summary>
    /// Select all the text in the document.
    /// </summary>
    procedure SelectAll();
    /// <summary>
    /// Remember the current position in the undo history as the position
    /// at which the document was saved.
    /// </summary>
    procedure SetSavePoint();
    /// <summary>
    /// Retrieve a buffer of cells.
    /// Returns the number of bytes in the buffer not including terminating NULs.
    /// </summary>
    /// <param name="ATr">
    /// The ATr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetStyledText(ATr: PSciTextRange): TSciPosition;
    /// <summary>
    /// Retrieve a buffer of cells that can be past 2GB.
    /// Returns the number of bytes in the buffer not including terminating NULs.
    /// </summary>
    /// <param name="ATr">
    /// The ATr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetStyledTextFull(ATr: PSciTextRangeFull): TSciPosition;
    /// <summary>
    /// Are there any redoable actions in the undo history?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function CanRedo(): Boolean;
    /// <summary>
    /// Retrieve the line number at which a particular marker is located.
    /// </summary>
    /// <param name="AMarkerHandle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function MarkerLineFromHandle(AMarkerHandle: Integer): TSciLine;
    /// <summary>
    /// Delete a marker.
    /// </summary>
    /// <param name="AMarkerHandle">
    /// Integer value
    /// </param>
    procedure MarkerDeleteHandle(AMarkerHandle: Integer);
    /// <summary>
    /// Retrieve marker handles of a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AWhich">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerHandleFromLine(ALine: TSciLine; AWhich: Integer): Integer;
    /// <summary>
    /// Retrieve marker number of a marker handle
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AWhich">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerNumberFromLine(ALine: TSciLine; AWhich: Integer): Integer;
    /// <summary>
    /// Is undo history being collected?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetUndoCollection(): Boolean;
    /// <summary>
    /// Are white space characters currently visible?
    /// Returns one of SCWS_* constants.
    /// </summary>
    /// <returns>
    /// Returns the viewws
    /// </returns>
    function GetViewWS(): NativeInt;
    /// <summary>
    /// Make white space characters invisible, always visible or visible outside indentation.
    /// </summary>
    /// <param name="AViewWS">
    /// The AViewWS parameter
    /// </param>
    procedure SetViewWS(AViewWS: NativeInt);
    /// <summary>
    /// Retrieve the current tab draw mode.
    /// Returns one of SCTD_* constants.
    /// </summary>
    /// <returns>
    /// Returns the tabdrawmode
    /// </returns>
    function GetTabDrawMode(): NativeInt;
    /// <summary>
    /// Set how tabs are drawn when visible.
    /// </summary>
    /// <param name="ATabDrawMode">
    /// The ATabDrawMode parameter
    /// </param>
    procedure SetTabDrawMode(ATabDrawMode: NativeInt);
    /// <summary>
    /// Find the position from a point within the window.
    /// </summary>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <param name="y">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionFromPoint(x: Integer; y: Integer): TSciPosition;
    /// <summary>
    /// Find the position from a point within the window but return
    /// INVALID_POSITION if not close to text.
    /// </summary>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <param name="y">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionFromPointClose(x: Integer; y: Integer): TSciPosition;
    /// <summary>
    /// Set caret to start of a line and ensure it is visible.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    procedure GotoLine(ALine: TSciLine);
    /// <summary>
    /// Set caret to a position and ensure it is visible.
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure GotoPos(ACaret: TSciPosition);
    /// <summary>
    /// Set the selection anchor to a position. The anchor is the opposite
    /// end of the selection from the caret.
    /// </summary>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure SetAnchor(AAnchor: TSciPosition);
    /// <summary>
    /// Retrieve the text of the line containing the caret.
    /// Returns the index of the caret on the line.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetCurLine(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Retrieve the position of the last correctly styled character.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetEndStyled(): TSciPosition;
    /// <summary>
    /// Convert all line endings in the document to one mode.
    /// </summary>
    /// <param name="AEolMode">
    /// The AEolMode parameter
    /// </param>
    procedure ConvertEOLs(AEolMode: NativeInt);
    /// <summary>
    /// Retrieve the current end of line mode - one of CRLF, CR, or LF.
    /// </summary>
    /// <returns>
    /// Returns the eolmode
    /// </returns>
    function GetEOLMode(): NativeInt;
    /// <summary>
    /// Set the current end of line mode.
    /// </summary>
    /// <param name="AEolMode">
    /// The AEolMode parameter
    /// </param>
    procedure SetEOLMode(AEolMode: NativeInt);
    /// <summary>
    /// Set the current styling position to start.
    /// The unused parameter is no longer used and should be set to 0.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AUnused">
    /// Integer value
    /// </param>
    procedure StartStyling(AStart: TSciPosition; AUnused: Integer);
    /// <summary>
    /// Change style from current styling position for length characters to a style
    /// and move the current styling position to after this newly styled segment.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure SetStyling(ALength: TSciPosition; AStyle: Integer);
    /// <summary>
    /// Is drawing done first into a buffer or direct to the screen?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetBufferedDraw(): Boolean;
    /// <summary>
    /// If drawing is buffered then each line of text is drawn into a bitmap buffer
    /// before drawing it to the screen to avoid flicker.
    /// </summary>
    /// <param name="ABuffered">
    /// Boolean value
    /// </param>
    procedure SetBufferedDraw(ABuffered: Boolean);
    /// <summary>
    /// Change the visible size of a tab to be a multiple of the width of a space character.
    /// </summary>
    /// <param name="ATabWidth">
    /// Integer value
    /// </param>
    procedure SetTabWidth(ATabWidth: Integer);
    /// <summary>
    /// Retrieve the visible size of a tab.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetTabWidth(): Integer;
    /// <summary>
    /// Set the minimum visual width of a tab.
    /// </summary>
    /// <param name="APixels">
    /// Integer value
    /// </param>
    procedure SetTabMinimumWidth(APixels: Integer);
    /// <summary>
    /// Get the minimum visual width of a tab.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetTabMinimumWidth(): Integer;
    /// <summary>
    /// Clear explicit tabstops on a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    procedure ClearTabStops(ALine: TSciLine);
    /// <summary>
    /// Add an explicit tab stop for a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="x">
    /// Integer value
    /// </param>
    procedure AddTabStop(ALine: TSciLine; x: Integer);
    /// <summary>
    /// Find the next explicit tab stop position on a line after a position.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetNextTabStop(ALine: TSciLine; x: Integer): Integer;
    /// <summary>
    /// Set the code page used to interpret the bytes of the document as characters.
    /// The SC_CP_UTF8 value can be used to enter Unicode mode.
    /// </summary>
    /// <param name="ACodePage">
    /// Integer value
    /// </param>
    procedure SetCodePage(ACodePage: Integer);
    /// <summary>
    /// Set the locale for displaying text.
    /// </summary>
    /// <param name="ALocaleName">
    /// Text string
    /// </param>
    procedure SetFontLocale(ALocaleName: PAnsiChar);
    /// <summary>
    /// Get the locale for displaying text.
    /// </summary>
    /// <param name="ALocaleName">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetFontLocale(ALocaleName: PAnsiChar): Integer;
    /// <summary>
    /// Is the IME displayed in a window or inline?
    /// </summary>
    /// <returns>
    /// Returns the imeinteraction
    /// </returns>
    function GetIMEInteraction(): NativeInt;
    /// <summary>
    /// Choose to display the IME in a window or inline.
    /// </summary>
    /// <param name="AImeInteraction">
    /// The AImeInteraction parameter
    /// </param>
    procedure SetIMEInteraction(AImeInteraction: NativeInt);
    /// <summary>
    /// Set the symbol used for a particular marker number.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="AMarkerSymbol">
    /// The AMarkerSymbol parameter
    /// </param>
    procedure MarkerDefine(AMarkerNumber: Integer; AMarkerSymbol: NativeInt);
    /// <summary>
    /// Set the foreground colour used for a particular marker number.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure MarkerSetFore(AMarkerNumber: Integer; AFore: TColor);
    /// <summary>
    /// Set the background colour used for a particular marker number.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure MarkerSetBack(AMarkerNumber: Integer; ABack: TColor);
    /// <summary>
    /// Set the background colour used for a particular marker number when its folding block is selected.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure MarkerSetBackSelected(AMarkerNumber: Integer; ABack: TColor);
    /// <summary>
    /// Set the foreground colour used for a particular marker number.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="AFore">
    /// The AFore parameter
    /// </param>
    procedure MarkerSetForeTranslucent(AMarkerNumber: Integer; AFore: TColorAlpha);
    /// <summary>
    /// Set the background colour used for a particular marker number.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="ABack">
    /// The ABack parameter
    /// </param>
    procedure MarkerSetBackTranslucent(AMarkerNumber: Integer; ABack: TColorAlpha);
    /// <summary>
    /// Set the background colour used for a particular marker number when its folding block is selected.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="ABack">
    /// The ABack parameter
    /// </param>
    procedure MarkerSetBackSelectedTranslucent(AMarkerNumber: Integer; ABack: TColorAlpha);
    /// <summary>
    /// Set the width of strokes used in .01 pixels so 50  = 1/2 pixel width.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="AHundredths">
    /// Integer value
    /// </param>
    procedure MarkerSetStrokeWidth(AMarkerNumber: Integer; AHundredths: Integer);
    /// <summary>
    /// Enable/disable highlight for current folding block (smallest one that contains the caret)
    /// </summary>
    /// <param name="AEnabled">
    /// Boolean value
    /// </param>
    procedure MarkerEnableHighlight(AEnabled: Boolean);
    /// <summary>
    /// Add a marker to a line, returning an ID which can be used to find or delete the marker.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerAdd(ALine: TSciLine; AMarkerNumber: Integer): Integer;
    /// <summary>
    /// Delete a marker from a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    procedure MarkerDelete(ALine: TSciLine; AMarkerNumber: Integer);
    /// <summary>
    /// Delete all markers with a particular number from all lines.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    procedure MarkerDeleteAll(AMarkerNumber: Integer);
    /// <summary>
    /// Get a bit mask of all the markers set on a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerGet(ALine: TSciLine): Integer;
    /// <summary>
    /// Find the next line at or after lineStart that includes a marker in mask.
    /// Return -1 when no more lines.
    /// </summary>
    /// <param name="ALineStart">
    /// Line number
    /// </param>
    /// <param name="AMarkerMask">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function MarkerNext(ALineStart: TSciLine; AMarkerMask: Integer): TSciLine;
    /// <summary>
    /// Find the previous line before lineStart that includes a marker in mask.
    /// </summary>
    /// <param name="ALineStart">
    /// Line number
    /// </param>
    /// <param name="AMarkerMask">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function MarkerPrevious(ALineStart: TSciLine; AMarkerMask: Integer): TSciLine;
    /// <summary>
    /// Define a marker from a pixmap.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="APixmap">
    /// Text string
    /// </param>
    procedure MarkerDefinePixmap(AMarkerNumber: Integer; APixmap: PAnsiChar);
    /// <summary>
    /// Add a set of markers to a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AMarkerSet">
    /// Integer value
    /// </param>
    procedure MarkerAddSet(ALine: TSciLine; AMarkerSet: Integer);
    /// <summary>
    /// Set the alpha used for a marker that is drawn in the text area, not the margin.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="AAlpha">
    /// The AAlpha parameter
    /// </param>
    procedure MarkerSetAlpha(AMarkerNumber: Integer; AAlpha: NativeInt);
    /// <summary>
    /// Get the layer used for a marker that is drawn in the text area, not the margin.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function MarkerGetLayer(AMarkerNumber: Integer): NativeInt;
    /// <summary>
    /// Set the layer used for a marker that is drawn in the text area, not the margin.
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="ALayer">
    /// The ALayer parameter
    /// </param>
    procedure MarkerSetLayer(AMarkerNumber: Integer; ALayer: NativeInt);
    /// <summary>
    /// Set a margin to be either numeric or symbolic.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <param name="AMarginType">
    /// The AMarginType parameter
    /// </param>
    procedure SetMarginTypeN(AMargin: Integer; AMarginType: NativeInt);
    /// <summary>
    /// Retrieve the type of a margin.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the margintypen
    /// </returns>
    function GetMarginTypeN(AMargin: Integer): NativeInt;
    /// <summary>
    /// Set the width of a margin to a width expressed in pixels.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <param name="APixelWidth">
    /// Integer value
    /// </param>
    procedure SetMarginWidthN(AMargin: Integer; APixelWidth: Integer);
    /// <summary>
    /// Retrieve the width of a margin in pixels.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMarginWidthN(AMargin: Integer): Integer;
    /// <summary>
    /// Set a mask that determines which markers are displayed in a margin.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <param name="AMask">
    /// Integer value
    /// </param>
    procedure SetMarginMaskN(AMargin: Integer; AMask: Integer);
    /// <summary>
    /// Retrieve the marker mask of a margin.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMarginMaskN(AMargin: Integer): Integer;
    /// <summary>
    /// Make a margin sensitive or insensitive to mouse clicks.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <param name="ASensitive">
    /// Boolean value
    /// </param>
    procedure SetMarginSensitiveN(AMargin: Integer; ASensitive: Boolean);
    /// <summary>
    /// Retrieve the mouse click sensitivity of a margin.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMarginSensitiveN(AMargin: Integer): Boolean;
    /// <summary>
    /// Set the cursor shown when the mouse is inside a margin.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <param name="ACursor">
    /// The ACursor parameter
    /// </param>
    procedure SetMarginCursorN(AMargin: Integer; ACursor: NativeInt);
    /// <summary>
    /// Retrieve the cursor shown in a margin.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the margincursorn
    /// </returns>
    function GetMarginCursorN(AMargin: Integer): NativeInt;
    /// <summary>
    /// Set the background colour of a margin. Only visible for SC_MARGIN_COLOUR.
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetMarginBackN(AMargin: Integer; ABack: TColor);
    /// <summary>
    /// Retrieve the background colour of a margin
    /// </summary>
    /// <param name="AMargin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetMarginBackN(AMargin: Integer): TColor;
    /// <summary>
    /// Allocate a non-standard number of margins.
    /// </summary>
    /// <param name="AMargins">
    /// Integer value
    /// </param>
    procedure SetMargins(AMargins: Integer);
    /// <summary>
    /// How many margins are there?.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMargins(): Integer;
    /// <summary>
    /// Clear all the styles and make equivalent to the global default style.
    /// </summary>
    procedure StyleClearAll();
    /// <summary>
    /// Set the foreground colour of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure StyleSetFore(AStyle: Integer; AFore: TColor);
    /// <summary>
    /// Set the background colour of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure StyleSetBack(AStyle: Integer; ABack: TColor);
    /// <summary>
    /// Set a style to be bold or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ABold">
    /// Boolean value
    /// </param>
    procedure StyleSetBold(AStyle: Integer; ABold: Boolean);
    /// <summary>
    /// Set a style to be italic or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AItalic">
    /// Boolean value
    /// </param>
    procedure StyleSetItalic(AStyle: Integer; AItalic: Boolean);
    /// <summary>
    /// Set the size of characters of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ASizePoints">
    /// Integer value
    /// </param>
    procedure StyleSetSize(AStyle: Integer; ASizePoints: Integer);
    /// <summary>
    /// Set the font of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AFontName">
    /// Text string
    /// </param>
    procedure StyleSetFont(AStyle: Integer; AFontName: PAnsiChar);
    /// <summary>
    /// Set a style to have its end of line filled or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AEolFilled">
    /// Boolean value
    /// </param>
    procedure StyleSetEOLFilled(AStyle: Integer; AEolFilled: Boolean);
    /// <summary>
    /// Reset the default style to its state at startup
    /// </summary>
    procedure StyleResetDefault();
    /// <summary>
    /// Set a style to be underlined or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AUnderline">
    /// Boolean value
    /// </param>
    procedure StyleSetUnderline(AStyle: Integer; AUnderline: Boolean);
    /// <summary>
    /// Get the foreground colour of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function StyleGetFore(AStyle: Integer): TColor;
    /// <summary>
    /// Get the background colour of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function StyleGetBack(AStyle: Integer): TColor;
    /// <summary>
    /// Get is a style bold or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetBold(AStyle: Integer): Boolean;
    /// <summary>
    /// Get is a style italic or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetItalic(AStyle: Integer): Boolean;
    /// <summary>
    /// Get the size of characters of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetSize(AStyle: Integer): Integer;
    /// <summary>
    /// Get the font of a style.
    /// Returns the length of the fontName
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AFontName">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetFont(AStyle: Integer; AFontName: PAnsiChar): Integer;
    /// <summary>
    /// Get is a style to have its end of line filled or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetEOLFilled(AStyle: Integer): Boolean;
    /// <summary>
    /// Get is a style underlined or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetUnderline(AStyle: Integer): Boolean;
    /// <summary>
    /// Get is a style mixed case, or to force upper or lower case.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetCase(AStyle: Integer): NativeInt;
    /// <summary>
    /// Get the character get of the font in a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetCharacterSet(AStyle: Integer): NativeInt;
    /// <summary>
    /// Get is a style visible or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetVisible(AStyle: Integer): Boolean;
    /// <summary>
    /// Get is a style changeable or not (read only).
    /// Experimental feature, currently buggy.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetChangeable(AStyle: Integer): Boolean;
    /// <summary>
    /// Get is a style a hotspot or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetHotSpot(AStyle: Integer): Boolean;
    /// <summary>
    /// Set a style to be mixed case, or to force upper or lower case.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ACaseVisible">
    /// The ACaseVisible parameter
    /// </param>
    procedure StyleSetCase(AStyle: Integer; ACaseVisible: NativeInt);
    /// <summary>
    /// Set the size of characters of a style. Size is in points multiplied by 100.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ASizeHundredthPoints">
    /// Integer value
    /// </param>
    procedure StyleSetSizeFractional(AStyle: Integer; ASizeHundredthPoints: Integer);
    /// <summary>
    /// Get the size of characters of a style in points multiplied by 100
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetSizeFractional(AStyle: Integer): Integer;
    /// <summary>
    /// Set the weight of characters of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AWeight">
    /// The AWeight parameter
    /// </param>
    procedure StyleSetWeight(AStyle: Integer; AWeight: NativeInt);
    /// <summary>
    /// Get the weight of characters of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetWeight(AStyle: Integer): NativeInt;
    /// <summary>
    /// Set the character set of the font in a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ACharacterSet">
    /// The ACharacterSet parameter
    /// </param>
    procedure StyleSetCharacterSet(AStyle: Integer; ACharacterSet: NativeInt);
    /// <summary>
    /// Set a style to be a hotspot or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AHotspot">
    /// Boolean value
    /// </param>
    procedure StyleSetHotSpot(AStyle: Integer; AHotspot: Boolean);
    /// <summary>
    /// Indicate that a style may be monospaced over ASCII graphics characters which enables optimizations.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ACheckMonospaced">
    /// Boolean value
    /// </param>
    procedure StyleSetCheckMonospaced(AStyle: Integer; ACheckMonospaced: Boolean);
    /// <summary>
    /// Get whether a style may be monospaced.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetCheckMonospaced(AStyle: Integer): Boolean;
    /// <summary>
    /// Set the stretch of characters of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AStretch">
    /// The AStretch parameter
    /// </param>
    procedure StyleSetStretch(AStyle: Integer; AStretch: NativeInt);
    /// <summary>
    /// Get the stretch of characters of a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetStretch(AStyle: Integer): NativeInt;
    /// <summary>
    /// Set the invisible representation for a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ARepresentation">
    /// Text string
    /// </param>
    procedure StyleSetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar);
    /// <summary>
    /// Get the invisible representation for a style.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ARepresentation">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar): Integer;
    /// <summary>
    /// Set the colour of an element. Translucency (alpha) may or may not be significant
    /// and this may depend on the platform. The alpha byte should commonly be 0xff for opaque.
    /// </summary>
    /// <param name="AElement">
    /// The AElement parameter
    /// </param>
    /// <param name="AColourElement">
    /// The AColourElement parameter
    /// </param>
    procedure SetElementColour(AElement: NativeInt; AColourElement: TColorAlpha);
    /// <summary>
    /// Get the colour of an element.
    /// </summary>
    /// <param name="AElement">
    /// The AElement parameter
    /// </param>
    /// <returns>
    /// Returns the elementcolour
    /// </returns>
    function GetElementColour(AElement: NativeInt): TColorAlpha;
    /// <summary>
    /// Use the default or platform-defined colour for an element.
    /// </summary>
    /// <param name="AElement">
    /// The AElement parameter
    /// </param>
    procedure ResetElementColour(AElement: NativeInt);
    /// <summary>
    /// Get whether an element has been set by SetElementColour.
    /// When false, a platform-defined or default colour is used.
    /// </summary>
    /// <param name="AElement">
    /// The AElement parameter
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetElementIsSet(AElement: NativeInt): Boolean;
    /// <summary>
    /// Get whether an element supports translucency.
    /// </summary>
    /// <param name="AElement">
    /// The AElement parameter
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetElementAllowsTranslucent(AElement: NativeInt): Boolean;
    /// <summary>
    /// Get the colour of an element.
    /// </summary>
    /// <param name="AElement">
    /// The AElement parameter
    /// </param>
    /// <returns>
    /// Returns the elementbasecolour
    /// </returns>
    function GetElementBaseColour(AElement: NativeInt): TColorAlpha;
    /// <summary>
    /// Set the foreground colour of the main and additional selections and whether to use this setting.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetSelFore(AUseSetting: Boolean; AFore: TColor);
    /// <summary>
    /// Set the background colour of the main and additional selections and whether to use this setting.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetSelBack(AUseSetting: Boolean; ABack: TColor);
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    /// <returns>
    /// Returns the selalpha
    /// </returns>
    function GetSelAlpha(): NativeInt;
    /// <summary>
    /// Set the alpha of the selection.
    /// </summary>
    /// <param name="AAlpha">
    /// The AAlpha parameter
    /// </param>
    procedure SetSelAlpha(AAlpha: NativeInt);
    /// <summary>
    /// Is the selection end of line filled?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetSelEOLFilled(): Boolean;
    /// <summary>
    /// Set the selection to have its end of line filled or not.
    /// </summary>
    /// <param name="AFilled">
    /// Boolean value
    /// </param>
    procedure SetSelEOLFilled(AFilled: Boolean);
    /// <summary>
    /// Get the layer for drawing selections
    /// </summary>
    /// <returns>
    /// Returns the selectionlayer
    /// </returns>
    function GetSelectionLayer(): NativeInt;
    /// <summary>
    /// Set the layer for drawing selections: either opaquely on base layer or translucently over text
    /// </summary>
    /// <param name="ALayer">
    /// The ALayer parameter
    /// </param>
    procedure SetSelectionLayer(ALayer: NativeInt);
    /// <summary>
    /// Get the layer of the background of the line containing the caret.
    /// </summary>
    /// <returns>
    /// Returns the caretlinelayer
    /// </returns>
    function GetCaretLineLayer(): NativeInt;
    /// <summary>
    /// Set the layer of the background of the line containing the caret.
    /// </summary>
    /// <param name="ALayer">
    /// The ALayer parameter
    /// </param>
    procedure SetCaretLineLayer(ALayer: NativeInt);
    /// <summary>
    /// Get only highlighting subline instead of whole line.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetCaretLineHighlightSubLine(): Boolean;
    /// <summary>
    /// Set only highlighting subline instead of whole line.
    /// </summary>
    /// <param name="ASubLine">
    /// Boolean value
    /// </param>
    procedure SetCaretLineHighlightSubLine(ASubLine: Boolean);
    /// <summary>
    /// Set the foreground colour of the caret.
    /// </summary>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetCaretFore(AFore: TColor);
    /// <summary>
    /// When key+modifier combination keyDefinition is pressed perform sciCommand.
    /// </summary>
    /// <param name="AKeyDefinition">
    /// The AKeyDefinition parameter
    /// </param>
    /// <param name="ASciCommand">
    /// Integer value
    /// </param>
    procedure AssignCmdKey(AKeyDefinition: TSciKeyModifies; ASciCommand: Integer);
    /// <summary>
    /// When key+modifier combination keyDefinition is pressed do nothing.
    /// </summary>
    /// <param name="AKeyDefinition">
    /// The AKeyDefinition parameter
    /// </param>
    procedure ClearCmdKey(AKeyDefinition: TSciKeyModifies);
    /// <summary>
    /// Drop all key mappings.
    /// </summary>
    procedure ClearAllCmdKeys();
    /// <summary>
    /// Set the styles for a segment of the document.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AStyles">
    /// Text string
    /// </param>
    procedure SetStylingEx(ALength: TSciPosition; AStyles: PAnsiChar);
    /// <summary>
    /// Set a style to be visible or not.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AVisible">
    /// Boolean value
    /// </param>
    procedure StyleSetVisible(AStyle: Integer; AVisible: Boolean);
    /// <summary>
    /// Get the time in milliseconds that the caret is on and off.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCaretPeriod(): Integer;
    /// <summary>
    /// Get the time in milliseconds that the caret is on and off. 0 = steady on.
    /// </summary>
    /// <param name="APeriodMilliseconds">
    /// Integer value
    /// </param>
    procedure SetCaretPeriod(APeriodMilliseconds: Integer);
    /// <summary>
    /// Set the set of characters making up words for when moving or selecting by word.
    /// First sets defaults like SetCharsDefault.
    /// </summary>
    /// <param name="ACharacters">
    /// Text string
    /// </param>
    procedure SetWordChars(ACharacters: PAnsiChar);
    /// <summary>
    /// Get the set of characters making up words for when moving or selecting by word.
    /// Returns the number of characters
    /// </summary>
    /// <param name="ACharacters">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetWordChars(ACharacters: PAnsiChar): Integer;
    /// <summary>
    /// Set the number of characters to have directly indexed categories
    /// </summary>
    /// <param name="ACountCharacters">
    /// Integer value
    /// </param>
    procedure SetCharacterCategoryOptimization(ACountCharacters: Integer);
    /// <summary>
    /// Get the number of characters to have directly indexed categories
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCharacterCategoryOptimization(): Integer;
    /// <summary>
    /// Start a sequence of actions that is undone and redone as a unit.
    /// May be nested.
    /// </summary>
    procedure BeginUndoAction();
    /// <summary>
    /// End a sequence of actions that is undone and redone as a unit.
    /// </summary>
    procedure EndUndoAction();
    /// <summary>
    /// Is an undo sequence active?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoSequence(): Integer;
    /// <summary>
    /// How many undo actions are in the history?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoActions(): Integer;
    /// <summary>
    /// Set action as the save point
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    procedure SetUndoSavePoint(AAction: Integer);
    /// <summary>
    /// Which action is the save point?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoSavePoint(): Integer;
    /// <summary>
    /// Set action as the detach point
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    procedure SetUndoDetach(AAction: Integer);
    /// <summary>
    /// Which action is the detach point?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoDetach(): Integer;
    /// <summary>
    /// Set action as the tentative point
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    procedure SetUndoTentative(AAction: Integer);
    /// <summary>
    /// Which action is the tentative point?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoTentative(): Integer;
    /// <summary>
    /// Set action as the current point
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    procedure SetUndoCurrent(AAction: Integer);
    /// <summary>
    /// Which action is the current point?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoCurrent(): Integer;
    /// <summary>
    /// Push one action onto undo history with no text
    /// </summary>
    /// <param name="AType">
    /// Integer value
    /// </param>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    procedure PushUndoActionType(AType: Integer; APos: TSciPosition);
    /// <summary>
    /// Set the text and length of the most recently pushed action
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure ChangeLastUndoActionText(ALength: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// What is the type of an action?
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoActionType(AAction: Integer): Integer;
    /// <summary>
    /// What is the position of an action?
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetUndoActionPosition(AAction: Integer): TSciPosition;
    /// <summary>
    /// What is the text of an action?
    /// </summary>
    /// <param name="AAction">
    /// Integer value
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoActionText(AAction: Integer; AText: PAnsiChar): Integer;
    /// <summary>
    /// Set an indicator to plain, squiggle or TT.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AIndicatorStyle">
    /// The AIndicatorStyle parameter
    /// </param>
    procedure IndicSetStyle(AIndicator: Integer; AIndicatorStyle: NativeInt);
    /// <summary>
    /// Retrieve the style of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetStyle(AIndicator: Integer): NativeInt;
    /// <summary>
    /// Set the foreground colour of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure IndicSetFore(AIndicator: Integer; AFore: TColor);
    /// <summary>
    /// Retrieve the foreground colour of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function IndicGetFore(AIndicator: Integer): TColor;
    /// <summary>
    /// Set an indicator to draw under text or over(default).
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AUnder">
    /// Boolean value
    /// </param>
    procedure IndicSetUnder(AIndicator: Integer; AUnder: Boolean);
    /// <summary>
    /// Retrieve whether indicator drawn under or over text.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function IndicGetUnder(AIndicator: Integer): Boolean;
    /// <summary>
    /// Set a hover indicator to plain, squiggle or TT.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AIndicatorStyle">
    /// The AIndicatorStyle parameter
    /// </param>
    procedure IndicSetHoverStyle(AIndicator: Integer; AIndicatorStyle: NativeInt);
    /// <summary>
    /// Retrieve the hover style of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetHoverStyle(AIndicator: Integer): NativeInt;
    /// <summary>
    /// Set the foreground hover colour of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure IndicSetHoverFore(AIndicator: Integer; AFore: TColor);
    /// <summary>
    /// Retrieve the foreground hover colour of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function IndicGetHoverFore(AIndicator: Integer): TColor;
    /// <summary>
    /// Set the attributes of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AFlags">
    /// The AFlags parameter
    /// </param>
    procedure IndicSetFlags(AIndicator: Integer; AFlags: NativeInt);
    /// <summary>
    /// Retrieve the attributes of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetFlags(AIndicator: Integer): NativeInt;
    /// <summary>
    /// Set the stroke width of an indicator in hundredths of a pixel.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AHundredths">
    /// Integer value
    /// </param>
    procedure IndicSetStrokeWidth(AIndicator: Integer; AHundredths: Integer);
    /// <summary>
    /// Retrieve the stroke width of an indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function IndicGetStrokeWidth(AIndicator: Integer): Integer;
    /// <summary>
    /// Set the foreground colour of all whitespace and whether to use this setting.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetWhitespaceFore(AUseSetting: Boolean; AFore: TColor);
    /// <summary>
    /// Set the background colour of all whitespace and whether to use this setting.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetWhitespaceBack(AUseSetting: Boolean; ABack: TColor);
    /// <summary>
    /// Set the size of the dots used to mark space characters.
    /// </summary>
    /// <param name="ASize">
    /// Integer value
    /// </param>
    procedure SetWhitespaceSize(ASize: Integer);
    /// <summary>
    /// Get the size of the dots used to mark space characters.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetWhitespaceSize(): Integer;
    /// <summary>
    /// Used to hold extra styling information for each line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AState">
    /// Integer value
    /// </param>
    procedure SetLineState(ALine: TSciLine; AState: Integer);
    /// <summary>
    /// Retrieve the extra styling information for a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLineState(ALine: TSciLine): Integer;
    /// <summary>
    /// Retrieve the last line number that has line state.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMaxLineState(): Integer;
    /// <summary>
    /// Is the background of the line containing the caret in a different colour?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetCaretLineVisible(): Boolean;
    /// <summary>
    /// Display the background of the line containing the caret in a different colour.
    /// </summary>
    /// <param name="AShow">
    /// Boolean value
    /// </param>
    procedure SetCaretLineVisible(AShow: Boolean);
    /// <summary>
    /// Get the colour of the background of the line containing the caret.
    /// </summary>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetCaretLineBack(): TColor;
    /// <summary>
    /// Set the colour of the background of the line containing the caret.
    /// </summary>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetCaretLineBack(ABack: TColor);
    /// <summary>
    /// Retrieve the caret line frame width.
    /// Width = 0 means this option is disabled.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCaretLineFrame(): Integer;
    /// <summary>
    /// Display the caret line framed.
    /// Set width != 0 to enable this option and width = 0 to disable it.
    /// </summary>
    /// <param name="AWidth">
    /// Integer value
    /// </param>
    procedure SetCaretLineFrame(AWidth: Integer);
    /// <summary>
    /// Set a style to be changeable or not (read only).
    /// Experimental feature, currently buggy.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AChangeable">
    /// Boolean value
    /// </param>
    procedure StyleSetChangeable(AStyle: Integer; AChangeable: Boolean);
    /// <summary>
    /// Display a auto-completion list.
    /// The lengthEntered parameter indicates how many characters before
    /// the caret should be used to provide context.
    /// </summary>
    /// <param name="ALengthEntered">
    /// Position in the document
    /// </param>
    /// <param name="AItemList">
    /// Text string
    /// </param>
    procedure AutoCShow(ALengthEntered: TSciPosition; AItemList: PAnsiChar);
    /// <summary>
    /// Remove the auto-completion list from the screen.
    /// </summary>
    procedure AutoCCancel();
    /// <summary>
    /// Is there an auto-completion list visible?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function AutoCActive(): Boolean;
    /// <summary>
    /// Retrieve the position of the caret when the auto-completion list was displayed.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function AutoCPosStart(): TSciPosition;
    /// <summary>
    /// User has selected an item so remove the list and insert the selection.
    /// </summary>
    procedure AutoCComplete();
    /// <summary>
    /// Define a set of character that when typed cancel the auto-completion list.
    /// </summary>
    /// <param name="ACharacterSet">
    /// Text string
    /// </param>
    procedure AutoCStops(ACharacterSet: PAnsiChar);
    /// <summary>
    /// Change the separator character in the string setting up an auto-completion list.
    /// Default is space but can be changed if items contain space.
    /// </summary>
    /// <param name="ASeparatorCharacter">
    /// Integer value
    /// </param>
    procedure AutoCSetSeparator(ASeparatorCharacter: Integer);
    /// <summary>
    /// Retrieve the auto-completion list separator character.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetSeparator(): Integer;
    /// <summary>
    /// Select the item in the auto-completion list that starts with a string.
    /// </summary>
    /// <param name="ASelect">
    /// Text string
    /// </param>
    procedure AutoCSelect(ASelect: PAnsiChar);
    /// <summary>
    /// Should the auto-completion list be cancelled if the user backspaces to a
    /// position before where the box was created.
    /// </summary>
    /// <param name="ACancel">
    /// Boolean value
    /// </param>
    procedure AutoCSetCancelAtStart(ACancel: Boolean);
    /// <summary>
    /// Retrieve whether auto-completion cancelled by backspacing before start.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function AutoCGetCancelAtStart(): Boolean;
    /// <summary>
    /// Define a set of characters that when typed will cause the autocompletion to
    /// choose the selected item.
    /// </summary>
    /// <param name="ACharacterSet">
    /// Text string
    /// </param>
    procedure AutoCSetFillUps(ACharacterSet: PAnsiChar);
    /// <summary>
    /// Should a single item auto-completion list automatically choose the item.
    /// </summary>
    /// <param name="AChooseSingle">
    /// Boolean value
    /// </param>
    procedure AutoCSetChooseSingle(AChooseSingle: Boolean);
    /// <summary>
    /// Retrieve whether a single item auto-completion list automatically choose the item.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function AutoCGetChooseSingle(): Boolean;
    /// <summary>
    /// Set whether case is significant when performing auto-completion searches.
    /// </summary>
    /// <param name="AIgnoreCase">
    /// Boolean value
    /// </param>
    procedure AutoCSetIgnoreCase(AIgnoreCase: Boolean);
    /// <summary>
    /// Retrieve state of ignore case flag.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function AutoCGetIgnoreCase(): Boolean;
    /// <summary>
    /// Display a list of strings and send notification when user chooses one.
    /// </summary>
    /// <param name="AListType">
    /// Integer value
    /// </param>
    /// <param name="AItemList">
    /// Text string
    /// </param>
    procedure UserListShow(AListType: Integer; AItemList: PAnsiChar);
    /// <summary>
    /// Set whether or not autocompletion is hidden automatically when nothing matches.
    /// </summary>
    /// <param name="AAutoHide">
    /// Boolean value
    /// </param>
    procedure AutoCSetAutoHide(AAutoHide: Boolean);
    /// <summary>
    /// Retrieve whether or not autocompletion is hidden automatically when nothing matches.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function AutoCGetAutoHide(): Boolean;
    /// <summary>
    /// Set autocompletion options.
    /// </summary>
    /// <param name="AOptions">
    /// The AOptions parameter
    /// </param>
    procedure AutoCSetOptions(AOptions: NativeInt);
    /// <summary>
    /// Retrieve autocompletion options.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetOptions(): NativeInt;
    /// <summary>
    /// Set whether or not autocompletion deletes any word characters
    /// after the inserted text upon completion.
    /// </summary>
    /// <param name="ADropRestOfWord">
    /// Boolean value
    /// </param>
    procedure AutoCSetDropRestOfWord(ADropRestOfWord: Boolean);
    /// <summary>
    /// Retrieve whether or not autocompletion deletes any word characters
    /// after the inserted text upon completion.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function AutoCGetDropRestOfWord(): Boolean;
    /// <summary>
    /// Register an XPM image for use in autocompletion lists.
    /// </summary>
    /// <param name="AType">
    /// Integer value
    /// </param>
    /// <param name="AXpmData">
    /// Text string
    /// </param>
    procedure RegisterImage(AType: Integer; AXpmData: PAnsiChar);
    /// <summary>
    /// Clear all the registered XPM images.
    /// </summary>
    procedure ClearRegisteredImages();
    /// <summary>
    /// Retrieve the auto-completion list type-separator character.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetTypeSeparator(): Integer;
    /// <summary>
    /// Change the type-separator character in the string setting up an auto-completion list.
    /// Default is '?' but can be changed if items contain '?'.
    /// </summary>
    /// <param name="ASeparatorCharacter">
    /// Integer value
    /// </param>
    procedure AutoCSetTypeSeparator(ASeparatorCharacter: Integer);
    /// <summary>
    /// Set the maximum width, in characters, of auto-completion and user lists.
    /// Set to 0 to autosize to fit longest item, which is the default.
    /// </summary>
    /// <param name="ACharacterCount">
    /// Integer value
    /// </param>
    procedure AutoCSetMaxWidth(ACharacterCount: Integer);
    /// <summary>
    /// Get the maximum width, in characters, of auto-completion and user lists.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetMaxWidth(): Integer;
    /// <summary>
    /// Set the maximum height, in rows, of auto-completion and user lists.
    /// The default is 5 rows.
    /// </summary>
    /// <param name="ARowCount">
    /// Integer value
    /// </param>
    procedure AutoCSetMaxHeight(ARowCount: Integer);
    /// <summary>
    /// Set the maximum height, in rows, of auto-completion and user lists.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetMaxHeight(): Integer;
    /// <summary>
    /// Set the style number used for auto-completion and user lists fonts.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure AutoCSetStyle(AStyle: Integer);
    /// <summary>
    /// Get the style number used for auto-completion and user lists fonts.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetStyle(): Integer;
    /// <summary>
    /// Set the number of spaces used for one level of indentation.
    /// </summary>
    /// <param name="AIndentSize">
    /// Integer value
    /// </param>
    procedure SetIndent(AIndentSize: Integer);
    /// <summary>
    /// Retrieve indentation size.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetIndent(): Integer;
    /// <summary>
    /// Indentation will only use space characters if useTabs is false, otherwise
    /// it will use a combination of tabs and spaces.
    /// </summary>
    /// <param name="AUseTabs">
    /// Boolean value
    /// </param>
    procedure SetUseTabs(AUseTabs: Boolean);
    /// <summary>
    /// Retrieve whether tabs will be used in indentation.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetUseTabs(): Boolean;
    /// <summary>
    /// Change the indentation of a line to a number of columns.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AIndentation">
    /// Integer value
    /// </param>
    procedure SetLineIndentation(ALine: TSciLine; AIndentation: Integer);
    /// <summary>
    /// Retrieve the number of columns that a line is indented.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLineIndentation(ALine: TSciLine): Integer;
    /// <summary>
    /// Retrieve the position before the first non indentation character on a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineIndentPosition(ALine: TSciLine): TSciPosition;
    /// <summary>
    /// Retrieve the column number of a position, taking tab width into account.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetColumn(APos: TSciPosition): TSciPosition;
    /// <summary>
    /// Count characters between two positions.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CountCharacters(AStart: TSciPosition; AEnd: TSciPosition): TSciPosition;
    /// <summary>
    /// Count code units between two positions.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CountCodeUnits(AStart: TSciPosition; AEnd: TSciPosition): TSciPosition;
    /// <summary>
    /// Show or hide the horizontal scroll bar.
    /// </summary>
    /// <param name="AVisible">
    /// Boolean value
    /// </param>
    procedure SetHScrollBar(AVisible: Boolean);
    /// <summary>
    /// Is the horizontal scroll bar visible?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetHScrollBar(): Boolean;
    /// <summary>
    /// Show or hide indentation guides.
    /// </summary>
    /// <param name="AIndentView">
    /// The AIndentView parameter
    /// </param>
    procedure SetIndentationGuides(AIndentView: NativeInt);
    /// <summary>
    /// Are the indentation guides visible?
    /// </summary>
    /// <returns>
    /// Returns the indentationguides
    /// </returns>
    function GetIndentationGuides(): NativeInt;
    /// <summary>
    /// Set the highlighted indentation guide column.
    /// 0 = no highlighted guide.
    /// </summary>
    /// <param name="AColumn">
    /// Position in the document
    /// </param>
    procedure SetHighlightGuide(AColumn: TSciPosition);
    /// <summary>
    /// Get the highlighted indentation guide column.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetHighlightGuide(): TSciPosition;
    /// <summary>
    /// Get the position after the last visible characters on a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineEndPosition(ALine: TSciLine): TSciPosition;
    /// <summary>
    /// Get the code page used to interpret the bytes of the document as characters.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCodePage(): Integer;
    /// <summary>
    /// Get the foreground colour of the caret.
    /// </summary>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetCaretFore(): TColor;
    /// <summary>
    /// In read-only mode?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetReadOnly(): Boolean;
    /// <summary>
    /// Sets the position of the caret.
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetCurrentPos(ACaret: TSciPosition);
    /// <summary>
    /// Sets the position that starts the selection - this becomes the anchor.
    /// </summary>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure SetSelectionStart(AAnchor: TSciPosition);
    /// <summary>
    /// Returns the position at the start of the selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionStart(): TSciPosition;
    /// <summary>
    /// Sets the position that ends the selection - this becomes the caret.
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetSelectionEnd(ACaret: TSciPosition);
    /// <summary>
    /// Returns the position at the end of the selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionEnd(): TSciPosition;
    /// <summary>
    /// Set caret to a position, while removing any existing selection.
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetEmptySelection(ACaret: TSciPosition);
    /// <summary>
    /// Sets the print magnification added to the point size of each style for printing.
    /// </summary>
    /// <param name="AMagnification">
    /// Integer value
    /// </param>
    procedure SetPrintMagnification(AMagnification: Integer);
    /// <summary>
    /// Returns the print magnification.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPrintMagnification(): Integer;
    /// <summary>
    /// Modify colours when printing for clearer printed text.
    /// </summary>
    /// <param name="AMode">
    /// The AMode parameter
    /// </param>
    procedure SetPrintColourMode(AMode: NativeInt);
    /// <summary>
    /// Returns the print colour mode.
    /// </summary>
    /// <returns>
    /// Returns the printcolourmode
    /// </returns>
    function GetPrintColourMode(): NativeInt;
    /// <summary>
    /// Find some text in the document.
    /// </summary>
    /// <param name="ASearchFlags">
    /// The ASearchFlags parameter
    /// </param>
    /// <param name="AFt">
    /// The AFt parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FindText(ASearchFlags: NativeInt; AFt: PSciFindText): TSciPosition;
    /// <summary>
    /// Find some text in the document.
    /// </summary>
    /// <param name="ASearchFlags">
    /// The ASearchFlags parameter
    /// </param>
    /// <param name="AFt">
    /// The AFt parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FindTextFull(ASearchFlags: NativeInt; AFt: PSciFindTextFull): TSciPosition;
    /// <summary>
    /// Draw the document into a display context such as a printer.
    /// </summary>
    /// <param name="ADraw">
    /// Boolean value
    /// </param>
    /// <param name="AFr">
    /// The AFr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FormatRange(ADraw: Boolean; AFr: PSciRangeToFormat): TSciPosition;
    /// <summary>
    /// Draw the document into a display context such as a printer.
    /// </summary>
    /// <param name="ADraw">
    /// Boolean value
    /// </param>
    /// <param name="AFr">
    /// The AFr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FormatRangeFull(ADraw: Boolean; AFr: PSciRangeToFormatFull): TSciPosition;
    /// <summary>
    /// Enable or disable change history.
    /// </summary>
    /// <param name="AChangeHistory">
    /// The AChangeHistory parameter
    /// </param>
    procedure SetChangeHistory(AChangeHistory: NativeInt);
    /// <summary>
    /// Report change history status.
    /// </summary>
    /// <returns>
    /// Returns the changehistory
    /// </returns>
    function GetChangeHistory(): NativeInt;
    /// <summary>
    /// Enable or disable undo selection history.
    /// </summary>
    /// <param name="AUndoSelectionHistory">
    /// The AUndoSelectionHistory parameter
    /// </param>
    procedure SetUndoSelectionHistory(AUndoSelectionHistory: NativeInt);
    /// <summary>
    /// Report undo selection history status.
    /// </summary>
    /// <returns>
    /// Returns the undoselectionhistory
    /// </returns>
    function GetUndoSelectionHistory(): NativeInt;
    /// <summary>
    /// Set selection from serialized form.
    /// </summary>
    /// <param name="ASelectionString">
    /// Text string
    /// </param>
    procedure SetSelectionSerialized(ASelectionString: PAnsiChar);
    /// <summary>
    /// Retrieve serialized form of selection.
    /// </summary>
    /// <param name="ASelectionString">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionSerialized(ASelectionString: PAnsiChar): TSciPosition;
    /// <summary>
    /// Retrieve the display line at the top of the display.
    /// </summary>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetFirstVisibleLine(): TSciLine;
    /// <summary>
    /// Retrieve the contents of a line.
    /// Returns the length of the line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLine(ALine: TSciLine; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Returns the number of lines in the document. There is always at least one.
    /// </summary>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetLineCount(): TSciLine;
    /// <summary>
    /// Enlarge the number of lines allocated.
    /// </summary>
    /// <param name="ALines">
    /// Line number
    /// </param>
    procedure AllocateLines(ALines: TSciLine);
    /// <summary>
    /// Sets the size in pixels of the left margin.
    /// </summary>
    /// <param name="APixelWidth">
    /// Integer value
    /// </param>
    procedure SetMarginLeft(APixelWidth: Integer);
    /// <summary>
    /// Returns the size in pixels of the left margin.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMarginLeft(): Integer;
    /// <summary>
    /// Sets the size in pixels of the right margin.
    /// </summary>
    /// <param name="APixelWidth">
    /// Integer value
    /// </param>
    procedure SetMarginRight(APixelWidth: Integer);
    /// <summary>
    /// Returns the size in pixels of the right margin.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMarginRight(): Integer;
    /// <summary>
    /// Is the document different from when it was last saved?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetModify(): Boolean;
    /// <summary>
    /// Select a range of text.
    /// </summary>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetSel(AAnchor: TSciPosition; ACaret: TSciPosition);
    /// <summary>
    /// Retrieve the selected text.
    /// Return the length of the text.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelText(AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Retrieve a range of text.
    /// Return the length of the text.
    /// </summary>
    /// <param name="ATr">
    /// The ATr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTextRange(ATr: PSciTextRange): TSciPosition;
    /// <summary>
    /// Retrieve a range of text that can be past 2GB.
    /// Return the length of the text.
    /// </summary>
    /// <param name="ATr">
    /// The ATr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTextRangeFull(ATr: PSciTextRangeFull): TSciPosition;
    /// <summary>
    /// Draw the selection either highlighted or in normal (non-highlighted) style.
    /// </summary>
    /// <param name="AHide">
    /// Boolean value
    /// </param>
    procedure HideSelection(AHide: Boolean);
    function GetSelectionHidden(): Boolean;
    /// <summary>
    /// Retrieve the x value of the point in the window where a position is displayed.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function PointXFromPosition(APos: TSciPosition): Integer;
    /// <summary>
    /// Retrieve the y value of the point in the window where a position is displayed.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function PointYFromPosition(APos: TSciPosition): Integer;
    /// <summary>
    /// Retrieve the line containing a position.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function LineFromPosition(APos: TSciPosition): TSciLine;
    /// <summary>
    /// Retrieve the position at the start of a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionFromLine(ALine: TSciLine): TSciPosition;
    /// <summary>
    /// Scroll horizontally and vertically.
    /// </summary>
    /// <param name="AColumns">
    /// Position in the document
    /// </param>
    /// <param name="ALines">
    /// Line number
    /// </param>
    procedure LineScroll(AColumns: TSciPosition; ALines: TSciLine);
    /// <summary>
    /// Ensure the caret is visible.
    /// </summary>
    procedure ScrollCaret();
    /// <summary>
    /// Scroll the argument positions and the range between them into view giving
    /// priority to the primary position then the secondary position.
    /// This may be used to make a search match visible.
    /// </summary>
    /// <param name="ASecondary">
    /// Position in the document
    /// </param>
    /// <param name="APrimary">
    /// Position in the document
    /// </param>
    procedure ScrollRange(ASecondary: TSciPosition; APrimary: TSciPosition);
    /// <summary>
    /// Replace the selected text with the argument text.
    /// </summary>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure ReplaceSel(AText: PAnsiChar);
    /// <summary>
    /// Set to read only or read write.
    /// </summary>
    /// <param name="AReadOnly">
    /// Boolean value
    /// </param>
    procedure SetReadOnly(AReadOnly: Boolean);
    /// <summary>
    /// Null operation.
    /// </summary>
    procedure Null();
    /// <summary>
    /// Will a paste succeed?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function CanPaste(): Boolean;
    /// <summary>
    /// Are there any undoable actions in the undo history?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function CanUndo(): Boolean;
    /// <summary>
    /// Delete the undo history.
    /// </summary>
    procedure EmptyUndoBuffer();
    /// <summary>
    /// Undo one action in the undo history.
    /// </summary>
    procedure Undo();
    /// <summary>
    /// Cut the selection to the clipboard.
    /// </summary>
    procedure Cut();
    /// <summary>
    /// Copy the selection to the clipboard.
    /// </summary>
    procedure Copy();
    /// <summary>
    /// Paste the contents of the clipboard into the document replacing the selection.
    /// </summary>
    procedure Paste();
    /// <summary>
    /// Clear the selection.
    /// </summary>
    procedure Clear();
    /// <summary>
    /// Replace the contents of the document with the argument text.
    /// </summary>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure SetText(AText: PAnsiChar);
    /// <summary>
    /// Retrieve all the text in the document.
    /// Returns number of characters retrieved.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetText(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Retrieve the number of characters in the document.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTextLength(): TSciPosition;
    /// <summary>
    /// Retrieve a pointer to a function that processes messages for this Scintilla.
    /// </summary>
    /// <returns>
    /// Returns the directfunction
    /// </returns>
    function GetDirectFunction(): Pointer;
    /// <summary>
    /// Retrieve a pointer to a function that processes messages for this Scintilla and returns status.
    /// </summary>
    /// <returns>
    /// Returns the directstatusfunction
    /// </returns>
    function GetDirectStatusFunction(): Pointer;
    /// <summary>
    /// Retrieve a pointer value to use as the first argument when calling
    /// the function returned by GetDirectFunction.
    /// </summary>
    /// <returns>
    /// Returns the directpointer
    /// </returns>
    function GetDirectPointer(): Pointer;
    /// <summary>
    /// Set to overtype (true) or insert mode.
    /// </summary>
    /// <param name="AOverType">
    /// Boolean value
    /// </param>
    procedure SetOvertype(AOverType: Boolean);
    /// <summary>
    /// Returns true if overtype mode is active otherwise false is returned.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetOvertype(): Boolean;
    /// <summary>
    /// Set the width of the insert mode caret.
    /// </summary>
    /// <param name="APixelWidth">
    /// Integer value
    /// </param>
    procedure SetCaretWidth(APixelWidth: Integer);
    /// <summary>
    /// Returns the width of the insert mode caret.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCaretWidth(): Integer;
    /// <summary>
    /// Sets the position that starts the target which is used for updating the
    /// document without affecting the scroll position.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    procedure SetTargetStart(AStart: TSciPosition);
    /// <summary>
    /// Get the position that starts the target.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetStart(): TSciPosition;
    /// <summary>
    /// Sets the virtual space of the target start
    /// </summary>
    /// <param name="ASpace">
    /// Position in the document
    /// </param>
    procedure SetTargetStartVirtualSpace(ASpace: TSciPosition);
    /// <summary>
    /// Get the virtual space of the target start
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetStartVirtualSpace(): TSciPosition;
    /// <summary>
    /// Sets the position that ends the target which is used for updating the
    /// document without affecting the scroll position.
    /// </summary>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    procedure SetTargetEnd(AEnd: TSciPosition);
    /// <summary>
    /// Get the position that ends the target.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetEnd(): TSciPosition;
    /// <summary>
    /// Sets the virtual space of the target end
    /// </summary>
    /// <param name="ASpace">
    /// Position in the document
    /// </param>
    procedure SetTargetEndVirtualSpace(ASpace: TSciPosition);
    /// <summary>
    /// Get the virtual space of the target end
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetEndVirtualSpace(): TSciPosition;
    /// <summary>
    /// Sets both the start and end of the target in one call.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    procedure SetTargetRange(AStart: TSciPosition; AEnd: TSciPosition);
    /// <summary>
    /// Retrieve the text in the target.
    /// </summary>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetText(AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Make the target range start and end be the same as the selection range start and end.
    /// </summary>
    procedure TargetFromSelection();
    /// <summary>
    /// Sets the target to the whole document.
    /// </summary>
    procedure TargetWholeDocument();
    /// <summary>
    /// Replace the target text with the argument text.
    /// Text is counted so it can contain NULs.
    /// Returns the length of the replacement text.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function ReplaceTarget(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Replace the target text with the argument text after \d processing.
    /// Text is counted so it can contain NULs.
    /// Looks for \d where d is between 1 and 9 and replaces these with the strings
    /// matched in the last search operation which were surrounded by \( and \).
    /// Returns the length of the replacement text including any change
    /// caused by processing the \d patterns.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function ReplaceTargetRE(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Replace the target text with the argument text but ignore prefix and suffix that
    /// are the same as current.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function ReplaceTargetMinimal(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Search for a counted string in the target and set the target to the found
    /// range. Text is counted so it can contain NULs.
    /// Returns start of found range or -1 for failure in which case target is not moved.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function SearchInTarget(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Set the search flags used by SearchInTarget.
    /// </summary>
    /// <param name="ASearchFlags">
    /// The ASearchFlags parameter
    /// </param>
    procedure SetSearchFlags(ASearchFlags: NativeInt);
    /// <summary>
    /// Get the search flags used by SearchInTarget.
    /// </summary>
    /// <returns>
    /// Returns the searchflags
    /// </returns>
    function GetSearchFlags(): NativeInt;
    /// <summary>
    /// Show a call tip containing a definition near position pos.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="ADefinition">
    /// Text string
    /// </param>
    procedure CallTipShow(APos: TSciPosition; ADefinition: PAnsiChar);
    /// <summary>
    /// Remove the call tip from the screen.
    /// </summary>
    procedure CallTipCancel();
    /// <summary>
    /// Is there an active call tip?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function CallTipActive(): Boolean;
    /// <summary>
    /// Retrieve the position where the caret was before displaying the call tip.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CallTipPosStart(): TSciPosition;
    /// <summary>
    /// Set the start position in order to change when backspacing removes the calltip.
    /// </summary>
    /// <param name="APosStart">
    /// Position in the document
    /// </param>
    procedure CallTipSetPosStart(APosStart: TSciPosition);
    /// <summary>
    /// Highlight a segment of the definition.
    /// </summary>
    /// <param name="AHighlightStart">
    /// Position in the document
    /// </param>
    /// <param name="AHighlightEnd">
    /// Position in the document
    /// </param>
    procedure CallTipSetHlt(AHighlightStart: TSciPosition; AHighlightEnd: TSciPosition);
    /// <summary>
    /// Set the background colour for the call tip.
    /// </summary>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure CallTipSetBack(ABack: TColor);
    /// <summary>
    /// Set the foreground colour for the call tip.
    /// </summary>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure CallTipSetFore(AFore: TColor);
    /// <summary>
    /// Set the foreground colour for the highlighted part of the call tip.
    /// </summary>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure CallTipSetForeHlt(AFore: TColor);
    /// <summary>
    /// Enable use of STYLE_CALLTIP and set call tip tab size in pixels.
    /// </summary>
    /// <param name="ATabSize">
    /// Integer value
    /// </param>
    procedure CallTipUseStyle(ATabSize: Integer);
    /// <summary>
    /// Set position of calltip, above or below text.
    /// </summary>
    /// <param name="AAbove">
    /// Boolean value
    /// </param>
    procedure CallTipSetPosition(AAbove: Boolean);
    /// <summary>
    /// Find the display line of a document line taking hidden lines into account.
    /// </summary>
    /// <param name="ADocLine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function VisibleFromDocLine(ADocLine: TSciLine): TSciLine;
    /// <summary>
    /// Find the document line of a display line taking hidden lines into account.
    /// </summary>
    /// <param name="ADisplayLine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function DocLineFromVisible(ADisplayLine: TSciLine): TSciLine;
    /// <summary>
    /// The number of display lines needed to wrap a document line
    /// </summary>
    /// <param name="ADocLine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function WrapCount(ADocLine: TSciLine): TSciLine;
    /// <summary>
    /// Set the fold level of a line.
    /// This encodes an integer level along with flags indicating whether the
    /// line is a header and whether it is effectively white space.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="ALevel">
    /// The ALevel parameter
    /// </param>
    procedure SetFoldLevel(ALine: TSciLine; ALevel: NativeInt);
    /// <summary>
    /// Retrieve the fold level of a line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the foldlevel
    /// </returns>
    function GetFoldLevel(ALine: TSciLine): NativeInt;
    /// <summary>
    /// Find the last child line of a header line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="ALevel">
    /// The ALevel parameter
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetLastChild(ALine: TSciLine; ALevel: NativeInt): TSciLine;
    /// <summary>
    /// Find the parent line of a child line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetFoldParent(ALine: TSciLine): TSciLine;
    /// <summary>
    /// Make a range of lines visible.
    /// </summary>
    /// <param name="ALineStart">
    /// Line number
    /// </param>
    /// <param name="ALineEnd">
    /// Line number
    /// </param>
    procedure ShowLines(ALineStart: TSciLine; ALineEnd: TSciLine);
    /// <summary>
    /// Make a range of lines invisible.
    /// </summary>
    /// <param name="ALineStart">
    /// Line number
    /// </param>
    /// <param name="ALineEnd">
    /// Line number
    /// </param>
    procedure HideLines(ALineStart: TSciLine; ALineEnd: TSciLine);
    /// <summary>
    /// Is a line visible?
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetLineVisible(ALine: TSciLine): Boolean;
    /// <summary>
    /// Are all lines visible?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetAllLinesVisible(): Boolean;
    /// <summary>
    /// Show the children of a header line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AExpanded">
    /// Boolean value
    /// </param>
    procedure SetFoldExpanded(ALine: TSciLine; AExpanded: Boolean);
    /// <summary>
    /// Is a header line expanded?
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetFoldExpanded(ALine: TSciLine): Boolean;
    /// <summary>
    /// Switch a header line between expanded and contracted.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    procedure ToggleFold(ALine: TSciLine);
    /// <summary>
    /// Switch a header line between expanded and contracted and show some text after the line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure ToggleFoldShowText(ALine: TSciLine; AText: PAnsiChar);
    /// <summary>
    /// Set the style of fold display text.
    /// </summary>
    /// <param name="AStyle">
    /// The AStyle parameter
    /// </param>
    procedure FoldDisplayTextSetStyle(AStyle: NativeInt);
    /// <summary>
    /// Get the style of fold display text.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function FoldDisplayTextGetStyle(): NativeInt;
    /// <summary>
    /// Set the default fold display text.
    /// </summary>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure SetDefaultFoldDisplayText(AText: PAnsiChar);
    /// <summary>
    /// Get the default fold display text.
    /// </summary>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetDefaultFoldDisplayText(AText: PAnsiChar): Integer;
    /// <summary>
    /// Expand or contract a fold header.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AAction">
    /// The AAction parameter
    /// </param>
    procedure FoldLine(ALine: TSciLine; AAction: NativeInt);
    /// <summary>
    /// Expand or contract a fold header and its children.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AAction">
    /// The AAction parameter
    /// </param>
    procedure FoldChildren(ALine: TSciLine; AAction: NativeInt);
    /// <summary>
    /// Expand a fold header and all children. Use the level argument instead of the line's current level.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="ALevel">
    /// The ALevel parameter
    /// </param>
    procedure ExpandChildren(ALine: TSciLine; ALevel: NativeInt);
    /// <summary>
    /// Expand or contract all fold headers.
    /// </summary>
    /// <param name="AAction">
    /// The AAction parameter
    /// </param>
    procedure FoldAll(AAction: NativeInt);
    /// <summary>
    /// Ensure a particular line is visible by expanding any header line hiding it.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    procedure EnsureVisible(ALine: TSciLine);
    /// <summary>
    /// Set automatic folding behaviours.
    /// </summary>
    /// <param name="AAutomaticFold">
    /// The AAutomaticFold parameter
    /// </param>
    procedure SetAutomaticFold(AAutomaticFold: NativeInt);
    /// <summary>
    /// Get automatic folding behaviours.
    /// </summary>
    /// <returns>
    /// Returns the automaticfold
    /// </returns>
    function GetAutomaticFold(): NativeInt;
    /// <summary>
    /// Set some style options for folding.
    /// </summary>
    /// <param name="AFlags">
    /// The AFlags parameter
    /// </param>
    procedure SetFoldFlags(AFlags: NativeInt);
    /// <summary>
    /// Ensure a particular line is visible by expanding any header line hiding it.
    /// Use the currently set visibility policy to determine which range to display.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    procedure EnsureVisibleEnforcePolicy(ALine: TSciLine);
    /// <summary>
    /// Sets whether a tab pressed when caret is within indentation indents.
    /// </summary>
    /// <param name="ATabIndents">
    /// Boolean value
    /// </param>
    procedure SetTabIndents(ATabIndents: Boolean);
    /// <summary>
    /// Does a tab pressed when caret is within indentation indent?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetTabIndents(): Boolean;
    /// <summary>
    /// Sets whether a backspace pressed when caret is within indentation unindents.
    /// </summary>
    /// <param name="ABsUnIndents">
    /// Boolean value
    /// </param>
    procedure SetBackSpaceUnIndents(ABsUnIndents: Boolean);
    /// <summary>
    /// Does a backspace pressed when caret is within indentation unindent?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetBackSpaceUnIndents(): Boolean;
    /// <summary>
    /// Sets the time the mouse must sit still to generate a mouse dwell event.
    /// </summary>
    /// <param name="APeriodMilliseconds">
    /// Integer value
    /// </param>
    procedure SetMouseDwellTime(APeriodMilliseconds: Integer);
    /// <summary>
    /// Retrieve the time the mouse must sit still to generate a mouse dwell event.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMouseDwellTime(): Integer;
    /// <summary>
    /// Get position of start of word.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="AOnlyWordCharacters">
    /// Boolean value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function WordStartPosition(APos: TSciPosition; AOnlyWordCharacters: Boolean): TSciPosition;
    /// <summary>
    /// Get position of end of word.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="AOnlyWordCharacters">
    /// Boolean value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function WordEndPosition(APos: TSciPosition; AOnlyWordCharacters: Boolean): TSciPosition;
    /// <summary>
    /// Is the range start..end considered a word?
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function IsRangeWord(AStart: TSciPosition; AEnd: TSciPosition): Boolean;
    /// <summary>
    /// Sets limits to idle styling.
    /// </summary>
    /// <param name="AIdleStyling">
    /// The AIdleStyling parameter
    /// </param>
    procedure SetIdleStyling(AIdleStyling: NativeInt);
    /// <summary>
    /// Retrieve the limits to idle styling.
    /// </summary>
    /// <returns>
    /// Returns the idlestyling
    /// </returns>
    function GetIdleStyling(): NativeInt;
    /// <summary>
    /// Sets whether text is word wrapped.
    /// </summary>
    /// <param name="AWrapMode">
    /// The AWrapMode parameter
    /// </param>
    procedure SetWrapMode(AWrapMode: NativeInt);
    /// <summary>
    /// Retrieve whether text is word wrapped.
    /// </summary>
    /// <returns>
    /// Returns the wrapmode
    /// </returns>
    function GetWrapMode(): NativeInt;
    /// <summary>
    /// Set the display mode of visual flags for wrapped lines.
    /// </summary>
    /// <param name="AWrapVisualFlags">
    /// The AWrapVisualFlags parameter
    /// </param>
    procedure SetWrapVisualFlags(AWrapVisualFlags: NativeInt);
    /// <summary>
    /// Retrive the display mode of visual flags for wrapped lines.
    /// </summary>
    /// <returns>
    /// Returns the wrapvisualflags
    /// </returns>
    function GetWrapVisualFlags(): NativeInt;
    /// <summary>
    /// Set the location of visual flags for wrapped lines.
    /// </summary>
    /// <param name="AWrapVisualFlagsLocation">
    /// The AWrapVisualFlagsLocation parameter
    /// </param>
    procedure SetWrapVisualFlagsLocation(AWrapVisualFlagsLocation: NativeInt);
    /// <summary>
    /// Retrive the location of visual flags for wrapped lines.
    /// </summary>
    /// <returns>
    /// Returns the wrapvisualflagslocation
    /// </returns>
    function GetWrapVisualFlagsLocation(): NativeInt;
    /// <summary>
    /// Set the start indent for wrapped lines.
    /// </summary>
    /// <param name="AIndent">
    /// Integer value
    /// </param>
    procedure SetWrapStartIndent(AIndent: Integer);
    /// <summary>
    /// Retrive the start indent for wrapped lines.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetWrapStartIndent(): Integer;
    /// <summary>
    /// Sets how wrapped sublines are placed. Default is fixed.
    /// </summary>
    /// <param name="AWrapIndentMode">
    /// The AWrapIndentMode parameter
    /// </param>
    procedure SetWrapIndentMode(AWrapIndentMode: NativeInt);
    /// <summary>
    /// Retrieve how wrapped sublines are placed. Default is fixed.
    /// </summary>
    /// <returns>
    /// Returns the wrapindentmode
    /// </returns>
    function GetWrapIndentMode(): NativeInt;
    /// <summary>
    /// Sets the degree of caching of layout information.
    /// </summary>
    /// <param name="ACacheMode">
    /// The ACacheMode parameter
    /// </param>
    procedure SetLayoutCache(ACacheMode: NativeInt);
    /// <summary>
    /// Retrieve the degree of caching of layout information.
    /// </summary>
    /// <returns>
    /// Returns the layoutcache
    /// </returns>
    function GetLayoutCache(): NativeInt;
    /// <summary>
    /// Sets the document width assumed for scrolling.
    /// </summary>
    /// <param name="APixelWidth">
    /// Integer value
    /// </param>
    procedure SetScrollWidth(APixelWidth: Integer);
    /// <summary>
    /// Retrieve the document width assumed for scrolling.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetScrollWidth(): Integer;
    /// <summary>
    /// Sets whether the maximum width line displayed is used to set scroll width.
    /// </summary>
    /// <param name="ATracking">
    /// Boolean value
    /// </param>
    procedure SetScrollWidthTracking(ATracking: Boolean);
    /// <summary>
    /// Retrieve whether the scroll width tracks wide lines.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetScrollWidthTracking(): Boolean;
    /// <summary>
    /// Measure the pixel width of some text in a particular style.
    /// NUL terminated text argument.
    /// Does not handle tab or control characters.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function TextWidth(AStyle: Integer; AText: PAnsiChar): Integer;
    /// <summary>
    /// Sets the scroll range so that maximum scroll position has
    /// the last line at the bottom of the view (default).
    /// Setting this to false allows scrolling one page below the last line.
    /// </summary>
    /// <param name="AEndAtLastLine">
    /// Boolean value
    /// </param>
    procedure SetEndAtLastLine(AEndAtLastLine: Boolean);
    /// <summary>
    /// Retrieve whether the maximum scroll position has the last
    /// line at the bottom of the view.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetEndAtLastLine(): Boolean;
    /// <summary>
    /// Retrieve the height of a particular line of text in pixels.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function TextHeight(ALine: TSciLine): Integer;
    /// <summary>
    /// Show or hide the vertical scroll bar.
    /// </summary>
    /// <param name="AVisible">
    /// Boolean value
    /// </param>
    procedure SetVScrollBar(AVisible: Boolean);
    /// <summary>
    /// Is the vertical scroll bar visible?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetVScrollBar(): Boolean;
    /// <summary>
    /// Append a string to the end of the document without changing the selection.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure AppendText(ALength: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// How many phases is drawing done in?
    /// </summary>
    /// <returns>
    /// Returns the phasesdraw
    /// </returns>
    function GetPhasesDraw(): NativeInt;
    /// <summary>
    /// In one phase draw, text is drawn in a series of rectangular blocks with no overlap.
    /// In two phase draw, text is drawn in a series of lines allowing runs to overlap horizontally.
    /// In multiple phase draw, each element is drawn over the whole drawing area, allowing text
    /// to overlap from one line to the next.
    /// </summary>
    /// <param name="APhases">
    /// The APhases parameter
    /// </param>
    procedure SetPhasesDraw(APhases: NativeInt);
    /// <summary>
    /// Choose the quality level for text from the FontQuality enumeration.
    /// </summary>
    /// <param name="AFontQuality">
    /// The AFontQuality parameter
    /// </param>
    procedure SetFontQuality(AFontQuality: NativeInt);
    /// <summary>
    /// Retrieve the quality level for text.
    /// </summary>
    /// <returns>
    /// Returns the fontquality
    /// </returns>
    function GetFontQuality(): NativeInt;
    /// <summary>
    /// Scroll so that a display line is at the top of the display.
    /// </summary>
    /// <param name="ADisplayLine">
    /// Line number
    /// </param>
    procedure SetFirstVisibleLine(ADisplayLine: TSciLine);
    /// <summary>
    /// Change the effect of pasting when there are multiple selections.
    /// </summary>
    /// <param name="AMultiPaste">
    /// The AMultiPaste parameter
    /// </param>
    procedure SetMultiPaste(AMultiPaste: NativeInt);
    /// <summary>
    /// Retrieve the effect of pasting when there are multiple selections.
    /// </summary>
    /// <returns>
    /// Returns the multipaste
    /// </returns>
    function GetMultiPaste(): NativeInt;
    /// <summary>
    /// Retrieve the value of a tag from a regular expression search.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="ATagNumber">
    /// Integer value
    /// </param>
    /// <param name="ATagValue">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetTag(ATagNumber: Integer; ATagValue: PAnsiChar): Integer;
    /// <summary>
    /// Join the lines in the target.
    /// </summary>
    procedure LinesJoin();
    /// <summary>
    /// Split the lines in the target into lines that are less wide than pixelWidth
    /// where possible.
    /// </summary>
    /// <param name="APixelWidth">
    /// Integer value
    /// </param>
    procedure LinesSplit(APixelWidth: Integer);
    /// <summary>
    /// Set one of the colours used as a chequerboard pattern in the fold margin
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetFoldMarginColour(AUseSetting: Boolean; ABack: TColor);
    /// <summary>
    /// Set the other colour used as a chequerboard pattern in the fold margin
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetFoldMarginHiColour(AUseSetting: Boolean; AFore: TColor);
    /// <summary>
    /// Enable or disable accessibility.
    /// </summary>
    /// <param name="AAccessibility">
    /// The AAccessibility parameter
    /// </param>
    procedure SetAccessibility(AAccessibility: NativeInt);
    /// <summary>
    /// Report accessibility status.
    /// </summary>
    /// <returns>
    /// Returns the accessibility
    /// </returns>
    function GetAccessibility(): NativeInt;
    /// <summary>
    /// Move caret down one line.
    /// </summary>
    procedure LineDown();
    /// <summary>
    /// Move caret down one line extending selection to new caret position.
    /// </summary>
    procedure LineDownExtend();
    /// <summary>
    /// Move caret up one line.
    /// </summary>
    procedure LineUp();
    /// <summary>
    /// Move caret up one line extending selection to new caret position.
    /// </summary>
    procedure LineUpExtend();
    /// <summary>
    /// Move caret left one character.
    /// </summary>
    procedure CharLeft();
    /// <summary>
    /// Move caret left one character extending selection to new caret position.
    /// </summary>
    procedure CharLeftExtend();
    /// <summary>
    /// Move caret right one character.
    /// </summary>
    procedure CharRight();
    /// <summary>
    /// Move caret right one character extending selection to new caret position.
    /// </summary>
    procedure CharRightExtend();
    /// <summary>
    /// Move caret left one word.
    /// </summary>
    procedure WordLeft();
    /// <summary>
    /// Move caret left one word extending selection to new caret position.
    /// </summary>
    procedure WordLeftExtend();
    /// <summary>
    /// Move caret right one word.
    /// </summary>
    procedure WordRight();
    /// <summary>
    /// Move caret right one word extending selection to new caret position.
    /// </summary>
    procedure WordRightExtend();
    /// <summary>
    /// Move caret to first position on line.
    /// </summary>
    procedure Home();
    /// <summary>
    /// Move caret to first position on line extending selection to new caret position.
    /// </summary>
    procedure HomeExtend();
    /// <summary>
    /// Move caret to last position on line.
    /// </summary>
    procedure LineEnd();
    /// <summary>
    /// Move caret to last position on line extending selection to new caret position.
    /// </summary>
    procedure LineEndExtend();
    /// <summary>
    /// Move caret to first position in document.
    /// </summary>
    procedure DocumentStart();
    /// <summary>
    /// Move caret to first position in document extending selection to new caret position.
    /// </summary>
    procedure DocumentStartExtend();
    /// <summary>
    /// Move caret to last position in document.
    /// </summary>
    procedure DocumentEnd();
    /// <summary>
    /// Move caret to last position in document extending selection to new caret position.
    /// </summary>
    procedure DocumentEndExtend();
    /// <summary>
    /// Move caret one page up.
    /// </summary>
    procedure PageUp();
    /// <summary>
    /// Move caret one page up extending selection to new caret position.
    /// </summary>
    procedure PageUpExtend();
    /// <summary>
    /// Move caret one page down.
    /// </summary>
    procedure PageDown();
    /// <summary>
    /// Move caret one page down extending selection to new caret position.
    /// </summary>
    procedure PageDownExtend();
    /// <summary>
    /// Switch from insert to overtype mode or the reverse.
    /// </summary>
    procedure EditToggleOvertype();
    /// <summary>
    /// Cancel any modes such as call tip or auto-completion list display.
    /// </summary>
    procedure Cancel();
    /// <summary>
    /// Delete the selection or if no selection, the character before the caret.
    /// </summary>
    procedure DeleteBack();
    /// <summary>
    /// If selection is empty or all on one line replace the selection with a tab character.
    /// If more than one line selected, indent the lines.
    /// </summary>
    procedure Tab();
    /// <summary>
    /// Indent the current and selected lines.
    /// </summary>
    procedure LineIndent();
    /// <summary>
    /// If selection is empty or all on one line dedent the line if caret is at start, else move caret.
    /// If more than one line selected, dedent the lines.
    /// </summary>
    procedure BackTab();
    /// <summary>
    /// Dedent the current and selected lines.
    /// </summary>
    procedure LineDedent();
    /// <summary>
    /// Insert a new line, may use a CRLF, CR or LF depending on EOL mode.
    /// </summary>
    procedure NewLine();
    /// <summary>
    /// Insert a Form Feed character.
    /// </summary>
    procedure FormFeed();
    /// <summary>
    /// Move caret to before first visible character on line.
    /// If already there move to first character on line.
    /// </summary>
    procedure VCHome();
    /// <summary>
    /// Like VCHome but extending selection to new caret position.
    /// </summary>
    procedure VCHomeExtend();
    /// <summary>
    /// Magnify the displayed text by increasing the sizes by 1 point.
    /// </summary>
    procedure ZoomIn();
    /// <summary>
    /// Make the displayed text smaller by decreasing the sizes by 1 point.
    /// </summary>
    procedure ZoomOut();
    /// <summary>
    /// Delete the word to the left of the caret.
    /// </summary>
    procedure DelWordLeft();
    /// <summary>
    /// Delete the word to the right of the caret.
    /// </summary>
    procedure DelWordRight();
    /// <summary>
    /// Delete the word to the right of the caret, but not the trailing non-word characters.
    /// </summary>
    procedure DelWordRightEnd();
    /// <summary>
    /// Cut the line containing the caret.
    /// </summary>
    procedure LineCut();
    /// <summary>
    /// Delete the line containing the caret.
    /// </summary>
    procedure LineDelete();
    /// <summary>
    /// Switch the current line with the previous.
    /// </summary>
    procedure LineTranspose();
    /// <summary>
    /// Reverse order of selected lines.
    /// </summary>
    procedure LineReverse();
    /// <summary>
    /// Duplicate the current line.
    /// </summary>
    procedure LineDuplicate();
    /// <summary>
    /// Transform the selection to lower case.
    /// </summary>
    procedure LowerCase();
    /// <summary>
    /// Transform the selection to upper case.
    /// </summary>
    procedure UpperCase();
    /// <summary>
    /// Scroll the document down, keeping the caret visible.
    /// </summary>
    procedure LineScrollDown();
    /// <summary>
    /// Scroll the document up, keeping the caret visible.
    /// </summary>
    procedure LineScrollUp();
    /// <summary>
    /// Delete the selection or if no selection, the character before the caret.
    /// Will not delete the character before at the start of a line.
    /// </summary>
    procedure DeleteBackNotLine();
    /// <summary>
    /// Move caret to first position on display line.
    /// </summary>
    procedure HomeDisplay();
    /// <summary>
    /// Move caret to first position on display line extending selection to
    /// new caret position.
    /// </summary>
    procedure HomeDisplayExtend();
    /// <summary>
    /// Move caret to last position on display line.
    /// </summary>
    procedure LineEndDisplay();
    /// <summary>
    /// Move caret to last position on display line extending selection to new
    /// caret position.
    /// </summary>
    procedure LineEndDisplayExtend();
    /// <summary>
    /// Like Home but when word-wrap is enabled goes first to start of display line
    /// HomeDisplay, then to start of document line Home.
    /// </summary>
    procedure HomeWrap();
    /// <summary>
    /// Like HomeExtend but when word-wrap is enabled extends first to start of display line
    /// HomeDisplayExtend, then to start of document line HomeExtend.
    /// </summary>
    procedure HomeWrapExtend();
    /// <summary>
    /// Like LineEnd but when word-wrap is enabled goes first to end of display line
    /// LineEndDisplay, then to start of document line LineEnd.
    /// </summary>
    procedure LineEndWrap();
    /// <summary>
    /// Like LineEndExtend but when word-wrap is enabled extends first to end of display line
    /// LineEndDisplayExtend, then to start of document line LineEndExtend.
    /// </summary>
    procedure LineEndWrapExtend();
    /// <summary>
    /// Like VCHome but when word-wrap is enabled goes first to start of display line
    /// VCHomeDisplay, then behaves like VCHome.
    /// </summary>
    procedure VCHomeWrap();
    /// <summary>
    /// Like VCHomeExtend but when word-wrap is enabled extends first to start of display line
    /// VCHomeDisplayExtend, then behaves like VCHomeExtend.
    /// </summary>
    procedure VCHomeWrapExtend();
    /// <summary>
    /// Copy the line containing the caret.
    /// </summary>
    procedure LineCopy();
    /// <summary>
    /// Move the caret inside current view if it's not there already.
    /// </summary>
    procedure MoveCaretInsideView();
    /// <summary>
    /// How many characters are on a line, including end of line characters?
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function LineLength(ALine: TSciLine): TSciPosition;
    /// <summary>
    /// Highlight the characters at two positions.
    /// </summary>
    /// <param name="APosA">
    /// Position in the document
    /// </param>
    /// <param name="APosB">
    /// Position in the document
    /// </param>
    procedure BraceHighlight(APosA: TSciPosition; APosB: TSciPosition);
    /// <summary>
    /// Use specified indicator to highlight matching braces instead of changing their style.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    procedure BraceHighlightIndicator(AUseSetting: Boolean; AIndicator: Integer);
    /// <summary>
    /// Highlight the character at a position indicating there is no matching brace.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    procedure BraceBadLight(APos: TSciPosition);
    /// <summary>
    /// Use specified indicator to highlight non matching brace instead of changing its style.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    procedure BraceBadLightIndicator(AUseSetting: Boolean; AIndicator: Integer);
    /// <summary>
    /// Find the position of a matching brace or INVALID_POSITION if no match.
    /// The maxReStyle must be 0 for now. It may be defined in a future release.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="AMaxReStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function BraceMatch(APos: TSciPosition; AMaxReStyle: Integer): TSciPosition;
    /// <summary>
    /// Similar to BraceMatch, but matching starts at the explicit start position.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="AStartPos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function BraceMatchNext(APos: TSciPosition; AStartPos: TSciPosition): TSciPosition;
    /// <summary>
    /// Are the end of line characters visible?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetViewEOL(): Boolean;
    /// <summary>
    /// Make the end of line characters visible or invisible.
    /// </summary>
    /// <param name="AVisible">
    /// Boolean value
    /// </param>
    procedure SetViewEOL(AVisible: Boolean);
    /// <summary>
    /// Retrieve a pointer to the document object.
    /// </summary>
    /// <returns>
    /// Returns the docpointer
    /// </returns>
    function GetDocPointer(): Pointer;
    /// <summary>
    /// Change the document object used.
    /// </summary>
    /// <param name="ADoc">
    /// The ADoc parameter
    /// </param>
    procedure SetDocPointer(ADoc: Pointer);
    /// <summary>
    /// Set which document modification events are sent to the container.
    /// </summary>
    /// <param name="AEventMask">
    /// The AEventMask parameter
    /// </param>
    procedure SetModEventMask(AEventMask: NativeInt);
    /// <summary>
    /// Retrieve the column number which text should be kept within.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetEdgeColumn(): TSciPosition;
    /// <summary>
    /// Set the column number of the edge.
    /// If text goes past the edge then it is highlighted.
    /// </summary>
    /// <param name="AColumn">
    /// Position in the document
    /// </param>
    procedure SetEdgeColumn(AColumn: TSciPosition);
    /// <summary>
    /// Retrieve the edge highlight mode.
    /// </summary>
    /// <returns>
    /// Returns the edgemode
    /// </returns>
    function GetEdgeMode(): NativeInt;
    /// <summary>
    /// The edge may be displayed by a line (EDGE_LINE/EDGE_MULTILINE) or by highlighting text that
    /// goes beyond it (EDGE_BACKGROUND) or not displayed at all (EDGE_NONE).
    /// </summary>
    /// <param name="AEdgeMode">
    /// The AEdgeMode parameter
    /// </param>
    procedure SetEdgeMode(AEdgeMode: NativeInt);
    /// <summary>
    /// Retrieve the colour used in edge indication.
    /// </summary>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetEdgeColour(): TColor;
    /// <summary>
    /// Change the colour used in edge indication.
    /// </summary>
    /// <param name="AEdgeColour">
    /// Color value
    /// </param>
    procedure SetEdgeColour(AEdgeColour: TColor);
    /// <summary>
    /// Add a new vertical edge to the view.
    /// </summary>
    /// <param name="AColumn">
    /// Position in the document
    /// </param>
    /// <param name="AEdgeColour">
    /// Color value
    /// </param>
    procedure MultiEdgeAddLine(AColumn: TSciPosition; AEdgeColour: TColor);
    /// <summary>
    /// Clear all vertical edges.
    /// </summary>
    procedure MultiEdgeClearAll();
    /// <summary>
    /// Get multi edge positions.
    /// </summary>
    /// <param name="AWhich">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetMultiEdgeColumn(AWhich: Integer): TSciPosition;
    /// <summary>
    /// Sets the current caret position to be the search anchor.
    /// </summary>
    procedure SearchAnchor();
    /// <summary>
    /// Find some text starting at the search anchor.
    /// Does not ensure the selection is visible.
    /// </summary>
    /// <param name="ASearchFlags">
    /// The ASearchFlags parameter
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function SearchNext(ASearchFlags: NativeInt; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Find some text starting at the search anchor and moving backwards.
    /// Does not ensure the selection is visible.
    /// </summary>
    /// <param name="ASearchFlags">
    /// The ASearchFlags parameter
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function SearchPrev(ASearchFlags: NativeInt; AText: PAnsiChar): TSciPosition;
    /// <summary>
    /// Retrieves the number of lines completely visible.
    /// </summary>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function LinesOnScreen(): TSciLine;
    /// <summary>
    /// Set whether a pop up menu is displayed automatically when the user presses
    /// the wrong mouse button on certain areas.
    /// </summary>
    /// <param name="APopUpMode">
    /// The APopUpMode parameter
    /// </param>
    procedure UsePopUp(APopUpMode: NativeInt);
    /// <summary>
    /// Is the selection rectangular? The alternative is the more common stream selection.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function SelectionIsRectangle(): Boolean;
    /// <summary>
    /// Set the zoom level. This number of points is added to the size of all fonts.
    /// It may be positive to magnify or negative to reduce.
    /// </summary>
    /// <param name="AZoomInPoints">
    /// Integer value
    /// </param>
    procedure SetZoom(AZoomInPoints: Integer);
    /// <summary>
    /// Retrieve the zoom level.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetZoom(): Integer;
    /// <summary>
    /// Create a new document object.
    /// Starts with reference count of 1 and not selected into editor.
    /// </summary>
    /// <param name="ABytes">
    /// Position in the document
    /// </param>
    /// <param name="ADocumentOptions">
    /// The ADocumentOptions parameter
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function CreateDocument(ABytes: TSciPosition; ADocumentOptions: NativeInt): Pointer;
    /// <summary>
    /// Extend life of document.
    /// </summary>
    /// <param name="ADoc">
    /// The ADoc parameter
    /// </param>
    procedure AddRefDocument(ADoc: Pointer);
    /// <summary>
    /// Release a reference to the document, deleting document if it fades to black.
    /// </summary>
    /// <param name="ADoc">
    /// The ADoc parameter
    /// </param>
    procedure ReleaseDocument(ADoc: Pointer);
    /// <summary>
    /// Get which document options are set.
    /// </summary>
    /// <returns>
    /// Returns the documentoptions
    /// </returns>
    function GetDocumentOptions(): NativeInt;
    /// <summary>
    /// Get which document modification events are sent to the container.
    /// </summary>
    /// <returns>
    /// Returns the modeventmask
    /// </returns>
    function GetModEventMask(): NativeInt;
    /// <summary>
    /// Set whether command events are sent to the container.
    /// </summary>
    /// <param name="ACommandEvents">
    /// Boolean value
    /// </param>
    procedure SetCommandEvents(ACommandEvents: Boolean);
    /// <summary>
    /// Get whether command events are sent to the container.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetCommandEvents(): Boolean;
    /// <summary>
    /// Change internal focus flag.
    /// </summary>
    /// <param name="AFocus">
    /// Boolean value
    /// </param>
    procedure SetFocus(AFocus: Boolean);
    /// <summary>
    /// Get internal focus flag.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetFocus(): Boolean;
    /// <summary>
    /// Change error status - 0 = OK.
    /// </summary>
    /// <param name="AStatus">
    /// The AStatus parameter
    /// </param>
    procedure SetStatus(AStatus: NativeInt);
    /// <summary>
    /// Get error status.
    /// </summary>
    /// <returns>
    /// Returns the status
    /// </returns>
    function GetStatus(): NativeInt;
    /// <summary>
    /// Set whether the mouse is captured when its button is pressed.
    /// </summary>
    /// <param name="ACaptures">
    /// Boolean value
    /// </param>
    procedure SetMouseDownCaptures(ACaptures: Boolean);
    /// <summary>
    /// Get whether mouse gets captured.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMouseDownCaptures(): Boolean;
    /// <summary>
    /// Set whether the mouse wheel can be active outside the window.
    /// </summary>
    /// <param name="ACaptures">
    /// Boolean value
    /// </param>
    procedure SetMouseWheelCaptures(ACaptures: Boolean);
    /// <summary>
    /// Get whether mouse wheel can be active outside the window.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMouseWheelCaptures(): Boolean;
    /// <summary>
    /// Sets the cursor to one of the SC_CURSOR* values.
    /// </summary>
    /// <param name="ACursorType">
    /// The ACursorType parameter
    /// </param>
    procedure SetCursor(ACursorType: NativeInt);
    /// <summary>
    /// Get cursor type.
    /// </summary>
    /// <returns>
    /// Returns the cursor
    /// </returns>
    function GetCursor(): NativeInt;
    /// <summary>
    /// Change the way control characters are displayed:
    /// If symbol is < 32, keep the drawn way, else, use the given character.
    /// </summary>
    /// <param name="ASymbol">
    /// Integer value
    /// </param>
    procedure SetControlCharSymbol(ASymbol: Integer);
    /// <summary>
    /// Get the way control characters are displayed.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetControlCharSymbol(): Integer;
    /// <summary>
    /// Move to the previous change in capitalisation.
    /// </summary>
    procedure WordPartLeft();
    /// <summary>
    /// Move to the previous change in capitalisation extending selection
    /// to new caret position.
    /// </summary>
    procedure WordPartLeftExtend();
    /// <summary>
    /// Move to the change next in capitalisation.
    /// </summary>
    procedure WordPartRight();
    /// <summary>
    /// Move to the next change in capitalisation extending selection
    /// to new caret position.
    /// </summary>
    procedure WordPartRightExtend();
    /// <summary>
    /// Set the way the display area is determined when a particular line
    /// is to be moved to by Find, FindNext, GotoLine, etc.
    /// </summary>
    /// <param name="AVisiblePolicy">
    /// The AVisiblePolicy parameter
    /// </param>
    /// <param name="AVisibleSlop">
    /// Integer value
    /// </param>
    procedure SetVisiblePolicy(AVisiblePolicy: NativeInt; AVisibleSlop: Integer);
    /// <summary>
    /// Delete back from the current position to the start of the line.
    /// </summary>
    procedure DelLineLeft();
    /// <summary>
    /// Delete forwards from the current position to the end of the line.
    /// </summary>
    procedure DelLineRight();
    /// <summary>
    /// Set the xOffset (ie, horizontal scroll position).
    /// </summary>
    /// <param name="AXOffset">
    /// Integer value
    /// </param>
    procedure SetXOffset(AXOffset: Integer);
    /// <summary>
    /// Get the xOffset (ie, horizontal scroll position).
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetXOffset(): Integer;
    /// <summary>
    /// Set the last x chosen value to be the caret x position.
    /// </summary>
    procedure ChooseCaretX();
    /// <summary>
    /// Set the focus to this Scintilla widget.
    /// </summary>
    procedure GrabFocus();
    /// <summary>
    /// Set the way the caret is kept visible when going sideways.
    /// The exclusion zone is given in pixels.
    /// </summary>
    /// <param name="ACaretPolicy">
    /// The ACaretPolicy parameter
    /// </param>
    /// <param name="ACaretSlop">
    /// Integer value
    /// </param>
    procedure SetXCaretPolicy(ACaretPolicy: NativeInt; ACaretSlop: Integer);
    /// <summary>
    /// Set the way the line the caret is on is kept visible.
    /// The exclusion zone is given in lines.
    /// </summary>
    /// <param name="ACaretPolicy">
    /// The ACaretPolicy parameter
    /// </param>
    /// <param name="ACaretSlop">
    /// Integer value
    /// </param>
    procedure SetYCaretPolicy(ACaretPolicy: NativeInt; ACaretSlop: Integer);
    /// <summary>
    /// Set printing to line wrapped (SC_WRAP_WORD) or not line wrapped (SC_WRAP_NONE).
    /// </summary>
    /// <param name="AWrapMode">
    /// The AWrapMode parameter
    /// </param>
    procedure SetPrintWrapMode(AWrapMode: NativeInt);
    /// <summary>
    /// Is printing line wrapped?
    /// </summary>
    /// <returns>
    /// Returns the printwrapmode
    /// </returns>
    function GetPrintWrapMode(): NativeInt;
    /// <summary>
    /// Set a fore colour for active hotspots.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetHotspotActiveFore(AUseSetting: Boolean; AFore: TColor);
    /// <summary>
    /// Get the fore colour for active hotspots.
    /// </summary>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetHotspotActiveFore(): TColor;
    /// <summary>
    /// Set a back colour for active hotspots.
    /// </summary>
    /// <param name="AUseSetting">
    /// Boolean value
    /// </param>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetHotspotActiveBack(AUseSetting: Boolean; ABack: TColor);
    /// <summary>
    /// Get the back colour for active hotspots.
    /// </summary>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetHotspotActiveBack(): TColor;
    /// <summary>
    /// Enable / Disable underlining active hotspots.
    /// </summary>
    /// <param name="AUnderline">
    /// Boolean value
    /// </param>
    procedure SetHotspotActiveUnderline(AUnderline: Boolean);
    /// <summary>
    /// Get whether underlining for active hotspots.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetHotspotActiveUnderline(): Boolean;
    /// <summary>
    /// Limit hotspots to single line so hotspots on two lines don't merge.
    /// </summary>
    /// <param name="ASingleLine">
    /// Boolean value
    /// </param>
    procedure SetHotspotSingleLine(ASingleLine: Boolean);
    /// <summary>
    /// Get the HotspotSingleLine property
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetHotspotSingleLine(): Boolean;
    /// <summary>
    /// Move caret down one paragraph (delimited by empty lines).
    /// </summary>
    procedure ParaDown();
    /// <summary>
    /// Extend selection down one paragraph (delimited by empty lines).
    /// </summary>
    procedure ParaDownExtend();
    /// <summary>
    /// Move caret up one paragraph (delimited by empty lines).
    /// </summary>
    procedure ParaUp();
    /// <summary>
    /// Extend selection up one paragraph (delimited by empty lines).
    /// </summary>
    procedure ParaUpExtend();
    /// <summary>
    /// Given a valid document position, return the previous position taking code
    /// page into account. Returns 0 if passed 0.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionBefore(APos: TSciPosition): TSciPosition;
    /// <summary>
    /// Given a valid document position, return the next position taking code
    /// page into account. Maximum value returned is the last position in the document.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionAfter(APos: TSciPosition): TSciPosition;
    /// <summary>
    /// Given a valid document position, return a position that differs in a number
    /// of characters. Returned value is always between 0 and last position in document.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="ARelative">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionRelative(APos: TSciPosition; ARelative: TSciPosition): TSciPosition;
    /// <summary>
    /// Given a valid document position, return a position that differs in a number
    /// of UTF-16 code units. Returned value is always between 0 and last position in document.
    /// The result may point half way (2 bytes) inside a non-BMP character.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="ARelative">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionRelativeCodeUnits(APos: TSciPosition; ARelative: TSciPosition): TSciPosition;
    /// <summary>
    /// Copy a range of text to the clipboard. Positions are clipped into the document.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    procedure CopyRange(AStart: TSciPosition; AEnd: TSciPosition);
    /// <summary>
    /// Copy argument text to the clipboard.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure CopyText(ALength: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// Set the selection mode to stream (SC_SEL_STREAM) or rectangular (SC_SEL_RECTANGLE/SC_SEL_THIN) or
    /// by lines (SC_SEL_LINES).
    /// </summary>
    /// <param name="ASelectionMode">
    /// The ASelectionMode parameter
    /// </param>
    procedure SetSelectionMode(ASelectionMode: NativeInt);
    /// <summary>
    /// Set the selection mode to stream (SC_SEL_STREAM) or rectangular (SC_SEL_RECTANGLE/SC_SEL_THIN) or
    /// by lines (SC_SEL_LINES) without changing MoveExtendsSelection.
    /// </summary>
    /// <param name="ASelectionMode">
    /// The ASelectionMode parameter
    /// </param>
    procedure ChangeSelectionMode(ASelectionMode: NativeInt);
    /// <summary>
    /// Get the mode of the current selection.
    /// </summary>
    /// <returns>
    /// Returns the selectionmode
    /// </returns>
    function GetSelectionMode(): NativeInt;
    /// <summary>
    /// Set whether or not regular caret moves will extend or reduce the selection.
    /// </summary>
    /// <param name="AMoveExtendsSelection">
    /// Boolean value
    /// </param>
    procedure SetMoveExtendsSelection(AMoveExtendsSelection: Boolean);
    /// <summary>
    /// Get whether or not regular caret moves will extend or reduce the selection.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMoveExtendsSelection(): Boolean;
    /// <summary>
    /// Retrieve the position of the start of the selection at the given line (INVALID_POSITION if no selection on this line).
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineSelStartPosition(ALine: TSciLine): TSciPosition;
    /// <summary>
    /// Retrieve the position of the end of the selection at the given line (INVALID_POSITION if no selection on this line).
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineSelEndPosition(ALine: TSciLine): TSciPosition;
    /// <summary>
    /// Move caret down one line, extending rectangular selection to new caret position.
    /// </summary>
    procedure LineDownRectExtend();
    /// <summary>
    /// Move caret up one line, extending rectangular selection to new caret position.
    /// </summary>
    procedure LineUpRectExtend();
    /// <summary>
    /// Move caret left one character, extending rectangular selection to new caret position.
    /// </summary>
    procedure CharLeftRectExtend();
    /// <summary>
    /// Move caret right one character, extending rectangular selection to new caret position.
    /// </summary>
    procedure CharRightRectExtend();
    /// <summary>
    /// Move caret to first position on line, extending rectangular selection to new caret position.
    /// </summary>
    procedure HomeRectExtend();
    /// <summary>
    /// Move caret to before first visible character on line.
    /// If already there move to first character on line.
    /// In either case, extend rectangular selection to new caret position.
    /// </summary>
    procedure VCHomeRectExtend();
    /// <summary>
    /// Move caret to last position on line, extending rectangular selection to new caret position.
    /// </summary>
    procedure LineEndRectExtend();
    /// <summary>
    /// Move caret one page up, extending rectangular selection to new caret position.
    /// </summary>
    procedure PageUpRectExtend();
    /// <summary>
    /// Move caret one page down, extending rectangular selection to new caret position.
    /// </summary>
    procedure PageDownRectExtend();
    /// <summary>
    /// Move caret to top of page, or one page up if already at top of page.
    /// </summary>
    procedure StutteredPageUp();
    /// <summary>
    /// Move caret to top of page, or one page up if already at top of page, extending selection to new caret position.
    /// </summary>
    procedure StutteredPageUpExtend();
    /// <summary>
    /// Move caret to bottom of page, or one page down if already at bottom of page.
    /// </summary>
    procedure StutteredPageDown();
    /// <summary>
    /// Move caret to bottom of page, or one page down if already at bottom of page, extending selection to new caret position.
    /// </summary>
    procedure StutteredPageDownExtend();
    /// <summary>
    /// Move caret left one word, position cursor at end of word.
    /// </summary>
    procedure WordLeftEnd();
    /// <summary>
    /// Move caret left one word, position cursor at end of word, extending selection to new caret position.
    /// </summary>
    procedure WordLeftEndExtend();
    /// <summary>
    /// Move caret right one word, position cursor at end of word.
    /// </summary>
    procedure WordRightEnd();
    /// <summary>
    /// Move caret right one word, position cursor at end of word, extending selection to new caret position.
    /// </summary>
    procedure WordRightEndExtend();
    /// <summary>
    /// Set the set of characters making up whitespace for when moving or selecting by word.
    /// Should be called after SetWordChars.
    /// </summary>
    /// <param name="ACharacters">
    /// Text string
    /// </param>
    procedure SetWhitespaceChars(ACharacters: PAnsiChar);
    /// <summary>
    /// Get the set of characters making up whitespace for when moving or selecting by word.
    /// </summary>
    /// <param name="ACharacters">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetWhitespaceChars(ACharacters: PAnsiChar): Integer;
    /// <summary>
    /// Set the set of characters making up punctuation characters
    /// Should be called after SetWordChars.
    /// </summary>
    /// <param name="ACharacters">
    /// Text string
    /// </param>
    procedure SetPunctuationChars(ACharacters: PAnsiChar);
    /// <summary>
    /// Get the set of characters making up punctuation characters
    /// </summary>
    /// <param name="ACharacters">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPunctuationChars(ACharacters: PAnsiChar): Integer;
    /// <summary>
    /// Reset the set of characters for whitespace and word characters to the defaults.
    /// </summary>
    procedure SetCharsDefault();
    /// <summary>
    /// Get currently selected item position in the auto-completion list
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetCurrent(): Integer;
    /// <summary>
    /// Get currently selected item text in the auto-completion list
    /// Returns the length of the item text
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetCurrentText(AText: PAnsiChar): Integer;
    /// <summary>
    /// Set auto-completion case insensitive behaviour to either prefer case-sensitive matches or have no preference.
    /// </summary>
    /// <param name="ABehaviour">
    /// The ABehaviour parameter
    /// </param>
    procedure AutoCSetCaseInsensitiveBehaviour(ABehaviour: NativeInt);
    /// <summary>
    /// Get auto-completion case insensitive behaviour.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetCaseInsensitiveBehaviour(): NativeInt;
    /// <summary>
    /// Change the effect of autocompleting when there are multiple selections.
    /// </summary>
    /// <param name="AMulti">
    /// The AMulti parameter
    /// </param>
    procedure AutoCSetMulti(AMulti: NativeInt);
    /// <summary>
    /// Retrieve the effect of autocompleting when there are multiple selections.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetMulti(): NativeInt;
    /// <summary>
    /// Set the way autocompletion lists are ordered.
    /// </summary>
    /// <param name="AOrder">
    /// The AOrder parameter
    /// </param>
    procedure AutoCSetOrder(AOrder: NativeInt);
    /// <summary>
    /// Get the way autocompletion lists are ordered.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetOrder(): NativeInt;
    /// <summary>
    /// Enlarge the document to a particular size of text bytes.
    /// </summary>
    /// <param name="ABytes">
    /// Position in the document
    /// </param>
    procedure Allocate(ABytes: TSciPosition);
    /// <summary>
    /// Returns the target converted to UTF8.
    /// Return the length in bytes.
    /// </summary>
    /// <param name="s">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function TargetAsUTF8(s: PAnsiChar): TSciPosition;
    /// <summary>
    /// Set the length of the utf8 argument for calling EncodedFromUTF8.
    /// Set to -1 and the string will be measured to the first nul.
    /// </summary>
    /// <param name="ABytes">
    /// Position in the document
    /// </param>
    procedure SetLengthForEncode(ABytes: TSciPosition);
    /// <summary>
    /// Translates a UTF8 string into the document encoding.
    /// Return the length of the result in bytes.
    /// On error return 0.
    /// </summary>
    /// <param name="AUtf8">
    /// Text string
    /// </param>
    /// <param name="AEncoded">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function EncodedFromUTF8(AUtf8: PAnsiChar; AEncoded: PAnsiChar): TSciPosition;
    /// <summary>
    /// Find the position of a column on a line taking into account tabs and
    /// multi-byte characters. If beyond end of line, return line end position.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AColumn">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FindColumn(ALine: TSciLine; AColumn: TSciPosition): TSciPosition;
    /// <summary>
    /// Can the caret preferred x position only be changed by explicit movement commands?
    /// </summary>
    /// <returns>
    /// Returns the caretsticky
    /// </returns>
    function GetCaretSticky(): NativeInt;
    /// <summary>
    /// Stop the caret preferred x position changing when the user types.
    /// </summary>
    /// <param name="AUseCaretStickyBehaviour">
    /// The AUseCaretStickyBehaviour parameter
    /// </param>
    procedure SetCaretSticky(AUseCaretStickyBehaviour: NativeInt);
    /// <summary>
    /// Switch between sticky and non-sticky: meant to be bound to a key.
    /// </summary>
    procedure ToggleCaretSticky();
    /// <summary>
    /// Enable/Disable convert-on-paste for line endings
    /// </summary>
    /// <param name="AConvert">
    /// Boolean value
    /// </param>
    procedure SetPasteConvertEndings(AConvert: Boolean);
    /// <summary>
    /// Get convert-on-paste setting
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetPasteConvertEndings(): Boolean;
    /// <summary>
    /// Replace the selection with text like a rectangular paste.
    /// </summary>
    /// <param name="ALength">
    /// Position in the document
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure ReplaceRectangular(ALength: TSciPosition; AText: PAnsiChar);
    /// <summary>
    /// Duplicate the selection. If selection empty duplicate the line containing the caret.
    /// </summary>
    procedure SelectionDuplicate();
    /// <summary>
    /// Set background alpha of the caret line.
    /// </summary>
    /// <param name="AAlpha">
    /// The AAlpha parameter
    /// </param>
    procedure SetCaretLineBackAlpha(AAlpha: NativeInt);
    /// <summary>
    /// Get the background alpha of the caret line.
    /// </summary>
    /// <returns>
    /// Returns the caretlinebackalpha
    /// </returns>
    function GetCaretLineBackAlpha(): NativeInt;
    /// <summary>
    /// Set the style of the caret to be drawn.
    /// </summary>
    /// <param name="ACaretStyle">
    /// The ACaretStyle parameter
    /// </param>
    procedure SetCaretStyle(ACaretStyle: NativeInt);
    /// <summary>
    /// Returns the current style of the caret.
    /// </summary>
    /// <returns>
    /// Returns the caretstyle
    /// </returns>
    function GetCaretStyle(): NativeInt;
    /// <summary>
    /// Set the indicator used for IndicatorFillRange and IndicatorClearRange
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    procedure SetIndicatorCurrent(AIndicator: Integer);
    /// <summary>
    /// Get the current indicator
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetIndicatorCurrent(): Integer;
    /// <summary>
    /// Set the value used for IndicatorFillRange
    /// </summary>
    /// <param name="AValue">
    /// Integer value
    /// </param>
    procedure SetIndicatorValue(AValue: Integer);
    /// <summary>
    /// Get the current indicator value
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetIndicatorValue(): Integer;
    /// <summary>
    /// Turn a indicator on over a range.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="ALengthFill">
    /// Position in the document
    /// </param>
    procedure IndicatorFillRange(AStart: TSciPosition; ALengthFill: TSciPosition);
    /// <summary>
    /// Turn a indicator off over a range.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="ALengthClear">
    /// Position in the document
    /// </param>
    procedure IndicatorClearRange(AStart: TSciPosition; ALengthClear: TSciPosition);
    /// <summary>
    /// Are any indicators present at pos?
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function IndicatorAllOnFor(APos: TSciPosition): Integer;
    /// <summary>
    /// What value does a particular indicator have at a position?
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function IndicatorValueAt(AIndicator: Integer; APos: TSciPosition): Integer;
    /// <summary>
    /// Where does a particular indicator start?
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function IndicatorStart(AIndicator: Integer; APos: TSciPosition): TSciPosition;
    /// <summary>
    /// Where does a particular indicator end?
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function IndicatorEnd(AIndicator: Integer; APos: TSciPosition): TSciPosition;
    /// <summary>
    /// Set number of entries in position cache
    /// </summary>
    /// <param name="ASize">
    /// Integer value
    /// </param>
    procedure SetPositionCache(ASize: Integer);
    /// <summary>
    /// How many entries are allocated to the position cache?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPositionCache(): Integer;
    /// <summary>
    /// Set maximum number of threads used for layout
    /// </summary>
    /// <param name="AThreads">
    /// Integer value
    /// </param>
    procedure SetLayoutThreads(AThreads: Integer);
    /// <summary>
    /// Get maximum number of threads used for layout
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLayoutThreads(): Integer;
    /// <summary>
    /// Copy the selection, if selection empty copy the line with the caret
    /// </summary>
    procedure CopyAllowLine();
    /// <summary>
    /// Cut the selection, if selection empty cut the line with the caret
    /// </summary>
    procedure CutAllowLine();
    /// <summary>
    /// Set the string to separate parts when copying a multiple selection.
    /// </summary>
    /// <param name="ASeparator">
    /// Text string
    /// </param>
    procedure SetCopySeparator(ASeparator: PAnsiChar);
    /// <summary>
    /// Get the string to separate parts when copying a multiple selection.
    /// </summary>
    /// <param name="ASeparator">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCopySeparator(ASeparator: PAnsiChar): Integer;
    /// <summary>
    /// Compact the document buffer and return a read-only pointer to the
    /// characters in the document.
    /// </summary>
    /// <returns>
    /// Returns the characterpointer
    /// </returns>
    function GetCharacterPointer(): Pointer;
    /// <summary>
    /// Return a read-only pointer to a range of characters in the document.
    /// May move the gap so that the range is contiguous, but will only move up
    /// to lengthRange bytes.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="ALengthRange">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the rangepointer
    /// </returns>
    function GetRangePointer(AStart: TSciPosition; ALengthRange: TSciPosition): Pointer;
    /// <summary>
    /// Return a position which, to avoid performance costs, should not be within
    /// the range of a call to GetRangePointer.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetGapPosition(): TSciPosition;
    /// <summary>
    /// Set the alpha fill colour of the given indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AAlpha">
    /// The AAlpha parameter
    /// </param>
    procedure IndicSetAlpha(AIndicator: Integer; AAlpha: NativeInt);
    /// <summary>
    /// Get the alpha fill colour of the given indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetAlpha(AIndicator: Integer): NativeInt;
    /// <summary>
    /// Set the alpha outline colour of the given indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <param name="AAlpha">
    /// The AAlpha parameter
    /// </param>
    procedure IndicSetOutlineAlpha(AIndicator: Integer; AAlpha: NativeInt);
    /// <summary>
    /// Get the alpha outline colour of the given indicator.
    /// </summary>
    /// <param name="AIndicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetOutlineAlpha(AIndicator: Integer): NativeInt;
    /// <summary>
    /// Set extra ascent for each line
    /// </summary>
    /// <param name="AExtraAscent">
    /// Integer value
    /// </param>
    procedure SetExtraAscent(AExtraAscent: Integer);
    /// <summary>
    /// Get extra ascent for each line
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetExtraAscent(): Integer;
    /// <summary>
    /// Set extra descent for each line
    /// </summary>
    /// <param name="AExtraDescent">
    /// Integer value
    /// </param>
    procedure SetExtraDescent(AExtraDescent: Integer);
    /// <summary>
    /// Get extra descent for each line
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetExtraDescent(): Integer;
    /// <summary>
    /// Which symbol was defined for markerNumber with MarkerDefine
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function MarkerSymbolDefined(AMarkerNumber: Integer): NativeInt;
    /// <summary>
    /// Set the text in the text margin for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure MarginSetText(ALine: TSciLine; AText: PAnsiChar);
    /// <summary>
    /// Get the text in the text margin for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetText(ALine: TSciLine; AText: PAnsiChar): Integer;
    /// <summary>
    /// Set the style number for the text margin for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure MarginSetStyle(ALine: TSciLine; AStyle: Integer);
    /// <summary>
    /// Get the style number for the text margin for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetStyle(ALine: TSciLine): Integer;
    /// <summary>
    /// Set the style in the text margin for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyles">
    /// Text string
    /// </param>
    procedure MarginSetStyles(ALine: TSciLine; AStyles: PAnsiChar);
    /// <summary>
    /// Get the styles in the text margin for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyles">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetStyles(ALine: TSciLine; AStyles: PAnsiChar): Integer;
    /// <summary>
    /// Clear the margin text on all lines
    /// </summary>
    procedure MarginTextClearAll();
    /// <summary>
    /// Get the start of the range of style numbers used for margin text
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure MarginSetStyleOffset(AStyle: Integer);
    /// <summary>
    /// Get the start of the range of style numbers used for margin text
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetStyleOffset(): Integer;
    /// <summary>
    /// Set the margin options.
    /// </summary>
    /// <param name="AMarginOptions">
    /// The AMarginOptions parameter
    /// </param>
    procedure SetMarginOptions(AMarginOptions: NativeInt);
    /// <summary>
    /// Get the margin options.
    /// </summary>
    /// <returns>
    /// Returns the marginoptions
    /// </returns>
    function GetMarginOptions(): NativeInt;
    /// <summary>
    /// Set the annotation text for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure AnnotationSetText(ALine: TSciLine; AText: PAnsiChar);
    /// <summary>
    /// Get the annotation text for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetText(ALine: TSciLine; AText: PAnsiChar): Integer;
    /// <summary>
    /// Set the style number for the annotations for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure AnnotationSetStyle(ALine: TSciLine; AStyle: Integer);
    /// <summary>
    /// Get the style number for the annotations for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetStyle(ALine: TSciLine): Integer;
    /// <summary>
    /// Set the annotation styles for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyles">
    /// Text string
    /// </param>
    procedure AnnotationSetStyles(ALine: TSciLine; AStyles: PAnsiChar);
    /// <summary>
    /// Get the annotation styles for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyles">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetStyles(ALine: TSciLine; AStyles: PAnsiChar): Integer;
    /// <summary>
    /// Get the number of annotation lines for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetLines(ALine: TSciLine): Integer;
    /// <summary>
    /// Clear the annotations from all lines
    /// </summary>
    procedure AnnotationClearAll();
    /// <summary>
    /// Set the visibility for the annotations for a view
    /// </summary>
    /// <param name="AVisible">
    /// The AVisible parameter
    /// </param>
    procedure AnnotationSetVisible(AVisible: NativeInt);
    /// <summary>
    /// Get the visibility for the annotations for a view
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AnnotationGetVisible(): NativeInt;
    /// <summary>
    /// Get the start of the range of style numbers used for annotations
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure AnnotationSetStyleOffset(AStyle: Integer);
    /// <summary>
    /// Get the start of the range of style numbers used for annotations
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetStyleOffset(): Integer;
    /// <summary>
    /// Release all extended (>255) style numbers
    /// </summary>
    procedure ReleaseAllExtendedStyles();
    /// <summary>
    /// Allocate some extended (>255) style numbers and return the start of the range
    /// </summary>
    /// <param name="ANumberStyles">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AllocateExtendedStyles(ANumberStyles: Integer): Integer;
    /// <summary>
    /// Add a container action to the undo stack
    /// </summary>
    /// <param name="AToken">
    /// Integer value
    /// </param>
    /// <param name="AFlags">
    /// The AFlags parameter
    /// </param>
    procedure AddUndoAction(AToken: Integer; AFlags: NativeInt);
    /// <summary>
    /// Find the position of a character from a point within the window.
    /// </summary>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <param name="y">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CharPositionFromPoint(x: Integer; y: Integer): TSciPosition;
    /// <summary>
    /// Find the position of a character from a point within the window.
    /// Return INVALID_POSITION if not close to text.
    /// </summary>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <param name="y">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CharPositionFromPointClose(x: Integer; y: Integer): TSciPosition;
    /// <summary>
    /// Set whether switching to rectangular mode while selecting with the mouse is allowed.
    /// </summary>
    /// <param name="AMouseSelectionRectangularSwitch">
    /// Boolean value
    /// </param>
    procedure SetMouseSelectionRectangularSwitch(AMouseSelectionRectangularSwitch: Boolean);
    /// <summary>
    /// Whether switching to rectangular mode while selecting with the mouse is allowed.
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMouseSelectionRectangularSwitch(): Boolean;
    /// <summary>
    /// Set whether multiple selections can be made
    /// </summary>
    /// <param name="AMultipleSelection">
    /// Boolean value
    /// </param>
    procedure SetMultipleSelection(AMultipleSelection: Boolean);
    /// <summary>
    /// Whether multiple selections can be made
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMultipleSelection(): Boolean;
    /// <summary>
    /// Set whether typing can be performed into multiple selections
    /// </summary>
    /// <param name="AAdditionalSelectionTyping">
    /// Boolean value
    /// </param>
    procedure SetAdditionalSelectionTyping(AAdditionalSelectionTyping: Boolean);
    /// <summary>
    /// Whether typing can be performed into multiple selections
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetAdditionalSelectionTyping(): Boolean;
    /// <summary>
    /// Set whether additional carets will blink
    /// </summary>
    /// <param name="AAdditionalCaretsBlink">
    /// Boolean value
    /// </param>
    procedure SetAdditionalCaretsBlink(AAdditionalCaretsBlink: Boolean);
    /// <summary>
    /// Whether additional carets will blink
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetAdditionalCaretsBlink(): Boolean;
    /// <summary>
    /// Set whether additional carets are visible
    /// </summary>
    /// <param name="AAdditionalCaretsVisible">
    /// Boolean value
    /// </param>
    procedure SetAdditionalCaretsVisible(AAdditionalCaretsVisible: Boolean);
    /// <summary>
    /// Whether additional carets are visible
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetAdditionalCaretsVisible(): Boolean;
    /// <summary>
    /// How many selections are there?
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSelections(): Integer;
    /// <summary>
    /// Is every selected range empty?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetSelectionEmpty(): Boolean;
    /// <summary>
    /// Clear selections to a single empty stream selection
    /// </summary>
    procedure ClearSelections();
    /// <summary>
    /// Set a simple selection
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure SetSelection(ACaret: TSciPosition; AAnchor: TSciPosition);
    /// <summary>
    /// Add a selection
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure AddSelection(ACaret: TSciPosition; AAnchor: TSciPosition);
    /// <summary>
    /// Find the selection index for a point. -1 when not at a selection.
    /// </summary>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <param name="y">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function SelectionFromPoint(x: Integer; y: Integer): Integer;
    /// <summary>
    /// Drop one selection
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    procedure DropSelectionN(ASelection: Integer);
    /// <summary>
    /// Set the main selection
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    procedure SetMainSelection(ASelection: Integer);
    /// <summary>
    /// Which selection is the main selection
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMainSelection(): Integer;
    /// <summary>
    /// Set the caret position of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetSelectionNCaret(ASelection: Integer; ACaret: TSciPosition);
    /// <summary>
    /// Return the caret position of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNCaret(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Set the anchor position of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure SetSelectionNAnchor(ASelection: Integer; AAnchor: TSciPosition);
    /// <summary>
    /// Return the anchor position of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNAnchor(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Set the virtual space of the caret of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <param name="ASpace">
    /// Position in the document
    /// </param>
    procedure SetSelectionNCaretVirtualSpace(ASelection: Integer; ASpace: TSciPosition);
    /// <summary>
    /// Return the virtual space of the caret of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNCaretVirtualSpace(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Set the virtual space of the anchor of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <param name="ASpace">
    /// Position in the document
    /// </param>
    procedure SetSelectionNAnchorVirtualSpace(ASelection: Integer; ASpace: TSciPosition);
    /// <summary>
    /// Return the virtual space of the anchor of the nth selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNAnchorVirtualSpace(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Sets the position that starts the selection - this becomes the anchor.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure SetSelectionNStart(ASelection: Integer; AAnchor: TSciPosition);
    /// <summary>
    /// Returns the position at the start of the selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNStart(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Returns the virtual space at the start of the selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNStartVirtualSpace(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Sets the position that ends the selection - this becomes the currentPosition.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetSelectionNEnd(ASelection: Integer; ACaret: TSciPosition);
    /// <summary>
    /// Returns the virtual space at the end of the selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNEndVirtualSpace(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Returns the position at the end of the selection.
    /// </summary>
    /// <param name="ASelection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNEnd(ASelection: Integer): TSciPosition;
    /// <summary>
    /// Set the caret position of the rectangular selection.
    /// </summary>
    /// <param name="ACaret">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionCaret(ACaret: TSciPosition);
    /// <summary>
    /// Return the caret position of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionCaret(): TSciPosition;
    /// <summary>
    /// Set the anchor position of the rectangular selection.
    /// </summary>
    /// <param name="AAnchor">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionAnchor(AAnchor: TSciPosition);
    /// <summary>
    /// Return the anchor position of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionAnchor(): TSciPosition;
    /// <summary>
    /// Set the virtual space of the caret of the rectangular selection.
    /// </summary>
    /// <param name="ASpace">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionCaretVirtualSpace(ASpace: TSciPosition);
    /// <summary>
    /// Return the virtual space of the caret of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionCaretVirtualSpace(): TSciPosition;
    /// <summary>
    /// Set the virtual space of the anchor of the rectangular selection.
    /// </summary>
    /// <param name="ASpace">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionAnchorVirtualSpace(ASpace: TSciPosition);
    /// <summary>
    /// Return the virtual space of the anchor of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionAnchorVirtualSpace(): TSciPosition;
    /// <summary>
    /// Set options for virtual space behaviour.
    /// </summary>
    /// <param name="AVirtualSpaceOptions">
    /// The AVirtualSpaceOptions parameter
    /// </param>
    procedure SetVirtualSpaceOptions(AVirtualSpaceOptions: NativeInt);
    /// <summary>
    /// Return options for virtual space behaviour.
    /// </summary>
    /// <returns>
    /// Returns the virtualspaceoptions
    /// </returns>
    function GetVirtualSpaceOptions(): NativeInt;
    /// <summary>
    /// On GTK, allow selecting the modifier key to use for mouse-based
    /// rectangular selection. Often the window manager requires Alt+Mouse Drag
    /// for moving windows.
    /// Valid values are SCMOD_CTRL(default), SCMOD_ALT, or SCMOD_SUPER.
    /// </summary>
    /// <param name="AModifier">
    /// Integer value
    /// </param>
    procedure SetRectangularSelectionModifier(AModifier: Integer);
    /// <summary>
    /// Get the modifier key used for rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetRectangularSelectionModifier(): Integer;
    /// <summary>
    /// Set the foreground colour of additional selections.
    /// Must have previously called SetSelFore with non-zero first argument for this to have an effect.
    /// </summary>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetAdditionalSelFore(AFore: TColor);
    /// <summary>
    /// Set the background colour of additional selections.
    /// Must have previously called SetSelBack with non-zero first argument for this to have an effect.
    /// </summary>
    /// <param name="ABack">
    /// Color value
    /// </param>
    procedure SetAdditionalSelBack(ABack: TColor);
    /// <summary>
    /// Set the alpha of the selection.
    /// </summary>
    /// <param name="AAlpha">
    /// The AAlpha parameter
    /// </param>
    procedure SetAdditionalSelAlpha(AAlpha: NativeInt);
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    /// <returns>
    /// Returns the additionalselalpha
    /// </returns>
    function GetAdditionalSelAlpha(): NativeInt;
    /// <summary>
    /// Set the foreground colour of additional carets.
    /// </summary>
    /// <param name="AFore">
    /// Color value
    /// </param>
    procedure SetAdditionalCaretFore(AFore: TColor);
    /// <summary>
    /// Get the foreground colour of additional carets.
    /// </summary>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetAdditionalCaretFore(): TColor;
    /// <summary>
    /// Set the main selection to the next selection.
    /// </summary>
    procedure RotateSelection();
    /// <summary>
    /// Swap that caret and anchor of the main selection.
    /// </summary>
    procedure SwapMainAnchorCaret();
    /// <summary>
    /// Add the next occurrence of the main selection to the set of selections as main.
    /// If the current selection is empty then select word around caret.
    /// </summary>
    procedure MultipleSelectAddNext();
    /// <summary>
    /// Add each occurrence of the main selection in the target to the set of selections.
    /// If the current selection is empty then select word around caret.
    /// </summary>
    procedure MultipleSelectAddEach();
    /// <summary>
    /// Indicate that the internal state of a lexer has changed over a range and therefore
    /// there may be a need to redraw.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function ChangeLexerState(AStart: TSciPosition; AEnd: TSciPosition): Integer;
    /// <summary>
    /// Find the next line at or after lineStart that is a contracted fold header line.
    /// Return -1 when no more lines.
    /// </summary>
    /// <param name="ALineStart">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function ContractedFoldNext(ALineStart: TSciLine): TSciLine;
    /// <summary>
    /// Centre current line in window.
    /// </summary>
    procedure VerticalCentreCaret();
    /// <summary>
    /// Move the selected lines up one line, shifting the line above after the selection
    /// </summary>
    procedure MoveSelectedLinesUp();
    /// <summary>
    /// Move the selected lines down one line, shifting the line below before the selection
    /// </summary>
    procedure MoveSelectedLinesDown();
    /// <summary>
    /// Set the identifier reported as idFrom in notification messages.
    /// </summary>
    /// <param name="AIdentifier">
    /// Integer value
    /// </param>
    procedure SetIdentifier(AIdentifier: Integer);
    /// <summary>
    /// Get the identifier.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetIdentifier(): Integer;
    /// <summary>
    /// Set the width for future RGBA image data.
    /// </summary>
    /// <param name="AWidth">
    /// Integer value
    /// </param>
    procedure RGBAImageSetWidth(AWidth: Integer);
    /// <summary>
    /// Set the height for future RGBA image data.
    /// </summary>
    /// <param name="AHeight">
    /// Integer value
    /// </param>
    procedure RGBAImageSetHeight(AHeight: Integer);
    /// <summary>
    /// Set the scale factor in percent for future RGBA image data.
    /// </summary>
    /// <param name="AScalePercent">
    /// Integer value
    /// </param>
    procedure RGBAImageSetScale(AScalePercent: Integer);
    /// <summary>
    /// Define a marker from RGBA data.
    /// It has the width and height from RGBAImageSetWidth/Height
    /// </summary>
    /// <param name="AMarkerNumber">
    /// Integer value
    /// </param>
    /// <param name="APixels">
    /// Text string
    /// </param>
    procedure MarkerDefineRGBAImage(AMarkerNumber: Integer; APixels: PAnsiChar);
    /// <summary>
    /// Register an RGBA image for use in autocompletion lists.
    /// It has the width and height from RGBAImageSetWidth/Height
    /// </summary>
    /// <param name="AType">
    /// Integer value
    /// </param>
    /// <param name="APixels">
    /// Text string
    /// </param>
    procedure RegisterRGBAImage(AType: Integer; APixels: PAnsiChar);
    /// <summary>
    /// Scroll to start of document.
    /// </summary>
    procedure ScrollToStart();
    /// <summary>
    /// Scroll to end of document.
    /// </summary>
    procedure ScrollToEnd();
    /// <summary>
    /// Set the technology used.
    /// </summary>
    /// <param name="ATechnology">
    /// The ATechnology parameter
    /// </param>
    procedure SetTechnology(ATechnology: NativeInt);
    /// <summary>
    /// Get the tech.
    /// </summary>
    /// <returns>
    /// Returns the technology
    /// </returns>
    function GetTechnology(): NativeInt;
    /// <summary>
    /// Create an ILoader*.
    /// </summary>
    /// <param name="ABytes">
    /// Position in the document
    /// </param>
    /// <param name="ADocumentOptions">
    /// The ADocumentOptions parameter
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function CreateLoader(ABytes: TSciPosition; ADocumentOptions: NativeInt): Pointer;
    /// <summary>
    /// On macOS, show a find indicator.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    procedure FindIndicatorShow(AStart: TSciPosition; AEnd: TSciPosition);
    /// <summary>
    /// On macOS, flash a find indicator, then fade out.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    procedure FindIndicatorFlash(AStart: TSciPosition; AEnd: TSciPosition);
    /// <summary>
    /// On macOS, hide the find indicator.
    /// </summary>
    procedure FindIndicatorHide();
    /// <summary>
    /// Move caret to before first visible character on display line.
    /// If already there move to first character on display line.
    /// </summary>
    procedure VCHomeDisplay();
    /// <summary>
    /// Like VCHomeDisplay but extending selection to new caret position.
    /// </summary>
    procedure VCHomeDisplayExtend();
    /// <summary>
    /// Is the caret line always visible?
    /// </summary>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetCaretLineVisibleAlways(): Boolean;
    /// <summary>
    /// Sets the caret line to always visible.
    /// </summary>
    /// <param name="AAlwaysVisible">
    /// Boolean value
    /// </param>
    procedure SetCaretLineVisibleAlways(AAlwaysVisible: Boolean);
    /// <summary>
    /// Set the line end types that the application wants to use. May not be used if incompatible with lexer or encoding.
    /// </summary>
    /// <param name="ALineEndBitSet">
    /// The ALineEndBitSet parameter
    /// </param>
    procedure SetLineEndTypesAllowed(ALineEndBitSet: NativeInt);
    /// <summary>
    /// Get the line end types currently allowed.
    /// </summary>
    /// <returns>
    /// Returns the lineendtypesallowed
    /// </returns>
    function GetLineEndTypesAllowed(): NativeInt;
    /// <summary>
    /// Get the line end types currently recognised. May be a subset of the allowed types due to lexer limitation.
    /// </summary>
    /// <returns>
    /// Returns the lineendtypesactive
    /// </returns>
    function GetLineEndTypesActive(): NativeInt;
    /// <summary>
    /// Set the way a character is drawn.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    /// <param name="ARepresentation">
    /// Text string
    /// </param>
    procedure SetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar);
    /// <summary>
    /// Get the way a character is drawn.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    /// <param name="ARepresentation">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar): Integer;
    /// <summary>
    /// Remove a character representation.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    procedure ClearRepresentation(AEncodedCharacter: PAnsiChar);
    /// <summary>
    /// Clear representations to default.
    /// </summary>
    procedure ClearAllRepresentations();
    /// <summary>
    /// Set the appearance of a representation.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    /// <param name="AAppearance">
    /// The AAppearance parameter
    /// </param>
    procedure SetRepresentationAppearance(AEncodedCharacter: PAnsiChar; AAppearance: NativeInt);
    /// <summary>
    /// Get the appearance of a representation.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the representationappearance
    /// </returns>
    function GetRepresentationAppearance(AEncodedCharacter: PAnsiChar): NativeInt;
    /// <summary>
    /// Set the colour of a representation.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    /// <param name="AColour">
    /// The AColour parameter
    /// </param>
    procedure SetRepresentationColour(AEncodedCharacter: PAnsiChar; AColour: TColorAlpha);
    /// <summary>
    /// Get the colour of a representation.
    /// </summary>
    /// <param name="AEncodedCharacter">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the representationcolour
    /// </returns>
    function GetRepresentationColour(AEncodedCharacter: PAnsiChar): TColorAlpha;
    /// <summary>
    /// Set the end of line annotation text for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Text string
    /// </param>
    procedure EOLAnnotationSetText(ALine: TSciLine; AText: PAnsiChar);
    /// <summary>
    /// Get the end of line annotation text for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AText">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function EOLAnnotationGetText(ALine: TSciLine; AText: PAnsiChar): Integer;
    /// <summary>
    /// Set the style number for the end of line annotations for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure EOLAnnotationSetStyle(ALine: TSciLine; AStyle: Integer);
    /// <summary>
    /// Get the style number for the end of line annotations for a line
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function EOLAnnotationGetStyle(ALine: TSciLine): Integer;
    /// <summary>
    /// Clear the end of annotations from all lines
    /// </summary>
    procedure EOLAnnotationClearAll();
    /// <summary>
    /// Set the visibility for the end of line annotations for a view
    /// </summary>
    /// <param name="AVisible">
    /// The AVisible parameter
    /// </param>
    procedure EOLAnnotationSetVisible(AVisible: NativeInt);
    /// <summary>
    /// Get the visibility for the end of line annotations for a view
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function EOLAnnotationGetVisible(): NativeInt;
    /// <summary>
    /// Get the start of the range of style numbers used for end of line annotations
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    procedure EOLAnnotationSetStyleOffset(AStyle: Integer);
    /// <summary>
    /// Get the start of the range of style numbers used for end of line annotations
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function EOLAnnotationGetStyleOffset(): Integer;
    /// <summary>
    /// Get whether a feature is supported
    /// </summary>
    /// <param name="AFeature">
    /// The AFeature parameter
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function SupportsFeature(AFeature: NativeInt): Boolean;
    /// <summary>
    /// Retrieve line character index state.
    /// </summary>
    /// <returns>
    /// Returns the linecharacterindex
    /// </returns>
    function GetLineCharacterIndex(): NativeInt;
    /// <summary>
    /// Request line character index be created or its use count increased.
    /// </summary>
    /// <param name="ALineCharacterIndex">
    /// The ALineCharacterIndex parameter
    /// </param>
    procedure AllocateLineCharacterIndex(ALineCharacterIndex: NativeInt);
    /// <summary>
    /// Decrease use count of line character index and remove if 0.
    /// </summary>
    /// <param name="ALineCharacterIndex">
    /// The ALineCharacterIndex parameter
    /// </param>
    procedure ReleaseLineCharacterIndex(ALineCharacterIndex: NativeInt);
    /// <summary>
    /// Retrieve the document line containing a position measured in index units.
    /// </summary>
    /// <param name="APos">
    /// Position in the document
    /// </param>
    /// <param name="ALineCharacterIndex">
    /// The ALineCharacterIndex parameter
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function LineFromIndexPosition(APos: TSciPosition; ALineCharacterIndex: NativeInt): TSciLine;
    /// <summary>
    /// Retrieve the position measured in index units at the start of a document line.
    /// </summary>
    /// <param name="ALine">
    /// Line number
    /// </param>
    /// <param name="ALineCharacterIndex">
    /// The ALineCharacterIndex parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function IndexPositionFromLine(ALine: TSciLine; ALineCharacterIndex: NativeInt): TSciPosition;
    /// <summary>
    /// Start notifying the container of all key presses and commands.
    /// </summary>
    procedure StartRecord();
    /// <summary>
    /// Stop notifying the container of all key presses and commands.
    /// </summary>
    procedure StopRecord();
    /// <summary>
    /// Retrieve the lexing language of the document.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLexer(): Integer;
    /// <summary>
    /// Colourise a segment of the document using the current lexing language.
    /// </summary>
    /// <param name="AStart">
    /// Position in the document
    /// </param>
    /// <param name="AEnd">
    /// Position in the document
    /// </param>
    procedure Colourise(AStart: TSciPosition; AEnd: TSciPosition);
    /// <summary>
    /// Set up a value that may be used by a lexer for some optional feature.
    /// </summary>
    /// <param name="AKey">
    /// Text string
    /// </param>
    /// <param name="AValue">
    /// Text string
    /// </param>
    procedure SetProperty(AKey: PAnsiChar; AValue: PAnsiChar);
    /// <summary>
    /// Set up the key words used by the lexer.
    /// </summary>
    /// <param name="AKeyWordSet">
    /// Integer value
    /// </param>
    /// <param name="AKeyWords">
    /// Text string
    /// </param>
    procedure SetKeyWords(AKeyWordSet: Integer; AKeyWords: PAnsiChar);
    /// <summary>
    /// Retrieve a "property" value previously set with SetProperty.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AKey">
    /// Text string
    /// </param>
    /// <param name="AValue">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetProperty(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a "property" value previously set with SetProperty,
    /// with "$()" variable replacement on returned buffer.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AKey">
    /// Text string
    /// </param>
    /// <param name="AValue">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPropertyExpanded(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a "property" value previously set with SetProperty,
    /// interpreted as an int AFTER any "$()" variable replacement.
    /// </summary>
    /// <param name="AKey">
    /// Text string
    /// </param>
    /// <param name="ADefaultValue">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPropertyInt(AKey: PAnsiChar; ADefaultValue: Integer): Integer;
    /// <summary>
    /// Retrieve the name of the lexer.
    /// Return the length of the text.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="ALanguage">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLexerLanguage(ALanguage: PAnsiChar): Integer;
    /// <summary>
    /// For private communication between an application and a known lexer.
    /// </summary>
    /// <param name="AOperation">
    /// Integer value
    /// </param>
    /// <param name="APointer">
    /// The APointer parameter
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function PrivateLexerCall(AOperation: Integer; APointer: Pointer): Pointer;
    /// <summary>
    /// Retrieve a '\n' separated list of properties understood by the current lexer.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="ANames">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function PropertyNames(ANames: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve the type of a property.
    /// </summary>
    /// <param name="AName">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function PropertyType(AName: PAnsiChar): NativeInt;
    /// <summary>
    /// Describe a property.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AName">
    /// Text string
    /// </param>
    /// <param name="ADescription">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DescribeProperty(AName: PAnsiChar; ADescription: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a '\n' separated list of descriptions of the keyword sets understood by the current lexer.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="ADescriptions">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DescribeKeyWordSets(ADescriptions: PAnsiChar): Integer;
    /// <summary>
    /// Bit set of LineEndType enumertion for which line ends beyond the standard
    /// LF, CR, and CRLF are supported by the lexer.
    /// </summary>
    /// <returns>
    /// Returns the lineendtypessupported
    /// </returns>
    function GetLineEndTypesSupported(): NativeInt;
    /// <summary>
    /// Allocate a set of sub styles for a particular base style, returning start of range
    /// </summary>
    /// <param name="AStyleBase">
    /// Integer value
    /// </param>
    /// <param name="ANumberStyles">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AllocateSubStyles(AStyleBase: Integer; ANumberStyles: Integer): Integer;
    /// <summary>
    /// The starting style number for the sub styles associated with a base style
    /// </summary>
    /// <param name="AStyleBase">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSubStylesStart(AStyleBase: Integer): Integer;
    /// <summary>
    /// The number of sub styles associated with a base style
    /// </summary>
    /// <param name="AStyleBase">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSubStylesLength(AStyleBase: Integer): Integer;
    /// <summary>
    /// For a sub style, return the base style, else return the argument.
    /// </summary>
    /// <param name="ASubStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetStyleFromSubStyle(ASubStyle: Integer): Integer;
    /// <summary>
    /// For a secondary style, return the primary style, else return the argument.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPrimaryStyleFromStyle(AStyle: Integer): Integer;
    /// <summary>
    /// Free allocated sub styles
    /// </summary>
    procedure FreeSubStyles();
    /// <summary>
    /// Set the identifiers that are shown in a particular style
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AIdentifiers">
    /// Text string
    /// </param>
    procedure SetIdentifiers(AStyle: Integer; AIdentifiers: PAnsiChar);
    /// <summary>
    /// Where styles are duplicated by a feature such as active/inactive code
    /// return the distance between the two types.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DistanceToSecondaryStyles(): Integer;
    /// <summary>
    /// Get the set of base styles that can be extended with sub styles
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AStyles">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSubStyleBases(AStyles: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve the number of named styles for the lexer.
    /// </summary>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetNamedStyles(): Integer;
    /// <summary>
    /// Retrieve the name of a style.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="AName">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function NameOfStyle(AStyle: Integer; AName: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a ' ' separated list of style tags like "literal quoted string".
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ATags">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function TagsOfStyle(AStyle: Integer; ATags: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a description of a style.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="AStyle">
    /// Integer value
    /// </param>
    /// <param name="ADescription">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DescriptionOfStyle(AStyle: Integer; ADescription: PAnsiChar): Integer;
    /// <summary>
    /// Set the lexer from an ILexer*.
    /// </summary>
    /// <param name="AIlexer">
    /// The AIlexer parameter
    /// </param>
    procedure SetILexer(AIlexer: Pointer);

    // Provisional
    /// <summary>
    /// Retrieve bidirectional text display state.
    /// </summary>
    /// <returns>
    /// Returns the bidirectional
    /// </returns>
    function GetBidirectional(): NativeInt;
    /// <summary>
    /// Set bidirectional text display state.
    /// </summary>
    /// <param name="ABidirectional">
    /// The ABidirectional parameter
    /// </param>
    procedure SetBidirectional(ABidirectional: NativeInt);

  strict protected
    // Auto-generated properties
    /// <summary>
    /// Retrieve the current tab draw mode.
    /// Returns one of SCTD_* constants.
    /// </summary>
    property TabDrawMode: NativeInt read GetTabDrawMode write SetTabDrawMode;
    /// <summary>
    /// Is printing line wrapped?
    /// </summary>
    property PrintWrapMode: NativeInt read GetPrintWrapMode write SetPrintWrapMode;
    /// <summary>
    /// Retrieve the caret line frame width.
    /// Width = 0 means this option is disabled.
    /// </summary>
    property CaretLineFrame: Integer read GetCaretLineFrame write SetCaretLineFrame;
    /// <summary>
    /// Which action is the save point?
    /// </summary>
    property UndoSavePoint: Integer read GetUndoSavePoint write SetUndoSavePoint;
    /// <summary>
    /// Returns the number of bytes in the document.
    /// </summary>
    property Length: TSciPosition read GetLength;
    /// <summary>
    /// Get the size of the dots used to mark space characters.
    /// </summary>
    property WhitespaceSize: Integer read GetWhitespaceSize write SetWhitespaceSize;
    /// <summary>
    /// Get the code page used to interpret the bytes of the document as characters.
    /// </summary>
    property CodePage: Integer read GetCodePage write SetCodePage;
    /// <summary>
    /// In read-only mode?
    /// </summary>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    /// <summary>
    /// Retrieve the visible size of a tab.
    /// </summary>
    property TabWidth: Integer read GetTabWidth write SetTabWidth;
    /// <summary>
    /// Retrieve the colour used in edge indication.
    /// </summary>
    property EdgeColour: TColor read GetEdgeColour write SetEdgeColour;
    /// <summary>
    /// Is every selected range empty?
    /// </summary>
    property SelectionEmpty: Boolean read GetSelectionEmpty;
    /// <summary>
    /// Retrive the start indent for wrapped lines.
    /// </summary>
    property WrapStartIndent: Integer read GetWrapStartIndent write SetWrapStartIndent;
    /// <summary>
    /// How many undo actions are in the history?
    /// </summary>
    property UndoActions: Integer read GetUndoActions;
    /// <summary>
    /// How many entries are allocated to the position cache?
    /// </summary>
    property PositionCache: Integer read GetPositionCache write SetPositionCache;
    /// <summary>
    /// Retrieve whether the maximum scroll position has the last
    /// line at the bottom of the view.
    /// </summary>
    property EndAtLastLine: Boolean read GetEndAtLastLine write SetEndAtLastLine;
    /// <summary>
    /// Which action is the tentative point?
    /// </summary>
    property UndoTentative: Integer read GetUndoTentative write SetUndoTentative;
    /// <summary>
    /// Retrieve whether tabs will be used in indentation.
    /// </summary>
    property UseTabs: Boolean read GetUseTabs write SetUseTabs;
    /// <summary>
    /// Get whether mouse gets captured.
    /// </summary>
    property MouseDownCaptures: Boolean read GetMouseDownCaptures write SetMouseDownCaptures;
    /// <summary>
    /// Retrieve a pointer to a function that processes messages for this Scintilla and returns status.
    /// </summary>
    property DirectStatusFunction: Pointer read GetDirectStatusFunction;
    /// <summary>
    /// Get the xOffset (ie, horizontal scroll position).
    /// </summary>
    property XOffset: Integer read GetXOffset write SetXOffset;
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    property SelAlpha: NativeInt read GetSelAlpha write SetSelAlpha;
    /// <summary>
    /// Get the identifier.
    /// </summary>
    property Identifier: Integer read GetIdentifier write SetIdentifier;
    /// <summary>
    /// Which action is the current point?
    /// </summary>
    property UndoCurrent: Integer read GetUndoCurrent write SetUndoCurrent;
    /// <summary>
    /// Get the number of characters to have directly indexed categories
    /// </summary>
    property CharacterCategoryOptimization: Integer read GetCharacterCategoryOptimization write SetCharacterCategoryOptimization;
    /// <summary>
    /// Is the IME displayed in a window or inline?
    /// </summary>
    property IMEInteraction: NativeInt read GetIMEInteraction write SetIMEInteraction;
    /// <summary>
    /// Retrieve the time the mouse must sit still to generate a mouse dwell event.
    /// </summary>
    property MouseDwellTime: Integer read GetMouseDwellTime write SetMouseDwellTime;
    /// <summary>
    /// Get the layer of the background of the line containing the caret.
    /// </summary>
    property CaretLineLayer: NativeInt read GetCaretLineLayer write SetCaretLineLayer;
    /// <summary>
    /// Is an undo sequence active?
    /// </summary>
    property UndoSequence: Integer read GetUndoSequence;
    /// <summary>
    /// Returns the size in pixels of the right margin.
    /// </summary>
    property MarginRight: Integer read GetMarginRight write SetMarginRight;
    /// <summary>
    /// Get cursor type.
    /// </summary>
    property Cursor: NativeInt read GetCursor write SetCursor;
    /// <summary>
    /// Get automatic folding behaviours.
    /// </summary>
    property AutomaticFold: NativeInt read GetAutomaticFold write SetAutomaticFold;
    /// <summary>
    /// Retrieve the quality level for text.
    /// </summary>
    property FontQuality: NativeInt read GetFontQuality write SetFontQuality;
    /// <summary>
    /// Return the virtual space of the anchor of the rectangular selection.
    /// </summary>
    property RectangularSelectionAnchorVirtualSpace: TSciPosition read GetRectangularSelectionAnchorVirtualSpace write SetRectangularSelectionAnchorVirtualSpace;
    /// <summary>
    /// Get the tech.
    /// </summary>
    property Technology: NativeInt read GetTechnology write SetTechnology;
    /// <summary>
    /// Report change history status.
    /// </summary>
    property ChangeHistory: NativeInt read GetChangeHistory write SetChangeHistory;
    /// <summary>
    /// Retrieve the current end of line mode - one of CRLF, CR, or LF.
    /// </summary>
    property EOLMode: NativeInt read GetEOLMode write SetEOLMode;
    /// <summary>
    /// Get the line end types currently recognised. May be a subset of the allowed types due to lexer limitation.
    /// </summary>
    property LineEndTypesActive: NativeInt read GetLineEndTypesActive;
    /// <summary>
    /// Get convert-on-paste setting
    /// </summary>
    property PasteConvertEndings: Boolean read GetPasteConvertEndings write SetPasteConvertEndings;
    /// <summary>
    /// Retrieve the zoom level.
    /// </summary>
    property Zoom: Integer read GetZoom write SetZoom;
    /// <summary>
    /// Get the virtual space of the target start
    /// </summary>
    property TargetStartVirtualSpace: TSciPosition read GetTargetStartVirtualSpace write SetTargetStartVirtualSpace;
    /// <summary>
    /// Retrieve the lexing language of the document.
    /// </summary>
    property Lexer: Integer read GetLexer;
    /// <summary>
    /// Get internal focus flag.
    /// </summary>
    property Focus: Boolean read GetFocus write SetFocus;
    /// <summary>
    /// Retrieve the degree of caching of layout information.
    /// </summary>
    property LayoutCache: NativeInt read GetLayoutCache write SetLayoutCache;
    /// <summary>
    /// Are all lines visible?
    /// </summary>
    property AllLinesVisible: Boolean read GetAllLinesVisible;
    /// <summary>
    /// Retrieve a pointer to a function that processes messages for this Scintilla.
    /// </summary>
    property DirectFunction: Pointer read GetDirectFunction;
    /// <summary>
    /// Are white space characters currently visible?
    /// Returns one of SCWS_* constants.
    /// </summary>
    property ViewWS: NativeInt read GetViewWS write SetViewWS;
    /// <summary>
    /// Retrieve the number of named styles for the lexer.
    /// </summary>
    property NamedStyles: Integer read GetNamedStyles;
    /// <summary>
    /// Get the fore colour for active hotspots.
    /// </summary>
    property HotspotActiveFore: TColor read GetHotspotActiveFore;// write SetHotspotActiveFore;
    /// <summary>
    /// Retrieve the edge highlight mode.
    /// </summary>
    property EdgeMode: NativeInt read GetEdgeMode write SetEdgeMode;
    /// <summary>
    /// Which selection is the main selection
    /// </summary>
    property MainSelection: Integer read GetMainSelection write SetMainSelection;
    /// <summary>
    /// Return options for virtual space behaviour.
    /// </summary>
    property VirtualSpaceOptions: NativeInt read GetVirtualSpaceOptions write SetVirtualSpaceOptions;
    /// <summary>
    /// Get the time in milliseconds that the caret is on and off.
    /// </summary>
    property CaretPeriod: Integer read GetCaretPeriod write SetCaretPeriod;
    /// <summary>
    /// Get the way control characters are displayed.
    /// </summary>
    property ControlCharSymbol: Integer read GetControlCharSymbol write SetControlCharSymbol;
    /// <summary>
    /// Is the document different from when it was last saved?
    /// </summary>
    property Modify: Boolean read GetModify;
    /// <summary>
    /// Get the search flags used by SearchInTarget.
    /// </summary>
    property SearchFlags: NativeInt read GetSearchFlags write SetSearchFlags;
    /// <summary>
    /// Retrieve the last line number that has line state.
    /// </summary>
    property MaxLineState: Integer read GetMaxLineState;
    /// <summary>
    /// Get the minimum visual width of a tab.
    /// </summary>
    property TabMinimumWidth: Integer read GetTabMinimumWidth write SetTabMinimumWidth;
    /// <summary>
    /// Returns the position at the end of the selection.
    /// </summary>
    property SelectionEnd: TSciPosition read GetSelectionEnd write SetSelectionEnd;
    /// <summary>
    /// Whether typing can be performed into multiple selections
    /// </summary>
    property AdditionalSelectionTyping: Boolean read GetAdditionalSelectionTyping write SetAdditionalSelectionTyping;
    /// <summary>
    /// Get the layer for drawing selections
    /// </summary>
    property SelectionLayer: NativeInt read GetSelectionLayer write SetSelectionLayer;
    /// <summary>
    /// Return the caret position of the rectangular selection.
    /// </summary>
    property RectangularSelectionCaret: TSciPosition read GetRectangularSelectionCaret write SetRectangularSelectionCaret;
    /// <summary>
    /// Get only highlighting subline instead of whole line.
    /// </summary>
    property CaretLineHighlightSubLine: Boolean read GetCaretLineHighlightSubLine write SetCaretLineHighlightSubLine;
    /// <summary>
    /// Get error status.
    /// </summary>
    property Status: NativeInt read GetStatus write SetStatus;
    /// <summary>
    /// Returns the position of the opposite end of the selection to the caret.
    /// </summary>
    property Anchor: TSciPosition read GetAnchor write SetAnchor;
    /// <summary>
    /// How many phases is drawing done in?
    /// </summary>
    property PhasesDraw: NativeInt read GetPhasesDraw write SetPhasesDraw;
    /// <summary>
    /// Get whether or not regular caret moves will extend or reduce the selection.
    /// </summary>
    property MoveExtendsSelection: Boolean read GetMoveExtendsSelection write SetMoveExtendsSelection;
    /// <summary>
    /// Retrieve the limits to idle styling.
    /// </summary>
    property IdleStyling: NativeInt read GetIdleStyling write SetIdleStyling;
    /// <summary>
    /// Retrieve how wrapped sublines are placed. Default is fixed.
    /// </summary>
    property WrapIndentMode: NativeInt read GetWrapIndentMode write SetWrapIndentMode;
    /// <summary>
    /// Is the vertical scroll bar visible?
    /// </summary>
    property VScrollBar: Boolean read GetVScrollBar write SetVScrollBar;
    /// <summary>
    /// How many selections are there?
    /// </summary>
    property Selections: Integer read GetSelections;
    /// <summary>
    /// Returns the position at the start of the selection.
    /// </summary>
    property SelectionStart: TSciPosition read GetSelectionStart write SetSelectionStart;
    /// <summary>
    /// Report undo selection history status.
    /// </summary>
    property UndoSelectionHistory: NativeInt read GetUndoSelectionHistory write SetUndoSelectionHistory;
    /// <summary>
    /// Retrieve the display line at the top of the display.
    /// </summary>
    property FirstVisibleLine: TSciLine read GetFirstVisibleLine write SetFirstVisibleLine;
    /// <summary>
    /// Retrieve whether text is word wrapped.
    /// </summary>
    property WrapMode: NativeInt read GetWrapMode write SetWrapMode;
    /// <summary>
    /// Return a position which, to avoid performance costs, should not be within
    /// the range of a call to GetRangePointer.
    /// </summary>
    property GapPosition: TSciPosition read GetGapPosition;
    /// <summary>
    /// Is the background of the line containing the caret in a different colour?
    /// </summary>
    property CaretLineVisible: Boolean read GetCaretLineVisible write SetCaretLineVisible;
    /// <summary>
    /// Retrieve a pointer value to use as the first argument when calling
    /// the function returned by GetDirectFunction.
    /// </summary>
    property DirectPointer: Pointer read GetDirectPointer;
    /// <summary>
    /// Get the foreground colour of additional carets.
    /// </summary>
    property AdditionalCaretFore: TColor read GetAdditionalCaretFore write SetAdditionalCaretFore;
    /// <summary>
    /// Get the background alpha of the caret line.
    /// </summary>
    property CaretLineBackAlpha: NativeInt read GetCaretLineBackAlpha write SetCaretLineBackAlpha;
    /// <summary>
    /// Get whether mouse wheel can be active outside the window.
    /// </summary>
    property MouseWheelCaptures: Boolean read GetMouseWheelCaptures write SetMouseWheelCaptures;
    /// <summary>
    /// Get the HotspotSingleLine property
    /// </summary>
    property HotspotSingleLine: Boolean read GetHotspotSingleLine write SetHotspotSingleLine;
    /// <summary>
    /// Returns the current style of the caret.
    /// </summary>
    property CaretStyle: NativeInt read GetCaretStyle write SetCaretStyle;
    /// <summary>
    /// Get maximum number of threads used for layout
    /// </summary>
    property LayoutThreads: Integer read GetLayoutThreads write SetLayoutThreads;
    /// <summary>
    /// Retrieve indentation size.
    /// </summary>
    property Indent: Integer read GetIndent write SetIndent;
    /// <summary>
    /// Get extra descent for each line
    /// </summary>
    property ExtraDescent: Integer read GetExtraDescent write SetExtraDescent;
    /// <summary>
    /// Is drawing done first into a buffer or direct to the screen?
    /// </summary>
    property BufferedDraw: Boolean read GetBufferedDraw write SetBufferedDraw;
    /// <summary>
    /// Get the mode of the current selection.
    /// </summary>
    property SelectionMode: NativeInt read GetSelectionMode write SetSelectionMode;
    /// <summary>
    /// Retrieve the document width assumed for scrolling.
    /// </summary>
    property ScrollWidth: Integer read GetScrollWidth write SetScrollWidth;
    /// <summary>
    /// Get the modifier key used for rectangular selection.
    /// </summary>
    property RectangularSelectionModifier: Integer read GetRectangularSelectionModifier write SetRectangularSelectionModifier;
    /// <summary>
    /// Retrieve the position of the last correctly styled character.
    /// </summary>
    property EndStyled: TSciPosition read GetEndStyled;
    /// <summary>
    /// Is the selection end of line filled?
    /// </summary>
    property SelEOLFilled: Boolean read GetSelEOLFilled write SetSelEOLFilled;
    /// <summary>
    /// Get the colour of the background of the line containing the caret.
    /// </summary>
    property CaretLineBack: TColor read GetCaretLineBack write SetCaretLineBack;
    /// <summary>
    /// Bit set of LineEndType enumertion for which line ends beyond the standard
    /// LF, CR, and CRLF are supported by the lexer.
    /// </summary>
    property LineEndTypesSupported: NativeInt read GetLineEndTypesSupported;
    /// <summary>
    /// Get whether command events are sent to the container.
    /// </summary>
    property CommandEvents: Boolean read GetCommandEvents write SetCommandEvents;
    /// <summary>
    /// Get the current indicator
    /// </summary>
    property IndicatorCurrent: Integer read GetIndicatorCurrent write SetIndicatorCurrent;
    property SelectionHidden: Boolean read GetSelectionHidden;
    /// <summary>
    /// Returns the number of lines in the document. There is always at least one.
    /// </summary>
    property LineCount: TSciLine read GetLineCount;
    /// <summary>
    /// Retrieve the effect of pasting when there are multiple selections.
    /// </summary>
    property MultiPaste: NativeInt read GetMultiPaste write SetMultiPaste;
    /// <summary>
    /// Report accessibility status.
    /// </summary>
    property Accessibility: NativeInt read GetAccessibility write SetAccessibility;
    /// <summary>
    /// Is undo history being collected?
    /// </summary>
    property UndoCollection: Boolean read GetUndoCollection write SetUndoCollection;
    /// <summary>
    /// Get the line end types currently allowed.
    /// </summary>
    property LineEndTypesAllowed: NativeInt read GetLineEndTypesAllowed write SetLineEndTypesAllowed;
    /// <summary>
    /// Get the foreground colour of the caret.
    /// </summary>
    property CaretFore: TColor read GetCaretFore write SetCaretFore;
    /// <summary>
    /// Get the margin options.
    /// </summary>
    property MarginOptions: NativeInt read GetMarginOptions write SetMarginOptions;
    /// <summary>
    /// Whether switching to rectangular mode while selecting with the mouse is allowed.
    /// </summary>
    property MouseSelectionRectangularSwitch: Boolean read GetMouseSelectionRectangularSwitch write SetMouseSelectionRectangularSwitch;
    /// <summary>
    /// Get which document modification events are sent to the container.
    /// </summary>
    property ModEventMask: NativeInt read GetModEventMask write SetModEventMask;
    /// <summary>
    /// Retrieve a pointer to the document object.
    /// </summary>
    property DocPointer: Pointer read GetDocPointer write SetDocPointer;
    /// <summary>
    /// How many margins are there?.
    /// </summary>
    property Margins: Integer read GetMargins write SetMargins;
    /// <summary>
    /// Get the back colour for active hotspots.
    /// </summary>
    property HotspotActiveBack: TColor read GetHotspotActiveBack;// write SetHotspotActiveBack;
    /// <summary>
    /// Retrieve the number of characters in the document.
    /// </summary>
    property TextLength: TSciPosition read GetTextLength;
    /// <summary>
    /// Get the position that ends the target.
    /// </summary>
    property TargetEnd: TSciPosition read GetTargetEnd write SetTargetEnd;
    /// <summary>
    /// Get the virtual space of the target end
    /// </summary>
    property TargetEndVirtualSpace: TSciPosition read GetTargetEndVirtualSpace write SetTargetEndVirtualSpace;
    /// <summary>
    /// Does a backspace pressed when caret is within indentation unindent?
    /// </summary>
    property BackSpaceUnIndents: Boolean read GetBackSpaceUnIndents write SetBackSpaceUnIndents;
    /// <summary>
    /// Returns the position of the caret.
    /// </summary>
    property CurrentPos: TSciPosition read GetCurrentPos write SetCurrentPos;
    /// <summary>
    /// Compact the document buffer and return a read-only pointer to the
    /// characters in the document.
    /// </summary>
    property CharacterPointer: Pointer read GetCharacterPointer;
    /// <summary>
    /// Whether additional carets are visible
    /// </summary>
    property AdditionalCaretsVisible: Boolean read GetAdditionalCaretsVisible write SetAdditionalCaretsVisible;
    /// <summary>
    /// Get the current indicator value
    /// </summary>
    property IndicatorValue: Integer read GetIndicatorValue write SetIndicatorValue;
    /// <summary>
    /// Returns true if overtype mode is active otherwise false is returned.
    /// </summary>
    property Overtype: Boolean read GetOvertype write SetOvertype;
    /// <summary>
    /// Return the anchor position of the rectangular selection.
    /// </summary>
    property RectangularSelectionAnchor: TSciPosition read GetRectangularSelectionAnchor write SetRectangularSelectionAnchor;
    /// <summary>
    /// Returns the print colour mode.
    /// </summary>
    property PrintColourMode: NativeInt read GetPrintColourMode write SetPrintColourMode;
    /// <summary>
    /// Get the position that starts the target.
    /// </summary>
    property TargetStart: TSciPosition read GetTargetStart write SetTargetStart;
    /// <summary>
    /// Returns the size in pixels of the left margin.
    /// </summary>
    property MarginLeft: Integer read GetMarginLeft write SetMarginLeft;
    /// <summary>
    /// Get extra ascent for each line
    /// </summary>
    property ExtraAscent: Integer read GetExtraAscent write SetExtraAscent;
    /// <summary>
    /// Return the virtual space of the caret of the rectangular selection.
    /// </summary>
    property RectangularSelectionCaretVirtualSpace: TSciPosition read GetRectangularSelectionCaretVirtualSpace write SetRectangularSelectionCaretVirtualSpace;
    /// <summary>
    /// Whether additional carets will blink
    /// </summary>
    property AdditionalCaretsBlink: Boolean read GetAdditionalCaretsBlink write SetAdditionalCaretsBlink;
    /// <summary>
    /// Whether multiple selections can be made
    /// </summary>
    property MultipleSelection: Boolean read GetMultipleSelection write SetMultipleSelection;
    /// <summary>
    /// Retrive the location of visual flags for wrapped lines.
    /// </summary>
    property WrapVisualFlagsLocation: NativeInt read GetWrapVisualFlagsLocation write SetWrapVisualFlagsLocation;
    /// <summary>
    /// Returns the print magnification.
    /// </summary>
    property PrintMagnification: Integer read GetPrintMagnification write SetPrintMagnification;
    /// <summary>
    /// Returns the width of the insert mode caret.
    /// </summary>
    property CaretWidth: Integer read GetCaretWidth write SetCaretWidth;
    /// <summary>
    /// Retrieve bidirectional text display state.
    /// </summary>
    property Bidirectional: NativeInt read GetBidirectional write SetBidirectional;
    /// <summary>
    /// Is the caret line always visible?
    /// </summary>
    property CaretLineVisibleAlways: Boolean read GetCaretLineVisibleAlways write SetCaretLineVisibleAlways;
    /// <summary>
    /// Get the highlighted indentation guide column.
    /// </summary>
    property HighlightGuide: TSciPosition read GetHighlightGuide write SetHighlightGuide;
    /// <summary>
    /// Can the caret preferred x position only be changed by explicit movement commands?
    /// </summary>
    property CaretSticky: NativeInt read GetCaretSticky write SetCaretSticky;
    /// <summary>
    /// Get whether underlining for active hotspots.
    /// </summary>
    property HotspotActiveUnderline: Boolean read GetHotspotActiveUnderline write SetHotspotActiveUnderline;
    /// <summary>
    /// Retrive the display mode of visual flags for wrapped lines.
    /// </summary>
    property WrapVisualFlags: NativeInt read GetWrapVisualFlags write SetWrapVisualFlags;
    /// <summary>
    /// Are the end of line characters visible?
    /// </summary>
    property ViewEOL: Boolean read GetViewEOL write SetViewEOL;
    /// <summary>
    /// Retrieve the column number which text should be kept within.
    /// </summary>
    property EdgeColumn: TSciPosition read GetEdgeColumn write SetEdgeColumn;
    /// <summary>
    /// Is the horizontal scroll bar visible?
    /// </summary>
    property HScrollBar: Boolean read GetHScrollBar write SetHScrollBar;
    /// <summary>
    /// Get which document options are set.
    /// </summary>
    property DocumentOptions: NativeInt read GetDocumentOptions;
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    property AdditionalSelAlpha: NativeInt read GetAdditionalSelAlpha write SetAdditionalSelAlpha;
    /// <summary>
    /// Are the indentation guides visible?
    /// </summary>
    property IndentationGuides: NativeInt read GetIndentationGuides write SetIndentationGuides;
    /// <summary>
    /// Retrieve whether the scroll width tracks wide lines.
    /// </summary>
    property ScrollWidthTracking: Boolean read GetScrollWidthTracking write SetScrollWidthTracking;
    /// <summary>
    /// Which action is the detach point?
    /// </summary>
    property UndoDetach: Integer read GetUndoDetach write SetUndoDetach;
    /// <summary>
    /// Retrieve line character index state.
    /// </summary>
    property LineCharacterIndex: NativeInt read GetLineCharacterIndex;
    /// <summary>
    /// Does a tab pressed when caret is within indentation indent?
    /// </summary>
    property TabIndents: Boolean read GetTabIndents write SetTabIndents;
  public
  end;

implementation

{ TCustomSciTextEditor }


procedure TCustomSciTextEditor.AddText(ALength: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_ADDTEXT, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.AddStyledText(ALength: TSciPosition; c: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_ADDSTYLEDTEXT, WPARAM(ALength), LPARAM(c));
end;

procedure TCustomSciTextEditor.InsertText(APos: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_INSERTTEXT, WPARAM(APos), LPARAM(AText));
end;

procedure TCustomSciTextEditor.ChangeInsertion(ALength: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_CHANGEINSERTION, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.ClearAll();
begin
  SendScintillaEditorMessage(SCI_CLEARALL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DeleteRange(AStart: TSciPosition; ALengthDelete: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_DELETERANGE, WPARAM(AStart), LPARAM(ALengthDelete));
end;

procedure TCustomSciTextEditor.ClearDocumentStyle();
begin
  SendScintillaEditorMessage(SCI_CLEARDOCUMENTSTYLE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetLength(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETLENGTH, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetCharAt(APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCHARAT, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.GetCurrentPos(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETCURRENTPOS, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetAnchor(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETANCHOR, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetStyleAt(APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSTYLEAT, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.GetStyleIndexAt(APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSTYLEINDEXAT, WPARAM(APos), LPARAM(0));
end;

procedure TCustomSciTextEditor.Redo();
begin
  SendScintillaEditorMessage(SCI_REDO, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUndoCollection(ACollectUndo: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETUNDOCOLLECTION, WPARAM(ACollectUndo), LPARAM(0));
end;

procedure TCustomSciTextEditor.SelectAll();
begin
  SendScintillaEditorMessage(SCI_SELECTALL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSavePoint();
begin
  SendScintillaEditorMessage(SCI_SETSAVEPOINT, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetStyledText(ATr: PSciTextRange): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSTYLEDTEXT, WPARAM(0), LPARAM(ATr));
end;

function TCustomSciTextEditor.GetStyledTextFull(ATr: PSciTextRangeFull): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSTYLEDTEXTFULL, WPARAM(0), LPARAM(ATr));
end;

function TCustomSciTextEditor.CanRedo(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_CANREDO, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.MarkerLineFromHandle(AMarkerHandle: Integer): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERLINEFROMHANDLE, WPARAM(AMarkerHandle), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarkerDeleteHandle(AMarkerHandle: Integer);
begin
  SendScintillaEditorMessage(SCI_MARKERDELETEHANDLE, WPARAM(AMarkerHandle), LPARAM(0));
end;

function TCustomSciTextEditor.MarkerHandleFromLine(ALine: TSciLine; AWhich: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERHANDLEFROMLINE, WPARAM(ALine), LPARAM(AWhich));
end;

function TCustomSciTextEditor.MarkerNumberFromLine(ALine: TSciLine; AWhich: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERNUMBERFROMLINE, WPARAM(ALine), LPARAM(AWhich));
end;

function TCustomSciTextEditor.GetUndoCollection(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETUNDOCOLLECTION, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.GetViewWS(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETVIEWWS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetViewWS(AViewWS: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETVIEWWS, WPARAM(AViewWS), LPARAM(0));
end;

function TCustomSciTextEditor.GetTabDrawMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETTABDRAWMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTabDrawMode(ATabDrawMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETTABDRAWMODE, WPARAM(ATabDrawMode), LPARAM(0));
end;

function TCustomSciTextEditor.PositionFromPoint(x: Integer; y: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONFROMPOINT, WPARAM(x), LPARAM(y));
end;

function TCustomSciTextEditor.PositionFromPointClose(x: Integer; y: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONFROMPOINTCLOSE, WPARAM(x), LPARAM(y));
end;

procedure TCustomSciTextEditor.GotoLine(ALine: TSciLine);
begin
  SendScintillaEditorMessage(SCI_GOTOLINE, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.GotoPos(ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_GOTOPOS, WPARAM(ACaret), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetAnchor(AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETANCHOR, WPARAM(AAnchor), LPARAM(0));
end;

function TCustomSciTextEditor.GetCurLine(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETCURLINE, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetEndStyled(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETENDSTYLED, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ConvertEOLs(AEolMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_CONVERTEOLS, WPARAM(AEolMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetEOLMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETEOLMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetEOLMode(AEolMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETEOLMODE, WPARAM(AEolMode), LPARAM(0));
end;

procedure TCustomSciTextEditor.StartStyling(AStart: TSciPosition; AUnused: Integer);
begin
  SendScintillaEditorMessage(SCI_STARTSTYLING, WPARAM(AStart), LPARAM(AUnused));
end;

procedure TCustomSciTextEditor.SetStyling(ALength: TSciPosition; AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_SETSTYLING, WPARAM(ALength), LPARAM(AStyle));
end;

function TCustomSciTextEditor.GetBufferedDraw(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETBUFFEREDDRAW, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetBufferedDraw(ABuffered: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETBUFFEREDDRAW, WPARAM(ABuffered), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTabWidth(ATabWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETTABWIDTH, WPARAM(ATabWidth), LPARAM(0));
end;

function TCustomSciTextEditor.GetTabWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETTABWIDTH, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTabMinimumWidth(APixels: Integer);
begin
  SendScintillaEditorMessage(SCI_SETTABMINIMUMWIDTH, WPARAM(APixels), LPARAM(0));
end;

function TCustomSciTextEditor.GetTabMinimumWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETTABMINIMUMWIDTH, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ClearTabStops(ALine: TSciLine);
begin
  SendScintillaEditorMessage(SCI_CLEARTABSTOPS, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.AddTabStop(ALine: TSciLine; x: Integer);
begin
  SendScintillaEditorMessage(SCI_ADDTABSTOP, WPARAM(ALine), LPARAM(x));
end;

function TCustomSciTextEditor.GetNextTabStop(ALine: TSciLine; x: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETNEXTTABSTOP, WPARAM(ALine), LPARAM(x));
end;

procedure TCustomSciTextEditor.SetCodePage(ACodePage: Integer);
begin
  SendScintillaEditorMessage(SCI_SETCODEPAGE, WPARAM(ACodePage), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetFontLocale(ALocaleName: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETFONTLOCALE, WPARAM(0), LPARAM(ALocaleName));
end;

function TCustomSciTextEditor.GetFontLocale(ALocaleName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETFONTLOCALE, WPARAM(0), LPARAM(ALocaleName));
end;

function TCustomSciTextEditor.GetIMEInteraction(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETIMEINTERACTION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetIMEInteraction(AImeInteraction: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETIMEINTERACTION, WPARAM(AImeInteraction), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarkerDefine(AMarkerNumber: Integer; AMarkerSymbol: NativeInt);
begin
  SendScintillaEditorMessage(SCI_MARKERDEFINE, WPARAM(AMarkerNumber), LPARAM(AMarkerSymbol));
end;

procedure TCustomSciTextEditor.MarkerSetFore(AMarkerNumber: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_MARKERSETFORE, WPARAM(AMarkerNumber), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.MarkerSetBack(AMarkerNumber: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_MARKERSETBACK, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetBackSelected(AMarkerNumber: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_MARKERSETBACKSELECTED, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetForeTranslucent(AMarkerNumber: Integer; AFore: TColorAlpha);
begin
  SendScintillaEditorMessage(SCI_MARKERSETFORETRANSLUCENT, WPARAM(AMarkerNumber), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.MarkerSetBackTranslucent(AMarkerNumber: Integer; ABack: TColorAlpha);
begin
  SendScintillaEditorMessage(SCI_MARKERSETBACKTRANSLUCENT, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetBackSelectedTranslucent(AMarkerNumber: Integer; ABack: TColorAlpha);
begin
  SendScintillaEditorMessage(SCI_MARKERSETBACKSELECTEDTRANSLUCENT, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetStrokeWidth(AMarkerNumber: Integer; AHundredths: Integer);
begin
  SendScintillaEditorMessage(SCI_MARKERSETSTROKEWIDTH, WPARAM(AMarkerNumber), LPARAM(AHundredths));
end;

procedure TCustomSciTextEditor.MarkerEnableHighlight(AEnabled: Boolean);
begin
  SendScintillaEditorMessage(SCI_MARKERENABLEHIGHLIGHT, WPARAM(AEnabled), LPARAM(0));
end;

function TCustomSciTextEditor.MarkerAdd(ALine: TSciLine; AMarkerNumber: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERADD, WPARAM(ALine), LPARAM(AMarkerNumber));
end;

procedure TCustomSciTextEditor.MarkerDelete(ALine: TSciLine; AMarkerNumber: Integer);
begin
  SendScintillaEditorMessage(SCI_MARKERDELETE, WPARAM(ALine), LPARAM(AMarkerNumber));
end;

procedure TCustomSciTextEditor.MarkerDeleteAll(AMarkerNumber: Integer);
begin
  SendScintillaEditorMessage(SCI_MARKERDELETEALL, WPARAM(AMarkerNumber), LPARAM(0));
end;

function TCustomSciTextEditor.MarkerGet(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERGET, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.MarkerNext(ALineStart: TSciLine; AMarkerMask: Integer): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERNEXT, WPARAM(ALineStart), LPARAM(AMarkerMask));
end;

function TCustomSciTextEditor.MarkerPrevious(ALineStart: TSciLine; AMarkerMask: Integer): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERPREVIOUS, WPARAM(ALineStart), LPARAM(AMarkerMask));
end;

procedure TCustomSciTextEditor.MarkerDefinePixmap(AMarkerNumber: Integer; APixmap: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_MARKERDEFINEPIXMAP, WPARAM(AMarkerNumber), LPARAM(APixmap));
end;

procedure TCustomSciTextEditor.MarkerAddSet(ALine: TSciLine; AMarkerSet: Integer);
begin
  SendScintillaEditorMessage(SCI_MARKERADDSET, WPARAM(ALine), LPARAM(AMarkerSet));
end;

procedure TCustomSciTextEditor.MarkerSetAlpha(AMarkerNumber: Integer; AAlpha: NativeInt);
begin
  SendScintillaEditorMessage(SCI_MARKERSETALPHA, WPARAM(AMarkerNumber), LPARAM(AAlpha));
end;

function TCustomSciTextEditor.MarkerGetLayer(AMarkerNumber: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERGETLAYER, WPARAM(AMarkerNumber), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarkerSetLayer(AMarkerNumber: Integer; ALayer: NativeInt);
begin
  SendScintillaEditorMessage(SCI_MARKERSETLAYER, WPARAM(AMarkerNumber), LPARAM(ALayer));
end;

procedure TCustomSciTextEditor.SetMarginTypeN(AMargin: Integer; AMarginType: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETMARGINTYPEN, WPARAM(AMargin), LPARAM(AMarginType));
end;

function TCustomSciTextEditor.GetMarginTypeN(AMargin: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINTYPEN, WPARAM(AMargin), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginWidthN(AMargin: Integer; APixelWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMARGINWIDTHN, WPARAM(AMargin), LPARAM(APixelWidth));
end;

function TCustomSciTextEditor.GetMarginWidthN(AMargin: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINWIDTHN, WPARAM(AMargin), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginMaskN(AMargin: Integer; AMask: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMARGINMASKN, WPARAM(AMargin), LPARAM(AMask));
end;

function TCustomSciTextEditor.GetMarginMaskN(AMargin: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINMASKN, WPARAM(AMargin), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginSensitiveN(AMargin: Integer; ASensitive: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETMARGINSENSITIVEN, WPARAM(AMargin), LPARAM(ASensitive));
end;

function TCustomSciTextEditor.GetMarginSensitiveN(AMargin: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMARGINSENSITIVEN, WPARAM(AMargin), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetMarginCursorN(AMargin: Integer; ACursor: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETMARGINCURSORN, WPARAM(AMargin), LPARAM(ACursor));
end;

function TCustomSciTextEditor.GetMarginCursorN(AMargin: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINCURSORN, WPARAM(AMargin), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginBackN(AMargin: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETMARGINBACKN, WPARAM(AMargin), LPARAM(ABack));
end;

function TCustomSciTextEditor.GetMarginBackN(AMargin: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINBACKN, WPARAM(AMargin), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMargins(AMargins: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMARGINS, WPARAM(AMargins), LPARAM(0));
end;

function TCustomSciTextEditor.GetMargins(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleClearAll();
begin
  SendScintillaEditorMessage(SCI_STYLECLEARALL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleSetFore(AStyle: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_STYLESETFORE, WPARAM(AStyle), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.StyleSetBack(AStyle: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_STYLESETBACK, WPARAM(AStyle), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.StyleSetBold(AStyle: Integer; ABold: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETBOLD, WPARAM(AStyle), LPARAM(ABold));
end;

procedure TCustomSciTextEditor.StyleSetItalic(AStyle: Integer; AItalic: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETITALIC, WPARAM(AStyle), LPARAM(AItalic));
end;

procedure TCustomSciTextEditor.StyleSetSize(AStyle: Integer; ASizePoints: Integer);
begin
  SendScintillaEditorMessage(SCI_STYLESETSIZE, WPARAM(AStyle), LPARAM(ASizePoints));
end;

procedure TCustomSciTextEditor.StyleSetFont(AStyle: Integer; AFontName: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_STYLESETFONT, WPARAM(AStyle), LPARAM(AFontName));
end;

procedure TCustomSciTextEditor.StyleSetEOLFilled(AStyle: Integer; AEolFilled: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETEOLFILLED, WPARAM(AStyle), LPARAM(AEolFilled));
end;

procedure TCustomSciTextEditor.StyleResetDefault();
begin
  SendScintillaEditorMessage(SCI_STYLERESETDEFAULT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleSetUnderline(AStyle: Integer; AUnderline: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETUNDERLINE, WPARAM(AStyle), LPARAM(AUnderline));
end;

function TCustomSciTextEditor.StyleGetFore(AStyle: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETFORE, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.StyleGetBack(AStyle: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETBACK, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.StyleGetBold(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETBOLD, WPARAM(AStyle), LPARAM(0)));
end;

function TCustomSciTextEditor.StyleGetItalic(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETITALIC, WPARAM(AStyle), LPARAM(0)));
end;

function TCustomSciTextEditor.StyleGetSize(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETSIZE, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.StyleGetFont(AStyle: Integer; AFontName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETFONT, WPARAM(AStyle), LPARAM(AFontName));
end;

function TCustomSciTextEditor.StyleGetEOLFilled(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETEOLFILLED, WPARAM(AStyle), LPARAM(0)));
end;

function TCustomSciTextEditor.StyleGetUnderline(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETUNDERLINE, WPARAM(AStyle), LPARAM(0)));
end;

function TCustomSciTextEditor.StyleGetCase(AStyle: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETCASE, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.StyleGetCharacterSet(AStyle: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETCHARACTERSET, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.StyleGetVisible(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETVISIBLE, WPARAM(AStyle), LPARAM(0)));
end;

function TCustomSciTextEditor.StyleGetChangeable(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETCHANGEABLE, WPARAM(AStyle), LPARAM(0)));
end;

function TCustomSciTextEditor.StyleGetHotSpot(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETHOTSPOT, WPARAM(AStyle), LPARAM(0)));
end;

procedure TCustomSciTextEditor.StyleSetCase(AStyle: Integer; ACaseVisible: NativeInt);
begin
  SendScintillaEditorMessage(SCI_STYLESETCASE, WPARAM(AStyle), LPARAM(ACaseVisible));
end;

procedure TCustomSciTextEditor.StyleSetSizeFractional(AStyle: Integer; ASizeHundredthPoints: Integer);
begin
  SendScintillaEditorMessage(SCI_STYLESETSIZEFRACTIONAL, WPARAM(AStyle), LPARAM(ASizeHundredthPoints));
end;

function TCustomSciTextEditor.StyleGetSizeFractional(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETSIZEFRACTIONAL, WPARAM(AStyle), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleSetWeight(AStyle: Integer; AWeight: NativeInt);
begin
  SendScintillaEditorMessage(SCI_STYLESETWEIGHT, WPARAM(AStyle), LPARAM(AWeight));
end;

function TCustomSciTextEditor.StyleGetWeight(AStyle: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETWEIGHT, WPARAM(AStyle), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleSetCharacterSet(AStyle: Integer; ACharacterSet: NativeInt);
begin
  SendScintillaEditorMessage(SCI_STYLESETCHARACTERSET, WPARAM(AStyle), LPARAM(ACharacterSet));
end;

procedure TCustomSciTextEditor.StyleSetHotSpot(AStyle: Integer; AHotspot: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETHOTSPOT, WPARAM(AStyle), LPARAM(AHotspot));
end;

procedure TCustomSciTextEditor.StyleSetCheckMonospaced(AStyle: Integer; ACheckMonospaced: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETCHECKMONOSPACED, WPARAM(AStyle), LPARAM(ACheckMonospaced));
end;

function TCustomSciTextEditor.StyleGetCheckMonospaced(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_STYLEGETCHECKMONOSPACED, WPARAM(AStyle), LPARAM(0)));
end;

procedure TCustomSciTextEditor.StyleSetStretch(AStyle: Integer; AStretch: NativeInt);
begin
  SendScintillaEditorMessage(SCI_STYLESETSTRETCH, WPARAM(AStyle), LPARAM(AStretch));
end;

function TCustomSciTextEditor.StyleGetStretch(AStyle: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETSTRETCH, WPARAM(AStyle), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleSetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_STYLESETINVISIBLEREPRESENTATION, WPARAM(AStyle), LPARAM(ARepresentation));
end;

function TCustomSciTextEditor.StyleGetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_STYLEGETINVISIBLEREPRESENTATION, WPARAM(AStyle), LPARAM(ARepresentation));
end;

procedure TCustomSciTextEditor.SetElementColour(AElement: NativeInt; AColourElement: TColorAlpha);
begin
  SendScintillaEditorMessage(SCI_SETELEMENTCOLOUR, WPARAM(AElement), LPARAM(AColourElement));
end;

function TCustomSciTextEditor.GetElementColour(AElement: NativeInt): TColorAlpha;
begin
  Result := SendScintillaEditorMessage(SCI_GETELEMENTCOLOUR, WPARAM(AElement), LPARAM(0));
end;

procedure TCustomSciTextEditor.ResetElementColour(AElement: NativeInt);
begin
  SendScintillaEditorMessage(SCI_RESETELEMENTCOLOUR, WPARAM(AElement), LPARAM(0));
end;

function TCustomSciTextEditor.GetElementIsSet(AElement: NativeInt): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETELEMENTISSET, WPARAM(AElement), LPARAM(0)));
end;

function TCustomSciTextEditor.GetElementAllowsTranslucent(AElement: NativeInt): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETELEMENTALLOWSTRANSLUCENT, WPARAM(AElement), LPARAM(0)));
end;

function TCustomSciTextEditor.GetElementBaseColour(AElement: NativeInt): TColorAlpha;
begin
  Result := SendScintillaEditorMessage(SCI_GETELEMENTBASECOLOUR, WPARAM(AElement), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETSELFORE, WPARAM(AUseSetting), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.SetSelBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETSELBACK, WPARAM(AUseSetting), LPARAM(ABack));
end;

function TCustomSciTextEditor.GetSelAlpha(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELALPHA, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelAlpha(AAlpha: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETSELALPHA, WPARAM(AAlpha), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelEOLFilled(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETSELEOLFILLED, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetSelEOLFilled(AFilled: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETSELEOLFILLED, WPARAM(AFilled), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionLayer(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONLAYER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionLayer(ALayer: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONLAYER, WPARAM(ALayer), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineLayer(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETLINELAYER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretLineLayer(ALayer: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINELAYER, WPARAM(ALayer), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineHighlightSubLine(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETCARETLINEHIGHLIGHTSUBLINE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetCaretLineHighlightSubLine(ASubLine: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINEHIGHLIGHTSUBLINE, WPARAM(ASubLine), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretFore(AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETCARETFORE, WPARAM(AFore), LPARAM(0));
end;

procedure TCustomSciTextEditor.AssignCmdKey(AKeyDefinition: TSciKeyModifies; ASciCommand: Integer);
begin
  SendScintillaEditorMessage(SCI_ASSIGNCMDKEY, WPARAM(AKeyDefinition), LPARAM(ASciCommand));
end;

procedure TCustomSciTextEditor.ClearCmdKey(AKeyDefinition: TSciKeyModifies);
begin
  SendScintillaEditorMessage(SCI_CLEARCMDKEY, WPARAM(AKeyDefinition), LPARAM(0));
end;

procedure TCustomSciTextEditor.ClearAllCmdKeys();
begin
  SendScintillaEditorMessage(SCI_CLEARALLCMDKEYS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetStylingEx(ALength: TSciPosition; AStyles: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETSTYLINGEX, WPARAM(ALength), LPARAM(AStyles));
end;

procedure TCustomSciTextEditor.StyleSetVisible(AStyle: Integer; AVisible: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETVISIBLE, WPARAM(AStyle), LPARAM(AVisible));
end;

function TCustomSciTextEditor.GetCaretPeriod(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETPERIOD, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretPeriod(APeriodMilliseconds: Integer);
begin
  SendScintillaEditorMessage(SCI_SETCARETPERIOD, WPARAM(APeriodMilliseconds), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWordChars(ACharacters: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETWORDCHARS, WPARAM(0), LPARAM(ACharacters));
end;

function TCustomSciTextEditor.GetWordChars(ACharacters: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETWORDCHARS, WPARAM(0), LPARAM(ACharacters));
end;

procedure TCustomSciTextEditor.SetCharacterCategoryOptimization(ACountCharacters: Integer);
begin
  SendScintillaEditorMessage(SCI_SETCHARACTERCATEGORYOPTIMIZATION, WPARAM(ACountCharacters), LPARAM(0));
end;

function TCustomSciTextEditor.GetCharacterCategoryOptimization(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCHARACTERCATEGORYOPTIMIZATION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.BeginUndoAction();
begin
  SendScintillaEditorMessage(SCI_BEGINUNDOACTION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.EndUndoAction();
begin
  SendScintillaEditorMessage(SCI_ENDUNDOACTION, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoSequence(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOSEQUENCE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoActions(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOACTIONS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUndoSavePoint(AAction: Integer);
begin
  SendScintillaEditorMessage(SCI_SETUNDOSAVEPOINT, WPARAM(AAction), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoSavePoint(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOSAVEPOINT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUndoDetach(AAction: Integer);
begin
  SendScintillaEditorMessage(SCI_SETUNDODETACH, WPARAM(AAction), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoDetach(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDODETACH, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUndoTentative(AAction: Integer);
begin
  SendScintillaEditorMessage(SCI_SETUNDOTENTATIVE, WPARAM(AAction), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoTentative(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOTENTATIVE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUndoCurrent(AAction: Integer);
begin
  SendScintillaEditorMessage(SCI_SETUNDOCURRENT, WPARAM(AAction), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoCurrent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOCURRENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PushUndoActionType(AType: Integer; APos: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_PUSHUNDOACTIONTYPE, WPARAM(AType), LPARAM(APos));
end;

procedure TCustomSciTextEditor.ChangeLastUndoActionText(ALength: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_CHANGELASTUNDOACTIONTEXT, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetUndoActionType(AAction: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOACTIONTYPE, WPARAM(AAction), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoActionPosition(AAction: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOACTIONPOSITION, WPARAM(AAction), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoActionText(AAction: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOACTIONTEXT, WPARAM(AAction), LPARAM(AText));
end;

procedure TCustomSciTextEditor.IndicSetStyle(AIndicator: Integer; AIndicatorStyle: NativeInt);
begin
  SendScintillaEditorMessage(SCI_INDICSETSTYLE, WPARAM(AIndicator), LPARAM(AIndicatorStyle));
end;

function TCustomSciTextEditor.IndicGetStyle(AIndicator: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETSTYLE, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetFore(AIndicator: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_INDICSETFORE, WPARAM(AIndicator), LPARAM(AFore));
end;

function TCustomSciTextEditor.IndicGetFore(AIndicator: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETFORE, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetUnder(AIndicator: Integer; AUnder: Boolean);
begin
  SendScintillaEditorMessage(SCI_INDICSETUNDER, WPARAM(AIndicator), LPARAM(AUnder));
end;

function TCustomSciTextEditor.IndicGetUnder(AIndicator: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_INDICGETUNDER, WPARAM(AIndicator), LPARAM(0)));
end;

procedure TCustomSciTextEditor.IndicSetHoverStyle(AIndicator: Integer; AIndicatorStyle: NativeInt);
begin
  SendScintillaEditorMessage(SCI_INDICSETHOVERSTYLE, WPARAM(AIndicator), LPARAM(AIndicatorStyle));
end;

function TCustomSciTextEditor.IndicGetHoverStyle(AIndicator: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETHOVERSTYLE, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetHoverFore(AIndicator: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_INDICSETHOVERFORE, WPARAM(AIndicator), LPARAM(AFore));
end;

function TCustomSciTextEditor.IndicGetHoverFore(AIndicator: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETHOVERFORE, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetFlags(AIndicator: Integer; AFlags: NativeInt);
begin
  SendScintillaEditorMessage(SCI_INDICSETFLAGS, WPARAM(AIndicator), LPARAM(AFlags));
end;

function TCustomSciTextEditor.IndicGetFlags(AIndicator: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETFLAGS, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetStrokeWidth(AIndicator: Integer; AHundredths: Integer);
begin
  SendScintillaEditorMessage(SCI_INDICSETSTROKEWIDTH, WPARAM(AIndicator), LPARAM(AHundredths));
end;

function TCustomSciTextEditor.IndicGetStrokeWidth(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETSTROKEWIDTH, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWhitespaceFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETWHITESPACEFORE, WPARAM(AUseSetting), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.SetWhitespaceBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETWHITESPACEBACK, WPARAM(AUseSetting), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.SetWhitespaceSize(ASize: Integer);
begin
  SendScintillaEditorMessage(SCI_SETWHITESPACESIZE, WPARAM(ASize), LPARAM(0));
end;

function TCustomSciTextEditor.GetWhitespaceSize(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETWHITESPACESIZE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetLineState(ALine: TSciLine; AState: Integer);
begin
  SendScintillaEditorMessage(SCI_SETLINESTATE, WPARAM(ALine), LPARAM(AState));
end;

function TCustomSciTextEditor.GetLineState(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINESTATE, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.GetMaxLineState(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMAXLINESTATE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineVisible(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETCARETLINEVISIBLE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetCaretLineVisible(AShow: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINEVISIBLE, WPARAM(AShow), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineBack(): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETLINEBACK, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretLineBack(ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINEBACK, WPARAM(ABack), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineFrame(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETLINEFRAME, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretLineFrame(AWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINEFRAME, WPARAM(AWidth), LPARAM(0));
end;

procedure TCustomSciTextEditor.StyleSetChangeable(AStyle: Integer; AChangeable: Boolean);
begin
  SendScintillaEditorMessage(SCI_STYLESETCHANGEABLE, WPARAM(AStyle), LPARAM(AChangeable));
end;

procedure TCustomSciTextEditor.AutoCShow(ALengthEntered: TSciPosition; AItemList: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_AUTOCSHOW, WPARAM(ALengthEntered), LPARAM(AItemList));
end;

procedure TCustomSciTextEditor.AutoCCancel();
begin
  SendScintillaEditorMessage(SCI_AUTOCCANCEL, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCActive(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_AUTOCACTIVE, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.AutoCPosStart(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCPOSSTART, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCComplete();
begin
  SendScintillaEditorMessage(SCI_AUTOCCOMPLETE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCStops(ACharacterSet: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_AUTOCSTOPS, WPARAM(0), LPARAM(ACharacterSet));
end;

procedure TCustomSciTextEditor.AutoCSetSeparator(ASeparatorCharacter: Integer);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETSEPARATOR, WPARAM(ASeparatorCharacter), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetSeparator(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETSEPARATOR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSelect(ASelect: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_AUTOCSELECT, WPARAM(0), LPARAM(ASelect));
end;

procedure TCustomSciTextEditor.AutoCSetCancelAtStart(ACancel: Boolean);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETCANCELATSTART, WPARAM(ACancel), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetCancelAtStart(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_AUTOCGETCANCELATSTART, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.AutoCSetFillUps(ACharacterSet: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETFILLUPS, WPARAM(0), LPARAM(ACharacterSet));
end;

procedure TCustomSciTextEditor.AutoCSetChooseSingle(AChooseSingle: Boolean);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETCHOOSESINGLE, WPARAM(AChooseSingle), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetChooseSingle(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_AUTOCGETCHOOSESINGLE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.AutoCSetIgnoreCase(AIgnoreCase: Boolean);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETIGNORECASE, WPARAM(AIgnoreCase), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetIgnoreCase(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_AUTOCGETIGNORECASE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.UserListShow(AListType: Integer; AItemList: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_USERLISTSHOW, WPARAM(AListType), LPARAM(AItemList));
end;

procedure TCustomSciTextEditor.AutoCSetAutoHide(AAutoHide: Boolean);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETAUTOHIDE, WPARAM(AAutoHide), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetAutoHide(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_AUTOCGETAUTOHIDE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.AutoCSetOptions(AOptions: NativeInt);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETOPTIONS, WPARAM(AOptions), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetOptions(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETOPTIONS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetDropRestOfWord(ADropRestOfWord: Boolean);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETDROPRESTOFWORD, WPARAM(ADropRestOfWord), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetDropRestOfWord(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_AUTOCGETDROPRESTOFWORD, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.RegisterImage(AType: Integer; AXpmData: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_REGISTERIMAGE, WPARAM(AType), LPARAM(AXpmData));
end;

procedure TCustomSciTextEditor.ClearRegisteredImages();
begin
  SendScintillaEditorMessage(SCI_CLEARREGISTEREDIMAGES, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetTypeSeparator(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETTYPESEPARATOR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetTypeSeparator(ASeparatorCharacter: Integer);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETTYPESEPARATOR, WPARAM(ASeparatorCharacter), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetMaxWidth(ACharacterCount: Integer);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETMAXWIDTH, WPARAM(ACharacterCount), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetMaxWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETMAXWIDTH, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetMaxHeight(ARowCount: Integer);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETMAXHEIGHT, WPARAM(ARowCount), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetMaxHeight(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETMAXHEIGHT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetStyle(AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETSTYLE, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetStyle(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETSTYLE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetIndent(AIndentSize: Integer);
begin
  SendScintillaEditorMessage(SCI_SETINDENT, WPARAM(AIndentSize), LPARAM(0));
end;

function TCustomSciTextEditor.GetIndent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETINDENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUseTabs(AUseTabs: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETUSETABS, WPARAM(AUseTabs), LPARAM(0));
end;

function TCustomSciTextEditor.GetUseTabs(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETUSETABS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetLineIndentation(ALine: TSciLine; AIndentation: Integer);
begin
  SendScintillaEditorMessage(SCI_SETLINEINDENTATION, WPARAM(ALine), LPARAM(AIndentation));
end;

function TCustomSciTextEditor.GetLineIndentation(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINEINDENTATION, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.GetLineIndentPosition(ALine: TSciLine): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINEINDENTPOSITION, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.GetColumn(APos: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETCOLUMN, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.CountCharacters(AStart: TSciPosition; AEnd: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_COUNTCHARACTERS, WPARAM(AStart), LPARAM(AEnd));
end;

function TCustomSciTextEditor.CountCodeUnits(AStart: TSciPosition; AEnd: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_COUNTCODEUNITS, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.SetHScrollBar(AVisible: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETHSCROLLBAR, WPARAM(AVisible), LPARAM(0));
end;

function TCustomSciTextEditor.GetHScrollBar(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETHSCROLLBAR, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetIndentationGuides(AIndentView: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETINDENTATIONGUIDES, WPARAM(AIndentView), LPARAM(0));
end;

function TCustomSciTextEditor.GetIndentationGuides(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETINDENTATIONGUIDES, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetHighlightGuide(AColumn: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETHIGHLIGHTGUIDE, WPARAM(AColumn), LPARAM(0));
end;

function TCustomSciTextEditor.GetHighlightGuide(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETHIGHLIGHTGUIDE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetLineEndPosition(ALine: TSciLine): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINEENDPOSITION, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.GetCodePage(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCODEPAGE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretFore(): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETFORE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetReadOnly(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETREADONLY, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetCurrentPos(ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETCURRENTPOS, WPARAM(ACaret), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionStart(AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONSTART, WPARAM(AAnchor), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionStart(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONSTART, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionEnd(ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONEND, WPARAM(ACaret), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionEnd(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetEmptySelection(ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETEMPTYSELECTION, WPARAM(ACaret), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetPrintMagnification(AMagnification: Integer);
begin
  SendScintillaEditorMessage(SCI_SETPRINTMAGNIFICATION, WPARAM(AMagnification), LPARAM(0));
end;

function TCustomSciTextEditor.GetPrintMagnification(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPRINTMAGNIFICATION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetPrintColourMode(AMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETPRINTCOLOURMODE, WPARAM(AMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetPrintColourMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETPRINTCOLOURMODE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.FindText(ASearchFlags: NativeInt; AFt: PSciFindText): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_FINDTEXT, WPARAM(ASearchFlags), LPARAM(AFt));
end;

function TCustomSciTextEditor.FindTextFull(ASearchFlags: NativeInt; AFt: PSciFindTextFull): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_FINDTEXTFULL, WPARAM(ASearchFlags), LPARAM(AFt));
end;

function TCustomSciTextEditor.FormatRange(ADraw: Boolean; AFr: PSciRangeToFormat): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_FORMATRANGE, WPARAM(ADraw), LPARAM(AFr));
end;

function TCustomSciTextEditor.FormatRangeFull(ADraw: Boolean; AFr: PSciRangeToFormatFull): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_FORMATRANGEFULL, WPARAM(ADraw), LPARAM(AFr));
end;

procedure TCustomSciTextEditor.SetChangeHistory(AChangeHistory: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETCHANGEHISTORY, WPARAM(AChangeHistory), LPARAM(0));
end;

function TCustomSciTextEditor.GetChangeHistory(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETCHANGEHISTORY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetUndoSelectionHistory(AUndoSelectionHistory: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETUNDOSELECTIONHISTORY, WPARAM(AUndoSelectionHistory), LPARAM(0));
end;

function TCustomSciTextEditor.GetUndoSelectionHistory(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETUNDOSELECTIONHISTORY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionSerialized(ASelectionString: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONSERIALIZED, WPARAM(0), LPARAM(ASelectionString));
end;

function TCustomSciTextEditor.GetSelectionSerialized(ASelectionString: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONSERIALIZED, WPARAM(0), LPARAM(ASelectionString));
end;

function TCustomSciTextEditor.GetFirstVisibleLine(): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_GETFIRSTVISIBLELINE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetLine(ALine: TSciLine; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINE, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.GetLineCount(): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINECOUNT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AllocateLines(ALines: TSciLine);
begin
  SendScintillaEditorMessage(SCI_ALLOCATELINES, WPARAM(ALines), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginLeft(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMARGINLEFT, WPARAM(0), LPARAM(APixelWidth));
end;

function TCustomSciTextEditor.GetMarginLeft(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINLEFT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginRight(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMARGINRIGHT, WPARAM(0), LPARAM(APixelWidth));
end;

function TCustomSciTextEditor.GetMarginRight(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINRIGHT, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetModify(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMODIFY, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetSel(AAnchor: TSciPosition; ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSEL, WPARAM(AAnchor), LPARAM(ACaret));
end;

function TCustomSciTextEditor.GetSelText(AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELTEXT, WPARAM(0), LPARAM(AText));
end;

function TCustomSciTextEditor.GetTextRange(ATr: PSciTextRange): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTEXTRANGE, WPARAM(0), LPARAM(ATr));
end;

function TCustomSciTextEditor.GetTextRangeFull(ATr: PSciTextRangeFull): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTEXTRANGEFULL, WPARAM(0), LPARAM(ATr));
end;

procedure TCustomSciTextEditor.HideSelection(AHide: Boolean);
begin
  SendScintillaEditorMessage(SCI_HIDESELECTION, WPARAM(AHide), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionHidden(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETSELECTIONHIDDEN, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.PointXFromPosition(APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_POINTXFROMPOSITION, WPARAM(0), LPARAM(APos));
end;

function TCustomSciTextEditor.PointYFromPosition(APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_POINTYFROMPOSITION, WPARAM(0), LPARAM(APos));
end;

function TCustomSciTextEditor.LineFromPosition(APos: TSciPosition): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_LINEFROMPOSITION, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.PositionFromLine(ALine: TSciLine): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONFROMLINE, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineScroll(AColumns: TSciPosition; ALines: TSciLine);
begin
  SendScintillaEditorMessage(SCI_LINESCROLL, WPARAM(AColumns), LPARAM(ALines));
end;

procedure TCustomSciTextEditor.ScrollCaret();
begin
  SendScintillaEditorMessage(SCI_SCROLLCARET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ScrollRange(ASecondary: TSciPosition; APrimary: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SCROLLRANGE, WPARAM(ASecondary), LPARAM(APrimary));
end;

procedure TCustomSciTextEditor.ReplaceSel(AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_REPLACESEL, WPARAM(0), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetReadOnly(AReadOnly: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETREADONLY, WPARAM(AReadOnly), LPARAM(0));
end;

procedure TCustomSciTextEditor.Null();
begin
  SendScintillaEditorMessage(SCI_NULL, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.CanPaste(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_CANPASTE, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.CanUndo(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_CANUNDO, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.EmptyUndoBuffer();
begin
  SendScintillaEditorMessage(SCI_EMPTYUNDOBUFFER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Undo();
begin
  SendScintillaEditorMessage(SCI_UNDO, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Cut();
begin
  SendScintillaEditorMessage(SCI_CUT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Copy();
begin
  SendScintillaEditorMessage(SCI_COPY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Paste();
begin
  SendScintillaEditorMessage(SCI_PASTE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Clear();
begin
  SendScintillaEditorMessage(SCI_CLEAR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetText(AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETTEXT, WPARAM(0), LPARAM(AText));
end;

function TCustomSciTextEditor.GetText(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTEXT, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetTextLength(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTEXTLENGTH, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetDirectFunction(): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_GETDIRECTFUNCTION, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.GetDirectStatusFunction(): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_GETDIRECTSTATUSFUNCTION, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.GetDirectPointer(): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_GETDIRECTPOINTER, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetOvertype(AOverType: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETOVERTYPE, WPARAM(AOverType), LPARAM(0));
end;

function TCustomSciTextEditor.GetOvertype(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETOVERTYPE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetCaretWidth(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETCARETWIDTH, WPARAM(APixelWidth), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETWIDTH, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTargetStart(AStart: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETTARGETSTART, WPARAM(AStart), LPARAM(0));
end;

function TCustomSciTextEditor.GetTargetStart(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTARGETSTART, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTargetStartVirtualSpace(ASpace: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETTARGETSTARTVIRTUALSPACE, WPARAM(ASpace), LPARAM(0));
end;

function TCustomSciTextEditor.GetTargetStartVirtualSpace(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTARGETSTARTVIRTUALSPACE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTargetEnd(AEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETTARGETEND, WPARAM(AEnd), LPARAM(0));
end;

function TCustomSciTextEditor.GetTargetEnd(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTARGETEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTargetEndVirtualSpace(ASpace: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETTARGETENDVIRTUALSPACE, WPARAM(ASpace), LPARAM(0));
end;

function TCustomSciTextEditor.GetTargetEndVirtualSpace(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTARGETENDVIRTUALSPACE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTargetRange(AStart: TSciPosition; AEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETTARGETRANGE, WPARAM(AStart), LPARAM(AEnd));
end;

function TCustomSciTextEditor.GetTargetText(AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETTARGETTEXT, WPARAM(0), LPARAM(AText));
end;

procedure TCustomSciTextEditor.TargetFromSelection();
begin
  SendScintillaEditorMessage(SCI_TARGETFROMSELECTION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.TargetWholeDocument();
begin
  SendScintillaEditorMessage(SCI_TARGETWHOLEDOCUMENT, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.ReplaceTarget(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_REPLACETARGET, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.ReplaceTargetRE(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_REPLACETARGETRE, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.ReplaceTargetMinimal(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_REPLACETARGETMINIMAL, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.SearchInTarget(ALength: TSciPosition; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_SEARCHINTARGET, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetSearchFlags(ASearchFlags: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETSEARCHFLAGS, WPARAM(ASearchFlags), LPARAM(0));
end;

function TCustomSciTextEditor.GetSearchFlags(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETSEARCHFLAGS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipShow(APos: TSciPosition; ADefinition: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSHOW, WPARAM(APos), LPARAM(ADefinition));
end;

procedure TCustomSciTextEditor.CallTipCancel();
begin
  SendScintillaEditorMessage(SCI_CALLTIPCANCEL, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.CallTipActive(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_CALLTIPACTIVE, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.CallTipPosStart(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_CALLTIPPOSSTART, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipSetPosStart(APosStart: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSETPOSSTART, WPARAM(APosStart), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipSetHlt(AHighlightStart: TSciPosition; AHighlightEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSETHLT, WPARAM(AHighlightStart), LPARAM(AHighlightEnd));
end;

procedure TCustomSciTextEditor.CallTipSetBack(ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSETBACK, WPARAM(ABack), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipSetFore(AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSETFORE, WPARAM(AFore), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipSetForeHlt(AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSETFOREHLT, WPARAM(AFore), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipUseStyle(ATabSize: Integer);
begin
  SendScintillaEditorMessage(SCI_CALLTIPUSESTYLE, WPARAM(ATabSize), LPARAM(0));
end;

procedure TCustomSciTextEditor.CallTipSetPosition(AAbove: Boolean);
begin
  SendScintillaEditorMessage(SCI_CALLTIPSETPOSITION, WPARAM(AAbove), LPARAM(0));
end;

function TCustomSciTextEditor.VisibleFromDocLine(ADocLine: TSciLine): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_VISIBLEFROMDOCLINE, WPARAM(ADocLine), LPARAM(0));
end;

function TCustomSciTextEditor.DocLineFromVisible(ADisplayLine: TSciLine): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_DOCLINEFROMVISIBLE, WPARAM(ADisplayLine), LPARAM(0));
end;

function TCustomSciTextEditor.WrapCount(ADocLine: TSciLine): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_WRAPCOUNT, WPARAM(ADocLine), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetFoldLevel(ALine: TSciLine; ALevel: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETFOLDLEVEL, WPARAM(ALine), LPARAM(ALevel));
end;

function TCustomSciTextEditor.GetFoldLevel(ALine: TSciLine): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETFOLDLEVEL, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.GetLastChild(ALine: TSciLine; ALevel: NativeInt): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_GETLASTCHILD, WPARAM(ALine), LPARAM(ALevel));
end;

function TCustomSciTextEditor.GetFoldParent(ALine: TSciLine): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_GETFOLDPARENT, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.ShowLines(ALineStart: TSciLine; ALineEnd: TSciLine);
begin
  SendScintillaEditorMessage(SCI_SHOWLINES, WPARAM(ALineStart), LPARAM(ALineEnd));
end;

procedure TCustomSciTextEditor.HideLines(ALineStart: TSciLine; ALineEnd: TSciLine);
begin
  SendScintillaEditorMessage(SCI_HIDELINES, WPARAM(ALineStart), LPARAM(ALineEnd));
end;

function TCustomSciTextEditor.GetLineVisible(ALine: TSciLine): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETLINEVISIBLE, WPARAM(ALine), LPARAM(0)));
end;

function TCustomSciTextEditor.GetAllLinesVisible(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETALLLINESVISIBLE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetFoldExpanded(ALine: TSciLine; AExpanded: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETFOLDEXPANDED, WPARAM(ALine), LPARAM(AExpanded));
end;

function TCustomSciTextEditor.GetFoldExpanded(ALine: TSciLine): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETFOLDEXPANDED, WPARAM(ALine), LPARAM(0)));
end;

procedure TCustomSciTextEditor.ToggleFold(ALine: TSciLine);
begin
  SendScintillaEditorMessage(SCI_TOGGLEFOLD, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.ToggleFoldShowText(ALine: TSciLine; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_TOGGLEFOLDSHOWTEXT, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.FoldDisplayTextSetStyle(AStyle: NativeInt);
begin
  SendScintillaEditorMessage(SCI_FOLDDISPLAYTEXTSETSTYLE, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.FoldDisplayTextGetStyle(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_FOLDDISPLAYTEXTGETSTYLE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetDefaultFoldDisplayText(AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETDEFAULTFOLDDISPLAYTEXT, WPARAM(0), LPARAM(AText));
end;

function TCustomSciTextEditor.GetDefaultFoldDisplayText(AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETDEFAULTFOLDDISPLAYTEXT, WPARAM(0), LPARAM(AText));
end;

procedure TCustomSciTextEditor.FoldLine(ALine: TSciLine; AAction: NativeInt);
begin
  SendScintillaEditorMessage(SCI_FOLDLINE, WPARAM(ALine), LPARAM(AAction));
end;

procedure TCustomSciTextEditor.FoldChildren(ALine: TSciLine; AAction: NativeInt);
begin
  SendScintillaEditorMessage(SCI_FOLDCHILDREN, WPARAM(ALine), LPARAM(AAction));
end;

procedure TCustomSciTextEditor.ExpandChildren(ALine: TSciLine; ALevel: NativeInt);
begin
  SendScintillaEditorMessage(SCI_EXPANDCHILDREN, WPARAM(ALine), LPARAM(ALevel));
end;

procedure TCustomSciTextEditor.FoldAll(AAction: NativeInt);
begin
  SendScintillaEditorMessage(SCI_FOLDALL, WPARAM(AAction), LPARAM(0));
end;

procedure TCustomSciTextEditor.EnsureVisible(ALine: TSciLine);
begin
  SendScintillaEditorMessage(SCI_ENSUREVISIBLE, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetAutomaticFold(AAutomaticFold: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETAUTOMATICFOLD, WPARAM(AAutomaticFold), LPARAM(0));
end;

function TCustomSciTextEditor.GetAutomaticFold(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETAUTOMATICFOLD, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetFoldFlags(AFlags: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETFOLDFLAGS, WPARAM(AFlags), LPARAM(0));
end;

procedure TCustomSciTextEditor.EnsureVisibleEnforcePolicy(ALine: TSciLine);
begin
  SendScintillaEditorMessage(SCI_ENSUREVISIBLEENFORCEPOLICY, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTabIndents(ATabIndents: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETTABINDENTS, WPARAM(ATabIndents), LPARAM(0));
end;

function TCustomSciTextEditor.GetTabIndents(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETTABINDENTS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetBackSpaceUnIndents(ABsUnIndents: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETBACKSPACEUNINDENTS, WPARAM(ABsUnIndents), LPARAM(0));
end;

function TCustomSciTextEditor.GetBackSpaceUnIndents(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETBACKSPACEUNINDENTS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetMouseDwellTime(APeriodMilliseconds: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMOUSEDWELLTIME, WPARAM(APeriodMilliseconds), LPARAM(0));
end;

function TCustomSciTextEditor.GetMouseDwellTime(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMOUSEDWELLTIME, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.WordStartPosition(APos: TSciPosition; AOnlyWordCharacters: Boolean): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_WORDSTARTPOSITION, WPARAM(APos), LPARAM(AOnlyWordCharacters));
end;

function TCustomSciTextEditor.WordEndPosition(APos: TSciPosition; AOnlyWordCharacters: Boolean): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_WORDENDPOSITION, WPARAM(APos), LPARAM(AOnlyWordCharacters));
end;

function TCustomSciTextEditor.IsRangeWord(AStart: TSciPosition; AEnd: TSciPosition): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_ISRANGEWORD, WPARAM(AStart), LPARAM(AEnd)));
end;

procedure TCustomSciTextEditor.SetIdleStyling(AIdleStyling: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETIDLESTYLING, WPARAM(AIdleStyling), LPARAM(0));
end;

function TCustomSciTextEditor.GetIdleStyling(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETIDLESTYLING, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWrapMode(AWrapMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETWRAPMODE, WPARAM(AWrapMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetWrapMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETWRAPMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWrapVisualFlags(AWrapVisualFlags: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETWRAPVISUALFLAGS, WPARAM(AWrapVisualFlags), LPARAM(0));
end;

function TCustomSciTextEditor.GetWrapVisualFlags(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETWRAPVISUALFLAGS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWrapVisualFlagsLocation(AWrapVisualFlagsLocation: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETWRAPVISUALFLAGSLOCATION, WPARAM(AWrapVisualFlagsLocation), LPARAM(0));
end;

function TCustomSciTextEditor.GetWrapVisualFlagsLocation(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETWRAPVISUALFLAGSLOCATION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWrapStartIndent(AIndent: Integer);
begin
  SendScintillaEditorMessage(SCI_SETWRAPSTARTINDENT, WPARAM(AIndent), LPARAM(0));
end;

function TCustomSciTextEditor.GetWrapStartIndent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETWRAPSTARTINDENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWrapIndentMode(AWrapIndentMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETWRAPINDENTMODE, WPARAM(AWrapIndentMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetWrapIndentMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETWRAPINDENTMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetLayoutCache(ACacheMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETLAYOUTCACHE, WPARAM(ACacheMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetLayoutCache(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETLAYOUTCACHE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetScrollWidth(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_SETSCROLLWIDTH, WPARAM(APixelWidth), LPARAM(0));
end;

function TCustomSciTextEditor.GetScrollWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSCROLLWIDTH, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetScrollWidthTracking(ATracking: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETSCROLLWIDTHTRACKING, WPARAM(ATracking), LPARAM(0));
end;

function TCustomSciTextEditor.GetScrollWidthTracking(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETSCROLLWIDTHTRACKING, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.TextWidth(AStyle: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_TEXTWIDTH, WPARAM(AStyle), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetEndAtLastLine(AEndAtLastLine: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETENDATLASTLINE, WPARAM(AEndAtLastLine), LPARAM(0));
end;

function TCustomSciTextEditor.GetEndAtLastLine(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETENDATLASTLINE, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.TextHeight(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_TEXTHEIGHT, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetVScrollBar(AVisible: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETVSCROLLBAR, WPARAM(AVisible), LPARAM(0));
end;

function TCustomSciTextEditor.GetVScrollBar(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETVSCROLLBAR, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.AppendText(ALength: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_APPENDTEXT, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetPhasesDraw(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETPHASESDRAW, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetPhasesDraw(APhases: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETPHASESDRAW, WPARAM(APhases), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetFontQuality(AFontQuality: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETFONTQUALITY, WPARAM(AFontQuality), LPARAM(0));
end;

function TCustomSciTextEditor.GetFontQuality(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETFONTQUALITY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetFirstVisibleLine(ADisplayLine: TSciLine);
begin
  SendScintillaEditorMessage(SCI_SETFIRSTVISIBLELINE, WPARAM(ADisplayLine), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMultiPaste(AMultiPaste: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETMULTIPASTE, WPARAM(AMultiPaste), LPARAM(0));
end;

function TCustomSciTextEditor.GetMultiPaste(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETMULTIPASTE, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetTag(ATagNumber: Integer; ATagValue: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETTAG, WPARAM(ATagNumber), LPARAM(ATagValue));
end;

procedure TCustomSciTextEditor.LinesJoin();
begin
  SendScintillaEditorMessage(SCI_LINESJOIN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LinesSplit(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_LINESSPLIT, WPARAM(APixelWidth), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetFoldMarginColour(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETFOLDMARGINCOLOUR, WPARAM(AUseSetting), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.SetFoldMarginHiColour(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETFOLDMARGINHICOLOUR, WPARAM(AUseSetting), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.SetAccessibility(AAccessibility: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETACCESSIBILITY, WPARAM(AAccessibility), LPARAM(0));
end;

function TCustomSciTextEditor.GetAccessibility(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETACCESSIBILITY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineDown();
begin
  SendScintillaEditorMessage(SCI_LINEDOWN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineDownExtend();
begin
  SendScintillaEditorMessage(SCI_LINEDOWNEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineUp();
begin
  SendScintillaEditorMessage(SCI_LINEUP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineUpExtend();
begin
  SendScintillaEditorMessage(SCI_LINEUPEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CharLeft();
begin
  SendScintillaEditorMessage(SCI_CHARLEFT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CharLeftExtend();
begin
  SendScintillaEditorMessage(SCI_CHARLEFTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CharRight();
begin
  SendScintillaEditorMessage(SCI_CHARRIGHT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CharRightExtend();
begin
  SendScintillaEditorMessage(SCI_CHARRIGHTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordLeft();
begin
  SendScintillaEditorMessage(SCI_WORDLEFT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordLeftExtend();
begin
  SendScintillaEditorMessage(SCI_WORDLEFTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordRight();
begin
  SendScintillaEditorMessage(SCI_WORDRIGHT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordRightExtend();
begin
  SendScintillaEditorMessage(SCI_WORDRIGHTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Home();
begin
  SendScintillaEditorMessage(SCI_HOME, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.HomeExtend();
begin
  SendScintillaEditorMessage(SCI_HOMEEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEnd();
begin
  SendScintillaEditorMessage(SCI_LINEEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEndExtend();
begin
  SendScintillaEditorMessage(SCI_LINEENDEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DocumentStart();
begin
  SendScintillaEditorMessage(SCI_DOCUMENTSTART, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DocumentStartExtend();
begin
  SendScintillaEditorMessage(SCI_DOCUMENTSTARTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DocumentEnd();
begin
  SendScintillaEditorMessage(SCI_DOCUMENTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DocumentEndExtend();
begin
  SendScintillaEditorMessage(SCI_DOCUMENTENDEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PageUp();
begin
  SendScintillaEditorMessage(SCI_PAGEUP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PageUpExtend();
begin
  SendScintillaEditorMessage(SCI_PAGEUPEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PageDown();
begin
  SendScintillaEditorMessage(SCI_PAGEDOWN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PageDownExtend();
begin
  SendScintillaEditorMessage(SCI_PAGEDOWNEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.EditToggleOvertype();
begin
  SendScintillaEditorMessage(SCI_EDITTOGGLEOVERTYPE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Cancel();
begin
  SendScintillaEditorMessage(SCI_CANCEL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DeleteBack();
begin
  SendScintillaEditorMessage(SCI_DELETEBACK, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Tab();
begin
  SendScintillaEditorMessage(SCI_TAB, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineIndent();
begin
  SendScintillaEditorMessage(SCI_LINEINDENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.BackTab();
begin
  SendScintillaEditorMessage(SCI_BACKTAB, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineDedent();
begin
  SendScintillaEditorMessage(SCI_LINEDEDENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.NewLine();
begin
  SendScintillaEditorMessage(SCI_NEWLINE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.FormFeed();
begin
  SendScintillaEditorMessage(SCI_FORMFEED, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHome();
begin
  SendScintillaEditorMessage(SCI_VCHOME, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHomeExtend();
begin
  SendScintillaEditorMessage(SCI_VCHOMEEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ZoomIn();
begin
  SendScintillaEditorMessage(SCI_ZOOMIN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ZoomOut();
begin
  SendScintillaEditorMessage(SCI_ZOOMOUT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DelWordLeft();
begin
  SendScintillaEditorMessage(SCI_DELWORDLEFT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DelWordRight();
begin
  SendScintillaEditorMessage(SCI_DELWORDRIGHT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DelWordRightEnd();
begin
  SendScintillaEditorMessage(SCI_DELWORDRIGHTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineCut();
begin
  SendScintillaEditorMessage(SCI_LINECUT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineDelete();
begin
  SendScintillaEditorMessage(SCI_LINEDELETE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineTranspose();
begin
  SendScintillaEditorMessage(SCI_LINETRANSPOSE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineReverse();
begin
  SendScintillaEditorMessage(SCI_LINEREVERSE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineDuplicate();
begin
  SendScintillaEditorMessage(SCI_LINEDUPLICATE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LowerCase();
begin
  SendScintillaEditorMessage(SCI_LOWERCASE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.UpperCase();
begin
  SendScintillaEditorMessage(SCI_UPPERCASE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineScrollDown();
begin
  SendScintillaEditorMessage(SCI_LINESCROLLDOWN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineScrollUp();
begin
  SendScintillaEditorMessage(SCI_LINESCROLLUP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DeleteBackNotLine();
begin
  SendScintillaEditorMessage(SCI_DELETEBACKNOTLINE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.HomeDisplay();
begin
  SendScintillaEditorMessage(SCI_HOMEDISPLAY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.HomeDisplayExtend();
begin
  SendScintillaEditorMessage(SCI_HOMEDISPLAYEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEndDisplay();
begin
  SendScintillaEditorMessage(SCI_LINEENDDISPLAY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEndDisplayExtend();
begin
  SendScintillaEditorMessage(SCI_LINEENDDISPLAYEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.HomeWrap();
begin
  SendScintillaEditorMessage(SCI_HOMEWRAP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.HomeWrapExtend();
begin
  SendScintillaEditorMessage(SCI_HOMEWRAPEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEndWrap();
begin
  SendScintillaEditorMessage(SCI_LINEENDWRAP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEndWrapExtend();
begin
  SendScintillaEditorMessage(SCI_LINEENDWRAPEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHomeWrap();
begin
  SendScintillaEditorMessage(SCI_VCHOMEWRAP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHomeWrapExtend();
begin
  SendScintillaEditorMessage(SCI_VCHOMEWRAPEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineCopy();
begin
  SendScintillaEditorMessage(SCI_LINECOPY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.MoveCaretInsideView();
begin
  SendScintillaEditorMessage(SCI_MOVECARETINSIDEVIEW, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.LineLength(ALine: TSciLine): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_LINELENGTH, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.BraceHighlight(APosA: TSciPosition; APosB: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_BRACEHIGHLIGHT, WPARAM(APosA), LPARAM(APosB));
end;

procedure TCustomSciTextEditor.BraceHighlightIndicator(AUseSetting: Boolean; AIndicator: Integer);
begin
  SendScintillaEditorMessage(SCI_BRACEHIGHLIGHTINDICATOR, WPARAM(AUseSetting), LPARAM(AIndicator));
end;

procedure TCustomSciTextEditor.BraceBadLight(APos: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_BRACEBADLIGHT, WPARAM(APos), LPARAM(0));
end;

procedure TCustomSciTextEditor.BraceBadLightIndicator(AUseSetting: Boolean; AIndicator: Integer);
begin
  SendScintillaEditorMessage(SCI_BRACEBADLIGHTINDICATOR, WPARAM(AUseSetting), LPARAM(AIndicator));
end;

function TCustomSciTextEditor.BraceMatch(APos: TSciPosition; AMaxReStyle: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_BRACEMATCH, WPARAM(APos), LPARAM(AMaxReStyle));
end;

function TCustomSciTextEditor.BraceMatchNext(APos: TSciPosition; AStartPos: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_BRACEMATCHNEXT, WPARAM(APos), LPARAM(AStartPos));
end;

function TCustomSciTextEditor.GetViewEOL(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETVIEWEOL, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetViewEOL(AVisible: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETVIEWEOL, WPARAM(AVisible), LPARAM(0));
end;

function TCustomSciTextEditor.GetDocPointer(): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_GETDOCPOINTER, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetDocPointer(ADoc: Pointer);
begin
  SendScintillaEditorMessage(SCI_SETDOCPOINTER, WPARAM(0), LPARAM(ADoc));
end;

procedure TCustomSciTextEditor.SetModEventMask(AEventMask: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETMODEVENTMASK, WPARAM(AEventMask), LPARAM(0));
end;

function TCustomSciTextEditor.GetEdgeColumn(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETEDGECOLUMN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetEdgeColumn(AColumn: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETEDGECOLUMN, WPARAM(AColumn), LPARAM(0));
end;

function TCustomSciTextEditor.GetEdgeMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETEDGEMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetEdgeMode(AEdgeMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETEDGEMODE, WPARAM(AEdgeMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetEdgeColour(): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETEDGECOLOUR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetEdgeColour(AEdgeColour: TColor);
begin
  SendScintillaEditorMessage(SCI_SETEDGECOLOUR, WPARAM(AEdgeColour), LPARAM(0));
end;

procedure TCustomSciTextEditor.MultiEdgeAddLine(AColumn: TSciPosition; AEdgeColour: TColor);
begin
  SendScintillaEditorMessage(SCI_MULTIEDGEADDLINE, WPARAM(AColumn), LPARAM(AEdgeColour));
end;

procedure TCustomSciTextEditor.MultiEdgeClearAll();
begin
  SendScintillaEditorMessage(SCI_MULTIEDGECLEARALL, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetMultiEdgeColumn(AWhich: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETMULTIEDGECOLUMN, WPARAM(AWhich), LPARAM(0));
end;

procedure TCustomSciTextEditor.SearchAnchor();
begin
  SendScintillaEditorMessage(SCI_SEARCHANCHOR, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.SearchNext(ASearchFlags: NativeInt; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_SEARCHNEXT, WPARAM(ASearchFlags), LPARAM(AText));
end;

function TCustomSciTextEditor.SearchPrev(ASearchFlags: NativeInt; AText: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_SEARCHPREV, WPARAM(ASearchFlags), LPARAM(AText));
end;

function TCustomSciTextEditor.LinesOnScreen(): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_LINESONSCREEN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.UsePopUp(APopUpMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_USEPOPUP, WPARAM(APopUpMode), LPARAM(0));
end;

function TCustomSciTextEditor.SelectionIsRectangle(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_SELECTIONISRECTANGLE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetZoom(AZoomInPoints: Integer);
begin
  SendScintillaEditorMessage(SCI_SETZOOM, WPARAM(AZoomInPoints), LPARAM(0));
end;

function TCustomSciTextEditor.GetZoom(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETZOOM, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.CreateDocument(ABytes: TSciPosition; ADocumentOptions: NativeInt): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_CREATEDOCUMENT, WPARAM(ABytes), LPARAM(ADocumentOptions)));
end;

procedure TCustomSciTextEditor.AddRefDocument(ADoc: Pointer);
begin
  SendScintillaEditorMessage(SCI_ADDREFDOCUMENT, WPARAM(0), LPARAM(ADoc));
end;

procedure TCustomSciTextEditor.ReleaseDocument(ADoc: Pointer);
begin
  SendScintillaEditorMessage(SCI_RELEASEDOCUMENT, WPARAM(0), LPARAM(ADoc));
end;

function TCustomSciTextEditor.GetDocumentOptions(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETDOCUMENTOPTIONS, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetModEventMask(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETMODEVENTMASK, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCommandEvents(ACommandEvents: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETCOMMANDEVENTS, WPARAM(ACommandEvents), LPARAM(0));
end;

function TCustomSciTextEditor.GetCommandEvents(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETCOMMANDEVENTS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetFocus(AFocus: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETFOCUS, WPARAM(AFocus), LPARAM(0));
end;

function TCustomSciTextEditor.GetFocus(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETFOCUS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetStatus(AStatus: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETSTATUS, WPARAM(AStatus), LPARAM(0));
end;

function TCustomSciTextEditor.GetStatus(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETSTATUS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMouseDownCaptures(ACaptures: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETMOUSEDOWNCAPTURES, WPARAM(ACaptures), LPARAM(0));
end;

function TCustomSciTextEditor.GetMouseDownCaptures(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMOUSEDOWNCAPTURES, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetMouseWheelCaptures(ACaptures: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETMOUSEWHEELCAPTURES, WPARAM(ACaptures), LPARAM(0));
end;

function TCustomSciTextEditor.GetMouseWheelCaptures(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMOUSEWHEELCAPTURES, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetCursor(ACursorType: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETCURSOR, WPARAM(ACursorType), LPARAM(0));
end;

function TCustomSciTextEditor.GetCursor(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETCURSOR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetControlCharSymbol(ASymbol: Integer);
begin
  SendScintillaEditorMessage(SCI_SETCONTROLCHARSYMBOL, WPARAM(ASymbol), LPARAM(0));
end;

function TCustomSciTextEditor.GetControlCharSymbol(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCONTROLCHARSYMBOL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordPartLeft();
begin
  SendScintillaEditorMessage(SCI_WORDPARTLEFT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordPartLeftExtend();
begin
  SendScintillaEditorMessage(SCI_WORDPARTLEFTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordPartRight();
begin
  SendScintillaEditorMessage(SCI_WORDPARTRIGHT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordPartRightExtend();
begin
  SendScintillaEditorMessage(SCI_WORDPARTRIGHTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetVisiblePolicy(AVisiblePolicy: NativeInt; AVisibleSlop: Integer);
begin
  SendScintillaEditorMessage(SCI_SETVISIBLEPOLICY, WPARAM(AVisiblePolicy), LPARAM(AVisibleSlop));
end;

procedure TCustomSciTextEditor.DelLineLeft();
begin
  SendScintillaEditorMessage(SCI_DELLINELEFT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.DelLineRight();
begin
  SendScintillaEditorMessage(SCI_DELLINERIGHT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetXOffset(AXOffset: Integer);
begin
  SendScintillaEditorMessage(SCI_SETXOFFSET, WPARAM(AXOffset), LPARAM(0));
end;

function TCustomSciTextEditor.GetXOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETXOFFSET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ChooseCaretX();
begin
  SendScintillaEditorMessage(SCI_CHOOSECARETX, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.GrabFocus();
begin
  SendScintillaEditorMessage(SCI_GRABFOCUS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetXCaretPolicy(ACaretPolicy: NativeInt; ACaretSlop: Integer);
begin
  SendScintillaEditorMessage(SCI_SETXCARETPOLICY, WPARAM(ACaretPolicy), LPARAM(ACaretSlop));
end;

procedure TCustomSciTextEditor.SetYCaretPolicy(ACaretPolicy: NativeInt; ACaretSlop: Integer);
begin
  SendScintillaEditorMessage(SCI_SETYCARETPOLICY, WPARAM(ACaretPolicy), LPARAM(ACaretSlop));
end;

procedure TCustomSciTextEditor.SetPrintWrapMode(AWrapMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETPRINTWRAPMODE, WPARAM(AWrapMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetPrintWrapMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETPRINTWRAPMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetHotspotActiveFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETHOTSPOTACTIVEFORE, WPARAM(AUseSetting), LPARAM(AFore));
end;

function TCustomSciTextEditor.GetHotspotActiveFore(): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETHOTSPOTACTIVEFORE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetHotspotActiveBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETHOTSPOTACTIVEBACK, WPARAM(AUseSetting), LPARAM(ABack));
end;

function TCustomSciTextEditor.GetHotspotActiveBack(): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETHOTSPOTACTIVEBACK, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetHotspotActiveUnderline(AUnderline: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETHOTSPOTACTIVEUNDERLINE, WPARAM(AUnderline), LPARAM(0));
end;

function TCustomSciTextEditor.GetHotspotActiveUnderline(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETHOTSPOTACTIVEUNDERLINE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetHotspotSingleLine(ASingleLine: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETHOTSPOTSINGLELINE, WPARAM(ASingleLine), LPARAM(0));
end;

function TCustomSciTextEditor.GetHotspotSingleLine(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETHOTSPOTSINGLELINE, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.ParaDown();
begin
  SendScintillaEditorMessage(SCI_PARADOWN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ParaDownExtend();
begin
  SendScintillaEditorMessage(SCI_PARADOWNEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ParaUp();
begin
  SendScintillaEditorMessage(SCI_PARAUP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ParaUpExtend();
begin
  SendScintillaEditorMessage(SCI_PARAUPEXTEND, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.PositionBefore(APos: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONBEFORE, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.PositionAfter(APos: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONAFTER, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.PositionRelative(APos: TSciPosition; ARelative: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONRELATIVE, WPARAM(APos), LPARAM(ARelative));
end;

function TCustomSciTextEditor.PositionRelativeCodeUnits(APos: TSciPosition; ARelative: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_POSITIONRELATIVECODEUNITS, WPARAM(APos), LPARAM(ARelative));
end;

procedure TCustomSciTextEditor.CopyRange(AStart: TSciPosition; AEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_COPYRANGE, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.CopyText(ALength: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_COPYTEXT, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetSelectionMode(ASelectionMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONMODE, WPARAM(ASelectionMode), LPARAM(0));
end;

procedure TCustomSciTextEditor.ChangeSelectionMode(ASelectionMode: NativeInt);
begin
  SendScintillaEditorMessage(SCI_CHANGESELECTIONMODE, WPARAM(ASelectionMode), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionMode(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONMODE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMoveExtendsSelection(AMoveExtendsSelection: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETMOVEEXTENDSSELECTION, WPARAM(AMoveExtendsSelection), LPARAM(0));
end;

function TCustomSciTextEditor.GetMoveExtendsSelection(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMOVEEXTENDSSELECTION, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.GetLineSelStartPosition(ALine: TSciLine): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINESELSTARTPOSITION, WPARAM(ALine), LPARAM(0));
end;

function TCustomSciTextEditor.GetLineSelEndPosition(ALine: TSciLine): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINESELENDPOSITION, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineDownRectExtend();
begin
  SendScintillaEditorMessage(SCI_LINEDOWNRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineUpRectExtend();
begin
  SendScintillaEditorMessage(SCI_LINEUPRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CharLeftRectExtend();
begin
  SendScintillaEditorMessage(SCI_CHARLEFTRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CharRightRectExtend();
begin
  SendScintillaEditorMessage(SCI_CHARRIGHTRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.HomeRectExtend();
begin
  SendScintillaEditorMessage(SCI_HOMERECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHomeRectExtend();
begin
  SendScintillaEditorMessage(SCI_VCHOMERECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.LineEndRectExtend();
begin
  SendScintillaEditorMessage(SCI_LINEENDRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PageUpRectExtend();
begin
  SendScintillaEditorMessage(SCI_PAGEUPRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.PageDownRectExtend();
begin
  SendScintillaEditorMessage(SCI_PAGEDOWNRECTEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StutteredPageUp();
begin
  SendScintillaEditorMessage(SCI_STUTTEREDPAGEUP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StutteredPageUpExtend();
begin
  SendScintillaEditorMessage(SCI_STUTTEREDPAGEUPEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StutteredPageDown();
begin
  SendScintillaEditorMessage(SCI_STUTTEREDPAGEDOWN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StutteredPageDownExtend();
begin
  SendScintillaEditorMessage(SCI_STUTTEREDPAGEDOWNEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordLeftEnd();
begin
  SendScintillaEditorMessage(SCI_WORDLEFTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordLeftEndExtend();
begin
  SendScintillaEditorMessage(SCI_WORDLEFTENDEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordRightEnd();
begin
  SendScintillaEditorMessage(SCI_WORDRIGHTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.WordRightEndExtend();
begin
  SendScintillaEditorMessage(SCI_WORDRIGHTENDEXTEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetWhitespaceChars(ACharacters: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETWHITESPACECHARS, WPARAM(0), LPARAM(ACharacters));
end;

function TCustomSciTextEditor.GetWhitespaceChars(ACharacters: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETWHITESPACECHARS, WPARAM(0), LPARAM(ACharacters));
end;

procedure TCustomSciTextEditor.SetPunctuationChars(ACharacters: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETPUNCTUATIONCHARS, WPARAM(0), LPARAM(ACharacters));
end;

function TCustomSciTextEditor.GetPunctuationChars(ACharacters: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPUNCTUATIONCHARS, WPARAM(0), LPARAM(ACharacters));
end;

procedure TCustomSciTextEditor.SetCharsDefault();
begin
  SendScintillaEditorMessage(SCI_SETCHARSDEFAULT, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetCurrent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETCURRENT, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetCurrentText(AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETCURRENTTEXT, WPARAM(0), LPARAM(AText));
end;

procedure TCustomSciTextEditor.AutoCSetCaseInsensitiveBehaviour(ABehaviour: NativeInt);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETCASEINSENSITIVEBEHAVIOUR, WPARAM(ABehaviour), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetCaseInsensitiveBehaviour(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETCASEINSENSITIVEBEHAVIOUR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetMulti(AMulti: NativeInt);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETMULTI, WPARAM(AMulti), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetMulti(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETMULTI, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AutoCSetOrder(AOrder: NativeInt);
begin
  SendScintillaEditorMessage(SCI_AUTOCSETORDER, WPARAM(AOrder), LPARAM(0));
end;

function TCustomSciTextEditor.AutoCGetOrder(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_AUTOCGETORDER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Allocate(ABytes: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_ALLOCATE, WPARAM(ABytes), LPARAM(0));
end;

function TCustomSciTextEditor.TargetAsUTF8(s: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_TARGETASUTF8, WPARAM(0), LPARAM(s));
end;

procedure TCustomSciTextEditor.SetLengthForEncode(ABytes: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETLENGTHFORENCODE, WPARAM(ABytes), LPARAM(0));
end;

function TCustomSciTextEditor.EncodedFromUTF8(AUtf8: PAnsiChar; AEncoded: PAnsiChar): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_ENCODEDFROMUTF8, WPARAM(AUtf8), LPARAM(AEncoded));
end;

function TCustomSciTextEditor.FindColumn(ALine: TSciLine; AColumn: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_FINDCOLUMN, WPARAM(ALine), LPARAM(AColumn));
end;

function TCustomSciTextEditor.GetCaretSticky(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETSTICKY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretSticky(AUseCaretStickyBehaviour: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETCARETSTICKY, WPARAM(AUseCaretStickyBehaviour), LPARAM(0));
end;

procedure TCustomSciTextEditor.ToggleCaretSticky();
begin
  SendScintillaEditorMessage(SCI_TOGGLECARETSTICKY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetPasteConvertEndings(AConvert: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETPASTECONVERTENDINGS, WPARAM(AConvert), LPARAM(0));
end;

function TCustomSciTextEditor.GetPasteConvertEndings(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETPASTECONVERTENDINGS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.ReplaceRectangular(ALength: TSciPosition; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_REPLACERECTANGULAR, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SelectionDuplicate();
begin
  SendScintillaEditorMessage(SCI_SELECTIONDUPLICATE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretLineBackAlpha(AAlpha: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINEBACKALPHA, WPARAM(AAlpha), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineBackAlpha(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETLINEBACKALPHA, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCaretStyle(ACaretStyle: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETCARETSTYLE, WPARAM(ACaretStyle), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretStyle(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETCARETSTYLE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetIndicatorCurrent(AIndicator: Integer);
begin
  SendScintillaEditorMessage(SCI_SETINDICATORCURRENT, WPARAM(AIndicator), LPARAM(0));
end;

function TCustomSciTextEditor.GetIndicatorCurrent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETINDICATORCURRENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetIndicatorValue(AValue: Integer);
begin
  SendScintillaEditorMessage(SCI_SETINDICATORVALUE, WPARAM(AValue), LPARAM(0));
end;

function TCustomSciTextEditor.GetIndicatorValue(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETINDICATORVALUE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicatorFillRange(AStart: TSciPosition; ALengthFill: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_INDICATORFILLRANGE, WPARAM(AStart), LPARAM(ALengthFill));
end;

procedure TCustomSciTextEditor.IndicatorClearRange(AStart: TSciPosition; ALengthClear: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_INDICATORCLEARRANGE, WPARAM(AStart), LPARAM(ALengthClear));
end;

function TCustomSciTextEditor.IndicatorAllOnFor(APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_INDICATORALLONFOR, WPARAM(APos), LPARAM(0));
end;

function TCustomSciTextEditor.IndicatorValueAt(AIndicator: Integer; APos: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_INDICATORVALUEAT, WPARAM(AIndicator), LPARAM(APos));
end;

function TCustomSciTextEditor.IndicatorStart(AIndicator: Integer; APos: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_INDICATORSTART, WPARAM(AIndicator), LPARAM(APos));
end;

function TCustomSciTextEditor.IndicatorEnd(AIndicator: Integer; APos: TSciPosition): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_INDICATOREND, WPARAM(AIndicator), LPARAM(APos));
end;

procedure TCustomSciTextEditor.SetPositionCache(ASize: Integer);
begin
  SendScintillaEditorMessage(SCI_SETPOSITIONCACHE, WPARAM(ASize), LPARAM(0));
end;

function TCustomSciTextEditor.GetPositionCache(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPOSITIONCACHE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetLayoutThreads(AThreads: Integer);
begin
  SendScintillaEditorMessage(SCI_SETLAYOUTTHREADS, WPARAM(AThreads), LPARAM(0));
end;

function TCustomSciTextEditor.GetLayoutThreads(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETLAYOUTTHREADS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CopyAllowLine();
begin
  SendScintillaEditorMessage(SCI_COPYALLOWLINE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.CutAllowLine();
begin
  SendScintillaEditorMessage(SCI_CUTALLOWLINE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetCopySeparator(ASeparator: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETCOPYSEPARATOR, WPARAM(0), LPARAM(ASeparator));
end;

function TCustomSciTextEditor.GetCopySeparator(ASeparator: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETCOPYSEPARATOR, WPARAM(0), LPARAM(ASeparator));
end;

function TCustomSciTextEditor.GetCharacterPointer(): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_GETCHARACTERPOINTER, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.GetRangePointer(AStart: TSciPosition; ALengthRange: TSciPosition): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_GETRANGEPOINTER, WPARAM(AStart), LPARAM(ALengthRange)));
end;

function TCustomSciTextEditor.GetGapPosition(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETGAPPOSITION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetAlpha(AIndicator: Integer; AAlpha: NativeInt);
begin
  SendScintillaEditorMessage(SCI_INDICSETALPHA, WPARAM(AIndicator), LPARAM(AAlpha));
end;

function TCustomSciTextEditor.IndicGetAlpha(AIndicator: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETALPHA, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.IndicSetOutlineAlpha(AIndicator: Integer; AAlpha: NativeInt);
begin
  SendScintillaEditorMessage(SCI_INDICSETOUTLINEALPHA, WPARAM(AIndicator), LPARAM(AAlpha));
end;

function TCustomSciTextEditor.IndicGetOutlineAlpha(AIndicator: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_INDICGETOUTLINEALPHA, WPARAM(AIndicator), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetExtraAscent(AExtraAscent: Integer);
begin
  SendScintillaEditorMessage(SCI_SETEXTRAASCENT, WPARAM(AExtraAscent), LPARAM(0));
end;

function TCustomSciTextEditor.GetExtraAscent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETEXTRAASCENT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetExtraDescent(AExtraDescent: Integer);
begin
  SendScintillaEditorMessage(SCI_SETEXTRADESCENT, WPARAM(AExtraDescent), LPARAM(0));
end;

function TCustomSciTextEditor.GetExtraDescent(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETEXTRADESCENT, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.MarkerSymbolDefined(AMarkerNumber: Integer): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_MARKERSYMBOLDEFINED, WPARAM(AMarkerNumber), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarginSetText(ALine: TSciLine; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_MARGINSETTEXT, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.MarginGetText(ALine: TSciLine; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARGINGETTEXT, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.MarginSetStyle(ALine: TSciLine; AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_MARGINSETSTYLE, WPARAM(ALine), LPARAM(AStyle));
end;

function TCustomSciTextEditor.MarginGetStyle(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARGINGETSTYLE, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarginSetStyles(ALine: TSciLine; AStyles: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_MARGINSETSTYLES, WPARAM(ALine), LPARAM(AStyles));
end;

function TCustomSciTextEditor.MarginGetStyles(ALine: TSciLine; AStyles: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARGINGETSTYLES, WPARAM(ALine), LPARAM(AStyles));
end;

procedure TCustomSciTextEditor.MarginTextClearAll();
begin
  SendScintillaEditorMessage(SCI_MARGINTEXTCLEARALL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarginSetStyleOffset(AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_MARGINSETSTYLEOFFSET, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.MarginGetStyleOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_MARGINGETSTYLEOFFSET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMarginOptions(AMarginOptions: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETMARGINOPTIONS, WPARAM(AMarginOptions), LPARAM(0));
end;

function TCustomSciTextEditor.GetMarginOptions(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETMARGINOPTIONS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AnnotationSetText(ALine: TSciLine; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_ANNOTATIONSETTEXT, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.AnnotationGetText(ALine: TSciLine; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ANNOTATIONGETTEXT, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.AnnotationSetStyle(ALine: TSciLine; AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_ANNOTATIONSETSTYLE, WPARAM(ALine), LPARAM(AStyle));
end;

function TCustomSciTextEditor.AnnotationGetStyle(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ANNOTATIONGETSTYLE, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.AnnotationSetStyles(ALine: TSciLine; AStyles: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_ANNOTATIONSETSTYLES, WPARAM(ALine), LPARAM(AStyles));
end;

function TCustomSciTextEditor.AnnotationGetStyles(ALine: TSciLine; AStyles: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ANNOTATIONGETSTYLES, WPARAM(ALine), LPARAM(AStyles));
end;

function TCustomSciTextEditor.AnnotationGetLines(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ANNOTATIONGETLINES, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.AnnotationClearAll();
begin
  SendScintillaEditorMessage(SCI_ANNOTATIONCLEARALL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AnnotationSetVisible(AVisible: NativeInt);
begin
  SendScintillaEditorMessage(SCI_ANNOTATIONSETVISIBLE, WPARAM(AVisible), LPARAM(0));
end;

function TCustomSciTextEditor.AnnotationGetVisible(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_ANNOTATIONGETVISIBLE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AnnotationSetStyleOffset(AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_ANNOTATIONSETSTYLEOFFSET, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.AnnotationGetStyleOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ANNOTATIONGETSTYLEOFFSET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ReleaseAllExtendedStyles();
begin
  SendScintillaEditorMessage(SCI_RELEASEALLEXTENDEDSTYLES, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.AllocateExtendedStyles(ANumberStyles: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ALLOCATEEXTENDEDSTYLES, WPARAM(ANumberStyles), LPARAM(0));
end;

procedure TCustomSciTextEditor.AddUndoAction(AToken: Integer; AFlags: NativeInt);
begin
  SendScintillaEditorMessage(SCI_ADDUNDOACTION, WPARAM(AToken), LPARAM(AFlags));
end;

function TCustomSciTextEditor.CharPositionFromPoint(x: Integer; y: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_CHARPOSITIONFROMPOINT, WPARAM(x), LPARAM(y));
end;

function TCustomSciTextEditor.CharPositionFromPointClose(x: Integer; y: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_CHARPOSITIONFROMPOINTCLOSE, WPARAM(x), LPARAM(y));
end;

procedure TCustomSciTextEditor.SetMouseSelectionRectangularSwitch(AMouseSelectionRectangularSwitch: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETMOUSESELECTIONRECTANGULARSWITCH, WPARAM(AMouseSelectionRectangularSwitch), LPARAM(0));
end;

function TCustomSciTextEditor.GetMouseSelectionRectangularSwitch(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMOUSESELECTIONRECTANGULARSWITCH, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetMultipleSelection(AMultipleSelection: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETMULTIPLESELECTION, WPARAM(AMultipleSelection), LPARAM(0));
end;

function TCustomSciTextEditor.GetMultipleSelection(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETMULTIPLESELECTION, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetAdditionalSelectionTyping(AAdditionalSelectionTyping: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALSELECTIONTYPING, WPARAM(AAdditionalSelectionTyping), LPARAM(0));
end;

function TCustomSciTextEditor.GetAdditionalSelectionTyping(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETADDITIONALSELECTIONTYPING, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetAdditionalCaretsBlink(AAdditionalCaretsBlink: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALCARETSBLINK, WPARAM(AAdditionalCaretsBlink), LPARAM(0));
end;

function TCustomSciTextEditor.GetAdditionalCaretsBlink(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETADDITIONALCARETSBLINK, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetAdditionalCaretsVisible(AAdditionalCaretsVisible: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALCARETSVISIBLE, WPARAM(AAdditionalCaretsVisible), LPARAM(0));
end;

function TCustomSciTextEditor.GetAdditionalCaretsVisible(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETADDITIONALCARETSVISIBLE, WPARAM(0), LPARAM(0)));
end;

function TCustomSciTextEditor.GetSelections(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONS, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionEmpty(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETSELECTIONEMPTY, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.ClearSelections();
begin
  SendScintillaEditorMessage(SCI_CLEARSELECTIONS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelection(ACaret: TSciPosition; AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTION, WPARAM(ACaret), LPARAM(AAnchor));
end;

procedure TCustomSciTextEditor.AddSelection(ACaret: TSciPosition; AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_ADDSELECTION, WPARAM(ACaret), LPARAM(AAnchor));
end;

function TCustomSciTextEditor.SelectionFromPoint(x: Integer; y: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_SELECTIONFROMPOINT, WPARAM(x), LPARAM(y));
end;

procedure TCustomSciTextEditor.DropSelectionN(ASelection: Integer);
begin
  SendScintillaEditorMessage(SCI_DROPSELECTIONN, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetMainSelection(ASelection: Integer);
begin
  SendScintillaEditorMessage(SCI_SETMAINSELECTION, WPARAM(ASelection), LPARAM(0));
end;

function TCustomSciTextEditor.GetMainSelection(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETMAINSELECTION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionNCaret(ASelection: Integer; ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONNCARET, WPARAM(ASelection), LPARAM(ACaret));
end;

function TCustomSciTextEditor.GetSelectionNCaret(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNCARET, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionNAnchor(ASelection: Integer; AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONNANCHOR, WPARAM(ASelection), LPARAM(AAnchor));
end;

function TCustomSciTextEditor.GetSelectionNAnchor(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNANCHOR, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionNCaretVirtualSpace(ASelection: Integer; ASpace: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONNCARETVIRTUALSPACE, WPARAM(ASelection), LPARAM(ASpace));
end;

function TCustomSciTextEditor.GetSelectionNCaretVirtualSpace(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNCARETVIRTUALSPACE, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionNAnchorVirtualSpace(ASelection: Integer; ASpace: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONNANCHORVIRTUALSPACE, WPARAM(ASelection), LPARAM(ASpace));
end;

function TCustomSciTextEditor.GetSelectionNAnchorVirtualSpace(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNANCHORVIRTUALSPACE, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionNStart(ASelection: Integer; AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONNSTART, WPARAM(ASelection), LPARAM(AAnchor));
end;

function TCustomSciTextEditor.GetSelectionNStart(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNSTART, WPARAM(ASelection), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionNStartVirtualSpace(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNSTARTVIRTUALSPACE, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetSelectionNEnd(ASelection: Integer; ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETSELECTIONNEND, WPARAM(ASelection), LPARAM(ACaret));
end;

function TCustomSciTextEditor.GetSelectionNEndVirtualSpace(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNENDVIRTUALSPACE, WPARAM(ASelection), LPARAM(0));
end;

function TCustomSciTextEditor.GetSelectionNEnd(ASelection: Integer): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETSELECTIONNEND, WPARAM(ASelection), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRectangularSelectionCaret(ACaret: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETRECTANGULARSELECTIONCARET, WPARAM(ACaret), LPARAM(0));
end;

function TCustomSciTextEditor.GetRectangularSelectionCaret(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETRECTANGULARSELECTIONCARET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRectangularSelectionAnchor(AAnchor: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETRECTANGULARSELECTIONANCHOR, WPARAM(AAnchor), LPARAM(0));
end;

function TCustomSciTextEditor.GetRectangularSelectionAnchor(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETRECTANGULARSELECTIONANCHOR, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRectangularSelectionCaretVirtualSpace(ASpace: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETRECTANGULARSELECTIONCARETVIRTUALSPACE, WPARAM(ASpace), LPARAM(0));
end;

function TCustomSciTextEditor.GetRectangularSelectionCaretVirtualSpace(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETRECTANGULARSELECTIONCARETVIRTUALSPACE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRectangularSelectionAnchorVirtualSpace(ASpace: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_SETRECTANGULARSELECTIONANCHORVIRTUALSPACE, WPARAM(ASpace), LPARAM(0));
end;

function TCustomSciTextEditor.GetRectangularSelectionAnchorVirtualSpace(): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_GETRECTANGULARSELECTIONANCHORVIRTUALSPACE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetVirtualSpaceOptions(AVirtualSpaceOptions: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETVIRTUALSPACEOPTIONS, WPARAM(AVirtualSpaceOptions), LPARAM(0));
end;

function TCustomSciTextEditor.GetVirtualSpaceOptions(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETVIRTUALSPACEOPTIONS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRectangularSelectionModifier(AModifier: Integer);
begin
  SendScintillaEditorMessage(SCI_SETRECTANGULARSELECTIONMODIFIER, WPARAM(AModifier), LPARAM(0));
end;

function TCustomSciTextEditor.GetRectangularSelectionModifier(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETRECTANGULARSELECTIONMODIFIER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetAdditionalSelFore(AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALSELFORE, WPARAM(AFore), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetAdditionalSelBack(ABack: TColor);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALSELBACK, WPARAM(ABack), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetAdditionalSelAlpha(AAlpha: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALSELALPHA, WPARAM(AAlpha), LPARAM(0));
end;

function TCustomSciTextEditor.GetAdditionalSelAlpha(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETADDITIONALSELALPHA, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetAdditionalCaretFore(AFore: TColor);
begin
  SendScintillaEditorMessage(SCI_SETADDITIONALCARETFORE, WPARAM(AFore), LPARAM(0));
end;

function TCustomSciTextEditor.GetAdditionalCaretFore(): TColor;
begin
  Result := SendScintillaEditorMessage(SCI_GETADDITIONALCARETFORE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.RotateSelection();
begin
  SendScintillaEditorMessage(SCI_ROTATESELECTION, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SwapMainAnchorCaret();
begin
  SendScintillaEditorMessage(SCI_SWAPMAINANCHORCARET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.MultipleSelectAddNext();
begin
  SendScintillaEditorMessage(SCI_MULTIPLESELECTADDNEXT, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.MultipleSelectAddEach();
begin
  SendScintillaEditorMessage(SCI_MULTIPLESELECTADDEACH, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.ChangeLexerState(AStart: TSciPosition; AEnd: TSciPosition): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_CHANGELEXERSTATE, WPARAM(AStart), LPARAM(AEnd));
end;

function TCustomSciTextEditor.ContractedFoldNext(ALineStart: TSciLine): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_CONTRACTEDFOLDNEXT, WPARAM(ALineStart), LPARAM(0));
end;

procedure TCustomSciTextEditor.VerticalCentreCaret();
begin
  SendScintillaEditorMessage(SCI_VERTICALCENTRECARET, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.MoveSelectedLinesUp();
begin
  SendScintillaEditorMessage(SCI_MOVESELECTEDLINESUP, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.MoveSelectedLinesDown();
begin
  SendScintillaEditorMessage(SCI_MOVESELECTEDLINESDOWN, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetIdentifier(AIdentifier: Integer);
begin
  SendScintillaEditorMessage(SCI_SETIDENTIFIER, WPARAM(AIdentifier), LPARAM(0));
end;

function TCustomSciTextEditor.GetIdentifier(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETIDENTIFIER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.RGBAImageSetWidth(AWidth: Integer);
begin
  SendScintillaEditorMessage(SCI_RGBAIMAGESETWIDTH, WPARAM(AWidth), LPARAM(0));
end;

procedure TCustomSciTextEditor.RGBAImageSetHeight(AHeight: Integer);
begin
  SendScintillaEditorMessage(SCI_RGBAIMAGESETHEIGHT, WPARAM(AHeight), LPARAM(0));
end;

procedure TCustomSciTextEditor.RGBAImageSetScale(AScalePercent: Integer);
begin
  SendScintillaEditorMessage(SCI_RGBAIMAGESETSCALE, WPARAM(AScalePercent), LPARAM(0));
end;

procedure TCustomSciTextEditor.MarkerDefineRGBAImage(AMarkerNumber: Integer; APixels: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_MARKERDEFINERGBAIMAGE, WPARAM(AMarkerNumber), LPARAM(APixels));
end;

procedure TCustomSciTextEditor.RegisterRGBAImage(AType: Integer; APixels: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_REGISTERRGBAIMAGE, WPARAM(AType), LPARAM(APixels));
end;

procedure TCustomSciTextEditor.ScrollToStart();
begin
  SendScintillaEditorMessage(SCI_SCROLLTOSTART, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.ScrollToEnd();
begin
  SendScintillaEditorMessage(SCI_SCROLLTOEND, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetTechnology(ATechnology: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETTECHNOLOGY, WPARAM(ATechnology), LPARAM(0));
end;

function TCustomSciTextEditor.GetTechnology(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETTECHNOLOGY, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.CreateLoader(ABytes: TSciPosition; ADocumentOptions: NativeInt): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_CREATELOADER, WPARAM(ABytes), LPARAM(ADocumentOptions)));
end;

procedure TCustomSciTextEditor.FindIndicatorShow(AStart: TSciPosition; AEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_FINDINDICATORSHOW, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.FindIndicatorFlash(AStart: TSciPosition; AEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_FINDINDICATORFLASH, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.FindIndicatorHide();
begin
  SendScintillaEditorMessage(SCI_FINDINDICATORHIDE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHomeDisplay();
begin
  SendScintillaEditorMessage(SCI_VCHOMEDISPLAY, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.VCHomeDisplayExtend();
begin
  SendScintillaEditorMessage(SCI_VCHOMEDISPLAYEXTEND, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetCaretLineVisibleAlways(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_GETCARETLINEVISIBLEALWAYS, WPARAM(0), LPARAM(0)));
end;

procedure TCustomSciTextEditor.SetCaretLineVisibleAlways(AAlwaysVisible: Boolean);
begin
  SendScintillaEditorMessage(SCI_SETCARETLINEVISIBLEALWAYS, WPARAM(AAlwaysVisible), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetLineEndTypesAllowed(ALineEndBitSet: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETLINEENDTYPESALLOWED, WPARAM(ALineEndBitSet), LPARAM(0));
end;

function TCustomSciTextEditor.GetLineEndTypesAllowed(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINEENDTYPESALLOWED, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetLineEndTypesActive(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINEENDTYPESACTIVE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETREPRESENTATION, WPARAM(AEncodedCharacter), LPARAM(ARepresentation));
end;

function TCustomSciTextEditor.GetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETREPRESENTATION, WPARAM(AEncodedCharacter), LPARAM(ARepresentation));
end;

procedure TCustomSciTextEditor.ClearRepresentation(AEncodedCharacter: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_CLEARREPRESENTATION, WPARAM(AEncodedCharacter), LPARAM(0));
end;

procedure TCustomSciTextEditor.ClearAllRepresentations();
begin
  SendScintillaEditorMessage(SCI_CLEARALLREPRESENTATIONS, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRepresentationAppearance(AEncodedCharacter: PAnsiChar; AAppearance: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETREPRESENTATIONAPPEARANCE, WPARAM(AEncodedCharacter), LPARAM(AAppearance));
end;

function TCustomSciTextEditor.GetRepresentationAppearance(AEncodedCharacter: PAnsiChar): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETREPRESENTATIONAPPEARANCE, WPARAM(AEncodedCharacter), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetRepresentationColour(AEncodedCharacter: PAnsiChar; AColour: TColorAlpha);
begin
  SendScintillaEditorMessage(SCI_SETREPRESENTATIONCOLOUR, WPARAM(AEncodedCharacter), LPARAM(AColour));
end;

function TCustomSciTextEditor.GetRepresentationColour(AEncodedCharacter: PAnsiChar): TColorAlpha;
begin
  Result := SendScintillaEditorMessage(SCI_GETREPRESENTATIONCOLOUR, WPARAM(AEncodedCharacter), LPARAM(0));
end;

procedure TCustomSciTextEditor.EOLAnnotationSetText(ALine: TSciLine; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_EOLANNOTATIONSETTEXT, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.EOLAnnotationGetText(ALine: TSciLine; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_EOLANNOTATIONGETTEXT, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.EOLAnnotationSetStyle(ALine: TSciLine; AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_EOLANNOTATIONSETSTYLE, WPARAM(ALine), LPARAM(AStyle));
end;

function TCustomSciTextEditor.EOLAnnotationGetStyle(ALine: TSciLine): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_EOLANNOTATIONGETSTYLE, WPARAM(ALine), LPARAM(0));
end;

procedure TCustomSciTextEditor.EOLAnnotationClearAll();
begin
  SendScintillaEditorMessage(SCI_EOLANNOTATIONCLEARALL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.EOLAnnotationSetVisible(AVisible: NativeInt);
begin
  SendScintillaEditorMessage(SCI_EOLANNOTATIONSETVISIBLE, WPARAM(AVisible), LPARAM(0));
end;

function TCustomSciTextEditor.EOLAnnotationGetVisible(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_EOLANNOTATIONGETVISIBLE, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.EOLAnnotationSetStyleOffset(AStyle: Integer);
begin
  SendScintillaEditorMessage(SCI_EOLANNOTATIONSETSTYLEOFFSET, WPARAM(AStyle), LPARAM(0));
end;

function TCustomSciTextEditor.EOLAnnotationGetStyleOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_EOLANNOTATIONGETSTYLEOFFSET, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.SupportsFeature(AFeature: NativeInt): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(SCI_SUPPORTSFEATURE, WPARAM(AFeature), LPARAM(0)));
end;

function TCustomSciTextEditor.GetLineCharacterIndex(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINECHARACTERINDEX, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.AllocateLineCharacterIndex(ALineCharacterIndex: NativeInt);
begin
  SendScintillaEditorMessage(SCI_ALLOCATELINECHARACTERINDEX, WPARAM(ALineCharacterIndex), LPARAM(0));
end;

procedure TCustomSciTextEditor.ReleaseLineCharacterIndex(ALineCharacterIndex: NativeInt);
begin
  SendScintillaEditorMessage(SCI_RELEASELINECHARACTERINDEX, WPARAM(ALineCharacterIndex), LPARAM(0));
end;

function TCustomSciTextEditor.LineFromIndexPosition(APos: TSciPosition; ALineCharacterIndex: NativeInt): TSciLine;
begin
  Result := SendScintillaEditorMessage(SCI_LINEFROMINDEXPOSITION, WPARAM(APos), LPARAM(ALineCharacterIndex));
end;

function TCustomSciTextEditor.IndexPositionFromLine(ALine: TSciLine; ALineCharacterIndex: NativeInt): TSciPosition;
begin
  Result := SendScintillaEditorMessage(SCI_INDEXPOSITIONFROMLINE, WPARAM(ALine), LPARAM(ALineCharacterIndex));
end;

procedure TCustomSciTextEditor.StartRecord();
begin
  SendScintillaEditorMessage(SCI_STARTRECORD, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.StopRecord();
begin
  SendScintillaEditorMessage(SCI_STOPRECORD, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetLexer(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETLEXER, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.Colourise(AStart: TSciPosition; AEnd: TSciPosition);
begin
  SendScintillaEditorMessage(SCI_COLOURISE, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.SetProperty(AKey: PAnsiChar; AValue: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETPROPERTY, WPARAM(AKey), LPARAM(AValue));
end;

procedure TCustomSciTextEditor.SetKeyWords(AKeyWordSet: Integer; AKeyWords: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETKEYWORDS, WPARAM(AKeyWordSet), LPARAM(AKeyWords));
end;

function TCustomSciTextEditor.GetProperty(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPROPERTY, WPARAM(AKey), LPARAM(AValue));
end;

function TCustomSciTextEditor.GetPropertyExpanded(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPROPERTYEXPANDED, WPARAM(AKey), LPARAM(AValue));
end;

function TCustomSciTextEditor.GetPropertyInt(AKey: PAnsiChar; ADefaultValue: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPROPERTYINT, WPARAM(AKey), LPARAM(ADefaultValue));
end;

function TCustomSciTextEditor.GetLexerLanguage(ALanguage: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETLEXERLANGUAGE, WPARAM(0), LPARAM(ALanguage));
end;

function TCustomSciTextEditor.PrivateLexerCall(AOperation: Integer; APointer: Pointer): Pointer;
begin
  Result := Pointer(SendScintillaEditorMessage(SCI_PRIVATELEXERCALL, WPARAM(AOperation), LPARAM(APointer)));
end;

function TCustomSciTextEditor.PropertyNames(ANames: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_PROPERTYNAMES, WPARAM(0), LPARAM(ANames));
end;

function TCustomSciTextEditor.PropertyType(AName: PAnsiChar): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_PROPERTYTYPE, WPARAM(AName), LPARAM(0));
end;

function TCustomSciTextEditor.DescribeProperty(AName: PAnsiChar; ADescription: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_DESCRIBEPROPERTY, WPARAM(AName), LPARAM(ADescription));
end;

function TCustomSciTextEditor.DescribeKeyWordSets(ADescriptions: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_DESCRIBEKEYWORDSETS, WPARAM(0), LPARAM(ADescriptions));
end;

function TCustomSciTextEditor.GetLineEndTypesSupported(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETLINEENDTYPESSUPPORTED, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.AllocateSubStyles(AStyleBase: Integer; ANumberStyles: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_ALLOCATESUBSTYLES, WPARAM(AStyleBase), LPARAM(ANumberStyles));
end;

function TCustomSciTextEditor.GetSubStylesStart(AStyleBase: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSUBSTYLESSTART, WPARAM(AStyleBase), LPARAM(0));
end;

function TCustomSciTextEditor.GetSubStylesLength(AStyleBase: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSUBSTYLESLENGTH, WPARAM(AStyleBase), LPARAM(0));
end;

function TCustomSciTextEditor.GetStyleFromSubStyle(ASubStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSTYLEFROMSUBSTYLE, WPARAM(ASubStyle), LPARAM(0));
end;

function TCustomSciTextEditor.GetPrimaryStyleFromStyle(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETPRIMARYSTYLEFROMSTYLE, WPARAM(AStyle), LPARAM(0));
end;

procedure TCustomSciTextEditor.FreeSubStyles();
begin
  SendScintillaEditorMessage(SCI_FREESUBSTYLES, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetIdentifiers(AStyle: Integer; AIdentifiers: PAnsiChar);
begin
  SendScintillaEditorMessage(SCI_SETIDENTIFIERS, WPARAM(AStyle), LPARAM(AIdentifiers));
end;

function TCustomSciTextEditor.DistanceToSecondaryStyles(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_DISTANCETOSECONDARYSTYLES, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.GetSubStyleBases(AStyles: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETSUBSTYLEBASES, WPARAM(0), LPARAM(AStyles));
end;

function TCustomSciTextEditor.GetNamedStyles(): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_GETNAMEDSTYLES, WPARAM(0), LPARAM(0));
end;

function TCustomSciTextEditor.NameOfStyle(AStyle: Integer; AName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_NAMEOFSTYLE, WPARAM(AStyle), LPARAM(AName));
end;

function TCustomSciTextEditor.TagsOfStyle(AStyle: Integer; ATags: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_TAGSOFSTYLE, WPARAM(AStyle), LPARAM(ATags));
end;

function TCustomSciTextEditor.DescriptionOfStyle(AStyle: Integer; ADescription: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(SCI_DESCRIPTIONOFSTYLE, WPARAM(AStyle), LPARAM(ADescription));
end;

procedure TCustomSciTextEditor.SetILexer(AIlexer: Pointer);
begin
  SendScintillaEditorMessage(SCI_SETILEXER, WPARAM(0), LPARAM(AIlexer));
end;

function TCustomSciTextEditor.GetBidirectional(): NativeInt;
begin
  Result := SendScintillaEditorMessage(SCI_GETBIDIRECTIONAL, WPARAM(0), LPARAM(0));
end;

procedure TCustomSciTextEditor.SetBidirectional(ABidirectional: NativeInt);
begin
  SendScintillaEditorMessage(SCI_SETBIDIRECTIONAL, WPARAM(ABidirectional), LPARAM(0));
end;

end.
