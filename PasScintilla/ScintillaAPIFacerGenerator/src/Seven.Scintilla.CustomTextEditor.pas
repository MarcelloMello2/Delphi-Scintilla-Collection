unit Seven.Scintilla.CustomTextEditor;

{
  Wrapper Pascal para Scintilla
  Gerado automaticamente por ScintillaFacerGenerator.exe
  Data: 04/06/2025 23:11:24
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
  Vcl.Graphics,
  Vcl.Controls,
  Seven.Scintilla.Types,
  Seven.Scintilla.BaseTextEditor;

  TCustomSciTextEditor = class(TBaseSciTextEditor)
  strict private
    FDirectPtr: Pointer;
    FDirectFunction: Pointer;
  strict protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    // Basics
    /// <summary>
    /// Add text to the document at current position.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure AddText(length: Integer; text: PAnsiChar);
    /// <summary>
    /// Add array of cells to document.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="c">
    /// The c parameter
    /// </param>
    procedure AddStyledText(length: Integer; c: PAnsiChar);
    /// <summary>
    /// Insert string at a position.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure InsertText(pos: Integer; text: PAnsiChar);
    /// <summary>
    /// Change the text that is being inserted in response to SC_MOD_INSERTCHECK
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure ChangeInsertion(length: Integer; text: PAnsiChar);
    /// <summary>
    /// Delete all text in the document.
    /// </summary>
    procedure ClearAll();
    /// <summary>
    /// Delete a range of text in the document.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="lengthDelete">
    /// Position in the document
    /// </param>
    procedure DeleteRange(start: Integer; lengthDelete: Integer);
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
    function GetLength(): Integer;
    /// <summary>
    /// Returns the character byte at the position.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCharAt(pos: Integer): Integer;
    /// <summary>
    /// Returns the position of the caret.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetCurrentPos(): Integer;
    /// <summary>
    /// Returns the position of the opposite end of the selection to the caret.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetAnchor(): Integer;
    /// <summary>
    /// Returns the style byte at the position.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetStyleAt(pos: Integer): Integer;
    /// <summary>
    /// Returns the unsigned style byte at the position.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetStyleIndexAt(pos: Integer): Integer;
    /// <summary>
    /// Redoes the next action on the undo history.
    /// </summary>
    procedure Redo();
    /// <summary>
    /// Choose between collecting actions into the undo
    /// history and discarding them.
    /// </summary>
    /// <param name="collectUndo">
    /// Boolean value
    /// </param>
    procedure SetUndoCollection(collectUndo: Boolean);
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
    /// <param name="tr">
    /// The tr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetStyledText(tr: PSciTextRange): Integer;
    /// <summary>
    /// Retrieve a buffer of cells that can be past 2GB.
    /// Returns the number of bytes in the buffer not including terminating NULs.
    /// </summary>
    /// <param name="tr">
    /// The tr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetStyledTextFull(tr: PSciTextRangeFull): Integer;
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
    /// <param name="markerHandle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function MarkerLineFromHandle(markerHandle: Integer): Integer;
    /// <summary>
    /// Delete a marker.
    /// </summary>
    /// <param name="markerHandle">
    /// Integer value
    /// </param>
    procedure MarkerDeleteHandle(markerHandle: Integer);
    /// <summary>
    /// Retrieve marker handles of a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="which">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerHandleFromLine(line: Integer; which: Integer): Integer;
    /// <summary>
    /// Retrieve marker number of a marker handle
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="which">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerNumberFromLine(line: Integer; which: Integer): Integer;
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
    function GetViewWS(): Integer;
    /// <summary>
    /// Make white space characters invisible, always visible or visible outside indentation.
    /// </summary>
    /// <param name="viewWS">
    /// The viewWS parameter
    /// </param>
    procedure SetViewWS(viewWS: Integer);
    /// <summary>
    /// Retrieve the current tab draw mode.
    /// Returns one of SCTD_* constants.
    /// </summary>
    /// <returns>
    /// Returns the tabdrawmode
    /// </returns>
    function GetTabDrawMode(): Integer;
    /// <summary>
    /// Set how tabs are drawn when visible.
    /// </summary>
    /// <param name="tabDrawMode">
    /// The tabDrawMode parameter
    /// </param>
    procedure SetTabDrawMode(tabDrawMode: Integer);
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
    function PositionFromPoint(x: Integer; y: Integer): Integer;
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
    function PositionFromPointClose(x: Integer; y: Integer): Integer;
    /// <summary>
    /// Set caret to start of a line and ensure it is visible.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    procedure GotoLine(line: Integer);
    /// <summary>
    /// Set caret to a position and ensure it is visible.
    /// </summary>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure GotoPos(caret: Integer);
    /// <summary>
    /// Set the selection anchor to a position. The anchor is the opposite
    /// end of the selection from the caret.
    /// </summary>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure SetAnchor(anchor: Integer);
    /// <summary>
    /// Retrieve the text of the line containing the caret.
    /// Returns the index of the caret on the line.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetCurLine(length: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve the position of the last correctly styled character.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetEndStyled(): Integer;
    /// <summary>
    /// Convert all line endings in the document to one mode.
    /// </summary>
    /// <param name="eolMode">
    /// The eolMode parameter
    /// </param>
    procedure ConvertEOLs(eolMode: Integer);
    /// <summary>
    /// Retrieve the current end of line mode - one of CRLF, CR, or LF.
    /// </summary>
    /// <returns>
    /// Returns the eolmode
    /// </returns>
    function GetEOLMode(): Integer;
    /// <summary>
    /// Set the current end of line mode.
    /// </summary>
    /// <param name="eolMode">
    /// The eolMode parameter
    /// </param>
    procedure SetEOLMode(eolMode: Integer);
    /// <summary>
    /// Set the current styling position to start.
    /// The unused parameter is no longer used and should be set to 0.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="unused">
    /// Integer value
    /// </param>
    procedure StartStyling(start: Integer; unused: Integer);
    /// <summary>
    /// Change style from current styling position for length characters to a style
    /// and move the current styling position to after this newly styled segment.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure SetStyling(length: Integer; style: Integer);
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
    /// <param name="buffered">
    /// Boolean value
    /// </param>
    procedure SetBufferedDraw(buffered: Boolean);
    /// <summary>
    /// Change the visible size of a tab to be a multiple of the width of a space character.
    /// </summary>
    /// <param name="tabWidth">
    /// Integer value
    /// </param>
    procedure SetTabWidth(tabWidth: Integer);
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
    /// <param name="pixels">
    /// Integer value
    /// </param>
    procedure SetTabMinimumWidth(pixels: Integer);
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
    /// <param name="line">
    /// Line number
    /// </param>
    procedure ClearTabStops(line: Integer);
    /// <summary>
    /// Add an explicit tab stop for a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="x">
    /// Integer value
    /// </param>
    procedure AddTabStop(line: Integer; x: Integer);
    /// <summary>
    /// Find the next explicit tab stop position on a line after a position.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="x">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetNextTabStop(line: Integer; x: Integer): Integer;
    /// <summary>
    /// Set the code page used to interpret the bytes of the document as characters.
    /// The SC_CP_UTF8 value can be used to enter Unicode mode.
    /// </summary>
    /// <param name="codePage">
    /// Integer value
    /// </param>
    procedure SetCodePage(codePage: Integer);
    /// <summary>
    /// Set the locale for displaying text.
    /// </summary>
    /// <param name="localeName">
    /// Text string
    /// </param>
    procedure SetFontLocale(localeName: PAnsiChar);
    /// <summary>
    /// Get the locale for displaying text.
    /// </summary>
    /// <param name="localeName">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetFontLocale(localeName: PAnsiChar): Integer;
    /// <summary>
    /// Is the IME displayed in a window or inline?
    /// </summary>
    /// <returns>
    /// Returns the imeinteraction
    /// </returns>
    function GetIMEInteraction(): Integer;
    /// <summary>
    /// Choose to display the IME in a window or inline.
    /// </summary>
    /// <param name="imeInteraction">
    /// The imeInteraction parameter
    /// </param>
    procedure SetIMEInteraction(imeInteraction: Integer);
    /// <summary>
    /// Set the symbol used for a particular marker number.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="markerSymbol">
    /// The markerSymbol parameter
    /// </param>
    procedure MarkerDefine(markerNumber: Integer; markerSymbol: Integer);
    /// <summary>
    /// Set the foreground colour used for a particular marker number.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure MarkerSetFore(markerNumber: Integer; fore: TColor);
    /// <summary>
    /// Set the background colour used for a particular marker number.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure MarkerSetBack(markerNumber: Integer; back: TColor);
    /// <summary>
    /// Set the background colour used for a particular marker number when its folding block is selected.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure MarkerSetBackSelected(markerNumber: Integer; back: TColor);
    /// <summary>
    /// Set the foreground colour used for a particular marker number.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="fore">
    /// The fore parameter
    /// </param>
    procedure MarkerSetForeTranslucent(markerNumber: Integer; fore: TColorAlpha);
    /// <summary>
    /// Set the background colour used for a particular marker number.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="back">
    /// The back parameter
    /// </param>
    procedure MarkerSetBackTranslucent(markerNumber: Integer; back: TColorAlpha);
    /// <summary>
    /// Set the background colour used for a particular marker number when its folding block is selected.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="back">
    /// The back parameter
    /// </param>
    procedure MarkerSetBackSelectedTranslucent(markerNumber: Integer; back: TColorAlpha);
    /// <summary>
    /// Set the width of strokes used in .01 pixels so 50  = 1/2 pixel width.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="hundredths">
    /// Integer value
    /// </param>
    procedure MarkerSetStrokeWidth(markerNumber: Integer; hundredths: Integer);
    /// <summary>
    /// Enable/disable highlight for current folding block (smallest one that contains the caret)
    /// </summary>
    /// <param name="enabled">
    /// Boolean value
    /// </param>
    procedure MarkerEnableHighlight(enabled: Boolean);
    /// <summary>
    /// Add a marker to a line, returning an ID which can be used to find or delete the marker.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerAdd(line: Integer; markerNumber: Integer): Integer;
    /// <summary>
    /// Delete a marker from a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    procedure MarkerDelete(line: Integer; markerNumber: Integer);
    /// <summary>
    /// Delete all markers with a particular number from all lines.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    procedure MarkerDeleteAll(markerNumber: Integer);
    /// <summary>
    /// Get a bit mask of all the markers set on a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarkerGet(line: Integer): Integer;
    /// <summary>
    /// Find the next line at or after lineStart that includes a marker in mask.
    /// Return -1 when no more lines.
    /// </summary>
    /// <param name="lineStart">
    /// Line number
    /// </param>
    /// <param name="markerMask">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function MarkerNext(lineStart: Integer; markerMask: Integer): Integer;
    /// <summary>
    /// Find the previous line before lineStart that includes a marker in mask.
    /// </summary>
    /// <param name="lineStart">
    /// Line number
    /// </param>
    /// <param name="markerMask">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function MarkerPrevious(lineStart: Integer; markerMask: Integer): Integer;
    /// <summary>
    /// Define a marker from a pixmap.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="pixmap">
    /// Text string
    /// </param>
    procedure MarkerDefinePixmap(markerNumber: Integer; pixmap: PAnsiChar);
    /// <summary>
    /// Add a set of markers to a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="markerSet">
    /// Integer value
    /// </param>
    procedure MarkerAddSet(line: Integer; markerSet: Integer);
    /// <summary>
    /// Set the alpha used for a marker that is drawn in the text area, not the margin.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="alpha">
    /// The alpha parameter
    /// </param>
    procedure MarkerSetAlpha(markerNumber: Integer; alpha: Integer);
    /// <summary>
    /// Get the layer used for a marker that is drawn in the text area, not the margin.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function MarkerGetLayer(markerNumber: Integer): Integer;
    /// <summary>
    /// Set the layer used for a marker that is drawn in the text area, not the margin.
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="layer">
    /// The layer parameter
    /// </param>
    procedure MarkerSetLayer(markerNumber: Integer; layer: Integer);
    /// <summary>
    /// Set a margin to be either numeric or symbolic.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <param name="marginType">
    /// The marginType parameter
    /// </param>
    procedure SetMarginTypeN(margin: Integer; marginType: Integer);
    /// <summary>
    /// Retrieve the type of a margin.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the margintypen
    /// </returns>
    function GetMarginTypeN(margin: Integer): Integer;
    /// <summary>
    /// Set the width of a margin to a width expressed in pixels.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <param name="pixelWidth">
    /// Integer value
    /// </param>
    procedure SetMarginWidthN(margin: Integer; pixelWidth: Integer);
    /// <summary>
    /// Retrieve the width of a margin in pixels.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMarginWidthN(margin: Integer): Integer;
    /// <summary>
    /// Set a mask that determines which markers are displayed in a margin.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <param name="mask">
    /// Integer value
    /// </param>
    procedure SetMarginMaskN(margin: Integer; mask: Integer);
    /// <summary>
    /// Retrieve the marker mask of a margin.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetMarginMaskN(margin: Integer): Integer;
    /// <summary>
    /// Make a margin sensitive or insensitive to mouse clicks.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <param name="sensitive">
    /// Boolean value
    /// </param>
    procedure SetMarginSensitiveN(margin: Integer; sensitive: Boolean);
    /// <summary>
    /// Retrieve the mouse click sensitivity of a margin.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetMarginSensitiveN(margin: Integer): Boolean;
    /// <summary>
    /// Set the cursor shown when the mouse is inside a margin.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <param name="cursor">
    /// The cursor parameter
    /// </param>
    procedure SetMarginCursorN(margin: Integer; cursor: Integer);
    /// <summary>
    /// Retrieve the cursor shown in a margin.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the margincursorn
    /// </returns>
    function GetMarginCursorN(margin: Integer): Integer;
    /// <summary>
    /// Set the background colour of a margin. Only visible for SC_MARGIN_COLOUR.
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetMarginBackN(margin: Integer; back: TColor);
    /// <summary>
    /// Retrieve the background colour of a margin
    /// </summary>
    /// <param name="margin">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function GetMarginBackN(margin: Integer): TColor;
    /// <summary>
    /// Allocate a non-standard number of margins.
    /// </summary>
    /// <param name="margins">
    /// Integer value
    /// </param>
    procedure SetMargins(margins: Integer);
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
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure StyleSetFore(style: Integer; fore: TColor);
    /// <summary>
    /// Set the background colour of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure StyleSetBack(style: Integer; back: TColor);
    /// <summary>
    /// Set a style to be bold or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="bold">
    /// Boolean value
    /// </param>
    procedure StyleSetBold(style: Integer; bold: Boolean);
    /// <summary>
    /// Set a style to be italic or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="italic">
    /// Boolean value
    /// </param>
    procedure StyleSetItalic(style: Integer; italic: Boolean);
    /// <summary>
    /// Set the size of characters of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="sizePoints">
    /// Integer value
    /// </param>
    procedure StyleSetSize(style: Integer; sizePoints: Integer);
    /// <summary>
    /// Set the font of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="fontName">
    /// Text string
    /// </param>
    procedure StyleSetFont(style: Integer; fontName: PAnsiChar);
    /// <summary>
    /// Set a style to have its end of line filled or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="eolFilled">
    /// Boolean value
    /// </param>
    procedure StyleSetEOLFilled(style: Integer; eolFilled: Boolean);
    /// <summary>
    /// Reset the default style to its state at startup
    /// </summary>
    procedure StyleResetDefault();
    /// <summary>
    /// Set a style to be underlined or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="underline">
    /// Boolean value
    /// </param>
    procedure StyleSetUnderline(style: Integer; underline: Boolean);
    /// <summary>
    /// Get the foreground colour of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function StyleGetFore(style: Integer): TColor;
    /// <summary>
    /// Get the background colour of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function StyleGetBack(style: Integer): TColor;
    /// <summary>
    /// Get is a style bold or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetBold(style: Integer): Boolean;
    /// <summary>
    /// Get is a style italic or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetItalic(style: Integer): Boolean;
    /// <summary>
    /// Get the size of characters of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetSize(style: Integer): Integer;
    /// <summary>
    /// Get the font of a style.
    /// Returns the length of the fontName
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="fontName">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetFont(style: Integer; fontName: PAnsiChar): Integer;
    /// <summary>
    /// Get is a style to have its end of line filled or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetEOLFilled(style: Integer): Boolean;
    /// <summary>
    /// Get is a style underlined or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetUnderline(style: Integer): Boolean;
    /// <summary>
    /// Get is a style mixed case, or to force upper or lower case.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetCase(style: Integer): Integer;
    /// <summary>
    /// Get the character get of the font in a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetCharacterSet(style: Integer): Integer;
    /// <summary>
    /// Get is a style visible or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetVisible(style: Integer): Boolean;
    /// <summary>
    /// Get is a style changeable or not (read only).
    /// Experimental feature, currently buggy.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetChangeable(style: Integer): Boolean;
    /// <summary>
    /// Get is a style a hotspot or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetHotSpot(style: Integer): Boolean;
    /// <summary>
    /// Set a style to be mixed case, or to force upper or lower case.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="caseVisible">
    /// The caseVisible parameter
    /// </param>
    procedure StyleSetCase(style: Integer; caseVisible: Integer);
    /// <summary>
    /// Set the size of characters of a style. Size is in points multiplied by 100.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="sizeHundredthPoints">
    /// Integer value
    /// </param>
    procedure StyleSetSizeFractional(style: Integer; sizeHundredthPoints: Integer);
    /// <summary>
    /// Get the size of characters of a style in points multiplied by 100
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetSizeFractional(style: Integer): Integer;
    /// <summary>
    /// Set the weight of characters of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="weight">
    /// The weight parameter
    /// </param>
    procedure StyleSetWeight(style: Integer; weight: Integer);
    /// <summary>
    /// Get the weight of characters of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetWeight(style: Integer): Integer;
    /// <summary>
    /// Set the character set of the font in a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="characterSet">
    /// The characterSet parameter
    /// </param>
    procedure StyleSetCharacterSet(style: Integer; characterSet: Integer);
    /// <summary>
    /// Set a style to be a hotspot or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="hotspot">
    /// Boolean value
    /// </param>
    procedure StyleSetHotSpot(style: Integer; hotspot: Boolean);
    /// <summary>
    /// Indicate that a style may be monospaced over ASCII graphics characters which enables optimizations.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="checkMonospaced">
    /// Boolean value
    /// </param>
    procedure StyleSetCheckMonospaced(style: Integer; checkMonospaced: Boolean);
    /// <summary>
    /// Get whether a style may be monospaced.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function StyleGetCheckMonospaced(style: Integer): Boolean;
    /// <summary>
    /// Set the stretch of characters of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="stretch">
    /// The stretch parameter
    /// </param>
    procedure StyleSetStretch(style: Integer; stretch: Integer);
    /// <summary>
    /// Get the stretch of characters of a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function StyleGetStretch(style: Integer): Integer;
    /// <summary>
    /// Set the invisible representation for a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="representation">
    /// Text string
    /// </param>
    procedure StyleSetInvisibleRepresentation(style: Integer; representation: PAnsiChar);
    /// <summary>
    /// Get the invisible representation for a style.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="representation">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function StyleGetInvisibleRepresentation(style: Integer; representation: PAnsiChar): Integer;
    /// <summary>
    /// Set the colour of an element. Translucency (alpha) may or may not be significant
    /// and this may depend on the platform. The alpha byte should commonly be 0xff for opaque.
    /// </summary>
    /// <param name="element">
    /// The element parameter
    /// </param>
    /// <param name="colourElement">
    /// The colourElement parameter
    /// </param>
    procedure SetElementColour(element: Integer; colourElement: TColorAlpha);
    /// <summary>
    /// Get the colour of an element.
    /// </summary>
    /// <param name="element">
    /// The element parameter
    /// </param>
    /// <returns>
    /// Returns the elementcolour
    /// </returns>
    function GetElementColour(element: Integer): TColorAlpha;
    /// <summary>
    /// Use the default or platform-defined colour for an element.
    /// </summary>
    /// <param name="element">
    /// The element parameter
    /// </param>
    procedure ResetElementColour(element: Integer);
    /// <summary>
    /// Get whether an element has been set by SetElementColour.
    /// When false, a platform-defined or default colour is used.
    /// </summary>
    /// <param name="element">
    /// The element parameter
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetElementIsSet(element: Integer): Boolean;
    /// <summary>
    /// Get whether an element supports translucency.
    /// </summary>
    /// <param name="element">
    /// The element parameter
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetElementAllowsTranslucent(element: Integer): Boolean;
    /// <summary>
    /// Get the colour of an element.
    /// </summary>
    /// <param name="element">
    /// The element parameter
    /// </param>
    /// <returns>
    /// Returns the elementbasecolour
    /// </returns>
    function GetElementBaseColour(element: Integer): TColorAlpha;
    /// <summary>
    /// Set the foreground colour of the main and additional selections and whether to use this setting.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetSelFore(useSetting: Boolean; fore: TColor);
    /// <summary>
    /// Set the background colour of the main and additional selections and whether to use this setting.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetSelBack(useSetting: Boolean; back: TColor);
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    /// <returns>
    /// Returns the selalpha
    /// </returns>
    function GetSelAlpha(): Integer;
    /// <summary>
    /// Set the alpha of the selection.
    /// </summary>
    /// <param name="alpha">
    /// The alpha parameter
    /// </param>
    procedure SetSelAlpha(alpha: Integer);
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
    /// <param name="filled">
    /// Boolean value
    /// </param>
    procedure SetSelEOLFilled(filled: Boolean);
    /// <summary>
    /// Get the layer for drawing selections
    /// </summary>
    /// <returns>
    /// Returns the selectionlayer
    /// </returns>
    function GetSelectionLayer(): Integer;
    /// <summary>
    /// Set the layer for drawing selections: either opaquely on base layer or translucently over text
    /// </summary>
    /// <param name="layer">
    /// The layer parameter
    /// </param>
    procedure SetSelectionLayer(layer: Integer);
    /// <summary>
    /// Get the layer of the background of the line containing the caret.
    /// </summary>
    /// <returns>
    /// Returns the caretlinelayer
    /// </returns>
    function GetCaretLineLayer(): Integer;
    /// <summary>
    /// Set the layer of the background of the line containing the caret.
    /// </summary>
    /// <param name="layer">
    /// The layer parameter
    /// </param>
    procedure SetCaretLineLayer(layer: Integer);
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
    /// <param name="subLine">
    /// Boolean value
    /// </param>
    procedure SetCaretLineHighlightSubLine(subLine: Boolean);
    /// <summary>
    /// Set the foreground colour of the caret.
    /// </summary>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetCaretFore(fore: TColor);
    /// <summary>
    /// When key+modifier combination keyDefinition is pressed perform sciCommand.
    /// </summary>
    /// <param name="keyDefinition">
    /// The keyDefinition parameter
    /// </param>
    /// <param name="sciCommand">
    /// Integer value
    /// </param>
    procedure AssignCmdKey(keyDefinition: Integer; sciCommand: Integer);
    /// <summary>
    /// When key+modifier combination keyDefinition is pressed do nothing.
    /// </summary>
    /// <param name="keyDefinition">
    /// The keyDefinition parameter
    /// </param>
    procedure ClearCmdKey(keyDefinition: Integer);
    /// <summary>
    /// Drop all key mappings.
    /// </summary>
    procedure ClearAllCmdKeys();
    /// <summary>
    /// Set the styles for a segment of the document.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="styles">
    /// Text string
    /// </param>
    procedure SetStylingEx(length: Integer; styles: PAnsiChar);
    /// <summary>
    /// Set a style to be visible or not.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="visible">
    /// Boolean value
    /// </param>
    procedure StyleSetVisible(style: Integer; visible: Boolean);
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
    /// <param name="periodMilliseconds">
    /// Integer value
    /// </param>
    procedure SetCaretPeriod(periodMilliseconds: Integer);
    /// <summary>
    /// Set the set of characters making up words for when moving or selecting by word.
    /// First sets defaults like SetCharsDefault.
    /// </summary>
    /// <param name="characters">
    /// Text string
    /// </param>
    procedure SetWordChars(characters: PAnsiChar);
    /// <summary>
    /// Get the set of characters making up words for when moving or selecting by word.
    /// Returns the number of characters
    /// </summary>
    /// <param name="characters">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetWordChars(characters: PAnsiChar): Integer;
    /// <summary>
    /// Set the number of characters to have directly indexed categories
    /// </summary>
    /// <param name="countCharacters">
    /// Integer value
    /// </param>
    procedure SetCharacterCategoryOptimization(countCharacters: Integer);
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
    /// <param name="action">
    /// Integer value
    /// </param>
    procedure SetUndoSavePoint(action: Integer);
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
    /// <param name="action">
    /// Integer value
    /// </param>
    procedure SetUndoDetach(action: Integer);
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
    /// <param name="action">
    /// Integer value
    /// </param>
    procedure SetUndoTentative(action: Integer);
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
    /// <param name="action">
    /// Integer value
    /// </param>
    procedure SetUndoCurrent(action: Integer);
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
    /// <param name="type">
    /// Integer value
    /// </param>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    procedure PushUndoActionType(type: Integer; pos: Integer);
    /// <summary>
    /// Set the text and length of the most recently pushed action
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure ChangeLastUndoActionText(length: Integer; text: PAnsiChar);
    /// <summary>
    /// What is the type of an action?
    /// </summary>
    /// <param name="action">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoActionType(action: Integer): Integer;
    /// <summary>
    /// What is the position of an action?
    /// </summary>
    /// <param name="action">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetUndoActionPosition(action: Integer): Integer;
    /// <summary>
    /// What is the text of an action?
    /// </summary>
    /// <param name="action">
    /// Integer value
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetUndoActionText(action: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Set an indicator to plain, squiggle or TT.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="indicatorStyle">
    /// The indicatorStyle parameter
    /// </param>
    procedure IndicSetStyle(indicator: Integer; indicatorStyle: Integer);
    /// <summary>
    /// Retrieve the style of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetStyle(indicator: Integer): Integer;
    /// <summary>
    /// Set the foreground colour of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure IndicSetFore(indicator: Integer; fore: TColor);
    /// <summary>
    /// Retrieve the foreground colour of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function IndicGetFore(indicator: Integer): TColor;
    /// <summary>
    /// Set an indicator to draw under text or over(default).
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="under">
    /// Boolean value
    /// </param>
    procedure IndicSetUnder(indicator: Integer; under: Boolean);
    /// <summary>
    /// Retrieve whether indicator drawn under or over text.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function IndicGetUnder(indicator: Integer): Boolean;
    /// <summary>
    /// Set a hover indicator to plain, squiggle or TT.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="indicatorStyle">
    /// The indicatorStyle parameter
    /// </param>
    procedure IndicSetHoverStyle(indicator: Integer; indicatorStyle: Integer);
    /// <summary>
    /// Retrieve the hover style of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetHoverStyle(indicator: Integer): Integer;
    /// <summary>
    /// Set the foreground hover colour of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure IndicSetHoverFore(indicator: Integer; fore: TColor);
    /// <summary>
    /// Retrieve the foreground hover colour of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the color value
    /// </returns>
    function IndicGetHoverFore(indicator: Integer): TColor;
    /// <summary>
    /// Set the attributes of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="flags">
    /// The flags parameter
    /// </param>
    procedure IndicSetFlags(indicator: Integer; flags: Integer);
    /// <summary>
    /// Retrieve the attributes of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetFlags(indicator: Integer): Integer;
    /// <summary>
    /// Set the stroke width of an indicator in hundredths of a pixel.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="hundredths">
    /// Integer value
    /// </param>
    procedure IndicSetStrokeWidth(indicator: Integer; hundredths: Integer);
    /// <summary>
    /// Retrieve the stroke width of an indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function IndicGetStrokeWidth(indicator: Integer): Integer;
    /// <summary>
    /// Set the foreground colour of all whitespace and whether to use this setting.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetWhitespaceFore(useSetting: Boolean; fore: TColor);
    /// <summary>
    /// Set the background colour of all whitespace and whether to use this setting.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetWhitespaceBack(useSetting: Boolean; back: TColor);
    /// <summary>
    /// Set the size of the dots used to mark space characters.
    /// </summary>
    /// <param name="size">
    /// Integer value
    /// </param>
    procedure SetWhitespaceSize(size: Integer);
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
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="state">
    /// Integer value
    /// </param>
    procedure SetLineState(line: Integer; state: Integer);
    /// <summary>
    /// Retrieve the extra styling information for a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLineState(line: Integer): Integer;
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
    /// <param name="show">
    /// Boolean value
    /// </param>
    procedure SetCaretLineVisible(show: Boolean);
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
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetCaretLineBack(back: TColor);
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
    /// <param name="width">
    /// Integer value
    /// </param>
    procedure SetCaretLineFrame(width: Integer);
    /// <summary>
    /// Set a style to be changeable or not (read only).
    /// Experimental feature, currently buggy.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="changeable">
    /// Boolean value
    /// </param>
    procedure StyleSetChangeable(style: Integer; changeable: Boolean);
    /// <summary>
    /// Display a auto-completion list.
    /// The lengthEntered parameter indicates how many characters before
    /// the caret should be used to provide context.
    /// </summary>
    /// <param name="lengthEntered">
    /// Position in the document
    /// </param>
    /// <param name="itemList">
    /// Text string
    /// </param>
    procedure AutoCShow(lengthEntered: Integer; itemList: PAnsiChar);
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
    function AutoCPosStart(): Integer;
    /// <summary>
    /// User has selected an item so remove the list and insert the selection.
    /// </summary>
    procedure AutoCComplete();
    /// <summary>
    /// Define a set of character that when typed cancel the auto-completion list.
    /// </summary>
    /// <param name="characterSet">
    /// Text string
    /// </param>
    procedure AutoCStops(characterSet: PAnsiChar);
    /// <summary>
    /// Change the separator character in the string setting up an auto-completion list.
    /// Default is space but can be changed if items contain space.
    /// </summary>
    /// <param name="separatorCharacter">
    /// Integer value
    /// </param>
    procedure AutoCSetSeparator(separatorCharacter: Integer);
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
    /// <param name="select">
    /// Text string
    /// </param>
    procedure AutoCSelect(select: PAnsiChar);
    /// <summary>
    /// Should the auto-completion list be cancelled if the user backspaces to a
    /// position before where the box was created.
    /// </summary>
    /// <param name="cancel">
    /// Boolean value
    /// </param>
    procedure AutoCSetCancelAtStart(cancel: Boolean);
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
    /// <param name="characterSet">
    /// Text string
    /// </param>
    procedure AutoCSetFillUps(characterSet: PAnsiChar);
    /// <summary>
    /// Should a single item auto-completion list automatically choose the item.
    /// </summary>
    /// <param name="chooseSingle">
    /// Boolean value
    /// </param>
    procedure AutoCSetChooseSingle(chooseSingle: Boolean);
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
    /// <param name="ignoreCase">
    /// Boolean value
    /// </param>
    procedure AutoCSetIgnoreCase(ignoreCase: Boolean);
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
    /// <param name="listType">
    /// Integer value
    /// </param>
    /// <param name="itemList">
    /// Text string
    /// </param>
    procedure UserListShow(listType: Integer; itemList: PAnsiChar);
    /// <summary>
    /// Set whether or not autocompletion is hidden automatically when nothing matches.
    /// </summary>
    /// <param name="autoHide">
    /// Boolean value
    /// </param>
    procedure AutoCSetAutoHide(autoHide: Boolean);
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
    /// <param name="options">
    /// The options parameter
    /// </param>
    procedure AutoCSetOptions(options: Integer);
    /// <summary>
    /// Retrieve autocompletion options.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetOptions(): Integer;
    /// <summary>
    /// Set whether or not autocompletion deletes any word characters
    /// after the inserted text upon completion.
    /// </summary>
    /// <param name="dropRestOfWord">
    /// Boolean value
    /// </param>
    procedure AutoCSetDropRestOfWord(dropRestOfWord: Boolean);
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
    /// <param name="type">
    /// Integer value
    /// </param>
    /// <param name="xpmData">
    /// Text string
    /// </param>
    procedure RegisterImage(type: Integer; xpmData: PAnsiChar);
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
    /// <param name="separatorCharacter">
    /// Integer value
    /// </param>
    procedure AutoCSetTypeSeparator(separatorCharacter: Integer);
    /// <summary>
    /// Set the maximum width, in characters, of auto-completion and user lists.
    /// Set to 0 to autosize to fit longest item, which is the default.
    /// </summary>
    /// <param name="characterCount">
    /// Integer value
    /// </param>
    procedure AutoCSetMaxWidth(characterCount: Integer);
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
    /// <param name="rowCount">
    /// Integer value
    /// </param>
    procedure AutoCSetMaxHeight(rowCount: Integer);
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
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure AutoCSetStyle(style: Integer);
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
    /// <param name="indentSize">
    /// Integer value
    /// </param>
    procedure SetIndent(indentSize: Integer);
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
    /// <param name="useTabs">
    /// Boolean value
    /// </param>
    procedure SetUseTabs(useTabs: Boolean);
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
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="indentation">
    /// Integer value
    /// </param>
    procedure SetLineIndentation(line: Integer; indentation: Integer);
    /// <summary>
    /// Retrieve the number of columns that a line is indented.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLineIndentation(line: Integer): Integer;
    /// <summary>
    /// Retrieve the position before the first non indentation character on a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineIndentPosition(line: Integer): Integer;
    /// <summary>
    /// Retrieve the column number of a position, taking tab width into account.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetColumn(pos: Integer): Integer;
    /// <summary>
    /// Count characters between two positions.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CountCharacters(start: Integer; end: Integer): Integer;
    /// <summary>
    /// Count code units between two positions.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function CountCodeUnits(start: Integer; end: Integer): Integer;
    /// <summary>
    /// Show or hide the horizontal scroll bar.
    /// </summary>
    /// <param name="visible">
    /// Boolean value
    /// </param>
    procedure SetHScrollBar(visible: Boolean);
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
    /// <param name="indentView">
    /// The indentView parameter
    /// </param>
    procedure SetIndentationGuides(indentView: Integer);
    /// <summary>
    /// Are the indentation guides visible?
    /// </summary>
    /// <returns>
    /// Returns the indentationguides
    /// </returns>
    function GetIndentationGuides(): Integer;
    /// <summary>
    /// Set the highlighted indentation guide column.
    /// 0 = no highlighted guide.
    /// </summary>
    /// <param name="column">
    /// Position in the document
    /// </param>
    procedure SetHighlightGuide(column: Integer);
    /// <summary>
    /// Get the highlighted indentation guide column.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetHighlightGuide(): Integer;
    /// <summary>
    /// Get the position after the last visible characters on a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineEndPosition(line: Integer): Integer;
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
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetCurrentPos(caret: Integer);
    /// <summary>
    /// Sets the position that starts the selection - this becomes the anchor.
    /// </summary>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure SetSelectionStart(anchor: Integer);
    /// <summary>
    /// Returns the position at the start of the selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionStart(): Integer;
    /// <summary>
    /// Sets the position that ends the selection - this becomes the caret.
    /// </summary>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetSelectionEnd(caret: Integer);
    /// <summary>
    /// Returns the position at the end of the selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionEnd(): Integer;
    /// <summary>
    /// Set caret to a position, while removing any existing selection.
    /// </summary>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetEmptySelection(caret: Integer);
    /// <summary>
    /// Sets the print magnification added to the point size of each style for printing.
    /// </summary>
    /// <param name="magnification">
    /// Integer value
    /// </param>
    procedure SetPrintMagnification(magnification: Integer);
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
    /// <param name="mode">
    /// The mode parameter
    /// </param>
    procedure SetPrintColourMode(mode: Integer);
    /// <summary>
    /// Returns the print colour mode.
    /// </summary>
    /// <returns>
    /// Returns the printcolourmode
    /// </returns>
    function GetPrintColourMode(): Integer;
    /// <summary>
    /// Find some text in the document.
    /// </summary>
    /// <param name="searchFlags">
    /// The searchFlags parameter
    /// </param>
    /// <param name="ft">
    /// The ft parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FindText(searchFlags: Integer; ft: PSciFindText): Integer;
    /// <summary>
    /// Find some text in the document.
    /// </summary>
    /// <param name="searchFlags">
    /// The searchFlags parameter
    /// </param>
    /// <param name="ft">
    /// The ft parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FindTextFull(searchFlags: Integer; ft: PSciFindTextFull): Integer;
    /// <summary>
    /// Draw the document into a display context such as a printer.
    /// </summary>
    /// <param name="draw">
    /// Boolean value
    /// </param>
    /// <param name="fr">
    /// The fr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FormatRange(draw: Boolean; fr: PFormatRange): Integer;
    /// <summary>
    /// Draw the document into a display context such as a printer.
    /// </summary>
    /// <param name="draw">
    /// Boolean value
    /// </param>
    /// <param name="fr">
    /// The fr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FormatRangeFull(draw: Boolean; fr: PFormatRangeFull): Integer;
    /// <summary>
    /// Enable or disable change history.
    /// </summary>
    /// <param name="changeHistory">
    /// The changeHistory parameter
    /// </param>
    procedure SetChangeHistory(changeHistory: Integer);
    /// <summary>
    /// Report change history status.
    /// </summary>
    /// <returns>
    /// Returns the changehistory
    /// </returns>
    function GetChangeHistory(): Integer;
    /// <summary>
    /// Enable or disable undo selection history.
    /// </summary>
    /// <param name="undoSelectionHistory">
    /// The undoSelectionHistory parameter
    /// </param>
    procedure SetUndoSelectionHistory(undoSelectionHistory: Integer);
    /// <summary>
    /// Report undo selection history status.
    /// </summary>
    /// <returns>
    /// Returns the undoselectionhistory
    /// </returns>
    function GetUndoSelectionHistory(): Integer;
    /// <summary>
    /// Set selection from serialized form.
    /// </summary>
    /// <param name="selectionString">
    /// Text string
    /// </param>
    procedure SetSelectionSerialized(selectionString: PAnsiChar);
    /// <summary>
    /// Retrieve serialized form of selection.
    /// </summary>
    /// <param name="selectionString">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionSerialized(selectionString: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve the display line at the top of the display.
    /// </summary>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetFirstVisibleLine(): Integer;
    /// <summary>
    /// Retrieve the contents of a line.
    /// Returns the length of the line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLine(line: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Returns the number of lines in the document. There is always at least one.
    /// </summary>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetLineCount(): Integer;
    /// <summary>
    /// Enlarge the number of lines allocated.
    /// </summary>
    /// <param name="lines">
    /// Line number
    /// </param>
    procedure AllocateLines(lines: Integer);
    /// <summary>
    /// Sets the size in pixels of the left margin.
    /// </summary>
    /// <param name="pixelWidth">
    /// Integer value
    /// </param>
    procedure SetMarginLeft(pixelWidth: Integer);
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
    /// <param name="pixelWidth">
    /// Integer value
    /// </param>
    procedure SetMarginRight(pixelWidth: Integer);
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
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetSel(anchor: Integer; caret: Integer);
    /// <summary>
    /// Retrieve the selected text.
    /// Return the length of the text.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelText(text: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a range of text.
    /// Return the length of the text.
    /// </summary>
    /// <param name="tr">
    /// The tr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTextRange(tr: PSciTextRange): Integer;
    /// <summary>
    /// Retrieve a range of text that can be past 2GB.
    /// Return the length of the text.
    /// </summary>
    /// <param name="tr">
    /// The tr parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTextRangeFull(tr: PSciTextRangeFull): Integer;
    /// <summary>
    /// Draw the selection either highlighted or in normal (non-highlighted) style.
    /// </summary>
    /// <param name="hide">
    /// Boolean value
    /// </param>
    procedure HideSelection(hide: Boolean);
    function GetSelectionHidden(): Boolean;
    /// <summary>
    /// Retrieve the x value of the point in the window where a position is displayed.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function PointXFromPosition(pos: Integer): Integer;
    /// <summary>
    /// Retrieve the y value of the point in the window where a position is displayed.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function PointYFromPosition(pos: Integer): Integer;
    /// <summary>
    /// Retrieve the line containing a position.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function LineFromPosition(pos: Integer): Integer;
    /// <summary>
    /// Retrieve the position at the start of a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionFromLine(line: Integer): Integer;
    /// <summary>
    /// Scroll horizontally and vertically.
    /// </summary>
    /// <param name="columns">
    /// Position in the document
    /// </param>
    /// <param name="lines">
    /// Line number
    /// </param>
    procedure LineScroll(columns: Integer; lines: Integer);
    /// <summary>
    /// Ensure the caret is visible.
    /// </summary>
    procedure ScrollCaret();
    /// <summary>
    /// Scroll the argument positions and the range between them into view giving
    /// priority to the primary position then the secondary position.
    /// This may be used to make a search match visible.
    /// </summary>
    /// <param name="secondary">
    /// Position in the document
    /// </param>
    /// <param name="primary">
    /// Position in the document
    /// </param>
    procedure ScrollRange(secondary: Integer; primary: Integer);
    /// <summary>
    /// Replace the selected text with the argument text.
    /// </summary>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure ReplaceSel(text: PAnsiChar);
    /// <summary>
    /// Set to read only or read write.
    /// </summary>
    /// <param name="readOnly">
    /// Boolean value
    /// </param>
    procedure SetReadOnly(readOnly: Boolean);
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
    /// <param name="text">
    /// Text string
    /// </param>
    procedure SetText(text: PAnsiChar);
    /// <summary>
    /// Retrieve all the text in the document.
    /// Returns number of characters retrieved.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetText(length: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve the number of characters in the document.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTextLength(): Integer;
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
    /// <param name="overType">
    /// Boolean value
    /// </param>
    procedure SetOvertype(overType: Boolean);
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
    /// <param name="pixelWidth">
    /// Integer value
    /// </param>
    procedure SetCaretWidth(pixelWidth: Integer);
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
    /// <param name="start">
    /// Position in the document
    /// </param>
    procedure SetTargetStart(start: Integer);
    /// <summary>
    /// Get the position that starts the target.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetStart(): Integer;
    /// <summary>
    /// Sets the virtual space of the target start
    /// </summary>
    /// <param name="space">
    /// Position in the document
    /// </param>
    procedure SetTargetStartVirtualSpace(space: Integer);
    /// <summary>
    /// Get the virtual space of the target start
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetStartVirtualSpace(): Integer;
    /// <summary>
    /// Sets the position that ends the target which is used for updating the
    /// document without affecting the scroll position.
    /// </summary>
    /// <param name="end">
    /// Position in the document
    /// </param>
    procedure SetTargetEnd(end: Integer);
    /// <summary>
    /// Get the position that ends the target.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetEnd(): Integer;
    /// <summary>
    /// Sets the virtual space of the target end
    /// </summary>
    /// <param name="space">
    /// Position in the document
    /// </param>
    procedure SetTargetEndVirtualSpace(space: Integer);
    /// <summary>
    /// Get the virtual space of the target end
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetEndVirtualSpace(): Integer;
    /// <summary>
    /// Sets both the start and end of the target in one call.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    procedure SetTargetRange(start: Integer; end: Integer);
    /// <summary>
    /// Retrieve the text in the target.
    /// </summary>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetTargetText(text: PAnsiChar): Integer;
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
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function ReplaceTarget(length: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Replace the target text with the argument text after \d processing.
    /// Text is counted so it can contain NULs.
    /// Looks for \d where d is between 1 and 9 and replaces these with the strings
    /// matched in the last search operation which were surrounded by \( and \).
    /// Returns the length of the replacement text including any change
    /// caused by processing the \d patterns.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function ReplaceTargetRE(length: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Replace the target text with the argument text but ignore prefix and suffix that
    /// are the same as current.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function ReplaceTargetMinimal(length: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Search for a counted string in the target and set the target to the found
    /// range. Text is counted so it can contain NULs.
    /// Returns start of found range or -1 for failure in which case target is not moved.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function SearchInTarget(length: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Set the search flags used by SearchInTarget.
    /// </summary>
    /// <param name="searchFlags">
    /// The searchFlags parameter
    /// </param>
    procedure SetSearchFlags(searchFlags: Integer);
    /// <summary>
    /// Get the search flags used by SearchInTarget.
    /// </summary>
    /// <returns>
    /// Returns the searchflags
    /// </returns>
    function GetSearchFlags(): Integer;
    /// <summary>
    /// Show a call tip containing a definition near position pos.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="definition">
    /// Text string
    /// </param>
    procedure CallTipShow(pos: Integer; definition: PAnsiChar);
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
    function CallTipPosStart(): Integer;
    /// <summary>
    /// Set the start position in order to change when backspacing removes the calltip.
    /// </summary>
    /// <param name="posStart">
    /// Position in the document
    /// </param>
    procedure CallTipSetPosStart(posStart: Integer);
    /// <summary>
    /// Highlight a segment of the definition.
    /// </summary>
    /// <param name="highlightStart">
    /// Position in the document
    /// </param>
    /// <param name="highlightEnd">
    /// Position in the document
    /// </param>
    procedure CallTipSetHlt(highlightStart: Integer; highlightEnd: Integer);
    /// <summary>
    /// Set the background colour for the call tip.
    /// </summary>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure CallTipSetBack(back: TColor);
    /// <summary>
    /// Set the foreground colour for the call tip.
    /// </summary>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure CallTipSetFore(fore: TColor);
    /// <summary>
    /// Set the foreground colour for the highlighted part of the call tip.
    /// </summary>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure CallTipSetForeHlt(fore: TColor);
    /// <summary>
    /// Enable use of STYLE_CALLTIP and set call tip tab size in pixels.
    /// </summary>
    /// <param name="tabSize">
    /// Integer value
    /// </param>
    procedure CallTipUseStyle(tabSize: Integer);
    /// <summary>
    /// Set position of calltip, above or below text.
    /// </summary>
    /// <param name="above">
    /// Boolean value
    /// </param>
    procedure CallTipSetPosition(above: Boolean);
    /// <summary>
    /// Find the display line of a document line taking hidden lines into account.
    /// </summary>
    /// <param name="docLine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function VisibleFromDocLine(docLine: Integer): Integer;
    /// <summary>
    /// Find the document line of a display line taking hidden lines into account.
    /// </summary>
    /// <param name="displayLine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function DocLineFromVisible(displayLine: Integer): Integer;
    /// <summary>
    /// The number of display lines needed to wrap a document line
    /// </summary>
    /// <param name="docLine">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function WrapCount(docLine: Integer): Integer;
    /// <summary>
    /// Set the fold level of a line.
    /// This encodes an integer level along with flags indicating whether the
    /// line is a header and whether it is effectively white space.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="level">
    /// The level parameter
    /// </param>
    procedure SetFoldLevel(line: Integer; level: Integer);
    /// <summary>
    /// Retrieve the fold level of a line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the foldlevel
    /// </returns>
    function GetFoldLevel(line: Integer): Integer;
    /// <summary>
    /// Find the last child line of a header line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="level">
    /// The level parameter
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetLastChild(line: Integer; level: Integer): Integer;
    /// <summary>
    /// Find the parent line of a child line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function GetFoldParent(line: Integer): Integer;
    /// <summary>
    /// Make a range of lines visible.
    /// </summary>
    /// <param name="lineStart">
    /// Line number
    /// </param>
    /// <param name="lineEnd">
    /// Line number
    /// </param>
    procedure ShowLines(lineStart: Integer; lineEnd: Integer);
    /// <summary>
    /// Make a range of lines invisible.
    /// </summary>
    /// <param name="lineStart">
    /// Line number
    /// </param>
    /// <param name="lineEnd">
    /// Line number
    /// </param>
    procedure HideLines(lineStart: Integer; lineEnd: Integer);
    /// <summary>
    /// Is a line visible?
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetLineVisible(line: Integer): Boolean;
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
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="expanded">
    /// Boolean value
    /// </param>
    procedure SetFoldExpanded(line: Integer; expanded: Boolean);
    /// <summary>
    /// Is a header line expanded?
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function GetFoldExpanded(line: Integer): Boolean;
    /// <summary>
    /// Switch a header line between expanded and contracted.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    procedure ToggleFold(line: Integer);
    /// <summary>
    /// Switch a header line between expanded and contracted and show some text after the line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure ToggleFoldShowText(line: Integer; text: PAnsiChar);
    /// <summary>
    /// Set the style of fold display text.
    /// </summary>
    /// <param name="style">
    /// The style parameter
    /// </param>
    procedure FoldDisplayTextSetStyle(style: Integer);
    /// <summary>
    /// Get the style of fold display text.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function FoldDisplayTextGetStyle(): Integer;
    /// <summary>
    /// Set the default fold display text.
    /// </summary>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure SetDefaultFoldDisplayText(text: PAnsiChar);
    /// <summary>
    /// Get the default fold display text.
    /// </summary>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetDefaultFoldDisplayText(text: PAnsiChar): Integer;
    /// <summary>
    /// Expand or contract a fold header.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="action">
    /// The action parameter
    /// </param>
    procedure FoldLine(line: Integer; action: Integer);
    /// <summary>
    /// Expand or contract a fold header and its children.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="action">
    /// The action parameter
    /// </param>
    procedure FoldChildren(line: Integer; action: Integer);
    /// <summary>
    /// Expand a fold header and all children. Use the level argument instead of the line's current level.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="level">
    /// The level parameter
    /// </param>
    procedure ExpandChildren(line: Integer; level: Integer);
    /// <summary>
    /// Expand or contract all fold headers.
    /// </summary>
    /// <param name="action">
    /// The action parameter
    /// </param>
    procedure FoldAll(action: Integer);
    /// <summary>
    /// Ensure a particular line is visible by expanding any header line hiding it.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    procedure EnsureVisible(line: Integer);
    /// <summary>
    /// Set automatic folding behaviours.
    /// </summary>
    /// <param name="automaticFold">
    /// The automaticFold parameter
    /// </param>
    procedure SetAutomaticFold(automaticFold: Integer);
    /// <summary>
    /// Get automatic folding behaviours.
    /// </summary>
    /// <returns>
    /// Returns the automaticfold
    /// </returns>
    function GetAutomaticFold(): Integer;
    /// <summary>
    /// Set some style options for folding.
    /// </summary>
    /// <param name="flags">
    /// The flags parameter
    /// </param>
    procedure SetFoldFlags(flags: Integer);
    /// <summary>
    /// Ensure a particular line is visible by expanding any header line hiding it.
    /// Use the currently set visibility policy to determine which range to display.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    procedure EnsureVisibleEnforcePolicy(line: Integer);
    /// <summary>
    /// Sets whether a tab pressed when caret is within indentation indents.
    /// </summary>
    /// <param name="tabIndents">
    /// Boolean value
    /// </param>
    procedure SetTabIndents(tabIndents: Boolean);
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
    /// <param name="bsUnIndents">
    /// Boolean value
    /// </param>
    procedure SetBackSpaceUnIndents(bsUnIndents: Boolean);
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
    /// <param name="periodMilliseconds">
    /// Integer value
    /// </param>
    procedure SetMouseDwellTime(periodMilliseconds: Integer);
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
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="onlyWordCharacters">
    /// Boolean value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function WordStartPosition(pos: Integer; onlyWordCharacters: Boolean): Integer;
    /// <summary>
    /// Get position of end of word.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="onlyWordCharacters">
    /// Boolean value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function WordEndPosition(pos: Integer; onlyWordCharacters: Boolean): Integer;
    /// <summary>
    /// Is the range start..end considered a word?
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function IsRangeWord(start: Integer; end: Integer): Boolean;
    /// <summary>
    /// Sets limits to idle styling.
    /// </summary>
    /// <param name="idleStyling">
    /// The idleStyling parameter
    /// </param>
    procedure SetIdleStyling(idleStyling: Integer);
    /// <summary>
    /// Retrieve the limits to idle styling.
    /// </summary>
    /// <returns>
    /// Returns the idlestyling
    /// </returns>
    function GetIdleStyling(): Integer;
    /// <summary>
    /// Sets whether text is word wrapped.
    /// </summary>
    /// <param name="wrapMode">
    /// The wrapMode parameter
    /// </param>
    procedure SetWrapMode(wrapMode: Integer);
    /// <summary>
    /// Retrieve whether text is word wrapped.
    /// </summary>
    /// <returns>
    /// Returns the wrapmode
    /// </returns>
    function GetWrapMode(): Integer;
    /// <summary>
    /// Set the display mode of visual flags for wrapped lines.
    /// </summary>
    /// <param name="wrapVisualFlags">
    /// The wrapVisualFlags parameter
    /// </param>
    procedure SetWrapVisualFlags(wrapVisualFlags: Integer);
    /// <summary>
    /// Retrive the display mode of visual flags for wrapped lines.
    /// </summary>
    /// <returns>
    /// Returns the wrapvisualflags
    /// </returns>
    function GetWrapVisualFlags(): Integer;
    /// <summary>
    /// Set the location of visual flags for wrapped lines.
    /// </summary>
    /// <param name="wrapVisualFlagsLocation">
    /// The wrapVisualFlagsLocation parameter
    /// </param>
    procedure SetWrapVisualFlagsLocation(wrapVisualFlagsLocation: Integer);
    /// <summary>
    /// Retrive the location of visual flags for wrapped lines.
    /// </summary>
    /// <returns>
    /// Returns the wrapvisualflagslocation
    /// </returns>
    function GetWrapVisualFlagsLocation(): Integer;
    /// <summary>
    /// Set the start indent for wrapped lines.
    /// </summary>
    /// <param name="indent">
    /// Integer value
    /// </param>
    procedure SetWrapStartIndent(indent: Integer);
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
    /// <param name="wrapIndentMode">
    /// The wrapIndentMode parameter
    /// </param>
    procedure SetWrapIndentMode(wrapIndentMode: Integer);
    /// <summary>
    /// Retrieve how wrapped sublines are placed. Default is fixed.
    /// </summary>
    /// <returns>
    /// Returns the wrapindentmode
    /// </returns>
    function GetWrapIndentMode(): Integer;
    /// <summary>
    /// Sets the degree of caching of layout information.
    /// </summary>
    /// <param name="cacheMode">
    /// The cacheMode parameter
    /// </param>
    procedure SetLayoutCache(cacheMode: Integer);
    /// <summary>
    /// Retrieve the degree of caching of layout information.
    /// </summary>
    /// <returns>
    /// Returns the layoutcache
    /// </returns>
    function GetLayoutCache(): Integer;
    /// <summary>
    /// Sets the document width assumed for scrolling.
    /// </summary>
    /// <param name="pixelWidth">
    /// Integer value
    /// </param>
    procedure SetScrollWidth(pixelWidth: Integer);
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
    /// <param name="tracking">
    /// Boolean value
    /// </param>
    procedure SetScrollWidthTracking(tracking: Boolean);
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
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function TextWidth(style: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Sets the scroll range so that maximum scroll position has
    /// the last line at the bottom of the view (default).
    /// Setting this to false allows scrolling one page below the last line.
    /// </summary>
    /// <param name="endAtLastLine">
    /// Boolean value
    /// </param>
    procedure SetEndAtLastLine(endAtLastLine: Boolean);
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
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function TextHeight(line: Integer): Integer;
    /// <summary>
    /// Show or hide the vertical scroll bar.
    /// </summary>
    /// <param name="visible">
    /// Boolean value
    /// </param>
    procedure SetVScrollBar(visible: Boolean);
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
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure AppendText(length: Integer; text: PAnsiChar);
    /// <summary>
    /// How many phases is drawing done in?
    /// </summary>
    /// <returns>
    /// Returns the phasesdraw
    /// </returns>
    function GetPhasesDraw(): Integer;
    /// <summary>
    /// In one phase draw, text is drawn in a series of rectangular blocks with no overlap.
    /// In two phase draw, text is drawn in a series of lines allowing runs to overlap horizontally.
    /// In multiple phase draw, each element is drawn over the whole drawing area, allowing text
    /// to overlap from one line to the next.
    /// </summary>
    /// <param name="phases">
    /// The phases parameter
    /// </param>
    procedure SetPhasesDraw(phases: Integer);
    /// <summary>
    /// Choose the quality level for text from the FontQuality enumeration.
    /// </summary>
    /// <param name="fontQuality">
    /// The fontQuality parameter
    /// </param>
    procedure SetFontQuality(fontQuality: Integer);
    /// <summary>
    /// Retrieve the quality level for text.
    /// </summary>
    /// <returns>
    /// Returns the fontquality
    /// </returns>
    function GetFontQuality(): Integer;
    /// <summary>
    /// Scroll so that a display line is at the top of the display.
    /// </summary>
    /// <param name="displayLine">
    /// Line number
    /// </param>
    procedure SetFirstVisibleLine(displayLine: Integer);
    /// <summary>
    /// Change the effect of pasting when there are multiple selections.
    /// </summary>
    /// <param name="multiPaste">
    /// The multiPaste parameter
    /// </param>
    procedure SetMultiPaste(multiPaste: Integer);
    /// <summary>
    /// Retrieve the effect of pasting when there are multiple selections.
    /// </summary>
    /// <returns>
    /// Returns the multipaste
    /// </returns>
    function GetMultiPaste(): Integer;
    /// <summary>
    /// Retrieve the value of a tag from a regular expression search.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="tagNumber">
    /// Integer value
    /// </param>
    /// <param name="tagValue">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetTag(tagNumber: Integer; tagValue: PAnsiChar): Integer;
    /// <summary>
    /// Join the lines in the target.
    /// </summary>
    procedure LinesJoin();
    /// <summary>
    /// Split the lines in the target into lines that are less wide than pixelWidth
    /// where possible.
    /// </summary>
    /// <param name="pixelWidth">
    /// Integer value
    /// </param>
    procedure LinesSplit(pixelWidth: Integer);
    /// <summary>
    /// Set one of the colours used as a chequerboard pattern in the fold margin
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetFoldMarginColour(useSetting: Boolean; back: TColor);
    /// <summary>
    /// Set the other colour used as a chequerboard pattern in the fold margin
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetFoldMarginHiColour(useSetting: Boolean; fore: TColor);
    /// <summary>
    /// Enable or disable accessibility.
    /// </summary>
    /// <param name="accessibility">
    /// The accessibility parameter
    /// </param>
    procedure SetAccessibility(accessibility: Integer);
    /// <summary>
    /// Report accessibility status.
    /// </summary>
    /// <returns>
    /// Returns the accessibility
    /// </returns>
    function GetAccessibility(): Integer;
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
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function LineLength(line: Integer): Integer;
    /// <summary>
    /// Highlight the characters at two positions.
    /// </summary>
    /// <param name="posA">
    /// Position in the document
    /// </param>
    /// <param name="posB">
    /// Position in the document
    /// </param>
    procedure BraceHighlight(posA: Integer; posB: Integer);
    /// <summary>
    /// Use specified indicator to highlight matching braces instead of changing their style.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    procedure BraceHighlightIndicator(useSetting: Boolean; indicator: Integer);
    /// <summary>
    /// Highlight the character at a position indicating there is no matching brace.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    procedure BraceBadLight(pos: Integer);
    /// <summary>
    /// Use specified indicator to highlight non matching brace instead of changing its style.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    procedure BraceBadLightIndicator(useSetting: Boolean; indicator: Integer);
    /// <summary>
    /// Find the position of a matching brace or INVALID_POSITION if no match.
    /// The maxReStyle must be 0 for now. It may be defined in a future release.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="maxReStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function BraceMatch(pos: Integer; maxReStyle: Integer): Integer;
    /// <summary>
    /// Similar to BraceMatch, but matching starts at the explicit start position.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="startPos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function BraceMatchNext(pos: Integer; startPos: Integer): Integer;
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
    /// <param name="visible">
    /// Boolean value
    /// </param>
    procedure SetViewEOL(visible: Boolean);
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
    /// <param name="doc">
    /// The doc parameter
    /// </param>
    procedure SetDocPointer(doc: Pointer);
    /// <summary>
    /// Set which document modification events are sent to the container.
    /// </summary>
    /// <param name="eventMask">
    /// The eventMask parameter
    /// </param>
    procedure SetModEventMask(eventMask: Integer);
    /// <summary>
    /// Retrieve the column number which text should be kept within.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetEdgeColumn(): Integer;
    /// <summary>
    /// Set the column number of the edge.
    /// If text goes past the edge then it is highlighted.
    /// </summary>
    /// <param name="column">
    /// Position in the document
    /// </param>
    procedure SetEdgeColumn(column: Integer);
    /// <summary>
    /// Retrieve the edge highlight mode.
    /// </summary>
    /// <returns>
    /// Returns the edgemode
    /// </returns>
    function GetEdgeMode(): Integer;
    /// <summary>
    /// The edge may be displayed by a line (EDGE_LINE/EDGE_MULTILINE) or by highlighting text that
    /// goes beyond it (EDGE_BACKGROUND) or not displayed at all (EDGE_NONE).
    /// </summary>
    /// <param name="edgeMode">
    /// The edgeMode parameter
    /// </param>
    procedure SetEdgeMode(edgeMode: Integer);
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
    /// <param name="edgeColour">
    /// Color value
    /// </param>
    procedure SetEdgeColour(edgeColour: TColor);
    /// <summary>
    /// Add a new vertical edge to the view.
    /// </summary>
    /// <param name="column">
    /// Position in the document
    /// </param>
    /// <param name="edgeColour">
    /// Color value
    /// </param>
    procedure MultiEdgeAddLine(column: Integer; edgeColour: TColor);
    /// <summary>
    /// Clear all vertical edges.
    /// </summary>
    procedure MultiEdgeClearAll();
    /// <summary>
    /// Get multi edge positions.
    /// </summary>
    /// <param name="which">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetMultiEdgeColumn(which: Integer): Integer;
    /// <summary>
    /// Sets the current caret position to be the search anchor.
    /// </summary>
    procedure SearchAnchor();
    /// <summary>
    /// Find some text starting at the search anchor.
    /// Does not ensure the selection is visible.
    /// </summary>
    /// <param name="searchFlags">
    /// The searchFlags parameter
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function SearchNext(searchFlags: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Find some text starting at the search anchor and moving backwards.
    /// Does not ensure the selection is visible.
    /// </summary>
    /// <param name="searchFlags">
    /// The searchFlags parameter
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function SearchPrev(searchFlags: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Retrieves the number of lines completely visible.
    /// </summary>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function LinesOnScreen(): Integer;
    /// <summary>
    /// Set whether a pop up menu is displayed automatically when the user presses
    /// the wrong mouse button on certain areas.
    /// </summary>
    /// <param name="popUpMode">
    /// The popUpMode parameter
    /// </param>
    procedure UsePopUp(popUpMode: Integer);
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
    /// <param name="zoomInPoints">
    /// Integer value
    /// </param>
    procedure SetZoom(zoomInPoints: Integer);
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
    /// <param name="bytes">
    /// Position in the document
    /// </param>
    /// <param name="documentOptions">
    /// The documentOptions parameter
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function CreateDocument(bytes: Integer; documentOptions: Integer): Pointer;
    /// <summary>
    /// Extend life of document.
    /// </summary>
    /// <param name="doc">
    /// The doc parameter
    /// </param>
    procedure AddRefDocument(doc: Pointer);
    /// <summary>
    /// Release a reference to the document, deleting document if it fades to black.
    /// </summary>
    /// <param name="doc">
    /// The doc parameter
    /// </param>
    procedure ReleaseDocument(doc: Pointer);
    /// <summary>
    /// Get which document options are set.
    /// </summary>
    /// <returns>
    /// Returns the documentoptions
    /// </returns>
    function GetDocumentOptions(): Integer;
    /// <summary>
    /// Get which document modification events are sent to the container.
    /// </summary>
    /// <returns>
    /// Returns the modeventmask
    /// </returns>
    function GetModEventMask(): Integer;
    /// <summary>
    /// Set whether command events are sent to the container.
    /// </summary>
    /// <param name="commandEvents">
    /// Boolean value
    /// </param>
    procedure SetCommandEvents(commandEvents: Boolean);
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
    /// <param name="focus">
    /// Boolean value
    /// </param>
    procedure SetFocus(focus: Boolean);
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
    /// <param name="status">
    /// The status parameter
    /// </param>
    procedure SetStatus(status: Integer);
    /// <summary>
    /// Get error status.
    /// </summary>
    /// <returns>
    /// Returns the status
    /// </returns>
    function GetStatus(): Integer;
    /// <summary>
    /// Set whether the mouse is captured when its button is pressed.
    /// </summary>
    /// <param name="captures">
    /// Boolean value
    /// </param>
    procedure SetMouseDownCaptures(captures: Boolean);
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
    /// <param name="captures">
    /// Boolean value
    /// </param>
    procedure SetMouseWheelCaptures(captures: Boolean);
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
    /// <param name="cursorType">
    /// The cursorType parameter
    /// </param>
    procedure SetCursor(cursorType: Integer);
    /// <summary>
    /// Get cursor type.
    /// </summary>
    /// <returns>
    /// Returns the cursor
    /// </returns>
    function GetCursor(): Integer;
    /// <summary>
    /// Change the way control characters are displayed:
    /// If symbol is < 32, keep the drawn way, else, use the given character.
    /// </summary>
    /// <param name="symbol">
    /// Integer value
    /// </param>
    procedure SetControlCharSymbol(symbol: Integer);
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
    /// <param name="visiblePolicy">
    /// The visiblePolicy parameter
    /// </param>
    /// <param name="visibleSlop">
    /// Integer value
    /// </param>
    procedure SetVisiblePolicy(visiblePolicy: Integer; visibleSlop: Integer);
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
    /// <param name="xOffset">
    /// Integer value
    /// </param>
    procedure SetXOffset(xOffset: Integer);
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
    /// <param name="caretPolicy">
    /// The caretPolicy parameter
    /// </param>
    /// <param name="caretSlop">
    /// Integer value
    /// </param>
    procedure SetXCaretPolicy(caretPolicy: Integer; caretSlop: Integer);
    /// <summary>
    /// Set the way the line the caret is on is kept visible.
    /// The exclusion zone is given in lines.
    /// </summary>
    /// <param name="caretPolicy">
    /// The caretPolicy parameter
    /// </param>
    /// <param name="caretSlop">
    /// Integer value
    /// </param>
    procedure SetYCaretPolicy(caretPolicy: Integer; caretSlop: Integer);
    /// <summary>
    /// Set printing to line wrapped (SC_WRAP_WORD) or not line wrapped (SC_WRAP_NONE).
    /// </summary>
    /// <param name="wrapMode">
    /// The wrapMode parameter
    /// </param>
    procedure SetPrintWrapMode(wrapMode: Integer);
    /// <summary>
    /// Is printing line wrapped?
    /// </summary>
    /// <returns>
    /// Returns the printwrapmode
    /// </returns>
    function GetPrintWrapMode(): Integer;
    /// <summary>
    /// Set a fore colour for active hotspots.
    /// </summary>
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetHotspotActiveFore(useSetting: Boolean; fore: TColor);
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
    /// <param name="useSetting">
    /// Boolean value
    /// </param>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetHotspotActiveBack(useSetting: Boolean; back: TColor);
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
    /// <param name="underline">
    /// Boolean value
    /// </param>
    procedure SetHotspotActiveUnderline(underline: Boolean);
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
    /// <param name="singleLine">
    /// Boolean value
    /// </param>
    procedure SetHotspotSingleLine(singleLine: Boolean);
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
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionBefore(pos: Integer): Integer;
    /// <summary>
    /// Given a valid document position, return the next position taking code
    /// page into account. Maximum value returned is the last position in the document.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionAfter(pos: Integer): Integer;
    /// <summary>
    /// Given a valid document position, return a position that differs in a number
    /// of characters. Returned value is always between 0 and last position in document.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="relative">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionRelative(pos: Integer; relative: Integer): Integer;
    /// <summary>
    /// Given a valid document position, return a position that differs in a number
    /// of UTF-16 code units. Returned value is always between 0 and last position in document.
    /// The result may point half way (2 bytes) inside a non-BMP character.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="relative">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function PositionRelativeCodeUnits(pos: Integer; relative: Integer): Integer;
    /// <summary>
    /// Copy a range of text to the clipboard. Positions are clipped into the document.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    procedure CopyRange(start: Integer; end: Integer);
    /// <summary>
    /// Copy argument text to the clipboard.
    /// </summary>
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure CopyText(length: Integer; text: PAnsiChar);
    /// <summary>
    /// Set the selection mode to stream (SC_SEL_STREAM) or rectangular (SC_SEL_RECTANGLE/SC_SEL_THIN) or
    /// by lines (SC_SEL_LINES).
    /// </summary>
    /// <param name="selectionMode">
    /// The selectionMode parameter
    /// </param>
    procedure SetSelectionMode(selectionMode: Integer);
    /// <summary>
    /// Set the selection mode to stream (SC_SEL_STREAM) or rectangular (SC_SEL_RECTANGLE/SC_SEL_THIN) or
    /// by lines (SC_SEL_LINES) without changing MoveExtendsSelection.
    /// </summary>
    /// <param name="selectionMode">
    /// The selectionMode parameter
    /// </param>
    procedure ChangeSelectionMode(selectionMode: Integer);
    /// <summary>
    /// Get the mode of the current selection.
    /// </summary>
    /// <returns>
    /// Returns the selectionmode
    /// </returns>
    function GetSelectionMode(): Integer;
    /// <summary>
    /// Set whether or not regular caret moves will extend or reduce the selection.
    /// </summary>
    /// <param name="moveExtendsSelection">
    /// Boolean value
    /// </param>
    procedure SetMoveExtendsSelection(moveExtendsSelection: Boolean);
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
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineSelStartPosition(line: Integer): Integer;
    /// <summary>
    /// Retrieve the position of the end of the selection at the given line (INVALID_POSITION if no selection on this line).
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetLineSelEndPosition(line: Integer): Integer;
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
    /// <param name="characters">
    /// Text string
    /// </param>
    procedure SetWhitespaceChars(characters: PAnsiChar);
    /// <summary>
    /// Get the set of characters making up whitespace for when moving or selecting by word.
    /// </summary>
    /// <param name="characters">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetWhitespaceChars(characters: PAnsiChar): Integer;
    /// <summary>
    /// Set the set of characters making up punctuation characters
    /// Should be called after SetWordChars.
    /// </summary>
    /// <param name="characters">
    /// Text string
    /// </param>
    procedure SetPunctuationChars(characters: PAnsiChar);
    /// <summary>
    /// Get the set of characters making up punctuation characters
    /// </summary>
    /// <param name="characters">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPunctuationChars(characters: PAnsiChar): Integer;
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
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AutoCGetCurrentText(text: PAnsiChar): Integer;
    /// <summary>
    /// Set auto-completion case insensitive behaviour to either prefer case-sensitive matches or have no preference.
    /// </summary>
    /// <param name="behaviour">
    /// The behaviour parameter
    /// </param>
    procedure AutoCSetCaseInsensitiveBehaviour(behaviour: Integer);
    /// <summary>
    /// Get auto-completion case insensitive behaviour.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetCaseInsensitiveBehaviour(): Integer;
    /// <summary>
    /// Change the effect of autocompleting when there are multiple selections.
    /// </summary>
    /// <param name="multi">
    /// The multi parameter
    /// </param>
    procedure AutoCSetMulti(multi: Integer);
    /// <summary>
    /// Retrieve the effect of autocompleting when there are multiple selections.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetMulti(): Integer;
    /// <summary>
    /// Set the way autocompletion lists are ordered.
    /// </summary>
    /// <param name="order">
    /// The order parameter
    /// </param>
    procedure AutoCSetOrder(order: Integer);
    /// <summary>
    /// Get the way autocompletion lists are ordered.
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AutoCGetOrder(): Integer;
    /// <summary>
    /// Enlarge the document to a particular size of text bytes.
    /// </summary>
    /// <param name="bytes">
    /// Position in the document
    /// </param>
    procedure Allocate(bytes: Integer);
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
    function TargetAsUTF8(s: PAnsiChar): Integer;
    /// <summary>
    /// Set the length of the utf8 argument for calling EncodedFromUTF8.
    /// Set to -1 and the string will be measured to the first nul.
    /// </summary>
    /// <param name="bytes">
    /// Position in the document
    /// </param>
    procedure SetLengthForEncode(bytes: Integer);
    /// <summary>
    /// Translates a UTF8 string into the document encoding.
    /// Return the length of the result in bytes.
    /// On error return 0.
    /// </summary>
    /// <param name="utf8">
    /// Text string
    /// </param>
    /// <param name="encoded">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function EncodedFromUTF8(utf8: PAnsiChar; encoded: PAnsiChar): Integer;
    /// <summary>
    /// Find the position of a column on a line taking into account tabs and
    /// multi-byte characters. If beyond end of line, return line end position.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="column">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function FindColumn(line: Integer; column: Integer): Integer;
    /// <summary>
    /// Can the caret preferred x position only be changed by explicit movement commands?
    /// </summary>
    /// <returns>
    /// Returns the caretsticky
    /// </returns>
    function GetCaretSticky(): Integer;
    /// <summary>
    /// Stop the caret preferred x position changing when the user types.
    /// </summary>
    /// <param name="useCaretStickyBehaviour">
    /// The useCaretStickyBehaviour parameter
    /// </param>
    procedure SetCaretSticky(useCaretStickyBehaviour: Integer);
    /// <summary>
    /// Switch between sticky and non-sticky: meant to be bound to a key.
    /// </summary>
    procedure ToggleCaretSticky();
    /// <summary>
    /// Enable/Disable convert-on-paste for line endings
    /// </summary>
    /// <param name="convert">
    /// Boolean value
    /// </param>
    procedure SetPasteConvertEndings(convert: Boolean);
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
    /// <param name="length">
    /// Position in the document
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure ReplaceRectangular(length: Integer; text: PAnsiChar);
    /// <summary>
    /// Duplicate the selection. If selection empty duplicate the line containing the caret.
    /// </summary>
    procedure SelectionDuplicate();
    /// <summary>
    /// Set background alpha of the caret line.
    /// </summary>
    /// <param name="alpha">
    /// The alpha parameter
    /// </param>
    procedure SetCaretLineBackAlpha(alpha: Integer);
    /// <summary>
    /// Get the background alpha of the caret line.
    /// </summary>
    /// <returns>
    /// Returns the caretlinebackalpha
    /// </returns>
    function GetCaretLineBackAlpha(): Integer;
    /// <summary>
    /// Set the style of the caret to be drawn.
    /// </summary>
    /// <param name="caretStyle">
    /// The caretStyle parameter
    /// </param>
    procedure SetCaretStyle(caretStyle: Integer);
    /// <summary>
    /// Returns the current style of the caret.
    /// </summary>
    /// <returns>
    /// Returns the caretstyle
    /// </returns>
    function GetCaretStyle(): Integer;
    /// <summary>
    /// Set the indicator used for IndicatorFillRange and IndicatorClearRange
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    procedure SetIndicatorCurrent(indicator: Integer);
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
    /// <param name="value">
    /// Integer value
    /// </param>
    procedure SetIndicatorValue(value: Integer);
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
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="lengthFill">
    /// Position in the document
    /// </param>
    procedure IndicatorFillRange(start: Integer; lengthFill: Integer);
    /// <summary>
    /// Turn a indicator off over a range.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="lengthClear">
    /// Position in the document
    /// </param>
    procedure IndicatorClearRange(start: Integer; lengthClear: Integer);
    /// <summary>
    /// Are any indicators present at pos?
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function IndicatorAllOnFor(pos: Integer): Integer;
    /// <summary>
    /// What value does a particular indicator have at a position?
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function IndicatorValueAt(indicator: Integer; pos: Integer): Integer;
    /// <summary>
    /// Where does a particular indicator start?
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function IndicatorStart(indicator: Integer; pos: Integer): Integer;
    /// <summary>
    /// Where does a particular indicator end?
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function IndicatorEnd(indicator: Integer; pos: Integer): Integer;
    /// <summary>
    /// Set number of entries in position cache
    /// </summary>
    /// <param name="size">
    /// Integer value
    /// </param>
    procedure SetPositionCache(size: Integer);
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
    /// <param name="threads">
    /// Integer value
    /// </param>
    procedure SetLayoutThreads(threads: Integer);
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
    /// <param name="separator">
    /// Text string
    /// </param>
    procedure SetCopySeparator(separator: PAnsiChar);
    /// <summary>
    /// Get the string to separate parts when copying a multiple selection.
    /// </summary>
    /// <param name="separator">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetCopySeparator(separator: PAnsiChar): Integer;
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
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="lengthRange">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the rangepointer
    /// </returns>
    function GetRangePointer(start: Integer; lengthRange: Integer): Pointer;
    /// <summary>
    /// Return a position which, to avoid performance costs, should not be within
    /// the range of a call to GetRangePointer.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetGapPosition(): Integer;
    /// <summary>
    /// Set the alpha fill colour of the given indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="alpha">
    /// The alpha parameter
    /// </param>
    procedure IndicSetAlpha(indicator: Integer; alpha: Integer);
    /// <summary>
    /// Get the alpha fill colour of the given indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetAlpha(indicator: Integer): Integer;
    /// <summary>
    /// Set the alpha outline colour of the given indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <param name="alpha">
    /// The alpha parameter
    /// </param>
    procedure IndicSetOutlineAlpha(indicator: Integer; alpha: Integer);
    /// <summary>
    /// Get the alpha outline colour of the given indicator.
    /// </summary>
    /// <param name="indicator">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the value
    /// </returns>
    function IndicGetOutlineAlpha(indicator: Integer): Integer;
    /// <summary>
    /// Set extra ascent for each line
    /// </summary>
    /// <param name="extraAscent">
    /// Integer value
    /// </param>
    procedure SetExtraAscent(extraAscent: Integer);
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
    /// <param name="extraDescent">
    /// Integer value
    /// </param>
    procedure SetExtraDescent(extraDescent: Integer);
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
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function MarkerSymbolDefined(markerNumber: Integer): Integer;
    /// <summary>
    /// Set the text in the text margin for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure MarginSetText(line: Integer; text: PAnsiChar);
    /// <summary>
    /// Get the text in the text margin for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetText(line: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Set the style number for the text margin for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure MarginSetStyle(line: Integer; style: Integer);
    /// <summary>
    /// Get the style number for the text margin for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetStyle(line: Integer): Integer;
    /// <summary>
    /// Set the style in the text margin for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="styles">
    /// Text string
    /// </param>
    procedure MarginSetStyles(line: Integer; styles: PAnsiChar);
    /// <summary>
    /// Get the styles in the text margin for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="styles">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function MarginGetStyles(line: Integer; styles: PAnsiChar): Integer;
    /// <summary>
    /// Clear the margin text on all lines
    /// </summary>
    procedure MarginTextClearAll();
    /// <summary>
    /// Get the start of the range of style numbers used for margin text
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure MarginSetStyleOffset(style: Integer);
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
    /// <param name="marginOptions">
    /// The marginOptions parameter
    /// </param>
    procedure SetMarginOptions(marginOptions: Integer);
    /// <summary>
    /// Get the margin options.
    /// </summary>
    /// <returns>
    /// Returns the marginoptions
    /// </returns>
    function GetMarginOptions(): Integer;
    /// <summary>
    /// Set the annotation text for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure AnnotationSetText(line: Integer; text: PAnsiChar);
    /// <summary>
    /// Get the annotation text for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetText(line: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Set the style number for the annotations for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure AnnotationSetStyle(line: Integer; style: Integer);
    /// <summary>
    /// Get the style number for the annotations for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetStyle(line: Integer): Integer;
    /// <summary>
    /// Set the annotation styles for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="styles">
    /// Text string
    /// </param>
    procedure AnnotationSetStyles(line: Integer; styles: PAnsiChar);
    /// <summary>
    /// Get the annotation styles for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="styles">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetStyles(line: Integer; styles: PAnsiChar): Integer;
    /// <summary>
    /// Get the number of annotation lines for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AnnotationGetLines(line: Integer): Integer;
    /// <summary>
    /// Clear the annotations from all lines
    /// </summary>
    procedure AnnotationClearAll();
    /// <summary>
    /// Set the visibility for the annotations for a view
    /// </summary>
    /// <param name="visible">
    /// The visible parameter
    /// </param>
    procedure AnnotationSetVisible(visible: Integer);
    /// <summary>
    /// Get the visibility for the annotations for a view
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function AnnotationGetVisible(): Integer;
    /// <summary>
    /// Get the start of the range of style numbers used for annotations
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure AnnotationSetStyleOffset(style: Integer);
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
    /// <param name="numberStyles">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AllocateExtendedStyles(numberStyles: Integer): Integer;
    /// <summary>
    /// Add a container action to the undo stack
    /// </summary>
    /// <param name="token">
    /// Integer value
    /// </param>
    /// <param name="flags">
    /// The flags parameter
    /// </param>
    procedure AddUndoAction(token: Integer; flags: Integer);
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
    function CharPositionFromPoint(x: Integer; y: Integer): Integer;
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
    function CharPositionFromPointClose(x: Integer; y: Integer): Integer;
    /// <summary>
    /// Set whether switching to rectangular mode while selecting with the mouse is allowed.
    /// </summary>
    /// <param name="mouseSelectionRectangularSwitch">
    /// Boolean value
    /// </param>
    procedure SetMouseSelectionRectangularSwitch(mouseSelectionRectangularSwitch: Boolean);
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
    /// <param name="multipleSelection">
    /// Boolean value
    /// </param>
    procedure SetMultipleSelection(multipleSelection: Boolean);
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
    /// <param name="additionalSelectionTyping">
    /// Boolean value
    /// </param>
    procedure SetAdditionalSelectionTyping(additionalSelectionTyping: Boolean);
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
    /// <param name="additionalCaretsBlink">
    /// Boolean value
    /// </param>
    procedure SetAdditionalCaretsBlink(additionalCaretsBlink: Boolean);
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
    /// <param name="additionalCaretsVisible">
    /// Boolean value
    /// </param>
    procedure SetAdditionalCaretsVisible(additionalCaretsVisible: Boolean);
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
    /// <param name="caret">
    /// Position in the document
    /// </param>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure SetSelection(caret: Integer; anchor: Integer);
    /// <summary>
    /// Add a selection
    /// </summary>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure AddSelection(caret: Integer; anchor: Integer);
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
    /// <param name="selection">
    /// Integer value
    /// </param>
    procedure DropSelectionN(selection: Integer);
    /// <summary>
    /// Set the main selection
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    procedure SetMainSelection(selection: Integer);
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
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetSelectionNCaret(selection: Integer; caret: Integer);
    /// <summary>
    /// Return the caret position of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNCaret(selection: Integer): Integer;
    /// <summary>
    /// Set the anchor position of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure SetSelectionNAnchor(selection: Integer; anchor: Integer);
    /// <summary>
    /// Return the anchor position of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNAnchor(selection: Integer): Integer;
    /// <summary>
    /// Set the virtual space of the caret of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <param name="space">
    /// Position in the document
    /// </param>
    procedure SetSelectionNCaretVirtualSpace(selection: Integer; space: Integer);
    /// <summary>
    /// Return the virtual space of the caret of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNCaretVirtualSpace(selection: Integer): Integer;
    /// <summary>
    /// Set the virtual space of the anchor of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <param name="space">
    /// Position in the document
    /// </param>
    procedure SetSelectionNAnchorVirtualSpace(selection: Integer; space: Integer);
    /// <summary>
    /// Return the virtual space of the anchor of the nth selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNAnchorVirtualSpace(selection: Integer): Integer;
    /// <summary>
    /// Sets the position that starts the selection - this becomes the anchor.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure SetSelectionNStart(selection: Integer; anchor: Integer);
    /// <summary>
    /// Returns the position at the start of the selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNStart(selection: Integer): Integer;
    /// <summary>
    /// Returns the virtual space at the start of the selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNStartVirtualSpace(selection: Integer): Integer;
    /// <summary>
    /// Sets the position that ends the selection - this becomes the currentPosition.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetSelectionNEnd(selection: Integer; caret: Integer);
    /// <summary>
    /// Returns the virtual space at the end of the selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNEndVirtualSpace(selection: Integer): Integer;
    /// <summary>
    /// Returns the position at the end of the selection.
    /// </summary>
    /// <param name="selection">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetSelectionNEnd(selection: Integer): Integer;
    /// <summary>
    /// Set the caret position of the rectangular selection.
    /// </summary>
    /// <param name="caret">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionCaret(caret: Integer);
    /// <summary>
    /// Return the caret position of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionCaret(): Integer;
    /// <summary>
    /// Set the anchor position of the rectangular selection.
    /// </summary>
    /// <param name="anchor">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionAnchor(anchor: Integer);
    /// <summary>
    /// Return the anchor position of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionAnchor(): Integer;
    /// <summary>
    /// Set the virtual space of the caret of the rectangular selection.
    /// </summary>
    /// <param name="space">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionCaretVirtualSpace(space: Integer);
    /// <summary>
    /// Return the virtual space of the caret of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionCaretVirtualSpace(): Integer;
    /// <summary>
    /// Set the virtual space of the anchor of the rectangular selection.
    /// </summary>
    /// <param name="space">
    /// Position in the document
    /// </param>
    procedure SetRectangularSelectionAnchorVirtualSpace(space: Integer);
    /// <summary>
    /// Return the virtual space of the anchor of the rectangular selection.
    /// </summary>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function GetRectangularSelectionAnchorVirtualSpace(): Integer;
    /// <summary>
    /// Set options for virtual space behaviour.
    /// </summary>
    /// <param name="virtualSpaceOptions">
    /// The virtualSpaceOptions parameter
    /// </param>
    procedure SetVirtualSpaceOptions(virtualSpaceOptions: Integer);
    /// <summary>
    /// Return options for virtual space behaviour.
    /// </summary>
    /// <returns>
    /// Returns the virtualspaceoptions
    /// </returns>
    function GetVirtualSpaceOptions(): Integer;
    /// <summary>
    /// On GTK, allow selecting the modifier key to use for mouse-based
    /// rectangular selection. Often the window manager requires Alt+Mouse Drag
    /// for moving windows.
    /// Valid values are SCMOD_CTRL(default), SCMOD_ALT, or SCMOD_SUPER.
    /// </summary>
    /// <param name="modifier">
    /// Integer value
    /// </param>
    procedure SetRectangularSelectionModifier(modifier: Integer);
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
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetAdditionalSelFore(fore: TColor);
    /// <summary>
    /// Set the background colour of additional selections.
    /// Must have previously called SetSelBack with non-zero first argument for this to have an effect.
    /// </summary>
    /// <param name="back">
    /// Color value
    /// </param>
    procedure SetAdditionalSelBack(back: TColor);
    /// <summary>
    /// Set the alpha of the selection.
    /// </summary>
    /// <param name="alpha">
    /// The alpha parameter
    /// </param>
    procedure SetAdditionalSelAlpha(alpha: Integer);
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    /// <returns>
    /// Returns the additionalselalpha
    /// </returns>
    function GetAdditionalSelAlpha(): Integer;
    /// <summary>
    /// Set the foreground colour of additional carets.
    /// </summary>
    /// <param name="fore">
    /// Color value
    /// </param>
    procedure SetAdditionalCaretFore(fore: TColor);
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
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function ChangeLexerState(start: Integer; end: Integer): Integer;
    /// <summary>
    /// Find the next line at or after lineStart that is a contracted fold header line.
    /// Return -1 when no more lines.
    /// </summary>
    /// <param name="lineStart">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function ContractedFoldNext(lineStart: Integer): Integer;
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
    /// <param name="identifier">
    /// Integer value
    /// </param>
    procedure SetIdentifier(identifier: Integer);
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
    /// <param name="width">
    /// Integer value
    /// </param>
    procedure RGBAImageSetWidth(width: Integer);
    /// <summary>
    /// Set the height for future RGBA image data.
    /// </summary>
    /// <param name="height">
    /// Integer value
    /// </param>
    procedure RGBAImageSetHeight(height: Integer);
    /// <summary>
    /// Set the scale factor in percent for future RGBA image data.
    /// </summary>
    /// <param name="scalePercent">
    /// Integer value
    /// </param>
    procedure RGBAImageSetScale(scalePercent: Integer);
    /// <summary>
    /// Define a marker from RGBA data.
    /// It has the width and height from RGBAImageSetWidth/Height
    /// </summary>
    /// <param name="markerNumber">
    /// Integer value
    /// </param>
    /// <param name="pixels">
    /// Text string
    /// </param>
    procedure MarkerDefineRGBAImage(markerNumber: Integer; pixels: PAnsiChar);
    /// <summary>
    /// Register an RGBA image for use in autocompletion lists.
    /// It has the width and height from RGBAImageSetWidth/Height
    /// </summary>
    /// <param name="type">
    /// Integer value
    /// </param>
    /// <param name="pixels">
    /// Text string
    /// </param>
    procedure RegisterRGBAImage(type: Integer; pixels: PAnsiChar);
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
    /// <param name="technology">
    /// The technology parameter
    /// </param>
    procedure SetTechnology(technology: Integer);
    /// <summary>
    /// Get the tech.
    /// </summary>
    /// <returns>
    /// Returns the technology
    /// </returns>
    function GetTechnology(): Integer;
    /// <summary>
    /// Create an ILoader*.
    /// </summary>
    /// <param name="bytes">
    /// Position in the document
    /// </param>
    /// <param name="documentOptions">
    /// The documentOptions parameter
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function CreateLoader(bytes: Integer; documentOptions: Integer): Pointer;
    /// <summary>
    /// On macOS, show a find indicator.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    procedure FindIndicatorShow(start: Integer; end: Integer);
    /// <summary>
    /// On macOS, flash a find indicator, then fade out.
    /// </summary>
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    procedure FindIndicatorFlash(start: Integer; end: Integer);
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
    /// <param name="alwaysVisible">
    /// Boolean value
    /// </param>
    procedure SetCaretLineVisibleAlways(alwaysVisible: Boolean);
    /// <summary>
    /// Set the line end types that the application wants to use. May not be used if incompatible with lexer or encoding.
    /// </summary>
    /// <param name="lineEndBitSet">
    /// The lineEndBitSet parameter
    /// </param>
    procedure SetLineEndTypesAllowed(lineEndBitSet: Integer);
    /// <summary>
    /// Get the line end types currently allowed.
    /// </summary>
    /// <returns>
    /// Returns the lineendtypesallowed
    /// </returns>
    function GetLineEndTypesAllowed(): Integer;
    /// <summary>
    /// Get the line end types currently recognised. May be a subset of the allowed types due to lexer limitation.
    /// </summary>
    /// <returns>
    /// Returns the lineendtypesactive
    /// </returns>
    function GetLineEndTypesActive(): Integer;
    /// <summary>
    /// Set the way a character is drawn.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    /// <param name="representation">
    /// Text string
    /// </param>
    procedure SetRepresentation(encodedCharacter: PAnsiChar; representation: PAnsiChar);
    /// <summary>
    /// Get the way a character is drawn.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    /// <param name="representation">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetRepresentation(encodedCharacter: PAnsiChar; representation: PAnsiChar): Integer;
    /// <summary>
    /// Remove a character representation.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    procedure ClearRepresentation(encodedCharacter: PAnsiChar);
    /// <summary>
    /// Clear representations to default.
    /// </summary>
    procedure ClearAllRepresentations();
    /// <summary>
    /// Set the appearance of a representation.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    /// <param name="appearance">
    /// The appearance parameter
    /// </param>
    procedure SetRepresentationAppearance(encodedCharacter: PAnsiChar; appearance: Integer);
    /// <summary>
    /// Get the appearance of a representation.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the representationappearance
    /// </returns>
    function GetRepresentationAppearance(encodedCharacter: PAnsiChar): Integer;
    /// <summary>
    /// Set the colour of a representation.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    /// <param name="colour">
    /// The colour parameter
    /// </param>
    procedure SetRepresentationColour(encodedCharacter: PAnsiChar; colour: TColorAlpha);
    /// <summary>
    /// Get the colour of a representation.
    /// </summary>
    /// <param name="encodedCharacter">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the representationcolour
    /// </returns>
    function GetRepresentationColour(encodedCharacter: PAnsiChar): TColorAlpha;
    /// <summary>
    /// Set the end of line annotation text for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Text string
    /// </param>
    procedure EOLAnnotationSetText(line: Integer; text: PAnsiChar);
    /// <summary>
    /// Get the end of line annotation text for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="text">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function EOLAnnotationGetText(line: Integer; text: PAnsiChar): Integer;
    /// <summary>
    /// Set the style number for the end of line annotations for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure EOLAnnotationSetStyle(line: Integer; style: Integer);
    /// <summary>
    /// Get the style number for the end of line annotations for a line
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function EOLAnnotationGetStyle(line: Integer): Integer;
    /// <summary>
    /// Clear the end of annotations from all lines
    /// </summary>
    procedure EOLAnnotationClearAll();
    /// <summary>
    /// Set the visibility for the end of line annotations for a view
    /// </summary>
    /// <param name="visible">
    /// The visible parameter
    /// </param>
    procedure EOLAnnotationSetVisible(visible: Integer);
    /// <summary>
    /// Get the visibility for the end of line annotations for a view
    /// </summary>
    /// <returns>
    /// Returns the value
    /// </returns>
    function EOLAnnotationGetVisible(): Integer;
    /// <summary>
    /// Get the start of the range of style numbers used for end of line annotations
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    procedure EOLAnnotationSetStyleOffset(style: Integer);
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
    /// <param name="feature">
    /// The feature parameter
    /// </param>
    /// <returns>
    /// Returns true if successful, false otherwise
    /// </returns>
    function SupportsFeature(feature: Integer): Boolean;
    /// <summary>
    /// Retrieve line character index state.
    /// </summary>
    /// <returns>
    /// Returns the linecharacterindex
    /// </returns>
    function GetLineCharacterIndex(): Integer;
    /// <summary>
    /// Request line character index be created or its use count increased.
    /// </summary>
    /// <param name="lineCharacterIndex">
    /// The lineCharacterIndex parameter
    /// </param>
    procedure AllocateLineCharacterIndex(lineCharacterIndex: Integer);
    /// <summary>
    /// Decrease use count of line character index and remove if 0.
    /// </summary>
    /// <param name="lineCharacterIndex">
    /// The lineCharacterIndex parameter
    /// </param>
    procedure ReleaseLineCharacterIndex(lineCharacterIndex: Integer);
    /// <summary>
    /// Retrieve the document line containing a position measured in index units.
    /// </summary>
    /// <param name="pos">
    /// Position in the document
    /// </param>
    /// <param name="lineCharacterIndex">
    /// The lineCharacterIndex parameter
    /// </param>
    /// <returns>
    /// Returns the line number
    /// </returns>
    function LineFromIndexPosition(pos: Integer; lineCharacterIndex: Integer): Integer;
    /// <summary>
    /// Retrieve the position measured in index units at the start of a document line.
    /// </summary>
    /// <param name="line">
    /// Line number
    /// </param>
    /// <param name="lineCharacterIndex">
    /// The lineCharacterIndex parameter
    /// </param>
    /// <returns>
    /// Returns the position in the document
    /// </returns>
    function IndexPositionFromLine(line: Integer; lineCharacterIndex: Integer): Integer;
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
    /// <param name="start">
    /// Position in the document
    /// </param>
    /// <param name="end">
    /// Position in the document
    /// </param>
    procedure Colourise(start: Integer; end: Integer);
    /// <summary>
    /// Set up a value that may be used by a lexer for some optional feature.
    /// </summary>
    /// <param name="key">
    /// Text string
    /// </param>
    /// <param name="value">
    /// Text string
    /// </param>
    procedure SetProperty(key: PAnsiChar; value: PAnsiChar);
    /// <summary>
    /// Set up the key words used by the lexer.
    /// </summary>
    /// <param name="keyWordSet">
    /// Integer value
    /// </param>
    /// <param name="keyWords">
    /// Text string
    /// </param>
    procedure SetKeyWords(keyWordSet: Integer; keyWords: PAnsiChar);
    /// <summary>
    /// Retrieve a "property" value previously set with SetProperty.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="key">
    /// Text string
    /// </param>
    /// <param name="value">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetProperty(key: PAnsiChar; value: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a "property" value previously set with SetProperty,
    /// with "$()" variable replacement on returned buffer.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="key">
    /// Text string
    /// </param>
    /// <param name="value">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPropertyExpanded(key: PAnsiChar; value: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a "property" value previously set with SetProperty,
    /// interpreted as an int AFTER any "$()" variable replacement.
    /// </summary>
    /// <param name="key">
    /// Text string
    /// </param>
    /// <param name="defaultValue">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPropertyInt(key: PAnsiChar; defaultValue: Integer): Integer;
    /// <summary>
    /// Retrieve the name of the lexer.
    /// Return the length of the text.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="language">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetLexerLanguage(language: PAnsiChar): Integer;
    /// <summary>
    /// For private communication between an application and a known lexer.
    /// </summary>
    /// <param name="operation">
    /// Integer value
    /// </param>
    /// <param name="pointer">
    /// The pointer parameter
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function PrivateLexerCall(operation: Integer; pointer: Pointer): Pointer;
    /// <summary>
    /// Retrieve a '\n' separated list of properties understood by the current lexer.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="names">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function PropertyNames(names: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve the type of a property.
    /// </summary>
    /// <param name="name">
    /// Text string
    /// </param>
    /// <returns>
    /// Returns the result
    /// </returns>
    function PropertyType(name: PAnsiChar): Integer;
    /// <summary>
    /// Describe a property.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="name">
    /// Text string
    /// </param>
    /// <param name="description">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DescribeProperty(name: PAnsiChar; description: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a '\n' separated list of descriptions of the keyword sets understood by the current lexer.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="descriptions">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DescribeKeyWordSets(descriptions: PAnsiChar): Integer;
    /// <summary>
    /// Bit set of LineEndType enumertion for which line ends beyond the standard
    /// LF, CR, and CRLF are supported by the lexer.
    /// </summary>
    /// <returns>
    /// Returns the lineendtypessupported
    /// </returns>
    function GetLineEndTypesSupported(): Integer;
    /// <summary>
    /// Allocate a set of sub styles for a particular base style, returning start of range
    /// </summary>
    /// <param name="styleBase">
    /// Integer value
    /// </param>
    /// <param name="numberStyles">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function AllocateSubStyles(styleBase: Integer; numberStyles: Integer): Integer;
    /// <summary>
    /// The starting style number for the sub styles associated with a base style
    /// </summary>
    /// <param name="styleBase">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSubStylesStart(styleBase: Integer): Integer;
    /// <summary>
    /// The number of sub styles associated with a base style
    /// </summary>
    /// <param name="styleBase">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSubStylesLength(styleBase: Integer): Integer;
    /// <summary>
    /// For a sub style, return the base style, else return the argument.
    /// </summary>
    /// <param name="subStyle">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetStyleFromSubStyle(subStyle: Integer): Integer;
    /// <summary>
    /// For a secondary style, return the primary style, else return the argument.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetPrimaryStyleFromStyle(style: Integer): Integer;
    /// <summary>
    /// Free allocated sub styles
    /// </summary>
    procedure FreeSubStyles();
    /// <summary>
    /// Set the identifiers that are shown in a particular style
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="identifiers">
    /// Text string
    /// </param>
    procedure SetIdentifiers(style: Integer; identifiers: PAnsiChar);
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
    /// <param name="styles">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function GetSubStyleBases(styles: PAnsiChar): Integer;
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
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="name">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function NameOfStyle(style: Integer; name: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a ' ' separated list of style tags like "literal quoted string".
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="tags">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function TagsOfStyle(style: Integer; tags: PAnsiChar): Integer;
    /// <summary>
    /// Retrieve a description of a style.
    /// Result is NUL-terminated.
    /// </summary>
    /// <param name="style">
    /// Integer value
    /// </param>
    /// <param name="description">
    /// Buffer to receive the result
    /// </param>
    /// <returns>
    /// Returns the requested value
    /// </returns>
    function DescriptionOfStyle(style: Integer; description: PAnsiChar): Integer;
    /// <summary>
    /// Set the lexer from an ILexer*.
    /// </summary>
    /// <param name="ilexer">
    /// The ilexer parameter
    /// </param>
    procedure SetILexer(ilexer: Pointer);

    // Provisional
    /// <summary>
    /// Retrieve bidirectional text display state.
    /// </summary>
    /// <returns>
    /// Returns the bidirectional
    /// </returns>
    function GetBidirectional(): Integer;
    /// <summary>
    /// Set bidirectional text display state.
    /// </summary>
    /// <param name="bidirectional">
    /// The bidirectional parameter
    /// </param>
    procedure SetBidirectional(bidirectional: Integer);

  published
    // Auto-generated properties
    /// <summary>
    /// Retrieve the current tab draw mode.
    /// Returns one of SCTD_* constants.
    /// </summary>
    property TabDrawMode: Integer read GetTabDrawMode write SetTabDrawMode;
    /// <summary>
    /// Is printing line wrapped?
    /// </summary>
    property PrintWrapMode: Integer read GetPrintWrapMode write SetPrintWrapMode;
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
    property Length: Integer read GetLength;
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
    property SelAlpha: Integer read GetSelAlpha write SetSelAlpha;
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
    property IMEInteraction: Integer read GetIMEInteraction write SetIMEInteraction;
    /// <summary>
    /// Retrieve the time the mouse must sit still to generate a mouse dwell event.
    /// </summary>
    property MouseDwellTime: Integer read GetMouseDwellTime write SetMouseDwellTime;
    /// <summary>
    /// Get the layer of the background of the line containing the caret.
    /// </summary>
    property CaretLineLayer: Integer read GetCaretLineLayer write SetCaretLineLayer;
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
    property Cursor: Integer read GetCursor write SetCursor;
    /// <summary>
    /// Get automatic folding behaviours.
    /// </summary>
    property AutomaticFold: Integer read GetAutomaticFold write SetAutomaticFold;
    /// <summary>
    /// Retrieve the quality level for text.
    /// </summary>
    property FontQuality: Integer read GetFontQuality write SetFontQuality;
    /// <summary>
    /// Return the virtual space of the anchor of the rectangular selection.
    /// </summary>
    property RectangularSelectionAnchorVirtualSpace: Integer read GetRectangularSelectionAnchorVirtualSpace write SetRectangularSelectionAnchorVirtualSpace;
    /// <summary>
    /// Get the tech.
    /// </summary>
    property Technology: Integer read GetTechnology write SetTechnology;
    /// <summary>
    /// Report change history status.
    /// </summary>
    property ChangeHistory: Integer read GetChangeHistory write SetChangeHistory;
    /// <summary>
    /// Retrieve the current end of line mode - one of CRLF, CR, or LF.
    /// </summary>
    property EOLMode: Integer read GetEOLMode write SetEOLMode;
    /// <summary>
    /// Get the line end types currently recognised. May be a subset of the allowed types due to lexer limitation.
    /// </summary>
    property LineEndTypesActive: Integer read GetLineEndTypesActive;
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
    property TargetStartVirtualSpace: Integer read GetTargetStartVirtualSpace write SetTargetStartVirtualSpace;
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
    property LayoutCache: Integer read GetLayoutCache write SetLayoutCache;
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
    property ViewWS: Integer read GetViewWS write SetViewWS;
    /// <summary>
    /// Retrieve the number of named styles for the lexer.
    /// </summary>
    property NamedStyles: Integer read GetNamedStyles;
    /// <summary>
    /// Get the fore colour for active hotspots.
    /// </summary>
    property HotspotActiveFore: TColor read GetHotspotActiveFore write SetHotspotActiveFore;
    /// <summary>
    /// Retrieve the edge highlight mode.
    /// </summary>
    property EdgeMode: Integer read GetEdgeMode write SetEdgeMode;
    /// <summary>
    /// Which selection is the main selection
    /// </summary>
    property MainSelection: Integer read GetMainSelection write SetMainSelection;
    /// <summary>
    /// Return options for virtual space behaviour.
    /// </summary>
    property VirtualSpaceOptions: Integer read GetVirtualSpaceOptions write SetVirtualSpaceOptions;
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
    property SearchFlags: Integer read GetSearchFlags write SetSearchFlags;
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
    property SelectionEnd: Integer read GetSelectionEnd write SetSelectionEnd;
    /// <summary>
    /// Whether typing can be performed into multiple selections
    /// </summary>
    property AdditionalSelectionTyping: Boolean read GetAdditionalSelectionTyping write SetAdditionalSelectionTyping;
    /// <summary>
    /// Get the layer for drawing selections
    /// </summary>
    property SelectionLayer: Integer read GetSelectionLayer write SetSelectionLayer;
    /// <summary>
    /// Return the caret position of the rectangular selection.
    /// </summary>
    property RectangularSelectionCaret: Integer read GetRectangularSelectionCaret write SetRectangularSelectionCaret;
    /// <summary>
    /// Get only highlighting subline instead of whole line.
    /// </summary>
    property CaretLineHighlightSubLine: Boolean read GetCaretLineHighlightSubLine write SetCaretLineHighlightSubLine;
    /// <summary>
    /// Get error status.
    /// </summary>
    property Status: Integer read GetStatus write SetStatus;
    /// <summary>
    /// Returns the position of the opposite end of the selection to the caret.
    /// </summary>
    property Anchor: Integer read GetAnchor write SetAnchor;
    /// <summary>
    /// How many phases is drawing done in?
    /// </summary>
    property PhasesDraw: Integer read GetPhasesDraw write SetPhasesDraw;
    /// <summary>
    /// Get whether or not regular caret moves will extend or reduce the selection.
    /// </summary>
    property MoveExtendsSelection: Boolean read GetMoveExtendsSelection write SetMoveExtendsSelection;
    /// <summary>
    /// Retrieve the limits to idle styling.
    /// </summary>
    property IdleStyling: Integer read GetIdleStyling write SetIdleStyling;
    /// <summary>
    /// Retrieve how wrapped sublines are placed. Default is fixed.
    /// </summary>
    property WrapIndentMode: Integer read GetWrapIndentMode write SetWrapIndentMode;
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
    property SelectionStart: Integer read GetSelectionStart write SetSelectionStart;
    /// <summary>
    /// Report undo selection history status.
    /// </summary>
    property UndoSelectionHistory: Integer read GetUndoSelectionHistory write SetUndoSelectionHistory;
    /// <summary>
    /// Retrieve the display line at the top of the display.
    /// </summary>
    property FirstVisibleLine: Integer read GetFirstVisibleLine write SetFirstVisibleLine;
    /// <summary>
    /// Retrieve whether text is word wrapped.
    /// </summary>
    property WrapMode: Integer read GetWrapMode write SetWrapMode;
    /// <summary>
    /// Return a position which, to avoid performance costs, should not be within
    /// the range of a call to GetRangePointer.
    /// </summary>
    property GapPosition: Integer read GetGapPosition;
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
    property CaretLineBackAlpha: Integer read GetCaretLineBackAlpha write SetCaretLineBackAlpha;
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
    property CaretStyle: Integer read GetCaretStyle write SetCaretStyle;
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
    property SelectionMode: Integer read GetSelectionMode write SetSelectionMode;
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
    property EndStyled: Integer read GetEndStyled;
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
    property LineEndTypesSupported: Integer read GetLineEndTypesSupported;
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
    property LineCount: Integer read GetLineCount;
    /// <summary>
    /// Retrieve the effect of pasting when there are multiple selections.
    /// </summary>
    property MultiPaste: Integer read GetMultiPaste write SetMultiPaste;
    /// <summary>
    /// Report accessibility status.
    /// </summary>
    property Accessibility: Integer read GetAccessibility write SetAccessibility;
    /// <summary>
    /// Is undo history being collected?
    /// </summary>
    property UndoCollection: Boolean read GetUndoCollection write SetUndoCollection;
    /// <summary>
    /// Get the line end types currently allowed.
    /// </summary>
    property LineEndTypesAllowed: Integer read GetLineEndTypesAllowed write SetLineEndTypesAllowed;
    /// <summary>
    /// Get the foreground colour of the caret.
    /// </summary>
    property CaretFore: TColor read GetCaretFore write SetCaretFore;
    /// <summary>
    /// Get the margin options.
    /// </summary>
    property MarginOptions: Integer read GetMarginOptions write SetMarginOptions;
    /// <summary>
    /// Whether switching to rectangular mode while selecting with the mouse is allowed.
    /// </summary>
    property MouseSelectionRectangularSwitch: Boolean read GetMouseSelectionRectangularSwitch write SetMouseSelectionRectangularSwitch;
    /// <summary>
    /// Get which document modification events are sent to the container.
    /// </summary>
    property ModEventMask: Integer read GetModEventMask write SetModEventMask;
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
    property HotspotActiveBack: TColor read GetHotspotActiveBack write SetHotspotActiveBack;
    /// <summary>
    /// Retrieve the number of characters in the document.
    /// </summary>
    property TextLength: Integer read GetTextLength;
    /// <summary>
    /// Get the position that ends the target.
    /// </summary>
    property TargetEnd: Integer read GetTargetEnd write SetTargetEnd;
    /// <summary>
    /// Get the virtual space of the target end
    /// </summary>
    property TargetEndVirtualSpace: Integer read GetTargetEndVirtualSpace write SetTargetEndVirtualSpace;
    /// <summary>
    /// Does a backspace pressed when caret is within indentation unindent?
    /// </summary>
    property BackSpaceUnIndents: Boolean read GetBackSpaceUnIndents write SetBackSpaceUnIndents;
    /// <summary>
    /// Returns the position of the caret.
    /// </summary>
    property CurrentPos: Integer read GetCurrentPos write SetCurrentPos;
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
    property RectangularSelectionAnchor: Integer read GetRectangularSelectionAnchor write SetRectangularSelectionAnchor;
    /// <summary>
    /// Returns the print colour mode.
    /// </summary>
    property PrintColourMode: Integer read GetPrintColourMode write SetPrintColourMode;
    /// <summary>
    /// Get the position that starts the target.
    /// </summary>
    property TargetStart: Integer read GetTargetStart write SetTargetStart;
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
    property RectangularSelectionCaretVirtualSpace: Integer read GetRectangularSelectionCaretVirtualSpace write SetRectangularSelectionCaretVirtualSpace;
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
    property WrapVisualFlagsLocation: Integer read GetWrapVisualFlagsLocation write SetWrapVisualFlagsLocation;
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
    property Bidirectional: Integer read GetBidirectional write SetBidirectional;
    /// <summary>
    /// Is the caret line always visible?
    /// </summary>
    property CaretLineVisibleAlways: Boolean read GetCaretLineVisibleAlways write SetCaretLineVisibleAlways;
    /// <summary>
    /// Get the highlighted indentation guide column.
    /// </summary>
    property HighlightGuide: Integer read GetHighlightGuide write SetHighlightGuide;
    /// <summary>
    /// Can the caret preferred x position only be changed by explicit movement commands?
    /// </summary>
    property CaretSticky: Integer read GetCaretSticky write SetCaretSticky;
    /// <summary>
    /// Get whether underlining for active hotspots.
    /// </summary>
    property HotspotActiveUnderline: Boolean read GetHotspotActiveUnderline write SetHotspotActiveUnderline;
    /// <summary>
    /// Retrive the display mode of visual flags for wrapped lines.
    /// </summary>
    property WrapVisualFlags: Integer read GetWrapVisualFlags write SetWrapVisualFlags;
    /// <summary>
    /// Are the end of line characters visible?
    /// </summary>
    property ViewEOL: Boolean read GetViewEOL write SetViewEOL;
    /// <summary>
    /// Retrieve the column number which text should be kept within.
    /// </summary>
    property EdgeColumn: Integer read GetEdgeColumn write SetEdgeColumn;
    /// <summary>
    /// Is the horizontal scroll bar visible?
    /// </summary>
    property HScrollBar: Boolean read GetHScrollBar write SetHScrollBar;
    /// <summary>
    /// Get which document options are set.
    /// </summary>
    property DocumentOptions: Integer read GetDocumentOptions;
    /// <summary>
    /// Get the alpha of the selection.
    /// </summary>
    property AdditionalSelAlpha: Integer read GetAdditionalSelAlpha write SetAdditionalSelAlpha;
    /// <summary>
    /// Are the indentation guides visible?
    /// </summary>
    property IndentationGuides: Integer read GetIndentationGuides write SetIndentationGuides;
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
    property LineCharacterIndex: Integer read GetLineCharacterIndex;
    /// <summary>
    /// Does a tab pressed when caret is within indentation indent?
    /// </summary>
    property TabIndents: Boolean read GetTabIndents write SetTabIndents;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

const
  ScintillaClassName = 'Scintilla';
  ScintillaDLL = 'SciLexer.dll';

procedure Register;
begin
  RegisterComponents('Scintilla', [TCustomSciTextEditor]);
end;

{ TCustomSciTextEditor }

constructor TCustomSciTextEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
  TabStop := True;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
end;

destructor TCustomSciTextEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomSciTextEditor.CreateWnd;
var
  LoadResult: THandle;
begin
  LoadResult := LoadLibrary(ScintillaDLL);
  if LoadResult = 0 then
    raise Exception.Create('Failed to load ' + ScintillaDLL);

  CreateWindowEx(0, ScintillaClassName, nil,
    WS_CHILD or WS_VISIBLE or WS_TABSTOP or WS_CLIPCHILDREN,
    0, 0, Width, Height, Handle, 0, HInstance, nil);

  // Get direct access for better performance
  FDirectPtr := Pointer(SendMessage(Handle, SCI_GETDIRECTPOINTER, 0, 0));
  FDirectFunction := Pointer(SendMessage(Handle, SCI_GETDIRECTFUNCTION, 0, 0));
end;

procedure TCustomSciTextEditor.DestroyWnd;
begin
  FDirectPtr := nil;
  FDirectFunction := nil;
  inherited DestroyWnd;
end;

procedure TCustomSciTextEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TCustomSciTextEditor.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1; // Prevent flicker
end;

procedure TCustomSciTextEditor.AddText(length: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_ADDTEXT, length, LPARAM(text));
end;

procedure TCustomSciTextEditor.AddStyledText(length: Integer; c: PAnsiChar);
begin
  SendMessage(Handle, SCI_ADDSTYLEDTEXT, length, c);
end;

procedure TCustomSciTextEditor.InsertText(pos: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_INSERTTEXT, pos, LPARAM(text));
end;

procedure TCustomSciTextEditor.ChangeInsertion(length: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_CHANGEINSERTION, length, LPARAM(text));
end;

procedure TCustomSciTextEditor.ClearAll();
begin
  SendMessage(Handle, SCI_CLEARALL, 0, 0);
end;

procedure TCustomSciTextEditor.DeleteRange(start: Integer; lengthDelete: Integer);
begin
  SendMessage(Handle, SCI_DELETERANGE, start, lengthDelete);
end;

procedure TCustomSciTextEditor.ClearDocumentStyle();
begin
  SendMessage(Handle, SCI_CLEARDOCUMENTSTYLE, 0, 0);
end;

function TCustomSciTextEditor.GetLength(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLENGTH, 0, 0);
end;

function TCustomSciTextEditor.GetCharAt(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCHARAT, pos, 0);
end;

function TCustomSciTextEditor.GetCurrentPos(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCURRENTPOS, 0, 0);
end;

function TCustomSciTextEditor.GetAnchor(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETANCHOR, 0, 0);
end;

function TCustomSciTextEditor.GetStyleAt(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSTYLEAT, pos, 0);
end;

function TCustomSciTextEditor.GetStyleIndexAt(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSTYLEINDEXAT, pos, 0);
end;

procedure TCustomSciTextEditor.Redo();
begin
  SendMessage(Handle, SCI_REDO, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoCollection(collectUndo: Boolean);
begin
  SendMessage(Handle, SCI_SETUNDOCOLLECTION, collectUndo, 0);
end;

procedure TCustomSciTextEditor.SelectAll();
begin
  SendMessage(Handle, SCI_SELECTALL, 0, 0);
end;

procedure TCustomSciTextEditor.SetSavePoint();
begin
  SendMessage(Handle, SCI_SETSAVEPOINT, 0, 0);
end;

function TCustomSciTextEditor.GetStyledText(tr: PSciTextRange): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSTYLEDTEXT, 0, tr);
end;

function TCustomSciTextEditor.GetStyledTextFull(tr: PSciTextRangeFull): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSTYLEDTEXTFULL, 0, tr);
end;

function TCustomSciTextEditor.CanRedo(): Boolean;
begin
  Result := SendMessage(Handle, SCI_CANREDO, 0, 0);
end;

function TCustomSciTextEditor.MarkerLineFromHandle(markerHandle: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERLINEFROMHANDLE, markerHandle, 0);
end;

procedure TCustomSciTextEditor.MarkerDeleteHandle(markerHandle: Integer);
begin
  SendMessage(Handle, SCI_MARKERDELETEHANDLE, markerHandle, 0);
end;

function TCustomSciTextEditor.MarkerHandleFromLine(line: Integer; which: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERHANDLEFROMLINE, line, which);
end;

function TCustomSciTextEditor.MarkerNumberFromLine(line: Integer; which: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERNUMBERFROMLINE, line, which);
end;

function TCustomSciTextEditor.GetUndoCollection(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETUNDOCOLLECTION, 0, 0);
end;

function TCustomSciTextEditor.GetViewWS(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETVIEWWS, 0, 0);
end;

procedure TCustomSciTextEditor.SetViewWS(viewWS: Integer);
begin
  SendMessage(Handle, SCI_SETVIEWWS, viewWS, 0);
end;

function TCustomSciTextEditor.GetTabDrawMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTABDRAWMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetTabDrawMode(tabDrawMode: Integer);
begin
  SendMessage(Handle, SCI_SETTABDRAWMODE, tabDrawMode, 0);
end;

function TCustomSciTextEditor.PositionFromPoint(x: Integer; y: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONFROMPOINT, x, y);
end;

function TCustomSciTextEditor.PositionFromPointClose(x: Integer; y: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONFROMPOINTCLOSE, x, y);
end;

procedure TCustomSciTextEditor.GotoLine(line: Integer);
begin
  SendMessage(Handle, SCI_GOTOLINE, line, 0);
end;

procedure TCustomSciTextEditor.GotoPos(caret: Integer);
begin
  SendMessage(Handle, SCI_GOTOPOS, caret, 0);
end;

procedure TCustomSciTextEditor.SetAnchor(anchor: Integer);
begin
  SendMessage(Handle, SCI_SETANCHOR, anchor, 0);
end;

function TCustomSciTextEditor.GetCurLine(length: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCURLINE, length, LPARAM(text));
end;

function TCustomSciTextEditor.GetEndStyled(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETENDSTYLED, 0, 0);
end;

procedure TCustomSciTextEditor.ConvertEOLs(eolMode: Integer);
begin
  SendMessage(Handle, SCI_CONVERTEOLS, eolMode, 0);
end;

function TCustomSciTextEditor.GetEOLMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETEOLMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetEOLMode(eolMode: Integer);
begin
  SendMessage(Handle, SCI_SETEOLMODE, eolMode, 0);
end;

procedure TCustomSciTextEditor.StartStyling(start: Integer; unused: Integer);
begin
  SendMessage(Handle, SCI_STARTSTYLING, start, unused);
end;

procedure TCustomSciTextEditor.SetStyling(length: Integer; style: Integer);
begin
  SendMessage(Handle, SCI_SETSTYLING, length, style);
end;

function TCustomSciTextEditor.GetBufferedDraw(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETBUFFEREDDRAW, 0, 0);
end;

procedure TCustomSciTextEditor.SetBufferedDraw(buffered: Boolean);
begin
  SendMessage(Handle, SCI_SETBUFFEREDDRAW, buffered, 0);
end;

procedure TCustomSciTextEditor.SetTabWidth(tabWidth: Integer);
begin
  SendMessage(Handle, SCI_SETTABWIDTH, tabWidth, 0);
end;

function TCustomSciTextEditor.GetTabWidth(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTABWIDTH, 0, 0);
end;

procedure TCustomSciTextEditor.SetTabMinimumWidth(pixels: Integer);
begin
  SendMessage(Handle, SCI_SETTABMINIMUMWIDTH, pixels, 0);
end;

function TCustomSciTextEditor.GetTabMinimumWidth(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTABMINIMUMWIDTH, 0, 0);
end;

procedure TCustomSciTextEditor.ClearTabStops(line: Integer);
begin
  SendMessage(Handle, SCI_CLEARTABSTOPS, line, 0);
end;

procedure TCustomSciTextEditor.AddTabStop(line: Integer; x: Integer);
begin
  SendMessage(Handle, SCI_ADDTABSTOP, line, x);
end;

function TCustomSciTextEditor.GetNextTabStop(line: Integer; x: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETNEXTTABSTOP, line, x);
end;

procedure TCustomSciTextEditor.SetCodePage(codePage: Integer);
begin
  SendMessage(Handle, SCI_SETCODEPAGE, codePage, 0);
end;

procedure TCustomSciTextEditor.SetFontLocale(localeName: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETFONTLOCALE, 0, LPARAM(localeName));
end;

function TCustomSciTextEditor.GetFontLocale(localeName: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETFONTLOCALE, 0, LPARAM(localeName));
end;

function TCustomSciTextEditor.GetIMEInteraction(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETIMEINTERACTION, 0, 0);
end;

procedure TCustomSciTextEditor.SetIMEInteraction(imeInteraction: Integer);
begin
  SendMessage(Handle, SCI_SETIMEINTERACTION, imeInteraction, 0);
end;

procedure TCustomSciTextEditor.MarkerDefine(markerNumber: Integer; markerSymbol: Integer);
begin
  SendMessage(Handle, SCI_MARKERDEFINE, markerNumber, markerSymbol);
end;

procedure TCustomSciTextEditor.MarkerSetFore(markerNumber: Integer; fore: TColor);
begin
  SendMessage(Handle, SCI_MARKERSETFORE, markerNumber, fore);
end;

procedure TCustomSciTextEditor.MarkerSetBack(markerNumber: Integer; back: TColor);
begin
  SendMessage(Handle, SCI_MARKERSETBACK, markerNumber, back);
end;

procedure TCustomSciTextEditor.MarkerSetBackSelected(markerNumber: Integer; back: TColor);
begin
  SendMessage(Handle, SCI_MARKERSETBACKSELECTED, markerNumber, back);
end;

procedure TCustomSciTextEditor.MarkerSetForeTranslucent(markerNumber: Integer; fore: TColorAlpha);
begin
  SendMessage(Handle, SCI_MARKERSETFORETRANSLUCENT, markerNumber, fore);
end;

procedure TCustomSciTextEditor.MarkerSetBackTranslucent(markerNumber: Integer; back: TColorAlpha);
begin
  SendMessage(Handle, SCI_MARKERSETBACKTRANSLUCENT, markerNumber, back);
end;

procedure TCustomSciTextEditor.MarkerSetBackSelectedTranslucent(markerNumber: Integer; back: TColorAlpha);
begin
  SendMessage(Handle, SCI_MARKERSETBACKSELECTEDTRANSLUCENT, markerNumber, back);
end;

procedure TCustomSciTextEditor.MarkerSetStrokeWidth(markerNumber: Integer; hundredths: Integer);
begin
  SendMessage(Handle, SCI_MARKERSETSTROKEWIDTH, markerNumber, hundredths);
end;

procedure TCustomSciTextEditor.MarkerEnableHighlight(enabled: Boolean);
begin
  SendMessage(Handle, SCI_MARKERENABLEHIGHLIGHT, enabled, 0);
end;

function TCustomSciTextEditor.MarkerAdd(line: Integer; markerNumber: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERADD, line, markerNumber);
end;

procedure TCustomSciTextEditor.MarkerDelete(line: Integer; markerNumber: Integer);
begin
  SendMessage(Handle, SCI_MARKERDELETE, line, markerNumber);
end;

procedure TCustomSciTextEditor.MarkerDeleteAll(markerNumber: Integer);
begin
  SendMessage(Handle, SCI_MARKERDELETEALL, markerNumber, 0);
end;

function TCustomSciTextEditor.MarkerGet(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERGET, line, 0);
end;

function TCustomSciTextEditor.MarkerNext(lineStart: Integer; markerMask: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERNEXT, lineStart, markerMask);
end;

function TCustomSciTextEditor.MarkerPrevious(lineStart: Integer; markerMask: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERPREVIOUS, lineStart, markerMask);
end;

procedure TCustomSciTextEditor.MarkerDefinePixmap(markerNumber: Integer; pixmap: PAnsiChar);
begin
  SendMessage(Handle, SCI_MARKERDEFINEPIXMAP, markerNumber, LPARAM(pixmap));
end;

procedure TCustomSciTextEditor.MarkerAddSet(line: Integer; markerSet: Integer);
begin
  SendMessage(Handle, SCI_MARKERADDSET, line, markerSet);
end;

procedure TCustomSciTextEditor.MarkerSetAlpha(markerNumber: Integer; alpha: Integer);
begin
  SendMessage(Handle, SCI_MARKERSETALPHA, markerNumber, alpha);
end;

function TCustomSciTextEditor.MarkerGetLayer(markerNumber: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERGETLAYER, markerNumber, 0);
end;

procedure TCustomSciTextEditor.MarkerSetLayer(markerNumber: Integer; layer: Integer);
begin
  SendMessage(Handle, SCI_MARKERSETLAYER, markerNumber, layer);
end;

procedure TCustomSciTextEditor.SetMarginTypeN(margin: Integer; marginType: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINTYPEN, margin, marginType);
end;

function TCustomSciTextEditor.GetMarginTypeN(margin: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINTYPEN, margin, 0);
end;

procedure TCustomSciTextEditor.SetMarginWidthN(margin: Integer; pixelWidth: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINWIDTHN, margin, pixelWidth);
end;

function TCustomSciTextEditor.GetMarginWidthN(margin: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINWIDTHN, margin, 0);
end;

procedure TCustomSciTextEditor.SetMarginMaskN(margin: Integer; mask: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINMASKN, margin, mask);
end;

function TCustomSciTextEditor.GetMarginMaskN(margin: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINMASKN, margin, 0);
end;

procedure TCustomSciTextEditor.SetMarginSensitiveN(margin: Integer; sensitive: Boolean);
begin
  SendMessage(Handle, SCI_SETMARGINSENSITIVEN, margin, sensitive);
end;

function TCustomSciTextEditor.GetMarginSensitiveN(margin: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMARGINSENSITIVEN, margin, 0);
end;

procedure TCustomSciTextEditor.SetMarginCursorN(margin: Integer; cursor: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINCURSORN, margin, cursor);
end;

function TCustomSciTextEditor.GetMarginCursorN(margin: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINCURSORN, margin, 0);
end;

procedure TCustomSciTextEditor.SetMarginBackN(margin: Integer; back: TColor);
begin
  SendMessage(Handle, SCI_SETMARGINBACKN, margin, back);
end;

function TCustomSciTextEditor.GetMarginBackN(margin: Integer): TColor;
begin
  Result := SendMessage(Handle, SCI_GETMARGINBACKN, margin, 0);
end;

procedure TCustomSciTextEditor.SetMargins(margins: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINS, margins, 0);
end;

function TCustomSciTextEditor.GetMargins(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINS, 0, 0);
end;

procedure TCustomSciTextEditor.StyleClearAll();
begin
  SendMessage(Handle, SCI_STYLECLEARALL, 0, 0);
end;

procedure TCustomSciTextEditor.StyleSetFore(style: Integer; fore: TColor);
begin
  SendMessage(Handle, SCI_STYLESETFORE, style, fore);
end;

procedure TCustomSciTextEditor.StyleSetBack(style: Integer; back: TColor);
begin
  SendMessage(Handle, SCI_STYLESETBACK, style, back);
end;

procedure TCustomSciTextEditor.StyleSetBold(style: Integer; bold: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETBOLD, style, bold);
end;

procedure TCustomSciTextEditor.StyleSetItalic(style: Integer; italic: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETITALIC, style, italic);
end;

procedure TCustomSciTextEditor.StyleSetSize(style: Integer; sizePoints: Integer);
begin
  SendMessage(Handle, SCI_STYLESETSIZE, style, sizePoints);
end;

procedure TCustomSciTextEditor.StyleSetFont(style: Integer; fontName: PAnsiChar);
begin
  SendMessage(Handle, SCI_STYLESETFONT, style, LPARAM(fontName));
end;

procedure TCustomSciTextEditor.StyleSetEOLFilled(style: Integer; eolFilled: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETEOLFILLED, style, eolFilled);
end;

procedure TCustomSciTextEditor.StyleResetDefault();
begin
  SendMessage(Handle, SCI_STYLERESETDEFAULT, 0, 0);
end;

procedure TCustomSciTextEditor.StyleSetUnderline(style: Integer; underline: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETUNDERLINE, style, underline);
end;

function TCustomSciTextEditor.StyleGetFore(style: Integer): TColor;
begin
  Result := SendMessage(Handle, SCI_STYLEGETFORE, style, 0);
end;

function TCustomSciTextEditor.StyleGetBack(style: Integer): TColor;
begin
  Result := SendMessage(Handle, SCI_STYLEGETBACK, style, 0);
end;

function TCustomSciTextEditor.StyleGetBold(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETBOLD, style, 0);
end;

function TCustomSciTextEditor.StyleGetItalic(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETITALIC, style, 0);
end;

function TCustomSciTextEditor.StyleGetSize(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETSIZE, style, 0);
end;

function TCustomSciTextEditor.StyleGetFont(style: Integer; fontName: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETFONT, style, LPARAM(fontName));
end;

function TCustomSciTextEditor.StyleGetEOLFilled(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETEOLFILLED, style, 0);
end;

function TCustomSciTextEditor.StyleGetUnderline(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETUNDERLINE, style, 0);
end;

function TCustomSciTextEditor.StyleGetCase(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETCASE, style, 0);
end;

function TCustomSciTextEditor.StyleGetCharacterSet(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETCHARACTERSET, style, 0);
end;

function TCustomSciTextEditor.StyleGetVisible(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETVISIBLE, style, 0);
end;

function TCustomSciTextEditor.StyleGetChangeable(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETCHANGEABLE, style, 0);
end;

function TCustomSciTextEditor.StyleGetHotSpot(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETHOTSPOT, style, 0);
end;

procedure TCustomSciTextEditor.StyleSetCase(style: Integer; caseVisible: Integer);
begin
  SendMessage(Handle, SCI_STYLESETCASE, style, caseVisible);
end;

procedure TCustomSciTextEditor.StyleSetSizeFractional(style: Integer; sizeHundredthPoints: Integer);
begin
  SendMessage(Handle, SCI_STYLESETSIZEFRACTIONAL, style, sizeHundredthPoints);
end;

function TCustomSciTextEditor.StyleGetSizeFractional(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETSIZEFRACTIONAL, style, 0);
end;

procedure TCustomSciTextEditor.StyleSetWeight(style: Integer; weight: Integer);
begin
  SendMessage(Handle, SCI_STYLESETWEIGHT, style, weight);
end;

function TCustomSciTextEditor.StyleGetWeight(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETWEIGHT, style, 0);
end;

procedure TCustomSciTextEditor.StyleSetCharacterSet(style: Integer; characterSet: Integer);
begin
  SendMessage(Handle, SCI_STYLESETCHARACTERSET, style, characterSet);
end;

procedure TCustomSciTextEditor.StyleSetHotSpot(style: Integer; hotspot: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETHOTSPOT, style, hotspot);
end;

procedure TCustomSciTextEditor.StyleSetCheckMonospaced(style: Integer; checkMonospaced: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETCHECKMONOSPACED, style, checkMonospaced);
end;

function TCustomSciTextEditor.StyleGetCheckMonospaced(style: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_STYLEGETCHECKMONOSPACED, style, 0);
end;

procedure TCustomSciTextEditor.StyleSetStretch(style: Integer; stretch: Integer);
begin
  SendMessage(Handle, SCI_STYLESETSTRETCH, style, stretch);
end;

function TCustomSciTextEditor.StyleGetStretch(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETSTRETCH, style, 0);
end;

procedure TCustomSciTextEditor.StyleSetInvisibleRepresentation(style: Integer; representation: PAnsiChar);
begin
  SendMessage(Handle, SCI_STYLESETINVISIBLEREPRESENTATION, style, LPARAM(representation));
end;

function TCustomSciTextEditor.StyleGetInvisibleRepresentation(style: Integer; representation: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_STYLEGETINVISIBLEREPRESENTATION, style, LPARAM(representation));
end;

procedure TCustomSciTextEditor.SetElementColour(element: Integer; colourElement: TColorAlpha);
begin
  SendMessage(Handle, SCI_SETELEMENTCOLOUR, element, colourElement);
end;

function TCustomSciTextEditor.GetElementColour(element: Integer): TColorAlpha;
begin
  Result := SendMessage(Handle, SCI_GETELEMENTCOLOUR, element, 0);
end;

procedure TCustomSciTextEditor.ResetElementColour(element: Integer);
begin
  SendMessage(Handle, SCI_RESETELEMENTCOLOUR, element, 0);
end;

function TCustomSciTextEditor.GetElementIsSet(element: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETELEMENTISSET, element, 0);
end;

function TCustomSciTextEditor.GetElementAllowsTranslucent(element: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETELEMENTALLOWSTRANSLUCENT, element, 0);
end;

function TCustomSciTextEditor.GetElementBaseColour(element: Integer): TColorAlpha;
begin
  Result := SendMessage(Handle, SCI_GETELEMENTBASECOLOUR, element, 0);
end;

procedure TCustomSciTextEditor.SetSelFore(useSetting: Boolean; fore: TColor);
begin
  SendMessage(Handle, SCI_SETSELFORE, useSetting, fore);
end;

procedure TCustomSciTextEditor.SetSelBack(useSetting: Boolean; back: TColor);
begin
  SendMessage(Handle, SCI_SETSELBACK, useSetting, back);
end;

function TCustomSciTextEditor.GetSelAlpha(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELALPHA, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelAlpha(alpha: Integer);
begin
  SendMessage(Handle, SCI_SETSELALPHA, alpha, 0);
end;

function TCustomSciTextEditor.GetSelEOLFilled(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETSELEOLFILLED, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelEOLFilled(filled: Boolean);
begin
  SendMessage(Handle, SCI_SETSELEOLFILLED, filled, 0);
end;

function TCustomSciTextEditor.GetSelectionLayer(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONLAYER, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionLayer(layer: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONLAYER, layer, 0);
end;

function TCustomSciTextEditor.GetCaretLineLayer(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINELAYER, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineLayer(layer: Integer);
begin
  SendMessage(Handle, SCI_SETCARETLINELAYER, layer, 0);
end;

function TCustomSciTextEditor.GetCaretLineHighlightSubLine(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINEHIGHLIGHTSUBLINE, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineHighlightSubLine(subLine: Boolean);
begin
  SendMessage(Handle, SCI_SETCARETLINEHIGHLIGHTSUBLINE, subLine, 0);
end;

procedure TCustomSciTextEditor.SetCaretFore(fore: TColor);
begin
  SendMessage(Handle, SCI_SETCARETFORE, fore, 0);
end;

procedure TCustomSciTextEditor.AssignCmdKey(keyDefinition: Integer; sciCommand: Integer);
begin
  SendMessage(Handle, SCI_ASSIGNCMDKEY, keyDefinition, sciCommand);
end;

procedure TCustomSciTextEditor.ClearCmdKey(keyDefinition: Integer);
begin
  SendMessage(Handle, SCI_CLEARCMDKEY, keyDefinition, 0);
end;

procedure TCustomSciTextEditor.ClearAllCmdKeys();
begin
  SendMessage(Handle, SCI_CLEARALLCMDKEYS, 0, 0);
end;

procedure TCustomSciTextEditor.SetStylingEx(length: Integer; styles: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETSTYLINGEX, length, LPARAM(styles));
end;

procedure TCustomSciTextEditor.StyleSetVisible(style: Integer; visible: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETVISIBLE, style, visible);
end;

function TCustomSciTextEditor.GetCaretPeriod(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETPERIOD, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretPeriod(periodMilliseconds: Integer);
begin
  SendMessage(Handle, SCI_SETCARETPERIOD, periodMilliseconds, 0);
end;

procedure TCustomSciTextEditor.SetWordChars(characters: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETWORDCHARS, 0, LPARAM(characters));
end;

function TCustomSciTextEditor.GetWordChars(characters: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWORDCHARS, 0, LPARAM(characters));
end;

procedure TCustomSciTextEditor.SetCharacterCategoryOptimization(countCharacters: Integer);
begin
  SendMessage(Handle, SCI_SETCHARACTERCATEGORYOPTIMIZATION, countCharacters, 0);
end;

function TCustomSciTextEditor.GetCharacterCategoryOptimization(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCHARACTERCATEGORYOPTIMIZATION, 0, 0);
end;

procedure TCustomSciTextEditor.BeginUndoAction();
begin
  SendMessage(Handle, SCI_BEGINUNDOACTION, 0, 0);
end;

procedure TCustomSciTextEditor.EndUndoAction();
begin
  SendMessage(Handle, SCI_ENDUNDOACTION, 0, 0);
end;

function TCustomSciTextEditor.GetUndoSequence(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOSEQUENCE, 0, 0);
end;

function TCustomSciTextEditor.GetUndoActions(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOACTIONS, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoSavePoint(action: Integer);
begin
  SendMessage(Handle, SCI_SETUNDOSAVEPOINT, action, 0);
end;

function TCustomSciTextEditor.GetUndoSavePoint(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOSAVEPOINT, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoDetach(action: Integer);
begin
  SendMessage(Handle, SCI_SETUNDODETACH, action, 0);
end;

function TCustomSciTextEditor.GetUndoDetach(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDODETACH, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoTentative(action: Integer);
begin
  SendMessage(Handle, SCI_SETUNDOTENTATIVE, action, 0);
end;

function TCustomSciTextEditor.GetUndoTentative(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOTENTATIVE, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoCurrent(action: Integer);
begin
  SendMessage(Handle, SCI_SETUNDOCURRENT, action, 0);
end;

function TCustomSciTextEditor.GetUndoCurrent(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOCURRENT, 0, 0);
end;

procedure TCustomSciTextEditor.PushUndoActionType(type: Integer; pos: Integer);
begin
  SendMessage(Handle, SCI_PUSHUNDOACTIONTYPE, type, pos);
end;

procedure TCustomSciTextEditor.ChangeLastUndoActionText(length: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_CHANGELASTUNDOACTIONTEXT, length, LPARAM(text));
end;

function TCustomSciTextEditor.GetUndoActionType(action: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOACTIONTYPE, action, 0);
end;

function TCustomSciTextEditor.GetUndoActionPosition(action: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOACTIONPOSITION, action, 0);
end;

function TCustomSciTextEditor.GetUndoActionText(action: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOACTIONTEXT, action, LPARAM(text));
end;

procedure TCustomSciTextEditor.IndicSetStyle(indicator: Integer; indicatorStyle: Integer);
begin
  SendMessage(Handle, SCI_INDICSETSTYLE, indicator, indicatorStyle);
end;

function TCustomSciTextEditor.IndicGetStyle(indicator: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICGETSTYLE, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetFore(indicator: Integer; fore: TColor);
begin
  SendMessage(Handle, SCI_INDICSETFORE, indicator, fore);
end;

function TCustomSciTextEditor.IndicGetFore(indicator: Integer): TColor;
begin
  Result := SendMessage(Handle, SCI_INDICGETFORE, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetUnder(indicator: Integer; under: Boolean);
begin
  SendMessage(Handle, SCI_INDICSETUNDER, indicator, under);
end;

function TCustomSciTextEditor.IndicGetUnder(indicator: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_INDICGETUNDER, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetHoverStyle(indicator: Integer; indicatorStyle: Integer);
begin
  SendMessage(Handle, SCI_INDICSETHOVERSTYLE, indicator, indicatorStyle);
end;

function TCustomSciTextEditor.IndicGetHoverStyle(indicator: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICGETHOVERSTYLE, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetHoverFore(indicator: Integer; fore: TColor);
begin
  SendMessage(Handle, SCI_INDICSETHOVERFORE, indicator, fore);
end;

function TCustomSciTextEditor.IndicGetHoverFore(indicator: Integer): TColor;
begin
  Result := SendMessage(Handle, SCI_INDICGETHOVERFORE, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetFlags(indicator: Integer; flags: Integer);
begin
  SendMessage(Handle, SCI_INDICSETFLAGS, indicator, flags);
end;

function TCustomSciTextEditor.IndicGetFlags(indicator: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICGETFLAGS, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetStrokeWidth(indicator: Integer; hundredths: Integer);
begin
  SendMessage(Handle, SCI_INDICSETSTROKEWIDTH, indicator, hundredths);
end;

function TCustomSciTextEditor.IndicGetStrokeWidth(indicator: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICGETSTROKEWIDTH, indicator, 0);
end;

procedure TCustomSciTextEditor.SetWhitespaceFore(useSetting: Boolean; fore: TColor);
begin
  SendMessage(Handle, SCI_SETWHITESPACEFORE, useSetting, fore);
end;

procedure TCustomSciTextEditor.SetWhitespaceBack(useSetting: Boolean; back: TColor);
begin
  SendMessage(Handle, SCI_SETWHITESPACEBACK, useSetting, back);
end;

procedure TCustomSciTextEditor.SetWhitespaceSize(size: Integer);
begin
  SendMessage(Handle, SCI_SETWHITESPACESIZE, size, 0);
end;

function TCustomSciTextEditor.GetWhitespaceSize(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWHITESPACESIZE, 0, 0);
end;

procedure TCustomSciTextEditor.SetLineState(line: Integer; state: Integer);
begin
  SendMessage(Handle, SCI_SETLINESTATE, line, state);
end;

function TCustomSciTextEditor.GetLineState(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINESTATE, line, 0);
end;

function TCustomSciTextEditor.GetMaxLineState(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMAXLINESTATE, 0, 0);
end;

function TCustomSciTextEditor.GetCaretLineVisible(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINEVISIBLE, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineVisible(show: Boolean);
begin
  SendMessage(Handle, SCI_SETCARETLINEVISIBLE, show, 0);
end;

function TCustomSciTextEditor.GetCaretLineBack(): TColor;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINEBACK, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineBack(back: TColor);
begin
  SendMessage(Handle, SCI_SETCARETLINEBACK, back, 0);
end;

function TCustomSciTextEditor.GetCaretLineFrame(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINEFRAME, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineFrame(width: Integer);
begin
  SendMessage(Handle, SCI_SETCARETLINEFRAME, width, 0);
end;

procedure TCustomSciTextEditor.StyleSetChangeable(style: Integer; changeable: Boolean);
begin
  SendMessage(Handle, SCI_STYLESETCHANGEABLE, style, changeable);
end;

procedure TCustomSciTextEditor.AutoCShow(lengthEntered: Integer; itemList: PAnsiChar);
begin
  SendMessage(Handle, SCI_AUTOCSHOW, lengthEntered, LPARAM(itemList));
end;

procedure TCustomSciTextEditor.AutoCCancel();
begin
  SendMessage(Handle, SCI_AUTOCCANCEL, 0, 0);
end;

function TCustomSciTextEditor.AutoCActive(): Boolean;
begin
  Result := SendMessage(Handle, SCI_AUTOCACTIVE, 0, 0);
end;

function TCustomSciTextEditor.AutoCPosStart(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCPOSSTART, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCComplete();
begin
  SendMessage(Handle, SCI_AUTOCCOMPLETE, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCStops(characterSet: PAnsiChar);
begin
  SendMessage(Handle, SCI_AUTOCSTOPS, 0, LPARAM(characterSet));
end;

procedure TCustomSciTextEditor.AutoCSetSeparator(separatorCharacter: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETSEPARATOR, separatorCharacter, 0);
end;

function TCustomSciTextEditor.AutoCGetSeparator(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETSEPARATOR, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSelect(select: PAnsiChar);
begin
  SendMessage(Handle, SCI_AUTOCSELECT, 0, LPARAM(select));
end;

procedure TCustomSciTextEditor.AutoCSetCancelAtStart(cancel: Boolean);
begin
  SendMessage(Handle, SCI_AUTOCSETCANCELATSTART, cancel, 0);
end;

function TCustomSciTextEditor.AutoCGetCancelAtStart(): Boolean;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETCANCELATSTART, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetFillUps(characterSet: PAnsiChar);
begin
  SendMessage(Handle, SCI_AUTOCSETFILLUPS, 0, LPARAM(characterSet));
end;

procedure TCustomSciTextEditor.AutoCSetChooseSingle(chooseSingle: Boolean);
begin
  SendMessage(Handle, SCI_AUTOCSETCHOOSESINGLE, chooseSingle, 0);
end;

function TCustomSciTextEditor.AutoCGetChooseSingle(): Boolean;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETCHOOSESINGLE, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetIgnoreCase(ignoreCase: Boolean);
begin
  SendMessage(Handle, SCI_AUTOCSETIGNORECASE, ignoreCase, 0);
end;

function TCustomSciTextEditor.AutoCGetIgnoreCase(): Boolean;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETIGNORECASE, 0, 0);
end;

procedure TCustomSciTextEditor.UserListShow(listType: Integer; itemList: PAnsiChar);
begin
  SendMessage(Handle, SCI_USERLISTSHOW, listType, LPARAM(itemList));
end;

procedure TCustomSciTextEditor.AutoCSetAutoHide(autoHide: Boolean);
begin
  SendMessage(Handle, SCI_AUTOCSETAUTOHIDE, autoHide, 0);
end;

function TCustomSciTextEditor.AutoCGetAutoHide(): Boolean;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETAUTOHIDE, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetOptions(options: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETOPTIONS, options, 0);
end;

function TCustomSciTextEditor.AutoCGetOptions(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETOPTIONS, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetDropRestOfWord(dropRestOfWord: Boolean);
begin
  SendMessage(Handle, SCI_AUTOCSETDROPRESTOFWORD, dropRestOfWord, 0);
end;

function TCustomSciTextEditor.AutoCGetDropRestOfWord(): Boolean;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETDROPRESTOFWORD, 0, 0);
end;

procedure TCustomSciTextEditor.RegisterImage(type: Integer; xpmData: PAnsiChar);
begin
  SendMessage(Handle, SCI_REGISTERIMAGE, type, LPARAM(xpmData));
end;

procedure TCustomSciTextEditor.ClearRegisteredImages();
begin
  SendMessage(Handle, SCI_CLEARREGISTEREDIMAGES, 0, 0);
end;

function TCustomSciTextEditor.AutoCGetTypeSeparator(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETTYPESEPARATOR, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetTypeSeparator(separatorCharacter: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETTYPESEPARATOR, separatorCharacter, 0);
end;

procedure TCustomSciTextEditor.AutoCSetMaxWidth(characterCount: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETMAXWIDTH, characterCount, 0);
end;

function TCustomSciTextEditor.AutoCGetMaxWidth(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETMAXWIDTH, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetMaxHeight(rowCount: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETMAXHEIGHT, rowCount, 0);
end;

function TCustomSciTextEditor.AutoCGetMaxHeight(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETMAXHEIGHT, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetStyle(style: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETSTYLE, style, 0);
end;

function TCustomSciTextEditor.AutoCGetStyle(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETSTYLE, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndent(indentSize: Integer);
begin
  SendMessage(Handle, SCI_SETINDENT, indentSize, 0);
end;

function TCustomSciTextEditor.GetIndent(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETINDENT, 0, 0);
end;

procedure TCustomSciTextEditor.SetUseTabs(useTabs: Boolean);
begin
  SendMessage(Handle, SCI_SETUSETABS, useTabs, 0);
end;

function TCustomSciTextEditor.GetUseTabs(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETUSETABS, 0, 0);
end;

procedure TCustomSciTextEditor.SetLineIndentation(line: Integer; indentation: Integer);
begin
  SendMessage(Handle, SCI_SETLINEINDENTATION, line, indentation);
end;

function TCustomSciTextEditor.GetLineIndentation(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINEINDENTATION, line, 0);
end;

function TCustomSciTextEditor.GetLineIndentPosition(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINEINDENTPOSITION, line, 0);
end;

function TCustomSciTextEditor.GetColumn(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCOLUMN, pos, 0);
end;

function TCustomSciTextEditor.CountCharacters(start: Integer; end: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_COUNTCHARACTERS, start, end);
end;

function TCustomSciTextEditor.CountCodeUnits(start: Integer; end: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_COUNTCODEUNITS, start, end);
end;

procedure TCustomSciTextEditor.SetHScrollBar(visible: Boolean);
begin
  SendMessage(Handle, SCI_SETHSCROLLBAR, visible, 0);
end;

function TCustomSciTextEditor.GetHScrollBar(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETHSCROLLBAR, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndentationGuides(indentView: Integer);
begin
  SendMessage(Handle, SCI_SETINDENTATIONGUIDES, indentView, 0);
end;

function TCustomSciTextEditor.GetIndentationGuides(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETINDENTATIONGUIDES, 0, 0);
end;

procedure TCustomSciTextEditor.SetHighlightGuide(column: Integer);
begin
  SendMessage(Handle, SCI_SETHIGHLIGHTGUIDE, column, 0);
end;

function TCustomSciTextEditor.GetHighlightGuide(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETHIGHLIGHTGUIDE, 0, 0);
end;

function TCustomSciTextEditor.GetLineEndPosition(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINEENDPOSITION, line, 0);
end;

function TCustomSciTextEditor.GetCodePage(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCODEPAGE, 0, 0);
end;

function TCustomSciTextEditor.GetCaretFore(): TColor;
begin
  Result := SendMessage(Handle, SCI_GETCARETFORE, 0, 0);
end;

function TCustomSciTextEditor.GetReadOnly(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETREADONLY, 0, 0);
end;

procedure TCustomSciTextEditor.SetCurrentPos(caret: Integer);
begin
  SendMessage(Handle, SCI_SETCURRENTPOS, caret, 0);
end;

procedure TCustomSciTextEditor.SetSelectionStart(anchor: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONSTART, anchor, 0);
end;

function TCustomSciTextEditor.GetSelectionStart(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONSTART, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionEnd(caret: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONEND, caret, 0);
end;

function TCustomSciTextEditor.GetSelectionEnd(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONEND, 0, 0);
end;

procedure TCustomSciTextEditor.SetEmptySelection(caret: Integer);
begin
  SendMessage(Handle, SCI_SETEMPTYSELECTION, caret, 0);
end;

procedure TCustomSciTextEditor.SetPrintMagnification(magnification: Integer);
begin
  SendMessage(Handle, SCI_SETPRINTMAGNIFICATION, magnification, 0);
end;

function TCustomSciTextEditor.GetPrintMagnification(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPRINTMAGNIFICATION, 0, 0);
end;

procedure TCustomSciTextEditor.SetPrintColourMode(mode: Integer);
begin
  SendMessage(Handle, SCI_SETPRINTCOLOURMODE, mode, 0);
end;

function TCustomSciTextEditor.GetPrintColourMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPRINTCOLOURMODE, 0, 0);
end;

function TCustomSciTextEditor.FindText(searchFlags: Integer; ft: PSciFindText): Integer;
begin
  Result := SendMessage(Handle, SCI_FINDTEXT, searchFlags, ft);
end;

function TCustomSciTextEditor.FindTextFull(searchFlags: Integer; ft: PSciFindTextFull): Integer;
begin
  Result := SendMessage(Handle, SCI_FINDTEXTFULL, searchFlags, ft);
end;

function TCustomSciTextEditor.FormatRange(draw: Boolean; fr: PFormatRange): Integer;
begin
  Result := SendMessage(Handle, SCI_FORMATRANGE, draw, fr);
end;

function TCustomSciTextEditor.FormatRangeFull(draw: Boolean; fr: PFormatRangeFull): Integer;
begin
  Result := SendMessage(Handle, SCI_FORMATRANGEFULL, draw, fr);
end;

procedure TCustomSciTextEditor.SetChangeHistory(changeHistory: Integer);
begin
  SendMessage(Handle, SCI_SETCHANGEHISTORY, changeHistory, 0);
end;

function TCustomSciTextEditor.GetChangeHistory(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCHANGEHISTORY, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoSelectionHistory(undoSelectionHistory: Integer);
begin
  SendMessage(Handle, SCI_SETUNDOSELECTIONHISTORY, undoSelectionHistory, 0);
end;

function TCustomSciTextEditor.GetUndoSelectionHistory(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETUNDOSELECTIONHISTORY, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionSerialized(selectionString: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETSELECTIONSERIALIZED, 0, LPARAM(selectionString));
end;

function TCustomSciTextEditor.GetSelectionSerialized(selectionString: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONSERIALIZED, 0, LPARAM(selectionString));
end;

function TCustomSciTextEditor.GetFirstVisibleLine(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETFIRSTVISIBLELINE, 0, 0);
end;

function TCustomSciTextEditor.GetLine(line: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINE, line, LPARAM(text));
end;

function TCustomSciTextEditor.GetLineCount(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINECOUNT, 0, 0);
end;

procedure TCustomSciTextEditor.AllocateLines(lines: Integer);
begin
  SendMessage(Handle, SCI_ALLOCATELINES, lines, 0);
end;

procedure TCustomSciTextEditor.SetMarginLeft(pixelWidth: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINLEFT, 0, pixelWidth);
end;

function TCustomSciTextEditor.GetMarginLeft(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINLEFT, 0, 0);
end;

procedure TCustomSciTextEditor.SetMarginRight(pixelWidth: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINRIGHT, 0, pixelWidth);
end;

function TCustomSciTextEditor.GetMarginRight(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINRIGHT, 0, 0);
end;

function TCustomSciTextEditor.GetModify(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMODIFY, 0, 0);
end;

procedure TCustomSciTextEditor.SetSel(anchor: Integer; caret: Integer);
begin
  SendMessage(Handle, SCI_SETSEL, anchor, caret);
end;

function TCustomSciTextEditor.GetSelText(text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELTEXT, 0, LPARAM(text));
end;

function TCustomSciTextEditor.GetTextRange(tr: PSciTextRange): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTEXTRANGE, 0, tr);
end;

function TCustomSciTextEditor.GetTextRangeFull(tr: PSciTextRangeFull): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTEXTRANGEFULL, 0, tr);
end;

procedure TCustomSciTextEditor.HideSelection(hide: Boolean);
begin
  SendMessage(Handle, SCI_HIDESELECTION, hide, 0);
end;

function TCustomSciTextEditor.GetSelectionHidden(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONHIDDEN, 0, 0);
end;

function TCustomSciTextEditor.PointXFromPosition(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POINTXFROMPOSITION, 0, pos);
end;

function TCustomSciTextEditor.PointYFromPosition(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POINTYFROMPOSITION, 0, pos);
end;

function TCustomSciTextEditor.LineFromPosition(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_LINEFROMPOSITION, pos, 0);
end;

function TCustomSciTextEditor.PositionFromLine(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONFROMLINE, line, 0);
end;

procedure TCustomSciTextEditor.LineScroll(columns: Integer; lines: Integer);
begin
  SendMessage(Handle, SCI_LINESCROLL, columns, lines);
end;

procedure TCustomSciTextEditor.ScrollCaret();
begin
  SendMessage(Handle, SCI_SCROLLCARET, 0, 0);
end;

procedure TCustomSciTextEditor.ScrollRange(secondary: Integer; primary: Integer);
begin
  SendMessage(Handle, SCI_SCROLLRANGE, secondary, primary);
end;

procedure TCustomSciTextEditor.ReplaceSel(text: PAnsiChar);
begin
  SendMessage(Handle, SCI_REPLACESEL, 0, LPARAM(text));
end;

procedure TCustomSciTextEditor.SetReadOnly(readOnly: Boolean);
begin
  SendMessage(Handle, SCI_SETREADONLY, readOnly, 0);
end;

procedure TCustomSciTextEditor.Null();
begin
  SendMessage(Handle, SCI_NULL, 0, 0);
end;

function TCustomSciTextEditor.CanPaste(): Boolean;
begin
  Result := SendMessage(Handle, SCI_CANPASTE, 0, 0);
end;

function TCustomSciTextEditor.CanUndo(): Boolean;
begin
  Result := SendMessage(Handle, SCI_CANUNDO, 0, 0);
end;

procedure TCustomSciTextEditor.EmptyUndoBuffer();
begin
  SendMessage(Handle, SCI_EMPTYUNDOBUFFER, 0, 0);
end;

procedure TCustomSciTextEditor.Undo();
begin
  SendMessage(Handle, SCI_UNDO, 0, 0);
end;

procedure TCustomSciTextEditor.Cut();
begin
  SendMessage(Handle, SCI_CUT, 0, 0);
end;

procedure TCustomSciTextEditor.Copy();
begin
  SendMessage(Handle, SCI_COPY, 0, 0);
end;

procedure TCustomSciTextEditor.Paste();
begin
  SendMessage(Handle, SCI_PASTE, 0, 0);
end;

procedure TCustomSciTextEditor.Clear();
begin
  SendMessage(Handle, SCI_CLEAR, 0, 0);
end;

procedure TCustomSciTextEditor.SetText(text: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETTEXT, 0, LPARAM(text));
end;

function TCustomSciTextEditor.GetText(length: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTEXT, length, LPARAM(text));
end;

function TCustomSciTextEditor.GetTextLength(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTEXTLENGTH, 0, 0);
end;

function TCustomSciTextEditor.GetDirectFunction(): Pointer;
begin
  Result := SendMessage(Handle, SCI_GETDIRECTFUNCTION, 0, 0);
end;

function TCustomSciTextEditor.GetDirectStatusFunction(): Pointer;
begin
  Result := SendMessage(Handle, SCI_GETDIRECTSTATUSFUNCTION, 0, 0);
end;

function TCustomSciTextEditor.GetDirectPointer(): Pointer;
begin
  Result := SendMessage(Handle, SCI_GETDIRECTPOINTER, 0, 0);
end;

procedure TCustomSciTextEditor.SetOvertype(overType: Boolean);
begin
  SendMessage(Handle, SCI_SETOVERTYPE, overType, 0);
end;

function TCustomSciTextEditor.GetOvertype(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETOVERTYPE, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretWidth(pixelWidth: Integer);
begin
  SendMessage(Handle, SCI_SETCARETWIDTH, pixelWidth, 0);
end;

function TCustomSciTextEditor.GetCaretWidth(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETWIDTH, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetStart(start: Integer);
begin
  SendMessage(Handle, SCI_SETTARGETSTART, start, 0);
end;

function TCustomSciTextEditor.GetTargetStart(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTARGETSTART, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetStartVirtualSpace(space: Integer);
begin
  SendMessage(Handle, SCI_SETTARGETSTARTVIRTUALSPACE, space, 0);
end;

function TCustomSciTextEditor.GetTargetStartVirtualSpace(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTARGETSTARTVIRTUALSPACE, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetEnd(end: Integer);
begin
  SendMessage(Handle, SCI_SETTARGETEND, end, 0);
end;

function TCustomSciTextEditor.GetTargetEnd(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTARGETEND, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetEndVirtualSpace(space: Integer);
begin
  SendMessage(Handle, SCI_SETTARGETENDVIRTUALSPACE, space, 0);
end;

function TCustomSciTextEditor.GetTargetEndVirtualSpace(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTARGETENDVIRTUALSPACE, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetRange(start: Integer; end: Integer);
begin
  SendMessage(Handle, SCI_SETTARGETRANGE, start, end);
end;

function TCustomSciTextEditor.GetTargetText(text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTARGETTEXT, 0, LPARAM(text));
end;

procedure TCustomSciTextEditor.TargetFromSelection();
begin
  SendMessage(Handle, SCI_TARGETFROMSELECTION, 0, 0);
end;

procedure TCustomSciTextEditor.TargetWholeDocument();
begin
  SendMessage(Handle, SCI_TARGETWHOLEDOCUMENT, 0, 0);
end;

function TCustomSciTextEditor.ReplaceTarget(length: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_REPLACETARGET, length, LPARAM(text));
end;

function TCustomSciTextEditor.ReplaceTargetRE(length: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_REPLACETARGETRE, length, LPARAM(text));
end;

function TCustomSciTextEditor.ReplaceTargetMinimal(length: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_REPLACETARGETMINIMAL, length, LPARAM(text));
end;

function TCustomSciTextEditor.SearchInTarget(length: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_SEARCHINTARGET, length, LPARAM(text));
end;

procedure TCustomSciTextEditor.SetSearchFlags(searchFlags: Integer);
begin
  SendMessage(Handle, SCI_SETSEARCHFLAGS, searchFlags, 0);
end;

function TCustomSciTextEditor.GetSearchFlags(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSEARCHFLAGS, 0, 0);
end;

procedure TCustomSciTextEditor.CallTipShow(pos: Integer; definition: PAnsiChar);
begin
  SendMessage(Handle, SCI_CALLTIPSHOW, pos, LPARAM(definition));
end;

procedure TCustomSciTextEditor.CallTipCancel();
begin
  SendMessage(Handle, SCI_CALLTIPCANCEL, 0, 0);
end;

function TCustomSciTextEditor.CallTipActive(): Boolean;
begin
  Result := SendMessage(Handle, SCI_CALLTIPACTIVE, 0, 0);
end;

function TCustomSciTextEditor.CallTipPosStart(): Integer;
begin
  Result := SendMessage(Handle, SCI_CALLTIPPOSSTART, 0, 0);
end;

procedure TCustomSciTextEditor.CallTipSetPosStart(posStart: Integer);
begin
  SendMessage(Handle, SCI_CALLTIPSETPOSSTART, posStart, 0);
end;

procedure TCustomSciTextEditor.CallTipSetHlt(highlightStart: Integer; highlightEnd: Integer);
begin
  SendMessage(Handle, SCI_CALLTIPSETHLT, highlightStart, highlightEnd);
end;

procedure TCustomSciTextEditor.CallTipSetBack(back: TColor);
begin
  SendMessage(Handle, SCI_CALLTIPSETBACK, back, 0);
end;

procedure TCustomSciTextEditor.CallTipSetFore(fore: TColor);
begin
  SendMessage(Handle, SCI_CALLTIPSETFORE, fore, 0);
end;

procedure TCustomSciTextEditor.CallTipSetForeHlt(fore: TColor);
begin
  SendMessage(Handle, SCI_CALLTIPSETFOREHLT, fore, 0);
end;

procedure TCustomSciTextEditor.CallTipUseStyle(tabSize: Integer);
begin
  SendMessage(Handle, SCI_CALLTIPUSESTYLE, tabSize, 0);
end;

procedure TCustomSciTextEditor.CallTipSetPosition(above: Boolean);
begin
  SendMessage(Handle, SCI_CALLTIPSETPOSITION, above, 0);
end;

function TCustomSciTextEditor.VisibleFromDocLine(docLine: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_VISIBLEFROMDOCLINE, docLine, 0);
end;

function TCustomSciTextEditor.DocLineFromVisible(displayLine: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_DOCLINEFROMVISIBLE, displayLine, 0);
end;

function TCustomSciTextEditor.WrapCount(docLine: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_WRAPCOUNT, docLine, 0);
end;

procedure TCustomSciTextEditor.SetFoldLevel(line: Integer; level: Integer);
begin
  SendMessage(Handle, SCI_SETFOLDLEVEL, line, level);
end;

function TCustomSciTextEditor.GetFoldLevel(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETFOLDLEVEL, line, 0);
end;

function TCustomSciTextEditor.GetLastChild(line: Integer; level: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLASTCHILD, line, level);
end;

function TCustomSciTextEditor.GetFoldParent(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETFOLDPARENT, line, 0);
end;

procedure TCustomSciTextEditor.ShowLines(lineStart: Integer; lineEnd: Integer);
begin
  SendMessage(Handle, SCI_SHOWLINES, lineStart, lineEnd);
end;

procedure TCustomSciTextEditor.HideLines(lineStart: Integer; lineEnd: Integer);
begin
  SendMessage(Handle, SCI_HIDELINES, lineStart, lineEnd);
end;

function TCustomSciTextEditor.GetLineVisible(line: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETLINEVISIBLE, line, 0);
end;

function TCustomSciTextEditor.GetAllLinesVisible(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETALLLINESVISIBLE, 0, 0);
end;

procedure TCustomSciTextEditor.SetFoldExpanded(line: Integer; expanded: Boolean);
begin
  SendMessage(Handle, SCI_SETFOLDEXPANDED, line, expanded);
end;

function TCustomSciTextEditor.GetFoldExpanded(line: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETFOLDEXPANDED, line, 0);
end;

procedure TCustomSciTextEditor.ToggleFold(line: Integer);
begin
  SendMessage(Handle, SCI_TOGGLEFOLD, line, 0);
end;

procedure TCustomSciTextEditor.ToggleFoldShowText(line: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_TOGGLEFOLDSHOWTEXT, line, LPARAM(text));
end;

procedure TCustomSciTextEditor.FoldDisplayTextSetStyle(style: Integer);
begin
  SendMessage(Handle, SCI_FOLDDISPLAYTEXTSETSTYLE, style, 0);
end;

function TCustomSciTextEditor.FoldDisplayTextGetStyle(): Integer;
begin
  Result := SendMessage(Handle, SCI_FOLDDISPLAYTEXTGETSTYLE, 0, 0);
end;

procedure TCustomSciTextEditor.SetDefaultFoldDisplayText(text: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETDEFAULTFOLDDISPLAYTEXT, 0, LPARAM(text));
end;

function TCustomSciTextEditor.GetDefaultFoldDisplayText(text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETDEFAULTFOLDDISPLAYTEXT, 0, LPARAM(text));
end;

procedure TCustomSciTextEditor.FoldLine(line: Integer; action: Integer);
begin
  SendMessage(Handle, SCI_FOLDLINE, line, action);
end;

procedure TCustomSciTextEditor.FoldChildren(line: Integer; action: Integer);
begin
  SendMessage(Handle, SCI_FOLDCHILDREN, line, action);
end;

procedure TCustomSciTextEditor.ExpandChildren(line: Integer; level: Integer);
begin
  SendMessage(Handle, SCI_EXPANDCHILDREN, line, level);
end;

procedure TCustomSciTextEditor.FoldAll(action: Integer);
begin
  SendMessage(Handle, SCI_FOLDALL, action, 0);
end;

procedure TCustomSciTextEditor.EnsureVisible(line: Integer);
begin
  SendMessage(Handle, SCI_ENSUREVISIBLE, line, 0);
end;

procedure TCustomSciTextEditor.SetAutomaticFold(automaticFold: Integer);
begin
  SendMessage(Handle, SCI_SETAUTOMATICFOLD, automaticFold, 0);
end;

function TCustomSciTextEditor.GetAutomaticFold(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETAUTOMATICFOLD, 0, 0);
end;

procedure TCustomSciTextEditor.SetFoldFlags(flags: Integer);
begin
  SendMessage(Handle, SCI_SETFOLDFLAGS, flags, 0);
end;

procedure TCustomSciTextEditor.EnsureVisibleEnforcePolicy(line: Integer);
begin
  SendMessage(Handle, SCI_ENSUREVISIBLEENFORCEPOLICY, line, 0);
end;

procedure TCustomSciTextEditor.SetTabIndents(tabIndents: Boolean);
begin
  SendMessage(Handle, SCI_SETTABINDENTS, tabIndents, 0);
end;

function TCustomSciTextEditor.GetTabIndents(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETTABINDENTS, 0, 0);
end;

procedure TCustomSciTextEditor.SetBackSpaceUnIndents(bsUnIndents: Boolean);
begin
  SendMessage(Handle, SCI_SETBACKSPACEUNINDENTS, bsUnIndents, 0);
end;

function TCustomSciTextEditor.GetBackSpaceUnIndents(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETBACKSPACEUNINDENTS, 0, 0);
end;

procedure TCustomSciTextEditor.SetMouseDwellTime(periodMilliseconds: Integer);
begin
  SendMessage(Handle, SCI_SETMOUSEDWELLTIME, periodMilliseconds, 0);
end;

function TCustomSciTextEditor.GetMouseDwellTime(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMOUSEDWELLTIME, 0, 0);
end;

function TCustomSciTextEditor.WordStartPosition(pos: Integer; onlyWordCharacters: Boolean): Integer;
begin
  Result := SendMessage(Handle, SCI_WORDSTARTPOSITION, pos, onlyWordCharacters);
end;

function TCustomSciTextEditor.WordEndPosition(pos: Integer; onlyWordCharacters: Boolean): Integer;
begin
  Result := SendMessage(Handle, SCI_WORDENDPOSITION, pos, onlyWordCharacters);
end;

function TCustomSciTextEditor.IsRangeWord(start: Integer; end: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_ISRANGEWORD, start, end);
end;

procedure TCustomSciTextEditor.SetIdleStyling(idleStyling: Integer);
begin
  SendMessage(Handle, SCI_SETIDLESTYLING, idleStyling, 0);
end;

function TCustomSciTextEditor.GetIdleStyling(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETIDLESTYLING, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapMode(wrapMode: Integer);
begin
  SendMessage(Handle, SCI_SETWRAPMODE, wrapMode, 0);
end;

function TCustomSciTextEditor.GetWrapMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWRAPMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapVisualFlags(wrapVisualFlags: Integer);
begin
  SendMessage(Handle, SCI_SETWRAPVISUALFLAGS, wrapVisualFlags, 0);
end;

function TCustomSciTextEditor.GetWrapVisualFlags(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWRAPVISUALFLAGS, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapVisualFlagsLocation(wrapVisualFlagsLocation: Integer);
begin
  SendMessage(Handle, SCI_SETWRAPVISUALFLAGSLOCATION, wrapVisualFlagsLocation, 0);
end;

function TCustomSciTextEditor.GetWrapVisualFlagsLocation(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWRAPVISUALFLAGSLOCATION, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapStartIndent(indent: Integer);
begin
  SendMessage(Handle, SCI_SETWRAPSTARTINDENT, indent, 0);
end;

function TCustomSciTextEditor.GetWrapStartIndent(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWRAPSTARTINDENT, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapIndentMode(wrapIndentMode: Integer);
begin
  SendMessage(Handle, SCI_SETWRAPINDENTMODE, wrapIndentMode, 0);
end;

function TCustomSciTextEditor.GetWrapIndentMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWRAPINDENTMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetLayoutCache(cacheMode: Integer);
begin
  SendMessage(Handle, SCI_SETLAYOUTCACHE, cacheMode, 0);
end;

function TCustomSciTextEditor.GetLayoutCache(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLAYOUTCACHE, 0, 0);
end;

procedure TCustomSciTextEditor.SetScrollWidth(pixelWidth: Integer);
begin
  SendMessage(Handle, SCI_SETSCROLLWIDTH, pixelWidth, 0);
end;

function TCustomSciTextEditor.GetScrollWidth(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSCROLLWIDTH, 0, 0);
end;

procedure TCustomSciTextEditor.SetScrollWidthTracking(tracking: Boolean);
begin
  SendMessage(Handle, SCI_SETSCROLLWIDTHTRACKING, tracking, 0);
end;

function TCustomSciTextEditor.GetScrollWidthTracking(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETSCROLLWIDTHTRACKING, 0, 0);
end;

function TCustomSciTextEditor.TextWidth(style: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_TEXTWIDTH, style, LPARAM(text));
end;

procedure TCustomSciTextEditor.SetEndAtLastLine(endAtLastLine: Boolean);
begin
  SendMessage(Handle, SCI_SETENDATLASTLINE, endAtLastLine, 0);
end;

function TCustomSciTextEditor.GetEndAtLastLine(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETENDATLASTLINE, 0, 0);
end;

function TCustomSciTextEditor.TextHeight(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_TEXTHEIGHT, line, 0);
end;

procedure TCustomSciTextEditor.SetVScrollBar(visible: Boolean);
begin
  SendMessage(Handle, SCI_SETVSCROLLBAR, visible, 0);
end;

function TCustomSciTextEditor.GetVScrollBar(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETVSCROLLBAR, 0, 0);
end;

procedure TCustomSciTextEditor.AppendText(length: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_APPENDTEXT, length, LPARAM(text));
end;

function TCustomSciTextEditor.GetPhasesDraw(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPHASESDRAW, 0, 0);
end;

procedure TCustomSciTextEditor.SetPhasesDraw(phases: Integer);
begin
  SendMessage(Handle, SCI_SETPHASESDRAW, phases, 0);
end;

procedure TCustomSciTextEditor.SetFontQuality(fontQuality: Integer);
begin
  SendMessage(Handle, SCI_SETFONTQUALITY, fontQuality, 0);
end;

function TCustomSciTextEditor.GetFontQuality(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETFONTQUALITY, 0, 0);
end;

procedure TCustomSciTextEditor.SetFirstVisibleLine(displayLine: Integer);
begin
  SendMessage(Handle, SCI_SETFIRSTVISIBLELINE, displayLine, 0);
end;

procedure TCustomSciTextEditor.SetMultiPaste(multiPaste: Integer);
begin
  SendMessage(Handle, SCI_SETMULTIPASTE, multiPaste, 0);
end;

function TCustomSciTextEditor.GetMultiPaste(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMULTIPASTE, 0, 0);
end;

function TCustomSciTextEditor.GetTag(tagNumber: Integer; tagValue: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTAG, tagNumber, LPARAM(tagValue));
end;

procedure TCustomSciTextEditor.LinesJoin();
begin
  SendMessage(Handle, SCI_LINESJOIN, 0, 0);
end;

procedure TCustomSciTextEditor.LinesSplit(pixelWidth: Integer);
begin
  SendMessage(Handle, SCI_LINESSPLIT, pixelWidth, 0);
end;

procedure TCustomSciTextEditor.SetFoldMarginColour(useSetting: Boolean; back: TColor);
begin
  SendMessage(Handle, SCI_SETFOLDMARGINCOLOUR, useSetting, back);
end;

procedure TCustomSciTextEditor.SetFoldMarginHiColour(useSetting: Boolean; fore: TColor);
begin
  SendMessage(Handle, SCI_SETFOLDMARGINHICOLOUR, useSetting, fore);
end;

procedure TCustomSciTextEditor.SetAccessibility(accessibility: Integer);
begin
  SendMessage(Handle, SCI_SETACCESSIBILITY, accessibility, 0);
end;

function TCustomSciTextEditor.GetAccessibility(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETACCESSIBILITY, 0, 0);
end;

procedure TCustomSciTextEditor.LineDown();
begin
  SendMessage(Handle, SCI_LINEDOWN, 0, 0);
end;

procedure TCustomSciTextEditor.LineDownExtend();
begin
  SendMessage(Handle, SCI_LINEDOWNEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineUp();
begin
  SendMessage(Handle, SCI_LINEUP, 0, 0);
end;

procedure TCustomSciTextEditor.LineUpExtend();
begin
  SendMessage(Handle, SCI_LINEUPEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.CharLeft();
begin
  SendMessage(Handle, SCI_CHARLEFT, 0, 0);
end;

procedure TCustomSciTextEditor.CharLeftExtend();
begin
  SendMessage(Handle, SCI_CHARLEFTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.CharRight();
begin
  SendMessage(Handle, SCI_CHARRIGHT, 0, 0);
end;

procedure TCustomSciTextEditor.CharRightExtend();
begin
  SendMessage(Handle, SCI_CHARRIGHTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeft();
begin
  SendMessage(Handle, SCI_WORDLEFT, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeftExtend();
begin
  SendMessage(Handle, SCI_WORDLEFTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordRight();
begin
  SendMessage(Handle, SCI_WORDRIGHT, 0, 0);
end;

procedure TCustomSciTextEditor.WordRightExtend();
begin
  SendMessage(Handle, SCI_WORDRIGHTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.Home();
begin
  SendMessage(Handle, SCI_HOME, 0, 0);
end;

procedure TCustomSciTextEditor.HomeExtend();
begin
  SendMessage(Handle, SCI_HOMEEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineEnd();
begin
  SendMessage(Handle, SCI_LINEEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndExtend();
begin
  SendMessage(Handle, SCI_LINEENDEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentStart();
begin
  SendMessage(Handle, SCI_DOCUMENTSTART, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentStartExtend();
begin
  SendMessage(Handle, SCI_DOCUMENTSTARTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentEnd();
begin
  SendMessage(Handle, SCI_DOCUMENTEND, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentEndExtend();
begin
  SendMessage(Handle, SCI_DOCUMENTENDEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.PageUp();
begin
  SendMessage(Handle, SCI_PAGEUP, 0, 0);
end;

procedure TCustomSciTextEditor.PageUpExtend();
begin
  SendMessage(Handle, SCI_PAGEUPEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.PageDown();
begin
  SendMessage(Handle, SCI_PAGEDOWN, 0, 0);
end;

procedure TCustomSciTextEditor.PageDownExtend();
begin
  SendMessage(Handle, SCI_PAGEDOWNEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.EditToggleOvertype();
begin
  SendMessage(Handle, SCI_EDITTOGGLEOVERTYPE, 0, 0);
end;

procedure TCustomSciTextEditor.Cancel();
begin
  SendMessage(Handle, SCI_CANCEL, 0, 0);
end;

procedure TCustomSciTextEditor.DeleteBack();
begin
  SendMessage(Handle, SCI_DELETEBACK, 0, 0);
end;

procedure TCustomSciTextEditor.Tab();
begin
  SendMessage(Handle, SCI_TAB, 0, 0);
end;

procedure TCustomSciTextEditor.LineIndent();
begin
  SendMessage(Handle, SCI_LINEINDENT, 0, 0);
end;

procedure TCustomSciTextEditor.BackTab();
begin
  SendMessage(Handle, SCI_BACKTAB, 0, 0);
end;

procedure TCustomSciTextEditor.LineDedent();
begin
  SendMessage(Handle, SCI_LINEDEDENT, 0, 0);
end;

procedure TCustomSciTextEditor.NewLine();
begin
  SendMessage(Handle, SCI_NEWLINE, 0, 0);
end;

procedure TCustomSciTextEditor.FormFeed();
begin
  SendMessage(Handle, SCI_FORMFEED, 0, 0);
end;

procedure TCustomSciTextEditor.VCHome();
begin
  SendMessage(Handle, SCI_VCHOME, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeExtend();
begin
  SendMessage(Handle, SCI_VCHOMEEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.ZoomIn();
begin
  SendMessage(Handle, SCI_ZOOMIN, 0, 0);
end;

procedure TCustomSciTextEditor.ZoomOut();
begin
  SendMessage(Handle, SCI_ZOOMOUT, 0, 0);
end;

procedure TCustomSciTextEditor.DelWordLeft();
begin
  SendMessage(Handle, SCI_DELWORDLEFT, 0, 0);
end;

procedure TCustomSciTextEditor.DelWordRight();
begin
  SendMessage(Handle, SCI_DELWORDRIGHT, 0, 0);
end;

procedure TCustomSciTextEditor.DelWordRightEnd();
begin
  SendMessage(Handle, SCI_DELWORDRIGHTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineCut();
begin
  SendMessage(Handle, SCI_LINECUT, 0, 0);
end;

procedure TCustomSciTextEditor.LineDelete();
begin
  SendMessage(Handle, SCI_LINEDELETE, 0, 0);
end;

procedure TCustomSciTextEditor.LineTranspose();
begin
  SendMessage(Handle, SCI_LINETRANSPOSE, 0, 0);
end;

procedure TCustomSciTextEditor.LineReverse();
begin
  SendMessage(Handle, SCI_LINEREVERSE, 0, 0);
end;

procedure TCustomSciTextEditor.LineDuplicate();
begin
  SendMessage(Handle, SCI_LINEDUPLICATE, 0, 0);
end;

procedure TCustomSciTextEditor.LowerCase();
begin
  SendMessage(Handle, SCI_LOWERCASE, 0, 0);
end;

procedure TCustomSciTextEditor.UpperCase();
begin
  SendMessage(Handle, SCI_UPPERCASE, 0, 0);
end;

procedure TCustomSciTextEditor.LineScrollDown();
begin
  SendMessage(Handle, SCI_LINESCROLLDOWN, 0, 0);
end;

procedure TCustomSciTextEditor.LineScrollUp();
begin
  SendMessage(Handle, SCI_LINESCROLLUP, 0, 0);
end;

procedure TCustomSciTextEditor.DeleteBackNotLine();
begin
  SendMessage(Handle, SCI_DELETEBACKNOTLINE, 0, 0);
end;

procedure TCustomSciTextEditor.HomeDisplay();
begin
  SendMessage(Handle, SCI_HOMEDISPLAY, 0, 0);
end;

procedure TCustomSciTextEditor.HomeDisplayExtend();
begin
  SendMessage(Handle, SCI_HOMEDISPLAYEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndDisplay();
begin
  SendMessage(Handle, SCI_LINEENDDISPLAY, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndDisplayExtend();
begin
  SendMessage(Handle, SCI_LINEENDDISPLAYEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.HomeWrap();
begin
  SendMessage(Handle, SCI_HOMEWRAP, 0, 0);
end;

procedure TCustomSciTextEditor.HomeWrapExtend();
begin
  SendMessage(Handle, SCI_HOMEWRAPEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndWrap();
begin
  SendMessage(Handle, SCI_LINEENDWRAP, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndWrapExtend();
begin
  SendMessage(Handle, SCI_LINEENDWRAPEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeWrap();
begin
  SendMessage(Handle, SCI_VCHOMEWRAP, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeWrapExtend();
begin
  SendMessage(Handle, SCI_VCHOMEWRAPEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineCopy();
begin
  SendMessage(Handle, SCI_LINECOPY, 0, 0);
end;

procedure TCustomSciTextEditor.MoveCaretInsideView();
begin
  SendMessage(Handle, SCI_MOVECARETINSIDEVIEW, 0, 0);
end;

function TCustomSciTextEditor.LineLength(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_LINELENGTH, line, 0);
end;

procedure TCustomSciTextEditor.BraceHighlight(posA: Integer; posB: Integer);
begin
  SendMessage(Handle, SCI_BRACEHIGHLIGHT, posA, posB);
end;

procedure TCustomSciTextEditor.BraceHighlightIndicator(useSetting: Boolean; indicator: Integer);
begin
  SendMessage(Handle, SCI_BRACEHIGHLIGHTINDICATOR, useSetting, indicator);
end;

procedure TCustomSciTextEditor.BraceBadLight(pos: Integer);
begin
  SendMessage(Handle, SCI_BRACEBADLIGHT, pos, 0);
end;

procedure TCustomSciTextEditor.BraceBadLightIndicator(useSetting: Boolean; indicator: Integer);
begin
  SendMessage(Handle, SCI_BRACEBADLIGHTINDICATOR, useSetting, indicator);
end;

function TCustomSciTextEditor.BraceMatch(pos: Integer; maxReStyle: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_BRACEMATCH, pos, maxReStyle);
end;

function TCustomSciTextEditor.BraceMatchNext(pos: Integer; startPos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_BRACEMATCHNEXT, pos, startPos);
end;

function TCustomSciTextEditor.GetViewEOL(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETVIEWEOL, 0, 0);
end;

procedure TCustomSciTextEditor.SetViewEOL(visible: Boolean);
begin
  SendMessage(Handle, SCI_SETVIEWEOL, visible, 0);
end;

function TCustomSciTextEditor.GetDocPointer(): Pointer;
begin
  Result := SendMessage(Handle, SCI_GETDOCPOINTER, 0, 0);
end;

procedure TCustomSciTextEditor.SetDocPointer(doc: Pointer);
begin
  SendMessage(Handle, SCI_SETDOCPOINTER, 0, LPARAM(doc));
end;

procedure TCustomSciTextEditor.SetModEventMask(eventMask: Integer);
begin
  SendMessage(Handle, SCI_SETMODEVENTMASK, eventMask, 0);
end;

function TCustomSciTextEditor.GetEdgeColumn(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETEDGECOLUMN, 0, 0);
end;

procedure TCustomSciTextEditor.SetEdgeColumn(column: Integer);
begin
  SendMessage(Handle, SCI_SETEDGECOLUMN, column, 0);
end;

function TCustomSciTextEditor.GetEdgeMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETEDGEMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetEdgeMode(edgeMode: Integer);
begin
  SendMessage(Handle, SCI_SETEDGEMODE, edgeMode, 0);
end;

function TCustomSciTextEditor.GetEdgeColour(): TColor;
begin
  Result := SendMessage(Handle, SCI_GETEDGECOLOUR, 0, 0);
end;

procedure TCustomSciTextEditor.SetEdgeColour(edgeColour: TColor);
begin
  SendMessage(Handle, SCI_SETEDGECOLOUR, edgeColour, 0);
end;

procedure TCustomSciTextEditor.MultiEdgeAddLine(column: Integer; edgeColour: TColor);
begin
  SendMessage(Handle, SCI_MULTIEDGEADDLINE, column, edgeColour);
end;

procedure TCustomSciTextEditor.MultiEdgeClearAll();
begin
  SendMessage(Handle, SCI_MULTIEDGECLEARALL, 0, 0);
end;

function TCustomSciTextEditor.GetMultiEdgeColumn(which: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMULTIEDGECOLUMN, which, 0);
end;

procedure TCustomSciTextEditor.SearchAnchor();
begin
  SendMessage(Handle, SCI_SEARCHANCHOR, 0, 0);
end;

function TCustomSciTextEditor.SearchNext(searchFlags: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_SEARCHNEXT, searchFlags, LPARAM(text));
end;

function TCustomSciTextEditor.SearchPrev(searchFlags: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_SEARCHPREV, searchFlags, LPARAM(text));
end;

function TCustomSciTextEditor.LinesOnScreen(): Integer;
begin
  Result := SendMessage(Handle, SCI_LINESONSCREEN, 0, 0);
end;

procedure TCustomSciTextEditor.UsePopUp(popUpMode: Integer);
begin
  SendMessage(Handle, SCI_USEPOPUP, popUpMode, 0);
end;

function TCustomSciTextEditor.SelectionIsRectangle(): Boolean;
begin
  Result := SendMessage(Handle, SCI_SELECTIONISRECTANGLE, 0, 0);
end;

procedure TCustomSciTextEditor.SetZoom(zoomInPoints: Integer);
begin
  SendMessage(Handle, SCI_SETZOOM, zoomInPoints, 0);
end;

function TCustomSciTextEditor.GetZoom(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETZOOM, 0, 0);
end;

function TCustomSciTextEditor.CreateDocument(bytes: Integer; documentOptions: Integer): Pointer;
begin
  Result := SendMessage(Handle, SCI_CREATEDOCUMENT, bytes, documentOptions);
end;

procedure TCustomSciTextEditor.AddRefDocument(doc: Pointer);
begin
  SendMessage(Handle, SCI_ADDREFDOCUMENT, 0, LPARAM(doc));
end;

procedure TCustomSciTextEditor.ReleaseDocument(doc: Pointer);
begin
  SendMessage(Handle, SCI_RELEASEDOCUMENT, 0, LPARAM(doc));
end;

function TCustomSciTextEditor.GetDocumentOptions(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETDOCUMENTOPTIONS, 0, 0);
end;

function TCustomSciTextEditor.GetModEventMask(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMODEVENTMASK, 0, 0);
end;

procedure TCustomSciTextEditor.SetCommandEvents(commandEvents: Boolean);
begin
  SendMessage(Handle, SCI_SETCOMMANDEVENTS, commandEvents, 0);
end;

function TCustomSciTextEditor.GetCommandEvents(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETCOMMANDEVENTS, 0, 0);
end;

procedure TCustomSciTextEditor.SetFocus(focus: Boolean);
begin
  SendMessage(Handle, SCI_SETFOCUS, focus, 0);
end;

function TCustomSciTextEditor.GetFocus(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETFOCUS, 0, 0);
end;

procedure TCustomSciTextEditor.SetStatus(status: Integer);
begin
  SendMessage(Handle, SCI_SETSTATUS, status, 0);
end;

function TCustomSciTextEditor.GetStatus(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSTATUS, 0, 0);
end;

procedure TCustomSciTextEditor.SetMouseDownCaptures(captures: Boolean);
begin
  SendMessage(Handle, SCI_SETMOUSEDOWNCAPTURES, captures, 0);
end;

function TCustomSciTextEditor.GetMouseDownCaptures(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMOUSEDOWNCAPTURES, 0, 0);
end;

procedure TCustomSciTextEditor.SetMouseWheelCaptures(captures: Boolean);
begin
  SendMessage(Handle, SCI_SETMOUSEWHEELCAPTURES, captures, 0);
end;

function TCustomSciTextEditor.GetMouseWheelCaptures(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMOUSEWHEELCAPTURES, 0, 0);
end;

procedure TCustomSciTextEditor.SetCursor(cursorType: Integer);
begin
  SendMessage(Handle, SCI_SETCURSOR, cursorType, 0);
end;

function TCustomSciTextEditor.GetCursor(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCURSOR, 0, 0);
end;

procedure TCustomSciTextEditor.SetControlCharSymbol(symbol: Integer);
begin
  SendMessage(Handle, SCI_SETCONTROLCHARSYMBOL, symbol, 0);
end;

function TCustomSciTextEditor.GetControlCharSymbol(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCONTROLCHARSYMBOL, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartLeft();
begin
  SendMessage(Handle, SCI_WORDPARTLEFT, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartLeftExtend();
begin
  SendMessage(Handle, SCI_WORDPARTLEFTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartRight();
begin
  SendMessage(Handle, SCI_WORDPARTRIGHT, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartRightExtend();
begin
  SendMessage(Handle, SCI_WORDPARTRIGHTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.SetVisiblePolicy(visiblePolicy: Integer; visibleSlop: Integer);
begin
  SendMessage(Handle, SCI_SETVISIBLEPOLICY, visiblePolicy, visibleSlop);
end;

procedure TCustomSciTextEditor.DelLineLeft();
begin
  SendMessage(Handle, SCI_DELLINELEFT, 0, 0);
end;

procedure TCustomSciTextEditor.DelLineRight();
begin
  SendMessage(Handle, SCI_DELLINERIGHT, 0, 0);
end;

procedure TCustomSciTextEditor.SetXOffset(xOffset: Integer);
begin
  SendMessage(Handle, SCI_SETXOFFSET, xOffset, 0);
end;

function TCustomSciTextEditor.GetXOffset(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETXOFFSET, 0, 0);
end;

procedure TCustomSciTextEditor.ChooseCaretX();
begin
  SendMessage(Handle, SCI_CHOOSECARETX, 0, 0);
end;

procedure TCustomSciTextEditor.GrabFocus();
begin
  SendMessage(Handle, SCI_GRABFOCUS, 0, 0);
end;

procedure TCustomSciTextEditor.SetXCaretPolicy(caretPolicy: Integer; caretSlop: Integer);
begin
  SendMessage(Handle, SCI_SETXCARETPOLICY, caretPolicy, caretSlop);
end;

procedure TCustomSciTextEditor.SetYCaretPolicy(caretPolicy: Integer; caretSlop: Integer);
begin
  SendMessage(Handle, SCI_SETYCARETPOLICY, caretPolicy, caretSlop);
end;

procedure TCustomSciTextEditor.SetPrintWrapMode(wrapMode: Integer);
begin
  SendMessage(Handle, SCI_SETPRINTWRAPMODE, wrapMode, 0);
end;

function TCustomSciTextEditor.GetPrintWrapMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPRINTWRAPMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotActiveFore(useSetting: Boolean; fore: TColor);
begin
  SendMessage(Handle, SCI_SETHOTSPOTACTIVEFORE, useSetting, fore);
end;

function TCustomSciTextEditor.GetHotspotActiveFore(): TColor;
begin
  Result := SendMessage(Handle, SCI_GETHOTSPOTACTIVEFORE, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotActiveBack(useSetting: Boolean; back: TColor);
begin
  SendMessage(Handle, SCI_SETHOTSPOTACTIVEBACK, useSetting, back);
end;

function TCustomSciTextEditor.GetHotspotActiveBack(): TColor;
begin
  Result := SendMessage(Handle, SCI_GETHOTSPOTACTIVEBACK, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotActiveUnderline(underline: Boolean);
begin
  SendMessage(Handle, SCI_SETHOTSPOTACTIVEUNDERLINE, underline, 0);
end;

function TCustomSciTextEditor.GetHotspotActiveUnderline(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETHOTSPOTACTIVEUNDERLINE, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotSingleLine(singleLine: Boolean);
begin
  SendMessage(Handle, SCI_SETHOTSPOTSINGLELINE, singleLine, 0);
end;

function TCustomSciTextEditor.GetHotspotSingleLine(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETHOTSPOTSINGLELINE, 0, 0);
end;

procedure TCustomSciTextEditor.ParaDown();
begin
  SendMessage(Handle, SCI_PARADOWN, 0, 0);
end;

procedure TCustomSciTextEditor.ParaDownExtend();
begin
  SendMessage(Handle, SCI_PARADOWNEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.ParaUp();
begin
  SendMessage(Handle, SCI_PARAUP, 0, 0);
end;

procedure TCustomSciTextEditor.ParaUpExtend();
begin
  SendMessage(Handle, SCI_PARAUPEXTEND, 0, 0);
end;

function TCustomSciTextEditor.PositionBefore(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONBEFORE, pos, 0);
end;

function TCustomSciTextEditor.PositionAfter(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONAFTER, pos, 0);
end;

function TCustomSciTextEditor.PositionRelative(pos: Integer; relative: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONRELATIVE, pos, relative);
end;

function TCustomSciTextEditor.PositionRelativeCodeUnits(pos: Integer; relative: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_POSITIONRELATIVECODEUNITS, pos, relative);
end;

procedure TCustomSciTextEditor.CopyRange(start: Integer; end: Integer);
begin
  SendMessage(Handle, SCI_COPYRANGE, start, end);
end;

procedure TCustomSciTextEditor.CopyText(length: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_COPYTEXT, length, LPARAM(text));
end;

procedure TCustomSciTextEditor.SetSelectionMode(selectionMode: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONMODE, selectionMode, 0);
end;

procedure TCustomSciTextEditor.ChangeSelectionMode(selectionMode: Integer);
begin
  SendMessage(Handle, SCI_CHANGESELECTIONMODE, selectionMode, 0);
end;

function TCustomSciTextEditor.GetSelectionMode(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONMODE, 0, 0);
end;

procedure TCustomSciTextEditor.SetMoveExtendsSelection(moveExtendsSelection: Boolean);
begin
  SendMessage(Handle, SCI_SETMOVEEXTENDSSELECTION, moveExtendsSelection, 0);
end;

function TCustomSciTextEditor.GetMoveExtendsSelection(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMOVEEXTENDSSELECTION, 0, 0);
end;

function TCustomSciTextEditor.GetLineSelStartPosition(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINESELSTARTPOSITION, line, 0);
end;

function TCustomSciTextEditor.GetLineSelEndPosition(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINESELENDPOSITION, line, 0);
end;

procedure TCustomSciTextEditor.LineDownRectExtend();
begin
  SendMessage(Handle, SCI_LINEDOWNRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineUpRectExtend();
begin
  SendMessage(Handle, SCI_LINEUPRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.CharLeftRectExtend();
begin
  SendMessage(Handle, SCI_CHARLEFTRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.CharRightRectExtend();
begin
  SendMessage(Handle, SCI_CHARRIGHTRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.HomeRectExtend();
begin
  SendMessage(Handle, SCI_HOMERECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeRectExtend();
begin
  SendMessage(Handle, SCI_VCHOMERECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndRectExtend();
begin
  SendMessage(Handle, SCI_LINEENDRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.PageUpRectExtend();
begin
  SendMessage(Handle, SCI_PAGEUPRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.PageDownRectExtend();
begin
  SendMessage(Handle, SCI_PAGEDOWNRECTEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageUp();
begin
  SendMessage(Handle, SCI_STUTTEREDPAGEUP, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageUpExtend();
begin
  SendMessage(Handle, SCI_STUTTEREDPAGEUPEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageDown();
begin
  SendMessage(Handle, SCI_STUTTEREDPAGEDOWN, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageDownExtend();
begin
  SendMessage(Handle, SCI_STUTTEREDPAGEDOWNEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeftEnd();
begin
  SendMessage(Handle, SCI_WORDLEFTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeftEndExtend();
begin
  SendMessage(Handle, SCI_WORDLEFTENDEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordRightEnd();
begin
  SendMessage(Handle, SCI_WORDRIGHTEND, 0, 0);
end;

procedure TCustomSciTextEditor.WordRightEndExtend();
begin
  SendMessage(Handle, SCI_WORDRIGHTENDEXTEND, 0, 0);
end;

procedure TCustomSciTextEditor.SetWhitespaceChars(characters: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETWHITESPACECHARS, 0, LPARAM(characters));
end;

function TCustomSciTextEditor.GetWhitespaceChars(characters: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETWHITESPACECHARS, 0, LPARAM(characters));
end;

procedure TCustomSciTextEditor.SetPunctuationChars(characters: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETPUNCTUATIONCHARS, 0, LPARAM(characters));
end;

function TCustomSciTextEditor.GetPunctuationChars(characters: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPUNCTUATIONCHARS, 0, LPARAM(characters));
end;

procedure TCustomSciTextEditor.SetCharsDefault();
begin
  SendMessage(Handle, SCI_SETCHARSDEFAULT, 0, 0);
end;

function TCustomSciTextEditor.AutoCGetCurrent(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETCURRENT, 0, 0);
end;

function TCustomSciTextEditor.AutoCGetCurrentText(text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETCURRENTTEXT, 0, LPARAM(text));
end;

procedure TCustomSciTextEditor.AutoCSetCaseInsensitiveBehaviour(behaviour: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETCASEINSENSITIVEBEHAVIOUR, behaviour, 0);
end;

function TCustomSciTextEditor.AutoCGetCaseInsensitiveBehaviour(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETCASEINSENSITIVEBEHAVIOUR, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetMulti(multi: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETMULTI, multi, 0);
end;

function TCustomSciTextEditor.AutoCGetMulti(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETMULTI, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetOrder(order: Integer);
begin
  SendMessage(Handle, SCI_AUTOCSETORDER, order, 0);
end;

function TCustomSciTextEditor.AutoCGetOrder(): Integer;
begin
  Result := SendMessage(Handle, SCI_AUTOCGETORDER, 0, 0);
end;

procedure TCustomSciTextEditor.Allocate(bytes: Integer);
begin
  SendMessage(Handle, SCI_ALLOCATE, bytes, 0);
end;

function TCustomSciTextEditor.TargetAsUTF8(s: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_TARGETASUTF8, 0, LPARAM(s));
end;

procedure TCustomSciTextEditor.SetLengthForEncode(bytes: Integer);
begin
  SendMessage(Handle, SCI_SETLENGTHFORENCODE, bytes, 0);
end;

function TCustomSciTextEditor.EncodedFromUTF8(utf8: PAnsiChar; encoded: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_ENCODEDFROMUTF8, utf8, LPARAM(encoded));
end;

function TCustomSciTextEditor.FindColumn(line: Integer; column: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_FINDCOLUMN, line, column);
end;

function TCustomSciTextEditor.GetCaretSticky(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETSTICKY, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretSticky(useCaretStickyBehaviour: Integer);
begin
  SendMessage(Handle, SCI_SETCARETSTICKY, useCaretStickyBehaviour, 0);
end;

procedure TCustomSciTextEditor.ToggleCaretSticky();
begin
  SendMessage(Handle, SCI_TOGGLECARETSTICKY, 0, 0);
end;

procedure TCustomSciTextEditor.SetPasteConvertEndings(convert: Boolean);
begin
  SendMessage(Handle, SCI_SETPASTECONVERTENDINGS, convert, 0);
end;

function TCustomSciTextEditor.GetPasteConvertEndings(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETPASTECONVERTENDINGS, 0, 0);
end;

procedure TCustomSciTextEditor.ReplaceRectangular(length: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_REPLACERECTANGULAR, length, LPARAM(text));
end;

procedure TCustomSciTextEditor.SelectionDuplicate();
begin
  SendMessage(Handle, SCI_SELECTIONDUPLICATE, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineBackAlpha(alpha: Integer);
begin
  SendMessage(Handle, SCI_SETCARETLINEBACKALPHA, alpha, 0);
end;

function TCustomSciTextEditor.GetCaretLineBackAlpha(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINEBACKALPHA, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretStyle(caretStyle: Integer);
begin
  SendMessage(Handle, SCI_SETCARETSTYLE, caretStyle, 0);
end;

function TCustomSciTextEditor.GetCaretStyle(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCARETSTYLE, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndicatorCurrent(indicator: Integer);
begin
  SendMessage(Handle, SCI_SETINDICATORCURRENT, indicator, 0);
end;

function TCustomSciTextEditor.GetIndicatorCurrent(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETINDICATORCURRENT, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndicatorValue(value: Integer);
begin
  SendMessage(Handle, SCI_SETINDICATORVALUE, value, 0);
end;

function TCustomSciTextEditor.GetIndicatorValue(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETINDICATORVALUE, 0, 0);
end;

procedure TCustomSciTextEditor.IndicatorFillRange(start: Integer; lengthFill: Integer);
begin
  SendMessage(Handle, SCI_INDICATORFILLRANGE, start, lengthFill);
end;

procedure TCustomSciTextEditor.IndicatorClearRange(start: Integer; lengthClear: Integer);
begin
  SendMessage(Handle, SCI_INDICATORCLEARRANGE, start, lengthClear);
end;

function TCustomSciTextEditor.IndicatorAllOnFor(pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICATORALLONFOR, pos, 0);
end;

function TCustomSciTextEditor.IndicatorValueAt(indicator: Integer; pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICATORVALUEAT, indicator, pos);
end;

function TCustomSciTextEditor.IndicatorStart(indicator: Integer; pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICATORSTART, indicator, pos);
end;

function TCustomSciTextEditor.IndicatorEnd(indicator: Integer; pos: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICATOREND, indicator, pos);
end;

procedure TCustomSciTextEditor.SetPositionCache(size: Integer);
begin
  SendMessage(Handle, SCI_SETPOSITIONCACHE, size, 0);
end;

function TCustomSciTextEditor.GetPositionCache(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPOSITIONCACHE, 0, 0);
end;

procedure TCustomSciTextEditor.SetLayoutThreads(threads: Integer);
begin
  SendMessage(Handle, SCI_SETLAYOUTTHREADS, threads, 0);
end;

function TCustomSciTextEditor.GetLayoutThreads(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLAYOUTTHREADS, 0, 0);
end;

procedure TCustomSciTextEditor.CopyAllowLine();
begin
  SendMessage(Handle, SCI_COPYALLOWLINE, 0, 0);
end;

procedure TCustomSciTextEditor.CutAllowLine();
begin
  SendMessage(Handle, SCI_CUTALLOWLINE, 0, 0);
end;

procedure TCustomSciTextEditor.SetCopySeparator(separator: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETCOPYSEPARATOR, 0, LPARAM(separator));
end;

function TCustomSciTextEditor.GetCopySeparator(separator: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETCOPYSEPARATOR, 0, LPARAM(separator));
end;

function TCustomSciTextEditor.GetCharacterPointer(): Pointer;
begin
  Result := SendMessage(Handle, SCI_GETCHARACTERPOINTER, 0, 0);
end;

function TCustomSciTextEditor.GetRangePointer(start: Integer; lengthRange: Integer): Pointer;
begin
  Result := SendMessage(Handle, SCI_GETRANGEPOINTER, start, lengthRange);
end;

function TCustomSciTextEditor.GetGapPosition(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETGAPPOSITION, 0, 0);
end;

procedure TCustomSciTextEditor.IndicSetAlpha(indicator: Integer; alpha: Integer);
begin
  SendMessage(Handle, SCI_INDICSETALPHA, indicator, alpha);
end;

function TCustomSciTextEditor.IndicGetAlpha(indicator: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICGETALPHA, indicator, 0);
end;

procedure TCustomSciTextEditor.IndicSetOutlineAlpha(indicator: Integer; alpha: Integer);
begin
  SendMessage(Handle, SCI_INDICSETOUTLINEALPHA, indicator, alpha);
end;

function TCustomSciTextEditor.IndicGetOutlineAlpha(indicator: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDICGETOUTLINEALPHA, indicator, 0);
end;

procedure TCustomSciTextEditor.SetExtraAscent(extraAscent: Integer);
begin
  SendMessage(Handle, SCI_SETEXTRAASCENT, extraAscent, 0);
end;

function TCustomSciTextEditor.GetExtraAscent(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETEXTRAASCENT, 0, 0);
end;

procedure TCustomSciTextEditor.SetExtraDescent(extraDescent: Integer);
begin
  SendMessage(Handle, SCI_SETEXTRADESCENT, extraDescent, 0);
end;

function TCustomSciTextEditor.GetExtraDescent(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETEXTRADESCENT, 0, 0);
end;

function TCustomSciTextEditor.MarkerSymbolDefined(markerNumber: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARKERSYMBOLDEFINED, markerNumber, 0);
end;

procedure TCustomSciTextEditor.MarginSetText(line: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_MARGINSETTEXT, line, LPARAM(text));
end;

function TCustomSciTextEditor.MarginGetText(line: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_MARGINGETTEXT, line, LPARAM(text));
end;

procedure TCustomSciTextEditor.MarginSetStyle(line: Integer; style: Integer);
begin
  SendMessage(Handle, SCI_MARGINSETSTYLE, line, style);
end;

function TCustomSciTextEditor.MarginGetStyle(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_MARGINGETSTYLE, line, 0);
end;

procedure TCustomSciTextEditor.MarginSetStyles(line: Integer; styles: PAnsiChar);
begin
  SendMessage(Handle, SCI_MARGINSETSTYLES, line, LPARAM(styles));
end;

function TCustomSciTextEditor.MarginGetStyles(line: Integer; styles: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_MARGINGETSTYLES, line, LPARAM(styles));
end;

procedure TCustomSciTextEditor.MarginTextClearAll();
begin
  SendMessage(Handle, SCI_MARGINTEXTCLEARALL, 0, 0);
end;

procedure TCustomSciTextEditor.MarginSetStyleOffset(style: Integer);
begin
  SendMessage(Handle, SCI_MARGINSETSTYLEOFFSET, style, 0);
end;

function TCustomSciTextEditor.MarginGetStyleOffset(): Integer;
begin
  Result := SendMessage(Handle, SCI_MARGINGETSTYLEOFFSET, 0, 0);
end;

procedure TCustomSciTextEditor.SetMarginOptions(marginOptions: Integer);
begin
  SendMessage(Handle, SCI_SETMARGINOPTIONS, marginOptions, 0);
end;

function TCustomSciTextEditor.GetMarginOptions(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMARGINOPTIONS, 0, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetText(line: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_ANNOTATIONSETTEXT, line, LPARAM(text));
end;

function TCustomSciTextEditor.AnnotationGetText(line: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_ANNOTATIONGETTEXT, line, LPARAM(text));
end;

procedure TCustomSciTextEditor.AnnotationSetStyle(line: Integer; style: Integer);
begin
  SendMessage(Handle, SCI_ANNOTATIONSETSTYLE, line, style);
end;

function TCustomSciTextEditor.AnnotationGetStyle(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_ANNOTATIONGETSTYLE, line, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetStyles(line: Integer; styles: PAnsiChar);
begin
  SendMessage(Handle, SCI_ANNOTATIONSETSTYLES, line, LPARAM(styles));
end;

function TCustomSciTextEditor.AnnotationGetStyles(line: Integer; styles: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_ANNOTATIONGETSTYLES, line, LPARAM(styles));
end;

function TCustomSciTextEditor.AnnotationGetLines(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_ANNOTATIONGETLINES, line, 0);
end;

procedure TCustomSciTextEditor.AnnotationClearAll();
begin
  SendMessage(Handle, SCI_ANNOTATIONCLEARALL, 0, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetVisible(visible: Integer);
begin
  SendMessage(Handle, SCI_ANNOTATIONSETVISIBLE, visible, 0);
end;

function TCustomSciTextEditor.AnnotationGetVisible(): Integer;
begin
  Result := SendMessage(Handle, SCI_ANNOTATIONGETVISIBLE, 0, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetStyleOffset(style: Integer);
begin
  SendMessage(Handle, SCI_ANNOTATIONSETSTYLEOFFSET, style, 0);
end;

function TCustomSciTextEditor.AnnotationGetStyleOffset(): Integer;
begin
  Result := SendMessage(Handle, SCI_ANNOTATIONGETSTYLEOFFSET, 0, 0);
end;

procedure TCustomSciTextEditor.ReleaseAllExtendedStyles();
begin
  SendMessage(Handle, SCI_RELEASEALLEXTENDEDSTYLES, 0, 0);
end;

function TCustomSciTextEditor.AllocateExtendedStyles(numberStyles: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_ALLOCATEEXTENDEDSTYLES, numberStyles, 0);
end;

procedure TCustomSciTextEditor.AddUndoAction(token: Integer; flags: Integer);
begin
  SendMessage(Handle, SCI_ADDUNDOACTION, token, flags);
end;

function TCustomSciTextEditor.CharPositionFromPoint(x: Integer; y: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_CHARPOSITIONFROMPOINT, x, y);
end;

function TCustomSciTextEditor.CharPositionFromPointClose(x: Integer; y: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_CHARPOSITIONFROMPOINTCLOSE, x, y);
end;

procedure TCustomSciTextEditor.SetMouseSelectionRectangularSwitch(mouseSelectionRectangularSwitch: Boolean);
begin
  SendMessage(Handle, SCI_SETMOUSESELECTIONRECTANGULARSWITCH, mouseSelectionRectangularSwitch, 0);
end;

function TCustomSciTextEditor.GetMouseSelectionRectangularSwitch(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMOUSESELECTIONRECTANGULARSWITCH, 0, 0);
end;

procedure TCustomSciTextEditor.SetMultipleSelection(multipleSelection: Boolean);
begin
  SendMessage(Handle, SCI_SETMULTIPLESELECTION, multipleSelection, 0);
end;

function TCustomSciTextEditor.GetMultipleSelection(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETMULTIPLESELECTION, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelectionTyping(additionalSelectionTyping: Boolean);
begin
  SendMessage(Handle, SCI_SETADDITIONALSELECTIONTYPING, additionalSelectionTyping, 0);
end;

function TCustomSciTextEditor.GetAdditionalSelectionTyping(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETADDITIONALSELECTIONTYPING, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalCaretsBlink(additionalCaretsBlink: Boolean);
begin
  SendMessage(Handle, SCI_SETADDITIONALCARETSBLINK, additionalCaretsBlink, 0);
end;

function TCustomSciTextEditor.GetAdditionalCaretsBlink(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETADDITIONALCARETSBLINK, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalCaretsVisible(additionalCaretsVisible: Boolean);
begin
  SendMessage(Handle, SCI_SETADDITIONALCARETSVISIBLE, additionalCaretsVisible, 0);
end;

function TCustomSciTextEditor.GetAdditionalCaretsVisible(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETADDITIONALCARETSVISIBLE, 0, 0);
end;

function TCustomSciTextEditor.GetSelections(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONS, 0, 0);
end;

function TCustomSciTextEditor.GetSelectionEmpty(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONEMPTY, 0, 0);
end;

procedure TCustomSciTextEditor.ClearSelections();
begin
  SendMessage(Handle, SCI_CLEARSELECTIONS, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelection(caret: Integer; anchor: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTION, caret, anchor);
end;

procedure TCustomSciTextEditor.AddSelection(caret: Integer; anchor: Integer);
begin
  SendMessage(Handle, SCI_ADDSELECTION, caret, anchor);
end;

function TCustomSciTextEditor.SelectionFromPoint(x: Integer; y: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_SELECTIONFROMPOINT, x, y);
end;

procedure TCustomSciTextEditor.DropSelectionN(selection: Integer);
begin
  SendMessage(Handle, SCI_DROPSELECTIONN, selection, 0);
end;

procedure TCustomSciTextEditor.SetMainSelection(selection: Integer);
begin
  SendMessage(Handle, SCI_SETMAINSELECTION, selection, 0);
end;

function TCustomSciTextEditor.GetMainSelection(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETMAINSELECTION, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNCaret(selection: Integer; caret: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONNCARET, selection, caret);
end;

function TCustomSciTextEditor.GetSelectionNCaret(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNCARET, selection, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNAnchor(selection: Integer; anchor: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONNANCHOR, selection, anchor);
end;

function TCustomSciTextEditor.GetSelectionNAnchor(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNANCHOR, selection, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNCaretVirtualSpace(selection: Integer; space: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONNCARETVIRTUALSPACE, selection, space);
end;

function TCustomSciTextEditor.GetSelectionNCaretVirtualSpace(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNCARETVIRTUALSPACE, selection, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNAnchorVirtualSpace(selection: Integer; space: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONNANCHORVIRTUALSPACE, selection, space);
end;

function TCustomSciTextEditor.GetSelectionNAnchorVirtualSpace(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNANCHORVIRTUALSPACE, selection, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNStart(selection: Integer; anchor: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONNSTART, selection, anchor);
end;

function TCustomSciTextEditor.GetSelectionNStart(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNSTART, selection, 0);
end;

function TCustomSciTextEditor.GetSelectionNStartVirtualSpace(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNSTARTVIRTUALSPACE, selection, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNEnd(selection: Integer; caret: Integer);
begin
  SendMessage(Handle, SCI_SETSELECTIONNEND, selection, caret);
end;

function TCustomSciTextEditor.GetSelectionNEndVirtualSpace(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNENDVIRTUALSPACE, selection, 0);
end;

function TCustomSciTextEditor.GetSelectionNEnd(selection: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSELECTIONNEND, selection, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionCaret(caret: Integer);
begin
  SendMessage(Handle, SCI_SETRECTANGULARSELECTIONCARET, caret, 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionCaret(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETRECTANGULARSELECTIONCARET, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionAnchor(anchor: Integer);
begin
  SendMessage(Handle, SCI_SETRECTANGULARSELECTIONANCHOR, anchor, 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionAnchor(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETRECTANGULARSELECTIONANCHOR, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionCaretVirtualSpace(space: Integer);
begin
  SendMessage(Handle, SCI_SETRECTANGULARSELECTIONCARETVIRTUALSPACE, space, 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionCaretVirtualSpace(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETRECTANGULARSELECTIONCARETVIRTUALSPACE, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionAnchorVirtualSpace(space: Integer);
begin
  SendMessage(Handle, SCI_SETRECTANGULARSELECTIONANCHORVIRTUALSPACE, space, 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionAnchorVirtualSpace(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETRECTANGULARSELECTIONANCHORVIRTUALSPACE, 0, 0);
end;

procedure TCustomSciTextEditor.SetVirtualSpaceOptions(virtualSpaceOptions: Integer);
begin
  SendMessage(Handle, SCI_SETVIRTUALSPACEOPTIONS, virtualSpaceOptions, 0);
end;

function TCustomSciTextEditor.GetVirtualSpaceOptions(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETVIRTUALSPACEOPTIONS, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionModifier(modifier: Integer);
begin
  SendMessage(Handle, SCI_SETRECTANGULARSELECTIONMODIFIER, modifier, 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionModifier(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETRECTANGULARSELECTIONMODIFIER, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelFore(fore: TColor);
begin
  SendMessage(Handle, SCI_SETADDITIONALSELFORE, fore, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelBack(back: TColor);
begin
  SendMessage(Handle, SCI_SETADDITIONALSELBACK, back, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelAlpha(alpha: Integer);
begin
  SendMessage(Handle, SCI_SETADDITIONALSELALPHA, alpha, 0);
end;

function TCustomSciTextEditor.GetAdditionalSelAlpha(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETADDITIONALSELALPHA, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalCaretFore(fore: TColor);
begin
  SendMessage(Handle, SCI_SETADDITIONALCARETFORE, fore, 0);
end;

function TCustomSciTextEditor.GetAdditionalCaretFore(): TColor;
begin
  Result := SendMessage(Handle, SCI_GETADDITIONALCARETFORE, 0, 0);
end;

procedure TCustomSciTextEditor.RotateSelection();
begin
  SendMessage(Handle, SCI_ROTATESELECTION, 0, 0);
end;

procedure TCustomSciTextEditor.SwapMainAnchorCaret();
begin
  SendMessage(Handle, SCI_SWAPMAINANCHORCARET, 0, 0);
end;

procedure TCustomSciTextEditor.MultipleSelectAddNext();
begin
  SendMessage(Handle, SCI_MULTIPLESELECTADDNEXT, 0, 0);
end;

procedure TCustomSciTextEditor.MultipleSelectAddEach();
begin
  SendMessage(Handle, SCI_MULTIPLESELECTADDEACH, 0, 0);
end;

function TCustomSciTextEditor.ChangeLexerState(start: Integer; end: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_CHANGELEXERSTATE, start, end);
end;

function TCustomSciTextEditor.ContractedFoldNext(lineStart: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_CONTRACTEDFOLDNEXT, lineStart, 0);
end;

procedure TCustomSciTextEditor.VerticalCentreCaret();
begin
  SendMessage(Handle, SCI_VERTICALCENTRECARET, 0, 0);
end;

procedure TCustomSciTextEditor.MoveSelectedLinesUp();
begin
  SendMessage(Handle, SCI_MOVESELECTEDLINESUP, 0, 0);
end;

procedure TCustomSciTextEditor.MoveSelectedLinesDown();
begin
  SendMessage(Handle, SCI_MOVESELECTEDLINESDOWN, 0, 0);
end;

procedure TCustomSciTextEditor.SetIdentifier(identifier: Integer);
begin
  SendMessage(Handle, SCI_SETIDENTIFIER, identifier, 0);
end;

function TCustomSciTextEditor.GetIdentifier(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETIDENTIFIER, 0, 0);
end;

procedure TCustomSciTextEditor.RGBAImageSetWidth(width: Integer);
begin
  SendMessage(Handle, SCI_RGBAIMAGESETWIDTH, width, 0);
end;

procedure TCustomSciTextEditor.RGBAImageSetHeight(height: Integer);
begin
  SendMessage(Handle, SCI_RGBAIMAGESETHEIGHT, height, 0);
end;

procedure TCustomSciTextEditor.RGBAImageSetScale(scalePercent: Integer);
begin
  SendMessage(Handle, SCI_RGBAIMAGESETSCALE, scalePercent, 0);
end;

procedure TCustomSciTextEditor.MarkerDefineRGBAImage(markerNumber: Integer; pixels: PAnsiChar);
begin
  SendMessage(Handle, SCI_MARKERDEFINERGBAIMAGE, markerNumber, LPARAM(pixels));
end;

procedure TCustomSciTextEditor.RegisterRGBAImage(type: Integer; pixels: PAnsiChar);
begin
  SendMessage(Handle, SCI_REGISTERRGBAIMAGE, type, LPARAM(pixels));
end;

procedure TCustomSciTextEditor.ScrollToStart();
begin
  SendMessage(Handle, SCI_SCROLLTOSTART, 0, 0);
end;

procedure TCustomSciTextEditor.ScrollToEnd();
begin
  SendMessage(Handle, SCI_SCROLLTOEND, 0, 0);
end;

procedure TCustomSciTextEditor.SetTechnology(technology: Integer);
begin
  SendMessage(Handle, SCI_SETTECHNOLOGY, technology, 0);
end;

function TCustomSciTextEditor.GetTechnology(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETTECHNOLOGY, 0, 0);
end;

function TCustomSciTextEditor.CreateLoader(bytes: Integer; documentOptions: Integer): Pointer;
begin
  Result := SendMessage(Handle, SCI_CREATELOADER, bytes, documentOptions);
end;

procedure TCustomSciTextEditor.FindIndicatorShow(start: Integer; end: Integer);
begin
  SendMessage(Handle, SCI_FINDINDICATORSHOW, start, end);
end;

procedure TCustomSciTextEditor.FindIndicatorFlash(start: Integer; end: Integer);
begin
  SendMessage(Handle, SCI_FINDINDICATORFLASH, start, end);
end;

procedure TCustomSciTextEditor.FindIndicatorHide();
begin
  SendMessage(Handle, SCI_FINDINDICATORHIDE, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeDisplay();
begin
  SendMessage(Handle, SCI_VCHOMEDISPLAY, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeDisplayExtend();
begin
  SendMessage(Handle, SCI_VCHOMEDISPLAYEXTEND, 0, 0);
end;

function TCustomSciTextEditor.GetCaretLineVisibleAlways(): Boolean;
begin
  Result := SendMessage(Handle, SCI_GETCARETLINEVISIBLEALWAYS, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineVisibleAlways(alwaysVisible: Boolean);
begin
  SendMessage(Handle, SCI_SETCARETLINEVISIBLEALWAYS, alwaysVisible, 0);
end;

procedure TCustomSciTextEditor.SetLineEndTypesAllowed(lineEndBitSet: Integer);
begin
  SendMessage(Handle, SCI_SETLINEENDTYPESALLOWED, lineEndBitSet, 0);
end;

function TCustomSciTextEditor.GetLineEndTypesAllowed(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINEENDTYPESALLOWED, 0, 0);
end;

function TCustomSciTextEditor.GetLineEndTypesActive(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINEENDTYPESACTIVE, 0, 0);
end;

procedure TCustomSciTextEditor.SetRepresentation(encodedCharacter: PAnsiChar; representation: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETREPRESENTATION, encodedCharacter, LPARAM(representation));
end;

function TCustomSciTextEditor.GetRepresentation(encodedCharacter: PAnsiChar; representation: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETREPRESENTATION, encodedCharacter, LPARAM(representation));
end;

procedure TCustomSciTextEditor.ClearRepresentation(encodedCharacter: PAnsiChar);
begin
  SendMessage(Handle, SCI_CLEARREPRESENTATION, encodedCharacter, 0);
end;

procedure TCustomSciTextEditor.ClearAllRepresentations();
begin
  SendMessage(Handle, SCI_CLEARALLREPRESENTATIONS, 0, 0);
end;

procedure TCustomSciTextEditor.SetRepresentationAppearance(encodedCharacter: PAnsiChar; appearance: Integer);
begin
  SendMessage(Handle, SCI_SETREPRESENTATIONAPPEARANCE, encodedCharacter, appearance);
end;

function TCustomSciTextEditor.GetRepresentationAppearance(encodedCharacter: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETREPRESENTATIONAPPEARANCE, encodedCharacter, 0);
end;

procedure TCustomSciTextEditor.SetRepresentationColour(encodedCharacter: PAnsiChar; colour: TColorAlpha);
begin
  SendMessage(Handle, SCI_SETREPRESENTATIONCOLOUR, encodedCharacter, colour);
end;

function TCustomSciTextEditor.GetRepresentationColour(encodedCharacter: PAnsiChar): TColorAlpha;
begin
  Result := SendMessage(Handle, SCI_GETREPRESENTATIONCOLOUR, encodedCharacter, 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationSetText(line: Integer; text: PAnsiChar);
begin
  SendMessage(Handle, SCI_EOLANNOTATIONSETTEXT, line, LPARAM(text));
end;

function TCustomSciTextEditor.EOLAnnotationGetText(line: Integer; text: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_EOLANNOTATIONGETTEXT, line, LPARAM(text));
end;

procedure TCustomSciTextEditor.EOLAnnotationSetStyle(line: Integer; style: Integer);
begin
  SendMessage(Handle, SCI_EOLANNOTATIONSETSTYLE, line, style);
end;

function TCustomSciTextEditor.EOLAnnotationGetStyle(line: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_EOLANNOTATIONGETSTYLE, line, 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationClearAll();
begin
  SendMessage(Handle, SCI_EOLANNOTATIONCLEARALL, 0, 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationSetVisible(visible: Integer);
begin
  SendMessage(Handle, SCI_EOLANNOTATIONSETVISIBLE, visible, 0);
end;

function TCustomSciTextEditor.EOLAnnotationGetVisible(): Integer;
begin
  Result := SendMessage(Handle, SCI_EOLANNOTATIONGETVISIBLE, 0, 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationSetStyleOffset(style: Integer);
begin
  SendMessage(Handle, SCI_EOLANNOTATIONSETSTYLEOFFSET, style, 0);
end;

function TCustomSciTextEditor.EOLAnnotationGetStyleOffset(): Integer;
begin
  Result := SendMessage(Handle, SCI_EOLANNOTATIONGETSTYLEOFFSET, 0, 0);
end;

function TCustomSciTextEditor.SupportsFeature(feature: Integer): Boolean;
begin
  Result := SendMessage(Handle, SCI_SUPPORTSFEATURE, feature, 0);
end;

function TCustomSciTextEditor.GetLineCharacterIndex(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINECHARACTERINDEX, 0, 0);
end;

procedure TCustomSciTextEditor.AllocateLineCharacterIndex(lineCharacterIndex: Integer);
begin
  SendMessage(Handle, SCI_ALLOCATELINECHARACTERINDEX, lineCharacterIndex, 0);
end;

procedure TCustomSciTextEditor.ReleaseLineCharacterIndex(lineCharacterIndex: Integer);
begin
  SendMessage(Handle, SCI_RELEASELINECHARACTERINDEX, lineCharacterIndex, 0);
end;

function TCustomSciTextEditor.LineFromIndexPosition(pos: Integer; lineCharacterIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_LINEFROMINDEXPOSITION, pos, lineCharacterIndex);
end;

function TCustomSciTextEditor.IndexPositionFromLine(line: Integer; lineCharacterIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_INDEXPOSITIONFROMLINE, line, lineCharacterIndex);
end;

procedure TCustomSciTextEditor.StartRecord();
begin
  SendMessage(Handle, SCI_STARTRECORD, 0, 0);
end;

procedure TCustomSciTextEditor.StopRecord();
begin
  SendMessage(Handle, SCI_STOPRECORD, 0, 0);
end;

function TCustomSciTextEditor.GetLexer(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLEXER, 0, 0);
end;

procedure TCustomSciTextEditor.Colourise(start: Integer; end: Integer);
begin
  SendMessage(Handle, SCI_COLOURISE, start, end);
end;

procedure TCustomSciTextEditor.SetProperty(key: PAnsiChar; value: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETPROPERTY, key, LPARAM(value));
end;

procedure TCustomSciTextEditor.SetKeyWords(keyWordSet: Integer; keyWords: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETKEYWORDS, keyWordSet, LPARAM(keyWords));
end;

function TCustomSciTextEditor.GetProperty(key: PAnsiChar; value: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPROPERTY, key, LPARAM(value));
end;

function TCustomSciTextEditor.GetPropertyExpanded(key: PAnsiChar; value: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPROPERTYEXPANDED, key, LPARAM(value));
end;

function TCustomSciTextEditor.GetPropertyInt(key: PAnsiChar; defaultValue: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPROPERTYINT, key, defaultValue);
end;

function TCustomSciTextEditor.GetLexerLanguage(language: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLEXERLANGUAGE, 0, LPARAM(language));
end;

function TCustomSciTextEditor.PrivateLexerCall(operation: Integer; pointer: Pointer): Pointer;
begin
  Result := SendMessage(Handle, SCI_PRIVATELEXERCALL, operation, LPARAM(pointer));
end;

function TCustomSciTextEditor.PropertyNames(names: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_PROPERTYNAMES, 0, LPARAM(names));
end;

function TCustomSciTextEditor.PropertyType(name: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_PROPERTYTYPE, name, 0);
end;

function TCustomSciTextEditor.DescribeProperty(name: PAnsiChar; description: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_DESCRIBEPROPERTY, name, LPARAM(description));
end;

function TCustomSciTextEditor.DescribeKeyWordSets(descriptions: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_DESCRIBEKEYWORDSETS, 0, LPARAM(descriptions));
end;

function TCustomSciTextEditor.GetLineEndTypesSupported(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETLINEENDTYPESSUPPORTED, 0, 0);
end;

function TCustomSciTextEditor.AllocateSubStyles(styleBase: Integer; numberStyles: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_ALLOCATESUBSTYLES, styleBase, numberStyles);
end;

function TCustomSciTextEditor.GetSubStylesStart(styleBase: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSUBSTYLESSTART, styleBase, 0);
end;

function TCustomSciTextEditor.GetSubStylesLength(styleBase: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSUBSTYLESLENGTH, styleBase, 0);
end;

function TCustomSciTextEditor.GetStyleFromSubStyle(subStyle: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSTYLEFROMSUBSTYLE, subStyle, 0);
end;

function TCustomSciTextEditor.GetPrimaryStyleFromStyle(style: Integer): Integer;
begin
  Result := SendMessage(Handle, SCI_GETPRIMARYSTYLEFROMSTYLE, style, 0);
end;

procedure TCustomSciTextEditor.FreeSubStyles();
begin
  SendMessage(Handle, SCI_FREESUBSTYLES, 0, 0);
end;

procedure TCustomSciTextEditor.SetIdentifiers(style: Integer; identifiers: PAnsiChar);
begin
  SendMessage(Handle, SCI_SETIDENTIFIERS, style, LPARAM(identifiers));
end;

function TCustomSciTextEditor.DistanceToSecondaryStyles(): Integer;
begin
  Result := SendMessage(Handle, SCI_DISTANCETOSECONDARYSTYLES, 0, 0);
end;

function TCustomSciTextEditor.GetSubStyleBases(styles: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_GETSUBSTYLEBASES, 0, LPARAM(styles));
end;

function TCustomSciTextEditor.GetNamedStyles(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETNAMEDSTYLES, 0, 0);
end;

function TCustomSciTextEditor.NameOfStyle(style: Integer; name: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_NAMEOFSTYLE, style, LPARAM(name));
end;

function TCustomSciTextEditor.TagsOfStyle(style: Integer; tags: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_TAGSOFSTYLE, style, LPARAM(tags));
end;

function TCustomSciTextEditor.DescriptionOfStyle(style: Integer; description: PAnsiChar): Integer;
begin
  Result := SendMessage(Handle, SCI_DESCRIPTIONOFSTYLE, style, LPARAM(description));
end;

procedure TCustomSciTextEditor.SetILexer(ilexer: Pointer);
begin
  SendMessage(Handle, SCI_SETILEXER, 0, LPARAM(ilexer));
end;

function TCustomSciTextEditor.GetBidirectional(): Integer;
begin
  Result := SendMessage(Handle, SCI_GETBIDIRECTIONAL, 0, 0);
end;

procedure TCustomSciTextEditor.SetBidirectional(bidirectional: Integer);
begin
  SendMessage(Handle, SCI_SETBIDIRECTIONAL, bidirectional, 0);
end;

end.
