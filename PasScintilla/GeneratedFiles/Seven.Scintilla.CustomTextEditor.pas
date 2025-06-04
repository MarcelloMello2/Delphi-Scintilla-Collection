unit Seven.Scintilla.CustomTextEditor;

{
  Wrapper Delphi para Scintilla
  Gerado automaticamente por Seven.Scintilla.IfaceGeneratorApp.exe
  Data: 04/06/2025 20:33:03
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Seven.Scintilla.SciTypes,
  Seven.Scintilla.BaseTextEditor;

const
  INVALID_POSITION = -1;
  SCI_START = 2000;
  SCI_OPTIONAL_START = 3000;
  SCI_LEXER_START = 4000;

  // WhiteSpace
  SCWS_INVISIBLE = 0;
  SCWS_VISIBLEALWAYS = 1;
  SCWS_VISIBLEAFTERINDENT = 2;
  SCWS_VISIBLEONLYININDENT = 3;

  // TabDrawMode
  SCTD_LONGARROW = 0;
  SCTD_STRIKEOUT = 1;

  // EndOfLine
  SC_EOL_CRLF = 0;
  SC_EOL_CR = 1;
  SC_EOL_LF = 2;
  SC_CP_UTF8 = 65001;

  // IMEInteraction
  SC_IME_WINDOWED = 0;
  SC_IME_INLINE = 1;

  // Alpha
  SC_ALPHA_TRANSPARENT = 0;
  SC_ALPHA_OPAQUE = 255;
  SC_ALPHA_NOALPHA = 256;

  // CursorShape
  SC_CURSORNORMAL = -1;
  SC_CURSORARROW = 2;
  SC_CURSORWAIT = 4;
  SC_CURSORREVERSEARROW = 7;

  // MarkerSymbol
  MARKER_MAX = 31;
  SC_MARK_CIRCLE = 0;
  SC_MARK_ROUNDRECT = 1;
  SC_MARK_ARROW = 2;
  SC_MARK_SMALLRECT = 3;
  SC_MARK_SHORTARROW = 4;
  SC_MARK_EMPTY = 5;
  SC_MARK_ARROWDOWN = 6;
  SC_MARK_MINUS = 7;
  SC_MARK_PLUS = 8;
  SC_MARK_VLINE = 9;
  SC_MARK_LCORNER = 10;
  SC_MARK_TCORNER = 11;
  SC_MARK_BOXPLUS = 12;
  SC_MARK_BOXPLUSCONNECTED = 13;
  SC_MARK_BOXMINUS = 14;
  SC_MARK_BOXMINUSCONNECTED = 15;
  SC_MARK_LCORNERCURVE = 16;
  SC_MARK_TCORNERCURVE = 17;
  SC_MARK_CIRCLEPLUS = 18;
  SC_MARK_CIRCLEPLUSCONNECTED = 19;
  SC_MARK_CIRCLEMINUS = 20;
  SC_MARK_CIRCLEMINUSCONNECTED = 21;
  SC_MARK_BACKGROUND = 22;
  SC_MARK_DOTDOTDOT = 23;
  SC_MARK_ARROWS = 24;
  SC_MARK_PIXMAP = 25;
  SC_MARK_FULLRECT = 26;
  SC_MARK_LEFTRECT = 27;
  SC_MARK_AVAILABLE = 28;
  SC_MARK_UNDERLINE = 29;
  SC_MARK_RGBAIMAGE = 30;
  SC_MARK_BOOKMARK = 31;
  SC_MARK_VERTICALBOOKMARK = 32;
  SC_MARK_BAR = 33;
  SC_MARK_CHARACTER = 10000;

  // MarkerOutline
  SC_MARKNUM_HISTORY_REVERTED_TO_ORIGIN = 21;
  SC_MARKNUM_HISTORY_SAVED = 22;
  SC_MARKNUM_HISTORY_MODIFIED = 23;
  SC_MARKNUM_HISTORY_REVERTED_TO_MODIFIED = 24;
  SC_MARKNUM_FOLDEREND = 25;
  SC_MARKNUM_FOLDEROPENMID = 26;
  SC_MARKNUM_FOLDERMIDTAIL = 27;
  SC_MARKNUM_FOLDERTAIL = 28;
  SC_MARKNUM_FOLDERSUB = 29;
  SC_MARKNUM_FOLDER = 30;
  SC_MARKNUM_FOLDEROPEN = 31;
  SC_MASK_HISTORY = $01E00000;
  SC_MASK_FOLDERS = $FE000000;
  SC_MAX_MARGIN = 4;

  // MarginType
  SC_MARGIN_SYMBOL = 0;
  SC_MARGIN_NUMBER = 1;
  SC_MARGIN_BACK = 2;
  SC_MARGIN_FORE = 3;
  SC_MARGIN_TEXT = 4;
  SC_MARGIN_RTEXT = 5;
  SC_MARGIN_COLOUR = 6;

  // StylesCommon
  STYLE_DEFAULT = 32;
  STYLE_LINENUMBER = 33;
  STYLE_BRACELIGHT = 34;
  STYLE_BRACEBAD = 35;
  STYLE_CONTROLCHAR = 36;
  STYLE_INDENTGUIDE = 37;
  STYLE_CALLTIP = 38;
  STYLE_FOLDDISPLAYTEXT = 39;
  STYLE_LASTPREDEFINED = 39;
  STYLE_MAX = 255;

  // CharacterSet
  SC_CHARSET_ANSI = 0;
  SC_CHARSET_DEFAULT = 1;
  SC_CHARSET_BALTIC = 186;
  SC_CHARSET_CHINESEBIG5 = 136;
  SC_CHARSET_EASTEUROPE = 238;
  SC_CHARSET_GB2312 = 134;
  SC_CHARSET_GREEK = 161;
  SC_CHARSET_HANGUL = 129;
  SC_CHARSET_MAC = 77;
  SC_CHARSET_OEM = 255;
  SC_CHARSET_RUSSIAN = 204;
  SC_CHARSET_OEM866 = 866;
  SC_CHARSET_CYRILLIC = 1251;
  SC_CHARSET_SHIFTJIS = 128;
  SC_CHARSET_SYMBOL = 2;
  SC_CHARSET_TURKISH = 162;
  SC_CHARSET_JOHAB = 130;
  SC_CHARSET_HEBREW = 177;
  SC_CHARSET_ARABIC = 178;
  SC_CHARSET_VIETNAMESE = 163;
  SC_CHARSET_THAI = 222;
  SC_CHARSET_8859_15 = 1000;

  // CaseVisible
  SC_CASE_MIXED = 0;
  SC_CASE_UPPER = 1;
  SC_CASE_LOWER = 2;
  SC_CASE_CAMEL = 3;
  SC_FONT_SIZE_MULTIPLIER = 100;

  // FontWeight
  SC_WEIGHT_NORMAL = 400;
  SC_WEIGHT_SEMIBOLD = 600;
  SC_WEIGHT_BOLD = 700;

  // FontStretch
  SC_STRETCH_ULTRA_CONDENSED = 1;
  SC_STRETCH_EXTRA_CONDENSED = 2;
  SC_STRETCH_CONDENSED = 3;
  SC_STRETCH_SEMI_CONDENSED = 4;
  SC_STRETCH_NORMAL = 5;
  SC_STRETCH_SEMI_EXPANDED = 6;
  SC_STRETCH_EXPANDED = 7;
  SC_STRETCH_EXTRA_EXPANDED = 8;
  SC_STRETCH_ULTRA_EXPANDED = 9;

  // Element
  SC_ELEMENT_LIST = 0;
  SC_ELEMENT_LIST_BACK = 1;
  SC_ELEMENT_LIST_SELECTED = 2;
  SC_ELEMENT_LIST_SELECTED_BACK = 3;
  SC_ELEMENT_SELECTION_TEXT = 10;
  SC_ELEMENT_SELECTION_BACK = 11;
  SC_ELEMENT_SELECTION_ADDITIONAL_TEXT = 12;
  SC_ELEMENT_SELECTION_ADDITIONAL_BACK = 13;
  SC_ELEMENT_SELECTION_SECONDARY_TEXT = 14;
  SC_ELEMENT_SELECTION_SECONDARY_BACK = 15;
  SC_ELEMENT_SELECTION_INACTIVE_TEXT = 16;
  SC_ELEMENT_SELECTION_INACTIVE_BACK = 17;
  SC_ELEMENT_SELECTION_INACTIVE_ADDITIONAL_TEXT = 18;
  SC_ELEMENT_SELECTION_INACTIVE_ADDITIONAL_BACK = 19;
  SC_ELEMENT_CARET = 40;
  SC_ELEMENT_CARET_ADDITIONAL = 41;
  SC_ELEMENT_CARET_LINE_BACK = 50;
  SC_ELEMENT_WHITE_SPACE = 60;
  SC_ELEMENT_WHITE_SPACE_BACK = 61;
  SC_ELEMENT_HOT_SPOT_ACTIVE = 70;
  SC_ELEMENT_HOT_SPOT_ACTIVE_BACK = 71;
  SC_ELEMENT_FOLD_LINE = 80;
  SC_ELEMENT_HIDDEN_LINE = 81;

  // Layer
  SC_LAYER_BASE = 0;
  SC_LAYER_UNDER_TEXT = 1;
  SC_LAYER_OVER_TEXT = 2;

  // IndicatorStyle
  INDIC_PLAIN = 0;
  INDIC_SQUIGGLE = 1;
  INDIC_TT = 2;
  INDIC_DIAGONAL = 3;
  INDIC_STRIKE = 4;
  INDIC_HIDDEN = 5;
  INDIC_BOX = 6;
  INDIC_ROUNDBOX = 7;
  INDIC_STRAIGHTBOX = 8;
  INDIC_DASH = 9;
  INDIC_DOTS = 10;
  INDIC_SQUIGGLELOW = 11;
  INDIC_DOTBOX = 12;
  INDIC_SQUIGGLEPIXMAP = 13;
  INDIC_COMPOSITIONTHICK = 14;
  INDIC_COMPOSITIONTHIN = 15;
  INDIC_FULLBOX = 16;
  INDIC_TEXTFORE = 17;
  INDIC_POINT = 18;
  INDIC_POINTCHARACTER = 19;
  INDIC_GRADIENT = 20;
  INDIC_GRADIENTCENTRE = 21;
  INDIC_POINT_TOP = 22;
  INDIC_CONTAINER = 8;
  INDIC_IME = 32;
  INDIC_IME_MAX = 35;
  INDIC_MAX = 35;

  // IndicatorNumbers
  INDICATOR_CONTAINER = 8;
  INDICATOR_IME = 32;
  INDICATOR_IME_MAX = 35;
  INDICATOR_HISTORY_REVERTED_TO_ORIGIN_INSERTION = 36;
  INDICATOR_HISTORY_REVERTED_TO_ORIGIN_DELETION = 37;
  INDICATOR_HISTORY_SAVED_INSERTION = 38;
  INDICATOR_HISTORY_SAVED_DELETION = 39;
  INDICATOR_HISTORY_MODIFIED_INSERTION = 40;
  INDICATOR_HISTORY_MODIFIED_DELETION = 41;
  INDICATOR_HISTORY_REVERTED_TO_MODIFIED_INSERTION = 42;
  INDICATOR_HISTORY_REVERTED_TO_MODIFIED_DELETION = 43;
  INDICATOR_MAX = 43;

  // IndicValue
  SC_INDICVALUEBIT = $1000000;
  SC_INDICVALUEMASK = $FFFFFF;

  // IndicFlag
  SC_INDICFLAG_NONE = 0;
  SC_INDICFLAG_VALUEFORE = 1;

  // AutoCompleteOption
  SC_AUTOCOMPLETE_NORMAL = 0;
  SC_AUTOCOMPLETE_FIXED_SIZE = 1;
  SC_AUTOCOMPLETE_SELECT_FIRST_ITEM = 2;

  // IndentView
  SC_IV_NONE = 0;
  SC_IV_REAL = 1;
  SC_IV_LOOKFORWARD = 2;
  SC_IV_LOOKBOTH = 3;

  // PrintOption
  SC_PRINT_NORMAL = 0;
  SC_PRINT_INVERTLIGHT = 1;
  SC_PRINT_BLACKONWHITE = 2;
  SC_PRINT_COLOURONWHITE = 3;
  SC_PRINT_COLOURONWHITEDEFAULTBG = 4;
  SC_PRINT_SCREENCOLOURS = 5;

  // FindOption
  SCFIND_NONE = $0;
  SCFIND_WHOLEWORD = $2;
  SCFIND_MATCHCASE = $4;
  SCFIND_WORDSTART = $00100000;
  SCFIND_REGEXP = $00200000;
  SCFIND_POSIX = $00400000;
  SCFIND_CXX11REGEX = $00800000;

  // ChangeHistoryOption
  SC_CHANGE_HISTORY_DISABLED = 0;
  SC_CHANGE_HISTORY_ENABLED = 1;
  SC_CHANGE_HISTORY_MARKERS = 2;
  SC_CHANGE_HISTORY_INDICATORS = 4;

  // UndoSelectionHistoryOption
  SC_UNDO_SELECTION_HISTORY_DISABLED = 0;
  SC_UNDO_SELECTION_HISTORY_ENABLED = 1;

  // FoldLevel
  SC_FOLDLEVELNONE = $0;
  SC_FOLDLEVELBASE = $400;
  SC_FOLDLEVELWHITEFLAG = $1000;
  SC_FOLDLEVELHEADERFLAG = $2000;
  SC_FOLDLEVELNUMBERMASK = $0FFF;

  // FoldDisplayTextStyle
  SC_FOLDDISPLAYTEXT_HIDDEN = 0;
  SC_FOLDDISPLAYTEXT_STANDARD = 1;
  SC_FOLDDISPLAYTEXT_BOXED = 2;

  // FoldAction
  SC_FOLDACTION_CONTRACT = 0;
  SC_FOLDACTION_EXPAND = 1;
  SC_FOLDACTION_TOGGLE = 2;
  SC_FOLDACTION_CONTRACT_EVERY_LEVEL = 4;

  // AutomaticFold
  SC_AUTOMATICFOLD_NONE = $0000;
  SC_AUTOMATICFOLD_SHOW = $0001;
  SC_AUTOMATICFOLD_CLICK = $0002;
  SC_AUTOMATICFOLD_CHANGE = $0004;

  // FoldFlag
  SC_FOLDFLAG_NONE = $0000;
  SC_FOLDFLAG_LINEBEFORE_EXPANDED = $0002;
  SC_FOLDFLAG_LINEBEFORE_CONTRACTED = $0004;
  SC_FOLDFLAG_LINEAFTER_EXPANDED = $0008;
  SC_FOLDFLAG_LINEAFTER_CONTRACTED = $0010;
  SC_FOLDFLAG_LEVELNUMBERS = $0040;
  SC_FOLDFLAG_LINESTATE = $0080;
  SC_TIME_FOREVER = 10000000;

  // IdleStyling
  SC_IDLESTYLING_NONE = 0;
  SC_IDLESTYLING_TOVISIBLE = 1;
  SC_IDLESTYLING_AFTERVISIBLE = 2;
  SC_IDLESTYLING_ALL = 3;

  // Wrap
  SC_WRAP_NONE = 0;
  SC_WRAP_WORD = 1;
  SC_WRAP_CHAR = 2;
  SC_WRAP_WHITESPACE = 3;

  // WrapVisualFlag
  SC_WRAPVISUALFLAG_NONE = $0000;
  SC_WRAPVISUALFLAG_END = $0001;
  SC_WRAPVISUALFLAG_START = $0002;
  SC_WRAPVISUALFLAG_MARGIN = $0004;

  // WrapVisualLocation
  SC_WRAPVISUALFLAGLOC_DEFAULT = $0000;
  SC_WRAPVISUALFLAGLOC_END_BY_TEXT = $0001;
  SC_WRAPVISUALFLAGLOC_START_BY_TEXT = $0002;

  // WrapIndentMode
  SC_WRAPINDENT_FIXED = 0;
  SC_WRAPINDENT_SAME = 1;
  SC_WRAPINDENT_INDENT = 2;
  SC_WRAPINDENT_DEEPINDENT = 3;

  // LineCache
  SC_CACHE_NONE = 0;
  SC_CACHE_CARET = 1;
  SC_CACHE_PAGE = 2;
  SC_CACHE_DOCUMENT = 3;

  // PhasesDraw
  SC_PHASES_ONE = 0;
  SC_PHASES_TWO = 1;
  SC_PHASES_MULTIPLE = 2;

  // FontQuality
  SC_EFF_QUALITY_MASK = $F;
  SC_EFF_QUALITY_DEFAULT = 0;
  SC_EFF_QUALITY_NON_ANTIALIASED = 1;
  SC_EFF_QUALITY_ANTIALIASED = 2;
  SC_EFF_QUALITY_LCD_OPTIMIZED = 3;

  // MultiPaste
  SC_MULTIPASTE_ONCE = 0;
  SC_MULTIPASTE_EACH = 1;

  // Accessibility
  SC_ACCESSIBILITY_DISABLED = 0;
  SC_ACCESSIBILITY_ENABLED = 1;

  // EdgeVisualStyle
  EDGE_NONE = 0;
  EDGE_LINE = 1;
  EDGE_BACKGROUND = 2;
  EDGE_MULTILINE = 3;

  // PopUp
  SC_POPUP_NEVER = 0;
  SC_POPUP_ALL = 1;
  SC_POPUP_TEXT = 2;

  // DocumentOption
  SC_DOCUMENTOPTION_DEFAULT = 0;
  SC_DOCUMENTOPTION_STYLES_NONE = $1;
  SC_DOCUMENTOPTION_TEXT_LARGE = $100;

  // Status
  SC_STATUS_OK = 0;
  SC_STATUS_FAILURE = 1;
  SC_STATUS_BADALLOC = 2;
  SC_STATUS_WARN_START = 1000;
  SC_STATUS_WARN_REGEX = 1001;

  // VisiblePolicy
  VISIBLE_SLOP = $01;
  VISIBLE_STRICT = $04;

  // CaretPolicy
  CARET_SLOP = $01;
  CARET_STRICT = $04;
  CARET_JUMPS = $10;
  CARET_EVEN = $08;

  // SelectionMode
  SC_SEL_STREAM = 0;
  SC_SEL_RECTANGLE = 1;
  SC_SEL_LINES = 2;
  SC_SEL_THIN = 3;

  // CaseInsensitiveBehaviour
  SC_CASEINSENSITIVEBEHAVIOUR_RESPECTCASE = 0;
  SC_CASEINSENSITIVEBEHAVIOUR_IGNORECASE = 1;

  // MultiAutoComplete
  SC_MULTIAUTOC_ONCE = 0;
  SC_MULTIAUTOC_EACH = 1;

  // Ordering
  SC_ORDER_PRESORTED = 0;
  SC_ORDER_PERFORMSORT = 1;
  SC_ORDER_CUSTOM = 2;

  // CaretSticky
  SC_CARETSTICKY_OFF = 0;
  SC_CARETSTICKY_ON = 1;
  SC_CARETSTICKY_WHITESPACE = 2;

  // CaretStyle
  CARETSTYLE_INVISIBLE = 0;
  CARETSTYLE_LINE = 1;
  CARETSTYLE_BLOCK = 2;
  CARETSTYLE_OVERSTRIKE_BAR = 0;
  CARETSTYLE_OVERSTRIKE_BLOCK = $10;
  CARETSTYLE_CURSES = $20;
  CARETSTYLE_INS_MASK = $F;
  CARETSTYLE_BLOCK_AFTER = $100;

  // MarginOption
  SC_MARGINOPTION_NONE = 0;
  SC_MARGINOPTION_SUBLINESELECT = 1;

  // AnnotationVisible
  ANNOTATION_HIDDEN = 0;
  ANNOTATION_STANDARD = 1;
  ANNOTATION_BOXED = 2;
  ANNOTATION_INDENTED = 3;

  // UndoFlags
  UNDO_NONE = 0;
  UNDO_MAY_COALESCE = 1;

  // VirtualSpace
  SCVS_NONE = 0;
  SCVS_RECTANGULARSELECTION = 1;
  SCVS_USERACCESSIBLE = 2;
  SCVS_NOWRAPLINESTART = 4;

  // Technology
  SC_TECHNOLOGY_DEFAULT = 0;
  SC_TECHNOLOGY_DIRECTWRITE = 1;
  SC_TECHNOLOGY_DIRECTWRITERETAIN = 2;
  SC_TECHNOLOGY_DIRECTWRITEDC = 3;
  SC_TECHNOLOGY_DIRECT_WRITE_1 = 4;

  // LineEndType
  SC_LINE_END_TYPE_DEFAULT = 0;
  SC_LINE_END_TYPE_UNICODE = 1;

  // RepresentationAppearance
  SC_REPRESENTATION_PLAIN = 0;
  SC_REPRESENTATION_BLOB = 1;
  SC_REPRESENTATION_COLOUR = $10;

  // EOLAnnotationVisible
  EOLANNOTATION_HIDDEN = $0;
  EOLANNOTATION_STANDARD = $1;
  EOLANNOTATION_BOXED = $2;
  EOLANNOTATION_STADIUM = $100;
  EOLANNOTATION_FLAT_CIRCLE = $101;
  EOLANNOTATION_ANGLE_CIRCLE = $102;
  EOLANNOTATION_CIRCLE_FLAT = $110;
  EOLANNOTATION_FLATS = $111;
  EOLANNOTATION_ANGLE_FLAT = $112;
  EOLANNOTATION_CIRCLE_ANGLE = $120;
  EOLANNOTATION_FLAT_ANGLE = $121;
  EOLANNOTATION_ANGLES = $122;

  // Supports
  SC_SUPPORTS_LINE_DRAWS_FINAL = 0;
  SC_SUPPORTS_PIXEL_DIVISIONS = 1;
  SC_SUPPORTS_FRACTIONAL_STROKE_WIDTH = 2;
  SC_SUPPORTS_TRANSLUCENT_STROKE = 3;
  SC_SUPPORTS_PIXEL_MODIFICATION = 4;
  SC_SUPPORTS_THREAD_SAFE_MEASURE_WIDTHS = 5;

  // LineCharacterIndexType
  SC_LINECHARACTERINDEX_NONE = 0;
  SC_LINECHARACTERINDEX_UTF32 = 1;
  SC_LINECHARACTERINDEX_UTF16 = 2;
  KEYWORDSET_MAX = 8;

  // TypeProperty
  SC_TYPE_BOOLEAN = 0;
  SC_TYPE_INTEGER = 1;
  SC_TYPE_STRING = 2;

  // ModificationFlags
  SC_MOD_NONE = $0;
  SC_MOD_INSERTTEXT = $1;
  SC_MOD_DELETETEXT = $2;
  SC_MOD_CHANGESTYLE = $4;
  SC_MOD_CHANGEFOLD = $8;
  SC_PERFORMED_USER = $10;
  SC_PERFORMED_UNDO = $20;
  SC_PERFORMED_REDO = $40;
  SC_MULTISTEPUNDOREDO = $80;
  SC_LASTSTEPINUNDOREDO = $100;
  SC_MOD_CHANGEMARKER = $200;
  SC_MOD_BEFOREINSERT = $400;
  SC_MOD_BEFOREDELETE = $800;
  SC_MULTILINEUNDOREDO = $1000;
  SC_STARTACTION = $2000;
  SC_MOD_CHANGEINDICATOR = $4000;
  SC_MOD_CHANGELINESTATE = $8000;
  SC_MOD_CHANGEMARGIN = $10000;
  SC_MOD_CHANGEANNOTATION = $20000;
  SC_MOD_CONTAINER = $40000;
  SC_MOD_LEXERSTATE = $80000;
  SC_MOD_INSERTCHECK = $100000;
  SC_MOD_CHANGETABSTOPS = $200000;
  SC_MOD_CHANGEEOLANNOTATION = $400000;
  SC_MODEVENTMASKALL = $7FFFFF;

  // Update
  SC_UPDATE_NONE = $0;
  SC_UPDATE_CONTENT = $1;
  SC_UPDATE_SELECTION = $2;
  SC_UPDATE_V_SCROLL = $4;
  SC_UPDATE_H_SCROLL = $8;

  // FocusChange
  SCEN_CHANGE = 768;
  SCEN_SETFOCUS = 512;
  SCEN_KILLFOCUS = 256;

  // Keys
  SCK_DOWN = 300;
  SCK_UP = 301;
  SCK_LEFT = 302;
  SCK_RIGHT = 303;
  SCK_HOME = 304;
  SCK_END = 305;
  SCK_PRIOR = 306;
  SCK_NEXT = 307;
  SCK_DELETE = 308;
  SCK_INSERT = 309;
  SCK_ESCAPE = 7;
  SCK_BACK = 8;
  SCK_TAB = 9;
  SCK_RETURN = 13;
  SCK_ADD = 310;
  SCK_SUBTRACT = 311;
  SCK_DIVIDE = 312;
  SCK_WIN = 313;
  SCK_RWIN = 314;
  SCK_MENU = 315;

  // KeyMod
  SCMOD_NORM = 0;
  SCMOD_SHIFT = 1;
  SCMOD_CTRL = 2;
  SCMOD_ALT = 4;
  SCMOD_SUPER = 8;
  SCMOD_META = 16;

  // CompletionMethods
  SC_AC_FILLUP = 1;
  SC_AC_DOUBLECLICK = 2;
  SC_AC_TAB = 3;
  SC_AC_NEWLINE = 4;
  SC_AC_COMMAND = 5;
  SC_AC_SINGLE_CHOICE = 6;

  // CharacterSource
  SC_CHARACTERSOURCE_DIRECT_INPUT = 0;
  SC_CHARACTERSOURCE_TENTATIVE_INPUT = 1;
  SC_CHARACTERSOURCE_IME_RESULT = 2;

  // Bidirectional
  SC_BIDIRECTIONAL_DISABLED = 0;
  SC_BIDIRECTIONAL_L2R = 1;
  SC_BIDIRECTIONAL_R2L = 2;
  INDIC0_MASK = $20;
  INDIC1_MASK = $40;
  INDIC2_MASK = $80;
  INDICS_MASK = $E0;


type
  // Tipos auxiliares

  TSciCharRange = Record
    cpMin : Longint;
	  cpMax : Longint;
  end;

  PSciTextRange = ^TSciTextRange;
  TSciTextRange = record
    chrg: TSciCharRange;
    lpstrText: PAnsiChar;
  end;

  PSciFindText = ^TSciFindText;
  TSciFindText = record
    chrg: TSciCharRange;
    lpstrText: PAnsiChar;
  end;

  PSciFormatRange = ^TSciFormatRange;
  TSciFormatRange = record
    hdc: HDC;
    hdcTarget: HDC;
    rc: TRect;
    rcPage: TRect;
    chrg: TSciCharRange;
  end;

  // Tipos de eventos
  TSciCharEvent = procedure(Sender: TObject; Ch: AnsiChar) of object;
  TSciPositionEvent = procedure(Sender: TObject; Position: Integer) of object;

  TCustomSciTextEditor = class(TBaseSciTextEditor)
  private
    // Eventos
    FOnStyleNeeded: TSciPositionEvent;
    FOnCharAdded: TSciCharEvent;
    FOnModifyAttemptRO: TNotifyEvent;

  protected
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  public
    constructor Create(AOwner: TComponent); override;

    // Basics
    procedure AddText(ALength: Integer; AText: PAnsiChar);
    procedure AddStyledText(ALength: Integer; C: PAnsiChar);
    procedure InsertText(APos: Integer; AText: PAnsiChar);
    procedure ChangeInsertion(ALength: Integer; AText: PAnsiChar);
    procedure ClearAll();
    procedure DeleteRange(AStart: Integer; ALengthDelete: Integer);
    procedure ClearDocumentStyle();
    function GetLength(): Integer;
    function GetCharAt(APos: Integer): Integer;
    function GetCurrentPos(): Integer;
    function GetAnchor(): Integer;
    function GetStyleAt(APos: Integer): Integer;
    function GetStyleIndexAt(APos: Integer): Integer;
    procedure Redo();
    procedure SetUndoCollection(ACollectUndo: Boolean);
    procedure SelectAll();
    procedure SetSavePoint();
    function GetStyledText(ATr: PSciTextRange): Integer;
    function GetStyledTextFull(ATr: Integer): Integer;
    function CanRedo(): Boolean;
    function MarkerLineFromHandle(AMarkerHandle: Integer): Integer;
    procedure MarkerDeleteHandle(AMarkerHandle: Integer);
    function MarkerHandleFromLine(ALine: Integer; AWhich: Integer): Integer;
    function MarkerNumberFromLine(ALine: Integer; AWhich: Integer): Integer;
    function GetUndoCollection(): Boolean;
    function GetViewWS(): Integer;
    procedure SetViewWS(AViewWS: Integer);
    function GetTabDrawMode(): Integer;
    procedure SetTabDrawMode(ATabDrawMode: Integer);
    function PositionFromPoint(X: Integer; Y: Integer): Integer;
    function PositionFromPointClose(X: Integer; Y: Integer): Integer;
    procedure GotoLine(ALine: Integer);
    procedure GotoPos(ACaret: Integer);
    procedure SetAnchor(AAnchor: Integer);
    function GetCurLine(ALength: Integer; AText: PAnsiChar): Integer;
    function GetEndStyled(): Integer;
    procedure ConvertEOLs(AEolMode: Integer);
    function GetEOLMode(): Integer;
    procedure SetEOLMode(AEolMode: Integer);
    procedure StartStyling(AStart: Integer; AUnused: Integer);
    procedure SetStyling(ALength: Integer; AStyle: Integer);
    function GetBufferedDraw(): Boolean;
    procedure SetBufferedDraw(ABuffered: Boolean);
    procedure SetTabWidth(ATabWidth: Integer);
    function GetTabWidth(): Integer;
    procedure SetTabMinimumWidth(APixels: Integer);
    function GetTabMinimumWidth(): Integer;
    procedure ClearTabStops(ALine: Integer);
    procedure AddTabStop(ALine: Integer; X: Integer);
    function GetNextTabStop(ALine: Integer; X: Integer): Integer;
    procedure SetCodePage(ACodePage: Integer);
    procedure SetFontLocale(ALocaleName: PAnsiChar);
    function GetFontLocale(ALocaleName: PAnsiChar): Integer;
    function GetIMEInteraction(): Integer;
    procedure SetIMEInteraction(AImeInteraction: Integer);
    procedure MarkerDefine(AMarkerNumber: Integer; AMarkerSymbol: Integer);
    procedure MarkerSetFore(AMarkerNumber: Integer; AFore: TColor);
    procedure MarkerSetBack(AMarkerNumber: Integer; ABack: TColor);
    procedure MarkerSetBackSelected(AMarkerNumber: Integer; ABack: TColor);
    procedure MarkerSetForeTranslucent(AMarkerNumber: Integer; AFore: Integer);
    procedure MarkerSetBackTranslucent(AMarkerNumber: Integer; ABack: Integer);
    procedure MarkerSetBackSelectedTranslucent(AMarkerNumber: Integer; ABack: Integer);
    procedure MarkerSetStrokeWidth(AMarkerNumber: Integer; AHundredths: Integer);
    procedure MarkerEnableHighlight(AEnabled: Boolean);
    function MarkerAdd(ALine: Integer; AMarkerNumber: Integer): Integer;
    procedure MarkerDelete(ALine: Integer; AMarkerNumber: Integer);
    procedure MarkerDeleteAll(AMarkerNumber: Integer);
    function MarkerGet(ALine: Integer): Integer;
    function MarkerNext(ALineStart: Integer; AMarkerMask: Integer): Integer;
    function MarkerPrevious(ALineStart: Integer; AMarkerMask: Integer): Integer;
    procedure MarkerDefinePixmap(AMarkerNumber: Integer; APixmap: PAnsiChar);
    procedure MarkerAddSet(ALine: Integer; AMarkerSet: Integer);
    procedure MarkerSetAlpha(AMarkerNumber: Integer; AAlpha: Integer);
    function MarkerGetLayer(AMarkerNumber: Integer): Integer;
    procedure MarkerSetLayer(AMarkerNumber: Integer; ALayer: Integer);
    procedure SetMarginTypeN(AMargin: Integer; AMarginType: Integer);
    function GetMarginTypeN(AMargin: Integer): Integer;
    procedure SetMarginWidthN(AMargin: Integer; APixelWidth: Integer);
    function GetMarginWidthN(AMargin: Integer): Integer;
    procedure SetMarginMaskN(AMargin: Integer; AMask: Integer);
    function GetMarginMaskN(AMargin: Integer): Integer;
    procedure SetMarginSensitiveN(AMargin: Integer; ASensitive: Boolean);
    function GetMarginSensitiveN(AMargin: Integer): Boolean;
    procedure SetMarginCursorN(AMargin: Integer; ACursor: Integer);
    function GetMarginCursorN(AMargin: Integer): Integer;
    procedure SetMarginBackN(AMargin: Integer; ABack: TColor);
    function GetMarginBackN(AMargin: Integer): TColor;
    procedure SetMargins(AMargins: Integer);
    function GetMargins(): Integer;
    procedure StyleClearAll();
    procedure StyleSetFore(AStyle: Integer; AFore: TColor);
    procedure StyleSetBack(AStyle: Integer; ABack: TColor);
    procedure StyleSetBold(AStyle: Integer; ABold: Boolean);
    procedure StyleSetItalic(AStyle: Integer; AItalic: Boolean);
    procedure StyleSetSize(AStyle: Integer; ASizePoints: Integer);
    procedure StyleSetFont(AStyle: Integer; AFontName: PAnsiChar);
    procedure StyleSetEOLFilled(AStyle: Integer; AEolFilled: Boolean);
    procedure StyleResetDefault();
    procedure StyleSetUnderline(AStyle: Integer; AUnderline: Boolean);
    function StyleGetFore(AStyle: Integer): TColor;
    function StyleGetBack(AStyle: Integer): TColor;
    function StyleGetBold(AStyle: Integer): Boolean;
    function StyleGetItalic(AStyle: Integer): Boolean;
    function StyleGetSize(AStyle: Integer): Integer;
    function StyleGetFont(AStyle: Integer; AFontName: PAnsiChar): Integer;
    function StyleGetEOLFilled(AStyle: Integer): Boolean;
    function StyleGetUnderline(AStyle: Integer): Boolean;
    function StyleGetCase(AStyle: Integer): Integer;
    function StyleGetCharacterSet(AStyle: Integer): Integer;
    function StyleGetVisible(AStyle: Integer): Boolean;
    function StyleGetChangeable(AStyle: Integer): Boolean;
    function StyleGetHotSpot(AStyle: Integer): Boolean;
    procedure StyleSetCase(AStyle: Integer; ACaseVisible: Integer);
    procedure StyleSetSizeFractional(AStyle: Integer; ASizeHundredthPoints: Integer);
    function StyleGetSizeFractional(AStyle: Integer): Integer;
    procedure StyleSetWeight(AStyle: Integer; AWeight: Integer);
    function StyleGetWeight(AStyle: Integer): Integer;
    procedure StyleSetCharacterSet(AStyle: Integer; ACharacterSet: Integer);
    procedure StyleSetHotSpot(AStyle: Integer; AHotspot: Boolean);
    procedure StyleSetCheckMonospaced(AStyle: Integer; ACheckMonospaced: Boolean);
    function StyleGetCheckMonospaced(AStyle: Integer): Boolean;
    procedure StyleSetStretch(AStyle: Integer; AStretch: Integer);
    function StyleGetStretch(AStyle: Integer): Integer;
    procedure StyleSetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar);
    function StyleGetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar): Integer;
    procedure SetElementColour(AElement: Integer; AColourElement: Integer);
    function GetElementColour(AElement: Integer): Integer;
    procedure ResetElementColour(AElement: Integer);
    function GetElementIsSet(AElement: Integer): Boolean;
    function GetElementAllowsTranslucent(AElement: Integer): Boolean;
    function GetElementBaseColour(AElement: Integer): Integer;
    procedure SetSelFore(AUseSetting: Boolean; AFore: TColor);
    procedure SetSelBack(AUseSetting: Boolean; ABack: TColor);
    function GetSelAlpha(): Integer;
    procedure SetSelAlpha(AAlpha: Integer);
    function GetSelEOLFilled(): Boolean;
    procedure SetSelEOLFilled(AFilled: Boolean);
    function GetSelectionLayer(): Integer;
    procedure SetSelectionLayer(ALayer: Integer);
    function GetCaretLineLayer(): Integer;
    procedure SetCaretLineLayer(ALayer: Integer);
    function GetCaretLineHighlightSubLine(): Boolean;
    procedure SetCaretLineHighlightSubLine(ASubLine: Boolean);
    procedure SetCaretFore(AFore: TColor);
    procedure AssignCmdKey(AKeyDefinition: Integer; ASciCommand: Integer);
    procedure ClearCmdKey(AKeyDefinition: Integer);
    procedure ClearAllCmdKeys();
    procedure SetStylingEx(ALength: Integer; AStyles: PAnsiChar);
    procedure StyleSetVisible(AStyle: Integer; AVisible: Boolean);
    function GetCaretPeriod(): Integer;
    procedure SetCaretPeriod(APeriodMilliseconds: Integer);
    procedure SetWordChars(ACharacters: PAnsiChar);
    function GetWordChars(ACharacters: PAnsiChar): Integer;
    procedure SetCharacterCategoryOptimization(ACountCharacters: Integer);
    function GetCharacterCategoryOptimization(): Integer;
    procedure BeginUndoAction();
    procedure EndUndoAction();
    function GetUndoSequence(): Integer;
    function GetUndoActions(): Integer;
    procedure SetUndoSavePoint(AAction: Integer);
    function GetUndoSavePoint(): Integer;
    procedure SetUndoDetach(AAction: Integer);
    function GetUndoDetach(): Integer;
    procedure SetUndoTentative(AAction: Integer);
    function GetUndoTentative(): Integer;
    procedure SetUndoCurrent(AAction: Integer);
    function GetUndoCurrent(): Integer;
    procedure PushUndoActionType(AType: Integer; APos: Integer);
    procedure ChangeLastUndoActionText(ALength: Integer; AText: PAnsiChar);
    function GetUndoActionType(AAction: Integer): Integer;
    function GetUndoActionPosition(AAction: Integer): Integer;
    function GetUndoActionText(AAction: Integer; AText: PAnsiChar): Integer;
    procedure IndicSetStyle(AIndicator: Integer; AIndicatorStyle: Integer);
    function IndicGetStyle(AIndicator: Integer): Integer;
    procedure IndicSetFore(AIndicator: Integer; AFore: TColor);
    function IndicGetFore(AIndicator: Integer): TColor;
    procedure IndicSetUnder(AIndicator: Integer; AUnder: Boolean);
    function IndicGetUnder(AIndicator: Integer): Boolean;
    procedure IndicSetHoverStyle(AIndicator: Integer; AIndicatorStyle: Integer);
    function IndicGetHoverStyle(AIndicator: Integer): Integer;
    procedure IndicSetHoverFore(AIndicator: Integer; AFore: TColor);
    function IndicGetHoverFore(AIndicator: Integer): TColor;
    procedure IndicSetFlags(AIndicator: Integer; AFlags: Integer);
    function IndicGetFlags(AIndicator: Integer): Integer;
    procedure IndicSetStrokeWidth(AIndicator: Integer; AHundredths: Integer);
    function IndicGetStrokeWidth(AIndicator: Integer): Integer;
    procedure SetWhitespaceFore(AUseSetting: Boolean; AFore: TColor);
    procedure SetWhitespaceBack(AUseSetting: Boolean; ABack: TColor);
    procedure SetWhitespaceSize(ASize: Integer);
    function GetWhitespaceSize(): Integer;
    procedure SetLineState(ALine: Integer; AState: Integer);
    function GetLineState(ALine: Integer): Integer;
    function GetMaxLineState(): Integer;
    function GetCaretLineVisible(): Boolean;
    procedure SetCaretLineVisible(AShow: Boolean);
    function GetCaretLineBack(): TColor;
    procedure SetCaretLineBack(ABack: TColor);
    function GetCaretLineFrame(): Integer;
    procedure SetCaretLineFrame(AWidth: Integer);
    procedure StyleSetChangeable(AStyle: Integer; AChangeable: Boolean);
    procedure AutoCShow(ALengthEntered: Integer; AItemList: PAnsiChar);
    procedure AutoCCancel();
    function AutoCActive(): Boolean;
    function AutoCPosStart(): Integer;
    procedure AutoCComplete();
    procedure AutoCStops(ACharacterSet: PAnsiChar);
    procedure AutoCSetSeparator(ASeparatorCharacter: Integer);
    function AutoCGetSeparator(): Integer;
    procedure AutoCSelect(ASelect: PAnsiChar);
    procedure AutoCSetCancelAtStart(ACancel: Boolean);
    function AutoCGetCancelAtStart(): Boolean;
    procedure AutoCSetFillUps(ACharacterSet: PAnsiChar);
    procedure AutoCSetChooseSingle(AChooseSingle: Boolean);
    function AutoCGetChooseSingle(): Boolean;
    procedure AutoCSetIgnoreCase(AIgnoreCase: Boolean);
    function AutoCGetIgnoreCase(): Boolean;
    procedure UserListShow(AListType: Integer; AItemList: PAnsiChar);
    procedure AutoCSetAutoHide(AAutoHide: Boolean);
    function AutoCGetAutoHide(): Boolean;
    procedure AutoCSetOptions(AOptions: Integer);
    function AutoCGetOptions(): Integer;
    procedure AutoCSetDropRestOfWord(ADropRestOfWord: Boolean);
    function AutoCGetDropRestOfWord(): Boolean;
    procedure RegisterImage(AType: Integer; AXpmData: PAnsiChar);
    procedure ClearRegisteredImages();
    function AutoCGetTypeSeparator(): Integer;
    procedure AutoCSetTypeSeparator(ASeparatorCharacter: Integer);
    procedure AutoCSetMaxWidth(ACharacterCount: Integer);
    function AutoCGetMaxWidth(): Integer;
    procedure AutoCSetMaxHeight(ARowCount: Integer);
    function AutoCGetMaxHeight(): Integer;
    procedure AutoCSetStyle(AStyle: Integer);
    function AutoCGetStyle(): Integer;
    procedure SetIndent(AIndentSize: Integer);
    function GetIndent(): Integer;
    procedure SetUseTabs(AUseTabs: Boolean);
    function GetUseTabs(): Boolean;
    procedure SetLineIndentation(ALine: Integer; AIndentation: Integer);
    function GetLineIndentation(ALine: Integer): Integer;
    function GetLineIndentPosition(ALine: Integer): Integer;
    function GetColumn(APos: Integer): Integer;
    function CountCharacters(AStart: Integer; AEnd: Integer): Integer;
    function CountCodeUnits(AStart: Integer; AEnd: Integer): Integer;
    procedure SetHScrollBar(AVisible: Boolean);
    function GetHScrollBar(): Boolean;
    procedure SetIndentationGuides(AIndentView: Integer);
    function GetIndentationGuides(): Integer;
    procedure SetHighlightGuide(AColumn: Integer);
    function GetHighlightGuide(): Integer;
    function GetLineEndPosition(ALine: Integer): Integer;
    function GetCodePage(): Integer;
    function GetCaretFore(): TColor;
    function GetReadOnly(): Boolean;
    procedure SetCurrentPos(ACaret: Integer);
    procedure SetSelectionStart(AAnchor: Integer);
    function GetSelectionStart(): Integer;
    procedure SetSelectionEnd(ACaret: Integer);
    function GetSelectionEnd(): Integer;
    procedure SetEmptySelection(ACaret: Integer);
    procedure SetPrintMagnification(AMagnification: Integer);
    function GetPrintMagnification(): Integer;
    procedure SetPrintColourMode(AMode: Integer);
    function GetPrintColourMode(): Integer;
    function FindText(ASearchFlags: Integer; AFt: PSciFindText): Integer;
    function FindTextFull(ASearchFlags: Integer; AFt: Integer): Integer;
    function FormatRange(ADraw: Boolean; AFr: PSciFormatRange): Integer;
    function FormatRangeFull(ADraw: Boolean; AFr: Integer): Integer;
    procedure SetChangeHistory(AChangeHistory: Integer);
    function GetChangeHistory(): Integer;
    procedure SetUndoSelectionHistory(AUndoSelectionHistory: Integer);
    function GetUndoSelectionHistory(): Integer;
    procedure SetSelectionSerialized(ASelectionString: PAnsiChar);
    function GetSelectionSerialized(ASelectionString: PAnsiChar): Integer;
    function GetFirstVisibleLine(): Integer;
    function GetLine(ALine: Integer; AText: PAnsiChar): Integer;
    function GetLineCount(): Integer;
    procedure AllocateLines(ALines: Integer);
    procedure SetMarginLeft(APixelWidth: Integer);
    function GetMarginLeft(): Integer;
    procedure SetMarginRight(APixelWidth: Integer);
    function GetMarginRight(): Integer;
    function GetModify(): Boolean;
    procedure SetSel(AAnchor: Integer; ACaret: Integer);
    function GetSelText(AText: PAnsiChar): Integer;
    function GetTextRange(ATr: PSciTextRange): Integer;
    function GetTextRangeFull(ATr: Integer): Integer;
    procedure HideSelection(AHide: Boolean);
    function GetSelectionHidden(): Boolean;
    function PointXFromPosition(APos: Integer): Integer;
    function PointYFromPosition(APos: Integer): Integer;
    function LineFromPosition(APos: Integer): Integer;
    function PositionFromLine(ALine: Integer): Integer;
    procedure LineScroll(AColumns: Integer; ALines: Integer);
    procedure ScrollCaret();
    procedure ScrollRange(ASecondary: Integer; APrimary: Integer);
    procedure ReplaceSel(AText: PAnsiChar);
    procedure SetReadOnly(AReadOnly: Boolean);
    procedure Null();
    function CanPaste(): Boolean;
    function CanUndo(): Boolean;
    procedure EmptyUndoBuffer();
    procedure Undo();
    procedure Cut();
    procedure Copy();
    procedure Paste();
    procedure Clear();
    procedure SetText(AText: PAnsiChar);
    function GetText(ALength: Integer; AText: PAnsiChar): Integer;
    function GetTextLength(): Integer;
    function GetDirectFunction(): Integer;
    function GetDirectStatusFunction(): Integer;
    function GetDirectPointer(): Integer;
    procedure SetOvertype(AOverType: Boolean);
    function GetOvertype(): Boolean;
    procedure SetCaretWidth(APixelWidth: Integer);
    function GetCaretWidth(): Integer;
    procedure SetTargetStart(AStart: Integer);
    function GetTargetStart(): Integer;
    procedure SetTargetStartVirtualSpace(ASpace: Integer);
    function GetTargetStartVirtualSpace(): Integer;
    procedure SetTargetEnd(AEnd: Integer);
    function GetTargetEnd(): Integer;
    procedure SetTargetEndVirtualSpace(ASpace: Integer);
    function GetTargetEndVirtualSpace(): Integer;
    procedure SetTargetRange(AStart: Integer; AEnd: Integer);
    function GetTargetText(AText: PAnsiChar): Integer;
    procedure TargetFromSelection();
    procedure TargetWholeDocument();
    function ReplaceTarget(ALength: Integer; AText: PAnsiChar): Integer;
    function ReplaceTargetRE(ALength: Integer; AText: PAnsiChar): Integer;
    function ReplaceTargetMinimal(ALength: Integer; AText: PAnsiChar): Integer;
    function SearchInTarget(ALength: Integer; AText: PAnsiChar): Integer;
    procedure SetSearchFlags(ASearchFlags: Integer);
    function GetSearchFlags(): Integer;
    procedure CallTipShow(APos: Integer; ADefinition: PAnsiChar);
    procedure CallTipCancel();
    function CallTipActive(): Boolean;
    function CallTipPosStart(): Integer;
    procedure CallTipSetPosStart(APosStart: Integer);
    procedure CallTipSetHlt(AHighlightStart: Integer; AHighlightEnd: Integer);
    procedure CallTipSetBack(ABack: TColor);
    procedure CallTipSetFore(AFore: TColor);
    procedure CallTipSetForeHlt(AFore: TColor);
    procedure CallTipUseStyle(ATabSize: Integer);
    procedure CallTipSetPosition(AAbove: Boolean);
    function VisibleFromDocLine(ADocLine: Integer): Integer;
    function DocLineFromVisible(ADisplayLine: Integer): Integer;
    function WrapCount(ADocLine: Integer): Integer;
    procedure SetFoldLevel(ALine: Integer; ALevel: Integer);
    function GetFoldLevel(ALine: Integer): Integer;
    function GetLastChild(ALine: Integer; ALevel: Integer): Integer;
    function GetFoldParent(ALine: Integer): Integer;
    procedure ShowLines(ALineStart: Integer; ALineEnd: Integer);
    procedure HideLines(ALineStart: Integer; ALineEnd: Integer);
    function GetLineVisible(ALine: Integer): Boolean;
    function GetAllLinesVisible(): Boolean;
    procedure SetFoldExpanded(ALine: Integer; AExpanded: Boolean);
    function GetFoldExpanded(ALine: Integer): Boolean;
    procedure ToggleFold(ALine: Integer);
    procedure ToggleFoldShowText(ALine: Integer; AText: PAnsiChar);
    procedure FoldDisplayTextSetStyle(AStyle: Integer);
    function FoldDisplayTextGetStyle(): Integer;
    procedure SetDefaultFoldDisplayText(AText: PAnsiChar);
    function GetDefaultFoldDisplayText(AText: PAnsiChar): Integer;
    procedure FoldLine(ALine: Integer; AAction: Integer);
    procedure FoldChildren(ALine: Integer; AAction: Integer);
    procedure ExpandChildren(ALine: Integer; ALevel: Integer);
    procedure FoldAll(AAction: Integer);
    procedure EnsureVisible(ALine: Integer);
    procedure SetAutomaticFold(AAutomaticFold: Integer);
    function GetAutomaticFold(): Integer;
    procedure SetFoldFlags(AFlags: Integer);
    procedure EnsureVisibleEnforcePolicy(ALine: Integer);
    procedure SetTabIndents(ATabIndents: Boolean);
    function GetTabIndents(): Boolean;
    procedure SetBackSpaceUnIndents(ABsUnIndents: Boolean);
    function GetBackSpaceUnIndents(): Boolean;
    procedure SetMouseDwellTime(APeriodMilliseconds: Integer);
    function GetMouseDwellTime(): Integer;
    function WordStartPosition(APos: Integer; AOnlyWordCharacters: Boolean): Integer;
    function WordEndPosition(APos: Integer; AOnlyWordCharacters: Boolean): Integer;
    function IsRangeWord(AStart: Integer; AEnd: Integer): Boolean;
    procedure SetIdleStyling(AIdleStyling: Integer);
    function GetIdleStyling(): Integer;
    procedure SetWrapMode(AWrapMode: Integer);
    function GetWrapMode(): Integer;
    procedure SetWrapVisualFlags(AWrapVisualFlags: Integer);
    function GetWrapVisualFlags(): Integer;
    procedure SetWrapVisualFlagsLocation(AWrapVisualFlagsLocation: Integer);
    function GetWrapVisualFlagsLocation(): Integer;
    procedure SetWrapStartIndent(AIndent: Integer);
    function GetWrapStartIndent(): Integer;
    procedure SetWrapIndentMode(AWrapIndentMode: Integer);
    function GetWrapIndentMode(): Integer;
    procedure SetLayoutCache(ACacheMode: Integer);
    function GetLayoutCache(): Integer;
    procedure SetScrollWidth(APixelWidth: Integer);
    function GetScrollWidth(): Integer;
    procedure SetScrollWidthTracking(ATracking: Boolean);
    function GetScrollWidthTracking(): Boolean;
    function TextWidth(AStyle: Integer; AText: PAnsiChar): Integer;
    procedure SetEndAtLastLine(AEndAtLastLine: Boolean);
    function GetEndAtLastLine(): Boolean;
    function TextHeight(ALine: Integer): Integer;
    procedure SetVScrollBar(AVisible: Boolean);
    function GetVScrollBar(): Boolean;
    procedure AppendText(ALength: Integer; AText: PAnsiChar);
    function GetPhasesDraw(): Integer;
    procedure SetPhasesDraw(APhases: Integer);
    procedure SetFontQuality(AFontQuality: Integer);
    function GetFontQuality(): Integer;
    procedure SetFirstVisibleLine(ADisplayLine: Integer);
    procedure SetMultiPaste(AMultiPaste: Integer);
    function GetMultiPaste(): Integer;
    function GetTag(ATagNumber: Integer; ATagValue: PAnsiChar): Integer;
    procedure LinesJoin();
    procedure LinesSplit(APixelWidth: Integer);
    procedure SetFoldMarginColour(AUseSetting: Boolean; ABack: TColor);
    procedure SetFoldMarginHiColour(AUseSetting: Boolean; AFore: TColor);
    procedure SetAccessibility(AAccessibility: Integer);
    function GetAccessibility(): Integer;
    procedure LineDown();
    procedure LineDownExtend();
    procedure LineUp();
    procedure LineUpExtend();
    procedure CharLeft();
    procedure CharLeftExtend();
    procedure CharRight();
    procedure CharRightExtend();
    procedure WordLeft();
    procedure WordLeftExtend();
    procedure WordRight();
    procedure WordRightExtend();
    procedure Home();
    procedure HomeExtend();
    procedure LineEnd();
    procedure LineEndExtend();
    procedure DocumentStart();
    procedure DocumentStartExtend();
    procedure DocumentEnd();
    procedure DocumentEndExtend();
    procedure PageUp();
    procedure PageUpExtend();
    procedure PageDown();
    procedure PageDownExtend();
    procedure EditToggleOvertype();
    procedure Cancel();
    procedure DeleteBack();
    procedure Tab();
    procedure LineIndent();
    procedure BackTab();
    procedure LineDedent();
    procedure NewLine();
    procedure FormFeed();
    procedure VCHome();
    procedure VCHomeExtend();
    procedure ZoomIn();
    procedure ZoomOut();
    procedure DelWordLeft();
    procedure DelWordRight();
    procedure DelWordRightEnd();
    procedure LineCut();
    procedure LineDelete();
    procedure LineTranspose();
    procedure LineReverse();
    procedure LineDuplicate();
    procedure LowerCase();
    procedure UpperCase();
    procedure LineScrollDown();
    procedure LineScrollUp();
    procedure DeleteBackNotLine();
    procedure HomeDisplay();
    procedure HomeDisplayExtend();
    procedure LineEndDisplay();
    procedure LineEndDisplayExtend();
    procedure HomeWrap();
    procedure HomeWrapExtend();
    procedure LineEndWrap();
    procedure LineEndWrapExtend();
    procedure VCHomeWrap();
    procedure VCHomeWrapExtend();
    procedure LineCopy();
    procedure MoveCaretInsideView();
    function LineLength(ALine: Integer): Integer;
    procedure BraceHighlight(APosA: Integer; APosB: Integer);
    procedure BraceHighlightIndicator(AUseSetting: Boolean; AIndicator: Integer);
    procedure BraceBadLight(APos: Integer);
    procedure BraceBadLightIndicator(AUseSetting: Boolean; AIndicator: Integer);
    function BraceMatch(APos: Integer; AMaxReStyle: Integer): Integer;
    function BraceMatchNext(APos: Integer; AStartPos: Integer): Integer;
    function GetViewEOL(): Boolean;
    procedure SetViewEOL(AVisible: Boolean);
    function GetDocPointer(): Integer;
    procedure SetDocPointer(ADoc: Integer);
    procedure SetModEventMask(AEventMask: Integer);
    function GetEdgeColumn(): Integer;
    procedure SetEdgeColumn(AColumn: Integer);
    function GetEdgeMode(): Integer;
    procedure SetEdgeMode(AEdgeMode: Integer);
    function GetEdgeColour(): TColor;
    procedure SetEdgeColour(AEdgeColour: TColor);
    procedure MultiEdgeAddLine(AColumn: Integer; AEdgeColour: TColor);
    procedure MultiEdgeClearAll();
    function GetMultiEdgeColumn(AWhich: Integer): Integer;
    procedure SearchAnchor();
    function SearchNext(ASearchFlags: Integer; AText: PAnsiChar): Integer;
    function SearchPrev(ASearchFlags: Integer; AText: PAnsiChar): Integer;
    function LinesOnScreen(): Integer;
    procedure UsePopUp(APopUpMode: Integer);
    function SelectionIsRectangle(): Boolean;
    procedure SetZoom(AZoomInPoints: Integer);
    function GetZoom(): Integer;
    function CreateDocument(ABytes: Integer; ADocumentOptions: Integer): Integer;
    procedure AddRefDocument(ADoc: Integer);
    procedure ReleaseDocument(ADoc: Integer);
    function GetDocumentOptions(): Integer;
    function GetModEventMask(): Integer;
    procedure SetCommandEvents(ACommandEvents: Boolean);
    function GetCommandEvents(): Boolean;
    procedure SetFocus(AFocus: Boolean);
    function GetFocus(): Boolean;
    procedure SetStatus(AStatus: Integer);
    function GetStatus(): Integer;
    procedure SetMouseDownCaptures(ACaptures: Boolean);
    function GetMouseDownCaptures(): Boolean;
    procedure SetMouseWheelCaptures(ACaptures: Boolean);
    function GetMouseWheelCaptures(): Boolean;
    procedure SetCursor(ACursorType: Integer);
    function GetCursor(): Integer;
    procedure SetControlCharSymbol(ASymbol: Integer);
    function GetControlCharSymbol(): Integer;
    procedure WordPartLeft();
    procedure WordPartLeftExtend();
    procedure WordPartRight();
    procedure WordPartRightExtend();
    procedure SetVisiblePolicy(AVisiblePolicy: Integer; AVisibleSlop: Integer);
    procedure DelLineLeft();
    procedure DelLineRight();
    procedure SetXOffset(AXOffset: Integer);
    function GetXOffset(): Integer;
    procedure ChooseCaretX();
    procedure GrabFocus();
    procedure SetXCaretPolicy(ACaretPolicy: Integer; ACaretSlop: Integer);
    procedure SetYCaretPolicy(ACaretPolicy: Integer; ACaretSlop: Integer);
    procedure SetPrintWrapMode(AWrapMode: Integer);
    function GetPrintWrapMode(): Integer;
    procedure SetHotspotActiveFore(AUseSetting: Boolean; AFore: TColor);
    function GetHotspotActiveFore(): TColor;
    procedure SetHotspotActiveBack(AUseSetting: Boolean; ABack: TColor);
    function GetHotspotActiveBack(): TColor;
    procedure SetHotspotActiveUnderline(AUnderline: Boolean);
    function GetHotspotActiveUnderline(): Boolean;
    procedure SetHotspotSingleLine(ASingleLine: Boolean);
    function GetHotspotSingleLine(): Boolean;
    procedure ParaDown();
    procedure ParaDownExtend();
    procedure ParaUp();
    procedure ParaUpExtend();
    function PositionBefore(APos: Integer): Integer;
    function PositionAfter(APos: Integer): Integer;
    function PositionRelative(APos: Integer; ARelative: Integer): Integer;
    function PositionRelativeCodeUnits(APos: Integer; ARelative: Integer): Integer;
    procedure CopyRange(AStart: Integer; AEnd: Integer);
    procedure CopyText(ALength: Integer; AText: PAnsiChar);
    procedure SetSelectionMode(ASelectionMode: Integer);
    procedure ChangeSelectionMode(ASelectionMode: Integer);
    function GetSelectionMode(): Integer;
    procedure SetMoveExtendsSelection(AMoveExtendsSelection: Boolean);
    function GetMoveExtendsSelection(): Boolean;
    function GetLineSelStartPosition(ALine: Integer): Integer;
    function GetLineSelEndPosition(ALine: Integer): Integer;
    procedure LineDownRectExtend();
    procedure LineUpRectExtend();
    procedure CharLeftRectExtend();
    procedure CharRightRectExtend();
    procedure HomeRectExtend();
    procedure VCHomeRectExtend();
    procedure LineEndRectExtend();
    procedure PageUpRectExtend();
    procedure PageDownRectExtend();
    procedure StutteredPageUp();
    procedure StutteredPageUpExtend();
    procedure StutteredPageDown();
    procedure StutteredPageDownExtend();
    procedure WordLeftEnd();
    procedure WordLeftEndExtend();
    procedure WordRightEnd();
    procedure WordRightEndExtend();
    procedure SetWhitespaceChars(ACharacters: PAnsiChar);
    function GetWhitespaceChars(ACharacters: PAnsiChar): Integer;
    procedure SetPunctuationChars(ACharacters: PAnsiChar);
    function GetPunctuationChars(ACharacters: PAnsiChar): Integer;
    procedure SetCharsDefault();
    function AutoCGetCurrent(): Integer;
    function AutoCGetCurrentText(AText: PAnsiChar): Integer;
    procedure AutoCSetCaseInsensitiveBehaviour(ABehaviour: Integer);
    function AutoCGetCaseInsensitiveBehaviour(): Integer;
    procedure AutoCSetMulti(AMulti: Integer);
    function AutoCGetMulti(): Integer;
    procedure AutoCSetOrder(AOrder: Integer);
    function AutoCGetOrder(): Integer;
    procedure Allocate(ABytes: Integer);
    function TargetAsUTF8(S: PAnsiChar): Integer;
    procedure SetLengthForEncode(ABytes: Integer);
    function EncodedFromUTF8(AUtf8: PAnsiChar; AEncoded: PAnsiChar): Integer;
    function FindColumn(ALine: Integer; AColumn: Integer): Integer;
    function GetCaretSticky(): Integer;
    procedure SetCaretSticky(AUseCaretStickyBehaviour: Integer);
    procedure ToggleCaretSticky();
    procedure SetPasteConvertEndings(AConvert: Boolean);
    function GetPasteConvertEndings(): Boolean;
    procedure ReplaceRectangular(ALength: Integer; AText: PAnsiChar);
    procedure SelectionDuplicate();
    procedure SetCaretLineBackAlpha(AAlpha: Integer);
    function GetCaretLineBackAlpha(): Integer;
    procedure SetCaretStyle(ACaretStyle: Integer);
    function GetCaretStyle(): Integer;
    procedure SetIndicatorCurrent(AIndicator: Integer);
    function GetIndicatorCurrent(): Integer;
    procedure SetIndicatorValue(AValue: Integer);
    function GetIndicatorValue(): Integer;
    procedure IndicatorFillRange(AStart: Integer; ALengthFill: Integer);
    procedure IndicatorClearRange(AStart: Integer; ALengthClear: Integer);
    function IndicatorAllOnFor(APos: Integer): Integer;
    function IndicatorValueAt(AIndicator: Integer; APos: Integer): Integer;
    function IndicatorStart(AIndicator: Integer; APos: Integer): Integer;
    function IndicatorEnd(AIndicator: Integer; APos: Integer): Integer;
    procedure SetPositionCache(ASize: Integer);
    function GetPositionCache(): Integer;
    procedure SetLayoutThreads(AThreads: Integer);
    function GetLayoutThreads(): Integer;
    procedure CopyAllowLine();
    procedure CutAllowLine();
    procedure SetCopySeparator(ASeparator: PAnsiChar);
    function GetCopySeparator(ASeparator: PAnsiChar): Integer;
    function GetCharacterPointer(): Integer;
    function GetRangePointer(AStart: Integer; ALengthRange: Integer): Integer;
    function GetGapPosition(): Integer;
    procedure IndicSetAlpha(AIndicator: Integer; AAlpha: Integer);
    function IndicGetAlpha(AIndicator: Integer): Integer;
    procedure IndicSetOutlineAlpha(AIndicator: Integer; AAlpha: Integer);
    function IndicGetOutlineAlpha(AIndicator: Integer): Integer;
    procedure SetExtraAscent(AExtraAscent: Integer);
    function GetExtraAscent(): Integer;
    procedure SetExtraDescent(AExtraDescent: Integer);
    function GetExtraDescent(): Integer;
    function MarkerSymbolDefined(AMarkerNumber: Integer): Integer;
    procedure MarginSetText(ALine: Integer; AText: PAnsiChar);
    function MarginGetText(ALine: Integer; AText: PAnsiChar): Integer;
    procedure MarginSetStyle(ALine: Integer; AStyle: Integer);
    function MarginGetStyle(ALine: Integer): Integer;
    procedure MarginSetStyles(ALine: Integer; AStyles: PAnsiChar);
    function MarginGetStyles(ALine: Integer; AStyles: PAnsiChar): Integer;
    procedure MarginTextClearAll();
    procedure MarginSetStyleOffset(AStyle: Integer);
    function MarginGetStyleOffset(): Integer;
    procedure SetMarginOptions(AMarginOptions: Integer);
    function GetMarginOptions(): Integer;
    procedure AnnotationSetText(ALine: Integer; AText: PAnsiChar);
    function AnnotationGetText(ALine: Integer; AText: PAnsiChar): Integer;
    procedure AnnotationSetStyle(ALine: Integer; AStyle: Integer);
    function AnnotationGetStyle(ALine: Integer): Integer;
    procedure AnnotationSetStyles(ALine: Integer; AStyles: PAnsiChar);
    function AnnotationGetStyles(ALine: Integer; AStyles: PAnsiChar): Integer;
    function AnnotationGetLines(ALine: Integer): Integer;
    procedure AnnotationClearAll();
    procedure AnnotationSetVisible(AVisible: Integer);
    function AnnotationGetVisible(): Integer;
    procedure AnnotationSetStyleOffset(AStyle: Integer);
    function AnnotationGetStyleOffset(): Integer;
    procedure ReleaseAllExtendedStyles();
    function AllocateExtendedStyles(ANumberStyles: Integer): Integer;
    procedure AddUndoAction(AToken: Integer; AFlags: Integer);
    function CharPositionFromPoint(X: Integer; Y: Integer): Integer;
    function CharPositionFromPointClose(X: Integer; Y: Integer): Integer;
    procedure SetMouseSelectionRectangularSwitch(AMouseSelectionRectangularSwitch: Boolean);
    function GetMouseSelectionRectangularSwitch(): Boolean;
    procedure SetMultipleSelection(AMultipleSelection: Boolean);
    function GetMultipleSelection(): Boolean;
    procedure SetAdditionalSelectionTyping(AAdditionalSelectionTyping: Boolean);
    function GetAdditionalSelectionTyping(): Boolean;
    procedure SetAdditionalCaretsBlink(AAdditionalCaretsBlink: Boolean);
    function GetAdditionalCaretsBlink(): Boolean;
    procedure SetAdditionalCaretsVisible(AAdditionalCaretsVisible: Boolean);
    function GetAdditionalCaretsVisible(): Boolean;
    function GetSelections(): Integer;
    function GetSelectionEmpty(): Boolean;
    procedure ClearSelections();
    procedure SetSelection(ACaret: Integer; AAnchor: Integer);
    procedure AddSelection(ACaret: Integer; AAnchor: Integer);
    function SelectionFromPoint(X: Integer; Y: Integer): Integer;
    procedure DropSelectionN(ASelection: Integer);
    procedure SetMainSelection(ASelection: Integer);
    function GetMainSelection(): Integer;
    procedure SetSelectionNCaret(ASelection: Integer; ACaret: Integer);
    function GetSelectionNCaret(ASelection: Integer): Integer;
    procedure SetSelectionNAnchor(ASelection: Integer; AAnchor: Integer);
    function GetSelectionNAnchor(ASelection: Integer): Integer;
    procedure SetSelectionNCaretVirtualSpace(ASelection: Integer; ASpace: Integer);
    function GetSelectionNCaretVirtualSpace(ASelection: Integer): Integer;
    procedure SetSelectionNAnchorVirtualSpace(ASelection: Integer; ASpace: Integer);
    function GetSelectionNAnchorVirtualSpace(ASelection: Integer): Integer;
    procedure SetSelectionNStart(ASelection: Integer; AAnchor: Integer);
    function GetSelectionNStart(ASelection: Integer): Integer;
    function GetSelectionNStartVirtualSpace(ASelection: Integer): Integer;
    procedure SetSelectionNEnd(ASelection: Integer; ACaret: Integer);
    function GetSelectionNEndVirtualSpace(ASelection: Integer): Integer;
    function GetSelectionNEnd(ASelection: Integer): Integer;
    procedure SetRectangularSelectionCaret(ACaret: Integer);
    function GetRectangularSelectionCaret(): Integer;
    procedure SetRectangularSelectionAnchor(AAnchor: Integer);
    function GetRectangularSelectionAnchor(): Integer;
    procedure SetRectangularSelectionCaretVirtualSpace(ASpace: Integer);
    function GetRectangularSelectionCaretVirtualSpace(): Integer;
    procedure SetRectangularSelectionAnchorVirtualSpace(ASpace: Integer);
    function GetRectangularSelectionAnchorVirtualSpace(): Integer;
    procedure SetVirtualSpaceOptions(AVirtualSpaceOptions: Integer);
    function GetVirtualSpaceOptions(): Integer;
    procedure SetRectangularSelectionModifier(AModifier: Integer);
    function GetRectangularSelectionModifier(): Integer;
    procedure SetAdditionalSelFore(AFore: TColor);
    procedure SetAdditionalSelBack(ABack: TColor);
    procedure SetAdditionalSelAlpha(AAlpha: Integer);
    function GetAdditionalSelAlpha(): Integer;
    procedure SetAdditionalCaretFore(AFore: TColor);
    function GetAdditionalCaretFore(): TColor;
    procedure RotateSelection();
    procedure SwapMainAnchorCaret();
    procedure MultipleSelectAddNext();
    procedure MultipleSelectAddEach();
    function ChangeLexerState(AStart: Integer; AEnd: Integer): Integer;
    function ContractedFoldNext(ALineStart: Integer): Integer;
    procedure VerticalCentreCaret();
    procedure MoveSelectedLinesUp();
    procedure MoveSelectedLinesDown();
    procedure SetIdentifier(AIdentifier: Integer);
    function GetIdentifier(): Integer;
    procedure RGBAImageSetWidth(AWidth: Integer);
    procedure RGBAImageSetHeight(AHeight: Integer);
    procedure RGBAImageSetScale(AScalePercent: Integer);
    procedure MarkerDefineRGBAImage(AMarkerNumber: Integer; APixels: PAnsiChar);
    procedure RegisterRGBAImage(AType: Integer; APixels: PAnsiChar);
    procedure ScrollToStart();
    procedure ScrollToEnd();
    procedure SetTechnology(ATechnology: Integer);
    function GetTechnology(): Integer;
    function CreateLoader(ABytes: Integer; ADocumentOptions: Integer): Integer;
    procedure FindIndicatorShow(AStart: Integer; AEnd: Integer);
    procedure FindIndicatorFlash(AStart: Integer; AEnd: Integer);
    procedure FindIndicatorHide();
    procedure VCHomeDisplay();
    procedure VCHomeDisplayExtend();
    function GetCaretLineVisibleAlways(): Boolean;
    procedure SetCaretLineVisibleAlways(AAlwaysVisible: Boolean);
    procedure SetLineEndTypesAllowed(ALineEndBitSet: Integer);
    function GetLineEndTypesAllowed(): Integer;
    function GetLineEndTypesActive(): Integer;
    procedure SetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar);
    function GetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar): Integer;
    procedure ClearRepresentation(AEncodedCharacter: PAnsiChar);
    procedure ClearAllRepresentations();
    procedure SetRepresentationAppearance(AEncodedCharacter: PAnsiChar; AAppearance: Integer);
    function GetRepresentationAppearance(AEncodedCharacter: PAnsiChar): Integer;
    procedure SetRepresentationColour(AEncodedCharacter: PAnsiChar; AColour: Integer);
    function GetRepresentationColour(AEncodedCharacter: PAnsiChar): Integer;
    procedure EOLAnnotationSetText(ALine: Integer; AText: PAnsiChar);
    function EOLAnnotationGetText(ALine: Integer; AText: PAnsiChar): Integer;
    procedure EOLAnnotationSetStyle(ALine: Integer; AStyle: Integer);
    function EOLAnnotationGetStyle(ALine: Integer): Integer;
    procedure EOLAnnotationClearAll();
    procedure EOLAnnotationSetVisible(AVisible: Integer);
    function EOLAnnotationGetVisible(): Integer;
    procedure EOLAnnotationSetStyleOffset(AStyle: Integer);
    function EOLAnnotationGetStyleOffset(): Integer;
    function SupportsFeature(AFeature: Integer): Boolean;
    function GetLineCharacterIndex(): Integer;
    procedure AllocateLineCharacterIndex(ALineCharacterIndex: Integer);
    procedure ReleaseLineCharacterIndex(ALineCharacterIndex: Integer);
    function LineFromIndexPosition(APos: Integer; ALineCharacterIndex: Integer): Integer;
    function IndexPositionFromLine(ALine: Integer; ALineCharacterIndex: Integer): Integer;
    procedure StartRecord();
    procedure StopRecord();
    function GetLexer(): Integer;
    procedure Colourise(AStart: Integer; AEnd: Integer);
    procedure SetProperty(AKey: PAnsiChar; AValue: PAnsiChar);
    procedure SetKeyWords(AKeyWordSet: Integer; AKeyWords: PAnsiChar);
    function GetProperty(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
    function GetPropertyExpanded(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
    function GetPropertyInt(AKey: PAnsiChar; ADefaultValue: Integer): Integer;
    function GetLexerLanguage(ALanguage: PAnsiChar): Integer;
    function PrivateLexerCall(AOperation: Integer; APointer: Integer): Integer;
    function PropertyNames(ANames: PAnsiChar): Integer;
    function PropertyType(AName: PAnsiChar): Integer;
    function DescribeProperty(AName: PAnsiChar; ADescription: PAnsiChar): Integer;
    function DescribeKeyWordSets(ADescriptions: PAnsiChar): Integer;
    function GetLineEndTypesSupported(): Integer;
    function AllocateSubStyles(AStyleBase: Integer; ANumberStyles: Integer): Integer;
    function GetSubStylesStart(AStyleBase: Integer): Integer;
    function GetSubStylesLength(AStyleBase: Integer): Integer;
    function GetStyleFromSubStyle(ASubStyle: Integer): Integer;
    function GetPrimaryStyleFromStyle(AStyle: Integer): Integer;
    procedure FreeSubStyles();
    procedure SetIdentifiers(AStyle: Integer; AIdentifiers: PAnsiChar);
    function DistanceToSecondaryStyles(): Integer;
    function GetSubStyleBases(AStyles: PAnsiChar): Integer;
    function GetNamedStyles(): Integer;
    function NameOfStyle(AStyle: Integer; AName: PAnsiChar): Integer;
    function TagsOfStyle(AStyle: Integer; ATags: PAnsiChar): Integer;
    function DescriptionOfStyle(AStyle: Integer; ADescription: PAnsiChar): Integer;
    procedure SetILexer(AIlexer: Integer);

    // Provisional
    function GetBidirectional(): Integer;
    procedure SetBidirectional(ABidirectional: Integer);

    // Deprecated
    procedure SetStyleBits(ABits: Integer);
    function GetStyleBits(): Integer;
    function GetStyleBitsNeeded(): Integer;
    procedure SetKeysUnicode(AKeysUnicode: Boolean);
    function GetKeysUnicode(): Boolean;
    function GetTwoPhaseDraw(): Boolean;
    procedure SetTwoPhaseDraw(ATwoPhase: Boolean);


  published
    // Properties geradas automaticamente de funções Get/Set


    // Eventos
    property OnStyleNeeded: TSciPositionEvent read FOnStyleNeeded write FOnStyleNeeded;
    property OnCharAdded: TSciCharEvent read FOnCharAdded write FOnCharAdded;
    property OnModifyAttemptRO: TNotifyEvent read FOnModifyAttemptRO write FOnModifyAttemptRO;
  end;

implementation

constructor TCustomSciTextEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
  TabStop := True;
end;

procedure TCustomSciTextEditor.CNNotify(var Message: TWMNotify);
//var
//  SCN: PSCNotification;
begin
  inherited;
  // Implementar tratamento de notificações aqui
  {
  SCN := PSCNotification(Message.NMHdr);
  case SCN^.nmhdr.code of
    SCN_STYLENEEDED:
      if Assigned(FOnStyleNeeded) then
        FOnStyleNeeded(Self, SCN^.position);
    SCN_CHARADDED:
      if Assigned(FOnCharAdded) then
        FOnCharAdded(Self, AnsiChar(SCN^.ch));
  end;
  }
end;

procedure TCustomSciTextEditor.AddText(ALength: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2001, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.AddStyledText(ALength: Integer; C: PAnsiChar);
begin
  SendScintillaEditorMessage(2002, WPARAM(ALength), LPARAM(C));
end;

procedure TCustomSciTextEditor.InsertText(APos: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2003, WPARAM(APos), LPARAM(AText));
end;

procedure TCustomSciTextEditor.ChangeInsertion(ALength: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2672, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.ClearAll();
begin
  SendScintillaEditorMessage(2004, 0, 0);
end;

procedure TCustomSciTextEditor.DeleteRange(AStart: Integer; ALengthDelete: Integer);
begin
  SendScintillaEditorMessage(2645, WPARAM(AStart), LPARAM(ALengthDelete));
end;

procedure TCustomSciTextEditor.ClearDocumentStyle();
begin
  SendScintillaEditorMessage(2005, 0, 0);
end;

function TCustomSciTextEditor.GetLength(): Integer;
begin
  Result := SendScintillaEditorMessage(2006, 0, 0);
end;

function TCustomSciTextEditor.GetCharAt(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2007, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.GetCurrentPos(): Integer;
begin
  Result := SendScintillaEditorMessage(2008, 0, 0);
end;

function TCustomSciTextEditor.GetAnchor(): Integer;
begin
  Result := SendScintillaEditorMessage(2009, 0, 0);
end;

function TCustomSciTextEditor.GetStyleAt(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2010, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.GetStyleIndexAt(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2038, WPARAM(APos), 0);
end;

procedure TCustomSciTextEditor.Redo();
begin
  SendScintillaEditorMessage(2011, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoCollection(ACollectUndo: Boolean);
begin
  SendScintillaEditorMessage(2012, WPARAM(ACollectUndo), 0);
end;

procedure TCustomSciTextEditor.SelectAll();
begin
  SendScintillaEditorMessage(2013, 0, 0);
end;

procedure TCustomSciTextEditor.SetSavePoint();
begin
  SendScintillaEditorMessage(2014, 0, 0);
end;

function TCustomSciTextEditor.GetStyledText(ATr: PSciTextRange): Integer;
begin
  Result := SendScintillaEditorMessage(2015, 0, LPARAM(ATr));
end;

function TCustomSciTextEditor.GetStyledTextFull(ATr: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2778, 0, LPARAM(ATr));
end;

function TCustomSciTextEditor.CanRedo(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2016, 0, 0));
end;

function TCustomSciTextEditor.MarkerLineFromHandle(AMarkerHandle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2017, WPARAM(AMarkerHandle), 0);
end;

procedure TCustomSciTextEditor.MarkerDeleteHandle(AMarkerHandle: Integer);
begin
  SendScintillaEditorMessage(2018, WPARAM(AMarkerHandle), 0);
end;

function TCustomSciTextEditor.MarkerHandleFromLine(ALine: Integer; AWhich: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2732, WPARAM(ALine), LPARAM(AWhich));
end;

function TCustomSciTextEditor.MarkerNumberFromLine(ALine: Integer; AWhich: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2733, WPARAM(ALine), LPARAM(AWhich));
end;

function TCustomSciTextEditor.GetUndoCollection(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2019, 0, 0));
end;

function TCustomSciTextEditor.GetViewWS(): Integer;
begin
  Result := SendScintillaEditorMessage(2020, 0, 0);
end;

procedure TCustomSciTextEditor.SetViewWS(AViewWS: Integer);
begin
  SendScintillaEditorMessage(2021, WPARAM(AViewWS), 0);
end;

function TCustomSciTextEditor.GetTabDrawMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2698, 0, 0);
end;

procedure TCustomSciTextEditor.SetTabDrawMode(ATabDrawMode: Integer);
begin
  SendScintillaEditorMessage(2699, WPARAM(ATabDrawMode), 0);
end;

function TCustomSciTextEditor.PositionFromPoint(X: Integer; Y: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2022, WPARAM(X), LPARAM(Y));
end;

function TCustomSciTextEditor.PositionFromPointClose(X: Integer; Y: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2023, WPARAM(X), LPARAM(Y));
end;

procedure TCustomSciTextEditor.GotoLine(ALine: Integer);
begin
  SendScintillaEditorMessage(2024, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.GotoPos(ACaret: Integer);
begin
  SendScintillaEditorMessage(2025, WPARAM(ACaret), 0);
end;

procedure TCustomSciTextEditor.SetAnchor(AAnchor: Integer);
begin
  SendScintillaEditorMessage(2026, WPARAM(AAnchor), 0);
end;

function TCustomSciTextEditor.GetCurLine(ALength: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2027, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetEndStyled(): Integer;
begin
  Result := SendScintillaEditorMessage(2028, 0, 0);
end;

procedure TCustomSciTextEditor.ConvertEOLs(AEolMode: Integer);
begin
  SendScintillaEditorMessage(2029, WPARAM(AEolMode), 0);
end;

function TCustomSciTextEditor.GetEOLMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2030, 0, 0);
end;

procedure TCustomSciTextEditor.SetEOLMode(AEolMode: Integer);
begin
  SendScintillaEditorMessage(2031, WPARAM(AEolMode), 0);
end;

procedure TCustomSciTextEditor.StartStyling(AStart: Integer; AUnused: Integer);
begin
  SendScintillaEditorMessage(2032, WPARAM(AStart), LPARAM(AUnused));
end;

procedure TCustomSciTextEditor.SetStyling(ALength: Integer; AStyle: Integer);
begin
  SendScintillaEditorMessage(2033, WPARAM(ALength), LPARAM(AStyle));
end;

function TCustomSciTextEditor.GetBufferedDraw(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2034, 0, 0));
end;

procedure TCustomSciTextEditor.SetBufferedDraw(ABuffered: Boolean);
begin
  SendScintillaEditorMessage(2035, WPARAM(ABuffered), 0);
end;

procedure TCustomSciTextEditor.SetTabWidth(ATabWidth: Integer);
begin
  SendScintillaEditorMessage(2036, WPARAM(ATabWidth), 0);
end;

function TCustomSciTextEditor.GetTabWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(2121, 0, 0);
end;

procedure TCustomSciTextEditor.SetTabMinimumWidth(APixels: Integer);
begin
  SendScintillaEditorMessage(2724, WPARAM(APixels), 0);
end;

function TCustomSciTextEditor.GetTabMinimumWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(2725, 0, 0);
end;

procedure TCustomSciTextEditor.ClearTabStops(ALine: Integer);
begin
  SendScintillaEditorMessage(2675, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.AddTabStop(ALine: Integer; X: Integer);
begin
  SendScintillaEditorMessage(2676, WPARAM(ALine), LPARAM(X));
end;

function TCustomSciTextEditor.GetNextTabStop(ALine: Integer; X: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2677, WPARAM(ALine), LPARAM(X));
end;

procedure TCustomSciTextEditor.SetCodePage(ACodePage: Integer);
begin
  SendScintillaEditorMessage(2037, WPARAM(ACodePage), 0);
end;

procedure TCustomSciTextEditor.SetFontLocale(ALocaleName: PAnsiChar);
begin
  SendScintillaEditorMessage(2760, 0, LPARAM(ALocaleName));
end;

function TCustomSciTextEditor.GetFontLocale(ALocaleName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2761, 0, LPARAM(ALocaleName));
end;

function TCustomSciTextEditor.GetIMEInteraction(): Integer;
begin
  Result := SendScintillaEditorMessage(2678, 0, 0);
end;

procedure TCustomSciTextEditor.SetIMEInteraction(AImeInteraction: Integer);
begin
  SendScintillaEditorMessage(2679, WPARAM(AImeInteraction), 0);
end;

procedure TCustomSciTextEditor.MarkerDefine(AMarkerNumber: Integer; AMarkerSymbol: Integer);
begin
  SendScintillaEditorMessage(2040, WPARAM(AMarkerNumber), LPARAM(AMarkerSymbol));
end;

procedure TCustomSciTextEditor.MarkerSetFore(AMarkerNumber: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(2041, WPARAM(AMarkerNumber), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.MarkerSetBack(AMarkerNumber: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(2042, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetBackSelected(AMarkerNumber: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(2292, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetForeTranslucent(AMarkerNumber: Integer; AFore: Integer);
begin
  SendScintillaEditorMessage(2294, WPARAM(AMarkerNumber), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.MarkerSetBackTranslucent(AMarkerNumber: Integer; ABack: Integer);
begin
  SendScintillaEditorMessage(2295, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetBackSelectedTranslucent(AMarkerNumber: Integer; ABack: Integer);
begin
  SendScintillaEditorMessage(2296, WPARAM(AMarkerNumber), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.MarkerSetStrokeWidth(AMarkerNumber: Integer; AHundredths: Integer);
begin
  SendScintillaEditorMessage(2297, WPARAM(AMarkerNumber), LPARAM(AHundredths));
end;

procedure TCustomSciTextEditor.MarkerEnableHighlight(AEnabled: Boolean);
begin
  SendScintillaEditorMessage(2293, WPARAM(AEnabled), 0);
end;

function TCustomSciTextEditor.MarkerAdd(ALine: Integer; AMarkerNumber: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2043, WPARAM(ALine), LPARAM(AMarkerNumber));
end;

procedure TCustomSciTextEditor.MarkerDelete(ALine: Integer; AMarkerNumber: Integer);
begin
  SendScintillaEditorMessage(2044, WPARAM(ALine), LPARAM(AMarkerNumber));
end;

procedure TCustomSciTextEditor.MarkerDeleteAll(AMarkerNumber: Integer);
begin
  SendScintillaEditorMessage(2045, WPARAM(AMarkerNumber), 0);
end;

function TCustomSciTextEditor.MarkerGet(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2046, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.MarkerNext(ALineStart: Integer; AMarkerMask: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2047, WPARAM(ALineStart), LPARAM(AMarkerMask));
end;

function TCustomSciTextEditor.MarkerPrevious(ALineStart: Integer; AMarkerMask: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2048, WPARAM(ALineStart), LPARAM(AMarkerMask));
end;

procedure TCustomSciTextEditor.MarkerDefinePixmap(AMarkerNumber: Integer; APixmap: PAnsiChar);
begin
  SendScintillaEditorMessage(2049, WPARAM(AMarkerNumber), LPARAM(APixmap));
end;

procedure TCustomSciTextEditor.MarkerAddSet(ALine: Integer; AMarkerSet: Integer);
begin
  SendScintillaEditorMessage(2466, WPARAM(ALine), LPARAM(AMarkerSet));
end;

procedure TCustomSciTextEditor.MarkerSetAlpha(AMarkerNumber: Integer; AAlpha: Integer);
begin
  SendScintillaEditorMessage(2476, WPARAM(AMarkerNumber), LPARAM(AAlpha));
end;

function TCustomSciTextEditor.MarkerGetLayer(AMarkerNumber: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2734, WPARAM(AMarkerNumber), 0);
end;

procedure TCustomSciTextEditor.MarkerSetLayer(AMarkerNumber: Integer; ALayer: Integer);
begin
  SendScintillaEditorMessage(2735, WPARAM(AMarkerNumber), LPARAM(ALayer));
end;

procedure TCustomSciTextEditor.SetMarginTypeN(AMargin: Integer; AMarginType: Integer);
begin
  SendScintillaEditorMessage(2240, WPARAM(AMargin), LPARAM(AMarginType));
end;

function TCustomSciTextEditor.GetMarginTypeN(AMargin: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2241, WPARAM(AMargin), 0);
end;

procedure TCustomSciTextEditor.SetMarginWidthN(AMargin: Integer; APixelWidth: Integer);
begin
  SendScintillaEditorMessage(2242, WPARAM(AMargin), LPARAM(APixelWidth));
end;

function TCustomSciTextEditor.GetMarginWidthN(AMargin: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2243, WPARAM(AMargin), 0);
end;

procedure TCustomSciTextEditor.SetMarginMaskN(AMargin: Integer; AMask: Integer);
begin
  SendScintillaEditorMessage(2244, WPARAM(AMargin), LPARAM(AMask));
end;

function TCustomSciTextEditor.GetMarginMaskN(AMargin: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2245, WPARAM(AMargin), 0);
end;

procedure TCustomSciTextEditor.SetMarginSensitiveN(AMargin: Integer; ASensitive: Boolean);
begin
  SendScintillaEditorMessage(2246, WPARAM(AMargin), LPARAM(ASensitive));
end;

function TCustomSciTextEditor.GetMarginSensitiveN(AMargin: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2247, WPARAM(AMargin), 0));
end;

procedure TCustomSciTextEditor.SetMarginCursorN(AMargin: Integer; ACursor: Integer);
begin
  SendScintillaEditorMessage(2248, WPARAM(AMargin), LPARAM(ACursor));
end;

function TCustomSciTextEditor.GetMarginCursorN(AMargin: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2249, WPARAM(AMargin), 0);
end;

procedure TCustomSciTextEditor.SetMarginBackN(AMargin: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(2250, WPARAM(AMargin), LPARAM(ABack));
end;

function TCustomSciTextEditor.GetMarginBackN(AMargin: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(2251, WPARAM(AMargin), 0);
end;

procedure TCustomSciTextEditor.SetMargins(AMargins: Integer);
begin
  SendScintillaEditorMessage(2252, WPARAM(AMargins), 0);
end;

function TCustomSciTextEditor.GetMargins(): Integer;
begin
  Result := SendScintillaEditorMessage(2253, 0, 0);
end;

procedure TCustomSciTextEditor.StyleClearAll();
begin
  SendScintillaEditorMessage(2050, 0, 0);
end;

procedure TCustomSciTextEditor.StyleSetFore(AStyle: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(2051, WPARAM(AStyle), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.StyleSetBack(AStyle: Integer; ABack: TColor);
begin
  SendScintillaEditorMessage(2052, WPARAM(AStyle), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.StyleSetBold(AStyle: Integer; ABold: Boolean);
begin
  SendScintillaEditorMessage(2053, WPARAM(AStyle), LPARAM(ABold));
end;

procedure TCustomSciTextEditor.StyleSetItalic(AStyle: Integer; AItalic: Boolean);
begin
  SendScintillaEditorMessage(2054, WPARAM(AStyle), LPARAM(AItalic));
end;

procedure TCustomSciTextEditor.StyleSetSize(AStyle: Integer; ASizePoints: Integer);
begin
  SendScintillaEditorMessage(2055, WPARAM(AStyle), LPARAM(ASizePoints));
end;

procedure TCustomSciTextEditor.StyleSetFont(AStyle: Integer; AFontName: PAnsiChar);
begin
  SendScintillaEditorMessage(2056, WPARAM(AStyle), LPARAM(AFontName));
end;

procedure TCustomSciTextEditor.StyleSetEOLFilled(AStyle: Integer; AEolFilled: Boolean);
begin
  SendScintillaEditorMessage(2057, WPARAM(AStyle), LPARAM(AEolFilled));
end;

procedure TCustomSciTextEditor.StyleResetDefault();
begin
  SendScintillaEditorMessage(2058, 0, 0);
end;

procedure TCustomSciTextEditor.StyleSetUnderline(AStyle: Integer; AUnderline: Boolean);
begin
  SendScintillaEditorMessage(2059, WPARAM(AStyle), LPARAM(AUnderline));
end;

function TCustomSciTextEditor.StyleGetFore(AStyle: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(2481, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.StyleGetBack(AStyle: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(2482, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.StyleGetBold(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2483, WPARAM(AStyle), 0));
end;

function TCustomSciTextEditor.StyleGetItalic(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2484, WPARAM(AStyle), 0));
end;

function TCustomSciTextEditor.StyleGetSize(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2485, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.StyleGetFont(AStyle: Integer; AFontName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2486, WPARAM(AStyle), LPARAM(AFontName));
end;

function TCustomSciTextEditor.StyleGetEOLFilled(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2487, WPARAM(AStyle), 0));
end;

function TCustomSciTextEditor.StyleGetUnderline(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2488, WPARAM(AStyle), 0));
end;

function TCustomSciTextEditor.StyleGetCase(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2489, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.StyleGetCharacterSet(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2490, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.StyleGetVisible(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2491, WPARAM(AStyle), 0));
end;

function TCustomSciTextEditor.StyleGetChangeable(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2492, WPARAM(AStyle), 0));
end;

function TCustomSciTextEditor.StyleGetHotSpot(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2493, WPARAM(AStyle), 0));
end;

procedure TCustomSciTextEditor.StyleSetCase(AStyle: Integer; ACaseVisible: Integer);
begin
  SendScintillaEditorMessage(2060, WPARAM(AStyle), LPARAM(ACaseVisible));
end;

procedure TCustomSciTextEditor.StyleSetSizeFractional(AStyle: Integer; ASizeHundredthPoints: Integer);
begin
  SendScintillaEditorMessage(2061, WPARAM(AStyle), LPARAM(ASizeHundredthPoints));
end;

function TCustomSciTextEditor.StyleGetSizeFractional(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2062, WPARAM(AStyle), 0);
end;

procedure TCustomSciTextEditor.StyleSetWeight(AStyle: Integer; AWeight: Integer);
begin
  SendScintillaEditorMessage(2063, WPARAM(AStyle), LPARAM(AWeight));
end;

function TCustomSciTextEditor.StyleGetWeight(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2064, WPARAM(AStyle), 0);
end;

procedure TCustomSciTextEditor.StyleSetCharacterSet(AStyle: Integer; ACharacterSet: Integer);
begin
  SendScintillaEditorMessage(2066, WPARAM(AStyle), LPARAM(ACharacterSet));
end;

procedure TCustomSciTextEditor.StyleSetHotSpot(AStyle: Integer; AHotspot: Boolean);
begin
  SendScintillaEditorMessage(2409, WPARAM(AStyle), LPARAM(AHotspot));
end;

procedure TCustomSciTextEditor.StyleSetCheckMonospaced(AStyle: Integer; ACheckMonospaced: Boolean);
begin
  SendScintillaEditorMessage(2254, WPARAM(AStyle), LPARAM(ACheckMonospaced));
end;

function TCustomSciTextEditor.StyleGetCheckMonospaced(AStyle: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2255, WPARAM(AStyle), 0));
end;

procedure TCustomSciTextEditor.StyleSetStretch(AStyle: Integer; AStretch: Integer);
begin
  SendScintillaEditorMessage(2258, WPARAM(AStyle), LPARAM(AStretch));
end;

function TCustomSciTextEditor.StyleGetStretch(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2259, WPARAM(AStyle), 0);
end;

procedure TCustomSciTextEditor.StyleSetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar);
begin
  SendScintillaEditorMessage(2256, WPARAM(AStyle), LPARAM(ARepresentation));
end;

function TCustomSciTextEditor.StyleGetInvisibleRepresentation(AStyle: Integer; ARepresentation: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2257, WPARAM(AStyle), LPARAM(ARepresentation));
end;

procedure TCustomSciTextEditor.SetElementColour(AElement: Integer; AColourElement: Integer);
begin
  SendScintillaEditorMessage(2753, WPARAM(AElement), LPARAM(AColourElement));
end;

function TCustomSciTextEditor.GetElementColour(AElement: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2754, WPARAM(AElement), 0);
end;

procedure TCustomSciTextEditor.ResetElementColour(AElement: Integer);
begin
  SendScintillaEditorMessage(2755, WPARAM(AElement), 0);
end;

function TCustomSciTextEditor.GetElementIsSet(AElement: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2756, WPARAM(AElement), 0));
end;

function TCustomSciTextEditor.GetElementAllowsTranslucent(AElement: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2757, WPARAM(AElement), 0));
end;

function TCustomSciTextEditor.GetElementBaseColour(AElement: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2758, WPARAM(AElement), 0);
end;

procedure TCustomSciTextEditor.SetSelFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(2067, WPARAM(AUseSetting), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.SetSelBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(2068, WPARAM(AUseSetting), LPARAM(ABack));
end;

function TCustomSciTextEditor.GetSelAlpha(): Integer;
begin
  Result := SendScintillaEditorMessage(2477, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelAlpha(AAlpha: Integer);
begin
  SendScintillaEditorMessage(2478, WPARAM(AAlpha), 0);
end;

function TCustomSciTextEditor.GetSelEOLFilled(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2479, 0, 0));
end;

procedure TCustomSciTextEditor.SetSelEOLFilled(AFilled: Boolean);
begin
  SendScintillaEditorMessage(2480, WPARAM(AFilled), 0);
end;

function TCustomSciTextEditor.GetSelectionLayer(): Integer;
begin
  Result := SendScintillaEditorMessage(2762, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionLayer(ALayer: Integer);
begin
  SendScintillaEditorMessage(2763, WPARAM(ALayer), 0);
end;

function TCustomSciTextEditor.GetCaretLineLayer(): Integer;
begin
  Result := SendScintillaEditorMessage(2764, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineLayer(ALayer: Integer);
begin
  SendScintillaEditorMessage(2765, WPARAM(ALayer), 0);
end;

function TCustomSciTextEditor.GetCaretLineHighlightSubLine(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2773, 0, 0));
end;

procedure TCustomSciTextEditor.SetCaretLineHighlightSubLine(ASubLine: Boolean);
begin
  SendScintillaEditorMessage(2774, WPARAM(ASubLine), 0);
end;

procedure TCustomSciTextEditor.SetCaretFore(AFore: TColor);
begin
  SendScintillaEditorMessage(2069, WPARAM(AFore), 0);
end;

procedure TCustomSciTextEditor.AssignCmdKey(AKeyDefinition: Integer; ASciCommand: Integer);
begin
  SendScintillaEditorMessage(2070, WPARAM(AKeyDefinition), LPARAM(ASciCommand));
end;

procedure TCustomSciTextEditor.ClearCmdKey(AKeyDefinition: Integer);
begin
  SendScintillaEditorMessage(2071, WPARAM(AKeyDefinition), 0);
end;

procedure TCustomSciTextEditor.ClearAllCmdKeys();
begin
  SendScintillaEditorMessage(2072, 0, 0);
end;

procedure TCustomSciTextEditor.SetStylingEx(ALength: Integer; AStyles: PAnsiChar);
begin
  SendScintillaEditorMessage(2073, WPARAM(ALength), LPARAM(AStyles));
end;

procedure TCustomSciTextEditor.StyleSetVisible(AStyle: Integer; AVisible: Boolean);
begin
  SendScintillaEditorMessage(2074, WPARAM(AStyle), LPARAM(AVisible));
end;

function TCustomSciTextEditor.GetCaretPeriod(): Integer;
begin
  Result := SendScintillaEditorMessage(2075, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretPeriod(APeriodMilliseconds: Integer);
begin
  SendScintillaEditorMessage(2076, WPARAM(APeriodMilliseconds), 0);
end;

procedure TCustomSciTextEditor.SetWordChars(ACharacters: PAnsiChar);
begin
  SendScintillaEditorMessage(2077, 0, LPARAM(ACharacters));
end;

function TCustomSciTextEditor.GetWordChars(ACharacters: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2646, 0, LPARAM(ACharacters));
end;

procedure TCustomSciTextEditor.SetCharacterCategoryOptimization(ACountCharacters: Integer);
begin
  SendScintillaEditorMessage(2720, WPARAM(ACountCharacters), 0);
end;

function TCustomSciTextEditor.GetCharacterCategoryOptimization(): Integer;
begin
  Result := SendScintillaEditorMessage(2721, 0, 0);
end;

procedure TCustomSciTextEditor.BeginUndoAction();
begin
  SendScintillaEditorMessage(2078, 0, 0);
end;

procedure TCustomSciTextEditor.EndUndoAction();
begin
  SendScintillaEditorMessage(2079, 0, 0);
end;

function TCustomSciTextEditor.GetUndoSequence(): Integer;
begin
  Result := SendScintillaEditorMessage(2799, 0, 0);
end;

function TCustomSciTextEditor.GetUndoActions(): Integer;
begin
  Result := SendScintillaEditorMessage(2790, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoSavePoint(AAction: Integer);
begin
  SendScintillaEditorMessage(2791, WPARAM(AAction), 0);
end;

function TCustomSciTextEditor.GetUndoSavePoint(): Integer;
begin
  Result := SendScintillaEditorMessage(2792, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoDetach(AAction: Integer);
begin
  SendScintillaEditorMessage(2793, WPARAM(AAction), 0);
end;

function TCustomSciTextEditor.GetUndoDetach(): Integer;
begin
  Result := SendScintillaEditorMessage(2794, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoTentative(AAction: Integer);
begin
  SendScintillaEditorMessage(2795, WPARAM(AAction), 0);
end;

function TCustomSciTextEditor.GetUndoTentative(): Integer;
begin
  Result := SendScintillaEditorMessage(2796, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoCurrent(AAction: Integer);
begin
  SendScintillaEditorMessage(2797, WPARAM(AAction), 0);
end;

function TCustomSciTextEditor.GetUndoCurrent(): Integer;
begin
  Result := SendScintillaEditorMessage(2798, 0, 0);
end;

procedure TCustomSciTextEditor.PushUndoActionType(AType: Integer; APos: Integer);
begin
  SendScintillaEditorMessage(2800, WPARAM(AType), LPARAM(APos));
end;

procedure TCustomSciTextEditor.ChangeLastUndoActionText(ALength: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2801, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetUndoActionType(AAction: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2802, WPARAM(AAction), 0);
end;

function TCustomSciTextEditor.GetUndoActionPosition(AAction: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2803, WPARAM(AAction), 0);
end;

function TCustomSciTextEditor.GetUndoActionText(AAction: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2804, WPARAM(AAction), LPARAM(AText));
end;

procedure TCustomSciTextEditor.IndicSetStyle(AIndicator: Integer; AIndicatorStyle: Integer);
begin
  SendScintillaEditorMessage(2080, WPARAM(AIndicator), LPARAM(AIndicatorStyle));
end;

function TCustomSciTextEditor.IndicGetStyle(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2081, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.IndicSetFore(AIndicator: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(2082, WPARAM(AIndicator), LPARAM(AFore));
end;

function TCustomSciTextEditor.IndicGetFore(AIndicator: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(2083, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.IndicSetUnder(AIndicator: Integer; AUnder: Boolean);
begin
  SendScintillaEditorMessage(2510, WPARAM(AIndicator), LPARAM(AUnder));
end;

function TCustomSciTextEditor.IndicGetUnder(AIndicator: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2511, WPARAM(AIndicator), 0));
end;

procedure TCustomSciTextEditor.IndicSetHoverStyle(AIndicator: Integer; AIndicatorStyle: Integer);
begin
  SendScintillaEditorMessage(2680, WPARAM(AIndicator), LPARAM(AIndicatorStyle));
end;

function TCustomSciTextEditor.IndicGetHoverStyle(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2681, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.IndicSetHoverFore(AIndicator: Integer; AFore: TColor);
begin
  SendScintillaEditorMessage(2682, WPARAM(AIndicator), LPARAM(AFore));
end;

function TCustomSciTextEditor.IndicGetHoverFore(AIndicator: Integer): TColor;
begin
  Result := SendScintillaEditorMessage(2683, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.IndicSetFlags(AIndicator: Integer; AFlags: Integer);
begin
  SendScintillaEditorMessage(2684, WPARAM(AIndicator), LPARAM(AFlags));
end;

function TCustomSciTextEditor.IndicGetFlags(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2685, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.IndicSetStrokeWidth(AIndicator: Integer; AHundredths: Integer);
begin
  SendScintillaEditorMessage(2751, WPARAM(AIndicator), LPARAM(AHundredths));
end;

function TCustomSciTextEditor.IndicGetStrokeWidth(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2752, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.SetWhitespaceFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(2084, WPARAM(AUseSetting), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.SetWhitespaceBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(2085, WPARAM(AUseSetting), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.SetWhitespaceSize(ASize: Integer);
begin
  SendScintillaEditorMessage(2086, WPARAM(ASize), 0);
end;

function TCustomSciTextEditor.GetWhitespaceSize(): Integer;
begin
  Result := SendScintillaEditorMessage(2087, 0, 0);
end;

procedure TCustomSciTextEditor.SetLineState(ALine: Integer; AState: Integer);
begin
  SendScintillaEditorMessage(2092, WPARAM(ALine), LPARAM(AState));
end;

function TCustomSciTextEditor.GetLineState(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2093, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.GetMaxLineState(): Integer;
begin
  Result := SendScintillaEditorMessage(2094, 0, 0);
end;

function TCustomSciTextEditor.GetCaretLineVisible(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2095, 0, 0));
end;

procedure TCustomSciTextEditor.SetCaretLineVisible(AShow: Boolean);
begin
  SendScintillaEditorMessage(2096, WPARAM(AShow), 0);
end;

function TCustomSciTextEditor.GetCaretLineBack(): TColor;
begin
  Result := SendScintillaEditorMessage(2097, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineBack(ABack: TColor);
begin
  SendScintillaEditorMessage(2098, WPARAM(ABack), 0);
end;

function TCustomSciTextEditor.GetCaretLineFrame(): Integer;
begin
  Result := SendScintillaEditorMessage(2704, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineFrame(AWidth: Integer);
begin
  SendScintillaEditorMessage(2705, WPARAM(AWidth), 0);
end;

procedure TCustomSciTextEditor.StyleSetChangeable(AStyle: Integer; AChangeable: Boolean);
begin
  SendScintillaEditorMessage(2099, WPARAM(AStyle), LPARAM(AChangeable));
end;

procedure TCustomSciTextEditor.AutoCShow(ALengthEntered: Integer; AItemList: PAnsiChar);
begin
  SendScintillaEditorMessage(2100, WPARAM(ALengthEntered), LPARAM(AItemList));
end;

procedure TCustomSciTextEditor.AutoCCancel();
begin
  SendScintillaEditorMessage(2101, 0, 0);
end;

function TCustomSciTextEditor.AutoCActive(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2102, 0, 0));
end;

function TCustomSciTextEditor.AutoCPosStart(): Integer;
begin
  Result := SendScintillaEditorMessage(2103, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCComplete();
begin
  SendScintillaEditorMessage(2104, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCStops(ACharacterSet: PAnsiChar);
begin
  SendScintillaEditorMessage(2105, 0, LPARAM(ACharacterSet));
end;

procedure TCustomSciTextEditor.AutoCSetSeparator(ASeparatorCharacter: Integer);
begin
  SendScintillaEditorMessage(2106, WPARAM(ASeparatorCharacter), 0);
end;

function TCustomSciTextEditor.AutoCGetSeparator(): Integer;
begin
  Result := SendScintillaEditorMessage(2107, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSelect(ASelect: PAnsiChar);
begin
  SendScintillaEditorMessage(2108, 0, LPARAM(ASelect));
end;

procedure TCustomSciTextEditor.AutoCSetCancelAtStart(ACancel: Boolean);
begin
  SendScintillaEditorMessage(2110, WPARAM(ACancel), 0);
end;

function TCustomSciTextEditor.AutoCGetCancelAtStart(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2111, 0, 0));
end;

procedure TCustomSciTextEditor.AutoCSetFillUps(ACharacterSet: PAnsiChar);
begin
  SendScintillaEditorMessage(2112, 0, LPARAM(ACharacterSet));
end;

procedure TCustomSciTextEditor.AutoCSetChooseSingle(AChooseSingle: Boolean);
begin
  SendScintillaEditorMessage(2113, WPARAM(AChooseSingle), 0);
end;

function TCustomSciTextEditor.AutoCGetChooseSingle(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2114, 0, 0));
end;

procedure TCustomSciTextEditor.AutoCSetIgnoreCase(AIgnoreCase: Boolean);
begin
  SendScintillaEditorMessage(2115, WPARAM(AIgnoreCase), 0);
end;

function TCustomSciTextEditor.AutoCGetIgnoreCase(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2116, 0, 0));
end;

procedure TCustomSciTextEditor.UserListShow(AListType: Integer; AItemList: PAnsiChar);
begin
  SendScintillaEditorMessage(2117, WPARAM(AListType), LPARAM(AItemList));
end;

procedure TCustomSciTextEditor.AutoCSetAutoHide(AAutoHide: Boolean);
begin
  SendScintillaEditorMessage(2118, WPARAM(AAutoHide), 0);
end;

function TCustomSciTextEditor.AutoCGetAutoHide(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2119, 0, 0));
end;

procedure TCustomSciTextEditor.AutoCSetOptions(AOptions: Integer);
begin
  SendScintillaEditorMessage(2638, WPARAM(AOptions), 0);
end;

function TCustomSciTextEditor.AutoCGetOptions(): Integer;
begin
  Result := SendScintillaEditorMessage(2639, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetDropRestOfWord(ADropRestOfWord: Boolean);
begin
  SendScintillaEditorMessage(2270, WPARAM(ADropRestOfWord), 0);
end;

function TCustomSciTextEditor.AutoCGetDropRestOfWord(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2271, 0, 0));
end;

procedure TCustomSciTextEditor.RegisterImage(AType: Integer; AXpmData: PAnsiChar);
begin
  SendScintillaEditorMessage(2405, WPARAM(AType), LPARAM(AXpmData));
end;

procedure TCustomSciTextEditor.ClearRegisteredImages();
begin
  SendScintillaEditorMessage(2408, 0, 0);
end;

function TCustomSciTextEditor.AutoCGetTypeSeparator(): Integer;
begin
  Result := SendScintillaEditorMessage(2285, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetTypeSeparator(ASeparatorCharacter: Integer);
begin
  SendScintillaEditorMessage(2286, WPARAM(ASeparatorCharacter), 0);
end;

procedure TCustomSciTextEditor.AutoCSetMaxWidth(ACharacterCount: Integer);
begin
  SendScintillaEditorMessage(2208, WPARAM(ACharacterCount), 0);
end;

function TCustomSciTextEditor.AutoCGetMaxWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(2209, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetMaxHeight(ARowCount: Integer);
begin
  SendScintillaEditorMessage(2210, WPARAM(ARowCount), 0);
end;

function TCustomSciTextEditor.AutoCGetMaxHeight(): Integer;
begin
  Result := SendScintillaEditorMessage(2211, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetStyle(AStyle: Integer);
begin
  SendScintillaEditorMessage(2109, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.AutoCGetStyle(): Integer;
begin
  Result := SendScintillaEditorMessage(2120, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndent(AIndentSize: Integer);
begin
  SendScintillaEditorMessage(2122, WPARAM(AIndentSize), 0);
end;

function TCustomSciTextEditor.GetIndent(): Integer;
begin
  Result := SendScintillaEditorMessage(2123, 0, 0);
end;

procedure TCustomSciTextEditor.SetUseTabs(AUseTabs: Boolean);
begin
  SendScintillaEditorMessage(2124, WPARAM(AUseTabs), 0);
end;

function TCustomSciTextEditor.GetUseTabs(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2125, 0, 0));
end;

procedure TCustomSciTextEditor.SetLineIndentation(ALine: Integer; AIndentation: Integer);
begin
  SendScintillaEditorMessage(2126, WPARAM(ALine), LPARAM(AIndentation));
end;

function TCustomSciTextEditor.GetLineIndentation(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2127, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.GetLineIndentPosition(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2128, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.GetColumn(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2129, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.CountCharacters(AStart: Integer; AEnd: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2633, WPARAM(AStart), LPARAM(AEnd));
end;

function TCustomSciTextEditor.CountCodeUnits(AStart: Integer; AEnd: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2715, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.SetHScrollBar(AVisible: Boolean);
begin
  SendScintillaEditorMessage(2130, WPARAM(AVisible), 0);
end;

function TCustomSciTextEditor.GetHScrollBar(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2131, 0, 0));
end;

procedure TCustomSciTextEditor.SetIndentationGuides(AIndentView: Integer);
begin
  SendScintillaEditorMessage(2132, WPARAM(AIndentView), 0);
end;

function TCustomSciTextEditor.GetIndentationGuides(): Integer;
begin
  Result := SendScintillaEditorMessage(2133, 0, 0);
end;

procedure TCustomSciTextEditor.SetHighlightGuide(AColumn: Integer);
begin
  SendScintillaEditorMessage(2134, WPARAM(AColumn), 0);
end;

function TCustomSciTextEditor.GetHighlightGuide(): Integer;
begin
  Result := SendScintillaEditorMessage(2135, 0, 0);
end;

function TCustomSciTextEditor.GetLineEndPosition(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2136, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.GetCodePage(): Integer;
begin
  Result := SendScintillaEditorMessage(2137, 0, 0);
end;

function TCustomSciTextEditor.GetCaretFore(): TColor;
begin
  Result := SendScintillaEditorMessage(2138, 0, 0);
end;

function TCustomSciTextEditor.GetReadOnly(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2140, 0, 0));
end;

procedure TCustomSciTextEditor.SetCurrentPos(ACaret: Integer);
begin
  SendScintillaEditorMessage(2141, WPARAM(ACaret), 0);
end;

procedure TCustomSciTextEditor.SetSelectionStart(AAnchor: Integer);
begin
  SendScintillaEditorMessage(2142, WPARAM(AAnchor), 0);
end;

function TCustomSciTextEditor.GetSelectionStart(): Integer;
begin
  Result := SendScintillaEditorMessage(2143, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionEnd(ACaret: Integer);
begin
  SendScintillaEditorMessage(2144, WPARAM(ACaret), 0);
end;

function TCustomSciTextEditor.GetSelectionEnd(): Integer;
begin
  Result := SendScintillaEditorMessage(2145, 0, 0);
end;

procedure TCustomSciTextEditor.SetEmptySelection(ACaret: Integer);
begin
  SendScintillaEditorMessage(2556, WPARAM(ACaret), 0);
end;

procedure TCustomSciTextEditor.SetPrintMagnification(AMagnification: Integer);
begin
  SendScintillaEditorMessage(2146, WPARAM(AMagnification), 0);
end;

function TCustomSciTextEditor.GetPrintMagnification(): Integer;
begin
  Result := SendScintillaEditorMessage(2147, 0, 0);
end;

procedure TCustomSciTextEditor.SetPrintColourMode(AMode: Integer);
begin
  SendScintillaEditorMessage(2148, WPARAM(AMode), 0);
end;

function TCustomSciTextEditor.GetPrintColourMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2149, 0, 0);
end;

function TCustomSciTextEditor.FindText(ASearchFlags: Integer; AFt: PSciFindText): Integer;
begin
  Result := SendScintillaEditorMessage(2150, WPARAM(ASearchFlags), LPARAM(AFt));
end;

function TCustomSciTextEditor.FindTextFull(ASearchFlags: Integer; AFt: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2196, WPARAM(ASearchFlags), LPARAM(AFt));
end;

function TCustomSciTextEditor.FormatRange(ADraw: Boolean; AFr: PSciFormatRange): Integer;
begin
  Result := SendScintillaEditorMessage(2151, WPARAM(ADraw), LPARAM(AFr));
end;

function TCustomSciTextEditor.FormatRangeFull(ADraw: Boolean; AFr: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2777, WPARAM(ADraw), LPARAM(AFr));
end;

procedure TCustomSciTextEditor.SetChangeHistory(AChangeHistory: Integer);
begin
  SendScintillaEditorMessage(2780, WPARAM(AChangeHistory), 0);
end;

function TCustomSciTextEditor.GetChangeHistory(): Integer;
begin
  Result := SendScintillaEditorMessage(2781, 0, 0);
end;

procedure TCustomSciTextEditor.SetUndoSelectionHistory(AUndoSelectionHistory: Integer);
begin
  SendScintillaEditorMessage(2782, WPARAM(AUndoSelectionHistory), 0);
end;

function TCustomSciTextEditor.GetUndoSelectionHistory(): Integer;
begin
  Result := SendScintillaEditorMessage(2783, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionSerialized(ASelectionString: PAnsiChar);
begin
  SendScintillaEditorMessage(2784, 0, LPARAM(ASelectionString));
end;

function TCustomSciTextEditor.GetSelectionSerialized(ASelectionString: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2785, 0, LPARAM(ASelectionString));
end;

function TCustomSciTextEditor.GetFirstVisibleLine(): Integer;
begin
  Result := SendScintillaEditorMessage(2152, 0, 0);
end;

function TCustomSciTextEditor.GetLine(ALine: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2153, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.GetLineCount(): Integer;
begin
  Result := SendScintillaEditorMessage(2154, 0, 0);
end;

procedure TCustomSciTextEditor.AllocateLines(ALines: Integer);
begin
  SendScintillaEditorMessage(2089, WPARAM(ALines), 0);
end;

procedure TCustomSciTextEditor.SetMarginLeft(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(2155, 0, LPARAM(APixelWidth));
end;

function TCustomSciTextEditor.GetMarginLeft(): Integer;
begin
  Result := SendScintillaEditorMessage(2156, 0, 0);
end;

procedure TCustomSciTextEditor.SetMarginRight(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(2157, 0, LPARAM(APixelWidth));
end;

function TCustomSciTextEditor.GetMarginRight(): Integer;
begin
  Result := SendScintillaEditorMessage(2158, 0, 0);
end;

function TCustomSciTextEditor.GetModify(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2159, 0, 0));
end;

procedure TCustomSciTextEditor.SetSel(AAnchor: Integer; ACaret: Integer);
begin
  SendScintillaEditorMessage(2160, WPARAM(AAnchor), LPARAM(ACaret));
end;

function TCustomSciTextEditor.GetSelText(AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2161, 0, LPARAM(AText));
end;

function TCustomSciTextEditor.GetTextRange(ATr: PSciTextRange): Integer;
begin
  Result := SendScintillaEditorMessage(2162, 0, LPARAM(ATr));
end;

function TCustomSciTextEditor.GetTextRangeFull(ATr: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2039, 0, LPARAM(ATr));
end;

procedure TCustomSciTextEditor.HideSelection(AHide: Boolean);
begin
  SendScintillaEditorMessage(2163, WPARAM(AHide), 0);
end;

function TCustomSciTextEditor.GetSelectionHidden(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2088, 0, 0));
end;

function TCustomSciTextEditor.PointXFromPosition(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2164, 0, LPARAM(APos));
end;

function TCustomSciTextEditor.PointYFromPosition(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2165, 0, LPARAM(APos));
end;

function TCustomSciTextEditor.LineFromPosition(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2166, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.PositionFromLine(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2167, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.LineScroll(AColumns: Integer; ALines: Integer);
begin
  SendScintillaEditorMessage(2168, WPARAM(AColumns), LPARAM(ALines));
end;

procedure TCustomSciTextEditor.ScrollCaret();
begin
  SendScintillaEditorMessage(2169, 0, 0);
end;

procedure TCustomSciTextEditor.ScrollRange(ASecondary: Integer; APrimary: Integer);
begin
  SendScintillaEditorMessage(2569, WPARAM(ASecondary), LPARAM(APrimary));
end;

procedure TCustomSciTextEditor.ReplaceSel(AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2170, 0, LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetReadOnly(AReadOnly: Boolean);
begin
  SendScintillaEditorMessage(2171, WPARAM(AReadOnly), 0);
end;

procedure TCustomSciTextEditor.Null();
begin
  SendScintillaEditorMessage(2172, 0, 0);
end;

function TCustomSciTextEditor.CanPaste(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2173, 0, 0));
end;

function TCustomSciTextEditor.CanUndo(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2174, 0, 0));
end;

procedure TCustomSciTextEditor.EmptyUndoBuffer();
begin
  SendScintillaEditorMessage(2175, 0, 0);
end;

procedure TCustomSciTextEditor.Undo();
begin
  SendScintillaEditorMessage(2176, 0, 0);
end;

procedure TCustomSciTextEditor.Cut();
begin
  SendScintillaEditorMessage(2177, 0, 0);
end;

procedure TCustomSciTextEditor.Copy();
begin
  SendScintillaEditorMessage(2178, 0, 0);
end;

procedure TCustomSciTextEditor.Paste();
begin
  SendScintillaEditorMessage(2179, 0, 0);
end;

procedure TCustomSciTextEditor.Clear();
begin
  SendScintillaEditorMessage(2180, 0, 0);
end;

procedure TCustomSciTextEditor.SetText(AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2181, 0, LPARAM(AText));
end;

function TCustomSciTextEditor.GetText(ALength: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2182, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetTextLength(): Integer;
begin
  Result := SendScintillaEditorMessage(2183, 0, 0);
end;

function TCustomSciTextEditor.GetDirectFunction(): Integer;
begin
  Result := SendScintillaEditorMessage(2184, 0, 0);
end;

function TCustomSciTextEditor.GetDirectStatusFunction(): Integer;
begin
  Result := SendScintillaEditorMessage(2772, 0, 0);
end;

function TCustomSciTextEditor.GetDirectPointer(): Integer;
begin
  Result := SendScintillaEditorMessage(2185, 0, 0);
end;

procedure TCustomSciTextEditor.SetOvertype(AOverType: Boolean);
begin
  SendScintillaEditorMessage(2186, WPARAM(AOverType), 0);
end;

function TCustomSciTextEditor.GetOvertype(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2187, 0, 0));
end;

procedure TCustomSciTextEditor.SetCaretWidth(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(2188, WPARAM(APixelWidth), 0);
end;

function TCustomSciTextEditor.GetCaretWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(2189, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetStart(AStart: Integer);
begin
  SendScintillaEditorMessage(2190, WPARAM(AStart), 0);
end;

function TCustomSciTextEditor.GetTargetStart(): Integer;
begin
  Result := SendScintillaEditorMessage(2191, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetStartVirtualSpace(ASpace: Integer);
begin
  SendScintillaEditorMessage(2728, WPARAM(ASpace), 0);
end;

function TCustomSciTextEditor.GetTargetStartVirtualSpace(): Integer;
begin
  Result := SendScintillaEditorMessage(2729, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetEnd(AEnd: Integer);
begin
  SendScintillaEditorMessage(2192, WPARAM(AEnd), 0);
end;

function TCustomSciTextEditor.GetTargetEnd(): Integer;
begin
  Result := SendScintillaEditorMessage(2193, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetEndVirtualSpace(ASpace: Integer);
begin
  SendScintillaEditorMessage(2730, WPARAM(ASpace), 0);
end;

function TCustomSciTextEditor.GetTargetEndVirtualSpace(): Integer;
begin
  Result := SendScintillaEditorMessage(2731, 0, 0);
end;

procedure TCustomSciTextEditor.SetTargetRange(AStart: Integer; AEnd: Integer);
begin
  SendScintillaEditorMessage(2686, WPARAM(AStart), LPARAM(AEnd));
end;

function TCustomSciTextEditor.GetTargetText(AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2687, 0, LPARAM(AText));
end;

procedure TCustomSciTextEditor.TargetFromSelection();
begin
  SendScintillaEditorMessage(2287, 0, 0);
end;

procedure TCustomSciTextEditor.TargetWholeDocument();
begin
  SendScintillaEditorMessage(2690, 0, 0);
end;

function TCustomSciTextEditor.ReplaceTarget(ALength: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2194, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.ReplaceTargetRE(ALength: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2195, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.ReplaceTargetMinimal(ALength: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2779, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.SearchInTarget(ALength: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2197, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetSearchFlags(ASearchFlags: Integer);
begin
  SendScintillaEditorMessage(2198, WPARAM(ASearchFlags), 0);
end;

function TCustomSciTextEditor.GetSearchFlags(): Integer;
begin
  Result := SendScintillaEditorMessage(2199, 0, 0);
end;

procedure TCustomSciTextEditor.CallTipShow(APos: Integer; ADefinition: PAnsiChar);
begin
  SendScintillaEditorMessage(2200, WPARAM(APos), LPARAM(ADefinition));
end;

procedure TCustomSciTextEditor.CallTipCancel();
begin
  SendScintillaEditorMessage(2201, 0, 0);
end;

function TCustomSciTextEditor.CallTipActive(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2202, 0, 0));
end;

function TCustomSciTextEditor.CallTipPosStart(): Integer;
begin
  Result := SendScintillaEditorMessage(2203, 0, 0);
end;

procedure TCustomSciTextEditor.CallTipSetPosStart(APosStart: Integer);
begin
  SendScintillaEditorMessage(2214, WPARAM(APosStart), 0);
end;

procedure TCustomSciTextEditor.CallTipSetHlt(AHighlightStart: Integer; AHighlightEnd: Integer);
begin
  SendScintillaEditorMessage(2204, WPARAM(AHighlightStart), LPARAM(AHighlightEnd));
end;

procedure TCustomSciTextEditor.CallTipSetBack(ABack: TColor);
begin
  SendScintillaEditorMessage(2205, WPARAM(ABack), 0);
end;

procedure TCustomSciTextEditor.CallTipSetFore(AFore: TColor);
begin
  SendScintillaEditorMessage(2206, WPARAM(AFore), 0);
end;

procedure TCustomSciTextEditor.CallTipSetForeHlt(AFore: TColor);
begin
  SendScintillaEditorMessage(2207, WPARAM(AFore), 0);
end;

procedure TCustomSciTextEditor.CallTipUseStyle(ATabSize: Integer);
begin
  SendScintillaEditorMessage(2212, WPARAM(ATabSize), 0);
end;

procedure TCustomSciTextEditor.CallTipSetPosition(AAbove: Boolean);
begin
  SendScintillaEditorMessage(2213, WPARAM(AAbove), 0);
end;

function TCustomSciTextEditor.VisibleFromDocLine(ADocLine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2220, WPARAM(ADocLine), 0);
end;

function TCustomSciTextEditor.DocLineFromVisible(ADisplayLine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2221, WPARAM(ADisplayLine), 0);
end;

function TCustomSciTextEditor.WrapCount(ADocLine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2235, WPARAM(ADocLine), 0);
end;

procedure TCustomSciTextEditor.SetFoldLevel(ALine: Integer; ALevel: Integer);
begin
  SendScintillaEditorMessage(2222, WPARAM(ALine), LPARAM(ALevel));
end;

function TCustomSciTextEditor.GetFoldLevel(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2223, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.GetLastChild(ALine: Integer; ALevel: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2224, WPARAM(ALine), LPARAM(ALevel));
end;

function TCustomSciTextEditor.GetFoldParent(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2225, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.ShowLines(ALineStart: Integer; ALineEnd: Integer);
begin
  SendScintillaEditorMessage(2226, WPARAM(ALineStart), LPARAM(ALineEnd));
end;

procedure TCustomSciTextEditor.HideLines(ALineStart: Integer; ALineEnd: Integer);
begin
  SendScintillaEditorMessage(2227, WPARAM(ALineStart), LPARAM(ALineEnd));
end;

function TCustomSciTextEditor.GetLineVisible(ALine: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2228, WPARAM(ALine), 0));
end;

function TCustomSciTextEditor.GetAllLinesVisible(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2236, 0, 0));
end;

procedure TCustomSciTextEditor.SetFoldExpanded(ALine: Integer; AExpanded: Boolean);
begin
  SendScintillaEditorMessage(2229, WPARAM(ALine), LPARAM(AExpanded));
end;

function TCustomSciTextEditor.GetFoldExpanded(ALine: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2230, WPARAM(ALine), 0));
end;

procedure TCustomSciTextEditor.ToggleFold(ALine: Integer);
begin
  SendScintillaEditorMessage(2231, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.ToggleFoldShowText(ALine: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2700, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.FoldDisplayTextSetStyle(AStyle: Integer);
begin
  SendScintillaEditorMessage(2701, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.FoldDisplayTextGetStyle(): Integer;
begin
  Result := SendScintillaEditorMessage(2707, 0, 0);
end;

procedure TCustomSciTextEditor.SetDefaultFoldDisplayText(AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2722, 0, LPARAM(AText));
end;

function TCustomSciTextEditor.GetDefaultFoldDisplayText(AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2723, 0, LPARAM(AText));
end;

procedure TCustomSciTextEditor.FoldLine(ALine: Integer; AAction: Integer);
begin
  SendScintillaEditorMessage(2237, WPARAM(ALine), LPARAM(AAction));
end;

procedure TCustomSciTextEditor.FoldChildren(ALine: Integer; AAction: Integer);
begin
  SendScintillaEditorMessage(2238, WPARAM(ALine), LPARAM(AAction));
end;

procedure TCustomSciTextEditor.ExpandChildren(ALine: Integer; ALevel: Integer);
begin
  SendScintillaEditorMessage(2239, WPARAM(ALine), LPARAM(ALevel));
end;

procedure TCustomSciTextEditor.FoldAll(AAction: Integer);
begin
  SendScintillaEditorMessage(2662, WPARAM(AAction), 0);
end;

procedure TCustomSciTextEditor.EnsureVisible(ALine: Integer);
begin
  SendScintillaEditorMessage(2232, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.SetAutomaticFold(AAutomaticFold: Integer);
begin
  SendScintillaEditorMessage(2663, WPARAM(AAutomaticFold), 0);
end;

function TCustomSciTextEditor.GetAutomaticFold(): Integer;
begin
  Result := SendScintillaEditorMessage(2664, 0, 0);
end;

procedure TCustomSciTextEditor.SetFoldFlags(AFlags: Integer);
begin
  SendScintillaEditorMessage(2233, WPARAM(AFlags), 0);
end;

procedure TCustomSciTextEditor.EnsureVisibleEnforcePolicy(ALine: Integer);
begin
  SendScintillaEditorMessage(2234, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.SetTabIndents(ATabIndents: Boolean);
begin
  SendScintillaEditorMessage(2260, WPARAM(ATabIndents), 0);
end;

function TCustomSciTextEditor.GetTabIndents(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2261, 0, 0));
end;

procedure TCustomSciTextEditor.SetBackSpaceUnIndents(ABsUnIndents: Boolean);
begin
  SendScintillaEditorMessage(2262, WPARAM(ABsUnIndents), 0);
end;

function TCustomSciTextEditor.GetBackSpaceUnIndents(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2263, 0, 0));
end;

procedure TCustomSciTextEditor.SetMouseDwellTime(APeriodMilliseconds: Integer);
begin
  SendScintillaEditorMessage(2264, WPARAM(APeriodMilliseconds), 0);
end;

function TCustomSciTextEditor.GetMouseDwellTime(): Integer;
begin
  Result := SendScintillaEditorMessage(2265, 0, 0);
end;

function TCustomSciTextEditor.WordStartPosition(APos: Integer; AOnlyWordCharacters: Boolean): Integer;
begin
  Result := SendScintillaEditorMessage(2266, WPARAM(APos), LPARAM(AOnlyWordCharacters));
end;

function TCustomSciTextEditor.WordEndPosition(APos: Integer; AOnlyWordCharacters: Boolean): Integer;
begin
  Result := SendScintillaEditorMessage(2267, WPARAM(APos), LPARAM(AOnlyWordCharacters));
end;

function TCustomSciTextEditor.IsRangeWord(AStart: Integer; AEnd: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2691, WPARAM(AStart), LPARAM(AEnd)));
end;

procedure TCustomSciTextEditor.SetIdleStyling(AIdleStyling: Integer);
begin
  SendScintillaEditorMessage(2692, WPARAM(AIdleStyling), 0);
end;

function TCustomSciTextEditor.GetIdleStyling(): Integer;
begin
  Result := SendScintillaEditorMessage(2693, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapMode(AWrapMode: Integer);
begin
  SendScintillaEditorMessage(2268, WPARAM(AWrapMode), 0);
end;

function TCustomSciTextEditor.GetWrapMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2269, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapVisualFlags(AWrapVisualFlags: Integer);
begin
  SendScintillaEditorMessage(2460, WPARAM(AWrapVisualFlags), 0);
end;

function TCustomSciTextEditor.GetWrapVisualFlags(): Integer;
begin
  Result := SendScintillaEditorMessage(2461, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapVisualFlagsLocation(AWrapVisualFlagsLocation: Integer);
begin
  SendScintillaEditorMessage(2462, WPARAM(AWrapVisualFlagsLocation), 0);
end;

function TCustomSciTextEditor.GetWrapVisualFlagsLocation(): Integer;
begin
  Result := SendScintillaEditorMessage(2463, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapStartIndent(AIndent: Integer);
begin
  SendScintillaEditorMessage(2464, WPARAM(AIndent), 0);
end;

function TCustomSciTextEditor.GetWrapStartIndent(): Integer;
begin
  Result := SendScintillaEditorMessage(2465, 0, 0);
end;

procedure TCustomSciTextEditor.SetWrapIndentMode(AWrapIndentMode: Integer);
begin
  SendScintillaEditorMessage(2472, WPARAM(AWrapIndentMode), 0);
end;

function TCustomSciTextEditor.GetWrapIndentMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2473, 0, 0);
end;

procedure TCustomSciTextEditor.SetLayoutCache(ACacheMode: Integer);
begin
  SendScintillaEditorMessage(2272, WPARAM(ACacheMode), 0);
end;

function TCustomSciTextEditor.GetLayoutCache(): Integer;
begin
  Result := SendScintillaEditorMessage(2273, 0, 0);
end;

procedure TCustomSciTextEditor.SetScrollWidth(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(2274, WPARAM(APixelWidth), 0);
end;

function TCustomSciTextEditor.GetScrollWidth(): Integer;
begin
  Result := SendScintillaEditorMessage(2275, 0, 0);
end;

procedure TCustomSciTextEditor.SetScrollWidthTracking(ATracking: Boolean);
begin
  SendScintillaEditorMessage(2516, WPARAM(ATracking), 0);
end;

function TCustomSciTextEditor.GetScrollWidthTracking(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2517, 0, 0));
end;

function TCustomSciTextEditor.TextWidth(AStyle: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2276, WPARAM(AStyle), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetEndAtLastLine(AEndAtLastLine: Boolean);
begin
  SendScintillaEditorMessage(2277, WPARAM(AEndAtLastLine), 0);
end;

function TCustomSciTextEditor.GetEndAtLastLine(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2278, 0, 0));
end;

function TCustomSciTextEditor.TextHeight(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2279, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.SetVScrollBar(AVisible: Boolean);
begin
  SendScintillaEditorMessage(2280, WPARAM(AVisible), 0);
end;

function TCustomSciTextEditor.GetVScrollBar(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2281, 0, 0));
end;

procedure TCustomSciTextEditor.AppendText(ALength: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2282, WPARAM(ALength), LPARAM(AText));
end;

function TCustomSciTextEditor.GetPhasesDraw(): Integer;
begin
  Result := SendScintillaEditorMessage(2673, 0, 0);
end;

procedure TCustomSciTextEditor.SetPhasesDraw(APhases: Integer);
begin
  SendScintillaEditorMessage(2674, WPARAM(APhases), 0);
end;

procedure TCustomSciTextEditor.SetFontQuality(AFontQuality: Integer);
begin
  SendScintillaEditorMessage(2611, WPARAM(AFontQuality), 0);
end;

function TCustomSciTextEditor.GetFontQuality(): Integer;
begin
  Result := SendScintillaEditorMessage(2612, 0, 0);
end;

procedure TCustomSciTextEditor.SetFirstVisibleLine(ADisplayLine: Integer);
begin
  SendScintillaEditorMessage(2613, WPARAM(ADisplayLine), 0);
end;

procedure TCustomSciTextEditor.SetMultiPaste(AMultiPaste: Integer);
begin
  SendScintillaEditorMessage(2614, WPARAM(AMultiPaste), 0);
end;

function TCustomSciTextEditor.GetMultiPaste(): Integer;
begin
  Result := SendScintillaEditorMessage(2615, 0, 0);
end;

function TCustomSciTextEditor.GetTag(ATagNumber: Integer; ATagValue: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2616, WPARAM(ATagNumber), LPARAM(ATagValue));
end;

procedure TCustomSciTextEditor.LinesJoin();
begin
  SendScintillaEditorMessage(2288, 0, 0);
end;

procedure TCustomSciTextEditor.LinesSplit(APixelWidth: Integer);
begin
  SendScintillaEditorMessage(2289, WPARAM(APixelWidth), 0);
end;

procedure TCustomSciTextEditor.SetFoldMarginColour(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(2290, WPARAM(AUseSetting), LPARAM(ABack));
end;

procedure TCustomSciTextEditor.SetFoldMarginHiColour(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(2291, WPARAM(AUseSetting), LPARAM(AFore));
end;

procedure TCustomSciTextEditor.SetAccessibility(AAccessibility: Integer);
begin
  SendScintillaEditorMessage(2702, WPARAM(AAccessibility), 0);
end;

function TCustomSciTextEditor.GetAccessibility(): Integer;
begin
  Result := SendScintillaEditorMessage(2703, 0, 0);
end;

procedure TCustomSciTextEditor.LineDown();
begin
  SendScintillaEditorMessage(2300, 0, 0);
end;

procedure TCustomSciTextEditor.LineDownExtend();
begin
  SendScintillaEditorMessage(2301, 0, 0);
end;

procedure TCustomSciTextEditor.LineUp();
begin
  SendScintillaEditorMessage(2302, 0, 0);
end;

procedure TCustomSciTextEditor.LineUpExtend();
begin
  SendScintillaEditorMessage(2303, 0, 0);
end;

procedure TCustomSciTextEditor.CharLeft();
begin
  SendScintillaEditorMessage(2304, 0, 0);
end;

procedure TCustomSciTextEditor.CharLeftExtend();
begin
  SendScintillaEditorMessage(2305, 0, 0);
end;

procedure TCustomSciTextEditor.CharRight();
begin
  SendScintillaEditorMessage(2306, 0, 0);
end;

procedure TCustomSciTextEditor.CharRightExtend();
begin
  SendScintillaEditorMessage(2307, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeft();
begin
  SendScintillaEditorMessage(2308, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeftExtend();
begin
  SendScintillaEditorMessage(2309, 0, 0);
end;

procedure TCustomSciTextEditor.WordRight();
begin
  SendScintillaEditorMessage(2310, 0, 0);
end;

procedure TCustomSciTextEditor.WordRightExtend();
begin
  SendScintillaEditorMessage(2311, 0, 0);
end;

procedure TCustomSciTextEditor.Home();
begin
  SendScintillaEditorMessage(2312, 0, 0);
end;

procedure TCustomSciTextEditor.HomeExtend();
begin
  SendScintillaEditorMessage(2313, 0, 0);
end;

procedure TCustomSciTextEditor.LineEnd();
begin
  SendScintillaEditorMessage(2314, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndExtend();
begin
  SendScintillaEditorMessage(2315, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentStart();
begin
  SendScintillaEditorMessage(2316, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentStartExtend();
begin
  SendScintillaEditorMessage(2317, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentEnd();
begin
  SendScintillaEditorMessage(2318, 0, 0);
end;

procedure TCustomSciTextEditor.DocumentEndExtend();
begin
  SendScintillaEditorMessage(2319, 0, 0);
end;

procedure TCustomSciTextEditor.PageUp();
begin
  SendScintillaEditorMessage(2320, 0, 0);
end;

procedure TCustomSciTextEditor.PageUpExtend();
begin
  SendScintillaEditorMessage(2321, 0, 0);
end;

procedure TCustomSciTextEditor.PageDown();
begin
  SendScintillaEditorMessage(2322, 0, 0);
end;

procedure TCustomSciTextEditor.PageDownExtend();
begin
  SendScintillaEditorMessage(2323, 0, 0);
end;

procedure TCustomSciTextEditor.EditToggleOvertype();
begin
  SendScintillaEditorMessage(2324, 0, 0);
end;

procedure TCustomSciTextEditor.Cancel();
begin
  SendScintillaEditorMessage(2325, 0, 0);
end;

procedure TCustomSciTextEditor.DeleteBack();
begin
  SendScintillaEditorMessage(2326, 0, 0);
end;

procedure TCustomSciTextEditor.Tab();
begin
  SendScintillaEditorMessage(2327, 0, 0);
end;

procedure TCustomSciTextEditor.LineIndent();
begin
  SendScintillaEditorMessage(2813, 0, 0);
end;

procedure TCustomSciTextEditor.BackTab();
begin
  SendScintillaEditorMessage(2328, 0, 0);
end;

procedure TCustomSciTextEditor.LineDedent();
begin
  SendScintillaEditorMessage(2814, 0, 0);
end;

procedure TCustomSciTextEditor.NewLine();
begin
  SendScintillaEditorMessage(2329, 0, 0);
end;

procedure TCustomSciTextEditor.FormFeed();
begin
  SendScintillaEditorMessage(2330, 0, 0);
end;

procedure TCustomSciTextEditor.VCHome();
begin
  SendScintillaEditorMessage(2331, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeExtend();
begin
  SendScintillaEditorMessage(2332, 0, 0);
end;

procedure TCustomSciTextEditor.ZoomIn();
begin
  SendScintillaEditorMessage(2333, 0, 0);
end;

procedure TCustomSciTextEditor.ZoomOut();
begin
  SendScintillaEditorMessage(2334, 0, 0);
end;

procedure TCustomSciTextEditor.DelWordLeft();
begin
  SendScintillaEditorMessage(2335, 0, 0);
end;

procedure TCustomSciTextEditor.DelWordRight();
begin
  SendScintillaEditorMessage(2336, 0, 0);
end;

procedure TCustomSciTextEditor.DelWordRightEnd();
begin
  SendScintillaEditorMessage(2518, 0, 0);
end;

procedure TCustomSciTextEditor.LineCut();
begin
  SendScintillaEditorMessage(2337, 0, 0);
end;

procedure TCustomSciTextEditor.LineDelete();
begin
  SendScintillaEditorMessage(2338, 0, 0);
end;

procedure TCustomSciTextEditor.LineTranspose();
begin
  SendScintillaEditorMessage(2339, 0, 0);
end;

procedure TCustomSciTextEditor.LineReverse();
begin
  SendScintillaEditorMessage(2354, 0, 0);
end;

procedure TCustomSciTextEditor.LineDuplicate();
begin
  SendScintillaEditorMessage(2404, 0, 0);
end;

procedure TCustomSciTextEditor.LowerCase();
begin
  SendScintillaEditorMessage(2340, 0, 0);
end;

procedure TCustomSciTextEditor.UpperCase();
begin
  SendScintillaEditorMessage(2341, 0, 0);
end;

procedure TCustomSciTextEditor.LineScrollDown();
begin
  SendScintillaEditorMessage(2342, 0, 0);
end;

procedure TCustomSciTextEditor.LineScrollUp();
begin
  SendScintillaEditorMessage(2343, 0, 0);
end;

procedure TCustomSciTextEditor.DeleteBackNotLine();
begin
  SendScintillaEditorMessage(2344, 0, 0);
end;

procedure TCustomSciTextEditor.HomeDisplay();
begin
  SendScintillaEditorMessage(2345, 0, 0);
end;

procedure TCustomSciTextEditor.HomeDisplayExtend();
begin
  SendScintillaEditorMessage(2346, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndDisplay();
begin
  SendScintillaEditorMessage(2347, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndDisplayExtend();
begin
  SendScintillaEditorMessage(2348, 0, 0);
end;

procedure TCustomSciTextEditor.HomeWrap();
begin
  SendScintillaEditorMessage(2349, 0, 0);
end;

procedure TCustomSciTextEditor.HomeWrapExtend();
begin
  SendScintillaEditorMessage(2450, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndWrap();
begin
  SendScintillaEditorMessage(2451, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndWrapExtend();
begin
  SendScintillaEditorMessage(2452, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeWrap();
begin
  SendScintillaEditorMessage(2453, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeWrapExtend();
begin
  SendScintillaEditorMessage(2454, 0, 0);
end;

procedure TCustomSciTextEditor.LineCopy();
begin
  SendScintillaEditorMessage(2455, 0, 0);
end;

procedure TCustomSciTextEditor.MoveCaretInsideView();
begin
  SendScintillaEditorMessage(2401, 0, 0);
end;

function TCustomSciTextEditor.LineLength(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2350, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.BraceHighlight(APosA: Integer; APosB: Integer);
begin
  SendScintillaEditorMessage(2351, WPARAM(APosA), LPARAM(APosB));
end;

procedure TCustomSciTextEditor.BraceHighlightIndicator(AUseSetting: Boolean; AIndicator: Integer);
begin
  SendScintillaEditorMessage(2498, WPARAM(AUseSetting), LPARAM(AIndicator));
end;

procedure TCustomSciTextEditor.BraceBadLight(APos: Integer);
begin
  SendScintillaEditorMessage(2352, WPARAM(APos), 0);
end;

procedure TCustomSciTextEditor.BraceBadLightIndicator(AUseSetting: Boolean; AIndicator: Integer);
begin
  SendScintillaEditorMessage(2499, WPARAM(AUseSetting), LPARAM(AIndicator));
end;

function TCustomSciTextEditor.BraceMatch(APos: Integer; AMaxReStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2353, WPARAM(APos), LPARAM(AMaxReStyle));
end;

function TCustomSciTextEditor.BraceMatchNext(APos: Integer; AStartPos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2369, WPARAM(APos), LPARAM(AStartPos));
end;

function TCustomSciTextEditor.GetViewEOL(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2355, 0, 0));
end;

procedure TCustomSciTextEditor.SetViewEOL(AVisible: Boolean);
begin
  SendScintillaEditorMessage(2356, WPARAM(AVisible), 0);
end;

function TCustomSciTextEditor.GetDocPointer(): Integer;
begin
  Result := SendScintillaEditorMessage(2357, 0, 0);
end;

procedure TCustomSciTextEditor.SetDocPointer(ADoc: Integer);
begin
  SendScintillaEditorMessage(2358, 0, LPARAM(ADoc));
end;

procedure TCustomSciTextEditor.SetModEventMask(AEventMask: Integer);
begin
  SendScintillaEditorMessage(2359, WPARAM(AEventMask), 0);
end;

function TCustomSciTextEditor.GetEdgeColumn(): Integer;
begin
  Result := SendScintillaEditorMessage(2360, 0, 0);
end;

procedure TCustomSciTextEditor.SetEdgeColumn(AColumn: Integer);
begin
  SendScintillaEditorMessage(2361, WPARAM(AColumn), 0);
end;

function TCustomSciTextEditor.GetEdgeMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2362, 0, 0);
end;

procedure TCustomSciTextEditor.SetEdgeMode(AEdgeMode: Integer);
begin
  SendScintillaEditorMessage(2363, WPARAM(AEdgeMode), 0);
end;

function TCustomSciTextEditor.GetEdgeColour(): TColor;
begin
  Result := SendScintillaEditorMessage(2364, 0, 0);
end;

procedure TCustomSciTextEditor.SetEdgeColour(AEdgeColour: TColor);
begin
  SendScintillaEditorMessage(2365, WPARAM(AEdgeColour), 0);
end;

procedure TCustomSciTextEditor.MultiEdgeAddLine(AColumn: Integer; AEdgeColour: TColor);
begin
  SendScintillaEditorMessage(2694, WPARAM(AColumn), LPARAM(AEdgeColour));
end;

procedure TCustomSciTextEditor.MultiEdgeClearAll();
begin
  SendScintillaEditorMessage(2695, 0, 0);
end;

function TCustomSciTextEditor.GetMultiEdgeColumn(AWhich: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2749, WPARAM(AWhich), 0);
end;

procedure TCustomSciTextEditor.SearchAnchor();
begin
  SendScintillaEditorMessage(2366, 0, 0);
end;

function TCustomSciTextEditor.SearchNext(ASearchFlags: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2367, WPARAM(ASearchFlags), LPARAM(AText));
end;

function TCustomSciTextEditor.SearchPrev(ASearchFlags: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2368, WPARAM(ASearchFlags), LPARAM(AText));
end;

function TCustomSciTextEditor.LinesOnScreen(): Integer;
begin
  Result := SendScintillaEditorMessage(2370, 0, 0);
end;

procedure TCustomSciTextEditor.UsePopUp(APopUpMode: Integer);
begin
  SendScintillaEditorMessage(2371, WPARAM(APopUpMode), 0);
end;

function TCustomSciTextEditor.SelectionIsRectangle(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2372, 0, 0));
end;

procedure TCustomSciTextEditor.SetZoom(AZoomInPoints: Integer);
begin
  SendScintillaEditorMessage(2373, WPARAM(AZoomInPoints), 0);
end;

function TCustomSciTextEditor.GetZoom(): Integer;
begin
  Result := SendScintillaEditorMessage(2374, 0, 0);
end;

function TCustomSciTextEditor.CreateDocument(ABytes: Integer; ADocumentOptions: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2375, WPARAM(ABytes), LPARAM(ADocumentOptions));
end;

procedure TCustomSciTextEditor.AddRefDocument(ADoc: Integer);
begin
  SendScintillaEditorMessage(2376, 0, LPARAM(ADoc));
end;

procedure TCustomSciTextEditor.ReleaseDocument(ADoc: Integer);
begin
  SendScintillaEditorMessage(2377, 0, LPARAM(ADoc));
end;

function TCustomSciTextEditor.GetDocumentOptions(): Integer;
begin
  Result := SendScintillaEditorMessage(2379, 0, 0);
end;

function TCustomSciTextEditor.GetModEventMask(): Integer;
begin
  Result := SendScintillaEditorMessage(2378, 0, 0);
end;

procedure TCustomSciTextEditor.SetCommandEvents(ACommandEvents: Boolean);
begin
  SendScintillaEditorMessage(2717, WPARAM(ACommandEvents), 0);
end;

function TCustomSciTextEditor.GetCommandEvents(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2718, 0, 0));
end;

procedure TCustomSciTextEditor.SetFocus(AFocus: Boolean);
begin
  SendScintillaEditorMessage(2380, WPARAM(AFocus), 0);
end;

function TCustomSciTextEditor.GetFocus(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2381, 0, 0));
end;

procedure TCustomSciTextEditor.SetStatus(AStatus: Integer);
begin
  SendScintillaEditorMessage(2382, WPARAM(AStatus), 0);
end;

function TCustomSciTextEditor.GetStatus(): Integer;
begin
  Result := SendScintillaEditorMessage(2383, 0, 0);
end;

procedure TCustomSciTextEditor.SetMouseDownCaptures(ACaptures: Boolean);
begin
  SendScintillaEditorMessage(2384, WPARAM(ACaptures), 0);
end;

function TCustomSciTextEditor.GetMouseDownCaptures(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2385, 0, 0));
end;

procedure TCustomSciTextEditor.SetMouseWheelCaptures(ACaptures: Boolean);
begin
  SendScintillaEditorMessage(2696, WPARAM(ACaptures), 0);
end;

function TCustomSciTextEditor.GetMouseWheelCaptures(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2697, 0, 0));
end;

procedure TCustomSciTextEditor.SetCursor(ACursorType: Integer);
begin
  SendScintillaEditorMessage(2386, WPARAM(ACursorType), 0);
end;

function TCustomSciTextEditor.GetCursor(): Integer;
begin
  Result := SendScintillaEditorMessage(2387, 0, 0);
end;

procedure TCustomSciTextEditor.SetControlCharSymbol(ASymbol: Integer);
begin
  SendScintillaEditorMessage(2388, WPARAM(ASymbol), 0);
end;

function TCustomSciTextEditor.GetControlCharSymbol(): Integer;
begin
  Result := SendScintillaEditorMessage(2389, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartLeft();
begin
  SendScintillaEditorMessage(2390, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartLeftExtend();
begin
  SendScintillaEditorMessage(2391, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartRight();
begin
  SendScintillaEditorMessage(2392, 0, 0);
end;

procedure TCustomSciTextEditor.WordPartRightExtend();
begin
  SendScintillaEditorMessage(2393, 0, 0);
end;

procedure TCustomSciTextEditor.SetVisiblePolicy(AVisiblePolicy: Integer; AVisibleSlop: Integer);
begin
  SendScintillaEditorMessage(2394, WPARAM(AVisiblePolicy), LPARAM(AVisibleSlop));
end;

procedure TCustomSciTextEditor.DelLineLeft();
begin
  SendScintillaEditorMessage(2395, 0, 0);
end;

procedure TCustomSciTextEditor.DelLineRight();
begin
  SendScintillaEditorMessage(2396, 0, 0);
end;

procedure TCustomSciTextEditor.SetXOffset(AXOffset: Integer);
begin
  SendScintillaEditorMessage(2397, WPARAM(AXOffset), 0);
end;

function TCustomSciTextEditor.GetXOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(2398, 0, 0);
end;

procedure TCustomSciTextEditor.ChooseCaretX();
begin
  SendScintillaEditorMessage(2399, 0, 0);
end;

procedure TCustomSciTextEditor.GrabFocus();
begin
  SendScintillaEditorMessage(2400, 0, 0);
end;

procedure TCustomSciTextEditor.SetXCaretPolicy(ACaretPolicy: Integer; ACaretSlop: Integer);
begin
  SendScintillaEditorMessage(2402, WPARAM(ACaretPolicy), LPARAM(ACaretSlop));
end;

procedure TCustomSciTextEditor.SetYCaretPolicy(ACaretPolicy: Integer; ACaretSlop: Integer);
begin
  SendScintillaEditorMessage(2403, WPARAM(ACaretPolicy), LPARAM(ACaretSlop));
end;

procedure TCustomSciTextEditor.SetPrintWrapMode(AWrapMode: Integer);
begin
  SendScintillaEditorMessage(2406, WPARAM(AWrapMode), 0);
end;

function TCustomSciTextEditor.GetPrintWrapMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2407, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotActiveFore(AUseSetting: Boolean; AFore: TColor);
begin
  SendScintillaEditorMessage(2410, WPARAM(AUseSetting), LPARAM(AFore));
end;

function TCustomSciTextEditor.GetHotspotActiveFore(): TColor;
begin
  Result := SendScintillaEditorMessage(2494, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotActiveBack(AUseSetting: Boolean; ABack: TColor);
begin
  SendScintillaEditorMessage(2411, WPARAM(AUseSetting), LPARAM(ABack));
end;

function TCustomSciTextEditor.GetHotspotActiveBack(): TColor;
begin
  Result := SendScintillaEditorMessage(2495, 0, 0);
end;

procedure TCustomSciTextEditor.SetHotspotActiveUnderline(AUnderline: Boolean);
begin
  SendScintillaEditorMessage(2412, WPARAM(AUnderline), 0);
end;

function TCustomSciTextEditor.GetHotspotActiveUnderline(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2496, 0, 0));
end;

procedure TCustomSciTextEditor.SetHotspotSingleLine(ASingleLine: Boolean);
begin
  SendScintillaEditorMessage(2421, WPARAM(ASingleLine), 0);
end;

function TCustomSciTextEditor.GetHotspotSingleLine(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2497, 0, 0));
end;

procedure TCustomSciTextEditor.ParaDown();
begin
  SendScintillaEditorMessage(2413, 0, 0);
end;

procedure TCustomSciTextEditor.ParaDownExtend();
begin
  SendScintillaEditorMessage(2414, 0, 0);
end;

procedure TCustomSciTextEditor.ParaUp();
begin
  SendScintillaEditorMessage(2415, 0, 0);
end;

procedure TCustomSciTextEditor.ParaUpExtend();
begin
  SendScintillaEditorMessage(2416, 0, 0);
end;

function TCustomSciTextEditor.PositionBefore(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2417, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.PositionAfter(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2418, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.PositionRelative(APos: Integer; ARelative: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2670, WPARAM(APos), LPARAM(ARelative));
end;

function TCustomSciTextEditor.PositionRelativeCodeUnits(APos: Integer; ARelative: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2716, WPARAM(APos), LPARAM(ARelative));
end;

procedure TCustomSciTextEditor.CopyRange(AStart: Integer; AEnd: Integer);
begin
  SendScintillaEditorMessage(2419, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.CopyText(ALength: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2420, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SetSelectionMode(ASelectionMode: Integer);
begin
  SendScintillaEditorMessage(2422, WPARAM(ASelectionMode), 0);
end;

procedure TCustomSciTextEditor.ChangeSelectionMode(ASelectionMode: Integer);
begin
  SendScintillaEditorMessage(2659, WPARAM(ASelectionMode), 0);
end;

function TCustomSciTextEditor.GetSelectionMode(): Integer;
begin
  Result := SendScintillaEditorMessage(2423, 0, 0);
end;

procedure TCustomSciTextEditor.SetMoveExtendsSelection(AMoveExtendsSelection: Boolean);
begin
  SendScintillaEditorMessage(2719, WPARAM(AMoveExtendsSelection), 0);
end;

function TCustomSciTextEditor.GetMoveExtendsSelection(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2706, 0, 0));
end;

function TCustomSciTextEditor.GetLineSelStartPosition(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2424, WPARAM(ALine), 0);
end;

function TCustomSciTextEditor.GetLineSelEndPosition(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2425, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.LineDownRectExtend();
begin
  SendScintillaEditorMessage(2426, 0, 0);
end;

procedure TCustomSciTextEditor.LineUpRectExtend();
begin
  SendScintillaEditorMessage(2427, 0, 0);
end;

procedure TCustomSciTextEditor.CharLeftRectExtend();
begin
  SendScintillaEditorMessage(2428, 0, 0);
end;

procedure TCustomSciTextEditor.CharRightRectExtend();
begin
  SendScintillaEditorMessage(2429, 0, 0);
end;

procedure TCustomSciTextEditor.HomeRectExtend();
begin
  SendScintillaEditorMessage(2430, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeRectExtend();
begin
  SendScintillaEditorMessage(2431, 0, 0);
end;

procedure TCustomSciTextEditor.LineEndRectExtend();
begin
  SendScintillaEditorMessage(2432, 0, 0);
end;

procedure TCustomSciTextEditor.PageUpRectExtend();
begin
  SendScintillaEditorMessage(2433, 0, 0);
end;

procedure TCustomSciTextEditor.PageDownRectExtend();
begin
  SendScintillaEditorMessage(2434, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageUp();
begin
  SendScintillaEditorMessage(2435, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageUpExtend();
begin
  SendScintillaEditorMessage(2436, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageDown();
begin
  SendScintillaEditorMessage(2437, 0, 0);
end;

procedure TCustomSciTextEditor.StutteredPageDownExtend();
begin
  SendScintillaEditorMessage(2438, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeftEnd();
begin
  SendScintillaEditorMessage(2439, 0, 0);
end;

procedure TCustomSciTextEditor.WordLeftEndExtend();
begin
  SendScintillaEditorMessage(2440, 0, 0);
end;

procedure TCustomSciTextEditor.WordRightEnd();
begin
  SendScintillaEditorMessage(2441, 0, 0);
end;

procedure TCustomSciTextEditor.WordRightEndExtend();
begin
  SendScintillaEditorMessage(2442, 0, 0);
end;

procedure TCustomSciTextEditor.SetWhitespaceChars(ACharacters: PAnsiChar);
begin
  SendScintillaEditorMessage(2443, 0, LPARAM(ACharacters));
end;

function TCustomSciTextEditor.GetWhitespaceChars(ACharacters: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2647, 0, LPARAM(ACharacters));
end;

procedure TCustomSciTextEditor.SetPunctuationChars(ACharacters: PAnsiChar);
begin
  SendScintillaEditorMessage(2648, 0, LPARAM(ACharacters));
end;

function TCustomSciTextEditor.GetPunctuationChars(ACharacters: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2649, 0, LPARAM(ACharacters));
end;

procedure TCustomSciTextEditor.SetCharsDefault();
begin
  SendScintillaEditorMessage(2444, 0, 0);
end;

function TCustomSciTextEditor.AutoCGetCurrent(): Integer;
begin
  Result := SendScintillaEditorMessage(2445, 0, 0);
end;

function TCustomSciTextEditor.AutoCGetCurrentText(AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2610, 0, LPARAM(AText));
end;

procedure TCustomSciTextEditor.AutoCSetCaseInsensitiveBehaviour(ABehaviour: Integer);
begin
  SendScintillaEditorMessage(2634, WPARAM(ABehaviour), 0);
end;

function TCustomSciTextEditor.AutoCGetCaseInsensitiveBehaviour(): Integer;
begin
  Result := SendScintillaEditorMessage(2635, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetMulti(AMulti: Integer);
begin
  SendScintillaEditorMessage(2636, WPARAM(AMulti), 0);
end;

function TCustomSciTextEditor.AutoCGetMulti(): Integer;
begin
  Result := SendScintillaEditorMessage(2637, 0, 0);
end;

procedure TCustomSciTextEditor.AutoCSetOrder(AOrder: Integer);
begin
  SendScintillaEditorMessage(2660, WPARAM(AOrder), 0);
end;

function TCustomSciTextEditor.AutoCGetOrder(): Integer;
begin
  Result := SendScintillaEditorMessage(2661, 0, 0);
end;

procedure TCustomSciTextEditor.Allocate(ABytes: Integer);
begin
  SendScintillaEditorMessage(2446, WPARAM(ABytes), 0);
end;

function TCustomSciTextEditor.TargetAsUTF8(S: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2447, 0, LPARAM(S));
end;

procedure TCustomSciTextEditor.SetLengthForEncode(ABytes: Integer);
begin
  SendScintillaEditorMessage(2448, WPARAM(ABytes), 0);
end;

function TCustomSciTextEditor.EncodedFromUTF8(AUtf8: PAnsiChar; AEncoded: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2449, WPARAM(AUtf8), LPARAM(AEncoded));
end;

function TCustomSciTextEditor.FindColumn(ALine: Integer; AColumn: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2456, WPARAM(ALine), LPARAM(AColumn));
end;

function TCustomSciTextEditor.GetCaretSticky(): Integer;
begin
  Result := SendScintillaEditorMessage(2457, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretSticky(AUseCaretStickyBehaviour: Integer);
begin
  SendScintillaEditorMessage(2458, WPARAM(AUseCaretStickyBehaviour), 0);
end;

procedure TCustomSciTextEditor.ToggleCaretSticky();
begin
  SendScintillaEditorMessage(2459, 0, 0);
end;

procedure TCustomSciTextEditor.SetPasteConvertEndings(AConvert: Boolean);
begin
  SendScintillaEditorMessage(2467, WPARAM(AConvert), 0);
end;

function TCustomSciTextEditor.GetPasteConvertEndings(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2468, 0, 0));
end;

procedure TCustomSciTextEditor.ReplaceRectangular(ALength: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2771, WPARAM(ALength), LPARAM(AText));
end;

procedure TCustomSciTextEditor.SelectionDuplicate();
begin
  SendScintillaEditorMessage(2469, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretLineBackAlpha(AAlpha: Integer);
begin
  SendScintillaEditorMessage(2470, WPARAM(AAlpha), 0);
end;

function TCustomSciTextEditor.GetCaretLineBackAlpha(): Integer;
begin
  Result := SendScintillaEditorMessage(2471, 0, 0);
end;

procedure TCustomSciTextEditor.SetCaretStyle(ACaretStyle: Integer);
begin
  SendScintillaEditorMessage(2512, WPARAM(ACaretStyle), 0);
end;

function TCustomSciTextEditor.GetCaretStyle(): Integer;
begin
  Result := SendScintillaEditorMessage(2513, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndicatorCurrent(AIndicator: Integer);
begin
  SendScintillaEditorMessage(2500, WPARAM(AIndicator), 0);
end;

function TCustomSciTextEditor.GetIndicatorCurrent(): Integer;
begin
  Result := SendScintillaEditorMessage(2501, 0, 0);
end;

procedure TCustomSciTextEditor.SetIndicatorValue(AValue: Integer);
begin
  SendScintillaEditorMessage(2502, WPARAM(AValue), 0);
end;

function TCustomSciTextEditor.GetIndicatorValue(): Integer;
begin
  Result := SendScintillaEditorMessage(2503, 0, 0);
end;

procedure TCustomSciTextEditor.IndicatorFillRange(AStart: Integer; ALengthFill: Integer);
begin
  SendScintillaEditorMessage(2504, WPARAM(AStart), LPARAM(ALengthFill));
end;

procedure TCustomSciTextEditor.IndicatorClearRange(AStart: Integer; ALengthClear: Integer);
begin
  SendScintillaEditorMessage(2505, WPARAM(AStart), LPARAM(ALengthClear));
end;

function TCustomSciTextEditor.IndicatorAllOnFor(APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2506, WPARAM(APos), 0);
end;

function TCustomSciTextEditor.IndicatorValueAt(AIndicator: Integer; APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2507, WPARAM(AIndicator), LPARAM(APos));
end;

function TCustomSciTextEditor.IndicatorStart(AIndicator: Integer; APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2508, WPARAM(AIndicator), LPARAM(APos));
end;

function TCustomSciTextEditor.IndicatorEnd(AIndicator: Integer; APos: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2509, WPARAM(AIndicator), LPARAM(APos));
end;

procedure TCustomSciTextEditor.SetPositionCache(ASize: Integer);
begin
  SendScintillaEditorMessage(2514, WPARAM(ASize), 0);
end;

function TCustomSciTextEditor.GetPositionCache(): Integer;
begin
  Result := SendScintillaEditorMessage(2515, 0, 0);
end;

procedure TCustomSciTextEditor.SetLayoutThreads(AThreads: Integer);
begin
  SendScintillaEditorMessage(2775, WPARAM(AThreads), 0);
end;

function TCustomSciTextEditor.GetLayoutThreads(): Integer;
begin
  Result := SendScintillaEditorMessage(2776, 0, 0);
end;

procedure TCustomSciTextEditor.CopyAllowLine();
begin
  SendScintillaEditorMessage(2519, 0, 0);
end;

procedure TCustomSciTextEditor.CutAllowLine();
begin
  SendScintillaEditorMessage(2810, 0, 0);
end;

procedure TCustomSciTextEditor.SetCopySeparator(ASeparator: PAnsiChar);
begin
  SendScintillaEditorMessage(2811, 0, LPARAM(ASeparator));
end;

function TCustomSciTextEditor.GetCopySeparator(ASeparator: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2812, 0, LPARAM(ASeparator));
end;

function TCustomSciTextEditor.GetCharacterPointer(): Integer;
begin
  Result := SendScintillaEditorMessage(2520, 0, 0);
end;

function TCustomSciTextEditor.GetRangePointer(AStart: Integer; ALengthRange: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2643, WPARAM(AStart), LPARAM(ALengthRange));
end;

function TCustomSciTextEditor.GetGapPosition(): Integer;
begin
  Result := SendScintillaEditorMessage(2644, 0, 0);
end;

procedure TCustomSciTextEditor.IndicSetAlpha(AIndicator: Integer; AAlpha: Integer);
begin
  SendScintillaEditorMessage(2523, WPARAM(AIndicator), LPARAM(AAlpha));
end;

function TCustomSciTextEditor.IndicGetAlpha(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2524, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.IndicSetOutlineAlpha(AIndicator: Integer; AAlpha: Integer);
begin
  SendScintillaEditorMessage(2558, WPARAM(AIndicator), LPARAM(AAlpha));
end;

function TCustomSciTextEditor.IndicGetOutlineAlpha(AIndicator: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2559, WPARAM(AIndicator), 0);
end;

procedure TCustomSciTextEditor.SetExtraAscent(AExtraAscent: Integer);
begin
  SendScintillaEditorMessage(2525, WPARAM(AExtraAscent), 0);
end;

function TCustomSciTextEditor.GetExtraAscent(): Integer;
begin
  Result := SendScintillaEditorMessage(2526, 0, 0);
end;

procedure TCustomSciTextEditor.SetExtraDescent(AExtraDescent: Integer);
begin
  SendScintillaEditorMessage(2527, WPARAM(AExtraDescent), 0);
end;

function TCustomSciTextEditor.GetExtraDescent(): Integer;
begin
  Result := SendScintillaEditorMessage(2528, 0, 0);
end;

function TCustomSciTextEditor.MarkerSymbolDefined(AMarkerNumber: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2529, WPARAM(AMarkerNumber), 0);
end;

procedure TCustomSciTextEditor.MarginSetText(ALine: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2530, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.MarginGetText(ALine: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2531, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.MarginSetStyle(ALine: Integer; AStyle: Integer);
begin
  SendScintillaEditorMessage(2532, WPARAM(ALine), LPARAM(AStyle));
end;

function TCustomSciTextEditor.MarginGetStyle(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2533, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.MarginSetStyles(ALine: Integer; AStyles: PAnsiChar);
begin
  SendScintillaEditorMessage(2534, WPARAM(ALine), LPARAM(AStyles));
end;

function TCustomSciTextEditor.MarginGetStyles(ALine: Integer; AStyles: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2535, WPARAM(ALine), LPARAM(AStyles));
end;

procedure TCustomSciTextEditor.MarginTextClearAll();
begin
  SendScintillaEditorMessage(2536, 0, 0);
end;

procedure TCustomSciTextEditor.MarginSetStyleOffset(AStyle: Integer);
begin
  SendScintillaEditorMessage(2537, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.MarginGetStyleOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(2538, 0, 0);
end;

procedure TCustomSciTextEditor.SetMarginOptions(AMarginOptions: Integer);
begin
  SendScintillaEditorMessage(2539, WPARAM(AMarginOptions), 0);
end;

function TCustomSciTextEditor.GetMarginOptions(): Integer;
begin
  Result := SendScintillaEditorMessage(2557, 0, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetText(ALine: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2540, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.AnnotationGetText(ALine: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2541, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.AnnotationSetStyle(ALine: Integer; AStyle: Integer);
begin
  SendScintillaEditorMessage(2542, WPARAM(ALine), LPARAM(AStyle));
end;

function TCustomSciTextEditor.AnnotationGetStyle(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2543, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.AnnotationSetStyles(ALine: Integer; AStyles: PAnsiChar);
begin
  SendScintillaEditorMessage(2544, WPARAM(ALine), LPARAM(AStyles));
end;

function TCustomSciTextEditor.AnnotationGetStyles(ALine: Integer; AStyles: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2545, WPARAM(ALine), LPARAM(AStyles));
end;

function TCustomSciTextEditor.AnnotationGetLines(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2546, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.AnnotationClearAll();
begin
  SendScintillaEditorMessage(2547, 0, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetVisible(AVisible: Integer);
begin
  SendScintillaEditorMessage(2548, WPARAM(AVisible), 0);
end;

function TCustomSciTextEditor.AnnotationGetVisible(): Integer;
begin
  Result := SendScintillaEditorMessage(2549, 0, 0);
end;

procedure TCustomSciTextEditor.AnnotationSetStyleOffset(AStyle: Integer);
begin
  SendScintillaEditorMessage(2550, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.AnnotationGetStyleOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(2551, 0, 0);
end;

procedure TCustomSciTextEditor.ReleaseAllExtendedStyles();
begin
  SendScintillaEditorMessage(2552, 0, 0);
end;

function TCustomSciTextEditor.AllocateExtendedStyles(ANumberStyles: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2553, WPARAM(ANumberStyles), 0);
end;

procedure TCustomSciTextEditor.AddUndoAction(AToken: Integer; AFlags: Integer);
begin
  SendScintillaEditorMessage(2560, WPARAM(AToken), LPARAM(AFlags));
end;

function TCustomSciTextEditor.CharPositionFromPoint(X: Integer; Y: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2561, WPARAM(X), LPARAM(Y));
end;

function TCustomSciTextEditor.CharPositionFromPointClose(X: Integer; Y: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2562, WPARAM(X), LPARAM(Y));
end;

procedure TCustomSciTextEditor.SetMouseSelectionRectangularSwitch(AMouseSelectionRectangularSwitch: Boolean);
begin
  SendScintillaEditorMessage(2668, WPARAM(AMouseSelectionRectangularSwitch), 0);
end;

function TCustomSciTextEditor.GetMouseSelectionRectangularSwitch(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2669, 0, 0));
end;

procedure TCustomSciTextEditor.SetMultipleSelection(AMultipleSelection: Boolean);
begin
  SendScintillaEditorMessage(2563, WPARAM(AMultipleSelection), 0);
end;

function TCustomSciTextEditor.GetMultipleSelection(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2564, 0, 0));
end;

procedure TCustomSciTextEditor.SetAdditionalSelectionTyping(AAdditionalSelectionTyping: Boolean);
begin
  SendScintillaEditorMessage(2565, WPARAM(AAdditionalSelectionTyping), 0);
end;

function TCustomSciTextEditor.GetAdditionalSelectionTyping(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2566, 0, 0));
end;

procedure TCustomSciTextEditor.SetAdditionalCaretsBlink(AAdditionalCaretsBlink: Boolean);
begin
  SendScintillaEditorMessage(2567, WPARAM(AAdditionalCaretsBlink), 0);
end;

function TCustomSciTextEditor.GetAdditionalCaretsBlink(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2568, 0, 0));
end;

procedure TCustomSciTextEditor.SetAdditionalCaretsVisible(AAdditionalCaretsVisible: Boolean);
begin
  SendScintillaEditorMessage(2608, WPARAM(AAdditionalCaretsVisible), 0);
end;

function TCustomSciTextEditor.GetAdditionalCaretsVisible(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2609, 0, 0));
end;

function TCustomSciTextEditor.GetSelections(): Integer;
begin
  Result := SendScintillaEditorMessage(2570, 0, 0);
end;

function TCustomSciTextEditor.GetSelectionEmpty(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2650, 0, 0));
end;

procedure TCustomSciTextEditor.ClearSelections();
begin
  SendScintillaEditorMessage(2571, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelection(ACaret: Integer; AAnchor: Integer);
begin
  SendScintillaEditorMessage(2572, WPARAM(ACaret), LPARAM(AAnchor));
end;

procedure TCustomSciTextEditor.AddSelection(ACaret: Integer; AAnchor: Integer);
begin
  SendScintillaEditorMessage(2573, WPARAM(ACaret), LPARAM(AAnchor));
end;

function TCustomSciTextEditor.SelectionFromPoint(X: Integer; Y: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2474, WPARAM(X), LPARAM(Y));
end;

procedure TCustomSciTextEditor.DropSelectionN(ASelection: Integer);
begin
  SendScintillaEditorMessage(2671, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetMainSelection(ASelection: Integer);
begin
  SendScintillaEditorMessage(2574, WPARAM(ASelection), 0);
end;

function TCustomSciTextEditor.GetMainSelection(): Integer;
begin
  Result := SendScintillaEditorMessage(2575, 0, 0);
end;

procedure TCustomSciTextEditor.SetSelectionNCaret(ASelection: Integer; ACaret: Integer);
begin
  SendScintillaEditorMessage(2576, WPARAM(ASelection), LPARAM(ACaret));
end;

function TCustomSciTextEditor.GetSelectionNCaret(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2577, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetSelectionNAnchor(ASelection: Integer; AAnchor: Integer);
begin
  SendScintillaEditorMessage(2578, WPARAM(ASelection), LPARAM(AAnchor));
end;

function TCustomSciTextEditor.GetSelectionNAnchor(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2579, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetSelectionNCaretVirtualSpace(ASelection: Integer; ASpace: Integer);
begin
  SendScintillaEditorMessage(2580, WPARAM(ASelection), LPARAM(ASpace));
end;

function TCustomSciTextEditor.GetSelectionNCaretVirtualSpace(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2581, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetSelectionNAnchorVirtualSpace(ASelection: Integer; ASpace: Integer);
begin
  SendScintillaEditorMessage(2582, WPARAM(ASelection), LPARAM(ASpace));
end;

function TCustomSciTextEditor.GetSelectionNAnchorVirtualSpace(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2583, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetSelectionNStart(ASelection: Integer; AAnchor: Integer);
begin
  SendScintillaEditorMessage(2584, WPARAM(ASelection), LPARAM(AAnchor));
end;

function TCustomSciTextEditor.GetSelectionNStart(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2585, WPARAM(ASelection), 0);
end;

function TCustomSciTextEditor.GetSelectionNStartVirtualSpace(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2726, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetSelectionNEnd(ASelection: Integer; ACaret: Integer);
begin
  SendScintillaEditorMessage(2586, WPARAM(ASelection), LPARAM(ACaret));
end;

function TCustomSciTextEditor.GetSelectionNEndVirtualSpace(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2727, WPARAM(ASelection), 0);
end;

function TCustomSciTextEditor.GetSelectionNEnd(ASelection: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2587, WPARAM(ASelection), 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionCaret(ACaret: Integer);
begin
  SendScintillaEditorMessage(2588, WPARAM(ACaret), 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionCaret(): Integer;
begin
  Result := SendScintillaEditorMessage(2589, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionAnchor(AAnchor: Integer);
begin
  SendScintillaEditorMessage(2590, WPARAM(AAnchor), 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionAnchor(): Integer;
begin
  Result := SendScintillaEditorMessage(2591, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionCaretVirtualSpace(ASpace: Integer);
begin
  SendScintillaEditorMessage(2592, WPARAM(ASpace), 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionCaretVirtualSpace(): Integer;
begin
  Result := SendScintillaEditorMessage(2593, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionAnchorVirtualSpace(ASpace: Integer);
begin
  SendScintillaEditorMessage(2594, WPARAM(ASpace), 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionAnchorVirtualSpace(): Integer;
begin
  Result := SendScintillaEditorMessage(2595, 0, 0);
end;

procedure TCustomSciTextEditor.SetVirtualSpaceOptions(AVirtualSpaceOptions: Integer);
begin
  SendScintillaEditorMessage(2596, WPARAM(AVirtualSpaceOptions), 0);
end;

function TCustomSciTextEditor.GetVirtualSpaceOptions(): Integer;
begin
  Result := SendScintillaEditorMessage(2597, 0, 0);
end;

procedure TCustomSciTextEditor.SetRectangularSelectionModifier(AModifier: Integer);
begin
  SendScintillaEditorMessage(2598, WPARAM(AModifier), 0);
end;

function TCustomSciTextEditor.GetRectangularSelectionModifier(): Integer;
begin
  Result := SendScintillaEditorMessage(2599, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelFore(AFore: TColor);
begin
  SendScintillaEditorMessage(2600, WPARAM(AFore), 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelBack(ABack: TColor);
begin
  SendScintillaEditorMessage(2601, WPARAM(ABack), 0);
end;

procedure TCustomSciTextEditor.SetAdditionalSelAlpha(AAlpha: Integer);
begin
  SendScintillaEditorMessage(2602, WPARAM(AAlpha), 0);
end;

function TCustomSciTextEditor.GetAdditionalSelAlpha(): Integer;
begin
  Result := SendScintillaEditorMessage(2603, 0, 0);
end;

procedure TCustomSciTextEditor.SetAdditionalCaretFore(AFore: TColor);
begin
  SendScintillaEditorMessage(2604, WPARAM(AFore), 0);
end;

function TCustomSciTextEditor.GetAdditionalCaretFore(): TColor;
begin
  Result := SendScintillaEditorMessage(2605, 0, 0);
end;

procedure TCustomSciTextEditor.RotateSelection();
begin
  SendScintillaEditorMessage(2606, 0, 0);
end;

procedure TCustomSciTextEditor.SwapMainAnchorCaret();
begin
  SendScintillaEditorMessage(2607, 0, 0);
end;

procedure TCustomSciTextEditor.MultipleSelectAddNext();
begin
  SendScintillaEditorMessage(2688, 0, 0);
end;

procedure TCustomSciTextEditor.MultipleSelectAddEach();
begin
  SendScintillaEditorMessage(2689, 0, 0);
end;

function TCustomSciTextEditor.ChangeLexerState(AStart: Integer; AEnd: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2617, WPARAM(AStart), LPARAM(AEnd));
end;

function TCustomSciTextEditor.ContractedFoldNext(ALineStart: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2618, WPARAM(ALineStart), 0);
end;

procedure TCustomSciTextEditor.VerticalCentreCaret();
begin
  SendScintillaEditorMessage(2619, 0, 0);
end;

procedure TCustomSciTextEditor.MoveSelectedLinesUp();
begin
  SendScintillaEditorMessage(2620, 0, 0);
end;

procedure TCustomSciTextEditor.MoveSelectedLinesDown();
begin
  SendScintillaEditorMessage(2621, 0, 0);
end;

procedure TCustomSciTextEditor.SetIdentifier(AIdentifier: Integer);
begin
  SendScintillaEditorMessage(2622, WPARAM(AIdentifier), 0);
end;

function TCustomSciTextEditor.GetIdentifier(): Integer;
begin
  Result := SendScintillaEditorMessage(2623, 0, 0);
end;

procedure TCustomSciTextEditor.RGBAImageSetWidth(AWidth: Integer);
begin
  SendScintillaEditorMessage(2624, WPARAM(AWidth), 0);
end;

procedure TCustomSciTextEditor.RGBAImageSetHeight(AHeight: Integer);
begin
  SendScintillaEditorMessage(2625, WPARAM(AHeight), 0);
end;

procedure TCustomSciTextEditor.RGBAImageSetScale(AScalePercent: Integer);
begin
  SendScintillaEditorMessage(2651, WPARAM(AScalePercent), 0);
end;

procedure TCustomSciTextEditor.MarkerDefineRGBAImage(AMarkerNumber: Integer; APixels: PAnsiChar);
begin
  SendScintillaEditorMessage(2626, WPARAM(AMarkerNumber), LPARAM(APixels));
end;

procedure TCustomSciTextEditor.RegisterRGBAImage(AType: Integer; APixels: PAnsiChar);
begin
  SendScintillaEditorMessage(2627, WPARAM(AType), LPARAM(APixels));
end;

procedure TCustomSciTextEditor.ScrollToStart();
begin
  SendScintillaEditorMessage(2628, 0, 0);
end;

procedure TCustomSciTextEditor.ScrollToEnd();
begin
  SendScintillaEditorMessage(2629, 0, 0);
end;

procedure TCustomSciTextEditor.SetTechnology(ATechnology: Integer);
begin
  SendScintillaEditorMessage(2630, WPARAM(ATechnology), 0);
end;

function TCustomSciTextEditor.GetTechnology(): Integer;
begin
  Result := SendScintillaEditorMessage(2631, 0, 0);
end;

function TCustomSciTextEditor.CreateLoader(ABytes: Integer; ADocumentOptions: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2632, WPARAM(ABytes), LPARAM(ADocumentOptions));
end;

procedure TCustomSciTextEditor.FindIndicatorShow(AStart: Integer; AEnd: Integer);
begin
  SendScintillaEditorMessage(2640, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.FindIndicatorFlash(AStart: Integer; AEnd: Integer);
begin
  SendScintillaEditorMessage(2641, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.FindIndicatorHide();
begin
  SendScintillaEditorMessage(2642, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeDisplay();
begin
  SendScintillaEditorMessage(2652, 0, 0);
end;

procedure TCustomSciTextEditor.VCHomeDisplayExtend();
begin
  SendScintillaEditorMessage(2653, 0, 0);
end;

function TCustomSciTextEditor.GetCaretLineVisibleAlways(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2654, 0, 0));
end;

procedure TCustomSciTextEditor.SetCaretLineVisibleAlways(AAlwaysVisible: Boolean);
begin
  SendScintillaEditorMessage(2655, WPARAM(AAlwaysVisible), 0);
end;

procedure TCustomSciTextEditor.SetLineEndTypesAllowed(ALineEndBitSet: Integer);
begin
  SendScintillaEditorMessage(2656, WPARAM(ALineEndBitSet), 0);
end;

function TCustomSciTextEditor.GetLineEndTypesAllowed(): Integer;
begin
  Result := SendScintillaEditorMessage(2657, 0, 0);
end;

function TCustomSciTextEditor.GetLineEndTypesActive(): Integer;
begin
  Result := SendScintillaEditorMessage(2658, 0, 0);
end;

procedure TCustomSciTextEditor.SetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar);
begin
  SendScintillaEditorMessage(2665, WPARAM(AEncodedCharacter), LPARAM(ARepresentation));
end;

function TCustomSciTextEditor.GetRepresentation(AEncodedCharacter: PAnsiChar; ARepresentation: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2666, WPARAM(AEncodedCharacter), LPARAM(ARepresentation));
end;

procedure TCustomSciTextEditor.ClearRepresentation(AEncodedCharacter: PAnsiChar);
begin
  SendScintillaEditorMessage(2667, WPARAM(AEncodedCharacter), 0);
end;

procedure TCustomSciTextEditor.ClearAllRepresentations();
begin
  SendScintillaEditorMessage(2770, 0, 0);
end;

procedure TCustomSciTextEditor.SetRepresentationAppearance(AEncodedCharacter: PAnsiChar; AAppearance: Integer);
begin
  SendScintillaEditorMessage(2766, WPARAM(AEncodedCharacter), LPARAM(AAppearance));
end;

function TCustomSciTextEditor.GetRepresentationAppearance(AEncodedCharacter: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2767, WPARAM(AEncodedCharacter), 0);
end;

procedure TCustomSciTextEditor.SetRepresentationColour(AEncodedCharacter: PAnsiChar; AColour: Integer);
begin
  SendScintillaEditorMessage(2768, WPARAM(AEncodedCharacter), LPARAM(AColour));
end;

function TCustomSciTextEditor.GetRepresentationColour(AEncodedCharacter: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2769, WPARAM(AEncodedCharacter), 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationSetText(ALine: Integer; AText: PAnsiChar);
begin
  SendScintillaEditorMessage(2740, WPARAM(ALine), LPARAM(AText));
end;

function TCustomSciTextEditor.EOLAnnotationGetText(ALine: Integer; AText: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(2741, WPARAM(ALine), LPARAM(AText));
end;

procedure TCustomSciTextEditor.EOLAnnotationSetStyle(ALine: Integer; AStyle: Integer);
begin
  SendScintillaEditorMessage(2742, WPARAM(ALine), LPARAM(AStyle));
end;

function TCustomSciTextEditor.EOLAnnotationGetStyle(ALine: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2743, WPARAM(ALine), 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationClearAll();
begin
  SendScintillaEditorMessage(2744, 0, 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationSetVisible(AVisible: Integer);
begin
  SendScintillaEditorMessage(2745, WPARAM(AVisible), 0);
end;

function TCustomSciTextEditor.EOLAnnotationGetVisible(): Integer;
begin
  Result := SendScintillaEditorMessage(2746, 0, 0);
end;

procedure TCustomSciTextEditor.EOLAnnotationSetStyleOffset(AStyle: Integer);
begin
  SendScintillaEditorMessage(2747, WPARAM(AStyle), 0);
end;

function TCustomSciTextEditor.EOLAnnotationGetStyleOffset(): Integer;
begin
  Result := SendScintillaEditorMessage(2748, 0, 0);
end;

function TCustomSciTextEditor.SupportsFeature(AFeature: Integer): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2750, WPARAM(AFeature), 0));
end;

function TCustomSciTextEditor.GetLineCharacterIndex(): Integer;
begin
  Result := SendScintillaEditorMessage(2710, 0, 0);
end;

procedure TCustomSciTextEditor.AllocateLineCharacterIndex(ALineCharacterIndex: Integer);
begin
  SendScintillaEditorMessage(2711, WPARAM(ALineCharacterIndex), 0);
end;

procedure TCustomSciTextEditor.ReleaseLineCharacterIndex(ALineCharacterIndex: Integer);
begin
  SendScintillaEditorMessage(2712, WPARAM(ALineCharacterIndex), 0);
end;

function TCustomSciTextEditor.LineFromIndexPosition(APos: Integer; ALineCharacterIndex: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2713, WPARAM(APos), LPARAM(ALineCharacterIndex));
end;

function TCustomSciTextEditor.IndexPositionFromLine(ALine: Integer; ALineCharacterIndex: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(2714, WPARAM(ALine), LPARAM(ALineCharacterIndex));
end;

procedure TCustomSciTextEditor.StartRecord();
begin
  SendScintillaEditorMessage(3001, 0, 0);
end;

procedure TCustomSciTextEditor.StopRecord();
begin
  SendScintillaEditorMessage(3002, 0, 0);
end;

function TCustomSciTextEditor.GetLexer(): Integer;
begin
  Result := SendScintillaEditorMessage(4002, 0, 0);
end;

procedure TCustomSciTextEditor.Colourise(AStart: Integer; AEnd: Integer);
begin
  SendScintillaEditorMessage(4003, WPARAM(AStart), LPARAM(AEnd));
end;

procedure TCustomSciTextEditor.SetProperty(AKey: PAnsiChar; AValue: PAnsiChar);
begin
  SendScintillaEditorMessage(4004, WPARAM(AKey), LPARAM(AValue));
end;

procedure TCustomSciTextEditor.SetKeyWords(AKeyWordSet: Integer; AKeyWords: PAnsiChar);
begin
  SendScintillaEditorMessage(4005, WPARAM(AKeyWordSet), LPARAM(AKeyWords));
end;

function TCustomSciTextEditor.GetProperty(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4008, WPARAM(AKey), LPARAM(AValue));
end;

function TCustomSciTextEditor.GetPropertyExpanded(AKey: PAnsiChar; AValue: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4009, WPARAM(AKey), LPARAM(AValue));
end;

function TCustomSciTextEditor.GetPropertyInt(AKey: PAnsiChar; ADefaultValue: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4010, WPARAM(AKey), LPARAM(ADefaultValue));
end;

function TCustomSciTextEditor.GetLexerLanguage(ALanguage: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4012, 0, LPARAM(ALanguage));
end;

function TCustomSciTextEditor.PrivateLexerCall(AOperation: Integer; APointer: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4013, WPARAM(AOperation), LPARAM(APointer));
end;

function TCustomSciTextEditor.PropertyNames(ANames: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4014, 0, LPARAM(ANames));
end;

function TCustomSciTextEditor.PropertyType(AName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4015, WPARAM(AName), 0);
end;

function TCustomSciTextEditor.DescribeProperty(AName: PAnsiChar; ADescription: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4016, WPARAM(AName), LPARAM(ADescription));
end;

function TCustomSciTextEditor.DescribeKeyWordSets(ADescriptions: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4017, 0, LPARAM(ADescriptions));
end;

function TCustomSciTextEditor.GetLineEndTypesSupported(): Integer;
begin
  Result := SendScintillaEditorMessage(4018, 0, 0);
end;

function TCustomSciTextEditor.AllocateSubStyles(AStyleBase: Integer; ANumberStyles: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4020, WPARAM(AStyleBase), LPARAM(ANumberStyles));
end;

function TCustomSciTextEditor.GetSubStylesStart(AStyleBase: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4021, WPARAM(AStyleBase), 0);
end;

function TCustomSciTextEditor.GetSubStylesLength(AStyleBase: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4022, WPARAM(AStyleBase), 0);
end;

function TCustomSciTextEditor.GetStyleFromSubStyle(ASubStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4027, WPARAM(ASubStyle), 0);
end;

function TCustomSciTextEditor.GetPrimaryStyleFromStyle(AStyle: Integer): Integer;
begin
  Result := SendScintillaEditorMessage(4028, WPARAM(AStyle), 0);
end;

procedure TCustomSciTextEditor.FreeSubStyles();
begin
  SendScintillaEditorMessage(4023, 0, 0);
end;

procedure TCustomSciTextEditor.SetIdentifiers(AStyle: Integer; AIdentifiers: PAnsiChar);
begin
  SendScintillaEditorMessage(4024, WPARAM(AStyle), LPARAM(AIdentifiers));
end;

function TCustomSciTextEditor.DistanceToSecondaryStyles(): Integer;
begin
  Result := SendScintillaEditorMessage(4025, 0, 0);
end;

function TCustomSciTextEditor.GetSubStyleBases(AStyles: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4026, 0, LPARAM(AStyles));
end;

function TCustomSciTextEditor.GetNamedStyles(): Integer;
begin
  Result := SendScintillaEditorMessage(4029, 0, 0);
end;

function TCustomSciTextEditor.NameOfStyle(AStyle: Integer; AName: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4030, WPARAM(AStyle), LPARAM(AName));
end;

function TCustomSciTextEditor.TagsOfStyle(AStyle: Integer; ATags: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4031, WPARAM(AStyle), LPARAM(ATags));
end;

function TCustomSciTextEditor.DescriptionOfStyle(AStyle: Integer; ADescription: PAnsiChar): Integer;
begin
  Result := SendScintillaEditorMessage(4032, WPARAM(AStyle), LPARAM(ADescription));
end;

procedure TCustomSciTextEditor.SetILexer(AIlexer: Integer);
begin
  SendScintillaEditorMessage(4033, 0, LPARAM(AIlexer));
end;

function TCustomSciTextEditor.GetBidirectional(): Integer;
begin
  Result := SendScintillaEditorMessage(2708, 0, 0);
end;

procedure TCustomSciTextEditor.SetBidirectional(ABidirectional: Integer);
begin
  SendScintillaEditorMessage(2709, WPARAM(ABidirectional), 0);
end;

procedure TCustomSciTextEditor.SetStyleBits(ABits: Integer);
begin
  SendScintillaEditorMessage(2090, WPARAM(ABits), 0);
end;

function TCustomSciTextEditor.GetStyleBits(): Integer;
begin
  Result := SendScintillaEditorMessage(2091, 0, 0);
end;

function TCustomSciTextEditor.GetStyleBitsNeeded(): Integer;
begin
  Result := SendScintillaEditorMessage(4011, 0, 0);
end;

procedure TCustomSciTextEditor.SetKeysUnicode(AKeysUnicode: Boolean);
begin
  SendScintillaEditorMessage(2521, WPARAM(AKeysUnicode), 0);
end;

function TCustomSciTextEditor.GetKeysUnicode(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2522, 0, 0));
end;

function TCustomSciTextEditor.GetTwoPhaseDraw(): Boolean;
begin
  Result := Boolean(SendScintillaEditorMessage(2283, 0, 0));
end;

procedure TCustomSciTextEditor.SetTwoPhaseDraw(ATwoPhase: Boolean);
begin
  SendScintillaEditorMessage(2284, WPARAM(ATwoPhase), 0);
end;


end.
