//CE_Desc_Include(helpdescriptions.txt)
unit ScintillaLanguageManager;
////////////////////////////////////////////////////////////////////////////////
// Unit    : ScintillaLanguageManager
// Purpose : Interface for Scintilla Lexer DLL
// Created : 05/03/2001
// Author  : Kiriakos Vlahos (kvlahos@london.edu)
//      $Id: ScintillaLanguageManager.pas,v 1.2 2004/12/03 17:18:59 hdalis Exp $
// History : 15/03/2001 Lexer sturtures and encapsulation of some common Lexers
//
//           29/09/2004 Initial Release with Delphi Scintilla Interface Components
//                      Added additional encapsulation, the possibility to
//                      define your own languages based on the existing lexers.
//                      No longer enumerated lexer type, changed to strings
//                      Added the possibility to define languages based on
//                      known lexers. Multiple languages can use the same
//                      lexer. ie. resourcedefinition use the same lexer as the
//                      c++ language.
//                      Added function to dynamically add languages to the
//                      Optionsdialog etc..
//                      (hdalis@users.sourceforge.net)
//           05/10/2004 Removed the typedef for TSciLangName (no longer needed)
//                      Renamed the SetLLanguage procedure to SetLanguageStr
//           13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
//                      generate the help.
//                      (hdalis@users.sourceforge.net)
//           27/10/2004 Renamed the TSciLanguageManager.LanguageStr to SelectedLanguage,
//                      and the SetLanguageStr to SetSelectedLanguage.
//                      Renamed TSciLangList.GetStyleListStr to GetStyleList.
//           30/10/2004 Added Comment* properties to TSciLangItem, used by
//                      CommentBox/CommentBlock functions in TScintilla.
// Bugfixes:
//           15/10/2004 Fixed a bug in GetStyleListStr. Didn't set the LanguageStr when
//                      nullitm didn't exits, and it was created. Caused
//                      TScintillaOptionsDlg to throw an exception with list error
//                      when you tried to add a style and no nullstyles existed.
//                      (hdalis@users.sourceforge.net)
////////////////////////////////////////////////////////////////////////////////

interface

uses SysUtils, Classes, Graphics;

{$Include commondefs.inc}

Type
{$Ifndef NOLANGCHANGEDEVENT}
	TSCEvent_languagechanged = procedure(Sender : TObject; newlang : String) of object;
{$Endif}

TSciCase = (CASE_MIXED = 0,CASE_UPPER = 1,CASE_LOWER = 2);
TSciLangKeywords = Record
	Name : string;
	KeywordListNumber : integer;
	KeyWords : string;
end;

TSciLangStyle = Record
	StyleNumber : integer;
	Name : string;
	FontName : string;
	FontSize : integer;
	FontStyles : TFontStyles;
	ForeColor : TColor;
	BackColor : TColor;
	CharCase : TSciCase;
	Visible: Boolean;
	Changeable : Boolean;
	Hotspot : Boolean;
	EOLFilled : Boolean;
end;

TSciKeywords = class(TCollectionItem)
private
	fKeyWords : TStrings;
	fName : string;
	fKeywordListNumber : integer;
	procedure SetKeywords(const Value: TStrings);
	procedure SetName(const Value: string);
	procedure SetKeywordListNumber(const Value: integer);
public
	constructor Create(Collection: TCollection); override;
	destructor Destroy; override;
	procedure Assign(Source: TPersistent); override;
	procedure AssignRec(Rec : TSciLangKeywords);
	procedure AssignToRec(var Rec : TSciLangKeywords);
published
	property KeywordListNumber : integer read FKeywordListNumber write SetKeywordListNumber;
	property Name : string read fName write SetName;
	property Keywords : TStrings read fKeywords write SetKeywords;
end;


TSciStyle = class(TCollectionItem)
private
	fStyleNumber : integer;
	fName : string;
	fFontName : string;
	fFontSize : integer;
	fFontStyles : TFontStyles;
	fForeColor : TColor;
	fBackColor : TColor;
	fCharCase : TSciCase;
	fVisible: Boolean;
	fChangeable: Boolean;
	fHotspot : boolean;
	fEOLFilled: Boolean;
	procedure SetBackColor(const Value: TColor);
	procedure SetFontName(const Value: string);
	procedure SetFontSize(const Value: integer);
	procedure SetFontStyles(const Value: TFontStyles);
	procedure SetForeColor(const Value: TColor);
	procedure SetName(const Value: string);
	procedure SetStyleNumber(const Value: integer);
	procedure SetCharCase(const Value: TSciCase);
	procedure SetVisible(const Value: Boolean);
	procedure SetChangeable(const Value: Boolean);
	procedure SetEOLFilled(const Value: Boolean);
	procedure SetHotspot(const Value: Boolean);
public
	constructor Create(Collection: TCollection); override;
	procedure Assign(Source: TPersistent); override;
	procedure AssignRec(Rec : TSciLangStyle);
	procedure AssignToRec(var Rec : TSciLangStyle);
published
	property FontName : string read fFontName write SetFontName;
	property FontSize : integer read fFontSize write SetFontSize;
	property FontStyles : TFontStyles read FFontStyles write SetFontStyles;
	property ForeColor : TColor read fForeColor write SetForeColor default clDefault;
	property BackColor : TColor read fBackColor write SetBackColor default clDefault;
	property CharCase : TSciCase read fCharCase write SetCharCase;
	property Visible : Boolean read fVisible write SetVisible default true;
	property Changeable : Boolean read fChangeable write SetChangeable default true;
	property EOLFilled : Boolean read fEOLFilled write SetEOLFilled default false;
	property Hotspot : Boolean read fHotspot write SetHotspot default false;
	property Name : string read fName write SetName;
	property StyleNumber : integer read fStyleNumber write SetStyleNumber;
end;

TSciKeyWordsList = class(TOwnedCollection)
private
	fEditor : TPersistent; //TScintilla
	procedure SetEditor(Editor : TPersistent);
public
	procedure Update(Item: TCollectionItem); override;
end;

TSciStyleList = class(TOwnedCollection)
private
	fEditor : TPersistent; //TScintilla
	procedure SetEditor(Editor : TPersistent);

public
	procedure Update(Item: TCollectionItem); override;
end;

TSciLangItem = class(TCollectionItem)
private
	fLexer : String;
	fStyles : TSciStyleList;
	fKeyWords : TSciKeyWordsList;
	fLanguageName : String;
	fEditor : TPersistent; //TScintilla
{$IFNDEF NOCOMMENTING}
  FCommentStart,FCommentMiddle,FCommentEnd,FCommentOneliner : String;
  FCommentAtLineStart : Boolean;
{$ENDIF}
	procedure SetLexer(const Value: String);
	procedure SetName(const Value: string);
	procedure SetKeywordsList(const Value: TSciKeyWordsList);
	procedure SetStyleList(const Value: TSciStyleList);
	procedure SetEditor(Value : TPersistent);
public
	constructor Create(Collection: TCollection); override;
	destructor Destroy; override;
	procedure Assign(Source: TPersistent); override;
	procedure Update;
published
	// The Language name
	property Name : string read fLanguageName write SetName;
	// The lexer to use with this language.
	property Lexer : String read fLexer write SetLexer;

	property Styles : TSciStyleList read fStyles write SetStyleList;
	property Keywords : TSciKeyWordsList read fKeyWords write SetKeywordsList;
{$IFNDEF NOCOMMENTING}
  property CommentStart : String read FCommentStart write FCommentStart;
  property CommentEnd : String read FCommentEnd write FCommentEnd;
  property CommentMiddle : String read FCommentMiddle write FCommentMiddle;
  property CommentOneliner : String read FCommentOneliner write FCommentOneliner;
  property CommentAtLineStart : Boolean read FCommentAtLineStart write FCommentAtLineStart;
{$ENDIF}
end;


TSciLangList = class(TOwnedCollection)
  private
    fEditor : TPersistent; //TScintilla
    procedure SetEditor(Editor : TPersistent);

  public
		procedure Select(const fLanguage : String);
		function  Find(const fLanguage : String) : TSciLangItem;
		function  GetStyleList(const fLanguage : String;const fLexerName : String='') : TSciLangItem;
	end;

	TSCEvent_lexupdate = procedure(Sender : TObject; Editor : TPersistent;lang : String;Item : TSciLangItem) of object;
	TSciLanguageManager = class (TPersistent)
	private
		fEditor : TPersistent; //TScintilla
		fSelectedLanguage : String;
		fLanguages : TSciLangList;
		flexerlist    : TStringList;
	{$Ifndef NOLANGCHANGEDEVENT}
		fOnLanguageChanged : TSCEvent_languagechanged;
	{$Endif}
		fOnLexUpdate : TSCEvent_lexupdate;

		procedure SetLanguageList(const Value : TSciLangList);
		procedure SetSelectedLanguage(const Value : String);
		{Retrieve the lexer corresponding to the 'Lang'}
		function  LangToLex(const Lang : String) : String;
   	procedure FillLexerNameMap;
	protected
		function  GetOwner: TPersistent; override;
	public
		constructor Create(Editor: TPersistent);
		destructor  Destroy; override;
		procedure   Assign(Source: TPersistent); override;
    function    GetLexerProps : String;
		procedure   Update;
		procedure   FillListWithLanguages(lst : TStrings;const Justlexers : Boolean=false);
	  function    AddLanguage(const Lang,Lexer : String) : TSciLangItem;
	  function    DupLanguage(const Langtocopy : String;const langname : String='') : TSciLangItem;
	  function    RemoveLanguage(const Langtoremove : String;var notinstaticlist : Boolean) : Boolean;
	published
		property    LanguageList : TSciLangList read fLanguages write SetLanguageList;
		property    SelectedLanguage : String read fSelectedLanguage write SetSelectedLanguage;
	{$Ifndef NOLANGCHANGEDEVENT}
		property    OnLanguageChanged : TSCEvent_languagechanged read fOnLanguageChanged write fOnLanguageChanged;
	{$Endif}
		property    OnLexUpdate : TSCEvent_lexupdate read fOnLexUpdate write fOnLexUpdate;
	end;

const
scicontainerconst='container';
cDefaultLexer='null';

implementation

Uses
	SciSupport,SciLexer,SciLexerMod,SciResLang
{$Ifdef COMPILER5},sciUtils{$Endif};

procedure TSciLangList.SetEditor(Editor : TPersistent);
var
  i : Integer;
begin
  try
    BeginUpdate;
    self.fEditor:=Editor;
    for i := 0 to Count - 1 do
    begin
      TSciLangItem(Items[i]).SetEditor(Editor);
    end;
  finally
	  EndUpdate;
  end;
end;


function  TSciLangList.Find(const fLanguage : String) : TSciLangItem;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
	begin
    if AnsiCompareStr(TSciLangItem(Items[i]).Name,fLanguage)=0 then
		begin
			Result :=TSciLangItem(Items[i]);
			Exit;
		end;
  end;
  Result :=nil;
end;

procedure TSciLangList.Select(const fLanguage : String);
var
  itm : TSciLangItem;
begin
	if not assigned(fEditor) then Exit;
  itm:=Find(fLanguage);
  if itm<>nil then itm.Update;
end;


function  TSciLangList.GetStyleList(const fLanguage : String;const fLexerName : String) : TSciLangItem;
var
  tmp : TSciLangItem;
  nullitm : TSciLangItem;
begin
  tmp:=Find(fLanguage);
  if tmp<>nil then
  begin
    Result:=tmp;
    Exit;
  end;
	tmp:=TSciLangItem(Add);
  nullitm:=Find('null');
  if nullitm<>nil then
  begin
    try
      tmp.Styles.BeginUpdate;
      if fEditor<>nil then tmp.SetEditor(fEditor);
      tmp.Styles.Assign(nullitm.Styles);
      tmp.Name:=fLanguage;
      if fLexerName='' then
        tmp.Lexer:=fLanguage
      else
        tmp.Lexer:=fLexerName;
    finally
      tmp.Styles.EndUpdate;
    end;
  end else
  begin
    tmp.Name:='null';
    tmp.Lexer:='null';
    tmp:=GetStyleList(fLanguage);
  end;
	Result:=tmp;
end;

procedure TSciLangItem.SetEditor(Value : TPersistent);
begin
	fEditor:=Value;
	Styles.SetEditor(fEditor);
	Keywords.SetEditor(fEditor);
end;

procedure TSciLangItem.SetName(const Value: string);
begin
	fLanguageName:=Value;
	Changed(false);
end;

procedure TSciLangItem.SetLexer(const Value: String);
begin
  if(fLexer<>Value) then
  begin
    //if fLexer=fLanguageName then
    //begin
    //  fLanguageName:=Value;
    //end;
    fLexer :=Value;
    Changed(false);
  end;
end;


procedure TSciLangItem.SetKeywordsList(const Value: TSciKeyWordsList);
begin
	fKeywords.Assign(Value);
	Changed(false);
end;

procedure TSciLangItem.SetStyleList(const Value: TSciStyleList);
begin
	fStyles.Assign(Value);
	Changed(false);
end;

procedure TSciLangItem.Update;
begin
	fKeyWords.Update(nil);
	fStyles.Update(nil);

	if fEditor<>nil then
  begin
    TScintilla(fEditor).Colourise(0, -1);
  end;

end;

constructor TSciLangItem.Create(Collection: TCollection);
begin
	inherited;
	fStyles:=TSciStyleList.Create(self,TSciStyle);
	fKeyWords:=TSciKeyWordsList.Create(self,TSciKeywords);
	fLanguageName := sLMLanguage+'['+IntToStr(Index)+']';
  fLexer :='null';
{$IFNDEF NOCOMMENTING}
  FCommentStart:='/*';
  FCommentMiddle:='*';
  FCommentEnd:='*/';
  FCommentOneliner:='//';
  FCommentAtLineStart:=True;
{$ENDIF}

end;

destructor TSciLangItem.Destroy;
begin
	inherited;
	if assigned(fStyles) then
	begin
		fStyles.Free;
		fStyles:=nil;
	end;
	if assigned(fKeyWords) then
	begin
		fKeyWords.Free;
		fKeyWords:=nil;
	end;
end;

procedure TSciLangItem.Assign(Source: TPersistent);
begin
	if Source is TSciLangItem then
	begin
		fLanguageName := TSciLangItem(Source).Name;
    fStyles.Assign(TSciLangItem(Source).Styles);
    fKeywords.Assign(TSciLangItem(Source).Keywords);
    fLexer:=TSciLangItem(Source).Lexer;
{$IFNDEF NOCOMMENTING}
    FCommentStart:=TSciLangItem(Source).CommentStart;
    FCommentMiddle:=TSciLangItem(Source).CommentMiddle;
    FCommentEnd:=TSciLangItem(Source).CommentEnd;
    FCommentOneliner:=TSciLangItem(Source).CommentOneliner;
    FCommentAtLineStart :=TSciLangItem(Source).CommentAtLineStart;
{$ENDIF}
  end else
    inherited;
end;


procedure TSciLanguageManager.SetLanguageList(const Value : TSciLangList);
begin
	fLanguages.Assign(Value);
	Update;
end;

procedure TSciKeywords.Assign(Source: TPersistent);
begin
	if Source is TSciKeywords then begin
		fName := TSciKeywords(Source).Name;
    fKeyWordListNumber := TSciKeywords(Source).KeywordListNumber;
    fKeywords.Assign(TSciKeywords(Source).Keywords);
  end else
    inherited;
end;

procedure TSciKeywords.AssignToRec(var Rec : TSciLangKeywords);
begin
  Rec.Name :=fName;
  Rec.KeywordListNumber:=fKeywordListNumber;
  Rec.KeyWords :=fKeywords.Text;
end;

procedure TSciKeywords.AssignRec(Rec : TSciLangKeywords);
begin
  fName := Rec.Name;
  fKeywordListNumber := Rec.KeywordListNumber;
{$Ifndef COMPILER5}
  fKeywords.Delimiter := ' ';
  fKeywords.DelimitedText := Rec.KeyWords;
{$Else}
  fKeywords.Text := Rec.KeyWords;
{$Endif}
end;

constructor TSciKeywords.Create(Collection: TCollection);
begin
  fKeywords := TStringList.Create;
  inherited;
  fName := sLMKeywords+'['+IntToStr(Index)+']';
end;

destructor TSciKeywords.Destroy;
begin
  fKeywords.Free;
  inherited;
end;

procedure TSciKeywords.SetKeywordListNumber(const Value: integer);
begin
  FKeywordListNumber := Value;
  Changed(False);
end;

procedure TSciKeywords.SetKeywords(const Value: TStrings);
begin
  fKeyWords.Assign(Value);
  Changed(False);
end;

procedure TSciKeywords.SetName(const Value: string);
begin
  fName := Value;
  Changed(False);
end;


{ TSciStyle }

procedure TSciStyle.Assign(Source: TPersistent);
begin
  if Source is TSciStyle then
	begin
		fStyleNumber := TSciStyle(Source).StyleNumber;
		fName := TSciStyle(Source).Name;
		fFontName := TSciStyle(Source).FontName;
		fFontSize := TSciStyle(Source).FontSize;
		fFontStyles := TSciStyle(Source).FontStyles;
		fForeColor :=TSciStyle(Source).ForeColor;
		fBackColor := TSciStyle(Source).BackColor;
		fCharCase := TSciStyle(Source).CharCase;
		fVisible := TSciStyle(Source).Visible;
		fChangeable := TSciStyle(Source).Changeable;
		fHotspot := TSciStyle(Source).HotSpot;
		fEOLFilled := TSciStyle(Source).EOLFilled;
  end else
    inherited;
end;

procedure TSciStyle.AssignRec(Rec: TSciLangStyle);
begin
  fStyleNumber := Rec.StyleNumber;
  fName := Rec.Name;
  fFontName := Rec.FontName;
  fFontSize := Rec.FontSize;
  fFontStyles := Rec.FontStyles;
  fForeColor :=Rec.ForeColor;
  fBackColor := Rec.BackColor;
  fCharCase := Rec.CharCase;
  fVisible := Rec.Visible;
  fChangeable := Rec.Changeable;
  fHotspot := Rec.Hotspot;
  fEOLFilled := Rec.EOLFilled;
end;

procedure TSciStyle.AssignToRec(var Rec : TSciLangStyle);
begin
  Rec.StyleNumber:=fStyleNumber;
  Rec.Name:=fName;
  Rec.FontName :=fFontName;
  Rec.FontSize :=fFontSize;
  Rec.FontStyles :=fFontStyles;
  Rec.ForeColor :=fForeColor;
  Rec.BackColor :=fBackColor;
  Rec.CharCase :=fCharCase;
  Rec.Visible :=fVisible;
  Rec.Changeable :=fChangeable;
  Rec.Hotspot :=fHotspot;
  Rec.EOLFilled :=fEOLFilled;
end;
constructor TSciStyle.Create(Collection: TCollection);
begin
  inherited;
  fStyleNumber := Index;
  fName := sLMStyle+'['+IntToStr(Index)+']';
  fFontName := '';
  fFontSize := 0;
	fFontStyles:=[];
	fCharCase:=CASE_MIXED;
  fForeColor := clDefault;
  fBackColor := clDefault;
  fVisible := True;
  fEOLFilled := False;
  fChangeable := True;
  fHotSpot := False;
end;

procedure TSciStyle.SetBackColor(const Value: TColor);
begin
  fBackColor := Value;
  Changed(False);
end;

procedure TSciStyle.SetChangeable(const Value: Boolean);
begin
  fChangeable := Value;
  Changed(False);
end;

procedure TSciStyle.SetCharCase(const Value: TSciCase);
begin
  fCharCase := Value;
  Changed(False);
end;

procedure TSciStyle.SetEOLFilled(const Value: Boolean);
begin
  fEOLFilled := Value;
  Changed(False);
end;

procedure TSciStyle.SetFontName(const Value: string);
begin
  fFontName := Value;
  Changed(False);
end;

procedure TSciStyle.SetFontSize(const Value: integer);
begin
  fFontSize := Value;
  Changed(False);
end;

procedure TSciStyle.SetFontStyles(const Value: TFontStyles);
begin
  FFontStyles := Value;
  Changed(False);
end;

procedure TSciStyle.SetForeColor(const Value: TColor);
begin
  fForeColor := Value;
  Changed(False);
end;

procedure TSciStyle.SetHotspot(const Value: Boolean);
begin
  fHotSpot := Value;
  Changed(False);
end;

procedure TSciStyle.SetName(const Value: string);
begin
  fName := Value;
  Changed(False);
end;

procedure TSciStyle.SetStyleNumber(const Value: integer);
begin
  fStyleNumber := Value;
  Changed(False);
end;

procedure TSciStyle.SetVisible(const Value: Boolean);
begin
  fVisible := Value;
  Changed(False);
end;

{ TSciStyleList }

procedure TSciStyleList.SetEditor(Editor: TPersistent);
begin
  self.fEditor := Editor;
end;

procedure TSciStyleList.Update(Item: TCollectionItem);
Var
  i : integer;
begin
  inherited;
  if not Assigned(fEditor) then Exit;
  if Assigned(Item) then with (Item as TSciStyle), TScintilla(fEditor) do begin
    if fFontName <> '' then StyleSetFont(StyleNumber, PChar(fFontName));
    if fFontSize > 0 then StyleSetSize (StyleNumber, fFontSize);
		if fForeColor <> clDefault then StyleSetFore(StyleNumber, fForeColor);
    if fBackColor <> clDefault then StyleSetBack(StyleNumber, fBackColor);
    StyleSetBold(StyleNumber, fsBold in fFontStyles);
    StyleSetItalic(StyleNumber, fsItalic in fFontStyles);
    StyleSetUnderline(StyleNumber, fsUnderline in fFontStyles);
    StyleSetCase(StyleNumber, Ord(fCharCase));
    StyleSetVisible(StyleNumber, fVisible);
    StyleSetChangeable(StyleNumber, fChangeable);
    StyleSetHotSpot(StyleNumber, fHotSpot);
    StyleSetEOLFilled(StyleNumber, fEOLFilled);
  end else
  begin
    for i := 0 to Count - 1 do Update(Items[i]);
  end;
end;

{ TSciKeyWordsList }

procedure TSciKeyWordsList.SetEditor(Editor: TPersistent);
begin
  self.fEditor := Editor;
end;

procedure TSciKeyWordsList.Update(Item: TCollectionItem);
Var
  i : integer;
begin
  inherited;
  if not Assigned(fEditor) then Exit;
  if Assigned(Item) then with (Item as TSciKeywords) do begin
{$Ifndef COMPILER5}
    Keywords.Delimiter := ' ';
    TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(Keywords.DelimitedText));
{$Else}
    TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(MergeStrings(Keywords)));
{$Endif}
  end else begin
    for i := 0 to Count - 1 do with (Items[i] as TSciKeywords) do
    begin
{$Ifndef COMPILER5}
      Keywords.Delimiter := ' ';
      TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(Keywords.DelimitedText));
{$Else}
      TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(MergeStrings(Keywords)));
{$Endif}
    end;
  end;
end;

{ TSciLanguageManager }

procedure TSciLanguageManager.Assign(Source: TPersistent);
begin
  if Source is TSciLanguageManager then
	begin
    //fLexerNamesMapper.Assign(TSciLanguageManager(Source).fLexerNamesMapper);
    flexerlist.Assign(TSciLanguageManager(Source).flexerlist);
		fLanguages.Assign(TSciLanguageManager(Source).LanguageList);
    fSelectedLanguage := TSciLanguageManager(Source).SelectedLanguage;
		Update;
{$Ifndef NOLANGCHANGEDEVENT}
		if assigned(fOnLanguageChanged) then
			fOnLanguageChanged(self,fSelectedLanguage);
{$Endif}

	end else
		inherited;
end;


constructor TSciLanguageManager.Create(Editor: TPersistent);
begin
	inherited Create;
	fEditor := Editor;
  flexerlist:=TStringList.Create;
  flexerlist.CaseSensitive:=True;
  FillLexerNameMap;
  fSelectedLanguage:='null';
	fLanguages:=TSciLangList.Create(self,TSciLangItem);
	fLanguages.SetEditor(fEditor);
end;

function TSciLanguageManager.LangToLex(const Lang : String) : String;
var
i : Integer;
cnt : Integer;
begin
  if (Lang='') or (AnsiCompareText(Lang,scicontainerconst)=0) then
  begin
    Result:=scicontainerconst;
  end;
  if fLanguages<>nil then
  begin
    cnt:=fLanguages.Count;
    for i:=0 to (cnt-1) do
    begin
      if AnsiCompareText(TSciLangItem(fLanguages.Items[i]).Name,Lang)=0 then
      begin
        Result:=TSciLangItem(fLanguages.Items[i]).Lexer;
        Exit;
      end;
    end;
  end;
  Result:=Lang;
end;
destructor TSciLanguageManager.Destroy;
begin
	fLanguages.Free;
  if assigned(flexerlist) then flexerlist.Free;
	inherited;
end;

function TSciLanguageManager.GetOwner: TPersistent;
begin
	Result := fEditor;
end;


procedure TSciLanguageManager.SetSelectedLanguage(const Value : String);
begin
	fSelectedLanguage:=Value;
  if (AnsiCompareText(fSelectedLanguage,scicontainerconst)<>0) and (fSelectedLanguage<>'') then
    fLanguages.Select(Value);
  Update;
	{$Ifndef NOLANGCHANGEDEVENT}
	if (assigned(fOnLanguageChanged)) then
		fOnLanguageChanged(self,Value);
	{$Endif}

end;

procedure TSciLanguageManager.Update;
var
itm : TSciLangItem;
mappedstr : String;
hyper : Boolean;
begin
	inherited;
	if not Assigned(fEditor) then Exit;
	with TScintilla(fEditor) do
	begin
    //
    mappedstr:=LangToLex(fSelectedLanguage);
    if AnsiCompareText(mappedstr,scicontainerconst)=0 then
    begin
      SetLexer(SCLEX_CONTAINER);
      StyleClearAll;
    end else
    begin
      SetLexerLanguage(PChar(mappedstr));
      if AnsiCompareText(fSelectedLanguage,'WML')=0 then
        SetProperty('html.tags.case.sensitive','1');
      StyleClearAll;
      hyper:=(AnsiCompareText(mappedstr,'hypertext')=0) or (AnsiCompareText(mappedstr,'asp')=0) or (AnsiCompareText(mappedstr,'php')=0);
      if (hyper=True) then
      begin
        SetProperty('html.tags.case.sensitive','0');
        SetStyleBits(7);
      end else
      begin
        if (AnsiCompareText(mappedstr,'xml')=0) then
        begin
          SetStyleBits(7);
          SetProperty('html.tags.case.sensitive','1');
        end else
        SetStyleBits(5);
      end;
    end;
  end;
  if AnsiCompareText(mappedstr,scicontainerconst)<>0 then
  begin
    fLanguages.SetEditor(fEditor);
    itm:=fLanguages.GetStyleList(fSelectedLanguage);
    // If assigned, perform other updates in the event.
    if assigned(fOnLexUpdate) then fOnLexUpdate(Self,fEditor,fSelectedLanguage,itm);
    itm.Update;
  end else if assigned(fOnLexUpdate) then fOnLexUpdate(Self,fEditor,fSelectedLanguage,nil);
end;

procedure TSciLanguageManager.FillListWithLanguages(lst : TStrings;const Justlexers : Boolean);
var
  cnt : Integer;
  i : Integer;
  tmpstr : String;
begin
  if lst=nil then Exit;
  try
    lst.BeginUpdate;
    lst.Clear;
    cnt:=flexerlist.Count;
    for i:=0 to (cnt-1) do
    begin
      tmpstr:=flexerlist.Strings[i];
      lst.Add(tmpstr);
    end;
    if (Justlexers=False) then
    begin
      cnt:=fLanguages.Count;
      for i:=0 to (cnt-1) do
      begin
        tmpstr:=TSciLangItem(fLanguages.Items[i]).Name;
        if lst.IndexOf(tmpstr)=-1 then
        lst.Add(tmpstr);
      end;
    end;
  finally
    lst.EndUpdate;
  end;
end;

procedure TSciLanguageManager.FillLexerNameMap;
begin
{Fill the namelist with all the lexers we have}
  with flexerlist do
  begin
    Add('null');
    Add('ada');
    Add('apdl');
    Add('asm');
    Add('au3');
    Add('ave');
    Add('baan');
    Add('bash');
    Add('bullant');
    Add('clw');
    Add('clwnocase');
    Add('conf');
    Add('cpp');
    Add('cppnocase');
    Add('tcl');
    Add('nncrontab');
    Add('css');
    Add('eiffel');
    Add('eiffelkw');
    Add('erlang');
    Add('escript');
    Add('forth');
    Add('fortran');
    Add('f77');
    Add('gui4cli');
    Add('hypertext');
    Add('xml');
    Add('asp');
    Add('php');
    Add('kix');
    Add('lisp');
    Add('lout');
    Add('lua');
    Add('matlab');
    Add('octave');
    Add('metapost');
    Add('mmixal');
    Add('lot');
    Add('mssql');
    Add('nsis');
    Add('batch');
    Add('diff');
    Add('props');
    Add('makefile');
    Add('errorlist');
    Add('latex');
    Add('pascal');
    Add('powerbasic');
    Add('perl');
    Add('pov');
    Add('ps');
    Add('python');
    Add('ruby');
    Add('scriptol');
    Add('specman');
    Add('sql');
    Add('tex');
    Add('vb');
    Add('vbscript');
    Add('verilog');
    Add('yaml');
    Add('asn1');
    Add('vhdl');
  end;
{
  with fextralist do
  begin
    Add('java');
    Add('javascript');
    Add('resource');
    Add('idl');
    Add('wml');
    Add('cs');
    Add('scheme');
    Add('xcode');
  end;
}
end;

function TSciLanguageManager.AddLanguage(const Lang,Lexer : String) : TSciLangItem;
var
  itm : TSciLangItem;
begin
  itm:=fLanguages.GetStyleList(Lang,Lexer);
  Result:=itm;
end;
function      TSciLanguageManager.DupLanguage(const Langtocopy : String;const langname : String) : TSciLangItem;
var
	itm : TSciLangItem;
	src : TSciLangItem;
begin
	src:=fLanguages.Find(Langtocopy);
	if src<>nil then
	begin
	itm:=TSciLangItem(fLanguages.Add);
	itm.Assign(src);
	end else itm:=nil;
	Result:=itm;
end;
function      TSciLanguageManager.RemoveLanguage(const Langtoremove : String;var notinstaticlist : Boolean) : Boolean;
var
	itm : TSciLangItem;
  nullitm : TSciLangItem;
begin
	itm:=fLanguages.Find(Langtoremove);
	if itm<>nil then
	begin
    if flexerlist.IndexOf(Langtoremove)<>-1 then
    begin
		  fLanguages.Delete(itm.Index);
      notinstaticlist:=False;
    end else
    begin
      nullitm:=fLanguages.GetStyleList('null');
      itm.Styles.Assign(nullitm.Styles);
      itm.Keywords.Clear;
      notinstaticlist:=True;
    end;
		Result:=True;
		Exit;
	end;
	Result:=false;
end;

// Returns any extra languages that has been added. Used by SciPropertyMgr
function TSciLanguageManager.GetLexerProps : String;
var
  tmp : String;
  i,cnt : Integer;
  itm : TSciLangItem;
begin
  tmp:='';
  cnt:=LanguageList.Count;
    tmp:=#13#10+'# ------------- Lexer '+sPropAssignments+' '+sPropSectionStart+' -------------'+#13#10;

    for i:=0 to (cnt-1) do
    begin
      itm:=TSciLangItem(LanguageList.Items[i]);
      if (flexerlist.IndexOf(itm.Name)=-1) then // Is this a language in the lexerlist?
          tmp:=tmp+'lexer.'+itm.Name+'='+itm.Lexer+#13#10;
    end;
    tmp:=tmp+'# -------------- Lexer '+sPropAssignments+' '+sPropSectionEnd+' --------------'+#13#10;
  Result:=tmp;

end;

end.
