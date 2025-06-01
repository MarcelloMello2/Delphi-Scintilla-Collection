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
	Author : hdalis (Jan Martin Pettersen)
	Created: 01/08/2004
			$Id: SciPropertyMgr.pas,v 1.7 2004/12/03 17:18:59 hdalis Exp $
	Purpose: Load/Save properties from a TScintilla/TScintillaAuto derived component

    Usage: Drop this component on the form, select the editor it should save and load properties
           from. Set the FileName property to the file it should read/write, and call Save or Load.

	History: 29/09/2004 First release
					 13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
											generate the help.
           26/10/2004 Moved the GetLexerMapperProps function to TSciLanguageManager.GetLexerProps (ScintillaSynLexer.Pas)
           14/11/2004 Changed the TSciPropertyLoader component to allow use with all classes descending from
                      TScintillaBase. A little 'feature' has also been fixed.. It searched for the '.' section.name divider
                      first, and if there wasn't a '.' in the propertyline before '=' then it would search the value for it,
                      and couldn't get a valid match for the line, and still contain the correct value.
                      Now it searches for the '=' first, and then matches properties within the left part,
                      and the right part of it is the value.
           02/12/2004 Moved all standalone functions to sciUtils.pas. They can, and probably will be used
                      by other units also.
}

unit SciPropertyMgr;
interface
uses  Classes,SciLexer,SciLexerMod,Graphics,ScintillaLanguageManager;

{$Include commondefs.inc}
Type
	TSCEvent_proploaditem = procedure(Sender : TObject; var CategoryName,PropName : String;var Num : Integer;var aValue : String;var Handled : Boolean) of object;
	TSCEvent_propsaveitem = procedure(Sender : TObject; const index : Integer;var CategoryName,PropName : String;var Num : Integer;var aValue : String;var MoreToDo : Boolean) of object;

TExtensionMapper=class(TObject)
  private
  extensions : TStringList;
  extsadded : Integer;
  function    GetProps : String; // Used by the propertymanager to output added extensions
	procedure   FillExtensionMap;
	public
	constructor Create;
	destructor  Destroy;override;
	procedure   Add(const mapfromext : String;const maptolexer : String);
	function    DetectLexer(const extension : string) : String;
end;

	{ Loads and saves properties for a TScintilladescending component.
		OnLoadItem and OnSaveItem may be used to save,parse additional properties
		if needed}

	TSciPropertyLoader=class(TComponent)
		private
			fFileName : String;
			fEditor         : TScintillaBase;
			fDefaultStyle   : TSciLangStyle;
			fOnPropLoadItem : TSCEvent_proploaditem;
			fOnPropSaveItem : TSCEvent_propsaveitem;
      fOtherSettings  : TStrings;
			procedure GenerateDefaultPropLine(var dest : String;component : TScintillaBase);
			procedure GenerateStylePropLine(var dest : String;const prefix : String;const styl : TSciStyle);
			procedure GenerateKwdPropLine(var dest : String;const prefix : String;const kwd : TSciKeywords);
			function  PropLineSplit(const s: String;var Lang : String;var stylenum : Integer;var prop : String;var value : String) : Boolean;
			function  ParseStyleDefinition(const definition : PChar;var sty : TSciLangStyle) : Boolean;

		protected
			function  ProcessDataStart(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
			function  ProcessDataEnd(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
			procedure Notification(AComponent: TComponent;Operation: TOperation);  override;

		public
      procedure   FillWithDefaultStyle(var sty : TSciLangStyle);
			constructor Create(AOwner : TComponent);override;
			destructor  Destroy;override;
			function    Load : Boolean; //Loads the propertyfile specified in 'FileName'
			function    Save : Boolean; //Saves to the propertyfile specified in 'FileName'
			function    LoadFromStream(fs : TStream) : Boolean;
			function    SaveToStream(fs : TStream) : Boolean;
      function    GetOtherSetting(const settingname : String;const defaultvalue : String='';const addifnonexistent : Boolean=False) : String;
      procedure   AddOtherSetting(const settingname : String;const value : String);
		published
			property 		FileName : String read fFileName write fFileName;
			property 		Editor : TScintillaBase read FEditor write fEditor;
			property 		OnLoadItem : TSCEvent_proploaditem read fOnPropLoadItem write fOnPropLoadItem;
			property 		OnSaveItem : TSCEvent_propsaveitem read fOnPropSaveItem write fOnPropSaveItem;
	end;


function  ExtMapper : TExtensionMapper;

implementation
uses Windows,SysUtils,SciLexerMemo,SciResLang,sciUtils;

var
  FExtMap : TExtensionMapper;


// Quite a bit of multiple instances of the same code maybe, but temporary
// Generates a string with the default color and font settings.
// Does NOT support the EOLFilled etc attributes.
procedure TSciPropertyLoader.GenerateDefaultPropLine(var dest : String;component : TScintillaBase);
var
	tmpstr : String;

begin
	tmpstr:='default.style=';
	tmpstr:=tmpstr+Format('fore:%s',[ColourToString(component.Font.Color)])+',';//fore
	tmpstr:=tmpstr+Format('back:%s',[ColourToString(component.Color)])+',';//back
  tmpstr:=tmpstr+Format('size:%d',[component.Font.Size])+',';//size
  tmpstr:=tmpstr+Format('font:%s',[component.Font.Name])+',';//font
  if fsBold in component.Font.Style then
    tmpstr:=tmpstr+'bold,'//bold
  else
    tmpstr:=tmpstr+'notbold,';//notbold
  if fsItalic in component.Font.Style then
    tmpstr:=tmpstr+'italics,' //italics
  else
    tmpstr:=tmpstr+'notitalics,';//notitalics

  if fsUnderline in component.Font.Style then
    tmpstr:=tmpstr+'underlined,'//underlined
  else
    tmpstr:=tmpstr+'notunderlined,';//notunderlined
  if fDefaultStyle.Visible=false  then
    tmpstr:=tmpstr+'notvisible,' //notvisible
  else
    tmpstr:=tmpstr+'visible,';//visible
  if fDefaultStyle.EOLFilled  then
    tmpstr:=tmpstr+'eolfilled,'//eolfilled
  else
    tmpstr:=tmpstr+'noteolfilled,';//noteolfilled
  if fDefaultStyle.Changeable  then
    tmpstr:=tmpstr+'changeable,'//changeable
  else
    tmpstr:=tmpstr+'notchangeable,';//notchangeable
  if fDefaultStyle.Hotspot  then
    tmpstr:=tmpstr+'hotspot,'//hotspot
  else
    tmpstr:=tmpstr+'nothotspot,';//nothotspot
  case fDefaultStyle.CharCase of
    CASE_UPPER: tmpstr:=tmpstr+'case:u,';
    CASE_LOWER: tmpstr:=tmpstr+'case:l,';
  end;

  dest :=tmpstr;
  if dest[Length(dest)]=',' then Delete(dest,Length(dest),1);
end;

// Generates a string with the properties of a single style.
procedure TSciPropertyLoader.GenerateStylePropLine(var dest : String;const prefix : String;const styl : TSciStyle);
var
	tmpstr : String;
	tmpdescr : String;
begin
	tmpstr:=Format('%s%d=',[prefix,styl.StyleNumber]);
	tmpdescr:=styl.Name;
	tmpdescr:=StrReplaceChar(tmpdescr,',','|');
	tmpstr:=tmpstr+'name:'+tmpdescr+','; //name
	if (styl.ForeColor<>clDefault) and (styl.ForeColor<>fDefaultStyle.ForeColor) then
		tmpstr:=tmpstr+Format('fore:%s',[ColourToString(styl.ForeColor)])+',';//fore

	if (styl.BackColor<>clDefault) and (styl.BackColor<>fDefaultStyle.BackColor) then
		tmpstr:=tmpstr+Format('back:%s',[ColourToString(styl.BackColor)])+',';//back

	if (styl.FontSize<>0) and (styl.FontSize<>fDefaultStyle.FontSize) then
		tmpstr:=tmpstr+Format('size:%d',[styl.FontSize])+',';//size

  if (styl.FontName<>'') and (styl.FontName<>fDefaultStyle.FontName)  then
		tmpstr:=tmpstr+Format('font:%s',[styl.FontName])+',';//font

  if (styl.FontStyles<>fDefaultStyle.FontStyles) then
  begin
		if ((fsBold in styl.FontStyles) <> (fsBold in fDefaultStyle.FontStyles)) then
		begin
			if fsBold in styl.FontStyles then
				tmpstr:=tmpstr+'bold,'//bold
			else
				tmpstr:=tmpstr+'notbold,';//notbold
    end;
		if ((fsItalic in styl.FontStyles) <> (fsItalic in fDefaultStyle.FontStyles)) then
		begin
			if fsItalic in styl.FontStyles then
				tmpstr:=tmpstr+'italics,' //italics
			else
				tmpstr:=tmpstr+'notitalics,';//notitalics
		end;
		if ((fsUnderline in styl.FontStyles) <> (fsUnderline in fDefaultStyle.FontStyles)) then
		begin
			if fsUnderline in styl.FontStyles then
				tmpstr:=tmpstr+'underlined,'//underlined
			else
				tmpstr:=tmpstr+'notunderlined,';//notunderlined
		end;
	end;//<>fontstyles

  if styl.Visible<>fDefaultStyle.Visible then
  begin
		if styl.Visible=false  then
			tmpstr:=tmpstr+'notvisible,' //notvisible
		else
			tmpstr:=tmpstr+'visible,';//visible
  end;

  if styl.EOLFilled<>fDefaultStyle.EOLFilled then
  begin
		if styl.EOLFilled  then
			tmpstr:=tmpstr+'eolfilled,'//eolfilled
		else
			tmpstr:=tmpstr+'noteolfilled,';//noteolfilled
  end;
  if styl.Changeable<>fDefaultStyle.Changeable then
  begin
		if styl.Changeable  then
			tmpstr:=tmpstr+'changeable,'//changeable
		else
			tmpstr:=tmpstr+'notchangeable,';//notchangeable
  end;
  if styl.Hotspot<>fDefaultStyle.Hotspot then
  begin
		if styl.Hotspot  then
			tmpstr:=tmpstr+'hotspot,'//hotspot
		else
			tmpstr:=tmpstr+'nothotspot,';//nothotspot
  end;
  if styl.CharCase<>fDefaultStyle.CharCase then
  begin
		case styl.CharCase of
			CASE_UPPER: tmpstr:=tmpstr+'case:u,';
			CASE_LOWER: tmpstr:=tmpstr+'case:l,';
		end;
  end;
  dest :=tmpstr;
  if dest[Length(dest)]=',' then Delete(dest,Length(dest),1);
end;

// Generates a propertyline with the specified keywords. May split in multiple lines if
// more than 8 in the list.
procedure TSciPropertyLoader.GenerateKwdPropLine(var dest : String;const prefix : String;const kwd : TSciKeywords);
var
	tmpstr,tmpdescr : String;
	i,cnt,kwdonline : Integer;

begin
  tmpstr:=Format('%s%d=',[prefix,kwd.KeywordListNumber]);
  tmpdescr:=kwd.Name;
  tmpdescr :=StrReplaceChar(tmpdescr,',','|');

  tmpstr:=tmpstr+'name:'+tmpdescr+'::';//name
	cnt:=kwd.Keywords.Count;
	if cnt>0 then
	begin
		kwdonline:=0;
		for i:=0 to (cnt-1) do
		begin
			if (kwdonline>8) then
			begin
				tmpstr:=tmpstr+'\'+#13#10;
				kwdonline:=0;
				tmpstr:=tmpstr+kwd.Keywords.Strings[i];
			end else
			tmpstr:=tmpstr+' '+kwd.Keywords.Strings[i];
			Inc(kwdonline);
		end;
		if tmpstr[Length(tmpstr)]='\' then Delete(tmpstr,Length(tmpstr),1);
		dest :=tmpstr;
	end else dest:='';
end;


//Splits a propertyline in 'Lang',stylenum and prop.
//Lines like
// prop.lang.stylenum=value or
// prop.lang=value
// are accepted
function TSciPropertyLoader.PropLineSplit(const s: String;var Lang : String;var stylenum : Integer;var prop : String;var value : String) : Boolean;
var
	tmp : String;
  propnamepart : String;
	code : Integer;
  tmpstylenum : String;
begin
  tmp:=s;
  propnamepart:=StrToken(tmp,'=');
  value:=tmp;
  if propnamepart='' then
  begin
    Result:=False;
    Exit;
  end;
  Lang:=StrToken(propnamepart,'.');
  if propnamepart='' then
  begin
    Result:=false;
    Exit;
  end;
  Prop:=StrToken(propnamepart,'.');
  if propnamepart='' then
  begin
    Result:=true;
    Exit;
  end;
  tmpstylenum:=propnamepart;
  if tmpstylenum<>'' then
    val(tmpstylenum,stylenum,code);
  Result:=true;
end;

// Parses a string with a styledefinition into a TSciLangStyle
function TSciPropertyLoader.ParseStyleDefinition(const definition : PChar;var sty : TSciLangStyle) : Boolean;
var
	vxal,opt,cpComma,colon : PChar;
	code,i : Integer;
  Changeable,EOLFilled,Hotspot : Boolean;
	wasset : array [0..6] of Boolean;
	ccase : TSciCase;
	vis : Boolean;
	name : String;
begin
	if (definition = nil) or (definition^ = #0) then
	begin
		Result:=false;
		Exit;
	end;
	GetMem(vxal,Length(definition)+1);
	StrCopy(vxal,definition);
	opt:=vxal;
	Changeable:=true;
	Hotspot:=false;
	EOLFilled:=false;
	vis:=True;
	ccase:=CASE_MIXED;
	for i:=Low(wasset) to High(wasset) do wasset[i]:=False;
	while (opt<>nil) do
	begin
		// Find attribute separator
		cpComma := AnsiStrScan(opt, ',');
		if (cpComma<>nil) then	// If found, we terminate the current attribute (opt) string
			cpComma^ := #0;

		// Find attribute name/value separator
		colon := AnsiStrScan(opt, ':');
		if (colon<>nil) then
		begin
			// If found, we terminate the current attribute name and point on the value
			colon^:=#0;
			Inc(colon);
		end;
		if (0 = CompStr(opt, 'italics',True)) then //'italics'
		begin
			Include(sty.FontStyles,fsItalic);
		end else
		if (0 = CompStr(opt, 'notitalics')) then //notitalics
		begin
			Exclude(sty.FontStyles,fsItalic);
		end else
		if (0 = CompStr(opt, 'bold')) then //bold
		begin
			Include(sty.FontStyles,fsBold);
		end else
		if (0 = CompStr(opt, 'notbold')) then //notbold
		begin
			Exclude(sty.FontStyles,fsBold);
		end else
		if (0 = CompStr(opt, 'font')) then //font
		begin
			sty.FontName:=colon;
		end else
		if (0 = CompStr(opt, 'name')) then
		begin
			name:=colon;
			name:=StrReplaceChar(name,'|',',');
      wasset[5]:=true;
		end else
		if (0 = CompStr(opt, 'fore')) then //'fore'
		begin
			sty.ForeColor:=ColourFromString(Trim(colon),clSilver);
		end else
		if (0 = CompStr(opt, 'back')) then //back
		begin
			sty.BackColor:=ColourFromString(Trim(colon),clBlack);
		end else
		if (0 = CompStr(opt, 'size')) then //size
		begin
			val (colon,sty.FontSize,code);
		end else
		if (0 = CompStr(opt, 'eolfilled')) then //eolfilled
		begin
			EOLFilled:=true;
			wasset[4]:=true;
		end else
		if (0 = CompStr(opt, 'noteolfilled')) then //noteolfilled
		begin
			EOLFilled:=false;
			wasset[4]:=true;
		end else
		if (0 = CompStr(opt, 'underlined')) then //underlined
		begin
			Include(sty.FontStyles,fsUnderline);
		end else
		if (0 = CompStr(opt, 'notunderlined')) then //notunderlined
		begin
			Exclude(sty.FontStyles,fsUnderline);
		end else
		if (0 = CompStr(opt, 'case')) then //case
		begin
			wasset[0]:=true;
			ccase:= CASE_MIXED;
			if (colon<>nil) then
			begin
				if (colon^ = 'u') then
					ccase:= CASE_UPPER
				else if (colon^ = 'l') then
					ccase:= CASE_LOWER;
			end;
		end else
		if (0 = CompStr(opt, 'visible')) then //visible
		begin
			wasset[1]:=true;
			Vis:=true;
		end else
		if (0 = CompStr(opt, 'notvisible')) then //notvisible
		begin
			wasset[1]:=true;
			Vis:=false;
		end else
		if (0 = CompStr(opt, 'changeable')) then //changeable
		begin
			wasset[2]:=true;
			Changeable:=true;
		end else
		if (0 = CompStr(opt, 'notchangeable')) then //notchangeable
		begin
			wasset[2]:=true;
			Changeable:=false;
		end else
		if (0 = CompStr(opt, 'hotspot')) then //hotspot
		begin
			wasset[3]:=true;
			Hotspot:=true;
		end else
		if (0 = CompStr(opt, 'nothotspot')) then //nothotspot
		begin
			wasset[3]:=true;
			Hotspot:=false;
		end;

		if (cpComma<>nil) then
			opt := cpComma + 1
		else
			opt := nil;
	end;
	if vxal<>nil then
	begin
		FreeMem(vxal);
	end;
	if wasset[0] then sty.CharCase:=ccase;
	if wasset[1] then sty.Visible:=vis;
	if wasset[2] then sty.Changeable:=Changeable;
	if wasset[3] then sty.Hotspot:=Hotspot;
	if wasset[4] then sty.EOLFilled:=EOLFilled;
	if wasset[5] then sty.Name:=name;

	Result:=true;
end;

procedure TSciPropertyLoader.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditor) and (Operation = opRemove) then FEditor := nil;
end;

constructor TSciPropertyLoader.Create(AOwner : TComponent);
begin
  inherited;
  fFileName:='';
	fDefaultStyle.EOLFilled:=False;
	fDefaultStyle.Visible:=True;
	fDefaultStyle.CharCase:=CASE_MIXED;
	fDefaultStyle.Changeable:=True;
	fDefaultStyle.Hotspot:=False;
	fDefaultStyle.FontStyles:=[];
	fDefaultStyle.FontName:='';
	fDefaultStyle.FontSize:=0;
	fDefaultStyle.ForeColor:=clDefault;
	fDefaultStyle.BackColor:=clDefault;
  fOtherSettings:=TStringList.Create;
  TStringList(fOtherSettings).CaseSensitive:=False;
  TStringList(fOtherSettings).Duplicates:=dupIgnore;
  TStringList(fOtherSettings).Sorted:=True;

end;

destructor  TSciPropertyLoader.Destroy;
begin
  if fOtherSettings<>nil then fOtherSettings.Free;
  inherited;
end;

procedure TSciPropertyLoader.FillWithDefaultStyle(var sty : TSciLangStyle);
begin
	sty.EOLFilled :=fDefaultStyle.EOLFilled;
	sty.Visible   :=fDefaultStyle.Visible;
	sty.CharCase  :=fDefaultStyle.CharCase;
	sty.Changeable:=fDefaultStyle.Changeable;
	sty.Hotspot   :=fDefaultStyle.Hotspot;
	sty.FontStyles:=fDefaultStyle.FontStyles;
	sty.FontName  :=fDefaultStyle.FontName;
	sty.FontSize  :=fDefaultStyle.FontSize;
	sty.ForeColor :=fDefaultStyle.ForeColor;
	sty.BackColor :=fDefaultStyle.BackColor;
end;

function    TSciPropertyLoader.LoadFromStream(fs : TStream) : Boolean;
var
	lst : TStringList;
	cnt,i,aStyle,speedcnt,ignorelines : Integer;
	aLanguage,aProp,aValue,tmpstr : String;
	Handled : Boolean;
begin
	lst:=nil;
  if fEditor=nil then
  begin
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,['TSciPropertyLoader']);
    Result:=false;
    Exit;
  end;
	try
    fOtherSettings.Clear;
		lst:=TStringList.Create;
		lst.LoadFromStream(fs);
		cnt :=lst.Count;
		ignorelines:=0;
		for i:=0 to (cnt-1) do
		begin
			speedcnt:=0;
			if ignorelines>0 then
			begin
				Dec(ignorelines);
				Continue;
			end;
			tmpstr:=Trim(lst.Strings[i]);
			if tmpstr='' then Continue;

			if Pos('#',tmpstr)=1 then Continue;
			while (tmpstr[Length(tmpstr)]='\') and ((i+speedcnt)<=(cnt-1)) do
			begin
				Inc(speedcnt);
        Delete(tmpstr,Length(tmpstr),1);
				tmpstr:=Trim(tmpstr)+' '+Trim(lst.Strings[i+speedcnt]);
			end;
			if speedcnt<>0 then ignorelines:=speedcnt;
			if PropLineSplit(tmpstr,aLanguage,aStyle,aProp,aValue) then
			begin
        if ProcessDataStart(aLanguage,aProp,aStyle,aValue)=false then
        begin
	        Handled:=false;
	        if assigned(fOnPropLoadItem) then
            fOnPropLoadItem(self,aLanguage,aProp,aStyle,aValue,Handled);
          if Handled=False then
            ProcessDataEnd(aLanguage,aProp,aStyle,aValue);
        end;

			end;//proplinesplit
		end;//for

	finally
		if lst<>nil then lst.Free;
	end;
  if fEditor is TScintilla then
    TScintilla(fEditor).LanguageManager.Update;
	Result:=true;
end;

function    TSciPropertyLoader.SaveToStream(fs : TStream) : Boolean;
var
	lst : TStringList;
	langcnt,stylcnt,kwdcnt,i,j,aStyle,aindex : Integer;
	itm : TSciLangItem;
	tmpstr,prefix,langname,aLanguage,aProp,aValue : String;
	tmpclr : TColor;
	More : Boolean;

begin
  lst:=nil;
  if fEditor=nil then
  begin
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,[TSciPropertyLoader]);
    Result:=false;
    Exit;
  end;
	try
    fDefaultStyle.ForeColor:=fEditor.Font.Color;
    fDefaultStyle.BackColor:=fEditor.Color;
    fDefaultStyle.FontName:=fEditor.Font.Name;
    fDefaultStyle.FontSize:=fEditor.Font.Size;
    fDefaultStyle.FontStyles:=fEditor.Font.Style;
		lst:=TStringList.Create;
		lst.Add(sInstructionHeader1);
		lst.Add(sInstructionHeader2);
		lst.Add('');
    tmpstr:='';

		lst.Add('# ---------------- '+sPropBaseSettings+' '+sPropSectionStart+' ----------------');
		GenerateDefaultPropLine(tmpstr,fEditor);
		lst.Add(tmpstr);
		lst.Add('default.WordWrap='+BoolToString(fEditor.WordWrap));
		lst.Add('default.Unicode='+BoolToString(fEditor.UseUnicode));
    lst.Add('default.WordChars='+FEditor.WordChars);
		lst.Add('default.ClearUndoAfterSave='+BoolToString(fEditor.ClearUndoAfterSave));
		tmpclr:=fEditor.GetCaretFore;
		lst.Add(Format('default.CaretFore=%s',[ColourToString(tmpclr)])); //caretfore
		tmpclr:=fEditor.GetCaretLineBack;
		lst.Add(Format('default.CaretBack=%s',[ColourToString(tmpclr)])); //caretback
		lst.Add(Format('default.CaretLineVisible=%s',[BoolToString(fEditor.GetCaretLineVisible)]));
		lst.Add(Format('default.CaretPeriod=%d',[fEditor.GetCaretPeriod]));
    lst.Add(Format('default.CaretWidth=%d',[fEditor.GetCaretWidth]));
    lst.Add(Format('default.IndentWidth=%d',[fEditor.GetIndent]));
    lst.Add(Format('default.TabWidth=%d',[fEditor.GetTabWidth]));
    lst.Add(Format('default.EOLMode=%d',[fEditor.GetEOLMode]));
    if fEditor is TScintillaMemo then
    begin
      lst.Add(Format('default.SelectForeColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.SelFore)]));
      lst.Add(Format('default.SelectBackColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.SelBack)]));
      lst.Add(Format('default.MarkerForeColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.MarkerFore)]));
      lst.Add(Format('default.MarkerBackColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.MarkerBack)]));
      lst.Add(Format('default.BookMarkForeColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.BookMarkFore)]));
      lst.Add(Format('default.BookMarkBackColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.BookMarkBack)]));
      lst.Add(Format('default.FoldMarginHighlightColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.FoldHi)]));
      lst.Add(Format('default.FoldMarginColor=%s',[ColourToString(TScintillaMemo(fEditor).Colors.FoldLo)]));
      lst.Add(Format('default.ActiveHotspotForeColor=%s',[ColourToString(TScintillaMemo(fEditor).ActiveHotSpot.ForeColor)]));
      lst.Add(Format('default.ActiveHotspotBackColor=%s',[ColourToString(TScintillaMemo(fEditor).ActiveHotSpot.BackColor)]));
      lst.Add(Format('default.ActiveHotspotUnderlined=%s',[BoolToString(TScintillaMemo(fEditor).ActiveHotSpot.Underlined)]));
      lst.Add(Format('default.ActiveHotspotSingleLine=%s',[BoolToString(TScintillaMemo(fEditor).ActiveHotSpot.SingleLine)]));
      lst.Add('default.Gutter='+BoolToString(Boolean(TScintillaMemo(fEditor).Gutter1.Width<>0)));
      lst.Add('default.LineNumbers='+BoolToString(Boolean(TScintillaMemo(fEditor).Gutter0.Width<>0)));
      lst.Add('default.KeepIndent='+BoolToString(KeepIndent in TScintillaMemo(fEditor).Indentation));
      lst.Add('default.TabIndents='+BoolToString(TabIndents in TScintillaMemo(fEditor).Indentation));
      lst.Add('default.BackspaceUnIndents='+BoolToString(BackSpaceUnIndents in TScintillaMemo(fEditor).Indentation));
      lst.Add('default.IndentationGuides='+BoolToString(IndentationGuides in TScintillaMemo(fEditor).Indentation));

    end;
    lst.Add(Format('default.EdgeColumn=%d',[fEditor.GetEdgeColumn]));
    lst.Add(Format('default.EdgeMode=%d',[fEditor.GetEdgeMode]));
    lst.Add(Format('default.EdgeColor=%s',[ColourToString(fEditor.GetEdgeColour)]));

    if fEditor is TScintilla then
    begin
		  lst.Add('default.CodeFolding='+BoolToString(foldFold in TScintilla(fEditor).Folding));
    end;
		lst.Add('# ---------------- '+sPropBaseSettings+' '+sPropSectionEnd+' ----------------');
    if fEditor is TScintilla then
    begin
		lst.Add(TScintilla(fEditor).LanguageManager.GetLexerProps);
		lst.Add(Extmapper.GetProps);
    end;
		if assigned(fOnPropSaveItem) then
		begin
			More:=True;
			aindex:=0;
			while(More=True) do
			begin
				More:=false;
				aProp:='';
				aLanguage:='';
				aValue:='';
				aStyle:=-1;
				fOnPropSaveItem(Self,aindex,aLanguage,aProp,aStyle,aValue,More);
				if (aProp='') and (aLanguage='') and (aValue='') then More:=false
				else
				begin
					if aStyle=-1 then
						lst.Add(Format('%s.%s=%s',[aLanguage,aProp,aValue]))
					else
						lst.Add(Format('%s.%s.%d=%s',[aLanguage,aProp,aStyle,aValue]));
				end;
				Inc(aindex);
			end;
		end;
    if fEditor is TScintilla then
    begin
      langcnt:=TScintilla(fEditor).LanguageManager.LanguageList.Count;
      for i:=0 to (langcnt-1) do
      begin
        itm:=TSciLangItem(TScintilla(fEditor).LanguageManager.LanguageList.Items[i]);
        langname :=itm.Name;
        if langname='' then langname:=IntToStr(i);
        prefix :=langname+'.';
        lst.Add(Format('# ---------------- '+sLMLanguage+' '+sPropSectionStart+': %s ----------------',[langname]));
        with itm do
        begin
          kwdcnt:=itm.Keywords.Count;
          if (kwdcnt>0) then
          begin
            lst.Add(Format('# 	%s for %s: ',[sLMKeywords,langname]));
          end;
          for j:=0 to (kwdcnt-1) do
          begin
            GenerateKwdPropLine(tmpstr,langname+'.keywords.',TSciKeywords(itm.Keywords.Items[j]));
            if tmpstr<>'' then
            begin
              lst.Add(tmpstr);
              lst.Add('');
            end;
          end;
          stylcnt:=itm.Styles.Count;
          if (stylcnt>0) then
          begin
            lst.Add('');
            lst.Add(Format('# 	%s for %s: ',[sLMStyles,langname]));
          end;
          for j:=0 to (stylcnt-1) do
          begin
            GenerateStylePropLine(tmpstr,prefix+'style.',TSciStyle(itm.Styles.Items[j]));
            if tmpstr<>'' then lst.Add(tmpstr);
          end;
          lst.Add(Format('# ---------------- '+sLMLanguage+' '+sPropSectionEnd+': %s ----------------',[langname]));
          lst.Add('');

        end;
      end;
    end;
    if fOtherSettings.Count>0 then lst.AddStrings(fOtherSettings);
		lst.SaveToStream(fs);
		Result:=true;
		Exit;
	finally
		if lst<>nil then lst.Free;
	end;
  Result:=false;

end;
function    TSciPropertyLoader.Load : Boolean;
var
	fs : TFileStream;
begin
	fs:=nil;
	try
		try
			fs:=TFileStream.Create(fFileName,fmOpenRead);
			LoadFromStream(fs);
			Result:=true;
		except
			on EStreamError do
			begin
				Result:=false;
        raise Exception.CreateResFmt(@sCouldntLoadFromFile,[fFileName]);
			end;
		end;
	finally
		if fs<>nil then fs.Free;
	end;
end;

// Parses the Categorynames: lexer, extension,default
function TSciPropertyLoader.ProcessDataStart(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
var
	tmpint,code : Integer;
	tmpsty : TSciLangStyle;
begin
  Result:=False;
	if (CompStr(CategoryName,'lexer')=0) then  // If it is a language definition
	begin
    if Editor is TScintilla then
		  TScintilla(Editor).LanguageManager.AddLanguage(PropName,aValue);
    Result:=True;
	end else
	if CompStr(CategoryName,'extension')=0 then // If it is a extension definition
	begin
		ExtMapper.Add(PropName,aValue);
		Result:=True;
	end else
	if (CompStr(CategoryName,'default')=0)  then //If it is the default style
	begin
    if CompStr('CaretFore',PropName)=0 then //caretfore
		begin
			fEditor.SetCaretFore(ColorToRGB(ColourFromString(ValueExtract(aValue,'CaretFore'),clBlack)));
			Result:=True;
		end else if CompStr('CaretBack',PropName)=0 then //caretlineback
		begin
			fEditor.SetCaretLineBack(ColorToRGB(ColourFromString(ValueExtract(aValue,'CaretBack'),clSilver)));
			Result:=True;
		end else if CompStr('CaretPeriod',PropName)=0 then
		begin
			Val(ValueExtract(aValue,'CaretPeriod'),tmpint,code);
			fEditor.SetCaretPeriod(tmpint);
			Result:=True;
		end else if CompStr('CaretWidth',PropName)=0 then
		begin
			Val(ValueExtract(aValue,'CaretWidth'),tmpint,code);
			fEditor.SetCaretWidth(tmpint);
			Result:=True;
		end else if CompStr('CaretLineVisible',PropName)=0 then
		begin
			if CompStr(ValueExtract(aValue,'CaretLineVisible'),'True')=0 then
				fEditor.SetCaretLineVisible(True)
			else
				fEditor.SetCaretLineVisible(False);
			Result:=True;
		end	else if (CompStr(PropName,'style')=0) then
		begin
			tmpsty.EOLFilled:=false;
			tmpsty.Visible:=True;
			tmpsty.CharCase:=CASE_MIXED;
			tmpsty.Changeable:=True;
			tmpsty.Hotspot:=False;
      tmpsty.FontStyles:=[];
      tmpsty.FontName:='';
      tmpsty.FontSize:=0;
      tmpsty.ForeColor:=clDefault;
      tmpsty.BackColor:=clDefault;
			ParseStyleDefinition(PChar(aValue),tmpsty);
      fDefaultStyle.FontName:=tmpsty.FontName;
      fDefaultStyle.FontSize:=tmpsty.FontSize;
      fDefaultStyle.FontStyles:=tmpsty.FontStyles;
      fDefaultStyle.ForeColor:=tmpsty.ForeColor;
      fDefaultStyle.BackColor:=tmpsty.BackColor;
      fDefaultStyle.CharCase:=tmpsty.CharCase;
      fDefaultStyle.Visible:=tmpsty.Visible;
      fDefaultStyle.Changeable:=tmpsty.Changeable;
      fDefaultStyle.Hotspot:=tmpsty.Hotspot;
      fDefaultStyle.EOLFilled:=tmpsty.EOLFilled;
			fEditor.Color:=tmpsty.BackColor;
			fEditor.Font.Color:=tmpsty.ForeColor;
			fEditor.Font.Name:=tmpsty.FontName;
			fEditor.Font.Size:=tmpsty.FontSize;
			fEditor.Font.Style:=tmpsty.FontStyles;
			Result:=True;
		end else if (CompStr(PropName,'WordWrap')=0) then //WordWrap
		begin
			fEditor.WordWrap:=StringToBool(aValue);
			Result:=True;
		end else if (CompStr(PropName,'WordChars')=0) then //WordWrap
		begin
			fEditor.WordChars:=aValue;
			Result:=True;
		end	else if (CompStr(PropName,'EOLMode')=0) then			//SelBack
    begin
      Val(ValueExtract(aValue,'EOLStyle'),tmpint,code);
      fEditor.SetEOLMode(tmpint);
      Result:=True;
    end else if (CompStr(PropName,'Unicode')=0) then			//UseUnicode
		begin
			fEditor.UseUnicode:=StringToBool(aValue);
			Result:=True;
		end	else if (CompStr(PropName,'ClearUndoAfterSave')=0) then			//ClearUndoAfterSave
		begin
			fEditor.ClearUndoAfterSave:=StringToBool(aValue);
			Result:=True;
		end	else if (CompStr(PropName,'IndentWidth')=0) then			//ClearUndoAfterSave
		begin
      Val(ValueExtract(aValue,'IndentWidth'),tmpint,code);
			fEditor.SetIndent(tmpint);
			Result:=True;
		end	else if (CompStr(PropName,'EdgeColumn')=0) then			//ClearUndoAfterSave
		begin
      Val(ValueExtract(aValue,'EdgeColumn'),tmpint,code);
			fEditor.SetEdgeColumn(tmpint);
			Result:=True;
		end else if (CompStr(PropName,'EdgeColor')=0) then			//ClearUndoAfterSave
		begin
			fEditor.SetEdgeColour(ColourFromString(aValue,clDefault));
			Result:=True;
		end else if (CompStr(PropName,'EdgeMode')=0) then			//ClearUndoAfterSave
		begin
      Val(ValueExtract(aValue,'EdgeMode'),tmpint,code);
			fEditor.SetEdgeMode(tmpint);
			Result:=True;
		end else if (CompStr(PropName,'TabWidth')=0) then			//ClearUndoAfterSave
		begin
      Val(ValueExtract(aValue,'TabWidth'),tmpint,code);
			fEditor.SetTabWidth(tmpint);
			Result:=True;
		end else if (fEditor is TScintillaMemo) then
    begin
      if (CompStr(PropName,'LineNumbers')=0) then			//ClearUndoAfterSave
      begin
        if StringToBool(aValue)=True then
        begin
          TScintillaMemo(fEditor).Gutter0.Width:=32;
          TScintillaMemo(fEditor).Gutter0.MarginType:=gutLineNumber;
        end else
        begin
          TScintillaMemo(fEditor).Gutter0.Width:=0;
        end;
        Result:=True;
      end else if (CompStr(PropName,'Gutter')=0) then			//ClearUndoAfterSave
      begin
        if StringToBool(aValue)=True then
        begin
          TScintillaMemo(fEditor).Gutter1.Width:=16;
          TScintillaMemo(fEditor).Gutter1.MarginType:=gutSymbol;
        end else
        begin
          TScintillaMemo(fEditor).Gutter1.Width:=0;
        end;
        Result:=True;
      end else
      if (CompStr(PropName,'SelectForeColor')=0) then			//SelFore
      begin
        TScintillaMemo(fEditor).Colors.SelFore:=ColourFromString(aValue,clYellow);
        Result:=True;
      end	else if (CompStr(PropName,'MarkerForeColor')=0) then
      begin
        TScintillaMemo(fEditor).Colors.MarkerFore:=ColourFromString(aValue,clYellow);
        Result:=True;
      end	else if (CompStr(PropName,'MarkerBackColor')=0) then
      begin
        TScintillaMemo(fEditor).Colors.MarkerBack:=ColourFromString(aValue,clBlue);
        Result:=True;
      end	else if (CompStr(PropName,'BookMarkForeColor')=0) then
      begin
        TScintillaMemo(fEditor).Colors.BookMarkFore:=ColourFromString(aValue,clWhite);
        Result:=True;
      end	else if (CompStr(PropName,'BookMarkBackColor')=0) then
      begin
        TScintillaMemo(fEditor).Colors.BookMarkBack:=ColourFromString(aValue,clGray);
        Result:=True;
      end	else if (CompStr(PropName,'FoldMarginHighlightColor')=0) then
      begin
        TScintillaMemo(fEditor).Colors.FoldHi:=ColourFromString(aValue,clBlack);
        Result:=True;
      end	else if (CompStr(PropName,'FoldMarginColor')=0) then
      begin
        TScintillaMemo(fEditor).Colors.FoldLo:=ColourFromString(aValue,clBlack);
        Result:=True;
      end	else
      if (CompStr(PropName,'KeepIndent')=0) then			//SelFore
      begin
        if StringToBool(aValue)=True then
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation+[KeepIndent]
        else
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation-[KeepIndent];
        Result:=True;
      end else
      if (CompStr(PropName,'TabIndents')=0) then			//SelFore
      begin
        if StringToBool(aValue)=True then
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation+[TabIndents]
        else
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation-[TabIndents];
        Result:=True;
      end else
      if (CompStr(PropName,'BackspaceUnIndents')=0) then			//SelFore
      begin
        if StringToBool(aValue)=True then
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation+[BackspaceUnIndents]
        else
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation-[BackspaceUnIndents];
        Result:=True;
      end else
      if (CompStr(PropName,'IndentationGuides')=0) then			//SelFore
      begin
        if StringToBool(aValue)=True then
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation+[IndentationGuides]
        else
          TScintillaMemo(fEditor).Indentation:=TScintillaMemo(fEditor).Indentation-[IndentationGuides];
        Result:=True;
      end else if (CompStr(PropName,'SelectBackColor')=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).Colors.SelBack:=ColourFromString(aValue,clNavy);
        Result:=True;
      end else if (CompStr(PropName,'ActiveHotspotForeColor')=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.ForeColor:=ColourFromString(aValue,clYellow);
        Result:=True;
      end else if (CompStr(PropName,'ActiveHotspotBackColor')=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.BackColor:=ColourFromString(aValue,clMaroon);
        Result:=True;
      end else if (CompStr(PropName,'ActiveHotspotUnderlined')=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.Underlined:=StringToBool(aValue);
        Result:=True;
      end else if (CompStr(PropName,'ActiveHotspotSingleLine')=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.SingleLine:=StringToBool(aValue);
        Result:=True;
      end;

      if fEditor is TScintilla then
      begin
        if (CompStr(PropName,'CodeFolding')=0) then			//CodeFolding
        begin
          if StringToBool(aValue)=True then
            TScintilla(fEditor).Folding:=TScintilla(fEditor).Folding+[foldFold]
          else
            TScintilla(fEditor).Folding:=TScintilla(fEditor).Folding-[foldFold];
          Result:=True;
        end else if (CompStr(PropName,'BraceHighlight')=0) then			//SelBack
        begin
          TScintilla(fEditor).BraceHilite:=StringToBool(aValue);
          Result:=True;
        end;
      end;
    end; //scintillamemo
	end;
end;


// Parses the Propnames: style,keywords
function TSciPropertyLoader.ProcessDataEnd(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
var
	kwdcnt,stycnt,srchend,j : Integer;
	wasdone : Boolean;
	tmpsty  : TSciLangStyle;
	stytmp  : TSciStyle;
	itm     : TSciLangItem;
	tmpkwd : TSciLangKeywords;
	kwdtmp : TSciKeywords;
	kwdname : String;

begin
  if (CompStr(PropName,'style')=0) or (CompStr(PropName,'keywords')=0) then
  begin
    if fEditor is TScintilla then
    begin
      Result:=false;
      itm:=TScintilla(fEditor).LanguageManager.LanguageList.Find(CategoryName);
      if itm=nil then
      begin
        itm:=TSciLangItem(TScintilla(fEditor).LanguageManager.LanguageList.Add);
        itm.Name:=CategoryName;
        itm.Lexer:=CategoryName;
      end;

      if itm<>nil then
      begin
        if (CompStr(PropName,'style')=0) then // If it is a style definition
        begin
          stycnt:=itm.Styles.Count;
          wasdone:=false;
          for j:=0 to (stycnt-1) do //Check if the style is in the list, and if so, just modify it.
          begin
            if TSciStyle(itm.Styles.Items[j]).StyleNumber=Num then
            begin
              TSciStyle(itm.Styles.Items[j]).AssignToRec(tmpsty);
              ParseStyleDefinition(PChar(aValue),tmpsty);
              TSciStyle(itm.Styles.Items[j]).AssignRec(tmpsty);
              wasdone:=true;
              Break;
            end;
          end;
          if wasdone=false then // If the style wasn't found
          begin
            stytmp:=TSciStyle(itm.Styles.Add);
            stytmp.AssignToRec(tmpsty);
            FillWithDefaultStyle(tmpsty);
            tmpsty.StyleNumber:=Num;
            ParseStyleDefinition(PChar(aValue),tmpsty);
            stytmp.AssignRec(tmpsty);
          end;

            //If the fEditor.parameter isn't nil and we're modifying the
            //currently active language then update display.


          //if itm.LanguageStr=fEditor.Highlighter.LanguageStr then
          //fEditor.Highlighter.Update;
          Result:=True;
        end else
        if CompStr(PropName,'keywords')=0 then // If it is a keyword definition
        begin
          kwdcnt:=itm.Keywords.Count;
          wasdone:=false;
          if (AnsiStrLIComp('name:',PChar(aValue),5)=0) then // If it's a keywordlist description
          begin
            srchend:=Pos('::',AValue);
            if srchend<>0 then
            begin
              kwdname:=Copy(aValue,6,srchend-6);
              Delete(aValue,1,srchend+1);
              kwdname:=StrReplaceChar(kwdname,'|',',');
            end else kwdname:='';
          end else kwdname:=''; //has'nt a keywordlist description

          for j:=0 to (kwdcnt-1) do //Check if the style is in the list, and if so, just modify it.
          begin
            if TSciKeywords(itm.Keywords.Items[j]).KeywordListNumber=Num then
            begin
              TSciKeywords(itm.Keywords.Items[j]).AssignToRec(tmpkwd);
              tmpkwd.KeyWords:=aValue;
              if kwdname<>'' then tmpkwd.Name:=kwdname;
              TSciKeywords(itm.Keywords.Items[j]).AssignRec(tmpkwd);
              wasdone:=true;
              Break;
            end;
          end;
          if wasdone=false then // If the keywordlist wasn't found
          begin
            kwdtmp:=TSciKeywords(itm.Keywords.Add);
            kwdtmp.AssignToRec(tmpkwd);
            if kwdname<>'' then tmpkwd.Name:=kwdname;
            tmpkwd.KeywordListNumber:=Num;
            tmpkwd.KeyWords:=aValue;
            kwdtmp.AssignRec(tmpkwd);
          end;//wasdone
          Result:=True;
        end;
      end;//itm<>nil
    end else//is not TScintilla
    Result:=True;
  end else //not keywords and not styles
  begin
    fOtherSettings.Add(CategoryName+'.'+PropName+'='+aValue);
    Result:=True;
  end;
end;

procedure TSciPropertyLoader.AddOtherSetting(const settingname : String;const value : String);
begin
  if fOtherSettings.IndexOf(settingname)<>-1 then
    fOtherSettings.Values[settingname]:=value
  else
    fOtherSettings.Add(settingname+'='+value);
end;

function TSciPropertyLoader.GetOtherSetting(const settingname : String;const defaultvalue : String;const addifnonexistent : Boolean) : String;
begin
  if fOtherSettings.IndexOfName(settingname)<>-1 then
  begin
    Result:=fOtherSettings.Values[settingname];
  end else
  begin
    if addifnonexistent=True then AddOtherSetting(settingname,defaultvalue);
    Result:=defaultvalue;
  end;
end;

function    TSciPropertyLoader.Save : Boolean;
var
	fs : TFileStream;
begin
	fs:=nil;
	try
		try
			fs:=TFileStream.Create(fFileName,fmCreate);
			SaveToStream(fs);
			Result:=true;
		except
			on EStreamError do
			begin
				Result:=false;
        raise Exception.CreateResFmt(@sCouldntWriteToFile,[fFileName]);
			end;
		end;
	finally
		if fs<>nil then fs.Free;
	end;
end;

function ExtMapper : TExtensionMapper;
begin
  if FExtMap=nil then
  FExtMap:=TExtensionMapper.Create;
  Result:=FExtMap;
end;

constructor TExtensionMapper.Create;
begin
  extensions:=nil;
  FillExtensionMap;
end;
destructor TExtensionMapper.Destroy;
begin
  if extensions<>nil then extensions.Free;
  inherited;
end;

{Fill the list with default extensionmappings.
}
procedure TExtensionMapper.FillExtensionMap;
begin
  if extensions=nil then extensions:=TStringList.Create;
  with extensions do
  begin
		Add('.c=C++/C');Add('.cc=C++/C');Add('.cpp=C++/C');Add('.cxx=C++/C');Add('.h=C++/C');Add('.hh=C++/C');Add('.hxx=C++/C');Add('.hpp=C++/C');
    Add('.cs=C#');
		Add('.rc=Resource');Add('.rc2=Resource');Add('.dlg=Resource');
		Add('.htm=HTML');Add('.html=HTML');Add('.asp=HTML');Add('.shtml=HTML');Add('.htd=HTML');Add('.htt=HTML');Add('.cfm=HTML');Add('.tpl=HTML');Add('.hta=HTML');
    Add('.php=HTML');Add('.php3=HTML');Add('.phtml=HTML');
		Add('.xml=XML');Add('.xsl=XML');Add('.svg=XML');Add('.xul=XML');Add('.xsd=XML');Add('.dtd=XML');Add('.xslt=XML');Add('.axl=XML');Add('.xrc=XML');Add('.bpk=XML');Add('.dpk=XML');Add('.dpr=XML');Add('.bpr=XML');Add('.ant=XML');
		Add('.wml=WML');
		Add('.pl=Perl');Add('.pm=Perl');Add('.cgi=Perl');Add('.pod=Perl');
		Add('.css=CSS');
		Add('.pas=Pascal');Add('.inc=Pascal');
		Add('.sql=SQL');
		Add('.vb=VB');Add('.bas=VB');Add('.frm=VB');Add('.cls=VB');Add('.ctl=VB');Add('.pag=VB');Add('.dsr=VB');Add('.dob=VB');
		Add('.vbs=VBScript');Add('.dsm=VBScript');
		Add('.java=Java');
		Add('.js=JavaScript');
		Add('.idl=IDL');Add('.odl=IDL');
		Add('.py=Python');Add('.pyw=Python');
		Add('.tcl=TCL/TK');
		Add('.lua=LUA');
    Add('.mib=asn1');
    Add('.vhdl=vhdl');
		Add('.diff=Diff');Add('.patch=Diff');
		Add('.mak=Makefile');Add('makefile=Makefile');Add('.make=Makefile');Add('.iface=Makefile');
		Add('.bat=Batch');Add('.cmd=Batch');Add('.nt=Batch');
		Add('.conf=Apache Config');
		Add('.properties=Properties');Add('.ini=Properties');Add('.inf=Properties');Add('.reg=Properties');Add('.url=Properties');Add('.cfg=Properties');Add('.cnf=Properties');
		Add('.ads=ADA');Add('.adb=ADA');
		Add('.spf=nnCronTab');Add('.tab=nnCronTab');
		Add('.lsp=Lisp');Add('.lisp=Lisp');
		Add('.scm=Scheme');Add('.smd=Scheme');Add('.ss=Scheme');
		Add('.au3=AutoIt 3');
    Add('.tex=tex');Add('.sty=tex');
    Add('.sh=bash');Add('.bsh=bash');
  end;
end;

// Add a language mapping.. New mappings overrides previous
procedure TExtensionMapper.Add(const mapfromext : String;const maptolexer : String);
var
  tmpstr : String;
begin
  if extensions=nil then Exit;
  if (mapfromext<>'') and (maptolexer<>'') then
  begin
  if mapfromext[1]<>'.' then
    tmpstr:='.'+mapfromext
  else tmpstr:=mapfromext;
  tmpstr:=tmpstr+'='+maptolexer;
  extensions.Insert(0,tmpstr); //Insert at top to override previous assignment
  Inc(extsadded);
  end;
end;

// Returns any extra languages that has been added. Used by SciPropertyMgr
function TExtensionMapper.GetProps : String;
var
  tmp : String;
  i : Integer;
begin
  tmp:='';
  if extensions=nil then
  begin
    Result:=tmp;
    Exit;
  end;
  if extsadded>0 then
  begin
    tmp:=#13#10+'# ---------------- '+sPropExtension+' '+sPropAssignments+' '+sPropSectionStart+' ----------------'+#13#10;
    for i:=0 to (extsadded-1) do
    begin
      if extensions.Names[i][1]<>'.' then
      tmp:=tmp+'extension.'+extensions.Strings[i]+#13#10
			else tmp:=tmp+'extension'+extensions.Names[i]+'='+ValueFromIndex(extensions,i)+#13#10;
    end;
    tmp:=tmp+'# ---------------- '+sPropExtension+' '+sPropAssignments+' '+sPropSectionEnd+' ----------------'+#13#10;
  end;
  Result:=tmp;
end;
function TExtensionMapper.DetectLexer(const extension : string) : String;
var
	i : Integer;
begin
  if extensions=nil then
  begin
    Result:='null';
    Exit;
  end;
  i:=extensions.IndexOfName(extension);
  if(i<>-1) then
  begin
		Result:=ValueFromIndex(extensions,i);
	end else
  Result :='null';
end;



end.