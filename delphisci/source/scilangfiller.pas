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
  Author: hdalis (hdalis@users.sourceforge.net)
  $Id: scilangfiller.pas,v 1.7 2004/12/03 17:19:00 hdalis Exp $
  Purpose: Functions for filling default languages or a null language in the designer.
  History: 29/09/2004 Initial Release
           17/10/2004 Added property editor for the Lexer property of
                      TSciLangItem
           30/10/2004 Added TSciLangSettings record, and the AddIt function has
                      a new parameter of this type to define the commenting style and other things
                      for the languages.
					 19/11/2004 Now you can select exactly what languages to add.
}


unit Scilangfiller;
interface
uses  SciSupport,ScintillaLanguageManager;
{$Include commondefs.inc}

procedure FillLanguages(langlist : TSciLangList;const usejustnull : Boolean=false;const stylesonly : Boolean=false);
implementation
uses SysUtils, Classes,Graphics, SciWhatToFillUnit,Controls;

type
TSciLangSettings = Record
{$IFNDEF NOCOMMENTING}
  CommentStart,CommentMiddle,CommentEnd,CommentOneliner : String;
  CommentAtLineStart : Boolean;
{$ENDIF}
end;


{$Include ScintillaLanguageStyles.inc}

procedure FillLanguages(langlist : TSciLangList;const usejustnull : Boolean;const stylesonly : Boolean);
var
frm : TTSciWhatToFillForm;

procedure AddIt(const langname : String;KeywordRecs : array of TSciLangKeywords;xStyles : array of TSciLangStyle;xLangSettings : TSciLangSettings;
const Lexer : String='');
	var
		i : integer;
		itm : TSciLangItem;
	begin
    itm:=langlist.Find(langname);
    if (itm<>nil) and (itm.Lexer='null') then
    begin
      if Lexer='' then itm.Lexer:=langname
      else itm.Lexer:=Lexer;
    end;
    if itm=nil then
    begin
		  itm :=TSciLangItem(langlist.Add);
      //tmprem itm.LanguageStr:=langname;
      itm.Name:=langname;
      if Lexer='' then itm.Lexer:=langname
      else
        itm.Lexer:=Lexer;
    end;
    {$IFNDEF NOCOMMENTING}
    itm.CommentStart:=xLangSettings.CommentStart;
    itm.CommentMiddle:=xLangSettings.CommentMiddle;
    itm.CommentEnd:=xLangSettings.CommentEnd;
    itm.CommentOneliner:=xLangSettings.CommentOneliner;
    itm.CommentAtLineStart:=xLangSettings.CommentAtLineStart;
    {$ENDIF}
		try
			if stylesonly=false then itm.KeyWords.BeginUpdate;
			itm.Styles.BeginUpdate;
			for i:=Low(DefaultStyles) to High(DefaultStyles) do
        TSciStyle(itm.Styles.Add).AssignRec(DefaultStyles[i]);
			if stylesonly=false then
			begin
			for i := Low(KeywordRecs) to High(KeywordRecs) do
				TSciKeyWords(itm.Keywords.Add).AssignRec(KeywordRecs[i]);
			end;
			for i := Low(xStyles) to High(xStyles) do
				TSciStyle(itm.Styles.Add).AssignRec(xStyles[i]);
		finally
			itm.Styles.EndUpdate;
			if stylesonly=false then
			  itm.KeyWords.EndUpdate;
		end;
	end;

begin
	try
		langlist.BeginUpdate;
if usejustnull=True then	langlist.Clear;
		AddIt('null',[],[],cpplanguagesettings);
		if (usejustnull=False) then
			begin
      frm:=TTSciWhatToFillForm.Create(nil);
      if frm.ShowModal=mrOk then
      begin
			if frm.c0.Checked=True then AddIt('C++/C',CPPKeywords,CPPStyles,cpplanguagesettings,'cpp');
			if frm.c1.Checked=True then AddIt('Java',JavaKeywords,CPPStyles,cpplanguagesettings,'cpp');
			if frm.c2.Checked=True then AddIt('JavaScript',JavaScriptKeywords,CPPStyles,cpplanguagesettings,'cpp');
			if frm.c3.Checked=True then AddIt('Resource',ResourceKeywords,CPPStyles,cpplanguagesettings,'cpp');
			if frm.c4.Checked=True then AddIt('IDL',IDLKeywords,CPPStyles,cpplanguagesettings,'cpp');
			if frm.c5.Checked=True then AddIt('cppnocase',[],CPPStyles,cpplanguagesettings);
			if frm.c6.Checked=True then AddIt('Pascal',PascalKeywords,PascalStyles,pascallanguagesettings,'pascal');
			if frm.c7.Checked=True then AddIt('VB',VBKeywords,VBStyles,vblanguagesettings,'vb');
			if frm.c8.Checked=True then AddIt('VBScript',VBKeywords,VBStyles,vblanguagesettings,'vbscript');
			if frm.c9.Checked=True then AddIt('Python',PythonKeywords,PythonStyles,pythonlanguagesettings,'python');
			if frm.c10.Checked=True then AddIt('Ruby',RubyKeywords,PythonStyles,pythonlanguagesettings,'ruby');
			if frm.c11.Checked=True then AddIt('CSS',CSSKeywords,CSSStyles,csslanguagesettings,'css');
			if frm.c12.Checked=True then AddIt('PERL',PerlKeywords,PerlStyles,pythonlanguagesettings,'perl');
			if frm.c13.Checked=True then AddIt('HTML',HTMLKeywords,HTMLStyles,htmllanguagesettings,'hypertext');
			if frm.c14.Checked=True then AddIt('XML',[],XMLStyles,htmllanguagesettings,'xml');
			if frm.c15.Checked=True then AddIt('SQL',SQLKeywords,SQLStyles,pythonlanguagesettings,'sql');
			if frm.c16.Checked=True then AddIt('PowerBasic',[],VBStyles,vblanguagesettings,'powerbasic');
			if frm.c17.Checked=True then AddIt('TCL/TK',TCLKeywords,TCLStyles,tcllanguagesettings,'tcl');
			if frm.c18.Checked=True then AddIt('Batch',BatKeywords,BatStyles,batlanguagesettings,'batch');
			if frm.c19.Checked=True then AddIt('Properties',[],PropsStyles,pythonlanguagesettings,'props');
			if frm.c20.Checked=True then AddIt('Makefile',[],MakeStyles,pythonlanguagesettings,'makefile');
			if frm.c21.Checked=True then AddIt('Diff',[],DiffStyles,pythonlanguagesettings,'diff');
			if frm.c22.Checked=True then AddIt('Apache Config',ConfKeywords,ConfStyles,pythonlanguagesettings,'conf');
			if frm.c23.Checked=True then AddIt('LUA',LuaKeywords,LuaStyles,lualanguagesettings,'lua');
			if frm.c24.Checked=True then AddIt('WML',WMLKeywords,XMLStyles,htmllanguagesettings,'xml');
			if frm.c25.Checked=True then AddIt('ADA',AdaKeywords,AdaStyles,adalanguagesettings,'ada');
			if frm.c26.Checked=True then AddIt('nnCronTab',NncrontabKeywords,NncrontabStyles,nncrontablanguagesettings,'nncrontab');
			if frm.c27.Checked=True then AddIt('C#',CSKeywords,CPPStyles,cpplanguagesettings,'cpp');
			if frm.c28.Checked=True then AddIt('Lisp',LispKeywords,LispStyles,lisplanguagesettings,'lisp');
			if frm.c29.Checked=True then AddIt('Scheme',SchemeKeywords,LispStyles,lisplanguagesettings,'lisp');
			if frm.c30.Checked=True then AddIt('ErrorList',[],[],cpplanguagesettings,'errorlist');
      if frm.c31.Checked=True then AddIt('MMixal',MMixalKeywords,MMixalStyles,cpplanguagesettings,'mmixal');
      if frm.c32.Checked=True then AddIt('LaTeX',[],LatexStyles,cpplanguagesettings,'latex');
      if frm.c33.Checked=True then AddIt('AutoIt 3',AutoItKeywords,AutoItStyles,autoitlanguagesettings,'au3');
      if frm.c34.Checked=True then AddIt('VHDL',VHDLKeywords,VHDLStyles,vhdllanguagesettings,'vhdl');
      if frm.c35.Checked=True then AddIt('ASN1',ASN1Keywords,ASN1Styles,cpplanguagesettings,'asn1');
      if frm.c36.Checked=True then AddIt('VXML',VXMLKeywords,XMLStyles,htmllanguagesettings,'xml');
      if frm<>nil then frm.Free;
      end;
			//AddIt('batch',[],[]);
		end;
  finally
		langlist.EndUpdate;
  end;
end;

end.
