//CE_Desc_Include(helpdescriptions.txt)
{
  Author : Jan Martin Pettersen (hdalis)
  History 29/09/2004 Initial Release
          17/10/2004 Moved the TExtensionMap to SciPropertyMgr.Pas,
                     usually used there.
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
  Author  : hdalis (hdalis@users.sourceforge.net)
       $Id: SciDetectUtils.pas,v 1.7 2004/12/03 17:18:58 hdalis Exp $
  History : 29/09/2004 Initial Release
            13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                       generate the help.
                       The TExtensionMapper is moved to SciPropertyMgr.pas
            24/10/2004 Removed the TLexerNamesMap class, and the LexerMapper function
                       these are now part of the TSciLanguageManager class (functions AddLanguage,
                       DupLanguage,RemoveLanguage,FillListWithLanguages)

}
{.$WEAKPACKAGEUNIT ON}
unit SciDetectUtils;
interface
uses Classes,SciLexer,SciLexerMod,ScintillaLanguageManager;
{$Include commondefs.inc}

procedure SetLangWordChars(fEditor : TScintilla;const Lang : String);

implementation
uses SysUtils;

{Set the wordchars based on the requested language}
procedure SetLangWordChars(fEditor : TScintilla;const Lang : String);
var
	ny : String;
begin
	if lang='' then ny:=sci_alphachars+sci_numericchars+sci_accentedchars+'_'
	else if AnsiCompareText('PERL',lang)=0 then ny:=sci_alphachars+sci_numericchars+'_$@%&'
	else if (AnsiCompareText('CSS',lang)=0) or (AnsiCompareText('hypertext',lang)=0) or (AnsiCompareText('HTML',lang)=0) then ny:=sci_alphachars+sci_numericchars+'_-'
	else if AnsiCompareText('PHP',lang)=0 then ny:=sci_alphachars+sci_numericchars+'_-$'
	else if (AnsiCompareText('XML',lang)=0) or (AnsiCompareText('WML',lang)=0) then ny:=sci_alphachars+sci_numericchars+'_-'
	else if AnsiCompareText('LUA',lang)=0 then ny:=sci_alphachars+sci_numericchars+'_%'
	else if (AnsiCompareText('Ave',lang)=0) or (AnsiCompareText('cpp',lang)=0) or
  (AnsiCompareText('Resource',lang)=0) or (AnsiCompareText('IDL',lang)=0) or
  (AnsiCompareText('Java',lang)=0) or (AnsiCompareText('JavaScript',lang)=0) or
  (AnsiCompareText('C#',lang)=0) or (AnsiCompareText('POV',lang)=0) or
  (AnsiCompareText('Specman',lang)=0)  then ny:=sci_alphachars+sci_numericchars+'_#'
	else if AnsiCompareText('Baan',lang)=0 then ny:=sci_alphachars+sci_numericchars+'_#.$'
	else if AnsiCompareText('Forth',lang)=0 then ny:=sci_alphachars+sci_numericchars+'%-'
	else if (AnsiCompareText('Lisp',lang)=0) or (AnsiCompareText('Scheme',lang)=0) then ny:=sci_alphachars+sci_numericchars+'_-<>.#+@$%^&=*!?'
	else if AnsiCompareText('lot',lang)=0 then ny:=sci_alphachars+sci_numericchars
	else if AnsiCompareText('nnCronTab',lang)=0 then ny:=sci_alphachars+sci_numericchars+'%-'
	else if AnsiCompareText('Verilog',lang)=0 then ny:=sci_alphachars+sci_numericchars+'_`$#'
	else
		ny:='_'+sci_alphachars;

	fEditor.WordChars:=ny;
end;

end.
