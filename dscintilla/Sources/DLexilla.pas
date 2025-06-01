{* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is DLexilla.pas
 *
 * The Initial Developer of the Original Code is Krystian Bigaj.
 *
 * Portions created by the Initial Developer are Copyright (C) 2010-2015
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * - Michal Gajek
 * - Marko Njezic
 * - Michael Staszewski
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *}

unit DLexilla;

interface

uses
  System.Classes, DScintillaTypes;

const
  cDLexillaDll  = 'Lexilla.dll';

type

{ TDLexilla }

  TDLexillaCreateLexer = function(Name: PAnsiChar): NativeUInt; stdcall;
  TDLexillaGetLexerCount = function: Integer; stdcall;
  TDLexillaGetLexerName = procedure(Index: Integer; Buffer: PAnsiChar; BufferLength: Integer); stdcall;
  TDLexillaLexerNameFromID = function(ID: Integer): PAnsiChar; stdcall;

  TDLexilla = class
  private
    FLexillaDllModule: string;
    FLexillaDllHandle: HMODULE;

    FCreateLexer: TDLexillaCreateLexer;
    FGetLexerCount: TDLexillaGetLexerCount;
    FGetLexerName: TDLexillaGetLexerName;
    FLexerNameFromID: TDLexillaLexerNameFromID;

    procedure LoadLexillaLibraryIfNeeded;
    procedure FreeLexillaLibrary;

    procedure InitializeFunctions;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function CreateLexer(const LexerName: UnicodeString): TDSciLexer;
    function GetLexerCount: Integer;
    function GetLexerName(const Index: Integer): UnicodeString;
    function LexerNameFromID(LexerID: Integer): UnicodeString;

    property LexillaDllHandle: HMODULE read FLexillaDllHandle;
  end;

implementation

{ TDLexilla }

uses
  System.SysUtils, WinApi.Windows;


constructor TDLexilla.Create;
begin
  FLexillaDllModule := cDLexillaDll;
end;

destructor TDLexilla.Destroy;
begin
  FreeLexillaLibrary;
  inherited;
end;

procedure TDLexilla.LoadLexillaLibraryIfNeeded;
begin
  if FLexillaDllHandle <> 0 then
    Exit;

  FLexillaDllHandle := LoadLibrary(PChar(FLexillaDllModule));
  if FLexillaDllHandle = 0 then
    RaiseLastOSError
  else
    InitializeFunctions;
end;

procedure TDLexilla.FreeLexillaLibrary;
begin
  if FLexillaDllHandle <> 0 then
  try
    FreeLibrary(FLexillaDllHandle);
  finally
    FLexillaDllHandle := 0;
  end;
end;

procedure TDLexilla.InitializeFunctions;
begin
  FCreateLexer := GetProcAddress(FLexillaDllHandle, 'CreateLexer');
  FGetLexerCount := GetProcAddress(FLexillaDllHandle, 'GetLexerCount');
  FGetLexerName := GetProcAddress(FLexillaDllHandle, 'GetLexerName');
  FLexerNameFromID := GetProcAddress(FLexillaDllHandle, 'LexerNameFromID');
end;

function TDLexilla.CreateLexer(const LexerName: UnicodeString): TDSciLexer;
begin
  LoadLexillaLibraryIfNeeded;
  Result := TDSciLexer(FCreateLexer(PAnsiChar(UTF8Encode(LexerName))));
end;

function TDLexilla.LexerNameFromID(LexerID: Integer): UnicodeString;
begin
  LoadLexillaLibraryIfNeeded;
  Result := string(FLexerNameFromID(LexerID));
end;

function TDLexilla.GetLexerCount: Integer;
begin
  LoadLexillaLibraryIfNeeded;
  Result := FGetLexerCount;
end;

function TDLexilla.GetLexerName(const Index: Integer): UnicodeString;
const
  BUFFER_LEN = 512;
var
  Buffer: array[0..BUFFER_LEN - 1] of AnsiChar;
begin
  LoadLexillaLibraryIfNeeded;
  FillChar(Buffer, SizeOf(Buffer), 0);
  FGetLexerName(Index, Buffer, SizeOf(Buffer));
  Result := string(Buffer);
end;

end.
