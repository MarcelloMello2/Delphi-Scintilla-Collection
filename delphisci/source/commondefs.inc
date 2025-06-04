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
{.$Define NOMACRORECORD} {Uncomment to use macrorecording hooks}

{Notice: This is here because of some intention to eventually get these
controls to support more than BCB6 and Delphi7.
These are, for now only tested with bcb6 and d7.

Drop me a hint of how (if you you had to) modify the code to get it to
work.

I'm also interested in the package source files you've created for that
version, or other contributions of code.
}

		{$DEFINE COMPILER9}
		{$DEFINE DELPHI9}
		{$DEFINE DELPHICOMPILER9}

	{$IFDEF VER160}
		{$DEFINE COMPILER8}
		{$DEFINE DELPHI8}
		{$DEFINE DELPHICOMPILER8}
	{$ENDIF}

	{$IFDEF VER150}
		{$DEFINE COMPILER7}
		{$IFDEF BCB}
			{$DEFINE BCB7}
		{$ELSE}
			{$DEFINE DELPHI7}
			{$DEFINE DELPHICOMPILER7}
		{$ENDIF}
	{$ENDIF}

	{$IFDEF VER140}
		{$DEFINE COMPILER6}
		{$IFDEF BCB}
			{$DEFINE BCB6}
		{$ELSE}
			{$DEFINE DELPHI6}
			{$DEFINE DELPHICOMPILER6}
		{$ENDIF}
	{$ENDIF}

	{$IFDEF VER130}
		{$DEFINE COMPILER5}
		{$IFDEF BCB}
			{$DEFINE BCB5}
		{$ELSE}
			{$DEFINE DELPHI5}
			{$DEFINE DELPHICOMPILER5}
		{$ENDIF}
	{$ENDIF}

	{$IFDEF BCB}
		{$DEFINE CPPBUILDER}
		{$DEFINE BCBCOMPILER}
	{$ELSE ~BCB}
		{$DEFINE DELPHI}
		{$DEFINE DELPHICOMPILER}
	{$ENDIF ~BCB}

