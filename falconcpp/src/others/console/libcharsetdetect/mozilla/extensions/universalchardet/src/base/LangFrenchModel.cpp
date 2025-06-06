/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* ***** BEGIN LICENSE BLOCK *****
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
 * The Original Code is Mozilla Communicator client code.
 *
 * The Initial Developer of the Original Code is
 * Netscape Communications Corporation.
 * Portions created by the Initial Developer are Copyright (C) 1998
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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
 * ***** END LICENSE BLOCK ***** */

#include "nsSBCharSetProber.h"

static const unsigned char french_WINDOWS_1252CharToOrderMap[] = 
{
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255, 30, 40, 31, 37, 27, 44, 48, 52, 32, 45, 61, 25, 29, 41, 43,
 34, 53, 38, 35, 39, 46, 42, 68, 63, 65, 71,255,255,255,255,255,
255,  2, 20, 12, 11,  1, 17, 18, 21,  4, 24, 57,  9, 13,  5, 10,
 14, 19,  7,  3,  6,  8, 16, 54, 23, 28, 36,255,255,255,255,255,
 64,180,179,178,177, 75,176,175,174,173,172,171, 91,170,169,168,
167,166, 62,111,110, 67, 78, 88,165, 97,164, 90, 82,163,162,161,
160,159,158,157,156,155,154, 86, 84, 69,101, 50,109,153, 96,152,
 81,151,108,150,100,149,148, 80,147,107,146, 51,145,144,143,142,
 70,141, 79, 72,140,139,138, 73, 77, 60, 76,137,136,135, 99,134,
106,133,132,131, 83,130, 98, 92,129, 95,128, 94,127,126,125,124,
 22, 93, 49,123,105,104, 89, 47, 26, 15, 33, 74,122,121, 56, 66,
120,103,119,118, 55,102, 87,117,116, 59,115, 58, 85,114,113,112,
};

static const PRUint8 frenchLangModel[] = 
{
2,3,3,3,3,3,3,3,3,3,3,3,3,3,0,3,3,3,2,3,2,0,3,2,0,0,0,3,2,0,0,0,
0,0,0,3,0,0,1,1,0,0,0,1,0,0,2,0,2,0,2,0,0,3,1,0,2,3,0,0,0,0,1,0,
2,2,3,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3,3,3,0,2,3,0,0,0,3,0,0,0,0,
0,0,1,3,0,0,0,0,0,0,0,0,0,0,3,0,0,0,1,0,0,1,1,3,3,0,0,0,0,0,0,0,
3,3,3,3,2,3,2,3,3,3,1,3,3,3,3,1,2,2,3,2,2,0,0,1,1,3,0,3,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,2,0,0,1,0,1,2,2,0,0,0,2,0,0,
3,3,3,2,3,3,3,2,3,3,3,3,3,3,3,3,3,3,3,3,1,0,3,2,1,3,0,1,0,0,0,0,
1,2,0,3,0,0,0,2,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,2,0,0,0,0,0,0,0,
3,3,3,3,3,3,3,3,2,3,3,3,2,2,3,3,3,3,3,2,3,0,2,2,0,2,0,2,0,1,0,0,
3,0,0,3,0,0,0,0,1,0,0,1,0,0,3,0,2,0,1,0,0,0,1,0,2,1,0,0,0,2,0,0,
3,3,3,3,1,3,3,3,2,3,1,2,2,3,3,0,1,0,0,1,3,0,0,0,0,3,0,2,0,0,0,0,
3,0,0,2,0,0,0,0,0,0,0,0,0,0,0,1,2,0,1,0,0,1,3,1,0,0,0,0,0,0,0,0,
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,0,1,1,0,3,0,3,0,0,0,2,
3,0,0,2,0,0,0,0,0,0,0,0,0,0,3,0,3,0,1,0,0,1,3,1,3,2,0,0,0,0,0,0,
3,3,3,3,3,3,3,1,3,3,3,3,3,3,3,3,3,3,3,3,2,0,3,3,0,2,0,3,0,0,0,0,
3,0,0,2,0,0,0,0,0,0,0,0,0,0,1,1,2,0,1,0,0,0,0,1,2,0,0,0,0,2,0,0,
3,3,3,3,1,3,1,3,3,3,2,3,3,3,3,2,2,3,3,3,3,3,1,0,0,3,1,3,0,0,0,0,
2,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,3,0,1,0,0,0,2,1,2,2,0,0,0,3,0,0,
3,2,3,3,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3,3,2,0,2,2,0,2,0,3,0,0,0,0,
2,0,0,3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,2,2,3,3,0,0,0,0,0,
3,3,3,3,1,2,3,3,1,3,2,1,3,0,3,2,1,2,1,2,2,0,0,2,0,3,1,2,0,1,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1,2,2,0,2,0,0,0,2,0,0,
3,3,3,3,3,3,3,3,3,3,2,3,1,0,3,0,1,2,3,0,3,0,0,0,0,3,0,2,0,0,1,0,
1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,3,0,2,1,0,0,0,2,0,0,
3,3,2,3,3,3,1,3,2,3,0,0,3,3,3,0,0,1,0,3,0,0,0,0,0,3,0,2,0,0,0,0,
3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1,0,0,0,2,1,0,2,0,0,0,0,0,0,
3,3,3,3,3,3,3,3,3,3,1,2,0,3,3,0,0,2,0,1,3,0,2,0,0,3,0,2,0,0,0,0,
3,0,1,0,0,0,0,0,0,0,0,0,0,0,2,0,3,0,0,0,0,1,2,1,0,2,0,0,0,0,0,0,
3,3,3,2,3,3,3,3,3,3,3,3,3,3,2,3,3,3,3,3,2,0,0,3,0,0,0,0,0,0,0,0,
0,0,0,2,0,0,0,0,0,0,0,0,0,0,1,0,2,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,
3,3,0,3,0,0,3,3,0,3,0,0,0,0,3,0,0,1,0,0,0,0,0,0,0,3,0,1,0,0,0,0,
3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,2,0,0,0,0,0,1,0,0,
3,3,3,3,0,1,3,3,3,3,0,1,0,0,3,0,3,1,0,0,0,0,0,0,0,2,0,1,0,0,0,0,
2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,
3,3,3,3,3,3,3,3,3,3,1,0,2,0,3,1,0,2,0,2,3,0,0,0,0,3,0,2,0,0,0,0,
2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,
0,1,1,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
3,3,3,3,1,2,3,3,3,3,2,1,1,0,3,1,0,0,0,3,0,0,0,2,0,2,0,3,0,0,0,0,
3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,1,0,0,0,2,0,1,0,0,0,0,0,0,
3,3,1,3,3,3,3,3,2,3,1,0,2,2,3,0,0,0,0,0,0,0,0,0,0,3,0,2,0,0,0,0,
2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,3,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
3,3,0,3,0,3,0,2,0,2,0,3,0,3,2,0,0,0,2,0,2,0,1,0,0,1,0,1,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
3,3,0,1,0,0,0,3,0,3,0,0,0,2,1,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,
3,3,0,3,0,0,0,3,2,3,0,0,0,0,2,0,0,0,0,0,2,2,0,0,2,0,3,2,2,3,1,3,
0,0,1,0,1,0,2,0,1,1,2,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,2,1,2,0,0,
0,0,3,0,3,3,3,0,2,0,2,3,3,1,0,3,1,3,2,2,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,1,3,1,3,3,2,3,3,0,2,2,3,1,0,2,1,2,1,0,3,0,2,0,2,0,2,0,2,2,2,2,
0,2,3,1,2,3,2,2,3,1,1,1,0,2,0,1,0,0,0,2,0,0,0,0,0,0,0,0,0,0,2,0,
3,3,3,1,2,2,3,0,2,3,2,2,2,2,3,1,1,2,0,1,0,0,1,0,0,1,0,1,0,0,0,0,
0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
3,3,0,3,0,0,0,2,1,3,0,1,3,0,2,0,0,1,0,0,0,0,0,0,1,2,2,1,2,2,1,2,
2,2,1,0,0,0,1,1,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,1,2,3,3,2,3,3,3,0,2,2,2,3,0,2,2,2,1,3,3,0,0,2,2,0,1,2,2,1,2,2,
0,2,2,2,1,2,2,2,3,2,0,2,1,2,0,2,0,0,0,0,3,0,0,0,1,0,0,0,2,0,0,0,
3,3,0,2,0,0,2,2,3,3,0,0,0,0,2,0,1,0,0,0,3,0,0,0,2,0,2,2,0,2,1,2,
0,2,2,0,1,1,2,0,2,0,2,1,0,2,0,2,0,0,0,2,1,0,1,0,0,0,0,0,2,1,0,0,
1,0,2,0,3,2,2,0,3,1,2,2,2,0,0,1,0,1,0,1,0,0,0,0,2,0,3,0,2,2,2,3,
0,2,2,1,1,2,3,2,3,3,2,2,0,0,0,2,0,0,0,1,2,0,0,0,0,0,0,1,1,0,2,0,
0,0,0,0,2,3,0,0,2,0,0,3,3,1,0,3,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
3,3,0,3,0,0,3,3,3,3,0,0,0,0,2,0,0,0,0,0,3,0,0,0,2,2,2,1,0,2,1,2,
0,1,2,0,2,3,2,0,0,0,2,1,2,2,0,0,1,0,0,2,0,0,1,0,0,0,0,1,1,0,0,0,
3,3,0,3,0,2,0,3,1,3,0,2,1,2,2,0,0,0,1,0,2,0,0,0,0,1,2,2,1,2,1,2,
0,2,2,0,1,1,2,2,1,0,2,2,0,2,0,0,0,0,0,1,0,0,0,0,1,1,0,2,2,0,0,0,
3,3,0,2,1,0,1,1,1,3,0,0,1,1,2,0,0,1,0,1,1,0,0,0,0,1,0,3,0,0,0,0,
0,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
3,3,0,3,0,0,2,3,0,2,0,0,1,2,3,0,0,0,0,0,0,0,0,1,1,2,3,0,0,2,1,2,
0,0,2,0,0,2,0,0,1,1,2,2,1,2,0,1,0,0,0,1,0,0,0,0,0,0,0,2,0,1,0,0,
3,3,0,2,0,0,0,2,0,3,0,0,0,0,3,0,0,0,0,0,2,0,0,0,2,0,3,1,2,2,2,2,
0,2,2,0,2,2,2,2,2,2,3,1,0,2,0,1,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,
3,3,1,2,0,0,3,3,0,3,0,1,0,0,2,0,0,0,0,0,3,0,0,0,2,1,3,1,1,2,2,3,
0,1,2,0,0,2,2,0,1,1,2,1,0,2,0,1,0,0,0,3,0,2,0,0,0,0,0,2,0,0,0,0,
3,3,0,3,0,0,3,2,3,3,0,0,0,0,2,0,0,0,0,0,0,0,0,0,2,0,2,0,1,2,1,2,
0,0,1,0,2,2,1,1,2,0,2,1,1,2,0,0,1,0,0,0,0,1,0,0,0,0,0,2,0,0,0,0,
3,3,0,3,0,0,0,2,0,3,0,1,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2,0,0,2,2,2,
0,2,3,0,2,1,3,1,2,1,1,2,0,2,0,2,0,0,0,0,1,0,1,0,0,0,0,1,1,0,0,0,
3,3,0,3,0,0,1,2,2,3,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2,1,0,2,0,2,
1,0,0,0,1,2,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,1,2,3,1,3,3,2,0,1,2,1,2,0,2,1,1,0,2,3,0,0,0,2,0,2,0,2,1,2,2,
0,2,2,0,2,2,2,2,3,2,1,2,2,2,0,1,0,0,0,0,0,0,0,0,1,0,2,0,0,0,1,0,
2,3,0,2,1,0,3,2,2,3,0,0,0,1,3,0,0,0,0,1,1,0,1,0,2,0,2,0,2,3,0,2,
1,2,2,0,0,2,0,1,1,0,2,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,
3,3,0,0,0,0,0,2,0,3,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2,0,0,2,0,0,
0,0,0,0,2,0,1,0,0,0,2,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
0,0,0,0,3,2,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,3,0,2,2,2,2,
0,2,2,0,2,3,2,2,2,1,2,1,0,0,0,2,0,0,0,0,0,0,0,0,1,0,0,0,0,0,2,0,
0,3,0,0,0,0,0,3,0,3,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
2,3,0,2,0,0,3,3,1,3,0,0,1,0,2,0,0,0,0,0,1,0,0,0,1,0,2,1,0,2,0,1,
1,1,1,0,0,1,1,2,2,2,2,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
0,0,1,2,2,3,0,0,3,0,0,3,2,2,0,1,0,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,1,1,0,0,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,0,0,0,2,0,2,0,2,2,2,2,
0,2,2,0,2,1,2,2,2,2,2,1,2,1,0,1,0,0,0,2,2,0,0,0,0,0,0,1,0,0,0,0,
0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
3,3,0,2,0,0,0,3,0,3,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,3,1,0,2,0,2,
0,0,0,0,1,2,1,1,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,2,0,0,0,0,
0,1,1,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
2,2,3,2,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,2,3,0,0,3,0,2,0,2,2,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,3,3,0,0,2,0,0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
2,2,2,3,0,1,1,2,1,3,0,1,2,0,1,0,1,1,0,1,2,0,0,0,0,0,0,1,0,0,0,0,
0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,1,3,2,0,2,0,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0,0,0,0,0,2,1,0,2,0,2,2,1,1,0,2,0,2,0,1,0,0,0,0,2,0,2,0,2,0,2,0,
0,2,2,0,2,2,1,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
2,3,0,2,0,0,2,1,1,2,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,2,0,1,1,0,0,
0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
2,2,0,2,0,0,0,2,0,2,0,0,0,0,2,0,0,0,0,0,2,1,0,0,0,0,2,1,0,2,0,2,
1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,
1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,3,
0,0,0,0,0,0,2,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
};

const SequenceModel WINDOWS_1252frenchModel = 
{
  french_WINDOWS_1252CharToOrderMap,
  frenchLangModel,
  (float)0.985451,
  PR_TRUE,
  "WINDOWS-1252",
  "french"
};
