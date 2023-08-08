      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2022.                           *
      *     Copyright Contributors to the GenevaERS Project.          *
      * SPDX-License-Identifier: Apache-2.0                           *
      *                                                               *
      *****************************************************************
      *                                                               *
      * Licensed under the Apache License,                            *
      * Version 2.0 (the "License");                                  *
      * you may not use this file except in                           *
      * compliance with the License.                                  *
      * You may obtain a copy of the License at                       *
      *                                                               *
      *     http://www.apache.org/licenses/LICENSE-2.0                *
      *                                                               *
      *  Unless required by applicable law or                         *
      *  agreed to in writing, software                               *
      *  distributed under the License is distributed                 *
      *  on an "AS IS" BASIS,                                         *
      *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express *
      *  or implied.                                                  *
      *  See the License for the specific language governing          *
      *  permissions and limitations under the License.               *
      *                                                               *
      *---------------------------------------------------------------*
      *     GVBCGDAT - COBOL CALLING INTERFACE FOR GVBGDAT.
      *                PROVIDES A COBOL PROGRAM THE CAPABILITY
      *                TO CONVERT A JULIAN (SERIAL) DAY TO A
      *                GREGORIAN DATE.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBGDAT' USING GDAT-PARAMETER-AREA.
      *
      *     CODING RULES:
      *         1) GDAT-JULIAN-DAY MUST BE BETWEEN 2378862 AND
      *            2488069.
      *
      *----------------------------------------------------------------*

       01  GDAT-PARAMETER-AREA.
           05  GDAT-OUTPUT.
               10  GDAT-GREGORIAN-DATE  PIC  9(008).
           05  GDAT-INPUT.
               10  GDAT-JULIAN-DAY      PIC S9(008) COMP.

