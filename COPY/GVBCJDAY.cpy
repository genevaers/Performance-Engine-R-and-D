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
      *     GVBCJDAY - COBOL CALLING INTERFACE FOR GVBJDAY.
      *                PROVIDES A COBOL PROGRAM THE CAPABILITY
      *                TO CONVERT A GREGORIAN DATE TO A JULIAN (SERIAL)
      *                DAY
      *
      *     CALLING EXAMPLE:
      *         CALL 'GVBJDAY' USING
      *             JDAY-GREGORIAN-DATE
      *             JDAY-JULIAN-DAY
      *
      *     CODING RULES:
      *         1) JDAY-GREGORIAN-DATE MUST BE A VALID DATE IN
      *            CCYYMMDD FORMAT BETWEEN 18010101 AND 20991231.
      *
      *----------------------------------------------------------------*

       01  JDAY-PARAMETER-AREA.
           05  JDAY-INPUT.
               10  JDAY-GREGORIAN-DATE  PIC  9(008).
           05  JDAY-OUTPUT.
               10  JDAY-JULIAN-DAY      PIC S9(008) COMP.

