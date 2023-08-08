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
      *     GVBCDL96 - COBOL CALLING INTERFACE FOR GVBDL96.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBDL96' USING DL96-PARAMETER-AREA.
      *
      *----------------------------------------------------------------*
      * IF YOU CHANGE THIS PARAMETER LIST, LOOK THROUGH ALL USER EXITS
      * FOR REFERENCE TO GVBCDL96 AND UPDATE ANY USER COPYBOOKS/MACROS
      * TO BE CONSISTENT WITH THE CHANGES.

       01  DL96-PARAMETER-AREA.

          05 DL96-VALUE-PTR.
              10 DL96-VALUE-PTR-HIGH        POINTER.
              10 DL96-VALUE-PTR-LOW         POINTER.
          05 DL96-VALUE-LEN                 PIC S9(04)    COMP.
          05 DL96-MASK-LEN                  PIC S9(04)    COMP.
          05 DL96-MASK-PTR                  POINTER.
      *
          05 DL96-VALUE-FMT                 PIC S9(04)    COMP.
          05 DL96-VALUE-CONTENT             PIC S9(04)    COMP.
          05 DL96-VALUE-DECIMALS            PIC S9(04)    COMP.
          05 DL96-VALUE-SCALING             PIC S9(04)    COMP.
          05 DL96-VALUE-SIGNED              PIC  X(01).
      *
          05 DL96-OUTPUT-FMT                PIC S9(04)    COMP.
          05 DL96-OUTPUT-CONTENT            PIC S9(04)    COMP.
          05 DL96-OUTPUT-DECIMALS           PIC S9(04)    COMP.
          05 DL96-OUTPUT-SCALING            PIC S9(04)    COMP.
          05 DL96-OUTPUT-SIGNED             PIC  X(01).
          05 DL96-OUTPUT-JUSTIFY            PIC  X(01).
      *
          05 DL96-WORK                      PIC  X(127).
      *
          05 DL96-FORMAT-ERR                PIC  X(01).
