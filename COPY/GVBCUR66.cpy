      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2008, 2011.                     *
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
      *     GVBCUR66 - COBOL CALLING INTERFACE FOR GVBUR66.
      *                PROVIDES A COBOL PROGRAM THE CAPIBILITY
      *                TO USE THE MVS ENQ/DEQ FACILITY FOR
      *                CONTROLLING ACCESS TO A SPECIFIED RESOURCE.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBUR66' USING UR66-PARAMETER-AREA.
      *
      *     CODING RULES:
      *         1) UR66-REQUEST-TYPE IS REQUIRED.  A VALUE OF "ENQ"
      *            OR "DEQ" MUST BE SPECIFIED.
      *         2) UR66-MAJOR-NAME IS REQUIRED.
      *         3) UR66-MINOR-NAME IS REQUIRED.
      *         4) UR66-CONTROL-TYPE IS OPTIONAL.  THE DEFAULT IS "E"
      *            WHICH REQUESTS EXCLUSIVE CONTROL.  A SHARED REQUEST
      *            IS SPECIFIED WITH AN "S".
      *         5) UR66-SCOPE-REQUEST IS OPTIONAL.  THE DEFAULT IS "3"
      *            WHICH REQUESTS A SCOPE OF 'SYSTEMS'.
      *
      *----------------------------------------------------------------*

       01  UR66-PARAMETER-AREA.
           05  UR66-REQUEST-TYPE        PIC  X(003)
                                           VALUE SPACES.
             88  UR66-REQ-ENQ              VALUE 'ENQ'.
             88  UR66-REQ-DEQ              VALUE 'DEQ'.
           05  UR66-CONTROL-TYPE        PIC  X(001)
                                           VALUE 'E'.
             88  UR66-CNTR-EXCLUSIVE       VALUE 'E'.
             88  UR66-CNTR-SHARED          VALUE 'S'.
           05  UR66-MAJOR-NAME          PIC  X(008)
                                           VALUE SPACES.
           05  UR66-MINOR-NAME          PIC  X(128)
                                           VALUE SPACES.
           05  UR66-SCOPE-REQUEST       PIC  X(001)
                                           VALUE '3'.
             88  UR66-SCOPE-STEP           VALUE '1'.
             88  UR66-SCOPE-SYSTEM         VALUE '2'.
             88  UR66-SCOPE-SYSTEMS        VALUE '3'.
           05  FILLER                   PIC  X(003)
                                           VALUE SPACES.
