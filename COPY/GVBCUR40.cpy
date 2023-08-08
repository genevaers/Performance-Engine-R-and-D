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
      *     GVBCUR40 - COBOL CALLING INTERFACE FOR GVBUR40.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBUR40' USING UR40-PARAMETER-AREA.
      *
      *----------------------------------------------------------------*

       01  UR40-PARAMETER-AREA.
           05  FILLER                       PIC  X(04)
                                               VALUE LOW-VALUES.
           05  UR40-COMPRESSED-RECORD       USAGE IS POINTER
                                               VALUE IS NULL.
           05  UR40-EXPANDED-RECORD         USAGE IS POINTER
                                               VALUE IS NULL.
           05  UR40-DICTIONARY-FORMAT       PIC  X(04)
                                               VALUE SPACES.
           05  UR40-DICTIONARY-NAME         PIC  X(08)
                                               VALUE SPACES.
           05  UR40-COMPRESSED-LENGTH       PIC S9(04)      COMP-4
                                               VALUE ZEROES.
           05  UR40-EXPANDED-LENGTH         PIC S9(04)      COMP-4
                                               VALUE ZEROES.
           05  UR40-RETURN-CODE             PIC S9(04)      COMP-4
                                               VALUE +0.
             88  UR40-RC-BAD-FUNCTION          VALUE +8.
             88  UR40-RC-EXP-LENGTH-ERROR      VALUE +12.
             88  UR40-RC-COMPRESSION-ERROR     VALUE +16.
             88  UR40-RC-EXPANSION-ERROR       VALUE +20.
             88  UR40-RC-DICT-NOT-FOUND        VALUE +24.
             88  UR40-RC-DICT-NOT-DELETED      VALUE +28.
           05  UR40-FUNCTION-CODE           PIC  X(01)
                                               VALUE SPACES.
             88  UR40-FCN-COMPRESS             VALUE 'C'.
             88  UR40-FCN-EXPAND               VALUE 'D'.
             88  UR40-FCN-ALL-DONE             VALUE 'T'.

       01  UR40-CICS-PARAMETER-AREA.
           05  UR40-CICS-FLG                PIC  X(01)
                                               VALUE 'N'.
             88  UR40-CICS-YES                 VALUE 'Y'.
             88  UR40-CICS-NO                  VALUE 'N'.
