      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2008, 2022.                     *
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
      *     GVBCUR35 - COBOL CALLING INTERFACE FOR GVBUR35.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBUR35'  USING  UR35-PARAMETER-AREA.
      *
      *     THE GVBUR35 CALLING STRUCTURE MUST BE INITIALIZED TO
      *     LOW-VALUES (BINARY ZEROES) BEFORE IT IS POPULATED WITH
      *     THE DESIRED VALUES.
      *         EX:  MOVE  LOW-VALUES  TO  UR35-PARAMETER-AREA.
      *
      *---------------------------------------------------------------*
      * IF YOU CHANGE THIS PARAMETER LIST, LOOK THROUGH ALL USER EXITS
      * FOR REFERENCE TO GVBCUR35 AND UPDATE ANY USER COPYBOOKS/MACROS
      * TO BE CONSISTENT WITH THE CHANGES.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       01  UR35-PARAMETER-AREA.
           05  UR35-FUNCTION-CODE       PIC  X(01)
                                           VALUE SPACES.
             88  UR35-FUNCTION-ALLOCATE    VALUE '1'.
             88  UR35-FUNCTION-DEALLOCATE  VALUE '2'.
           05  FILLER                   PIC  X(01)
                                           VALUE SPACES.
           05  UR35-RETURN-CODE         PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-VOLUME-SEQ-NUMBER   PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-VOLUME-COUNT        PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-PRIMARY-SPACE       PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-SECONDARY-SPACE     PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-DCB-BLOCK-SIZE      PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-DCB-LRECL           PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  UR35-KEY-LENGTH          PIC S9(04) COMP-4
                                           VALUE ZEROES.
             88  UR35-KEY-LEN-NO           VALUE +0.
             88  UR35-KEY-LEN-VALID        VALUE +1 THRU +255.
           05  UR35-KEY-POSITION        PIC S9(04) COMP-4
                                           VALUE ZEROES.
             88  UR35-KEY-POS-NO           VALUE +0.
             88  UR35-KEY-POS-VALID        VALUE +1 THRU +256.
           05  UR35-RETENTION-PERIOD    PIC S9(04) COMP-4
                                           VALUE ZEROES.
             88  UR35-RET-PER-VALID        VALUE +0 THRU +9999.
           05  UR35-DIRECTORY-BLOCKS    PIC S9(04) COMP-4
                                           VALUE ZEROES.
           05  FILLER                   PIC  X(54)
                                           VALUE SPACES.
           05  UR35-DD-NAME             PIC  X(08)
                                           VALUE SPACES.
           05  UR35-DATASET-NAME        PIC  X(46)
                                           VALUE SPACES.
           05  UR35-MEMBER-NAME         PIC  X(08)
                                           VALUE SPACES.
           05  FILLER  REDEFINES  UR35-MEMBER-NAME.
               10  UR35-RELATIVE-GDG    PIC  X(08).
           05  UR35-EXISTING-DISP       PIC  X(03)
                                           VALUE SPACES.
           05  UR35-NORMAL-DISP         PIC  X(07)
                                           VALUE SPACES.
           05  UR35-CONDITIONAL-DISP    PIC  X(07)
                                           VALUE SPACES.
           05  UR35-VOLSER-NUMBER       PIC  X(06)
                                           VALUE SPACES.
           05  FILLER                   PIC  X(30)
                                           VALUE SPACES.
           05  UR35-CLOSE-INDICATOR     PIC  X(01)
                                           VALUE SPACES.
             88  UR35-FREE-AT-CLOSE        VALUE 'Y'.
           05  UR35-RECORD-FORMAT       PIC  X(02)
                                           VALUE SPACES.
           05  UR35-SPACE-TRACKS        PIC  X(01)
                                           VALUE SPACES.
             88  UR35-TRACKS-REQUESTED     VALUE 'Y'.
           05  UR35-SPACE-CYLINDERS     PIC  X(01)
                                           VALUE SPACES.
             88  UR35-CYLINDERS-REQUESTED  VALUE 'Y'.
           05  UR35-RELEASE-SPECIFIED   PIC  X(01)
                                           VALUE SPACES.
             88  UR35-RELEASE-YES          VALUE 'Y'.
           05  UR35-UNIT-NAME           PIC  X(08)
                                           VALUE SPACES.
           05  UR35-DATASET-ORG         PIC  X(04)
                                           VALUE SPACES.
             88  UR35-DSORG-PS             VALUE 'PS'.
             88  UR35-DSORG-PO             VALUE 'PO'.
             88  UR35-DSORG-VSAM           VALUE 'VSAM'.
             88  UR35-DSORG-DA             VALUE 'DA'.
           05  UR35-VSAM-ORGANIZATION   PIC  X(04)
                                           VALUE SPACES.
             88  UR35-VSAM-KSDS            VALUE 'KSDS'.
             88  UR35-VSAM-ESDS            VALUE 'ESDS'.
             88  UR35-VSAM-RRDS            VALUE 'RRDS'.
             88  UR35-VSAM-LSDS            VALUE 'LSDS'.
           05  UR35-EXP-DATE-CCYYDDD    PIC  X(07)
                                           VALUE SPACES.
           05  FILLER                   PIC  X(282)
                                           VALUE SPACES.
           05  UR35-SYSOUT-MSGCLASS     PIC  X(01)
                                           VALUE SPACES.
             88  UR35-SYSOUT-DEFAULT       VALUE '*' '$'.
           05  UR35-SYSOUT-HOLD         PIC  X(01)
                                           VALUE SPACES.
             88  UR35-SYSOUT-HOLD-YES      VALUE 'Y'.
           05  UR35-SYSOUT-COPIES       PIC S9(04) COMP-4
                                           VALUE ZEROES.
             88  UR35-SYSOUT-COPIES-VALID  VALUE +0 THRU +255.
           05  UR35-SYSOUT-OUTLIM       PIC S9(09) COMP-4
                                           VALUE ZEROES.
             88  UR35-SYSOUT-OUTLIM-VALID  VALUE +0 THRU +16777215.
