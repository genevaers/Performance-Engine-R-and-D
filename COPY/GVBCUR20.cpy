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
      *     GVBCUR20 - COBOL CALLING INTERFACE FOR GVBUR20.
      *
      *     COPYBOOK INSERTION EXAMPLE:
      *         COPY  GVBCUR20 REPLACING ==:XXX:== BY ==@@@==.
      *              WHERE "@@@" IS A UNIQUE FILE IDENTIFIER.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBUR20' USING UR20-@@@-INTERFACE.
      *              WHERE "@@@" IS THE FILE ID.
      *
      *     NOTES:  1) THE FIRST CALL OPENS THE FILE.
      *             2) THE NEXT CALL(S) PERFORMS THE I/O.
      *             3) THE LAST CALL CLOSES THE FILE.
      *
      *----------------------------------------------------------------*

       01  UR20-:XXX:-INTERFACE.
           05  UR20-:XXX:-FUNCTION          PIC S9(04)       COMP-4
                                               VALUE +0.
             88  UR20-:XXX:-OPEN               VALUE +0.
             88  UR20-:XXX:-CLOSE              VALUE +4.
             88  UR20-:XXX:-READ-SEQ-REC       VALUE +8.
             88  UR20-:XXX:-READ-DIR-BLOCK     VALUE +12.
             88  UR20-:XXX:-WRITE-REC          VALUE +16.
             88  UR20-:XXX:-WRITE-BLOCK        VALUE +20.
             88  UR20-:XXX:-READ-KEYED-REC     VALUE +24.
           05  UR20-:XXX:-RETURN-CODE       PIC S9(04)       COMP-4
                                               VALUE +0.
             88  UR20-:XXX:-RC-IS-A-OKAY       VALUE +0.
             88  UR20-:XXX:-RC-WARNING         VALUE +4.
             88  UR20-:XXX:-RC-END-OF-FILE     VALUE +8.
             88  UR20-:XXX:-RC-NOT-FOUND       VALUE +8.
             88  UR20-:XXX:-RC-PERMANENT-ERROR VALUE +16.
           05  UR20-:XXX:-ERROR-CODE        PIC S9(04)       COMP-4
                                               VALUE +0.
             88  UR20-:XXX:-SUCCESSFUL         VALUE +0.
             88  UR20-:XXX:-BAD-WRKAREA-PTR    VALUE +1.
             88  UR20-:XXX:-INVALID-FUNCTION   VALUE +2.
             88  UR20-:XXX:-INVALID-IO-OPTION  VALUE +3.
             88  UR20-:XXX:-ALREADY-OPENED     VALUE +4.
             88  UR20-:XXX:-OPEN-OUTPUT-FAILED VALUE +5.
             88  UR20-:XXX:-OPEN-INPUT-FAILED  VALUE +6.
             88  UR20-:XXX:-FILE-NEVER-OPENED  VALUE +7.
             88  UR20-:XXX:-ALREADY-CLOSED     VALUE +8.
           05  UR20-:XXX:-RECORD-LENGTH     PIC S9(04)       COMP-4
                                               VALUE +0.
           05  UR20-:XXX:-RECORD-AREA-PTR   POINTER
                                               VALUE NULL.
           05  UR20-:XXX:-RBN               PIC S9(09)       COMP-4
                                               VALUE +0.
           05  UR20-:XXX:-DDNAME            PIC  X(08)
                                               VALUE SPACES.
           05  FILLER  REDEFINES  UR20-:XXX:-DDNAME.
               10  UR20-:XXX:-DD-BASE       PIC  X(06).
               10  UR20-:XXX:-DD-ID         PIC  X(02).
           05  UR20-:XXX:-OPTION1           PIC  X(01)
                                               VALUE SPACES.
             88  UR20-:XXX:-OPEN-INPUT         VALUE 'I'.
             88  UR20-:XXX:-OPEN-OUTPUT        VALUE 'O'.
             88  UR20-:XXX:-OPEN-DIRECT        VALUE 'D'.
           05  UR20-:XXX:-OPTION2           PIC  X(01)
                                               VALUE LOW-VALUES.
           05  UR20-:XXX:-NO-OF-BUFFERS     PIC S9(04)       COMP-4
                                               VALUE +5.
           05  UR20-:XXX:-WORK-AREA-PTR     POINTER
                                               VALUE NULL.

