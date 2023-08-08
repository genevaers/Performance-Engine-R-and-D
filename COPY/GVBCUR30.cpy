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
      *     GVBCUR30 - COBOL CALLING INTERFACE FOR GVBUR30.
      *
      *     CALLING EXAMPLE:
      *         CALL  'GVBUR30' USING UR30-AREA
      *                               UR30-SQL-LENGTH
      *                               UR30-SQL
      *                               UR30-DBMS-CONNECTION
      *
      *----------------------------------------------------------------*

       01  UR30-PARAMETER-AREA.
           05  UR30-FUNCTION-CODE      PIC S9(04) COMP.
               88  UR30-88-FUNCTION-OPEN          VALUE +0.
               88  UR30-88-FUNCTION-CLOSE         VALUE +4.
               88  UR30-88-FUNCTION-READ-SEQ      VALUE +8.
           05  UR30-RETURN-CODE        PIC S9(04) COMP.
               88  UR30-88-SUCCESSFUL             VALUE +0.
               88  UR30-88-SUCCESSFUL-WARNING     VALUE +4.
               88  UR30-88-END-OF-TABLE           VALUE +8.
               88  UR30-88-PERMANENT-ERROR        VALUE +16.
           05  UR30-ERROR-CODE         PIC S9(04) COMP.
               88  UR30-88-NO-ERROR               VALUE +0.
               88  UR30-88-BAD-WORK-AREA-PTR      VALUE +1.
               88  UR30-88-UNDEFINED-FUNCTION     VALUE +2.
               88  UR30-88-UNDEFINED-I-O-MODE     VALUE +3.
               88  UR30-88-TABLE-ALREADY-OPEN     VALUE +4.
               88  UR30-88-OPEN-FOR-OUTPUT-FAILED VALUE +5.
               88  UR30-88-OPEN-FOR-INPUT-FAILED  VALUE +6.
               88  UR30-88-TABLE-NEVER-OPENED     VALUE +7.
               88  UR30-88-TABLE-ALREADY-CLOSED   VALUE +8.
               88  UR30-88-BAD-ROW-LENGTH         VALUE +9.
           05  UR30-RECORD-LENGTH      PIC S9(04) COMP.
           05  UR30-RECORD-AREA-ADDRESS POINTER.
           05  UR30-RBN                PIC S9(08) COMP.
           05  UR30-DDNAME             PIC  X(08) VALUE SPACES.
           05  UR30-OPTION1            PIC  X(01).
               88  UR30-88-INPUT-SQL-MODE         VALUE 'I'.
               88  UR30-88-INPUT-VSAM-MODE        VALUE 'V'.
           05  UR30-OPTION2            PIC  X(01).
               88  UR30-88-SQL-FORMATTED          VALUE 'F'.
               88  UR30-88-UNFORMATTED            VALUE 'U'.
           05  UR30-NO-OF-BUFFERS      PIC  S9(04) COMP.
           05  UR30-AREA-POINTER       POINTER.

       01  FILLER.
           05  UR30-SQL-LENGTH         PIC S9(08) COMP.
      *  IF YOUR SQL STATEMENT WILL NOT FIT IN 1000 CHARACTERS,
      *  DEFINE YOUR OWN DATA ITEM.
           05  UR30-SQL                PIC  X(1000).
           05  UR30-DBMS-CONNECTION    PIC  X(48).
