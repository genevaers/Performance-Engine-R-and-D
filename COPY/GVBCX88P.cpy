      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2008, 2016.                     *
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
      ****************************************************************
      *      GVBCX88P - FORMAT EXIT PARAMETERS                       *
      *                                                              *
      ****************************************************************

      *   THE VALUE OF X88PARM1-VIEW-ID IS PASSED FROM GVBMR88.
      *
       01  X88PARM1-VIEW-ID                PIC S9(09) COMP.

      *   THE LAYOUT OF X88PARM2-PRINT-REC IS SPECIFIED BY THE VIEW
      *   DEFINITION AND THE CURRENT VALUE IS PASSED TO THIS EXIT
      *   PROGRAM FROM GVBMR88.
      *
      *   TO POINT X88PARM2-PRINT-REC TO YOUR OWN STRUCTURE
      *   (FOR EXAMPLE, LS-PRINT-REC), EXECUTE THE STATEMENT:
      *
      *    SET ADDRESS OF LS-PRINT-REC TO
      *        ADDRESS OF X88PARM2-PRINT-REC
      *
      *   WHENEVER THIS PROGRAM IS GIVEN CONTROL.
      *
       01  X88PARM2-PRINT-REC              PIC  X(260).

      *   THE LAYOUT OF X88PARM3-STARTUP-PARMS IS USER-DEFINED. THE
      *   VALUE IS SET IN THE VIEW PROPERTIES AND PASSED TO THIS EXIT
      *   PROGRAM FROM GVBMR88.
      *
      *   TO POINT X88PARM3-STARTUP-PARMS TO YOUR OWN STRUCTURE
      *   (FOR EXAMPLE, LS-STARTUP-PARMS), EXECUTE THE STATEMENT:
      *
      *    SET ADDRESS OF LS-STARTUP-PARMS TO
      *        ADDRESS OF X88PARM3-STARTUP-PARMS
      *
      *   THE FIRST TIME THIS PROGRAM IS GIVEN CONTROL.
      *
       01  X88PARM3-STARTUP-PARMS          PIC  X(32).

      *   THE VALUES IN X88PARM4-REPORT-FIELDS ARE PASSED FROM GVBMR88.
      *   X88PARM4-REPORT-SECTION-ID VARIES WITH THE TYPE OF REPORT
      *   LINE BEING PASSED TO THE EXIT PROGRAM.
      *   X88PARM4-CURRENT-LINE-NBR IS INCREMENTED FOR EVERY PRINT LINE
      *   AND X88PARM4-CURRENT-PAGE-NBR IS INCREMENTED FOR EVERY PAGE.
      *   X88PARM4-MAX-PAGE-SIZE AND X88PARM4-MAX-LINE-SIZE ARE SET IN
      *   THE VIEW PROPERTIES.  X88PARM4-REPORT-LINE-LENGTH IS EQUAL TO
      *   THE SUM OF THE COLUMN WIDTHS, THE COLUMN SPACING AND (IN
      *   CATEGORIZED MODE) THE WIDTH OF THE SORT KEY HIERARCHY.
      *   X88PARM4-FISCAL-MONTH IS SET BY A PARM PASSED INTO GVBMR86 OR
      *   GVBMR90 AND IS ASSOCIATED WITH A CONTROL RECORD ID SPECIFIED
      *   IN THE VIEW PROPERTIES.  X88PARM4-COMPANY-NAME IS SET IN THE
      *   CONTROL RECORD POINTED TO BY THE VIEW PROPERTIES.
      *   X88PARM4-REPORT-TITLE AND X88PARM4-OWNER-USER-ID ARE SET
      *   IN THE VIEW DEFINITION.
      *
       01  X88PARM4-REPORT-FIELDS.
           05  X88PARM4-REPORT-SECTION-ID  PIC  X(02).
               88  X88PARM4-BLANK-LINE     VALUE 'BL'.
               88  X88PARM4-COLUMN-HEADING VALUE 'CH'.
               88  X88PARM4-DASH-LINE      VALUE 'BD'.
               88  X88PARM4-DETAIL-LINE    VALUE 'DL'.
               88  X88PARM4-PAGE-HEADING   VALUE 'PH'.
               88  X88PARM4-SORT-HEADING   VALUE 'SH'.
               88  X88PARM4-SUBTOTAL-LINE  VALUE 'SL'.
           05  X88PARM4-CURRENT-LINE-NBR   PIC  S9(4)  COMP.
           05  X88PARM4-CURRENT-PAGE-NBR   PIC  S9(7)  COMP-3.
           05  X88PARM4-MAX-PAGE-SIZE      PIC  S9(4)  COMP.
           05  X88PARM4-MAX-LINE-SIZE      PIC  S9(4)  COMP.
           05  X88PARM4-REPORT-LINE-LENGTH PIC  S9(4)  COMP.
           05  X88PARM4-FISCAL-MONTH.
               10  X88PARM4-FISCAL-CC      PIC  9(02).
               10  X88PARM4-FISCAL-YY      PIC  9(02).
               10  X88PARM4-FISCAL-MM      PIC  9(02).
           05  X88PARM4-COMPANY-NAME       PIC  X(80).
           05  X88PARM4-REPORT-TITLE       PIC  X(80).
           05  X88PARM4-OWNER-USER-ID      PIC  X(08).

      *   THE VALUES IN X88PARM5-RUN-FIELDS ARE PASSED FROM GVBMR88.
      *   X88PARM5-RUN-NUMBER IS ASSIGNED BY GVBMR86. BY DEFAULT,
      *   X88PARM5-RUN-DATE IS THE CURRENT DATE, BUT THIS MAY BE
      *   OVERRIDDEN BY A PARM PASSED INTO GVBMR86 OR GVBMR90.
      *   X88PARM5-PROCESS-DATE AND PARM5-PROCESS-TIME ARE SET
      *   TO THE DATE AND TIME THAT GVBMR90 IS RUN.
      *
       01  X88PARM5-RUN-FIELDS.
           05  X88PARM5-RUN-NUMBER         PIC S9(09) COMP.
           05  X88PARM5-RUN-DATE.
               10  X88PARM5-RUN-CC         PIC  9(02).
               10  X88PARM5-RUN-YY         PIC  9(02).
               10  X88PARM5-RUN-MM         PIC  9(02).
               10  X88PARM5-RUN-DD         PIC  9(02).
           05  X88PARM5-PROCESS-DATE.
               10  X88PARM5-PROCESS-CC     PIC  9(02).
               10  X88PARM5-PROCESS-YY     PIC  9(02).
               10  X88PARM5-PROCESS-MM     PIC  9(02).
               10  X88PARM5-PROCESS-DD     PIC  9(02).
           05  X88PARM5-PROCESS-TIME.
               10  X88PARM5-PROCESS-HH     PIC  9(02).
               10  X88PARM5-PROCESS-NN     PIC  9(02).
               10  X88PARM5-PROCESS-SS     PIC  9(02).

      *   X88PARM6-OUTPUT-RECORD-PTR SHOULD BE SET IN THE EXIT PROGRAM
      *   TO THE ADDRESS OF THE LINE TO BE PRINTED.
      *
       01  X88PARM6-OUTPUT-RECORD-PTR      POINTER.

      *   GVBMR88 DOES NOT GUARANTEE THAT WORKING STORAGE IN A COBOL
      *   PROGRAM WILL BE REMAIN IN THE SAME STATE ON SUBSEQUENT CALLS
      *   TO THE PROGRAM, SO ANY DATA ITEMS WHICH NEED TO BE RETAINED
      *   BETWEEN CALLS SHOULD BE PLACED IN A DYNAMICALLY ALLOCATED
      *   AREA.
      *
      *   THE FIRST TIME THE EXIT PROGRAM IS CALLED, THE GENEVA
      *   ROUTINE GVBUR05 SHOULD BE CALLED TO ALLOCATE A WORK AREA AND
      *   X88PARM7-WORK-AREA-ANCHOR SHOULD BE SET TO POINT TO IT.
      *
      *   FOR EXAMPLE:
      *
      *    MOVE LENGTH OF LS-WORK-AREA TO WS-WORK-AREA-LENGTH
      *
      *    CALL GVBUR05
      *        USING X88PARM7-WORK-AREA-ANCHOR
      *              WS-WORK-AREA-LENGTH
      *
      *    SET ADDRESS OF LS-WORK-AREA TO
      *        X88PARM7-WORK-AREA-ANCHOR
      *
      *   ON SUBSEQUENT CALLS, X88PARM7-WORK-AREA-ANCHOR RETAINS ITS
      *   VALUE AND MAY BE POINTED TO THE STRUCTURE AGAIN.
      *   FOR EXAMPLE:
      *
      *    SET ADDRESS OF LS-WORK-AREA TO
      *        X88PARM7-WORK-AREA-ANCHOR
      *
       01  X88PARM7-WORK-AREA-ANCHOR       POINTER.

