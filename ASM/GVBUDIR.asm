**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2023.
*     Copyright Contributors to the GenevaERS Project.
* SPDX-License-Identifier: Apache-2.0
*
**********************************************************************
*
*  Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
*  or implied.
*  See the License for the specific language governing permissions
*  and limitations under the License.
*
***********************************************************************
*
***********************************************************************
*
*  This program obtains a list of members in a PDS/PDSE and writes
*  An output file to DDNAME DDDRUCK. It is run before GVBUPDS.
*
***********************************************************************
         TITLE    'EXAMPLE: OBTAIN LIST OF MEMBERS IN PDSE OR PDS'
*
         IHASAVER DSECT=YES,SAVER=YES,SAVF4SA=YES,SAVF5SA=YES,TITLE=NO
*
         YREGS
*
*        DYNAMIC AREA
*
DYNAREA  DSECT
*
SAVEAREA DC    18F'0'
SVA2     DS    18F
*
         DS    0F
OUTDCB   DS    XL(OUTFILEL)    REENTRANT DCB AND DCBE AREAS
         DS    0F
OU2DCB   DS    XL(OU2FILEL)    REENTRANT DCB AND DCBE AREAS
         DS    0F
INDCB    DS    XL(SINDCBEL)    REENTRANT DCB AND DCBE AREAS
*
         DS    0F
WKDECB   DS    F
         DS    0F
REENTWK  DS    XL256              RE-ENTRANT PARAMETER   LIST
WKPRINT  DS    XL131           PRINT LINE
         DS    XL1
         DS   0F
WKREENT  DS    XL256           REENTRANT WORKAREA
WKDBLWK  DS    XL08            DOUBLE WORK WORKAREA
WKDDNAME DS    CL8
WKRETC   DS    F
WKBUFFER DS    A
WKTOKNRC DS    A                  NAME/TOKEN  SERVICES RETURN CODE
WKTOKNAM DS    XL16               TOKEN NAME
WKTOKN   DS   0XL16               TOKEN VALUE
WKTOKNCTT DS   A                  A(CTT)
WKECBLST DS    A                  ADDRESS OF ECB LIST TO WAIT ON
WORKDATE DS    CL16
W_TOD    DS    CL16
IPLSTCK  DS    D
CONV_MFL DS    0F
         STCKCONV MF=L
CONV_LEN EQU *-CONV_MFL
SAVR13   DS    A
INAREA   DS    CL256              AREA TO READ DIRECTORY INTO
TTRN     DS    F               TTRN OF THE START OF THE MEMBER
BLDLLIST DS    0F              LIST OF MEMBER NAMES FOR BLDL
BLDLENUM DS    H               NUMBER OF ENTRIES (10 FOR EXAMPLE)
BLDLEBYT DS    H               NUMBER OF BYTES PER ENTRY
BLDLMEMB DS    CL8             NAME OF MEMBER, SUPPLIED BY USER
         DS    CL3             TTR OF FIRST RECORD (SET BY BLDL)
*                              THE FOLLOWING 3 FIELDS ARE SET BY BLDL
         DS    X               K BYTE, CONCATENATION NUMBER
         DS    X               Z BYTE, LOCATION CODE
         DS    X               C BYTE, FLAG AND USER DATA LENGTH
*
         DS    A
         DS    A
         DS    H
PDSENTLN DS    H
PDSENTNM DS 800CL8
DYNLEN   EQU   *-DYNAREA                 DYNAMIC AREA LENGTH
*
*
PDSEDIR  DSECT
PDSELL   DS    H
PDSENM   DS    CL8
PDSEREST DS    CL34
DDSEENTL EQU   *-PDSENM
*
*
EXAMPLE  RMODE 24
EXAMPLE  AMODE 31
*
EXAMPLE  CSECT
*
*        ENTRY LINKAGE
*
         STM   R14,R12,12(R13)           PUSH CALLER REGISTERS
         LLGTR R12,R15                   ESTABLISH ...
         USING EXAMPLE,R12               ... ADDRESSABILITY
*
         GETMAIN R,LV=DYNLEN             GET DYNAMIC STORAGE
         LR    R11,R1                    MOVE GETMAINED ADDRESS TO R11
         USING DYNAREA,11                ADDRESSABILITY TO DSECT
         ST    R13,SAVEAREA+4            SAVE CALLER SAVE AREA ADDRESS
         LA    R15,SAVEAREA              GET ADDRESS OF OWN SAVE AREA
         ST    R15,8(,R13)               STORE IN CALLER SAVE AREA
         LR    R13,R15                   GET ADDRESS OF OWN SAVE AREA
*
         GETMAIN R,LV=32768
         ST    R1,WKBUFFER
*
*      OPEN MESSAGE FILE
         LA    R14,OUTFILE               COPY MODEL   DCB
D1       USING IHADCB,OUTDCB
         MVC   OUTDCB(OUTFILEL),0(R14)
         LAY   R0,OUTDCB                 SET  DCBE ADDRESS IN  DCB
         AGHI  R0,OUTFILE0
         STY   R0,D1.DCBDCBE
*
         LAY   R2,OUTDCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)
         TM    48(R2),X'10'              SUCCESSFULLY OPENED  ??
         JO    MAIN_100                  YES - BYPASS ABEND
         WTO 'MBTEST: DDPRINT OPEN FAILED'
         J     DONE
*
*                                                                @I1015
*      ECHO DDNAME
MAIN_100 EQU   *
         MVC   WKDDNAME,=CL8'DDPRINT'
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(32),=CL32'MBTEST: BEING EXECUTED WITH DD:'
         MVC   WKPRINT+32(8),WKDDNAME
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
* GET LIST OF MEMBERS INTO THE TABLE PDSENTNM
*
         LA    R14,OU2FILE               COPY MODEL   DCB
D2       USING IHADCB,OU2DCB
         MVC   OU2DCB(OU2FILEL),0(R14)
         LAY   R0,OU2DCB                 SET  DCBE ADDRESS IN  DCB
         AGHI  R0,OU2FILE0
         STY   R0,D2.DCBDCBE
*
         LAY   R2,OU2DCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)
         TM    48(R2),X'10'              SUCCESSFULLY OPENED  ??
         JO    MAIN_102                  YES - BYPASS ABEND
         WTO 'MBTEST: DDDRUCK OPEN FAILED'
         J     DONE
*
MAIN_102 EQU   *
         LA    R14,SINDCB                COPY MODEL   DCB
D3       USING IHADCB,INDCB
         MVC   INDCB(SINDCBEL),0(R14)
         LAY   R0,INDCB                  SET  DCBE ADDRESS IN  DCB
         AGHI  R0,SINDCBL
         STY   R0,D3.DCBDCBE
*
         LAY   R2,INDCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT)
         TM    48(R2),X'10'              SUCCESSFULLY OPENED  ??
         JO    A0100                     YES - BYPASS ABEND
         WTO 'MBTEST: PDSEINDD OPEN FAILED'
         J     A0112
*
         USING PDSEDIR,R7
A0100    EQU   *
         WTO 'PDSE DD OPENED'
         LAY   R4,PDSENTNM
         GET   INDCB,INAREA       READ MEMBER DIRECTORY RECORD
A0104    EQU   *
         LAY   R7,INAREA
         LGH   R5,PDSELL
         LGR   R3,R5
         AR    R5,R7
         AGHI  R5,-2              ADJUST MAX ADDR FOR LENGTH (H)
         AGHI  R3,-2              ADJUST LENGTH OF ENTRIES FOR LEN (H)
         XR    R2,R2
         D     R2,=A(DDSEENTL)
         AH    R3,PDSENTLN        ACCUMULATE TOTAL NUMBER OF MEMBERS
         STH   R3,PDSENTLN
A0108    EQU   *
         CLI   PDSENM,X'FF'
         JE    A0110
*
         MVC   0(8,R4),PDSENM
         LA    R4,8(R4)
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(17),=CL17'MBTEST: XXXXXXXX '
         MVC   WKPRINT+8(8),PDSENM
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         LA    R2,OU2DCB
         LA    R0,PDSENM
         PUT   (R2),(R0)
*
         LA    R7,DDSEENTL(,R7)
         CR    R7,R5
         JL    A0108
*
         GET   INDCB,INAREA       READ MEMBER DIRECTORY RECORD
         J     A0104
*
A0110    EQU   *
         LAY   R2,INDCB
         MVC   WKREENT(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
*
A0112    EQU   *
         LAY   R2,OU2DCB
         MVC   WKREENT(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
*
*        RETURN TO CALLER
*
DONE     EQU   *                         RETURN TO CALLER
         LAY   R2,OUTDCB
         MVC   WKREENT(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)
*
DONEDONE EQU   *                         RETURN TO CALLER
         L     R13,SAVEAREA+4            CALLER'S SAVE AREA ADDRESS
         L     R15,WKRETC
         ST    R15,16(,R13)
         FREEMAIN R,LV=DYNLEN,A=(11)     FREE DYNAMIC STORAGE
         LM    R14,R12,12(R13)           POP REGISTERS
         BR    R14                       RETURN TO CALLER
*
         DS    0D
MVCR14R1 MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *
         DS    0D
CLCR1R14 CLC   0(0,R1),0(R14)     * * * * E X E C U T E D * * * *
*
*
*        STATICS
*
*
*        CONSTANTS
*
H1       DC    H'1'
H4       DC    H'4'
H255     DC    H'255'
F04      DC    F'04'
F40      DC    F'40'
F4096    DC    F'4096'
CTTEYEB  DC    CL8'GVBCTT'
TKNNAME  DC    CL8'MTESTB'
GENEVA   DC    CL8'GENEVA'
TOKNPERS DC    F'0'                    TOKEN PERSISTENCE
TOKNLVL1 DC    A(1)                    NAME/TOKEN  AVAILABILITY  LEVEL
TOKNLVL2 DC    A(2)                    NAME/TOKEN  AVAILABILITY  LEVEL
*
         DS   0D
MODE31   EQU   X'8000'
         DS   0D
OPENPARM DC    XL8'8000000000000000'
*
OUTFILE  DCB   DSORG=PS,DDNAME=DDPRINT,MACRF=(PM),DCBE=OUTFDCBE,       X
               RECFM=FB,LRECL=131
OUTFILE0 EQU   *-OUTFILE
OUTFDCBE DCBE  RMODE31=BUFF
OUTFILEL EQU   *-OUTFILE
*
*
OU2FILE  DCB   DSORG=PS,DDNAME=DDDRUCK,MACRF=(PM),DCBE=OU2FDCBE,       X
               RECFM=FB,LRECL=8
OU2FILE0 EQU   *-OU2FILE
OU2FDCBE DCBE  RMODE31=BUFF
OU2FILEL EQU   *-OU2FILE
*
*
SINDCB   DCB   DSORG=PS,DDNAME=PDSEINDD,MACRF=(GM),DCBE=SINDCBE,       X
               RECFM=U
SINDCBL  EQU   *-SINDCB
SINDCBE  DCBE  RMODE31=BUFF,EODAD=A0110
SINDCBEL EQU   *-SINDCB
*
*
SPACES   DC    CL256' '
XHEXFF   DC 1024X'FF'
*
*
         LTORG ,
*
NUMMSK   DC    XL12'402020202020202020202021'
PATTERN  DC X'202020202020202020202020202020 '
ZEROES   DC    80XL1'00'
*
*******************************************************
*                 UNPACKED NUMERIC TRANSLATION MATRIX
*******************************************************
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
TRTACLVL DC    XL16'00D500000000000000C5000000000000'  00-0F
         DC    XL16'D9000000000000000000000000000000'  10-1F
         DC    XL16'E4000000000000000000000000000000'  20-2F
         DC    XL16'00000000000000000000000000000000'  30-3F
         DC    XL16'C3000000000000000000000000000000'  40-4F
         DC    XL16'00000000000000000000000000000000'  50-5F
         DC    XL16'00000000000000000000000000000000'  60-6F
         DC    XL16'00000000000000000000000000000000'  70-7F
         DC    XL16'C1000000000000000000000000000000'  80-8F
         DC    XL16'00000000000000000000000000000000'  90-9F
         DC    XL16'00000000000000000000000000000000'  A0-AF
         DC    XL16'00000000000000000000000000000000'  B0-BF
         DC    XL16'00000000000000000000000000000000'  C0-CF
         DC    XL16'00000000000000000000000000000000'  D0-DF
         DC    XL16'00000000000000000000000000000000'  E0-EF
         DC    XL16'00000000000000000000000000000000'  F0-FF
*
TRTTBLU  DC    XL16'08080808080808080808080808080808'  00-0F
         DC    XL16'08080808080808080808080808080808'  10-1F
         DC    XL16'08080808080808080808080808080808'  20-2F
         DC    XL16'08080808080808080808080808080808'  30-3F
         DC    XL16'08080808080808080808080808080808'  40-4F
         DC    XL16'08080808080808080808080808080808'  50-5F
         DC    XL16'08080808080808080808080808080808'  60-6F
         DC    XL16'08080808080808080808080808080808'  70-7F
         DC    XL16'08080808080808080808080808080808'  80-8F
         DC    XL16'08080808080808080808080808080808'  90-9F
         DC    XL16'08080808080808080808080808080808'  A0-AF
         DC    XL16'08080808080808080808080808080808'  B0-BF
         DC    XL16'08080808080808080808080808080808'  C0-CF
         DC    XL16'08080808080808080808080808080808'  D0-DF
         DC    XL16'08080808080808080808080808080808'  E0-EF
         DC    XL16'00000000000000000000080808080808'  F0-FF
*
         DS   0F
         DCBD  DSORG=PS
*
         IHADCBE
*
JFCBAR   DSECT
         IEFJFCBN LIST=YES
*
         CVT   DSECT=YES
*
         IHAPSA
*
         END
