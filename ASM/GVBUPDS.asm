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
*  This program uses the PDS/PDSE member list produced by GVBUDIR and
*  scans each member for suspiciously translated characters that
*  could cause problems during assembly/compilation of source code.
*
*  Acceptable characters are defined in table ALPHATRT.
*
***********************************************************************

         TITLE    'EXAMPLE: scan members of pdse for suspicious chars'
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
DINDCB   DS    XL(SDINDCBEL)   REENTRANT DCB AND DCBE AREAS
         DS    0F
MINDCB   DS    XL(SMINDCBEL)   REENTRANT DCB AND DCBE AREAS
*
         DS    0F
WKDECB   DS    F
         DS    0F
REENTWK  DS    XL256              RE-ENTRANT PARAMETER   LIST
WKPRINT  DS    XL255           PRINT LINE
         DS    XL1
         DS   0F
WKREENT  DS    XL256           REENTRANT WORKAREA
DBLWORK  DS    D               DOUBLE WORK WORKAREA
WKDBLWK  DS    D               DOUBLE WORK WORKAREA
DBLWORK2 DS    D               DOUBLE WORK WORKAREA
         DS    D               DOUBLE WORK WORKAREA
WKDDNAME DS    CL8
WKRETC   DS    F
WKMEMCT  DS    F
WKMEMLCT DS    F
WKTOTLCT DS    F
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
         WTO 'MCTEST: DDPRINT OPEN FAILED'
         MVC   WKRETC,=F'8'
         J     DONE
*
*                                                                @I1015
*      ECHO DDNAME
MAIN_100 EQU   *
         MVC   WKDDNAME,=CL8'DDPRINT'
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(32),=CL32'MCTEST: BEING EXECUTED WITH DD:'
         MVC   WKPRINT+32(8),WKDDNAME
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
* GET LIST OF MEMBERS INTO THE TABLE PDSENTNM
*
         LA    R14,SDINDCB               COPY MODEL   DCB
D2       USING IHADCB,DINDCB
         MVC   DINDCB(SDINDCBEL),0(R14)
         LAY   R0,DINDCB                 SET  DCBE ADDRESS IN  DCB
         AGHI  R0,SDINDCBL
         STY   R0,D2.DCBDCBE
*
         LAY   R2,DINDCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT)
         TM    48(R2),X'10'              SUCCESSFULLY OPENED  ??
         JO    A0100                     YES - BYPASS ABEND
         WTO 'MCTEST: DDDIRIN OPEN FAILED'
         MVC   WKRETC,=F'8'
         J     DONE
*
A0100    EQU   *
         XR    R7,R7
         LAY   R4,PDSENTNM
         GET   DINDCB,INAREA      READ MEMBER DIRECTORY RECORD
A0104    EQU   *
         LA    R7,1(,R7)          KEEP A COUNT
         MVC   0(8,R4),INAREA     COPY MEMBER TO TABLE
         LA    R4,8(R4)
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(17),=CL17'MCTEST: XXXXXXXX '
         MVC   WKPRINT+8(8),INAREA
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         GET   DINDCB,INAREA      READ MEMBER DIRECTORY RECORD
         J     A0104
*
A0110    EQU   *
         STH   R7,PDSENTLN
         LAY   R2,DINDCB
         CLOSE ((R2))
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(44),=CL44'MCTEST: EXPECTING TO PROCESS XXXXXXX M+
               EMBERS'
         LH    R15,PDSENTLN
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   DBLWORK2,NUMMASK
         ED    DBLWORK2,DBLWORK+4
         MVC   WKPRINT+28(8),DBLWORK2
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
* GO THROUGH EACH MEMBER, OPENING AND READING THE MEMBER
*
A0180    EQU   *
*
         LA    R14,SMINDCB               COPY MODEL   DCB
D3       USING IHADCB,MINDCB
         MVC   MINDCB(SMINDCBEL),0(R14)
         LAY   R0,MINDCB                 SET  DCBE ADDRESS IN  DCB
         AGHI  R0,SMINDCBL
         STY   R0,D3.DCBDCBE
*
         LAY   R2,MINDCB
         MVC   WKREENT(8),OPENPARM
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT)
         TM    48(R2),X'10'              SUCCESSFULLY OPENED  ??
         JO    A0200                     YES - BYPASS ABEND
         WTO 'MCTEST: DDMEMIN OPEN FAILED'
         MVC   WKRETC,=F'8'
         J     DONE
*
A0200    EQU   *
         LH    R8,PDSENTLN
         LAY   R9,PDSENTNM
         XC    WKMEMCT,WKMEMCT
         XC    WKTOTLCT,WKTOTLCT
*
A0201    EQU   *               --> LOOP THROUGH ALL MEMEBERS ----------
         XC    WKMEMLCT,WKMEMLCT
*
         MVC   BLDLENUM,=H'1'         NUMBER OF ENTRIES
         MVC   BLDLEBYT,=H'14'        NUMBER OF BYTES PER ENTRY
         MVC   BLDLMEMB,0(R9)         NAME OF MEMBER
         LAY   R2,MINDCB
         BLDL  (R2),BLDLLIST       RETRIEVE THE RELATIVE DISK LOCATION
*                                  OF NAMED MEMBER
         LA    R4,WKDECB
         LA    R3,BLDLLIST+4       POINT TO THE FIRST ENTRY IN THE LIST
         LAY   R2,MINDCB
         MVC   TTRN(4),8(R3)       GET RELATIVE DISK ADDRESS OF MEMBER
         FIND  (R2),TTRN,C         POINT TO THE MEMBER
*
         ASI   WKMEMCT,1           TOTAL MEMBERS
*
A0202    EQU   *
         LAY   R2,MINDCB
         L     R7,WKBUFFER
         READ  DECBX,SF,(R2),(R7)  READ A BLOCK OF THE MEMBER
         CHECK DECBX               WAIT FOR COMPLETION OF READ
**      DC H'0'
*
         XR    R4,R4
         LH    R5,D3.DCBBLKSI
**       LAY   R1,DECBX            R1 => DECBX
**       ICM   R1,B'1111',16(R1)   R1 => STATUS AREA
**       LH    R5,14(,R1)          R5 == LENGTH RETURNED BY READ
         LH    R2,D3.DCBLRECL
         DR    R4,R2               R5 = RECORDS IN BLOCK
A0204    EQU   *
         CLC   0(80,R7),ZEROES
         JE    A0210
*
         ASI   WKMEMLCT,1
         ASI   WKTOTLCT,1
*
         XR    R2,R2
         TRT   0(80,R7),ALPHATRT
         LTR   R2,R2
         JZ    A0206
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(57),=CL57'MCTEST: suspicious character x on line+
                Xxxxxxx of member xxxxxxxx : '
         MVC   WKPRINT+29(1),0(R1)
         L     R15,WKMEMLCT
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   DBLWORK2,NUMMASK
         ED    DBLWORK2,DBLWORK+4
         MVC   WKPRINT+38(8),DBLWORK2
         MVC   WKPRINT+57(8),0(R9)
         MVC   WKPRINT+68(80),0(R7)
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
A0206    EQU   *
         AH    R7,D3.DCBLRECL
         BRCT  R5,A0204
*
         J     A0202
*
*
A0210    EQU   *
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(35),=CL35'MCTEST: XXXXXXXX XXXXXXX LINES READ'
         MVC   WKPRINT+8(8),0(R9)
         L     R15,WKMEMLCT
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   DBLWORK2,NUMMASK
         ED    DBLWORK2,DBLWORK+4
         MVC   WKPRINT+16(8),DBLWORK2
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
         LA    R9,8(,R9)
         BRCT  R8,A0201        --> END OF LOOP
*
         LAY   R2,MINDCB
         CLOSE ((R2))
*
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(53),=CL53'MCTEST: PROCESSED XXXXXXX MEMBERS XXXX+
               XXX TOTAL LINES'
         L     R15,WKMEMCT
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   DBLWORK2,NUMMASK
         ED    DBLWORK2,DBLWORK+4
         MVC   WKPRINT+17(8),DBLWORK2
         L     R15,WKTOTLCT
         CVD   R15,DBLWORK
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         MVC   DBLWORK2,NUMMASK
         ED    DBLWORK2,DBLWORK+4
         MVC   WKPRINT+33(8),DBLWORK2
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
*
*
         MVC   WKPRINT,SPACES
         MVC   WKPRINT(8),=CL8'MCTEST: '
         MVC   WKPRINT+8(09),=CL9'COMPLETED'
         LA    R2,OUTDCB
         LA    R0,WKPRINT
         PUT   (R2),(R0)
         J     DONE
*
*
*
A0260    EQU   *
       WTO 'SYNAD CALLED'
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
               RECFM=FB,LRECL=255 131
OUTFILE0 EQU   *-OUTFILE
OUTFDCBE DCBE  RMODE31=BUFF
OUTFILEL EQU   *-OUTFILE
*
*
SMINDCB  DCB  DSORG=PO,DDNAME=DDMEMIN,MACRF=(R),DCBE=SMINDCBE,         X
               RECFM=FB,LRECL=80
SMINDCBL EQU   *-SMINDCB
SMINDCBE DCBE  RMODE31=BUFF,EODAD=A0210,SYNAD=A0260
SMINDCBEL EQU   *-SMINDCB
*
*
SDINDCB  DCB   DSORG=PS,DDNAME=DDDIRIN,MACRF=(GM),DCBE=SDINDCBE,       X
               RECFM=FB,LRECL=8
SDINDCBL EQU   *-SDINDCB
SDINDCBE DCBE  RMODE31=BUFF,EODAD=A0110
SDINDCBEL EQU   *-SDINDCB
*
*
SPACES   DC    CL256' '
XHEXFF   DC 1024X'FF'
*
*
         LTORG ,
*
NUMMSK   DC    XL12'402020202020202020202021'
NUMMASK  DC    XL08'4020202020202120'
PATTERN  DC X'202020202020202020202020202020 '
ZEROES   DC    80XL1'00'
*
*******************************************************
*                 UNPACKED NUMERIC TRANSLATION MATRIX
*******************************************************
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F
*
         DS    0F
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
ALPHATRT DC  256X'01' ALPHABETIC AND NATIONAL AND NORMAL CHARACTERS
         ORG ALPHATRT+C' '   SPACE
         DC  X'00'
         ORG ALPHATRT+C'$'   US NATIONAL CHARACTERS
         DC  X'00'
         ORG ALPHATRT+C'#'
         DC  X'00'
         ORG ALPHATRT+C'@'
         DC  X'00'
         ORG ALPHATRT+C'!'   NORMAL CHARACTERS.. ???
         DC  X'00'
         ORG ALPHATRT+C'%'
         DC  X'00'
         ORG ALPHATRT+X'50'  C'&'
         DC  X'00'
         ORG ALPHATRT+C'/'
         DC  X'00'
         ORG ALPHATRT+C'\'
         DC  X'00'
         ORG ALPHATRT+C'*'
         DC  X'00'
         ORG ALPHATRT+C'('
         DC  X'00'
         ORG ALPHATRT+C')'
         DC  X'00'
         ORG ALPHATRT+C'<'
         DC  X'00'
         ORG ALPHATRT+C'>'
         DC  X'00'
         ORG ALPHATRT+C'_'
         DC  X'00'
         ORG ALPHATRT+C'~'
         DC  X'00'
         ORG ALPHATRT+C'|'
         DC  X'00'
         ORG ALPHATRT+C'-'
         DC  X'00'
         ORG ALPHATRT+C'+'
         DC  X'00'
         ORG ALPHATRT+C'='
         DC  X'00'
         ORG ALPHATRT+C''''
         DC  X'00'
         ORG ALPHATRT+C'"'
         DC  X'00'
         ORG ALPHATRT+C'?'
         DC  X'00'
         ORG ALPHATRT+C':'
         DC  X'00'
         ORG ALPHATRT+C';'
         DC  X'00'
         ORG ALPHATRT+C','
         DC  X'00'
         ORG ALPHATRT+C'.'
         DC  X'00'
         ORG ALPHATRT+C'a'   alphabet
         DC 9X'00'
         ORG ALPHATRT+C'j'
         DC 9X'00'
         ORG ALPHATRT+C's'
         DC 8X'00'
         ORG ALPHATRT+C'A'
         DC 9X'00'
         ORG ALPHATRT+C'J'
         DC 9X'00'
         ORG ALPHATRT+C'S'
         DC 8X'00'
         ORG ALPHATRT+C'0'   numbers
         DC 10X'00'
         ORG ,
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
